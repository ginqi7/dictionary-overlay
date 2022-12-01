''' Add translation overlay for unknown words.'''
import asyncio
import json
import os
import re
import shutil
from threading import Timer

import snowballstemmer
import websocket_bridge_python
from sexpdata import dumps
from tokenizers import Tokenizer
from tokenizers.models import BPE
from tokenizers.pre_tokenizers import Whitespace

from pystardict import Dictionary

snowball_stemmer =  snowballstemmer.stemmer('english')
sdcv_dictionary_path = os.path.join(
    os.path.dirname(__file__), "resources", "kdic-ec-11w"
)
sdcv_dictionary = Dictionary(sdcv_dictionary_path, in_memory=True)

tokenizer = Tokenizer(BPE())
pre_tokenizer = Whitespace()
dictionary = {}

def in_or_stem_in(word:str, words) -> bool:
    return word in words or snowball_stemmer.stemWord(word) in words

async def parse(sentence: str):
    only_unknown_words = await bridge.get_emacs_var(
        "dictionary-overlay-just-unknown-words"
    )
    tokens = pre_tokenizer.pre_tokenize_str(sentence)
    if only_unknown_words == "true":
        tokens = [token for token in tokens if in_or_stem_in(token[0].lower(), unknown_words)]
    else:
        tokens = [token for token in tokens if new_word_p(token[0].lower())]
    return tokens

def new_word_p(word: str) -> bool:
    if len(word) < 3:
        return False
    if re.match(r"[\W\d]", word, re.M | re.I):
        return False
    return not in_or_stem_in(word, known_words)

def dump_knownwords_to_file():
    with open(knownwords_file_path, "w", encoding="utf-8") as f:
        for word in known_words:
            f.write(f"{word}\n")

def dump_unknownwords_to_file():
    with open(unknownwords_file_path, "w", encoding="utf-8") as f:
        for word in unknown_words:
            f.write(f"{word}\n")

def dump_dictionary_to_file():
    with open(dictionary_file_path, "w", encoding="utf-8") as f:
        json.dump(dictionary, f, ensure_ascii=False, indent=4)

def snapshot():
    try:
        dump_dictionary_to_file()
        dump_knownwords_to_file()
        dump_unknownwords_to_file()
    except:
        pass

    Timer(30, snapshot).start()

# dispatch message recived from Emacs.
async def on_message(message):
    info = json.loads(message)
    cmd = info[1][0].strip()
    if cmd == "render":
        sentence = info[1][1]
        buffer_name = info[1][3]
        await render(sentence, buffer_name)
        await run_and_log("(dictionary-overlay-refresh-overlays)")
    elif cmd == "jump_next_unknown_word":
        sentence = info[1][1]
        point = info[1][2]
        await jump_next_unknown_word(sentence, point)
    elif cmd == "jump_prev_unknown_word":
        sentence = info[1][1]
        point = info[1][2]
        await jump_prev_unknown_word(sentence, point)
    elif cmd == "mark_word_known":
        word = info[1][1]
        stem_word = snowball_stemmer.stemWord(word)
        if word in unknown_words:
            unknown_words.remove(word)
        if stem_word in unknown_words:
            unknown_words.remove(stem_word)
        known_words.add(word)
        known_words.add(stem_word)
    elif cmd == "mark_word_unknown":
        word = info[1][1]
        stem_word = snowball_stemmer.stemWord(word)
        if word in known_words:
            known_words.remove(word)
        if stem_word in known_words:
            known_words.remove(stem_word)
        unknown_words.add(word)
        unknown_words.add(stem_word)
    elif cmd == "mark_buffer":
        sentence = info[1][1]
        mark_buffer(sentence)
    elif cmd == "mark_buffer_unknown":
        sentence = info[1][1]
        mark_buffer_unknown(sentence)
    elif cmd == "modify_translation":
        # give user a selection to modify word translation.
        # combine with update_translation
        word = info[1][1]
        await modify_translation(word)
    elif cmd == "update_translation":
        # update translate in memory
        word = info[1][1]
        translation = info[1][2]
        dictionary[word]=translation

    else:
        print(f"not fount handler for {cmd}", flush=True)

async def modify_translation(word: str):
    all_translations = []
    # add all translations to make user select.
    # add word translation on local dictionary.
    all_translations.append(dictionary.get(word))
    # add word all translations on local sdcv dictionary
    all_translations.extend(extract_translations(sdcv_dictionary.get(word)))
    # add word stem all translations on local sdcv dictionary
    all_translations.extend(extract_translations(\
        sdcv_dictionary.get(snowball_stemmer.stemWord(word))))
    # add word web stranslation
    result = await web_translate(word)
    all_translations.append(result)
    # remove duplicative translations
    all_translations = list(set(all_translations))
    sexp = dumps(all_translations)
    cmd = f'(dictionary-overlay-choose-translate "{word}" \'{sexp})'
    await run_and_log(cmd)

def mark_buffer(sentence: str):
    tokens = pre_tokenizer.pre_tokenize_str(sentence)
    words = [
        token[0].lower() for token in tokens if not in_or_stem_in(token[0].lower(), unknown_words)
    ]

    for word in words:
        known_words.add(word)
        known_words.add(snowball_stemmer.stemWord(word))

def mark_buffer_unknown(sentence: str):
    tokens = pre_tokenizer.pre_tokenize_str(sentence)
    words = [
        token[0].lower() for token in tokens if not in_or_stem_in(token[0].lower(), known_words)
    ]
    for word in words:
        unknown_words.add(word)
        unknown_words.add(snowball_stemmer.stemWord(word))

def get_command_result(command_string, cwd=None):
    import subprocess

    process = subprocess.Popen(
        command_string,
        cwd=cwd,
        shell=True,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="utf-8",
    )
    ret = process.wait()
    return "".join((process.stdout if ret == 0 else process.stderr).readlines()).strip()  # type: ignore

async def jump_next_unknown_word(sentence: str, point: int):
    tokens = await parse(sentence)
    # todo: write this with build-in 'any' function
    for token in tokens:
        begin = token[1][0] + 1
        if point < begin:
            cmd = "(goto-char {begin})".format(begin=begin)
            await run_and_log(cmd)
            break

async def jump_prev_unknown_word(sentence: str, point: int):
    tokens = await parse(sentence)
    # todo: write this with build-in 'any' function
    for token in reversed(tokens):
        begin = token[1][0] + 1
        if point > begin:
            cmd = "(goto-char {begin})".format(begin=begin)
            await run_and_log(cmd)
            break

async def web_translate(word: str) -> str:
    try:
        if shutil.which("crow"):
            result = get_command_result(f'crow -t zh-CN --json -e {crow_engine} "{word}"')
            return json.loads(result)["translation"]
        else:
            import google_translate  # type: ignore
            result = google_translate.translate(word, dst_lang='zh')
            return result["trans"][0]
    except ImportError:
        msg= f"[Dictionary-overlay]you do not have a network dictionary installed and the queried word [\"{word}\"] is not in the local dictionary, please install crow-translate or google-translate"
        print(msg)
        await bridge.message_to_emacs(msg)
        return ""
    except Exception as e:
        print (e)
        msg = "[Dictionary-overlay]web-translate error, check your network. or run (websocket-bridge-app-open-buffer 'dictionary-overlay) see the error details."
        await bridge.message_to_emacs(msg)
        return ""

def extract_translations(msg:str):
    '''extract translations by regex'''
    re_chinese_words = re.compile("[\u4e00-\u9fa5]+")
    return re.findall(re_chinese_words, msg);

async def translate(word: str):
    ''' translate word.'''
    if word in sdcv_dictionary:
        # default show the first translation in sdcv dictionary
        translations = extract_translations(sdcv_dictionary.get(word))
        if translations:
            return translations[0]
    return await web_translate(word)

async def render(message, buffer_name):
    '''call Emacs render message'''
    try:
        tokens = await parse(message)
        for token in tokens:
            word = token[0].lower()
            # first try find translation in local dictionary text
            # dictionary contains last translation
            chinese = dictionary.get(word, "")
            if chinese == "":
                # if first step find nothing, then try find translation on sdcv or web.
                chinese = await translate(word)
                dictionary[word] = chinese
            if chinese == "":
                # if find nothing, don't run render function in emacs.
                return
            await render_word(token, chinese, buffer_name)
    except Exception as e:
        msg = "[Dictionary-overlay]Render buffer error. Run (websocket-bridge-app-open-buffer 'dictionary-overlay) see the error details"
        await bridge.message_to_emacs(msg)
        print(e)

async def render_word(token, chinese, buffer_name):
    word = token[0]
    begin = token[1][0] + 1
    end = token[1][1] + 1
    cmd = f'(dictionary-add-overlay-from {begin} {end} "{word}" "{chinese}" "{buffer_name}")'
    await run_and_log(cmd)

# eval in emacs and log the command.
async def run_and_log(cmd):
    print(cmd, flush=True)
    await bridge.eval_in_emacs(cmd)

async def main():
    snapshot()
    global bridge
    bridge = websocket_bridge_python.bridge_app_regist(on_message)
    await asyncio.gather(init(), bridge.start())

async def init():
    global dictionary_file_path, knownwords_file_path, unknownwords_file_path, known_words, unknown_words, crow_engine
    crow_engine = await bridge.get_emacs_var("dictionary-overlay-crow-engine")
    crow_engine = crow_engine.strip('"')
    user_data_directory = await bridge.get_emacs_var("dictionary-overlay-user-data-directory")
    user_data_directory = os.path.expanduser(user_data_directory.strip('"'))
    dictionary_file_path = os.path.join(user_data_directory, "dictionary.json")
    knownwords_file_path = os.path.join(user_data_directory, "knownwords.txt")
    unknownwords_file_path = os.path.join(user_data_directory, "unknownwords.txt")
    create_user_data_file_if_not_exist(dictionary_file_path, "{}")
    create_user_data_file_if_not_exist(knownwords_file_path)
    create_user_data_file_if_not_exist(unknownwords_file_path)
    with open(dictionary_file_path, "r", encoding="utf-8") as f: dictionary = json.load(f)
    with open(knownwords_file_path, "r", encoding="utf-8") as f:  known_words= set(f.read().split())
    with open(unknownwords_file_path, "r", encoding="utf-8") as f:  unknown_words= set(f.read().split())

def create_user_data_file_if_not_exist(path: str, content=None):
    if not os.path.exists(path):
        # Build parent directories when file is not exist.
        basedir = os.path.dirname(path)
        if not os.path.exists(basedir):
            os.makedirs(basedir)

        with open(path, "w", encoding="utf-8") as f:
            if content:
                f.write(content)

        print(f"[dictionary-overlay] auto create user data file {path}")

asyncio.run(main())
