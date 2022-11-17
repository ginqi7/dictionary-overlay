from pystardict import Dictionary
from threading import Timer
from tokenizers import Tokenizer
from tokenizers.models import BPE
from tokenizers.pre_tokenizers import Whitespace

import asyncio
import json
import os
import re
import shutil
import websocket_bridge_python
 

sdcv_dictionary_path = os.path.join(
    os.path.dirname(__file__), "resources", "kdic-ec-11w"
)
sdcv_dictionary = Dictionary(sdcv_dictionary_path, in_memory=True)

sdcv_words = {}
candidates = []
for word in sdcv_dictionary.keys():
    first_line_translation = sdcv_dictionary.dict[word].split()[0]
    candidate_word = word.lower().replace('"', " ")
    candidate_translation = first_line_translation.split(".")[-1].split(";")[0]

    sdcv_words[candidate_word] = candidate_translation


tokenizer = Tokenizer(BPE())
pre_tokenizer = Whitespace()
dictionary = {}


async def parse(sentence: str):
    only_unknown_words = await bridge.get_emacs_var(
        "dictionary-overlay-just-unknown-words"
    )
    tokens = pre_tokenizer.pre_tokenize_str(sentence)
    if only_unknown_words == "true":
        tokens = [token for token in tokens if token[0].lower() in unknown_words]
    else:
        tokens = [token for token in tokens if new_word_p(token[0].lower())]
    return tokens


def new_word_p(word: str) -> bool:
    if len(word) < 3:
        return False
    if re.match(r"[\W\d]", word, re.M | re.I):
        return False
    return word not in known_words


def dump_knownwords_to_file():
    with open(knownwords_file_path, "w") as f:
        for word in known_words:
            f.write(f"{word}\n")


def dump_unknownwords_to_file():
    with open(unknownwords_file_path, "w") as f:
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
    sentence = info[1][1]
    point = info[1][2]
    if cmd == "render":
        await render(sentence)
    elif cmd == "jump_next_unknown_word":
        await jump_next_unknown_word(sentence, point)
    elif cmd == "jump_prev_unknown_word":
        await jump_prev_unknown_word(sentence, point)
    elif cmd == "mark_word_known":
        word = info[1][3]
        if word in unknown_words:
            unknown_words.remove(word)
        known_words.add(word)
    elif cmd == "mark_word_unknown":
        word = info[1][3]
        if word in known_words:
            known_words.remove(word)
        unknown_words.add(word)
    elif cmd == "mark_buffer":
        mark_buffer(sentence)
    else:
        print(f"not fount handler for {cmd}", flush=True)


def mark_buffer(sentence: str):
    tokens = pre_tokenizer.pre_tokenize_str(sentence)
    words = [
        token[0].lower() for token in tokens if token[0].lower() not in unknown_words
    ]
    for word in words:
        known_words.add(word)


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
        
def web_translate(word: str) -> str:
    if shutil.which("crow"):
        result = get_command_result(f'crow -t zh-CN --json -e google "{word}"')
        return json.loads(result)["translation"]
    else:
        try:
            import google_translate     # type: ignore
            result = google_translate.translate(word, dst_lang='zh')
            return result["trans"][0]
        except:
            raise Exception(f"you do not have a network dictionary installed and the queried word [{word}] is not in the local dictionary, please install crow-translate or google-translate")

def translate(word: str):
    if word in sdcv_words:
        return sdcv_words[word]
    else:
        return web_translate(word)

async def render(message):
    try:
        tokens = await parse(message)
        for token in tokens:
            word = token[0].lower()
            chinese = ""

            if word in dictionary:
                chinese = dictionary[word]

            if chinese == "%s" or chinese == "":
                chinese = translate(word)
                dictionary[word] = chinese

            await render_word(token, chinese)
    except Exception as e:
        print(e)


async def render_word(token, chinese):
    word = token[0]
    display = "{en}({zh})".format(en=word, zh=chinese)
    begin = token[1][0] + 1
    end = token[1][1] + 1
    cmd = '(dictionary-add-overlay-from {begin} {end} "{word}" "{display}")'.format(
        begin=begin, end=end, word=word, display=display
    )
    await run_and_log(cmd)


# eval in emacs and log the command.
async def run_and_log(cmd):
    print(cmd, flush=True)
    await bridge.eval_in_emacs(cmd)


async def main():
    snapshot()
    global bridge
    bridge = websocket_bridge_python.bridge_app_regist(on_message)
    await asyncio.gather(init_user_data(), bridge.start())


async def init_user_data():
    global dictionary_file_path, knownwords_file_path, unknownwords_file_path, known_words, unknown_words
    user_data_directory = await bridge.get_emacs_var("dictionary-overlay-user-data-directory")
    user_data_directory = os.path.expanduser(user_data_directory.strip('"'))
    dictionary_file_path = os.path.join(user_data_directory, "dictionary.json")
    knownwords_file_path = os.path.join(user_data_directory, "knownwords.txt")
    unknownwords_file_path = os.path.join(user_data_directory, "unknownwords.txt")
    create_user_data_file_if_not_exist(dictionary_file_path, "{}")
    create_user_data_file_if_not_exist(knownwords_file_path)
    create_user_data_file_if_not_exist(unknownwords_file_path)
    with open(dictionary_file_path, "r") as f: dictionary = json.load(f)
    with open(knownwords_file_path, "r") as f:  known_words= set(f.read().split())
    with open(unknownwords_file_path, "r") as f:  unknown_words= set(f.read().split())
    
def create_user_data_file_if_not_exist(path: str, content=None):
    if not os.path.exists(path):
        # Build parent directories when file is not exist.
        basedir = os.path.dirname(path)
        if not os.path.exists(basedir):
            os.makedirs(basedir)

        with open(path, "w") as f: 
            if content:
                f.write(content)
                
        print(f"[dictionary-overlay] auto create user data file {path}")                

asyncio.run(main())
