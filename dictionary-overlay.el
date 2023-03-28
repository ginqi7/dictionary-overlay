;;; dictionary-overlay.el --- Add overlay for new English word  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `dictionary-overlay-start'
;;    Start dictionary-overlay.
;;    Keybinding: M-x dictionary-overlay-start
;;  `dictionary-overlay-stop'
;;    Stop dictionary-overlay.
;;    Keybinding: M-x dictionary-overlay-stop
;;  `dictionary-overlay-restart'
;;    Restart dictionary-overlay.
;;    Keybinding: M-x dictionary-overlay-restart
;;  `dictionary-overlay-render-buffer'
;;    Render current buffer.
;;    Keybinding: M-x dictionary-overlay-render-buffer
;;  `dictionary-overlay-toggle'
;;    Toggle current buffer.
;;    Keybinding: M-x dictionary-overlay-toggle
;;  `dictionary-overlay-refresh-buffer'
;;    Refresh current buffer.
;;    Keybinding: r
;;  `dictionary-overlay-jump-first-unknown-word'
;;    Jump to first unknown word.
;;    Keybinding: <
;;  `dictionary-overlay-jump-last-unknown-word'
;;    Jump to last unknown word.
;;    Keybinding: >
;;  `dictionary-overlay-jump-next-unknown-word'
;;    Jump to next unknown word.
;;    Keybinding: n
;;  `dictionary-overlay-jump-prev-unknown-word'
;;    Jump to previous unknown word.
;;    Keybinding: p
;;  `dictionary-overlay-jump-out-of-overlay'
;;    Jump out overlay so that we no longer in keymap.
;;    Keybinding: <escape>
;;  `dictionary-overlay-mark-word-known'
;;    Mark current word known.
;;    Keybinding: M-x dictionary-overlay-mark-word-known
;;  `dictionary-overlay-mark-word-unknown'
;;    Mark current word unknown.
;;    Keybinding: M-x dictionary-overlay-mark-word-unknown
;;  `dictionary-overlay-mark-word-smart'
;;    Smartly mark current word as known or unknown.
;;    Keybinding: M-x dictionary-overlay-mark-word-smart
;;  `dictionary-overlay-mark-word-smart-reversely'
;;    Smartly mark current word known or unknown smartly, but reversely.
;;    Keybinding: M
;;  `dictionary-overlay-mark-buffer'
;;    Mark all words as known, except those in `unknownwords' list.
;;    Keybinding: M-x dictionary-overlay-mark-buffer
;;  `dictionary-overlay-mark-buffer-unknown'
;;    Mark all words as unknown, except those in `unknownwords' list.
;;    Keybinding: M-x dictionary-overlay-mark-buffer-unknown
;;  `dictionary-overlay-lookup'
;;    Look up word in a third-parity dictionary.
;;    Keybinding: d
;;  `dictionary-overlay-install'
;;    Install all python dependencies.
;;    Keybinding: M-x dictionary-overlay-install
;;  `dictionary-overlay-macos-install-core-services'
;;    Install all python dependencies.
;;    Keybinding: M-x dictionary-overlay-macos-install-core-services
;;  `dictionary-overlay-install-google-translate'
;;    Install all google-translate dependencies.
;;    Keybinding: M-x dictionary-overlay-install-google-translate
;;  `dictionary-overlay-modify-translation'
;;    Modify current word's translation.
;;    Keybinding: c
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `dictionary-overlay-just-unknown-words'
;;    If t, show overlay for words in unknownwords list.
;;    default = t
;;  `dictionary-overlay-position'
;;    Where to show translation.
;;    default = 'after
;;  `dictionary-overlay-user-data-directory'
;;    Place user data in Emacs directory.
;;    default = (locate-user-emacs-file "dictionary-overlay-data/")
;;  `dictionary-overlay-translation-format'
;;    Translation format.
;;    default = "(%s)"
;;  `dictionary-overlay-crow-engine'
;;    Crow translate engine.
;;    default = "google"
;;  `dictionary-overlay-inhibit-keymap'
;;    When non-nil, don't use `dictionary-overlay-map'.
;;    default = nil
;;  `dictionary-overlay-auto-jump-after'
;;    Auto jump to next unknown word.
;;    default = 'nil
;;  `dictionary-overlay-recenter-after-mark-and-jump'
;;    Recenter after mark or jump.
;;    default = nil
;;  `dictionary-overlay-lookup-with'
;;    Look up word with fn.
;;    default = 'dictionary-lookup-definition
;;  `dictionary-overlay-translators'
;;    The translators and theirs's order.
;;    default = '("local" "sdcv" "darwin" "web")
;;  `dictionary-overlay-sdcv-dictionary-path'
;;    User defined sdcv dictionary path.
;;    default = nil
;;  `dictionary-overlay-python'
;;    The Python interpreter.
;;    default = "python3"

;;; Code:

(require 'websocket-bridge)

(defgroup dictionary-overlay ()
  "Dictionary overlay for words in buffers."
  :group 'applications)

(defface dictionary-overlay-unknownword nil
  "Face for dictionary-overlay unknown words."
  :group 'dictionary-overlay)

(defface dictionary-overlay-translation nil
  "Face for dictionary-overlay translations."
  :group 'dictionary-overlay)

(defvar dictionary-overlay-py-path
  (concat (file-name-directory load-file-name)
          "dictionary-overlay.py"))

(defvar dictionary-overlay-py-requirements-path
  (concat (file-name-directory load-file-name) "requirements.txt"))

(defvar-local dictionary-overlay-active-p nil
  "Check current buffer if active dictionary-overlay.")

(defvar-local dictionary-overlay-hash-table nil
  "Hash-table contains overlays for dictionary-overlay.
The key's format is begin:end:word:translation.")

(defvar-local dictionary-overlay-hash-table-keys '()
  "Contains all hashtable-keys for dictionary-overlay.
The key's format is begin:end:word:translation.")

(defcustom dictionary-overlay-just-unknown-words t
  "If t, show overlay for words in unknownwords list.
If nil, show overlay for words not in knownwords list."
  :group 'dictionary-overlay
  :type '(boolean))

(defcustom dictionary-overlay-position 'after
  "Where to show translation.
If value is \\='after, put translation after word
If value is \\='help-echo, show it when mouse over word."
  :group 'dictionary-overlay
  :type '(choice
          (cons :tag "Show after word" 'after)
          (cons :tag "Show in help-echo" 'help-echo)))

(defcustom dictionary-overlay-user-data-directory
  (locate-user-emacs-file "dictionary-overlay-data/")
  "Place user data in Emacs directory."
  :group 'dictionary-overlay
  :type '(directory))

(defcustom dictionary-overlay-translation-format "(%s)"
  "Translation format."
  :group 'dictionary-overlay
  :type '(string))

(defcustom dictionary-overlay-crow-engine "google"
  "Crow translate engine."
  :group 'dictionary-overlay
  :type '(string))

;; SRC: ideas from `symbol-overlay', tip hat!
(defcustom dictionary-overlay-inhibit-keymap nil
  "When non-nil, don't use `dictionary-overlay-map'.
This is intended for buffers/modes that use the keymap text
property for their own purposes.  Because this package uses
overlays it would always override the text property keymaps
of such packages."
  :group 'dictionary-overlay
  :type '(boolean))

(defcustom dictionary-overlay-auto-jump-after '()
  "Auto jump to next unknown word.
Main purpose of auto jump is to keep cursor stay within overlay
to facilitate the usage of keymap. For MARK-WORD-(UN)KNOWN,
usually jump to the next unknown word, but depends on direction
set by `dictionary-overlay-jump-direction'. For RENDER-BUFFER, if
current cursor is within overlay, do nothing; otherwise move to
next overlay."
  :group 'dictionary-overlay
  :type '(repeat
          (choice
           (const :tag "Don't jump" nil)
           (const :tag "After mark word known" mark-word-known)
           (const :tag "After mark word unknown" mark-word-unknown)
           (const :tag "After refresh buffer" render-buffer))))

(defvar-local dictionary-overlay-jump-direction 'next
  "Direction to jump word.")

(defcustom dictionary-overlay-recenter-after-mark-and-jump nil
  "Recenter after mark or jump."
  :group 'dictionary-overlay
  :type '(choice (boolean :tag "Do nothing" nil)
                 (integer :tag "Recenter to lines" N)))

(defcustom dictionary-overlay-lookup-with 'dictionary-lookup-definition
  "Look up word with fn."
  :group 'dictionary-overlay
  :type '(function))

(defcustom dictionary-overlay-translators '("local" "sdcv" "darwin" "web")
  "The translators and theirs's order."
  :group 'dictionary-overlay
  :type '(list))

(defcustom dictionary-overlay-sdcv-dictionary-path nil
  "User defined sdcv dictionary path."
  :group 'dictionary-overlay
  :type '(string))

(defcustom dictionary-overlay-python "python3"
  "The Python interpreter."
  :type 'string)

(defvar dictionary-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'dictionary-overlay-lookup)
    (define-key map (kbd "r") #'dictionary-overlay-refresh-buffer)
    (define-key map (kbd "p") #'dictionary-overlay-jump-prev-unknown-word)
    (define-key map (kbd "n") #'dictionary-overlay-jump-next-unknown-word)
    (define-key map (kbd "<") #'dictionary-overlay-jump-first-unknown-word)
    (define-key map (kbd ">") #'dictionary-overlay-jump-last-unknown-word)
    (define-key map (kbd "m") #'dictionary-overlay-mark-word-smart)
    (define-key map (kbd "M") #'dictionary-overlay-mark-word-smart-reversely)
    (define-key map (kbd "c") #'dictionary-overlay-modify-translation)
    (define-key map (kbd "<escape>") #'dictionary-overlay-jump-out-of-overlay)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")

(defun dictionary-overlay-start ()
  "Start dictionary-overlay."
  (interactive)
  (websocket-bridge-server-start)
  (websocket-bridge-app-start
   "dictionary-overlay"
   dictionary-overlay-python
   dictionary-overlay-py-path))

(defun dictionary-overlay-stop ()
  "Stop dictionary-overlay."
  (interactive)
  (websocket-bridge-app-exit "dictionary-overlay"))

(defun dictionary-overlay-restart ()
  "Restart dictionary-overlay."
  (interactive)
  (dictionary-overlay-stop)
  (dictionary-overlay-start)
  ;; REVIEW: really need bring this buffer to front? or we place it at bottom?
  ;; (split-window-below -10)
  ;; (other-window 1)
  ;; (websocket-bridge-app-open-buffer "dictionary-overlay")
  )

(defun websocket-bridge-call-buffer (func-name)
  "Call grammarly function on current buffer by FUNC-NAME."
  (websocket-bridge-call "dictionary-overlay" func-name
                         (buffer-string)
                         (point)
                         (buffer-name)))

(defun websocket-bridge-call-word (func-name)
  "Call grammarly function on current word by FUNC-NAME."
  (let ((word (downcase (thing-at-point 'word))))
    (websocket-bridge-call "dictionary-overlay" func-name
                           (downcase (thing-at-point 'word)))
    (message word)))

(defun dictionary-overlay-render-buffer ()
  "Render current buffer."
  (interactive)
  (if (not (dictionary-overlay-ready-p))
      (message "Dictionary-Overlay not ready, please wait a second.")
    (when (not (member "dictionary-overlay" websocket-bridge-app-list))
      (dictionary-overlay-start))
    (setq-local dictionary-overlay-active-p t)
    (dictionary-overlay-refresh-buffer)
    (when (member 'render-buffer dictionary-overlay-auto-jump-after)
      (websocket-bridge-call-buffer "jump_next_unknown_word"))
    ))

(defun dictionary-overlay-toggle ()
  "Toggle current buffer."
  (interactive)
  (if dictionary-overlay-active-p
      (progn
        ;; reset all hash-table-keys and delete all overlays
        (setq-local dictionary-overlay-hash-table-keys '())
        (dictionary-overlay-refresh-overlays)
        (setq-local dictionary-overlay-active-p nil)
        (message "Dictionary overlay removed."))
    (progn
      (dictionary-overlay-render-buffer)
      (message "Dictionary overlay rendered."))))

(defun dictionary-overlay-refresh-overlays ()
  "Refresh overlays: remove overlays and hash-table items when not needed."
  (maphash
   (lambda (key val)
     (when (not (member key dictionary-overlay-hash-table-keys))
       (remhash key dictionary-overlay-hash-table)
       (delete-overlay val)))
   dictionary-overlay-hash-table))

(defun dictionary-overlay-refresh-buffer ()
  "Refresh current buffer."
  (interactive)
  (when dictionary-overlay-active-p
    (when (not dictionary-overlay-hash-table)
      (setq-local dictionary-overlay-hash-table (make-hash-table :test 'equal)))
    (setq-local dictionary-overlay-hash-table-keys '())
    (websocket-bridge-call-buffer "render")))

(defun dictionary-overlay-first-unknown-word-pos ()
  "End of last unknown word pos.
NOTE: Retrieval of word pos relies on `dictionary-overlay-hash-table-keys',
so currently won't work for auto jump after render buffer. Same as to
`dictionary-overlay-last-unknown-word-pos'."
  (string-to-number
   (car (split-string
         (car (last dictionary-overlay-hash-table-keys)) ":"))))

(defun dictionary-overlay-cursor-before-first-unknown-word-p ()
  "Whether cursor is after word beginning of last unknown word."
  (<= (point) (dictionary-overlay-first-unknown-word-pos)))

(defun dictionary-overlay-last-unknown-word-pos ()
  "Beginning of last unnow word pos."
  (string-to-number
   (car (split-string
         (car dictionary-overlay-hash-table-keys) ":"))))

(defun dictionary-overlay-cursor-after-last-unknown-word-p ()
  "Whether cursor is after word beginning of last unknown word."
  (>= (point) (dictionary-overlay-last-unknown-word-pos)))

(defun dictionary-overlay-jump-first-unknown-word ()
  "Jump to first unknown word."
  (interactive)
  (goto-char (dictionary-overlay-first-unknown-word-pos)))

(defun dictionary-overlay-jump-last-unknown-word ()
  "Jump to last unknown word."
  (interactive)
  (goto-char (dictionary-overlay-last-unknown-word-pos)))

(defun dictionary-overlay-jump-next-unknown-word ()
  "Jump to next unknown word."
  (interactive)
  (if (dictionary-overlay-cursor-after-last-unknown-word-p)
      (dictionary-overlay-jump-first-unknown-word)
    (websocket-bridge-call-buffer "jump_next_unknown_word"))
  (setq-local dictionary-overlay-jump-direction 'next)
  (when dictionary-overlay-recenter-after-mark-and-jump
    (recenter dictionary-overlay-recenter-after-mark-and-jump)))

(defun dictionary-overlay-jump-prev-unknown-word ()
  "Jump to previous unknown word."
  (interactive)
  (if (dictionary-overlay-cursor-before-first-unknown-word-p)
      (dictionary-overlay-jump-last-unknown-word)
    (websocket-bridge-call-buffer "jump_prev_unknown_word"))
  (setq-local dictionary-overlay-jump-direction 'prev)
  (when dictionary-overlay-recenter-after-mark-and-jump
    (recenter dictionary-overlay-recenter-after-mark-and-jump)))

(defun dictionary-overlay-jump-out-of-overlay ()
  "Jump out overlay so that we no longer in keymap.
Usually overlay keymap has a higher priority than other major and
minor mode keymap. Jumping out of overlay facilitates the usage
of original mode keymap. Since overlay is everywhere, don't expect it
to work consistently, but usually it does a decent job."
  (interactive)
  (forward-word))

(defun dictionary-overlay-mark-word-known ()
  "Mark current word known."
  (interactive)
  (websocket-bridge-call-word "mark_word_known")
  (when (member 'mark-word-known dictionary-overlay-auto-jump-after)
    (pcase dictionary-overlay-jump-direction
      (`next (dictionary-overlay-jump-next-unknown-word))
      (`prev (dictionary-overlay-jump-prev-unknown-word))))
  (dictionary-overlay-refresh-buffer)
  (when dictionary-overlay-recenter-after-mark-and-jump
    (recenter dictionary-overlay-recenter-after-mark-and-jump)))

(defun dictionary-overlay-mark-word-unknown ()
  "Mark current word unknown."
  (interactive)
  (websocket-bridge-call-word "mark_word_unknown")
  (when (member 'mark-word-unknown dictionary-overlay-auto-jump-after)
    (pcase dictionary-overlay-jump-direction
      (`next (dictionary-overlay-jump-next-unknown-word))
      (`prev (dictionary-overlay-jump-prev-unknown-word))))
  (dictionary-overlay-refresh-buffer)
  (when dictionary-overlay-recenter-after-mark-and-jump
    (recenter dictionary-overlay-recenter-after-mark-and-jump)))

(defun dictionary-overlay-mark-word-smart ()
  "Smartly mark current word as known or unknown.
Based on value of `dictionary-overlay-just-unknown-words'
Usually when value is t, we want to mark word as unknown. Vice versa.
If you need reverse behavior, use:
`dictionary-overlay-mark-word-smart-reversely' instead."
  (interactive)
  (if dictionary-overlay-just-unknown-words
      (dictionary-overlay-mark-word-unknown)
    (dictionary-overlay-mark-word-known)))

(defun dictionary-overlay-mark-word-smart-reversely ()
  "Smartly mark current word known or unknown smartly, but reversely.
Based on value of `dictionary-overlay-just-unknown-words'"
  (interactive)
  (if dictionary-overlay-just-unknown-words
      (dictionary-overlay-mark-word-known)
    (dictionary-overlay-mark-word-unknown)))

(defun dictionary-overlay-mark-buffer ()
  "Mark all words as known, except those in `unknownwords' list."
  (interactive)
  (when (y-or-n-p
         "Mark all as KNOWN, EXCEPT those in unknownwords list?")
    (websocket-bridge-call-buffer "mark_buffer")
    (dictionary-overlay-refresh-buffer)))

(defun dictionary-overlay-mark-buffer-unknown ()
  "Mark all words as unknown, except those in `unknownwords' list."
  (interactive)
  (when (y-or-n-p
         "Mark all as UNKNOWN, EXCEPT those in unknownwords list?")
    (websocket-bridge-call-buffer "mark_buffer_unknown")
    (dictionary-overlay-refresh-buffer)))

(defun dictionary-overlay-lookup ()
  "Look up word in a third-parity dictionary.
NOTE: third party dictionaries have their own implemention of
getting words. Probably the word will be the same as the one
dictionary-overlay gets."
  (interactive)
  (funcall dictionary-overlay-lookup-with))

(defun dictionary-add-overlay-from (begin end source target buffer-name)
  "Add overlay for SOURCE and TARGET from BEGIN to END in BUFFER-NAME."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((ov (make-overlay begin end (get-buffer buffer-name)))
            (hash-table-key
             (format "%s:%s:%s:%s" begin end source target)))
        ;; record the overlay's key
        (add-to-list 'dictionary-overlay-hash-table-keys hash-table-key)
        (when (not (gethash hash-table-key dictionary-overlay-hash-table))
          ;; create an overly only when the key not exists
          (overlay-put ov 'face 'dictionary-overlay-unknownword)
          (overlay-put ov 'evaporate t)
          (unless dictionary-overlay-inhibit-keymap
            (overlay-put ov 'keymap dictionary-overlay-map))
          (pcase dictionary-overlay-position
            ('after
             (progn
               (overlay-put
                ov 'after-string
                (propertize
                 (format dictionary-overlay-translation-format target)
                 'face 'dictionary-overlay-translation))))
            ('help-echo
             (overlay-put
              ov 'help-echo
              (format dictionary-overlay-translation-format target))))
          (puthash hash-table-key ov dictionary-overlay-hash-table))))))

(defun dictionary-overlay-install ()
  "Install all python dependencies."
  (interactive)
  (let ((process-environment
         (cons "NO_COLOR=true" process-environment))
        (process-buffer-name "*dictionary-overlay-install*"))
    (set-process-sentinel
     (start-process "dictionary-overlay-install"
                    process-buffer-name
                    "pip" "install" "-r"
                    dictionary-overlay-py-requirements-path)
     (lambda (p _m)
       (when (eq 0 (process-exit-status p))
         (with-current-buffer (process-buffer p)
           (ansi-color-apply-on-region (point-min) (point-max))))))
    (split-window-below)
    (other-window 1)
    (switch-to-buffer process-buffer-name)))

(defun dictionary-overlay-macos-install-core-services ()
  "Install all python dependencies."
  (interactive)
  (let ((process-environment
         (cons "NO_COLOR=true" process-environment))
        (process-buffer-name "*dictionary-overlay-install*"))
    (set-process-sentinel
     (start-process "dictionary-overlay-install"
                    process-buffer-name
                    "pip" "install"
                    "pyobjc-framework-CoreServices"
                    )
     (lambda (p _m)
       (when (eq 0 (process-exit-status p))
         (with-current-buffer (process-buffer p)
           (ansi-color-apply-on-region (point-min) (point-max))))))
    (split-window-below)
    (other-window 1)
    (switch-to-buffer process-buffer-name)))


(defun dictionary-overlay-install-google-translate ()
  "Install all google-translate dependencies."
  (interactive)
  (let* ((process-environment
          (cons "NO_COLOR=true" process-environment))
         (process-buffer-name "*dictionary-overlay-install*")
         (temp-install-directory
          (make-temp-file "install-google-translate" t))
         (process-cmd
          (format
           (concat "git clone https://git.ookami.one/cgit/google-translate/ %s; "
                   "cd %s; "
                   "pip install build; "
                   "make install")
           temp-install-directory temp-install-directory)))
    (set-process-sentinel
     (start-process-shell-command
      "dictionary-overlay-install-google-translate"
      process-buffer-name
      process-cmd)
     (lambda (p _m)
       (when (eq 0 (process-exit-status p))
         (with-current-buffer (process-buffer p)
           (ansi-color-apply-on-region (point-min) (point-max))))))
    (split-window-below)
    (other-window 1)
    (switch-to-buffer process-buffer-name)))

(defun dictionary-overlay-modify-translation ()
  "Modify current word's translation."
  (interactive)
  (let ((word (downcase (thing-at-point 'word t))))
    (websocket-bridge-call "dictionary-overlay"
                           "modify_translation"
                           word)))

(defun dictionary-overlay-choose-translate (word candidates)
  "Choose WORD's translation CANDIDATES."
  (let ((translation
         (completing-read "Choose or input translation: " candidates)))
    (websocket-bridge-call "dictionary-overlay"
                           "update_translation"
                           word
                           translation))
  (dictionary-overlay-render-buffer))

(defun dictionary-overlay-ready-p ()
  "Check diction-overly if ready."
  (and
   (member "dictionary-overlay" websocket-bridge-app-list)
   (boundp 'websocket-bridge-client-dictionary-overlay)))

(provide 'dictionary-overlay)
;;; dictionary-overlay.el ends here
