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
;;  `dictionary-overlay-stop'
;;    Stop dictionary-overlay.
;;  `dictionary-overlay-restart'
;;    Restart dictionary-overlay and show process.
;;  `dictionary-overlay-render-buffer'
;;    Render current buffer.
;;  `dictionary-overlay-toggle'
;;    Toggle current buffer.
;;  `dictionary-overlay-jump-next-unknown-word'
;;    Jump to next unknown word.
;;  `dictionary-overlay-jump-prev-unknown-word'
;;    Jump to prev unknown word.
;;  `dictionary-overlay-jump-first-unknown-word'
;;    Jump to first unknown word.
;;  `dictionary-overlay-jump-last-unknown-word'
;;    Jump to last unknown word.
;;  `dictionary-overlay-mark-word-known'
;;    Mark current word known.
;;  `dictionary-overlay-mark-word-unknown'
;;    Mark current word unknown.
;;  `dictionary-overlay-jump-out-of-overlay'
;;    Move cursor out of overlay.
;;  `dictionary-overlay-mark-word-smart'
;;    Smartly mark current word as known or unknow.
;;  `dictionary-overlay-mark-word-smart-reversely'
;;    Smartly mark current word as known or unknow, inverse version of the above.
;;  `dictionary-overlay-mark-buffer'
;;    Mark all words as known, except those in `unknownwords' list.
;;  `dictionary-overlay-mark-buffer-unknown'
;;    Mark all words as unknown, except those in `unknownwords' list.
;;  `dictionary-overlay-install'
;;    Install all python dependencies.
;;  `dictionary-overlay-install-google-translate'
;;    Install all google-translate dependencies.
;;  `dictionary-overlay-modify-translation'
;;    Modify current word's translation.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `dictionary-overlay-just-unknown-words'
;;    If t, show overlay for words in unknownwords list.
;;    default = t
;;  `dictionary-overlay-auto-jump-after'
;;    Auto jump after commands
;;    Options: 'mark-word-known, 'mark-word-unknown, 'render-buffer
;;    default = '()
;;  `dictionary-overlay-inhibit-keymap'
;;    If t, show overlay for words in unknownwords list.
;;    default = t
;;  `dictionary-overlay-position'
;;    If value is 'after, put translation after word
;;    If value is 'help-echo, show it when mouse over word
;;    default = 'after
;;  `dictonary-overlay-recenter-after-mark-and-jump'
;;    If t, recenter after mark or jump.
;;    default is nil
;;  `dictionary-overlay-user-data-directory'
;;    Place user data in Emacs directory.
;;    default = (locate-user-emacs-file "dictionary-overlay-data/")
;;  `dictionary-overlay-translation-format'
;;    Translation format
;;    default = "(%s)"
;;  `dictionary-overlay-crow-engine'
;;    Crow translate engine
;;    default = "google"

;;; Code:

(require 'websocket-bridge)

(defgroup dictionary-overlay
  ()
  "Dictionary overlay for words in buffers."
  :group 'applications)

(defface dictionary-overlay-unknownword nil
  "Face for dictionary-overlay unknown words."
  :group 'dictionary-overlay)

(defface dictionary-overlay-translation nil
  "Face for dictionary-overlay translations."
  :group 'dictionary-overlay)

(defvar dictionary-overlay-py-path
  (concat
   (file-name-directory load-file-name)
   "dictionary-overlay.py"))

(defvar dictionary-overlay-py-requirements-path
  (concat (file-name-directory load-file-name) "requirements.txt"))

(defvar-local dictionary-overlay-active-p nil
  "Check current buffer if active dictionary-overlay.")

(defvar-local dictionary-overlay-hash-table
    (make-hash-table :test 'equal)
  "Hash-table contains overlays for dictionary-overlay.
The key's format is begin:end:word:translation.")

(defvar-local dictionary-overlay-hash-table-keys
    '()
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

(defcustom dictionary-overlay-translation-format
  "(%s)"
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
set by 'dictionary-overlay-jump-direction'. For RENDER-BUFFER, if
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

(defcustom dictonary-overlay-recenter-after-mark-and-jump nil
  "Recenter after mark or jump."
  :group 'dictionary-overlay
  :type '(boolean))

(defvar dictionary-overlay-map
  (let ((map (make-sparse-keymap)))
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
  (websocket-bridge-app-start
   "dictionary-overlay"
   "python3"
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
                         (point)))

(defun websocket-bridge-call-word (func-name)
  "Call grammarly function on current word by FUNC-NAME."
  (let ((word (downcase (thing-at-point 'word))))
    (websocket-bridge-call "dictionary-overlay" func-name
                           (downcase (thing-at-point 'word)))
    (message word)))

(defun dictionary-overlay-render-buffer ()
  "Render current buffer."
  (interactive)
  (when (not (member "dictionary-overlay" websocket-bridge-app-list))
    (dictionary-overlay-start))
  (setq-local dictionary-overlay-active-p t)
  (dictionary-overlay-refresh-buffer)
  (when (member 'render-buffer dictionary-overlay-auto-jump-after)
    (dictionary-overlay-jump-next-unknown-word)))

(defun dictionary-overlay-toggle ()
  "Toggle current buffer."
  (interactive)
  (if dictionary-overlay-active-p
      (progn
        ;; reset all hash-table-keys and delete all overlays
        (setq-local dictionary-overlay-hash-table-keys '())
        (dictionary-overlay-refresh-overlays)
        (setq-local dictionary-overlay-active-p nil))
    (dictionary-overlay-render-buffer)))

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
    (progn
      (setq-local dictionary-overlay-hash-table-keys '())
      (websocket-bridge-call-buffer "render"))))

(defun dictionary-overlay-jump-next-unknown-word ()
  "Jump to next unknown word."
  (interactive)
  (websocket-bridge-call-buffer "jump_next_unknown_word")
  (setq-local dictionary-overlay-jump-direction 'next)
  (when dictonary-overlay-recenter-after-mark-and-jump
    (recenter)))

(defun dictionary-overlay-jump-prev-unknown-word ()
  "Jump to prev unknown word."
  (interactive)
  (websocket-bridge-call-buffer "jump_prev_unknown_word")
  (setq-local dictionary-overlay-jump-direction 'prev)
  (when dictonary-overlay-recenter-after-mark-and-jump
    (recenter)))

(defun dictionary-overlay-jump-first-unknown-word ()
  "Jump to first unknown word."
  (interactive)
  (goto-char
   (string-to-number
    (car (string-split
          (car (last dictionary-overlay-hash-table-keys)) ":" t)))))

(defun dictionary-overlay-jump-last-unknown-word ()
  "Jump to last unknown word."
  (interactive)
  (goto-char
   (string-to-number
    (car (string-split
          (car dictionary-overlay-hash-table-keys) ":" t)))))

(defun dictionary-overlay-jump-out-of-overlay ()
  "Jump out overlay so that we no longer in keymap.
Usually overlay keymap has a higher priority than local major
mode and minor mode key map. Jumping out of overlay facilitates the
usage of original keymap. The command name is subjected to change
depending on reliablity."
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
  (when dictonary-overlay-recenter-after-mark-and-jump
    (recenter)))

(defun dictionary-overlay-mark-word-unknown ()
  "Mark current word unknown."
  (interactive)
  (websocket-bridge-call-word "mark_word_unknown")
  (when (member 'mark-word-unknown dictionary-overlay-auto-jump-after)
    (pcase dictionary-overlay-jump-direction
      (`next (dictionary-overlay-jump-next-unknown-word))
      (`prev (dictionary-overlay-jump-prev-unknown-word))))
  (dictionary-overlay-refresh-buffer)
  (when dictonary-overlay-recenter-after-mark-and-jump
    (recenter)))

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

(defun dictionary-add-overlay-from (begin end source target)
  "Add a overlay with range BEGIN to END for the translation SOURCE to TARGET."
  (let ((ov (make-overlay begin end))
        (hash-table-key
         (format "%s:%s:%s:%s" begin end source target)))
    ;; record the overlay's key
    (add-to-list 'dictionary-overlay-hash-table-keys hash-table-key)
    (when (not (gethash hash-table-key dictionary-overlay-hash-table))
      ;; create an overly only when the key not exists
      (overlay-put ov 'face 'dictionary-overlay-unknownword)
      (pcase dictionary-overlay-position
        ('after
         (progn
           (overlay-put
            ov 'after-string
            (propertize
             (format dictionary-overlay-translation-format target)
             'face 'dictionary-overlay-translation))
           (overlay-put ov 'evaporate t)
           (unless dictionary-overlay-inhibit-keymap
             (overlay-put ov 'keymap dictionary-overlay-map))))
        ('help-echo
         (overlay-put
          ov 'help-echo
          (format dictionary-overlay-translation-format target))))
      (puthash hash-table-key ov dictionary-overlay-hash-table))))

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

(provide 'dictionary-overlay)
;;; dictionary-overlay.el ends here
