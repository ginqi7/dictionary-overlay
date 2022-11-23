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
;;  `dictionary-overlay-mark-word-known'
;;    Mark current word known.
;;  `dictionary-overlay-mark-word-unknown'
;;    Mark current word unknown.
;;  `dictionary-overlay-mark-buffer'
;;    Mark all words as known, except those in `unknownwords' list.
;;  `dictionary-overlay-mark-buffer-unknown'
;;    Mark all words as unknown, except those in `unknownwords' list.
;;  `dictionary-overlay-install'
;;    Install all python dependencies.
;;  `dictionary-overlay-install-google-translate'
;;    Install all google-translate.
;;  `dictionary-overlay-modify-translation'
;;    Modify current word's translation.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'websocket-bridge)


(defgroup dictionary-overlay ()
  "Dictionary overlay for words in buffers."
  :group 'applications)

(defface dictionary-overlay-unknownwords '((t (:height 0.6 :inherit shadow)))
  "Face for dictionary-overlay unknown words."
  :group 'dictionary-overlay)

(defvar dictionary-overlay-py-path
  (concat (file-name-directory load-file-name)
          "dictionary-overlay.py"))

(defvar dictionary-overlay-py-requirements-path
  (concat (file-name-directory load-file-name)
          "requirements.txt"))

(defvar-local dictionary-overlay-active-p nil
  "Check current buffer if active dictionary-overlay.")

(defcustom dictionary-overlay-just-unknown-words t
  "If t, show overlay for words in unknownwords list.
If nil, show overlay for words not in knownwords list."
  :group 'dictionary-overlay
  :type 'boolean)

(defcustom dictionary-overlay-user-data-directory
  (locate-user-emacs-file "dictionary-overlay-data/")
  "Place user data in Emacs directory."
  :group 'dictionary-overlay
  :type 'directory)

(defun dictionary-overlay-start ()
  "Start dictionary-overlay."
  (interactive)
  (websocket-bridge-app-start
   "dictionary-overlay"
   "python3"
   dictionary-overlay-py-path))

(defun dictionary-overlay-restart ()
  "Restart dictionary-overlay and show process."
  (interactive)
  (websocket-bridge-app-exit "dictionary-overlay")
  (dictionary-overlay-start)
  (split-window-below)
  (websocket-bridge-app-open-buffer "dictionary-overlay"))


(defun websocket-bridge-call-buffer(func-name)
  "Call grammarly function on current buffer by FUNC-NAME."
  (websocket-bridge-call "dictionary-overlay" func-name
                         (buffer-string)
                         (point)))

(defun websocket-bridge-call-word(func-name)
  "Call grammarly function on current word by FUNC-NAME."
  (websocket-bridge-call "dictionary-overlay" func-name
                         (downcase (thing-at-point 'word))))


(defun dictionary-overlay-render-buffer ()
  "Render current buffer."
  (interactive)
  (when (not (member "dictionary-overlay" websocket-bridge-app-list))
    (dictionary-overlay-start))
  (setq-local dictionary-overlay-active-p t)
  (dictionary-overlay-refresh-buffer))

(defun dictionary-overlay-toggle ()
  "Toggle current buffer."
  (interactive)
  (if dictionary-overlay-active-p
      (progn
        (remove-overlays)
        (setq-local dictionary-overlay-active-p nil))
    (dictionary-overlay-render-buffer)))

(defun dictionary-overlay-refresh-buffer ()
  "Refresh current buffer."
  (when dictionary-overlay-active-p
    (remove-overlays)
    (websocket-bridge-call-buffer "render")))

(defun dictionary-overlay-jump-next-unknown-word ()
  "Jump to next unknown word."
  (interactive)
  (websocket-bridge-call-buffer "jump_next_unknown_word"))

(defun dictionary-overlay-jump-prev-unknown-word ()
  "Jump to prev unknown word."
  (interactive)
  (websocket-bridge-call-buffer "jump_prev_unknown_word"))

(defun dictionary-overlay-mark-word-known()
  "Mark current word known."
  (interactive)
  (websocket-bridge-call-word "mark_word_known")
  (dictionary-overlay-refresh-buffer))

(defun dictionary-overlay-mark-word-unknown ()
  "Mark current word unknown."
  (interactive)
  (websocket-bridge-call-word "mark_word_unknown")
  (dictionary-overlay-refresh-buffer))

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

(defun dictionary-overlay-add-simple (begin end text translation)
  "Add simple overlay with range BEGIN to END for TEXT TRANSLATION."
  (let ((ov (make-overlay end (+ end 1))))
    (overlay-put ov 'display (format " %s " translation))
    (overlay-put ov 'face 'dictionary-overlay-unknownwords)))

(defun dictionary-overlay-add-emphasis (begin end text translation)
  "Add emphasis overlays with range BEGIN to END for TEXT TRANSLATION."
  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'display text)
    (overlay-put ov 'face 'font-lock-keyword-face))
  (let ((ov (make-overlay end (+ end 1))))
    (overlay-put ov 'display (format "(%s)" translation))
    (overlay-put ov 'face 'font-lock-comment-face)))

(defvar dictionary-overlay-add-function #'dictionary-overlay-add-simple "Add overlay function.")

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
  (let ((translation (completing-read
                      "Choose or input translation: " candidates)))
    (websocket-bridge-call "dictionary-overlay"
                           "update_translation"
                           word
                           translation))
  (dictionary-overlay-render-buffer))

(provide 'dictionary-overlay)
;;; dictionary-overlay.el ends here
