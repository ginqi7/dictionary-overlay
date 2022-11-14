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

;;; Code:

(require 'websocket-bridge)
(setq dictionary-overlay-py-path
      (concat
       (file-name-directory load-file-name)
       "dictionary-overlay.py"))

(defvar dictionary-overlay-just-unknown-words t)

(defun dictionary-overlay-start ()
  "Start dictionary overlay."
  (interactive)
  (websocket-bridge-app-start "dictionary-overlay" "python" dictionary-overlay-py-path))

(defun dictionary-overlay-restart ()
  "Restart websocket bridge grammarly and show process."
  (interactive)
  (websocket-bridge-app-exit "dictionary-overlay")
  (dictionary-overlay-start)
  (split-window-below)
  (websocket-bridge-app-open-buffer "dictionary-overlay")
  )


(defun websocket-bridge-call-buffer(func-name &optional word)
  "Call grammarly function on current buffer by FUNC-NAME."
  (websocket-bridge-call "dictionary-overlay" func-name
                         (buffer-string)
                         (point)
                         word))

(defun dictionary-overlay-render-buffer ()
  (interactive)
  (remove-overlays)
  (websocket-bridge-call-buffer "render"))


(defun dictionary-overlay-jump-next-unkown-word () (interactive)
       (websocket-bridge-call-buffer "jump_next_unkown_word"))

(defun dictionary-overlay-jump-prev-unkown-word () (interactive))

(defun dictionary-overlay-mark-word-know()
  (interactive)
  (let ((word (downcase (thing-at-point 'word t))))
    (websocket-bridge-call-buffer "mark_word_know" word))
  (dictionary-overlay-render-buffer)
  )

(defun dictionary-overlay-mark-word-unknow()
  (interactive)
  (let ((word (downcase (thing-at-point 'word t))))
    (websocket-bridge-call-buffer "mark_word_unknow" word))
  (dictionary-overlay-render-buffer)
  )

(defun dictionary-overlay-mark-buffer()
  (interactive)
  (websocket-bridge-call-buffer "mark_buffer")
  
  (dictionary-overlay-render-buffer)
  )


(defun dictionary-add-overlay-from(begin end word display)

  (let ((ov (make-overlay begin end)))
    (overlay-put ov 'display display)
    ))

(provide 'dictionary-overlay)
;;; dictionary-overlay.el ends here

