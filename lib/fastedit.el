;;; fastedit.el --- My personal editing shortcuts

;; Copyright (C) 2012  Andi Albrecht

;; Author: Andi Albrecht <andi@lap-pu09>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Requires:
;;   thing-cmd.el (http://www.emacswiki.org/emacs/thing-cmds.el)
;;   - hide-comnt.el (http://www.emacswiki.org/emacs/HideOrIgnoreComments)
;;
;; Functions:
;;   Insert line before [Ctrl-Shift-Return]
;;   Join line [Strg-j]
;;   Join line bewow [Strg-Shift-j]
;;   Break line [Alt-j]
;;   Swap line down [Alt-Down]
;;   Swap line up [Alt-Up]
;;   Comment or uncomment region or line
;;   Kill "word" backwards (Python mode) [M-Delete]
;;   Mark thing at point (Ctrl-c f m)
;;   Move region/line up/down [M-up/-down]

;;

;;; Code:

(defun insert-line-before()
  "Insert a new line before current line and indent."
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-according-to-mode)
)
(global-set-key (kbd "C-S-<return>") 'insert-line-before)


(defun join-line-below()
  "Like join-line, but joins the line below."
  (interactive)
  (join-line 1))

;; alt-j breaks lines
(global-set-key (kbd "C-j") 'join-line)
(global-set-key (kbd "C-S-j") 'join-line-below)
;; this is py-newline-and-indent in python-mode
(add-hook 'python-mode-hook '(lambda ()
             (define-key py-mode-map (kbd "C-j") 'join-line)
             (define-key py-mode-map (kbd "C-S-j") 'join-line-below)))


;; move-line is taken from here:
;; http://www.emacswiki.org/emacs/MoveLine
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position)))
)


(defun aa/py-backward-kill-token()
  "Kill token left from cursor."
  (interactive)
  (delete-region (point) (progn
                           (re-search-backward "\[ )\] ")
                           (forward-char 1)
                           (point))))

(add-hook 'python-mode-hook '(lambda ()
             (define-key py-mode-map (kbd "M-<delete>")
         'aa/py-backward-kill-token)))


;; Distraction free follows here

;; Stolen from http://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
(defun toggle-mode-line ()
  "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

(defun distraction-free-toggle-fringes()
  (interactive)
  (if (equal (car (window-fringes)) 0)
      (set-window-fringes nil nil nil)
    (set-window-fringes nil 0 0))
)

(defun distraction-free-enable()
  (interactive)
  (toggle-mode-line)
  (distraction-free-toggle-fringes)
  (set-window-margins (selected-window) 20 20)
)

(defun distraction-free-disable()
  (interactive)
  (toggle-mode-line)
  (distraction-free-toggle-fringes)
  (set-window-margins (selected-window) 0 0)
)

(defun distraction-free-simple()
  (interactive)
  (if (equal (window-margins (selected-window)) '(nil))
      ;; enable it
      (distraction-free-enable)
    (distraction-free-disable))
)

(defun distraction-free()
  (interactive)
  (switch-full-screen)
  (distraction-free-simple))
(global-set-key (kbd "<f8> d") 'distraction-free)
(global-set-key (kbd "<f8> D") 'distraction-free-simple)

;; Highlight TODOs
;; Grabbed from Emacs Starter Kit https://github.com/dgutov/emacs-starter-kit/blob/0969080c73323bc0b872e385107d7337a845ab4d/starter-kit-defuns.el
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\|XXX\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'python-mode-hook 'esk-add-watchwords)
(add-hook 'text-mode-hook 'esk-add-watchwords)


;; Mark thing at point
;; The code is copied from here: http://emacswiki.org/emacs/MarkCommands
(autoload 'mark-thing "thing-cmds")
(defun mark-a-word-or-thing (arg)
  "Select word on or before current point, and move point to beginning of word.

   With a prefix ARG, first prompts for type of things and select ARG
   things but you need to move the point to the beginnig of thing
   first.

   But if a thing has been selected, then extend the selection by one
   thing on the other side of the point.  (So to select backwards,
   select to the right first.)"
  (interactive "P")
  (if (or arg mark-active)
      (call-interactively 'mark-thing)
    (skip-syntax-backward "w_")
    (mark-thing 'word)))
(global-set-key (kbd "C-c f m") 'mark-a-word-or-thing)

;; Move line or region up or down
;; Grabbed von SO http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)


;; see http://emacsrocks.com/e12.html
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))
;; ^^^


;; see http://emacsrocks.com/e12.html
;; requires mark-multi available from ELPA
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))
;; ^^^


(defun dired-cleanupdir ()
  "Clean up artifacts in current directory."
  (interactive)
  (shell-command "find . -name \"*.pyc\" -delete")
  (shell-command "find . -name \"*~\" -delete")
  (revert-buffer)
)
(define-key dired-mode-map (kbd "C-c C-c") 'dired-cleanupdir)




(provide 'fastedit)
;;; fastedit.el ends here
