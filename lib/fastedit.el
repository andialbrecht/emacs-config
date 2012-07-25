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

;; Insert line before [Ctrl-Shift-Return]
;; Join line [Strg-j]
;; Join line bewow [Strg-Shift-j]
;; Break line [Alt-j]
;; Swap line down [Alt-Down]
;; Swap line up [Alt-Up]
;; Comment or uncomment region or line
;; Kill "word" backwards (Python mode) [M-Delete]

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
;; Based on http://stackoverflow.com/questions/8551320/highlighting-todos-in-all-programming-modes
(defun highlight-todos (font-lock-add-keywords nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\|XXX\\)" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'highlight-todos)

(provide 'fastedit)
;;; fastedit.el ends here
