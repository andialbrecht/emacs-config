;;; python-flake8.el --- Thin wrapper around python-pylint to run flake8

;; Copyright (C) 2011  Andi Albrecht

;; Author: Andi Albrecht <albrecht.andi@gmail.com>
;; Keywords: languages

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

;; This is a thin wrapper around python-pylint
;; (https://gist.github.com/302848) to run flake8.

;; Sometimes I want to run pylint and sometimes flake8. This simple
;; wrapper just re-uses python-pylint's compilation mode and
;; configures it to use flake8 and parses it's output.

;;; Code:


(defgroup python-flake8 nil
  "Wrapper around python-pylint for running flake8"
  :prefix "python-flake8-"
  :group 'tools)

(defconst python-flake8-regexp-alist
  (let* ((basepre "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\s+")
         (base (concat basepre "\\(%s.*\\)$")))
    (list
     (list (format base "[FE]") 1 2 3)
     (list (format base "[RWC]") 1 2 3 1)
     (list (concat basepre ".* is too complex ([0-9]+)$") 1 2)))
  "Regexp used to match PYLINT hits.  See `compilation-error-regexp-alist'.")

(defcustom python-flake8-options '()
  "Options to pass to flake8."
  :type '(repeat string)
  :group 'python-flake8)

(defcustom python-flake8-command "flake8"
  "flake8 command."
  :type '(file)
  :group 'python-flake8)

(defun python-flake8 ()
  "Utilize python-pylint to run flake8 instead."
  (interactive)
  (let* ((python-pylint-command python-flake8-command)
         (python-pylint-options python-flake8-options)
         (python-pylint-regexp-alist python-flake8-regexp-alist))
    (python-pylint)))


(defalias 'flake8 'python-flake8)

(provide 'python-flake8)
;;; python-flake8.el ends here