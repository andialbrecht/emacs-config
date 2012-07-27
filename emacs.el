
(setq aa-libfiles-dir (expand-file-name "lib" dotfiles-dir))
(setq aa-vendor-dir (expand-file-name "vendor" dotfiles-dir))
(add-to-list 'load-path aa-libfiles-dir)
(add-to-list 'load-path aa-vendor-dir)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Remove unused UI elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)

;; shhht, give me some time to think, don't blink
(blink-cursor-mode 0)

;; show matching parens
(show-paren-mode 1)

;; always show column numbers
(column-number-mode 1)

;; Narrow (C-x n n)
(put 'narrow-to-region 'disabled nil)

;; take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(condition-case nil
    (set-default-font "Ubuntu Mono 16")
  (error (condition-case nil
             (set-default-font "Cousine")
           (error (condition-case nil
                      (set-default-font "Monaco")
                    (error nil))))))

(cond
 ((= 24 emacs-major-version)
  ;; eval lighty directly, adding it to command-switch-alist will result
  ;; in a later evaluation.
  (add-to-list 'custom-theme-load-path aa-libfiles-dir)
  (if (member "-lighty" command-line-args)
      (progn
        (setq command-line-args (delete "-lighty" command-line-args))
        (load-theme 'solarized-light t)
        (set-default-font "Ubuntu Mono 28"))
    (load-theme 'candy t))
  ))

(defun aa/window-set-size-internal (is-width &optional reqsize)
  "Prompts for window size (in columns) and adjusts buffer accordingly."
  (if is-width
      (progn
        (setq size (window-width))
        (setq prompt "Width: "))
    (progn
      (setq size (window-height))
      (setq prompt "Height: ")))
  (if (eq reqsize nil)
      (setq reqsize (string-to-int
                     (read-from-minibuffer prompt (format "%d" size)))))
  (if (> reqsize size)
      (enlarge-window (- reqsize size) is-width)
    (shrink-window (- size reqsize) is-width)))

(defun aa/window-set-width ()
  "Set window width."
  (interactive)
  (aa/window-set-size-internal t))

(defun aa/window-set-height ()
  "Set window height."
  (interactive)
  (aa/window-set-size-internal nil))

(defun aa/make80 ()
  "Make the current window 80 chars wide."
  (interactive)
  (aa/window-set-size-internal t 80))

(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

;;(require 'perspective)
;;(persp-mode)

(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
  (quote (("default"
    ("Programming" ;; prog stuff not already in MyProjectX
     (or
      (mode . c-mode)
      (mode . c++-mode)
      (mode . python-mode)
      (mode . emacs-lisp-mode)
      (mode . lisp-mode)
      (mode . sql-mode)
      (mode . html-mode)
      (mode . js2-mode)
      (mode . pascal-mode)
      (mode . makefile-gmake-mode)
      (mode . nxml-mode)
      (mode . yaml-mode)
      (mode . sh-mode)
      (mode . rst-mode)
      (mode . go-mode)
      (mode . po-mode)
      ;; etc
      ))
    ("Dired"
     (or
      (mode . dired-mode)))
    ("Version Control"
     (or
      (mode . magit-mode)
      (name . "^*magit")
      (mode . ahg-status-mode)))
    ("Org" ;; all org-related buffers
     (or
      (mode . org-mode)
      (mode . org-agenda-mode)
      (mode . diary-mode)
      (mode . calendar-mode)
      (name . "^*Fancy Diary")
      ))
    ("Emacs"
     (or
      (name . "^\\*scratch\\*$")
      (name . "^\\*Messages\\*$")
      (name . "^\\*ielm\\*$")
      (mode . help-mode)))
    ))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

(require 'yasnippet)
(yas/global-mode 1)

(yas/load-directory (expand-file-name "snippets" dotfiles-dir))

(defun yas/org-very-safe-expand ()
                 (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'tramp)
(setq tramp-default-method "ssh")

(require 'org)
(require 'org-src)  ;; edit src inline
(require 'htmlize)  ;; required for export

(add-hook 'org-mode-hook
         (lambda ()
            (toggle-truncate-lines)))

(define-key org-mode-map (kbd "C-c #") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c #") 'org-edit-src-exit)

(require 'fastedit)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Only use tab-width of 2 for certain modes.
(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq-default tab-width 2))))
      '(js2-mode-hook
        js-mode-hook
        css-mode-hook
))

(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (setq show-trailing-whitespace t))))
      '(text-mode-hook
        emacs-lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        css-mode-hook
        ))

(require 'column-marker)
(mapc (lambda (hook)
        (add-hook hook (lambda () (interactive) (column-marker-1 80))))
      '(org-mode-hook
        emacs-lisp-mode-hook
        python-mode-hook
        js2-mode-hook
        text-mode-hook))

(require 'highlight-symbol)
(global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-prev)

(require 'python)
(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(defvar py-mode-map python-mode-map)
(add-hook 'python-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'python-imenu-create-index)))

(require 'flymake)
(setq flymake-no-changes-timeout 3)

(when (load "flymake" t)
  (load "flymake-cursor")
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      ;; uncomment either flake8 oder pyflakes
      ;; (list "flake8" (list local-file))
      (list "pyflakes" (list local-file))
      ))
  (add-to-list 'flymake-allowed-file-name-masks
               '("devel.+\\.py$" flymake-pyflakes-init)))

(add-hook 'python-mode-hook
          (lambda ()
           ; Activate flymake unless buffer is a tmp buffer for the interpreter
            (if (not (eq buffer-file-name nil))
                (progn
                  (flymake-mode t)
                  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
                  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)))))

(add-to-list 'load-path (expand-file-name "pycomplexity" aa-vendor-dir))
(require 'linum)
(require 'pycomplexity)
(setq pycomplexity-python "python")
(add-hook 'python-mode-hook
          (function (lambda ()
                      (pycomplexity-mode)
                      (linum-mode))))

(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import i?pdb")
  (highlight-lines-matching-regexp "i?pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(require 'ipython)

(require 'pony-mode)

(require 'po-mode)
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)

(require 'magit)
(require 'magit-svn)

(add-to-list 'load-path (expand-file-name "monky" aa-vendor-dir))
(require 'monky)
(setq monky-process-type 'cmdserver)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
;; on Debian/Ubuntu you'll need to set the executable
(setq ack-executable (executable-find "ack-grep"))

(autoload 'simple-rtm-mode "simple-rtm" "Interactive mode for Remember The Milk" t)
(eval-after-load 'simple-rtm
  '(progn
     (display-simple-rtm-tasks-mode t)))

(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 300)
(setq twittering-url-show-status nil)

(global-set-key (kbd "<f12>") 'persp-switch-quick)
(global-set-key (kbd "C-<f12>") 'persp-switch)
(global-set-key (kbd "M-<f12>") 'aa/make80)

(global-set-key (kbd "<f8> w") 'whitespace-mode)
(global-set-key (kbd "<f8> c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<f8> r") 'replace-string)
(global-set-key (kbd "<f8> R") 'replace-regexp)
(global-set-key (kbd "<f8> a") 'ack)
(global-set-key (kbd "<f8> A") 'ack-same)
(global-set-key (kbd "<f8> g") 'imenu)
(global-set-key (kbd "<f8> p l") 'python-pylint)
(global-set-key (kbd "<f8> p f") 'python-flake8)

(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c h") 'monky-status)
