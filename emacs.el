;; Basic UI customization
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq inhibit-splash-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; give it a bit more space
(setq-default left-margin-width 4 right-margin-width 4)
(set-window-buffer nil (current-buffer))

;; center buffer in window
;; see https://stackoverflow.com/questions/24955253/centre-emacs-buffer-within-window
(defun my-resize-margins ()
  (let ((margin-size (/ (- (frame-width) 90) 2)))
    (if (< margin-size 0)
      (set-window-margins nil 0 0)
      (set-window-margins nil margin-size margin-size))))
(add-hook 'window-configuration-change-hook #'my-resize-margins)
(my-resize-margins)

;; Fonts and colors
(set-frame-font "Monaco-18" nil t)
(if (>= emacs-major-version 28)
  (load-theme 'modus-vivendi)
  (load-theme 'tango-dark))

;; Highlight column 80
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Fullscreen toggle
;; see https://www.emacswiki.org/emacs/FullScreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
)
(global-set-key (kbd "M-<return>") 'toggle-fullscreen)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package '(magit))
  (unless (package-installed-p package)
    (package-install package)))

;; MAC stuff
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; Magit
(require 'magit)
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))
(global-set-key (kbd "C-c g") 'magit)

;; ibuffer
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


;; Everything else
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(package-selected-packages '(ag markdown-mode magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:background "gray31" :underline "OrangeRed1")))))
