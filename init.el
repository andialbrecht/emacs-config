;; Emacs startup file.

;; define dotfiles-dir where all the files live.
(setq dotfiles-dir (file-name-directory
		    (or load-file-name (buffer-file-name))))

;; Load up Org Mode and Babel
(require 'org)
(require 'ob-tangle)

;; load up the main file
(org-babel-load-file (expand-file-name "emacs.org" dotfiles-dir))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-executable "ack-grep" t)
 '(css-indent-offset 2)
 '(custom-safe-themes (quote ("50c913c6fafd0ba842d9777f1b97c193187a74c792b86c7b38d295e69257c94c" default)))
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\..*/")))
 '(ispell-dictionary "american" t)
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t)
 '(js2-global-externs (list "$"))
 '(py-electric-comment-p nil)
 '(py-indent-honors-inline-comment t)
 '(python-indent-guess-indent-offset nil)
 '(safe-local-variable-values (quote ((py-indent-offset . 4) (ispell-dictionary . "german") (ispell-dictionary "german") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/lfea/bin/python") (TeX-master . "master") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/lf30ea/bin/python" :settings "settings") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/babbel/bin/python" :settings "settings") (virtualenv-default-directory . "") (virtualenv-workon . "dlgi") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/lowfett/bin/python" :settings "settings"))))
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-match ((t (:background "black" :foreground "gold" :underline (:color foreground-color :style line) :weight bold))) t)
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height 1.0))) t)
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :height 1.0))) t)
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :height 1.0))) t)
 '(font-lock-warning-face ((t (:inherit error :box (:line-width 1 :color "dark red")))))
 '(highlight ((t (:background "gray14"))))
 '(hl-line ((t (:inherit highlight :weight bold))))
 '(monky-diff-add ((t (:foreground "olive drab"))))
 '(org-todo ((t (:foreground "red" :weight bold))))
 '(persp-selected-face ((t (:foreground "olive drab" :weight bold))) t)
 '(rst-level-1-face ((t (:weight bold))) t)
 '(rst-level-2-face ((t (:weight bold))) t)
 '(variable-pitch ((t (:family "Ubuntu Mono")))))
