;; Emacs startup file.

;; define dotfiles-dir where all the files live.
(setq dotfiles-dir (file-name-directory
		    (or load-file-name (buffer-file-name))))

;; Load up Org Mode and Babel
(require 'org-install)
(require 'ob-tangle)

;; load up the main file
(org-babel-load-file (expand-file-name "emacs.org" dotfiles-dir))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-cleanup-whitespace t)
 '(python-indent-guess-indent-offset nil)
 '(safe-local-variable-values (quote ((ispell-dictionary "german") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/lfea/bin/python") (TeX-master . "master") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/lf30ea/bin/python" :settings "settings") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/babbel/bin/python" :settings "settings") (virtualenv-default-directory . "") (virtualenv-workon . "dlgi") (pony-settings make-pony-project :python "/home/andi/.virtualenvs/lowfett/bin/python" :settings "settings"))))
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
