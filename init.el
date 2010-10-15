;;; init.el --- Where all the magic begins
;;
;; This file is copied from Emacs Starter Kit
;; (http://github.com/eschulte/emacs-starter-kit) and adjusted to fit
;; my needs.
;;
;; This is the first thing to get loaded.
;;

(setq dotfiles-dir (file-name-directory 
		    (or load-file-name (buffer-file-name))))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org-mode" (expand-file-name
					     "src" dotfiles-dir))))
;; Load up Org Mode and Babel
(require 'org-install)
(require 'ob-tangle)

;; load up the main file
(org-babel-load-file (expand-file-name "emacs.org" dotfiles-dir))

;;; init.el ends here
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gnus-cite-1 ((((class color) (background light)) (:foreground "slategray"))))
 '(gnus-group-mail-3 ((t (:foreground "yellowgreen" :weight bold))))
 '(gnus-group-mail-3-empty ((((class color) (background dark)) (:foreground "grey60"))))
 '(gnus-group-mail-low-empty ((((class color) (background dark)) (:foreground "grey60"))))
 '(gnus-group-news-3 ((t (:foreground "lightskyblue2" :weight bold))))
 '(gnus-group-news-3-empty ((((class color) (background dark)) (:foreground "grey60"))))
 '(gnus-header-name ((((class color) (background dark)) (:foreground "olivedrab"))))
 '(gnus-header-subject ((t (:foreground "#edd400" :weight bold))))
 '(gnus-summary-cancelled ((((class color)) (:foreground "grey40"))))
 '(gnus-summary-normal-ancient ((((class color) (background dark)) (:foreground "grey60"))))
 '(hl-line ((t (:inherit highlight :background "grey20"))))
 '(magit-diff-add ((nil (:foreground "green"))))
 '(magit-item-highlight ((nil (:background "grey10"))))
 '(mm-uu-extract ((((type tty) (class color) (background dark)) (:background "grey20"))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :slant italic :weight bold))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "color-102" :weight bold))) t)
 '(org-agenda-done ((t (:foreground "grey50" :weight normal))))
 '(org-agenda-structure ((((class color) (min-colors 88) (background light)) (:foreground "white"))))
 '(org-document-info ((((class color) (background light)) (:foreground "brightblue"))))
 '(org-document-title ((((class color) (background light)) (:foreground "brightblue" :weight bold :height 1.44))))
 '(org-hide ((t (:foreground "#333333"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "green" :weight semi-bold))))
 '(org-table ((((class color) (min-colors 88) (background light)) (:foreground "white")))))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(canlock-password "f015ffc50e20f5ed0abfa805a29a25a7164e790b")
 '(safe-local-variable-values (quote ((org-export-latex-image-default-option . "width=30em"))))
 '(w3m-home-page "file:///home/andi/.w3m/bookmark.html"))

