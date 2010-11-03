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
 '(column-marker-1 ((t (:overline "red" :underline "red"))))
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
 '(jabber-chat-prompt-foreign ((t (:foreground "lightskyblue" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "grey50" :weight bold))))
 '(jabber-roster-user-away ((t (:foreground "gray40" :slant italic :weight normal))))
 '(jabber-roster-user-online ((t (:foreground "yellowgreen" :slant normal :weight bold))))
 '(jabber-title-large ((t (:inherit variable-pitch :weight bold :height 1.2 :width ultra-expanded))))
 '(jabber-title-medium ((t (:inherit variable-pitch :weight bold :height 1.2 :width expanded))))
 '(magit-diff-add ((nil (:foreground "green"))))
 '(magit-item-highlight ((nil (:background "grey10"))))
 '(mm-uu-extract ((((type tty) (class color) (background dark)) (:background "grey20"))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :slant italic :weight bold))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "color-102" :weight bold))) t)
 '(org-agenda-done ((t (:foreground "grey50" :weight normal))))
 '(org-agenda-structure ((((class color) (min-colors 88) (background light)) (:foreground "white"))))
 '(org-date ((((class color) (background dark)) (:foreground "lightseagreen" :underline t))))
 '(org-document-info ((((class color) (background light)) (:foreground "brightblue"))))
 '(org-document-title ((t (:foreground "white" :weight semi-bold :height 1.22 :family "URW Bookman L"))))
 '(org-hide ((t (:foreground "#333333"))))
 '(org-level-1 ((t (:inherit outline-1 :slant normal :family "URW Bookman L"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "tan3" :weight semi-bold :family "URW Bookman L"))))
 '(org-level-4 ((t (:foreground "yellow green"))))
 '(org-scheduled ((((class color) (min-colors 88) (background dark)) (:foreground "#73d216"))))
 '(org-scheduled-today ((((class color) (min-colors 88) (background dark)) (:foreground "#73d216"))))
 '(org-table ((((class color) (min-colors 88) (background light)) (:foreground "white"))))
 '(pycomplexity-complexity-high ((t (:background "#660000" :foreground "#660000"))))
 '(pycomplexity-complexity-low ((t (:background "#2A4C08" :foreground "#2A4C08"))))
 '(pycomplexity-complexity-normal ((t (:background "#7f7200" :foreground "#7f7200"))))
 '(rst-level-1-face ((t (:background "black" :weight bold))) t)
 '(rst-level-2-face ((t (:background "black" :foreground "lightskyblue" :weight bold))) t)
 '(rst-level-3-face ((t (:background "black" :foreground "white" :weight bold))) t)
 '(variable-pitch ((t (:family "URW Bookman L"))))
 '(w3m-anchor ((((class color) (background dark)) (:foreground "LightSkyBlue"))))
 '(w3m-form ((((class color) (background dark)) (:foreground "chocolate4" :underline t))))
 '(window-number-face ((((type x w32 mac)) (:foreground "lightblue")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "f015ffc50e20f5ed0abfa805a29a25a7164e790b")
 '(ecb-source-path (quote ("/home/andi/devel/avsam" "/home/andi/devel/emacs-config")))
 '(pascal-auto-lineup nil)
 '(pascal-indent-level 2)
 '(pascal-indent-nested-functions nil)
 '(pascal-tab-always-indent nil)
 '(safe-local-variable-values (quote ((virtualenv-default-directory . "/home/andi/devel/avsam/applikation") (virtualenv-workon . "avsweb") (virtualenv-default-directory . "/home/andi/devel/proto_puforms/src") (virtualenv-workon . "protopuf") (show-trailing-whitespace . t) (org-export-latex-image-default-option . "width=30em"))))
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-cookie-accept-domains (quote ("localhost" "localhost:8080")))
 '(w3m-home-page "file:///home/andi/.w3m/bookmark.html")
 '(w3m-use-cookies t t))

