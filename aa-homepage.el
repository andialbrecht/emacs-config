(defun aa/homepage-generate ()
  "Creates homepage and blog files."
  (interactive)
  (org-publish-project "andi")
  (let (myBuf killBuf)
    (setq killBuf nil)
    (setq myBuf (get-file-buffer "~/web/org/2011.org"))
    (if (not myBuf)
	(progn
	  (setq myBuf (find-file "~/web/org/2011.org"))
	  (setq killBuf t))
      nil)
    (org-jekyll-export-blog)
    (if killBuf	(kill-buffer myBuf) nil)
    )
  ;;(shell-command "touch ~/web/org/index.org")
  ;;(shell-command "cd ~/web/jekyll && jekyll")
)
(defun aa/homepage-publish ()
  "Create and publish homepage."
  (interactive)
  (aa/homepage-generate)
  (aa/homepage-sync)
)
(defun aa/homepage-sync ()
  "Sync local homepage with remote."
  (interactive)
  (shell-command (concat "rsync -e ssh -q -avz -r --delete "
			 "/home/andi/web/jekyll/_site/ "
			 "andialbrecht.de:~/public_html/"))
  ;; TODO: Ping pubhub
)

(defvar aa-jekyll-server-process "jekyll-aa")
(defvar aa-jekyll-server-name "*jekyll-aa*")
(defvar aa-jekyll-bin "/var/lib/gems/1.8/bin/jekyll")

(defun aa-server-start ()
  (interactive)
  (if (not (equal (process-status aa-jekyll-server-process) 'run))
      (let ((default-directory "~/web/jekyll/"))
	(message "Starting jekyll server")
	(start-process aa-jekyll-server-process aa-jekyll-server-name
		       aa-jekyll-bin "--auto" "--lsi" "--serve")
	(set-process-query-on-exit-flag
	 (get-process aa-jekyll-server-process) nil)
	))
  (message "Homepage running on http://localhost:4000"))

(defun aa-server-stop ()
  (interactive)
  (if (equal (process-status aa-jekyll-server-process) 'run)
      (progn
	(delete-process aa-jekyll-server-process)
	(kill-buffer aa-jekyll-server-name))))

(provide 'aa-homepage)