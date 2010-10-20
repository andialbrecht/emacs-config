;;; sunrise-commander-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (sr-dired sunrise-cd sunrise) "sunrise-commander"
;;;;;;  "sunrise-commander.el" (19646 52114))
;;; Generated autoloads from sunrise-commander.el

(autoload 'sunrise "sunrise-commander" "\
Starts the Sunrise Commander. If the param `left-directory' is given the left
  window  will  display  this  directory  (the  same   for   `right-directory').
  Specifying nil for any of these values uses the default, ie. home.

\(fn &optional LEFT-DIRECTORY RIGHT-DIRECTORY FILENAME)" t nil)

(autoload 'sunrise-cd "sunrise-commander" "\
Run Sunrise but give it the current directory to use.

\(fn)" t nil)

(autoload 'sr-dired "sunrise-commander" "\
Visits the given directory in sr-mode.

\(fn DIRECTORY &optional SWITCHES)" t nil)

;;;***

;;;### (autoloads nil nil ("sunrise-commander-pkg.el") (19646 52114
;;;;;;  222099))

;;;***

(provide 'sunrise-commander-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sunrise-commander-autoloads.el ends here
