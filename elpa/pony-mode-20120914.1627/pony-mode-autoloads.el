;;; pony-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pony-mode-disable pony-load-snippets pony-test-hl-files
;;;;;;  pony-test-down pony-test-up pony-test-goto-err pony-test-open
;;;;;;  pony-test pony-tags pony-south-initial pony-south-migrate
;;;;;;  pony-south-schemamigration pony-south-convert pony-syncdb
;;;;;;  pony-startapp pony-shell pony-browser pony-temp-server pony-restart-server
;;;;;;  pony-stopserver pony-runserver pony-loaddata pony-dumpdata
;;;;;;  pony-flush pony-manage pony-goto-settings pony-resolve pony-goto-template
;;;;;;  pony-fabric-deploy pony-fabric pony-fabric-run pony-fabric-list-commands
;;;;;;  pony-fabric-p pony-db-shell pony-buildout-bin pony-buildout
;;;;;;  pony-buildout-list-bin pony-buildout-cmd pony-setting pony-get-setting
;;;;;;  pony-setting-p pony-get-settings-file-basename pony-command-if-exists
;;;;;;  pony-reload-mode pony-read-file) "pony-mode" "pony-mode.el"
;;;;;;  (20582 35739))
;;; Generated autoloads from pony-mode.el

(autoload 'pony-read-file "pony-mode" "\
Read the contents of `filepath'

\(fn FILEPATH)" nil nil)

(autoload 'pony-reload-mode "pony-mode" "\


\(fn)" t nil)

(autoload 'pony-command-if-exists "pony-mode" "\
Run `command` if it exists

\(fn PROC-NAME COMMAND ARGS)" nil nil)

(autoload 'pony-get-settings-file-basename "pony-mode" "\
Return the name of the settings file to use for this
project. By default this is 'settings', but it can be changed
locally with .dir-locals.el.

\(fn)" nil nil)

(autoload 'pony-setting-p "pony-mode" "\
Predicate to determine whether a `setting' exists for the current project

\(fn SETTING)" nil nil)

(autoload 'pony-get-setting "pony-mode" "\
Get the pony settings.py value for `setting`

\(fn SETTING)" nil nil)

(autoload 'pony-setting "pony-mode" "\
Interactively display a setting value in the minibuffer

\(fn)" t nil)

(autoload 'pony-buildout-cmd "pony-mode" "\
Return the buildout command or nil if we're not in a buildout

\(fn)" nil nil)

(autoload 'pony-buildout-list-bin "pony-mode" "\
List the commands available in the buildout bin dir

\(fn)" nil nil)

(autoload 'pony-buildout "pony-mode" "\
Run buildout again on the current project

\(fn)" t nil)

(autoload 'pony-buildout-bin "pony-mode" "\
Run a script from the buildout bin/ dir

\(fn)" t nil)

(autoload 'pony-db-shell "pony-mode" "\
Run interpreter for this project's default database as an inferior process.

\(fn)" t nil)

(autoload 'pony-fabric-p "pony-mode" "\
Is this project using fabric?

\(fn)" nil nil)

(autoload 'pony-fabric-list-commands "pony-mode" "\
List of all fabric commands for project as strings

\(fn)" nil nil)

(autoload 'pony-fabric-run "pony-mode" "\
Run fabric command

\(fn CMD)" nil nil)

(autoload 'pony-fabric "pony-mode" "\
Run a fabric command

\(fn)" t nil)

(autoload 'pony-fabric-deploy "pony-mode" "\
Deploy project with fab deploy

\(fn)" t nil)

(autoload 'pony-goto-template "pony-mode" "\
Jump-to-template-at-point

\(fn)" t nil)
?

(autoload 'pony-resolve "pony-mode" "\
Jump to the view file that URL resolves to

This feature is somewhat experimental and known to break in some cases.

Bug reports welcome. Patches even more so :)

\(fn URL)" t nil)

(autoload 'pony-goto-settings "pony-mode" "\


\(fn)" t nil)

(autoload 'pony-manage "pony-mode" "\
Interactively call the pony manage command.

Second string that is read from minibuffer may be an actual
list of space separated arguments for the previously chosen management
command. If some of the arguments contain space itself they should be quoted
with double quotes like \"...\".

\(fn)" t nil)

(autoload 'pony-flush "pony-mode" "\
Flush the app

\(fn)" t nil)

(autoload 'pony-dumpdata "pony-mode" "\
Dumpdata to json

\(fn)" t nil)

(autoload 'pony-loaddata "pony-mode" "\
Load a fixture into the current project's dev database

\(fn)" t nil)

(autoload 'pony-runserver "pony-mode" "\
Start the Django development server.

If the server is currently running, just switch to the buffer.

If you are currently in the *ponyserver* buffer, restart the server

\(fn)" t nil)

(autoload 'pony-stopserver "pony-mode" "\
Stop the dev server

\(fn)" t nil)

(autoload 'pony-restart-server "pony-mode" "\
Restart the pony Django dev server.
Django extras does this better with the Werkzeug server, but sometimes
you can't have nice things.

\(fn)" t nil)

(autoload 'pony-temp-server "pony-mode" "\
Relatively regularly during development, I need/want to set up a development
server instance either on a nonstandard (or second) port, or that will be accessible
to the outside world for some reason. Meanwhile, i don't want to set my default host to 0.0.0.0
This function allows you to run a server with a 'throwaway' host:port

\(fn)" t nil)

(autoload 'pony-browser "pony-mode" "\
Open a tab at the development server

\(fn)" t nil)

(autoload 'pony-shell "pony-mode" "\
Open a Python shell with the current pony project's context loaded.

If the project has the django_extras package installed, then use the excellent
`shell_plus' command. Otherwise, fall back to manage.py shell 

\(fn)" t nil)

(autoload 'pony-startapp "pony-mode" "\
Run the pony startapp command

\(fn)" t nil)

(autoload 'pony-syncdb "pony-mode" "\
Run Syncdb on the current project

\(fn)" t nil)

(autoload 'pony-south-convert "pony-mode" "\
Convert an existing app to south

\(fn)" t nil)

(autoload 'pony-south-schemamigration "pony-mode" "\
Create migration for modification

\(fn)" t nil)

(autoload 'pony-south-migrate "pony-mode" "\
Migrate app

\(fn)" t nil)

(autoload 'pony-south-initial "pony-mode" "\
Run the initial south migration for an app

\(fn)" t nil)

(autoload 'pony-tags "pony-mode" "\
Generate new tags table

\(fn)" t nil)

(autoload 'pony-test "pony-mode" "\
Run the test(s) given by `command'.

\(fn COMMAND)" t nil)

(autoload 'pony-test-open "pony-mode" "\
Open the file in a traceback at the line specified

\(fn)" t nil)

(autoload 'pony-test-goto-err "pony-mode" "\
Go to the file and line of the last stack trace in a test buffer

\(fn)" t nil)

(autoload 'pony-test-up "pony-mode" "\
Move up the traceback one level

\(fn)" t nil)

(autoload 'pony-test-down "pony-mode" "\
Move up the traceback one level

\(fn)" t nil)

(autoload 'pony-test-hl-files "pony-mode" "\
Highlight instances of Files in Test buffers

\(fn)" nil nil)

(autoload 'pony-load-snippets "pony-mode" "\
Load snippets if yasnippet installed and pony-snippet-dir is set

\(fn)" t nil)

(autoload 'pony-mode-disable "pony-mode" "\
Turn off pony-mode in this buffer

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pony-mode-pkg.el" "pony-tpl.el") (20582
;;;;;;  35739 946043))

;;;***

(provide 'pony-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pony-mode-autoloads.el ends here
