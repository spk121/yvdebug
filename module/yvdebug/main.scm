(define-module (yvdebug main)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 binary-ports)
  #:use-module (yvdebug terminal)
  #:use-module (system repl repl)
  #:use-module (yvdebug typelib)
  #:use-module (srfi srfi-43)
  #:use-module (mlg utils)
  #:use-module (mlg logging)
  #:use-module (gi)
  #:export (go))

(define (activate app)
  ;; Construct a GtkBuilder instance and load our UI description.
  (let* ((mainwindow (application-window:new app))
         (builder (builder:new-from-resource "/com/lonelycactus/yvdebug/mainwindow.ui"))
         (maingrid (get-object builder "main_grid"))
         (terminal (make-terminal (get-object builder "terminal"))))

    (add mainwindow maingrid)
    (set-title mainwindow "YVDebug")
    (attach-current-io-ports terminal)
    (show-all mainwindow)
    (call-with-new-thread
     (lambda ()
       (start-repl #:debug #t)))
    #t))

(define (go args)
  ;; Load the resources bundle: UI files, etc
  (let ((gresource (find-data-file "yvdebug/yvdebug.gresource")))
    (if gresource
        (log-debug "found resource file ~S" gresource)
        (log-error "could not find or open yvdebug.gresource resource file"))

    (let ((app (application:new "com.lonelycactus.yvdebug"
                                (number->application-flags 0))))
      (resources-register (resource:load gresource))
      (connect app application:activate activate)

      ;; Don't pass command line here: We don't want any of the command
      ;; line being interpreted by Gtk.
      (run app))))
