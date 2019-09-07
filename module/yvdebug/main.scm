(define-module (yvdebug main)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 binary-ports)
  #:use-module (yvdebug terminal)
  #:use-module (system repl repl)
  #:use-module (yvdebug typelib)
  #:use-module (srfi srfi-43)
  #:use-module (mlg utils)
  #:export (go))

(define (activate app)
  ;; Construct a GtkBuilder instance and load our UI description.
  (let* ((builder (builder:new-from-resource "/com/lonelycactus/yvdebug/mainwindow.ui"))
         (mainwindow (get-object builder "mainwindow"))
         (terminal (make-terminal (get-object builder "terminal"))))
    (attach-current-io-ports terminal)
    (show-all mainwindow)
    (call-with-new-thread
     (lambda ()
       (start-repl #:debug #t)))
    #t))

(define (go args)
  (let ((gresource (find-data-file "yvdebug/yvdebug.gresource")))
    (unless gresource
      (error "Could not open ~S" "yvdebug.gresource"))
    (format #t "Found resource file ~S" gresource)
    (init)
    (resources-register (resource:load gresource))
    (let ((app (application:new "com.lonelycactus.yvdebug"
                                (number->application-flags 0))))
      (connect app application:activate activate)
      
      ;; Don't pass command line here: We don't want any of the command
      ;; line being interpreted by Gtk.
      (application:run app)
      #t)))
