;; Copyright 2019 Michael Gran <spk121@yahoo.com>
;;
;; This file is part of YVDebug.
;;
;; YVDebug is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; YVDebug is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with YVDebug.  If not, see <https://www.gnu.org/licenses/>.

(define-module (yvdebug main)
  ;; #:use-module (ice-9 command-line)
  ;; #:use-module (ice-9 binary-ports)
  #:use-module (yvdebug terminal)
  #:use-module (system repl repl)
  #:use-module (yvdebug typelib)
  #:use-module (yvdebug errorlog)
  ;; #:use-module (srfi srfi-43)
  #:use-module (mlg utils)
  #:use-module (mlg logging)
  #:use-module (gi)
  #:export (go))

(define (activate app)
  ;; Construct a GtkBuilder instance and load our UI description.
  (let* ((mainwindow (application-window:new app))
         (builder (builder:new-from-resource "/com/lonelycactus/yvdebug/mainwindow.ui"))
         (maingrid (get-object builder "main_grid"))
         (terminal (make-terminal (get-object builder "terminal")))
         (EML (make <ErrorMessageList>)))

    (add mainwindow maingrid)
    (set-title mainwindow "YVDebug")
    (attach-current-io-ports terminal)
    (show-all mainwindow)

    (attach-current-error-ports EML)
    (call-with-new-thread
     (lambda ()
       (attach-current-error-ports EML)
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
