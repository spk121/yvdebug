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
  #:use-module (ice-9 control)
  #:use-module (ice-9 top-repl)
  ;; #:use-module (ice-9 binary-ports)
  #:use-module (system repl repl)
  #:use-module (srfi srfi-11)
  #:use-module (mlg utils)
  #:use-module (mlg logging)
  #:use-module (gi)
  #:use-module (yvdebug terminal)
  #:use-module (yvdebug typelib)
  #:use-module (yvdebug errorlog)
  #:use-module (yvdebug interpreter)
  #:export (go))

(define (activate app)
  ;; Construct a GtkBuilder instance and load our UI description.
  (let* ((main-window (application-window:new app))
         (builder (builder:new-from-resource "/com/lonelycactus/yvdebug/mainwindow.ui")))
    (define (obj str)
      (warn-if-false (get-object builder str)))
    (let ((main-grid (obj "main-grid"))
          (terminal (obj "terminal"))
          (terminal-scrollbar (obj "terminal-scrollbar"))
          (errorlog-text-view (obj "errorlog-text-view"))
          (errorlog-scrollbar (obj "errorlog-scrollbar"))
          (errorlog-clear-button (obj "errorlog-clear-button"))
          (errorlog-error-toggle-button (obj "errorlog-error-toggle-button"))
          (errorlog-warning-toggle-button (obj "errorlog-warning-toggle-button"))
          (errorlog-search-entry (obj "errorlog-search-entry")))
      (add main-window main-grid)
      (set-title main-window "YVDebug")
      (let ((Terminal (make-terminal terminal))
            (ErrorLog (make-error-log errorlog-text-view
                                      errorlog-clear-button
                                      errorlog-error-toggle-button
                                      errorlog-warning-toggle-button
                                      errorlog-search-entry
                                      errorlog-scrollbar)))
        (attach-current-io-ports Terminal)
        (attach-current-error-ports ErrorLog)
        (show-all main-window)
        (run-interpreter (command-line) Terminal)))))

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
