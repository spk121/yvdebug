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

(define-module (yvdebug interpreter)
  #:use-module (system vm vm)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:use-module (mlg assert)
  #:use-module (yvdebug terminal)
  #:use-module (yvdebug errorlog)
  #:export (spawn-interpreter-thread))

(define (rejigger-program-arguments argv)
  (let* ((argv2
          (fold
           (lambda (x prev)
             (if (member x '("--no-debug" "--debug" "-q"))
                 prev
                 (append (list x) prev)))
           '()
           argv)))
    (format #t "COMMANDLINEARGS ~S~%" argv2)
    argv2))

(define (make-entry-point argv)
  "This parses the command line arguments to create a procedure that
can be called as the entry point of this script.  This procedure lives
in its own prompt."
  (assert (list? argv))
  ;; When the real Guile interpreter spawns, it checks to see
  ;; if it should call setlocale.  But since this interpreter
  ;; is already running in Guile, I guess we assume that it is
  ;; taken care of.
  ;; (when (should-install-locale?)
  ;;   (setlocale LC_ALL ""))

  ;; Maybe we need to cull some standard Guile command-line
  ;; arguments
  (set-program-arguments (rejigger-program-arguments argv))

  ;; Generically we'd run (ice-9 top-repl) but if we're passed
  ;; a script, we'll start there
  (compile-shell-switches argv))

(define (spawn-interpreter-thread argv terminal errlog)
  "Spawns and returns a new thread.  In that thread, parse a
command-line argument list and then run a Guile interpreter. Bind the
thread's I/O ports to the GUI's terminal and errlog widgets."
  (assert (list? argv))
  (assert (terminal? terminal))
  (assert (errorlog? errlog))
  (call-with-new-thread
   (lambda ()
     (attach-current-io-ports terminal)
     (attach-current-error-ports errlog)
     (let ([entry-point (make-entry-point argv)])
       (restore-signals)
       (usleep 10)
       (eval entry-point (current-module))
       (usleep 10)
       0))))
