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
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 threads)
  #:use-module (yvdebug terminal)
  #:export (run-interpreter))

(define (get-integer-from-environment var def)
  (let ((val (getenv var)))
    (cond
     ((not val)
      def)
     (else
      (let ((res (false-if-exception (inexact->exact (string->number val)))))
        (if (not res)
            (begin
              (format (current-error-port) "yvdebug: warning: invalid ~s: ~s\n"
                      var val)
              def)
            res))))))

(define (should-install-locale)
  (get-integer-from-environment "GUILE_INSTALL_LOCALE" 1))

(define (inner-main argv)
  "aka scm-shell"
  (eval (compile-shell-switches argv)
        (current-module)))

(define (rejigger-program-arguments argv)
  (let* ((argv2
          (fold 
           (lambda (x prev)
             (if (member x '("--no-debug" "--debug" "-q"))
                 prev
                 (append (list x) prev)))
           '()
           argv))
         (argv3 (append argv2 '("--debug" "-q"))))
    (format #t "COMMANDLINEARGS ~S~%" argv3)
    argv3))

(define (scm-boot-guile main-func argv)
  (exit (call-with-new-thread
         (lambda ()
           (when (should-install-locale)
             (setlocale LC_ALL ""))
           (let ((ret (main-func (command-line))))
             (restore-signals)
             (usleep 10)
             ret)))))

(define (run-interpreter argv Terminal)
  (call-with-new-thread
   (lambda ()
     ;(set-current-input-port (fdopen 0 "r0"))
     ;(set-current-output-port (fdopen 1 "w0"))
     (attach-current-io-ports Terminal)
     (when (should-install-locale)
       (setlocale LC_ALL ""))
     (set-program-arguments (rejigger-program-arguments argv))
     (let ((ret (eval (compile-shell-switches argv) (current-module))))
       (restore-signals)
       (usleep 10)
       ret))))
