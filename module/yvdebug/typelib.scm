(define-module (yvdebug typelib)
  #:use-module (gi)
  #:use-module (gi util)
  #:use-module (srfi srfi-1)
  #:use-module (gi repository))

(push-duplicate-handler! 'merge-generics)

(define (typelib-export-escape lib version badnames)
  (require lib version)
  (let ((%bindings (apply append
                          (map load
                               (infos lib))))
        (%badnames (append %rnrs-syntax badnames)))
    (write %badnames)
    (for-each (lambda (binding)
                (if (member binding %badnames)
                    (let ((new-binding
                           (symbol-append
                            (string->symbol (string-downcase lib))
                            ':
                            binding)))
                      (format #t "~s - ~s -> ~s \n" lib binding new-binding)
                      (export new-binding binding))
                    ;; else
                    (begin
                      (format #t "~s ~s ~s\n" lib binding (symbol? binding))
                      (export binding))))
              %bindings)))

(typelib->module (current-module) "Gio" "2.0")
(typelib->module (current-module) "Gtk" "3.0")
(typelib->module (current-module) "Vte" "2.91")
