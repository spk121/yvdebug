(define-module (yvdebug typelib)
  #:use-module (gi)
  #:use-module (gi util)
  #:use-module (srfi srfi-1)
  #:use-module (gi repository))

(push-duplicate-handler! 'merge-generics)

(define (escape sym prefix badnames)
  (if (member sym badnames)
      (symbol-append prefix sym)
      ;; else
      sym))

(define (typelib->export lib version prefix badnames)
  (require lib version)
  (let* ((names (apply append (map load (infos lib))))
         (name-translations (map
                             (lambda (name)
                               (cons name (escape name prefix badnames)))
                             names)))
    (module-export! (current-module) name-translations)))

(define %badnames (append %rnrs-syntax '(connect)))

(typelib->export "GLib" "2.0" 'g/ %badnames)
(typelib->export "Gio" "2.0" 'gio/ %badnames)
(typelib->export "Gtk" "3.0" 'gtk/ %badnames)
(typelib->export "Vte" "2.91" 'vte/ %badnames)
