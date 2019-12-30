(define-module (yvdebug typelib)
  #:use-module (gi)
  #:use-module (gi util)
  #:use-module (srfi srfi-1)
  #:use-module (gi repository))

(define %badnames (append %rnrs-syntax
                          '(connect format write append close)))

(push-duplicate-handler! 'merge-generics)

(define (escape sym prefix badnames)
  "Add a prefix to this symbol if it is in the badnames list"
  (if (member sym badnames)
      (symbol-append prefix sym)
      ;; else
      sym))

(define (typelib->export lib version prefix badnames)
  "Load and export everything in LIB VERSION, adding a PREFIX to
exported name of procedures whose names appear in BADNAMES"
  (define (escape-name name)
    (escape name prefix badnames))
  (require lib version)
  (let* ((names (append-map load (infos lib)))
         (name-translations (map
                             (lambda (name)
                               `(,name . ,(escape-name name)))
                             names)))
    (module-export! (current-module) name-translations)))


(typelib->export "GLib" "2.0" 'g/ %badnames)
(typelib->export "Gio" "2.0" 'gio/ %badnames)
(typelib->export "Gdk" "3.0" 'gdk/ %badnames)
(typelib->export "Gtk" "3.0" 'gtk/ %badnames)
(typelib->export "Vte" "2.91" 'vte/ %badnames)
