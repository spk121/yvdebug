;; This module manages the terminal widget and PTY's that are stdin
;; and stdout of our Guile REPL.

(define-module (yvdebug terminal)
  #:use-module (ncurses extra)
  #:use-module (yvdebug typelib)
  #:use-module (oop goops)
  #:export (make-terminal
            get-vte-terminal
            attach-current-io-ports
            detach-current-io-ports
            terminal?))

(define-class <Terminal> ()
  ;; The master-pty is owned by this terminal widget
  (master-pty #:init-value #f #:getter get-master-pty #:setter set-master-pty!
              #:init-keyword #:master-pty)
  ;; The slave pty is what the Guile interpreter uses as stdin/stdout
  (slave-pty #:init-value #f #:getter get-slave-pty #:setter set-slave-pty!
             #:init-keyword #:slave-pty)
  ;; The <VtePty> form of the master pty
  (vte-pty #:init-value #f #:getter get-vte-pty #:setter set-vte-pty!
           #:init-keyword #:vte-pty)
  ;; The <VteTerminal> itself
  (vte-terminal #:init-value #f #:getter get-vte-terminal #:setter set-vte-terminal!
                #:init-keyword #:vte-terminal)
  ;; Are we using our slave port as current-input-port and current-output-port?
  (attached? #:init-value #f #:getter is-attached? #:setter set-attached-flag!)
  (stdin #:init-value (current-input-port) #:getter get-stdin)
  (stdout #:init-value (current-output-port) #:getter get-stdout))

(define (terminal? x)
  (is-a? x <Terminal>))

(define (make-master-pty)
  "Ask the kernel to make a pseudoterminal and make a Guile port from
it."
  (let ((master-pty (openpt (logior O_RDWR O_NOCTTY))))
    (grantpt master-pty)
    (unlockpt master-pty)
    (ptsmakeraw master-pty)
    master-pty))

(define (make-slave-pty master-pty)
  "Given a master pty port, make a slave pty port"
  (let ((slave-pty (open-file (ptsname master-pty) "r+0")))
    (ptsmakeraw slave-pty)
    slave-pty))

(define (make-pty-port-pair)
  "Make a pair of linked pseudoterminal ports"
  (let* ((master-pty (make-master-pty))
         (slave-pty (make-slave-pty master-pty)))
    (cons master-pty slave-pty)))

(define (set-termios-to-sane port)
  "A rather normal terminal behavior for a terminal port"
  (let ((termios (tcgetattr port)))
    (termios-flag-set! termios
                       '(CREAD BRKINT ICRNL ICANON  ECHO ECHOE ECHOK
                               OPOST ONLCR))
    (termios-flag-clear! termios
                         '(IGNBRK INLCR IGNCR ECHONL NOFLSH IXOFF IXANY
                                  OCRNL ONOCR))
    (false-if-exception (termios-ispeed-set! termios 115200))
    (false-if-exception (termios-ospeed-set! termios 115200))
    (tcsetattr! port TCSANOW termios)))

(define (set-termios-to-defaults port)
  (set-termios-to-sane port))

;; Assuming a new TERMINAL comes from builder.
(define-method (make-terminal terminal)
  (let* ((pty-pair (make-pty-port-pair))
         (vte-pty (pty:new-foreign-sync (port->fdes (car pty-pair)))))
    (set-size terminal 80 25)
    (set-pty terminal vte-pty)
    (set-termios-to-defaults (car pty-pair))
    (set-termios-to-defaults (cdr pty-pair))
    (make <Terminal>
      #:master-pty (car pty-pair)
      #:slave-pty (cdr pty-pair)
      #:vte-pty vte-pty
      #:vte-terminal terminal)))

(define-method (attach-current-io-ports (terminal <Terminal>))
  "All stdin/stdout is from/to the terminal widget"
  (set-attached-flag! terminal #t)
  (set-current-input-port (get-slave-pty terminal))
  (set-current-output-port (get-slave-pty terminal)))

(define-method (detach-current-io-ports (terminal <Terminal>))
  "Detach stdin/stdout from the terminal widget"
  (set-attached-flag! terminal #f)
  (set-current-input-port (get-stdin terminal))
  (set-current-output-port (get-stdout terminal)))
