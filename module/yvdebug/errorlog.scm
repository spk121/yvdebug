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

;; This module manages the error log widget and the current-error-port
;; and the current-warn-port of our Guile REPL.

(define-module (yvdebug errorlog)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (mlg assert)
  #:use-module (mlg bytevectors)
  #:use-module (mlg logging)
  #:use-module (mlg strings)
  #:use-module (gi)
  #:use-module (yvdebug typelib)
  #:export (make-error-log
            attach-current-error-ports
            errorlog?))

;; The ErrorMessageList is an ordered list of (number,string) pairs,
;; where the number indicates the message severity (either ERROR or
;; WARNING) and the string is an error message.  New messages are
;; added to the beginning of the list.

;; The current-error-port and current-warning-port are co-opted so
;; that they print into the <ErrorMessageList>

;; The message list gets copied into the GtkTextBuffer, perhaps
;; filtered by a search text or a message severity mask.

(define EML_WARNING 2)
(define EML_ERROR 1)

(define-class <ErrorMessageList> ()
  ;; The actual list
  (lst #:init-value '() #:getter get-list #:setter set-list!)
  (lst-mutex #:init-thunk make-mutex #:getter get-list-mutex)

  ;; The custom error and warning ports
  (errport #:init-value #f #:getter get-errport #:setter set-errport!)
  (warnport #:init-value #f #:getter get-warnport #:setter set-warnport!)

  ;; Are we capturing the current-error-port and current-warn-port?
  (attached? #:init-value #f #:getter is-attached? #:setter set-attached-flag!)
  (stderr #:init-value (current-error-port) #:getter get-stderr)
  (stdwarn #:init-value (current-warning-port) #:getter get-stdwarn))

(define-method (define-error-port (EML <ErrorMessageList>) (severity <integer>))
  "Given SEVERITY, an integer, this defines a new port"
  (define (the-write! bv index count)
    ;; We're line buffered, so hopefully this is one line's data.
    (let ((linestr (utf8->string (subbytevector bv index count))))
      (with-mutex (get-list-mutex EML)
        (set-list! EML
                   (append (get-list EML)
                           (list (cons severity linestr))))
        (idle-add PRIORITY_DEFAULT_IDLE
                  (lambda x
                    ;; (with-mutex (get-list-mutex EML)
                    (update-text-buf *error-log-cur*)
                      #f)
                  ;; )
                  )))
    count)
  (define (close)
    ;; I don't think the error or warnings ports get closed.
    (warn-if-reached))
  (let ((port
         (make-custom-binary-output-port "logport"
                                         the-write!
                                         #f    ; get position
                                         #f    ; set position
                                         close)))
    (setvbuf port 'line)
    port))

(define-method (make-error-ports (EML <ErrorMessageList>))
  "Define custom output ports for the error message list"
  (log-debug "Making error ports")
  (set-errport! EML
                (define-error-port EML EML_ERROR))
  (set-warnport! EML
                 (define-error-port EML EML_WARNING)))

(define-method (attach-current-error-ports (EML <ErrorMessageList>))
  "All stderr/stdwarn is send to the Error Message List."
  (log-debug "attaching current error ports")
  (unless (and (get-errport EML) (get-warnport EML))
    (make-error-ports EML))
  (set-attached-flag! EML #t)
  (current-error-port (get-errport EML))
  (current-warning-port (get-warnport EML)))

(define-method (detach-current-error-ports (EML <ErrorMessageList>))
  "Detach stderr/stdwar from the Error Message List and print it to the
standard error and warning ports"
  (set-attached-flag! EML #f)
  (set-current-error-port (get-stderr EML))
  (set! current-warning-port (get-stdwarn EML)))

(define-method (clear-list (EML <ErrorMessageList>))
  (set-list! EML '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The <ErrorLog> widget is the 'view' to the <ErrorMessageList>
;; model.  It displays the <ErrorMessageList> in the <GtkTextView>,
;; with buttons and search boxes restrict what get shown.

;; on press, errorlog_clear_button clears delete the list in the EML

;; When on, errorlog_error_toggle_button, and errorlog_warning_toggle_button
;; allow the display of those messages.

;; On 'activate', the text of the errorlog_search_entry, restricts
;; the output to lines containing that text

(define-class <ErrorLog> ()
  ;; The widgets
  (txt-view #:init-value #f #:getter get-txt-view #:setter set-txt-view!
            #:init-keyword #:txt-view)
  (txt-buf #:init-value #f #:getter get-txtbuf #:setter set-txtbuf!
           #:init-keyword #:txt-buf)
  (tag-table #:init-value #f #:getter get-tag-table #:setter set-tab-table!
             #:init-keyword #:tag-table)
  (clear-btn #:init-value #f #:getter get-clear-btn
             #:init-keyword #:clear-btn)
  (error-toggle-btn #:getter get-error-toggle-btn
                    #:init-keyword #:error-toggle-btn)
  (warning-toggle-btn #:getter get-warning-toggle-btn
                      #:init-keyword #:warning-toggle-btn)
  (search-entry #:init-value "" #:getter get-search-entry
                #:init-keyword #:search-entry)
  ;; The model
  (message-list #:getter get-message-list
   #:init-keyword #:message-list))

(define (errorlog? x)
  (is-a? x <ErrorLog>))

(define *error-log-cur* #f)

(define (make-error-log txt-view clear-btn error-toggle-btn warning-toggle-btn
                        search-entry)
  (let* ((tag-table (text-tag-table:new))
         (txt-buf (text-buffer:new tag-table))
         (MessageList (make <ErrorMessageList>)))
    (set-buffer txt-view txt-buf)
    (let ((ErrorLog (make <ErrorLog>
                      #:txt-view txt-view
                      #:txt-buf txt-buf
                      #:tag-table tag-table
                      #:clear-btn clear-btn
                      #:error-toggle-btn error-toggle-btn
                      #:warning-toggle-btn warning-toggle-btn
                      #:search-entry search-entry
                      #:message-list MessageList)))
      (connect error-toggle-btn toggled
               (lambda x
                 (update-text-buf ErrorLog)))
      (connect warning-toggle-btn toggled
               (lambda x
                 (update-text-buf ErrorLog)))
      (connect search-entry search-changed
               (lambda x
                 (update-text-buf ErrorLog)))
      (connect clear-btn clicked
               (lambda x
                 (clear-list MessageList)
                 (update-text-buf ErrorLog)))

      ;; Make a mark to point to the end of the buffer.
      (let ([end-iter (make <GtkTextIter>)])
        (get-end-iter! txt-buf end-iter)
        (text-buffer:create-mark txt-buf "end-mark" end-iter #f))

      (set! *error-log-cur* ErrorLog)

      ErrorLog)))

(define-method (attach-current-error-ports (errlog <ErrorLog>))
  (attach-current-error-ports (get-message-list errlog)))

(define (filter-message message show-warnings show-errors search-text)
  (assert (pair? message))
  (assert (string? search-text))
  (let ((severity (car message))
        (text (cdr message)))
    (assert (number? severity))
    (assert (string? text))
    (cond
     ((and (not show-warnings)
           (= severity EML_WARNING))
      "")
     ((and (not show-errors)
           (= severity EML_ERROR))
      "")
     ((not (string-contains text search-text))
      "")
     (else
      (string-append
       (cond
        ((= severity EML_ERROR) "🛑 ")
        ((= severity EML_WARNING) "⚠ ")
        (else
         "  "))
       (string-ensure-single-newline text))))))

(define-method (update-text-buf ErrorLog)
  "Update the text contents of the error log.  This makes a text
rendering of the <ErrorMessageList> based on the state of the toggle
buttons and the search filter box"
  (let ((show-warnings (get-active? (get-error-toggle-btn ErrorLog)))
        (show-errors (get-active? (get-warning-toggle-btn ErrorLog)))
        (search-text (get-text (get-search-entry ErrorLog)))
        (messages (get-list (get-message-list ErrorLog))))
    (let ([body-text (string-append-map
                      (lambda (msg)
                        (filter-message msg show-warnings show-errors search-text))
                      messages)]
          [txtbuf (get-txtbuf ErrorLog)])
      (set-text txtbuf body-text -1)

      ;; Later, scroll to bottom.  You can't scroll to bottom
      ;; immediately because the text view has to absorb the text
      ;; first, so we wait one tick.
      (idle-add PRIORITY_DEFAULT_IDLE
                (lambda x
                  ;; Scroll to bottom
                  (let ([end-iter (make <GtkTextIter>)]
                        [lines-num (get-line-count txtbuf)]
                        [end-mark (get-mark txtbuf "end-mark")])
                    (get-iter-at-line! txtbuf end-iter (1- lines-num))
                    (move-mark txtbuf end-mark end-iter)
                    (scroll-mark-onscreen (get-txt-view ErrorLog) end-mark))
                  #f ; don't keep running
                  ))))

  #f)
