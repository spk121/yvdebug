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
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (mlg bytevectors)
  #:use-module (mlg logging)
  #:use-module (mlg strings)
  #:use-module (yvdebug typelib)
  #:export (make-error-log
            attach-current-error-ports))

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

  ;; A reference to a ErrorLog
  (errorlog #:init-value #f #:getter get-errorlog #:setter set-errorlog!)

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
      (log-debug "New error port entry: Severity ~S Message ~S" severity linestr)
      (set-list! EML
                 (append (list (cons severity linestr))
                         (get-list EML)))
      ;; We processed all the bytes.
      (log-debug "The list ~S" (get-list EML))
      (and=> (get-errorlog EML) update-text-buf)
      count))
  (define (close)
    ;; I don't think the error or warnings ports get closed.
    (warn-if-reached))
  (log-debug "In define-error-port ~S ~S" EML severity)
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
  (search-entry #:getter get-search-entry
                #:init-keyword #:search-entry)
  (scrollbar
   #:init-keyword #:scrollbar)

  ;; The model
  (message-list #:getter get-message-list
   #:init-keyword #:message-list))

(define (make-error-log txt-view clear-btn error-toggle-btn warning-toggle-btn
                        search-entry scrollbar)
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
                      #:scrollbar scrollbar
                      #:search-entry search-entry
                      #:message-list MessageList)))
      (set-errorlog! MessageList ErrorLog)
      ErrorLog)))

(define-method (attach-current-error-ports (errlog <ErrorLog>))
  (attach-current-error-ports (get-message-list errlog)))

(define-method (update-text-buf (errlog <ErrorLog>))
  (let ((show-warnings (get-active? (get-error-toggle-btn errlog)))
        (show-errors (get-active? (get-warning-toggle-btn errlog)))
        (search-text (get-text (get-search-entry errlog)))
        (messages (get-list (get-message-list errlog))))
    (define (filter-message message)
      (let ((severity (car message))
            (text (cdr message)))
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
           (string-ensure-single-newline text))))))
    (log-debug "update text: warnings ~s errors ~s searchtxt ~s"
               show-warnings show-errors search-text)
    (let ((body-text (string-append-map filter-message messages)))
      (log-debug "Setting body text to ~S" body-text)
      (set-text (get-txtbuf errlog) body-text -1))))
