;;
;; Protolk: flexible Scheme objects with message passing and prototypes
;;
;; Copyright © 2012  John Croisant.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


;;; The [...] read syntax. E.g.:
;;;
;;;   [pob msg arg1 ...]
;;;
;;; is read as:
;;;
;;;   (send pob (quote msg) arg1 ...)
;;;
(module protolk-syntax-send-brackets ()
(import scheme chicken)
(use protolk)

(set-read-syntax! #\[
  (lambda (port)
    (let loop ((c (peek-char port)) (exps '()))
      (cond
       ((eof-object? c)
        (error "EOF encountered while parsing [ ... ] clause"))
       ((char=? c #\])
        (read-char port)                ; discard
        (let ((rexps (reverse exps)))
          `(send ,(car rexps) ',(cadr rexps) ,@(cddr rexps))))
       ((char-whitespace? c)
        (read-char port)                ; discard whitespace
        (loop (peek-char port) exps))
       (else
        (let ((exp (read port)))
          (loop (peek-char port)
                (cons exp exps))))))))
)
