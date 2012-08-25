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


;;; This module contains utility procedures and/or syntax used in the
;;; Protolk source code itself. These are NOT part of the Protolk API,
;;; and may change at any time. Do not use them.

(module %protolk-util *
(import scheme chicken)


;;; Returns #t if x is #<unspecified>
(define (void? x) (equal? x (void)))


;;; Returns a lambda that takes a pair and returns true if the car of
;;; that pair matches x. The keyword argument pred is the predicate to
;;; use for comparing x to the car of the pair. This is mainly useful
;;; for searching or filtering alists.
;;;
;;; Example:
;;;
;;; ((car=? b) '(b . 2))
;;; ; #t
;;;
;;; (remove (car=? 'b) '((a . 1) (b . 2) (c . 3)))
;;; ; ((a . 1) (c . 3))
;;;
(define (car=? x #!key (pred equal?))
  (lambda (pair) (pred (car pair) x)))


(define (make-exception kinds message . other-properties)
  (apply make-composite-condition
         (make-property-condition 'exn 'message message)
         (map (lambda (kind)
                (apply make-property-condition kind
                       'message message
                       other-properties))
              (if (list? kinds)
                  kinds
                  (list kinds)))))

(define (raise kinds message . other-properties)
  (abort (apply make-exception kinds message other-properties)))


) ;; end  module %protolk-util
