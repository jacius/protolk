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


(cond-expand
 (compiling-extension
  (require-library protolk-internal protolk-primitives))
 (else
  (load-relative "protolk-internal.scm")
  (load-relative "protolk-primitives.scm")))


(module protolk
  (make-pob
   send
   prop-reader
   prop-writer
   std-prop-resolver
   std-method-resolver
   std-_receive
   std-_method-missing
   std-_display

   own-prop  set-own-prop!
   assert-active-pob)

(import scheme chicken)
(import protolk-internal protolk-primitives)
(use extras)


;;;;;;;;;;;;;
;; PRINTER
;;

(define-record-printer pob
  (lambda (pob out)
    ((cdr (%resolve-method pob '_display std-_display))
     pob out)))

(define (std-_display self #!optional (port (current-output-port)))
  (unless (pob? self) (raise 'type (sprintf "Not a pob: ~s" self)))
  (display "#<pob>" port))


;;;;;;;;;;;;;;
;; CORE API
;;

(define (make-pob #!key
                  (base    #f)
                  (props   '())
                  (methods '())
                  (prop-resolver   std-prop-resolver)
                  (method-resolver std-method-resolver))
  (%make-pob base props methods prop-resolver method-resolver))


(define (send pob message . args)
  ((cdr (%resolve-method pob '_receive std-_receive))
   pob message args))

(define (prop-reader prop-name)
  (lambda (self)
    (cdr (%resolve-prop self prop-name))))

(define (prop-writer prop-name)
  (lambda (self value)
    (%set-prop! self prop-name value)))


(define (std-prop-resolver self prop-name
                           #!optional (default (void)))
  (if (%has-prop? self prop-name)
      (cons self (%prop self prop-name))
      (let ((base (%pob-base self)))
        (if (pob? base)
            (%resolve-prop base prop-name default)
            (cons #f default)))))

(define (std-method-resolver self method-name
                             #!optional (default (void)))
  (if (%has-method? self method-name)
      (cons self (%method self method-name))
      (let ((base (%pob-base self)))
        (if (pob? base)
            (%resolve-method base method-name default)
            (cons #f default)))))


(define (std-_method-missing self method-name args)
  (raise 'no-method
         (sprintf "undefined method '~s for ~s" method-name self)
         'pob self
         'method-name method-name
         'args args))

(define (std-_receive self message args)
  (let ((method (cdr (%resolve-method self message))))
    (if (not (equal? method (void)))
        (apply method self args)
        ((cdr (%resolve-method self '_method-missing std-_method-missing))
         self message args))))


;;;;;;;;;;;;;;;;;;;
;; ENCAPSULATION
;;

(define (set-own-prop! prop-name value)
  (let ((self (%self)))
    (if (pob? self)
        (%set-prop! self prop-name value)
        (raise 'context "No active self in the current context."
               'prop-name prop-name
               'value value))))

(define own-prop
  (getter-with-setter
   (lambda (prop-name)
     (let ((self (%self)))
       (if (pob? self)
           (%prop self prop-name)
           (raise 'context "No active self in the current context."
                  'prop-name prop-name))))
   set-own-prop!))


(define (assert-active-pob pob #!optional message)
  (cond
   ((not (%self))
    (raise 'context
           "There is no active pob in the current context."))
   ((not (equal? pob (%self)))
    (raise 'context
           (or message
               (sprintf
                "~s is not the active pob in the current context."
                pob))
           'pob pob
           'active-pob (%self)))
   (else
    #t)))


) ;; end module protolk
