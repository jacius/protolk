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


(load-relative "protolk-internal")
(load-relative "protolk-primitives")

(module protolk
  (make-pob
   send
   prop-reader
   prop-writer
   stdpob
   stdpob-derive
   stdpob-ancestors
   stdpob-has-ancestor?
   stdpob-_resolve-prop
   stdpob-_resolve-method
   stdpob-_method-missing
   stdpob-_receive
   stdpob-responds-to?
   stdpob-_display)

(import scheme chicken)
(import protolk-internal protolk-primitives)
(use extras)


;;;;;;;;;;
;; UTIL
;;

;; Use stdpob-_resolve-method to find self's _resolve-method method
;; (or stdpob-_resolve-method if not found), then use that to find
;; the desired method and return it (or the default if not found).
(define (%resolved-method self method-name #!optional (default (void)))
  (let ((resolve-method
         (cdr (stdpob-_resolve-method self '_resolve-method
                                      stdpob-_resolve-method))))
    (cdr (resolve-method
          self method-name
          default))))


;;;;;;;;;;;;;
;; PRINTER
;;

(define-record-printer pob
  (lambda (pob out)
    ((%resolved-method pob '_display stdpob-_display)
     pob out)))


;;;;;;;;;;;;;;
;; CORE API
;;

(define (make-pob #!key (props '()) (methods '()))
  (%make-pob props methods))


(define (send pob message . args)
  ((%resolved-method pob '_receive stdpob-_receive)
   pob message args))

(define (prop-reader prop-name)
  (lambda (self)
    (cdr ((%resolved-method self '_resolve-prop stdpob-_resolve-prop)
          self prop-name))))

(define (prop-writer prop-name)
  (lambda (self value)
    (%set-prop! self prop-name value)))


;;;;;;;;;;;;;;;;;;;;
;; STDPOB METHODS
;;

(define (stdpob-derive self #!key (props '()) (methods '()))
  (unless (or (pob? self) (equal? self #f))
    (raise 'type (sprintf "Not a pob: ~s" self)))
  (make-pob props: `((base . ,self) ,@props)
            methods: methods))

(define (stdpob-ancestors self)
  (let ((base (%prop self 'base #f)))
    (if (pob? base)
        (cons base (send base 'ancestors))
        '())))

(define (stdpob-has-ancestor? self other)
  (let ((base (%prop self 'base #f)))
    (cond ((not (pob? base))
           #f)
          ((eq? base other)
           #t)
          (else
           (send base 'has-ancestor? other)))))

(define (stdpob-_resolve-prop self prop-name
                              #!optional (default (void)))
  (if (%has-prop? self prop-name)
      (cons self (%prop self prop-name))
      (let ((base (%prop self 'base #f)))
        (if (pob? base)
            (stdpob-_resolve-prop base prop-name default)
            (cons #f default)))))

(define (stdpob-_resolve-method self method-name
                                #!optional (default (void)))
  (if (%has-method? self method-name)
      (cons self (%method self method-name))
      (let ((base (%prop self 'base #f)))
        (if (pob? base)
            (stdpob-_resolve-method base method-name default)
            (cons #f default)))))

(define (stdpob-_method-missing self method-name args)
  (raise 'no-method
         (sprintf "undefined method '~s for ~s" method-name self)
         'pob self
         'method-name method-name
         'args args))

(define (stdpob-_receive self message args)
  (let ((method (%resolved-method self message)))
    (if (not (equal? method (void)))
        (apply method self args)
        ((%resolved-method self '_method-missing stdpob-_method-missing)
         self message args))))

(define (stdpob-responds-to? self message . args)
  (or (not (void? (%method self message)))
      (let ((base (%prop self 'base #f)))
        (and (pob? base)
             (apply send base 'responds-to? message args)))))

(define (stdpob-_display self #!optional (port (current-output-port)))
  (unless (pob? self) (raise 'type (sprintf "Not a pob: ~s" self)))
  (display "#<pob>" port))


;;;;;;;;;;;;
;; STDPOB
;;

(define stdpob
  (make-pob
   props: `((base . #f))
   methods: `((derive . ,stdpob-derive)
              (ancestors . ,stdpob-ancestors)
              (has-ancestor? . ,stdpob-has-ancestor?)
              (_resolve-prop . ,stdpob-_resolve-prop)
              (_resolve-method . ,stdpob-_resolve-method)
              (_method-missing . ,stdpob-_method-missing)
              (_receive . ,stdpob-_receive)
              (responds-to? . ,stdpob-responds-to?)
              (_display . ,stdpob-_display))))

) ;; end module protolk
