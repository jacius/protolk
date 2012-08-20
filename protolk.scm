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


(load-relative "util")
(load-relative "primitives")

(module protolk
  (make-pob
   stdpob
   stdpob-derive
   stdpob-ancestors
   stdpob-has-ancestor?
   stdpob-_resolve-prop
   stdpob-_resolve-method)

(import scheme chicken)
(import %protolk-util protolk-primitives)
(use extras)


;;;;;;;;;;;;;;
;; CORE API
;;

(define (make-pob #!key (props '()) (methods '()))
  (%make-pob props methods))


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
        (cons base (stdpob-ancestors base))
        '())))

(define (stdpob-has-ancestor? self other)
  (let ((base (%prop self 'base #f)))
    (cond ((not (pob? base))
           #f)
          ((eq? base other)
           #t)
          (else
           (stdpob-has-ancestor? base other)))))

(define (stdpob-_resolve-prop self prop-name)
  (if (%has-prop? self prop-name)
      (cons self (%prop self prop-name))
      (let ((base (%prop self 'base #f)))
        (if (pob? base)
            (stdpob-_resolve-prop base prop-name)
            (cons #f (void))))))

(define (stdpob-_resolve-method self method-name)
  (if (%has-method? self method-name)
      (cons self (%method self method-name))
      (let ((base (%prop self 'base #f)))
        (if (pob? base)
            (stdpob-_resolve-method base method-name)
            (cons #f (void))))))


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
              (_resolve-method . ,stdpob-_resolve-method))))

) ;; end module protolk
