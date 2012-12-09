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
  (require-library protolk-internal protolk-primitives protolk))
 (else
  (load-relative "protolk-internal.scm")
  (load-relative "protolk-primitives.scm")
  (load-relative "protolk.scm")))


(module protolk-stdpob
  (stdpob-derive
   stdpob-ancestors
   stdpob-has-ancestor?
   stdpob-responds-to?
   stdpob)

(import scheme chicken)
(import protolk-internal protolk-primitives protolk)
(use extras)


;;;;;;;;;;;;;;;;;;;;
;; STDPOB METHODS
;;


(define (stdpob-derive self
                       #!key
                       (props          '())
                       (methods        '())
                       (prop-resolver   (%pob-prop-resolver self))
                       (method-resolver (%pob-method-resolver self)))
  (unless (pob? self)
    (raise 'type (sprintf "Not a pob: ~s" self)))
  (make-pob
   base:            self
   props:           props
   methods:         methods
   prop-resolver:   prop-resolver
   method-resolver: method-resolver))

(define (stdpob-ancestors self)
  (let ((base (%pob-base self)))
    (if (pob? base)
        (cons base (send base 'ancestors))
        '())))

(define (stdpob-has-ancestor? self other)
  (let ((base (%pob-base self)))
    (cond ((not (pob? base)) #f)
          ((eq? base other)  #t)
          (else              (send base 'has-ancestor? other)))))

(define (stdpob-responds-to? self message . args)
  (or (not (void? (%method self message)))
      (let ((base (%pob-base self)))
        (and (pob? base)
             (apply send base 'responds-to? message args)))))


;;;;;;;;;;;;
;; STDPOB
;;

(define stdpob
  (make-pob
   methods: `((_display        ,std-_display)
              (_method-missing ,std-_method-missing)
              (_receive        ,std-_receive)
              (derive          ,stdpob-derive)
              (ancestors       ,stdpob-ancestors)
              (has-ancestor?   ,stdpob-has-ancestor?)
              (responds-to?    ,stdpob-responds-to?))))


) ;; end module protolk-stdpob
