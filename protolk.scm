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

   std-derive
   std-ancestors
   std-has-ancestor?
   std-resolve-prop
   std-resolve-method
   std-_method-missing
   std-_receive
   std-responds-to?
   std-_display

   stdpob)

(import scheme chicken)
(import protolk-internal protolk-primitives)
(use extras)


;;;;;;;;;;
;; UTIL
;;

;; Use std-resolve-method to find self's _resolve-method method
;; (or std-resolve-method if not found), then use that to find
;; the desired method and return it (or the default if not found).
(define (%resolved-method self method-name #!optional (default (void)))
  (let ((resolve-method
         (cdr (std-resolve-method self '_resolve-method
                                      std-resolve-method))))
    (cdr (resolve-method
          self method-name
          default))))


;;;;;;;;;;;;;
;; PRINTER
;;

(define-record-printer pob
  (lambda (pob out)
    ((%resolved-method pob '_display std-_display)
     pob out)))


;;;;;;;;;;;;;;
;; CORE API
;;

(define (make-pob #!key
                  (base    #f)
                  (props   '())
                  (methods '())
                  (resolve-prop   std-resolve-prop)
                  (resolve-method std-resolve-method))
  (%make-pob base props methods resolve-prop resolve-method))


(define (send pob message . args)
  ((%resolved-method pob '_receive std-_receive)
   pob message args))

(define (prop-reader prop-name)
  (lambda (self)
    (cdr ((%resolved-method self '_resolve-prop std-resolve-prop)
          self prop-name))))

(define (prop-writer prop-name)
  (lambda (self value)
    (%set-prop! self prop-name value)))


;;;;;;;;;;;;;;;;;;;;;;
;; STANDARD METHODS
;;

(define (std-derive self #!key (props '()) (methods '()))
  (unless (pob? self)
    (raise 'type (sprintf "Not a pob: ~s" self)))
  (make-pob base: self props: props methods: methods))

(define (std-ancestors self)
  (let ((base (%pob-base self)))
    (if (pob? base)
        (cons base (send base 'ancestors))
        '())))

(define (std-has-ancestor? self other)
  (let ((base (%pob-base self)))
    (cond ((not (pob? base))
           #f)
          ((eq? base other)
           #t)
          (else
           (send base 'has-ancestor? other)))))

(define (std-resolve-prop self prop-name
                           #!optional (default (void)))
  (if (%has-prop? self prop-name)
      (cons self (%prop self prop-name))
      (let ((base (%pob-base self)))
        (if (pob? base)
            (std-resolve-prop base prop-name default)
            (cons #f default)))))

(define (std-resolve-method self method-name
                             #!optional (default (void)))
  (if (%has-method? self method-name)
      (cons self (%method self method-name))
      (let ((base (%pob-base self)))
        (if (pob? base)
            (std-resolve-method base method-name default)
            (cons #f default)))))

(define (std-_method-missing self method-name args)
  (raise 'no-method
         (sprintf "undefined method '~s for ~s" method-name self)
         'pob self
         'method-name method-name
         'args args))

(define (std-_receive self message args)
  (let ((method (%resolved-method self message)))
    (if (not (equal? method (void)))
        (apply method self args)
        ((%resolved-method self '_method-missing std-_method-missing)
         self message args))))

(define (std-responds-to? self message . args)
  (or (not (void? (%method self message)))
      (let ((base (%pob-base self)))
        (and (pob? base)
             (apply send base 'responds-to? message args)))))

(define (std-_display self #!optional (port (current-output-port)))
  (unless (pob? self) (raise 'type (sprintf "Not a pob: ~s" self)))
  (display "#<pob>" port))


;;;;;;;;;;;;
;; STDPOB
;;

(define stdpob
  (make-pob
   base: #f
   methods: `((derive          . ,std-derive)
              (ancestors       . ,std-ancestors)
              (has-ancestor?   . ,std-has-ancestor?)
              (_resolve-prop   . ,std-resolve-prop)
              (_resolve-method . ,std-resolve-method)
              (_method-missing . ,std-_method-missing)
              (_receive        . ,std-_receive)
              (responds-to?    . ,std-responds-to?)
              (_display        . ,std-_display))))

) ;; end module protolk
