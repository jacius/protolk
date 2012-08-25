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


(module protolk-primitives
  (%make-pob
   pob?
   %pob-props    %pob-set-props!
   %pob-methods  %pob-set-methods!
   %prop         %has-prop?
   %set-prop!    %unset-prop!
   %method       %has-method?
   %set-method!  %unset-method!)

(import scheme chicken)
(import %protolk-util)
(use srfi-1)


;;;;;;;;;;;;;;;;;;;;;
;; POB RECORD TYPE
;;

(define-record-type pob
  (%make-pob props methods)
  pob?
  (props %pob-props %pob-set-props!)
  (methods %pob-methods %pob-set-methods!))


;;;;;;;;;;;;;;;;;;;;;
;; PROP PROCEDURES
;;

(define (%has-prop? pob prop-name)
  (and (assoc prop-name (%pob-props pob)) #t))

(define (%prop pob prop-name #!optional (default (void)))
  (let ((pair (assoc prop-name (%pob-props pob))))
    (if pair
        (cdr pair)
        default)))

(define (%set-prop! pob prop-name value)
  (%pob-set-props! pob (cons (cons prop-name value)
                             (%pob-props pob))))

(define (%unset-prop! pob prop-name)
  (%pob-set-props! pob (remove (car=? prop-name)
                               (%pob-props pob))))


;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE PROCEDURES
;;

(define (%has-method? pob method-name)
  (and (assoc method-name (%pob-methods pob)) #t))

(define (%method pob method-name #!optional (default (void)))
  (let ((pair (assoc method-name (%pob-methods pob))))
    (if pair
        (cdr pair)
        default)))

(define (%set-method! pob method-name value)
  (%pob-set-methods! pob (cons (cons method-name value)
                               (%pob-methods pob))))

(define (%unset-method! pob method-name)
  (%pob-set-methods! pob (remove (car=? method-name)
                                 (%pob-methods pob))))



) ;; end module protolk-primitives
