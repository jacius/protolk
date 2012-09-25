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
  (require-library protolk-internal))
 (else
  (load-relative "protolk-internal.scm")))


(module protolk-primitives
  (%make-pob
   pob?

   %pob-base     %pob-set-base!
   %pob-props    %pob-set-props!
   %pob-methods  %pob-set-methods!

   %pob-prop-resolver    %pob-set-prop-resolver!
   %pob-method-resolver  %pob-set-method-resolver!

   %has-prop?
   %prop         %resolve-prop
   %set-prop!    %unset-prop!

   %has-method?
   %method       %resolve-method
   %set-method!  %unset-method!

   %method-context
   %active-pob
   %active-method-name

   %super-context)


(import scheme chicken)
(import protolk-internal)
(use srfi-1)


;;;;;;;;;;;;;;;;;;;;;
;; POB RECORD TYPE
;;

(define-record-type pob
  (%make-pob base props methods prop-resolver method-resolver)
  pob?
  (base            %pob-base            %pob-set-base!)
  (props           %pob-props           %pob-set-props!)
  (methods         %pob-methods         %pob-set-methods!)
  (prop-resolver    %pob-prop-resolver    %pob-set-prop-resolver!)
  (method-resolver  %pob-method-resolver  %pob-set-method-resolver!))

;;; prop-resolver and method-resolvers are procedures used to resolve a
;;; prop/method via recursive inheritance from the base. The
;;; procedures must accept two required args and one optional arg:
;;;
;;; 1. The target pob.
;;;
;;; 2. The prop/method name to resolve.
;;;
;;; 3. (Optional) Default value to return in case resolution fails.
;;;    This optional arg must itself default to #<unspecified>,
;;;    i.e. the return value of (void).
;;;
;;; If the prop/method is not found in the target pob, and the target
;;; pob has a base pob, the procedure should invoke the base pob's
;;; prop-resolver/method-resolver procedure on the base pob, to
;;; recursively resolve.
;;;
;;; The procedures must return a cons containing:
;;;
;;; 1. The pob in which the prop/method was found, or #f if the
;;;    prop/method was not found anywhere.
;;;
;;; 2. The value of the prop/method that was found, or (if the target
;;;    pob has no base pob), the default value specified as the third
;;;    (optional) arg to the procedure.
;;;
;;; See std-prop-resolver and std-method-resolver in protolk.scm for
;;; sample implementations.
;;;
;;; These procedures are required to be set in a pob. When a pob is
;;; derived to create another pob, the original pob's prop-resolver and
;;; method-resolver procedures should be copied into the derived pob at
;;; the time of derivation, unless replacement procedures are
;;; specified at the time of derivation.


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

(define (%resolve-prop pob prop-name #!optional (default (void)))
  ((%pob-prop-resolver pob) pob prop-name default))

(define (%set-prop! pob prop-name value)
  (%pob-set-props! pob (cons (cons prop-name value)
                             (%pob-props pob))))

(define (%unset-prop! pob prop-name)
  (%pob-set-props! pob (remove (car=? prop-name)
                               (%pob-props pob))))


;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD PROCEDURES
;;

(define (%has-method? pob method-name)
  (and (assoc method-name (%pob-methods pob)) #t))

(define (%method pob method-name #!optional (default (void)))
  (let ((pair (assoc method-name (%pob-methods pob))))
    (if pair
        (cdr pair)
        default)))

(define (%resolve-method pob method-name #!optional (default (void)))
  ((%pob-method-resolver pob) pob method-name default))

(define (%set-method! pob method-name value)
  (%pob-set-methods! pob (cons (cons method-name value)
                               (%pob-methods pob))))

(define (%unset-method! pob method-name)
  (%pob-set-methods! pob (remove (car=? method-name)
                                 (%pob-methods pob))))



;;;;;;;;;;;;;;;;;;;
;; ENCAPSULATION
;;

;;; Stores (pob 'method-name arg1 ...) list for the currently running
;;; method. Used as a stack via parameterize.
(define %method-context (make-parameter #f))

(define (%active-pob)
  (let ((mc (%method-context)))
    (if mc (car mc) #f)))

(define (%active-method-name)
  (let ((mc (%method-context)))
    (if mc (cadr mc) #f)))



;;;;;;;;;;;
;; SUPER
;;

(define %super-context (make-parameter #f))


) ;; end module protolk-primitives
