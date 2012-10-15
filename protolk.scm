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
   assert-active-pob
   with-method-context
   define-method
   define-private-method
   %rewrite-args

   super
   super*
   super?)

(import scheme chicken)
(import protolk-internal protolk-primitives)
(use extras srfi-1)


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
  (let ((active-pob (%active-pob)))
    (if (pob? active-pob)
        (%set-prop! active-pob prop-name value)
        (raise 'context "No active pob in the current context."
               'prop-name prop-name
               'value value))))

(define own-prop
  (getter-with-setter
   (lambda (prop-name)
     (let ((active-pob (%active-pob)))
       (if (pob? active-pob)
           (cdr (%resolve-prop active-pob prop-name))
           (raise 'context "No active pob in the current context."
                  'prop-name prop-name))))
   set-own-prop!))


(define (assert-active-pob pob #!optional message)
  (cond
   ((not (pob? (%active-pob)))
    (raise 'context
           (or message
               "There is no active pob in the current context.")))
   ((not (eq? pob (%active-pob)))
    (raise 'context
           (or message
               (sprintf
                "~s is not the active pob in the current context."
                pob))
           'pob pob
           'active-pob (%active-pob)))
   (else
    #t)))


;;;;;;;;;;;;;;;;;;;;;;
;; DEFINING METHODS
;;

(define-syntax with-method-context
  (syntax-rules ()
    ((with-method-context context . body)
     (parameterize ((%method-context context))
       . body))))


(define-syntax %rewrite-args
  (syntax-rules (~required ~optional ~key #!optional #!key #!rest)
    ;; Recursion end case
    ((_ arg-type)
     (list))

    ;; Switching argument modes
    ((_ arg-type #!optional . more-args)
     (%rewrite-args ~optional . more-args))
    ((_ arg-type #!key . more-args)
     (%rewrite-args ~key . more-args))

    ;; #!rest just gobbles up all the remaining args
    ((_ arg-type #!rest arg . more-args)
     arg)

    ((_ ~required arg . more-args)
     (cons arg (%rewrite-args ~required . more-args)))

    ;; Optional arg with and without default value.
    ((_ ~optional (arg value) . more-args)
     (cons arg (%rewrite-args ~optional . more-args)))
    ((_ ~optional arg . more-args)
     (cons arg (%rewrite-args ~optional . more-args)))

    ;; Keyword arg with and without default value.
    ((_ ~key (arg value) . more-args)
     (cons* (string->keyword (symbol->string 'arg)) arg
            (%rewrite-args ~key . more-args)))
    ((_ ~key arg . more-args)
     (cons* (string->keyword (symbol->string 'arg)) arg
            (%rewrite-args ~key . more-args)))))


(define-syntax define-method
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((pob (cadr exp))
            (signature (caddr exp))
            (method-name (car signature))
            (args (cdr signature))
            (body (cdddr exp)))
       `(%set-method! ,pob ',method-name
          (lambda (,(inject 'self) ,@args)
            (with-method-context
             (cons* ,(inject 'self) ',method-name
                    (%rewrite-args ~required ,@args))
             ,@body)))))))


(define-syntax define-private-method
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((pob (cadr exp))
            (signature (caddr exp))
            (method-name (car signature))
            (args (cdr signature))
            (body (cdddr exp)))
       `(%set-method! ,pob ',method-name
          (lambda (,(inject 'self) ,@args)
            (if (eq? (%active-pob) ,(inject 'self))
             (with-method-context
              (cons* ,(inject 'self) ',method-name
                     (%rewrite-args ~required ,@args))
              ,@body)
             (raise ',(inject 'private-method)
              (sprintf
               "private method '~s called for ~s"
               ',method-name ,(inject 'self))
              ',(inject 'pob) ,(inject 'self)
              ',(inject 'method-name) ',method-name
              ',(inject 'args) (%rewrite-args
                                ~required ,@args)))))))))


;;;;;;;;;;;
;; SUPER
;;

(define-syntax super
  (syntax-rules ()
    ((super . args)
     (%super . args))))

(define-syntax super*
  (syntax-rules ()
    ((super*)
     (%super*))))

(define (super?)
  (let ((context (%method-context))
        (invoked (%super-invoked-procs)))
    (if context
        (not (not (%super-resolve-next-method
                   (car context) (cadr context) invoked)))
        (raise '(context super)
               "Cannot invoke super? outside of a method context."))))


) ;; end module protolk
