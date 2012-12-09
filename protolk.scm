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
   set-base!
   send
   std-prop-resolver
   std-method-resolver
   std-_receive
   std-_method-missing
   std-_display

   own-prop  set-own-prop!  unset-own-prop!
   is-receiver?
   assert-is-receiver
   with-method-context
   make-method
   make-private-method
   set-method!
   define-method
   define-private-method
   %rewrite-args

   super
   super*
   super?
   apply-super

   prop-reader
   prop-writer
   define-prop-readers
   define-prop-writers
   define-prop-accessors)

(import scheme chicken)
(use extras srfi-1 lolevel)

(import protolk-internal protolk-primitives)
(reexport (only protolk-primitives pob?))


;;;;;;;;;;;;;
;; PRINTER
;;

(define-record-printer pob
  (lambda (pob out)
    ((cdr (%resolve-method pob '_display std-_display))
     pob out)))

(define (std-_display self #!optional (port (current-output-port)))
  (unless (pob? self) (raise 'type (sprintf "Not a pob: ~s" self)))
  (fprintf port "#<pob 0x~X>" (pointer->address (object->pointer self))))


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

(define (set-base! pob new-base)
  (if (and new-base (%has-ancestor? new-base pob))
      (raise 'cyclic-ancestry
             (sprintf "setting base of ~s to ~s would cause cyclic ancestry"
                      pob new-base)
             'pob pob
             'new-base new-base)
      (%pob-set-base! pob new-base)))


(define (send pob message . args)
  ((cdr (%resolve-method pob '_receive std-_receive))
   pob message args))


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
  (let ((receiver (%receiver)))
    (if (pob? receiver)
        (%set-prop! receiver prop-name value)
        (raise 'context "No receiver in the current context."
               'prop-name prop-name
               'value value))))

(define own-prop
  (getter-with-setter
   (lambda (prop-name)
     (let ((receiver (%receiver)))
       (if (pob? receiver)
           (cdr (%resolve-prop receiver prop-name))
           (raise 'context "No receiver in the current context."
                  'prop-name prop-name))))
   set-own-prop!))

(define (unset-own-prop! prop-name)
  (let ((receiver (%receiver)))
    (if (pob? receiver)
        (%unset-prop! receiver prop-name)
        (raise 'context "No receiver in the current context."
               'prop-name prop-name))))


(define (is-receiver? pob)
  (and (pob? pob) (eq? pob (%receiver))))


(define (assert-is-receiver pob #!optional message)
  (cond
   ((not (pob? (%receiver)))
    (raise 'context
           (or message
               "There is no receiver in the current context.")))
   ((not (eq? pob (%receiver)))
    (raise 'context
           (or message
               (sprintf
                "~s is not the receiver in the current context."
                pob))
           'pob pob
           'receiver (%receiver)))
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


(define-syntax make-method
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((signature (cadr exp))
            (method-name (car signature))
            (args (cdr signature))
            (body (cddr exp)))
       `(lambda (,(inject 'self) ,@args)
          (with-method-context
           (cons* ,(inject 'self) ',method-name
                  (%rewrite-args ~required ,@args))
           ,@body))))))


(define-syntax make-private-method
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let* ((signature (cadr exp))
            (method-name (car signature))
            (args (cdr signature))
            (body (cddr exp)))
       `(lambda (,(inject 'self) ,@args)
          (if (eq? (%receiver) ,(inject 'self))
              (with-method-context
               (cons* ,(inject 'self) ',method-name
                      (%rewrite-args ~required ,@args))
               ,@body)
              (raise ',(inject 'private-method)
                     (sprintf "private method '~s called for ~s"
                              ',method-name ,(inject 'self))
                     ',(inject 'pob) ,(inject 'self)
                     ',(inject 'method-name) ',method-name
                     ',(inject 'args) (%rewrite-args
                                       ~required ,@args))))))))


(define (set-method! pob method-name procedure)
  (%set-method! pob method-name procedure))


(define-syntax define-method
  (syntax-rules ()
    ((define-method pob (name . args) . body)
     (set-method! pob 'name
      (make-method (name . args)
       . body)))))

(define-syntax define-private-method
  (syntax-rules ()
    ((define-private-method pob (name . args) . body)
     (set-method! pob 'name
      (make-private-method (name . args)
       . body)))))


;;;;;;;;;;;
;; SUPER
;;

(define-syntax super
  (syntax-rules ()
    ((super . args)
     (%super . args))))

(define-syntax apply-super
  (syntax-rules ()
    ((apply-super . args)
     (apply %super . args))))

(define-syntax super*
  (syntax-rules ()
    ((super*)
     (%super*))))

(define (super?)
  (let ((context (%method-context))
        (invoked (%super-invoked-procs)))
    (if context
        (let ((this-method (%super-resolve-next-method
                            (car context) (cadr context) invoked)))
          (not (not (%super-resolve-next-method
                     (car context) (cadr context) (cons this-method invoked)))))
        (raise '(context super)
               "Cannot invoke super? outside of a method context."))))


;;;;;;;;;;;;;;;
;; ACCESSORS

;;; These are defined down here because they need with-method-context.

(define (prop-reader prop-name)
  (lambda (self)
    (with-method-context (list self prop-name)
      (own-prop prop-name))))

(define (prop-writer prop-name)
  (lambda (self value)
    (with-method-context (list self prop-name value)
      (set-own-prop! prop-name value))))


(define (define-prop-readers pob prop-names)
  (for-each
   (lambda (prop)
     (if (list? prop)
         (set-method! pob (cadr prop)
                      (prop-reader (car prop)))
         (set-method! pob prop (prop-reader prop))))
   prop-names))


(define (define-prop-writers pob prop-names)
  (for-each
   (lambda (prop)
     (let* ((prop-name (if (list? prop)
                           (car prop)
                           prop))
            (method-name (if (list? prop)
                             (cadr prop)
                             (string->symbol
                              (sprintf "set-~A!" prop-name)))))
       (set-method! pob method-name (prop-writer prop-name))))
   prop-names))


(define (define-prop-accessors pob prop-names)
  (for-each
   (lambda (prop)
     (let* ((prop-name (if (list? prop)
                           (car prop)
                           prop))
            (reader-name (if (and (list? prop) (> (length prop) 1))
                             (cadr prop)
                             prop-name))
            (writer-name (if (and (list? prop) (> (length prop) 2))
                             (caddr prop)
                             (string->symbol
                              (sprintf "set-~A!" reader-name)))))
       (set-method! pob reader-name (prop-reader prop-name))
       (set-method! pob writer-name (prop-writer prop-name))))
   prop-names))


) ;; end module protolk
