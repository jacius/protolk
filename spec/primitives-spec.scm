
(load-relative "helpers")
(load-relative "../protolk-primitives")
(import protolk-primitives
        protolk-internal)


;;; Unhygienic because parts of the expansion will be displayed in
;;; error messages, etc.
(define-syntax describe-pob-slot
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((make-pob-exp '(%make-pob 0 1 2 3 4))
            (slotnum  (list-ref exp 1))
            (slotname (list-ref exp 2))
            (getter   (list-ref exp 3))
            (setter   (list-ref exp 4)))
       `(begin
          
          (describe ,(sprintf "~a" getter)
            (it ,(sprintf "returns the pob's ~a slot value" slotname)
              (equal? (,getter ,make-pob-exp)
                      ,slotnum))
            (it "fails when given a non-pob"
              (raises? ()
                (,getter 'notapob)))
            (it "fails when given no args"
              (raises? ()
                (,getter))))

          (describe ,(sprintf "~a" setter)
            (it ,(sprintf "modifies the pob's ~a slot value" slotname)
              (let ((pob ,make-pob-exp))
                (,setter pob 'new-slot-value)
                (equal? (,getter pob) 'new-slot-value)))
            (it "fails when given a non-pob"
              (raises? ()
                (,setter 'notapob 'new-slot-value)))
            (it "fails when given no args"
              (raises? ()
                (,setter)))
            (it "fails when given only one arg"
              (let ((pob ,make-pob-exp))
                (raises? ()
                  (,setter pob))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POB PRITIMIVE RECORD TYPE
;;

(describe "pob pritimive record type"

  (describe "%make-pob"
    (it "succeeds when given a base, props, methods, prop-resolver, and method-resolver"
      (not (raises? ()
             (%make-pob #f '() '() #f #f))))
    (it "fails when given no args"
      (raises? ()
        (%make-pob)))
    (it "fails when given only one arg"
      (raises? ()
        (%make-pob #f)))
    (it "fails when given only two args"
      (raises? ()
        (%make-pob #f '())))
    (it "fails when given only three args"
      (raises? ()
        (%make-pob #f '() '())))
    (it "fails when given only four args"
      (raises? ()
        (%make-pob #f '() '() #f)))
    (it "fails when given six or more args"
      (raises? ()
        (%make-pob #f '() '() #f #f #f))))


  (describe "pob?"
    (it "returns #t when given a pob"
      (pob? (%make-pob #f '() '() #f #f)))
    (it "returns #f when given a non-pob"
      (not (pob? 'notapob)))
    (it "fails when given no args"
      (raises? ()
        (pob?))))


  (describe-pob-slot 0 "base"    %pob-base    %pob-set-base!)
  (describe-pob-slot 1 "props"   %pob-props   %pob-set-props!)
  (describe-pob-slot 2 "methods" %pob-methods %pob-set-methods!)
  (describe-pob-slot 3 "prop-resolver"
                     %pob-prop-resolver
                     %pob-set-prop-resolver!)
  (describe-pob-slot 4 "method-resolver"
                     %pob-method-resolver
                     %pob-set-method-resolver!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE PROP ACCESSORS
;;

(describe "primitive prop accessors"

  (describe "%has-prop?"
    (it "returns #t when the pob has a matching prop"
      (%has-prop? (%make-pob #f '((a . 1)) '() #f #f) 'a))
    (it "returns #f when the pob does not have a matching prop"
      (not (%has-prop? (%make-pob #f '((a . 1)) '() #f #f) 'foo)))
    (it "fails when given a non-pob"
      (raises? ()
        (%has-prop? 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises? ()
        (%has-prop? (%make-pob #f '((a . 1)) '() #f #f)))))

  (describe "%prop"
    (it "returns the value of a matching prop"
      (equal? (%prop (%make-pob #f '((a . 1)) '() #f #f) 'a)
              1))
    (it "returns #<unspecified> when there is no matching prop"
      (equal? (%prop (%make-pob #f '((a . 1)) '() #f #f) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%prop (%make-pob #f '((a . 1)) '() #f #f) 'foo 'result)
              'result))
    (it "fails when given a non-pob"
      (raises? ()
        (%prop 'foo 'bar)))
    (it "fails when no prop name is specified"
      (raises? ()
        (%prop (%make-pob #f '((a . 1)) '() #f #f)))))

  (describe "%resolve-prop"
    (it "calls the pob's prop-resolver with the given args"
      (let ((p (%make-pob #f '() '() #f #f)))
        (%pob-set-prop-resolver!
         p
         (lambda (pob prop-name default)
           (if (and (equal? pob p)
                    (equal? prop-name 'some-prop)
                    (equal? default 'default-value))
               (raise 'success "Success!")
               (raise 'success "Failure!"))))
        (raises? (success)
          (%resolve-prop p 'some-prop 'default-value)))))

  (describe "%set-prop!"
    (it "sets the specified prop in the pob"
      (let ((pob (%make-pob #f '() '() #f #f)))
        (%set-prop! pob 'a 1)
        (equal? (%prop pob 'a) 1)))
    (it "replaces the value of existing props with that name"
      (let ((pob (%make-pob #f '((a . 1)) '() #f #f)))
        (%set-prop! pob 'a 2)
        (equal? (%prop pob 'a) 2)))
    (it "fails when given no args"
      (raises? ()
        (%set-prop!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%set-prop! 'foo 'a 1)))
    (it "fails when no prop name or value is specified"
      (raises? ()
        (%set-prop! (%make-pob #f '() '() #f #f))))
    (it "fails when no value is specified"
      (raises? ()
        (%set-prop (%make-pob #f '() '() #f #f) 'a))))

  (describe "%unset-prop!"
    (it "removes all matching props from the pob"
      (let ((pob (%make-pob #f '((a . 2) (a . 1)) '() #f #f)))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "has no effect if there are no matching props"
      (let ((pob (%make-pob #f '() '() #f #f)))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "fails when given no args"
      (raises? ()
        (%unset-prop!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%unset-prop! 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises? ()
        (%unset-prop! (%make-pob #f '() '() #f #f))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE METHOD ACCESSORS
;;

(describe "primitive method accessors"
  (define (fn pob) #t)
  (define (fn2 pob) #f)

  (describe "%has-method?"
    (it "returns #t when the pob has a matching method"
      (%has-method? (%make-pob #f '() `((m . ,fn)) #f #f) 'm))
    (it "returns #f when the pob does not have a matching method"
      (not (%has-method? (%make-pob #f '() `((m . ,fn)) #f #f) 'foo)))
    (it "fails when given a non-pob"
      (raises? ()
        (%has-method? 'foo 'm)))
    (it "fails when no method name is specified"
      (raises? ()
        (%has-method? (%make-pob #f '() `((m . ,fn)) #f #f)))))

  (describe "%method"
    (it "returns the value of a matching method"
      (equal? (%method (%make-pob #f '() `((m . ,fn)) #f #f) 'm)
              fn))
    (it "returns #<unspecified> when there is no matching method"
      (equal? (%method (%make-pob #f '() `((m . ,fn)) #f #f) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%method (%make-pob #f '() `((m . ,fn)) #f #f) 'foo fn)
              fn))
    (it "fails when given a non-pob"
      (raises? ()
        (%method 'foo 'm)))
    (it "fails when no method name is specified"
      (raises? ()
        (%method (%make-pob #f '() `((m . ,fn)) #f #f)))))

  (describe "%resolve-method"
    (it "calls the pob's method-resolver with the given args"
      (let ((p (%make-pob #f '() '() #f #f)))
        (%pob-set-method-resolver!
         p
         (lambda (pob method-name default)
           (if (and (equal? pob p)
                    (equal? method-name 'some-method)
                    (equal? default 'default-value))
               (raise 'success "Success!")
               (raise 'success "Failure!"))))
        (raises? (success)
          (%resolve-method p 'some-method 'default-value)))))

  (describe "%set-method!"
    (it "sets the specified method in the pob"
      (let ((pob (%make-pob #f '() '() #f #f)))
        (%set-method! pob 'm fn)
        (equal? (%method pob 'm) fn)))
    (it "replaces the value of existing methods with that name"
      (let ((pob (%make-pob #f '() `((m . ,fn)) #f #f)))
        (%set-method! pob 'm fn)
        (equal? (%method pob 'm) fn)))
    (it "fails when given no args"
      (raises? ()
        (%set-method!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%set-method! 'foo 'm fn)))
    (it "fails when no method name or value is specified"
      (raises? ()
        (%set-method! (%make-pob #f '() '() #f #f))))
    (it "fails when no value is specified"
      (raises? ()
        (%set-method (%make-pob #f '() '() #f #f) 'fn))))

  (describe "%unset-method!"
    (it "removes all matching methods from the pob"
      (let ((pob (%make-pob #f '() `((m . fn2) (m . fn)) #f #f)))
        (%unset-method! pob 'm)
        (not (%has-method? pob 'm))))
    (it "has no effect if there are no matching methods"
      (let ((pob (%make-pob #f '() '() #f #f)))
        (not (%has-method? pob 'm))))
    (it "fails when given no args"
      (raises? ()
        (%unset-method!)))
    (it "fails when given a non-pob"
      (raises? ()
        (%unset-method! 'foo 'a)))
    (it "fails when no method name is specified"
      (raises? ()
        (%unset-method! (%make-pob #f '() '() #f #f))))))



;;;;;;;;;;;;;;;;;;;
;; ESCAPSULATION
;;

(describe "%method-context"
  (define pob (%make-pob #f '() '() #f #f))

  (it "is a parameter used to store context for the current method"
    (let ((arg1 1) (arg2 2) (arg3 3))
      (parameterize ((%method-context
                      (list pob 'some-method arg1 arg2 arg3)))
        (equal? (%method-context)
                (list pob 'some-method arg1 arg2 arg3))))))


(describe "%active-pob"
  (define pob (%make-pob #f '() '() #f #f))

  (it "returns the active pob in the current context"
    (parameterize ((%method-context
                    (list pob 'some-method)))
      (equal? (%active-pob) pob)))

  (it "returns #f if there is no active pob"
    (equal? (%active-pob) #f))

  (it "is read-only"
    (and
     (raises? ()
       (%active-pob pob))
     (raises? ()
       (set! (%active-pob) pob)))))


(describe "%active-method-name"
  (define pob (%make-pob #f '() '() #f #f))

  (it "returns the active method name in the current context"
    (parameterize ((%method-context (list pob 'some-method)))
      (equal? (%active-method-name) 'some-method)))

  (it "returns #f if there is no active method"
    (equal? (%active-method-name) #f))

  (it "is read-only"
    (and
     (raises? ()
       (%active-method-name 'some-method))
     (raises? ()
       (set! (%active-method-name) 'some-method)))))


;;;;;;;;;;;
;; SUPER
;;

(describe "%super-context"
  (define pob (%make-pob #f '() '() #f #f))

  (it "is a parameter that stores the super pob, method name, and args"
    (parameterize
        ((%super-context (list pob 'some-method (list 1 2))))
      (equal? (%super-context)
              (list pob 'some-method (list 1 2)))))

  (it "is #f by default"
    (eq? (%super-context) #f)))


(describe "%same-super-context?"
  (define pob (%make-pob #f '((a . 1)) '() #f #f))
  (define pob2 (%make-pob #f '((a . 2)) '() #f #f))
  
  (it "is true if the pob and method name match the super context"
    (parameterize ((%super-context (list pob 'some-method 1 2)))
      (%same-super-context? pob 'some-method)))

  (it "is false if the pob does not match the super context"
    (parameterize ((%super-context (list pob 'some-method 1 2)))
      (not (%same-super-context? pob2 'some-method))))

  (it "is false if the pob does not match the super context"
    (parameterize ((%super-context (list pob 'some-method 1 2)))
      (not (%same-super-context? pob 'other-method))))

  (it "is false if there is no super context"
    (parameterize ((%super-context #f))
      (not (%same-super-context? pob 'other-method)))))


(describe "%super-invoked-procs"
  (define (m1 pob) 1)
  (define (m2 pob) 2)

  (it "is a parameter that stores a list of already-invoked procedures"
    (parameterize
        ((%super-invoked-procs (list m1 m2)))
      (equal? (%super-invoked-procs)
              (list m1 m2))))

  (it "is the empty list by default"
    (equal? (%super-invoked-procs) (list))))


(describe "%super-resolve-next-method"
  (define (m1 pob) 1)
  (define (m2 pob) 2)

  ;; A simple mock method resolver that resolves the 'm method to the
  ;; given value of m, and fails to resolve any other method.
  (define (mock-mresolver m)
    (lambda (pob method-name #!optional default)
      (if (eq? 'm method-name)
          (cons pob m)
          (cons #f default))))

  (define pob1
    (%make-pob #f '() '()
               #f (mock-mresolver m1)))
  (define pob2
    (%make-pob pob1 '() '()
               #f (mock-mresolver m1)))
  (define pob3
    (%make-pob pob2 '() '()
               #f (mock-mresolver m2)))

  (it "returns the pob's method if there are no invoked procs"
    (eq? m2 (%super-resolve-next-method pob3 'm (list))))

  (it "returns the next inherited proc not in the list"
    (eq? m1 (%super-resolve-next-method pob3 'm (list m2))))

  (it "returns #f if there are no inherited procs not in the list"
    (eq? #f (%super-resolve-next-method pob3 'm (list m2 m1))))

  (it "does not care about the order of the already invoked procs"
    (eq? #f (%super-resolve-next-method pob3 'm (list m1 m2))))

  (it "returns #f if pob neither contains nor inherits the method"
    (eq? #f (%super-resolve-next-method pob3 'foo (list))))

  (it "returns #f if pob has no base and no method"
    (eq? #f (%super-resolve-next-method pob1 'foo (list)))))


(describe "%start-super"
  (it "takes a pob, method name, and list of args"
    (with-replacements ((%continue-super (lambda x 'noop)))
      (not (raises? () (%start-super 'pob 'method (list 1 2))))))
  
  (it "should set up a new super context then call %continue-super"
    (with-replacements ((%continue-super
                         (lambda x (list (%super-context)
                                         (%super-invoked-procs)))))
      (equal? (list (list 'pob 'method (list 1 2)) (list))
              (%start-super 'pob 'method (list 1 2))))))


(describe "%continue-super"
  (it "calls the proc returned by %super-resolve-next-method"
    (with-replacements ((%super-resolve-next-method
                         (lambda x (lambda v v))))
      (equal? (%continue-super 'some-pob 'some-method (list 1 2))
              (list 'some-pob 1 2))))

  (it "sets %super-context and %super-invoked-procs before the call"
    (define (m . args)
      (list (%super-context) (%super-invoked-procs)))

    (with-replacements ((%super-resolve-next-method (lambda x m)))
      (equal? (%continue-super 'some-pob 'some-method (list 1 2))
              (list (list 'some-pob 'some-method (list 1 2))
                    (list m)))))

  (it "raises '(context super) if there is no next method"
    (with-replacements ((%super-resolve-next-method
                         (lambda x #f)))
      (raises? (context super)
        (%continue-super 'some-pob 'some-method (list 1 2)))))

  (it "includes the pob and method name in the error"
    (with-replacements ((%super-resolve-next-method
                         (lambda x #f)))
      (let ((exn (raises? (context super)
                   (%continue-super 'some-pob 'some-method
                                    (list 1 2)))))
        (and (equal? (get-condition-property exn 'super 'pob)
                     'some-pob)
             (equal? (get-condition-property exn 'super 'method-name)
                     'some-method))))))


(describe "%super"
  (define pob (%make-pob #f '() '() #f #f))

  (it "can be called from within a method context"
    (not (raises? (context super)
           (parameterize ((%method-context (list pob 'some-method)))
             (with-replacements ((%start-super (lambda v v)))
               (%super 1 2))))))

  (it "calls %start-super if called with no super context"
    (parameterize ((%method-context (list pob 'some-method)))
      (with-replacements
          ((%same-super-context? (lambda x #f))
           (%start-super (lambda x 'foo)))
        (equal? (%super 1 2) 'foo))))

  (it "calls %start-super if called with a different super context"
    (parameterize ((%method-context (list pob 'some-method)))
      (with-replacements
          ((%same-super-context? (lambda x #f))
           (%start-super (lambda x 'foo))
           (%continue-super (lambda x (error "should not happen"))))
        (equal? (%super 1 2) 'foo))))

  (it "calls %continue-super called with the same super context"
    (parameterize ((%method-context (list pob 'some-method)))
      (with-replacements
          ((%same-super-context? (lambda x #t))
           (%start-super (lambda x (error "should not happen")))
           (%continue-super (lambda x 'foo)))
        (equal? (%super 1 2) 'foo))))

  (it "fails if called from outside a method context"
    (raises? (context super)
      (%super 1 2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
