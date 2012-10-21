
(load-relative "spec-helpers")
(load-relative "../protolk")
(import protolk
        protolk-primitives
        protolk-internal)
(import-for-syntax protolk)

(use extras)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WITH-METHOD-CONTEXT
;;

(describe "with-method-context"
  (it "is a macro that sets the method context"
    (let ((pob (make-pob)))
      (equal? (list pob 'some-method 1 2 3)
              (with-method-context (list pob 'some-method 1 2 3)
                (%method-context)))))

  (it "only sets the method context within the scope of its body"
    (let ((pob (make-pob)))
      (with-method-context (list pob 'some-method 1 2 3)
        'noop)
      (equal? #f (%method-context)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE-METHOD
;;

(describe "make-method"
  (it "exands into a method procedure definition"
    (let ((m (make-method (some-method arg1 arg2 arg3)
               (list self arg1 arg2 arg3))))
      (equal? (m 'self 1 2 3) (list 'self 1 2 3))))

  (it "sets the method context within the scope of the method body"
    (let ((m (make-method (some-method arg1 arg2 arg3)
               (%method-context))))
      (equal? (m 'self 1 2 3)
              (list 'self 'some-method 1 2 3))))

  (it "sets the method context appropriately for no args"
    (let ((m (make-method (some-method)
               (%method-context))))
      (equal? (m 'self)
              (list 'self 'some-method))))

  (it "sets the method context appropriately for rest args"
    (let ((m (make-method (some-method arg1 #!rest more-args)
               (%method-context))))
      (equal? (m 'self 1 2 3 4)
              (list 'self 'some-method 1 2 3 4))))

  (it "sets the method context appropriately for optional args"
    (let ((m (make-method (some-method arg1 #!optional arg2 (arg3 6) arg4)
               (%method-context))))
      (equal? (m 'self 1 2)
              (list 'self 'some-method 1 2 6 #f))))

  (it "sets the method context appropriately for keyword args"
    (let ((m (make-method (some-method #!key arg1 (arg2 4) arg3)
               (%method-context))))
      (equal? (m 'self arg3: 3)
              (list 'self 'some-method #:arg1 #f #:arg2 4 #:arg3 3))))

  (it "sets the method context for rest and keyword args together"
    (let ((m (make-method (some-method arg1 #!rest more-args #!key (arg2 4) arg3)
               (%method-context))))
      ;; The rest arg consumes all remaining args, including the keywords.
      (and
       (equal? (m 'self 1)
               (list 'self 'some-method 1))
       (equal? (m 'self 1 2 arg2: 3 4)
               (list 'self 'some-method 1 2 arg2: 3 4)))))

  (it "sets the method context for optional and keyword args together"
    (let ((m (make-method (some-method #!optional arg1 #!key (arg2 4) arg3)
               (%method-context))))
      (and
       (equal? (m 'self 1 arg3: 3)
               (list 'self 'some-method 1 #:arg2 4 #:arg3 3))
       (equal? (m 'self arg3: 3)
               ;; The usual quirky behavior for using optional and
               ;; keyword args together:
               (list 'self 'some-method #:arg3 #:arg2 4 #:arg3 #f)))))

  (it "sets the method context for optional, rest, and keyword args together"
    (let ((m (make-method (some-method arg1
                                       #!optional (arg2 2) arg3
                                       #!rest more-args
                                       #!key (arg4 4) arg5)
               (%method-context))))
      (and
       (equal? (m 'self 1)
               (list 'self 'some-method 1 2 #f))
       (equal? (m 'self 1 2 3 4)
               (list 'self 'some-method 1 2 3 4))
       (equal? (m 'self 1 2 3 arg4: 44 4)
               (list 'self 'some-method 1 2 3 arg4: 44 4))
       (equal? (m 'self 1 2 3 arg4: 44 arg5: 55)
               (list 'self 'some-method 1 2 3 arg4: 44 arg5: 55))
       (equal? (m 'self 1 2 3 4 5 6 7 8 9 10)
               (list 'self 'some-method 1 2 3 4 5 6 7 8 9 10))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE-PRIVATE-METHOD
;;

(describe "make-private-method"

  (it "expands into a private method procedure definition"
    (let ((m (make-private-method
                 (some-method arg1 arg2 arg3)
               (list self arg1 arg2 arg3))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (equal? (m 'self 1 2 3) (list 'self 1 2 3)))))


  (describe "the resulting method"
    (it "succeeds if the receiver is the active pob"
      (let ((m (make-private-method (some-method) 'noop)))
        (not (raises? ()
               (parameterize ((%method-context (list 'self 'a-method)))
                 (m 'self))))))

    (it "raises an error if the receiver is not the active pob"
      (let ((m (make-private-method (some-method) 'noop)))
        (raises? ()
          (m 'self)))))


  (describe "the error the method raises"
    (it "is a 'private-method exception"
      (let ((m (make-private-method (some-method) 'noop)))
        (raises? (private-method)
          (m 'self))))

    (it "contains the pob whose method was called"
      (let* ((m (make-private-method (some-method) 'noop))
             (exn (raises? (private-method)
                    (m 'self))))
        (eq? (get-condition-property exn 'private-method 'pob)
             'self)))

    (it "contains the name of the method that was called"
      (let* ((m (make-private-method (some-method) 'noop))
             (exn (raises? (private-method)
                    (m 'self))))
        (eq? (get-condition-property
              exn 'private-method 'method-name)
             'some-method)))

    (it "contains the args that were passed to the method"
      (let* ((m (make-private-method (some-method arg1 arg2 arg3) 'noop))
             (exn (raises? (private-method)
                    (m 'self 1 2 3))))
        (equal? (get-condition-property exn 'private-method 'args)
                (list 1 2 3))))

    (it "contains empty args if no args were passed to the method"
      (let* ((m (make-private-method (some-method) 'noop))
             (exn (raises? (private-method)
                    (m 'self))))
        (equal? (get-condition-property exn 'private-method 'args)
                (list)))))


  ;; The rest are equivalent to the make-method specs:

  (it "sets the method context within the scope of the method body"
    (let ((m (make-private-method
                 (some-method arg1 arg2 arg3)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (equal? (m 'self 1 2 3)
                (list 'self 'some-method 1 2 3)))))

  (it "sets the method context appropriately for no args"
    (let ((m (make-private-method
                 (some-method)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (equal? (m 'self)
                (list 'self 'some-method)))))

  (it "sets the method context appropriately for rest args"
    (let ((m (make-private-method
                 (some-method arg1 #!rest more-args)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (equal? (m 'self 1 2 3 4)
                (list 'self 'some-method 1 2 3 4)))))

  (it "sets the method context appropriately for optional args"
    (let ((m (make-private-method
                 (some-method arg1 #!optional arg2 (arg3 6) arg4)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (equal? (m 'self 1 2)
                (list 'self 'some-method 1 2 6 #f)))))

  (it "sets the method context appropriately for keyword args"
    (let ((m (make-private-method
                 (some-method #!key arg1 (arg2 4) arg3)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (equal? (m 'self arg3: 3)
                (list 'self 'some-method #:arg1 #f #:arg2 4 #:arg3 3)))))

  (it "sets the method context for rest and keyword args together"
    (let ((m (make-private-method
                 (some-method arg1 #!rest more-args #!key (arg2 4) arg3)
               (%method-context))))
      ;; The rest arg consumes all remaining args, including the keywords.
      (parameterize ((%method-context (list 'self 'some-method)))
        (and
         (equal? (m 'self 1)
                 (list 'self 'some-method 1))
         (equal? (m 'self 1 2 arg2: 3 4)
                 (list 'self 'some-method 1 2 arg2: 3 4))))))

  (it "sets the method context for optional and keyword args together"
    (let ((m (make-private-method
                 (some-method #!optional arg1 #!key (arg2 4) arg3)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (and
         (equal? (m 'self 1 arg3: 3)
                 (list 'self 'some-method 1 #:arg2 4 #:arg3 3))
         (equal? (m 'self arg3: 3)
                 ;; The usual quirky behavior for using optional and
                 ;; keyword args together:
                 (list 'self 'some-method #:arg3 #:arg2 4 #:arg3 #f))))))

  (it "sets the method context for optional, rest, and keyword args together"
    (let ((m (make-private-method
                 (some-method arg1
                              #!optional (arg2 2) arg3
                              #!rest more-args
                              #!key (arg4 4) arg5)
               (%method-context))))
      (parameterize ((%method-context (list 'self 'some-method)))
        (and
         (equal? (m 'self 1)
                 (list 'self 'some-method 1 2 #f))
         (equal? (m 'self 1 2 3 4)
                 (list 'self 'some-method 1 2 3 4))
         (equal? (m 'self 1 2 3 arg4: 44 4)
                 (list 'self 'some-method 1 2 3 arg4: 44 4))
         (equal? (m 'self 1 2 3 arg4: 44 arg5: 55)
                 (list 'self 'some-method 1 2 3 arg4: 44 arg5: 55))
         (equal? (m 'self 1 2 3 4 5 6 7 8 9 10)
                 (list 'self 'some-method 1 2 3 4 5 6 7 8 9 10)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE-METHOD
;;

(describe "define-method"
  (it "is a macro that defines a method in a pob"
    (let ((pob (make-pob)))
      (define-method pob (some-method arg1 arg2 arg3)
        (+ arg1 arg2 arg3))
      (equal? (send pob 'some-method 1 2 3) 6)))

  (it "replaces any existing method with the same name in that pob"
    (let ((pob (make-pob methods: `((some-method
                                     ,(lambda (pob arg1 arg2 arg3)
                                        (+ arg1 arg2 arg3)))))))
      (define-method pob (some-method arg1 arg2 arg3)
        (- arg1 arg2 arg3))
      (equal? (send pob 'some-method 1 2 3) -4)))

  (describe "the defined method"
    (it "applies to the receiving pob even when the method is inherited"
      (let* ((pob (make-pob))
             (pob2 (make-pob base: pob)))
        (define-method pob (get-self)
          self)
        (equal? pob2 (send pob2 'get-self))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE-PRIVATE-METHOD
;;

(describe "define-private-method"
  (it "is a macro that defines a method in a pob"
    (let ((pob (make-pob)))
      (define-private-method pob (some-method arg1 arg2 arg3)
        (+ arg1 arg2 arg3))
      (parameterize ((%method-context (list pob 'a-method)))
        (equal? (send pob 'some-method 1 2 3) 6))))

  (it "replaces any existing method with the same name in that pob"
    (let ((pob (make-pob methods: `((some-method
                                     ,(lambda (pob arg1 arg2 arg3)
                                        (+ arg1 arg2 arg3)))))))
      (define-private-method pob (some-method arg1 arg2 arg3)
        (- arg1 arg2 arg3))
      (parameterize ((%method-context (list pob 'a-method)))
        (equal? (send pob 'some-method 1 2 3) -4))))

  (describe "the defined method"
    (it "is a private method"
      (let ((pob (make-pob)))
        (define-private-method pob (some-method arg1 arg2 arg3)
                               (+ arg1 arg2 arg3))
        (raises? (private-method)
          (send pob 'some-method 1 2 3))))
    
    (it "applies to the receiving pob even when the method is inherited"
      (let* ((pob (make-pob))
             (pob2 (make-pob base: pob)))
        (define-private-method pob (get-self)
          self)
        (parameterize ((%method-context (list pob2 'get-self)))
          (equal? pob2 (send pob2 'get-self)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SUPER
;;

(describe "super"
  (it "is a macro"
    (not (equal? 'super (car (expand '(super 1 2 3))))))

  (it "expands to a %super call"
    (with-replacements ((%super (lambda x x)))
      (equal? (list 1 2 3) (super 1 2 3)))))


(describe "super*"
  (it "is a macro"
    (not (equal? 'super* (car (expand '(super*))))))

  (it "accepts no args"
    (raises? ()
      (expand '(super* 1 2 3))))

  (it "expands to a %super* call"
    (with-replacements ((%super* (lambda x 'foo)))
      (equal? 'foo (super*)))))


(describe "super?"
  (it "acceps no args"
    (raises? (arity)
      (super? 1)))

  (it "fails if there is no super context"
    (raises? (super context)
      (super?)))

  (it "returns #f if there is no next super method"
    (parameterize ((%method-context (list 'some-pob 'some-method)))
      (with-replacements
          ((%super-resolve-next-method
            (lambda (pob method-name invoked-methods)
              #f)))
        (not (super?)))))

  (it "returns #t if there is a next super method"
    (parameterize ((%method-context (list 'some-pob 'some-method)))
      (with-replacements
          ((%super-resolve-next-method
            (lambda (pob method-name invoked-methods)
              (and (eq? pob 'some-pob)
                   (eq? method-name 'some-method)))))
        (super?)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
