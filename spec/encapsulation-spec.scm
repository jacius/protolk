
(load-relative "spec-helpers")
(load-relative "../protolk")
(import protolk
        protolk-primitives
        protolk-internal)
(import-for-syntax protolk)

(use extras)


;;;;;;;;;;;;;;;;;;;
;; ESCAPSULATION
;;

(describe "own-prop"
  (it "returns the prop value from the active self"
    (let ((pob (make-pob props: '((a 1)))))
      (parameterize ((%method-context (list pob 'some-method)))
        (equal? (own-prop 'a) 1))))

  (it "raises a 'context error if there is no active self"
    (raises? (context)
      (own-prop 'a)))

  (it "is settable with set!"
    (let ((pob (make-pob props: '((a 1)))))
      (parameterize ((%method-context (list pob 'some-method)))
        (set! (own-prop 'a) 2)
        (equal? (own-prop 'a) 2)))))


(describe "set-own-prop!"
  (it "modifies the prop value in the active self"
    (let ((pob (make-pob props: '((a 1)))))
      (parameterize ((%method-context (list pob 'some-method)))
        (set-own-prop! 'a 2)
        (equal? (own-prop 'a) 2))))

  (it "returns #<unspecified> on success"
    (let ((pob (make-pob props: '((a 1)))))
      (parameterize ((%method-context (list pob 'some-method)))
        (equal? (set-own-prop! 'a 2) (void)))))

  (it "raises a 'context error if there is no active self"
    (raises? (context)
      (set-own-prop! 'a 2))))


(describe "assert-active-pob"
  (it "returns #t if the pob is the active pob"
    (let ((pob (make-pob props: '((a 1)))))
      (parameterize ((%method-context (list pob 'some-method)))
        (equal? (assert-active-pob pob) #t))))

  (it "raises a 'context error if the given pob is not the active pob"
    (let ((pob (make-pob)))
      (parameterize ((%method-context (list pob 'some-method)))
        (raises? (context)
          (assert-active-pob (make-pob))))))

  (it "raises a 'context error if given #f when there is no active pob"
    (raises? (context)
      (assert-active-pob #f)))

  (it "accepts an optional error message to use on failure"
    (let ((pob (make-pob))
          (pob2 (make-pob))
          (message "Fail!"))
      (parameterize ((%method-context (list pob 'some-method)))
        (let ((exn (raises? (context)
                     (assert-active-pob pob2 message))))
          (eq?
           (get-condition-property exn 'context 'message)
           message)))))

  (it "also uses the custom error message when there is no active pob"
    (let ((message "Fail!"))
      (parameterize ((%method-context #f))
        (let ((exn (raises? (context)
                     (assert-active-pob (make-pob) message))))
          (eq?
           (get-condition-property exn 'context 'message)
           message)))))
  
  (it "fails if given no args"
    (raises? (arity)
      (assert-active-pob))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))