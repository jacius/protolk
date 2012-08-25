
(load-relative "helpers")

(load-relative "../protolk-primitives")
(import protolk-primitives)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POB PRITIMIVE RECORD TYPE
;;

(describe "pob pritimive record type"

  (describe "%make-pob"
    (it "succeeds when given a props alist and a methods alist"
      (not (raises-exception? ()
             (%make-pob '() '()))))
    (it "fails when given no args"
      (raises-exception? ()
        (%make-pob)))
    (it "fails when given only one arg"
      (raises-exception? ()
        (%make-pob '())))
    (it "fails when given too many args"
      (raises-exception? ()
        (%make-pob '() '() '()))))

  (describe "pob?"
    (it "returns #t when given a pob"
      (pob? (%make-pob '() '())))
    (it "returns #f when given a non-pob"
      (not (pob? 'foo)))
    (it "fails when given no args"
      (raises-exception? ()
        (pob?))))

  
  (describe "%pob-props"
    (it "returns the pob's props alist"
      (equal? (%pob-props (%make-pob '((a . 1)) '()))
              '((a . 1))))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%pob-props 'foo)))
    (it "fails when given no args"
      (raises-exception? ()
        (%pob-props))))

  (describe "%pob-set-props!"
    (it "modifies the pob's props alist"
      (let ((pob (%make-pob '() '())))
        (%pob-set-props! pob '((a . 1)))
        (equal? (%pob-props pob)
                '((a . 1)))))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%pob-set-props! 'foo '((a . 1)))))
    (it "fails when given no args"
      (raises-exception? ()
        (%pob-set-props!)))
    (it "fails when given only one arg"
      (raises-exception? ()
        (%pob-set-props! (%make-pob '() '())))))

  (define (fn pob) #t)
  
  (describe "%pob-methods"
    (it "returns the pob's methods alist"
      (equal? (%pob-methods (%make-pob '() `((m . ,fn))))
              `((m . ,fn))))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%pob-methods 'foo)))
    (it "fails when given no args"
      (raises-exception? ()
        (%pob-methods))))

  (describe "%pob-set-methods!"
    (it "modifies the pob's methods alist"
      (let ((pob (%make-pob '() '())))
        (%pob-set-methods! pob `((m . ,fn)))
        (equal? (%pob-methods pob)
                `((m . ,fn)))))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%pob-set-methods! 'foo `((m . ,fn)))))
    (it "fails when given no args"
      (raises-exception? ()
        (%pob-set-methods!)))
    (it "fails when given only one arg"
      (raises-exception? ()
        (%pob-set-methods! (%make-pob '() '()))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE PROP ACCESSORS
;;

(describe "primitive prop accessors"

  (describe "%has-prop?"
    (it "returns #t when the pob has a matching prop"
      (%has-prop? (%make-pob '((a . 1)) '()) 'a))
    (it "returns #f when the pob does not have a matching prop"
      (not (%has-prop? (%make-pob '((a . 1)) '()) 'foo)))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%has-prop? 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises-exception? ()
        (%has-prop? (%make-pob '((a . 1)) '())))))

  (describe "%prop"
    (it "returns the value of a matching prop"
      (equal? (%prop (%make-pob '((a . 1)) '()) 'a)
              1))
    (it "returns #<unspecified> when there is no matching prop"
      (equal? (%prop (%make-pob '((a . 1)) '()) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%prop (%make-pob '((a . 1)) '()) 'foo 'result)
              'result))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%prop 'foo 'bar)))
    (it "fails when no prop name is specified"
      (raises-exception? ()
        (%prop (%make-pob '((a . 1)) '())))))

  (describe "%set-prop!"
    (it "sets the specified prop in the pob"
      (let ((pob (%make-pob '() '())))
        (%set-prop! pob 'a 1)
        (equal? (%prop pob 'a) 1)))
    (it "replaces the value of existing props with that name"
      (let ((pob (%make-pob '((a . 1)) '())))
        (%set-prop! pob 'a 2)
        (equal? (%prop pob 'a) 2)))
    (it "fails when given no args"
      (raises-exception? ()
        (%set-prop!)))    
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%set-prop! 'foo 'a 1)))
    (it "fails when no prop name or value is specified"
      (raises-exception? ()
        (%set-prop! (%make-pob '() '()))))
    (it "fails when no value is specified"
      (raises-exception? ()
        (%set-prop (%make-pob '() '()) 'a))))

  (describe "%unset-prop!"
    (it "removes all matching props from the pob"
      (let ((pob (%make-pob '((a . 2) (a . 1)) '())))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "has no effect if there are no matching props"
      (let ((pob (%make-pob '() '())))
        (%unset-prop! pob 'a)
        (not (%has-prop? pob 'a))))
    (it "fails when given no args"
      (raises-exception? ()
        (%unset-prop!)))    
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%unset-prop! 'foo 'a)))
    (it "fails when no prop name is specified"
      (raises-exception? ()
        (%unset-prop! (%make-pob '() '()))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMITIVE METHOD ACCESSORS
;;

(describe "primitive method accessors"
  (define (fn pob) #t)
  (define (fn2 pob) #f)

  (describe "%has-method?"
    (it "returns #t when the pob has a matching method"
      (%has-method? (%make-pob '() `((m . ,fn))) 'm))
    (it "returns #f when the pob does not have a matching method"
      (not (%has-method? (%make-pob '() `((m . ,fn))) 'foo)))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%has-method? 'foo 'm)))
    (it "fails when no method name is specified"
      (raises-exception? ()
        (%has-method? (%make-pob '() `((m . ,fn)))))))

  (describe "%method"
    (it "returns the value of a matching method"
      (equal? (%method (%make-pob '() `((m . ,fn))) 'm)
              fn))
    (it "returns #<unspecified> when there is no matching method"
      (equal? (%method (%make-pob '() `((m . ,fn))) 'foo)
              (void)))
    (it "optionally accepts a default return value"
      (equal? (%method (%make-pob '() `((m . ,fn))) 'foo fn)
              fn))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%method 'foo 'm)))
    (it "fails when no method name is specified"
      (raises-exception? ()
        (%method (%make-pob '() `((m . ,fn)))))))

  (describe "%set-method!"
    (it "sets the specified method in the pob"
      (let ((pob (%make-pob '() '())))
        (%set-method! pob 'm fn)
        (equal? (%method pob 'm) fn)))
    (it "replaces the value of existing methods with that name"
      (let ((pob (%make-pob '() `((m . ,fn)))))
        (%set-method! pob 'm fn)
        (equal? (%method pob 'm) fn)))
    (it "fails when given no args"
      (raises-exception? ()
        (%set-method!)))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%set-method! 'foo 'm fn)))
    (it "fails when no method name or value is specified"
      (raises-exception? ()
        (%set-method! (%make-pob '() '()))))
    (it "fails when no value is specified"
      (raises-exception? ()
        (%set-method (%make-pob '() '()) 'fn))))

  (describe "%unset-method!"
    (it "removes all matching methods from the pob"
      (let ((pob (%make-pob '() `((m . fn2) (m . fn)))))
        (%unset-method! pob 'm)
        (not (%has-method? pob 'm))))
    (it "has no effect if there are no matching methods"
      (let ((pob (%make-pob '() '())))
        (not (%has-method? pob 'm))))
    (it "fails when given no args"
      (raises-exception? ()
        (%unset-method!)))
    (it "fails when given a non-pob"
      (raises-exception? ()
        (%unset-method! 'foo 'a)))
    (it "fails when no method name is specified"
      (raises-exception? ()
        (%unset-method! (%make-pob '() '()))))))


(test-exit)
