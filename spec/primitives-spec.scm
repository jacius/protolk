
(load-relative "helpers")

(load-relative "../primitives")
(import protolk-primitives)


(describe "pob pritimive record type"

  (describe "%make-pob"
    (it "succeeds when given a props alist and a methods alist"
      (not (raises-error? (%make-pob '() '()))))
    (it "fails when given no args"
      (raises-error? (%make-pob)))
    (it "fails when given only one arg"
      (raises-error? (%make-pob '())))
    (it "fails when given too many args"
      (raises-error? (%make-pob '() '() '()))))

  (describe "pob?"
    (it "returns #t when given a pob"
      (pob? (%make-pob '() '())))
    (it "returns #f when given a non-pob"
      (not (pob? 'foo)))
    (it "fails when given no args"
      (raises-error? (pob?))))

  
  (describe "%pob-props"
    (it "returns the pob's props alist"
      (equal? (%pob-props (%make-pob '((a . 1)) '()))
              '((a . 1))))
    (it "fails when given a non-pob"
      (raises-error? (%pob-props 'foo)))
    (it "fails when given no args"
      (raises-error? (%pob-props))))

  (describe "%pob-set-props!"
    (it "modifies the pob's props alist"
      (let ((pob (%make-pob '() '())))
        (%pob-set-props! pob '((a . 1)))
        (equal? (%pob-props pob)
                '((a . 1)))))
    (it "fails when given a non-pob"
      (raises-error? (%pob-set-props! 'foo '((a . 1)))))
    (it "fails when given no args"
      (raises-error? (%pob-set-props!)))
    (it "fails when given only one arg"
      (raises-error? (%pob-set-props! (%make-pob '() '())))))

  
  (describe "%pob-methods"
    (it "returns the pob's methods alist"
      (let ((meth (lambda (pob) #t)))
        (equal? (%pob-methods (%make-pob '() `((m . ,meth))))
                `((m . ,meth)))))
    (it "fails when given a non-pob"
      (raises-error? (%pob-methods 'foo)))
    (it "fails when given no args"
      (raises-error? (%pob-methods))))

  (describe "%pob-set-methods!"
    (let ((fn (lambda (pob) #t)))
      (it "modifies the pob's methods alist"
        (let ((pob (%make-pob '() '())))
          (%pob-set-methods! pob `((m . ,fn)))
          (equal? (%pob-methods pob)
                  `((m . ,fn)))))
      (it "fails when given a non-pob"
        (raises-error? (%pob-set-methods! 'foo `((m . ,fn)))))
      (it "fails when given no args"
        (raises-error? (%pob-set-methods!)))
      (it "fails when given only one arg"
        (raises-error? (%pob-set-methods! (%make-pob '() '())))))))


(test-exit)
