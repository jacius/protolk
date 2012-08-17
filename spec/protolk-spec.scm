
(load-relative "helpers")

(load-relative "../protolk")
(import protolk
        protolk-primitives)


;;;;;;;;;;;;;;
;; CORE API
;;

(describe "make-pob"
  (define (fn pob) #t)

  (it "accepts #:props and #:methods keyword arguments"
    (not (raises-error? (make-pob props: '((a . 1))
                                  methods: `((m . ,fn))))))

  (it "returns a pob with the specified props and methods"
    (let ((p (make-pob props: '((a . 1)) methods: `((m . ,fn)))))
      (and (pob? p)
           (equal? (%pob-props p) '((a . 1)))
           (equal? (%pob-methods p) `((m . ,fn))))))

  (it "allows the props argument to be omitted"
    (not (raises-error? (make-pob methods: `((m . ,fn))))))

  (it "allows the methods argument to be omitted"
    (not (raises-error? (make-pob props: `((a . 1))))))

  (it "allows both arguments to be omitted"
    (not (raises-error? (make-pob))))
  
  (it "returns a pob with no props if the props argument is omitted"
    (equal? (%pob-props (make-pob methods: `((m . ,fn)))) '()))
   
  (it "returns a pob with no methods if the props argument is omitted"
    (equal? (%pob-methods (make-pob props: `((a . 1)))) '()))

  (it "returns a pob with no props or methods if both arguments are omitted"
    (let ((p (make-pob)))
      (and (equal? (%pob-props p) '())
           (equal? (%pob-methods p) '())))))


;;;;;;;;;;;;
;; STDPOB
;;

(describe "stdpob"
  (it "should be a pob already provided by protolk"
    (pob? stdpob))

  (it "should have a 'base prop set to #f"
    (equal? (%prop stdpob 'base) #f))

  (it "should have a 'derive method set to stdpob-derive"
    (equal? (%method stdpob 'derive) stdpob-derive))

  (it "should have an 'ancestors method set to stdpob-ancestors"
    (equal? (%method stdpob 'ancestors) stdpob-ancestors))

  (it "should have a 'has-ancestor? method set to stdpob-has-ancestor?"
    (equal? (%method stdpob 'has-ancestor?) stdpob-has-ancestor?)))


(describe "stdpob-derive"
  (define base-pob (make-pob))
  (define (fn pob) #t)

  (it "returns a new pob based on the given pob"
    (equal? (%prop (stdpob-derive base-pob) 'base) base-pob))

  (it "allows the base pob to be #f"
    (equal? (%prop (stdpob-derive #f) 'base) #f))

  (it "fails when given a non-pob other than #f"
    (raises-error? (stdpob-derive 'foo)))

  (it "fails when the pob argument is omitted"
    (raises-error? (stdpob-derive)))

  (it "sets the specified props to the new pob"
    (let ((new-pob (stdpob-derive base-pob #:props '((a . 1)))))
      (equal? (%prop new-pob 'a) 1)))

  (it "sets no props (except base) if #:props is omitted"
    (let ((new-pob (stdpob-derive base-pob)))
      (equal? (%pob-props new-pob) `((base . ,base-pob)))))

  (it "sets the specified methods to the new pob"
    (let ((new-pob (stdpob-derive base-pob #:methods `((m . ,fn)))))
      (equal? (%method new-pob 'm) fn)))

  (it "sets no methods if #:methods is omitted"
    (let ((new-pob (stdpob-derive base-pob)))
      (equal? (%pob-methods new-pob) '()))))


(describe "stdpob-ancestors"
  (define pob1 (make-pob props: '((base . #f))))
  (define pob2 (stdpob-derive pob1))
  (define pob3 (stdpob-derive pob2))
  (define pob4 (stdpob-derive pob3))

  (it "returns a list of all the pob's ancestors, most immediate first"
    (equal? (stdpob-ancestors pob4) (list pob3 pob2 pob1)))

  (it "ends the lookup chain when it encounters a non-pob base"
    (let* ((pob-a (make-pob props: '((base . foo))))
           (pob-b (stdpob-derive pob-a)))
      (equal? (stdpob-ancestors pob-b) (list pob-a))))
  
  (it "returns an empty list if the pob's base is #f"
    (equal? (stdpob-ancestors (make-pob props: '((base . #f)))) '()))

  (it "returns an empty list if the pob's base is unspecified"
    (equal? (stdpob-ancestors (make-pob)) '()))

  (it "fails when given a non-pob"
    (raises-error? (stdpob-ancestors #f))))


(describe "stdpob-has-ancestor?"
  (define pob1 (make-pob props: '((base . #f))))
  (define pob2 (stdpob-derive pob1))
  (define pob3 (stdpob-derive pob2))
  (define pob2b (stdpob-derive pob1))

  (it "returns #t if the second pob is an ancestor of the first pob"
    (stdpob-has-ancestor? pob3 pob1))

  (it "returns #f if the second pob is not an ancestor of the first pob"
    (not (stdpob-has-ancestor? pob3 pob2b)))

  (it "returns #f if both arguments are the same pob"
    (not (stdpob-has-ancestor? pob2 pob2)))

  (it "returns #f if the second argument is #f"
    (not (stdpob-has-ancestor? pob1 #f)))

  (it "returns #f if the second argument is not a pob"
    (not (stdpob-has-ancestor? pob3 'foo)))
  
  (it "fails when given no args"
    (raises-error? (stdpob-has-ancestor?)))

  (it "fails when given only one pob"
    (raises-error? (stdpob-has-ancestor? pob3)))

  (it "fails when given too many pobs"
    (raises-error? (stdpob-has-ancestor? pob3 pob2 pob1)))

  (it "fails when the first argument is not a pob"
    (raises-error? (stdpob-has-ancestor? 'foo pob1))))


(test-exit)
