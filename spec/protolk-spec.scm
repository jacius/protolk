
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
    (equal? (%method stdpob 'derive) stdpob-derive)))


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


(test-exit)
