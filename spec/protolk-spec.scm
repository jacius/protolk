
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


(test-exit)
