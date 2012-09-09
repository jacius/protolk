

(load-relative "helpers")
(load-relative "../protolk-stdpob")
(import protolk-stdpob
        protolk
        protolk-primitives
        protolk-internal)


(use extras)


;;;;;;;;;;;;;;;;;
;; STDPOB METHODS
;;

(describe "stdpob-derive"
  (define base-pob (make-pob))
  (define (fn pob) #t)
  (define (resprop pob) (cons pob #t))
  (define (resmeth pob) (cons pob fn))

  (it "fails when given #f as the base"
    (raises? (type)
      (stdpob-derive #f)))

  (it "fails when given a non-pob as the base"
    (raises? (type)
      (stdpob-derive 'notapob)))

  (it "fails when given no args"
    (raises? (arity)
      (stdpob-derive)))

  (it "returns a derived pob with the specified contents"
    (let ((p (stdpob-derive base-pob
                         props: '((a . 1))
                         methods: `((m . ,fn))
                         resolve-prop: resprop
                         resolve-method: resmeth)))
      (and (pob? p)
           (equal? (%pob-base p)           base-pob)
           (equal? (%pob-props p)          '((a . 1)))
           (equal? (%pob-methods p)        `((m . ,fn)))
           (equal? (%pob-resolve-prop p)   resprop)
           (equal? (%pob-resolve-method p) resmeth))))

  (it "initializes props to the empty list if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (stdpob-derive b
                          methods: `((m . ,fn))
                          resolve-prop: resprop
                          resolve-method: resmeth)))
      (equal? (%pob-props p) '())))

  (it "initializes methods to the empty list if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (stdpob-derive b
                          props: '((a . 1))
                          resolve-prop: resprop
                          resolve-method: resmeth)))
      (equal? (%pob-methods p) '())))

  (it "initializes resolve-prop to the base's resolve-prop if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (stdpob-derive b
                          props: '((a . 1))
                          methods: `((m . ,fn))
                          resolve-method: resmeth)))
      (equal? (%pob-resolve-prop p) resprop)))

  (it "initializes resolve-method to the base's resolve-method if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (stdpob-derive b
                          props: '((a . 1))
                          methods: `((m . ,fn))
                          resolve-prop: resprop)))
      (equal? (%pob-resolve-method p) resmeth))))


(describe "stdpob-ancestors"
  (define pob1 (make-pob methods: `((ancestors . ,stdpob-ancestors))))
  (define pob2 (stdpob-derive pob1))
  (define pob3 (stdpob-derive pob2))
  (define pob4 (stdpob-derive pob3))

  (it "returns a list of all the pob's ancestors, most immediate first"
    (equal? (stdpob-ancestors pob4)
            (list pob3 pob2 pob1)))

  (it "sends 'ancestors to the base to continue the lookup chain"
    (let* ((pob-a (make-pob))
           (pob-b (stdpob-derive pob-a methods: `((ancestors . ,stdpob-ancestors)))))
      (%set-method! pob-a '_receive
        (lambda (self message . args)
          (if (and (equal? self pob-a) (equal? message 'ancestors))
              (raise 'success "Success!")
              (apply std-_receive self message args))))
      (raises? (success)
        (stdpob-ancestors pob-b))))

  (it "ends the lookup chain when it encounters a #f base"
    (let* ((pob-a (make-pob methods: `((ancestors . ,stdpob-ancestors))))
           (pob-b (stdpob-derive pob-a)))
      (equal? (stdpob-ancestors pob-b) (list pob-a))))

  (it "returns an empty list if the pob's base is #f"
    (equal? (stdpob-ancestors (make-pob base: #f))
            '()))

  (it "returns an empty list if the pob's base is unspecified"
    (equal? (stdpob-ancestors (make-pob))
            '()))

  (it "fails when given a non-pob"
    (raises? (type)
      (stdpob-ancestors #f))))


(describe "stdpob-has-ancestor?"
  (define pob1 (make-pob methods: `((has-ancestor? . ,stdpob-has-ancestor?))))
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

  (it "sends 'has-ancestor? to the base to continue the lookup"
    (let* ((pob-a (make-pob))
           (pob-b (stdpob-derive pob-a))
           (pob-c (stdpob-derive pob-b
                   methods: `((has-ancestor? . ,stdpob-has-ancestor?)))))
      (%set-method! pob-b '_receive
        (lambda (self message . args)
          (if (and (equal? self pob-b) (equal? message 'has-ancestor?))
              (raise 'success "Success!")
              (apply std-_receive self message args))))
      (raises? (success)
        (stdpob-has-ancestor? pob-c pob-a))))
  
  (it "fails when given no args"
    (raises? (arity)
      (stdpob-has-ancestor?)))

  (it "fails when given only one pob"
    (raises? (arity)
      (stdpob-has-ancestor? pob3)))

  (it "fails when given too many args"
    (raises? (arity)
      (stdpob-has-ancestor? pob3 pob2 pob1)))

  (it "fails when the first argument is not a pob"
    (raises? (type)
      (stdpob-has-ancestor? 'foo pob1))))


(describe "stdpob-responds-to?"
  (define (fn self) #t)

  (define pob1
    (make-pob methods: `((a . ,fn) (x . ,(void))
                         (responds-to? . ,stdpob-responds-to?))))
  (define pob2
    (stdpob-derive pob1 methods: `((b . ,fn) (y . ,(void)))))
  (define pob3
    (stdpob-derive pob2 methods: `((c . ,fn) (z . ,(void)))))

  (it "returns #t if the pob has its own matching method"
    (stdpob-responds-to? pob3 'c))
  (it "returns #t if the pob inherits a matching method from its base"
    (stdpob-responds-to? pob3 'b))
  (it "returns #t if the pob inherits a matching method from any ancestor"
    (stdpob-responds-to? pob3 'a))

  (it "returns #f if neither the pob nor ancestors have a matching method"
    (not (stdpob-responds-to? pob3 'foo)))
  
  (it "returns #f if the pob defines the matching method as #<unspecified>"
    (not (stdpob-responds-to? pob3 'z)))
  (it "returns #f if the pob inherits #<unspecified> from its base"
    (not (stdpob-responds-to? pob3 'y)))
  (it "returns #f if the pob inherits #<unspecified> from any ancestor"
    (not (stdpob-responds-to? pob3 'x)))

  (it "accepts (but ignores) any number of args after the message"
    (not (raises? ()
           (stdpob-responds-to? pob3 'a 1 2 3 4 5 6 7 8 9 0))))

  (it "sends 'responds-to? to the base to continue the lookup"
    (let* ((pob-a (make-pob))
           (pob-b (stdpob-derive pob-a
                   methods: `((responds-to? . ,stdpob-responds-to?)))))
      (%set-method! pob-a '_receive
        (lambda (self message . args)
          (if (and (equal? self pob-a) (equal? message 'responds-to?))
              (raise 'success "Success!")
              (apply std-_receive self message args))))
      (raises? (success)
        (stdpob-responds-to? pob-b 'foo))))
  
  (it "fails if given no args"
    (raises? (arity) (stdpob-responds-to?)))
  
  (it "fails if the message is omitted"
    (raises? (arity) (stdpob-responds-to? pob3))))


;;;;;;;;;;;;
;; STDPOB
;;

(describe "stdpob"
  (it "is a pob that exists by default"
    (pob? stdpob))

  (it "has base set to #f"
    (equal? (%pob-base stdpob) #f))

  (it "has resolve-prop sot to std-resolve-prop"
    (equal? (%pob-resolve-prop stdpob) std-resolve-prop))

  (it "has resolve-method set to std-resolve-method"
    (equal? (%pob-resolve-method stdpob) std-resolve-method))
  
  (it "has a '_display method set to std-_display?"
    (equal? (%method stdpob '_display) std-_display))

  (it "has a '_receive method set to std-_receive"
    (equal? (%method stdpob '_receive) std-_receive))

  (it "has a '_method-missing method set to std-_method-missing"
    (equal? (%method stdpob '_method-missing) std-_method-missing))

  (it "has a 'derive method set to stdpob-derive"
    (equal? (%method stdpob 'derive) stdpob-derive))

  (it "has an 'ancestors method set to stdpob-ancestors"
    (equal? (%method stdpob 'ancestors) stdpob-ancestors))

  (it "has a 'has-ancestor? method set to stdpob-has-ancestor?"
    (equal? (%method stdpob 'has-ancestor?) stdpob-has-ancestor?))

  (it "has a 'responds-to? method set to stdpob-responds-to?"
    (equal? (%method stdpob 'responds-to?) stdpob-responds-to?)))



(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
