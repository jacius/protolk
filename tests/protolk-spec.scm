
(load-relative "helpers")
(load-relative "../protolk")
(import protolk
        protolk-primitives
        protolk-internal)

(use extras)


;;;;;;;;;;;;;
;; PRINTER
;;

(describe "pob record type printer"
  (define (display-foo self port) (display "foo" port))
  
  (define pob1 (make-pob))
  (define pob2 (std-derive pob1 methods: `((_display . ,display-foo))))
  (define pob3 (std-derive pob2))

  (it "uses std-_display if it has no _display method"
    (equal? (with-output-to-string
              (lambda () (display pob1)))
            "#<pob>"))
  
  (it "uses the pob's _display method if it has one"
    (equal? (with-output-to-string
              (lambda () (display pob2)))
            "foo"))

  (it "uses the _display method inherited from its ancestors"
    (equal? (with-output-to-string
              (lambda () (display pob3)))
            "foo")))



;;;;;;;;;;;;;;
;; CORE API
;;

(describe "make-pob"
  (define (fn pob) #t)
  (define (resprop pob) (cons pob #t))
  (define (resmeth pob) (cons pob fn))

  (it "accepts base, props, methods, resolve-prop, and resolve-method keyword args"
    (not (raises? ()
           (make-pob base: #f
                     props: '((a . 1))
                     methods: `((m . ,fn))
                     resolve-prop: resprop
                     resolve-method: resmeth))))

  (it "returns a pob with the specified contents"
    (let ((p (make-pob base: #f
                       props: '((a . 1))
                       methods: `((m . ,fn))
                       resolve-prop: resprop
                       resolve-method: resmeth)))
      (and (pob? p)
           (equal? (%pob-base p)           #f)
           (equal? (%pob-props p)          '((a . 1)))
           (equal? (%pob-methods p)        `((m . ,fn)))
           (equal? (%pob-resolve-prop p)   resprop)
           (equal? (%pob-resolve-method p) resmeth))))

  (it "allows all arguments to be omitted"
    (not (raises? ()
           (make-pob))))

  (it "initializes base to #f if omitted"
    (equal? #f
            (%pob-base (make-pob props: '((a . 1))
                                 methods: `((m . ,fn))
                                 resolve-prop: resprop
                                 resolve-method: resmeth))))

  (it "initializes props to the empty list if omitted"
    (equal? '()
            (%pob-props (make-pob base: #f
                                  methods: `((m . ,fn))
                                  resolve-prop: resprop
                                  resolve-method: resmeth))))

  (it "initializes methods to the empty list if omitted"
    (equal? '()
            (%pob-methods (make-pob base: #f
                                    props: '((a . 1))
                                    resolve-prop: resprop
                                    resolve-method: resmeth))))

  (it "initializes resolve-prop to std-resolve-prop if omitted"
    (equal? std-resolve-prop
            (%pob-resolve-prop (make-pob base: #f
                                         props: '((a . 1))
                                         methods: `((m . ,fn))
                                         resolve-method: resmeth))))

  (it "initializes resolve-method to std-resolve-method if omitted"
    (equal? std-resolve-method
            (%pob-resolve-method (make-pob base: #f
                                           props: '((a . 1))
                                           methods: `((m . ,fn))
                                           resolve-prop: resprop)))))


(describe "send"
  (define (noop . args) #t)
  (define base-pob
    (make-pob methods: `((_resolve-method . ,std-resolve-method)
                         (_resolve-prop   . ,std-resolve-prop)
                         (_method-missing . ,noop))))
  
  ;; it "uses the pob's resolve-method to find its _receive method"
  (let* ((pob (std-derive base-pob))
         (stub-resolve
          (lambda (self method-name #!optional default)
            (if (eq? method-name '_receive)
                (raise 'success "Success!")
                (%resolve-method self method-name default)))))
    (%pob-set-resolve-method! pob stub-resolve)
    (it "uses the pob's resolve-method to find its _receive method"
      (raises? (success)
        (send pob 'amethod 1 2 3))))

  ;; it "invokes the _receive method with the expected arguments"
  (let* ((pob (std-derive base-pob))
         (expected-args (list pob 'amethod '(1 2 3)))
         (stub-receive (lambda args
                         (if (equal? args expected-args)
                              (raise 'success "Success!")
                              (raise 'failure
                                     (sprintf "Expected args ~s, got ~s"
                                              expected-args args))))))
    (%set-method! pob '_receive stub-receive)
    (it "invokes the _receive method with the expected arguments"
      (raises? (success)
        (send pob 'amethod 1 2 3)))))


(describe "prop-reader"
  (it "accepts a prop name and returns a procedure"
    (procedure? (prop-reader 'some-prop)))

  (it "fails if given no args"
    (raises? (arity)
      (prop-reader)))

  (it "fails if given too many args"
    (raises? (arity)
      (prop-reader 'some-prop 'foo)))
  
  (describe "the procedure"
    (define some-prop-reader (prop-reader 'some-prop))
    (define pob1 (make-pob props: '((some-prop . some-value))))

    (it "returns the given pob's value for the matching prop"
      (equal? (some-prop-reader pob1)
              'some-value))

    (it "fails if given a non-pob"
      (raises? (type)
        (some-prop-reader 'foo)))
    
    (it "fails if given no args"
      (raises? (arity)
        (some-prop-reader)))

    (it "fails if given too many args"
      (raises? (arity)
        (some-prop-reader pob1 'foo)))))


(describe "prop-writer"
  (it "accepts a prop name and returns a procedure"
    (procedure? (prop-writer 'some-prop)))

  (it "fails if given no args"
    (raises? (arity)
      (prop-writer)))

  (it "fails if given too many args"
    (raises? (arity)
      (prop-writer 'some-prop 'foo)))
  
  (describe "the procedure"
    (define some-prop-writer (prop-writer 'some-prop))

    (it "sets the matching prop's value in the given pob"
      (let ((pob (make-pob)))
        (some-prop-writer pob 'some-value)
        (equal? (%prop pob 'some-prop)
                'some-value)))

    (it "returns void"
      (let ((pob (make-pob)))
        (equal? (some-prop-writer pob 'some-value)
                (void))))
    
    (it "fails if given a non-pob"
      (raises? (type)
        (some-prop-writer 'foo 'bar)))
    
    (it "fails if given no args"
      (raises? (arity)
        (some-prop-writer)))

    (it "fails if given too many args"
      (let ((pob (make-pob)))
        (raises? (arity)
          (some-prop-writer pob 'some-value 'foo))))))


;;;;;;;;;;;;;;;;;
;; STD METHODS
;;

(describe "std-derive"
  (define base-pob (make-pob))
  (define (fn pob) #t)
  (define (resprop pob) (cons pob #t))
  (define (resmeth pob) (cons pob fn))

  (it "fails when given #f as the base"
    (raises? (type)
      (std-derive #f)))

  (it "fails when given a non-pob as the base"
    (raises? (type)
      (std-derive 'notapob)))

  (it "fails when given no args"
    (raises? (arity)
      (std-derive)))

  (it "returns a derived pob with the specified contents"
    (let ((p (std-derive base-pob
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
           (p (std-derive b
                          methods: `((m . ,fn))
                          resolve-prop: resprop
                          resolve-method: resmeth)))
      (equal? (%pob-props p) '())))

  (it "initializes methods to the empty list if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (std-derive b
                          props: '((a . 1))
                          resolve-prop: resprop
                          resolve-method: resmeth)))
      (equal? (%pob-methods p) '())))

  (it "initializes resolve-prop to the base's resolve-prop if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (std-derive b
                          props: '((a . 1))
                          methods: `((m . ,fn))
                          resolve-method: resmeth)))
      (equal? (%pob-resolve-prop p) resprop)))

  (it "initializes resolve-method to the base's resolve-method if omitted"
    (let* ((b (make-pob resolve-prop: resprop
                        resolve-method: resmeth))
           (p (std-derive b
                          props: '((a . 1))
                          methods: `((m . ,fn))
                          resolve-prop: resprop)))
      (equal? (%pob-resolve-method p) resmeth))))


(describe "std-ancestors"
  (define pob1 (make-pob methods: `((ancestors . ,std-ancestors))))
  (define pob2 (std-derive pob1))
  (define pob3 (std-derive pob2))
  (define pob4 (std-derive pob3))

  (it "returns a list of all the pob's ancestors, most immediate first"
    (equal? (std-ancestors pob4)
            (list pob3 pob2 pob1)))

  (it "sends 'ancestors to the base to continue the lookup chain"
    (let* ((pob-a (make-pob))
           (pob-b (std-derive pob-a methods: `((ancestors . ,std-ancestors)))))
      (%set-method! pob-a '_receive
        (lambda (self message . args)
          (if (and (equal? self pob-a) (equal? message 'ancestors))
              (raise 'success "Success!")
              (apply std-_receive self message args))))
      (raises? (success)
        (std-ancestors pob-b))))

  (it "ends the lookup chain when it encounters a #f base"
    (let* ((pob-a (make-pob methods: `((ancestors . ,std-ancestors))))
           (pob-b (std-derive pob-a)))
      (equal? (std-ancestors pob-b) (list pob-a))))

  (it "returns an empty list if the pob's base is #f"
    (equal? (std-ancestors (make-pob base: #f))
            '()))

  (it "returns an empty list if the pob's base is unspecified"
    (equal? (std-ancestors (make-pob))
            '()))

  (it "fails when given a non-pob"
    (raises? (type)
      (std-ancestors #f))))


(describe "std-has-ancestor?"
  (define pob1 (make-pob methods: `((has-ancestor? . ,std-has-ancestor?))))
  (define pob2 (std-derive pob1))
  (define pob3 (std-derive pob2))
  (define pob2b (std-derive pob1))

  (it "returns #t if the second pob is an ancestor of the first pob"
    (std-has-ancestor? pob3 pob1))

  (it "returns #f if the second pob is not an ancestor of the first pob"
    (not (std-has-ancestor? pob3 pob2b)))

  (it "returns #f if both arguments are the same pob"
    (not (std-has-ancestor? pob2 pob2)))

  (it "returns #f if the second argument is #f"
    (not (std-has-ancestor? pob1 #f)))

  (it "returns #f if the second argument is not a pob"
    (not (std-has-ancestor? pob3 'foo)))

  (it "sends 'has-ancestor? to the base to continue the lookup"
    (let* ((pob-a (make-pob))
           (pob-b (std-derive pob-a))
           (pob-c (std-derive pob-b
                   methods: `((has-ancestor? . ,std-has-ancestor?)))))
      (%set-method! pob-b '_receive
        (lambda (self message . args)
          (if (and (equal? self pob-b) (equal? message 'has-ancestor?))
              (raise 'success "Success!")
              (apply std-_receive self message args))))
      (raises? (success)
        (std-has-ancestor? pob-c pob-a))))
  
  (it "fails when given no args"
    (raises? (arity)
      (std-has-ancestor?)))

  (it "fails when given only one pob"
    (raises? (arity)
      (std-has-ancestor? pob3)))

  (it "fails when given too many args"
    (raises? (arity)
      (std-has-ancestor? pob3 pob2 pob1)))

  (it "fails when the first argument is not a pob"
    (raises? (type)
      (std-has-ancestor? 'foo pob1))))


(describe "std-resolve-prop"
  (define pob1 (make-pob props: '((a . 1) (b . 2) (c . 3) (d . 4))))
  (define pob2 (std-derive pob1 props: `((a . 11) (c . ,(void)))))
  (define pob3 (std-derive pob2 props: '((b . 22))))

  (it "returns a list with self and the prop value, if self defines the prop"
    (equal? (std-resolve-prop pob3 'b) (cons pob3 22)))

  (it "returns a cons with the nearest ancestor that defines the prop, and the prop value"
    (equal? (std-resolve-prop pob3 'a) (cons pob2 11)))

  (it "searches ancestors recursively to find the prop value"
    (equal? (std-resolve-prop pob3 'd) (cons pob1 4)))
 
  (it "returns #f and the default value if the prop is not found"
    (equal? (std-resolve-prop pob3 'z 'default) (cons #f 'default)))

  (it "uses #<unspecified> as the default value by default"
    (equal? (std-resolve-prop pob3 'z) (cons #f (void))))

  (it "stops searching if it ever finds the prop defined as #<unspecified>"
    (equal? (std-resolve-prop pob3 'c) (cons pob2 (void))))

  (it "fails if given no args"
    (raises? (arity)
      (std-resolve-prop)))

  (it "fails if the prop name is omitted"
    (raises? (arity)
      (std-resolve-prop pob3)))

  (it "does not fail if given the pob and prop name"
    (not (raises? ()
           (std-resolve-prop pob3 'a))))

  (it "does not fail if given the pob, prop name, and default value"
    (not (raises? ()
           (std-resolve-prop pob3 'a 'default))))
  
  (it "fails if given too many args"
    (raises? ()
      (std-resolve-prop pob3 'a 'b 'c)))

  (it "fails if given a non-pob for the first arg"
    (raises? (type)
      (std-resolve-prop 'foo 'a))))


(describe "std-resolve-method"
  (define (fn1 self) 1)
  (define (fn2 self) 2)
  (define (fn3 self) 3)
  (define (fn4 self) 4)
  (define (fn5 self) 5)
  (define (fn6 self) 6)

  (define pob1
    (make-pob methods: `((m . ,fn1) (n . ,fn2) (o . ,fn3) (p . ,fn4))))
  (define pob2
    (std-derive pob1 methods: `((m . ,fn5) (o . ,(void)))))
  (define pob3
    (std-derive pob2 methods: `((n . ,fn6))))

  (it "returns a pair with self and the definition if self defines it"
    (equal? (std-resolve-method pob3 'n) (cons pob3 fn6)))

  (it "returns a pair with the nearest ancestor that defines the method, and the definition"
    (equal? (std-resolve-method pob3 'm) (cons pob2 fn5)))

  (it "searches ancestors recursively to find the definition"
    (equal? (std-resolve-method pob3 'p) (cons pob1 fn4)))
 
  (it "returns #f and the default value if the method is not found"
    (equal? (std-resolve-method pob3 'z 'default) (cons #f 'default)))

  (it "uses #<unspecified> as the default value by default"
    (equal? (std-resolve-method pob3 'z) (cons #f (void))))
  
  (it "stops searching if it ever finds the method defined as #<unspecified>"
    (equal? (std-resolve-method pob3 'o) (cons pob2 (void))))

  (it "fails if given no args"
    (raises? (arity)
      (std-resolve-method)))

  (it "fails if the method name is omitted"
    (raises? (arity)
      (std-resolve-method pob3)))

  (it "does not fail if given the pob and method name"
    (not (raises? ()
           (std-resolve-method pob3 'm))))

  (it "does not fail if given the pob, method name, and default value"
    (not (raises? ()
           (std-resolve-method pob3 'm 'default))))
  
  (it "fails if given too many args"
    (raises? ()
      (std-resolve-method pob3 'm 'default 'o)))

  (it "fails if given a non-pob for the first arg"
    (raises? (type)
      (std-resolve-method 'foo 'm))))


(describe "std-_method-missing"
  (define pob1 (make-pob))

  (it "fails when given only one arg"
    (raises? (arity)
      (std-_method-missing pob1)))

  (it "fails when given only two args"
    (raises? (arity)
      (std-_method-missing pob1 'badmethod)))

  (it "fails when given too many args"
    (raises? (arity)
      (std-_method-missing pob1 'badmethod '(1 2 3) 'foo)))

  (it "raises a 'no-method exception as part of normal operation"
    (raises? (no-method)
      (std-_method-missing pob1 'badmethod '(1 2 3))))

  (describe "the 'no-method exception"
    (define exn (raises? (no-method)
                  (std-_method-missing
                   pob1 'badmethod '(1 2 3))))

   (it "contains an informative error message"
     (string=? (get-condition-property exn 'no-method 'message)
               "undefined method 'badmethod for #<pob>"))

   (it "contains a 'pob property with the pob who received the message"
     (equal? (get-condition-property exn 'no-method 'pob)
             pob1))
   
   (it "contains a 'method-name property with the method name"
     (equal? (get-condition-property exn 'no-method 'method-name)
             'badmethod))

   (it "contains an 'args property with the args intended for the method"
     (equal? (get-condition-property exn 'no-method 'args)
             '(1 2 3)))))


(describe "std-_receive"
  (define (noop . args) #t)
  (define base-pob
    (make-pob methods: `((_resolve-method . ,std-resolve-method)
                         (_resolve-prop   . ,std-resolve-prop)
                         (_method-missing . ,std-_method-missing))))

  (it "uses the pob's resolve-method to find a matching method"
    (let* ((stub-resolve (lambda args (raise 'success "Success!")))
           (pob (std-derive base-pob
                 resolve-method: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "invokes the matching method if found"
    (let* ((stub-method (lambda args (raise 'success "Success!")))
           (stub-resolve (lambda args (cons 'some-pob stub-method)))
           (pob (std-derive base-pob
                 resolve-method: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "resolves _method-missing if no matching method was found"
    (let* ((stub-resolve (lambda (self method-name #!optional default)
                           (case method-name
                             ((amethod) (cons #f default))
                             ((_method-missing)
                              (raise 'success "Success!")))))
           (pob (std-derive base-pob
                 resolve-method: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "invokes _method-missing if no matching method was found"
    (let* ((stub-mm (lambda args (raise 'success "Success")))
           (stub-resolve (lambda (self method-name #!optional default)
                           (case method-name
                             ((amethod) (cons #f default))
                             ((_method-missing) (cons 'some-pob stub-mm)))))
           (pob (std-derive base-pob
                 resolve-method: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "fails if given only a pob"
    (raises? (arity)
      (std-_receive base-pob)))

  (it "fails if given only a pob and method name"
    (raises? (arity)
      (std-_receive base-pob 'amethod)))

  (it "fails if given too many args"
    (raises? (arity)
      (std-_receive base-pob 'amethod '(1 2) 'foo))))


(describe "std-responds-to?"
  (define (fn self) #t)

  (define pob1
    (make-pob methods: `((a . ,fn) (x . ,(void))
                         (responds-to? . ,std-responds-to?))))
  (define pob2
    (std-derive pob1 methods: `((b . ,fn) (y . ,(void)))))
  (define pob3
    (std-derive pob2 methods: `((c . ,fn) (z . ,(void)))))

  (it "returns #t if the pob has its own matching method"
    (std-responds-to? pob3 'c))
  (it "returns #t if the pob inherits a matching method from its base"
    (std-responds-to? pob3 'b))
  (it "returns #t if the pob inherits a matching method from any ancestor"
    (std-responds-to? pob3 'a))

  (it "returns #f if neither the pob nor ancestors have a matching method"
    (not (std-responds-to? pob3 'foo)))
  
  (it "returns #f if the pob defines the matching method as #<unspecified>"
    (not (std-responds-to? pob3 'z)))
  (it "returns #f if the pob inherits #<unspecified> from its base"
    (not (std-responds-to? pob3 'y)))
  (it "returns #f if the pob inherits #<unspecified> from any ancestor"
    (not (std-responds-to? pob3 'x)))

  (it "accepts (but ignores) any number of args after the message"
    (not (raises? ()
           (std-responds-to? pob3 'a 1 2 3 4 5 6 7 8 9 0))))

  (it "sends 'responds-to? to the base to continue the lookup"
    (let* ((pob-a (make-pob))
           (pob-b (std-derive pob-a
                   methods: `((responds-to? . ,std-responds-to?)))))
      (%set-method! pob-a '_receive
        (lambda (self message . args)
          (if (and (equal? self pob-a) (equal? message 'responds-to?))
              (raise 'success "Success!")
              (apply std-_receive self message args))))
      (raises? (success)
        (std-responds-to? pob-b 'foo))))
  
  (it "fails if given no args"
    (raises? (arity) (std-responds-to?)))
  
  (it "fails if the message is omitted"
    (raises? (arity) (std-responds-to? pob3))))


(describe "std-_display"
  (define pob (make-pob))
  
  (it "writes \"#<pob>\" to the given port"
    (equal? (call-with-output-string
             (lambda (port)
               (std-_display pob port)))
            "#<pob>"))

  (it "uses (current-output-port) if the port is omitted"
    (equal? (with-output-to-string
              (lambda () (std-_display pob)))
            "#<pob>"))

  (it "fails if given a non-pob"
    (raises? (type)
      (std-_display 'foo (current-output-port))))

  (it "fails if the port is not a port"
    (raises? (type)
      (std-_display pob 'foo))))



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
  
  (it "has a 'derive method set to std-derive"
    (equal? (%method stdpob 'derive) std-derive))

  (it "has an 'ancestors method set to std-ancestors"
    (equal? (%method stdpob 'ancestors) std-ancestors))

  (it "has a 'has-ancestor? method set to std-has-ancestor?"
    (equal? (%method stdpob 'has-ancestor?) std-has-ancestor?))

  (it "has a '_method-missing method set to std-_method-missing"
    (equal? (%method stdpob '_method-missing) std-_method-missing))

  (it "has a '_receive method set to std-_receive"
    (equal? (%method stdpob '_receive) std-_receive))

  (it "has a 'responds-to? method set to std-responds-to?"
    (equal? (%method stdpob 'responds-to?) std-responds-to?))

  (it "has a '_display method set to std-_display?"
    (equal? (%method stdpob '_display) std-_display)))



(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
