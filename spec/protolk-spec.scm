
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
  (define pob2 (make-pob base: pob1 methods: `((_display . ,display-foo))))
  (define pob3 (make-pob base: pob2))

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

  (it "accepts base, props, methods, prop-resolver, and method-resolver keyword args"
    (not (raises? ()
           (make-pob base: #f
                     props: '((a . 1))
                     methods: `((m . ,fn))
                     prop-resolver: resprop
                     method-resolver: resmeth))))

  (it "returns a pob with the specified contents"
    (let ((p (make-pob base: #f
                       props: '((a . 1))
                       methods: `((m . ,fn))
                       prop-resolver: resprop
                       method-resolver: resmeth)))
      (and (pob? p)
           (equal? (%pob-base p)           #f)
           (equal? (%pob-props p)          '((a . 1)))
           (equal? (%pob-methods p)        `((m . ,fn)))
           (equal? (%pob-prop-resolver p)   resprop)
           (equal? (%pob-method-resolver p) resmeth))))

  (it "allows all arguments to be omitted"
    (not (raises? ()
           (make-pob))))

  (it "initializes base to #f if omitted"
    (equal? #f
            (%pob-base (make-pob props: '((a . 1))
                                 methods: `((m . ,fn))
                                 prop-resolver: resprop
                                 method-resolver: resmeth))))

  (it "initializes props to the empty list if omitted"
    (equal? '()
            (%pob-props (make-pob base: #f
                                  methods: `((m . ,fn))
                                  prop-resolver: resprop
                                  method-resolver: resmeth))))

  (it "initializes methods to the empty list if omitted"
    (equal? '()
            (%pob-methods (make-pob base: #f
                                    props: '((a . 1))
                                    prop-resolver: resprop
                                    method-resolver: resmeth))))

  (it "initializes prop-resolver to std-prop-resolver if omitted"
    (equal? std-prop-resolver
            (%pob-prop-resolver (make-pob base: #f
                                         props: '((a . 1))
                                         methods: `((m . ,fn))
                                         method-resolver: resmeth))))

  (it "initializes method-resolver to std-method-resolver if omitted"
    (equal? std-method-resolver
            (%pob-method-resolver (make-pob base: #f
                                           props: '((a . 1))
                                           methods: `((m . ,fn))
                                           prop-resolver: resprop)))))


(describe "send"
  (define (noop . args) #t)
  (define base-pob
    (make-pob methods: `((_method-missing . ,noop))))
  
  ;; it "uses the pob's method-resolver to find its _receive method"
  (let* ((pob (make-pob base: base-pob))
         (stub-resolve
          (lambda (self method-name #!optional default)
            (if (eq? method-name '_receive)
                (raise 'success "Success!")
                (%resolve-method self method-name default)))))
    (%pob-set-method-resolver! pob stub-resolve)
    (it "uses the pob's method-resolver to find its _receive method"
      (raises? (success)
        (send pob 'amethod 1 2 3))))

  ;; it "invokes the _receive method with the expected arguments"
  (let* ((pob (make-pob base: base-pob))
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

(describe "std-prop-resolver"
  (define pob1 (make-pob props: '((a . 1) (b . 2) (c . 3) (d . 4))))
  (define pob2 (make-pob base: pob1 props: `((a . 11) (c . ,(void)))))
  (define pob3 (make-pob base: pob2 props: '((b . 22))))

  (it "returns a list with self and the prop value, if self defines the prop"
    (equal? (std-prop-resolver pob3 'b) (cons pob3 22)))

  (it "returns a cons with the nearest ancestor that defines the prop, and the prop value"
    (equal? (std-prop-resolver pob3 'a) (cons pob2 11)))

  (it "searches ancestors recursively to find the prop value"
    (equal? (std-prop-resolver pob3 'd) (cons pob1 4)))
 
  (it "returns #f and the default value if the prop is not found"
    (equal? (std-prop-resolver pob3 'z 'default) (cons #f 'default)))

  (it "uses #<unspecified> as the default value by default"
    (equal? (std-prop-resolver pob3 'z) (cons #f (void))))

  (it "stops searching if it ever finds the prop defined as #<unspecified>"
    (equal? (std-prop-resolver pob3 'c) (cons pob2 (void))))

  (it "fails if given no args"
    (raises? (arity)
      (std-prop-resolver)))

  (it "fails if the prop name is omitted"
    (raises? (arity)
      (std-prop-resolver pob3)))

  (it "does not fail if given the pob and prop name"
    (not (raises? ()
           (std-prop-resolver pob3 'a))))

  (it "does not fail if given the pob, prop name, and default value"
    (not (raises? ()
           (std-prop-resolver pob3 'a 'default))))
  
  (it "fails if given too many args"
    (raises? ()
      (std-prop-resolver pob3 'a 'b 'c)))

  (it "fails if given a non-pob for the first arg"
    (raises? (type)
      (std-prop-resolver 'foo 'a))))


(describe "std-method-resolver"
  (define (fn1 self) 1)
  (define (fn2 self) 2)
  (define (fn3 self) 3)
  (define (fn4 self) 4)
  (define (fn5 self) 5)
  (define (fn6 self) 6)

  (define pob1
    (make-pob methods: `((m . ,fn1) (n . ,fn2) (o . ,fn3) (p . ,fn4))))
  (define pob2
    (make-pob base: pob1 methods: `((m . ,fn5) (o . ,(void)))))
  (define pob3
    (make-pob base: pob2 methods: `((n . ,fn6))))

  (it "returns a pair with self and the definition if self defines it"
    (equal? (std-method-resolver pob3 'n) (cons pob3 fn6)))

  (it "returns a pair with the nearest ancestor that defines the method, and the definition"
    (equal? (std-method-resolver pob3 'm) (cons pob2 fn5)))

  (it "searches ancestors recursively to find the definition"
    (equal? (std-method-resolver pob3 'p) (cons pob1 fn4)))
 
  (it "returns #f and the default value if the method is not found"
    (equal? (std-method-resolver pob3 'z 'default) (cons #f 'default)))

  (it "uses #<unspecified> as the default value by default"
    (equal? (std-method-resolver pob3 'z) (cons #f (void))))
  
  (it "stops searching if it ever finds the method defined as #<unspecified>"
    (equal? (std-method-resolver pob3 'o) (cons pob2 (void))))

  (it "fails if given no args"
    (raises? (arity)
      (std-method-resolver)))

  (it "fails if the method name is omitted"
    (raises? (arity)
      (std-method-resolver pob3)))

  (it "does not fail if given the pob and method name"
    (not (raises? ()
           (std-method-resolver pob3 'm))))

  (it "does not fail if given the pob, method name, and default value"
    (not (raises? ()
           (std-method-resolver pob3 'm 'default))))
  
  (it "fails if given too many args"
    (raises? ()
      (std-method-resolver pob3 'm 'default 'o)))

  (it "fails if given a non-pob for the first arg"
    (raises? (type)
      (std-method-resolver 'foo 'm))))


(describe "std-_receive"
  (define (noop . args) #t)
  (define base-pob
    (make-pob methods: `((_method-missing . ,std-_method-missing))))

  (it "uses the pob's method-resolver to find a matching method"
    (let* ((stub-resolve (lambda args (raise 'success "Success!")))
           (pob (make-pob base: base-pob
                 method-resolver: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "invokes the matching method if found"
    (let* ((stub-method (lambda args (raise 'success "Success!")))
           (stub-resolve (lambda args (cons 'some-pob stub-method)))
           (pob (make-pob base: base-pob
                 method-resolver: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "resolves _method-missing if no matching method was found"
    (let* ((stub-resolve (lambda (self method-name #!optional default)
                           (case method-name
                             ((amethod) (cons #f default))
                             ((_method-missing)
                              (raise 'success "Success!")))))
           (pob (make-pob base: base-pob
                 method-resolver: stub-resolve)))
      (raises? (success)
        (std-_receive pob 'amethod '(1 2 3)))))

  (it "invokes _method-missing if no matching method was found"
    (let* ((stub-mm (lambda args (raise 'success "Success")))
           (stub-resolve (lambda (self method-name #!optional default)
                           (case method-name
                             ((amethod) (cons #f default))
                             ((_method-missing) (cons 'some-pob stub-mm)))))
           (pob (make-pob base: base-pob
                 method-resolver: stub-resolve)))
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


(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))