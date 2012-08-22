
(load-relative "helpers")

(load-relative "../protolk")
(import protolk
        protolk-primitives
        %protolk-util)

(use extras)


;;;;;;;;;;;;;;
;; CORE API
;;

(describe "make-pob"
  (define (fn pob) #t)

  (it "accepts #:props and #:methods keyword arguments"
    (not (raises-exception? ()
           (make-pob props: '((a . 1))
                     methods: `((m . ,fn))))))

  (it "returns a pob with the specified props and methods"
    (let ((p (make-pob props: '((a . 1)) methods: `((m . ,fn)))))
      (and (pob? p)
           (equal? (%pob-props p) '((a . 1)))
           (equal? (%pob-methods p) `((m . ,fn))))))

  (it "allows the props argument to be omitted"
    (not (raises-exception? ()
           (make-pob methods: `((m . ,fn))))))

  (it "allows the methods argument to be omitted"
    (not (raises-exception? ()
           (make-pob props: `((a . 1))))))

  (it "allows both arguments to be omitted"
    (not (raises-exception? ()
           (make-pob))))

  (it "returns a pob with no props if the props argument is omitted"
    (equal? (%pob-props (make-pob methods: `((m . ,fn)))) '()))
   
  (it "returns a pob with no methods if the props argument is omitted"
    (equal? (%pob-methods (make-pob props: `((a . 1)))) '()))

  (it "returns a pob with no props or methods if both arguments are omitted"
    (let ((p (make-pob)))
      (and (equal? (%pob-props p) '())
           (equal? (%pob-methods p) '())))))


(describe "send"
  (define (noop . args) #t)
  (define base-pob
    (make-pob props: '((base . #f))
              methods: `((_resolve-method . ,stdpob-_resolve-method)
                         (_resolve-prop   . ,stdpob-_resolve-prop)
                         (_method-missing . ,noop))))
  
  ;; it "uses the pob's _resolve-method method to find its _receive method"
  (let* ((pob (stdpob-derive base-pob))
         (stub-resolve
          (lambda (self method-name #!optional default)
            (if (eq? method-name '_receive)
                (raise 'success "Success!")
                (stdpob-_resolve-method self method-name default)))))
    (%set-method! pob '_resolve-method stub-resolve)
    (it "uses the pob's _resolve-method method to find its _receive method"
      (raises-exception? (success)
        (send pob 'amethod 1 2 3))))

  ;; it "invokes the _receive method with the expected arguments"
  (let* ((pob (stdpob-derive base-pob))
         (expected-args (list pob 'amethod '(1 2 3)))
         (stub-receive (lambda args
                         (if (equal? args expected-args)
                              (raise 'success "Success!")
                              (raise 'failure
                                     (sprintf "Expected args ~s, got ~s"
                                              expected-args args))))))
    (%set-method! pob '_receive stub-receive)
    (it "invokes the _receive method with the expected arguments"
      (raises-exception? (success)
        (send pob 'amethod 1 2 3)))))



;;;;;;;;;;;;;;;;;;;;
;; STDPOB METHODS
;;

(describe "stdpob-derive"
  (define base-pob (make-pob))
  (define (fn pob) #t)

  (it "returns a new pob based on the given pob"
    (equal? (%prop (stdpob-derive base-pob) 'base) base-pob))

  (it "allows the base pob to be #f"
    (equal? (%prop (stdpob-derive #f) 'base) #f))

  (it "fails when given a non-pob other than #f"
    (raises-exception? (type)
      (stdpob-derive 'foo)))

  (it "fails when the pob argument is omitted"
    (raises-exception? (arity)
      (stdpob-derive)))

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
    (raises-exception? (type)
      (stdpob-ancestors #f))))


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
    (raises-exception? (arity)
      (stdpob-has-ancestor?)))

  (it "fails when given only one pob"
    (raises-exception? (arity)
      (stdpob-has-ancestor? pob3)))

  (it "fails when given too many args"
    (raises-exception? (arity)
      (stdpob-has-ancestor? pob3 pob2 pob1)))

  (it "fails when the first argument is not a pob"
    (raises-exception? (type)
      (stdpob-has-ancestor? 'foo pob1))))


(describe "stdpob-_resolve-prop"
  (define pob1 (make-pob props: '((base . #f) (a . 1) (b . 2) (c . 3) (d . 4))))
  (define pob2 (stdpob-derive pob1 props: `((a . 11) (c . ,(void)))))
  (define pob3 (stdpob-derive pob2 props: '((b . 22))))

  (it "returns a list with self and the prop value, if self defines the prop"
    (equal? (stdpob-_resolve-prop pob3 'b) (cons pob3 22)))

  (it "returns a cons with the nearest ancestor that defines the prop, and the prop value"
    (equal? (stdpob-_resolve-prop pob3 'a) (cons pob2 11)))

  (it "searches ancestors recursively to find the prop value"
    (equal? (stdpob-_resolve-prop pob3 'd) (cons pob1 4)))
 
  (it "returns #f and the default value if the prop is not found"
    (equal? (stdpob-_resolve-prop pob3 'z 'default) (cons #f 'default)))

  (it "uses #<unspecified> as the default value by default"
    (equal? (stdpob-_resolve-prop pob3 'z) (cons #f (void))))

  (it "stops searching if it ever finds the prop defined as #<unspecified>"
    (equal? (stdpob-_resolve-prop pob3 'c) (cons pob2 (void))))

  (it "fails if given no args"
    (raises-exception? (arity)
      (stdpob-_resolve-prop)))

  (it "fails if the prop name is omitted"
    (raises-exception? (arity)
      (stdpob-_resolve-prop pob3)))

  (it "does not fail if given the pob and prop name"
    (not (raises-exception? ()
           (stdpob-_resolve-prop pob3 'a))))

  (it "does not fail if given the pob, prop name, and default value"
    (not (raises-exception? ()
           (stdpob-_resolve-prop pob3 'a 'default))))
  
  (it "fails if given too many args"
    (raises-exception? ()
      (stdpob-_resolve-prop pob3 'a 'b 'c)))

  (it "fails if given a non-pob for the first arg"
    (raises-exception? (type)
      (stdpob-_resolve-prop 'foo 'a))))


(describe "stdpob-_resolve-method"
  (define (fn1 self) 1)
  (define (fn2 self) 2)
  (define (fn3 self) 3)
  (define (fn4 self) 4)
  (define (fn5 self) 5)
  (define (fn6 self) 6)

  (define pob1
    (make-pob props: '((base . #f))
              methods: `((m . ,fn1) (n . ,fn2) (o . ,fn3) (p . ,fn4))))
  (define pob2
    (stdpob-derive pob1 methods: `((m . ,fn5) (o . ,(void)))))
  (define pob3
    (stdpob-derive pob2 methods: `((n . ,fn6))))

  (it "returns a pair with self and the definition if self defines it"
    (equal? (stdpob-_resolve-method pob3 'n) (cons pob3 fn6)))

  (it "returns a pair with the nearest ancestor that defines the method, and the definition"
    (equal? (stdpob-_resolve-method pob3 'm) (cons pob2 fn5)))

  (it "searches ancestors recursively to find the definition"
    (equal? (stdpob-_resolve-method pob3 'p) (cons pob1 fn4)))
 
  (it "returns #f and the default value if the method is not found"
    (equal? (stdpob-_resolve-method pob3 'z 'default) (cons #f 'default)))

  (it "uses #<unspecified> as the default value by default"
    (equal? (stdpob-_resolve-method pob3 'z) (cons #f (void))))
  
  (it "stops searching if it ever finds the method defined as #<unspecified>"
    (equal? (stdpob-_resolve-method pob3 'o) (cons pob2 (void))))

  (it "fails if given no args"
    (raises-exception? (arity)
      (stdpob-_resolve-method)))

  (it "fails if the method name is omitted"
    (raises-exception? (arity)
      (stdpob-_resolve-method pob3)))

  (it "does not fail if given the pob and method name"
    (not (raises-exception? ()
           (stdpob-_resolve-method pob3 'm))))

  (it "does not fail if given the pob, method name, and default value"
    (not (raises-exception? ()
           (stdpob-_resolve-method pob3 'm 'default))))
  
  (it "fails if given too many args"
    (raises-exception? ()
      (stdpob-_resolve-method pob3 'm 'default 'o)))

  (it "fails if given a non-pob for the first arg"
    (raises-exception? (type)
      (stdpob-_resolve-method 'foo 'm))))


(describe "stdpob-_method-missing"
  (define pob1 (make-pob))

  (it "fails when given only one arg"
    (raises-exception? (arity)
      (stdpob-_method-missing pob1)))

  (it "fails when given only two args"
    (raises-exception? (arity)
      (stdpob-_method-missing pob1 'badmethod)))

  (it "fails when given too many args"
    (raises-exception? (arity)
      (stdpob-_method-missing pob1 'badmethod '(1 2 3) 'foo)))

  (it "raises a 'no-method exception as part of normal operation"
    (raises-exception? (no-method)
      (stdpob-_method-missing pob1 'badmethod '(1 2 3))))

  (describe "the 'no-method exception"
    (define exn (raises-exception? (no-method)
                  (stdpob-_method-missing
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


(describe "stdpob-_receive"
  (define (noop . args) #t)
  (define base-pob
    (make-pob props: '((base . #f))
              methods: `((_resolve-method . ,stdpob-_resolve-method)
                         (_resolve-prop   . ,stdpob-_resolve-prop)
                         (_method-missing . ,stdpob-_method-missing))))

  (it "uses _resolve-method to find a matching method"
    (let* ((results #f)
           (stub-resolve (lambda args
                           (set! results args)
                           (cons 'foo noop)))
           (pob (stdpob-derive base-pob
                 methods: `((_resolve-method . ,stub-resolve)))))
      (stdpob-_receive pob 'amethod '(1 2 3))
      (equal? results (list pob 'amethod))))

  (it "invokes the method returned by _resolve-method if found"
    (let* ((results #f)
           (stub-method (lambda args (set! results args)))
           (stub-resolve (lambda args (cons 'foo stub-method)))
           (pob (stdpob-derive base-pob
                 methods: `((_resolve-method . ,stub-resolve)))))
      (stdpob-_receive pob 'amethod '(1 2 3))
      (equal? results (list pob 1 2 3))))

  (it "uses _resolve-method to find _method-missing if method not found"
    (let* ((results #f)
           (stub-resolve (lambda (self method-name #!optional default)
                           (case method-name
                             ((amethod) (cons #f default))
                             ((_method-missing)
                              (set! results (list self method-name))
                              (cons 'foo noop)))))
           (pob (stdpob-derive base-pob
                 methods: `((_resolve-method . ,stub-resolve)))))
      (stdpob-_receive pob 'amethod '(1 2 3))
      (equal? results (list pob '_method-missing))))

  (it "invokes _method-missing if method not found"
    (let* ((results #f)
           (stub-mm (lambda args (set! results args)))
           (stub-resolve (lambda (self method-name #!optional default)
                           (case method-name
                             ((amethod) (cons #f default))
                             ((_method-missing) (cons 'foo stub-mm)))))
           (pob (stdpob-derive base-pob
                 methods: `((_resolve-method . ,stub-resolve)))))
      (stdpob-_receive pob 'amethod '(1 2 3))
      (equal? results (list pob 'amethod '(1 2 3)))))

  (it "fails if given only a pob"
    (raises-exception? (arity)
      (stdpob-_receive base-pob)))

  (it "fails if given only a pob and method name"
    (raises-exception? (arity)
      (stdpob-_receive base-pob 'amethod)))

  (it "fails if given too many args"
    (raises-exception? (arity)
      (stdpob-_receive base-pob 'amethod '(arg1 arg2) 'foo))))



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
    (equal? (%method stdpob 'has-ancestor?) stdpob-has-ancestor?))

  (it "should have a '_resolve-prop method set to stdpob-_resolve-prop"
    (equal? (%method stdpob '_resolve-prop) stdpob-_resolve-prop))

  (it "should have a '_resolve-method method set to stdpob-_resolve-method"
    (equal? (%method stdpob '_resolve-method) stdpob-_resolve-method))

  (it "should have a '_method-missing method set to stdpob-_method-missing"
    (equal? (%method stdpob '_method-missing) stdpob-_method-missing))

  (it "should have a '_receive method set to stdpob-_receive"
    (equal? (%method stdpob '_receive) stdpob-_receive)))


(test-exit)
