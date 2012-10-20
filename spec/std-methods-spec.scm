
(load-relative "spec-helpers")
(load-relative "../protolk")
(import protolk
        protolk-primitives
        protolk-internal)
(import-for-syntax protolk)

(use extras)


;;;;;;;;;;;;;
;; PRINTER
;;

(describe "pob record type printer"
  (define (display-foo self port) (display "foo" port))
  
  (define pob1 (make-pob))
  (define pob2 (make-pob base: pob1 methods: `((_display ,display-foo))))
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


;;;;;;;;;;;;;;;;;
;; STD METHODS
;;

(describe "std-_receive"
  (define (noop . args) #t)
  (define base-pob
    (make-pob methods: `((_method-missing ,std-_method-missing))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
