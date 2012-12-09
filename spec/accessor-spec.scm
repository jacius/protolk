
(load-relative "spec-helpers")
(load-relative "../protolk")
(import protolk
        protolk-primitives
        protolk-internal)
(import-for-syntax protolk)

(use extras)


;;;;;;;;;;;;;;;;;;;;
;; PROP ACCESSORS
;;

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
    (define pob1 (make-pob props: '((some-prop some-value))))

    (it "returns the given pob's value for the matching prop"
      (equal? (some-prop-reader pob1)
              'some-value))

    (it "fails if given a non-pob"
      (raises? ()
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
      (raises? ()
        (some-prop-writer 'foo 'bar)))

    (it "fails if given no args"
      (raises? (arity)
        (some-prop-writer)))

    (it "fails if given too many args"
      (let ((pob (make-pob)))
        (raises? (arity)
          (some-prop-writer pob 'some-value 'foo))))))


(describe "define-prop-readers"
  (it "adds prop reader methods to the pob for all the given prop names"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-readers pob '(a b c))
      (and
       (= 3 (length (map car (%pob-methods pob))))
       (= 1 (send pob 'a))
       (= 2 (send pob 'b))
       (= 3 (send pob 'c)))))

  (it "allows specifying custom method names for each prop"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-readers pob '((a apple) (b banana) c))
      (and
       (= 3 (length (map car (%pob-methods pob))))
       (= 1 (send pob 'apple))
       (= 2 (send pob 'banana))
       (= 3 (send pob 'c)))))

  (it "does nothing if given an empty list of prop names"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-readers pob '())
      (null? (%pob-methods pob))))

  (it "fails if given no list of prop names"
    (let ((pob (make-pob)))
      (raises? (arity)
        (define-prop-readers pob))))

  (it "fails if given a non-pob"
    (raises? (type)
      (define-prop-readers 'not-a-pob '(a b c))))

  (it "fails if given no args"
    (raises? (arity)
      (define-prop-readers))))


(describe "define-prop-writers"
  (it "adds prop writer methods to the pob for all the given prop names"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-writers pob '(a b c))
      (send pob 'set-a! 4)
      (send pob 'set-b! 5)
      (send pob 'set-c! 6)
      (and
       (= 3 (length (map car (%pob-methods pob))))
       (= 4 (%prop pob 'a))
       (= 5 (%prop pob 'b))
       (= 6 (%prop pob 'c)))))

  (it "allows specifying custom method names for each prop"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-writers pob '((a apple:) (b set-banana!) c))
      (send pob apple: 4)
      (send pob 'set-banana! 5)
      (send pob 'set-c! 6)
      (and
       (= 3 (length (map car (%pob-methods pob))))
       (= 4 (%prop pob 'a))
       (= 5 (%prop pob 'b))
       (= 6 (%prop pob 'c)))))

  (it "does nothing if given an empty list of prop names"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-writers pob '())
      (null? (%pob-methods pob))))

  (it "fails if given no list of prop names"
    (let ((pob (make-pob)))
      (raises? (arity)
        (define-prop-writers pob))))

  (it "fails if given a non-pob"
    (raises? (type)
      (define-prop-writers 'not-a-pob '(a b c))))

  (it "fails if given no args"
    (raises? (arity)
      (define-prop-writers))))


(describe "define-prop-accessors"
  (it "adds prop reader and writer methods to the pob for all the given prop names"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-accessors pob '(a b c))
      (send pob 'set-a! 4)
      (send pob 'set-b! 5)
      (send pob 'set-c! 6)
      (and
       (= 6 (length (map car (%pob-methods pob))))
       (= 4 (send pob 'a))
       (= 5 (send pob 'b))
       (= 6 (send pob 'c)))))

  (it "allows specifying custom method names for each prop"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-accessors pob '((a apple)
                                   (b banana banana=)
                                   c))
      (send pob 'set-apple! 4)
      (send pob 'banana= 5)
      (send pob 'set-c! 6)
      (and
       (= 6 (length (map car (%pob-methods pob))))
       (= 4 (send pob 'apple))
       (= 5 (send pob 'banana))
       (= 6 (send pob 'c)))))

  (it "does nothing if given an empty list of prop names"
    (let ((pob (make-pob props: '((a 1) (b 2) (c 3)))))
      (define-prop-accessors pob '())
      (null? (%pob-methods pob))))

  (it "fails if given no list of prop names"
    (let ((pob (make-pob)))
      (raises? (arity)
        (define-prop-accessors pob))))

  (it "fails if given a non-pob"
    (raises? (type)
      (define-prop-accessors 'not-a-pob '(a b c))))

  (it "fails if given no args"
    (raises? (arity)
      (define-prop-accessors))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 ((not protolk-all-tests)
  (test-exit))
 (else))
