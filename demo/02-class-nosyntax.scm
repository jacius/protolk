
;;; The contents of this file are made available under the CC0 1.0
;;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;;; http://creativecommons.org/publicdomain/zero/1.0/

;;; This program demonstrates how you can emulate classical
;;; inheritance in Protolk:
;;;
;;;   - Defining class pobs
;;;   - Creating "instances" of the class pobs
;;;   - Initializing instances
;;;   - Creating a class hierarchy


(cond-expand
 (compiling
  (require-library protolk-primitives protolk protolk-stdpob))
 (else
  (load-relative "../protolk-primitives.scm")
  (load-relative "../protolk.scm")
  (load-relative "../protolk-stdpob.scm")
  (load-relative "../protolk-syntax.scm")))

(import protolk-primitives protolk protolk-stdpob)
(import-for-syntax protolk protolk-syntax)

(use data-structures extras)

(begin-for-syntax
 (enable-syntax-send)
 (enable-syntax-own-prop))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper procedures
;;

(define (string-upcase-first s)
  (if (> (string-length s) 0)
      (string-append
       (string (char-upcase (string-ref s 0)))
       (substring s 1 (string-length s)))
      s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class
;;

(define (display-class class out)
  (fprintf out "#<class ~a>" [class class-name]))

(define (display-instance instance out)
  (fprintf out "#<~a~a>"
           [instance class-name]
           (if [instance responds-to? 'name]
               (sprintf " ~s" [instance name])
               "")))

(define <class>
  [stdpob derive
          props: '((class-name "class"))
          methods: `((class-name ,(prop-reader 'class-name))
                     (_display ,display-class))])

(define-method <class> (new #!rest args)
  (let ((instance [self derive
                        methods: `((_display ,display-instance))]))
    (if [instance responds-to? 'initialize]
        (apply send instance 'initialize args))
    instance))

(define-method <class> (instance-of? klass)
  [self has-ancestor? klass])


(define-syntax define-class
  (syntax-rules ()
    ((define-class klass () . args)
     (define klass (send <class> 'derive . args)))
    ((define-class klass (base) . args)
     (define klass (send base 'derive . args)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animal
;;

(define-class <animal> ()
  props: '((class-name "animal"))
  methods: `((name ,(prop-reader 'name))
             (appearance ,(prop-reader 'appearance))))

(define-method <animal> (initialize #!rest all-keys #!key name appearance)
  (when (super?)
    (apply-super all-keys))
  (%set-method! self '_display display-instance)
  (when name
    (set! @name name))
  (when appearance
    (set! @appearance appearance)))

(define-method <animal> (description)
  (string-intersperse
   (filter string?
           ;; e.g. "Mittens the fuzzy kitten"
           (list @name "the" @appearance @class-name))
   " "))

(define-method <animal> (move #!key to)
  (if to
      (printf "~a moves to ~a.~n"
              (string-upcase-first [self description])
              to)
      (printf "~a moves around.~n"
              (string-upcase-first [self description]))))

(define-method <animal> (make-sound #!key at)
  (if at
      (printf "~a makes an animal sound at ~a.~n"
              (string-upcase-first [self description])
              (if [at instance-of? <animal>]
                  [at description]
                  at))
      (printf "~a makes an animal sound.~n"
              (string-upcase-first [self description]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific kinds of animals
;;

(define-class <cat> (<animal>)
  props: '((class-name "cat")
           (appearance "fuzzy")))

(define-method <cat> (move #!key to)
  (if to
      (when (super?)
        (super*))
      (printf "~a wanders around, looking for someplace to take a nap.~n"
              (string-upcase-first [self description]))))

(define-method <cat> (make-sound #!key at)
  (if at
      (printf "~a meows at ~a.~n"
              (string-upcase-first [self description])
              (if [at instance-of? <animal>]
                  [at description]
                  at))
      (printf "~a meows for attention.~n"
              (string-upcase-first [self description]))))



(define-class <dog> (<animal>)
  props: '((class-name "dog")
           (appearance "furry")))

(define-method <dog> (move #!key to)
  (if to
      (when (super?)
        (super*))
      (printf "~a walks around, patrolling its territory.~n"
              (string-upcase-first [self description]))))

(define-method <dog> (make-sound #!key at)
  (if at
      (printf "~a barks at ~a.~n"
              (string-upcase-first [self description])
              (if [at instance-of? <animal>]
                  [at description]
                  at)
              at)
      (printf "~a howls into the night.~n"
              (string-upcase-first [self description]))))



(define-class <bird> (<animal>)
  props: '((class-name "bird")
           (appearance "feathery")))

(define-method <bird> (move #!key to)
  (if to
      (printf "~a flies to ~a.~n"
              (string-upcase-first [self description])
              to)
      (printf "~a soars through the air without a care in the world.~n"
              (string-upcase-first [self description]))))

(define-method <bird> (make-sound #!key at)
  (if at
      (printf "~a chirps at ~a.~n"
              (string-upcase-first [self description])
              (if [at instance-of? <animal>]
                  [at description]
                  at))
      (printf "~a chirps and sings loudly.~n"
              (string-upcase-first [self description]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scenario
;;

(define mittens
  [<cat> new name: "Mittens"])

(define rover
  [<dog> new name: "Rover"])

(define tweety
  [<bird> new name: "Tweety" appearance: "yellow"])

(define twitty
  [<bird> new name: "Twitty" appearance: "blue"])


[tweety move]
[mittens move]
[mittens move to: "a sunny spot"]
[tweety move to: "a tree branch above the sunny spot"]
[tweety make-sound]
[twitty move to: "sit next to Tweety"]
[twitty make-sound at: tweety]
[mittens make-sound at: twitty]
[rover move]
[rover make-sound at: mittens]
[mittens move to: "the top of the tree"]
[twitty move to: "another tree"]
[tweety move to: "follow Twitty"]
[mittens make-sound]
