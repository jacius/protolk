
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
  (load-relative "../protolk-stdpob.scm")))

(import protolk-primitives protolk protolk-stdpob)
(import-for-syntax protolk)

(use data-structures extras)


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
  (fprintf out "#<class ~a>" (send class 'class-name)))

(define (display-instance instance out)
  (fprintf out "#<~a~a>"
           (send instance 'class-name)
           (if (send instance 'responds-to? 'name)
               (sprintf " ~s" (send instance 'name))
               "")))

(define <class>
  (send stdpob 'derive
          props: '((class-name "class"))
          methods: `((class-name ,(prop-reader 'class-name))
                     (_display ,display-class))))

(define-method <class> (new #!rest args)
  (let ((instance (send self 'derive
                        methods: `((_display ,display-instance)))))
    (if (send instance 'responds-to? 'initialize)
        (apply send instance 'initialize args))
    instance))

(define-method <class> (instance-of? klass)
  (send self 'has-ancestor? klass))


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
    (set! (own-prop 'name) name))
  (when appearance
    (set! (own-prop 'appearance) appearance)))

(define-method <animal> (description)
  (string-intersperse
   (filter string?
           ;; e.g. "Mittens the fuzzy kitten"
           (list (own-prop 'name) "the"
                 (own-prop 'appearance)
                 (own-prop 'class-name)))
   " "))

(define-method <animal> (move #!key to)
  (if to
      (printf "~a moves to ~a.~n"
              (string-upcase-first (send self 'description))
              to)
      (printf "~a moves around.~n"
              (string-upcase-first (send self 'description)))))

(define-method <animal> (make-sound #!key at)
  (if at
      (printf "~a makes an animal sound at ~a.~n"
              (string-upcase-first (send self 'description))
              (if (send at 'instance-of? <animal>)
                  (send at 'description)
                  at))
      (printf "~a makes an animal sound.~n"
              (string-upcase-first (send self 'description)))))


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
              (string-upcase-first (send self 'description)))))

(define-method <cat> (make-sound #!key at)
  (if at
      (printf "~a meows at ~a.~n"
              (string-upcase-first (send self 'description))
              (if (send at 'instance-of? <animal>)
                  (send at 'description)
                  at))
      (printf "~a meows for attention.~n"
              (string-upcase-first (send self 'description)))))



(define-class <dog> (<animal>)
  props: '((class-name "dog")
           (appearance "furry")))

(define-method <dog> (move #!key to)
  (if to
      (when (super?)
        (super*))
      (printf "~a walks around, patrolling its territory.~n"
              (string-upcase-first (send self 'description)))))

(define-method <dog> (make-sound #!key at)
  (if at
      (printf "~a barks at ~a.~n"
              (string-upcase-first (send self 'description))
              (if (send at 'instance-of? <animal>)
                  (send at 'description)
                  at)
              at)
      (printf "~a howls into the night.~n"
              (string-upcase-first (send self 'description)))))



(define-class <bird> (<animal>)
  props: '((class-name "bird")
           (appearance "feathery")))

(define-method <bird> (move #!key to)
  (if to
      (printf "~a flies to ~a.~n"
              (string-upcase-first (send self 'description))
              to)
      (printf "~a soars through the air without a care in the world.~n"
              (string-upcase-first (send self 'description)))))

(define-method <bird> (make-sound #!key at)
  (if at
      (printf "~a chirps at ~a.~n"
              (string-upcase-first (send self 'description))
              (if (send at 'instance-of? <animal>)
                  (send at 'description)
                  at))
      (printf "~a chirps and sings loudly.~n"
              (string-upcase-first (send self 'description)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scenario
;;

(define mittens
  (send <cat> 'new name: "Mittens"))

(define rover
  (send <dog> 'new name: "Rover"))

(define tweety
  (send <bird> 'new name: "Tweety" appearance: "yellow"))

(define twitty
  (send <bird> 'new name: "Twitty" appearance: "blue"))


(send tweety 'move)
(send mittens 'move)
(send mittens 'move to: "a sunny spot")
(send tweety 'move to: "a tree branch above the sunny spot")
(send tweety 'make-sound)
(send twitty 'move to: "sit next to Tweety")
(send twitty 'make-sound at: tweety)
(send mittens 'make-sound at: twitty)
(send rover 'move)
(send rover 'make-sound at: mittens)
(send mittens 'move to: "the top of the tree")
(send twitty 'move to: "another tree")
(send tweety 'move to: "follow Twitty")
(send mittens 'make-sound)
