
;;; The contents of this file are made available under the CC0 1.0
;;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;;; http://creativecommons.org/publicdomain/zero/1.0/

;;; This program demonstrates some fundamental Protolk concepts:
;;;
;;;   - Deriving from stdpob to create your own custom pobs
;;;   - Deriving from your own custom pobs to create more pobs
;;;   - Inheriting props and methods from base pobs
;;;   - Defining props and prop reader methods in a pob
;;;   - Defining methods on a pob
;;;   - Invoking super methods from within a pob's method
;;;   - Sending messages to pobs to invoke methods
;;;
;;; This demo uses protolk reader extensions. If you compile this
;;; demo, you should specify (at least) the following flags:
;;;
;;;   -X protolk-syntax-send-brackets -X protolk-syntax-own-prop-at
;;;
;;; See 01-basic-nosyntax.scm for this demo without reader extensions.


(cond-expand
 (compiling
  (require-library protolk protolk-stdpob))
 (else
  (load-relative "../protolk.scm")
  (load-relative "../protolk-stdpob.scm")
  (load-relative "../protolk-syntax-send-brackets.scm")
  (load-relative "../protolk-syntax-own-prop-at.scm")))

(import protolk
        protolk-stdpob
        protolk-syntax-send-brackets
        protolk-syntax-own-prop-at)

(require-extension extras)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper procedures
;;

(define (size-to-string cm-size)
  (cond
   ((> cm-size 100)
    (sprintf "~am" (/ cm-size 100)))
   ((< cm-size 1)
    (sprintf "~amm" (* cm-size 10)))
   (else
    (sprintf "~acm" cm-size))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fish prototypes
;;

(define fish
  [stdpob derive
          props: '((name "fish")
                   (size 5)) ; centimeters
          methods: `((name ,(prop-reader 'name))
                     (size ,(prop-reader 'size)))])

(define-method fish (_display out)
  (fprintf out "#<~a>" [self description]))

(define-method fish (description)
  (sprintf "~a ~a" (size-to-string @size) @name))

(define-method fish (swim)
  (printf "The ~a swims around.~n"
          [self description]))

(define-method fish (eat other)
  ;; The fish can only swallow things half its size or smaller.
  (if (> (/ [self size] 2)
         [other size])
      (printf "The ~a swallows the ~a.~n"
              [self description]
              [other description])
      (printf "The ~a is too small to swallow the ~a.~n"
              [self description]
              [other description])))


;;; Not all plankton are fish, but many fish eat plankton.
(define plankton
  [stdpob derive
          props: '((name "plankton")
                   (size 0.0))
          methods: `((name ,(prop-reader 'name))
                     (size ,(prop-reader 'size)))])

(define-method plankton (_display out)
  (fprintf out "#<~a>" [self description]))

(define-method plankton (description)
  "plankton")

(define-method plankton (swim)
  (printf "The ~a floats along with the currents.~n"
          [self description]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some specific fishes
;;
;; NOTE: In reality, these creatures do not all live in the same
;; habitat. Suspend your disbelief.
;;

;;; Paedocypris progenetica is the smallest known species of fish.
(define paedocypris
  [fish derive
        props: '((name "paedocypris")
                 (size 0.79))])

(define-method paedocypris (swim)
  (printf "The ~a darts to and fro.~n"
          [self description]))


(define halibut
  [fish derive
        props: '((name "halibut")
                 (size 20))])

(define bluefin-tuna
  [fish derive
        props: '((name "bluefin tuna")
                 (size 200))])


(define whale-shark
  [fish derive
        props: '((name "whale shark")
                 (size 1200))])

(define-method whale-shark (eat other)
  (if (< 1 [other size])
      ;; Whale sharks are filter feeders.
      (printf "The ~a only eats very small things, not the ~a.~n"
              [self description]
              [other description])
      (super*)))


(define great-white-shark
  [fish derive
        props: '((name "great white shark")
                 (size 460))])

(define-method great-white-shark (swim)
  (printf "The ~a skulks around ominously.~n"
          [self description]))

(define-method great-white-shark (eat other)
  (let ((self-size [self size])
        (other-size [other size]))
    (cond
     ((< other-size (* self-size 0.1))
      (printf "The ~a swallows the ~a in one gulp!~n"
              [self description]
              [other description]))
     ((< other-size (/ self-size 2))
      (printf "The ~a gobbles up the ~a. Om nom nom!~n"
              [self description]
              [other description]))
     (else
      (printf "The ~a takes a big bite out of the ~a. Chomp!~n"
              [self description]
              [other description])))))


;;; An improbable scenario

[plankton swim]
[paedocypris swim]
[paedocypris eat plankton]

[fish swim]
[fish eat paedocypris]

[halibut swim]
[halibut eat fish]

[whale-shark swim]
[whale-shark eat plankton]
[whale-shark eat paedocypris]
[whale-shark eat halibut]

[great-white-shark swim]

[bluefin-tuna swim]
[bluefin-tuna eat great-white-shark]

[great-white-shark eat halibut]
[great-white-shark eat bluefin-tuna]
[great-white-shark eat whale-shark]
