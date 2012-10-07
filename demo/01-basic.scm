
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


(cond-expand
 (compiling
  (require-library protolk protolk-stdpob))
 (else
  (load-relative "../protolk.scm")
  (load-relative "../protolk-stdpob.scm")))

(import protolk protolk-stdpob)
(import-for-syntax protolk)

(use extras)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
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
  (send stdpob 'derive
        props: '((name "fish")
                 (size 5)) ; centimeters
        methods: `((name ,(prop-reader 'name))
                   (size ,(prop-reader 'size)))))

(define-method (fish _display out)
  (fprintf out "#<~a>" (send self 'description)))

(define-method (fish description)
  (sprintf "~a ~a"
           (size-to-string (own-prop 'size))
           (own-prop 'name)))

(define-method (fish swim)
  (printf "The ~a swims around.~n"
          (send self 'description)))

(define-method (fish eat other)
  ;; The fish can only swallow things half its size or smaller.
  (if (> (/ (send self 'size) 2)
         (send other 'size))
      (printf "The ~a swallows the ~a.~n"
              (send self 'description)
              (send other 'description))
      (printf "The ~a is too small to swallow the ~a.~n"
              (send self 'description)
              (send other 'description))))


;;; Not all plankton are fish, but many fish eat plankton.
(define plankton
  (send stdpob 'derive
        props: '((name "plankton")
                 (size 0.0))
        methods: `((name ,(prop-reader 'name))
                   (size ,(prop-reader 'size)))))

(define-method (plankton _display out)
  (fprintf out "#<~a>" (send self 'description)))

(define-method (plankton description)
  "plankton")

(define-method (plankton swim)
  (printf "The ~a floats along with the currents.~n"
          (send self 'description)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some specific fishes
;;
;; NOTE: In reality, these creatures do not all live in the same
;; habitat. Suspend your disbelief.
;;

;;; Paedocypris progenetica is the smallest known species of fish.
(define paedocypris
  (send fish 'derive
        props: '((name "paedocypris")
                 (size 0.79))))

(define-method (paedocypris swim)
  (printf "The ~a darts to and fro.~n"
          (send self 'description)))


(define halibut
  (send fish 'derive
        props: '((name "halibut")
                 (size 20))))

(define bluefin-tuna
  (send fish 'derive
        props: '((name "bluefin tuna")
                 (size 200))))


(define whale-shark
  (send fish 'derive
        props: '((name "whale shark")
                 (size 1200))))

(define-method (whale-shark eat other)
  (if (< 1 (send other 'size))
      ;; Whale sharks are filter feeders.
      (printf "The ~a only eats very small things, not the ~a.~n"
              (send self 'description)
              (send other 'description))
      (super*)))


(define great-white-shark
  (send fish 'derive
        props: '((name "great white shark")
                 (size 460))))

(define-method (great-white-shark swim)
  (printf "The ~a skulks around ominously.~n"
          (send self 'description)))

(define-method (great-white-shark eat other)
  (let ((self-size (send self 'size))
        (other-size (send other 'size)))
    (cond
     ((< other-size (* self-size 0.1))
      (printf "The ~a swallows the ~a in one gulp!~n"
              (send self 'description)
              (send other 'description)))
     ((< other-size (/ self-size 2))
      (printf "The ~a gobbles up the ~a. Om nom nom!~n"
              (send self 'description)
              (send other 'description)))
     (else
      (printf "The ~a takes a big bite out of the ~a. Chomp!~n"
              (send self 'description)
              (send other 'description))))))


;;; An improbable scenario

(send plankton 'swim)
(send paedocypris 'swim)
(send paedocypris 'eat plankton)

(send fish 'swim)
(send fish 'eat paedocypris)

(send halibut 'swim)
(send halibut 'eat fish)

(send whale-shark 'swim)
(send whale-shark 'eat plankton)
(send whale-shark 'eat paedocypris)
(send whale-shark 'eat halibut)

(send great-white-shark 'swim)

(send bluefin-tuna 'swim)
(send bluefin-tuna 'eat great-white-shark)

(send great-white-shark 'eat halibut)
(send great-white-shark 'eat bluefin-tuna)
(send great-white-shark 'eat whale-shark)
