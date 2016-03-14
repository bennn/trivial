#lang typed/racket/base

(define-type Probability Nonnegative-Real)
(define-type Population (cons Automaton* Automaton*))
(define-type Automaton* [Vectorof Automaton])
(define-type Payoff Nonnegative-Real)

(define-type State Natural)
(define-type Transition* [Vectorof Transition])
(define-type Transition [Vectorof State])

(require "automata.rkt"
)

(provide
defects cooperates tit-for-tat grim-trigger match-pair automaton-reset clone
make-random-automaton
automaton-payoff
Automaton
Probability Population Automaton* Payoff)
