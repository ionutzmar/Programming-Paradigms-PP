#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack null)
(define (make-stack)
  '())

(define (push element stack)
  (cons element stack))
(define (top stack) (car stack))
(define (pop stack)
  (if (null? stack)
      null
      (cdr stack)))


;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (append (list stack) (list (list co-varnames)) (list (list co-consts)) (list (list co-names)) (list (list co-code)) (list (list IC))))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (caadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (caaddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (caadr (cddr stack-machine)))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (caadr (cdddr stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-IC (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (caaddr (cdddr stack-machine)))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (let indexation((index 0)
                  (sym symbols))
       (cond
         ((null? sym) -1)
         ((equal? (car sym) symbol) index)
         (else (indexation (add1 index) (cdr sym)))
        )
    ))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (let ((index (get-symbol-index symbol)))
    (if (equal? index -1)
         (0)
         (append (take stack-machine index) (list (list item)) (drop stack-machine (add1 index)))
         )
    ))
 
  

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (append (list (push value (car stack-machine))) (cdr stack-machine)))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (append (list (pop (car stack-machine))) (cdr stack-machine)))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (letrec ((IC (get-IC stack-machine))
           (instruction (list-ref (get-code stack-machine) IC)))
    (cond
      ((equal? (car instruction) 'RETURN_VALUE) stack-machine)
      ((equal? (car instruction) 'POP_TOP) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (pop-exec-stack stack-machine))))
      ((equal? (car instruction) 'LOAD_CONST) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (push-exec-stack (hash-ref (get-consts stack-machine) (cdr instruction)) stack-machine))))
      ((equal? (car instruction) 'STORE_FAST) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (pop-exec-stack (update-stack-machine (hash-set (get-varnames stack-machine) (cdr instruction) (top (get-stack stack-machine))) 'CO-VARNAMES stack-machine)))))
      ((equal? (car instruction) 'LOAD_FAST) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (push-exec-stack (hash-ref (get-varnames stack-machine) (cdr instruction)) stack-machine))))
      ((equal? (car instruction) 'BINARY_ADD) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER
                               (update-stack-machine (car (append (list (+ (car (get-stack stack-machine)) (car (cdr (get-stack stack-machine))))) (cddr (get-stack stack-machine)))) 'STACK stack-machine))))
      ((equal? (car instruction) 'BINARY_SUBTRACT) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (update-stack-machine (car (append (list (- (car (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))) (cddr (get-stack stack-machine)))) 'STACK stack-machine))))
      ((equal? (car instruction) 'BINARY_MODULO) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (update-stack-machine (car (append (list (modulo (car (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))) (cddr (get-stack stack-machine)))) 'STACK stack-machine))))
      ((equal? (car instruction) 'INPLACE_ADD) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER
                               (update-stack-machine (car (append (list (+ (car (get-stack stack-machine)) (car (cdr (get-stack stack-machine))))) (cddr (get-stack stack-machine)))) 'STACK stack-machine))))
      ((equal? (car instruction) 'INPLACE_SUBTRACT) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (update-stack-machine (car (append (list (- (car (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))) (cddr (get-stack stack-machine)))) 'STACK stack-machine))))
      ((equal? (car instruction) 'INPLACE_MODULO) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER 
                               (update-stack-machine (car (append (list (modulo (car (cdr (get-stack stack-machine))) (top (get-stack stack-machine)))) (cddr (get-stack stack-machine)))) 'STACK stack-machine))))
      ((equal? (car instruction) 'COMPARE_OP) (run-stack-machine (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER
                                                                                       (push-exec-stack (((get-cmpop) (cdr instruction)) (cadr (get-stack stack-machine)) (car (get-stack stack-machine))) (pop-exec-stack (pop-exec-stack stack-machine))))))
      ((equal? (car instruction) 'POP_JUMP_IF_FALSE) (run-stack-machine
                               (if (top (get-stack stack-machine))
                                   (pop-exec-stack (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-machine))
                                   (pop-exec-stack (update-stack-machine (quotient (cdr instruction) 2) 'INSTRUCTION-COUNTER stack-machine))
                                   )))
      ((equal? (car instruction) 'POP_JUMP_IF_TRUE) (run-stack-machine
                               (if (top (get-stack stack-machine))
                                   (pop-exec-stack (update-stack-machine (quotient (cdr instruction) 2) 'INSTRUCTION-COUNTER stack-machine))
                                   (pop-exec-stack (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-machine))
                                   )))
      ((equal? (car instruction) 'JUMP_ABSOLUTE ) (run-stack-machine (update-stack-machine (cdr instruction) 'INSTRUCTION-COUNTER stack-machine)))
      ((equal? (car instruction) 'FOR_ITER) (run-stack-machine
                                             (if (empty? (top (get-stack stack-machine)))
                                                 (run-stack-machine (pop-exec-stack) (update-stack-machine (cdr instruction) 'INSTRUCTION-COUNTER stack-machine))
                                                 (push-exec-stack (car (car (get-stack stack-machine))) (push-exec-stack (cdr (car (get-stack stack-machine)))) (pop-exec-stack stack-machine)))))
      
      (else (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-machine))
      )
      ))
