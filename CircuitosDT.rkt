;; Taller 2: Abstracción de datos y sintáxis abstracta
;; Integrantes grupo #15:
;; Jonathan Aristizabal - 2322626
;; Andrey Quiceno - 2326081
;; Johan Ceballos - 2372229
;; Fecha: 14-03-2025

#lang eopl

;; ==========================
;; Gramática BNF
;; ==========================


;; <circuit> ::= '(circuit <gate-list>)
;; <gate_list> ::= empty | <gate> <gate_list>
;; <gate> ::= '(gate <gate_id> <type> <input-list>)
;; <gate-id> ::= identificador de la compuerta
;; <type> ::= and | or | not | xor
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
;; <gate_ref> ::= identificador de otra compuerta

(define-datatype circuit circuit?
  (a-circuit (gates gate-list?)))  ;; Un circuito tiene una lista de compuertas

(define-datatype gate-list gate-list?
  (empty-gate-list)                    ;; Lista vacía
  (cons-gate (gate gate?) (rest gate-list?)))  ;; Lista de compuertas

(define-datatype gate gate?
  (a-gate (id symbol?)               ;; Identificador de la compuerta
             (gate-type type?)           ;; Tipo de compuerta
             (inputs input-list?)))      ;; Lista de entradas

(define-datatype type type?
  (and-type)    ;; Compuerta AND
  (or-type)     ;; Compuerta OR
  (not-type)    ;; Compuerta NOT
  (xor-type))   ;; Compuerta XOR

(define-datatype input-list input-list?
  (empty-input-list)                     ;; Lista vacía de entradas
  (bool-input (value boolean?)            ;; Entrada booleana
              (rest input-list?))
  (ref-input (ref symbol?)           ;; Referencia a otra compuerta
                  (rest input-list?)))

;; ========================================
;; Ejemplos de uso circuitos
;; ========================================

;; El resultado esperado son los AST de los circuitos presentados:

;; A --| NOT |--> SALIDA
(define C1 (a-circuit
   (cons-gate
    (a-gate 'G1 (not-type) 
               (ref-input 'A (empty-input-list)))
    (empty-gate-list))))

;; A --|
;;      | AND --> SALIDA
;; B --|
(define C2(a-circuit
   (cons-gate
    (a-gate 'G1 (and-type) 
               (ref-input 'A (ref-input 'B (empty-input-list))))
    (empty-gate-list))))

;; #t --| OR |-- G1 --| NOT |--> SALIDA
;; #f --|
(define C3(a-circuit
   (cons-gate
    (a-gate 'G1 (or-type) 
               (bool-input #t (bool-input #f (empty-input-list))))
    (cons-gate
     (a-gate 'G2 (not-type) 
                (ref-input 'G1 (empty-input-list)))
     (empty-gate-list)))))

;; #t --|
;;      | XOR --> SALIDA
;; #f --|
(define C4(a-circuit
   (cons-gate
    (a-gate 'G1 (xor-type) 
               (bool-input #t (bool-input #f (empty-input-list))))
    (empty-gate-list))))

;; #t --| AND |-- G1 --| OR |-- G2 --| XOR |-- G3 --| NOT |--> SALIDA
;; #f --|     |        |    |        |              |
;;            |   #t   |    |   #t   |
(define C5(a-circuit
   (cons-gate
    (a-gate 'G1 (and-type) 
               (bool-input #t (bool-input #f (empty-input-list))))
    (cons-gate
     (a-gate 'G2 (or-type) 
                (ref-input 'G1 (bool-input #t (empty-input-list))))
     (cons-gate
      (a-gate 'G3 (xor-type) 
                 (ref-input 'G2 (bool-input #f (empty-input-list))))
      (cons-gate
       (a-gate 'G4 (not-type) 
                  (ref-input 'G3 (empty-input-list)))
       (empty-gate-list)))))))

(display "\nPruebas de gramática basada en datatypes:")
(newline)
(display "\nPrueba 1: Circuito#1 (C1)")
(newline)
(display C1)
(newline)
(display "\nPrueba 2: Circuito#3 (C3)")
(newline)
(display C3)
(newline)
(display "\nPrueba 3: Circuito#5 (C5)")
(newline)
(display C5)
(newline)