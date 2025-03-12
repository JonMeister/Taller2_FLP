#lang eopl

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
  (and-gate)    ;; Compuerta AND
  (or-gate)     ;; Compuerta OR
  (not-gate)    ;; Compuerta NOT
  (xor-gate))   ;; Compuerta XOR

(define-datatype input-list input-list?
  (empty-input-list)                     ;; Lista vacía de entradas
  (bool-input (value boolean?)            ;; Entrada booleana
              (rest input-list?))
  (ref-input (ref symbol?)           ;; Referencia a otra compuerta
                  (rest input-list?)))

;; EJEMPLOS DEL CIRCUIT --------------------------------

;; A --| NOT |--> SALIDA
(a-circuit
   (cons-gate
    (a-gate 'G1 (not-gate) 
               (ref-input 'A (empty-input-list)))
    (empty-gate-list)))

;; A --|
;;      | AND --> SALIDA
;; B --|
(a-circuit
   (cons-gate
    (a-gate 'G1 (and-gate) 
               (ref-input 'A (ref-input 'B (empty-input-list))))
    (empty-gate-list)))

;; #t --| OR |-- G1 --| NOT |--> SALIDA
;; #f --|
(a-circuit
   (cons-gate
    (a-gate 'G1 (or-gate) 
               (bool-input #t (bool-input #f (empty-input-list))))
    (cons-gate
     (a-gate 'G2 (not-gate) 
                (ref-input 'G1 (empty-input-list)))
     (empty-gate-list))))

;; #t --|
;;      | XOR --> SALIDA
;; #f --|
(a-circuit
   (cons-gate
    (a-gate 'G1 (xor-gate) 
               (bool-input #t (bool-input #f (empty-input-list))))
    (empty-gate-list)))

;; #t --| AND |-- G1 --| OR |-- G2 --| XOR |-- G3 --| NOT |--> SALIDA
;; #f --|     |        |    |        |              |
;;            |   #t   |    |   #t   |
(a-circuit
   (cons-gate
    (a-gate 'G1 (and-gate) 
               (bool-input #t (bool-input #f (empty-input-list))))
    (cons-gate
     (a-gate 'G2 (or-gate) 
                (ref-input 'G1 (bool-input #t (empty-input-list))))
     (cons-gate
      (a-gate 'G3 (xor-gate) 
                 (ref-input 'G2 (bool-input #f (empty-input-list))))
      (cons-gate
       (a-gate 'G4 (not-gate) 
                  (ref-input 'G3 (empty-input-list)))
       (empty-gate-list))))))