;; Taller 2: Abstracción de datos y sintáxis abstracta
;; Integrantes grupo #15:
;; Jonathan Aristizabal - 2322626
;; Andrey Quiceno - 2326081
;; Johan Ceballos - 2372229
;; Fecha: 16-03-2025

#lang eopl

;; Definición de la gramática basada en datatypes:
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

;; Función PARSEBNF: 'Text' -> AST
;;                    list  -> AST


;; PARSEBNF: Función principal que recibe una lista "code".
;; Verifica si la primera palabra clave es 'circuit.
;; - Si es un 'circuit, llama a `parse-gate-list` para analizar la lista de compuertas.
;; - Si no, devuelve un circuito vacío con (empty-gate-list).

(define PARSEBNF
  (lambda (code)
    (a-circuit (parse-gate-list (cadr code)))))

;; parse-gate-list: Función que analiza una lista de compuertas.
;; - Si la lista está vacía, devuelve una lista vacía de compuertas.
;; - Si el primer elemento es 'gate_list', ignora el símbolo y sigue analizando el resto. Si se tiene
;;   en cuenta que las entradas siempre son correctas, este manejo no genera inconvenientes.
;; - Si no, analiza la primera compuerta y la agrega recursivamente a la lista de compuertas procesadas.

(define parse-gate-list
  (lambda (gate-list)
    (cond
      [(null? gate-list)
       (empty-gate-list)]  
      
      [(eq? (car gate-list) 'gate_list)
       (parse-gate-list (cdr gate-list))] 
      
      [else
       (cons-gate (parse-a-gate (car gate-list))  
                  (parse-gate-list (cdr gate-list)))  
       ])))

;; parse-a-gate: Función que analiza una compuerta individual.
;; Se espera que una compuerta tenga un identificador, un tipo y una lista de entradas.
;; - Obtiene el nombre de la compuerta.
;; - Obtiene el tipo de compuerta y lo convierte con parse-type.
;; - Obtiene la lista de entradas y la convierte con parse-input-list.

(define parse-a-gate
  (lambda (gate)
    (a-gate (cadr gate)  ;; Nombre de la compuerta
            (parse-type (caddr gate))  ;; Tipo de compuerta
            (parse-input-list (cadddr gate)))))  ;; Lista de entradas

;; parse-type: Función que analiza el tipo de una compuerta.
;; Se espera que el tipo sea de la forma (type <tipo>).
;; - Devuelve el tipo correspondiente si es reconocido.
(define parse-type
  (lambda (type)
    (cond
      [(equal? type '(type or)) (or-type)]  
      [(equal? type '(type not)) (not-type)] 
      [(equal? type '(type and)) (and-type)]  
      [(equal? type '(type xor)) (xor-type)]))) 


;; parse-input-list: Función que analiza una lista de entradas de una compuerta.
;; - Si la lista está vacía, devuelve una lista vacía de entradas.
;; - Si encuentra 'input_list, lo ignora y sigue analizando el resto.
;; - Si el elemento actual es un booleano, lo convierte en una entrada booleana.
;; - Si el elemento actual es un símbolo, lo convierte en una referencia de entrada.
(define parse-input-list
  (lambda (input-list)
    (cond
      [(null? input-list) (empty-input-list)]  
      
      [(equal? (car input-list) 'input_list)
       (parse-input-list (cdr input-list))]  
      
      [(boolean? (car input-list)) 
       (bool-input (car input-list) (parse-input-list (cdr input-list)))]  
      
      [(symbol? (car input-list)) 
       (ref-input (car input-list) (parse-input-list (cdr input-list)))]))) 




;; UNPARSEBNF: AST -> 'Text
;;             AST -> list

;; - Retorna una lista con símbolo 'circuit' como primer elemento y llama a unparse-gate-list
;;   para procesar la lista de compuertas.
(define UNPARSEBNF
  (lambda (AST)
    (cases circuit AST
      (a-circuit (gate-list)
        (list 'circuit (unparse-gate-list gate-list))))))

;; unparse-gate-list: Convierte una estructura gate-list a su representación en listas.
;; - Inserta símbolo 'gate-list al inicio de la lista.
;; - Llama a unparse-gates para procesar la lista de compuertas.
(define (unparse-gate-list gate-list)
  (cons 'gate_list (unparse-gates gate-list)))

;; unparse-gates: Recorre la estructura de gate-list y devuelve una lista de compuertas.
;; - Si la lista de compuertas está vacía, devuelve una lista vacía.
;; - Si hay compuertas, procesa la primera con unparse-gate y procesa el resto recursivamente
;;   con unparse-gates.
(define (unparse-gates gates)
  (cases gate-list gates
    (empty-gate-list () '())
    (cons-gate (gate gate-list)
      (cons (unparse-gate gate)
            (unparse-gates gate-list)))))

;; unparse-gate: Convierte una compuerta a la lista de forma ('gate <id> <type> <input-list>).
;; - Extrae el id, llama unparse-type y unparse-input-list para extraer el tipo
;;   y la lista de entradas
;; - Devuelve una lista con símbolo 'gate en la primera posición y los demás elementos.
(define (unparse-gate g)
  (cases gate g
    (a-gate (id gate-type inputs)
      (list 'gate
            id
            (unparse-type gate-type)
            (unparse-input-list inputs)))))

;; unparse-type: Convierte un tipo de compuerta a su representación en listas.
;; - Devuelve una lista de forma '(type <tipo>).
(define (unparse-type t)
  (cases type t
    (and-type () '(type and))
    (or-type  () '(type or))
    (not-type () '(type not))
    (xor-type () '(type xor))))

;; unparse-input-list: Convierte una estructura input-list a la forma ('input-list (<inputs>)).
;; - Inserta símbolo 'input-list al inicio de la lista.
;; - Llama a unparse-inputs para procesar la lista de entradas.
(define (unparse-input-list input-list)
  (cons 'input_list (unparse-inputs input-list)))

;; unparse-inputs: Recorre la estructura de input-list y  acumula sus elementos (booleanos o referencias).
;; - Si la lista de entradas está vacía, devuelve una lista vacía.
;; - Si la entrada es un valor booleano, lo agrega a la lista y procesa el resto recursivamente.
;; - Si la entrada es una referencia, lo agrega a la lista y procesa el resto recursivamente.
(define (unparse-inputs inputs)
  (cases input-list inputs
    (empty-input-list () '())
    (bool-input (bool inputs)
      (cons bool (unparse-inputs inputs)))
    (ref-input (ref inputs)
      (cons ref (unparse-inputs inputs)))))

;; ========================================
;; Pruebas PARSEBNF
;; ========================================

;; Devuelven el AST de cada entrada.
(define exp1 
  (PARSEBNF
    '(circuit
      (gate_list
        (gate G1 (type or) (input_list A B))
        (gate G2 (type and) (input_list A B))
        (gate G3 (type not) (input_list G2))
        (gate G4 (type and) (input_list G1 G3))))))

(define exp2 
  (PARSEBNF
    '(circuit
       (gate_list
       (gate G1 (type or) (input_list A B))
       (gate G2 (type not) (input_list G1))))))

(define exp3 
  (PARSEBNF
    '(circuit
      (gate_list
        (gate G1 (type not) (input_list #t))))))

(display "Pruebas de PARSEBNF:")
(newline)
(display "\nPrueba 1:")
(newline)
(display exp1)
(newline)
(display "\nPrueba 2:")
(newline)
(display exp2)
(newline)
(display "\nPrueba 3:")
(newline)
(display exp3)
(newline)

;; ========================================
;; Pruebas UNPARSEBNF
;; ========================================

;; Devuelven las entradas iniciales que se dieron al PARSEBNF

(display "\nPruebas de UNPARSEBNF:")
(newline)
(display "\nPrueba 1:")
(newline)
(display (UNPARSEBNF exp1))
(newline)
(display "\nPrueba 2:")
(newline)
(display (UNPARSEBNF exp2))
(newline)
(display "\nPrueba 3:")
(newline)
(display (UNPARSEBNF exp3))
(newline)

