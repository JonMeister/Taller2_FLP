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
  (and-type)    ;; Compuerta AND
  (or-type)     ;; Compuerta OR
  (not-type)    ;; Compuerta NOT
  (xor-type)   ;; Compuerta XOR
  (unknown-type))

(define-datatype input-list input-list?
  (empty-input-list)                     ;; Lista vacía de entradas
  (bool-input (value boolean?)            ;; Entrada booleana
              (rest input-list?))
  (ref-input (ref symbol?)           ;; Referencia a otra compuerta
                  (rest input-list?)))

(define PARSEBNF
  (lambda (code)
    (if (equal? (car code) 'circuit)
        (a-circuit (parse-gate-list (cadr code)))
        (a-circuit (empty-gate-list)))))  ;; Si hay un error, devuelve un circuito vacío

(define parse-gate-list
  (lambda (gate-list)
    (cond
      
      [(null? gate-list)
       (empty-gate-list)]
      
      [(eq? (car gate-list) 'gate_list)
       (parse-gate-list (cdr gate-list))]
      
      [else
       (cons-gate (parse-a-gate (car gate-list))
                  (parse-gate-list (cdr gate-list)))])))

(define parse-a-gate
  (lambda (gate)
    (a-gate (cadr gate) (parse-type (caddr gate)) (parse-input-list (cadddr gate)))))

(define parse-type
  (lambda (type)
    (cond
      [(equal? type '(type or)) (or-type)]
      [(equal? type '(type not)) (not-type)]
      [(equal? type '(type and)) (and-type)]
      [(equal? type '(type xor)) (xor-type)]
      [else (unknown-type)]))) ;; Devuelve un tipo desconocido en lugar de error

(define parse-input-list
  (lambda (input-list)
    (cond
      [(null? input-list) (empty-input-list)]
      
      [(equal? (car input-list) 'input_list)
       (parse-input-list (cdr input-list))]
      
      [(boolean? (car input-list)) 
       (bool-input (car input-list) (parse-input-list (cdr input-list)))]
      [(symbol? (car input-list)) 
       (ref-input (car input-list) (parse-input-list (cdr input-list)))]
      
      [else (empty-input-list)])))  ;; Si hay entradas inválidas, devuelve una lista vacía



;; UNPARSEBNF: Convierte el AST a su representación en listas.

(define UNPARSEBNF
  (lambda (AST)
    (cases circuit AST
      (a-circuit (gate-list)
        (list 'circuit (unparse-gate-list gate-list))))))

;; Convierte una estructura gate-list a la lista con 'gate-list en la primera posición.
(define (unparse-gate-list gate-list)
  (cons 'gate_list (unparse-gates gate-list)))

;; Función para recorrer la estructura de gate-list y devolver una lista de compuertas.
(define (unparse-gates gates)
  (cases gate-list gates
    (empty-gate-list () '())
    (cons-gate (gate gate-list)
      (cons (unparse-gate gate)
            (unparse-gates gate-list)))))

;; Convierte una compuerta a la lista de forma ('gate <id> <type> <input-list>).
(define (unparse-gate g)
  (cases gate g
    (a-gate (id gate-type inputs)
      (list 'gate
            id
            (unparse-type gate-type)
            (unparse-input-list inputs)))))

;; Convierte el tipo de compuerta a su símbolo ('(type and), '(type or), etc.).
(define (unparse-type t)
  (cases type t
    (and-type () '(type and))
    (or-type  () '(type or))
    (not-type () '(type not))
    (xor-type () '(type xor))
    (unknown-type () '(type unknown))))

;; Convierte la estructura input-list a la forma ('input-list (<inputs>)).
(define (unparse-input-list input-list)
  (cons 'input_list (unparse-inputs input-list)))

;; Recorre la estructura de input-list y acumula sus elementos (booleanos o referencias).
(define (unparse-inputs inputs)
  (cases input-list inputs
    (empty-input-list () '())
    (bool-input (bool inputs)
      (cons bool (unparse-inputs inputs)))
    (ref-input (ref inputs)
      (cons ref (unparse-inputs inputs)))))



