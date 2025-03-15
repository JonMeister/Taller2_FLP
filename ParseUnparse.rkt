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
      [(null? gate-list) (empty-gate-list)]
      [(equal? (car gate-list) 'gate-list)(cons-gate (parse-a-gate (cadr gate-list)) (parse-gate-list (cdr(cdr gate-list))))]
      [else (empty-gate-list)])))  ;; Si no es una lista válida, devuelve una lista vacía

(define parse-a-gate
  (lambda (gate)
    (a-gate (cadr gate) (parse-type (caddr gate)) (parse-input-list (cadddr gate)))))

(define parse-type
  (lambda (type)
    (cond
      [(equal? type 'or) (or-type)]
      [(equal? type 'not) (not-type)]
      [(equal? type 'and) (and-type)]
      [(equal? type 'xor) (xor-type)]
      [else (unknown-type)]))) ;; Devuelve un tipo desconocido en lugar de error

(define parse-input-list
  (lambda (input-list)
    (cond
      [(null? input-list) (empty-input-list)]
      
      [(equal? (car input-list) 'input-list)
       (parse-input-list (cdr input-list))]
      
      [(boolean? (car input-list)) 
       (bool-input (car input-list) (parse-input-list (cdr input-list)))]
      [(symbol? (car input-list)) 
       (ref-input (car input-list) (parse-input-list (cdr input-list)))]
      
      [else (empty-input-list)])))  ;; Si hay entradas inválidas, devuelve una lista vacía


(PARSEBNF '(circuit 
    (gate-list
      (gate G1 and (input-list #t #f))
      (gate G2 not (input-list G1)))))
