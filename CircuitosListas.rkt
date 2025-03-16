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



;; =====================================
;; Constructores para la representación
;; =====================================


;; Constructor de un circuito
(define (make-circuit gate-list)                             
  (list 'circuit gate-list))   ;; Crea un circuito con una lista de compuertas.

;; Constructor de una lista de compuertas
(define (make-gate-list . gates)
  (cons 'gate_list gates))    ;;Crea una lista de compuertas a partir de una o más compuertas.

;; Constructor de una compuerta lógica
(define (make-gate gate-id type input-list)
  (list 'gate gate-id type input-list)) ;; Crea una compuerta lógica con un identificador, tipo y lista de entradas.

;; Constructor de una lista de entradas
(define (make-input-list . inputs)
  (cons 'input_list inputs))   ;; Crea una lista de entradas, que pueden ser valores booleanos o referencias a otras compuertas.

;; ==============================
;; Extractores para los datos
;; ==============================

; Extraer la lista de compuertas de un circuito
(define (circuit->gate-list circuit)
  (cadr circuit)) ;; Devuelve la lista de compuertas de un circuito."

;; Extraer la primera compuerta de una lista de compuertas
(define (gate-list->first gate-list)
  ;; Devuelve la primera compuerta de una lista de compuertas."
  (cadr gate-list))

;; Extraer el resto de compuertas de una lista de compuertas
(define (gate-list->rest gate-list)
  ;; Devuelve la lista de compuertas sin la primera."
  (cons 'gate-list (cddr gate-list)))

;; Extraer el identificador de una compuerta
(define (gate->gate-id gate)
  ;; Devuelve el identificador de una compuerta."
  (cadr gate))

;; Extraer el tipo de una compuerta
(define (gate->type gate)
  ;; Devuelve el tipo de una compuerta lógica."
  (caddr gate))

;; Extraer la lista de entradas de una compuerta
(define (gate->input-list gate)
  ;; Devuelve la lista de entradas de una compuerta."
  (cadddr gate))

;; Extraer el primer elemento de una lista de entradas
(define (input-list->first input-list)
  ;; Devuelve el primer elemento de una lista de entradas."
  (cadr input-list))

;; Extraer el resto de elementos de una lista de entradas
(define (input-list->rest input-list)
  ;; Devuelve la lista de entradas sin el primer elemento."
  (cons 'input-list (cddr input-list)))


;; ========================================
;; Ejemplos de uso con los circuitos dados
;; ========================================

;; Ejemplo 1: Circuito con una compuerta NOT
(define circuito1
  (make-circuit
   (make-gate-list
    (make-gate 'G1 '(type not) (make-input-list 'A)))))

;; Ejemplo 2: Circuito AND simple
(define circuito2
  (make-circuit
   (make-gate-list
    (make-gate 'G1 '(type and) (make-input-list 'A 'B)))))

;; Ejemplo 3: Combinación de compuertas
(define circuito3
  (make-circuit
   (make-gate-list
    (make-gate 'G1 '(type or) (make-input-list 'A 'B))
    (make-gate 'G2 '(type not) (make-input-list 'G1)))))

;; Ejemplo 4: Implementación de XOR sin XOR
(define circuito4
  (make-circuit
   (make-gate-list
    (make-gate 'G1 '(type or) (make-input-list 'A 'B))
    (make-gate 'G2 '(type and) (make-input-list 'A 'B))
    (make-gate 'G3 '(type not) (make-input-list 'G2))
    (make-gate 'G4 '(type and) (make-input-list 'G1 'G3)))))

;; ========================================
;; Pruebas con los circuitos dados
;; ========================================

(display "Circuito 1:")
(newline)
(newline)
(display circuito1)
(newline)
(display "\nPrueba 1: Extraer lista de compuertas de circuito1")
(newline)
(display (circuit->gate-list circuito1)) 
(newline) ;; Resultado esperado: (gate-list (gate G1 not (input-list A)))
(display "**********************************************************")
(newline)

(display "Circuito 4:")
(newline)
(newline)
(display circuito4)
(newline)
(display "\nPrueba 2: Extraer tipo de la compuerta G4")
(newline)
(display (gate->type (gate-list->first (gate-list->rest (gate-list->rest (gate-list->rest (circuit->gate-list circuito4)))))))
(newline) ;; Resultado esperado: and
(display "**********************************************************")
(newline)

(display "Circuito 2:")
(newline)
(newline)
(display circuito2)
(newline)
(display "\nPrueba 3: Extraer identificador de la primera compuerta")
(newline)
(display (gate->gate-id (gate-list->first (circuit->gate-list circuito2))))
(newline) ;; Resultado esperado: G1
(display "**********************************************************")
(newline)

