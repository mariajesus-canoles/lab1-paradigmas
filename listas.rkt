#lang racket

(provide (all-defined-out))

;Descripción: Función que calcula el largo de una lista
;Dominio: Lista 
;Recorrido: Entero 
;Recursión: Cola
(define calcular-largo-lista (lambda (L)
                               (define calcular-largo-lista-aux (lambda (L n)
                                                                  (if (null? L)
                                                                      n
                                                                      (calcular-largo-lista-aux (cdr L) (+ n 1)))))
                               (calcular-largo-lista-aux L 0)))


;Descripción: Funcion que aplica una funcion a un elemento
;Dominio: Función X String o Entero
;Recorrido: Booleano
(define aplicar-funcion-elemento (lambda (f elemento)
                                   (if (f elemento)
                                       #t
                                       #f)))


;Descripción: Función que aplica una función a todos los elementos de una lista
;Dominio: Función X Entero X Lista
;Recorrido: Booleano
;Recursión: Cola
(define aplicar-funcion-lista (lambda (f n L)
                                (if (null? L)
                                    #t
                                    (if (aplicar-funcion-elemento f (car L))
                                        (aplicar-funcion-lista f (- n 1) (cdr L))
                                        #f))))


;Descripción: Función que aplica una función a una lista de listas
;Dominio: Función X Entero X Lista de listas
;Recorrido: Booleano
;Recursión: Cola
(define aplicar-funcion-lista-de-listas (lambda (f n L)
                                          (if (null? L)
                                              #t
                                              (if (aplicar-funcion-lista f (calcular-largo-lista (car L)) (car L))
                                                  (aplicar-funcion-lista-de-listas f (- n 1) (cdr L))
                                                  #f))))
                                          

;Descripción: Función que agrega un elemento al final de una lista
;Dominio: String o Entero X Lista
;Recorrido: Lista
(define agregar-elemento-final-lista (lambda (elemento L)
                                       (append L (list elemento))))


;Descripción: Función que agrega un elemento al principio de una lista
;Dominio: String o Entero X Lista
;Recorrido: Lista
(define agregar-elemento-principio-lista (lambda (elemento L)
                                           (append (list elemento) L)))


;Descripción: Función que agrega una lista aux a una lista
;Dominio: Dos listas
;Recorrido: Lista de listas
;Recursión: Cola
(define agregar-lista-final-lista (lambda (lista L)
                                    (define agregar-lista-final-lista-aux (lambda (largo lista L)
                                                                            (if (= largo 1)
                                                                                (agregar-elemento-final-lista (car lista) L)
                                                                                (agregar-lista-final-lista-aux (- largo 1) (cdr lista) (agregar-elemento-final-lista (car lista) L)))))
                                    (agregar-lista-final-lista-aux (calcular-largo-lista lista) lista L)))


;Descripción: Función que comprueba si un elemento se encuentra dentro de una lista
;Dominio: String o Entero X Lista
;Recorrido: Booleano
;Recursión: Cola
(define elemento-en-lista (lambda (elemento L)
                            (if (equal? elemento (car L))
                                #t
                                (if (null? (cdr L))
                                    #f
                                    (elemento-en-lista elemento (cdr L))))))


;Descripción: Función que obtiene un elemento de una lista
;Dominio: Entero X Lista
;Recorrido: Entero o String
;Recursión: Cola
(define obtener-elemento-lista (lambda (n L)
                                 (if (= n 0)
                                     (car L)
                                     (obtener-elemento-lista (- n 1) (cdr L)))))


;Descripción: Función que invierte una lista
;Dominio: Lista X Lista o Null
;Recorrido: Lista
;Recursión: Cola
(define invertir-lista (lambda (lista lista-aux)
                        (if (null? lista)
                            lista-aux
                            (invertir-lista (cdr lista) (cons (car lista) lista-aux)))))
                             

