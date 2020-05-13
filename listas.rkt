#lang racket

(provide (all-defined-out))

;Descripción: Función que calcula el largo de una lista
;Dominio: Lista 
;Recorrido: Número natural + {0}
;Recursión: Cola
(define calcular-largo-lista (lambda (L)
                               (define calcular-largo-lista-aux (lambda (L n)
                                                                  (if (null? L)
                                                                      n
                                                                      (calcular-largo-lista-aux (cdr L) (+ n 1)))))
                               (calcular-largo-lista-aux L 0)))

;Descripción: Funcion que aplica una funcion a un elemento
;Dominio: Función y elemento que puede ser un string o entero
;Recorrido: Booleano
;Recursión:
(define aplicar-funcion-elemento (lambda (f elemento)
                                   (if (f elemento)
                                       #t
                                       #f)))

;Descripción: Función que aplica una función a todos los elementos de una lista
;Dominio: Función, entero representando el largo de lista y una lista
;Recorrido: Booleano
;Recursión: Cola
(define aplicar-funcion-lista (lambda (f n L)
                                (if (null? L)
                                    #t
                                    (if (aplicar-funcion-elemento f (car L))
                                        (aplicar-funcion-lista f (- n 1) (cdr L))
                                        #f))))

;Descripción: Función que agrega un elemento al final de una lista
;Dominio: Elemento que puede ser un string o entero y una lista
;Recorrido: Lista con nuevo elemento
;Recursión:
(define agregar-elemento-final-lista (lambda (elemento L)
                                       (append L (list elemento))))

;Descripción: Función que agrega una lista aux a una lista
;Dominio: Dos listas
;Recorrido: Lista que contiene a otra lista
;Recursión: Cola
(define agregar-lista-final-lista (lambda (lista L)
                                    (define agregar-lista-final-lista-aux (lambda (largo lista L)
                                                                            (if (= largo 1)
                                                                                (agregar-elemento-final-lista (car lista) L)
                                                                                (agregar-lista-final-lista-aux (- largo 1) (cdr lista) (agregar-elemento-final-lista (car lista) L)))))
                                    (agregar-lista-final-lista-aux (calcular-largo-lista lista) lista L)))

;Descripción: Función que comprueba si un elemento se encuentra dentro de una lista
;Dominio: String o entero representando el elemento y una lista 
;Recorrido: Booleano
;Recursión: Cola
(define elemento-en-lista (lambda (elemento L)
                            (if (equal? elemento (car L))
                                #t
                                (if (null? (cdr L))
                                    #f
                                    (elemento-en-lista elemento (cdr L))))))
                                
                             













