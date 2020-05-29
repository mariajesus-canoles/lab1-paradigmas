#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA INDEX > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea un index
;Dominio: String X ... X String
;Recorrido: Index
(define (index . archivo)
  (if (> (calcular-largo-lista archivo) 0)
      (if (aplicar-funcion-lista string? (calcular-largo-lista archivo) archivo)
          archivo
          null)
      null))

;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista representa un index
;Dominio: 'a type
;Recorrido: Booleano
(define index? (lambda (L)
                 (if (list? L)
                     (if (null? L)
                         #f
                         (if (aplicar-funcion-lista string? (calcular-largo-lista L) L)
                             #t
                             #f))
                     #f)))

;-----<SELECTORES>-----


;Descripción: Función que obtiene un archivo en la posición n (elemento) del la lista que representa el index (lista). La lista comienza con la posición 0
;Dominio: Entero X Index
;Recorrido: String 
;Recursión: Cola
(define get-archivo-index (lambda (n L)
                            (if (index? L)
                                (if (= n 0)
                                    (car L)
                                    (get-archivo-index (- n 1) (cdr L)))
                                null)))

;-----<MODIFICADORES>-----

;Descripción: Función que agrega un archivo al index
;Dominio: String X Index
;Recorrido: Index
;Recursión: Cola
(define agregar-archivo-index (lambda (archivo L)
                                (define agregar-archivo-index-aux (lambda (archivo L nuevo-L)
                                                                    (if (null? L)
                                                                        (agregar-elemento-final-lista archivo nuevo-L)
                                                                        (agregar-archivo-index-aux archivo (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                (if (and (index? L) (string? archivo))
                                    (agregar-archivo-index-aux archivo L null)
                                    null)))



;Descripción: Función que elimina un archivo del index
;Dominio: String X Index
;Recorrido: Index
;Recursión: Cola
(define eliminar-archivo-index (lambda (archivo L)
                                 (define eliminar-archivo-index-aux (lambda (archivo L nuevo-L)
                                                                      (if (equal? archivo (car L))
                                                                          (if (null? (cdr L))
                                                                              nuevo-L
                                                                              (agregar-lista-final-lista (cdr L) nuevo-L))
                                                                          (eliminar-archivo-index-aux archivo (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                 (if (and (index? L) (elemento-en-lista archivo L))
                                     (eliminar-archivo-index-aux archivo L null)
                                     null)))

;-----<MODIFICADORES>-----
;Recorrido: Boolean
(define archivo-en-index (lambda (archivo index)
                               (elemento-en-lista archivo index)))







                               