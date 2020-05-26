#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA WORKSPACE > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea un workspace
;Dominio: String X ... X String
;Recorrido: Workspace
(define (workspace . archivo)
  (if (> (calcular-largo-lista archivo) 0)
      (if (aplicar-funcion-lista string? (calcular-largo-lista archivo) archivo)
          archivo
          '())
      '()))
     

;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista representa un workspace
;Dominio: 'a type
;Recorrido: Booleano
(define workspace? (lambda (L)
                     (if (list? L)
                         (if (null? L)
                             #f
                             (if (aplicar-funcion-lista string? (calcular-largo-lista L) L)
                                 #t
                                 #f))
                         #f)))
                         

;-----<SELECTORES>-----


;Descripción: Función que obtiene un archivo de la posición n (elemento) de unworkspace. El workspace comienza con la posición 0
;Dominio: Entero X Workspace
;Recorrido: String 
;Recursión: Cola
(define get-archivo-workspace (lambda (n L)
                                (if (workspace? L)
                                    (if (= n 0)
                                        (car L)
                                        (get-archivo-workspace (- n 1) (cdr L)))
                                    -1)))
                          
                          
;-----<MODIFICADORES>-----

;Descripción: Función que agrega un archivo al wokspace
;Dominio: String X Workspace
;Recorrido: Workspace
;Recursión: Cola
(define agregar-archivo-workspace (lambda (archivo L)
                                    (define agregar-archivo-workspace-aux (lambda (archivo L nuevo-L)
                                                                  (if (null? L)
                                                                      (agregar-elemento-final-lista archivo nuevo-L)
                                                                      (agregar-archivo-workspace-aux archivo (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                    (if (and (workspace? L) (string? archivo))
                                        (agregar-archivo-workspace-aux archivo L '())
                                        -1)))

;Descripción: Función que elimina un archivo del workspace
;Dominio: String x Workspace
;Recorrido: Workspace
;Recursión: Cola
(define eliminar-archivo-workspace (lambda (archivo L)
                                     (define eliminar-archivo-workspace-aux (lambda (archivo L nuevo-L)
                                                                    (if (equal? archivo (car L))
                                                                        (if (null? (cdr L))
                                                                            nuevo-L
                                                                            (agregar-lista-final-lista (cdr L) nuevo-L))
                                                                        (eliminar-archivo-workspace-aux archivo (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                     (if (and (workspace? L) (elemento-en-lista archivo L))
                                         (eliminar-archivo-workspace-aux archivo L '())
                                         -1)))
                                                             
                                                                             