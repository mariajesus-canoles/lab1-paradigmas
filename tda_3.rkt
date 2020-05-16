#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA LOCAL REPOSITORY > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea un repositorio local
;Dominio: lista de n argumentos de strings, donde el primer elemento representa la rama, el segundo el mensaje del commit y los demás los archivos que acompañan al commit
;Recorrido: una lista que contiene un elemento y una lista
;Recursión:
(define (local-repository rama . commit)
  (if (string? rama)
      (if (> (calcular-largo-lista commit) 1)
          (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
              (cons rama (cons commit null))
              '())
          '())
      '()))


;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista ingresada cumple con los requisitos para considerarla un repositorio local 
;Dominio: Lista
;Recorrido: Booleano
;Recursión: Cola
(define local-repository? (lambda (L)
                             (define local-repository?-aux (lambda (L)
                                                              (if (null? (cdr L))
                                                                  (if (and (> (calcular-largo-lista (car L)) 1) (aplicar-funcion-lista string? (calcular-largo-lista (car L)) (car L)))
                                                                      #t
                                                                      #f)
                                                                  (if (> (calcular-largo-lista (car L)) 1)
                                                                      (if (aplicar-funcion-lista string? (calcular-largo-lista (car L)) (car L))
                                                                          (local-repository?-aux (cdr L))
                                                                          #f)
                                                                      #f))))
                             (if (list? L)
                                 (if (null? L)
                                     #f
                                     (local-repository?-aux (cdr L)))
                                 #f)))
                                       
                           

;-----<SELECTORES>-----

;Descripción: Función que obtiene un commit del repositorio local
;Dominio: Una lista que representa el repositorio local y un entero n que representa la posición del commit (empieza del 0)
;Recorrido: Lista que representa un commit
;Recursión:
(define get-commit-local-repository (lambda (n L)
                                      (if (local-repository? L)
                                          (if (number? n)
                                              (if (< n (calcular-largo-lista L))
                                                  (obtener-elemento-lista n (cdr L))
                                                  -1)
                                              -1)
                                          -1)))

;-----<MODIFICADORES>-----

;Descripción: Funciónq ue agrega un commit a un repositorio local
;Dominio: Lista representa un commit y una lista que representa un repositorio local
;Recorrido: Una lista que representa un repositorio local con un commit agregado
;Recursión:

(define agregar-commit-local-repository (lambda (commit L)
                                          (define agregar-commit-local-repository-aux (lambda (commit L nuevo-L)
                                                                                        (if (null? L)
                                                                                            (agregar-elemento-final-lista commit nuevo-L)
                                                                                            (agregar-commit-local-repository-aux commit (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                          (if (local-repository? L)
                                              (if (> (calcular-largo-lista commit) 1)
                                                  (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
                                                      (agregar-commit-local-repository-aux commit L '())
                                                      -1)
                                                  -1)
                                              -1)))
                                              
                                                                                                                        
