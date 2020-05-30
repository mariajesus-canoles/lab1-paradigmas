#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA LOCAL REPOSITORY > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea un repositorio local, donde el primer elemento representa la rama,
;             el segundo el mensaje del commit y los demás los archivos que acompañan al commit
;Dominio: String X ... X String
;Recorrido: Local-Repository
(define (local-repository rama . commit)
  (if (string? rama)
      (if (> (calcular-largo-lista commit) 1)
          (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
              (cons rama (cons commit null))
              null)
          null)
      null))


;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista ingresada cumple con los requisitos para considerarla un repositorio local 
;Dominio: 'a type
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

;Descripción: Función que obtiene el ultimo commit agregado al repositorio local 
;Dominio: Entero X Local-Repository
;Recorrido: Commit
(define get-commit-local-repository (lambda (L)
                                      (if (local-repository? L)
                                                  (obtener-elemento-lista (- (calcular-largo-lista (cdr L)) 1) (cdr L))
                                                  null)))

;-----<MODIFICADORES>-----

;Descripción: Función que agrega un commit a un repositorio local
;Dominio: Commit X Local-Repository
;Recorrido: Local-Repository
;Recursión:Cola
(define agregar-commit-local-repository (lambda (commit L)
                                          (define agregar-commit-local-repository-aux (lambda (commit L nuevo-L)
                                                                                        (if (null? L)
                                                                                            (agregar-elemento-final-lista commit nuevo-L)
                                                                                            (agregar-commit-local-repository-aux commit (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                          (if (local-repository? L)
                                              (if (> (calcular-largo-lista commit) 1)
                                                  (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
                                                      (agregar-commit-local-repository-aux commit L null)
                                                      null)
                                                  null)
                                              null)))
                                              

