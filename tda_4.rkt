#lang racket
(require "listas.rkt")
(provide (all-defined-out))
;ej: '("master" ("Primer commit" "archivo5") ("Segundo commit" "archivo4" "archivo4.5"))
; ----------------------- < TDA REMOTE REPOSITORY > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea un repositorio remoto, donde el primer elemento representa la rama,
;             el segundo el mensaje del commit y los demás los archivos que acompañan al commit
;Dominio: String X ... X String
;Recorrido: Remote-Repository
(define (remote-repository rama . commit)
  (if (string? rama)
      (if (> (calcular-largo-lista commit) 1)
          (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
              (cons rama (cons commit null))
              null)
          null)
      null))


;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista ingresada cumple con los requisitos para considerarla un repositorio remoto
;Dominio: 'a type
;Recorrido: Booleano
;Recursión: Cola
(define remote-repository? (lambda (L)
                             (define remote-repository?-aux (lambda (L)
                                                              (if (null? (cdr L))
                                                                  (if (and (> (calcular-largo-lista (car L)) 1) (aplicar-funcion-lista string? (calcular-largo-lista (car L)) (car L)))
                                                                      #t
                                                                      #f)
                                                                  (if (> (calcular-largo-lista (car L)) 1)
                                                                      (if (aplicar-funcion-lista string? (calcular-largo-lista (car L)) (car L))
                                                                          (remote-repository?-aux (cdr L))
                                                                          #f)
                                                                      #f))))
                             (if (list? L)
                                 (if (null? L)
                                     #f
                                     (remote-repository?-aux (cdr L)))
                                 #f)))
                                       
                           

;-----<SELECTORES>-----

;Descripción: Función que obtiene un commit del repositorio remoto 
;Dominio: Entero X Remote-Repository
;Recorrido: Commit
(define get-commit-remote-repository (lambda (n L)
                                      (if (remote-repository? L)
                                          (if (number? n)
                                              (if (< n (calcular-largo-lista L))
                                                  (obtener-elemento-lista n (cdr L))
                                                  null)
                                              null)
                                          null)))


;Descripción: Funcion que obtiene los ultimos n commits en el remote repository
;Dominio: Entero X Lista Strings X Lista Strings o Null
;Recorrido: Lista Strings
;Recursion: Cola
(define get-commits-remote-repository (lambda (n L nuevo-L)
                                        (if (= n 0)
                                            nuevo-L
                                            (get-commits-remote-repository (- n 1) (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))



;-----<MODIFICADORES>-----

;Descripción: Función que agrega un commit a un repositorio remoto
;Dominio: Commit X Remote-Repository
;Recorrido: Remote-Repository
;Recursión: Cola
(define agregar-commit-remote-repository (lambda (commit L)
                                          (define agregar-commit-remote-repository-aux (lambda (commit L nuevo-L)
                                                                                        (if (null? L)
                                                                                            (agregar-elemento-final-lista commit nuevo-L)
                                                                                            (agregar-commit-remote-repository-aux commit (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                          (if (remote-repository? L)
                                              (if (> (calcular-largo-lista commit) 1)
                                                  (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
                                                      (agregar-commit-remote-repository-aux commit L null)
                                                      null)
                                                  null)
                                              null)))

;Descripcion: Funcion que agrega commits a un repositorio remoto
;Dominio: Lista de Listas Strings X Remote-Repository
;Recorrido: Remote-Repository
(define agregar-commits-remote-repository (lambda (commits L)
                                            (if (null? commits)
                                                L
                                                (agregar-commits-remote-repository (cdr commits) (agregar-commit-remote-repository (car commits) L)))))
                                            
                                            
                                            
                                            
                                              