#lang racket

(require "listas.rkt")
(require "tda_1.rkt")
(require "tda_2.rkt")
(require "tda_3.rkt")
(require "tda_4.rkt")
(provide (all-defined-out))

; ----------------------- < TDA ZONAS > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea la zona de trabajo que contiene al workspace, index, local y remote repository
;Dominio: Cuatro listas, donde la primera lista representa el workspace, la segunda el index, la tercera el local repository y la cuarta el remote repository
;Recorrido: Una lista de cuatro listas
;Recursión:
(define zonas (lambda (workspace index local-repository remote-repository)
                (if (workspace? workspace)
                    (if (index? index)
                        (if (local-repository? local-repository)
                            (if (remote-repository? remote-repository)
                                (cons workspace (cons index (cons local-repository (cons remote-repository null))))
                                '())
                            '())
                        '())
                    '())))


;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista ingresada cumple con los requisitos para considerarse zonas
;Dominio: Lista de listas
;Recorrido: Booleano
;Recursión: 
(define zonas? (lambda (L)
                 (if (list? L)
                     (if (null? L)
                         #f
                         (if (workspace? (car L))
                             (if (index? (cadr L))
                                 (if (local-repository? (caddr L))
                                     (if (remote-repository? (cadddr L))
                                         #t
                                         #f)
                                     #f)
                                 #f)
                             #f))
                     #f)))
                           

;-----<SELECTORES>-----

;Descripción: Función que obtiene el workspace de zonas
;Dominio: Una lista de listas que representa zonas
;Recorrido: Lista que representa el workspace
;Recursión:
(define get-workspace-zonas (lambda (L)
                              (if (zonas? L)
                                  (car L)
                                  -1)))


;Descripción: Función que obtiene el index de zonas
;Dominio: Una lista de listas que representa zonas
;Recorrido: Lista que representa el index
;Recursión:
(define get-index-zonas (lambda (L)
                          (if (zonas? L)
                              (cadr L)
                              -1)))


;Descripción: Función que obtiene el local repository de zonas
;Dominio: Una lista de listas que representa zonas
;Recorrido: Lista que representa el local repository
;Recursión:
(define get-local-repository-zonas (lambda (L)
                                     (if (zonas? L)
                                         (caddr L)
                                         -1)))


;Descripción: Función que obtiene el remote repository de zonas
;Dominio: Una lista de listas que representa zonas
;Recorrido: Lista que representa el remote repository
;Recursión:
(define get-remote-repository-zonas (lambda (L)
                                      (if (zonas? L)
                                          (cadddr L)
                                          -1)))


;-----<MODIFICADORES>-----

;Descripción: Función que modifica el workspace de las zonas
;Dominio: Lista que representa el workspace y una lista de listas que representa las zonas
;Recorrido: Una lista de listas que representa las zonas
;Recursión:
(define set-workspace-zonas (lambda (workspace L)
                              (if (zonas? L)
                                  (if (workspace? workspace)
                                      (cons workspace (cons (get-index-zonas L) (cons (get-local-repository-zonas L) (cons (get-remote-repository-zonas L) null))))
                                      -1)
                                  -1)))


;Descripción: Función que modifica el index de las zonas
;Dominio: Lista que representa el index y una lista de listas que representa las zonas
;Recorrido: Una lista de listas que representa las zonas
;Recursión:
(define set-index-zonas (lambda (index L)
                              (if (zonas? L)
                                  (if (index? index)
                                      (cons (get-workspace-zonas L) (cons index (cons (get-local-repository-zonas L) (cons (get-remote-repository-zonas L) null))))
                                      -1)
                                  -1)))


;Descripción: Función que modifica el local repository de las zonas
;Dominio: Lista que representa el local repository y una lista de listas que representa las zonas
;Recorrido: Una lista de listas que representa las zonas
;Recursión:
(define set-local-repository-zonas (lambda (local-repository L)
                              (if (zonas? L)
                                  (if (local-repository? local-repository)
                                      (cons (get-workspace-zonas L) (cons (get-index-zonas L) (cons local-repository (cons (get-remote-repository-zonas L) null))))
                                      -1)
                                  -1)))


;Descripción: Función que modifica el remote repository de las zonas
;Dominio: Lista que representa el remote repository y una lista de listas que representa las zonas
;Recorrido: Una lista de listas que representa las zonas
;Recursión:
(define set-remote-repository-zonas (lambda (remote-repository L)
                              (if (zonas? L)
                                  (if (remote-repository? remote-repository)
                                      (cons (get-workspace-zonas L) (cons (get-index-zonas L) (cons (get-local-repository-zonas L) (cons remote-repository null))))
                                      -1)
                                  -1)))
                                              