#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA LOCAL REPOSITORY > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:
(define (local-repository . commit)
  (if (> (calcular-largo-lista commit) 0)
      (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
          commit
          '())
      '()))

;-----<PERTENENCIA>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:

;-----<SELECTORES>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:

;-----<MODIFICADORES>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:

