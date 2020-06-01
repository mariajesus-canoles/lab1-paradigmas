#lang racket

(require "listas.rkt")
(require "tda_1.rkt")
(require "tda_2.rkt")
(require "tda_3.rkt")
(require "tda_4.rkt")
(require "tda_5.rkt")
(require racket/date)


; ----------------------- < FUNCIONES OBLIGATORIAS > -----------------------

;Descripción: Función que permite aplicar los comandos en git 
;Dominio: Comando
;Recorrido: Función
(define git (lambda (comando)
                (if (equal? comando pull)
                    pull
                    (if (equal? comando add)
                        add
                        (if (equal? comando commit)
                            commit
                            (if (equal? comando push)
                                push
                                (if (equal? comando status)
                                    status
                                    (if (equal? comando log)
                                        log
                                        (if (equal? comando branch)
                                            branch
                                            null)))))))))


;Descripción: Función auxiliar de la función "pull". Crea la nueva zona con el historial del comando aplicado
;Dominio: Lista Strings o Null X Zonas X Remote-Repository X Lista Strings
;Recorrido: Zonas
;Recursión: Cola
(define pull-aux (lambda (inf zonas remote-repository archivos)
                   (if (null? remote-repository)
                       (list (agregar-elemento-final-lista (cons "PULL" (date->string (current-date) second)) inf) (set-workspace-zonas (agregar-lista-final-lista archivos (get-workspace-zonas zonas)) zonas))
                       (pull-aux inf zonas (cdr remote-repository) (agregar-lista-final-lista (cdr (car remote-repository)) archivos)))))


;Descripción: Función que trae el contenido del remote repository al workspace 
;Dominio: Zonas
;Recorrido: Zonas
;Recursión: Cola (funcion auxiliar)
(define pull (lambda (zonas)
               (if (zonas? zonas)
                   (pull-aux null zonas (cdr (get-remote-repository-zonas zonas)) null)
                   (if (zonas? (cadr zonas))
                       (pull-aux (car zonas) (cadr zonas) (cdr (get-remote-repository-zonas (cadr zonas))) null)
                       null))))


;Descripción: Función auxiliar de la función "add". Agrega los archivos ingresados al Index
;Dominio: Lista String o Null X Index
;Recorrido: Zonas
;Recursión: Natural
(define add-aux (lambda (archivos index)
                  (if (null? archivos)
                      (if (null? index)
                          null
                          (cons (car index) (add-aux null (cdr index))))
                      (if (archivo-en-index (car archivos) index)
                          (add-aux (cdr archivos) index)
                          (cons (car archivos) (add-aux (cdr archivos) index))))))


;Descripción: Función que agrega los cambios del Workspace al Index
;Dominio: Lista String o Null X Zonas
;Recorrido: Zonas
;Recursión: Natural (función auxiliar)
(define add (lambda (archivos)
              (lambda (zonas)
                (if (zonas? zonas)
                    (if (null? archivos)
                        (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) null) (set-index-zonas (add-aux (get-workspace-zonas zonas) (get-index-zonas zonas)) zonas))
                        (if (archivos-en-workspace archivos (get-workspace-zonas zonas))
                            (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) null) (set-index-zonas (add-aux archivos (get-index-zonas zonas)) zonas))
                            null))
                    (if (zonas? (cadr zonas))
                        (if (null? archivos)
                            (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) (car zonas)) (set-index-zonas (add-aux (get-workspace-zonas (cadr zonas)) (get-index-zonas (cadr zonas))) (cadr zonas)))
                            (if (archivos-en-workspace archivos (get-workspace-zonas (cadr zonas)))
                                (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) (car zonas)) (set-index-zonas (add-aux archivos (get-index-zonas (cadr zonas))) (cadr zonas)))
                                null))
                        null)))))


;Descripción: Función auxiliar de la función "commit". Crea una nueva zona con el historial del comando aplicado
;Dominio: Lista o Null X String X Zonas
;Recorrido: Zonas
(define commit-aux (lambda (inf mensaje zonas)
                     (list (agregar-elemento-final-lista (cons "COMMIT" (date->string (current-date) second)) inf)
                           (set-local-repository-zonas (agregar-commit-local-repository (agregar-elemento-principio-lista mensaje (get-index-zonas zonas)) (get-local-repository-zonas zonas)) zonas))))
                         

;Descripción: Función que genera un commit
;Dominio: String X Zonas
;Recorrido: Zonas
(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (string? mensaje)
                       (if (zonas? zonas)
                           (commit-aux null mensaje zonas)
                           (if (zonas? (cadr zonas))
                               (commit-aux (car zonas) mensaje (cadr zonas))
                               null))
                       null))))


;Descripción: Función auxiliar de la función "push".Crea una nueva zona con el historial del comando aplicado
;Dominio: Lista o Null X Zonas
;Recorrido: Zonas
(define push-aux (lambda (inf zonas)
                   (list (agregar-elemento-final-lista (cons "PUSH" (date->string (current-date) second)) inf)
                   (set-remote-repository-zonas (agregar-commits-remote-repository (verificar-cambios-repositorys (cdr (caddr zonas)) (cdr (cadddr zonas)) null) (get-remote-repository-zonas zonas)) zonas))))
                   

;Descripción: Función que envia los commits del index al local repository
;Dominio: Zonas
;Recorrido: Zonas
(define push (lambda (zonas)
               (if (zonas? zonas)
                   (push-aux null zonas)
                   (if (zonas? (cadr zonas))
                       (push-aux (car zonas) (cadr zonas))
                       null))))


;Descripcion: Función auxiliar de la función "zonas->string". Crea un string con la lista ingresada agregando saltos de linea
;Dominio: Lista String
;Recorrido: String
;Recursion: Cola
(define zonas->string-aux (lambda (L)
                            (if (null? (cdr L))
                                (car L)
                                (string-append (car L) "\n" (zonas->string-aux (cdr L))))))


;Descripcion: Función auxiliar 2 de la función "zonas->string". Crea un string con la lista ingresada agregando saltos de linea que añade a un texto 
;Dominio: Lista String X Sring
;Recorrido: String
;Recursion: Cola
(define zonas->string-aux2 (lambda (L texto)
                             (if (null? L)
                                 texto
                                 (zonas->string-aux2 (cdr L) (string-append texto "\n\n" (zonas->string-aux (car L)))))))


;Descripcion: Función auxiliar 3 de la función "zonas->string". Crea un string con el comando aplicado
;Dominio: Lista String 
;Recorrido: String                            
(define zonas->string-aux3 (lambda (L)
                             (string-append "{" (cdr L) "} se aplica el comando " (car L) "\n")))


;Descripcion: Función auxiliar 4 de la función "zonas->string". Crea un string con la lista ingresada agregando saltos de linea que añade a un texto 
;Dominio: Lista String X String
;Recorrido: String
;Recursion: Cola
(define zonas->string-aux4 (lambda (L texto)
                             (if (null? L)
                                 texto
                                 (zonas->string-aux4 (cdr L) (string-append texto (zonas->string-aux3 (car L)))))))


;Descripcion: Función que genera un string con el contenido de Zonas
;Dominio: Zonas
;Recorrido: String
(define zonas->string (lambda (zonas)
                        (if (zonas? zonas)
                            (string-append "\n\n══════════════«  WORKSPACE  »═══════════════\n\n" (zonas->string-aux (car zonas))
                                           "\n\n════════════════«  INDEX  »═════════════════\n\n" (zonas->string-aux (cadr zonas))
                                           "\n\n═══════════«  LOCAL REPOSITORY  »═══════════" (zonas->string-aux2 (invertir-lista (cdr (caddr zonas)) null) "")
                                           "\n\n══════════«  REMOTE REPOSITORY  »═══════════" (zonas->string-aux2 (invertir-lista (cdr (cadddr zonas)) null) ""))
                            (if (zonas? (cadr zonas))
                                (string-append "\n\n══════════════«  WORKSPACE  »═══════════════\n\n" (zonas->string-aux (car (cadr zonas)))
                                               "\n\n════════════════«  INDEX  »═════════════════\n\n" (zonas->string-aux (cadr (cadr zonas)))
                                               "\n\n═══════════«  LOCAL REPOSITORY  »═══════════" (zonas->string-aux2 (invertir-lista (cdr (caddr (cadr zonas))) null) "")
                                               "\n\n══════════«  REMOTE REPOSITORY  »═══════════" (zonas->string-aux2 (invertir-lista (cdr (cadddr (cadr zonas))) null) "")
                                               "\n\n══════════«  HISTORY COMMANDS  »════════════\n\n" (zonas->string-aux4 (invertir-lista (car zonas) null) "") "\n\n")
                                null))))
                                               
 



                                                     

; ----------------------- < FUNCIONES EXTRAS > -----------------------



;Descripción: Función que retorna un string con la información del ambiente de trabajo
;Dominio: Zonas
;Recorrido: String
(define status (lambda (zonas)
                 (if (zonas? zonas)
                     (string-append "ARCHIVOS AGREGADOS AL INDEX:\n"
                                    (if (null? (archivos-en-index (cadr zonas) (archivos-local-repository (cdr (caddr zonas)) null) null))
                                        "No se han agregado nuevos archivos al Index"
                                        (zonas->string-aux (archivos-en-index (cadr zonas) (archivos-local-repository (cdr (caddr zonas)) null) null)))
                                    "\n\nCANTIDAD DE NUEVOS COMMITS EN EL LOCAL REPOSITORY:\n"
                                    (if (null? (verificar-cambios-repositorys (cdr (caddr zonas)) (cdr (cadddr zonas)) null))
                                        "No se han agregado nuevos commits en el Local Repository"
                                        (number->string (calcular-largo-lista (verificar-cambios-repositorys (cdr (caddr zonas)) (cdr (cadddr zonas)) null))))
                                    "\n\nRAMA ACTUAL DEL LOCAL REPOSITORY:\n" (car (caddr zonas)) "\n")
                     (if (zonas? (cadr zonas))
                         (string-append "ARCHIVOS AGREGADOS AL INDEX:\n"
                                        (if (null? (archivos-en-index (cadr (cadr zonas)) (archivos-local-repository (cdr (caddr (cadr zonas))) null) null))
                                            "No se han agregado nuevos archivos al Index"
                                            (zonas->string-aux (archivos-en-index (cadr (cadr zonas)) (archivos-local-repository (cdr (caddr (cadr zonas))) null) null)))
                                        "\n\nCANTIDAD DE NUEVOS COMMITS EN EL LOCAL REPOSITORY:\n"
                                        (if (null? (verificar-cambios-repositorys (cdr (caddr (cadr zonas))) (cdr (cadddr (cadr zonas))) null))
                                            "No se han agregado nuevos commits en el Local Repository"
                                            (number->string (calcular-largo-lista (verificar-cambios-repositorys (cdr (caddr (cadr zonas))) (cdr (cadddr (cadr zonas))) null))))
                                        "\n\nRAMA ACTUAL DEL LOCAL REPOSITORY:\n" (car (caddr (cadr zonas))) "\n")
                         null))))


;Descripcion: Funcion auxiliar de la funcion "log". Genera un string de los ultimos n commits
;Dominio: Lista String X Entero X String
;Recorrido: String
;Recursion: Cola
(define log-aux (lambda (commits n texto)
                  (if (= n 0)
                      texto
                      (log-aux (cdr commits) (- n 1) (string-append texto (car (car commits)) "\n")))))
                      

;Descripcion: Función que genera un string con los ultimos 5 commits
;Dominio: Zonas
;Recorrido: String
(define log (lambda (zonas)
              (if (zonas? zonas)
                  (if (>= (calcular-largo-lista (cdr (get-remote-repository-zonas zonas))) 5)
                      (string-append "Los últimos 5 commits sobre el repositorio son:\n"
                                     (log-aux (get-commits-remote-repository 5 (invertir-lista (cdr (get-remote-repository-zonas zonas)) null) null) 5 ""))
                      (string-append "\nLos commits almacenados en el repositorio son menores que 5\n"))
                  (if (zonas? (cadr zonas))
                      (if (>= (calcular-largo-lista (cdr (get-remote-repository-zonas (cadr zonas)))) 5)
                          (string-append "Los últimos 5 commits sobre el repositorio son:\n"
                                         (log-aux (get-commits-remote-repository 5 (invertir-lista (cdr (get-remote-repository-zonas (cadr zonas))) null) null) 5 ""))
                          (string-append "\nLos commits almacenados en el repositorio son menores que 5\n"))
                      null))))
                      

;Descripcion: Funcion auxiliar de la funcion "branch". Crea una nueva zona agregando el historial del comando aplicado                  
;Dominio:
;Recorrido:  
(define branch-aux (lambda (inf repository zonas)
                     (list (agregar-elemento-final-lista (cons "BRANCH" (date->string (current-date) second)) inf) (set-repository-zonas repository zonas))))


;Descripcion: Funcion que toma el historial del ultimo commit y genera una nueva rama                  
;Dominio: String X Zonas
;Recorrido: Zonas               
(define branch (lambda (nombre-rama)
                 (lambda (zonas)
                   (if (zonas? zonas)
                       (branch-aux null (list nombre-rama (get-commit-remote-repository (- (calcular-largo-lista (cdr (get-remote-repository-zonas zonas))) 1) (get-remote-repository-zonas zonas))) zonas)
                       (if (zonas? (cadr zonas))
                           (branch-aux (car zonas) (list nombre-rama (get-commit-remote-repository (- (calcular-largo-lista (cdr (get-remote-repository-zonas (cadr zonas)))) 1) (get-remote-repository-zonas (cadr zonas)))) (cadr zonas))
                           null)))))
                           
                       




; ----------------------- < EJEMPLOS DEL USO DE LAS FUNCIONES > -----------------------



;EJEMPLOS DE USO FUNCIONES OBLIGATORIAS

;(git pull)
;(git push)
;(git commit)

;((git pull) '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c"))))
;((git pull) '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c")))))
;((git pull) '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am"))(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c"))("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))

;(((git add) (list "arch9.c")) '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c"))))
;(((git add) null) '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c"))))
;(((git add) null) '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am"))(("arch9.c" "arch8.c")("arch7.c")("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c"))("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))

;(((git commit) "Tercer commit") '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c"))))
;(((git commit) "Tercer commit") '((("ADD" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c" "arch8.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))
;(((git commit) "Tercer commit") '((("ADD" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c")  ("arch7.c" "arch8.c" "arch0.c")  ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))

;((git push) '((("ADD" . "Monday, June 1st, 2020 12:34:26am"))(("arch9.c" "arch8.c") ("arch7.c" "arch8.c" "arch0.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c")))))
;((git push) '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c")) ("Master" ("Primer commit" "arch6.c"))))
;((git push) '((("ADD" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c" "arch8.c" "arch0.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c") ("Tercer commit" "arch0.c"))("Master" ("Primer commit" "arch6.c")))))

;(zonas->string '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c")) ("Master" ("Primer commit" "arch6.c"))))
;(zonas->string '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c")  ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))
;(display (zonas->string '((("ADD" . "Monday, June 1st, 2020 12:34:26am") ("PUSH" . "Monday, June 1st, 2020 1:12:14am")) (("arch9.c" "arch8.c") ("arch7.c" "arch8.c" "arch0.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c") ("Tercer commit" "arch0.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c") ("Tercer commit" "arch0.c"))))))



;EJEMPLOS DE USO DE FUNCIONES EXTRAS

;((git status) '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c")) ("Master" ("Primer commit" "arch6.c"))))
;(display ((git status)  '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arch3.5")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c"))))))
;(display ((git status)  '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c"))))))

;((git log)  '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arch3.5.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))
;(display ((git log)  '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am"))  (("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arch3.5.c") ("Cuarto commit" "arch5.3.c") ("Quinto commit" "arch21.c" "arch43.c") ("Sexto commit" "archX.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arch3.5.c")  ("Cuarto commit" "arch5.3.c") ("Quinto commit" "arch21.c" "arch43.c") ("Sexto commit" "archX.c"))))))
;(display ((git log)  '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am")) (("arch9.c" "arch8.c") ("arch7.c")  ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arch3.5.c") ("Cuarto commit" "arch5.3.c") ("Quinto commit" "arch21.c" "arch43.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arch3.5.c")  ("Cuarto commit" "arch5.3.c") ("Quinto commit" "arch21.c" "arch43.c"))))))

;(((git branch) "Nueva Ramita") '((("COMMIT" . "Monday, June 1st, 2020 12:34:26am"))(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c")))))
;(((git branch) "Ramita Jesús") '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arcX.c"))("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arcX.c"))))
;(((git branch) "Ramita XXX") '(("arch9.c" "arch8.c") ("arch7.c") ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arcX.c")) ("Master" ("Primer commit" "arch6.c") ("Segundo commit" "arch5.c" "arch4.c") ("Tercer commit" "arcX.c"))))



