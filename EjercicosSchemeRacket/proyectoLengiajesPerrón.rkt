#lang racket

;(define (<nombre-funcion> <args>) <cuerpo>)

;crear un paciente
(define (crearPaciente ID  nombreApellido genero peso estatura )
	(list    ID
		 nombreApellido
		genero
		peso
                estatura
                (calcularIMC peso estatura)
                (calcularValoracion (calcularIMC peso estatura))
	)
)
(define (calcularIMC peso altura) (/ peso (* altura altura)))
(define(calcularValoracion IMC)(if (< IMC  18) 'flaquisimo 'normal))
(define pacientes '())
;funciones de consulta

(define (identificacion paciente)
	(car paciente)
)
(define (nombre paciente)
	(car paciente)
  )
(define (genero paciente)
	(car paciente)
  )
(define (peso paciente)
	(car paciente)
  )
(define (estatura paciente)
	(car paciente)
  )
(define (IMC paciente)
	(car  paciente))

(define (valoracion paciente)
	(car paciente))




(define(principal)
(define(mostrarMenu)
                (newline)
		(display "****** MENU ******" )
		(newline)
		(display "(1)   Ingresar Paciente" )
		(newline)
		(display "(2)   Buscar Paciente" )
		(newline)
		(display "(3)   Buscar Alertas" )
		(newline)
       	        (display "(4)   Modificar Datos" )
		(newline)
                (display "(5)   Eliminar Paciente" )
		(newline)
                (display "(0)   SALIR" ) 
		(newline)
                (display "---->" )
                (read)
               
                

  )

  ;función para leer datos del teclado
  (define (leerTeclado mensaje)
    (display mensaje)
    (display " ----> ")
    (read)
    )

  ; INTRODUCIR LOS DATOS DE UN DONANTE DESDE EL TECLADO
   (define (leerrDatosPaciente)
       (crearPaciente
          (leerTeclado "Identificacion: ")
	  (leerTeclado "Nombre: ")
	  (leerTeclado "Genero: ")
	  (leerTeclado "Peso: ")
          (leerTeclado "Estatura: ")
	  )	     
    )
;mostrar pacientes

  (define (mostrarPacientes listaPacientes)
  (do
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;; Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    ;; Cuerpo del bucle
     (display "Identificacion: ")
     (display (identificacion (car listaAuxiliar)))
     (newline)
     (display "Nombre: ")
     (display (nombre (car listaAuxiliar)))
     (newline)
     (display "Genero: ")
     (display (genero (car listaAuxiliar)))
     (newline)
     (display "Peso: ")
     (display (peso (car listaAuxiliar)))
     (newline)
     (newline)
    )
  )

  ;-------


(define (rev l) (rev2 l '()))
  (define (rev2 l r)
    (cond
      ((null? l) r)
      (else (rev2 (cdr l) (cons (car l) r))) ))
  
 ;MOSTRAR ALERTAS

 (define (buscarAlertas listaPacientes )
  (do
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((equal? car(rev listaAuxiliar) 'flaquisimo) (display  (car listaAuxiliar))(newline)
                                                   
                                                    )
      (else '())
           )
      )
  
     
    )
  
;buscar paciente por código
  
 (define (buscarPaciente listaPacientes cod)
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((= (identificacion (car listaAuxiliar)) cod) (display (car listaAuxiliar))
                                                     (set! cod -1))
      (else '())
           )
      )
   (if(= cod -1)
      '()
      (display "Codigo No encontrado !")
      )
     
    )

;--------------------------------------------
  

  
;ELIMINAR UN PACINETE

  (define (eliminarPaciente listaPacientes cod)
   (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
     
    
    (cond
      ((= (identificacion (car listaAuxiliar)) cod) (set! pacientes(remove (car listaAuxiliar) pacientes))
                                                  
                                                    (set! cod -1))
      (else 'pasiente-no-se-eliminó)
           )
      )
   (if(= cod -1)
      '()
      (display "Codigo No encontrado !")
      )
     
    )

  
 
;MODIFICAR DATOS
  
   (define (modificarDatos listaPacientes cod )
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ;( set! x ( add1 x ) )
      ((= (identificacion (car listaAuxiliar)) cod) 
                                                      (set! pacientes(list-set listaAuxiliar 3 'a))
                                                                                                            
                                                 
                                                    (set! cod -1))
      (else '())
           )
      )
   (if(= cod -1)
      '()
      (display "Codigo No encontrado !")
      )
     
    )

  
 ;------------------------------
  
 ;PROGRAMA DE LOS PACINETES
  
    (do
	
	(
         ;; Lista para almacenar pacientes
	    
          
	 (opcion (mostrarMenu) (mostrarMenu))
	 )
	;; condicion de salida
	((= opcion 0) (display "fin del programa"))
	;; cuerpo del bucle
	(cond
          ;; REGISTRAR PACIENTE
	  ((= opcion 1) 
	     (display "Introduccion de datos de un Paciente")
	     (newline)
             ;; Uso obligatorio de set!
	     (set! pacientes (append pacientes (list (leerrDatosPaciente))))
             
	     )
          ;; BUSCACR PACIENTE
	  ((= opcion 2)
	     (define opt  (leerTeclado "Digite el codigo: "))
             (buscarPaciente pacientes opt)
	     (newline)
	     
	  )
          ;; MOSTRAR ALERTAS
	  ((= opcion 3) 
	     
           (buscarAlertas pacientes)
          )
          ;; MODIFICAR DATOS
	  ((= opcion 4)
           (modificarDatos pacientes 2)
	        
          )
          ;; ELIMINAR PACIENTE
	  ((= opcion 5)
           (define id  (leerTeclado "Digite el codigo: "))
             ;( eliminar pacientes)
             ;(display "eliminado")
            (eliminarPaciente pacientes id)
            
	     (newline)
	       
          )
          ;; CONTROL DE ERRORES
	  (else (display "Opcion incorrecta")
		(newline)
	  )
	)
        ;; 
         
    )
        
    )
  
 
(principal)


  