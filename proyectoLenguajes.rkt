#lang racket
(require rnrs/mutable-pairs-6)

;(define (<nombre-funcion> <args>) <cuerpo>)
;ALBEIRO SILVA MUÑOZ
;ANGÉLICA PINTO REBOLLEDO

;CREAR PACEINTE
;Esta función crea un paciente recibiendo como parámetros los valores digitados por el
;el usuario, donde se crea una lista de listas, se crea una lista para cada campo para facilitar el acceso a los mismos.
(define (crearPaciente ID  nombreApellido genero peso estatura )
	(list   (list 'identificacion ID)
		(list 'nombre nombreApellido)
		(list 'genero genero)
		(list 'peso peso)
                (list 'estatura estatura)
                (list 'IMC (calcularIMC peso estatura))
                (list 'valoracion (calcularValoracion (calcularIMC peso estatura)))
	)
)

;CREAR LA LISTA DE LOS PACIENTES
;Esta función crea una lista vacía para luego agregar, eliminar, modificar pacientes, primero se había creado la lista
;dentro de el ciclo "do" pero no era posible actualizar cuando se eliminaba un paciente, por esta razón se la creó fuera de el programa principal
(define pacientes '())


;CALCULAR IMC;
;,default-information originate
;Esta función calcula el IMC del paciente recibiendo como parámetros el peso y la estatura
(define (calcularIMC peso altura) (/ peso (* altura altura)))


;CALCULAR VALORACIÓN
;Esta función calcula la valoración del paciente recibiendo como parámetro el IMC del paciente, y
;dependiendo del valor del IMC le asigna una descripción
(define(calcularValoracion IMC)	(cond
         
	                        ((< IMC 18.5) 
	                         "peso inferior al normal"
             
                	         )
         
	                        ((and (>= IMC 18.5)(<= IMC 24.9))
	                         "normal"
	     
	                         )
        
	                        ((and (>= IMC 25.0)(<= IMC 29.9))
	                         "peso superior al normal"
       
                                 )
        
	                        ((>= IMC 30.0)
                                 "Obesidad"
	       
                                 )
       
	                        (else (display "Ninguna Opción es correcta")
	                          (newline)
	                        )
                            )
  )


;FUNCIONES DE ACCESO A LOS ATRIBUTOS DE CADA PACIENTE

;Con estas funciones accedemos a los atributos del paciente utilizando la función assoc. Donde esta función localiza el primer elemento
;de la lista que coincida con el primer argumento de la función assoc y nos regresa el par, como nos rrgresa el par de elemtentos utilizamos
;la función cadr que e slo mismo (car(cdr(.....))) para sacar sólo el dato y no el identificador que lo acompaña
(define (identificacion paciente)
	(cadr (assoc 'identificacion paciente))
)
(define (nombre paciente)
	(cadr (assoc 'nombre paciente))
)
(define (genero paciente)
	(cadr (assoc 'genero paciente))
)
(define (peso paciente)
	(cadr (assoc 'peso paciente))
)
(define (estatura paciente)
	(cadr (assoc 'estatura paciente))
)
(define (IMC paciente)
	(cadr (assoc 'IMC paciente))
)
(define (valoracion paciente)
	(cadr (assoc 'valoracion paciente))
)


 ;FUNCIÓN PARA LEER DE TECLADO
  
  ;Esta función se utiliza para pedir los datos al usuario
  
  (define (leerTeclado mensaje)
    (display mensaje)
    (display " ----> ")
    (read)
    )

 ;INTRODUCIR LOS DATOS DE UN DONANTE DESDE EL TECLADO

  ;Esta función llama a la función crearPaciente y que también llama a la función leerTeclado para que cada dato leído sea ingresado
  ;en la lista de crearPaciente


(define (leerrDatosPaciente)
  (crearPaciente
   (leerTeclado "Identificacion: ")
   (leerTeclado "Nombre_Apellido: ")
   (leerTeclado "Género: ")
   (leerTeclado "Peso (Kg): ")
   (leerTeclado "Estatura(m): ")
  )
)


;-----------------------------------------------


  ;MOSTRAR PACIENTE
  ;Esta función muestra cada campo de la lista de un paciente, recibe un pacinete como parámetro y utilizamos las funciones accesoras creadas anteriormente
  ;para mostrar el dato, utilizamos la función car porque recibimos una lista de pacientes desde donde se encontró la coincidencia hacia adelante
  (define (mostrarPaciente paciente) (display "Identificacion: ")(display (identificacion (car paciente)))(newline)
                                                    (display "Nombre: ")(display (nombre (car paciente)))(newline)
                                                    (display "Género: ")(display (genero (car paciente)))(newline)
                                                    (display "Peso: ")(display (peso (car paciente)))(newline)
                                                    (display "Etatura: ")(display (estatura (car paciente)))(newline)
                                                    (display "IMC: ")(display (IMC (car paciente)))(newline)
                                                    (display "Valoración: ") (display (valoracion (car paciente)))(newline)(newline))
  

  
;BUSCAR PACIENTE POR CÓDIGO
  ;esta función muestra el paciente encontrado según el código digitado, donde coincide la identificación
 (define (buscarPaciente listaPacientes cod)
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((= (identificacion (car listaAuxiliar)) cod) (mostrarPaciente listaAuxiliar)(set! cod -1))
           (else '())
    )
   )
   (if(= cod -1)
      '()
      (display "¡Código no encontrado!")
      )
     
 )


;(set! pacientes (list-set pacientes (index-of pacientes (car listaAuxiliar))(list-set (list-ref pacientes (index-of pacientes (car listaAuxiliar))) 4  (pedirEstatura nuevaEstatura))))



   
 ;MOSTRAR ALERTAS POR OBESIDAD

    ;(do ((x x (cdr x))
     ;  (sum 0 (+ sum (car x))))
      ;((null? x) sum))
  
  ;Esta función muestra los paceintes con obesidad, donde se utiliza un ciclo do para realizar la búsqueda, con la linea
  ;(listaAuxiliar listaPacientes (cdr listaAuxiliar)) vamos a ir pasando por cada elemnto d ela lista general y preguntamos 
  ; si en el pacinete n el campo obesidad es igual a "Obesidad" ,llamamos a la función mostrarPaciente. Utilizamos la función car para sacr el primer
  ;elemento d el alista auxiliar, ya que esta lista nos ttrae todos los elementos desde donde se encontró la coincidencia hacia adelante.

 (define (buscarAlertasPorObesidad listaPacientes )
  (do
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((equal? (valoracion (car listaAuxiliar)) "Obesidad") (mostrarPaciente listaAuxiliar))
      (else "NO EXISTEN PACIENTES")
           )
   )
  
     
)

   ;MOSTRAR ALERTAS POR SUPERIOR AL NORMAL

 (define (buscarAlertasPorSuperiorNormal listaPacientes )
  (do
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((equal? (valoracion (car listaAuxiliar)) "peso superior al normal") (mostrarPaciente listaAuxiliar))
      (else '())
     )
   )
  
     
 )

   ;MOSTRAR ALERTAS POR INFERIOR AL NORMAL

 (define (buscarAlertasPorInferiorNormal listaPacientes )
  (do
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((equal? (valoracion (car listaAuxiliar)) "peso inferior al normal") (mostrarPaciente listaAuxiliar))
      (else "NO EXISTEN PACIENTES")
     )
   )
   
  
 )

  ;MODIFICAR DATOS

 
;MENÚ PARA MOSTRAR LAS OPCIONES QUE QUIERE MODIFICAR (PESO O ALTURA)
;---------------------------------
(define (subMenu)(define (opcionCambiar)
                    (display "1. modificar peso")
                    (newline)
                    (display "2. modificar estatura")
                    (newline)
                    (display "0. atrás")
                    (newline)
                    (display "-->")
                    (leer))
                    
   (do
	
	( 
	 (opcionCambio (opcionCambiar) (opcionCambiar))
	 )
	; condicion de salida
	((= opcionCambio 0) (display "."))
	; cuerpo del bucle
	(cond
          ; 
	  ((= opcionCambio 1) 
	     (define id  (leerTeclado "Digite la identificación: "))
             (newline)
             (define nuevoPeso  (leerTeclado "Digite el nuevo peso(Kg) "))
             (modificarPeso pacientes id nuevoPeso)
             (modificarIMC pacientes id)
             (modificarValoracion pacientes id)
             (newline)
             (display "**paciente modificado**")
             (newline)
             (newline)
             
             
	     )
          ;; 
	  ((= opcionCambio 2)
	     (define id  (leerTeclado "Digite la identificación: "))
             (newline)
             (define nuevaEstatura  (leerTeclado "Digite la nueva estatura(m): "))
             (modificarEstatura pacientes id nuevaEstatura)
             (modificarIMC pacientes id)
             (modificarValoracion pacientes id)
             (newline)
             (display "**paciente modificado**")
             (newline)
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


;-------------------------------------------------------------------------------------------------------------
  
  

 

  
;FUNCIONES PARA CREAR LOS NUEVOS CAMPOS CON LOS VALORES NUEVOS (CREAN LA LISTA ('PESO VALOR_PESO....))

  ;Estas funciones crean los campos actualizados que van a ser ingresados en el paciente, devuelven una lista porque cada campo del paciente es una lista 
;------------------------------------------------
(define (crearEstatura nuevaEstatura) (list 'estatura nuevaEstatura ) )
(define (crearPeso nuevoPeso) (list 'peso nuevoPeso ) )
(define (crearIMC IMC)(list 'IMC IMC))
(define(crearValoracion IMC)(list 'valoracion IMC))
  ;---------------------------


  
 ;INGRESAR LA NUEVA ESTATURA EN LA LISTA DE PACIENTES
  ;Esta función actualiza la estaura d eun paciente, primero se encuentra el paciente que va a ser actualizado
  ;luego se utilizan funciones anidadadas de la siguiente manera:
  ;La función más interna es la función (indx-of), que nos devuelve un índice pasándole como parámetrouna lista(en este caso la lista general)
  ;y un elemento (en este caso el paciente encontrado),
  ;Luego se utiliza la función list-ref que recibe como parámetrouna lista (en este caso pacientes) y un índice, el obtenido con la función index-of,
  ;y nos devuelve un elemento.
  ;Luego con la función list-set que recibe como parámetro una lista (en este caso un paciente obtenido por la función list-ref) una pocición (en este
  ; caso la número 4 que e sdonde está la estatura) y recibe el elemento a ingresar, el que s eva a actualizar (en este caso la función crear estatura
  ; mencionada anteriormente)
  ;hasta el momento hemos actualizado un paciente, pero ese paciente hay que actualizarlo en la lista general
  ;Utilizamos la función list-set nuevamente, esta vez le pasamos como parámetro la lista general(pacientes) un índice (que se calcula con la función
  ; index-of) y EL ELEMENTO A ACTUALIZAR QUE ES EL PACIENTE YA ACTUALIZADO
  ;Por último se hace un set a pacientes para que se actualice la lista general
  ;--------------------------------
 (define (modificarEstatura listaPacientes cod nuevaEstatura)
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond

       ;(set! pacientes(list-set pacientes indice(lis-set (list-ref pacientes indice) 7(read))))
      ;(list-set '(zero one two) 2 "two")
       ; (zero one "two")
      ;(index-of '(1 2 3 4) 3) 2


      
      ((= (identificacion (car listaAuxiliar)) cod)  (set! pacientes
                                                           (list-set pacientes
                                                                     (index-of pacientes
                                                                               (car listaAuxiliar))
                                                                     (list-set
                                                                          (car listaAuxiliar) 4  (crearEstatura nuevaEstatura))))(set! cod -1))
      (else '())
     )
   )
   (if(= cod -1)
      '()
      (display "¡identificación no encontrada!")
      )

   )

  ;-----------------------
  ;INFRESAR EL NUEVO PESO A LA LISTA DE PACIENTES
  (define (modificarPeso listaPacientes cod nuevoPeso)
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond

       ;(set! pacientes(list-set pacientes indice(lis-set (list-ref pacientes indice) 7(read))))
      ;(list-set '(zero one two) 2 "two")
       ; (zero one "two")
      ;(index-of '(1 2 3 4) 3) 2


      
      ((= (identificacion (car listaAuxiliar)) cod)  (set! pacientes
                                                           (list-set pacientes
                                                                     (index-of pacientes
                                                                               (car listaAuxiliar))
                                                                     (list-set
                                                                      (car listaAuxiliar) 3  (crearPeso nuevoPeso))))(set! cod -1))
      (else '())
     )
   )
   (if(= cod -1)
      '()
      (display "¡Identificación no encontrada!")
      )
       )

  ;--------------------------------
  ;SE MODIFICA EL CAMPO IMC CON EL NUEVO PESO O LA NUEVA ESTATURA

  (define (modificarIMC listaPacientes cod )
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      
      ((= (identificacion (car listaAuxiliar)) cod)  (set! pacientes
                                                           (list-set pacientes
                                                                     (index-of pacientes (car listaAuxiliar))
                                                                     (list-set
                                                                      (car listaAuxiliar) 5  (crearIMC (calcularIMC  (peso(car listaAuxiliar))(estatura(car listaAuxiliar)))))))(set! cod -1))
      (else '())
     )
   )
   (if(= cod -1)
      '()
      (display "¡Identificación no encontrada!")
      )
       )

 ;------------------------------

;SE MODIFICA EL CAMPO VALORACIÓN CON EL NUEVO PESO O LA NUEVA ESTATURA
  (define (modificarValoracion listaPacientes cod )
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond

       ;(set! pacientes(list-set pacientes indice(lis-set (list-ref pacientes indice) 7(read))))
      ;(list-set '(zero one two) 2 "two")
       ; (zero one "two")
      ;(index-of '(1 2 3 4) 3) 2


      
      ((= (identificacion (car listaAuxiliar)) cod)  (set! pacientes
                                                           (list-set pacientes
                                                                     (index-of pacientes
                                                                               (car listaAuxiliar))
                                                                     (list-set
                                                                      (car listaAuxiliar) 6  (crearValoracion (calcularValoracion (IMC(car listaAuxiliar)))))))(set! cod -1))
      (else '())
     )
   )
   (if(= cod -1)
      '()
      (display "¡Identificación no encontrada")
      )
       )


;ELIMINAR UN PACINETE
  ;Esta función elimina un paciente, hacemos uso del ciclo do, recorremos la lista y donde la identificación coincide utilizamos la función remove
  ; que recibe como parámetros un elemto a eliminar y una lista d ela cual va a ser eliminado, en este caso le pasamos el car de la lista auxiliar porque
  ;listaAuxiliar nos trae todos los elelentos desde donde se encontró la coincidencia hacia a delante, y también utilizamos la función set para que se actualice
  ;la lista general

  ;(do ((<variable 1 > <init 1 > <paso 1 >))) 

  (define (eliminarPaciente listaPacientes cod)
   (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
     
    
    (cond
      ((= (identificacion (car listaAuxiliar)) cod) (set! pacientes(remove (car listaAuxiliar) pacientes ))(display "PACIENTE ELIMINADO")(newline)
                                                  
                                                    (set! cod -1))
      (else '())
           )
      )
   (if(= cod -1)
      '()
      (display "¡Código no encontrado!")
      )
     
    )

  ;---------------------
 (define (validarDatos listaPacientes )
  (do
   
    (
     (listaAuxiliar listaPacientes (cdr listaAuxiliar))
     )
    ;Condicion de salida del bucle
    ((null? listaAuxiliar) (newline))
    
    (cond
      ((< (identificacion (car listaAuxiliar)) 0) (eliminarPaciente listaAuxiliar (identificacion (car listaAuxiliar)))(display "paciente no ingresado"))
      ((< (peso (car listaAuxiliar)) 0) (eliminarPaciente listaAuxiliar (identificacion (car listaAuxiliar)))(display "paciente no ingresado"))
      ((< (estatura (car listaAuxiliar)) 0) (eliminarPaciente listaAuxiliar (identificacion (car listaAuxiliar)))(display "paciente no ingresado"))
      ((> (estatura (car listaAuxiliar)) 2.8) (eliminarPaciente listaAuxiliar (identificacion (car listaAuxiliar)))(display "paciente no ingresado"))
      ((> (peso (car listaAuxiliar)) 635) (eliminarPaciente listaAuxiliar (identificacion (car listaAuxiliar)))(display "paciente no ingresado"))
      ;((and (> (length pacientes) 1) (= (identificacion (car (remove listaAuxiliar (last (listaAuxiliar))))) (identificacion (last pacientes)))) (eliminarPaciente listaAuxiliar (identificacion (last pacientes)))(display "paciente no ingresado"))
      
      (else '())
    )
   )
  
     
 )


;PROGRAMA PRINCIPAL
;El programa principal contiene la interacción del programa

(define (terminarPrograma)(display "programa terminado...."))

 ;MOSTRAR MENÚ
 ;Esta función lo que hace es mostrar el menú de opciones al usuario junto con la función "read" para pedir la opción
(define (menu-principal)
     (display "\n\t*** MENU PRINCIPAL ***\n
\t[1] Registrar datos.
\t[2] Buscar un paciente.
\t[3] Buscar alertas.
\t[4] Modificar datos.
\t[5] Eliminar un paciente.
\t[6] Salir.
---> ") 
  (leer)
)

(define (leer) (read))

(define (iniciarPrograma opcion)
  (if (not(= opcion 6))
      [begin
        (cond
          [(and (number? opcion) (= opcion 1)) [ begin (display "Introduccion de datos de un Paciente")(newline)
             ;; Uso obligatorio de set!
             (set! pacientes (append pacientes (list (leerrDatosPaciente))))
             (validarDatos pacientes)
             (newline)
             
              
             (iniciarPrograma (menu-principal)) ]]
          [(and (number? opcion) (= opcion 2)) [ begin (define id  (leerTeclado "Digite el codigo: "))
             (newline)
             (newline)
             (newline)
             (buscarPaciente pacientes id)
	     (newline)(iniciarPrograma (menu-principal)) ]]
          [(and (number? opcion) (= opcion 3)) [ begin (newline)
	     (display "***PACIENTES CON PESO INFERIOR AL NORMAL***")
             (newline)
             (buscarAlertasPorInferiorNormal pacientes )
             (newline)
             (display "***PACIENTES CON PESO SUPERIOR AL NORMAL***")
             (newline)
             (buscarAlertasPorSuperiorNormal pacientes )
             (newline)
             (display "***PACIENTES CON OBESIDAD***")
             (newline)
            ; (display (length (pacientes)))
             (buscarAlertasPorObesidad pacientes )(iniciarPrograma (menu-principal)) ]]
          [(and (number? opcion) (= opcion 4)) [ begin (subMenu)(iniciarPrograma (menu-principal)) ]]
          [(and (number? opcion) (= opcion 5)) [ begin (define id  (leerTeclado "Digite la identificación: "))
             (eliminarPaciente pacientes id)(iniciarPrograma (menu-principal)) ]]
          

          (else
           [ begin (display "\nOpcion invalida, ingrese de nuevo una opcion.\n\n") (iniciarPrograma (menu-principal)) ]
           )
          )
        ]
      (terminarPrograma)
  )
)

(iniciarPrograma (menu-principal))


