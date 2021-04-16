***********************
***** MERGE 2011 ******
***********************
*Mayra Sáenz
*Mayo 2014

clear

global ruta = "${surveysFolder}"

local PAIS VEN
local ENCUESTA EHM
local ANO "2011"
local ronda s2 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


use "`base_in'\hog112.dta", clear
sort entidad control linea serie num_hog 
egen id = group(entidad control linea serie num_hog)
duplicates report id
g pesoh = peso
saveold "`base_in'\hog112_.dta", replace
  
use "`base_in'\per112.dta", clear
sort entidad control linea serie num_hog 
egen id = group(entidad control linea serie num_hog)
duplicates report id
g pesop = peso
saveold "`base_in'\per112_.dta", replace 
  
use "`base_in'\viv112.dta", clear
sort entidad control linea serie 
duplicates report entidad control linea serie 
g pesov = peso
drop peso
saveold "`base_in'\viv112_.dta", replace 


use "`base_in'\viv112_.dta"  
merge 1:m entidad control linea serie using "`base_in'\hog112_.dta"
drop _merge
merge 1:m id using "`base_in'\per112_.dta"
  
  
label var	control	"código único por entidad federal, que resume la identificación de un segmento / sector en la muestra"
label var	entidad	"código según la división político territorial (dpt) de la entidad federal"
label var	linea	"número correlativo del 1 al n, que se asigna a las viviendas dentro del área"
label var	num_hog	"número del hogar según sea el principal (1) o no (2 y mas)"
label var	num_per	"número de línea, posición en la que se coloca a las personas, miembros habituales del hogar a lo largo del cuestionario"
label var	pesov	"peso de la vivienda"
label var	pesoh	"peso del hogar"
label var	pesop	"peso de la persona"
label var	ph12	"número de cuartos para dormir"
label var	ph13a	"¿ tiene uso exclusivo de baños con ducha  o regadera? "
label var	ph13b	"número de baños con ducha o regadera "
label var	ph14a	"¿ posee nevera ?"
label var	ph14b	"¿posee lavadora?"
label var	ph14c	"¿ posee televisor ?"
label var	ph14d	"¿ posee cocina a gas ó eléctrica ?"
label var	ph14e	" ¿posee cocina de 'kerosene u otros ?"
label var	ph14f	"¿ posee secadora de ropa ?"
label var	ph14g	"¿ posee calentador de agua ?"
label var	ph14h	"¿ posee aire acondicionado?"
label var	ph14i	"¿posee filtro de agua? (solo a partir de 2003)"
label var	ph14j	"¿posee radio? (solo a partir de 2003)"
label var	ph14k	"¿posee horno microondas? (solo a partir de 2003)"
label var	ph14l	"¿posee teléfono movil celular? (solo a partir de 2003)"
label var	ph14m	"¿posee televisión por cable? (solo a partir de 2003)"
label var	ph14n	"¿posee computadora? (solo a partir de 2003)"
label var	ph14o	"¿posee acceso a internet? (solo a partir de 2003)"
label var	ph14p	"¿ninguno? (solo a partir de 2003)"
label var	ph15	"numero de automoviles tiene ese hogar"
label var	ph16a	"tenencia"
label var	ph16b	"alquiler"
label var	ph17	"gastos del hogar dependen de..."
label var	pp18	"sexo "
label var	pp19	"parentesco "
label var	pp20	"edad de años cumplidos"
label var	pp21	"situación conyugal"
label var	pp22a	"identificación del familiar núcleo"
label var	pp22b	"nexo del núcleo"
label var	pp23                         	"tiempo de residencia en esta entidad"
label var	pp24	"alfabetismo (sabe leer y escribir)"
label var	pp25a	"nivel educativo"
label var	pp25b	"ultimo grado aprobado"
label var	pp25c	"ultimo semestre aprobado"
label var	pp26	"título en educación  superior"
label var	pp27	"asistencia a un centro de educación"
label var	pp28	"causa de no asistencia de un centro de enseñanza"
label var	pp29	"¿ que hizo la semana pasada?"
label var	pp30	"actividad casera  esta pregunta admite un máximo de tres respuestas de 1994 a 1998 ( recibió o va recibir pago de dinero)                        2004    durante la semana pasada ...... realizo en su casa o fuera de ella alguna actividad por la cual recibio o va a recibir pago en dinero, tales como:"
label var	pp31	"¿ tiene algún trabajo o negocio ?"
label var	pp32	"motivos por la cual no 'trabajó la semana pasada"
label var	pp33a	"la semana pasada realizo alguna otra actividad  por la  que percibió ingresos"
label var	pp33b	"cuantas actividades realizadas"
label var	pp34	"horas trabajadas la semana pasada en su actividad principal"
label var	pp35	"horas que trabaja normalmente"
label var	pp36	"ademas de su trabajo principal realiza... normalmente alguna otra actividad por la que percibe ingresos tales como, ventas, contratos "
label var	pp37	"horas trabajadas en todos sus trabajos o negocios"
label var	pp38	"ha hecho algo para trabajar horas adicionales"
label var	pp39	"¿cuando fue la última vez que hizo algo para conseguir trabajo o establecer un negocio solo o asociado?"
label var	pp40	"ha realizado alguna de estas diligencias ?"
label var	pp41	"ha hecho alguna de estas diligencias la semana pasada ? (a partir de 2004 sólo se le hace esta pregunta a ayudantes familiares)"
label var	pp42	"en caso de resultar positiva la diligencia efectuada ¿ estaría disponible para empezar a trabajar?"
label var	pp43	"motivos por la cual no está trabajando actualmente (a partir del 2004 se pregunta ¿por cual de estos motivos ...... no esta buscando trabajo actualmente?                                       "
label var	pp44	"¿por cuál de estos motivos no ha hecho diligencias para trabajar horas adicionales?"
label var	pp45	"trabajó con anterioridad siendo remunerado"
label var	pp46a	"meses sin trabajar (solo cesantes)"
label var	pp46b	"años sin trabajar  (solo cesantes)"
label var	pp47	"cua l fue la causa ppal. por la que no continuó en su  último trabajo"
label var	pp48	"grupos de ocupación (ver anexo 3)"
label var	pp49	"ramas de actividad (ver anexo 4) "
label var	pp50a	"cuantas personas remuneradas trabajan en la empresa negocio o establecimiento"
label var	pp50b	"(pp46b para 1994-1998, y pp50b a partir de 2do-2004 se refiere al segundo trabajo)"
label var	pp51a	"¿ el negocio o empresa para el cual…. realiza su trabajo (principal/secundario) está amparado bajo"
label var	pp51b	"… alguna figura jurídica, tal como: s.a., c.a., s.r.l., fundación, cooperativa, etc ?"
*label var	pp53	"¿el negocio o empresa para el cual ...... realiza su trabajo (ppal/secund) lleva libros de contabilidad? "
label var	pp53a	"el negocio o empresa  para el cual… realiza su trabajo (principal/secundario) lleva libros de contabilidad?"
label var	pp53b	"el negocio o empresa  para el cual… realiza su trabajo (principal/secundario) lleva libros de contabilidad?"
label var	pp54	"categoria de ocupación"
label var	pp55	"institución en la cual trabaja ..... es:"
label var	pp56a	"tiene derecho de utilidades, aguinaldo o bonif"
label var	pp56b	"tiene derecho de vacaciones?"
label var	pp56c	"tiene derecho a prestaciones sociale?"
label var	pp56d	"Ningun beneficio?"
label var	pp57	"trabajo principal tiene un salario minimo "
label var	pp58	"categoría de salario minimo ¿en su trabajo ppal. gana"
label var	pp59	"en el trabajo principal que realiza (cuanto gana  o le pagan aproximadamente al mes)"
label var	pp60	"monto que ganó el mes pasado en todos sus trabajos"
label var	pp61a 	"recibio ingresos el mes pasado por: pension de sobreviviente, o"
label var	pp61b	"recibio ingresos el mes pasado por: ayuda familiar"
label var	pp61c	"recibio ingresos el mes pasado por: subsidio familiar"
label var	pp61d	"recibio ingresos el mes pasado por: beca o ayuda escolar"
label var	pp61e	"recibio ingresos el mes pasado por: pension o jubilación por seguro social"
label var	pp61f	"recibio ingresos el mes pasado por: jubilacion por trabajo"
label var	pp61g	"recibio ingresos el mes pasado por: renta de propiedades"
label var	pp61h	"recibio ingresos el mes pasado por: intereses o dividendos"
label var	pp61i	"recibio ingresos el mes pasado por: otros?"
label var	pp61j	"Ninguno?"
label var	pp61k	"monto"
label var	pv1	"tipo de vivienda"
label var	pv10	"número de baños con ducha o regadera"
label var	pv11a	"servicio eléctrico público"
label var	pv11b	"recolección directa de basura"
label var	pv11c	"container  de basura"
label var	pv11d	"servicio telefónico (telefónico fijo, anexo 2003)"
label var	pv11e	"ninguno"
label var	pv2	"paredes"
label var	pv3	"techo"
label var	pv4	"piso"
label var	pv5	"total de cuartos, contando sala, comedor y otros "
label var	pv6	"número de cuartos utilizados para dormir (por los residentes de esta vivienda anexo 2003)"
label var	pv7	"a esta vivienda llega el agua por:"
label var	pv8	"servicio de eliminación de excretas"
label var	pv9	"número de pocetas"
label var	serie	"valor correlativo del 1 al n que se asigna a las viviendas levantadas dentro del segmento/sector"

* Comprime y guarda base
compress
saveold "`base_out'", replace

log close





