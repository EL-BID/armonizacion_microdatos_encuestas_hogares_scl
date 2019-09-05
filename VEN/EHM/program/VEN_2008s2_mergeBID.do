*Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Fecha: Octubre, 2013

clear
set more off
cd "\\Sdssrv03\surveys\survey\VEN\EHM\2008\s2\data_orig\"

*Identificacion bases de datos
*--------------------------------
use per082.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesop
cap drop id_*
save per082.dta, replace

use hog082.dta, clear
sort entidad control linea serie num_hog
cap rename peso pesoh
cap drop id_*
save hog082.dta, replace

use viv082.dta, clear
sort entidad control linea serie
cap drop id_*
cap rename peso pesov
save viv082.dta, replace

*Merge
*--------
use hog082.dta, clear
joinby entidad control linea serie using viv082.dta,  _merge(_merge)
tab _merge
drop _merge

joinby entidad control linea serie num_hog using per082.dta, _merge(_merge)
tab _merge
drop _merge


*Etiqueto variables
*----------------------
label var	control	"c�digo �nico por entidad federal, que resume la identificaci�n de un segmento / sector en la muestra"
label var	entidad	"c�digo seg�n la divisi�n pol�tico territorial (dpt) de la entidad federal"
label var	linea	"n�mero correlativo del 1 al n, que se asigna a las viviendas dentro del �rea"
label var	num_hog	"n�mero del hogar seg�n sea el principal (1) o no (2 y mas)"
label var	num_per	"n�mero de l�nea, posici�n en la que se coloca a las personas, miembros habituales del hogar a lo largo del cuestionario"
*label var	pesov	"peso de la vivienda"
label var	pesoh	"peso del hogar"
label var	pesop	"peso de la persona"
label var	ph12	"n�mero de cuartos para dormir"
label var	ph13a	"� tiene uso exclusivo de ba�os con ducha  o regadera? "
label var	ph13b	"n�mero de ba�os con ducha o regadera "
label var	ph14a	"� posee nevera ?"
label var	ph14b	"�posee lavadora?"
label var	ph14c	"� posee televisor ?"
label var	ph14d	"� posee cocina a gas � el�ctrica ?"
label var	ph14e	" �posee cocina de 'kerosene u otros ?"
label var	ph14f	"� posee secadora de ropa ?"
label var	ph14g	"� posee calentador de agua ?"
label var	ph14h	"� posee aire acondicionado?"
label var	ph14i	"�posee filtro de agua? (solo a partir de 2003)"
label var	ph14j	"�posee radio? (solo a partir de 2003)"
label var	ph14k	"�posee horno microondas? (solo a partir de 2003)"
label var	ph14l	"�posee tel�fono movil celular? (solo a partir de 2003)"
label var	ph14m	"�posee televisi�n por cable? (solo a partir de 2003)"
label var	ph14n	"�posee computadora? (solo a partir de 2003)"
label var	ph14o	"�posee acceso a internet? (solo a partir de 2003)"
label var	ph14p	"�ninguno? (solo a partir de 2003)"
label var	ph15	"numero de automoviles tiene ese hogar"
label var	ph16a	"tenencia"
label var	ph16b	"alquiler"
label var	ph17	"gastos del hogar dependen de..."
label var	pp18	"sexo "
label var	pp19	"parentesco "
label var	pp20	"edad de a�os cumplidos"
label var	pp21	"situaci�n conyugal"
label var	pp22a	"identificaci�n del familiar n�cleo"
label var	pp22b	"nexo del n�cleo"
label var	pp23                         	"tiempo de residencia en esta entidad"
label var	pp24	"alfabetismo (sabe leer y escribir)"
label var	pp25a	"nivel educativo"
label var	pp25b	"ultimo grado aprobado"
label var	pp25c	"ultimo semestre aprobado"
label var	pp26	"t�tulo en educaci�n  superior"
label var	pp27	"asistencia a un centro de educaci�n"
label var	pp28	"causa de no asistencia de un centro de ense�anza"
label var	pp29	"� que hizo la semana pasada?"
label var	pp30	"actividad casera  esta pregunta admite un m�ximo de tres respuestas de 1994 a 1998 ( recibi� o va recibir pago de dinero)                        2004    durante la semana pasada ...... realizo en su casa o fuera de ella alguna actividad por la cual recibio o va a recibir pago en dinero, tales como:"
label var	pp31	"� tiene alg�n trabajo o negocio ?"
label var	pp32	"motivos por la cual no 'trabaj� la semana pasada"
label var	pp33a	"la semana pasada realizo alguna otra actividad  por la  que percibi� ingresos"
label var	pp33b	"cuantas actividades realizadas"
label var	pp34	"horas trabajadas la semana pasada en su actividad principal"
label var	pp35	"horas que trabaja normalmente"
*label var	pp36	"ademas de su trabajo principal realiza... normalmente alguna otra actividad por la que percibe ingresos tales como, ventas, contratos "
*label var	pp37	"horas trabajadas en todos sus trabajos o negocios"
*label var	pp38	"ha hecho algo para trabajar horas adicionales"
label var	pp39	"�cuando fue la �ltima vez que hizo algo para conseguir trabajo o establecer un negocio solo o asociado?"
label var	pp40	"ha realizado alguna de estas diligencias ?"
label var	pp41	"ha hecho alguna de estas diligencias la semana pasada ? (a partir de 2004 s�lo se le hace esta pregunta a ayudantes familiares)"
label var	pp42	"en caso de resultar positiva la diligencia efectuada � estar�a disponible para empezar a trabajar?"
label var	pp43	"motivos por la cual no est� trabajando actualmente (a partir del 2004 se pregunta �por cual de estos motivos ...... no esta buscando trabajo actualmente?                                       "
*label var	pp44	"�por cu�l de estos motivos no ha hecho diligencias para trabajar horas adicionales?"
label var	pp45	"trabaj� con anterioridad siendo remunerado"
label var	pp46a	"meses sin trabajar (solo cesantes)"
label var	pp46b	"a�os sin trabajar  (solo cesantes)"
label var	pp47	"cua l fue la causa ppal. por la que no continu� en su  �ltimo trabajo"
label var	pp48	"grupos de ocupaci�n (ver anexo 3)"
label var	pp49	"ramas de actividad (ver anexo 4) "
label var	pp50a	"cuantas personas remuneradas trabajan en la empresa negocio o establecimiento"
label var	pp50b	"(pp46b para 1994-1998, y pp50b a partir de 2do-2004 se refiere al segundo trabajo)"
label var	pp51a	"� el negocio o empresa para el cual�. realiza su trabajo (principal/secundario) est� amparado bajo"
label var	pp51b	"� alguna figura jur�dica, tal como: s.a., c.a., s.r.l., fundaci�n, cooperativa, etc ?"
*label var	pp53	"�el negocio o empresa para el cual ...... realiza su trabajo (ppal/secund) lleva libros de contabilidad? "
*label var	pp53a	"el negocio o empresa  para el cual� realiza su trabajo (principal/secundario) lleva libros de contabilidad?"
*label var	pp53b	"el negocio o empresa  para el cual� realiza su trabajo (principal/secundario) lleva libros de contabilidad?"
label var	pp54	"categoria de ocupaci�n"
label var	pp55	"instituci�n en la cual trabaja ..... es:"
label var	pp56a	"tiene derecho de utilidades, aguinaldo o bonif"
label var	pp56b	"tiene derecho de vacaciones?"
label var	pp56c	"tiene derecho a prestaciones sociale?"
label var	pp56d	"Ningun beneficio?"
label var	pp57	"trabajo principal tiene un salario minimo "
label var	pp58	"categor�a de salario minimo �en su trabajo ppal. gana"
label var	pp59	"en el trabajo principal que realiza (cuanto gana  o le pagan aproximadamente al mes)"
label var	pp60	"monto que gan� el mes pasado en todos sus trabajos"
label var	pp61a 	"recibio ingresos el mes pasado por: pension de sobreviviente, o"
label var	pp61b	"recibio ingresos el mes pasado por: ayuda familiar"
label var	pp61c	"recibio ingresos el mes pasado por: subsidio familiar"
label var	pp61d	"recibio ingresos el mes pasado por: beca o ayuda escolar"
label var	pp61e	"recibio ingresos el mes pasado por: pension o jubilaci�n por seguro social"
label var	pp61f	"recibio ingresos el mes pasado por: jubilacion por trabajo"
label var	pp61g	"recibio ingresos el mes pasado por: renta de propiedades"
label var	pp61h	"recibio ingresos el mes pasado por: intereses o dividendos"
label var	pp61i	"recibio ingresos el mes pasado por: otros?"
label var	pp61j	"Ninguno?"
label var	pp61k	"monto"
/*
label var	pv1	"tipo de vivienda"
label var	pv10	"n�mero de ba�os con ducha o regadera"
label var	pv11a	"servicio el�ctrico p�blico"
label var	pv11b	"recolecci�n directa de basura"
label var	pv11c	"container  de basura"
label var	pv11d	"servicio telef�nico (telef�nico fijo, anexo 2003)"
label var	pv11e	"ninguno"
label var	pv2	"paredes"
label var	pv3	"techo"
label var	pv4	"piso"
label var	pv5	"total de cuartos, contando sala, comedor y otros "
label var	pv6	"n�mero de cuartos utilizados para dormir (por los residentes de esta vivienda anexo 2003)"
label var	pv7	"a esta vivienda llega el agua por:"
label var	pv8	"servicio de eliminaci�n de excretas"
label var	pv9	"n�mero de pocetas"
label var	serie	"valor correlativo del 1 al n que se asigna a las viviendas levantadas dentro del segmento/sector"
*/

saveold "\\Sdssrv03\surveys\survey\VEN\EHM\2008\s2\data_merge\VEN_2008s2.dta", replace

 


