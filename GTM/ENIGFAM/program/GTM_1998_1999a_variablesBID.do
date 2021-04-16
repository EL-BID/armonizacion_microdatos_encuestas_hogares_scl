
* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


global ruta = "${surveysFolder}"

local PAIS GTM
local ENCUESTA ENIGFAM
local ANO "1998_1999"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                       


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: m10_m11
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

*****************************************************************************************************************
******                                  GUATEMALA Marzo 1998 a Marzo 1999                                  ******
******                    ENIGFAM 1998 (ENCUESTA NACIONAL DE INGRESOS Y GASTOS FAMILIARES)                 ****** 
******                                      35.725 personas                                                ****** 
******                                       7.139 Hogares                                                 ******
*****************************************************************************************************************
       


gen factor_ch=factorex
label var factor_ch "Factor de expansion del Hogar"

gen factor_ci=factorex /* 'FACTOREX' ya viene creada en la base */
label var factor_ci "Factor de Expansion del Individuo"


*** HOUSING ***

gen aguared_ch=.
replace aguared_ch=1 if distagua==1
replace aguared_ch=0 if distagua==2

gen aguadist_ch=.
replace aguadist_ch=1 if servagua==1 | servagua==4 | servagua==6
replace aguadist_ch=2 if servagua==2 | servagua==3 | servagua==5
replace aguadist_ch=3 if servagua==7 | servagua==8 | servagua==9 | servagua==0

gen aguamala_ch=.
replace aguamala_ch=1 if servagua>=1 & servagua<=6
replace aguamala_ch=0 if servagua==7 | servagua==8 | servagua==9 | servagua==0

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if luz==1
replace luz_ch=0 if luz==2

gen luzmide_ch=.

gen combust_ch=.
replace combust_ch=1 if combcoci==1 | combcoci==2 | combcoci==3
replace combust_ch=0 if combcoci==4 | combcoci==5 

gen bano_ch=.
replace bano_ch=1 if servsani>=1 & servsani<=8
replace bano_ch=0 if servsani==9

gen banoex_ch=.
replace banoex=1 if servsani>=1 & servsani<=4
replace banoex=0 if servsani>=5 & servsani<=8

* Modificaciones Marcela Rubio - Octubre 2014

/*
gen des1_ch=.

gen des2_ch=.
replace des2_ch=1 if servsani==1 | servsani==2 | servsani==5 | servsani==6
replace des2_ch=2 if servsani==3 | servsani==4 | servsani==7 | servsani==8
replace des2_ch=0 if servsani==9
*/

gen des1_ch = .
replace des1_ch = 0 if servsani == 9
replace des1_ch = 1 if servsani == 1 | servsani == 2 | servsani == 3 | servsani==5 | servsani==6 | servsani==7
replace des1_ch = 2 if servsani == 4 | servsani==8

gen des2_ch = .
replace des2_ch = 0 if servsani == 9
replace des2_ch = 1 if servsani == 1 | servsani == 2 | servsani == 3 | servsani==5 | servsani==6 | servsani==7 | servsani == 4 | servsani==8


gen piso_ch=.
replace piso_ch=0 if piso==5
replace piso_ch=1 if piso==1 | piso==2 | piso==3 | piso==4
replace piso_ch=2 if piso==6

gen pared_ch=.
replace pared_ch=0 if paredes==7 | paredes==8
replace pared_ch=1 if paredes>=1 & paredes<=6
replace pared_ch=2 if paredes==9

gen techo_ch=.
replace techo_ch=0 if techo==5
replace techo_ch=1 if techo>=1 & techo<=4
replace techo_ch=2 if techo==6

gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (servagua >=1 & servagua <=7)
replace aguamejorada_ch = 0 if (servagua >=8 & servagua <=9) | servagua  == 0
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (servsan >=1 & servsan <=4)
replace banomejorado_ch = 0 if (servsan >=5 & servsan <=9)

gen dorm_ch=.
replace dorm_ch=nrodormi if nrodormi>=0 

gen cuartos_ch=.
replace cuartos_ch=nrocuart if nrocuart>=0

gen cocina_ch=.
replace cocina_ch=1 if cuarcoci==1 | cuarcoci==3
replace cocina_ch=0 if cuarcoci==2 | (cuarcoci>=4 & cuarcoci<=8)

gen telef_ch=.
replace telef_ch=1 if telefono==1
replace telef_ch=0 if telefono==2

gen refrig_ch=.
replace refrig_ch=1 if refri==1
replace refrig_ch=0 if refri==2

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if auto==1
replace auto_ch=0 if auto==2

gen compu_ch=.
replace compu_ch=1 if computa==1
replace compu_ch=0 if computa==2

gen internet_ch=.

gen cel_ch=.
replace cel_ch=1 if celular==1
replace cel_ch=0 if celular==2

gen vivi1_ch=.
replace vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2
replace vivi1_ch=3 if tipoviv>=3 & tipoviv<=6

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if tenencia==3
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=2 if tenencia==2
replace viviprop_ch=3 if tenencia==4 | tenencia==5

gen vivitit_ch=.

gen vivialq_ch=divarri if divarri>0

gen vivialqimp_ch=alqimp if alqimp>0


* ZONA
gen byte zona_c=1 if area==1 /* Urbana */
replace zona_c=0 if area==2 /* Rural */
label variable zona_c "ZONA GEOGRAFICA"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

* COUNTRY - YEAR
gen str3 pais_c="GTM"
label variable pais_c "Nonmbre del Pais"
gen anio_c=1998
label variable anio_c "Año de la Encuesta"

* Periodo de Referencia: Marzo 1998 a Marzo 1999
gen byte mes_c=.
label variable mes_c "Mes de la Encuesta"

* SEXO
gen sexo_ci=sexo
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"  
label value sexo_ci sexo_ci

* PARENTESCO

gen relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if (parentco==4 | parentco==5 | parentco==6 | parentco==7)
replace relacion_ci=5 if (parentco==9 | parentco==10)
replace relacion_ci=6 if parentco==8
label var relacion_ci "Parentesco o relacion con el Jefe del Hogar"
label define relacion_ci 1 "Jefe(a)" 2 "Esposo(a) o compañero(a)" 3 "Hijo(a)" 4 "Otro pariente" 5 "Otro NO pariente" 6 "Empleado(a) domestico(a)" 
label value relacion_ci relacion_ci

* EDAD
gen edad_ci=edad
label var edad_ci "Edad del Individuo"

* Identificador del Hogar
egen idh_ch=group(region estrato area boleta) 
label var idh_ch "Identificador Unico del Hogar"

* Identificador Individual
gen idp_ci=nroper
label var idp_ci "Identificador Individual dentro del Hogar"

sort idh_ch idp_ci

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "Numero de Conyuges"

egen nhijos_ch=sum(relacion_ci==3), by (idh_ch)
label variable nhijos_ch "Numero de Hijos"
egen notropari_ch=sum(relacion_ci>3 & relacion_ci<5), by (idh_ch)
label variable notropari_ch "Numero de Otros Parientes "
egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label variable notronopari_ch "Numero de Otros NO Parientes "
egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label variable nempdom_ch "Numero de Empleados Domesticos"

* HOUSEHOLD TYPE (unipersonal, nuclear, ampliado, compuesto, corresidentes)    
* note: These are all defined in terms of relationship to household head

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

* Checking 'clasehog' (que todas las personas pertenezcan a un hogar con 'clasehog' definido)
assert clasehog==int(clasehog) 
assert clasehog>=1 & clasehog<=5 
sum clasehog
scalar claseT=r(N)
assert claseT==35725


* HOUSEHOLD COMPOSITION VARIABLES 
/* note: These are unrelated to who is the head
   note: That childh denotes the number of children of the head, while numkids counts the number of all kids in the household */

sort idh_ch

* NUMBER OF PERSONS IN THE HOUSEHOLD (not including domestic employees or other relatives)
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5 ), by (idh_ch)
label variable nmiembros_ch "Numero de miembros en el Hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<6)), by (idh_ch)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<1)),  by (idh_ch)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

*** RAZA: Se puede generar agrupando la variable 'p03a04'
*gen ethnic=1 if p03a04== /* INDIGENA */
*replace ethnic=2 if p03a04== /* NO INDIGENA */

*** ESTADO CIVIL PARA PERSONAS DE 10 AÑOS O MAS DE EDAD
gen civil_ci=.  
replace civil_ci=1 if estcivil==1 | estcivil==6 /* SOLTERO */
replace civil_ci=2 if estcivil==2 | estcivil==3 /* UNION FORMAL O INFORMAL */
replace civil_ci=3 if estcivil==4 /* SEPARADO O DIVORCIADO */
replace civil_ci=4 if estcivil==5 /* VIUDO */
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*** REPORTED HEAD OF HOUSEHOLD
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de Hogar Declarado"

*** We want to know if there is only one head in each hh and if there is a hh with no head:
egen hh=sum(jefe), by (idh_ch)
capture assert hh==1


*******************************************************************************************
* VARIABLES DEL MERCADO LABORAL

* EN 2002 PERSONAS DE 7 AÑOS Y MAS DE EDAD *
* EN 1998 ESTE BLOQUE DE PREGUNTAS ESTABA DIRIGIDO A LAS PERSONAS DE 7 AÑOS Y MAS DE EDAD *
* EN 2000 ESTE BLOQUE DE PREGUNTAS ESTABA DIRIGIDO A LAS PERSONAS DE 5 AÑOS Y MAS DE EDAD *
*******************************************************************************************
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

*1 = GUA 1998
gen salmm_ci= 565.05

label var salmm_ci "Salario minimo legal"


*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

****************
*cotizando_ci***
****************
gen cotizando_ci= 1 if (dyv01>0 & dyv01<=5109)  
replace cotizando_ci=0 if  dyv01==0
recode cotizando_ci .=0 if condi==11 | condi==12 | condi==13 | condi==21
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	

label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=1 if (dyv01>0 & dyv01<=5109)  
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

gen formal_ci=(cotizando_ci==1)

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


****************
****condocup_ci*
****************
* variable alternativa generada con variables originales. MGD 05/22/2014
* la disponiilidad para trabajar se pregunta unicamente a quienes no buscaron trabajo.
* Coincide con la generada. 
gen condocup_ci=.
replace condocup_ci=1 if teniatra==1 | notrab==1 
replace condocup_ci=2 if (teniatra==2 | notrab==2) & (buscomes==1 | buscosem==1) 
recode condocup_ci .=3 if edad>=7
replace condocup_ci=4 if edad<7 
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
gen cesante_ci=1 if condi==21 & condocup_ci==2
replace cesante_ci=0 if condi!=21 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

*Nota MGD 07/17/2014: no se genra la avriable ya que la opcion para tamano de empresa solamente categoriza de 1 a 10 personas
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************

gen pension_ci=1 if ycm20>0 & ycm20!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=ycm20 if ycm20>0 & ycm20!=.

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**tecnica_ci*
*************

gen tecnica_ci=.
replace tecnica_ci=1 if tienecap==1
label var tecnica_ci "=1 formacion terciaria tecnica"
/*
* EMP
 /*1998: Trabajo la semana pasada? Aunque no trabajo, tenia empleo?
 En 2000 ademas de los que responden "SI" a la pregunta "Trabajo la semana pasada?" se incluyen a quienes no han trabajado pero tienen trabajo. En 2002 solo aparecen como empleados quienes responden "Trabajar" a la pregunta "Que hizo la semana pasada?" Ademas se agregan como empleados a quienes no reportan "trabajar" pero tienen alguna categoria ocupacional en la actividad principal porque aunque hayan estudiado, sean jubilados, hayan realizado quehaceres del hogar, han trabajado sin pago, o vendieron algun producto o realizaron algun servicio o ayudo al trabajo de otras personas o tenia algun negocio, comercio, fabrica. */
gen byte emp_ci=.
replace emp_ci=1 if (teniatra==1 | notrab==1)
replace emp_ci=0 if teniatra==2 & notrab==2
label var emp_ci "Empleado en la semana de referencia"

* desemp1
* Individuos que no han trabajado la semana pasada y declaran haber estado buscando trabajo
gen byte desemp1_ci=.
replace desemp1_ci=1 if (buscosem==1 & emp_ci~=1)
replace desemp1_ci=0 if emp_ci==1 | (emp==0 & buscosem==2)
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

* desemp2
gen byte desemp2_ci=desemp1_ci
replace desemp2_ci=1 if (emp_ci~=1 & buscosem~=1 & pqnobus==2) /* 'pqnobus'=2: Espera para comenzar un trabajo ya arreglado */
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista" /* No incluye temporada agricola*/

* desemp3: No se puede crear en 2000, en 1998 esta la variable 'vdp25' 
* 'vdp25': Aunque no busco trabajo, habia buscado trabajo o trato de establecer su propia empresa o negocio en las 4 semanas anteriores?
* En 1998 esta la variable 'buscomes': Habia buscado trabajo o trato de establecer su propia empresa o negocio en las 4 semanas anteriores?
gen byte desemp3_ci=desemp2_ci
replace desemp3_ci=1 if (emp_ci~=1 & buscosem~=1 & buscomes==1)
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

* PEA: Poblacion Economicamente Activa
gen byte pea1_ci=1 if (emp_ci==1 | desemp1_ci==1)
label var pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"
gen byte pea2_ci=1 if (emp_ci==1 | desemp2_ci==1)
label var pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"
gen byte pea3_ci=1 if (emp_ci==1 | desemp3_ci==1)
label var pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"
replace pea1_ci=0 if pea1_ci~=1
replace pea2_ci=0 if pea2_ci~=1
replace pea3_ci=0 if pea3_ci~=1
*/

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"
* Trabajadores Desalentados
/* 1998: pqnobus=3 "Cree que no hay posibilidad de conseguir trabajo"
No se puede crear en 2002, no estan las opciones "Se canso de buscar" o "Piensa que no hay trabajo" en la pregunta "Porque no busco trabajo?" que si aparecen en el 2000. Se podria considerar la opcion "Hay trabajo pero no se lo dan a el", pero esta alternativa no se considera en el 2000. */ 
gen byte desalent_ci=.
replace desalent_ci=1 if pqnobus==3
replace desalent_ci=0 if pea_ci==1 | (pqnobus!=3 & pqnobus>0)
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*** Horas trabajadas en la Actividad Principal
* La pregunta es: En el trabajo que tuvo la semana pasada o la ultima vez, cuantas horas trabajo cada uno de los siguientes dias: lunes, martes, miercoles, jueves, viernes, sabado, domingo? Luego se suma cada una de las horas por dia para llegar al total por semana.  
gen horaspri_ci=.
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen horastot_ci=hrstotal if hrstotal>0 
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

* Trabajadores Subempleados
/* 2000: 'P10B10A': Desea y esta dispuesto a trabajar mas horas por semana? */
/* 2002: 'p05d01': Ademas de las horas que trabaja actualmente en todos sus trabajos, desea trabajar mas horas a la semana? */
gen subemp_ci=0
replace subemp_ci=1 if horastot_ci<=30 & dispues==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

* Trabajadores a Medio Tiempo
gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30  & dispues==2 & emp_ci==1
replace tiempoparc_ci=0 if ((horastot_ci>30  & dispues==2) | dispues==1) & emp_ci==1
label var tiempoparc_ci "Trabajadores a medio tiempo"

* Contratos
gen contrato_ci=.
label var contrato_ci "Personas empleadas que han firmado un contrato de trabajo"

* Beneficios (Seguridad Social)
/* 1998: Not available
2002: La pregunta exacta, dirigida a todos los ocupados de 7 años o mas, de la encuesta es: Esta ud vinculado al Instituto Guatemalteco de Seguridad Social (IGSS) como afiliado (1), contribuyente (2), beneficiario (3), pensionado (4), ninguna de las anteriores(5)? Solo 50 contribuyentes pagan, los demas no pagan (ver pregunta 'p05b26'). */
gen segsoc_ci=.
label variable segsoc_ci "Personas que cuentan con seguro social"

* Numero de ocupaciones
gen nempleos_ci=1 if emp_ci==1 /* 1 empleo */
replace nempleos_ci=2 if (emp_ci==1 & masdeuno==1 ) /* 2 o mas empleos */  
replace nempleos_ci=0 if emp_ci~=1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

* Tamaño de la firma
gen byte tamfirma_ci=.
replace tamfirma_ci=0 if tamest>0 & tamest<=5 /* 5 o menos */
replace tamfirma_ci=1 if tamest>5 /* mas de 5 */
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 5 trabajadores" 0 "5 o menos trabajadores"
label values tamfirma_ci tamfirma_ci
/*Mayra Sáenz-No incorporo modificaci'on LMK la segunda l'inea corresponde a afiliaci'on al seguro
gen firmapeq_ci= .
replace firmapeq_ci= 1 if tamest<=5
recode firmapeq_ci .=0 if (dyv01>0 & dyv01<=5109)  
*/
*** Sector Publico
gen spublico_ci=1 if categpri==2
replace spublico_ci=0 if categpri>0 & categpri!=2
label var spublico_ci "Personas que trabajan en el sector publico"

************************************************************************************************************
*** VARIABLES DE DEMANDA LABORAL
************************************************************************************************************

* 'ocupp' ocupacion a dos digitos
* 'ocupr' ocupacion a un digito

* OCCUPACION
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupp>=2113 & ocupp<=3480) & emp==1
replace ocupa_ci=2 if (ocupp>=1110 & ocupp<=1319) & emp==1
replace ocupa_ci=3 if (ocupp>=4111 & ocupp<=4223) & emp==1
replace ocupa_ci=4 if (ocupp>=5220 & ocupp<=5260) & emp==1 
replace ocupa_ci=5 if ((ocupp>=5112 & ocupp<=5169) |  (ocupp>=9111 & ocupp<=9162)) & emp==1
replace ocupa_ci=6 if ((ocupp>=6111 & ocupp<=6210) |  (ocupp>=9211 & ocupp<=9213))  & emp==1
replace ocupa_ci=7 if ((ocupp>=7111 & ocupp<=8340) |  (ocupp>=9311 & ocupp<=9333)) & emp==1
replace ocupa_ci=8 if ocupp==110 & emp==1
replace ocupa_ci=9 if ocupp==99999 & emp==1
label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "Profesionales y tecnicos" 2 "Directores y funcionarios superiores" 3 "Personal administrativo y nivel intermedio" 4 "Comerciantes y vendedores" 5 "Trabajadores en servicios" 6 "Trabajadores agricolas y afines" 7 "Obreros no agricolas, conductores de maquinas y vehiculos de transporte y similares" 8 "Fuerzas armadas" 9 "Otras ocupaciones no clasificadas en las anteriores"
label values ocupa_ci ocupa_ci

* 'ramap' rama a dos digitos
* 'ramar' rama a un digito

* RAMA
/*
gen rama_ci=.
replace rama_ci=1 if (ramar==1) & emp==1
replace rama_ci=2 if (ramar==2) & emp==1
replace rama_ci=3 if (ramar==3) & emp==1
replace rama_ci=4 if (ramar==4) & emp==1
replace rama_ci=5 if (ramar==5) & emp==1
replace rama_ci=6 if (ramar==6) & emp==1
replace rama_ci=7 if (ramar==7) & emp==1
replace rama_ci=8 if (ramar==8) & emp==1
replace rama_ci=9 if (ramar>=9) & emp==1
label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, sivicultura y pesca" 2 "Explotacion de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construccion" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci
*/
gen rama_ci=.
* MLO = supongo que se mantiene CIIU rev 3
replace rama_ci=1 if (ramaprin>=111 &  ramaprin<=500) & emp_ci==1
replace rama_ci=2 if (ramaprin>=1010 & ramaprin<=1429) & emp_ci==1
replace rama_ci=3 if (ramaprin>=1511 & ramaprin<=3720) & emp_ci==1
replace rama_ci=4 if (ramaprin>=4010 & ramaprin<=4100) & emp_ci==1
replace rama_ci=5 if (ramaprin>=4510 & ramaprin<=4540) & emp_ci==1
replace rama_ci=6 if (ramaprin>=5010 & ramaprin<=5520) & emp_ci==1
replace rama_ci=7 if (ramaprin>=6010 & ramaprin<=6420) & emp_ci==1
replace rama_ci=8 if (ramaprin>=6511 & ramaprin<=7020) & emp_ci==1
replace rama_ci=9 if (ramaprin>=7111 & ramaprin<=9900) & emp_ci==1

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, sivicultura y pesca" 2 "Explotacion de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construccion" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci




/* *** ocupault 
gen ocupault=.
replace ocupault=1 if (p05c04d1==2 | p05c04d1==3) 
replace ocupault=2 if p05c04d1==1 
replace ocupault=3 if p05c04d1==4 
replace ocupault=4 if p05c04d1==5 & p05c04==52 
replace ocupault=5 if p05c04d1==5 & p05c04==51 
replace ocupault=6 if p05c04d1==6 
replace ocupault=7 if (p05c04d1==7 | p05c04d1==8) 
replace ocupault=8 if p05c04d1==0 
replace ocupault=9 if p05c04d1==9 
label var ocupault "Ocupation in last job"
label define ocupault 1 "Profesionales y tecnicos" 2 "Directores y funcionarios superiores" 3 "Personal administrativo y nivel intermedio" 4 "Comerciantes y vendedores" 5 "Trabajadores en servicios" 6 "Trabajadores agricolas y afines" 7 "Obreros no agricolas, conductores de maquinas y vehiculos de transporte y similares" 8 "Fuerzas armadas" 9 "Otras ocupaciones no clasificadas en las anteriores"
label values ocupault ocupault

**** ramault 
gen ramault=.
replace ramault=1 if (p05c05d1==1) 
replace ramault=2 if (p05c05d1==2) 
replace ramault=3 if (p05c05d1==3) 
replace ramault=4 if (p05c05d1==4) 
replace ramault=5 if (p05c05d1==5) 
replace ramault=6 if (p05c05d1==6)  
replace ramault=7 if (p05c05d1==7) 
replace ramault=8 if (p05c05d1==8) 
replace ramault=9 if (p05c05d1==9) 
replace ramault=10 if (p05c05d1==10) 
label var ramault "Rama in last job"
label define ramault 1 "Agricultura, caza, sivicultura y pesca" 2 "Explotacion de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construccion" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales" 10 "Otros"
label values ramault ramault
tab p05c05d1 if ramault==.

**** ocupau12 
gen ocupau12=.
replace ocupau12=1 if (p06a08d1==2 | p06a08d1==3) 
replace ocupau12=2 if p06a08d1==1 
replace ocupau12=3 if p06a08d1==4 
replace ocupau12=4 if p06a08d1==5 & p06a08==52 
replace ocupau12=5 if p06a08d1==5 & p06a08==51 
replace ocupau12=6 if p06a08d1==6 
replace ocupau12=7 if (p06a08d1==7 | p06a08d1==8) 
replace ocupau12=8 if p06a08d1==0 
replace ocupau12=9 if p06a08d1==9 
label var ocupau12 "Ocupation in last job for unemployed"
label define ocupau12 1 "Profesionales y tecnicos" 2 "Directores y funcionarios superiores" 3 "Personal administrativo y nivel intermedio" 4 "Comerciantes y vendedores" 5 "Trabajadores en servicios" 6 "Trabajadores agricolas y afines" 7 "Obreros no agricolas, conductores de maquinas y vehiculos de transporte y similares" 8 "Fuerzas armadas" 9 "Otras ocupaciones no clasificadas en las anteriores"
label values ocupau12 ocupau12
tab p06a08d1 emp_ci if ocupau12==.

**** ramau12 
gen ramau12=.
replace ramau12=1 if (p06a09d1==1) 
replace ramau12=2 if (p06a09d1==2) 
replace ramau12=3 if (p06a09d1==3) 
replace ramau12=4 if (p06a09d1==4) 
replace ramau12=5 if (p06a09d1==5) 
replace ramau12=6 if (p06a09d1==6)  
replace ramau12=7 if (p06a09d1==7) 
replace ramau12=8 if (p06a09d1==8) 
replace ramau12=9 if (p06a09d1==9) 
replace ramau12=10 if (p06a09d1==10) 
label var ramau12 "Rama in last job for unemployed"
label define ramau12 1 "Agricultura, caza, sivicultura y pesca" 2 "Explotacion de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construccion" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales" 10 "Otros"
label values ramau12 ramau12
tab p06a09d1 if ramau12==.*/

*** DURACION DEL DESEMPLEO
gen durades_ci=sembus/4.3 if sembus>0 /* Solo a los DESOCUPADOS: Cuantas semanas hace que esta buscando trabajo? */
label var durades_ci "Duracion del Desempleo (en meses)"

/*** CESANTE: Solo a los DESOCUPADOS: Busco trabajo por primera vez o trabajo antes?
gen cesante_ci=1 if p06a06==2  Trabajo antes 
replace cesante_ci=0 if p06a06==1  Primera vez */

*** Antiguedad (AÑOS) (Cuantos años continuos lleva trabajando en esta empresa o negocio?) (NOT AVAILABLE IN 1998)
/* En 2000: en total, cuantos años lleva trabajando? (No menciona que sean continuos) */
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en años)"

******************************************************************************************************
* VARIABLES EDUCATIVAS

* PARA PERSONAS DE 7 AÑOS O MAS DE EDAD 

* EN 2000 EXISTE UN BLOQUE DE EDUCACION PREESCOLAR PARA PERSONAS DE MENOS DE 7 AÑOS, QUE TAMPOCO APARECE EN 1998
* Los que tienen edad<=6 tienen 'missings' en estas variables
******************************************************************************************************

* AÑOS DE EDUCACION

* NIVGRADO

/* 
  NIVELGRADO=p03a09a
1 Ninguno
2 Preparatoria
3 Educacion de Adultos
4 Primaria
5 Secundaria
6 Superior
7 Post-grado
*/

gen byte aedu_ci=.

*** Ninguno 
replace aedu_ci=0 if nivgrado==50 

*** Preprimaria
replace aedu_ci=0 if nivgrado==10

*** Primaria
replace aedu_ci=1 if nivgrado==21 
replace aedu_ci=2 if nivgrado==22
replace aedu_ci=3 if nivgrado==23 
replace aedu_ci=4 if nivgrado==24 
replace aedu_ci=5 if nivgrado==25 
replace aedu_ci=6 if nivgrado==26 
replace aedu_ci=6 if nivgrado==27
*** Educacion Media
replace aedu_ci=7 if nivgrado==31
replace aedu_ci=8 if nivgrado==32
replace aedu_ci=9 if nivgrado==33
replace aedu_ci=10 if nivgrado==34
replace aedu_ci=11 if nivgrado==35
replace aedu_ci=12 if nivgrado==36
replace aedu_ci=12 if nivgrado==37
*** Educacion Superior
replace aedu_ci=13 if nivgrado==41 
replace aedu_ci=14 if nivgrado==42
replace aedu_ci=15 if nivgrado==43 
replace aedu_ci=16 if nivgrado==44 
replace aedu_ci=17 if nivgrado==45 
replace aedu_ci=18 if nivgrado==46 
replace aedu_ci=19 if nivgrado==47 
replace aedu_ci=20 if nivgrado==48 
replace aedu_ci=21 if nivgrado==49 

*replace aedu_ci=. if nivgrado==27 | nivgrado==37 | nivgrado==49 | nivgrado==0
label variable aedu_ci "Años de Educacion"


/* Groups based on the years of schooling:
6 cumulative categories
NOSCHOOL: no school 
ALSPRI: at least some primary
ALCPRI: at least complete primary
ALSSEC: at least some secondary
ALCLSEC: at least 9th grade completed
ALCSEC: at least complete secondary
ALSHIGH: at least some higher

gen byte noschool=.
replace noschool=1 if aedu_ci==0
replace noschool=0 if aedu_ci!=. & aedu_ci>0
label variable noschool "Completed zero years of schooling"

* SOME PRIMARY
gen byte alspri=0
replace alspri=1 if aedu_ci>0
replace alspri=. if aedu_ci==.
label variable alspri "Completed at least SOME primary"

* COMPL. PRIMARY
gen byte alcpri=0
replace alcpri=1 if aedu_ci>=6
replace alcpri=. if aedu_ci==.
label variable alcpri "Completed at least primary"

* AT LEAST SOME SECONDARY EDUCATION
gen byte alssec=0
replace alssec=1 if aedu_ci>=7
replace alssec=. if aedu_ci==.
label variable alssec "Completed at least SOME secondary school"

* COMPL. 1st CYCLE SECONDARY
gen byte alc1csec=0
replace alc1csec=1 if aedu_ci>=9
replace alc1csec=. if aedu_ci==.
label variable alc1csec "Completed at least first cycle secondary"

* COMPL. 2nd CYCLE SECONDARY (completed all of secondary school)
gen byte alcsec=0
replace alcsec=1 if aedu_ci>=12
replace alcsec=. if aedu_ci==.
label variable alcsec "Completed at least secondary school"

* SOME HIGHER EDUCATION
gen byte alshigh=0
replace alshigh=1 if ( p03a09a==6 | p03a09a==7 )
replace alshigh=. if aedu_ci==.
label variable alshigh "Completed at least SOME higher schooling"

For the technical school
* Technical Secondary School
gen techschl=.
NOT AVAILABLE  

* SHIGHACA: This variable will be zero for higher technical education and one for higher academic education
NOT AVAILABLE
gen shighaca=.
*/
** Categorias educativas excluyentes
gen eduno_ci=.
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.
replace eduno_ci=. if nivgrado==10
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupre_ci=.
replace edupre=1 if nivgrado==10
replace edupre=0 if nivgrado!=10 & aedu_ci<.
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu_ci>=6 & aedu_ci!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu_ci!=.) 
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=12 & aedu_ci!=.)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=0 if (aedu_ci>=0 & aedu_ci<12) | (aedu_ci>12 & aedu_ci!=.) 
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=0 if (aedu_ci>=0 & aedu_ci<=12) | (aedu_ci>=17 & aedu_ci!=.)
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc_ci=1 if aedu_ci>=17 & aedu_ci!=.
replace eduuc_ci=0 if aedu_ci>=0 & aedu_ci<17
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"


gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (nivgrado==31 | nivgrado==32)
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (nivgrado==33)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (nivgrado==34 | nivgrado==35)
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

* ASISTE
gen asiste_ci=0 if asist==3
replace asiste_ci=1 if asist==1 | asist==2
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

* POR QUE NO ASISTE (En 1998 la pregunta sobre causa de inasistencia es solo para personas de 7 a 14 años de edad)
gen pqnoasis_ci=.
replace pqnoasis_ci=rznoasis if rznoasis>0
label var pqnoasis_ci "Razon principal por la cual ha abandonado o ha dejado de asistir a clases este año"
label define pqnoasis_ci 1 "Queda lejos la escuela" 2 "No hay escuela" 3 "Falta de dinero" 4 "No le gusta, no quiere ir" 5 "Quehaceres de la casa" 6 "Padres no quieren" 7 "Por trabajo" 8 "Por otra causa" 9 "Discapacidad" 10 "Vacaciones"
label values pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==3
replace pqnoasis1_ci= 2 if  pqnoasis_ci==7
replace pqnoasis1_ci= 3 if  pqnoasis_ci==6 | pqnoasis_ci==9
replace pqnoasis1_ci= 4 if  pqnoasis_ci==4
replace pqnoasis1_ci= 5 if  pqnoasis_ci==5
replace pqnoasis1_ci= 6 if  pqnoasis_ci==10
replace pqnoasis1_ci= 8 if  pqnoasis_ci==1 | pqnoasis_ci==2
replace pqnoasis1_ci= 9 if  pqnoasis_ci==8

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

* PUBLIC OR PRIVATE SCHOOL
/* 1998: needs to be used with attend variable since it only holds for those who are currently attending school (Las opciones son: No Asiste, Asiste a PUBLICO, Asiste a PRIVADO)

   2000: Solo para los que se han inscripto para el año escolar 2000:
         El plantel educativo donde se inscribio es: 
	 1 Ministerio de Educacion
	 2 PRONADE
	 3 Nuevas Escuelas Unitarias (NEUS)
         4 Otra Institucion del Gobbierno Central
	 5 Municipal
	 6 Cooperativa
	 7 Comunitario
	 8 Privado
	 9 ONG's
	 10 Otro

Se han considerado como PUBLICAS las opciones 1, 2, 3, 4 y 5 para 2000 */
gen edupub_ci=1 if asist==1
replace edupub=0 if asist==2
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos" /* NOT AVAILABLE IN 2002 */

* ILLITERACY
/*gen alfabet=.
replace alfabet=1 if p03a05==1 'p03a05': Sabe leer y escribir?
replace alfabet=0 if p03a05==2 

 En 2002 la variable 'alfabet' se pregunta igual que en 1998. 
Pero entre 1998 y 2000 han una modificacion en la pregunta. 
     En 1998 la encuesta pregunta: Sabe leer Y escribir? SI - NO
     En 2000 la encuesta pregunta: Sabe leer y escribir en español? Lee Y escribe - Solo lee - No lee ni escribe
 Este cambio podria explicar parte de la reduccion en la tasa de alfabetismo en Guatemala:
 1998: 79.3 %
 2000: 70.9 % */


**********************************
*** INCOME VARIABLES (MONTHLY) ***
**********************************

* Create a dummy indicating this person's income should NOT be included in y*_ch
gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

sort idh_ch

*****************************************************************
*** INGRESOS LABORALES (PARA PESONAS DE 7 AÑOS O MAS DE EDAD) ***
*****************************************************************

***************************
*** OCUPACION PRINCIPAL ***
***************************

/* categpri CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL
1 - 2 ASALARIADOS
3 - 5 INDEPENDIENTES
6 TRAB SIN PAGO */
gen categopri_ci=.
replace categopri_ci=1 if categpri==5 & emp_ci==1
replace categopri_ci=2 if (categpri==3 | categpri==4) & emp_ci==1
replace categopri_ci=3 if (categpri==1 | categpri==2) & emp_ci==1
replace categopri_ci=4 if categpri==6 & emp_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"
label define categosec_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

* INGRESO LABORAL MONETARIO ACTIVIDAD PRINCIPAL
* EMPLEADO, OBRERO, SERVICIO DOMESTICO

/* ycm01p sueldo, salario o jornal 
ycm02p horas extra
ycm03p aguinaldo
ycm04p bonificaciones
ycm05p viaticos
ycm06p propinas, comisiones
ycm07p otros ingresos */

egen salario=rsum(ycm01p ycm02p ycm03p ycm04p ycm05p ycm06p ycm07p) if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD SECUNDARIA
* EMPLEADO, OBRERO, SERVICIO DOMESTICO

/* ycm01s sueldo, salario o jornal 
ycm02s horas extra
ycm03s aguinaldo
ycm04s bonificaciones
ycm05s viaticos
ycm06s propinas, comisiones
ycm07s otros ingresos */

egen salario2=rsum(ycm01s ycm02s ycm03s ycm04s ycm05s ycm06s ycm07s) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL
* EMPLEADO, OBRERO, SERVICIO DOMESTICO

/* ycnm01p alimentos y bebidas
ycnm02p ropa o calzado
ycnm03p uso de la vivienda, agua, electricidad 
ycnm04p gastos de automovil o transporte
ycnm05p servicios de enseñanza
ycnm06p otros bienes y servicios */

egen salarionm=rsum(ycnm01p ycnm02p ycnm03p ycnm04p ycnm05p ycnm06p) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA
* EMPLEADO, OBRERO, SERVICIO DOMESTICO

/* ycnm01s alimentos y bebidas
ycnm02s ropa o calzado
ycnm03s uso de la vivienda, agua, electricidad 
ycnm04s gastos de automovil o transporte
ycnm05s servicios de enseñanza
ycnm06s otros bienes y servicios */

egen salarionm2=rsum(ycnm01s ycnm02s ycnm03s ycnm04s ycnm05s ycnm06s) if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD PRINCIPAL
* CUENTA PROPIA (SIN TRABAJADORES)

/* ycm08p ingreso monetario por cuenta propia
ycm09p otros ingresos monetarios */

egen indep=rsum(ycm08p ycm09p) if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD SECUNDARIA
* CUENTA PROPIA (SIN TRABAJADORES)

/* ycm08s ingreso monetario por cuenta propia
ycm09s otros ingresos monetarios */

egen indep2=rsum(ycm08s ycm09s) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL
* CUENTA PROPIA (SIN TRABAJADORES)

/* ycnm07p alimentos y bebidas 
ycnm08p ropa y calzado
ycnm09p otros bienes y servicios */

egen indepnm=rsum(ycnm07p ycnm08p ycnm09p) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA
* CUENTA PROPIA (SIN TRABAJADORES)

/* ycnm07s alimentos y bebidas 
ycnm08s ropa y calzado
ycnm09s otros bienes y servicios */

egen indepnm2=rsum(ycnm07s ycnm08s ycnm09s) if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD PRINCIPAL
* EMPRESARIO O PATRONO DE SOCIEDADES JURIDICAS (CON TRABAJADORES)

/* ycm10p ingreso monetario como empresario o patron
ycm11p otros ingresos */

egen patron=rsum(ycm10p ycm11p) if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD SECUNDARIA
* EMPRESARIO O PATRONO DE SOCIEDADES JURIDICAS (CON TRABAJADORES)

/* ycm10s ingreso monetario como empresario o patron 
ycm11s otros ingresos */

egen patron2=rsum(ycm10s ycm11s) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL
* EMPRESARIO O PATRONO DE SOCIEDADES JURIDICAS (CON TRABAJADORES)

/* ycnm10p alimentos  
ycnm11p ropa y calzado 
ycnm12p otros bienes y servicios */

egen patronnm=rsum(ycnm10p ycnm11p ycnm12p) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA
* EMPRESARIO O PATRONO DE SOCIEDADES JURIDICAS (CON TRABAJADORES)

/* ycnm10s alimentos 
ycnm11s ropa y calzado
ycnm12s otros bienes y servicios */

egen patronnm2=rsum(ycnm10s ycnm11s ycnm12s) if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD PRINCIPAL
* EMPRESARIO O PATRONO DE SOCIEDADES NO JURIDICAS (CON TRABAJADORES)

/* ycm12p ingreso monetario como empresario o patron */

gen patronnj=ycm12p if edad>=7

* INGRESO LABORAL MONETARIO ACTIVIDAD SECUNDARIA
* EMPRESARIO O PATRONO DE SOCIEDADES NO JURIDICAS (CON TRABAJADORES)

/* ycm12s ingreso monetario como empresario o patron */

gen patronnj2=ycm12s if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL
* EMPRESARIO O PATRONO DE SOCIEDADES NO JURIDICAS (CON TRABAJADORES)

/* ycnm13p alimentos  
ycnm14p ropa y calzado 
ycnm15p otros bienes y servicios */

egen patronnjnm=rsum(ycnm13p ycnm14p ycnm15p) if edad>=7

* INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA
* EMPRESARIO O PATRONO DE SOCIEDADES NO JURIDICAS (CON TRABAJADORES)

/* ycnm13s alimentos 
ycnm14s ropa y calzado
ycnm15s otros bienes y servicios */

egen patronnjnm2=rsum(ycnm13s ycnm14s ycnm15s) if edad>=7

* VALORACION DE LA PRODUCCION PARA AUTOCONSUMO
* ACTIVIDAD PRINCIPAL

/* ycnm16p productos de cultivo
ycnm17p productos pecuarios y de granja
ycnm18p productos de caza, silvicultura y pesca */

egen autoco=rsum(ycnm16p ycnm17p ycnm18p) if edad>=7

* VALORACION DE LA PRODUCCION PARA AUTOCONSUMO
* ACTIVIDAD SECUNDARIA

/* ycnm16s productos de cultivo
ycnm17s productos pecuarios y de granja
ycnm18s productos de caza, silvicultura y pesca */

egen autoco2=rsum(ycnm16s ycnm17s ycnm18s) if edad>=7

***************************************************
** INGRESO LABORAL MONETARIO ACTIVIDAD PRINCIPAL **
***************************************************

egen ylmpri_ci=rsum(salario indep patron patronnj)
replace ylmpri_ci=. if salario==. & indep==. & patron==. & patronnj==.
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

*************************************
** INGRESO LABORAL MONETARIO TOTAL **
*************************************

egen ylm_ci=rsum(salario salario2 indep indep2 patron patron2 patronnj patronnj2)
replace ylm_ci=. if salario==. & salario2==. & indep==. & indep2==. & patron==. & patron2==. & patronnj==. & patronnj2==.
label var ylm_ci "Ingreso Laboral Monetario Total"

****************************************
** INGRESO LABORAL NO MONETARIO TOTAL **
****************************************

egen ylnm_ci=rsum(salarionm salarionm2 indepnm indepnm2 patronnm patronnm2 patronnjnm patronnjnm2)
replace ylnm_ci=. if salarionm==. & salarionm2==. & indepnm==. & indepnm2==. & patronnm==. & patronnm2==. & patronnjnm==. & patronnjnm2==. 
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

egen ylnm1_ci=rsum(salarionm salarionm2 indepnm indepnm2 patronnm patronnm2 patronnjnm patronnjnm2 autoco autoco2)
replace ylnm1_ci=. if salarionm==. & salarionm2==. & indepnm==. & indepnm2==. & patronnm==. & patronnm2==. & patronnjnm==. & patronnjnm2==. & autoco==. & autoco2==.
label var ylnm1_ci "Ingreso Laboral NO Monetario Total (incluye Autoconsumo)"

* INGRESOS NO LABORALES
* INGRESOS MONETARIOS COMO RENTISTAS

/* ycm13 alquiler de habitaciones a huespedes
ycm14 alquiler de casas, terrenos y fincas
ycm15 intereses percibidos
ycm16 rentas por propiedad de marcas, patentes
ycm17 dividendos por acciones
ycm18 otras rentas en dinero */

egen renta=rsum(ycm13 ycm14 ycm15 ycm16 ycm17 ycm18) if edad>=7

* TRANSFERENCIAS MONETARIAS RECIBIDAS

/* ycm19 pensiones alimenticias
ycm20 pensiones y rentas vitalicias 
ycm21 ayuda en dinero de otro hogar
ycm22 ayuda en dinero proveniente del exterior
ycm23 jubilaciones o pensiones por enfermedad
ycm24 seguro por desempleo
ycm25 otras transferencias */

egen transfer=rsum(ycm19 ycm20 ycm21 ycm22 ycm23 ycm24 ycm25) if edad>=7

* INGRESOS MONETARIOS ANUALES (quetzales por mes)

/* edi01 venta por acciones, bono y titulos
edi02 venta por bienes inmuebles
edi03 venta de vehiculos del hogar
ync01 herencias, legados y donaciones
ync02 premios de loteria y juegos de azar
edi04 venta de mobiliario, equipo y maquinaria 
edi05 prestamos bancarios
edi06 prestamos de particulares
edi07 otra clase de prestamos
edi08 venta de moneda extranjera
edi09 venta de oro, joyas o bienes del hogar
ync03 indemnizaciones, seguros de vida, despidos
ycm26 otros ingresos anuales */

egen otrosy=rsum(edi01 edi02 edi03 ync01 ync02 edi04 edi05 edi06 edi07 edi08 edi09 ync03 ycm26) if edad>=7

****************************************
** INGRESO NO LABORAL MONETARIO TOTAL **
****************************************

egen ynlm_ci=rsum(renta transfer otrosy)
replace ynlm_ci=. if renta==. & transfer==. & otrosy==.
label var ynlm_ci "Ingreso NO Laboral Monetario"

egen autocons_ci=rsum(autoco autoco2)
replace autocons_ci=. if autoco==. & autoco2==.
label var autocons_ci "Autoconsumo Individual"

egen remesas_ci=rsum(ycm21 ycm22) if edad>=7
label var remesas_ci "Remesas Individuales"

** INGRESO NO LABORAL NO MONETARIO **
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

*** FLAGS
gen byte nrylmpri_ci=0
*replace nrylmpri_ci=1 if
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

*** Dummy para el Hogar
capture drop nrylmpri_ch
sort idh_ch
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh_ch) 
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembros_ci==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ylnm1_ch=sum(ylnm1_ci) if miembros_ci==1, by(idh_ch)
label var ylnm1_ch "Ingreso Laboral No Monetario del Hogar (incluye Autoconsumo)"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"


egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"

gen rentaimp=yci02 if yci02>0
egen rentaimp_ch=sum(rentaimp) if miembros_ci==1, by(idh_ch)

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen ynlnm2_ch=rsum(ynlnm_ch rentaimp_ch) if miembros_ci==1 & ycm13==0 & ycm14==0
label var ynlnm2_ch "Ingreso No Laboral No Monetario del Hogar (con Renta Imputada)"


replace ynlnm_ch=. if ynlnm_ci==.
replace ylm_ch =. if miembros_ci==0
replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ylnm1_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0

gen ylnmpri_ci=.
gen ylmsec_ci=.
gen ylnmsec_ci=. 
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen region_BID_c=.
gen region_c=.
gen categoinac_ci=.

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña
gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.

*** INGRESO HORARIO DE TODOS LOS TRABAJOS ***
/* this is not accurate in the sense that if you have more than one job
you will have wage averaged over several jobs */
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*** INGRESO HORARIO DE LA OCUPACION PRINCIPAL ***
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"


drop hh salario salario2 indep indep2 patron patron2 patronnj patronnj2 salarionm salarionm2 indepnm indepnm2 patronnm patronnm2 patronnjnm patronnjnm2 autoco autoco2 renta transfer otrosy

/*
**Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
qui sum factor_ch	idh_ch	idp_c	zona_c	pais_c	anio_c	mes_c	relacion_ci	factor_ci	sexo_ci	edad_ci	civil_ci	///
jefe_ci	nconyuges_ch	nhijos_ch	notropari_ch	notronopari_ch	nempdom_ch	clasehog_ch	nmiembros_ch	///
miembros_ci	nmayor21_ch	nmenor21_ch	nmayor65_ch	nmenor6_ch	nmenor1_ch	ocupa_ci	rama_ci	horaspri_ci	///
horastot_ci	ylmpri_ci	ylnmpri_ci	ylmsec_ci	ylnmsec_ci	ylmotros_ci	ylnmotros_ci	nrylmpri_ci	tcylmpri_ci ///
ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci	nrylmpri_ch	tcylmpri_ch	ylm_ch	ylnm_ch	ylmnr_ch	ynlm_ch	ynlnm_ch	///
ylmhopri_ci	ylmho_ci	rentaimp_ch	autocons_ci	autocons_ch	remesas_ci	remesas_ch	durades_ci	antiguedad_ci ///
emp_ci	desemp_ci	pea_ci	 desalent_ci	subemp_ci	tiempoparc_ci ///
categopri_ci	categosec_ci	nempleos_ci	spublico_ci	aedu_ci	eduno_ci ///
edupi_ci	edupc_ci	edusi_ci	edusc_ci	eduui_ci	eduuc_ci	edus1i_ci	edus1c_ci	edus2i_ci ///
edus2c_ci	edupre_ci	eduac_ci	asiste_ci	pqnoasis	repite_ci	repiteult_ci	edupub_ci	///
aguared_ch	aguadist_ch	aguamala_ch	aguamide_ch	luz_ch	luzmide_ch	combust_ch	bano_ch	banoex_ch	///
des1_ch	des2_ch	piso_ch	pared_ch	techo_ch	resid_ch	dorm_ch	cuartos_ch	cocina_ch	telef_ch ///
refrig_ch	freez_ch	auto_ch	compu_ch	internet_ch	cel_ch	vivi1_ch	vivi2_ch	viviprop_ch	///
vivitit_ch	vivialq_ch	vivialqimp_ch region_BID_c region_c raza_ci        lp25_ci	       lp4_ci	 ///
lp_ci	       lpe_ci	       cotizando_ci	         afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	///
tamemp_ci categoinac_ci formal_ci
*/

* MGR: modifico esta sección ya que variables no están en el mismo orden que el resto de bases armonizadas

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_idioma_ci  id_ind_ci id_afro_ci raza_ci  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close
