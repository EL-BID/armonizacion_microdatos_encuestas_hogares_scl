
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
 


*global ruta = "${surveysFolder}"

local PAIS CRI
local ENCUESTA ENAHO
local ANO "2012"
local ronda m7 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Costa Rica
Encuesta: ENAHO
Round: m7
Autores: Mayra Sáenz
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 1 de octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=1 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2014	
gen region_c=region

label define region_c  ///
	       1 "Central" ///
           2 "Chorotega" ///
           3 "Pacífico central" ///
           4 "Brunca" ///
           5 "Huetar Atlántica" ///
           6 "Huetar Norte"
label value region_c region_c
label var region_c "División política, region de planificacion"


*====================================================================================================================================*
*                                                    VARIABLES DEL HOGAR                                                             *
*====================================================================================================================================*
* En total son 8 variables.

************************************************************
*** 1.- FACTOR_CH: factor de expansión del hogar         ***
************************************************************

gen factor_ch=factor
label var factor_ch "Factor de expansion del hogar"

************************************************************
*** 2._ IDH_CH: Identificador del hogar                  ***
************************************************************

destring segmento cuestionario hogar, replace
sort segmento cuestionario hogar
egen idh_ch = group(segmento cuestionario hogar)
duplicates report idh_ch

label var idh_ch "ID del hogar"


*/
************************************************************
*** 3.- IDP_CI: Identificador de personas                ***
************************************************************
/*
***NOTA***
 ____________________________________________________________________________________________________________________________
| En la hoja de nombres y definiciones de las variables del sociómetro se nombra como idp_c al identificador de personas y   |
| no idp_ci.                                                                                                                 |
|____________________________________________________________________________________________________________________________|
*/

gen idp_ci= r1
label var idp_ci "ID de la persona en el hogar"

************************************************************
***4.- ZONA_C: Zona Urbana vs Rural                      ***
************************************************************
gen zona_c=0 if zona==2
replace zona_c=1 if zona==1
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************************************************************
****5._ PAIS_C: Nombre del país.                         ***
************************************************************
gen str3 pais_c="CRI"
label variable pais_c "Pais"


************************************************************
***6._ANIO_C  :Año de la Encuesta.                       ***
************************************************************
gen anio_c=2012
label variable anio_c "Anio de la encuesta"

************************************************************
***7._MES_C  :Mes de la Encuesta.***
************************************************************
gen mes_c=7
label variable mes_c "Mes de la encuesta"
label define mes_c 7 "Julio" 
label value mes_c mes_c

******************************************************************
***8._RELACION_CI:Relacion o parentesco con el jefe de Hogar.***
******************************************************************
gen relacion_ci=1 		if a3==1
replace relacion_ci=2 	if a3==2
replace relacion_ci=3 	if a3==3
replace relacion_ci=4 	if a3>=4 & a3<=10 
replace relacion_ci=5 	if a3==11 | a3==13
replace relacion_ci=6 	if a3==12
label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci



*====================================================================================================================================*
*                                                          VARIABLES DEMOGRAFICAS                                                    *
*====================================================================================================================================*
* En el área de demografía son en total 18 variables.


******************************************************************
***1._FACTOR_CI  :Factor de Expansion a nivel individual. ***
******************************************************************
gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

******************************************************************
***2._SEXO_CI  :Sexo.                                          ***
******************************************************************
gen sexo_ci=a4
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

******************************************************************
***3._EDAD_CI  :Edad.***
******************************************************************
gen edad_ci=a5
replace edad_ci=. if a5==98 | a5==99 
label variable edad_ci "Edad del individuo"

******************************************************************
***4._CIVIL_CI  :Estado Civil.***
******************************************************************
gen civil_ci=.
replace civil_ci=1 if a25==6
replace civil_ci=2 if a25==1 | a25==2
replace civil_ci=3 if a25==3 | a25==4
replace civil_ci=4 if a25==5
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

******************************************************************
***5._JEFE_CI  :Jefe de Hogar declarado. ***
******************************************************************
gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"

******************************************************************
***6._NCONYUGES_CH  :Numero de Conyuges o Esposos/as.***
******************************************************************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

******************************************************************
***7._NHIJOS_CH  :Numero de Hijos/as.                         ***
******************************************************************
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************************************************************
***8._NOTROPARI_CH  :Numero de Otros Parientes.***
******************************************************************
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

******************************************************************
***9._NOTRONOPARI_CH  :Numero de Otros No Parientes.***
******************************************************************
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"

******************************************************************
***10._NEMPDOM_CH  :Numero de Empleados Domesticos.***
******************************************************************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

******************************************************************
***11._CLASEHOG_CH  :Tipo de Hogar.***
******************************************************************
gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************************************************************
***12._NMIEMBROS_CH  :Numero Total de miembros del Hogar.***
******************************************************************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

******************************************************************************************************************
***13._MIEMBROS_CI  :Indica las personas que son miembros del hogar. Sirve para construir el ingreso del hogar.***
******************************************************************************************************************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

******************************************************************************************************************
***14._NMAYOR21_CH  :Numero de miembros del Hogar con 21 años o mas de edad.***
******************************************************************************************************************
by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

**********************************************************************************
***15._NMENOR21_CH  :Numero de miembros del Hogar con menos de 21 años de edad.***
**********************************************************************************
by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*******************************************************************************
***16._NMAYOR65_CH  :Numero de miembros del Hogar con 65 años o mas de edad.***
*******************************************************************************
by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

********************************************************************************
***17._NMENOR6_CH  :Numero de miembros del Hogar con menos de 6 años de edad.***
********************************************************************************
by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

********************************************************************************
***18._NMENOR1_CH  :Numero de miembros del Hogar con menos de 1 año de edad.***
********************************************************************************
by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 
	***************
	***afroind_ci***
	***************
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		
	
	*************
	***dis_ci***
	**************
gen nodis = inlist(0,a7a, a7b)
replace nodis=. if a7a>=.
recode nodis 1=0 if inrange(a7a,1,7)
recode nodis 1=0 if inrange(a7b,1,7)
gen dis_ci = nodis
recode dis_ci 0=1 1=0
drop nodis
	
	*************
	***dis_ch***
	**************		
egen dis_ch  = sum(dis_ci), by(idh_ch) 
replace dis_ch=1 if dis_ch>=1 & dis_ch!=. 

*====================================================================================================================================*
*                                                          VARIABLES DEL MERCADO LABORAL                                              *
*====================================================================================================================================*

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci = lp

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = cba

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=1 if (a10>=1 & a10<=3) | a13==1
recode cotizando_ci .=0 
/** solo a asalariados cotizantes en 1er y 2da ocupacion y voluntarios
gen cotizando_ci=1 if e10a==1 | f9a==1 | a13==1
recode cotizando_ci .=0 */
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
replace tipopen_ci=a12
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=a11
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if b1==1 | (b2 >= 1 & b2 <= 8) | b3==1 | (b5 >= 1 & b5 <= 5) & estabili != 30 /*ocupado frecuente*/
replace condocup_ci=1 if b1==1 | (b2 >= 1 & b2 <= 8) | b3==1 | (b5 >= 1 & b5 <= 5) & estabili == 30 /*ocupado estacional*/
replace condocup_ci=2 if (b6== 8 | b6== 9) & ((b7a== 1 | b7b== 2 | b7c== 3 | b7d== 4 | b7e== 5 | b7f==6 | b7g == 7 | b7h == 08 | b7i == 09 | b7j == 10 | b7k== 11) | (b8==1 | b8==2 | b8==3)) & g6==1 /*desempleado cesante*/
replace condocup_ci=2 if (b6== 8 | b6==9 ) & ((b7a== 1 | b7b== 2 | b7c== 3 | b7d== 4 | b7e== 5 | b7f== 6 | b7g== 7 | b7h ==08 | b7i ==09 | b7j== 10 | b7k== 11) | (b8==1 | b8==2 | b8==3)) & g6==2 /*desempleado primera vez*/
recode condocup_ci .=3 if  edad_ci>=12
replace condocup_ci=4 if  edad_ci<12

/*esta version no permitia determinar la situacion de las personas entre 12 y 15
replace condocup_ci=1 if condact==11 | condact==12
replace condocup_ci=2 if condact==21 | condact==22
replace condocup_ci=3 if condact>=31 & condact<=34 & edad_ci>=10
replace condocup_ci=4 if  edad_ci<12*/
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
* MGD 12/4/2015: condicionado a que este desocupado
gen cesante_ci=1 if g6==1 & condocup_ci==2
recode cesante_ci .=0 if condocup_ci==2
* No todos los desempleados respondieron si han trabajado antes
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************

gen pension_ci=1 if h9j == 1 
replace pension_ci =0 if h9j == 2 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************
recode h9j1 (99999999=.)
/*
1 mes
           2 bimestre
           3 trimestre
           4 cuatrimestre
           6 semestre
           8 año
           9 ignorado
*/

gen ypen_ci=h9j1 if  h9j2 ==1
replace ypen_ci=h9j1/2 if  h9j2 ==2
replace ypen_ci=h9j1/3 if  h9j2 ==3
replace ypen_ci=h9j1/4 if  h9j2 ==4
replace ypen_ci=h9j1/6 if  h9j2 ==6
replace ypen_ci=h9j1/12 if  h9j2 ==8
replace ypen_ci=. if  h9j2 ==9

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen byte pensionsub_ci= 1 if h9e==1
replace pensionsub_ci =0 if h9e==2
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen aux_ps= h9e1 if h9e2==1
replace aux_ps = . if h9e1==9999999
destring aux_ps, replace
gen  ypensub_ci=aux_ps
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"



*************
**salmm_ci***
*************

* 2012 Anuario Estadístico. Ministerio de Trabajo y Seguridad Social de Costa Rica
gen salmm_ci= 	211129 if zona_c==0
replace salmm_ci = 265239.6 if zona_c==1
label var salmm_ci "Salario minimo legal"

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

***********************************************************************************************************************************************
***8._DESALENT_CI : Trabajadores desalentados: personas que creen que por alguna razon no conseguiran trabajo.***
***********************************************************************************************************************************************
gen desalent_ci=(emp_ci==0 & (b8==5 | b8==6 |b8==7 |b8==8))
replace desalent_ci=. if condactre==.
label var desalent_ci "Trabajadores desalentados"

**************************************************************************
***9._HORASPRI_CI : Horas totales trabajadas en la actividad principal.***
**************************************************************************
gen horaspri_ci=c2a1 
replace horaspri_ci=. if c2a1==999
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

***************************************************************************
***10._HORASSEC_CI : Horas totales trabajadas en la actividad secundaria.***
***************************************************************************
gen horassec_ci=c2b1 
replace horassec_ci=. if c2b1==999
replace horassec_ci=. if emp_ci==0
label var horassec_ci "Horas trabajadas semanalmente en el trabajo secundario"

***************************************************************************
***11._HORASTOT_CI : Horas totales trabajadas en todas las actividades.***
***************************************************************************
egen horastot_ci=rsum(horaspri_ci horassec_ci), missing 
replace horastot_ci=. if horaspri_ci==. & horassec_ci==.
replace horastot_ci=. if emp_ci==0
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*********************************************************************************
***12._SUBEMP_CI : Trabajadores sub-empleados: personas dispuestas a trabajar mas,
*** pero trabajan 30 horas a la semana o menos.***
*********************************************************************************
* Modificacion MGD 06/24/2014: mal generada la variable, se corrije con las variables desea trabajar mas horas y disponibilidad.
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci<=30 & c3==1 & (c4a==1 | c4a==2)) & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*********************************************************************************
***13._TIEMPOPARC_CI : Trabajadores a medio tiempo: personas que trabajan menos de
*** 30 horas a la semana y no quieren trabajar mas.
*********************************************************************************
gen tiempoparc_ci=(horastot_ci<=30 & c1==2)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 

*********************************************************************************
***14._CATEGOPRI_CI : Categoria ocupacional en la actividad principal.        ***
*********************************************************************************
gen categopri_ci=.
replace categopri_ci=1 if posiempagrupri==21
replace categopri_ci=2 if posiempagrupri==22 
replace categopri_ci=3 if posiempagrupri==11 | posiempagrupri==12 
replace categopri_ci=4 if posiempagrupri==13 
replace categopri_ci=. if emp_ci==0
label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

*********************************************************************************
***15._CATEGOSEC_CI : Categoria ocupacional en la actividad secundaria.
*********************************************************************************
gen categosec_ci=.
replace categosec_ci=1 if posiempagrusec==21
replace categosec_ci=2 if posiempagrusec==22 
replace categosec_ci=3 if posiempagrusec==11 | posiempagrusec==12 
replace categosec_ci=4 if posiempagrusec==13 
replace categosec_ci=. if emp_ci==0
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if e1==1 & categopri_ci==3
replace tipocontrato_ci=2 if (e1>=2 & e1<=3) & categopri_ci==3
recode tipocontrato_ci .=0
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
* No hay pregunta de firma de contrato solo tipo de trabajo. MGD06/13/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if e1==1 & categopri_ci==3
replace tipocontrato_ci=2 if (e1>=2 & e1<=5) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************************************************************************************
***18._NEMPLEOS_CI : Numero de empleos.
*************************************************************************************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & c1==1
replace nempleos_ci=2 if emp_ci==1 & c1==2
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 
/*
*************************************************************************************
***19._FIRMAPEQ_CI : Trabajadores formales.***
*************************************************************************************
gen firmapeq_ci=.
replace firmapeq_ci=1 if c10<=5
replace firmapeq_ci=0 if c10>5 & c10<99
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores formales"
*/
*************************************************************************************
***20._SPUBLICO_CI : Personas que trabajan en el sector publico.***
*************************************************************************************
gen spublico_ci=.
label var spublico_ci "Personas que trabajan en el sector público"

************************************************************************************
*** 21._ OCUPA_CI: Ocupacion laboral actividad principal.***
************************************************************************************
* Nota MGD 08/04/2014: se recodifico segun la clasificacion CIUO-08.
gen ocupa_ci=.
replace ocupa_ci=1 if c9a>=2111 & c9a<=3719 & emp_ci==1
replace ocupa_ci=2 if c9a>=1111 & c9a<=1439 & emp_ci==1
replace ocupa_ci=3 if c9a>=4110 & c9a<=4419 & emp_ci==1
replace ocupa_ci=4 if ((c9a>=5210 & c9a<=5249) | (c9a>=9510 & c9a<=9520)) & emp_ci==1
replace ocupa_ci=5 if ((c9a>=5110 & c9a<=5170) | (c9a>=5311 & c9a<=5419) | (c9a>=9111 & c9a<=9129) | (c9a>=9611 & c9a<=9624)) & emp_ci==1
replace ocupa_ci=6 if ((c9a>=6111 & c9a<=6340) | (c9a>=9211 & c9a<=9216)) & emp_ci==1
replace ocupa_ci=7 if ((c9a>=7111 & c9a<=8350) | (c9a>=9311 & c9a<=9412)) & emp_ci==1
replace ocupa_ci=9 if c9a>=9629 & c9a!=. & emp_ci==1
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"

***********************************************************************************************
***22._RAMA_CI : Rama laboral actividad principal. En base a grandes Divisiones (ISIC Rev. 2)***
***********************************************************************************************
gen rama_ci=.
replace rama_ci=1 if (c8>=1111 & c8<=3220)   & emp_ci==1
replace rama_ci=2 if (c8>=5100 & c8<=9900) & emp_ci==1
replace rama_ci=3 if (c8>=10100 & c8<=33200) & emp_ci==1
replace rama_ci=4 if (c8>=35000 & c8<=39000) & emp_ci==1
replace rama_ci=5 if (c8>=41000 & c8<=43900) & emp_ci==1
replace rama_ci=6 if ((c8>=45100 & c8<=47990) | (c8>=55100 & c8<=56300)) & emp_ci==1
replace rama_ci=7 if ((c8>=49110 & c8<=53200) | (c8>=61100 & c8<=61909)) & emp_ci==1
replace rama_ci=8 if (c8>=64110 & c8<=68200) & emp_ci==1
replace rama_ci=9 if ((c8>=58110 & c8<=60200) | (c8>=62010 & c8<=63990) | (c8>=69100 & c8<=99000)) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


****************************************************************
***23._DURADES_CI : Duracion del desempleo.                  ***
****************************************************************

/*
           1 un mes o menos
           2 más de un mes a tres meses
           3 más de tres meses a seis meses
           4 más de seis meses a un año
           5 más de un año a tres años
           6 más de tres años
           9 ignorado
*/


gen durades_ci=.
*un mes o menos
replace durades_ci=(1+4.3)/2/4.3 if g2==1
*MÁS de 1 a 3 meses
replace durades_ci=(1+3)/2 if g2==2
*MÁS de 3 a 6 meses
replace durades_ci=(3+6)/2 if g2==3
*MÁS de 6 a 12 meses
replace durades_ci=(6+12)/2 if g2==4
*MÁS de 1 año a 3 años
replace durades_ci=(12+36)/2 if g2==5
*MÁS de 3 años
replace durades_ci=(36+48)/2 if g2==6
label variable durades_ci "Duracion del desempleo en meses"




****************************************************************
***24._ANTIGUEDAD_CI : Antiguedad en la actividad actual.***
****************************************************************
* Correccion MGD 08/05/2014: generacion de variable que estaba como missing.

* Independientes
g aux0=d4a
g aux1=d4b*30
g aux2=d4c*365
egen ind=rsum(aux0 aux1 aux2), m
replace ind=ind/365

* Asalariados
g aux3=e3a
g aux4=e3b*30
g aux5=e3c*365
egen asal=rsum(aux3 aux4 aux5), m
replace asal=asal/365

egen antiguedad_ci=rsum(ind asal), m
replace antiguedad_ci=. if antiguedad_ci==99
replace antiguedad_ci=. if antiguedad_ci>edad_ci
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***tamemp_ci***
*******************
         
*Costa Rica Pequeña 1 a 5, Mediana 6 a 19, Grande Más de 19
/*
gen tamemp_ci = 1 if (c10>=1 & c10<=5)
replace tamemp_ci = 2 if (c10>=6 & c10<=19)
replace tamemp_ci = 3 if (c10>19)
*/

* 2014, 06 Modificacion MLO, según los codigos del formulario
gen tamemp_ci = 1 if (c10>=1 & c10<=5)
replace tamemp_ci = 2 if (c10>=6 & c10<=10)
replace tamemp_ci = 3 if (c10>10  & c10!=.)
replace tamemp_ci = . if c10==99 /*missing*/

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp_o = 1 if (c10>=1 & c10<=9)
replace tamemp_o = 2 if (c10>=10 & c10<=11)
replace tamemp_o = 3 if (c10>11  & c10!=.)
replace tamemp_o = . if c10==99 /*missing*/

label define tamemp_o 1 "[1-9]" 2 "[10-49]" 3 "[50 y mas]"
label value tamemp_o tamemp_o
label var tamemp_o "Tamaño de empresa-OECD"


*******************
***categoinac_ci***
*******************
/*

raznoacteco:
           1 pensionado o jubilado
           2 rentista
           3 asistencia a centro de estudios
           4 obligaciones del propio hogar
           5 con discapacidad o enfermedad
           6 otro motivo

*/


gen categoinac_ci = 1 if (raznoacteco == 1 & condocup_ci==3)
replace categoinac_ci = 2 if (raznoacteco == 3 & condocup_ci==3)
replace categoinac_ci = 3 if (raznoacteco == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"


*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

gen byte formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"


					**************
					***INGRESOS***
					**************

***************************************************************************
***25._YLMPRI_CI : Ingreso laboral monetario actividad principal.***
***************************************************************************
/*
gen ganancia=c13f  			if c13f1==1
replace ganancia=c13f/2  	if c13f1==2
replace ganancia=c13f/3  	if c13f1==3
replace ganancia=c13f/4  	if c13f1==4
replace ganancia=c13f/6  	if c13f1==6
replace ganancia=c13f/12  	if c13f1==8
replace ganancia=.		  	if c13f1==9

gen salario=e12a

egen ylmpri_ci=rsum(ganancia salario), missing
replace ylmpri_ci=. if ganancia==. & salario==.
replace ylmpri_ci=0 if categopri_ci==4
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=. if ylmpri_ci==1.00e+08

label var  ylmpri_ci "Ingreso laboral monetario actividad principal"
*/
*Modificación Mayra Sáenz - Abril 2014

/*
*Sin considerar las imputaciones ni el coeficiente de subdeclaracion.
 capture drop spif aif gpif dif gpina gpsfi gpia spmn ipsp asp sesp bsp oisp inmpina inmpia

foreach v of varlist _all {
capture recode `v' (99999999=.)
*sum `v'
*return list
}    

capture drop spif
g spif =.
replace spif = d13b if 0 <= d13b & d13b < 99999999 

capture drop aif
g aif =.
replace aif = d13c1/12 if d13c==1

capture drop gpif
g gpif=.
replace gpif = d13d if 0 <= d13d & d13d < 99999999 

capture drop dif
g dif= d13e/12 if 0 <= d13e < 99999999 

capture drop gpina
g gpina = .
replace gpina = d14a if d12==2 | d13a==2
replace gpina = d16 if (d14a < d16 & d16 < 99999999) | (d14a== 99999999 & d16>0) 
replace gpina = d17 if (d3==1 | d3==2) & ((01401<=c8<=01403) |c8>=10101) & d17 <99999999 

capture drop gpsfi
g gpsfi =. 
replace gpsfi= c13f/c13f1 if  0< c13f1 <8 
replace gpsfi= c13f/12    if  c13f1==8 
replace gpsfi= 0 if c13f1==0 & c13f==0 

capture drop gpia
g gpia= d15a/d15a1 if (d12==3 | d13a==3) & 0 <d15a1< 8 
replace gpia=d15a/12 if (d12==3 | d13a==3) & d15a1==8 
replace gpia= 0 if (d12==3 | d13a==3) & d15a==0 & d15a1==0
replace gpia= 99999999 if (d12==3 | d13a==3) & d15a==99999999 & d15a1 == 9 
replace gpia= d16 if (d12==3 | d13a ==3) & (gpia == 99999999 | gpia == 0 & d16 > 0)
replace gpia= d16 if (d12==3 | d13a ==3) & (gpia < 99999999 & d16 > gpia) 
replace gpia= d15a if (d12==3 | (d12==1 & d13a==3)) & (d15a==0 & d15a1== .) 
replace gpia=d16 if (d12==3 | (d12==1 & d13a==2)) & (gpia==99999999 | gpia== . | gpia== 0) & d16~= . 
replace gpia = d17 if d3==1 | d3==2 & (01110 <= c8 <= 01300) | (01501 <= c8 <= 05002) 

capture drop inmpina
g inmpina=.
replace inmpina= d14f if d14d==1 & d14e==1 

capture drop inmpia
g inmpia=. 
replace inmpia =d15e/d15e1 if d15d==1 & (0<d15e1<8)
replace inmpia=d15e/12 if d15e1 ==8 
replace inmpia=0 if d15e==0 & d15e1==0

*Sin imputaciones
capture drop spmn
g spmn =.
replace spmn = e12a 	    if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==3 & e12a!=. 
replace spmn = e12a 	    if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==4 
replace spmn = e12a 	    if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==3 & e12a!=. 
replace spmn = e12a 	    if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==4 
replace spmn = e12a 	    if 1<e12a<99999999 & e12b==2 
replace spmn = e12a+e12c 	if 1<e12a<99999999 & e12b==3 & e10c==5 
replace spmn = e15a         if e12a==0 & e10a==1 & e10b==3 & e15a!=. 
replace spmn = e15a     	if e12a==0 & e10a==1 & e10b==4 
replace spmn = e15a 	    if e12a==0 & e10a==2 & e10b==3 & e15a!=.
replace spmn = e15a     	if e12a==0 & e10a==2 & e10b==4 
replace spmn = e15b     	if e12a==1 | e8a==1 

capture drop ipsp
g ipsp =.
replace ipsp = e11b if e11a==1 

capture drop asp
g asp=. 
replace asp= e14a1/12 if e14a==1 & e14a2==8 
replace asp = e14a1/e14a2 if e14a==1 & 0<e14a2<8 

capture drop sesp
g sesp =. 
replace sesp= e14b1/12 if e14b==3 & e14b2==8 
replace sesp = e14b1/e14b2 if e14b==3 & 0<e14b2<8 

capture drop bsp
g bsp =. 
replace bsp = e14c1/12 if e14c==5 & e14c2==8 
replace bsp = e14c1/e14c2  if e14c==5 & 0<e14c2<8 

capture drop oisp
g oisp=. 
replace oisp = e14d1/12 if e14d==7 & e14d2==8 
replace oisp = e14d1/e14d2 if e14d==7 & 0<e14d2<8 

capture drop spnma
g spnma =. 
replace spnma = e13a1 if e13a==1 

capture drop spnmv
g spnmv =.
replace spnmv = e13b1 if e13b==3 

capture drop spnmt
g spnmt =.
replace spnmt = e13c1 if e13c==5 

capture drop spnmvh
g spnmvh =. 
replace spnmvh = e13d1 if e13d==7 
 
capture drop spnmo
g spnmo =.
replace spnmo = e13e1 if e13e==1  
*/
egen ylmpri_ci=  rsum(spif aif gpif dif gpina gpsfi gpia spmn ipsp asp sesp bsp oisp), missing
replace ylmpri_ci= . if ithn == 99999999 
replace ylmpri_ci= 0 if ithn == 0
replace ylmpri_ci= . if ithn == .
label var  ylmpri_ci "Ingreso laboral monetario actividad principal"


********************************************************************************
***26._YLNMPRI_CI : Ingreso laboral no monetario actividad principal.***
********************************************************************************
/*for var e13a1 e13b1 e13c1 e13d1 e13e1 e14a1 e14b1 e14c1 e14d1: replace X=. if X==99999999

gen temp1=e14a1  		if e14a2==1 
replace temp1=e14a1/2  	if e14a2==2
replace temp1=e14a1/3  	if e14a2==3
replace temp1=e14a1/4  	if e14a2==4
replace temp1=e14a1/6  	if e14a2==6
replace temp1=e14a1/12 	if e14a2==8
replace temp1=.		  	if e14a2==9

gen temp2=e14b1  		if e14b2==1
replace temp2=e14b1/2  	if e14b2==2
replace temp2=e14b1/3  	if e14b2==3
replace temp2=e14b1/4  	if e14b2==4
replace temp2=e14b1/6  	if e14b2==6
replace temp2=e14b1/12 	if e14b2==8
replace temp2=.		  	if e14b2==9

gen temp3=e14c1  		if e14c2==1
replace temp3=e14c1/2  	if e14c2==2
replace temp3=e14c1/3  	if e14c2==3
replace temp3=e14c1/4  	if e14c2==4
replace temp3=e14c1/6  	if e14c2==6
replace temp3=e14c1/12 	if e14c2==8
replace temp3=.		  	if e14c2==9

gen temp4=e14d1  		if e14d2==1
replace temp4=e14d1/2  	if e14d2==2
replace temp4=e14d1/3  	if e14d2==3
replace temp4=e14d1/4  	if e14d2==4
replace temp4=e14d1/6  	if e14d2==6
replace temp4=e14d1/12 	if e14d2==8
replace temp4=.		  	if e14d2==9

egen ylnmpri_ci=rsum(e13a1 e13b1 e13c1 e13d1 e13e1 temp1 temp2 temp3 temp4), missing
egen perd=rowmiss(e13a1 e13b1 e13c1 e13d1 e13e1 temp1 temp2 temp3 temp4)
replace ylnmpri_ci=. if perd==9
drop temp* perd
*/
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
egen ylnmpri_ci= rsum(inmpina inmpia spnma spnmv spnmt spnmvh spnmo), missing
replace ylnmpri_ci= . if ithn == 99999999 
replace ylnmpri_ci= 0 if ithn == 0
replace ylnmpri_ci= . if ithn == .
label var ylnmpri "Ingreso laboral no monetario act. principal"

***********************************************************************************************
***27._NRYLMPRI_CI : Identificador de No Respuesta (NR) del ingreso de la actividad principal.
***********************************************************************************************
gen nrylmpri_ci=.
label var nrylmpri_ci "Identificador de No Respuesta (NR) del ingreso de la actividad principal"

***************************************************************************
***28._YLMSEC_CI : Ingreso laboral monetario actividad secundaria.
***************************************************************************
/*
gen ganancia2=f8a	  		if f8a1==1
replace ganancia2=f8a/2  	if f8a1==2
replace ganancia2=f8a/3  	if f8a1==3
replace ganancia2=f8a/4  	if f8a1==4
replace ganancia2=f8a/6  	if f8a1==6
replace ganancia2=f8a/12  	if f8a1==8
replace ganancia2=.		  	if f8a1==9


gen salario2=f10a

egen ylmsec_ci=rsum(ganancia2 salario2), missing
replace ylmsec_ci=. if ganancia2==. & salario2==.
replace ylmsec_ci=0 if categosec_ci==4
replace ylmsec_ci=. if emp_ci==0
replace ylmsec_ci=. if ylmsec_ci==1.00e+08
label var ylmsec_ci "Ingreso laboral monetario actividad secundaria"
*/


*Modificación Mayra Sáenz - Abril 2014
/*Sin considerar imputaciones
capture drop gsi
g gsi  =.
replace gsi= f8a/f8a1 if 0<f8a1<8 
replace gsi= f8a/12 if f8a1==8 
replace gsi =0 if f8a== 0 & f8a1==0 

capture drop inmsi
g inmsi =.  
replace inmsi = f8b/f8b1 if 0<f8b1<8 
replace inmsi = f8b/12 if f8b1==8 
replace inmsi = 0 if f8b1==0 & f8b==0 

capture drop ssmn
g ssmn = .
replace ssmn = f10a 	  if 2<f10a<99999999 & f10b==1 & f9a ==1 & f9b ==3 & f10a !=.  
replace ssmn = f10a       if 2<f10a<99999999 & f10b==1 & f9a ==1 & f9b ==4 
replace ssmn = f10a 	  if 2<f10a<99999999 & f10b==1 & f9a ==2 & f9b ==3 & f10a !=.  
replace ssmn = f10a 	  if 2<f10a<99999999 & f10b==1 & f9a ==2 & f9b ==4 
replace ssmn = f10a 	  if 2<f10a<99999999 & f10b==2 
replace ssmn = f10a+f10c  if 2<f10a<99999999 & f10b==3 & f9c==5 
replace ssmn = f12a	      if f10a ==0 & f9a ==1 & f9b ==3 & f10a !=. 
replace ssmn = f12a       if f10a ==0 & f9a ==1 & f9b ==4 
replace ssmn = f12a 	  if f10a ==0 & f9a ==2 & f9b ==3 & f10a !=.
replace ssmn = f12a       if f10a ==0 & f9a==2 & f9b==4 
replace ssmn = f12b       if f10a ==1  

capture drop ssnm
g ssnm =.
replace ssnm = f11b if f11a==1 

capture drop ia
g ia =.	
replace ia = h9a1/h9a2 	if h9a==1 & 0 < h9a2 <8 
replace ia = h9a1/12 	if h9a==1 & h9a2==8 

capture drop ii
g ii =.
replace ii = h9b1/h9b2 	if h9b==1 & 0 < h9b2 <8 
replace ii = h9b1/12 	if h9b==1 & h9b2==8 

capture drop id 
g id=.
replace id = h9c1/h9c2 	if h9c==1 & 0 < h9c2 <8 
replace id = h9c1/12 	if h9c==1 & h9c2==8 

capture drop ib
g ib=. 	
replace ib = h9d1/h9d2 	if h9d==1 & 0 < h9d2 <8 
replace ib = h9d1/12 	if h9d==1 & h9d2==8 

capture drop trnc
g trnc =. 	
replace trnc = h9e1/h9e2 	if h9e==1 & 0 < h9e2 <8 
replace trnc = h9e1/12 	if h9e==1 & h9e2==8 

capture drop timas
g timas =.
replace timas = h9f1/h9f2 	if h9f==1 & 0 < h9f2 <8 
replace timas = h9f1/12 	if h9f==1 & h9f2==8 

capture drop ts
g ts=.	
replace ts = h9g1/h9g2 	if h9g==1 & 0 < h9g2 <8 
replace ts = h9g1/12 	if h9g==1 & h9g2==8 

capture drop tbc
g tbc =.
replace tbc = h9h1/h9h2 	if h9h==1 & 0 < h9h2 <8 
replace tbc = h9h1/12 	if h9h==1 & h9h2==8 

capture drop tpa
g tpa=.
replace tpa = h9i1/h9i2 	if h9i==1 & 0 < h9i2 <8 
replace tpa = h9i1/12 	if h9i==1 & h9i2==8 

capture drop tpn
g tpn=.
replace tpn = h9j1/h9j2 	if h9j==1 & 0 < h9j2 <8 
replace tpn = h9j1/12 	if h9j==1 & h9j2==8 

capture drop tpe
g tpe=.
replace tpe = h9k1/h9k2 	if h9k==1 & 0 < h9k2 <8 
replace tpe = h9k1/12 	if h9k==1 & h9k2==8 

capture drop tap
g tap=.
replace tap = h9l1/h9l2 	if h9l==1 & 0 < h9l2 <8 
replace tap = h9l1/12 	if h9l==1 & h9l2==8 
 
capture drop te
g te=.	
replace te = h9m1/h9m2 	if h9m==1 & 0 < h9m2 <8 
replace te = h9m1/12 	if h9m==1 & h9m2==8 

capture drop tdp
g tdp=.
replace tdp = h9n1/h9n2 	if h9n==1 & 0 < h9n2 <8 
replace tdp = h9n1/12 	if h9n==1 & h9n2==8 

capture drop ot
g ot =.
replace ot = h9o1/h9o2 	if h9o==1 & 0 < h9o2 <8 
replace ot = h9o1/12 	if h9o==1 & h9o2==8 

capture drop tnm 
g tnm =.	
replace tnm = h10c/h10c1 	if h10a==1 & 0<h10c1<8 //& línea==1 
replace tnm = h10c/12 	if h10a==1 & h10c1==8 //& línea==1 
  */
egen ylmsec_ci= rsum(gsi ssmn), missing
replace ylmsec_ci= . if ithn == 99999999 
replace ylmsec_ci= 0 if ithn == 0
replace ylmsec_ci= . if ithn == .
label var ylmsec_ci "Ingreso laboral monetario actividad secundaria"

********************************************************************************
***29._YLNMSEC_CI : Ingreso laboral no monetario actividad secundaria.***
********************************************************************************
*gen ylnmsec_ci=f11b
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
egen ylnmsec_ci= rsum(inmsi ssnm), missing
replace ylnmsec_ci= . if ithn == 99999999 
replace ylnmsec_ci= 0 if ithn == 0
replace ylnmsec_ci= . if ithn == .
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"

************************************************************************************
***30._YLM_CI : Ingreso laboral monetario total. ***
************************************************************************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"

***************************************************************************************
***31._YLNM_CI : Ingreso laboral no monetario total. ***
***************************************************************************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral no monetario total"

*********************************************************************
*** 32._YLMOTROS_CI : Ingreso laboral monetario otros trabajos.    ***
*********************************************************************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario otros trabajos"

*********************************************************************
***  33._YLNMOTROS_CI : Ingreso laboral no monetario otros trabajos.
*********************************************************************


gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral no monetario otros trabajos"

***********************************************************************
***34._YNLM_CI : Ingreso no laboral monetario (otras fuentes).      ***
***********************************************************************
*Para este año se dispone de información sobre: alquileres, intereses, dividendos, beneficios de cooperativas,
*pensiones, IMAS, subsidios, becas, pensión alimenticia, jubilaciones nacionales, jubilaciones internacionales, 
*aguinaldos, remesas, transferencias y otras transferencias.
/*
*Montos:
ren h9a1 h9a3
ren h9b1 h9a4
ren h9c1 h9a5
ren h9d1 h9a6
ren h9e1 h9a7
ren h9f1 h9a8
ren h9g1 h9a9
ren h9h1 h9a10
ren h9i1 h9a11
ren h9j1 h9a12
ren h9k1 h9a13
ren h9l1 h9a14
ren h9m1 h9a15
ren h9n1 h9a16
ren h9o1 h9a17

*Frecuencias
ren h9a2 h9b3
ren h9b2 h9b4
ren h9c2 h9b5
ren h9d2 h9b6
ren h9e2 h9b7
ren h9f2 h9b8
ren h9g2 h9b9
ren h9h2 h9b10
ren h9i2 h9b11
ren h9j2 h9b12
ren h9k2 h9b13
ren h9l2 h9b14
ren h9m2 h9b15
ren h9n2 h9b16
ren h9o2 h9b17


forvalues var=3(1)17{
	recode h9a`var' (99999999=.)
	gen ing_`var'=h9a`var'	 		if h9b`var'==1
	replace ing_`var'=h9a`var'/2 	if h9b`var'==2
	replace ing_`var'=h9a`var'/3 	if h9b`var'==3
	replace ing_`var'=h9a`var'/4 	if h9b`var'==4
	replace ing_`var'=h9a`var'/6 	if h9b`var'==6
	replace ing_`var'=h9a`var'/12 	if h9b`var'==8
	replace ing_`var'=.			 	if h9b`var'==9
		}	

egen ynlm_ci=rsum(ing_3-ing_17), missing
*/
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
*Ingreso por renta de la propiedad, transferencias monetarias
egen ynlm_ci=rsum(ia ii id ib trnc timas ts tbc tpa tpn tpe tap te tdp ot), missing
replace ynlm_ci= . if ithn == 99999999 
replace ynlm_ci= 0 if ithn == 0
replace ynlm_ci= . if ithn == .
label var ynlm_ci "Ingreso no laboral monetario (otras fuentes)"
**********************************************************************
*** 35._YNLNM_CI : Ingreso no laboral no monetario (otras fuentes).***
**********************************************************************
**Modificación Mayra Sáenz - Abril 2014
*Transferencias no monetarias.
gen ynlnm_ci=tnm
replace ynlnm_ci= . if ithn == 99999999 
replace ynlnm_ci= 0 if ithn == 0
replace ynlnm_ci= . if ithn == .
label var ynlnm_ci "Ingreso no laboral no monetario"
**********************************************************************
***36._REMESAS_CI : Remesas reportadas por el individuo.***
**********************************************************************
*gen remesas_ci=ing_15
*drop ing_3-ing_15
*Modificación Mayra Sáenz - Abril 2014
*Transferencias del extranjero
g remesas_ci= te
replace remesas_ci= . if ithn == 99999999 
replace remesas_ci= 0 if ithn == 0
replace remesas_ci= . if ithn == .
label var remesas_ci "Remesas reportadas por el individuo"

***********************************************************************************
***37._NRYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros
***No Sabe/No Responde el ingreso de la actividad principal. 
***********************************************************************************

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Id. hogar si algun miembro no sabe o no respond el ingreso act. principal"

***********************************************************************************************
***38._TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***39._TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

********************************************************
***40._YLM_CH : Ingreso laboral monetario del Hogar.***
********************************************************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar"

**********************************************************
*** 41._YLMNR_CH : Ingreso laboral monetario del Hogar.***
**********************************************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del Hogar considera No respuesta"

************************************************************
*** 42._YNLM_CH : Ingreso no laboral monetario del Hogar.***
************************************************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del Hogar"

************************************************************
*** 43._YLNM_CH : Ingreso laboral no monetario del Hogar.***
************************************************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch  "Ingreso laboral no monetario del Hogar"

************************************************************
***44._REMESAS_CH : Remesas del Hogar.                   ***
************************************************************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas del Hogar"

* N/A*
************************************************************
***45._YNLNM_CH : Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 
************************************************************
***46._RENTAIMP_CH : Rentas imputadas del hogar.
************************************************************
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

************************************************************
***47._AUTOCONS_CH : Autoconsumo del Hogar.
************************************************************
gen autocons_ch=.
label var autocons_ch "Autoconsumo del Hogar"

************************************************************
***48._AUTOCONS_CI : Autoconsumo reportado por el individuo.
************************************************************
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

********************************************************************
***49._YLMHOPRI_CI : Salario  monetario de la actividad principal.
********************************************************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario  monetario de la actividad principal"

*********************************************************************
*** 50._YLMHO_CI : Salario  monetario de todas las actividades.***
*********************************************************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario  monetario de todas las actividades"


*====================================================================================================================================*
*                                                   VARIABLES DE EDUCACIÓN
*====================================================================================================================================*			
* En esta área son en total 19 variables.

*********************************************************************
***1._AEDU_CI : Años de educacion.
*********************************************************************

gen aedu_ci=.
replace aedu_ci=0 if a15==0 | a15==1 
label var aedu_ci "Años de educación"

*Primaria
replace aedu_ci=1 if a15==11 
replace aedu_ci=2 if a15==12
replace aedu_ci=3 if a15==13
replace aedu_ci=4 if a15==14
replace aedu_ci=5 if a15==15
replace aedu_ci=6 if a15==16

*Secundaria (académica y técnica)
replace aedu_ci=7 if a15==21 | a15==31
replace aedu_ci=8 if a15==22 | a15==32
replace aedu_ci=9 if a15==23 | a15==33
replace aedu_ci=10 if a15==24 | a15==34
replace aedu_ci=11 if a15==25 | a15==26 | a15==35 
replace aedu_ci=12 if a15==36 | a15==37

*Superior (universitario o para-universitario)
replace aedu_ci=13 if a15==41 | a15==51
replace aedu_ci=14 if a15==42 | a15==52
replace aedu_ci=15 if a15==43 | a15==53
replace aedu_ci=16 if a15==54 
replace aedu_ci=17 if a15==55
replace aedu_ci=18 if a15==56

*Postgrado
replace aedu_ci=19 if a15==71 | a15==81
replace aedu_ci=20 if a15==72 | a15==82
replace aedu_ci=21 if a15==73 | a15==83
replace aedu_ci=22 if a15==74 | a15==84


********************************************************************************************************************************
***2._EDUNO_CI : Personas sin educacion (se refiere a primaria, secundaria y universitaria(o terciaria); excluye preescolar).
********************************************************************************************************************************
/*gen eduno_ci=0
replace eduno_ci=1 if a15==0 | a15==1 
label variable eduno_ci "Cero anios de educacion"*/

gen eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
label variable eduno_ci "Cero anios de educacion"


********************************************************************************************************************************
***3._EDUPI_CI : Personas que no han completado la educacion primaria.
********************************************************************************************************************************
/*gen edupi_ci=0
replace edupi_ci=1 if (a15>=11 & a15<16) 
label variable edupi_ci "Primaria incompleta"*/

gen edupi_ci=0
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
label variable edupi_ci "Primaria incompleta"

********************************************************************************************************************************
***4._EDUPC_CI : Personas que han completado la educacion primaria.
********************************************************************************************************************************
/*gen edupc_ci=0
replace edupc_ci=1 if a15==16
label variable edupc_ci "Primaria completa"*/

gen edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
label variable edupc_ci "Primaria completa"

********************************************************************************************************************************
***5._EDUSI_CI : Peronas que no han completado la educacion secundaria.
********************************************************************************************************************************
/*gen edusi_ci=0
replace edusi_ci=1 if (a15>=21 & a15<=26) 
replace edusi_ci=1 if (a15>=31 & a15<=35) 
label variable edusi_ci "Secundaria incompleta"*/

gen edusi_ci=0
replace edusi_ci=1 if (aedu_ci>6 & aedu_ci<11) | a15==35
label variable edusi_ci "Secundaria incompleta"

********************************************************************************************************************************
***6._EDUSC_CI : Personas que han completado la educacion secundaria.

********************************************************************************************************************************
/*gen edusc_ci=0
replace edusc_ci=1 if  a15==36 | a15==37 
label variable edusc_ci "Secundaria completa"*/

gen edusc_ci=0
replace edusc_ci=1 if  (aedu_ci==11 | aedu_ci==12) & a15!=35
label variable edusc_ci "Secundaria completa"

********************************************************************************************************************************
***7._EDUS1I_CI : Personas que no han completado el primer ciclo de la educacion secundaria.
********************************************************************************************************************************
gen edus1i_ci=0
replace edus1i_ci=1 if (a15>=21 & a15<=22)
replace edus1i_ci=1 if (a15>=31 & a15<=32)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

********************************************************************************************************************************
***8._EDUS1C_CI : Personas que han completado el primer ciclo de la educacion secundaria.
********************************************************************************************************************************
gen edus1c_ci=0
replace edus1c_ci=1 if a15==23 | a15==33
label variable edus1c_ci "1er ciclo de la secundaria completo"

********************************************************************************************************************************
***9._EDUS2I_CI : Personas que no han completado el segundo ciclo de la educacion secundaria.
********************************************************************************************************************************
gen edus2i_ci=0
replace edus2i_ci=1 if (a15==24 | a15 == 25 | a15 == 26)
replace edus2i_ci=1 if (a15>=34 & a15<=35)
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

********************************************************************************************************************************
***10._EDUS2C_CI : Personas que han completado el segundo ciclo de la educacion secundaria.
********************************************************************************************************************************
gen edus2c_ci=0
replace edus2c_ci=1 if a15==36 | a15==37
label variable edus2c_ci "2do ciclo de la secundaria completo"

********************************************************************************************************************************
***11._EDUUI_CI : Peronas que no han completado la educacion universitaria o terciaria.***
********************************************************************************************************************************
gen eduui_ci=0
replace eduui_ci=1 if (a15>=41 & a15<=43)
replace eduui_ci=1 if (a15>=51 & a15<=54)
label variable eduui_ci "Superior incompleto"

********************************************************************************************************************************
***12._EDUUC_CI : Personas que han completado la educacion universitaria o terciaria.***
********************************************************************************************************************************
gen byte eduuc_ci=0
replace eduuc_ci=1 if a15>=55
label variable eduuc_ci "Superior completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

********************************************************************************************************************************
***13._EDUPRE_CI : Educacion preescolar.
********************************************************************************************************************************
gen edupre_ci=.
replace edupre_ci=(a15==1)
label variable edupre_ci "Educacion preescolar"

********************************************************************************************************************************
***13.A_ASISPRE_CI : Asistencia a Educacion preescolar.
********************************************************************************************************************************
*Variable agregada por Iván Bornacelly - 01/16/2017
	g asispre_ci=.
	replace asispre_ci=1 if (a14==1 | a14==2) & a5>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"

********************************************************************************************************************************
***14._EDUAC_CI : Educación terciaria académica versus educación terciaria no-académica***
********************************************************************************************************************************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

********************************************************************************************************************************
***15._ASISTE_CI : Personas que actualmente asisten a centros de enseñanza.***
********************************************************************************************************************************
gen asiste_ci=.
replace asiste_ci=1 if a14>=1 & a14<=8
replace asiste_ci=0 if a14==0
label variable asiste_ci "Asiste actualmente a la escuela"

********************************************************************************************************************************
***16._PQNOASIS (2) : Razones para no asistir a la escuela.***
********************************************************************************************************************************
gen pqnoasis_ci=a18
label define pqnoasis_ci 1  "tiene que trabajar" 2  "prefiere trabajar" 3  "tiene que cuidar niños, ancianos u otras personas" ///
4  "tiene que ayudar en oficios domésticos  " 5  "no puede pagar los estudios  " 6  "problemas de acceso al sistema escolar" ///
7  "le cuesta el estudio" 8  "no está interesado en el aprendizaje formal" 9  "embarazo o matrimonio" 10  "enfermedad o discapacidad" ///
11  "no tiene edad" 12  "falta ganar pruebas del mep; exámenes de admisión.  " 13  "otro" 99  "ignorado"

label value pqnoasis_ci pqnoasis_ci
label variable pqnoasis_ci  " Razón por que no asiste a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if a18==5
replace pqnoasis1_ci = 2 if a18==1
replace pqnoasis1_ci = 3 if a18==7 | a18==10
replace pqnoasis1_ci = 4 if a18==2 | a18==8
replace pqnoasis1_ci = 5 if a18==3 | a18==4 | a18==9
replace pqnoasis1_ci = 7 if a18==11 
replace pqnoasis1_ci = 8 if a18==6
replace pqnoasis1_ci = 9 if a18==12 | a18==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

********************************************************************************************************************************
***17._REPITE_CI : Personas que han repetido al menos un año o grado.***
********************************************************************************************************************************

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado o año"

********************************************************************************************************************************
***18._REPITEULT_CI : Personas que han repetido el ultimo grado.
********************************************************************************************************************************

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el último grado"

********************************************************************************************************************************
***19._EDUPUB_CI : Personas que asisten a centros de enseñanza publicos.***
********************************************************************************************************************************
gen edupub_ci=0
replace edupub_ci=1 if a16==1 
replace edupub_ci=. if a16==.
label var edupub_ci "Personas asisten a centros de enseñanza públicos"
*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if a15>=41 & a15<=43
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

*====================================================================================================================================*
*                                                     VARIABLES DE LA VIVIENDA                                                       *
*====================================================================================================================================*

********************************************************************************************************************************
***1._AGUARED_CH : Acceso a una fuente de agua por red
********************************************************************************************************************************
gen aguared_ch=.
replace aguared_ch=1 if v11>=1 & v11<=3
replace aguared_ch=0 if v11==0
label var aguared_ch "Acceso a una fuente de agua por red"

********************************************************************************************************************************
***2._AGUADIST_CH : Ubicación de la principal fuente de agua
********************************************************************************************************************************
gen aguadist_ch=.
replace aguadist_ch=1 if v11==1
replace aguadist_ch=2 if v11==2
replace aguadist_ch=3 if v11==3
label define aguadist_ch 1 "tubería dentro de la vivienda" 2 " tubería fuera de la vivienda pero dentro del lote o edificio" 3 "tubería fuera del lote o edificio"
label var aguadist_ch "Ubicación de la principal fuente de agua"

********************************************************************************************************************************
***3._AGUAMALA_CH : La principal fuente de agua es "Unimproved" según los MDG
********************************************************************************************************************************
/*
***NOTA***
 ____________________________________________________________________________________________________________________________
| Toma el valor de 1 cuando la principal fuente de agua es "unimproved" y 0 caso contrario, por lo cual asigna 1             |
| cuando la pregunta v11 es igual a 6 o 7, pero la v11 sólo toma valores de 0,1,2,3 y 9.                                     |
| Debería tomar el valor de 1 cuando v11 es igual a 0.                                                                       |
|____________________________________________________________________________________________________________________________|
*/


gen aguamala_ch=.
replace aguamala_ch=0 if v11>=1 & v11<=5
replace aguamala_ch=1 if v11==0
label var aguamala_ch "Principal fuente de agua es unimproved"

********************************************************************************************************************************
***4._AGUAMIDE_CH : El hogar usa un medidor para pagar por su consumo de agua
********************************************************************************************************************************
gen aguamide_ch=.
label var aguamide "Hogar usa un medidor para pagar por su consumo de agua"

********************************************************************************************************************************
***5._LUZ_CH : La principal fuente de iluminación es electricidad
********************************************************************************************************************************
gen luz_ch=.
replace luz_ch=1 if v15>=1 & v15<=5
replace luz_ch=0 if v15==0| v15==6 
label var luz_ch "Hogar usa un medidor para pagar por su consumo de electricidad"


********************************************************************************************************************************
***6._LUZMIDE_CH : El hogar usa un medidor para pagar por su consumo de electricidad
********************************************************************************************************************************
gen luzmide_ch=.
label var luzmide_ch "Principal fuente de iluminación es electricidad"

********************************************************************************************************************************
***7._COMBUST_CH : El combustible principal usado en el hogar es gas o electricidad
********************************************************************************************************************************
gen combust_ch=.
replace combust_ch=1 if  v16==1 | v16==2
replace combust_ch=0 if  v16==3 | v16==4
label var combust_ch "Combustible principal del hogar es gas o electricidad"

********************************************************************************************************************************
***8._BANO_CH : El hogar tiene algún tipo de servicio higiénico (inodoro o letrina)
********************************************************************************************************************************
gen bano_ch=.
replace bano_ch=1 if v14a==1
replace bano_ch=0 if v14a==0
label var bano_ch "Hogar tiene algún tipo de servicio higiénico"
********************************************************************************************************************************
***9._BANOEX_CH : El servicio higiénico es de uso exclusivo del hogar
********************************************************************************************************************************
gen banoex_ch=.
replace banoex_ch=1 if v14b==1
replace banoex_ch=0 if v14b==2
label var banoex_ch "Servicio higiénico de uso exclusivo del hogar"

********************************************************************************************************************************
***10._DES1_CH : Tipo de desagüe incluyendo la definición de "Unimproved" del MDG
********************************************************************************************************************************
gen des1_ch=.
replace des1_ch=0 if v13a==0
replace des1_ch=1 if v13a==1 |v13a==2 |v13a==3
replace des1_ch=2 if v13a==4 
label define des1_ch 0 "No tiene" 1 "Alcantarilla, tanque séptico común y fosa biológica" 2 "hueco, letrina"
label value des1_ch des1_ch
label var des1_ch "Tipo de desague incluido unimproved"
********************************************************************************************************************************
***11._DES2_CH : Tipo de desagüe sin incluir la definición de "Unimproved" del MDG
********************************************************************************************************************************
***NOTA***
/*
____________________________________________________________________________________________________________________________
|Según la definición de variables del sociómetro, esta variable debería asignar el valor de 2 sólo cuando el hogar tiene    |
|'otro tipo de desague'. Sin embargo, asigna 2 cuando tiene 'otro tipo de desague' Y cuando tiene 'pozo ciego o letrina',   |
| lo cual,debería estar con el código 1.                                                                                    | 
|___________________________________________________________________________________________________________________________|
*/
gen des2_ch=0 if v13a==0
replace des2_ch =1 if v13a==1 |v13a==2 |v13a==3 | v13a==4 
replace des2_ch=2 if v13a==5 
label var des2_ch "Tipo de desague sin incluir unimproved"

********************************************************************************************************************************
***12._PISO_CH : Materiales de construcción del piso
********************************************************************************************************************************
gen piso_ch=.
replace piso_ch=0 if v6==0
replace piso_ch=1 if v6>=1 & v6<=3
replace piso_ch=2 if v6==4 | v6 ==5 
label define piso_ch 0 "Piso de tierra" 1 "Materiales permanentes" 2 "Otros materiales"
label value piso_ch piso_ch
label var piso_ch "Materiales de construcción del piso"


********************************************************************************************************************************
***13._PARED_CH : Materiales de construcción de las paredes
********************************************************************************************************************************
gen pared_ch=.
replace pared_ch=0 if v3==7 |v3==0
replace pared_ch=1 if v3>=1  & v3<=6
replace pared_ch=2 if v3==8
label define pared_ch  0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales"
label value pared_ch pared_ch
label var pared_ch "Materiales de construcción de las paredes"
********************************************************************************************************************************
***14._TECHO_CH : Materiales de construcción del techo
********************************************************************************************************************************
gen techo_ch=.
replace techo_ch=0 if v4==0
replace techo_ch=1 if v4>=1 & v4<=3
replace techo_ch=2 if v4==4 |v4==5
label define techo_ch  0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales"
label value techo_ch techo_ch
label var techo_ch "Materiales de construcción del techo"

********************************************************************************************************************************
***15._RESID_CH : Método de eliminación de residuos
********************************************************************************************************************************
gen resid_ch=.
replace resid_ch=0 if v17a==1
replace resid_ch=1 if v17a==2 |v17a==3
replace resid_ch=2 if v17a==4 |v17a==5
replace resid_ch=3 if v17a==6
label define resid_ch 0 "Recolección pública" 1 "Quemados o enterrados" 2 "Tirados a un espacio abierto" 3 "Otros"
label value resid_ch resid_ch
label var resid_ch "Método de eliminación de residuos"

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v12 >=1 & v12 <=5)
replace aguamejorada_ch = 0 if (v12 >=6 & v12 <=7)
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  ((v13a >=1 & v13a <=4) & v13b ==1)
replace banomejorado_ch = 0 if  ((v13a >=1 & v13a <=4) & v13b ==2) | (v13a ==5 | v13a ==0) |   v14a == 2

********************************************************************************************************************************
***16._DORM_CH : Cantidad de habitación que se destinan exclusivamente para dormir
********************************************************************************************************************************
gen dorm_ch=.
replace dorm_ch=v8 if v8>=0 & v8<=10
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

********************************************************************************************************************************
***17._CUARTOS_CH : Cantidad de habitaciones en el hogar***
********************************************************************************************************************************
gen cuartos_ch=.
replace cuartos_ch=v9 if v9>=1 & v9<=20
label var cuartos_ch "Cantidad de habitaciones en el hogar"

********************************************************************************************************************************
***18._COCINA_CH : Si existe un cuarto separado y exclusivo para cocinar
********************************************************************************************************************************
gen cocina_ch=.
label var cocina_ch "Si existe un cuarto separado y exclusivo para cocinar"

********************************************************************************************************************************
**19._TELEF_CH : El hogar tiene servicio telefónico fijo
********************************************************************************************************************************
gen telef_ch=.
replace telef_ch=1 if  v18b==3
replace telef_ch=0 if  v18b==4
label var telef_ch "El hogar tiene servicio telefónico"

********************************************************************************************************************************
**20._REFRIG_CH : El hogar posee heladera o refrigerador.
********************************************************************************************************************************
gen refrig_ch=.
replace refrig_ch=1 if  v18c==5
replace refrig_ch=0 if  v18c==6
label var refrig_ch "El hogar posee heladera o refrigerador"


********************************************************************************************************************************
**21._FREEZ_CH : El hogar posee freezer o congelador.
********************************************************************************************************************************
gen freez_ch=.
label var freez_ch "El hogar posee freezer o congelador"

********************************************************************************************************************************
**22._AUTO_CH : El hogar posee automóvil particular
********************************************************************************************************************************
gen auto_ch=.
replace auto_ch=1 if  v18i==3
replace auto_ch=0 if  v18i==4
label var auto_ch "El hogar posee automóvil particular"

********************************************************************************************************************************
**23._COMPU_CH : El hogar posee computadora
********************************************************************************************************************************
* Modificaciones Marcela Rubio Septiembre 2014: cuestionario cambió de 2011 a 2012 y ahora pregunta por tenencia de computadora portatil y de escritorio
/*
gen compu_ch=.
replace compu_ch=1 if  v18f==3
replace compu_ch=0 if  v18f==4
label var compu_ch "El hogar posee computadora"
*/
gen compu_ch= (v18f==3 | v18ff==5)
label var compu_ch "El hogar posee computadora"
********************************************************************************************************************************
**24._INTERNET_CH : El hogar posee conexión a Internet
********************************************************************************************************************************
gen internet_ch=.
replace internet_ch=1 if  v19a==1
replace internet_ch=0 if  v19a==0
label var internet_ch "El hogar posee conexión a Internet"

********************************************************************************************************************************
**25._CEL_CH : El hogar tiene servicio telefónico celular
********************************************************************************************************************************
gen cel_ch=.
replace cel_ch=1 if  v18a==1
replace cel_ch=0 if  v18a==2
label var cel_ch "El hogar tiene servicio telefónico celular"

********************************************************************************************************************************
**26._VIVI1_CH : Tipo de vivienda en la que reside el hogar
********************************************************************************************************************************
gen vivi1_ch=.
replace vivi1_ch=1 if  v1==1 |v1==2
replace vivi1_ch=2 if  v1==3 |v1==4
replace vivi1_ch=3 if  v1==5 |v1==6 | v1==7
label define vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros"
label value vivi1_ch vivi1_ch
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"

********************************************************************************************************************************
**27._VIVI2_CH : La vivienda en la que reside el hogar es una casa o un departamento.
********************************************************************************************************************************
gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3
label var vivi2_ch "La vivienda en la que reside es una casa o departamento"
********************************************************************************************************************************
**28._VIVIPROP_CH : Propiedad de la vivienda
********************************************************************************************************************************
gen viviprop_ch=.
replace viviprop_ch=0 if v2a==3
replace viviprop_ch=1 if v2a==1
replace viviprop_ch=2 if v2a==2
replace viviprop_ch=3 if v2a==4 | v2a==5
label define viviprop_ch 0 "Alquilada" 1 " Propia y totalmente pagada" 2 "Propia y en proceso de pago"  3 "Ocupada (propia de facto)"
label value viviprop_ch viviprop_ch
label var viviprop_ch "Propiedad de la vivienda"
********************************************************************************************************************************
**29._VIVITIT_CH : El hogar posee un título de propiedad
********************************************************************************************************************************
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"
********************************************************************************************************************************
**30._VIVIALQ_CH : Alquiler mensual
********************************************************************************************************************************
gen vivialq_ch=.
replace vivialq_ch=v2a1 	if v2a == 3
replace vivialq_ch=. 		if v2a1==99999999
label var vivialq_ch "Alquiler mensual"
********************************************************************************************************************************
**31._VIVIALQIMP_CH : Alquiler mensual imputado
********************************************************************************************************************************
***NOTA***
/*
 ____________________________________________________________________________________________________________________________
| Esta variable estaba reemplazando los valores en la variable anterior, hubo un error en reemplazar los valores en la       |
| variable correcta.                                                                                                         | 
gen vivialqimp_ch=.
replace vivialq_ch=v2b 	
replace vivialq_ch=. 		if v2b==99999999
|____________________________________________________________________________________________________________________________|
*/

gen vivialqimp_ch=.
replace vivialqimp_ch=v2b 	
replace vivialqimp_ch=. 		if v2b==99999999
label var vivialqimp_ch " Alquiler mensual imputado"

*******************
*** benefdes_ci ***
*******************

g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"

******************************
* Variables SPH - PMTC y PNC *
******************************

* PTMC: Avancemos (a8)
* PNC: Pensión régimen no contributivo (h9e)

* Ingreso del hogar
egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)
drop ingreso_total

* Monto
gen tmc = h9f1 if a8==1
destring tmc, replace

* Personas que reciben transferencias monetarias condicionadas
bys idh_ch: egen ing_ptmc = sum(tmc)
gen percibe_ptmc=(a8==1)
bys idh_ch: egen ptmc_ch=max(percibe_ptmc)
replace ptmc_ch  = ((percibe_ptmc==1)| (ing_ptmc>0 & ing_ptmc!=.))
replace ing_ptmc=. if y_hog==.

* Personas que perciben pensiones
bys idh_ch: egen ing_pension = sum(h9e1)
gen pnc_ci=(h9e==1)
replace pnc_ci = ((pnc_ci==1)|(ing_pension>0 & ing_pension!=.))
replace ing_pension=. if y_hog==.

* Adultos mayores
gen mayor64_ci=(edad>64 & edad!=.)
bys idh_ch: egen mayor64_ch=max(mayor64_ci)

*ingreso neto del hogar
gen y_pc     = y_hog / nmiembros_ch 
gen y_pc_net = (y_hog - ing_ptmc - ing_pension) / nmiembros_ch

lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

lab def pnc_ci 1 "Beneficiario PNC" 0 "No beneficiario PNC"
lab val pnc_ci pnc_ci

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(lugnac>1) if lugnac!=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	/* La encuesta pregunta por la residencia de hace 2 años */
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	/* No se puede diferenciar paises LAC de no LAC */
	
	**********************
	*** migrantiguo5_ci **
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	/* La encuesta pregunta por la residencia de hace 2 años */
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	/* No se puede diferenciar paises LAC de no LAC */


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

*_____________________________________________________________________________________________________*

*  Pobres extremos, pobres moderados, vulnerables y no pobres 
* con base en ingreso neto (Sin transferencias)
* y líneas de pobreza internacionales
gen     grupo_int = 1 if (y_pc_net<lp31_ci)
replace grupo_int = 2 if (y_pc_net>=lp31_ci & y_pc_net<(lp31_ci*1.6))
replace grupo_int = 3 if (y_pc_net>=(lp31_ci*1.6) & y_pc_net<(lp31_ci*4))
replace grupo_int = 4 if (y_pc_net>=(lp31_ci*4) & y_pc_net<.)

tab grupo_int, gen(gpo_ingneto)

* Crear interacción entre recibirla la PTMC y el gpo de ingreso
gen ptmc_ingneto1 = 0
replace ptmc_ingneto1 = 1 if ptmc_ch == 1 & gpo_ingneto1 == 1

gen ptmc_ingneto2 = 0
replace ptmc_ingneto2 = 1 if ptmc_ch == 1 & gpo_ingneto2 == 1

gen ptmc_ingneto3 = 0
replace ptmc_ingneto3 = 1 if ptmc_ch == 1 & gpo_ingneto3 == 1

gen ptmc_ingneto4 = 0
replace ptmc_ingneto4 = 1 if ptmc_ch == 1 & gpo_ingneto4 == 1

lab def grupo_int 1 "Pobre extremo" 2 "Pobre moderado" 3 "Vulnerable" 4 "No pobre"
lab val grupo_int grupo_int


/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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


rename c8 codindustria 
rename c9a codocupa
compress


saveold "`base_out'", replace


log close





