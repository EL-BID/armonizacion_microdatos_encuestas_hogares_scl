* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys"

local PAIS COL
local ENCUESTA ENH-FT
local ANO "1991"
local ronda m9 
local log_file  = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in1  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'_cabecera.dta"
local base_in2  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'_resto.dta"
local base_out1 = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\areas\\`PAIS'_`ANO'`ronda'_cabeceraBID.dta"
local base_out  = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                      
capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Colombia
Encuesta: ENH-FT
Round: m9
Autores: 
Generación nuevas variables LMK: 
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: noviembre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in1', clear

***************
***region_c ***
***************
gen region_c=.
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************
* Region_BID *
************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************
gen factor_ch=factor_ci
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
***************
gen idh_ch=ca_IDENT
sort idh_ch
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************
bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=1
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************
gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=1991
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=9
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo"
label define mes_c 6 "Junio" 7 " Julio" 8 "Agosto",add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
label value mes_c mes_c

*****************
***relacion_ci***
*****************
*no tengo diccionario de parentesco, se asume que el significado de los codigos es igual al de 1996*
gen paren=real(ca_3_T01)
gen relacion_ci=.
replace relacion_ci=1 if paren==1
replace relacion_ci=2 if paren==2 
replace relacion_ci=3 if paren==3 | paren==4
replace relacion_ci=4 if paren==15
replace relacion_ci=5 if paren==16 | paren==18| paren==19
replace relacion_ci=6 if paren==17


label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=.
replace sexo_ci=1 if ca_4_T01=="1"
replace sexo_ci=2 if ca_4_T01=="2"

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=real(ca_5_T01)
label variable edad_ci "Edad del individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 if ca_6_T01=="5"
replace civil_ci=2 if ca_6_T01=="1" | ca_6_T01=="2" 
replace civil_ci=3 if ca_6_T01=="4"
replace civil_ci=4 if ca_6_T01=="3" 
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

*************
***jefe_ci***
*************
gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"

******************
***nconyuges_ch***
******************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"


****************
***nempdom_ch***
****************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

*****************
***clasehog_ch***
*****************
gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************
* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
*by idh_ch, sort: egen nmiembros_ch=sum(paren>=1 & paren<=16)
label variable nmiembros_ch "Numero de familiares en el hogar"
*todos menos el servicio paren17, sus hijos paren18 y los pensionistas paren19*

****************
***miembros_ci***
****************
* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
*gen miembros_ci=(paren>=1 & paren<=16)
label variable miembros_ci "Miembro del hogar"

*****************
***nmayor21_ch***
*****************
by idh_ch, sort: egen nmayor21_ch=sum((miembros_ci==1) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************
by idh_ch, sort: egen nmenor21_ch=sum((miembros_ci==1) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((miembros_ci==1) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((miembros_ci==1) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((miembros_ci==1) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
gen salmm_ci= 	51716.1
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************


*2014, 01 revision MLO
destring ca_13_T05  ca_13_T05  ca_14_T05 ca_15_T05 ca_17_T05 ca_16_T05 ca_18_T05 ca_19_T05, replace
/*
gen condocup_ci=.
replace condocup_ci=1 if ca_13_T05==1 | ((ca_13_T05!=1) & (ca_14_T05==1 | ca_15_T05==1  |  ca_16_T05 >=1 & ca_16_T05<=7))
replace condocup_ci=2 if (condocup_ci!=1) & (ca_17_T05==1 |ca_18_T05==1 & (ca_19_T05>=1 & ca_19_T05<=4))
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci= . if ca_13_T05==.
/*
replace condocup_ci=1 if ca_13_T05=="1" | ((ca_13_T05>="2" & ca_13_T05<="8") & (ca_14_T05=="1"|ca_15_T05=="1" ))
replace condocup_ci=2 if (ca_13_T05>="2" & ca_13_T05<="8") & (ca_17_T05=="1" |ca_18_T05=="1")
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci= . if ca_13_T05=="."*/

*2014, 01 Modificacion MLO
*replace condocup_ci=4 if edad_ci<10
replace condocup_ci=4 if edad_ci<12
*/

* Modificacion MGD 06/25/2014: correccion de la edad minima de la encuesta y reduccion de condicionalidades en condocup=2 e =1
gen condocup_ci=.
replace condocup_ci=1 if  ca_13_T05 == 1 | ca_14_T05==1 | ca_15_T05==1 
replace condocup_ci=2 if (condocup_ci!=1)& ( ca_13_T05 == 2 | ca_17_T05==1 | (ca_18_T05==1 & ca_19_T05>=1 & ca_19_T05<=4 ) )
recode condocup_ci .=3 if  edad>=12
recode condocup_ci .=4 if edad_ci<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "1 Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
label define instpen_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, Ecopetrol etc)" 4 "Fondo Subsidiado (Prosperar,etc.)" 
label value instpen_ci instpen_ci

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
/*gen cesante_ci=(ca_41_T07=="1")
label var cesante_ci "Desocupado - definicion oficial del pais"	*/

* MGD 12/1/2015: se genera la variable
destring ca_41_T07, replace
gen cesante_ci=1 if ca_41_T07==2 & condocup_ci==2
replace cesante_ci=0 if condocup_ci==2 & cesante_ci!=1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

*************
*ypen_ci    *
*************
gen pension1=real(ca_46_T0708) if ca_46_T0704=="1"
gen pension2=real(ca_20_T0508) if ca_20_T0504=="1"
recode pension1 (999998=.) (999999=.)
recode pension2 (999998=.) (999999=.)
egen aux_p=rsum(pension1 pension2),m
gen ypen_ci=aux_p if aux_p>0 & aux_p<.
replace ypen_ci= . if aux_p==9999999999  | aux_p==999999 
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci=(aux_p>0 & aux_p<.)
label var pension_ci "1=Recibe pension contributiva"
drop pension1 pension2 aux_p


****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"

****************
*categoinac_ci**
***************
gen categoinac_ci=. /*no encuentro diccionario */

************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)

***************
***formal_ci***
***************

* Modificaciones Marcela Rubio: se genera como missing ya que no contamos con variable de afiliado o cotizando
/*
gen formal_ci=(cotizando_ci==1)
*/
gen formal_ci=.

/************
***emp_ci***
************
/* personas de 10 o mas*/
gen trab=(ca_13_T05=="1") /*ultima semana trabajando */
replace trab=1 if (ca_13_T05>="2" & ca_13_T05<="8") & (ca_14_T05=="1"|ca_15_T05=="1" )
gen byte emp_ci=(trab==1)

replace trab=2 if (ca_13_T05>="2" & ca_13_T05<="8") & (ca_17_T05=="1" |ca_18_T05=="1")

****************
***desemp1_ci***
****************

gen desemp1_ci=.
* Solo desempleados de una semana*

gen desemp1u_ci=(ca_13_T05>="2" & ca_13_T05<="8" & ca_17_T05=="1")

****************
***desemp2_ci*** 
****************

gen desemp2_ci=.
* el periodo tambien es una semana solo que se incluye a quienes esperan respuesta de un trabajo*
*para esta encuesta NA*

****************
***desemp3_ci***
****************
gen desemp3_ci=(desemp1u_ci==1 | ca_18_T05=="1") 

*Con esta definicion se obtiene la tasa de desempleo oficial publicada para Colombia
*Se construye desemp1 para hacerla comparable con los demás países*

*************
***pea1_ci***
*************
gen pea1_ci=.

gen pea1u_ci=0
replace pea1u_ci=1 if emp_ci==1 |desemp1u_ci==1

replace desemp1u_ci=. if pea1u_ci==0
*************
***pea2_ci***
*************
*************

gen pea2_ci=.
*no la genero porque no se genera desemp2_ci*

*************
***pea3_ci***
*************
gen pea3_ci=0
replace pea3_ci=1 if emp_ci==1 |desemp3_ci==1
replace desemp3_ci=. if pea3_ci==0
*/

*****************
***desalent_ci***
*****************

gen desalent_ci=.
/*no disponible*/

***************
***subemp_ci***
***************
gen subemp_ci=0
gen tothoras=real(ca_23_T06)
replace  tothoras=. if ca_23_T06=="99"
replace subemp_ci=1 if tothoras<=30  &  emp_ci==1 & ca_24_T06=="1"


*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothora<30 & emp_ci==1 & ca_24_T06=="2"

*****************
***horaspri_ci***
*****************

gen horaspri_ci=.

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras  if emp_ci==1 
drop tothoras

******************
***categopri_ci***
******************
gen categ=real(ca_29_T06)
gen categopri_ci=.

replace categopri_ci=1 if categ ==6
replace categopri_ci=2 if categ ==5
replace categopri_ci=3 if categ ==2 | categ ==3 | categ ==4  
replace categopri_ci=4 if categ ==1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************
/*no se puede*/
gen categosec_ci=.

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

/*****************
***contrato_ci***
*****************
/*no disponible*/
gen contrato_ci=.
gen contrato1_ci=(ca_21_T06=="1") if emp_ci==1
*contrato1_ci, como no existe la variable que indica si tiene contrato de*
*trabajo, entonces se construye una variable que indica si el trabajo es*
*permanente (1) o temporal (2), se supondria que los permanentes tienen contrato*
*y la mayoria de los temporales NO*

/*Para los asalariados el trabajo es permanente, cuando la relación laboral se deriva de un acuerdo
definitivo para la realización de un trabajo en forma regular a través de un contrato de trabajo con 
duración superior a un año o a término indefinido. 

Para los independientes el trabajo es permanente, cuando este se ejerce en forma regular y continua. 
se entiende por trabajo temporal, aquel que ejerce una persona en forma esporádica o no continua, 
trabajando sólo por  ciertas épocas o períodos o cuando tiene un contrato de trabajo hasta por un (1) año,
aunque tenga renovaciones sucesivas.*/

***************
***segsoc_ci***
***************

gen segsoc_ci=.
/*NA*/
*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
/*NA*/

*****************
***firmapeq_ci***
*****************
*gen firmapeq_ci=.
/*NA*/

*****************
***spublico_ci***
*****************
*Mod. MLO 10/2015
gen spublico_ci=(categ==3)
*gen spublico1_ci=(categ==3)

**************
***ocupa_ci***
**************
/*
gen ocupa=ca_26_T06
replace ocupa="." if ca_26_T06=="00"
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=1 if ocupa_ci==0 
replace ocupa_ci=7 if ocupa_ci==8|ocupa_ci==9
replace ocupa_ci=9 if ca_26_T06=="99"
*no existe clasificacion para FF AA*
drop ocupa
*/
/* segun diccionario 1995
INCODIFICABLE		00
PROFESIONAL-TECN1	01:09 (1)
PROFECIONAL-TECN2	11:19 (1)
DIRECTORES FUNCI 	20:21 (2)
PERSONAL ADTIVO  	30:39 (3)
CIANTES VENDEDO1 	40:45 (4)
CIANTES-VENDEDO2	49    (4)
TRABAJ SERVICIOS 	50:59 (5)
AGRIC FORESTALES 	60:64 (6)
NO AGRICOLAS     	70:98 (7)
NO CLASIFICADOS  	99
*/
replace ca_26_T06="." if ca_26_T06=="00"

destring ca_26_T06, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (ca_26_T06>=1 & ca_26_T06<=19)  & emp_ci==1  
replace ocupa_ci=2 if (ca_26_T06>=20 & ca_26_T06<=21) & emp_ci==1
replace ocupa_ci=3 if (ca_26_T06>=30 & ca_26_T06<=39) & emp_ci==1
replace ocupa_ci=4 if (ca_26_T06>=40 & ca_26_T06<=49) & emp_ci==1
replace ocupa_ci=5 if (ca_26_T06>=50 & ca_26_T06<=59) & emp_ci==1
replace ocupa_ci=6 if (ca_26_T06>=60 & ca_26_T06<=64) & emp_ci==1
replace ocupa_ci=7 if (ca_26_T06>=70 & ca_26_T06<=98) & emp_ci==1
replace ocupa_ci=9 if (ca_26_T06==0 |  ca_26_T06==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

*************
***rama_ci***
*************
gen rama1=real(ca_28_T06)
replace rama1=. if rama1==99 
gen rama_ci=.
*esta encuesta se realiza con el CIIU Rev 2 (NO 3!!!)*
replace rama_ci=1 if rama1>=11 & rama1<=13  
replace rama_ci=2 if rama1>=21 & rama1<=29 
replace rama_ci=3 if rama1>=31 & rama1<=39 
replace rama_ci=4 if rama1>=41 & rama1<=42  
replace rama_ci=5 if rama1>=50 & rama1<=59
replace rama_ci=6 if rama1>=61 & rama1<=63  
replace rama_ci=7 if rama1>=71 & rama1<=72  
replace rama_ci=8 if rama1>=81 & rama1<=83 
replace rama_ci=9 if rama1>=91 & rama1<=96
replace rama_ci=. if rama1==99
drop rama1

****************
***durades_ci***
****************

gen durades_ci=real(ca_36_T07)/4.3
replace durades_ci=. if ca_36_T07=="99"
/* está truncada en 98*/
*ca_36_T07 en semanas*
* durades debe estar expresado en meses*

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.


***********************************************
***** 	     	INGRESOS	       ********
***********************************************

****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=.

*****************
*** ylnmpri_ci***
*****************

gen ylnmpri_ci=.

***************
***ylmsec_ci***
***************

gen ylmsec_ci=.

******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.

************
***ylm_ci***
************

/*Remuneracion a los empleados ca_29_T06==2,3,4*/
gen yprid=real(ca_30_T06) if emp_ci==1 & (ca_29_T06=="2"|ca_29_T06=="3"|ca_29_T06=="4") & ca_31_T06=="1"
replace yprid=real(ca_30_T06)*2 if emp_ci==1 & (ca_29_T06=="2"|ca_29_T06=="3"|ca_29_T06=="4") & ca_31_T06=="2"
replace yprid=real(ca_30_T06)*3 if emp_ci==1 & (ca_29_T06=="2"|ca_29_T06=="3"|ca_29_T06=="4") & ca_31_T06=="3"
replace yprid=real(ca_30_T06)*4.2 if emp_ci==1 & (ca_29_T06=="2"|ca_29_T06=="3"|ca_29_T06=="4") & ca_31_T06=="4"
replace yprid=real(ca_30_T06)*22 if emp_ci==1 & (ca_29_T06=="2"|ca_29_T06=="3"|ca_29_T06=="4")  & ca_31_T06=="5"
replace yprid=. if emp_ci==1 & (ca_29_T06=="2"|ca_29_T06=="3"|ca_29_T06=="4") & ca_31_T06=="9"
replace yprid=. if (ca_30_T06=="999999" | ca_30_T06=="999998")

/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid=0 if emp_ci==1 & ca_29_T06=="1"

/*Ganancia los trabajos para patrones y cuenta propia*/
gen yprid1=real(ca_33_T06) if emp_ci==1 & (ca_29_T06=="5"| ca_29_T06=="6")
replace yprid1=. if (ca_33_T06=="999999" | ca_33_T06=="999998")
*/999998 no sabe, 999999 no informa*/

*para inactivos y desempleados
gen yprid2=real(ca_20_T0508) if ca_20_T0501=="1" & ca_20_T0502=="" & ca_20_T0503==""  & ca_20_T0504=="" & ca_20_T0505=="" & ca_20_T0506=="" & ca_20_T0507==""
replace yprid2=. if (ca_20_T0508=="999999" | ca_20_T0508=="999998")

gen yprid3=real(ca_46_T0708) if ca_46_T0701=="1" & ca_46_T0702=="" & ca_46_T0703==""  & ca_46_T0704=="" & ca_46_T0705=="" & ca_46_T0706=="" & ca_46_T0707==""
replace yprid3=. if (ca_46_T0708=="999999" | ca_46_T0708=="999998")

egen ylm_ci=rsum(yprid1 yprid yprid2 yprid3)
replace ylm_ci=. if yprid1==. & yprid==. & yprid2==. & yprid3==.



*****************
****nrylm_ci*****
*****************

gen nrylm_ci=(ylm_ci==. & emp_ci==1)

*como no se puede identificar los ingresos de la actividad principal porque*
*no estan discriminados de esa manera, se creará una variable similar para *
*identificar la no respuesta para ingresos laborales de todos los trabajos*


*************
***ylnm_ci***
*************

gen ylnm_ci=.

/*
gen ylnm=real(ca_32_T0602) if emp_ci==1 & ca_32_T0601=="1"
replace ylnm=. if (ca_32_T0602=="999999" | ca_32_T0602=="999998")
999998 no sabe, 999999 no informa*/
* solo existe para urbano por eso no se genera 

*************
***ynlm_ci***
*************
*inactivos*
gen temi=real(ca_20_T0508)
replace temi=. if ca_20_T0501=="1" & ca_20_T0502=="" & ca_20_T0503==""  & ca_20_T0504=="" & ca_20_T0505=="" & ca_20_T0506=="" & ca_20_T0507==""
replace temi=. if (ca_20_T0508=="999999" | ca_20_T0508=="999998")

*ocupados*
gen temo=real(ca_34_T0602) if emp_ci==1 & ca_34_T0601=="1"
replace temo=. if (ca_34_T0602=="999999" | ca_34_T0602=="999998")

*desocupados*
gen temd=real(ca_46_T0708)
replace temd=. if ca_46_T0701=="1" & ca_46_T0702=="" & ca_46_T0703==""  & ca_46_T0704=="" & ca_46_T0705=="" & ca_46_T0706=="" & ca_46_T0707==""
replace temd=. if (ca_46_T0708=="999999" | ca_46_T0708=="999998")

egen ynlm_ci=rsum(temi temo temd)
replace ynlm_ci=. if temi==. & temo==. & temd==.

*****************
***remesas_ci***
*****************

gen remesas_ci=.
gen ynlnm_ci=.

************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************
*Creating a Flag label for those households where someone has a *
*ylmpri_ci as missing*

gen nrylmpri_ch=.

******************
***  nrylm_ch  ***
******************
*esta variable no se encuentra en el listado la creo para generar el ingreso*
*laboral del hogar que no incluye las no respuestas, este ingreso es la*
*alternativa al construido con ylmpri_ci*

by idh_ch, sort: egen nrylm_ch=sum(nrylm_ci) if miembros_ci==1
replace nrylm_ch=1 if nrylm_ch>0 & nrylm_ch<.
replace nrylm_ch=. if nrylm_ch==.

*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylm_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1

**********************************
*** remesas_ch & remesasnm_ch ***
**********************************

gen remesash=.

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash)
replace remesas_ch=. if remesasi==. 

gen remesasnm_ch=.


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm_ch=rsum(ynlm remesash)
replace ynlm_ch=. if ynlm==. 
drop ynlm

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=remesasnm_ch

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=.

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

**************************
  *** tcylm_ci ***
**************************
 
 ** TOP CODE **
 
 gen tcylm_ci= .
 replace tcylm_ci= 0 if ylm_ci>=0 & ylm_ci<=999997
 replace tcylm_ci= 1 if ylm_ci==999998
 
 
****************************
***VARIABLES DE EDUCACION***
****************************
/*
En Colombia, la educación primaria dura 5 años y la educación secundaria *
*6 años y universitaria hasta 5-7 años, dependiendo la carrera*/ 

*** Missings
gen byte yedc=.
gen yedc1=real(ca_11_T01) 
replace yedc=. if yedc1==99

** No educacion, preescolar, jardin o pre-primaria
replace yedc=0 if (yedc1==10 | yedc1==20) 
/*10 ninguno, 20 es primaria y ningun anio terminado*/

*** primaria 
replace yedc=1 if yedc1==21
replace yedc=2 if yedc1==22
replace yedc=3 if yedc1==23 
replace yedc=4 if yedc1==24 
replace yedc=5 if (yedc1==25 | yedc1==30) 

*** secundaria 
replace yedc=6 if yedc1==31 
replace yedc=7 if yedc1==32 
replace yedc=8 if yedc1==33 
replace yedc=9 if yedc1==34 
replace yedc=10 if yedc1==35 
replace yedc=11 if (yedc1==36 | yedc1==37| yedc1==40) 
* hay una persona en esta categoria yedc1==37*

*** superior o universitario  *** 
replace yedc=12 if yedc1==41 
replace yedc=13 if yedc1==42 
replace yedc=14 if yedc1==43
replace yedc=15 if yedc1==44 
replace yedc=16 if yedc1==45 
replace yedc=17 if yedc1==46
replace yedc=18 if yedc1==47

gen byte aedu_ci=yedc

**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<5
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==5
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>5 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

** SE DEFINE PRIMER CICLO DE SECUNDARIA LOS PRIMEROS 4 DE 6 ANIOS **

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>5 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo" 

** SE DEFINE SEGUNDO CICLO DE SECUNDARIA LOS ULTIMOS 2 DE 6 ANIOS **

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<11
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==11
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo" 

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<16
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=16
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if ca_10_T01=="1"
replace asiste_ci=0 if ca_10_T01=="2"
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=.

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

***************
***repite_ci***
***************

gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(ca_5_T00_=="1" | ca_5_T00_=="2")


****************
**aguadist_ch***
****************

gen aguadist_ch=.

****************
**aguamala_ch***
****************

gen aguamala_ch=.
replace aguamala_ch=(ca_5_T00_ !="1" & ca_5_T00_ !="2")

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=( ca_5_T003 =="1")
*1 si la vivienda tiene conexion a energia electrica*
*ver pregunta 5 del datos de la vivienda*


****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=.

****************
****bano_ch*****
****************

gen bano_ch=.
replace bano_ch=(ca_3_T00_=="1"| ca_3_T00_=="2"| ca_3_T00_=="3")


****************
****banoex_ch***
****************

gen banoex_ch=.
replace banoex_ch=(ca_4_T00_=="1")


****************
****des1_ch*****
****************

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing
gen des1_ch=.
replace des1_ch = 0 if bano_ch==0
replace des1_ch = 1 if ca_3_T00_=="1" | ca_3_T00_=="2"
replace des1_ch = 2 if ca_3_T00_=="3"


****************
****des2_ch*****
****************

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing
gen des2_ch=.
replace des2_ch = 0 if bano_ch==0
replace des2_ch = 1 if ca_3_T00_=="1" | ca_3_T00_=="2" | ca_3_T00_=="3"


****************
****piso_ch*****
****************

gen piso_ch=.
replace piso_ch=(ca_4_T00~="5") 

****************
****pared_ch****
****************

*Para unificar la creación de esta variable se cambia la programación
*gen pared_ch=.
*replace pared_ch=(ca_3_T00!="7" & ca_3_T00!="5")

gen pared_ch=.
replace pared_ch=(ca_3_T00=="1" | ca_3_T00=="2" | ca_3_T00=="3" | ca_3_T00=="6")

*Bahareque: Pared de palos entretejidos con cañas y barro*
*no permanente: guadua, caña, tela y desechos*
 
****************
****techo_ch****
****************

gen techo_ch=.

****************
****resid_ch****
****************

gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
destring ca_5_T00_ ca_3_T00_ ca_4_T00_, replace
	
g       aguamejorada_ch = 1 if (ca_5_T00_ >=1 & ca_5_T00_ <=2) | (ca_5_T00_ >=4 & ca_5_T00_ <=5) | ca_5_T00_==7
replace aguamejorada_ch = 0 if  ca_5_T00_ ==3 | ca_5_T00_==6 | ca_5_T00_==8
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if   (ca_3_T00_ >=1 & ca_3_T00_ <=3) & ca_4_T00_ ==1
replace banomejorado_ch = 0 if  ((ca_3_T00_ >=1 & ca_3_T00_ <=3) & ca_4_T00_ ==2) | ca_3_T00_ ==4

****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=real(ca_2_T00_)

****************
***cuartos_ch***
****************

gen cuartos_ch=.
replace cuartos_ch=real(ca_1_T00_)

****************
***cocina_ch****
****************

gen cocina_ch=.
replace cocina_ch=(ca_6_T00_=="3")


****************
****telef_ch****
****************

gen telef_ch=(ca_5_T004=="1")

****************
****refrig_ch***
****************


gen refrig_ch=.

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch****
****************

gen auto_ch=.

****************
****compu_ch****
****************

gen compu_ch=.

****************
**internet_ch***
****************

gen internet_ch=.

****************
****cel_ch******
****************

gen cel_ch=.

****************
****vivi1_ch****
****************

gen vivi1_ch=.
replace vivi1_ch=real(ca_1_T00)
replace vivi1_ch=3 if vivi1_ch>2 & vivi1_ch!=.


****************
****vivi2_ch****
****************

gen vivi2_ch=.
replace vivi2_ch=(vivi1_ch==1 |vivi1_ch==2)


*******************
****viviprop_ch****
*******************

gen viviprop_ch=real(ca_8_T00_)
replace viviprop_ch=0 if viviprop_ch==3 | viviprop_ch==4 
replace viviprop_ch=3 if viviprop_ch==5 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=real(ca_9_T00_)
replace vivialq_ch=. if vivialq_ch>=999998 

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.

gen ylnmotros_ci=.
gen ylmotros_ci=. 
gen  tcylmpri_ch =.

*---------------------------------------------------------------------------*
*---------------------------------------------------------------------------*
*Y.L.. Nota-> Termina Armonizacion de base "CABECERA" y Empieza Armonizacion "RESTO"
saveold "`base_out1'", replace
*---------------------------------------------------------------------------*
*---------------------------------------------------------------------------*

clear 
use `base_in2', clear

***************
***region_c ***
***************
gen region_c=.
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************
* Region_BID *
************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************

gen factor_ch=factor_ci
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************
gen idh_ch=re_IDENT
sort idh_ch
label variable idh_ch "ID del hogar"


**************
****idp_ci****
**************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1991
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=9
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo"
label define mes_c 6 "Junio" 7 " Julio" 8 "Agosto",add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
label value mes_c mes_c

*****************
***relacion_ci***
*****************
*no tengo diccionario de parentesco, se asume que el significado de los codigos es igual al de 1996*
gen paren=real(re_3_T02)
gen relacion_ci=.
replace relacion_ci=1 if paren==1
replace relacion_ci=2 if paren==2 
replace relacion_ci=3 if paren==3 | paren==4
replace relacion_ci=4 if paren==5
replace relacion_ci=5 if paren==6 | paren==7 | paren==8 | paren==10 
replace relacion_ci=6 if paren==9


label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci=.
replace sexo_ci=1 if re_4_T02=="1"
replace sexo_ci=2 if re_4_T02=="2"

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=real(re_5_T02)
label variable edad_ci "Edad del individuo"

*************
** raza_ci **
*************
gen raza_ci=.

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 if re_6_T02=="5"
replace civil_ci=2 if re_6_T02=="1" | re_6_T01=="3" 
replace civil_ci=3 if re_6_T02=="2"
replace civil_ci=4 if re_6_T02=="4" 
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


*************
***jefe_ci***
*************
gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"


******************
***nconyuges_ch***
******************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"


****************
***nempdom_ch***
****************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

*****************
***clasehog_ch***
*****************
gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************
* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
*by idh_ch, sort: egen nmiembros_ch=sum(paren>=1 & paren<=6)
label variable nmiembros_ch "Numero de familiares en el hogar"
*todos menos pensionistas paren7, trabajadores paren8, servicio paren9 y sus hijos paren10 *


****************
***miembros_ci***
****************
* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
*gen miembros_ci=(paren>=1 & paren<=6)
label variable miembros_ci "Miembro del hogar"


*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum(miembros_ci==1 & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum(miembros_ci==1 & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum(miembros_ci==1 & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum(miembros_ci==1 & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum(miembros_ci==1 & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"



************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

*********
*lp25_ci
*********
gen lp25_ci =13939.47
label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = 22303.15
label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
gen salmm_ci=51720.00
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************

*2014, 01 revision MLO
destring re_19_T03 re_20_T03  re_21_T03 re_22_T03   re_22_T03 re_23_T03 re_24_T03 re_25_T03, replace
/*
gen condocup_ci=.
replace condocup_ci=1 if re_19_T03 == 1 | (re_19_T03 == 1 & (re_20_T03 == 1 |  re_21_T03==1 |  re_22_T03==1 | (re_23_T03>=1 & re_23_T03<=5)))
replace condocup_ci=2 if condocup_ci!=1 & (re_24_T03==1)
recode condocup_ci .=3
/*
destring re_19_T03     re_20_T03  re_21_T03 re_22_T03   re_22_T03, replace
replace condocup_ci=1 if re_19_T03 == 1 | (re_19_T03 == 1 & (  re_20_T03 == 1 |  re_21_T03==1 |  re_22_T03==1))
replace condocup_ci=2 if re_19_T03==2
recode condocup_ci .=3 if (re_19_T03==3 |  re_19_T03==5|  re_19_T03==6)  */
*2014, 01 Modificacion MLO
*replace condocup_ci=4 if edad_ci<10
replace condocup_ci=4 if edad_ci<12
*/
* Modificacion MGD 06/25/2014: agregan alternativas a desocupados y edad minima 12
gen condocup_ci=.
replace condocup_ci=1 if  (re_19_T03 == 1 | re_20_T03==1 | re_21_T03==1 | re_22_T03==1 ) & edad_ci>=12
replace condocup_ci=2 if condocup_ci!=1 & (re_19_T03 == 2 | re_24_T03 ==1 | re_25_T03==3)  & edad_ci>=12 
recode condocup_ci .=3 if  edad>=12
recode condocup_ci .=4 if edad_ci<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "1 Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
label define instpen_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, Ecopetrol etc)" 4 "Fondo Subsidiado (Prosperar,etc.)" 
label value instpen_ci instpen_ci

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
/*destring re_50_T06, replace
gen cesante_ci= (re_50_T06==1)
label var cesante_ci "Desocupado - definicion oficial del pais"	*/

* MGD 12/1/2015: se condiciona a que este desocupado
destring re_50_T06, replace
gen cesante_ci=1 if re_50_T06==1 & condocup_ci==2
replace cesante_ci=0 if condocup_ci==2 & cesante_ci!=1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

*************
*ypen_ci    *
*************
gen pension1=real(re_44_T0507) if re_44_T0503=="1"
gen pension2=real(re_56_T0708) if re_56_T0704=="1"
recode pension1 (999998=.) (999999=.)
recode pension2 (999998=.) (999999=.)
egen aux_p=rsum(pension1 pension2)
gen ypen_ci=aux_p if aux_p>0 & aux_p<.
replace ypen_ci= . if aux_p==9999999999  | aux_p==99
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci=(aux_p>0 & aux_p<.)
label var pension_ci "1=Recibe pension contributiva"
drop pension1 pension2 aux_p


****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 


***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"

****************
*categoinac_ci**
***************
gen categoinac_ci=. /*no encuentro diccionario */

************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)

***************
***formal_ci***
***************
gen formal_ci=(cotizando_ci==1)

/************
***emp_ci***
************
/* personas de 10 o mas*/
gen trab=(re_19_T03=="1") 
/*ultima semana trabajando */

replace trab=1 if (re_19_T03>="2" & re_19_T03<="8") & (re_20_T03=="1"|re_21_T03=="1"|re_22_T03=="1")
*ocupados: trabajo al menos 1 hora remunerada en la semana de referencia de la encuesta, no trabajo 
*pero tenia trabajo, trab familiar sin remuneración*
gen byte emp_ci=(trab==1)

****************
***desemp1_ci***
****************

gen desemp1_ci=.
* Solo desempleados de una semana

* Para Cabecera existe una pregunta adicional: hizo alguna diligencia para conseguir trabajo em la última 
*semana?, esta pregunta NO existe para RURAL (re_19_T03=="2") 

****************
***desemp2_ci*** 
****************

gen desemp2_ci=.
* el periodo tambien es una semana solo que se incluye a quienes esperan respuesta de un trabajo*

****************
***desemp3_ci***
****************
gen desemp3_ci=(re_19_T03>="2" & re_19_T03<="8" & re_24_T03=="1")

*re_24_T03=="1" es si ha buscado trabajo en el último anio*

*************
***pea1_ci***
*************
gen pea1_ci=.

*************
***pea2_ci***
*************
gen pea2_ci=.
*no la genero porque no se genera desemp2_ci*

*************
***pea3_ci***
*************
gen pea3_ci=0
replace pea3_ci=1 if emp_ci==1 |desemp3_ci==1

replace desemp3_ci=. if pea3_ci==0
*/

*****************
***desalent_ci***
*****************
gen desalent_ci=.
/*no disponible*/

***************
***subemp_ci***
***************

gen subemp_ci=0
gen tothoras=real(re_40_T05) if emp_ci==1
replace tothoras=. if re_40_T05=="99"

replace subemp_ci=1 if tothoras<=30 & re_41_T05=="1"
replace subemp_ci=. if emp_ci!=1

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras<=30 & re_41_T05=="2"
replace tiempoparc_ci=. if emp_ci!=1

*****************
***horaspri_ci***
*****************

gen horaspri_ci=.

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras if emp_ci==1 

******************
***categopri_ci***
******************

gen categopri_ci=.

gen categ=real(re_29_T04)
replace categopri_ci=1 if categ==4
replace categopri_ci=2 if categ==5
replace categopri_ci=3 if categ==2 | categ==3 | categ==1 
replace categopri_ci=4 if categ==6

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************

gen categosec_ci=.

*no se puede crear para urbano
*gen categ=real(re_38_T04)
*replace categosec_ci=1 if categ ==4
*replace categosec_ci=2 if categ ==5
*replace categosec_ci=3 if categ ==1 | categ==2 | categ==3
*replace categosec_ci=4 if categ ==6


label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

/*****************
***contrato_ci***
*****************

gen contrato_ci=.
/*no disponible*/

***************
***segsoc_ci***
***************

gen segsoc_ci=.
/*NA*/
*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.

*****************
***firmapeq_ci***
*****************

*gen byte firmapeq_ci=.
/*NA*/

*****************
***spublico_ci***
*****************
gen spublico_ci=.

**************
***ocupa_ci***
**************
/*
gen ocupa=re_27_T04
replace ocupa="." if re_27_T04=="00"

gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=1 if ocupa_ci==0 
replace ocupa_ci=7 if ocupa_ci==8|ocupa_ci==9
replace ocupa_ci=9 if ocupa=="99"
*no existe clasificacion para FF AA*
drop ocupa
*/
/* segun diccionario 1995
INCODIFICABLE		00
PROFESIONAL-TECN1	01:09 (1)
PROFECIONAL-TECN2	11:19 (1)
DIRECTORES FUNCI 	20:21 (2)
PERSONAL ADTIVO  	30:39 (3)
CIANTES VENDEDO1 	40:45 (4)
CIANTES-VENDEDO2	49    (4)
TRABAJ SERVICIOS 	50:59 (5)
AGRIC FORESTALES 	60:64 (6)
NO AGRICOLAS     	70:98 (7)
no existe 		      (8)
NO CLASIFICADOS  	99    (9)
*/
replace re_27_T04="." if re_27_T04=="00"

destring re_27_T04, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (re_27_T04>=1 & re_27_T04<=19)  & emp_ci==1  
replace ocupa_ci=2 if (re_27_T04>=20 & re_27_T04<=21) & emp_ci==1
replace ocupa_ci=3 if (re_27_T04>=30 & re_27_T04<=39) & emp_ci==1
replace ocupa_ci=4 if (re_27_T04>=40 & re_27_T04<=49) & emp_ci==1
replace ocupa_ci=5 if (re_27_T04>=50 & re_27_T04<=59) & emp_ci==1
replace ocupa_ci=6 if (re_27_T04>=60 & re_27_T04<=64) & emp_ci==1
replace ocupa_ci=7 if (re_27_T04>=70 & re_27_T04<=98) & emp_ci==1
replace ocupa_ci=9 if (re_27_T04==0 |  re_27_T04==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

*************
***rama_ci***
*************
gen rama1=real(re_28_T04)
replace rama1=. if rama1==99 
gen rama_ci=.
*esta encuesta se realiza con el CIIU Rev 2 (NO 3!!!)*
replace rama_ci=1 if rama1>=11 & rama1<=13  
replace rama_ci=2 if rama1>=21 & rama1<=29 
replace rama_ci=3 if rama1>=31 & rama1<=39 
replace rama_ci=4 if rama1>=41 & rama1<=42  
replace rama_ci=5 if rama1>=50 & rama1<=59
replace rama_ci=6 if rama1>=61 & rama1<=63  
replace rama_ci=7 if rama1>=71 & rama1<=72  
replace rama_ci=8 if rama1>=81 & rama1<=83 
replace rama_ci=9 if rama1>=91 & rama1<=96
replace rama_ci=. if rama1==99
drop rama1

****************
***durades_ci***
****************

gen durades_ci=real(re_45_T06)/4.3
replace durades_ci=. if re_45_T06=="99"
/* está truncada en 98*/

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.


***********************************************
***** 	     	INGRESOS	       ********
***********************************************

****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.

/*Remuneracion a los empleados CATEGOPRI=1,2,3
No impongo la condicion porque el resto son missing y no es necesario*/

gen yprid=real(re_31_T0401) if emp_ci==1  & re_31_T0402=="1"
replace yprid=real(re_31_T0401)*4.2 if emp_ci==1  & re_31_T0402=="2"
replace yprid=real(re_31_T0401)*22 if emp_ci==1 & re_31_T0402=="3"
replace yprid=. if emp_ci==1 & re_31_T0402=="4"
replace yprid=. if (re_31_T0401=="999999" | re_31_T0401=="999998")

/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid=0 if emp_ci==1 & categopri_ci==4

/*Ganancia los trabajos para patrones y cuenta propia 
 es para resto 91-99 ANUAL*/
gen yprid1=real(re_33_T04) if emp_ci==1 
replace yprid1=. if (re_33_T04=="99999999" | re_33_T04=="99999998")
replace yprid1=yprid1/12 if emp_ci==1
*/99999998 no sabe, 99999999 no informa*/

egen ylmpri=rsum(yprid1 yprid)
replace ylmpri=. if yprid1==. & yprid==.

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=.

*****************
*** ylnmpri_ci***
*****************

gen ylnmpri_ci=.

***************
***ylmsec_ci***
***************

gen ylmsec_ci=.

gen ylmsec=real(re_39_T04)
replace ylmsec=. if re_39_T04=="9999999" |re_39_T04=="9999998" 

******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.
 
************
***ylm_ci***
************
*para inactivos y desempleados VER COUNTRY SPECIFIC CODE BOOK
gen yprid2=real(re_56_T0708) if re_56_T0701=="1" & re_56_T0702==""  & re_56_T0703=="" & re_56_T0704=="" & re_56_T0705=="" & re_56_T0706=="" & re_56_T0707=="" 
replace yprid2=. if (re_56_T0708=="999999" | re_56_T0708=="999998")

egen ylm_ci=rsum(ylmpri ylmsec yprid2)
replace ylm_ci=. if ylmpri==. & ylmsec==. & yprid2==. 

*****************
***nrylm_ci***
*****************

gen nrylm_ci=(ylm_ci==. & emp_ci==1)

*************
***ylnm_ci***
*************

gen ylnm_ci=.


*************
***ynlm_ci***
*************
*se incluyen intereses, arriendos, pensiones, ayudas, etc*
* y para desocupados e inactivos se excluye el rubro trabajo:REVISAR*

*inactivos y desocupados*
gen temi=real(re_56_T0708)
replace temi=. if re_56_T0701=="1" & re_56_T0702==""  & re_56_T0703=="" & re_56_T0704=="" & re_56_T0705=="" & re_56_T0706=="" & re_56_T0707=="" 
replace temi=. if (re_56_T0708=="999999" | re_56_T0708=="999998")
*ocupados*
gen temo=real(re_44_T0507) if emp_ci==1 
replace temo=. if (re_44_T0507=="999999" | re_44_T0507=="999998")
replace temo=. if emp_ci==0 

egen ynlm_ci=rsum(temi temo)
replace ynlm_ci=. if temo==. & temi==.


*****************
***remesas_ci***
*****************

gen remesas_ci=.
gen ynlnm_ci=.

************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************
*Creating a Flag label for those households where someone has a *
*ylmpri_ci as missing*

gen nrylmpri_ch=.

******************
***  nrylm_ch  ***
******************
*esta variable no se encuentra en el listado la creo para generar el ingreso*
*laboral del hogar que no incluye las no respuestas, este ingreso es la*
*alternativa al construido con ylmpri_ci*

by idh_ch, sort: egen nrylm_ch=sum(nrylm_ci) if miembros_ci==1
replace nrylm_ch=1 if nrylm_ch>0 & nrylm_ch<.
replace nrylm_ch=. if nrylm_ch==.

*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylm_ch==1


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1

**********************************
*** remesas_ch & remesasnm_ch ***
**********************************

gen remesash=.

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash)
replace remesas_ch=. if remesasi==. 

gen remesasnm_ch=.


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm_ch=rsum(ynlm remesash)
replace ynlm_ch=. if ynlm==. 
drop ynlm

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=remesasnm_ch

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=.

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=.


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

**************************
  *** tcylmpri_ci ***
**************************
 
 ** TOP CODE **
 
 gen tcylmpri_ci= .
 replace tcylmpri_ci= 0 if ylmpri_ci>=0 & ylmpri_ci<=999997
 replace tcylmpri_ci= 1 if ylmpri_ci==999998
 
 
****************************
***VARIABLES DE EDUCACION***
****************************
/*
En Colombia, la educación primaria dura 5 años y la educación secundaria *
*6 años y universitaria hasta 5-7 años, dependiendo la carrera*/ 

*** Missings
gen byte yedc=.
gen yedc1=real(re_10_T02) 
replace yedc=. if yedc1==99

** No educacion, preescolar, jardin o pre-primaria
replace yedc=0 if (yedc1==10 | yedc1==20) 
/*10 ninguno, 20 es primaria y ningun anio terminado*/

*** primaria 
replace yedc=1 if yedc1==21
replace yedc=2 if yedc1==22
replace yedc=3 if yedc1==23 
replace yedc=4 if yedc1==24 
replace yedc=5 if (yedc1==25 | yedc1==30) 

*** secundaria 
replace yedc=6 if yedc1==31 
replace yedc=7 if yedc1==32 
replace yedc=8 if yedc1==33 
replace yedc=9 if yedc1==34 
replace yedc=10 if yedc1==35 
replace yedc=11 if (yedc1==36 | yedc1==40) 

*** superior o universitario  *** 
replace yedc=12 if yedc1==41 
replace yedc=13 if yedc1==42 
replace yedc=14 if yedc1==43
replace yedc=15 if yedc1==44 
replace yedc=16 if yedc1==45 
replace yedc=17 if yedc1==46
replace yedc=18 if yedc1==47

gen byte aedu_ci=yedc

**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<5
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==5
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>5 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

** SE DEFINE PRIMER CICLO DE SECUNDARIA LOS PRIMEROS 4 DE 6 ANIOS **

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>5 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo" 

** SE DEFINE SEGUNDO CICLO DE SECUNDARIA LOS ULTIMOS 2 DE 6 ANIOS **

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<11
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==11
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo" 

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<16
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=16
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if re_9_T02=="1"
replace asiste_ci=0 if re_9_T02=="2"
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=.

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

***************
***repite_ci***
***************

gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(re_5_T01_=="1" | re_5_T01_=="2")


****************
**aguadist_ch***
****************

gen aguadist_ch=.

****************
**aguamala_ch***
****************

gen aguamala_ch=.
replace aguamala_ch=(re_5_T01_ !="1" & re_5_T01_ !="2")

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=(re_11_T01_ =="1")
*en cabecera se construyo como=1 si la vivienda tiene conexion a energia electrica*
*por falta de informacion , en rural si existe la pregunta*


****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=.
replace combust_ch=(re_10_T011=="1"|re_10_T011=="7") 

****************
****bano_ch*****
****************

gen bano_ch=.
replace bano_ch=(re_3_T01_=="1"| re_3_T01_=="2"| re_3_T01_=="3")


****************
****banoex_ch***
****************

gen banoex_ch=.
replace banoex_ch=(re_4_T01_=="1")


****************
****des1_ch*****
****************

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing
gen des1_ch=.
replace des1_ch = 0 if bano_ch==0
replace des1_ch = 1 if re_3_T01_=="1" | re_3_T01_=="2"
replace des1_ch = 2 if re_3_T01_=="3"


****************
****des2_ch*****
****************

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing
gen des2_ch=.
replace des2_ch = 0 if bano_ch==0
replace des2_ch = 1 if re_3_T01_=="1" | re_3_T01_=="2" | re_3_T01_=="3"


****************
****piso_ch*****
****************

gen piso_ch=.
replace piso_ch=(re_4i_T01~="5") 
replace piso_ch=. if re_4i_T01==""

****************
****pared_ch****
****************
*Para unificar la creación de esta variable se cambia la programación

*gen pared_ch=.
*replace pared_ch=(re_3i_T01!="7" & re_3i_T01!="5")
*replace pared_ch=. if re_3i_T01==""


gen pared_ch=.
replace pared_ch=(re_3i_T01=="1" | re_3i_T01=="2" | re_3i_T01=="3" | re_3i_T01=="6")


*Bahareque: Pared de palos entretejidos con cañas y barro*
*no permanente: guadua o caña y tela /desechos*
 
****************
****techo_ch****
****************

gen techo_ch=.

****************
****resid_ch****
****************

gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
destring re_5_T01_  re_3_T01_  re_4_T01_ , replace
	
g       aguamejorada_ch = 1 if (re_5_T01_  >=1 & re_5_T01_  <=2) | (re_5_T01_   >=4 & re_5_T01_  <=5) | re_5_T01_  ==7
replace aguamejorada_ch = 0 if  re_5_T01_  ==3 | re_5_T01_  ==6 | re_5_T01_  ==8
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if   (re_3_T01_  >=1 & re_3_T01_  <=3) & re_4_T01_  ==1
replace banomejorado_ch = 0 if  ((re_3_T01_  >=1 & re_3_T01_  <=3) & re_4_T01_  ==2) | re_3_T01_  ==4

****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=real(re_2_T01_)

****************
***cuartos_ch***
****************

gen cuartos_ch=.
replace cuartos_ch=real(re_1_T01_)

****************
***cocina_ch****
****************

gen cocina_ch=.
replace cocina_ch=(re_6_T01_=="2")


****************
****telef_ch****
****************

gen telef_ch=(re_5i_T014=="1")

****************
****refrig_ch***
****************
gen refrig_ch=.

****************
****freez_ch****
****************
gen freez_ch=.


****************
****auto_ch****
****************
gen auto_ch=.

****************
****compu_ch****
****************
gen compu_ch=.

****************
**internet_ch***
****************
gen internet_ch=.

****************
****cel_ch******
****************
gen cel_ch=.

****************
****vivi1_ch****
****************
gen vivi1_ch=.
replace vivi1_ch=real(re_1i_T01)
replace vivi1_ch=3 if vivi1_ch>2 & vivi1_ch<=8


****************
****vivi2_ch****
****************
gen vivi2_ch=.
replace vivi2_ch=(vivi1_ch==1 |vivi1_ch==2)


*******************
****viviprop_ch****
*******************
gen viviprop_ch=real(re_8_T01_)
replace viviprop_ch=0 if viviprop_ch==3 | viviprop_ch==4 
replace viviprop_ch=3 if viviprop_ch==5 

******************
****vivitit_ch****
******************
gen vivitit_ch=.

*******************
****vivialq_ch****
*******************
gen vivialq_ch=real(re_9_T01_)
replace vivialq_ch=. if vivialq_ch>=999998 

*********************
****vivialqimp_ch****
*********************
gen vivialqimp_ch=.

gen ylnmotros_ci=.
gen ylmotros_ci=. 
gen  tcylmpri_ch =.


append using "`base_out1'"
order re_* ca_*

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


