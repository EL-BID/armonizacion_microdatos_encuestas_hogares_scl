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
local ANO "1999"
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

/* revision 12 agosto de 2005
1.Se genero la variable segsoc_ci, estaba como missing pero existen los datos para las cabeceras

CODIGO ANTERIOR: gen segsoc_ci=.
CODIGO NUEVO: incluye  pensiones 

2.comentario agosto 12 de 2005_MFP*
Personas no ocupadas QUE RESPONDEN A LA PREGUNTA DE PENSIONES, quedaron con missing, 
PREGUNTAR Y QUEDA SUJETO A CAMBIO

*revision  Octubre 20 de 2005
Corrección en la construcción de la variable rama_ci. La construcción de esta variable parece responder a 
los codigos CIIU revision 3, pero todas las encuestas anteriores al 2001 se realizaron con la codificación 
CIIU revision 2.

* Revision Marzo 2006 miembros: 
Conteo de miembros en el hogar si relacion_ci [1-4] y dummy de miembro
incluye a los relacion_ci==5. No era homogeneo el criterio, se arregla y se incluye un cambio adicional
los miembros se determinan con parentesco y no relacion para asi poder incluir a los no parientes que hacen 
parte del hogar, sin incluir a los hijos del servicio domestico.

codigo anterior:
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

gen miembro_ci=(relacion_ci<6)
label variable miembro_ci "Miembro del hogar"
PILAS: No se cambiaron las otras variables de composicion del hogar

May 19, 2006 (Analia)
The following command:
gen pared_ch=(ca_2i_T01~="9")
replace pared_ch=0 if ca_2i_T01=="8"
replace pared_ch=0 if ca_2i_T01=="7"
was replaced with
destring ca_2i_T01, replace
gen pared_ch=(ca_2i_T01<=3 | ca_2i_T01==5)
9 is not a category and there are no observations with 8!!!==> Everything was being categorized as permanent.
*Febrero 6 2007
Spublico solo esta disponible para urbano asi que se genera missing y se crea una variable 
spublico1_ci que solo tiene valores para urbano
*/

***************
***region_c ***
***************
gen region_c=real(ca_7_T10_2)
label define region_c       /// 
	5  "Antioquia"	        ///
	8  "Atlántico"	        ///
	11 "Bogotá, D.C"	    ///
	13 "Bolívar" 	        ///
	15 "Boyacá"	            ///
	17 "Caldas"	            ///
	18 "Caquetá"	        ///
	19 "Cauca"	            ///
	20 "Cesár"	            ///
	23 "Córdoba"	        ///
	25 "Cundinamarca"       ///
	27 "Chocó"	            ///
	41 "Huila"	            ///
	44 "La Guajira"	        ///
	47 "Magdalena"	        ///
	50 "Meta"	            ///
	52 "Nariño"	            ///
	54 "Norte de Santander"	///
	63 "Quindío"	        ///
	66 "Risaralda"	        ///
	68 "Santander"	        ///
	70 "Sucre"	            ///
	73 "Tolima"	            ///
	76 "Valle"              ///
	81 "Arauca"             /// 
	85 "Casanare"           ///
	86 "Putumayo"           ///
	88 "S-Andres-Pr"        ///
	91 "Amazonas"           ///
	94 "Guainia"            ///
	95 "Guaviare"           ///
	97 "Vaupes"             ///
	99 "Vichada"     
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
***factor_ch***
***************

gen factor_ch=real(FEXDANE)
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
label value zona_c zona_ci

************
****pais****
************
gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=1999
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
gen paren=real(ca_3_T10)
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

gen factor_ci=real(FEXDANE)
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=.
replace sexo_ci=1 if ca_4_T10=="1"
replace sexo_ci=2 if ca_4_T10=="2"

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=real(ca_5_T10)
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
replace civil_ci=1 if ca_6_T10=="5"
replace civil_ci=2 if ca_6_T10=="1" | ca_6_T10=="2" 
replace civil_ci=3 if ca_6_T10=="4"
replace civil_ci=4 if ca_6_T10=="3" 

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
* 2014, 01, MLO modificado segun documento metodologico
*by idh_ch, sort: egen nmiembros_ch=sum(paren>=1 & paren<=16)
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************
* 2014, 01, MLO modificado segun documento metodologico
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
*gen miembros_ci=(paren>=1 & paren<=16)
label variable miembros_ci "Miembro del hogar"


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
gen salmm_ci= 	236460.00 /*salario diario *30*/
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if  ca_13_T50 == 1 | ca_14_T501==1 | ca_14_T502==1 | ca_15_T50==1 | (ca_16_T50>=1 & ca_16_T50<=7)
replace condocup_ci=2 if (condocup_ci!=1)& ((ca_17_T50==1) | (ca_18_T50==1 & ca_19_T50 <=4 & ca_19_T50!=.))
recode condocup_ci .=3 
replace condocup_ci=4 if edad_ci<10
*/

* Modificacion MGD 06/25/2014: correccion de la edad minima de la encuesta y reducciond e condicionalidades en condocup=2
destring  ca_13_T50 ca_14_T501 ca_14_T502 ca_15_T50 ca_18_T50 ca_19_T50  ca_16_T50 ca_17_T50, replace
gen condocup_ci=.
replace condocup_ci=1 if  ca_13_T50 == 1 | ca_14_T501==1 | ca_14_T502==1 | ca_15_T50==1 
replace condocup_ci=2 if (condocup_ci!=1)& ( ca_13_T50 == 2 | ca_17_T50==1 | (ca_18_T50==1 & ca_19_T50>=1 & ca_19_T50<=4 ) )
recode condocup_ci .=3 if  edad>=12
recode condocup_ci .=4 if edad_ci<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
destring ca_35_T601, replace
gen afiliado_ci=.
replace afiliado_ci=1 if ca_35_T601 ==1 
recode afiliado_ci .=0 if condocup_ci==1 | condocup_ci==2 
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
destring ca_41_T70 , replace
gen cesante_ci=1 if ca_41_T70==2 & condocup_ci==2
replace cesante_ci=0 if condocup_ci==2 & cesante_ci!=1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
destring ca_20_T504 ca_33_T603 ca_46_T704, replace
*MGD 11/30 2015: falto recodificar los missings
recode ca_20_T504 (98=.) (99=.)
recode ca_33_T603 (98=.) (99=.)
recode ca_46_T704 (98=.) (99=.)

egen aux_p=rsum(ca_20_T504 ca_33_T603 ca_46_T704), m
gen pension_ci=1 if aux_p>0 & aux_p<.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

*************
*ypen_ci*
*************
gen ypen_ci=aux_p if aux_p>0 & aux_p<.
replace ypen_ci= . if aux_p==9999999999
replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension contributiva"

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

*************
***formal_ci***
*************
gen formal_ci=(afiliado_ci==1)


/************
***emp_ci***
************
/* personas de 10 o mas*/
gen trab=(ca_13_T50=="1") /*ultima semana trabajando */
replace trab=1 if (ca_13_T50>="2" & ca_13_T50<="8") & (ca_14_T501=="1" | ca_14_T502=="1"| ca_15_T50=="1")

*antes incluia ca_16_T50=="." no se por que 

replace trab=2 if (ca_13_T50>="2" & ca_13_T50<="8") & (ca_17_T50=="1"|ca_18_T50=="1")

gen byte emp_ci=(trab==1)
/* Es empleado: en la ultima semana*/

****************
***desemp1_ci***
****************

gen desemp1_ci=.
/* la definicion incluye desempeados de solo una semana*/

gen desemp1u_ci=(ca_13_T50>="2" & ca_13_T50<="8" & ca_17_T50=="1")

****************
***desemp2_ci*** 
****************

gen desemp2_ci=.

****************
***desemp3_ci***
****************
gen desemp3_ci=(desemp1u_ci==1 |ca_18_T50=="1")

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

gen pea2_ci=.

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
gen tothoras=real(ca_22_T60)
replace subemp_ci=1 if tothoras<=30  & emp_ci==1 & ca_24_T60=="1"


*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras<30 & emp_ci==1 & ca_24_T60=="2"

*****************
***horaspri_ci***
*****************

gen horaspri_ci=.

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras  if emp_ci==1 

******************
***categopri_ci***
******************
gen categ=real(ca_29_T60)
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

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.

*****************
***firmapeq_ci***
*****************

*gen firmapeq_ci=.
/*NA*/


*****************
***spublico_ci***
*****************

gen spublico_ci=(categ==3)
*gen spublico1_ci=(categ==3)


**************
***ocupa_ci***
**************
/*
gen ocupa=ca_26_T60
replace ocupa="." if ca_26_T60=="00"

gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=1 if ocupa_ci==0 
replace ocupa_ci=7 if ocupa_ci==8|ocupa_ci==9
replace ocupa_ci=9 if ca_26_T60=="99"
*no existe clasificacion para FF AA*
drop ocupa
*/
replace ca_26_T60="9" if ca_26_T60=="9*" | ca_26_T60=="9." | ca_26_T60=="6*"
destring ca_26_T60, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (ca_26_T60>=1 & ca_26_T60<=19)  & emp_ci==1  
replace ocupa_ci=2 if (ca_26_T60>=20 & ca_26_T60<=21) & emp_ci==1
replace ocupa_ci=3 if (ca_26_T60>=30 & ca_26_T60<=39) & emp_ci==1
replace ocupa_ci=4 if (ca_26_T60>=40 & ca_26_T60<=49) & emp_ci==1
replace ocupa_ci=5 if (ca_26_T60>=50 & ca_26_T60<=59) & emp_ci==1
replace ocupa_ci=6 if (ca_26_T60>=60 & ca_26_T60<=64) & emp_ci==1
replace ocupa_ci=7 if (ca_26_T60>=70 & ca_26_T60<=98) & emp_ci==1
replace ocupa_ci=9 if (ca_26_T60==0 |  ca_26_T60==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

*************
***rama_ci***
*************
gen rama1=real(ca_28_T60)
gen rama_ci=.
replace rama_ci=1 if rama1>=11 & rama1<=13  
replace rama_ci=2 if rama1>=21 & rama1<=29 
replace rama_ci=3 if rama1>=31 & rama1<=39 
replace rama_ci=4 if rama1>=41 & rama1<=42  
replace rama_ci=5 if rama1==50 
replace rama_ci=6 if rama1>=61 & rama1<=63  
replace rama_ci=7 if rama1>=71 & rama1<=72  
replace rama_ci=8 if rama1>=81 & rama1<=83 
replace rama_ci=9 if rama1>=91 & rama1<=96
replace rama_ci=. if rama1==99
drop rama1

****************
***durades_ci***
****************

gen durades_ci=real(ca_36_T70)/4.3
replace durades_ci=. if ca_36_T70=="99"
/* esta truncada en 98*/

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

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

/*Remuneracion a los empleos*/
gen yprid=real(ca_30_T601) if emp_ci==1 &  ca_30_T602=="1"
replace yprid=real(ca_30_T601)*2 if emp_ci==1  & ca_30_T602=="2"
replace yprid=real(ca_30_T601)*3 if emp_ci==1  & ca_30_T602=="3"
replace yprid=real(ca_30_T601)*4 if emp_ci==1  & ca_30_T602=="4"
replace yprid=real(ca_30_T601)*22 if emp_ci==1 & ca_30_T602=="5"
replace yprid=. if emp_ci==1 & ca_29_T60=="4" & ca_30_T602=="9"

replace yprid=. if yprid==99 /*no informa*/
gen nosabe_is1=(ca_30_T601=="99")
/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid=0 if emp_ci==1 & ca_29_T60=="1"

/*Ganancia los trabajos- solo tiene valores para ca_29_T60==5 y 6 */
gen yprid1=real(ca_32_T60) if emp_ci==1 & (ca_29_T60=="5"|ca_29_T60=="6")

replace yprid1=. if yprid1==99 /*no informa*/
gen nosabe_ig1=(ca_32_T60=="99")

*para inactivos y desempleados VER COUNTRY SPECIFIC CODE BOOK
gen yprid2=real(ca_20_T501) 
replace yprid2=. if yprid2==99
gen yprid3=real(ca_46_T701) 
replace yprid3=. if yprid3==99

*todos
egen ylm_ci=rsum(yprid1 yprid yprid2 yprid3)
replace ylm_ci=. if yprid1==. & yprid==. & yprid2==. & yprid3==.


*****************
****nrylm_ci*****
*****************

gen nrylm_ci=(ylm_ci==. & emp_ci==1)

*************
***ylnm_ci***
*************

gen ylnma=real(ca_31_T601) if emp_ci==1 
replace ylnma=. if ylnma==99 
gen nosabe_inma1=(ca_31_T601=="99")
gen ylnmv=real(ca_31_T602) if emp_ci==1 
replace ylnmv=. if ylnmv==99 
gen nosabe_inmv1=(ca_31_T602=="99")
egen ylnm_ci=rsum(ylnma ylnmv), miss

****revisar****

*************
***ynlm_ci***
*************

*inactivos*  
gen intereses=real(ca_20_T502)
gen arriendos=real(ca_20_T503)
gen pensiones=ca_20_T504
gen ayudas=real(ca_20_T505)
gen otras=real(ca_20_T506)
for var arriendos pensiones ayudas intereses otras: replace X=. if X==99

egen temi=rsum(arriendos pensiones ayudas intereses otras )
replace temi=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*ocupados*  
gen intereses=real(ca_33_T601)
gen arriendos=real(ca_33_T602)
gen pensiones=ca_33_T603
gen ayudas=real(ca_33_T604)
gen otras=real(ca_33_T605)
for var arriendos pensiones ayudas intereses otras: replace X=. if X==99

egen temo=rsum(arriendos pensiones ayudas intereses otras )
replace temo=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*desocupados*  
gen intereses=real(ca_46_T702)
gen arriendos=real(ca_46_T703)
gen pensiones=ca_46_T704
gen ayudas=real(ca_46_T705)
gen otras=real(ca_46_T706)
for var arriendos pensiones ayudas intereses otras: replace X=. if X==99
egen temd=rsum(arriendos pensiones ayudas intereses otras )
replace temd=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 
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
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

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


****************************
***VARIABLES DE EDUCACION***
****************************
*Yessenia Loayza (may 2014) -> corrigo variable aed_ci 

*** people who have missings
gen byte yedc=.
gen yedc1=real(ca_12_T101) /*Todos*/
replace yedc=. if yedc1==999

** No education preescolar o jardin o pre-primaria
replace yedc=0 if yedc1<=300

*** primaria 
replace yedc=1 if yedc1==301 
replace yedc=2 if yedc1==302 
replace yedc=3 if yedc1==303 
replace yedc=4 if yedc1==304 
replace yedc=5 if yedc1==305 | yedc1==400

*** secundaria 
replace yedc=6  if yedc1==406 
replace yedc=7  if yedc1==407 
replace yedc=8  if yedc1==408 
replace yedc=9  if yedc1==409 
replace yedc=10 if yedc1==410
replace yedc=11 if yedc1==411 | yedc1==500

*** superior o universitario  *** 
replace yedc=12 if yedc1==501
replace yedc=13 if yedc1==502
replace yedc=14 if yedc1==503
replace yedc=15 if yedc1==504
replace yedc=16 if yedc1==505
replace yedc=17 if yedc1==506
replace yedc=18 if yedc1==507
replace yedc=19 if yedc1==508
replace yedc=20 if yedc1==509

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

gen byte edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=.
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
replace asiste_ci=1 if ca_11_T101=="1"
replace asiste_ci=0 if ca_11_T101=="2"
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
replace edupub_ci=1 if ca_11_T102=="1"
replace edupub_ci=0 if ca_11_T102=="2"
label variable edupub_ci "Asiste a centros publicos"

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(ca_4_T01_=="1" | ca_4_T01_=="2")


****************
**aguadist_ch***
****************

gen aguadist_ch=.

****************
**aguamala_ch***
****************

gen aguamala_ch=(ca_4_T01_~="1")
replace aguamala_ch=0 if ca_4_T01_=="2"

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=(ca_4i_T013=="1")


****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=(ca_5_T01=="1"|ca_5_T01=="3")

****************
****bano_ch*****
****************


gen bano_ch=(ca_2_T01=="1"|ca_2_T01=="2"|ca_2_T01=="4")


****************
****banoex_ch***
****************

gen banoex_ch=.


****************
****des1_ch*****
****************

gen des1_ch=.
replace des1_ch=0 if ca_2_T01=="6" 
replace des1_ch=1 if ca_2_T01=="1"|ca_2_T01=="4"
replace des1_ch=2 if ca_2_T01=="2"
replace des1_ch=3 if ca_2_T01=="3"|ca_2_T01=="5"



****************
****des2_ch*****
****************

gen des2_ch=.

 
****************
****piso_ch*****
****************

gen piso_ch=(ca_3i_T01~="1") 



****************
****pared_ch****
****************

destring ca_2i_T01, replace
gen pared_ch=(ca_2i_T01<=3 | ca_2i_T01==5)

****************
****techo_ch****
****************

gen techo_ch=.

****************
****resid_ch****
****************

gen basura=real(ca_3_T01_)
gen resid_ch=0 if basura==4
replace resid_ch=1 if basura==3 
replace resid_ch=2 if basura==2 | basura==1 
drop basura

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
destring ca_4_T01_ ca_2_T01_ , replace
g       aguamejorada_ch = 1 if (ca_4_T01_ >=1 & ca_4_T01_ <=3) | ca_4_T01_==5  | ca_4_T01_==8
replace aguamejorada_ch = 0 if  ca_4_T01_==4 | (ca_4_T01_ >=6 & ca_4_T01_ <=7) | ca_4_T01_==0
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if ( ca_2_T01_ >=1 &  ca_2_T01_ <=4)
replace banomejorado_ch = 0 if ( ca_2_T01_ >=5 &  ca_2_T01_ <=6)

****************
****dorm_ch*****
****************

gen dorm_ch=.

****************
***cuartos_ch***
****************

gen cuartos_ch=real(ca_1_T01_)
replace cuartos_ch=. if cuartos_ch==99

****************
***cocina_ch****
****************

gen cocina_ch=.


****************
****telef_ch****
****************

gen telef_ch=(ca_4i_T014=="1")

****************
****refrig_ch***
****************


gen refrig_ch=(ca_8_T01_2=="1")

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

gen vivi1_ch=real(ca_1i_T01)
replace vivi1_ch=3 if vivi1_ch>3


****************
****vivi2_ch****
****************

gen vivi2_ch=.


*******************
****viviprop_ch****
*******************

gen viviprop_ch=real(ca_6_T01_)
replace viviprop_ch=0 if viviprop_ch==3 | viviprop_ch==4 
replace viviprop_ch=3 if viviprop_ch==5 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=real(ca_7_T01_)
replace vivialq_ch=. if vivialq_ch<999

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.

gen ylnmotros_ci=.
gen ylmotros_ci=. 
gen  tcylmpri_ch =.
gen  tcylmpri_ci =.


*----------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------*
*Y.L.. Nota-> Termina Armonizacion de base "CABECERA" y Empieza Armonizacion "RESTO"
saveold "`base_out1'", replace
*----------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------*

clear 
use `base_in2', clear

***************
***region_c ***
***************
gen region_c=real(re_7_T10_2)
label define region_c       /// 
	5  "Antioquia"	        ///
	8  "Atlántico"	        ///
	11 "Bogotá, D.C"	    ///
	13 "Bolívar" 	        ///
	15 "Boyacá"	            ///
	17 "Caldas"	            ///
	18 "Caquetá"	        ///
	19 "Cauca"	            ///
	20 "Cesár"	            ///
	23 "Córdoba"	        ///
	25 "Cundinamarca"       ///
	27 "Chocó"	            ///
	41 "Huila"	            ///
	44 "La Guajira"	        ///
	47 "Magdalena"	        ///
	50 "Meta"	            ///
	52 "Nariño"	            ///
	54 "Norte de Santander"	///
	63 "Quindío"	        ///
	66 "Risaralda"	        ///
	68 "Santander"	        ///
	70 "Sucre"	            ///
	73 "Tolima"	            ///
	76 "Valle"              ///
	81 "Arauca"             /// 
	85 "Casanare"           ///
	86 "Putumayo"           ///
	88 "S-Andres-Pr"        ///
	91 "Amazonas"           ///
	94 "Guainia"            ///
	95 "Guaviare"           ///
	97 "Vaupes"             ///
	99 "Vichada"     
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************
gen factor_ch=real(FEXDANE)
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
label value zona_c zona_ci

************
****pais****
************

gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1999
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
gen paren=real(re_3_T10)
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
gen factor_ci=real(FEXDANE)
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci=.
replace sexo_ci=1 if re_4_T10=="1"
replace sexo_ci=2 if re_4_T10=="2"
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=real(re_5_T10)
label variable edad_ci "Edad del individuo"

*************
** raza_ci **
*************
gen raza_ci=.

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 if re_6_T10=="5"
replace civil_ci=2 if re_6_T10=="1" | re_6_T10=="2" 
replace civil_ci=3 if re_6_T10=="4"
replace civil_ci=4 if re_6_T10=="3" 
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
* 2014, 01, MLO modificado segun documento metodologico
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
*by idh_ch, sort: egen nmiembros_ch=sum(paren>=1 & paren<=6)
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************
* 2014, 01, MLO modificado segun documento metodologico
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
*gen miembros_ci=(paren>=1 & paren<=6)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
*********
*lp25_ci
*********
gen lp25_ci = 60627.96
label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci =97004.74
label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"

*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= . if clase==1 /*cabecera*/
replace lp_ci= . if clase==2  /*resto*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci= . if clase==1 /*cabecera*/
replace lpe_ci= . if clase==2  /*resto*/
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
gen salmm_ci= 	236460.00
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if  re_19_T50 == 1 | re_20_T50==1 | re_22_T50==1 | (re_23_T50>=1 & re_23_T50<=7)
replace condocup_ci=2 if (condocup_ci!=1) & re_24_T50==1
recode condocup_ci .=3 
replace condocup_ci=4 if edad_ci<10
*/

* Modificacion MGD 06/25/2014: agregan alternativas a desocupados
destring  re_19_T50 re_20_T50 re_22_T50 re_23_T50 re_21_T50 re_24_T50 re_25_T50, replace
gen condocup_ci=.
replace condocup_ci=1 if  (re_19_T50 == 1 | re_20_T50==1 | re_21_T50==1 | re_22_T50==1 ) & edad_ci>=12
replace condocup_ci=2 if condocup_ci!=1 & (re_19_T50 == 2 | re_24_T50 ==1 | re_25_T50==3)  & edad_ci>=12 
recode condocup_ci .=3 if  edad>=12
recode condocup_ci .=4 if edad_ci<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
destring re_43_T612 , replace
gen afiliado_ci=.
replace afiliado_ci=1 if re_43_T612 ==1
recode afiliado_ci .=0 if condocup_ci==1 | condocup_ci==2 
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
/*gen cesante_ci=. /*en el formulario rural no se pregunta como en la urbana, no es posible identificarlo*/ 
label var cesante_ci "Desocupado - definicion oficial del pais"	*/

* MGD 12/1/2015: se genera la variable
destring re_50_T70, replace
gen cesante_ci=1 if re_50_T70==1 & condocup_ci==2
replace cesante_ci=0 if condocup_ci==2 & cesante_ci!=1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
destring re_44_T613, replace
recode re_44_T613 (99=.)
gen aux_p=re_44_T613
gen pension_ci=1 if aux_p>0 & aux_p<.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

*************
*ypen_ci*
*************
gen ypen_ci=aux_p if aux_p>0 & aux_p<.
replace ypen_ci= . if aux_p==9999999999
replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension contributiva"

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

*************
***formal_ci***
*************
gen formal_ci=(afiliado_ci==1)

/************
***emp_ci***
************
/* personas de 10 o mas*/
gen trab=(re_19_T50=="1") /*ultima semana trabajando */
replace trab=1 if (re_19_T50>="2" &  re_19_T50<="8") & (re_20_T50=="1" |re_21_T50=="1"| re_22_T50=="1")
replace trab=2 if (re_19_T50>="2" &  re_19_T50<="8" & re_24_T50=="1" )

gen byte emp_ci=(trab==1)
/* Es desempleado: en la ultimo anio*/

****************
***desemp1_ci***
****************

gen desemp1_ci=.
/* la definicion incluye desempeados de solo una semana*/


****************
***desemp2_ci*** 
****************

gen desemp2_ci=.


****************
***desemp3_ci***
****************
gen desemp3_ci=(trab==2)


*************
***pea1_ci***
*************

gen pea1_ci=.

*************
***pea2_ci***
*************

gen pea2_ci=.

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
* No se puede crear para Urbano (re_25_T50=="1")


***************
***subemp_ci***
***************
gen subemp_ci=0
gen tothoras=real(re_40_T61) if emp_ci==1
replace tothoras=. if re_40_T61=="99"
replace subemp_ci=1 if tothoras<=30 & emp_ci==1 & re_41_T61=="1"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras<=30 & emp_ci==1 & re_41_T61=="2"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=.

*****************
***horastot_ci***
*****************
gen horastot_ci=tothoras  if emp_ci==1 


******************
***categopri_ci***
******************
gen categopri_ci=.
gen categ=real(re_29_T60)
replace categopri_ci=1 if categ ==4
replace categopri_ci=2 if categ ==5
replace categopri_ci=3 if categ ==1 | categ ==2 | categ ==3 
replace categopri_ci=4 if categ ==6

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************
gen categosec_ci=.

/* no se puede crear para urbano 
gen categ1=real(re_38_T60)
replace categosec_ci=1 if categ1 ==4
replace categosec_ci=2 if categ1 ==5
replace categosec_ci=3 if categ1 ==1 | categ1 ==2 | categ1 ==3 
replace categosec_ci=4 if categ1 ==6
*/

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


/*****************
***contrato_ci***
*****************
/*no disponible*/
gen contrato_ci=.

***************
***segsoc_ci***
***************
gen segsoc_ci=.
replace segsoc_ci=(re_43_T612=="1") if emp_ci==1
*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.

*****************
***firmapeq_ci***
*****************

*gen firmapeq_ci=.
/*NA*/

*****************
***spublico_ci***
*****************

gen spublico_ci=.

**************
***ocupa_ci***
**************
/*
gen ocupa=re_27_T60
replace ocupa="." if re_27_T60=="00"

gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=1 if ocupa_ci==0
replace ocupa_ci=7 if ocupa_ci==8|ocupa_ci==9
replace ocupa_ci=9 if ocupa=="99"
*no existe clasificacion para FF AA*
drop ocupa
*/
destring re_27_T60, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (re_27_T60>=1 & re_27_T60<=19)  & emp_ci==1  
replace ocupa_ci=2 if (re_27_T60>=20 & re_27_T60<=21) & emp_ci==1
replace ocupa_ci=3 if (re_27_T60>=30 & re_27_T60<=39) & emp_ci==1
replace ocupa_ci=4 if (re_27_T60>=40 & re_27_T60<=49) & emp_ci==1
replace ocupa_ci=5 if (re_27_T60>=50 & re_27_T60<=59) & emp_ci==1
replace ocupa_ci=6 if (re_27_T60>=60 & re_27_T60<=64) & emp_ci==1
replace ocupa_ci=7 if (re_27_T60>=70 & re_27_T60<=98) & emp_ci==1
replace ocupa_ci=9 if (re_27_T60==0 |  re_27_T60==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0


*************
***rama_ci***
*************
gen rama1=real(re_28_T60)
gen rama_ci=.
replace rama_ci=1 if rama1>=11 & rama1<=13  
replace rama_ci=2 if rama1>=21 & rama1<=29 
replace rama_ci=3 if rama1>=31 & rama1<=39 
replace rama_ci=4 if rama1>=41 & rama1<=42  
replace rama_ci=5 if rama1==50 
replace rama_ci=6 if rama1>=61 & rama1<=63  
replace rama_ci=7 if rama1>=71 & rama1<=72  
replace rama_ci=8 if rama1>=81 & rama1<=83 
replace rama_ci=9 if rama1>=91 & rama1<=96
replace rama_ci=. if rama1==99
drop rama1

****************
***durades_ci***
****************

gen durades_ci=real(re_45_T70)/4.3
/*tiene un problema de truncamietno*/ 
replace durades_ci=. if re_45_T70=="99"


*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.



*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.

/*Remuneracion al empleo*/
gen yprid=real(re_31_T601) if emp_ci==1 & re_31_T602=="1"
replace yprid=real(re_31_T601)*4.2 if emp_ci==1 & re_31_T602=="2" 
replace yprid=real(re_31_T601)*22 if emp_ci==1 &  re_31_T602=="3" 
replace yprid=. if emp_ci==1 & re_31_T602=="9" 
replace yprid=. if re_31_T601=="98" /*no sabe*/
replace yprid=. if re_31_T601=="99" /*no informa*/

/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid=0 if emp_ci==1 & re_29_T60=="6"

/*Ganancia al trabajo*/
gen yprid1=real(re_33_T60)/12 if emp_ci==1 
replace yprid1=. if re_33_T60=="98" /*no sabe*/
replace yprid1=. if re_33_T60=="99" /*no informa*/

egen ylmpri=rsum(yprid yprid1)
replace ylmpri=. if yprid==. & yprid1==.
replace ylmpri=. if emp_ci==0


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=.

*****************
*** ylnmpri_ci***
*****************
gen ynlmpri_ci=.

gen ylnm1=real(re_32_T601) if emp_ci==1 
replace ylnm1=. if ylnm1==99 
gen ylnm2=real(re_32_T602) if emp_ci==1 
replace ylnm2=. if ylnm2==99 

egen ylnmpri=rsum(ylnm1 ylnm2)
replace ylnmpri=. if  ylnm1==. & ylnm2==.  
/*Alvaro AM 07-2019, reemplazamos valores extremos 
que elevan ingresos medios y reducen artificialmente
tasas de probeza*/
replace ylnmpri=. if ylnmpri>5000000
drop ylnm1 ylnm2

***************
***ylmsec_ci***
***************

gen ylmsec_ci=.

gen ylmsec=real(re_39_T61) if emp_ci==1 
replace ylmsec=. if ylmsec==98 /*no sabe*/
replace ylmsec=. if ylmsec==99 /*no informa*/

******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.

************
***ylm_ci***
************

*para inactivos y desempleados VER COUNTRY SPECIFIC CODE BOOK
gen yprid2=real(re_53_T701) 
replace yprid2=. if yprid2==99

egen ylm_ci=rsum(ylmpri ylmsec yprid2)
replace ylm_ci=. if ylmpri==. & ylmsec==. & yprid2==. 

*****************
****nrylm_ci*****
*****************

gen nrylm_ci=(ylm_ci==. & emp_ci==1)

*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri ylnmsec), miss
replace ylnm_ci=. if ylnmpri==. & ylnmsec==.

*************
***ynlm_ci***
*************

*inactivosy desocupados*  
gen intereses=real(re_53_T702)
gen arriendos=real(re_53_T703)
gen pensiones=real(re_53_T704)
gen ayudas=real(re_53_T705)
gen otras=real(re_53_T706)
for var arriendos pensiones ayudas intereses otras: replace X=. if X==99

egen temi=rsum(arriendos pensiones ayudas intereses otras )
replace temi=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*ocupados*  
gen intereses=real(re_44_T611)
gen arriendos=real(re_44_T612)
gen pensiones=re_44_T613
gen ayudas=real(re_44_T614)
gen otras=real(re_44_T615)
for var arriendos pensiones ayudas intereses otras: replace X=. if X==99

egen temo=rsum(arriendos pensiones ayudas intereses otras )
replace temo=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*TODOS*
egen ynlm_ci=rsum(temi temo )
replace ynlm_ci=. if temi==. & temo==.

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
*Creating a flag label for those households where someone has a ylmpri_ci as missing

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


****************************
***VARIABLES DE EDUCACION***
****************************
*Yessenia Loayza (may 2014) -> corrigo variable aed_ci 

*** people who have missings
gen byte yedc=.
gen yedc1=real(re_13_T10) /*Todos*/
replace yedc=. if yedc1==999

** No education preescolar o jardin o pre-primaria
replace yedc=0 if yedc1<=300

*** primaria 
replace yedc=1 if yedc1==301 
replace yedc=2 if yedc1==302 
replace yedc=3 if yedc1==303 
replace yedc=4 if yedc1==304 
replace yedc=5 if yedc1==305 | yedc1==400

*** secundaria 
replace yedc=6  if yedc1==406 
replace yedc=7  if yedc1==407 
replace yedc=8  if yedc1==408 
replace yedc=9  if yedc1==409 
replace yedc=10 if yedc1==410
replace yedc=11 if yedc1==411 | yedc1==500

*** superior o universitario  *** 
replace yedc=12 if yedc1==501
replace yedc=13 if yedc1==502
replace yedc=14 if yedc1==503
replace yedc=15 if yedc1==504
replace yedc=16 if yedc1==505
replace yedc=17 if yedc1==506
replace yedc=18 if yedc1==507
replace yedc=19 if yedc1==508
replace yedc=20 if yedc1==509

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

gen byte edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=.
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
replace asiste_ci=1 if re_11_T10=="1"
replace asiste_ci=0 if re_11_T10=="2"
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

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if re_12_T10=="1"
replace edupub_ci=0 if re_12_T10=="2"
label variable edupub_ci "Asiste a centros publicos"


********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(re_4_T01_=="1" | re_4_T01_=="2")


****************
**aguadist_ch***
****************

gen aguadist_ch=.

****************
**aguamala_ch***
****************

gen aguamala_ch=(re_4_T01_~="1")
replace aguamala_ch=0 if re_4_T01_=="2"

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=(re_4i_T013=="1")


****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=(re_5_T01=="1"|re_5_T01=="3")

****************
****bano_ch*****
****************


gen bano_ch=(re_2_T01=="1"|re_2_T01=="2"|re_2_T01=="4")


****************
****banoex_ch***
****************

gen banoex_ch=.


****************
****des1_ch*****
****************

gen des1_ch=.
replace des1_ch=0 if re_2_T01=="6" 
replace des1_ch=1 if re_2_T01=="1"|re_2_T01=="4"
replace des1_ch=2 if re_2_T01=="2"
replace des1_ch=3 if re_2_T01=="3"|re_2_T01=="5"



****************
****des2_ch*****
****************

gen des2_ch=.

 
****************
****piso_ch*****
****************

gen piso_ch=(re_3i_T01~="1") 



****************
****pared_ch****
****************

destring re_2i_T01, replace
gen pared_ch=(re_2i_T01<=3 | re_2i_T01==5)

****************
****techo_ch****
****************

gen techo_ch=.

****************
****resid_ch****
****************

gen basura=real(re_3_T01_)
gen resid_ch=0 if basura==4
replace resid_ch=1 if basura==3 
replace resid_ch=2 if basura==2 | basura==1 
drop basura

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
destring re_4_T01 re_2_T01_ , replace
g       aguamejorada_ch = 1 if (re_4_T01 >=1 & re_4_T01 <=3) | re_4_T01==5  | re_4_T01==8
replace aguamejorada_ch = 0 if re_4_T01==4 | (re_4_T01 >=6 & re_4_T01 <=7) | re_4_T01==0
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if ( re_2_T01_ >=1 &  re_2_T01_ <=4)
replace banomejorado_ch = 0 if ( re_2_T01_ >=5 &  re_2_T01_ <=6)


****************
****dorm_ch*****
****************

gen dorm_ch=.

****************
***cuartos_ch***
****************

gen cuartos_ch=real(re_1_T01_)
replace cuartos_ch=. if cuartos_ch==99

****************
***cocina_ch****
****************

gen cocina_ch=.


****************
****telef_ch****
****************

gen telef_ch=(re_4i_T014=="1")

****************
****refrig_ch***
****************


gen refrig_ch=(re_8_T01_2=="1")

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

gen vivi1_ch=real(re_1i_T01)
replace vivi1_ch=3 if vivi1_ch>3


****************
****vivi2_ch****
****************

gen vivi2_ch=.


*******************
****viviprop_ch****
*******************

gen viviprop_ch=real(re_6_T01_)
replace viviprop_ch=0 if viviprop_ch==3 | viviprop_ch==4 
replace viviprop_ch=3 if viviprop_ch==5 

******************
****vivitit_ch****
******************
gen vivitit_ch=.

*******************
****vivialq_ch****
*******************
gen vivialq_ch=real(re_7_T01_)
replace vivialq_ch=. if vivialq_ch<999

*********************
****vivialqimp_ch****
*********************
gen vivialqimp_ch=.

gen ylnmotros_ci=.
gen ylmotros_ci=. 
gen  tcylmpri_ch =.
gen  tcylmpri_ci =.

append using "`base_out1'"
order re_* ca_*

/*Alvaro AM 07-2019: genero este nuevo id porque el destring no recoge todos los digitos del string
de id para COL-1999, lo que resulta en ids de hogar duplicados y en una subestimaciÃ³n de pobreza
porque se sobreestiman los ingresos familiares. 
Ver: https://www.statalist.org/forums/forum/general-stata-discussion/general/1371814-problem-with-format-of-variable-after-using-destring-stata-13-1-mp-on-windows-7
*/
egen numeric_id = group(idh_ch)
destring idh_ch, replace
replace idh_ch=numeric_id

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


