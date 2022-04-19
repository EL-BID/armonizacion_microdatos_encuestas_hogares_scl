* (Versión Stata 13)
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

local PAIS COL
local ENCUESTA ENH-FT
local ANO "2000"
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

/*revision  Octubre 20 de 2005
Corrección en la construcción de la variable rama_ci. La cosntruccion de esta variable parece responder a 
los codigos CIIU revision 3, pero todas las encuestas anteriores al 2001 se realizaron con la codificación 
CIIU revision 2.

CODIGO ANTERIOR:
gen rama1=real(ca_32_T60)
gen rama_ci=.
replace rama_ci=1 if rama1>=1 & rama1<=9  
replace rama_ci=2 if rama1>=10 & rama1<=14  
replace rama_ci=3 if rama1>=15 & rama1<=37  
replace rama_ci=4 if rama1>=40 & rama1<=41  
replace rama_ci=5 if rama1>=45
replace rama_ci=6 if rama1>=50 & rama1<=55  
replace rama_ci=7 if rama1>=60 & rama1<=64  
replace rama_ci=8 if rama1>=65 & rama1<=74  
replace rama_ci=9 if rama1>=75 & rama1<=99  
drop rama1

HORAS
Había un problema en la generacion de las horas totales, la variable era missing, si alguna de las 
variables que la componen estaba como missing
CODIGO ANTERIOR:
gen tothoras=promhora+promhora1+promhora2

May 22, 2006 (Analia)
The following command:
gen pared_ch=(ca_2i_T01~="9")
replace pared_ch=0 if ca_2i_T01=="8"
replace pared_ch=0 if ca_2i_T01=="7"
was replaced with
destring ca_2i_T01, replace
gen pared_ch=(ca_2i_T01<=3 | ca_2i_T01==5)
9 is not a category and there are no observations with 8!!!==> Everything was being categorized as permanent.
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

gen anio_c=2000
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
replace relacion_ci=5 if paren==16 | paren==18| paren==19 | paren==20| paren==21
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
label variable nmiembros_ch "Numero de miembros del hogar"

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
*gen miembros_ci=(paren>=1 & paren<=16)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

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
* COL 2000
gen salmm_ci= 	260100.00 /* salario horario *30*/
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************
destring  ca_16_T50 ca_17_T50 ca_18_T50 ca_19_T50  ca_16_T50 ca_17_T50 ca_18_T50  ca_19_T50  ca_20_T502 ca_20_T503 ca_20_T504  ca_20_T505 ca_21_T50 ca_22_T50 ca_23_T501 ca_28_T50, replace

* Modificacion MGD 06/25/2014: se corrigio la edad minima de la encuesta que es 12 años.
gen condocup_ci=.
replace condocup_ci=1 if  ca_16_T50 == 1 | ca_17_T50==1 | ca_18_T50==1 | ca_19_T50==1
replace condocup_ci=2 if condocup_ci!=1 & (ca_16_T50==2 | ca_22_T50==1 | ca_23_T501==1 | ca_23_T501==3) & ca_28_T50==1
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2 & edad>=12
replace condocup_ci=4 if edad_ci<12

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "InacTivo" 4 "Menor de PET" 
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
label var cotizando_ci "1 CoTizanTe a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*insTpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "InsTiTucion proveedora de la pension - variable original de cada pais" 
label define instpen_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, EcopeTrol eTc)" 4 "Fondo Subsidiado (Prosperar,eTc.)" 
label value instpen_ci insTpen_ci

*****************
*Tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de conTraTo segun su duracion"
label define tipocontrato_ci 1 "PermanenTe/indefinido" 2 "Temporal" 3 "Sin conTraTo/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
destring ca_60_T70, replace
gen cesante_ci=1 if ca_60_T70==2 & condocup_ci==2
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
destring ca_38_T602 ca_65_T703 ca_72_T803, replace
*MGD 11/30 2015: falto recodificar los missings
recode ca_38_T602 (98=.) (99=.)
recode ca_65_T703 (98=.) (99=.)
recode ca_72_T803 (98=.) (99=.)

egen aux_p=rsum(ca_38_T602 ca_65_T703 ca_72_T803), m
gen pension_ci=1 if aux_p>0 & aux_p<.
recode pension_ci .=0
label var pension_ci "1=Recibe pension conTribuTiva"

****************
*Tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

*************
*ypen_ci*
*************
gen ypen_ci=aux_p if aux_p>0 & aux_p<.
replace ypen_ci= . if aux_p==9999999999
replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension conTribuTiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no conTribuTiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no conTribuTiva"

*************
*tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "1=formacion Terciaria Tecnica"

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

* Modificaciones Marcela Rubio - Noviembre 2014: se genera como missing ya que la variable afiliado o cotizando han sido generadas como missing
/*
gen formal_ci=(cotizando_ci==1)
*/

gen formal_ci = .

/************
***emp_ci***
************

gen trab=(ca_16_T50=="1") /*ultima semana trabajando */
replace trab=1 if (ca_16_T50>="2" & ca_16_T50<="7") & (ca_17_T50=="1" |ca_18_T50=="1" |ca_20_T501=="2" )

gen byte emp_ci=(trab==1)
/* Es empleado: en la ultima semana*/

gen desemp_abierto=1 if ca_22_T50=="1" & ca_28_T50=="1"
gen desemp_oculto=1 if  (ca_23_T501>="02" & ca_23_T501<="10") & ca_26_T50=="1"
gen desemp_DANE=1 if  desemp_abierto==1 | desemp_oculto==1 | ca_28_T50 =="1"

****************
***desemp1_ci***
****************

gen desemp1_ci=.
*la definicion incluye desempleados de solo una semana
*se podria intentar generar una proxy, con la definición de desempleo abierto 

****************
***desemp2_ci*** 
****************

gen desemp2_ci=.

****************
***desemp3_ci***
****************
gen desemp3_ci=(ca_22_T50=="1" | ca_25_T50=="1" |ca_26_T50=="1")

* ca_22_T50 diligencias en el ultimo mes
* ca_25_T50 diligencias despues del ultimo empleo
* ca_26_T50 diligencias en los ultimos 12 meses

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
*/

*****************
***desalent_ci***
*****************
gen desalent_ci=((ca_23_T501>=4 & ca_23_T501<=11)|ca_23_T501==2)
*Ver Country Specific Code Book: COLOMBIA *

***************
***subemp_ci***
***************
gen subemp_ci=0

*horas en el trabajo principal*
/*todas las horas estan truncadas en 120- 98*/
gen promhora=real(ca_40_T60) if emp_ci==1 
replace promhora=120 if ca_40_T60=="998"
replace promhora=. if ca_40_T60=="999"

gen hora_no=real(ca_41_T602) if ca_41_T601=="1" & emp_ci==1 
replace hora_no=120 if ca_41_T602=="998"
replace hora_no=. if ca_41_T602=="999"

gen hora_ad=real(ca_42_T602) if ca_42_T601=="1" & emp_ci==1 
replace hora_ad=98 if ca_42_T602=="998"
replace hora_ad=. if ca_42_T602=="999"

*horas en la actividad secundaria*
gen promhora1=real(ca_47_T611) if emp_ci==1 
replace promhora1=98 if ca_47_T611=="98" /*acotado*/
replace promhora1=. if ca_47_T611=="99"

*Horas en otras actividades*
gen promhora2=real(ca_47_T614) if emp_ci==1 
replace promhora2=98 if ca_47_T614=="98" /*acotado*/
replace promhora2=. if ca_47_T614=="99"

*horas adicionales (disponibles) que podría trabajara a la semana*
gen promhora3=real(ca_53_T61) if emp_ci==1 
replace promhora3=97 if ca_53_T61=="98" /*acotado*/
replace promhora3=. if ca_53_T61=="99"

egen tothoras=rowtotal(promhora promhora1 promhora2)
replace tothoras=. if promhora==. & promhora1==. & promhora2==.
replace tothoras=. if tothoras>=168
replace subemp_ci=1 if tothoras<=30  & emp_ci==1 & ca_52_T61=="1"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras<30 & emp_ci==1 & ca_52_T61=="2"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=promhora

*****************
***horastot_ci***
*****************
gen horastot_ci=tothoras  if emp_ci==1 

******************
***categopri_ci***
******************
gen categ=real(ca_33_T60)
gen categopri_ci=.
replace categopri_ci=1 if categ ==5
replace categopri_ci=2 if categ ==4
replace categopri_ci=3 if categ ==1 | categ ==2 | categ ==3 
replace categopri_ci=4 if categ ==6
replace categopri_ci=0 if categ ==7
*categ ==7 es "otros"*

label define categopri_ci 0 "Otros"  1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************
******************
gen categs=real(ca_44_T61)
gen categosec_ci=.

* Modificacion MLO: abr, 2015.

replace categosec_ci=1 if categs ==3 
replace categosec_ci=2 if categs ==2 
replace categosec_ci=3 if categs ==1  
replace categosec_ci=4 if categs ==4
/*
replace categosec_ci=1 if categs ==5
replace categosec_ci=2 if categs ==4
replace categosec_ci=3 if categs ==1 | categs ==2 | categs ==3  
replace categosec_ci=4 if categs ==6
replace categosec_ci=0 if categs ==7*/

label define categosec_ci 0 "Otros" 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"
drop categs


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if ca_43_T61=="2"
replace nempleos_ci=2 if ca_43_T61=="1"

*****************
***firmapeq_ci***
*****************
*gen firmapeq_ci=.


*****************
***spublico_ci***
*****************
gen spublico_ci=(categ==2)


**************
***ocupa_ci***
**************
/*
gen ocupa=ca_30_T60
replace ocupa="." if ca_30_T60=="00"
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=1 if ocupa_ci==0 
replace ocupa_ci=7 if ocupa_ci==8 | ocupa_ci==9
replace ocupa_ci=9 if ocupa=="99"
drop ocupa
*/
destring ca_30_T60, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (ca_30_T60>=1 & ca_30_T60<=19)  & emp_ci==1  
replace ocupa_ci=2 if (ca_30_T60>=20 & ca_30_T60<=21) & emp_ci==1
replace ocupa_ci=3 if (ca_30_T60>=30 & ca_30_T60<=39) & emp_ci==1
replace ocupa_ci=4 if (ca_30_T60>=40 & ca_30_T60<=49) & emp_ci==1
replace ocupa_ci=5 if (ca_30_T60>=50 & ca_30_T60<=59) & emp_ci==1
replace ocupa_ci=6 if (ca_30_T60>=60 & ca_30_T60<=64) & emp_ci==1
replace ocupa_ci=7 if (ca_30_T60>=70 & ca_30_T60<=98) & emp_ci==1
replace ocupa_ci=9 if (ca_30_T60==0 |  ca_30_T60==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

*************
***rama_ci***
*************
* CIIU rev.2
gen rama1=real(ca_32_T60)
gen rama_ci=.
replace rama_ci=1 if rama1>=11 & rama1<=13  
replace rama_ci=2 if rama1>=21 & rama1<=29 
replace rama_ci=3 if rama1>=31 & rama1<=39 
replace rama_ci=4 if rama1>=41 & rama1<=42  
replace rama_ci=5 if rama1==50 
replace rama_ci=6 if rama1>=61 & rama1<=63  
replace rama_ci=7 if rama1>=71 & rama1<=72  
replace rama_ci=8 if rama1>=81 & rama1<=83 
replace rama_ci=9 if rama1>=91 & rama1<=98
replace rama_ci=. if rama1==99
drop rama1

****************
***durades_ci***
****************
gen durades_ci=real(ca_56_T70)/4.3
*replace durades_ci=61.9 if ca_56_T70=="998"
/*está truncada en 260 semanas---> 61.9 meses*/
replace durades_ci=. if ca_56_T70=="999" | ca_56_T70=="998"

*ca_56_T70 es en semanas*
*durades_ci es en meses*

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

/*Remuneracion al empleo para dependientes*/
gen yprid=real(ca_34_T60) if emp_ci==1 & (ca_33_T60>="1" & ca_33_T60<="3")

replace yprid=. if yprid==98 /*no sabe*/
replace yprid=. if yprid==99 /*no informa*/
gen nosabe_is1=(ca_34_T60=="98")
gen nosabe_is2=(ca_34_T60=="99")
gen nosabe_is3=(ca_34_T60~=".")
/*Ganancia al trabajo: cuenta propia, patrones, otros*/
gen yprid1=real(ca_37_T60) if emp_ci==1 & (ca_33_T60=="4" | ca_33_T60=="5"| ca_33_T60=="7")

replace yprid1=. if yprid1==98 /*no sabe*/
replace yprid1=. if yprid1==99 /*no informa*/
gen nosabe_ig1=(ca_37_T60=="98")
gen nosabe_ig2=(ca_37_T60=="99")
gen nosabe_ig3=(ca_37_T60~=".")

/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid1=0 if emp_ci==1 & ca_33_T60=="6"

egen ylmpri_ci=rsum(yprid yprid1)
replace ylmpri_ci=. if yprid==. & yprid1==.
replace ylmpri_ci=0 if categopri_ci==4

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

*****************
*** ylnmpri_ci***
*****************

gen ylnmpri1=real(ca_35_T602) if emp_ci==1 & ca_35_T601=="1"
replace ylnmpri1=. if ylnmpri1==98
gen ylnmpri2=real(ca_36_T602) if emp_ci==1 & ca_36_T601=="1"
replace ylnmpri2=. if ylnmpri2==98
egen ylnmpri_ci=rsum(ylnmpri1 ylnmpri2)
replace ylnmpri_ci=. if ylnmpri1==. & ylnmpri2==.  
gen nosabe_inma1=(ca_35_T602=="98")
gen nosabe_inma2=(ca_36_T602=="99")
gen nosabe_inmv1=(ca_35_T602=="98")
gen nosabe_inmv2=(ca_36_T602=="99")

***************
***ylmsec_ci***
***************

gen ylmsec_ci=real(ca_45_T61) if emp_ci==1 
replace ylmsec_ci=. if ylmsec_ci==98 /*no sabe*/
replace ylmsec_ci=. if ylmsec_ci==99 /*no informa*/

gen nosabe_isec1=(ca_45_T61=="98")
gen nosabe_isec2=(ca_45_T61=="99")
gen nosabe_isec3=(ca_45_T61==".")

******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.

************
***ylm_ci***
************

*para inactivos y desempleados VER COUNTRY SPECIFIC CODE BOOK
gen yprid2=real(ca_72_T801) 
replace yprid2=. if yprid2==99
gen yprid3=real(ca_65_T701) 
replace yprid3=. if yprid3==99

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci yprid2 yprid3)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & yprid2==. & yprid3==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.

*************
***ynlm_ci***
*************

*inactivos*  
gen arriendos=real(ca_72_T802)
gen pensiones=ca_72_T803
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(ca_73_T801)/12 if ca_73_T801!="99" & ca_73_T801!="98"
gen intereses=real(ca_73_T801)/12 if ca_73_T801!="99" & ca_73_T801!="98"
gen otras=real(ca_73_T801)/12 if ca_73_T801!="99"& ca_73_T801!="98"

egen temi=rsum(arriendos pensiones ayudas intereses otras )
replace temi=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*ocupados*  

gen arriendos=real(ca_38_T601)
gen pensiones=ca_38_T602
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(ca_39_T601)/12 if ca_39_T601!="99" & ca_39_T601!="98"
gen intereses=real(ca_39_T602)/12  if ca_39_T602!="99" & ca_39_T602!="98"
gen otras=real(ca_39_T603)/12 if ca_39_T603!="99" & ca_39_T603!="98"

egen temo=rsum(arriendos pensiones ayudas intereses otras )
replace temo=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*desocupados*  
gen arriendos=real(ca_65_T702)
gen pensiones=ca_65_T703
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(ca_66_T701)/12 if ca_66_T701!="99" & ca_66_T701!="98"
gen intereses=real(ca_66_T702)/12 if ca_66_T702!="99" & ca_66_T702!="98"
gen otras=real(ca_66_T703)/12 if ca_66_T703!="99"& ca_66_T703!="98"

egen temd=rsum(arriendos pensiones ayudas intereses otras )
replace temd=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*todos*
egen ynlm_ci=rsum(temi temo temd)
replace ynlm_ci=. if temi==. & temo==. & temd==.

*****************
***remesas_ci****
*****************

gen remesas_ci=.

****************
*** ynlnm_ci ***
****************

gen ynlnm_ci=.


************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

******************
*** nrylmpri_ch***
******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

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
***ylmhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)


****************************
***VARIABLES DE EDUCACION***
****************************
*Yessenia Loayza (may 2014) -> corrigo variable aedu_ci 

*** people who have missings
gen byte yedc=.
gen yedc1=real(ca_15_T10) /*Todos*/
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
replace yedc=12 if yedc1==412
replace yedc=13 if yedc1==413

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
replace yedc=21 if yedc1==510
replace yedc=22 if yedc1==511
replace yedc=22 if yedc1==512
replace yedc=23 if yedc1==513
replace yedc=24 if yedc1==514
replace yedc=25 if yedc1==515

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
replace asiste_ci=1 if ca_13_T10=="1"
replace asiste_ci=0 if ca_13_T10=="2"
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
replace edupub_ci=1 if ca_14_T10=="1"
replace edupub_ci=0 if ca_14_T10=="2"
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

gen telef_ch=(ca_8_T01_1=="1")

****************
****refrig_ch***
****************


gen refrig_ch=(ca_8_T01_3=="1")

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

gen  tcylmpri_ci =.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ch=.

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

gen anio_c=2000
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
replace relacion_ci=4 if paren==15 
replace relacion_ci=5 if paren==16 | paren==18| paren==19 | paren==20| paren==21
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
*by idh_ch, sort: egen nmiembros_ch=sum(paren>=1 & paren<=6)
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de miembros del hogar"

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
*gen miembros_ci=(paren>=1 & paren<=6)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

*********
*lp25_ci
*********
gen lp25_ci = 66218.55
label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci =105949.7
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
* COL 2000
gen salmm_ci= 	260100.00
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************
destring  re_16_T50 re_17_T50 re_18_T50 re_19_T50  re_16_T50 re_17_T50 re_18_T50  re_19_T50  re_20_T502 re_20_T503 re_20_T504  re_20_T505 re_21_T50 re_22_T50 re_23_T501 re_28_T50, replace

* Modificacion MGD 06/25/2014: se corrigio la edad minima de la encuesta que es 12 años.
gen condocup_ci=.
replace condocup_ci=1 if  re_16_T50 == 1 | re_17_T50==1 | re_18_T50==1 | re_19_T50==1
replace condocup_ci=2 if condocup_ci!=1 & (re_16_T50==2 | re_22_T50==1 | re_23_T501==1 | re_23_T501==3) & re_28_T50==1
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2 & edad>=12
replace condocup_ci=4 if edad_ci<12

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "InacTivo" 4 "Menor de PET" 
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
destring re_60_T70, replace
gen cesante_ci=1 if re_60_T70==2 & condocup_ci==2
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
destring re_38_T602 re_65_T703 re_72_T803, replace
egen aux_p=rsum(re_38_T602 re_65_T703 re_72_T803), m
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
gen formal_ci=(cotizando_ci==1)


/************
***emp_ci***
************

/* personas de 10 o mas*/
gen trab=(re_16_T50=="1") /*ultima semana trabajando */
replace trab=1 if (re_16_T50>="2" & re_16_T50<="7") & (re_17_T50=="1" | re_18_T50=="1" | re_20_T501=="2" )
*cambio en la encuesta re_16_T50 diferente numeracion, se incrementan el numero de preguntas*
* pero se puede conservar los criterios homogéneos para la creación de las variables

replace trab=2 if re_22_T50=="1" &  re_28_T50=="1"

gen byte emp_ci=(trab==1)
/* Es empleado: en la ultima semana*/

gen desemp_abierto=1 if re_22_T50=="1" & re_28_T50=="1"
gen desemp_oculto=1 if  (re_23_T501>="02" & re_23_T501<="10") & re_26_T50=="1"
gen desemp_DANE=1 if  desemp_abierto==1 | desemp_oculto==1 | re_28_T50 =="1"

****************
***desemp1_ci***
****************

gen desemp1_ci=.
*la definicion incluye desempeados de solo una semana
*se podria intentar generar una proxy, con la definición de desempleo abierto 

****************
***desemp2_ci*** 
****************

gen desemp2_ci=.

****************
***desemp3_ci***
****************
gen desemp3_ci=(re_22_T50=="1" | re_25_T50=="1" |re_26_T50=="1")

* re_22_T50 diligencias en el ultimo mes
* re_25_T50 diligencias despues del ultimo empleo
* re_26_T50 diligencias en los ultimos 12 meses

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
*/


*****************
***desalent_ci***
*****************

gen desalent_ci=((re_23_T501>=4 & re_23_T501<=11)|re_23_T501==2)

***************
***subemp_ci***
***************
gen subemp_ci=0

*horas semanales normalmente trabajo principal*
gen promhora=real(re_40_T60) if emp_ci==1 
replace promhora=120 if re_40_T60=="998"
replace promhora=. if re_40_T60=="999"/*esta acotada*/

*horas no trabajadas en la semana pasada*
gen hora_no=real(re_41_T602) if re_41_T601=="1" & emp_ci==1 
replace hora_no=120 if re_41_T602=="998"
replace hora_no=. if re_41_T602=="999"
/*esta acotada*/

*horas adicionales en la semana pasada*
gen hora_ad=real(re_42_T602) if re_42_T601=="1" & emp_ci==1 
replace hora_ad=120 if re_42_T602=="998"
replace hora_ad=. if re_42_T602=="999"

*horas trabajadas en la semana pasada en esa actividad secundaria*
gen promhora1=real(re_47_T611) if emp_ci==1 
replace promhora1=98 if re_47_T611=="98" /*acotado*/
replace promhora1=. if re_47_T611=="99"

*horas trabajadas en la semana pasada en actividades comunitarias y/o voluntarias*
gen promhora2=real(re_47_T614) if emp_ci==1 
replace promhora2=98 if re_47_T614=="98" /*acotado*/
replace promhora2=. if re_47_T614=="99"

*horas adicionales (disponibles) que podría trabajara a la semana*
gen promhora3=real(re_53_T61) if emp_ci==1 
replace promhora3=98 if re_53_T61=="98" /*acotado*/
replace promhora3=. if re_53_T61=="99"
/*dan cosas extrañas xq trabaja el promedio, dejo lo mas cercano al DANE*/

egen tothoras=rowtotal (promhora promhora1 promhora2)
replace tothoras=. if promhora==. & promhora1==. & promhora2==.
replace tothoras=. if tothoras>=168

replace subemp_ci=1 if (tothoras<=30  & emp_ci==1) & re_52_T61=="1"


*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras<=30 & emp_ci==1 & re_52_T61=="2"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=promhora

*****************
***horastot_ci***
*****************
gen horastot_ci=tothoras  if emp_ci==1 

******************
***categopri_ci***
******************
gen categ=real(re_33_T60)
gen categopri_ci=.
replace categopri_ci=1 if categ ==5
replace categopri_ci=2 if categ ==4
replace categopri_ci=3 if categ ==1 | categ ==2 | categ ==3  
replace categopri_ci=4 if categ ==6
replace categopri_ci=0 if categ ==7
*7== otros*
label define categopri_ci 0 "Otros"  1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
gen categs=real(re_44_T61)

* Modificacion MLO: abr, 2015.
gen categosec_ci=.
replace categosec_ci=1 if categs ==3 
replace categosec_ci=2 if categs ==2 
replace categosec_ci=3 if categs ==1  
replace categosec_ci=4 if categs ==4

/*
replace categosec_ci=1 if categs ==5
replace categosec_ci=2 if categs ==4
replace categosec_ci=3 if categs ==1 | categs ==2 | categs ==3 
replace categosec_ci=4 if categs ==6
replace categopri_ci=0 if categs ==7*/

*categs ==7 es "otros"*

label define categosec_ci 0 "Otros" 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"
drop categs


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if re_43_T61=="2"
replace nempleos_ci=2 if re_43_T61=="1"

*****************
***firmapeq_ci***
*****************
*gen firmapeq_ci=.

*****************
***spublico_ci***
*****************
gen spublico_ci=(re_33_T60=="2")

**************
***ocupa_ci***
**************
/*
gen ocupa=re_30_T60
replace ocupa="." if re_30_T60=="00"
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=1 if ocupa_ci==0 
replace ocupa_ci=7 if ocupa_ci==8 | ocupa_ci==9
replace ocupa_ci=9 if ocupa=="99"
drop ocupa
*/
destring re_30_T60, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (re_30_T60>=1 & re_30_T60<=19)  & emp_ci==1  
replace ocupa_ci=2 if (re_30_T60>=20 & re_30_T60<=21) & emp_ci==1
replace ocupa_ci=3 if (re_30_T60>=30 & re_30_T60<=39) & emp_ci==1
replace ocupa_ci=4 if (re_30_T60>=40 & re_30_T60<=49) & emp_ci==1
replace ocupa_ci=5 if (re_30_T60>=50 & re_30_T60<=59) & emp_ci==1
replace ocupa_ci=6 if (re_30_T60>=60 & re_30_T60<=64) & emp_ci==1
replace ocupa_ci=7 if (re_30_T60>=70 & re_30_T60<=98) & emp_ci==1
replace ocupa_ci=9 if (re_30_T60==0 |  re_30_T60==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

*************
***rama_ci***
*************

gen rama1=real(re_32_T60)
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

gen durades_ci=real(re_56_T70)/4.3
*replace durades_ci=61.9 if re_56_T70=="998"
/*está truncada en 260 semanas---> 61.9 meses*/
replace durades_ci=. if re_56_T70=="999" | re_56_T70=="998"

*re_56_T70 es en semanas*
*durades_ci es en meses*
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

/*Remuneracion al empleo*/
gen yprid=real(re_34_T60) if emp_ci==1 & (re_33_T60=="1"| re_33_T60=="2"| re_33_T60=="3")
replace yprid=. if (yprid==98 | yprid==99 )

/*Ganancia al trabajo ANUAL */
gen yprid1=real(re_37_T60)/12 if emp_ci==1 & (re_33_T60=="4"|re_33_T60=="5"| re_33_T60=="7")
replace yprid1=. if (re_37_T60=="98" | re_37_T60=="99")

/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid1=0 if emp_ci==1 & re_33_T60=="6"

/*los ingresos estan en pesos y tiene que se mayor de 100*/
egen ylmpri_ci=rsum(yprid yprid1)
replace ylmpri_ci=. if yprid==. & yprid1==.
replace ylmpri_ci=. if emp_ci==0


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


*****************
*** ylnmpri_ci***
*****************

gen rre_35_T602=real(re_35_T602) if emp_ci==1 & re_35_T601=="1"
replace rre_35_T602=. if rre_35_T602==98
gen rre_36_T602=real(re_36_T602) if emp_ci==1 & re_36_T601=="1"
replace rre_36_T602=. if rre_36_T602==98 
egen ylnmpri_ci=rsum(rre_35_T602 rre_36_T602)
replace ylnmpri_ci=. if rre_35_T602==. &  rre_36_T602==.

***************
***ylmsec_ci***
***************

gen ylmsec_ci=real(re_45_T61) if emp_ci==1 
replace ylmsec_ci=. if (ylmsec_ci==98 | ylmsec_ci==99)

******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.

************
***ylm_ci***
************

*para inactivos y desempleados VER COUNTRY SPECIFIC CODE BOOK
gen yprid2=real(re_72_T801) 
replace yprid2=. if yprid2==99
gen yprid3=real(re_65_T701) 
replace yprid3=. if yprid3==99

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci yprid2 yprid3)
replace ylm_ci=. if ylmpri==. & ylmsec==. & yprid2==. & yprid3==.

*****************
****nrylm_ci*****
*****************

gen nrylm_ci=(ylm_ci==. & emp_ci==1)

*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)

*************
***ynlm_ci***
*************
*inactivos*  
gen arriendos=real(re_72_T802)
gen pensiones=re_72_T803
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(re_73_T801)/12 if re_73_T801!="99" & re_73_T801!="98"
gen intereses=real(re_73_T801)/12 if re_73_T801!="99" & re_73_T801!="98"
gen otras=real(re_73_T801)/12 if re_73_T801!="99"& re_73_T801!="98"

egen temi=rsum(arriendos pensiones ayudas intereses otras )
replace temi=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*ocupados*  

gen arriendos=real(re_38_T601)
gen pensiones=re_38_T602
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(re_39_T601)/12 if re_39_T601!="99" & re_39_T601!="98"
gen intereses=real(re_39_T602)/12  if re_39_T602!="99" & re_39_T602!="98"
gen otras=real(re_39_T603)/12 if re_39_T603!="99" & re_39_T603!="98"

egen temo=rsum(arriendos pensiones ayudas intereses otras )
replace temo=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*desocupados*  
gen arriendos=real(re_65_T702)
gen pensiones=re_65_T703
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(re_66_T701)/12 if re_66_T701!="99" & re_66_T701!="98"
gen intereses=real(re_66_T702)/12 if re_66_T702!="99" & re_66_T702!="98"
gen otras=real(re_66_T703)/12 if re_66_T703!="99"& re_66_T703!="98"

egen temd=rsum(arriendos pensiones ayudas intereses otras )
replace temd=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*todos*
egen ynlm_ci=rsum(temi temo temd)
replace ynlm_ci=. if temi==. & temo==. & temd==.

*****************
***remesas_ci***
*****************

gen remesas_ci=.

****************
*** ynlnm_ci ***
****************

gen ynlnm_ci=.

************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

******************
*** nrylmpri_ch***
******************
*Creating a flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

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

*Yessenia Loayza (may 2014) -> corrigo variable aedu_ci 
*** people who have missings
gen byte yedc=.
gen yedc1=real(re_15_T101) /*Todos*/
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
replace yedc=6 if yedc1==406 
replace yedc=7 if yedc1==407 
replace yedc=8 if yedc1==408 
replace yedc=9 if yedc1==409 
replace yedc=10 if yedc1==410
replace yedc=11 if yedc1==411 | yedc1==500 
replace yedc=12 if yedc1==412 
replace yedc=13 if yedc1==413 

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
replace yedc=21 if yedc1==510
replace yedc=22 if yedc1==511
replace yedc=23 if yedc1==512
replace yedc=24 if yedc1==513
replace yedc=25 if yedc1==514
replace yedc=26 if yedc1==515

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
replace asiste_ci=1 if re_13_T10=="1"
replace asiste_ci=0 if re_13_T10=="2"
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
replace edupub_ci=1 if re_14_T10=="1"
replace edupub_ci=0 if re_14_T10=="2"
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
replace aguamala_ch=0 if  re_4_T01_=="2"

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

gen telef_ch=(re_8_T01_1=="1")

****************
****refrig_ch***
****************


gen refrig_ch=(re_8_T01_3=="1")

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

gen  tcylmpri_ci =.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ch=.

append using "`base_out1'"
order re_* ca_*

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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

rename ca_30_T60 codocupa
rename ca_32_T60 codindustria
destring codindustria, replace

compress


saveold "`base_out'", replace


log close



