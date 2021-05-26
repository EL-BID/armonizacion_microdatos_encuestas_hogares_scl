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
local ENCUESTA ECH
local ANO "2001"
local ronda t3 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                      
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Colombia
Encuesta: ECH
Round: t3
Autores: Analia
Generación nuevas variables LMK: 
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: noviembre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear


************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***region_c ***
***************
gen region_c= real(substr(pIDENT,1,2))
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
	76 "Valle"	
label value region_c region_c
label var region_c "division politico-administrativa, departamento"


***************
***factor_ch***
***************
gen factor_ch=FACTEXP
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

gen idh_ch=pIDENT
sort idh_ch
label variable idh_ch "ID del hogar"

/*Alvaro AM 07-2019: genero este nuevo id porque el destring no recoge todos los digitos del string
de id para COL-2001, lo que resulta en ids de hogar duplicados y en una subestimaciÃ³n de pobreza
porque se sobreestiman los ingresos familiares. 
Ver: https://www.statalist.org/forums/forum/general-stata-discussion/general/1371814-problem-with-format-of-variable-after-using-destring-stata-13-1-mp-on-windows-7
*/
egen numeric_id = group(idh_ch)
destring idh_ch, replace
replace idh_ch=numeric_id

**************
****idp_ci****
**************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=area
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_ci
/*la definicion de rural tiene que ser tomada con cuidado
puesto que es una definicion politico-geografica, no por 
densidad demografica*/

************
****pais****
************

gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2001
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=mes_c1
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo"
label define mes_c 6 "Junio" 7 " Julio" 8 "Agosto",add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
label value mes_c mes_c

*****************
***relacion_ci***
*****************
gen paren=real(p3_T10)
gen relacion_ci=.
replace relacion_ci=1 if paren==1
replace relacion_ci=2 if paren==2 
replace relacion_ci=3 if paren==3
replace relacion_ci=4 if paren>=4 & paren<=9
replace relacion_ci=5 if (paren>=10 & paren<=11) |(paren>=13 & paren<=15)
replace relacion_ci=6 if paren==12
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
gen factor_ci=FACTEXP
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci=.
replace sexo_ci=1 if p4_T10=="1"
replace sexo_ci=2 if p4_T10=="2"
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=real(p5_T10)
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
replace civil_ci=1 if p6_T10=="5"
replace civil_ci=2 if p6_T10=="1" | p6_T10=="2" 
replace civil_ci=3 if p6_T10=="4"
replace civil_ci=4 if p6_T10=="3" 

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
*by idh_ch, sort: egen nmiembros_ch=sum(paren>=1 & paren<=11)
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

* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
*gen miembros_ci=(paren>=1 & paren<=11)
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
* COL 2001
gen salmm_ci= 	9533.333*30
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************
destring  p12_T50 p13_T50 p14_T50 p15_T501 p16_T50 p17_T50 p18_T50 p23_T50, replace
gen condocup_ci=.
replace condocup_ci=1 if  p12_T50 == 1  | p13_T50==1 | p14_T50==1 | p15_T501==1
replace condocup_ci=2 if condocup_ci!=1 & (p12_T50 == 2 | p16_T50==1 | (p18_T50==1 | p18_T50==3)) & p23_T50==1
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2 & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
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
* Solo hay variable para este año por lo que no se puede homologar.
gen cotizando_ci=.
/*
* 2014, 01 Incorporacion MLO
gen cotizando_ci=0 if condocup_ci==1 | condocup_ci==2
replace cotizando_ci=1 if  (p6H_T10=="1" |  p6H_T10=="2") & (condocup_ci==1 | condocup_ci==2)
label var cotizando_ci "1 Cotizante a la Seguridad Social"
*/

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
destring p53_T70, replace
gen cesante_ci=1 if p53_T70==2 
replace cesante_ci=0 if p53_T70==1 
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
destring p32_T602 p58_T703 p65_T803, replace
egen aux_p=rsum(p32_T602 p58_T703 p65_T803),m
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
* MGD 11/30/2015: error al poner condicion de pension_ci<1000
gen ypen_ci=aux_p if aux_p>0 & aux_p<.
replace ypen_ci= . if aux_p==9999999999
replace ypen_ci=. if pension_ci==0 /*|  pension_ci<1000*/
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
gen categoinac_ci=. 
label var categoinac_ci "Condición de inactividad"
label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos"
label value categoinac_ci categoinac_ci
/*Y.L. en la ECH No se puede clasificar a todos los inactivos.
Ademas, la pregunta F18/p18 no considera la categoria jubilado. */

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
gen formal_ci=.

/************
***emp_ci***
************
gen trab=(p12_T50=="1")
replace trab=1 if (p12_T50>="2" & p12_T50<="5") & (p13_T50=="1" | p14_T50=="1"|p15_T501=="1" )
replace trab=2 if (p12_T50>="2" & p12_T50<="5") & p23_T50=="1" 
*definicion anterior de desempleo*

gen byte emp_ci=(trab==1)


gen desemp_abierto=1 if p16_T50=="1" & p23_T50=="1"
*p16_T50 hizo diligencias en las ultimas 4 semanas
*p23_T50 estaba disponible para trabajar la semana pasada*
gen desemp_oculto=1 if  (p18_T50>="02" & p18_T50<="10") & p21_T50=="1"
* no ha buscado trabajo en la ultima semana, razones validas, pero si ha buscado en el año
gen desemp_DANE=1 if  desemp_abierto==1 | desemp_oculto==1 | p23_T50=="1"

****************
***desemp1_ci***
****************

gen desemp1_ci=.
/* la definicion incluye desempeados de solo una semana*/

****************
***desemp2_ci*** 
****************

gen desemp2_ci=.
*Tambien debe ser una semana, por eso no se genera

****************
***desemp3_ci***
****************
gen desemp3_ci=(p16_T50=="1" |p20_T50=="1" | p21_T50=="1") 
*p16_T50 diligencias en el ultimo mes
*p20_T50 despues de su ultimo empleo
*p21_T50 12 meses

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
gen desalent_ci=((p18_T50>=04 & p18_T50<=11)|p18_T50==02)

***************
***subemp_ci***
***************

/*todas las horas estan truncadas en 120- 98*/
gen promhora=real(p34_T61) if emp_ci==1 
replace promhora=120 if p34_T61=="998"
replace promhora=. if p34_T61=="999"

/*
gen hora_no=real(p35_T612) if p35_T611=="1" & emp_ci==1 
replace hora_no=120 if p35_T612=="998"
replace hora_no=. if p35_T612=="999"

gen hora_ad=real(p36_T612) if p36_T611=="1" & emp_ci==1 
replace hora_ad=120 if p36_T612=="998"
replace hora_ad=. if p36_T612=="999"

gen promhora2=real(p39_T624) if emp_ci==1 
replace promhora2=98 if p39_T624=="98" /*acotado*/
replace promhora2=. if p39_T624=="99"

gen promhora3=real(p41_T62) if emp_ci==1 
replace promhora3=98 if p41_T62=="98" /*acotado*/
replace promhora3=. if p41_T62=="99"
*/

gen promhora1=real(p39_T621) if emp_ci==1 
replace promhora1=98 if p39_T621=="98" /*acotado*/
replace promhora1=. if p39_T621=="99"

egen tothoras=rowtotal (promhora promhora1)
replace tothoras=. if promhora==. & promhora1==. 
replace tothoras=. if tothoras>=168

gen subemp_ci=0
replace subemp_ci=1 if tothoras<=30  & emp_ci==1 & p40_T62=="1"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras<30 & emp_ci==1 & p40_T62=="2"

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
gen categopri_ci=.
*Urbana/cabecera
gen categurb=real(p27_T60cab)
replace categopri_ci=1 if categurb ==5                                  & zona_c==1
replace categopri_ci=2 if categurb ==4                                  & zona_c==1
replace categopri_ci=3 if (categurb ==1 | categurb ==2 | categurb ==3 ) & zona_c==1 
replace categopri_ci=4 if categurb ==6                                  & zona_c==1
replace categopri_ci=0 if categurb ==7                                  & zona_c==1
*rural/resto
gen categrur=real(p27_T60res)
replace categopri_ci=1 if categrur ==6                                  & zona_c==0
replace categopri_ci=2 if categrur ==5                                  & zona_c==0
replace categopri_ci=3 if (categrur>=1 & categrur <=3)                  & zona_c==0  
replace categopri_ci=4 if categrur ==7                                  & zona_c==0
replace categopri_ci=0 if categrur ==8  | categrur ==4                  & zona_c==0
label define categopri_ci 0 "Otros"  1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"
*Y.L -> coloco la categoria 4 en otros para hacerlo comprarable a la serie.

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
replace nempleos_ci=1 if p37_T62=="2"
replace nempleos_ci=2 if p37_T62=="1"

/*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
/*NA*/
*/

*****************
***spublico_ci***
*****************
gen spublico_ci=(p27_T60res=="2" | p27_T60cab=="2")

**************
***ocupa_ci***
**************
destring p24_T60, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (p24_T60>=1 & p24_T60<=19)  & emp_ci==1  
replace ocupa_ci=2 if (p24_T60>=20 & p24_T60<=21) & emp_ci==1
replace ocupa_ci=3 if (p24_T60>=30 & p24_T60<=39) & emp_ci==1
replace ocupa_ci=4 if (p24_T60>=40 & p24_T60<=49) & emp_ci==1
replace ocupa_ci=5 if (p24_T60>=50 & p24_T60<=59) & emp_ci==1
replace ocupa_ci=6 if (p24_T60>=60 & p24_T60<=64) & emp_ci==1
replace ocupa_ci=7 if (p24_T60>=70 & p24_T60<=98) & emp_ci==1
replace ocupa_ci=9 if (p24_T60==0 |  p24_T60==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

*************
***rama_ci***
*************
gen rama1=real(p26_T60)
gen rama_ci=. 
replace rama_ci=1 if rama1>=1 & rama1<=5  
replace rama_ci=2 if rama1>=10 & rama1<=14  
replace rama_ci=3 if rama1>=15 & rama1<=37  
replace rama_ci=4 if rama1>=40 & rama1<=41  
replace rama_ci=5 if rama1>=45
replace rama_ci=6 if rama1>=50 & rama1<=55  
replace rama_ci=7 if rama1>=60 & rama1<=64  
replace rama_ci=8 if rama1>=65 & rama1<=71  
replace rama_ci=9 if rama1>=72 & rama1<=99  
replace rama_ci=. if emp_ci==0
drop rama1

****************
***durades_ci***
****************
gen durades_ci=real(p49_T70)/4.3
*replace durades_ci=61.9 if p54_T70=="998"
/*está truncada en 260 semanas---> 61.9 meses*/
replace durades_ci=. if p49_T70=="999" | p49_T70=="998"

*p54_T70 es en semanas*
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
gen ypridcab=real(p28_T60) if emp_ci==1 & (p27_T60cab>="1" & p27_T60cab<="3") 
replace ypridcab=. if ypridcab==98 /*no sabe*/
replace ypridcab=. if ypridcab==99 /*no informa*/
gen nosabe_is1cab=(p28_T60=="98" )
gen nosabe_is2cab=(p28_T60=="99" )

/*Ganancia al trabajo mensual*/
gen yprid1cab=real(p31_T60cab) if emp_ci==1 & (p27_T60cab=="4"|p27_T60cab=="5"|p27_T60cab=="7") & zona_c==1
replace yprid1cab=. if yprid1cab==98 & zona_c==1 /*no sabe*/
replace yprid1cab=. if yprid1cab==99 & zona_c==1 /*no informa*/
gen nosabe_ig1cab=(p31_T60cab=="98" & zona_c==1) 
gen nosabe_ig2cab=(p31_T60cab=="99" & zona_c==1)
/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid1cab=0 if emp_ci==1 & p27_T60cab=="6" & zona_c==1


*rural
/*Remuneracion al empleo*/
gen ypridres=real(p28_T60) if emp_ci==1 & (p27_T60res>="1" & p27_T60res<="4") & zona_c==0
replace ypridres=. if ypridres==98 & zona_c==0 /*no sabe*/
replace ypridres=. if ypridres==99 & zona_c==0 /*no informa*/
/*Ganancia al trabajo ANUAL*/
gen yprid1res=real(p31_T60res)/12 if emp_ci==1 & (p27_T60res=="6"|p27_T60res=="5"|p27_T60res=="8") & zona_c==0
replace yprid1res=. if p31_T60res=="98" & zona_c==0 /*no sabe*/
replace yprid1res=. if p31_T60res=="99" & zona_c==0 /*no informa*/
/* Se le pone ceros a los trabajadores no remunerados*/
replace yprid1res=0 if emp_ci==1 & p27_T60res=="7" & zona_c==0
egen yprid =rsum(ypridcab ypridres)
replace yprid =. if ypridcab==. & ypridres==.
egen yprid1= rsum(yprid1cab yprid1res)
replace yprid1 =. if yprid1cab==. & yprid1res==.

egen ylmpri_ci=rsum(yprid yprid1)
replace ylmpri_ci=. if yprid==. & yprid1==.
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=0 if categopri_ci==4

*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

*****************
*** ylnmpri_ci***
*****************
*alimentos*
gen ylnmpria=real(p29_T602) if emp_ci==1 & p29_T601=="1"
replace ylnmpria=. if ylnmpria==98 
gen nosabe_inma1=(p29_T602=="98")
*vivienda*
gen ylnmprib=real(p30_T602) if emp_ci==1 & p30_T601=="1"
replace ylnmprib=. if ylnmprib==98 
gen nosabe_inmv1=(p30_T602=="98")
*Transporte*
gen ylnmpric=real(p30_T604) if emp_ci==1 & p30_T603=="1"
replace ylnmpric=. if ylnmpric==98 
gen nosabe_inmele1=(p30_T604=="98")
*otros: electrodomesticos, mercados, sodexho, etc*
gen ylnmprid=real(p30_T606) if emp_ci==1 & p30_T605=="1"
replace ylnmprid=. if ylnmprid==98 
gen nosabe_inmt1=(p30_T606=="98")

egen ylnmpri_ci=rsum(ylnmpria ylnmprib ylnmpric ylnmprid)
replace ylnmpri_ci=. if ylnmpria==. &  ylnmprib==. &  ylnmpric==. &  ylnmprid==. 

***************
***ylmsec_ci***
***************

gen ylmsec_ci=real(p38_T62) if emp_ci==1 
replace ylmsec_ci=. if ylmsec_ci==98 /*no sabe*/
replace ylmsec_ci=. if ylmsec_ci==99 /*no informa*/
gen nosabe_isec1=(p38_T62=="98")
gen nosabe_isec2=(p38_T62=="99")

******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.

************
***ylm_ci***
************

*para inactivos y desempleados VER COUNTRY SPECIFIC CODE BOOK
gen yprid2=real(p65_T801) 
replace yprid2=. if yprid2==99
gen yprid3=real(p58_T701) 
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
gen arriendos=real(p65_T802)
gen pensiones=p65_T803
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(p66_T801)/12 if p66_T801!="99" & p66_T801!="98"
gen intereses=real(p66_T802)/12 if p66_T802!="99" & p66_T802!="98"
gen otras=real(p66_T803)/12 if p66_T803!="99"& p66_T803!="98"

egen temi=rsum(arriendos pensiones ayudas intereses otras )
replace temi=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*ocupados*  
gen arriendos=real(p32_T601)
gen pensiones=p32_T602
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(p33_T601)/12 if p33_T601!="99" & p33_T601!="98"
gen intereses=real(p33_T602)/12  if p33_T602!="99" & p33_T602!="98"
gen otras=real(p33_T603)/12 if p33_T603!="99" & p33_T603!="98"

egen temo=rsum(arriendos pensiones ayudas intereses otras )
replace temo=. if arriendos==. & pensiones==. & ayudas==. & intereses==. & otras==.
drop arriendos pensiones ayudas intereses otras 

*desocupados*  
gen arriendos=real(p58_T702)
gen pensiones=p58_T703
for var arriendos pensiones: replace X=. if X==99 | X==98

gen ayudas=real(p59_T701)/12 if p59_T701!="99" & p59_T701!="98"
gen intereses=real(p59_T702)/12 if p59_T702!="99" & p59_T702!="98"
gen otras=real(p59_T703)/12 if p59_T703!="99"& p59_T703!="98"

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

**************
***ynlnm_ci***
**************

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
gen yedc1=real(p10_T10) /*Todos*/
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
replace yedc=11 if yedc1==411  | yedc1==500 
replace yedc=12 if yedc1==412 /* Y.L -> 12 años de educacion secundaria "son muy pocos"*/
replace yedc=13 if yedc1==413 /* Y.L -> 13 años de educacion secundaria "son muy pocos"*/

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
gen byte edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>=6 & aedu_ci<9)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci==10 
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen byte edus2c_ci=0
replace edus2c_ci=1 if (aedu_ci>=11 & aedu_ci<=13) 
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************
*Y.L - > Para la educación superior no es posible saber cuantos anios dura el ciclo esta es una aprox.
gen byte eduui_ci=(aedu_ci>11 & aedu_ci<16)
label variable eduui_ci "Superior incompleto"

***************
***eduuc_ci****
***************
*Y.L. -> Para la educación superior no es posible saber cuantos anios dura el ciclo esta es una aprox.
gen byte eduuc_ci= (aedu_ci>=16 & aedu_ci!=.)
label variable eduuc_ci "Superior completo"

***************
***edupre_ci***
***************
gen byte edupre_ci=(p10_T10=="201")
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
replace asiste_ci=1 if p8_T10_=="1"
replace asiste_ci=0 if p8_T10_=="2"
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
replace edupub_ci=1 if p9_T10_=="1"
replace edupub_ci=0 if p9_T10_=="2"
label variable edupub_ci "Asiste a centros publicos"

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************
gen aguared_ch=(p4_T01_=="1" | p4_T01_=="2")
replace aguared_ch=. if p4_T01_=="."

****************
**aguadist_ch***
****************

gen aguadist_ch=.

****************
**aguamala_ch***
****************
destring p4_T01_, replace
gen aguamala_ch=(p4_T01_==5 | p4_T01_==9)
replace aguamala_ch=. if p4_T01_==.

****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

* Modificaciones Marcela Rubio - Noviembre 2014: se genera variable como missing ya que con var p4i_T013 no puedo generarse luz_ch ya que sólo toma el valor de 1
/*
destring p4i_T013, replace
gen luz_ch=(p4i_T013==1)
replace luz_ch=. if p4i_T013==.
*/

gen luz_ch=.

****************
***luzmide_ch***
****************
gen luzmide_ch=.

****************
***combust_ch***
****************
gen combust_ch=(p5_T01=="1"|p5_T01=="3")

****************
****bano_ch*****
****************
gen bano_ch=(p2_T01=="1"|p2_T01=="2"|p2_T01=="4")


****************
****banoex_ch***
****************
gen banoex_ch=.


****************
****des1_ch*****
****************
gen des1_ch=.
replace des1_ch=0 if p2_T01=="6" 
replace des1_ch=1 if p2_T01=="1"|p2_T01=="2"
replace des1_ch=2 if p2_T01=="3" | p2_T01=="4"
replace des1_ch=3 if p2_T01=="5"
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch



****************
****des2_ch*****
****************

gen des2_ch=.

 
****************
****piso_ch*****
****************

gen piso_ch=(p3i_T01~="1")



****************
****pared_ch****
****************

destring p2i_T01, replace
gen pared_ch=(p2i_T01<=3 | p2i_T01==5)

****************
****techo_ch****
****************

gen techo_ch=.

****************
****resid_ch****
****************

gen basura=real(p3_T01_)
gen resid_ch=0 if basura==4
replace resid_ch=1 if basura==3 
replace resid_ch=2 if basura==2 | basura==1 
drop basura
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
destring p4_T01_ p2_T01_, replace
g       aguamejorada_ch = 1 if (p4_T01_ >=1 & p4_T01_ <=4) | p4_T01_ ==6 | p4_T01_ ==9
replace aguamejorada_ch = 0 if  p4_T01_==5 | (p4_T01_ >=7 & p4_T01_ <=8) | p4_T01_ ==0
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if ( p2_T01_ >=1 &  p2_T01_ <=4)
replace banomejorado_ch = 0 if ( p2_T01_ >=5 &  p2_T01_ <=6)
		
****************
****dorm_ch*****
****************

gen dorm_ch=.

****************
***cuartos_ch***
****************

gen cuartos_ch=real(p1_T01_)
replace cuartos_ch=. if cuartos_ch==99

****************
***cocina_ch****
****************

gen cocina_ch=.


****************
****telef_ch****
****************

gen telef_ch=(p8_T01_1=="1")

****************
****refrig_ch***
****************


gen refrig_ch=(p8_T01_3=="1")

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
gen vivi1_ch=real(p1i_T01)
replace vivi1_ch=3 if vivi1_ch>=3
replace vivi1_ch=. if vivi1_ch==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

****************
****vivi2_ch****
****************
gen vivi2_ch=.


*******************
****viviprop_ch****
*******************
gen viviprop_ch=real(p6_T01_)
replace viviprop_ch=0 if viviprop_ch==3  
replace viviprop_ch=3 if viviprop_ch==5 | viviprop_ch==4
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=real(p7_T01_)
replace vivialq_ch=. if vivialq_ch<999

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.

gen  tcylmpri_ci =.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ch=.

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



compress


saveold "`base_out'", replace


log close



