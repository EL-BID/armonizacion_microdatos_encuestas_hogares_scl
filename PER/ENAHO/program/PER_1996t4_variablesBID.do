



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

local PAIS PER
local ENCUESTA ENAHO
local ANO "1996"
local ronda t4 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENAHO
Round: t4
Autores: 
Última versión: Mayra Sáenz - Email: saenzmayra.a@gmail.com - mayras@iadb.org
Fecha última modificación: agosto 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

***************
***region_c ***
***************
tostring ubi, replace
gen digito ="0"
gen length =length(ubi)
egen aux = concat(digito ubi) if length==5
replace ubi=aux if length==5
drop digito length aux 

gen region_c=real(substr(ubi,1,2))
label define region_c ///
1"Amazonas"	          ///
2"Ancash"	          ///
3"Apurimac"	          ///
4"Arequipa"	          ///
5"Ayacucho"	          ///
6"Cajamarca"	      ///
7"Callao"	          ///
8"Cusco"	          ///
9"Huancavelica"	      ///
10"Huanuco"	          ///
11"Ica"	              ///
12"Junin"	          ///
13"La libertad"	      ///
14"Lambayeque"	      ///
15"Lima"	          ///
16"Loreto"	          ///
17"Madre de Dios"	  ///
18"Moquegua"	      ///
19"Pasco"	          ///
20"Piura"	          ///
21"Puno"	          ///
22"San Martín"	      ///
23"Tacna"	          ///
24"Tumbes"	          ///
25"Ucayali"	
label value region_c region_c
label var region_c "division politico-administrativa, departamento" 

************************
*** region según BID ***
************************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************

gen factor_ch=fac130
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

sort  ubi seg viv hog 
egen idh_ch= group( ubi seg viv hog)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci= piiin 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=1 if p000b1 == 11 | p000b1 == 21 | p000b1== 22
replace zona_c=0 if p000b1 == 32 | p000b1 == 42

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="PER"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1996
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
tostring pfec, replace 
gen mes_c=real(substr(pfec,3,2))
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if p43==1
replace relacion_ci=2 if p43==2
replace relacion_ci=3 if p43==3
replace relacion_ci=4 if p43>=4 & p43<=6
replace relacion_ci=5 if p43==8 | p43==9
replace relacion_ci=6 if p43==7

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci

/*En este año no hay empleados domésticos ni pensionistas*/


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=fac130
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci= p46

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=p47a if p47b==1
replace edad_ci=0 if p47b==2
replace edad_ci=. if edad==99
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if p50==6
replace civil_ci=2 if p50==1 | p50==2
replace civil_ci=3 if p50==4 | p50==5
replace civil_ci=4 if p50==3

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
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
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

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************
capture gen condocup_ci=.
replace condocup_ci=1 if p68==1 | p69 == 1 | p701 ==1 | p702 ==1 | p703 ==1 | p704 ==1 | p705 ==1 | p706 ==1 | p707 ==1  | p708 ==1 
replace condocup_ci=2 if (p68==2 & p99==1 & p100==1) 
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 6"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (desculey>0 & desculey<99999)  /*a ocupados subordinados: empleados u obreros*/
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (desculey>0 & desculey<99999) 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=ocprango 
label define  tamemp_ci 1"menos de 100" 2"de 100 a 499" 3"de 500 y más p"
label var tamemp_ci "# empleados en la empresa de la actividad principal"

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (tranco01==1 | tranco06==1) /* A todas las per mayores de 13*/
replace pension_ci=. if tranco01==. & tranco06==.

*************
**  ypen_ci *
*************
gen ypen_ci=. 
/*Nota: La pregunta resume el monto de todas las 
transferencias no se puede distinguir cual es por pensiones*/
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if hatraant==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =linea97
label var lp_ci "Linea de pobreza oficial del pais"

*********
*li_ci***
*********
gen lpe_ci =linea97 
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*******************
****** tc_c *******
*******************

gen tc_c= 2.453333 
label var tc_c "Tasa de cambio LCU/USD"

*******************
****** ipc_c ******
*******************

gen ipc_c =  61.26602
label var ipc_c "Índice de precios al consumidor base 2011=100"

*******************
****** ppp_c ******
*******************

gen ppp_c = 1.378235 
label var ppp_c "Factor de conversión PPP LCU/USD"

****************
*lp25_2005_ci***
****************

*MGR Jul,2015: se modifican líneas de pobreza internacionales con año base PPP2011. Se renombran líneas con año base PPP2005. 
gen lp25_2005_ci = 99.21872
label var lp25_2005_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2005"

***************
*lp4_2005_ci***
***************

*MGR Jul,2015: se modifican líneas de pobreza internacionales con año base PPP2011. Se renombran líneas con año base PPP2005. 
gen lp4_2005_ci = 158.75
label var lp4_2005_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2005"

********* 
*lp25_ci
*********

gen lp25_ci = 73.07928
capture label var lp25_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2011"

*********
*lp4_ci*
*********
gen lp4_ci = 116.9269  
capture label var lp4_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2011"


*************
**salmm_ci***
*************

gen salmm_ci= .
replace salmm_ci=215
label var salmm_ci "Salario minimo legal"

***************
***tecnica_ci**
***************
gen tecnica_ci=(niveduca==5 | niveduca==6 | nivasist==4)
label var tecnica_ci "=1 formacion terciaria tecnica"	

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

****************
***formal_ci ***
****************
gen formal_ci=(cotizando_ci==1)

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & bustraba==2 & (razbustr==1 | razbustr==2))


*****************
***horaspri_ci***
*****************

gen horaspri_ci=ocphorto
replace horaspri_ci=. if ocphorto==99 | emp_ci~=1


*****************
***horastot_ci***
*****************

gen p518_alt=ocshortr
replace p518_alt=. if ocshortr==99

egen horastot_ci=rsum(horaspri_ci p518_alt)
replace horastot_ci=. if horaspri_ci==. & p518_alt==.
replace horastot_ci=. if emp_ci~=1

drop p518_alt

***************
***subemp_ci***
***************
/*Sobre las horas normalmente trabajadas*/

gen subemp_ci=0
replace subemp_ci=1 if (thtrabaj==1 & horaspri_ci<=30) & thdispon==1 
replace subemp_ci=1 if (thtrabaj==2 & thnormal<=30) & thdispon==1
replace subemp_ci=. if emp_ci==.
* Modificacion con subempleo visible: quiere trabajar mas horas y esta disponible a trabajar mas horas. MGD 06/19/2014
gen subemp_ci1=0
replace subemp_ci1=1 if horaspri_ci<=30 & thdispon==1 & emp_ci==1 

*******************
***tiempoparc_ci***
*******************
/*Sobre las horas normalmente trabajadas*/

gen tiempoparc_ci=0
/*replace tiempoparc_ci=1 if (thtrabaj==1 & horastot_ci<=30) & thdispon==2
replace tiempoparc_ci=1 if (thtrabaj==2 & thnormal<=30) & thdispon==2
replace tiempoparc_ci=. if emp_ci==.*/
* 10/20/2015 MGD: no se usa las horas totales trabajadas sino solo las de la actividad principal.
replace tiempoparc_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30) & thdispon==2 & emp_ci==1
replace tiempoparc_ci=. if emp_ci==0


******************
***categopri_ci***
******************
* 10/20/2015 MGD: se añade la categoria otra clasificacion para sacarlos de los no remunerados

gen categopri_ci=.
replace categopri_ci=0 if ocpcateg==7
replace categopri_ci=1 if ocpcateg==1
replace categopri_ci=2 if ocpcateg==2 
replace categopri_ci=3 if ocpcateg==3 | ocpcateg==4 | ocpcateg==6 
replace categopri_ci=4 if ocpcateg==5 /*| ocpcateg==7*/

label define categopri_ci 0 "Otra clasificación" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
* 10/20/2015 MGD: se añade la categoria otra clasificacion para sacarlos de los no remunerados

gen categosec_ci=.
replace categosec_ci=0 if ocscateg==7
replace categosec_ci=1 if ocscateg==1
replace categosec_ci=2 if ocscateg==2 
replace categosec_ci=3 if ocscateg==3 | ocscateg==4 | ocscateg==6
replace categosec_ci=4 if ocscateg==5 /*| ocscateg==7*/

label define categosec_ci 0 "Otra clasificación"1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=2 if emp_ci==1 & ocsactiv==1
replace nempleos_ci=1 if emp_ci==1 & ocsactiv==2 

*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if ocpnrotr<=5
replace firmapeq_ci=0 if ocpnrotr>5
replace firmapeq_ci=0 if ocprango==2 | ocprango==3
replace firmapeq_ci=. if emp_ci~=1


*****************
***spublico_ci***
*****************

gen spublico_ci=(ocptraba==1 | ocptraba==2 | ocptraba==3)
replace spublico_ci=. if emp_ci~=1


**************
***ocupa_ci***
**************
gen ocupa_ci=.
replace ocupa_ci=1 if (ocprinci>=211 & ocprinci<=396) & emp_ci==1
replace ocupa_ci=2 if (ocprinci>=111 & ocprinci<=148) & emp_ci==1
replace ocupa_ci=3 if (ocprinci>=411 & ocprinci<=462) & emp_ci==1
replace ocupa_ci=4 if (ocprinci>=571 & ocprinci<=583) | (ocprinci>=911 & ocprinci<=931) & emp_ci==1
replace ocupa_ci=5 if (ocprinci>=511 & ocprinci<=565) | (ocprinci>=941 & ocprinci<=961) & emp_ci==1
replace ocupa_ci=6 if (ocprinci>=611 & ocprinci<=641) | (ocprinci>=971 & ocprinci<=973) & emp_ci==1
replace ocupa_ci=7 if (ocprinci>=711 & ocprinci<=886) | (ocprinci>=981 & ocprinci<=987) & emp_ci==1
replace ocupa_ci=8 if (ocprinci>=11 & ocprinci<=24) & emp_ci==1


*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (ocpciiuu>=100 & ocpciiuu<=500) & emp_ci==1
replace rama_ci=2 if (ocpciiuu>=1000 & ocpciiuu<=1429) & emp_ci==1
replace rama_ci=3 if (ocpciiuu>=1500 & ocpciiuu<=3720) & emp_ci==1
replace rama_ci=4 if (ocpciiuu>=4000 & ocpciiuu<=4100) & emp_ci==1
replace rama_ci=5 if (ocpciiuu>=4500 & ocpciiuu<=4550) & emp_ci==1
replace rama_ci=6 if (ocpciiuu>=5000 & ocpciiuu<=5520) & emp_ci==1 
replace rama_ci=7 if (ocpciiuu>=6000 & ocpciiuu<=6420) & emp_ci==1
replace rama_ci=8 if (ocpciiuu>=6500 & ocpciiuu<=7499) & emp_ci==1
replace rama_ci=9 if (ocpciiuu>=7500 & ocpciiuu<=9503) & emp_ci==1


****************
***durades_ci***
****************

gen durades_ci=.


*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************

*Para los trabajadores dependientes

gen ypridbd=.

replace ypridbd=ingdliq*30 if indpago==1 
replace ypridbd=ingdliq*4.3 if indpago==2 
replace ypridbd=ingdliq*2 if indpago==3 
replace ypridbd=ingdliq if indpago==4 
replace ypridbd=. if ingdliq==99999

replace ypridbd=0 if categopri_ci==4 
/* A los trabajadores no remunerados que trabajan menos de 15 horas la encuesta no les
pregunta acerca de sus ingresos y los manda a la sección de desempleo. Como este grupo en
realidad está trabajando, reemplazo su ingreso missing por cero*/

replace ypridbd=. if categopri_ci<=2

*Para los trabajadores independientes
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen yprijbi=.
replace yprijbi=ingindep
replace yprijbi=. if ingindep==99999
replace yprijbi=. if categopri_ci>2

*Ingreso laboral monetario para todos

egen ylmpri_ci=rsum(yprijbi ypridbd)
replace ylmpri_ci=. if ypridbd==. & yprijbi==.
replace ylmpri_ci=. if emp~=1


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

*Ingreso laboral no monetario de los dependientes

local especie= "alime vesti trans vivie salud otro"

foreach i of local especie { 

gen especie`i'=.
replace especie`i'=(est`i')*30  if fre`i'==1
replace especie`i'=(est`i')*4.3 if fre`i'==2
replace especie`i'=(est`i')*2   if fre`i'==3
replace especie`i'=est`i'     if fre`i'==4
replace especie`i'=(est`i')/2   if fre`i'==5
replace especie`i'=(est`i')/3   if fre`i'==6
replace especie`i'=(est`i')/6   if fre`i'==7
replace especie`i'=(est`i')/12  if fre`i'==8
replace especie`i'=. if est`i'==9999 
}


rename especiealime especie1
rename especievesti especie2
rename especietrans especie3
rename especievivie especie4
rename especiesalud especie5
rename especieotro especie6


egen ylnmprid=rsum(especie1 especie2 especie3 especie4 especie5 especie6)
replace ylnmprid=. if especie1==. &  especie2==. & especie3==. & especie4==. & especie5==. & especie6==.
replace ylnmprid=0 if categopri_ci==4
replace ylnmprid=0 if pagespec==2
replace ylnmprid=. if categopri_ci<=2 | categopri_ci==.

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=ingautoc
replace ylnmprii=. if ingautoc==99999
replace ylnmprii=. if categopri_ci>2

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii)
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1


***************
***ylmsec_ci***
***************

*Ingreso laboral monetario para todos

gen ylmsec_ci=ingsecun
replace ylmsec_ci=. if ingsecun==99999
replace ylmsec_ci=0 if ingsnore==0
replace ylmsec_ci=. if emp~=1


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.


************
***ylm_ci***
************

*Sumamos ingresos extraordinarios por trabajo dependiente
egen ingext=rsum(ingsnavi ingspatr ingsvaca ingsesco ingspart ingsrela ingsotro)
replace ingext=. if ingsnavi==. & ingspatr==. & ingsvaca==. & ingsesco==. & ingspart==. & ingsrela==. & ingsotro==.
replace ingext=ingext/12

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ingext)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ingext==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

gen transco=ingtraco
replace transco=. if ingtraco==99999

gen rentas=ingrenta
replace rentas=. if ingrenta==99999

egen ynlm_ci=rsum(transco rentas)
replace ynlm_ci=. if transco==. & rentas==. 

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.


****************
***remesas_ci***
****************

gen remesas_ci=ingtraex
replace remesas_ci=. if ingtraex==99999

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


**************
*** ylm_ch ***
**************

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

******************
*** remesas_ch ***
******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen autocons_ci=ingautoc
replace autocons_ci=. if ingautoc==99999
replace autocons_ci=. if emp_ci~=1


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=alqmens2
replace rentaimp_ch=. if alqmens2==9999


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

/*En esta sección es sólo para los residentes habituales 
mayores a los 3 años de edad*/

destring anoaprob, replace

gen byte aedu_ci=.

replace aedu_ci=0 if niveduca==1 | niveduca==2

replace aedu_ci=1 if niveduca==3 & anoaprob==0
replace aedu_ci=2 if niveduca==3 & anoaprob==1
replace aedu_ci=3 if niveduca==3 & anoaprob==2
replace aedu_ci=4 if niveduca==3 & anoaprob==3
replace aedu_ci=5 if niveduca==3 & anoaprob==4
replace aedu_ci=6 if niveduca==3 & anoaprob==5

replace aedu_ci=7 if niveduca==4 & anoaprob==0
replace aedu_ci=7 if niveduca==4 & anoaprob==1
replace aedu_ci=8 if niveduca==4 & anoaprob==2
replace aedu_ci=9 if niveduca==4 & anoaprob==3
replace aedu_ci=10 if niveduca==4 & anoaprob==4
replace aedu_ci=11 if niveduca==4 & anoaprob==5

replace aedu_ci=12 if (niveduca>=5 & niveduca<=8) & anoaprob==1
replace aedu_ci=13 if (niveduca>=5 & niveduca<=8) & anoaprob==2
replace aedu_ci=14 if (niveduca>=5 & niveduca<=8) & anoaprob==3
replace aedu_ci=15 if (niveduca>=5 & niveduca<=8) & anoaprob==4
replace aedu_ci=16 if (niveduca>=5 & niveduca<=8) & anoaprob==5
replace aedu_ci=17 if (niveduca>=5 & niveduca<=8) & anoaprob==6
replace aedu_ci=18 if (niveduca>=5 & niveduca<=8) & anoaprob==7

replace aedu_ci=. if perseduc==0



**************
***eduno_ci***
**************

gen byte eduno_ci=(niveduca==1 | niveduca==2) 
replace eduno_ci=. if niveduca==. | niveduca==99
replace eduno_ci=. if perseduc==0 | aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(niveduca==3 & aedu_ci<=5)
replace edupi_ci=. if niveduca==. | niveduca==99
replace edupi_ci=. if perseduc==0 | aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(niveduca==3 & aedu_ci==6)
replace edupc_ci=. if niveduca==. | niveduca==99
replace edupc_ci=. if perseduc==0 | aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(niveduca==4 & aedu_ci<=10)
replace edusi_ci=. if niveduca==. | niveduca==99
replace edusi_ci=. if perseduc==0 | aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(niveduca==4 & aedu_ci==11)
replace edusc_ci=. if niveduca==. | niveduca==99
replace edusc_ci=. if perseduc==0 | aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(niveduca==4 & aedu_ci<=8)
replace edus1i_ci=. if niveduca==. | niveduca==99
replace edus1i_ci=. if perseduc==0 | aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(niveduca==4 & aedu_ci==9)
replace edus1c_ci=. if niveduca==. | niveduca==99
replace edus1c_ci=. if perseduc==0 | aedu_ci==.
label variable edus1c_ci "1er ciclo de la eecundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(niveduca==4 & aedu_ci==10)
replace edus2i_ci=. if niveduca==. | niveduca==99
replace edus2i_ci=. if perseduc==0 | aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(niveduca==4 & aedu_ci>=11)
replace edus2c_ci=. if niveduca==. | niveduca==99
replace edus2c_ci=. if perseduc==0 | aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(niveduca==5 | niveduca==7)
replace eduui_ci=. if niveduca==. | niveduca==99
replace eduui_ci=. if perseduc==0 | aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(niveduca==6 | niveduca==8)
replace eduuc_ci=. if niveduca==. | niveduca==99
replace eduuc_ci=. if perseduc==0 | aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(niveduca==2)
replace edupre_ci=. if niveduca==. | niveduca==99
replace edupre_ci=. if perseduc==0 | aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (niveduca==7 | niveduca==8)
replace eduac_ci=0 if (niveduca==5 | niveduca==6)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(asisccee==1)
replace asiste_ci=. if asisccee==9
replace asiste_ci=. if perseduc==0
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis=razdejes

label variable pqnoasis "Razones para no asistir a la escuela"
label define pqnoasis 1 "Estoy trabajando"
label define pqnoasis 2 "No me interesa", add
label define pqnoasis 3 "Por enfermedad", add
label define pqnoasis 4 "Prob. econ", add
label define pqnoasis 5 "Prob.fam", add
label define pqnoasis 6 "Bajas notas", add
label define pqnoasis 7 "Termino", add
label define pqnoasis 8 "Otra razón", add
label define pqnoasis 99 "Missing", add
label value pqnoasis pqnoasis

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if razdejes==4
replace pqnoasis1_ci = 2 if razdejes==1
replace pqnoasis1_ci = 3 if razdejes==3 | razdejes==5
replace pqnoasis1_ci = 4 if razdejes==2
replace pqnoasis1_ci = 6 if razdejes==7
replace pqnoasis1_ci = 9 if razdejes==6 | razdejes==8

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci



***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.

***************
***edupub_ci***
***************

gen edupub_ci=(ceneduca==1)
replace edupub_ci=. if ceneduca==9
replace edupub_ci=. if perseduc==0



**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(abasagua==1 | abasagua==2)

gen aguadist_ch=1 if abasagua==1
replace aguadist_ch=2 if abasagua==2
replace aguadist_ch=3 if abasagua>=3 & abasagua<=7

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=alumbra1

gen luzmide_ch=.
/*NA*/

gen combust_ch=(combuco1==1 | combuco2==2)

gen bano_ch=.
/*NA*/

gen banoex_ch=.
/*NA*/

gen des1_ch=0 if serhigie==5
replace des1_ch=1 if serhigie>=1 & serhigie<=2
replace des1_ch=2 if serhigie==3
replace des1_ch=3 if serhigie==4


gen des2_ch=0 if serhigie==5
replace des2_ch=1 if serhigie>=1 & serhigie<=2
replace des2_ch=2 if serhigie==3

gen piso_ch=0 if matpiso==6
replace piso_ch=1 if matpiso>=1 & matpiso<=5
replace piso_ch=2 if matpiso==7

gen pared_ch=0 if matpared==3 | matpared==4 | matpared==7
replace pared_ch=1 if matpared==1 | matpared==2 | matpared==5 | matpared==6
replace pared_ch=2 if matpared==8

gen techo_ch=.
/*NA*/

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (abasagua >=1 & abasagua <=3) | abasagua ==5
replace aguamejorada_ch = 0 if  abasagua ==4 | (abasagua >=6 & abasagua <=7)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (serhigie >=1 & serhigie <=3)
replace banomejorado_ch = 0 if (serhigie >=4 & serhigie <=5)


gen dorm_ch=.
/*NA*/

gen cuartos_ch=tothabit
replace cuartos_ch=. if tothabit==99

gen cocina_ch=.
/*NA*/

gen telef_ch=(tienfono==1 & tiencelu==1)

gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/

gen cel_ch=(tiencelu==1)

gen vivi1_ch=1 if tipvivie==1
replace vivi1_ch=2 if tipvivie==2
replace vivi1_ch=3 if tipvivie>2


gen vivi2_ch=(tipvivie<=2)

gen viviprop_ch=0 if tenenviv==1
replace viviprop_ch=1 if tenenviv==2
replace viviprop_ch=2 if tenenviv==4
replace viviprop_ch=3 if tenenviv==3 | tenenviv>5
replace viviprop_ch=. if tenenviv==9

gen viviitit_ch=.
/*NA*/

gen vivialq_ch=alqmens1 if viviprop_ch==0 & alqmens1~=9999

gen vivialqimp_ch=alqmens2
replace vivialqimp_ch=. if alqmens2==9999

**********
***raza***
**********
gen raza_ci= .
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* También se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
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


do "$ruta\harmonized\\Labels_Harmonized_DataBank.do"

saveold "`base_out'", replace
log off
log close


