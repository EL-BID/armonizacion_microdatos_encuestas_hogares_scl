* (Versión Stata 12)
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

local PAIS PAN
local ENCUESTA EHPM
local ANO "2012"
local ronda m3

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Panama
Encuesta: EH
Round: Agosto
Autores: 
Versión 2014:
Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Última versión: Cesar Lins (SCL/GDI) - Marzo 2021

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c



******************************************************************************
*	HOUSEHOLD and DEMOGRAPHIC VARIABLES
******************************************************************************

************
* region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

destring prov, replace
gen region_c=  prov

label define region_c  ///
1	"Bocas del Toro" ///
2	"Coclé" ///
3	"Colón" ///
4	"Chiriquí" ///
5	"Darién" ///
6	"Herrera" ///
7	"Los Santos" ///
8	"Panamá" ///
9	"Veraguas" ///
10	"Kuna Yala" ///
11	"Emberá" ///
12	"Ngäbe-Buglé"		  
label value region_c region_c
label var region_c "División política, provincias"

******************************
*	factor_ci
******************************

gen factor_ci= fac15_e   
label var factor_ci "Factor de expansion del individuo"

	***************
	***upm_ci***
	***************
gen upm_ci=unidad
	***************
	***estrato_ci***
	***************
gen estrato_ci=estra


******************************
*	idh_ch
******************************
sort llave_sec prov estra unidad cuest hogar
egen idh_ch = group(llave_sec prov estra unidad cuest hogar)
label var idh_ch "ID del hogar"


******************************
*	idp_ci
******************************

gen idp_ci = nper
label var idp_ci "ID de la persona en el hogar"

******************************
*	relacion_ci
******************************
gen relacion_ci=.
replace relacion_ci=1 if p1==1
replace relacion_ci=2 if p1==2
replace relacion_ci=3 if p1==3
replace relacion_ci=4 if p1==4 
replace relacion_ci=5 if p1==6
replace relacion_ci=6 if p1==5
label var relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
label value relacion_ci relacion_ci


******************************
*	factor_ch
******************************
*gen factorjefe=factor_ci if relacion_ci==1
*by idh_ch, sort: egen factor_ch=sum(factorjefe)
g factor_ch= fac_hog_e
label var factor_ch "Factor de expansion del hogar"

******************************
*	zona_c
******************************
destring areareco, replace 
gen zona_c=0 if areareco==2
replace zona_c=1 if areareco==1
label var zona_c "Zona del pais"
label define zona_c 1 "Urban" 0 "Rural"
label value zona_c zona_c

******************************
*	pais_c
******************************
gen str3 pais_c="PAN"
label var pais_c "Pais"

******************************
*	anio_c
******************************
gen anio_c=2012
label var anio_c "Year of the survey"

******************************
*	mes_c
******************************
gen mes_c=3
label var mes_c "Month of the survey"
label value mes_c mes_c

******************************
*	sexo_ci
******************************
gen sexo_ci=p2
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

******************************
*	edad_ci
******************************
gen edad_ci=p3
label var edad_ci "Edad del individuo"



******************************
*	civil_ci
******************************

gen civil_ci=.
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci estcivil_ci

******************************
*	jefe_ci
******************************
gen jefe_ci=(relacion_ci==1)
label var jefe_ci "Jefe de hogar"

***************************************************************************
*	nconyuges_ch & nhijos_ch & notropari_ch & notronopari_ch & nempdom_ch
****************************************************************************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label var nconyuges_ch "Numero de conyuges"
label var nhijos_ch "Numero de hijos"
label var notropari_ch "Numero de otros familiares"
label var notronopari_ch "Numero de no familiares"
label var nempdom_ch "Numero de empleados domesticos"

******************************
*	clasehog_ch
******************************
gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0)   
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.)
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<.
label var clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

***************************************************************************************
*	nmiembros_ch & nmayor21_ch & nmenor21_ch & nmayor65_ch & nmenor6_ch & nmenor1_ch  
***************************************************************************************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label var nmiembros_ch "Numero de familiares en el hogar"

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
gen miembros_ci=(relacion_ci<5)
label var miembros_ci "Miembro del hogar"

				
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

	***************
	*** afroind_ci ***
	***************
**Pregunta: ¿Se considera usted indígena? (indi_rec) (1 - no indígena; 2 - indígena)
**No se identifica a personas afrodescendientes. Todos los no-indígenas se categorizan como "otro". 
**En el 2011 se convierte en la EHPM (no solo EH) 

gen afroind_ci=. 
replace afroind_ci=1 if indi_rec==2
replace afroind_ci=2 if indi_rec==0
replace afroind_ci=3 if indi_rec==1


	***************
	*** afroind_ch ***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = sum(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	*** afroind_ano_c ***
	*******************
gen afroind_ano_c=2001

	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 

	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 


******************************************************************************
*	LABOR MARKET
******************************************************************************

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if p08_16 >= 1 & p08_16 <= 5 
replace condocup_ci=2 if  p08_16 == 6 |  p08_16 == 7 
replace condocup_ci=3 if  p08_16 >= 10 &  p08_16 <= 17 |  p08_16 == 0 |  p08_16 == 9 | p08_16 == 8
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
ta condocup_ci [w=factor_ci], missing
*/
* Alternativa 2: condicionado a la busqueda de empleo. MGD 06/06/2014
gen condocup_ci=.
replace condocup_ci=1 if p08_16 >= 1 & p08_16 <= 5 
replace condocup_ci=2 if  (p08_16>=6 & p08_16<=9) 
recode condocup_ci .=3 if  edad_ci>=10
recode condocup_ci .=4 if  edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


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


******************************
*	horaspri_ci
******************************
gen horaspri_ci=p37 if p37>0 & p37<99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

******************************
*	horastot_ci
******************************
g horastot_ci=p37 if p37>0 & p37<99
replace horastot_ci=. if (p37==99)| emp_ci==0
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	desalent_ci
******************************

generat desalent_ci = 1  if  p08_16==10
replace desalent_ci = 0  if p08_16!=10
replace desalent_ci =.   if p08_16==.

******************************
*	subemp_ci
******************************
gen subemp_ci=.
* no esta la pregunta si desea trabajar mas horas en el cuestionario
/*
gen subemp_ci=(emp_ci==1 & p50==1 & horastot_ci<=30)
replace subemp_ci=. if emp_ci==0 | emp_ci==.
*/
******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci=.
/*gen tiempoparc_ci=(emp_ci==1 & p50==2 & horastot_ci<=30)
replace tiempoparc_ci=. if emp_ci==0 | emp_ci==.*/
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"


******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************
* Modificacion MGD 07/22/2014: se utiliza la clasificacion CIUO-08
gen aux_p26=substr(p26,1,4)
destring aux_p26, replace

g ocupa_ci=.
replace ocupa_ci=1 if (aux_p26>=2110 & aux_p26<=3999) & emp_ci==1
replace ocupa_ci=2 if (aux_p26>=1110 & aux_p26<=1999) & emp_ci==1
replace ocupa_ci=3 if (aux_p26>=4110 & aux_p26<=4999) & emp_ci==1
replace ocupa_ci=4 if ((aux_p26>=5211 & aux_p26<=5249) | (aux_p26>=9510 & aux_p26<=9522)) & emp_ci==1
replace ocupa_ci=5 if ((aux_p26>=5111 & aux_p26<=5169) | (aux_p26>=5311 & aux_p26<=5999) | (aux_p26>=9111 & aux_p26<=9129) | (aux_p26>=9611 & aux_p26<=9624)) & emp_ci==1
replace ocupa_ci=6 if ((aux_p26>=6110 & aux_p26<=6341) | (aux_p26>=9211 & aux_p26<=9216)) & emp_ci==1
replace ocupa_ci=7 if ((aux_p26>=7110 & aux_p26<=8999) | (aux_p26>=9311 & aux_p26<=9412)) & emp_ci==1
replace ocupa_ci=8 if  (aux_p26>=110 & aux_p26<=310)& emp_ci==1
replace ocupa_ci=9 if (aux_p26==9629 | aux_p26==9999 ) & emp_ci==1

drop aux_p26
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral" 


******************************
*	rama_ci
******************************
destring p28, replace
gen rama_ci=. 
replace rama_ci=1 if (p28>=111 & p28<=322)  & emp_ci==1
replace rama_ci=2 if (p28>=510 & p28<=990) & emp_ci==1
replace rama_ci=3 if (p28>=1010 & p28<=3320) & emp_ci==1 
replace rama_ci=4 if (p28>=3510 & p28<=3900) & emp_ci==1
replace rama_ci=5 if (p28>=4100 & p28<=4390) & emp_ci==1
replace rama_ci=6 if ((p28>=4510 & p28<=4820) | (p28>=5510 & p28<=5632))& emp_ci==1
replace rama_ci=7 if ((p28>=4911 & p28<=5330) | (p28>=6110 & p28<=6190)) & emp_ci==1
replace rama_ci=8 if (p28>=6411 & p28<=6822) & emp_ci==1
replace rama_ci=9 if ((p28>=5811 & p28<=6022) | (p28>=6201 & p28<=6399) | (p28>=6910 & p28<=9910)) & emp_ci==1

label var rama_ci "Rama actividad principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*	categopri_ci
******************************
gen categopri_ci=0     if                      emp_ci==1
replace categopri_ci=1 if p31==8             & emp_ci==1
replace categopri_ci=2 if (p31==7 | p31==9)  & emp_ci==1
replace categopri_ci=3 if (p31>=1 & p31<=6 ) & emp_ci==1
replace categopri_ci=4 if p31==10            & emp_ci==1
label var categopri_ci "Categoria ocupacional en la actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************************
*	categosec_ci
******************************
* En este año no se detalla esta información en la actividad secundaria
gen categosec_ci=.
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label var categosec_ci "Categoria ocupacional en la actividad secundaria"

******************************
*	nempleos_ci
******************************
destring p38, replace
gen nempleos_ci=0     if emp_ci==1
replace nempleos_ci=1 if p38==3
replace nempleos_ci=2 if p38==1 | p38==2

******************************
*	spublico_ci
******************************
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if p31==1 & emp_ci==1
replace spublico_ci=. if emp_ci==0
label var spublico_ci "Trabaja en sector publico"

******************************
*	durades_ci
******************************
*  MGD 04/02/2015: para el codigo p19=100 que se refiere a menos de un mes, se codifica como 0.5 meses
gen durades_ci=. 
replace durades_ci=0.5 if p19==100
replace durades_ci=p19-200 if p19>=201 /*& p21<298*/
label var durades_ci "Duracion del desempleo en meses"

******************************
*	antiguedad_ci
******************************
destring p34, replace force
gen m=p34-100 if p34>=100 & p34<=111
gen a=p34-200 if p34>=201 & p34<=299 

gen antiguedad_ci=.
replace antiguedad_ci=a
replace antiguedad_ci=m/12 if a==.
drop a m

******************************************************************************
*		INCOME
******************************************************************************
*Modificación Mayra Saenz Mayo 2014
foreach var of varlist _all {
qui destring `var', replace
qui capture recode `var' (99999=.) (999=.) (9999=.) (99998=.)
}
******************************
*	ylmpri_ci 
******************************
destring p361 p362 p363 p39, replace
generat ylmpri_ci=p361 if p361>0 & p361<999998 & categopri_ci==3
replace ylmpri_ci=p363 if p363>0 & p363<999998 & (categopri_ci==1 | categopri_ci==2) 
replace ylmpri_ci=0    if categopri==4
replace ylmpri_ci=.    if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario act. principal (mes)"

******************************
*	nrylmpri_ci 
******************************
gen nrylmpri_ci=(((p361>=99998 & p361<.) | (p363>=99998 & p363<.)) & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0
label var nrylmpri_ci "Identificador de NR de ingreso"

******************************
*	ylmsec_ci
******************************
gen ylmsec_ci=p39 if p39>0 & p39<99999  
replace ylmsec_ci=. if emp_ci==0
label var ylmsec_ci "Ingreso laboral monetario act secundaria (mes)"


****************
***ylnmsec_ci***
****************

g ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


******************************
*	ylm_ci 
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
label var ylm_ci "Ingreso laboral monetario total"



******************************
*	ylnm_ci
******************************

egen ylnmpri_ci=rsum(p362 p364), missing
replace ylnmpri_ci=. if emp_ci==0
gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral no monetario total"

******************************
*	ynlm_ci
******************************
destring p72a p72b p72c1 p72c2 p72d p72e p72f1 p72f2 p72f3 p72f4 p72g1 p72g2 p72g4 p72g5 p72h p72i p72j p72k p72l p72m, replace

gen jub=p72a if p72a>0 & p72a<999999


egen ynlme= rsum(p72a p72b p72c1 p72c2 p72d p72e p72f1 p72f2 p72f3 p72f4 p72g1 p72g2 p72g4 p72g5 p72h p72i p72j p72k p72l p72m) if emp_ci==1, missing
egen ynlmd= rsum(p72a p72b p72c1 p72c2 p72d p72e p72f1 p72f2 p72f3 p72f4 p72g1 p72g2 p72g4 p72g5 p72h p72i p72j p72k p72l p72m) if emp_ci==0, missing
egen ynlm_ci=rsum(ynlme ynlmd), missing
label var ynlm_ci "Ingreso no laboral monetario(mes)"


******************************
*	nrylmpri_ch
******************************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
label var nrylmpri_ch "Identificador de hogares donde miembro NS/NR ingreso"

******************************
*	ylm_ch & ylm1_ch 
******************************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar-ignora NR"

****************************
*    ylmnr_ch & ylmnr1_ch  
****************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
label var ylmnr_ch "Ing laboral monetario del Hogar"

******************************
*	ylnm_ch  
******************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ing laboral no monetario del Hogar - ignora NR" 

*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
destring p72c3 p72c4 p72c5 p72c6 p72c6_a p72g3, replace
egen ynlnm_ci=rsum(p72c3 p72c4 p72c5 p72c6 p72c6_a p72g3), missing
label var ynlnm_ci "Ingreso no laboral no monetario"
***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing 
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

******************************
*	remesas_ci & remesas_ch 
******************************
destring p55_total, replace
replace p55_total=. if p55_total==99998 

gen remesas_ci=.
replace remesas_ci= p55_total/12 if p53_dinero == 1


by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing 
label var remesas_ch "Remesas del hogar"


******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci
label var ynlm_ch "Ingreso no laboral monetario del Hogar" 

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.
******************************
*	autocons_ci 
******************************
*Revisar cómo se codifica esta especificación que consta en el cuestionario: 
/*b. Investigue el ingreso neto (entradas menos gastos en la
actividad). En el caso del sector agropecuario, pregunta
sobre autoconsumo y/o autosuministro.*/
*p36
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci

******************************
*	rentaimp_ch 
******************************
gen rentaimp_ch=.

******************************
*	ylmhopri_ci & ylmhopri1_ci
******************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal"


******************************
*	ylmho_ci & ylm1ho_ci
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades"



******************************************************************************
*	VARIABLES OF HOUSEHOLD INFRAESTRUCTURE 
******************************************************************************
gen aguared_ch=.
gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen luz_ch= (h2a_luz_el==1)
gen luzmide_ch=( h5a_serv_i==1 | h5b_serv_i==1)
gen combust_ch=.
gen bano_ch=.
gen banoex_ch=.
gen des1_ch=.
gen des2_ch=.
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=. 
**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**	
gen aguamejorada_ch = .
gen  banomejorado_ch = .  
gen dorm_ch=.
gen cuartos_ch=.
gen cocina_ch=.
gen telef_ch= (h2c_telefo==1)
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=(h4_computa==1)
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch=.
gen vivi2_ch=.
gen viviprop_ch=.
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=.

******************************************************************************
*	EDUCATION
******************************************************************************

******************************
*	aedu_ci
******************************
destring p6, replace
generat grado=p6-10 if p6>=11 & p6<=16
replace grado=p6-20 if p6>=21 & p6<=23
replace grado=p6-30 if p6>=31 & p6<=36
replace grado=p6-40 if p6>=41 & p6<=45
replace grado=p6-50 if p6>=51 & p6<=57
replace grado=p6-60 if p6>=61 & p6<=62
replace grado=p6-70 if p6>=71 & p6<=73
replace grado=p6-80 if p6>=81 & p6<=84
replace grado=0 if p6 == 1 | p6 == 2 | p6 == 3

gen nivel=0 
replace nivel=1 if p6>=11 & p6<=16
replace nivel=2 if p6>=21 & p6<=23
replace nivel=3 if p6>=31 & p6<=36
replace nivel=4 if p6>=41 & p6<=45
replace nivel=5 if p6>=51 & p6<=57
replace nivel=6 if p6>=61 & p6<=62
replace nivel=7 if p6>=71 & p6<=73
replace nivel=8 if p6>=81 & p6<=84

gen aedu_ci=0            if nivel==0 
replace aedu_ci=grado    if nivel==1
replace aedu_ci=grado+6  if nivel==2 | nivel==3
replace aedu_ci=grado+12 if (nivel==4 | nivel==5) & grado<=7
replace aedu_ci=grado+17 if nivel>=6 & nivel<=8



******************************
*	eduno_ci
******************************
gen eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label var eduno_ci "Personas sin educacion"

******************************
*	edupi_ci
******************************
gen edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label var edupi_ci "Personas que no han completado Primaria"

******************************
*	edupc_ci
******************************
gen edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label var edupc_ci "Primaria Completa"

******************************
*	edusi_ci
******************************
gen edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edupc_ci=. if aedu_ci==.
label var edusi_ci "Secundaria Incompleta"

******************************
*	edusc_ci
******************************
gen edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label var edusc_ci "Secundaria Completa"

******************************
*	edus1i_ci
******************************
gen edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"

******************************
*	edus1c_ci
******************************
gen edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label var edus1c_ci "1er ciclo de Educacion Secundaria Completo"

******************************
*	edus2i_ci
******************************
gen edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
label var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

******************************
*	edus2c_ci
******************************
gen edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media

******************************
*	eduui_ci
******************************
gen eduui_ci=(aedu_ci>12 & aedu_ci<17) 
replace eduui_ci=. if aedu_ci==.
label var eduui_ci "Universitaria o Terciaria Incompleta"

******************************
*	eduuc_ci
******************************
gen eduuc_ci=(aedu_ci>=17)
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "Universitaria o Terciaria Completa"

******************************
*	edupre_ci
******************************
gen edupre_ci=.
label var edupre_ci "Educacion preescolar"
notes: la encuesta no tiene codigo de educacion preescolar 

******************************
*	asispre_ci
******************************
gen asispre_ci=.
label var asispre_ci "Asistencia a Educacion preescolar"
notes: la encuesta no tiene codigo de educacion preescolar 

******************************
*	eduac_ci
******************************
gen eduac_ci=.
replace eduac_ci=0 if nivel==5
replace eduac_ci=1 if nivel==4
label var eduac_ci "Educ terciaria academica vs Educ terciaria no academica"

******************************
*	asiste_ci
******************************
gen asiste_ci=(p5==1)
replace asiste_ci=. if p5==0
label var asiste "Personas que actualmente asisten a centros de enseñanza"

******************************
*	pqnoasis_ci_ci
******************************
gen pqnoasis_ci=p5a if p5a>0
label var pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "No se ofrece el nivel o grado escolar en la comunidad" 2 "Necesita trabajar",add
label define pqnoasis_ci 3 "Falta de recursos económicos" 4 "Quehaceres domesticos", add 
label define pqnoasis_ci 5 "Falta de interes" 6 "Embarazo" 7 "Enfermedad" , add
label define pqnoasis_ci 8 "No tiene la edad requerida" 9 "Está muy distante" 10 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p5a==3
replace pqnoasis1_ci = 2 if p5a==2
replace pqnoasis1_ci = 3 if p5a==7
replace pqnoasis1_ci = 4 if p5a==5
replace pqnoasis1_ci = 5 if p5a==4 | p5a==6
replace pqnoasis1_ci = 7 if p5a==8
replace pqnoasis1_ci = 8 if p5a==1 | p5a==9
replace pqnoasis1_ci = 9 if p5a==10

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

gen edupub_ci=.
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"

******************************
*	repiteult_ci  & repite_ci
******************************
gen repiteult_ci=.
gen repite_ci=.
*NA
drop nivel grado

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

* PAN 2012

gen salmm_ci= . /*461*/
replace salmm_ci= 332.8 if rama_ci==1
replace salmm_ci= 513.6 if rama_ci==2
replace salmm_ci= 387.6 if rama_ci==3
replace salmm_ci= 566.4 if rama_ci==4
replace salmm_ci= 552 if rama_ci==5
replace salmm_ci= 407.4 if rama_ci==6
replace salmm_ci= 478.8 if rama_ci==7
replace salmm_ci= 553.8 if rama_ci==8
replace salmm_ci= 490.62 if rama_ci==9
replace salmm_ci= 475.89 if salmm_ci==.

label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********
destring areareco, replace
gen lp_ci =.

/*replace lp_ci= . if areareco=="U" & dist==1 /* Cdad. Panamá*/
replace lp_ci= . if areareco=="U" & dist==3 /* Zona urbana districto san miguelito*/
replace lp_ci= . if ((dist!=1 & dist!=3) & areareco=="U") | areareco=="R"  /* resto urbano o rural*/
*/

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
/*replace lpe_ci= . if areareco=="U" & dist==1 /* Cdad. Panamá*/
replace lpe_ci= . if areareco=="U" & dist==3 /* Zona urbana districto san miguelito*/
replace lpe_ci= . if ((dist!=1 & dist!=3) & areareco=="U") | areareco=="R"  /* resto urbano o rural*/
*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci =1 if p4==1  /* afiliado directo */
recode afiliado_ci .=0 if condocup_ci==1 | condocup_ci==2
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
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p32==1 | p32==4) & categopri_ci==3
replace tipocontrato_ci=2 if (p32==2 | p32==3) & categopri_ci==3
replace tipocontrato_ci=3 if (p32==5 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************
* MGD 12/4/2015: se corrige la inclusion de ceros con p24 >100 ya que antes solamnte constaba p24!=999
gen cesante_ci=1 if p24>100 & p24!=999 & p24!=. & condocup_ci==2
recode cesante_ci .=0 if condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"		

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

*************
*tamemp_ci
*************
gen tamemp_ci=1 if p29==1 & emp_ci==1
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if (p29==2 | p29==3 | p29==4  )& emp_ci==1
*Empresas grandes
replace tamemp_ci=3 if p29==5  & emp_ci==1
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw= fac15_e]

gen tamemp_o=1 if (p29==1 | p29==2)
 *Empresas medianas
replace tamemp_o=2 if (p29==3 | p29==4  )
*Empresas grandes
replace tamemp_o=3 if p29==5  
label var  tamemp_o "Tamaño de Empresa-OECD"
label define tamemp_o 1"[1-9]" 2"[10-49]" 3"[50 y mas]"
label values tamemp_o tamemp_o


*************
*categoinac_ci
*************
gen categoinac_ci=1 if p08_16==11 | p08_16==12
*Estudiantes
replace categoinac_ci=2 if p08_16==13
*Quehaceres del Hogar
replace categoinac_ci=3 if p08_16==14
*Otra razon
replace categoinac_ci=4 if p08_16==15 | p08_16==16 | p08_16==17
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
label var  categoinac_ci "Condición de Inactividad" 

*************
**pension_ci*
*************

replace p72a =. if p72a==.
replace p72b =. if  p72b==.
egen aux_p=rsum(p72a p72b), missing
destring aux_p, replace
gen pension_ci=1 if aux_p>0 & aux_p!=. & aux_p!=99999
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=aux_p

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=1 if p72g5>0 & p72g5<=99999
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
* este año se incluye programa 100 a los 70
gen ypensub_ci=p72g5 if p72g5>0 & p72g5<=99999
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
*tecnica_ci**
*************

gen tecnica_ci=.
* falta generar
label var tecnica_ci "1=formacion terciaria tecnica"




	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
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




compress


saveold "`base_out'", replace


log close



