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
local ENCUESTA EH
local ANO "2006"
local ronda m8

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
Versión 2008: do file preparado por Melisa Morales para Suzanne Duryea 
Melisa Morales sugiere chequearlo
María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com - 10 de Octubre de 2013
Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com - 2014
Última actualización: Cesar Lins - Marzo 2021

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

destring _all, replace

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
* Region_c *
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
gen factor_ci=fac15_e
label var factor_ci "Individual Expansion Factor"
******************************
*	idh_ch
******************************
egen idh_ch=group(prov dist corre estra unidad cuest hogar areareco)
label var idh_ch "ID del hogar"
******************************
*	idp_ci
******************************
gen idp_ci=nper
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
gen factorjefe=factor_ci if relacion_ci==1
by idh_ch, sort: egen factor_ch=sum(factorjefe)
/*o con comando bys*/
label var factor_ch "Household Expansion Factor"
drop factorjefe

******************************
*	zona_c
******************************
gen byte zona_c=0 if areareco==2
replace zona_c=1 if areareco==1
label var zona_c "Area of the country"
label define zona_c 1 "Urban" 0 "Rural"
label value zona_c zona_c
******************************
*	pais_c
******************************
gen str3 pais_c="PAN"
label var pais_c "Panama"
******************************
*	anio_c
******************************
gen anio_c=2006
label var anio_c "Year of the survey"
******************************
*	mes_c
******************************
gen mes_c=8
label var mes_c "Month of the survey"
label define mes_c 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
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
label var edad_ci "Age"




******************************
*	civil_ci
******************************
gen civil_ci=.
replace civil_ci=1 if p5==7 | p5==8
replace civil_ci=2 if p5==1 | p5==4
replace civil_ci=3 if p5==2 | p5==3 | p5==5
replace civil_ci=4 if p5==6
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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
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
replace condocup_ci=1 if p10_18 >= 1 & p10_18 <= 5 
replace condocup_ci=2 if  p10_18 == 6 |  p10_18 == 7
replace condocup_ci=3 if  p10_18 >= 8 &  p10_18 <= 17 |  p10_18 == 0
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/
* Alternativa 2: condicionado a la busqueda de empleo. MGD 06/06/2014
gen condocup_ci=.
replace condocup_ci=1 if p10_18>= 1 & p10_18<= 5 
replace condocup_ci=2 if  (p10_18>=6 & p10_18<=9) 
recode condocup_ci .=3 if  edad_ci>=10
recode condocup_ci .=4 if  edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


/*
******************************
*	emp_ci
******************************
gen emp_ci=(p10_18>=1 & p10_18<=5)

******************************
*	desemp1_ci	
******************************
gen desemp1_ci=(emp_ci==0 & p10_18==6)
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo en el periodo de referencia"

******************************
*	desemp2_ci
******************************
gen desemp2_ci=(emp_ci==0 & (p10_18==6 | p10_18==9))
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ultima semana pero esperan respuesta de solicit"

******************************
*	desemp3_ci
******************************
gen desemp3_ci=(emp_ci==0 & (p10_18==6 | p10_18==7 | p10_18==9))
label var desemp3_ci "des2 + no tienen trabajo pero buscaron antes de la semana pasada"

*ver el q ya consiguio tabajo

******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)*/


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
gen horaspri_ci=p43 if p43>0 & p43<99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

******************************
*	horastot_ci
******************************
egen horastot_ci=rsum(p43 p48) if p43>0 & p43<99, missing
replace horastot_ci=horaspri_ci if p48==99 | p43==0
replace horastot_ci=. if (p43==0 & p48==0) | (p43==99 | p48==99)|emp_ci==0
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	desalent_ci
******************************
gen desalent_ci=(emp_ci==0 & p10_18==10)

******************************
*	subemp_ci
******************************
gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & p50==1 & horaspri_ci<=30)

* Alternativa considerando disponibilidad. MGD 06/19/2014
gen subemp_ci1=0
replace subemp_ci1=1 if (emp_ci==1 & p50==1 & horaspri_ci<=30) & (p53==1 | (p54>=1 & p54<=3))

******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci=(p50==2 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"

******************************************************************************
*		LABOR DEMAND
******************************************************************************


******************************
*	ocupa_ci
******************************
* Modificacion MGD 07/22/2014: se utiliza la clasificacion CNO de Panama
g aux_p28=p28
destring aux_p28, replace

g ocupa_ci=.
replace ocupa_ci=1 if (aux_p28>=140 & aux_p28<=662) & emp_ci==1
replace ocupa_ci=2 if (aux_p28>=1 & aux_p28<=139) & emp_ci==1
replace ocupa_ci=3 if (aux_p28>=663 & aux_p28<=817) & emp_ci==1
replace ocupa_ci=4 if ((aux_p28>=880 & aux_p28<=917) | (aux_p28>=1555 & aux_p28<=1565)) & emp_ci==1
replace ocupa_ci=5 if ((aux_p28>=818 & aux_p28<=879) | (aux_p28>=1566 & aux_p28<=1608)) & emp_ci==1
replace ocupa_ci=6 if (aux_p28>=918 & aux_p28<=1000)  & emp_ci==1
replace ocupa_ci=7 if ((aux_p28>=1001 & aux_p28<=1554) | (aux_p28>=1609 & aux_p28<=1649)) & emp_ci==1
replace ocupa_ci=8 if  (aux_p28>=1650 & aux_p28<=1651)& emp_ci==1
replace ocupa_ci=9 if (aux_p28>=1652 & aux_p28<=1653 ) & emp_ci==1

drop aux_p28
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral" 

******************************
*	rama_ci
******************************
gen rama_ci=. 
replace rama_ci=1 if p30>=0111 & p30<=502 
replace rama_ci=2 if p30>=1320 & p30<=1429
replace rama_ci=3 if p30>=1511 & p30<=3720
replace rama_ci=4 if p30>=4010 & p30<=4100
replace rama_ci=5 if p30>=4510 & p30<=4550
replace rama_ci=6 if p30>=5110 & p30<=5530
replace rama_ci=7 if p30>=6010 & p30<=6420
replace rama_ci=8 if p30>=6511 & p30<=7020
replace rama_ci=9 if p30>=7111 & p30<=9900
label var rama_ci "Rama actividad principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*	categopri_ci
******************************
gen categopri_ci=0 if emp_ci==1
replace categopri_ci=1 if p33==8 & emp_ci==1 
replace categopri_ci=2 if p33==7 | p33==9 & emp_ci==1
replace categopri_ci=3 if p33>=1 & p33<=6 & emp_ci==1
*MLO: agregue la condicion que sea ocupado
*puse Miembro de una cooperativa de produccion (p33==9)dentro de cuenta propia

replace categopri_ci=4 if p33==10 & emp_ci==1
*MLO= inclui condicion que sea ocupado
label var categopri_ci "Categoria ocupacional en la actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************************
*	categosec_ci
******************************
gen categosec_ci=0 if emp_ci==1
*p45 

label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label var categosec_ci "Categoria ocupacional en la actividad secundaria"

/******************************
*	contrato_ci
******************************
gen contrato_ci=0 if emp_ci==1
replace contrato_ci=1 if p34>=2 & p34<=4 
label var contrato_ci "Persona empleada que firmo contrato de trabajo"

*con values 0 quedarian individuos sin contrato o trabajadores permanentes

******************************
*	segsoc_ci
******************************
gen segsoc_ci=.
*para mi si se puede generar con p4 */

******************************
*	nempleos_ci
******************************
gen nempleos_ci=0 if emp_ci==1
replace nempleos_ci=1 if p44==3
replace nempleos_ci=2 if p44==1 | p44==2

/******************************
*	firmapeq_ci
******************************
gen firmapeq_ci=0 if emp_ci==1
replace firmapeq_ci=1 if p31==1 
label var firmapeq_ci "1=5 o menos trabajadores"*/
******************************
*	spublico_ci
******************************
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if p33==1
label var spublico_ci "Trabaja en sector publico"
******************************
*	durades_ci
******************************
gen durades_ci=. 
replace durades_ci=0.5 if p21==100
replace durades_ci=p21-200 if p21>=201 /*& p21<=299*/
label var durades_ci "Duracion del desempleo en meses"
******************************
*	antiguedad_ci
******************************
gen m=p40-100 if p40>=100 & p40<=111
gen a=p40-200 if p40>=201 & p40<=299 

gen antiguedad_ci=.
replace antiguedad_ci=a
replace antiguedad_ci=m/12 if a==.
drop a m




******************************
*	ylmpri_ci & ylmpri1_ci
******************************
gen ylmpri_ci=p421 if p421>0 & p421<9999 & categopri_ci==3
replace ylmpri_ci=p423 if p423>0 & p423<9999 & (categopri_ci==1 | categopri_ci==2) 
replace ylmpri_ci=0 if categopri==4
replace ylmpri_ci=. if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario act. principal (mes)"

gen aguin=p55h if p55h>0 & p55h<9999

egen ylmpri1_ci=rsum(ylmpri_ci aguin), missing
replace ylmpri1_ci=. if ylmpri_ci==. & aguin==.
replace ylmpri1_ci=. if emp_ci==0
replace ylmpri1_ci=. if ylmpri_ci==. & (p422==0 | p422==9999)

******************************
*	nrylmpri_ci & nrylmpri1_ci
******************************
gen nrylmpri_ci=(((p421>=9999 & p421<.) | (p423>=9999 & p423<.)) & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0
label var nrylmpri_ci "Identificador de NR de ingreso"
******************************
*	ylmsec_ci
******************************
gen ylmsec_ci=p49 if p49>0 & p49<9999  
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
*	ylm_ci & ylm1_ci
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
label var ylm_ci "Ingreso laboral monetario total"

egen ylm1_ci= rsum(ylmpri1_ci ylmsec_ci), missing

******************************
*	ylnm_ci
******************************
gen ylnmpri_ci=p422 if p422>0 & p422<9999
replace ylnmpri_ci=. if emp_ci==0
gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral no monetario total"

*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario"
***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 

******************************
*	ynlm_ci
******************************
gen jub=p55a if p55a>0 & p55a<9999
gen pension=p55b if p55b>0 & p55b<9999

*2014, 02 modificacion MLO
replace p55c1=. if p55c1==9999
replace p55c2=. if p55c2==9999
egen ayfam=rsum(p55c1 p55c2 p55c3 p55c4 p55c5), missing 

*gen ayfam=p55c1+p55c2+p55c3+p55c4+p55c5 if p55c1<9999 & p55c2 <9999 & p55c3 <9999 & p55c4<9999 & p55c5 <9999  
gen alqui=p55d if p55d>0 & p55d<9999
gen loter=p55e if p55e>0 & p55e<9999
gen becas=p55f if p55f>0 & p55f<9999
gen agro=p55i if p55i>0 & p55i<9999
gen otroy=p55j if p55j>0 & p55j<9999

egen ynlme1= rsum(jub ayfam alqui loter becas agro otroy) if emp_ci==1, missing

egen ynlmd1= rsum(jub ayfam alqui loter becas agro aguin otroy) if emp_ci==0, missing

egen ynlm1_ci=rsum(ynlme1 ynlmd1), missing

egen ynlme= rsum(jub ayfam alqui loter becas otroy) if emp_ci==1, missing

egen ynlmd= rsum(jub ayfam alqui loter becas aguin otroy) if emp_ci==0, missing

egen ynlm_ci=rsum(ynlme ynlmd), missing
label var ynlm_ci "Ingreso no laboral monetario(mes)"

drop jub pension alqui loter becas agro otroy ayfam ynlme ynlmd ynlme1 ynlmd1 aguin

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
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar-ignora NR"

****************************
*    ylmnr_ch & ylmnr1_ch  
****************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, missing

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
*replace ylmnr1_ch=. if nrylmpri1_ch==1

label var ylmnr_ch "Ing laboral monetario del Hogar"

******************************
*	ylnm_ch  
******************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ing laboral no monetario del Hogar - ignora NR" 

******************************
*	remesas_ci & remesas_ch 
******************************
gen remesas_ci=.
gen remesas_ch=.

******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci
by idh_ch, sort: egen ynlm1_ch=sum(ynlm1_ci) if miembros_ci
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
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************
gen autocons_ch=.

******************************
*	rentaimp_ch 
******************************
gen rentaimp_ch=.

******************************
*	ylmhopri_ci & ylmhopri1_ci
******************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal"

gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

******************************
*	ylmho_ci & ylm1ho_ci
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades"

gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)

******************************************************************************
*	VARIABLES OF HOUSEHOLD INFRAESTRUCTURE 
******************************************************************************
notes: Survey de Panama no pregunta caracteristicas de vivienda por lo que no se pueden construir las variables aguared_ch,aguadist_ch,aguamala_ch,aguamide_ch,luz_ch,luzmide_ch,combust_ch,bano_ch,banoex_ch,des1_ch,des2_ch,piso_ch,pared_ch,techo_ch,resid_ch,dorm_ch,cuartos_ch,cocina_ch,telef_ch,refrig_ch,freez_ch,auto_ch,compu_ch,internet_ch,cel_ch,vivi1_ch,vivi2_ch,viviprop_ch,vivitit_ch,vivialq_ch,vivialqimp_ch.
 
gen aguared_ch=.
gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen luz_ch=.
gen luzmide_ch=.
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
gen telef_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
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

notes: variables educativas no consideran educacion especial p8==70, por lo que los missing values de aedu_ci corresponden a p8==70.

******************************
*	aedu_ci
******************************
gen grado=p8-10 if p8>=11 & p8<=16
replace grado=p8-20 if p8>=21 & p8<=26
replace grado=p8-30 if p8>=31 & p8<=33
replace grado=p8-40 if p8>=41 & p8<=49
replace grado=p8-50 if p8>=51 & p8<=53
replace grado=0 if p8==60

gen nivel=0 if p8==60
replace nivel=1 if p8>=11 & p8<=16
replace nivel=2 if p8>=21 & p8<=26
replace nivel=3 if p8>=31 & p8<=33
replace nivel=4 if p8>=41 & p8<=49
replace nivel=5 if p8>=51 & p8<=53

gen aedu_ci=0 if nivel==0
replace aedu_ci=grado if nivel==1
replace aedu_ci=grado+6 if nivel==2 | nivel==3
replace aedu_ci=grado+12 if nivel==4 | nivel==5

*replace aedu_ci=0 if edad_ci<5

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
gen asiste_ci=(p7==1)
replace asiste_ci=. if p7==0
label var asiste "Personas que actualmente asisten a centros de enseñanza"

******************************
*	pqnoasis_ci
******************************
gen pqnoasis_ci=p7a if p7a>0
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

g       pqnoasis1_ci = 1 if p7a==3
replace pqnoasis1_ci = 2 if p7a==2
replace pqnoasis1_ci = 3 if p7a==7
replace pqnoasis1_ci = 4 if p7a==5
replace pqnoasis1_ci = 5 if p7a==4 | p7a==6
replace pqnoasis1_ci = 7 if p7a==8
replace pqnoasis1_ci = 8 if p7a==1 | p7a==9
replace pqnoasis1_ci = 9 if p7a==10

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

* PAN 2006

gen salmm_ci= . /*264.14*/
replace salmm_ci= 247.2 if rama_ci==1
replace salmm_ci= 295.2 if rama_ci==2
replace salmm_ci= 260 if rama_ci==3
replace salmm_ci= 288 if rama_ci==4
replace salmm_ci= 359.2 if rama_ci==5
replace salmm_ci= 267.73 if rama_ci==6
replace salmm_ci= 283.47 if rama_ci==7
replace salmm_ci= 325.6 if rama_ci==8
replace salmm_ci= 309.6 if rama_ci==9
replace salmm_ci= 292.89 if salmm_ci==.

label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********
destring areareco dist, replace
gen lp_ci =.
replace lp_ci= 67.13 if areareco==1 & dist==1 /* Cdad. Panamá*/
replace lp_ci= 67.13 if areareco==1 & dist==3 /* Zona urbana districto san miguelito*/
replace lp_ci= 74.12 if ((dist!=1 & dist!=3) & areareco==1) | areareco==2  /* resto urbano o rural*/


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =.
replace lpe_ci= 32.65 if areareco==1 & dist==1 /* Cdad. Panamá*/
replace lpe_ci= 32.65 if areareco==1 & dist==3 /* Zona urbana districto san miguelito*/
replace lpe_ci= 33.17 if ((dist!=1 & dist!=3) & areareco==1) | areareco==2  /* resto urbano o rural*/

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
replace afiliado_ci =1 if p4==1  /* afiliado directo*/
recode afiliado_ci .=0 if p10_18 >= 1 & p10_18 <= 7
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
replace tipocontrato_ci=1 if p34==1 | p34==4 & categopri_ci==3
replace tipocontrato_ci=2 if (p34==2 | p34==3) & categopri_ci==3
replace tipocontrato_ci=3 if (p34==5 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci



*************
*cesante_ci* 
*************
* MGD 12/4/2015: se corrige la inclusion de ceros con p26 >100. Antes solamnte se incluia a los missings ==999
gen cesante_ci=1 if p26>100 & p26!=999 & condocup_ci==2
*gen cesante_ci=1 if p26==999 
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
gen tamemp_ci=1 if p31==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p31==2 | p31==3 | p31==4
*Empresas grandes
replace tamemp_ci=3 if p31==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw= fac15_e]

*************
*categoinac_ci
*************

gen categoinac_ci=1 if p10_18==11 | p10_18==12
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if p10_18==13
*Quehaceres del Hogar
replace categoinac_ci=3 if p10_18==14
*Otra razon
replace categoinac_ci=4 if p10_18==15 | p10_18==16 | p10_18==17
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo

*************
**pension_ci*
*************
* 2014, 02 MLO modificacion cambio de missing en 99999
replace p55a=. if p55a==9999
replace p55b=. if p55b==9999
egen aux_p=rsum(p55a p55b), missing
gen pension_ci=1 if aux_p>0 & aux_p!=. & aux_p!=999999
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

egen ypen_ci=rsum(p55a p55b), missing
*replace ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen byte pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p8==31 | p8==32| p8==33 
recode tecnica_ci .=0
label var tecnica_ci "1=formacion terciaria tecnica"

	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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



compress


saveold "`base_out'", replace


log close






