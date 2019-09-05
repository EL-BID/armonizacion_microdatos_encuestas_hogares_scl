**************************************
****** Last update: July, 2006  ****** 
**************************************

/*

Instituto Nacional de Estadística, Encuesta Permanente de Hogares de Propósitos Múltiples. May, 2005.

All indicators presented as part of this research initiative are estimates 
based on microdata from sample based household surveys. Hence, they ARE NOT
IDB OFFICIAL STATISTICS, are bound to have an implicit sampling error, and 
are not strictly comparable with estimates from other data sources 
(census and administrative records). Official indicators should be obtained 
directly from each country’s statistical agency.

Comments are welcomed at eqxis-bid@iadb.org
*/

* The following Do File is provided to give users the possibility to replicate
* calculations of indicators presented in this system using the same methodology,
* variables and other relevant criteria. The researcher that prepared this Do File
* is solely responsible for its contents.

/*
************************************************************************************
//////////////////////////////////////INDEX/////////////////////////////////////////
   
   The following do file is divided in these sections:
   
   	- Merge of the original databases files in a unique database.
	- Relevant variables created for the disaggregation of indicators:
		a) Urban/Rural areas (if available)
		b) Years of education 
		c) Income or consumption quintiles
		d) Economically Active Population
		e) Ethnic identification (if available)
	- Definition of the survey design (stratum and primary sample units).
	- Calculation of the indicators, including the questions of the survey used  
	  and all their categories. 

//////////////////////////////////////INDEX/////////////////////////////////////////
************************************************************************************
*/

************************************************************************************
************************************************************************************

* MERGE
/*
  Not available(one file database)
*/

************************************************************************************
************************************************************************************

*********************
*** HONDURAS 2004 ***
*********************

set more off
set linesize 255
set mem 100m

use "F:\EQxIS\BASES DE DATOS\HONDURAS\hnd04-may.dta", clear

log using "F:\EQxIS\LOGS\HND04.log", replace

/*
rela_j
 1. jefe del hogar
 2. esposa(o) o compañera(o) 
 3. hijos
 4. hijastros 
 5. padres 
 6. hermanos 
 7. yernos y nueras 
 8. otros parientes 
 9. otros no parientes 
 10. servicio domestico
*/

* Variables

 rename rela_j parentco
 rename ur area
 rename p01 alfabet
 rename p02 asiste
 rename p05a nivasiste
 rename p05b grado

 tab area [iw=factor]

* Gender classification of the population refering to the head of the household.

 sort hogar nper

 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(hogar)

 tab sexo   [iw=factor]
 tab sexo_d [iw=factor]
 
 tab sexo sexo_d if parentco==1

 sort hogar nper

* Dwelling ID

 gen str7 hogar_s=string(hogar)
 gen id_viv=substr(hogar_s,1,6)

** Years of education. 
/*

2. ¿Asiste actualmente a algún centro educativo?
1. SI ==> 5

4. ¿Cuál es el nivel y año educativo más alto que aprobó?

Nivel(p04a)				
 1. Ninguno			
 2. Centro de Alfabetización	
 3. Pre-Escolar			
 4. Primaria			
 5. Ciclo común			
 6. Secundaria Diversificado	
 7. Técnico Superior		
 8. Superior Universitario	
 9. Post-grado			
99. No sabe			

Año Educativo (p04b)

5. ¿Qué nivel y año educativo cursa actualmente?

Nivel(p05a)				
 2. Centro de Alfabetización	 
 3. Pre-Escolar			 
 4. Primaria			 
 5. Ciclo común			 
 6. Secundaria Diversificado	 
 7. Técnico Superior		 
 8. Superior Universitario	 
 9. Post-grado			 
99. No sabe			 

Año Educativo (p05b)

*/
* Included in the database

 rename anosest a_orig

 gen	 anoest=0  if (p04a==1 | p04a==2 | p04a==3) | (nivasiste==2 | nivasiste==3) | (nivasiste==4 & grado==1)
 replace anoest=1  if (p04a==4 & p04b==1) 		       | (nivasiste==4 & grado==2)
 replace anoest=2  if (p04a==4 & p04b==2) 		       | (nivasiste==4 & grado==3)
 replace anoest=3  if (p04a==4 & p04b==3) 		       | (nivasiste==4 & grado==4)
 replace anoest=4  if (p04a==4 & p04b==4) 		       | (nivasiste==4 & grado==5)
 replace anoest=5  if (p04a==4 & p04b==5) 		       | (nivasiste==4 & grado==6)
 replace anoest=6  if (p04a==4 & p04b==6) 		       | (nivasiste==4 & grado==7) | (nivasiste==5 & grado==1)
 replace anoest=7  if (p04a==4 & p04b==7) | (p04a==5 & p04b==1) | (nivasiste==4 & grado==8) | (nivasiste==5 & grado==2)
 replace anoest=8  if (p04a==4 & p04b==8) | (p04a==5 & p04b==2) | (nivasiste==4 & grado==9) | (nivasiste==5 & grado==3)
 replace anoest=9  if (p04a==4 & p04b==9) | (p04a==5 & p04b==3) | (nivasiste==6 & grado==1) | (nivasiste==7 & grado==1)
 replace anoest=10 if (p04a==6 & p04b==1) | (p04a==7 & p04b==1) | (nivasiste==6 & grado==2) | (nivasiste==7 & grado==2)
 replace anoest=11 if (p04a==6 & p04b==2) | (p04a==7 & p04b==2) | (nivasiste==6 & grado==3) | (nivasiste==7 & grado==3)
 replace anoest=12 if (p04a==6 & p04b==3) | (p04a==7 & p04b==3) | (nivasiste==6 & grado==4) | (nivasiste==8 & grado==1)
 replace anoest=13 if (p04a==6 & p04b==4) | (p04a==8 & p04b==1) | (nivasiste==8 & grado==2)
 replace anoest=14 if (p04a==8 & p04b==2) | (nivasiste==8 & grado==3)
 replace anoest=15 if (p04a==8 & p04b==3) | (nivasiste==8 & grado==4)
 replace anoest=16 if (p04a==8 & p04b==4) | (nivasiste==8 & grado==5)
 replace anoest=17 if (p04a==8 & p04b==5) | (nivasiste==8 & grado==6) | (nivasiste==9 & grado==1) 
 replace anoest=18 if (p04a==8 & p04b==6) | (p04a==9 & p04b==1) | (nivasiste==8 & grado==7) | (nivasiste==9 & grado==2) 
 replace anoest=19 if (p04a==8 & p04b==7) | (p04a==9 & p04b==2) | (nivasiste==8 & grado==8) | (nivasiste==9 & grado==3) 
 replace anoest=20 if (p04a==8 & p04b==8) | (p04a==9 & p04b==3) | (nivasiste==9 & grado==4) 
 replace anoest=21 if (p04a==9 & p04b==4)

**********************
** INCOME QUINTILES **
**********************

* yperhg => Ingreso mensual del hogar percapita

 egen double ypcapita=max(yperhg) if yperhg>0, by(hogar) 

 rename quintil quint_orig /* Quintil ingreso del hogar */
 egen  quintil_orig=max(quint_orig), by(hogar) 

 sum quintil_orig ypcapita
 sum quintil_orig if ypcapita==.

** Equal size quintiles

* National
 gen vectoruno=1

 sort ypcapita
 gen double ypcapita_ord=sum(vectoruno) if ypcapita!=.
 sort hogar nper

* Urban Rural
 generate double ypcurbano=. 
 replace ypcurbano= ypcapita  if area==1
 generate double ypcrural=.
 replace ypcrural=  ypcapita  if area==2

 sort ypcurbano
 gen double ypcurbano_ord=sum(vectoruno) if ypcurbano!=.
 sort hogar nper

 sort ypcrural
 gen double ypcrural_ord=sum(vectoruno) if ypcrural!=.
 sort hogar nper

** Xtile

 xtile quintil=ypcapita_ord    [w=factor], nq(5)
 xtile quintil_2=ypcapita      [w=factor], nq(5)
 xtile qtlurbano=ypcurbano_ord [w=factor], nq(5) 
 xtile qtlrural= ypcrural_ord  [w=factor], nq(5)

 format ypcapita* ypcurbano ypcrural %15.0g 

 gen     quinarea=qtlurbano if area==1
 replace quinarea=qtlrural  if area==2

 count
 sum quin*
 sum ypc*

 tab quintil [iw=factor]
 tab quintil quintil_2

 sort hogar nper

** Economic Active Population  (10 years or more of age)
* Included in the database
* condact
/* Condición de Actividad
 1. Ocupados	
 2. Desocupados
 3. Inactivos
*/

 gen	 TASADESO=0 if condact==1
 replace TASADESO=1 if condact==2

********************
*** Strata & PSU ***
********************

* The definition of the survey design for the svymean commands
* is based on the available survey's sample design documentation, 
* and the variables available in the database, therefore the following 
* specification might be an approximation.

* Domains

/*
Strata

Domi (vs. Dominio)
 1. Tegucigalpa 
 2. San pedro sula
 3. Ciudades medianas
 4. Ciudades pequeñas
 5. Rural 
 */

 gen str1 estrato1=substr(hogar_s,2,1)
 destring estrato1, replace

 gen	 strata=domi if domi<5
 replace strata=5 if domi==5 & estrato1==0
 replace strata=6 if domi==5 & estrato1==1
 
* PSU

 gen str5 psu=substr(hogar_s,1,5)

 svyset [pweight=factor], strata(strata) psu(psu)
 svydes

************
** ETHNIC **
************

* NA

***************
*** REGIONS ***
***************

/*
 1. Tegucigalpa 
 2. San pedro sula
 3. Resto urbano
 4. Rural 
*/

 gen region=dominio

 tab region [iw=factor]

/*
************************************************************************************
************************************************************************************
//////////////////////////////DO FILE GUIDE/////////////////////////////////////////

* All the indicators presented in EQxIS can be calculated using the svymean commands.

* For example:

* The syntax used in this Do File =>

noisily display "Net Enrolment Ratio in Primary"
global variable NERP
gen     NERP=0 if (DENOMINATOR)
replace NERP=1 if (NUMERATOR)
global indicador 6 " "
  * National indicators 	
	do "$DefvarsS"		
	noisily do "$TablasS"		  
	do "$DefvarsE"
	noisily do "$TablasE"
  * Regional indicators 
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

* Is equivalent to =>

gen     NERP=0 if (DENOMINATOR)
replace NERP=1 if (NUMERATOR)

svymean NERP, available by(combination of dissagregations) 
svymean NERP 

//////////////////////////////DO FILE GUIDE/////////////////////////////////////////
************************************************************************************
************************************************************************************
*/

*****************************
*** Codes & Access Routes ***
*****************************

  global pais 22 " "
  global ano 2004 " "
  global total 1 " "
  global totals 1 " "
  global genero 2 " "
  global etnia 3 " "
  global hombre 2 " "
  global mujer 3 " "
  global noindig 4 " "
  global indig 5 " "

  global VSEXO sexo
  global VSEXO_D sexo_d
  global VAREA area
  global VQUINTIL quintil
  global VQUINAREA quinarea
  global VETNIA indigena
  global VREGION1 region
  
  global DefvarsS "F:\EQxIS\Calculos\DefvarsS.do" 
  global DefvarsS_SEXOD "F:\EQxIS\Calculos\DefvarsS_SEXOD.do" 
  global DefvarsE "F:\EQxIS\Calculos\DefvarsE.do" 
  global TablasS "F:\EQxIS\Calculos\TablasS.do"
  global TablasE "F:\EQxIS\Calculos\TablasE.do"
  global DefvarsSR "F:\EQxIS\Calculos\DefvarsSR.do"
  global TablasSR "F:\EQxIS\Calculos\TablasSR.do"
  global DefvarsSWENAS "F:\EQxIS\Calculos\DefvarsSWENAS.do"
  global TablasSWENAS "F:\EQxIS\Calculos\TablasSWENAS.do"
  global DefvarsSMean "F:\EQxIS\Calculos\DefvarsSMean.do" 
  global DefvarsEMean "F:\EQxIS\Calculos\DefvarsEMean.do" 
  global TablasSMean "F:\EQxIS\Calculos\TablasSMean.do"
  global TablasEMean "F:\EQxIS\Calculos\TablasEMean.do"
  global DefvarsSTOTN_SECTEN "F:\EQxIS\Calculos\DefvarsSTOTN_SECTEN.do" 
  global DefvarsSTOTD_SECTEN "F:\EQxIS\Calculos\DefvarsSTOTD_SECTEN.do" 
  global TablasS_SECTEN "F:\EQxIS\Calculos\TablasS_SECTEN.do"
  global TablasSOBS "F:\EQxIS\Calculos\TablasSOBS.do" 
  global TablasEOBS "F:\EQxIS\Calculos\TablasEOBS.do"

  global DefvarsSRE1 "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSRE1.do" 
  global TablasSRE1  "F:\EQxIS\Calculos\REGIONES\HND\TablasSRE1.do"
  global DefvarsSRE2 "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSRE2.do" 
  global TablasSRE2  "F:\EQxIS\Calculos\REGIONES\HND\TablasSRE2.do"
  global DefvarsSRatioRE1   "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSRatioRE1.do" 
  global TablasSRatioRE1    "F:\EQxIS\Calculos\REGIONES\HND\TablasSRatioRE1.do"
  global TablasSOBSRatioRE1 "F:\EQxIS\Calculos\REGIONES\HND\TablasSOBSRatioRE1.do"
  global DefvarsSRatioRE2   "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSRatioRE2.do" 
  global TablasSRatioRE2    "F:\EQxIS\Calculos\REGIONES\HND\TablasSRatioRE2.do"
  global TablasSOBSRatioRE2 "F:\EQxIS\Calculos\REGIONES\HND\TablasSOBSRatioRE2.do"
  global DefvarsSWENASRE1 "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSWENASRE1.do" 
  global TablasSWENASRE1  "F:\EQxIS\Calculos\REGIONES\HND\TablasSWENASRE1.do"
  global DefvarsSWENASRE2 "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSWENASRE2.do" 
  global TablasSWENASRE2  "F:\EQxIS\Calculos\REGIONES\HND\TablasSWENASRE2.do"
  global DefvarsSRE1_SEXOD "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSRE1_SEXOD.do" 
  global DefvarsSRE2_SEXOD "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSRE2_SEXOD.do" 
  global DefvarsSURBANRE1 "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSURBANRE1.do" 
  global TablasSURBANRE1  "F:\EQxIS\Calculos\REGIONES\HND\TablasSURBANRE1.do"
  global DefvarsSURBANRE2 "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSURBANRE2.do" 
  global TablasSURBANRE2  "F:\EQxIS\Calculos\REGIONES\HND\TablasSURBANRE2.do"
  global DefvarsSURBANRE1_SEXOD "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSURBANRE1_SEXOD.do" 
  global DefvarsSURBANRE2_SEXOD "F:\EQxIS\Calculos\REGIONES\HND\DefvarsSURBANRE2_SEXOD.do" 
  global TablasSMeanRE1  "F:\EQxIS\Calculos\REGIONES\HND\TablasSMeanRE1.do"
  global TablasSMeanRE2  "F:\EQxIS\Calculos\REGIONES\HND\TablasSMeanRE2.do"

quietly{
set linesize 255

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*

2. ¿Asiste actualmente a algún centro educativo?
1. SI ==> 5

4. ¿Cuál es el nivel y año educativo más alto que aprobó?

Nivel(p04a)				
 1. Ninguno			
 2. Centro de Alfabetización	
 3. Pre-Escolar			
 4. Primaria			
 5. Ciclo común			
 6. Secundaria Diversificado	
 7. Técnico Superior		
 8. Superior Universitario	
 9. Post-grado			
99. No sabe			

Año Educativo (p04b)

5. ¿Qué nivel y año educativo cursa actualmente?

Nivel(p05a)				
 2. Centro de Alfabetización	 
 3. Pre-Escolar			 
 4. Primaria			 
 5. Ciclo común			 
 6. Secundaria Diversificado	 
 7. Técnico Superior		 
 8. Superior Universitario	 
 9. Post-grado			 
99. No sabe			 

Año Educativo (p05b)

*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 noisily display "Net Enrolment Ratio in Primary"
 global variable NERP
 gen	 NERP=0 if (edad>=7 & edad<=12) & (asiste>=1 & asiste<=2) 
 replace NERP=1 if (edad>=7 & edad<=12) & (nivasiste==4 & (grado>=1 & grado<=6))
 global indicador 6 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 noisily display "Net Enrolment Ratio in Secondary"
 global variable NERS
 gen	 NERS=0 if (edad>=13 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS=1 if (edad>=13 & edad<=18) & ((nivasiste==4 & (grado>=7 & grado<=9)) | (nivasiste==5 | nivasiste==6))
 global indicador 49 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	
* Upper secondary
* Secundaria Diversificado

 noisily display "Net Enrolment Ratio in Secondary - upper"
 global variable NERS2
 gen	 NERS2=0 if (edad>=16 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS2=1 if (edad>=16 & edad<=18) & (nivasiste==6)
 global indicador 95 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 noisily display "Literacy Rate of 15-24 Years Old"
 global variable ALFABET
 gen	 ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 
 global indicador 8 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 noisily display "Literacy Rate of 15-24 Years Old INE"
 global variable ALFABET2
 gen	 ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1)
 global indicador 53 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  (nivasiste==4 & (grado>=1 & grado<=6))
 gen sec=1  if ((nivasiste==4 & (grado>=7 & grado<=9)) | (nivasiste==5 | nivasiste==6))
 gen ter=1  if  (nivasiste==8)

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 noisily display "Ratio of Girls to Boys in School"
 noisily display "Primary"
 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.
 global variable RPRIMM
 global variable2 RPRIMH
 global indicador 64 " "
  /* National indicators */
	do "$DefvarsSR"
	noisily do "$TablasSR"
  /* Regional indicators */
	do "$DefvarsSRatioRE1"
	noisily do "$TablasSRatioRE1"
 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
 global variable RATIOPRIM
 global indicador 64 " "
  /* National indicators - Observations */
	do "$DefvarsS"
	noisily do "$TablasSOBS"
  /* Regional indicators - Observations  */
	do "$DefvarsSRE1"
	noisily do "$TablasSOBSRatioRE1"
	
** Target 4, Ratio of Girls to Boys in Secondary*

 noisily display "Secondary"
 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.
 global variable RSECM
 global variable2 RSECH
 global indicador 65 " "
  /* National indicators */
	do "$DefvarsSR"
	noisily do "$TablasSR"
  /* Regional indicators */
	do "$DefvarsSRatioRE1"
	noisily do "$TablasSRatioRE1"
 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
 global variable RATIOSEC
 global indicador 65 " "
  /* National indicators - Observations */
	do "$DefvarsS"
	noisily do "$TablasSOBS"
  /* Regional indicators - Observations  */
	do "$DefvarsSRE1"
	noisily do "$TablasSOBSRatioRE1"
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 noisily display "Tertiary"
 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.
 global variable RTERM
 global variable2 RTERH
 global indicador 66 " "
  /* National indicators */
	do "$DefvarsSR"
	noisily do "$TablasSR"
  /* Regional indicators */
	do "$DefvarsSRatioRE1"
	noisily do "$TablasSRatioRE1"
 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  
 global variable RATIOTER
 global indicador 66 " "
  /* National indicators - Observations */
	do "$DefvarsS"
	noisily do "$TablasSOBS"
  /* Regional indicators - Observations  */
	do "$DefvarsSRE1"
	noisily do "$TablasSOBSRatioRE1"

noisily{
	tab sexo quintil  if ter==1
	tab sexo quinarea if ter==1 & area==1
	tab sexo quinarea if ter==1 & area==2
	tab sexo region   if ter==1 
	}

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 noisily display "All"
 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.
 global variable RALLM
 global variable2 RALLH
 global indicador 9 " "
  /* National indicators */
	do "$DefvarsSR"
	noisily do "$TablasSR"
  /* Regional indicators */
	do "$DefvarsSRatioRE1"
	noisily do "$TablasSRatioRE1"
 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    
 global variable RATIOALL
 global indicador 9 " "
  /* National indicators - Observations */
	do "$DefvarsS"
	noisily do "$TablasSOBS"
  /* Regional indicators - Observations  */
	do "$DefvarsSRE1"
	noisily do "$TablasSOBSRatioRE1"

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 noisily display "Ratio of Literate Women to Men 15-24 year olds"
 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.
 global variable MA2
 global variable2 HA2
 global indicador 80 " "
  /* National indicators */
	do "$DefvarsSR"
	noisily do "$TablasSR"
  /* Regional indicators */
	do "$DefvarsSRatioRE1"
	noisily do "$TablasSRatioRE1"
 gen     RATIOLIT2=0 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 global variable RATIOLIT2
 global indicador 80 " "
  /* National indicators - Observations */
	do "$DefvarsS"
	noisily do "$TablasSOBS"
  /* Regional indicators - Observations  */
	do "$DefvarsSRE1"
	noisily do "$TablasSOBSRatioRE1"

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 noisily display "Ratio of Literate Women to Men 15-24 year olds"
 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.
 global variable MA
 global variable2 HA
 global indicador 10 " "
  /* National indicators */
	do "$DefvarsSR"
	noisily do "$TablasSR"
  /* Regional indicators */
	do "$DefvarsSRatioRE1"
	noisily do "$TablasSRatioRE1"
 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 global variable RATIOLIT
 global indicador 10 " "
  /* National indicators - Observations */
	do "$DefvarsS"
	noisily do "$TablasSOBS"
  /* Regional indicators - Observations  */
	do "$DefvarsSRE1"
	noisily do "$TablasSOBSRatioRE1"
	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*
29.A. ¿Cuál es o era su categoría ocupacional en la ocupación principal?
 1. Empleado u obrero público 
 2. Empleado u obrero privado 
 3. Empleado doméstico 
TRABAJADORES CUENTA PROPIA
 4. Miembro de cooperativa de producción 
 5. Cuenta propia que no contrata mano de obra temporal 
 6. Cuenta propia que contrata mano de obra temporal 
 7. Empleador o socio activo 
PRODUCTORES AGROPECUARIOS
 8. Miembro de cooperativa, asentamiento o grupo
 9. Cuenta propia que no contrata mano de obra temporal
 10. Cuenta propia que contrata mano de obra temporal
 11. Patrón o socio de la finca
 12. Trabajador familiar no remunerado
 13. Trabajador no remunerado

categop (variable included in the database) =>
categoria ocupacional (ocupacion principal)
 1. Empleado publico 
 2. Empleado privado
 3. Empleada domestica
 4. Cuenta propia 
 5. Trabajador no remunerado 
*/

* Without Domestic Service

 noisily display "WENAS without domestic servants"
 global variable WENAS
 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=2) & (rama>=2 & rama<=9) & (condact==1)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=2) & (rama>=2 & rama<=9) & (condact==1) & (sexo==2)
 global indicador 11 " "

 * RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 
  /* National indicators */
	do "$DefvarsSWENAS"
	noisily do "$TablasSWENAS"
  /* Regional URBAN indicators */
	do "$DefvarsSWENASRE1"
	noisily do "$TablasSWENASRE1"

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 noisily display "WENAS with domestic servants"
 global variable WENASD
 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (rama>=2 & rama<=9) & (condact==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (rama>=2 & rama<=9) & (condact==1) & (sexo==2)
 global indicador 54 " "

 * RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 
  /* National indicators */
	do "$DefvarsSWENAS"
	noisily do "$TablasSWENAS"
  /* Regional URBAN indicators */
	do "$DefvarsSWENASRE1"
	noisily do "$TablasSWENASRE1"

** Access to Electricity ** Additional Indicator
/*
7. ¿Qué tipo de alumbrado utiliza en la vivienda?
 1. Servicio Público
 2. Servicio privado colectivo
 3. Planta propia
 4. Energía solar
 5. Vela
 6. Candil o lámpara de gas 
 7. Ocote
 8. Otro:________________________ */

 egen elec=max(v07), by(id_viv)

* Gender classification of the population refers to the head of the household.

 noisily display "Proportion of Population with access to electricity"
 global variable ELEC
 gen	 ELEC=0  if (elec>=1 & elec<=8) /* Total population excluding missing information */
 replace ELEC=1  if (elec>=1 & elec<=4)
 global indicador 87 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"

/*
5. Servicio de Agua (v05a)
a) ¿Tiene en la vivienda o en la propiedad, tubería instalada para agua?
 1. Sí  2. No

b) ¿Cómo obtiene el agua que utiliza en la vivienda? (v05b)
 1. Servicio Público por tubería
 2. Servicio Privado por tubería
 3. Pozo malacate
 4. Pozo con bomba
 5. Río, riachuelo, manantial, ojo de agua
 6. Carro Cisterna
 7. Pick-Up barriles o con drones o barriles
 8. Otro

c) ¿Cómo es el suministro de agua?
 1. Permanente
 2. Irregular

d) En los últimos quince días, ¿Con qué frecuencia tuvo el suministro de agua en su vivienda?
	- Cuántos días
	- Horas promedio por día

e) ¿Dónde obtiene el agua?
 1. Dentro de la vivienda
 2. Fuera de la vivienda y dentro de la propiedad
 3. Fuera de la propiedad a menos de 100 metros
 4. Fuera de la propiedad a más de 100 metros
*/

 noisily{
 tab numhog
 tab v05a
 tab v05a numhog
 tab nper if numhog==1 & nper==1 	
	}

 egen agua=max(v05b), by(id_viv)
 egen lugabast=max(v05e), by(id_viv)

* Gender classification of the population refers to the head of the household.

 noisily display "Improved Water Source"
 global variable WATER
 gen	 WATER=0 if (agua>=1 & agua<=8) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4)
 global indicador 30 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*

6. Servicio Sanitario
a) ¿Tiene algún tipo de servicio sanitario?
 1. Sí 
 2. No ==> 7

b) ¿Qué tipo de servicio sanitario tiene?
 1. Inodoro conectado a alcantarilla
 2. Inodoro conectado a pozo séptico
 3. Inodoro con desagüe a río, laguna, mar
 4. Letrina con descarga a río, laguna, mar
 5. Letrina con cierre hidráulico
 6. Letrina con pozo séptico
 7. Letrina con pozo negro

c) El uso del servicio sanitario es:
 1. Exclusivo de la vivienda
 2. Compartido con otras viviendas
*/

 egen servsani=max(v06a), by(id_viv)
 egen sanita=  max(v06b), by(id_viv)
 egen servexc= max(v06c), by(id_viv)

* Gender classification of the population refers to the head of the household.

 noisily display "Improved Sanitation"
 global variable SANITATION
 gen	 SANITATION=0 if (servsani>=1 & servsani<=2)	/* Total population excluding missing information */
 replace SANITATION=1 if ((sanita>=1 & sanita<=2) | (sanita==6))
 global indicador 31 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*
1. Tipo de vivienda			10. ¿Cómo es la tenencia de esta vivienda?
v01					v10a
 1. Casa individual			 1. Propietario y completamente pagada
 2. Casa de mat. nat. (Rancho)		 2. Propietario recuperada legalizada
 3. Casa Improvisada (Desechos)		 3. Propietario recuperada sin legalizar
 4. Apartamento				 4. Propietario y la está pagando
 5. Cuarto en mesón o cuartería 	 5. Alquilada
 6. Barracón				 6. Cedida sin pago
 7. Local no construido para habitación 	
 pero usado como vivienda
 8. Otro

2. ¿Cuál es el material predominante 	3. ¿Cuál es el material predominante en el piso?
en la construcción de las paredes?
 1. Ladrillo, piedra o bloque		 1. Cerámica
 2. Adobe				 2. Ladrillo de cemento
 3. Material prefabricado		 3. Ladrillo de granito
 4. Madera				 4. Ladrillo de barro
 5. Bahareque, vara o caña		 5. Plancha de cemento
 6. Desechos				 6. Madera
 7. Otro				 7. Tierra
				 	 8. Otro

12. Cantidad de Piezas de la Vivienda
a). ¿Cuántas piezas tiene esta vivienda? (incluya la cocina pero no el baño)
*/

 egen tenencia=max(v10a), by(id_viv)
 egen tipoviv=max(v01), by(id_viv)
 egen piso=max(v03), by(id_viv)
 egen pared=max(v02), by(id_viv)
 egen nrocuart=max(v12a), by(id_viv)
 replace nrocuart=. if v12a==99
 egen pers=max(totpervi), by(id_viv) /* Total de personas de la vivienda */

 gen persroom=pers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=8) & (tenencia>=1 & tenencia<=6)  /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==2 | tipoviv==3 | tipoviv==6 | tipoviv==7 | tipoviv==8) | (tenencia==6)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (pared>=1 & pared<=7) & (piso>=1 & piso<=8)  /* Total population excluding missing information */
 replace secten_2=1 if (pared>=5 & pared<=7) | (piso==7 | piso==8)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 noisily display "Secure Tenure"
 global variable SECTEN
 gen	 SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1)  /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)
 global indicador 32 " "

  /* Urban indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS_SECTEN"
  /* Regional URBAN indicators */
	do "$DefvarsSURBANRE1_SEXOD"
	noisily do "$TablasSURBANRE1"

* Dirt floors

* Gender classification of the population refers to the head of the household.

* 3. ¿Cuál es el material predominante en el piso?
 noisily display "Proportion of Population living in dwellings with dirt floors"
 global variable DIRT
 gen	 DIRT=0 if (piso>=1 & piso<=8) /* Total population excluding missing information */
 replace DIRT=1 if (piso==7)
 global indicador 88 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	
** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 noisily display "Unemployment Rate 15 to 24"
 global variable UNMPLYMENT15	
 gen	 UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (TASADESO==0 | TASADESO==1) 
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (TASADESO==1 )
 global indicador 45 " "

  /* Urban indicators */
	do "$DefvarsS"
	noisily do "$TablasS_SECTEN"
  /* Regional URBAN indicators */
	do "$DefvarsSURBANRE1"
	noisily do "$TablasSURBANRE1"

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

/*
11. ¿Tiene en esta vivienda los siguientes bienes?
f. Teléfono fijo 
g. Teléfono celular 
*/

 egen telefono=max(v11f), by(id_viv)
 egen celular=max(v11g), by(id_viv)

* Gender classification of the population refers to the head of the household.

 noisily display "Telephone Lines"
 global variable TELCEL
 gen	 TELCEL=0 if (telefono>=1 & telefono<=2) & (celular>=1 & celular<=2) /* Total population excluding missing information */
 replace TELCEL=1 if (celular==1 | telefono==1)
 global indicador 47 " "

  /* National indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"

** FIXED LINES
* Gender classification of the population refers to the head of the household.

 noisily display "Telephone Lines & Cellular Phones"
 global variable TEL
 gen	 TEL=0 if (telefono>=1 & telefono<=2) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1)
 global indicador 99 " "

  /* National indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"
	
** CEL LINES
* Gender classification of the population refers to the head of the household.

 noisily display "Telephone Lines & Cellular Phones"
 global variable CEL
 gen	 CEL=0 if (celular>=1 & celular<=2) /* Total population excluding missing information */
 replace CEL=1 if (celular==1)
 global indicador 100 " "

  /* National indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"
	
** Target 18, Indicator: "Personal computers in use per 100 population"
/*
11. ¿Tiene en esta vivienda los siguientes bienes?
i. Computadora 
*/

 egen computadora=max(v11i), by(id_viv)

* Gender classification of the population refers to the head of the household.

 noisily display "Personal Computers"
 global variable COMPUTER
 gen	 COMPUTER=0 if (computadora>=1 & computadora<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computadora==1)
 global indicador 48 " "

  /* National indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"

** Target 18, Indicator: "Internet users per 100 population"
/*
10. Durante el mes pasado, ¿tuvo acceso a internet?
 1. Sí
 2. No 
 9. No sabe

11.¿En que sitio tuvo acceso a internet? (p11)
 1. En su casa (a)
 2. En un cyber-café o negocio de internet (b)
 3. En su trabajo (c)
 4. En la escuela, colegio o universidad (d)
 5. En una oficina de HONDUTEL (e)
 6. Otro (f)
*/

** In order to mantain comparability with other countries the following
** restricted definition is used.

 egen internet=max(p11a), by(hogar) /* Tuvo acceso a internet en su casa*/

* Gender classification of the population refers to the head of the household.

 noisily display "Internet Users"
 global variable INTUSERS
 gen	 INTUSERS=0 
 replace INTUSERS=1 if (internet==1)
 global indicador 55 " "

  /* National indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"
	
************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 noisily display "Children Under Age 15 who are Working"
 global variable CHILDREN
 gen     CHILDREN=0 if  (edad>=12 & edad<=14) 
 replace CHILDREN=1 if  (edad>=12 & edad<=14) & (condact==1)
 global indicador 56 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1

 noisily display "Persons per Room"
 global variable PERSROOM2
 global indicador 62 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 noisily display "Population living in households with less than 2 persons (inclusive) per room"
 global variable PLT2
 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
 global indicador 102 " "

  /* National indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"

** Disconnected Youths
/*
17. ¿Por qué no buscó trabajo ni trató de establecer su propio
negocio o finca la semana pasada?
 1. Se incorporará a un trabajo antes de un mes			==>22
 2. Tiene trabajo asegurado después de un mes 			==>22
 3. Espera respuesta a gestiones 				==>22
 4. Está esperando la próxima temporada de trabajo 		==>22
 5. Cree que no encontrará trabajo				==>22
 6. Dejó de buscar trabajo momentáneamente			==>22
 7. No tiene tierra, capital, ni materia prima			==>22
 8. No tiene tiempo para buscar trabajo				==>18
 9. No tiene necesidad de trabajar				==>18
 10. Por su edad no puede trabajar				==>18
 11.  Otro							==>18

18. ¿Cuál es su condición actual?
 1. Jubilado
 2. Pensionista
 3. Rentista
 4. Estudiante
 5. Realiza los quehaceres del hogar
 6. Discapacitado
 7. Vejez
 8. Menor de edad
 9. Otro
*/

 noisily display "Disconnected Youths"
 global variable DISCONN
 gen	 DISCONN=0 if (edad>=15 & edad<=24) 
 replace DISCONN=1 if (edad>=15 & edad<=24) & ((p17==5) | ((p17>=8 & p17<=11) & (p18==9)))
 global indicador 63 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

*** Rezago escolar

 gen	 rezago=0	if (anoest>=0 & anoest<99)  & edad==7 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==8
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==9
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==10
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==11
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==11

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==12
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==12

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==13
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==14
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==15
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==16
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==17
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==17

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==18
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==18

* Primary and Secondary [ISCED 1, 2 & 3]

 noisily display "Rezago Escolar"
 global variable REZ
 gen	 REZ=0 if (edad>=8 & edad<=18) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=8 & edad<=18) & (rezago==1)
 global indicador 90 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"

* Primary completion rate [15 - 24 years of age]

 noisily display "Primary completion rate [15 - 24 years of age]"
 global variable PRIMCOMP
 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
 global indicador 93 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	
* Average years of education of the population 15+

 noisily display "Average Years of Education"

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))
 global variable AEDUC_15
 global indicador 94 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	
 noisily display "Average Years of Education 15-24"

 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))
 global variable AEDUC_15_24
 global indicador 103 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	
 noisily display "Average Years of Education 25+"

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
 global variable AEDUC_25
 global indicador 104 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	
* Grade for age

 noisily display "Grade for age"
 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=18) & (anoest>=0 & anoest<99)
 global variable GFA
 global indicador 105 " "
 
  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	
* Grade for age primary

 noisily display "Grade for age primary"
 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
 global indicador 106 " "
 global variable GFAP
 
  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	
* Grade for age Secondary

 noisily display "Grade for age secondary"
 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=18) & (anoest>=0 & anoest<99)
 global variable GFAS
 global indicador 107 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"

}

log close


