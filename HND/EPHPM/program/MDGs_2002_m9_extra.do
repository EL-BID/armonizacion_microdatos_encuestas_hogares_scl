**************************************
****** Last update: July, 2006  ****** 
**************************************

/*

Instituto Nacional de Estadística, Encuesta Permanente de Hogares de Propósitos Múltiples. May, 2002.

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
*** HONDURAS 2002 ***
*********************

clear
set mem 120m
set more off
set linesize 255

use "${surveysFolder}\EQxIS\BASES DE DATOS\HONDURAS\hnd02-may.dta", clear

log using "${surveysFolder}\EQxIS\LOGS\HND02.log", replace
          
/*
rela_j
 1. jefe del hogar
 2. esposa(o) o compañera(o) 
 3. hijos o hijastros de mayor a menor
 4. padres 
 5. yernos y nueras 
 6. otros parientes(nietos,abuelos,tios...)
 7. otros no parientes(suegros,cuñados...)
 8. servicio doméstico
*/

* Variables

 rename rela_j parentco
 rename ur area
 rename p01 alfabet
 rename p02 asiste

 tab area [iw=factor]

 tab nper parentco
 recode parentco (6=1) if nper==1

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

 tostring hogar,gen(hogar_s) format(%12.0f) force

 gen id_viv=substr(hogar_s,1,7)	/* hogar_s => String variable identical to "hogar" */

** Years of education. 
/*
2. ¿Asiste actualmente a algún establecimiento de enseñanza?
 1. SI / 2. NO

p03a. 
3. ¿Cuál es el nivel mas alto de estudio que esta cursando o curso? y
¿cuál es el último año aprobado en ese nivel? (p03b)
			
 01. ninguno	
 02. pre primaria
 03. centro de alfabetización	
 04. maestro en casa			Ultimo año aprobado
 05. primaria(1-9)				==> p03b
 06. secundaria ciclo común(1-3)		==> p03b
 07. secundaria diversificada(1-4)		==> p03b
 08. superior no universitaria(1-4)		==> p03b
 09. superior universitaria(1-8)		==> p03b
 10. post-grado(1-4)				==> p03b	
 11. no sabe, no respondió	
			
*/
* Included in the database

 rename anosest a_orig

 gen	 anoest=0  if (p03a==1 | p03a==2 | p03a==3 | p03a==4) | (p03a==5 & p03b==0) 
 replace anoest=1  if (p03a==5 & p03b==1) 	
 replace anoest=2  if (p03a==5 & p03b==2) 	
 replace anoest=3  if (p03a==5 & p03b==3) 	
 replace anoest=4  if (p03a==5 & p03b==4) 	
 replace anoest=5  if (p03a==5 & p03b==5) 	
 replace anoest=6  if (p03a==5 & p03b==6) | (p03a==6 & p03b==0) 	
 replace anoest=7  if (p03a==5 & p03b==7) | (p03a==6 & p03b==1) 
 replace anoest=8  if (p03a==5 & p03b==8) | (p03a==6 & p03b==2) 
 replace anoest=9  if (p03a==5 & p03b==9) | (p03a==6 & p03b==3) | (p03a==7 & p03b==0) 
 replace anoest=10 if (p03a==7 & p03b==1) 
 replace anoest=11 if (p03a==7 & p03b==2) 
 replace anoest=12 if (p03a==7 & p03b==3) | (p03a==9 & p03b==0) | (p03a==8 & p03b==0)  
 replace anoest=13 if (p03a==7 & p03b==4) | (p03a==9 & p03b==1) | (p03a==8 & p03b==1)  
 replace anoest=14 if (p03a==9 & p03b==2) | (p03a==8 & p03b==2)  
 replace anoest=15 if (p03a==9 & p03b==3) | (p03a==8 & p03b==3)  
 replace anoest=16 if (p03a==9 & p03b==4) | (p03a==8 & p03b==4)
 replace anoest=17 if (p03a==9 & p03b==5) | (p03a==10 & p03b==0) 
 replace anoest=18 if (p03a==9 & p03b==6) | (p03a==10 & p03b==1) 
 replace anoest=19 if (p03a==9 & p03b==7) | (p03a==10 & p03b==2) 
 replace anoest=20 if (p03a==9 & p03b==8) | (p03a==10 & p03b==3) 
 replace anoest=21 if (p03a==10 & p03b==4)

**********************
** INCOME QUINTILES **
**********************

* yperhg => Ingreso mensual del hogar percapita

 egen double ypcapita=max(yperhg) if yperhg>0, by(hogar) 

 rename quintil quint_orig /* Quintil ingreso  percapita del hogar */

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

 xtile quintil=ypcapita_ord 	[w=factor], nq(5)
 xtile quintil_2=ypcapita 	[w=factor], nq(5)
 xtile qtlurbano=ypcurbano_ord  [w=factor], nq(5) 
 xtile qtlrural= ypcrural_ord   [w=factor], nq(5)

 format ypcapita* ypcurbano ypcrural %15.0g 

 gen     quinarea=qtlurbano if area==1
 replace quinarea=qtlrural  if area==2

 count
 sum quin*
 sum ypc*

 tab quintil [iw=factor]
 tab quintil quintil_2

 sort hogar nper

** Economic Active Population (10 years or more of age)
* Included in the database
* condact
/* Condición de Actividad
1. Ocupados	
2. Desocupados
3. Inactivos
*/

 gen 	 TASADESO=0 if condact==1
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

 gen strata=domi if domi<5
 replace strata=5 if domi==5 & estrato1==0
 replace strata=6 if domi==5 & estrato1==1

* PSU

 gen str5 psu=substr(hogar_s,1,5)
 
 svyset [pweight=factor], strata(strata) psu(psu)
 svydes

************
** ETHNIC **
************

/*
5 years or more of age.

9. ¿a que grupo etnico o raza considera que pertenece(...)? 
p09

01. garifuna    
02. negro ingles 
03. tolupán	
04. pech (paya)	
05. misquito 
06. lenca
07. tawahka (sumo)
08. chorti
09. mestizo ó ladino
10. otro
99. ns/nr
*/

 gen	 indigena=1 if (p09==1 | (p09>=3 & p09<=8))
 replace indigena=0 if (p09==2 | p09==9 | p09==10)

** Missings. Persons with less than 5 years of age
* Filling missings using the mother auto identification.
* 3. "hijos o hijastros de mayor a menor"

 gen pertn_m=indigena if (parentco==2 & sexo==2) | (parentco==1 & sexo==2)
 
 egen pertn_mh=max(pertn_m), by(hogar)
 
 replace indigena=pertn_mh if edad<=4 & (parentco==3)

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
Departamentos

 1. Atlantida 
 2. Colon 
 3. Comayagua 
 4. Copan 
 5. Cortes 
 6. Choluteca 
 7. El Paraiso 
 8. Francisco Morazan
 9. Gracias a Dios	==> NOT INCLUDED IN THE SAMPLE
10. Intibuca
11. Islas de la Bahía   ==> NOT INCLUDED IN THE SAMPLE
12. La Paz 
13. Lempira 
14. Ocotepeque 
15. Olancho 
16. Santa Barbara 
17. Valle
18. Yoro 
*/ 

 gen region2=depto
 
 tab region2 [iw=factor]
 
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
  global ano 2002 " "
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
  global VREGION2 region2
  
  global DefvarsS "${surveysFolder}\EQxIS\Calculos\DefvarsS.do" 
  global DefvarsS_SEXOD "${surveysFolder}\EQxIS\Calculos\DefvarsS_SEXOD.do" 
  global DefvarsE "${surveysFolder}\EQxIS\Calculos\DefvarsE.do" 
  global TablasS "${surveysFolder}\EQxIS\Calculos\TablasS.do"
  global TablasE "${surveysFolder}\EQxIS\Calculos\TablasE.do"
  global DefvarsSR "${surveysFolder}\EQxIS\Calculos\DefvarsSR.do"
  global TablasSR "${surveysFolder}\EQxIS\Calculos\TablasSR.do"
  global DefvarsSWENAS "${surveysFolder}\EQxIS\Calculos\DefvarsSWENAS.do"
  global TablasSWENAS "${surveysFolder}\EQxIS\Calculos\TablasSWENAS.do"
  global DefvarsSMean "${surveysFolder}\EQxIS\Calculos\DefvarsSMean.do" 
  global DefvarsEMean "${surveysFolder}\EQxIS\Calculos\DefvarsEMean.do" 
  global TablasSMean "${surveysFolder}\EQxIS\Calculos\TablasSMean.do"
  global TablasEMean "${surveysFolder}\EQxIS\Calculos\TablasEMean.do"
  global DefvarsSTOTN_SECTEN "${surveysFolder}\EQxIS\Calculos\DefvarsSTOTN_SECTEN.do" 
  global DefvarsSTOTD_SECTEN "${surveysFolder}\EQxIS\Calculos\DefvarsSTOTD_SECTEN.do" 
  global TablasS_SECTEN "${surveysFolder}\EQxIS\Calculos\TablasS_SECTEN.do"
  global TablasSOBS "${surveysFolder}\EQxIS\Calculos\TablasSOBS.do" 
  global TablasEOBS "${surveysFolder}\EQxIS\Calculos\TablasEOBS.do"

  global DefvarsSRE1 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSRE1.do" 
  global TablasSRE1  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSRE1.do"
  global DefvarsSRE2 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSRE2.do" 
  global TablasSRE2  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSRE2.do"
  global DefvarsSRatioRE1   "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSRatioRE1.do" 
  global TablasSRatioRE1    "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSRatioRE1.do"
  global TablasSOBSRatioRE1 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSOBSRatioRE1.do"
  global DefvarsSRatioRE2   "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSRatioRE2.do" 
  global TablasSRatioRE2    "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSRatioRE2.do"
  global TablasSOBSRatioRE2 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSOBSRatioRE2.do"
  global DefvarsSWENASRE1 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSWENASRE1.do" 
  global TablasSWENASRE1  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSWENASRE1.do"
  global DefvarsSWENASRE2 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSWENASRE2.do" 
  global TablasSWENASRE2  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSWENASRE2.do"
  global DefvarsSRE1_SEXOD "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSRE1_SEXOD.do" 
  global DefvarsSRE2_SEXOD "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSRE2_SEXOD.do" 
  global DefvarsSURBANRE1 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSURBANRE1.do" 
  global TablasSURBANRE1  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSURBANRE1.do"
  global DefvarsSURBANRE2 "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSURBANRE2.do" 
  global TablasSURBANRE2  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSURBANRE2.do"
  global DefvarsSURBANRE1_SEXOD "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSURBANRE1_SEXOD.do" 
  global DefvarsSURBANRE2_SEXOD "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\DefvarsSURBANRE2_SEXOD.do" 
  global TablasSMeanRE1  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSMeanRE1.do"
  global TablasSMeanRE2  "${surveysFolder}\EQxIS\Calculos\REGIONES\HND\2002\TablasSMeanRE2.do"

quietly{
set linesize 255

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*

2. ¿Asiste actualmente a algún establecimiento de enseñanza?
1. SI 

3. ¿Cuál es el nivel mas alto de estudio que esta cursando o curso? y
¿cuál es el último año aprobado en ese nivel? (p03b)
p03a. 
			
01. ninguno	
02. pre primaria
03. centro de alfabetización	
04. maestro en casa			Ultimo año aprobado
05. primaria(1-9)				==> p03b
06. secundaria ciclo común(1-3)			==> p03b
07. secundaria diversificada(1-4)		==> p03b
08. superior no universitaria(1-4)		==> p03b
09. superior universitaria(1-8)			==> p03b
10. post-grado(1-4)				==> p03b	
11. no sabe, no respondió	
			
*/

 rename p03a niveduc
 rename p03b ultgrad

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 noisily display "Net Enrolment Ratio in Primary"
 global variable NERP
 gen	 NERP=0 if (edad>=7 & edad<=12) & (asiste>=1 & asiste<=2) 
 replace NERP=1 if (edad>=7 & edad<=12) & (asiste==1) & (niveduc==5 & (ultgrad>=0 & ultgrad<=5))
 global indicador 6 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 noisily display "Net Enrolment Ratio in Secondary"
 global variable NERS
 gen	 NERS=0 if (edad>=13 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS=1 if (edad>=13 & edad<=18) & (asiste==1) & ((niveduc==5 & (ultgrad>=6 & ultgrad<=9)) | ((niveduc==6)  | (niveduc==7 & (ultgrad>=0 & ultgrad<=2))))
 global indicador 49 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"
	
* Upper secondary
* Secundaria Diversificado

 noisily display "Net Enrolment Ratio in Secondary - upper"
 global variable NERS2
 gen	 NERS2=0 if (edad>=16 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS2=1 if (edad>=16 & edad<=18) & ((niveduc==7 & (ultgrad>=0 & ultgrad<=2)) | (niveduc==6 & ultgrad==3) )
 global indicador 95 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 noisily display "Literacy Rate of 15-24 Years Old"
 global variable ALFABET
 gen     ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 
 global indicador 8 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 noisily display "Literacy Rate of 15-24 Years Old INE"
 global variable ALFABET2
 gen     ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1)
 global indicador 53 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if (asiste==1) & (niveduc==5 & (ultgrad>=0 & ultgrad<=5))
 gen sec=1  if (asiste==1) & ((niveduc==5 & (ultgrad>=6 & ultgrad<=9)) | ((niveduc==6)  | (niveduc==7 & (ultgrad>=0 & ultgrad<=2))))
 gen ter=1  if (asiste==1) & (((niveduc==8 & (ultgrad>=0 & ultgrad<=3)) | (niveduc==9 & (ultgrad>=0 & ultgrad<=4))) | (niveduc==7 & (ultgrad==4 | ultgrad==3)))

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
	do "$DefvarsSRatioRE2"
	noisily do "$TablasSRatioRE2"	
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
	do "$DefvarsSRE2"
	noisily do "$TablasSOBSRatioRE2"
	
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
	do "$DefvarsSRatioRE2"
	noisily do "$TablasSRatioRE2"	
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
	do "$DefvarsSRE2"
	noisily do "$TablasSOBSRatioRE2"
	
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
	do "$DefvarsSRatioRE2"
	noisily do "$TablasSRatioRE2"	
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
	do "$DefvarsSRE2"
	noisily do "$TablasSOBSRatioRE2"

noisily{
	tab sexo quintil  if ter==1
	tab sexo quinarea if ter==1 & area==1
	tab sexo quinarea if ter==1 & area==2
	tab sexo region   if ter==1 
	tab sexo region2  if ter==1 
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
	do "$DefvarsSRatioRE2"
	noisily do "$TablasSRatioRE2"	
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
	do "$DefvarsSRE2"
	noisily do "$TablasSOBSRatioRE2"

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
	do "$DefvarsSRatioRE2"
	noisily do "$TablasSRatioRE2"	
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
	do "$DefvarsSRE2"
	noisily do "$TablasSOBSRatioRE2"

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
	do "$DefvarsSRatioRE2"
	noisily do "$TablasSRatioRE2"	
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
	do "$DefvarsSRE2"
	noisily do "$TablasSOBSRatioRE2"

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*

34. ¿Cuál es o era su categoría ocupacional 		rama
en la ocupación principal?			 	rama de actividad economica
1. Empleado u obrero publico	
2. Empleado u obrero privado(exc.serv.dom)	
3. Servicio domestico	
4. Miembro de cooperativa, asentamiento	
5. Trabajador por cuenta propia que no contrata mano de obra temporal	
6. Trabajador por cuenta propia que contrata mano de obra temporal	
7. Empleador o socio activo
8. Trabajado familiar no remunerado
9. Trabajador no remunerado
99. ns/nr	

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
	do "$DefvarsE"
 	noisily do "$TablasE"
  /* Regional URBAN indicators */
	do "$DefvarsSWENASRE1"
	noisily do "$TablasSWENASRE1"
	do "$DefvarsSWENASRE2"
	noisily do "$TablasSWENASRE2"
		
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 noisily display "WENAS with domestic servants"
 global variable WENASD
 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (rama>=2 & rama<=9) & (condact==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (rama>=2 & rama<=9) & (condact==1) & (sexo==2)
 global indicador 54 " "

  /* National indicators */
	do "$DefvarsSWENAS"
	noisily do "$TablasSWENAS"
	do "$DefvarsE"
 	noisily do "$TablasE"
  /* Regional URBAN indicators */
	do "$DefvarsSWENASRE1"
	noisily do "$TablasSWENASRE1"
	do "$DefvarsSWENASRE2"
	noisily do "$TablasSWENASRE2"

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

noisily {
 tab numhog
 tab v04a
 tab v04a numhog
 tab nper if numhog==1 & nper==1 	
	}

/*These variables are  available in the database only for the household head*/

** Access to Electricity ** Additional Indicator
/*
6. ¿Qué tipo de alumbrado tiene?
 1. Servicio público
 2. Planta privada colectiva
 3. Planta privada individual
 4. Energia solar
 5. Vela
 6. Candil o lampara de gas
 7. Otro
 */

 egen elec=max(v06), by(hogar)

* Gender classification of the population refers to the head of the household.

 noisily display "Proportion of Population with access to electricity"
 global variable ELEC
 gen	 ELEC=0 if (elec>=1 & elec<=7) /* Total population excluding missing information */
 replace ELEC=1 if (elec>=1 & elec<=4)
 global indicador 87 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE" 	
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	do "$DefvarsSRE2_SEXOD"
 	noisily do "$TablasSRE2"

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

/*
4. Servicio de Agua (v04a)
a) ¿Tiene tubería instalada para agua?
 1. Sí  2. No

b) ¿De dónde proviene el agua que utiliza? (v04b)
 1. Servicio Público 
 2. Servicio Privado colectivo
 3. Pozo malacate
 4. Pozo con bomba
 5. Río, riachuelo, manantial, ojo de agua
 6. Cisterna
 7. Otro

c) ¿De dónde la obtiene? (v04c)
 1. Dentro de la vivienda
 2. Fuera de la vivienda y dentro de la propiedad
 3. Fuera de la propiedad a menos de 100 metros
 4. Fuera de la propiedad a más de 100 metros

*/
/*These variables are  available in the database only for the household head*/

 egen agua=max(v04b), by(hogar)
 egen lugabast=max(v04c), by(hogar)

* Gender classification of the population refers to the head of the household.

 noisily display "Improved Water Source"
 global variable WATER
 gen	 WATER=0 if (agua>=1 & agua<=7) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4) 
 global indicador 30 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE" 	
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	do "$DefvarsSRE2_SEXOD"
 	noisily do "$TablasSRE2"

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*
5. Servicio Sanitario
a) ¿Qué tipo de servicio sanitario tiene? (v05a)
 1. Inodoro conectado a red de alcantarilla
 2. Inodoro conectado a pozo séptico
 3. Letrina con cierre hidráulico
 4. Letrina con pozo séptico
 5. Letrina con pozo negro
 6. No tiene

c) El uso del servicio sanitario es:
 1. Exclusivo de la vivienda
 2. Compartido con otras viviendas
*/

/*These variables are  available in the database only for the household head*/

 egen sanita=max(v05a), by(hogar)
 egen servexc=max(v05b), by(hogar)

* Gender classification of the population refers to the head of the household.

 noisily display "Improved Sanitation"
 global variable SANITATION
 gen	 SANITATION=0 if (sanita>=1 & sanita<=6) /* Total population excluding missing information */
 replace SANITATION=1 if (sanita>=1 & sanita<=2) | sanita==4
 global indicador 31 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE" 	
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	do "$DefvarsSRE2_SEXOD"
 	noisily do "$TablasSRE2"

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*

1. Tipo de vivienda			10. ¿En caracter de qué ocupan esta vivienda?
v01					v09a
1. Casa o apartamento independiente	1. Propietario y esta pagada totalmente
2. Rancho				2. Propietario y la esta pagando
3. Cuarto en mesón o cuartería 		3. Alquilada
4. Barracón				4. Cedida sin pago
5. Casa improvisada			5. Recuperada legalizada
6. Local no construido para habitación	6. Recuperada sin legalizar
pero usado como vivienda		9. ns/n
7. Otro							
9. ns/nr 
		
2. Material predominante en		3. ¿Cuál es el material predominante en el piso?
la construcción de las paredes(v02)	v03
1. Ladrillo, piedra o bloque		1. Cerámica
2. Adobe				2. Ladrillo de cemento
3. Madera				3. Ladrillo de barro
4. Bahareque, vara o caña		4. Plancha de cemento
5. Desechos				5. Madera
6. Otro					6. Tierra
9. ns/nr				7. Otro
					9. ns/nr	

11. Cantidad de Piezas de la Vivienda
a). ¿Cuántas piezas tiene esta vivienda?
(incluya la cocina pero no el baño)
v11a
*/

/*These variables are  available in the database only for the household head*/

 egen tenencia=max(v09a), by(hogar)
 egen tipoviv=max(v01), by(hogar)
 egen piso=max(v03), by(hogar)
 egen pared=max(v02), by(hogar)
 recode v11a (40=.)
 egen nrocuart=max(v11a), by(id_viv)
 egen pers=max(totpervi ), by(hogar) /* Total de personas de la vivienda */

 gen persroom=pers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=7) & (tenencia>=1 & tenencia<=6) /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==2 | tipoviv==4 | tipoviv==5 | tipoviv==6 | tipoviv==7) | (tenencia==4)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (pared>=1 & pared<=6) & (piso>=1 & piso<=7) /* Total population excluding missing information */
 replace secten_2=1 if (pared>=4 & pared<=6) | (piso==6 | piso==7)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 noisily display "Secure Tenure"
 global variable SECTEN
 gen	 SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)
 global indicador 32 " "

  /* Urban indicators */
	do "$DefvarsS_SEXOD"
	noisily do "$TablasS_SECTEN"
	do "$DefvarsE"
	noisily do "$TablasE"
  /* Regional URBAN indicators */
	do "$DefvarsSURBANRE1_SEXOD"
	noisily do "$TablasSURBANRE1"
	do "$DefvarsSURBANRE2_SEXOD"
	noisily do "$TablasSURBANRE2"
	
* Dirt floors

* Gender classification of the population refers to the head of the household.

* 3. ¿Cuál es el material predominante en el piso?

 noisily display "Proportion of Population living in dwellings with dirt floors"
 global variable DIRT
 gen	 DIRT=0 if (piso>=1 & piso<=7) /* Total population excluding missing information */
 replace DIRT=1 if (piso==6)
 global indicador 88 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE" 	
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	do "$DefvarsSRE2_SEXOD"
 	noisily do "$TablasSRE2"

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
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional URBAN indicators */
	do "$DefvarsSURBANRE1"
	noisily do "$TablasSURBANRE1"
	do "$DefvarsSURBANRE2"
	noisily do "$TablasSURBANRE2"

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

/*
10. ¿Tiene en esta vivienda?
d. Teléfono
*/

 egen telefono=max(v10d), by(hogar)

 noisily display "Telephone Lines"
 global variable TELEPHONE
 gen	 TELEPHONE=0 if (telefono==1 | telefono==2) /* Total population excluding missing information */
 replace TELEPHONE=1 if (telefono==1)
 global indicador 47 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE" 	
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	do "$DefvarsSRE2_SEXOD"
 	noisily do "$TablasSRE2"

 noisily display "Telephone Lines"
 global variable TEL
 gen	 TEL=0 if (telefono==1 | telefono==2) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1)
 global indicador 99 " "

   /* National indicators */
 	do "$DefvarsS_SEXOD"
 	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE" 	
   /* Regional indicators */
 	do "$DefvarsSRE1_SEXOD"
 	noisily do "$TablasSRE1"
 	do "$DefvarsSRE2_SEXOD"
 	noisily do "$TablasSRE2" 	
 	
************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 noisily display "Children Under Age 15 who are Working"
 global variable CHILDREN
 gen CHILDREN=0 if      (edad>=12 & edad<15) 
 replace CHILDREN=1 if ((edad>=12 & edad<15) & (condact==1))
 global indicador 56 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"

** CCA 41 Number of Persons per Room*

 noisily display "Persons per Room"
 gen PERSROOM2=persroom if parentco==1
 global variable PERSROOM2
 global indicador 62 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"

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
	do "$DefvarsE"
	noisily do "$TablasE"
  /* Regional indicators */
	do "$DefvarsSRE1_SEXOD"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2_SEXOD"
	noisily do "$TablasSRE2"

** Disconnected Youths
/*
26. ¿Por qué no buscó trabajo ni trató de establecer su propio
negocio o finca?
1. Se incorporará a un trabajo antes de un mes			
2. Tiene trabajo asegurado después de un mes 			
3. Tiene respuesta a gestiones 						
4. Está esperando la siguiente temporada de trabajo 		
5. Cree que no encontrará trabajo					
6. Dejó de buscar trabajo momentáneamente			
7. No tiene tierra ni capital 						
8. No tiene tiempo para buscar trabajo				
9. No tiene necesidad de trabajar					
10. Por su edad no puede trabajar					
11.  Otros										

27. ¿Cuál es su condición actual?
1. Jubilado o pensionista
2. Rentista
3. Estudiante
4. Realiza los quehaceres del hogar
5. Incapacitado temporalmente
6. Incapacitado permanentemente
7. Otros
*/

 noisily display "Disconnected Youths"
 global variable DISCONN
 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & ((p27==7) & (p26==5 | p26==6 | (p26>=8 & p26<=11))) 
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
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"
	
* Primary completion rate [15 - 24 years of age]

 noisily display "Primary completion rate [15 - 24 years of age]"
 global variable PRIMCOMP
 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
 global indicador 93 " "

  /* National indicators */
	do "$DefvarsS"
	noisily do "$TablasS"
	do "$DefvarsE"
	noisily do "$TablasE"	
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSRE2"
	
* Average years of education of the population 15+

 noisily display "Average Years of Education"

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))
 global variable AEDUC_15
 global indicador 94 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"
	
 noisily display "Average Years of Education 15-24"

 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))
 global variable AEDUC_15_24
 global indicador 103 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"
	
 noisily display "Average Years of Education 25+"

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
 global variable AEDUC_25
 global indicador 104 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"
	
* Grade for age

 noisily display "Grade for age"
 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=18) & (anoest>=0 & anoest<99)
 global variable GFA
 global indicador 105 " "
 
  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"
	
* Grade for age primary

 noisily display "Grade for age primary"
 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
 global indicador 106 " "
 global variable GFAP
 
  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"
	
* Grade for age Secondary

 noisily display "Grade for age secondary"
 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=18) & (anoest>=0 & anoest<99)
 global variable GFAS
 global indicador 107 " "

  /* National indicators */
	do "$DefvarsSMean"
	noisily do "$TablasSMean"
	do "$DefvarsEMean"
 	noisily do "$TablasEMean"
  /* Regional indicators */
	do "$DefvarsSRE1"
	noisily do "$TablasSMeanRE1"
	do "$DefvarsSRE2"
	noisily do "$TablasSMeanRE2"
}

log close


