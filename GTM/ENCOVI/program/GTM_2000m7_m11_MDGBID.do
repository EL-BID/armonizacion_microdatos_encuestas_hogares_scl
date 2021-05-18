
use ${surveysFolder}\ARM\GUA\2000\Arm_data\GUA2000EA_BID_2.dta, clear

keep factor_ci -  region
sort idh_ch idp_ci

save ${surveysFolder}\ARM\GUA\2000\Arm_data\GUA2000EA_BID.dta, replace

use ${surveysFolder}\ARM\GUA\2000\Orig_data\gta00.dta, clear

gen idh_ch=id_hogar
gen idp_ci=id_pers

sort idh_ch idp_ci
drop _merge

merge idh_ch idp_ci using ${surveysFolder}\ARM\GUA\2000\Arm_data\GUA2000EA_BID.dta

save ${surveysFolder}\ARM\GUA\2000\Arm_data\GUA2000EA_BID.dta, replace


 tab area [iw=factorex]

* Gender classification of the population refering to the head of the household.

 sort id_hogar id_pers

 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hog)

 tab sexo   [iw=factorex]
 tab sexo_d [iw=factorex]
 
 tab sexo sexo_d if parentco==1

 sort id_hogar id_pers
 
** Years of education. 
* Included in the database
 
** Economic Active Population 
* Included in the database

 gen	 TASADESO=0 if condact==1
 replace TASADESO=1 if condact==2 | condact==3

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if (edad>=7 & edad<=12) & (seinscri>=1 & seinscri<=2)
 replace NERP=1 if (edad>=7 & edad<=12) & (niveli==2)
  
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (seinscri>=1 & seinscri<=2)
 replace NERS=1 if (edad>=13 & edad<=18) & (niveli==3)

** Upper secondary
* Educación Media - Ciclo Diversificado

 gen     NERS2=0 if (edad>=16 & edad<=18) & (seinscri>=1 & seinscri<=2)
 replace NERS2=1 if (edad>=16 & edad<=18) & (niveli==3) & (gradoi>=4 & gradoi<=6)
  
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen	 alfabet=1 if alfabes>=1 & alfabes<=2
 replace alfabet=0 if alfabet==.


** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen     ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==0)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1)

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if niveli==2
 gen sec=1  if niveli==3
 gen ter=1  if niveli==4

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen     RATIOLIT2=0 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ>=1 & categ<=3) & (ramar>=2 & ramar<=12) & (condact==1)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ>=1 & categ<=3) & (ramar>=2 & ramar<=12) & (condact==1) & (sexo==2)

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen     WENASD=0 if (edad>=15 & edad<=64) & (categ>=1 & categ<=4) & (ramar>=2 & ramar<=12) & (condact==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ>=1 & categ<=4) & (ramar>=2 & ramar<=12) & (condact==1) & (sexo==2)


*Proportion of 1 Year Old Children Immunized Against Measles*

 gen	 MEASLES=0 if (edad>=1 & edad<=4) & (saram==1 | saram==2)
 replace MEASLES=1 if (edad>=1 & edad<=4) & (saram==1)

*Proportion of Births Attended by Skilled Health Personnel*
* 12 to 49 in survey questionnaire

 gen	 SKILLED=0 if (qaten>=1 & qaten<=8) & (edad>=15 & edad<=49)
 replace SKILLED=1 if (qaten>=2 & qaten<=6) & (edad>=15 & edad<=49)

*Condom Use of the Contraceptive Prevalence Rate*

 gen	 CONDOM=0 if ((edad>=15 & edad<=49) & (estcivil==1 | estcivil==2) &  (sexo==2)  & (usado01==1 | usado02==1| usado03==1 | usado04==1 | usado05==1 | usado06==1 | usado07==1 | usado08==1))
 replace CONDOM=1 if ((edad>=15 & edad<=49) & (estcivil==1 | estcivil==2) &  (sexo==2)  & (usado07==1)) 

*Contraceptive Prevalence Rate*

 gen	 CONTRACEPTIVE=0 if ((edad>=15 & edad<=49) & (estcivil==1 | estcivil==2)) & (sexo==2)
 replace CONTRACEPTIVE=1 if ((edad>=15 & edad<=49) & (estcivil==1 | estcivil==2)) & (usado01==1 | usado02==1| usado03==1 | usado04==1 | usado05==1 | usado06==1 | usado07==1 | usado08==1) &  (sexo==2)
 

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (conecele>=1 & conecele<=2) /* Total population excluding missing information */
 replace ELEC=1 if (conecele==1) 
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (servagua>=1 & servagua<=8) /* Total population excluding missing information */
 replace WATER=1 if (servagua>=1 & servagua<=4) 
	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (servsani>=1 & servsani<=5) /* Total population excluding missing information */
 replace SANITATION=1 if (servsani>=1 & servsani<=2) 
 	
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

 gen persroom=pers/cuartdis /* Personas en el hogar / total de cuartos del hogar 
 							(excl. cocina, baños, pasillos, 
 							garajes y los dedicados a negocios) */

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tenencia>=1 & tenencia<=7) & (tipoviv>=1 & tipoviv<=6)  /* Total population excluding missing information */
 replace secten_1=1 if (tenencia>=6 & tenencia<=7) | (tipoviv>=4 & tipoviv<=6)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (paredes>=1 & paredes<=9) & (piso>=1 & piso<=6) /* Total population excluding missing information */
 replace secten_2=1 if (paredes>=6 & paredes<=9) | (piso>=5 & piso<=6)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 gen	 SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=6) /* Total population excluding missing information */
 replace DIRT=1 if (piso==5)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (TASADESO==0 | TASADESO==1) & (edad>=15 & edad<=24)
 replace UNMPLYMENT15=1 if (TASADESO==1)	       & (edad>=15 & edad<=24)
	
** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (telefijo==1 | telefijo==2) & (celular==1 | celular==2) /* Total population excluding missing information */
 replace TELCEL=1 if  (telefijo==1 		  |  celular==1)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefijo==1 | telefijo==2) /* Total population excluding missing information */
 replace TEL=1 if (telefijo==1)
	
** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (celular==1 | celular==2) /* Total population excluding missing information */
 replace CEL=1 if (celular==1)

** Target 18, Indicator: "Personal computers in use per 100 population"

 gen	 COMPUTER=0 if (tiene20>=0 & tiene20<=5)
 replace COMPUTER=1 if (tiene20>=1 & tiene20<=5)

* Target 18, Indicator: "Internet users per 100 population"

 gen	 INTUSERS=0 if (internet==1 | internet==2)
 replace INTUSERS=1 if (internet==1)
	
************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* As a percentaje of the economic active population consider in the survey
* 12 to 14

 gen	 CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & (condact==1)

** CCA 41 Number of Persons per Room*

 gen PERSROOM2=persroom if parentco==1

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

** Disconnected Youths

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (rznobus==4 | (rznobus>=9 & rznobus<=13) | rznobus==99)
 
******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

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

 gen	 REZ=0 if (edad>=8 & edad<=18) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=8 & edad<=18) & (rezago==1)
	
* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
	
* Average years of education of the population 15+

 noisily display "Average Years of Education"

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))

 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=18) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=18) & (anoest>=0 & anoest<99)

save ${surveysFolder}\ARM\GUA\2000\Arm_data\GUA2000EA_BID.dta, replace
