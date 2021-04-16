*Mayra Sáenz
*->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<-*
*              Malnutrition, Breastfeeding, and Anemia Indicators                                             *          
*->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<-*
clear all
use "${surveysFolder}\survey\JAM\SLC\2010\m5\data_merge\JAM_2010m5.dta"
set more off
*Main directory
*global path = "${surveysFolder}\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators"
global path ="${surveysFolder}\DATA.IDB\Documents\DIA_Publication\xls\JamaicaZscores"
* Folder of outputs
*global outputs = "${surveysFolder}\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators\outputs"
global outputs ="${surveysFolder}\DATA.IDB\Documents\DIA_Publication\xls\JamaicaZscores"

*Source of comparison: http://www.who.int/nutgrowthdb/database/countries/who_standards/jam_dat.pdf
*==============================================================================================================*
* Input Variables
*==============================================================================================================*

foreach v of varlist _all {
	local lowname=lower("`v'")
	capture rename `v' `lowname'
}


g edadmes = ageyrs * 12 + agemth if ageyrs <5
*Jamaica 
g factor =finwght
label var factor "Factor de expasion"

*Sex of child: 
destring sex, replace
rename sex sexo
label define sexo 1 "Sex of child: Male" 2 "Sex of child: Female"
label value sexo sexo

*Household wealth index:
rename popquint quintil
label define quintil 1 "Household wealth index: Lowest" 2 "Household wealth index: Second" 3 "Household wealth index: Middle" 4 "Household wealth index: Fourth" 5 "Household wealth index: Highest"
label value quintil quintil

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Variables for Malnutrition Indicators
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
recode c10 (99.99=.)
recode c11 (999.99=.)

g pesokg   = c10 
g alturacm = c11

destring alturacm, replace
/*x: Indicador de datos válidos */

gen x =1 if (edadmes ~=. & alturacm ~=. & pesokg ~=.) & (edadmes >0 & alturacm  >0 & pesokg  >0)
keep if x ==1

*Muestra total
egen mtot =sum(x) if (edadmes ~=. & alturacm ~=. & pesokg ~=.) & (edadmes >0 & alturacm  >0 & pesokg  >0)

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*Para desagregaciones
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*Residence: Urban and Rural

destring area, replace
g residence =  1 if (area == 1  | area ==2 | area ==4 | area ==5)
replace residence = 2 if (area ==3)

label define residence 1 "Residence: Urban" 2 "Residence: Rural"
label value residence residence
label var residence "Residence 1=urban"


*Age in months intervals for malnutrition indicators: 
g amonth = .
replace amonth = 1 if edadmes <6
replace amonth = 2 if edadmes >=6 & edadmes <=8
replace amonth = 3 if edadmes >=9 & edadmes <=11
replace amonth = 4 if edadmes >=12 & edadmes <=17
replace amonth = 5 if edadmes >=18 & edadmes <=23
replace amonth = 6 if edadmes >=24 & edadmes <=35
replace amonth = 7 if edadmes >=36 & edadmes <=47
replace amonth = 8 if edadmes >=48 & edadmes <=59
label define amonth 1 "Age in months: <6" 2 "Age in months:6-8" 3 "Age in months: 9-11" 4 "Age in months: 12-17" 5 "Age in months: 18-23" 6 "Age in months: 24-35" 7 "Age in months: 36-47" 8 "Age in months: 48-59"
label value amonth amonth
label var amonth "Intervals of age in months"


*Mother's education
 g motheredu =.

 label var motheredu "Mother's education"
 label define motheredu 1 "Mother's education: No education" 2 "Mother's education: Primary" 3 "Mother's education: Secondary or higher" 4 "Mother's education: Missing"
 label value motheredu motheredu

* Measured lying or standing (1=lying 2=standing 9=missing)
destring c12, replace
rename c12  how_meas
recode how_meas (97=.) (99=.)


destring residence edadmes sexo alturacm pesokg how_meas motheredu, replace


*==============================================================================================================*
*                                      Malnutrition Indicators                                                 *
*==============================================================================================================*


zscore06, a(edadmes) s(sexo) h(alturacm) w(pesokg) measure(how_meas)

foreach var of varlist haz06 waz06 whz06 bmiz06 {
recode `var' (99=.)
}


tabstat quintil haz06   [w=factor]if (haz06> -6 & haz06<= 6), by(quintil) stat(mean)
tabstat residence haz06 [w=factor] if (haz06> -6 & haz06<= 6), by(residence) stat(mean)
tabstat amonth haz06    [w=factor] if (haz06> -6 & haz06<= 6), by(amonth) stat(mean)
tabstat sexo haz06      [w=factor] if (haz06> -6 & haz06<= 6), by(sexo) stat(mean)
tabstat haz06           [w=factor] if (haz06> -6 & haz06<= 6),  stat(mean)

