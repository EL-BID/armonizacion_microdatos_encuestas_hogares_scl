*Mayra Sáenz
*->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<-*
*              Malnutrition, Breastfeeding, and Anemia Indicators                                             *          
*->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<-*
clear all
use "${surveysFolder}\survey\JAM\SLC\2002\m5\data_merge\JAM_2002m5.dta"
set more off
*Main directory
global path = "${surveysFolder}\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators"
* Folder of outputs
global outputs = "${surveysFolder}\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators\outputs"

*Source of comparison: http://www.who.int/nutgrowthdb/database/countries/who_standards/jam_dat.pdf
*==============================================================================================================*
* Input Variables
*==============================================================================================================*

foreach v of varlist _all {
	local lowname=lower("`v'")
	capture rename `v' `lowname'
}
/*
ds, has(type string)
    foreach var of varlist `r(varlist)' {
	    replace `var' = "." if strpos(`var',"N")
        replace `var' = "." if strpos(`var',"NN")
		replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"NNN")
		replace `var' = "." if strpos(`var',"NNNN")
		replace `var' = "." if strpos(`var',"NNNNN")
				       destring `var', replace
    }
*/

/*
tostring int_date, replace
g e_ano  = substr(int_date,-2,.)
g e_mes  = substr(int_date,-4,2)
g e_dia  = substr(int_date,-6,2)
g e_dia1 = substr(int_date,-5,1)



replace e_ano = "2002" if e_ano =="02"
replace e_ano = "2003" if e_ano =="03"
replace e_ano = "2004" if e_ano =="04"
replace e_ano = "." if e_ano =="20" | e_ano=="82"

gen emes =  e_mes if e_mes >= "03" & e_mes <= "12"
replace e_mes = emes

gen edia =  e_dia if e_dia >= "01" & e_dia <= "31"
replace e_dia = edia

gen edia1 =  e_dia1 if e_dia1 >= "0" & e_dia1 <= "9"
replace e_dia1 = edia1

destring e_ano e_mes e_dia e_dia1, replace
replace e_dia = e_dia1 if e_dia==.
*replace e_ano = 2000+e_ano
  
 rename c011 b_day
 rename c012 b_mth
 rename c013 b_year
 
 
 gen anios= "19" if  b_year >=89 & b_year <=99
 replace anios = "200" if  b_year ==0 | b_year ==1 | b_year ==2
 tostring b_year, replace
 egen b_year1 = concat(anios b_year) if b_year !="."
 replace b_year= b_year1
 destring b_year, replace
 drop b_year1 anios
 
* edad en meses
destring  b_day  b_mth b_year e_ano e_mes e_dia, replace
recode b_mth 99=.
g       edadmes = (e_ano - b_year)*12 + (e_mes - b_mth) + (e_dia - b_day)/30.416667
replace edadmes=. if edadmes<0
*/
rename c061 age_yrs
rename c062 age_mths

replace age_yrs = age if age_yrs == 9 & age_mths ==99
recode age_mths 99=. if age_mths ==99
recode age_mths 9=. if age_yrs != age
replace age_yrs = age if age_yrs != age & age_mths ==.

g edadmes = age_yrs * 12 + age_mths if age_yrs <5


*Jamaica no necesita factor de expansión.
g factor =1
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

g pesokg   = c10 
g alturacm = c11

destring alturacm, replace
/*x: Indicador de datos válidos */
gen x = 1 if (edadmes ~=. & alturacm ~=. & pesokg ~=.)
*Muestra total
egen mtot =sum(x)


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
 /*
 replace motheredu = 1 if hc61==0
 replace motheredu = 2 if hc61==1
 replace motheredu = 3 if hc61==2 | hc61==3
 replace motheredu = 4 if hc61==9
 */
 label var motheredu "Mother's education"
 label define motheredu 1 "Mother's education: No education" 2 "Mother's education: Primary" 3 "Mother's education: Secondary or higher" 4 "Mother's education: Missing"
 label value motheredu motheredu

* Measured lying or standing (1=lying 2=standing 9=missing)
destring c12, replace
rename c12  how_meas


destring residence edadmes sexo alturacm pesokg how_meas motheredu, replace


*==============================================================================================================*
*                                      Malnutrition Indicators                                                 *
*==============================================================================================================*

zscore06, a(edadmes) s(sexo) h(alturacm) w(pesokg) measure(how_meas)

*Height-for-age below -2 SD
g hab2sd    = (haz06> -6 & haz06<-2)*100 if (haz06> -6 & haz06< 6)
*Height-for-age below -3 SD
g hab3sd    = (haz06> -6 & haz06<-3)*100 if (haz06> -6 & haz06< 6)
*Height-for-age mean Z-score
/*
The mean z-score is calculated as one of the summary statistics to represent the nutritional status of children
in a population. This indicator describes the nutritional status of the population as a whole without the use of 
a cut-off. A mean z-score of less than 0, i.e., a negative value, for stunting, wasting, or underweight, suggests
the nutritional status of the survey population is poorer on average than that of the WHO Growth Standards population.*/
egen hamzs_mtot   = mean(haz06) if (haz06> -6 & haz06< 6)

*Weight-for-age above +2 SD
*El segundo quintil tiene un punto porcentual más. La diferencia disminuye un poco si no se incluye al 2.
g waa2sd  = (waz06>= 2 & waz06< 5)*100 if (waz06> -6 & waz06< 5)

*Weight-for-age below -2 SD
g wab2sd = (waz06> -6 & waz06<-2)*100 if (waz06> -6 & waz06< 5)

*Weight-for-age below -3 SD
g wab3sd = (waz06> -6 & waz06<-3)*100 if (waz06> -6 & waz06< 5)

*Weight-for-age mean Z-score
egen wamzs_mtot    = mean(waz06) if (waz06> -6 & waz06< 5)

*Weight-for-height above +2 SD
g wha2sd  = (whz06>= 2 & whz06< 5)*100 if (whz06>=-5 & whz06< 5)
*Weight-for-height below -2 SD
g whb2sd  = (whz06>=-5 & whz06<-2)*100 if (whz06>=-5 & whz06< 5)

*Weight-for-height below -3 SD
g whb3sd  = (whz06>=-5 & whz06<-3)*100 if (whz06>=-5 & whz06< 5)
*Weight-for-height mean Z-score
egen whmzs_mtot    = mean(whz06) if (whz06>=-5 & whz06< 5)
*Se genera mtot sólo para fines de subtítulo de la tabla



*____________________*
* Desagregaciones    *
*____________________*

bys residence: egen nres = sum(x)
bys amonth: egen nage = sum(x) 
bys quintil: egen nquint = sum(x) 
bys motheredu: egen nmedu = sum(x) 
bys sexo: egen nsex = sum(x) 

*Mean Z scores
*HAZ06
bys residence: egen haz06_res = mean(haz06) if (haz06> -6 & haz06< 6) 
bys amonth: egen haz06_age = mean(haz06) 
bys quintil: egen haz06_quint = mean(haz06) 
bys motheredu: egen haz06_medu = mean(haz06) 
bys sexo: egen haz06_sex = mean(haz06) 

*waz06
bys residence: egen waz06_res = mean(waz06) if (waz06> -6 & waz06< 6) 
bys amonth: egen waz06_age = mean(waz06) 
bys quintil: egen waz06_quint = mean(waz06) 
bys motheredu: egen waz06_medu = mean(waz06) 
bys sexo: egen waz06_sex = mean(waz06) 

*whz06
bys residence: egen whz06_res = mean(whz06) if (whz06> -6 & whz06< 6) 
bys amonth: egen whz06_age = mean(whz06) 
bys quintil: egen whz06_quint = mean(whz06) 
bys motheredu: egen whz06_medu = mean(whz06) 
bys sexo: egen whz06_sex = mean(whz06) 


foreach i in hab2sd hab3sd waa2sd wab2sd wab3sd wha2sd whb2sd whb3sd {

*Total
egen `i'mtot = mean(`i') 
*Residence: Urban/Rural
bys residence: egen `i'res = mean(`i') 
*Age in months:
bys amonth: egen `i'age = mean(`i') 
*Size at birth: Very small
*Size at birth: Small
*Size at birth: Average or larger
*Size at birth: Missing

*Household wealth index: 
bys quintil: egen `i'quint = mean(`i') 
*Mother's education
bys motheredu: egen `i'medu = mean(`i') 

*Birth interval: First birth
*Birth interval: Less than 24 months
*Birth interval: 24-47 months
*Birth interval: 48+ months
*Sex of child
bys sexo: egen `i'sex = mean(`i') 

*Mother's interview status: Missing
 }

 *=========================================================================================*
 * Generación de bases de datos por cada tipo de desagregación                              *
 *=========================================================================================*
 
 
  preserve
*Se genera total sólo para fines de subtítulo de la tabla
capture g total = 1
label define total 1 "Total: Total"
label value total total
collapse (max) *mtot [iw=factor],  by(total)
saveold "$outputs\malnut_temporal.dta", replace
export excel using "$outputs\health_indicators.xlsx", sheet("Malnutrition02") sheetmodify cell(B13)   
restore

 preserve
collapse (max) *res [iw=factor], by(residence)
saveold "$outputs\malnut_temporal.dta", replace
export excel using "$outputs\health_indicators.xlsx", sheet("Malnutrition02") sheetmodify cell(B14) 
restore

 preserve
 *elimino la variable original mortgage temporalmente, ya que me da problemas en el collapse
 capture drop mortgage
 rename age edad
 destring *age, replace
collapse (max) *age [iw=factor], by(amonth)
saveold "$outputs\malnut_temporal.dta", replace
export excel using "$outputs\health_indicators.xlsx", sheet("Malnutrition02") sheetmodify cell(B16) 
restore

preserve
collapse (max) *quint [iw=factor], by(quintil)
saveold "$outputs\malnut_temporal.dta", replace
export excel using "$outputs\health_indicators.xlsx", sheet("Malnutrition02") sheetmodify cell(B24) 
restore

/*
preserve
collapse (max) *medu [iw=factor], by(motheredu)
saveold "$outputs\malnut_temporal.dta", replace
export excel using "$outputs\health_indicators.xlsx", sheet("Malnutrition02") sheetmodify cell(B32) firstrow(variables)
restore
*/

preserve
collapse (max) *sex [iw=factor], by(sexo)
saveold "$outputs\malnut_temporal.dta", replace
export excel using "$outputs\health_indicators.xlsx", sheet("Malnutrition02") sheetmodify cell(B29) 
restore



