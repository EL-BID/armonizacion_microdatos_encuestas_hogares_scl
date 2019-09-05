*Mayra Sáenz
*->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<-*
*              Malnutrition, Breastfeeding, and Anemia Indicators                                             *          
*->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<->:<-*

clear all
use "Z:\survey\JAM\SLC\2000\m5\data_merge\JAM_2000m5.dta"
set more off
*Main directory
*global path = "D:\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators"
global path ="D:\DATA.IDB\Documents\DIA_Publication\xls\JamaicaZscores"
* Folder of outputs
*global outputs = "D:\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators\outputs"

global outputs ="D:\DATA.IDB\Documents\DIA_Publication\xls\JamaicaZscores"
*Source of comparison: http://www.who.int/nutgrowthdb/database/countries/who_standards/jam_dat.pdf
*==============================================================================================================*
* Input Variables
*==============================================================================================================*

foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}


rename c031 age_yrs
rename c032 age_mths

replace age_yrs = age if age_yrs == 9 & age_mths ==99
recode age_mths 99=. if age_mths ==99
recode age_mths 9=. if age_yrs != age
replace age_yrs = age if age_yrs != age & age_mths ==.

g edadmes = age_yrs * 12 + age_mths if age_yrs <5

*g edadmes = age * 12 + age_mths if age <5

*Jamaica no necesita factor de expansión.
g factor =1
label var factor "Factor de expasion"

preserve
*Para calcular asistencia escolar de niños de 6-11 años
g asiste_ci = (b01 !=19 & b01 !=.)
g q5 = popquint 
g edad_ci = age

tab asiste_ci [w=factor] if edad_ci >=6 & edad_ci <=11
table q5 asiste_ci if edad_ci >=6 & edad_ci <=11

restore



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

g pesokg   = c07 
g alturacm = c08

destring alturacm, replace
/*x: Indicador de datos válidos */
gen x = 1 if (edadmes ~=. & alturacm ~=. & pesokg ~=.)
*Muestra total
egen mtot =sum(x) if (edadmes ~=. & alturacm ~=. & pesokg ~=.)

keep if x ==1
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*Para desagregaciones
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*Residence: Urban and Rural

destring area, replace
g residence =  1 if (area == 1  | area ==2)
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
destring c09, replace
rename c09  how_meas
recode how_meas (9=.)


destring residence edadmes sexo alturacm pesokg how_meas motheredu , replace


*==============================================================================================================*
*                                      Malnutrition Indicators                                                 *
*==============================================================================================================*

zscore06, a(edadmes) s(sexo) h(alturacm) w(pesokg) measure(how_meas)
foreach var of varlist haz06 waz06 whz06 bmiz06 {
recode `var' (99=.)
}


*Height-for-age mean Z-score
tabstat quintil haz06   [w=factor]if (haz06> -6 & haz06<= 6), by(quintil) stat(mean)
tabstat residence haz06 [w=factor] if (haz06> -6 & haz06<= 6), by(residence) stat(mean)
tabstat amonth haz06    [w=factor] if (haz06> -6 & haz06<= 6), by(amonth) stat(mean)
tabstat sexo haz06      [w=factor] if (haz06> -6 & haz06<= 6), by(sexo) stat(mean)
tabstat haz06           [w=factor] if (haz06> -6 & haz06<= 6),  stat(mean)

keep residence edadmes sexo alturacm pesokg how_meas motheredu quintil haz06 waz06 whz06 factor amonth
g pais_c = "JAM"
g anio_c =2000

saveold "D:\DATA.IDB\Documents\DIA_Publication\db\JAMzscor2000", replace


*Jamaica 2010

clear all
use "Z:\survey\JAM\SLC\2010\m5\data_merge\JAM_2010m5.dta"
set more off
*Main directory
*global path = "D:\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators"
global path ="D:\DATA.IDB\Documents\DIA_Publication\xls\JamaicaZscores"
* Folder of outputs
*global outputs = "D:\DATA.IDB\Anexos_Encuestas_de_Salud\Health_Indicators\outputs"
global outputs ="D:\DATA.IDB\Documents\DIA_Publication\xls\JamaicaZscores"

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


preserve
*Para calcular asistencia escolar de niños de 6-11 años
g asiste_ci = (b1 !=19 & b1 !=. & b1 !=97)
g q5 = quintil 
g edad_ci = ageyrs

tab asiste_ci [iw=factor] if edad_ci >=6 & edad_ci <=11
table q5 asiste_ci [iw=factor] if edad_ci >=6 & edad_ci <=11

restore



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



keep residence edadmes sexo alturacm pesokg how_meas motheredu quintil haz06 waz06 whz06 factor amonth
g pais_c = "JAM"
g anio_c =2010

saveold "D:\DATA.IDB\Documents\DIA_Publication\db\JAMzscor2010", replace





append using "D:\DATA.IDB\Documents\DIA_Publication\db\JAMzscor2000"



*Grafico 1: Cada panel dos años

qui kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2000, generate(x_2000 y_2000)
qui kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010, generate(x_2010 y_2010)

#delimit ;
twoway (line y_2000 x_2000, lcolor(black) lwidth(medthick) lpattern(dash)) (line y_2010 x_2010,
xline(-2, lwidth(medthick) lcolor(red)) xline(-3, lwidth(medthick) lcolor(red)) lcolor(blue) 
lwidth(medthick)) ,   legend(order(1 "2000" 2 "2010") textwidth(25)) 
xtitle(Height for age mean Z score) ytitle(Density) xlabel(-5(1)5) scheme(s1color);
#delimit cr

graph export "D:\DATA.IDB\Documents\DIA_Publication\graphs\JAMFig100_10.pdf", as(pdf)replace
graph export "D:\DATA.IDB\Documents\DIA_Publication\graphs\JAMFig100_10.png", as(png)replace

*Grafico 2: Cada panel con la ultima encuesta de cada pais por quintiles
gen q5= quintil

kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010 & q5==1, generate(x_Q1 y_Q1)
kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010 & q5==2, generate(x_Q2 y_Q2)
kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010 & q5==3, generate(x_Q3 y_Q3)
kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010 & q5==4, generate(x_Q4 y_Q4)
kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010 & q5==5, generate(x_Q5 y_Q5)

#delimit ;
twoway (line y_Q1 x_Q1, lwidth(medthick) lcolor(black) lpattern(dash)) 
(line y_Q2 x_Q2, lwidth(medthick) lcolor(edkblue) lpattern(solid) ) 
(line y_Q3 x_Q3,  lwidth(medthick)lcolor(gs4) lpattern(solid)) 
(line y_Q4 x_Q4,  lwidth(medthick) lcolor(emidblue) lpattern(longdash)) 
(line y_Q5 x_Q5,  lwidth(medthick) lcolor(gs8) lpattern(dash dot) xline(-2, lwidth(medthick) lcolor(red)) 
xline(-3, lwidth(medthick) lcolor(red))),  legend(order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5") 
textwidth(25))  xtitle(Height for age mean Z score) ytitle(Density)  xlabel(-5(1)5); 
#delimit cr

graph export "D:\DATA.IDB\Documents\DIA_Publication\graphs\JAMFig200_10.pdf", as(pdf)replace
graph export "D:\DATA.IDB\Documents\DIA_Publication\graphs\JAMFig200_10.png", as(png)replace


*Grafico 3: Cada panel con la última encuesta de cada pais por zona
*----------
kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010  & residence==1, generate(x_urb y_urb)
kdensity  haz06 if (haz06>= -6 & haz06<=6) & anio_c==2010  & residence==2, generate(x_rur y_rur)


#delimit ;
twoway (line y_urb x_urb, lcolor(black) lwidth(medthick) lpattern(dash)) (line y_rur x_rur,
xline(-2, lwidth(medthick) lcolor(red)) xline(-3, lwidth(medthick) lcolor(red)) lcolor(blue) 
lwidth(medthick)) ,   legend(order(1 "Urban" 2 "Rural") textwidth(25)) 
xtitle(Height for age mean Z score) ytitle(Density) xlabel(-5(1)5) scheme(s1color);
#delimit cr

graph export "D:\DATA.IDB\Documents\DIA_Publication\graphs\JAMFig300_10.pdf", as(pdf)replace
graph export "D:\DATA.IDB\Documents\DIA_Publication\graphs\JAMFig300_10.png", as(png)replace
