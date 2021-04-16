* Description: Use the final Database from Antigua and Nueva
* Input : Old and new database.
* Output: Append Database so they are ready to work. 
* Period: 1995 - 2010
* Created by: Julian Messina and Manuel Fernandez


***************************************
***************************************

clear
set mem 1300m
set more off
capture log close
capture clear matrix
capture scalar drop _all


**Note: this local should be change by the user
cd ${surveysFolder}\Users\wb377246\Documents\Julian\LM_W\BRA

***************************************
***************************************


**s1. Open old dataset (from 1982-I to 2001-III); keep observations with wage data and append to rest of dataset (2002-I to 2010-I) 

use Data\PMEALLANTIGUA_Final-unemployment, clear

count
keep if wage_a!=.

gen base=1

* Add a level to de ID so that...

append using Data\PMEALLNUEVA_Final-unemployment
count
keep if wage_a!=.

replace base=2 if base==.

* keep the unemployment rates to be used
drop td11 td12 td13 td14 td15


* Generate an id once we have grouped the datasets
egen id2=group(id base)
drop id 
rename id2 id
order id

**s2. Rename variables so that they are the same for brazil and Argentina

rename age_calculed edad
rename cartera employment
recode employment (0=2) 

rename yeduc aedu

gen edadsq=edad*edad

rename peso pondera
rename metro_area aglomerado
rename id id_ind
rename male hombre
rename wage_a wage_m

**s3. generate groups for clusters and time dummys

tostring year, gen(y)
tostring quarter, gen(q)
gen yq=y+q
destring yq, replace force
label var yq "Year-Quarter ID"
drop y q 


duplicates tag id_ind month, gen(dup1)
duplicates tag id_ind, gen(dup)

egen aglo_yq=group(yq aglomerado)
egen aglo_ag=group(aglomerado age_td)
egen aglo_ag_sex=group(aglomerado age_td hombre)
egen aglo_ag_yq=group(aglomerado age_td yq)
order yq aglo_yq aglo_ag aglo_ag_yq



**s21. Merge with dataset of CPI (Originally from ILS)

merge year quarter using Data\CPI, uniqusing sort
drop if _merge==2
drop _merge



**------CURRENCY CHANGES IN BRAZIL--------**
* Divide by 1000 before 1986/4
* Divide by 1000 before 1989/3
* Divide by 1000 before 1993/9
* Divide by 2750 before 1994/8

tostring year, gen(anio)
tostring month, gen(mess)
replace mess="0"+mess if real(mess)<10

gen tiempo=anio+mess

replace wage_m=wage_m/1000 if real(tiempo)<198604
replace wage_m=wage_m/1000 if real(tiempo)<198903
replace wage_m=wage_m/1000 if real(tiempo)<199309
replace wage_m=wage_m/2750 if real(tiempo)<199408

drop tiempo

**------CURRENCY CHANGES IN BRAZIL--------**


**s22. change the wage variable into real terms (2010-1=123.545) and generate a log

gen wage_m_real=(wage_m/CPI)*123.545
gen log_wage=ln(wage_m_real)

preserve
keep log_wage
histogram log_wage
restore

rename td9 td8
rename td10 td9
rename td16 td14


preserve
keep log_wage
histogram log_wage
restore 

**s23 Since we want quarterly data, but have monthly observations. We collapse the dataset by quarter adn individual 

* Variable with no variations
global nv aglo_yq aglo_ag aglo_ag_yq  aglomerado hombre work_status toccu employment year edad aedu ciiu ///
pondera age_td quarter td8 td9 td14 edadsq aglo_ag_sex

* Variable with variation
global vv wage_m wage_m_real log_wage

collapse (first) $nv (mean) $vv, by(yq id_ind)

		label var hombre 			"=1 if person is male"
		label var wage_m 			"wage (actual)"
		label var ciiu			"ciiu activities"
		label var employment 		"1 if formal, 0 if informal. carteira de trabalho in this job?"
		label var aedu			"categories of years of education created by IGBE"
		label var pondera			"expansion factor"
		
		label var work_status 		"status in the labor force"
		label var toccu			"type of occupation"
		label var edadsq			"age squared"

order  yq id_ind aglo_yq aglo_ag aglo_ag_yq aglo_ag_sex

		label var id_ind			"Individual ID"
		label var  yq 			"year - quarter"
		label var aglo_yq 		"group (aglomerado yq)"
		label var aglo_ag 		"group (aglomerado age_group)"
		label var aglo_ag_yq 		"group (aglomerado yq age_group)"
		label var aglomerado 		"aglomerado (region)"
		label var aglo_ag_sex  		"group (aglomerado age group_sex)"

		label var td8 			"tasa de desempleo nacional trimestral"
		label var td9			"tasa de desempleo regional trimestral"
		label var td14			"tasa de desempleo trimestral por region, edad y sexo"

compress
save Data\bra_ALL_LCRCE, replace
