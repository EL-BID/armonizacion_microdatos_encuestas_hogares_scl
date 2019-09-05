/*
** Description: This program takes the output of program 3, generates the individual id and creates the unemployment rate for the Antigua database.
			*It treats the year 1982 differently since we do not have the weight for that year. In step 4 of this program it creates the 
			* unemployment rate for the year 1983 to 2001 and in step 5 it creates the rate for 1982 without weigth. Step 6 appends the database.
** Input      : Output of Program 3.
** Output     : variables of interest for the study on unemployment, including unemployment rate per month and quarter 1982 to September 2001
** by Julian Messina and David Argente
*/

clear

set mem 1300m, perm
set more off
*set trace on
capture log close

**Note: this local should be change by the user
local propiodir = "C:\Users\wb377246\Documents\Julian\LM_W"

*** Declare Global Directory
global dir	  = "`propiodir'\BRA\Data\"
global dirin  = "`propiodir'\BRA\Data\pme\"


*** Use TOTAL data: Output Program 3 ***
use "$dirin\\PMEANTIGUA_82_91.dta"

append using "$dirin\\PMEANTIGUA_92_01.dta"

**************************************************************
* Keep the variables to be use in the unemployment exercises
**************************************************************

keep  control serie metro_area orden month male cfamilia nfamilia dob v001 ordem  /*
*/ mob work_status toccu cartera serie year yob age_calculed wage_a yeduc ciiu peso v001 condom npess
compress


* Generate the individual id. 

		order v001 serie metro_a orden ordem condom male dob mob yob
		qui egen id = group (v001 serie metro_a orden ordem condom male dob mob yob)
	
		local propiodir = "C:\Users\wb377246\Documents\Julian\LM_W"

		drop v001 serie orden ordem condom dob mob yob control npess cfamilia nfamilia 
		order id
		drop if id == .
		
		label var id "individual id"

		** Generate the control variable to see how many times the id appears in the database
		qui bys id: egen control_id = count(id)	
		drop if control_id > 8
		drop control_id	
		count

		* Same id, month and year
		duplicates drop id month year, force
	

sort id year month	


* 2. *** Crea la variable age_td para las tasas de desempleo

gen age_td = .
replace age_td = 1 if  age_calculed <25
replace age_td = 2 if  age_calculed>=25 &  age_calculed<40
replace age_td = 3 if  age_calculed>=40 &  age_calculed<55
replace age_td = 4 if  age_calculed>=55 

label var age_td "=1 [0,25) =2 [25,40) =3 [40,55) =4 [55,max]"

* 2.1 *** Crea la variable quarter para calcular las tasas de desempleo por trimestre

gen quarter=.
replace quarter=1 if month ==1 | month ==2 | month ==3
replace quarter=2 if month ==4 | month ==5 | month ==6
replace quarter=3 if month ==7 | month ==8 | month ==9
replace quarter=4 if month ==10| month==11 | month ==12
 


* 3 *** Creating the variables activo (pea) and desa  (unemployed)***

capture drop activo
capture drop desa

drop if age_calculed<14
drop if work_status == 9 | work_status==.

gen activo = .
replace activo = 1 if work_status == 1 | work_status == 2 | work_status == 3 

gen desa = .
replace desa = 1 if work_status == 3

label var activo "PEA"
label var desa "Unemployed"

tab work_status

/*
status in the |
       labor force |      Freq.     Percent        Cum.
-------------------+-----------------------------------
            worker | 10,508,055       52.55       52.55
worker but no work |    456,726        2.28       54.83
        job search |    657,339        3.29       58.12
           retired |  2,071,125       10.36       68.48
           student |  2,178,707       10.90       79.37
          domestic |  3,691,700       18.46       97.83
             other |    433,196        2.17      100.00
-------------------+-----------------------------------
             Total | 19,996,848      100.00


*/


compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace		



*4.9 Tasa de desempleo nacional trimestral

collapse (sum) desa activo [fw=peso], by (year quarter)
gen td9 = (desa/activo)*100

egen codmerge = group (year quarter)
sort codmerge
drop desa activo year quarter

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td9 "tasa de desempleo nacional trimestral"


erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace



*4.10 Creando Tasa de desempleo regional  trimestral

collapse (sum) desa activo [fw=peso], by (year quarter  metro_area)
gen td10 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area)
sort codmerge
drop year quarter  metro_area desa activo 

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td10 "tasa de desempleo regional trimestral"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace


/*
*4.11 Creando la tasa de desempleo trimestral por edades

collapse (sum) desa activo [fw=peso], by (year quarter age_td)
gen td11 = (desa/activo)*100

egen codmerge = group (year quarter age_td)
sort codmerge
drop year quarter age_td desa activo 

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td11 "tasa de desempleo por trimestral edades"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace



*4.12 Creando la tasa de desempleo  trimestral por sexo

collapse (sum) desa activo [fw=peso], by (year quarter male)
gen td12 = (desa/activo)*100

egen codmerge = group (year quarter male)
sort codmerge
drop year quarter male desa activo 

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter male)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td12 "tasa de desempleo trimestral por sexo"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace



*4.13 Creando la tasa de desempleo  trimestral por edades y sexo

collapse (sum) desa activo [fw=peso], by (year quarter male age_td)
gen td13 = (desa/activo)*100

egen codmerge = group (year quarter male age_td)
sort codmerge
drop year quarter male age_td desa activo 

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter male age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td13 "tasa de desempleo trimestral por sexo y edades"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace




*4.14 Creando la tasa de desempleo trimestral regional por edades

collapse (sum) desa activo [fw=peso], by (year quarter  metro_area age_td)
gen td14 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area age_td)
sort codmerge
drop year quarter  metro_area age_td desa activo 

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td14 "tasa de desempleo trimestral por region y edades"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace



*4.15 Creando la tasa de desempleo regional  trimestral por sexo

collapse (sum) desa activo [fw=peso], by (year quarter  metro_area male)
gen td15 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area male)
sort codmerge
drop year quarter  metro_area male desa activo 

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area male)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td15 "tasa de desempleo trimestral por region y sexo"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace


*/

*4.16 Creando la tasa de desempleo regional trimestral por edad y sexo

collapse (sum) desa activo [fw=peso], by (year quarter  metro_area male age_td)
gen td16 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area male age_td)
sort codmerge
drop year quarter  metro_area male desa activo age_td

save "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLANTIGUA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area male age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td16 "tasa de desempleo trimestral por region, edad y sexo"

erase "$dir\\PMEALLANTIGUA_Final-unemployment-temp1.dta"
qui compress
*/

/*Save in data sub-folder*/

save "$dir\\PMEALLANTIGUA_Final-unemployment.dta", replace




*** End of Program ***
