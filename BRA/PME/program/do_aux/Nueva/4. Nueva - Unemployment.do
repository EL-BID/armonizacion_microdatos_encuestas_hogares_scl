/*
** Description: This program takes the inputs from program 3 and appends all the years of the New PME from 2002 to November 2010. 
*** It creates the id identifier, and calculates the unemployment rate.
** Input: Program 3, PME Nueva.
** by David Argente
** Survey: PME (2002-2010)
*/

*** Use the big data ***
clear
set mem 1g, perm
set more off

**Note: this local should be change by the user
local propiodir = "C:\Users\wb377246\Documents\Julian\LM_W"


*** Declare Global Directory

global dir = "`propiodir'\BRA\Data\"



use $dir\PME2002_TOTAL_FINAL.dta, clear


* 1. *** Appending Nueva PME from 2002 to 2010
	
foreach x of numlist 2003 2004 2005 2006 2007 2008 2009 2010 {

qui append using $dir\PME`x'_TOTAL_FINAL.dta

}


* 2. *** Creates the individual identifier

		
		/* Generate the individual id. 
		We used 2 kind of variables: Household and Individual. We follow the methodology proposed by
		Perez and Suarez (2008) from "Sobre i painel da pesquisa mensal de emprego do IGBE". We include the Metropolitan Area, Control Number
		Serie Number, Panel, Rotational Group as a Household Variables. The day of birth, month of birth, year of birth and gender were used as individual variables.
		*/

		qui egen id = group (metro control serie_n panel rgroup dob mob yob male)
		drop if id == .

		
		** Generate the control variable to see how many times the id is present in the database
		qui bys id: egen control_id = count(id)	
		** drop if the id repeated more than 8 times (0.3 percent of the sample)
		drop if control_id > 8		
		
		** drop unnecesary variables
		drop  control_number serie_number panel rgroup dob mob yob

		** Order database
	
		order  id metro_area month year direct_survey male race age_calculed attending yeduc work_status toccu temporal cartera private wage_a  hours_a  tenurem ssecurity fsize ciiu industry occupation  peso

		** keeps variables of interest, these are the same variables as the files for the Antigua PME
		
		keep  id metro_area month year male age_calculed yeduc work_status toccu cartera wage_a ciiu peso 



* 3. *** Generates the variable age_td and the variable quarter. Both will be used to calculate different definitions of the unemployment rate ***

gen age_td = .
replace age_td = 1 if  age_calculed <25
replace age_td = 2 if  age_calculed>=25 &  age_calculed<40
replace age_td = 3 if  age_calculed>=40 &  age_calculed<55
replace age_td = 4 if  age_calculed>=55 

label var age_td "=1 [0,25) =2 [25,40) =3 [40,55) =4 [55,max]"


gen quarter=.
replace quarter=1 if month ==1 | month ==2 | month ==3
replace quarter=2 if month ==4 | month ==5 | month ==6
replace quarter=3 if month ==7 | month ==8 | month ==9
replace quarter=4 if month ==10| month==11 | month ==12



* 4. *** Generates the unemployment rate for individuals older than 14 years of age. The first 8 definitions are monthly unemployment rates, the next 8 are by quarter.


capture drop activo
capture drop desa
tab work_status

/*
status in |
  the labor |
      force |      Freq.     Percent        Cum.
------------+-----------------------------------
   occupied |  4,166,956       50.40       50.40
 unemployed |    464,310        5.62       56.02
   inactive |  3,635,796       43.98      100.00
------------+-----------------------------------
      Total |  8,267,062      100.00

*/


gen activo = .
replace activo = 1 if work_status == 1 | work_status == 2
gen desa = .
replace desa = 1 if work_status == 2

drop if age_calculed<14

save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace




*4.9 Tasa de desempleo nacional trimestral

collapse (sum) desa activo [aw=peso], by (year quarter)
gen td9 = (desa/activo)*100

egen codmerge = group (year quarter)
sort codmerge
drop desa activo year quarter

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td9 "tasa de desempleo nacional trimestral"


erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace




*4.10 Creando Tasa de desempleo regional  trimestral

collapse (sum) desa activo [aw=peso], by (year quarter  metro_area)
gen td10 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area)
sort codmerge
drop year quarter  metro_area desa activo 

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td10 "tasa de desempleo regional trimestral"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace




*4.11 Creando la tasa de desempleo trimestral por edades

collapse (sum) desa activo [aw=peso], by (year quarter age_td)
gen td11 = (desa/activo)*100

egen codmerge = group (year quarter age_td)
sort codmerge
drop year quarter age_td desa activo 

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td11 "tasa de desempleo por trimestral edades"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace




*4.12 Creando la tasa de desempleo  trimestral por sexo

collapse (sum) desa activo [aw=peso], by (year quarter male)
gen td12 = (desa/activo)*100

egen codmerge = group (year quarter male)
sort codmerge
drop year quarter male desa activo 

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter male)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td12 "tasa de desempleo trimestral por sexo"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace




*4.13 Creando la tasa de desempleo  trimestral por edades y sexo

collapse (sum) desa activo [aw=peso], by (year quarter male age_td)
gen td13 = (desa/activo)*100

egen codmerge = group (year quarter male age_td)
sort codmerge
drop year quarter male age_td desa activo 

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter male age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td13 "tasa de desempleo trimestral por sexo y edades"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace





*4.14 Creando la tasa de desempleo trimestral regional por edades

collapse (sum) desa activo [aw=peso], by (year quarter  metro_area age_td)
gen td14 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area age_td)
sort codmerge
drop year quarter  metro_area age_td desa activo 

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td14 "tasa de desempleo trimestral por region y edades"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace




*4.15 Creando la tasa de desempleo regional  trimestral por sexo

collapse (sum) desa activo [aw=peso], by (year quarter  metro_area male)
gen td15 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area male)
sort codmerge
drop year quarter  metro_area male desa activo 

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area male)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td15 "tasa de desempleo trimestral por region y sexo"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace





*4.16 Creando la tasa de desempleo regional trimestral por edad y sexo

collapse (sum) desa activo [aw=peso], by (year quarter  metro_area male age_td)
gen td16 = (desa/activo)*100

egen codmerge = group (year quarter  metro_area male age_td)
sort codmerge
drop year quarter  metro_area male desa activo age_td

save "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta", replace

clear
use "$dir\\PMEALLNUEVA_Final-unemployment.dta"
egen codmerge = group(year quarter  metro_area male age_td)
sort codmerge

merge codmerge using "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
tab _merge
drop codmerge _merge
label var td16 "tasa de desempleo trimestral por region, edad y sexo"

erase "$dir\\PMEALLNUEVA_Final-unemployment-temp1.dta"
qui compress

* Drop posible duplicates in id time and year (less than 1% on sample)
duplicates drop id year month, force

/*Save in unemployment sub-folder*/
save "$dir\\PMEALLNUEVA_Final-unemployment.dta", replace


*** - End subprogram - ***
