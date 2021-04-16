
** Description: This program prepares the variable weight to merge it with the databases PME 1982-1990
** Input: Database "factor" created by Edwin
** Output: Database ready to be merged with the rest of the data.
** by Julian Messina and David Argente
** November 2010

clear
set more off
set mem 200m

** Declare Global Directory **

local propiodir = "${surveysFolder}\Users\wb377246\Documents\Julian\LM_W"

cd "`propiodir'\BRA\Data\pme\"


use factor.dta, clear 

gen month1 = substr(ano,1,3)
gen y = substr(ano,-2,.)
destring y, g(anos)
gen yy = "19"
gen yym = "20"

gen year = yym + y if anos<=02
replace year = yy + y if anos>02

gen month = .
replace month = 1 if month1 == "Jan"
replace month = 2 if month1 == "Feb"
replace month = 3 if month1 == "Mar"
replace month = 4 if month1 == "Apr"
replace month = 5 if month1 == "May"
replace month = 6 if month1 == "Jun"
replace month = 7 if month1 == "Jul"
replace month = 8 if month1 == "Aug"
replace month = 9 if month1 == "Sep"
replace month = 10 if month1 == "Oct"
replace month = 11 if month1 == "Nov"
replace month = 12 if month1 == "Dec"

drop yy yym y ano anos month1

destring year, replace

/* We compute the expansion factor for 1982 using the same values of january 1983 under the assumption that
the demographics between regions didn't change substantialy in those years
*/

foreach var in  rio_jan sao_pau por_ale bel_hor recife salvador total {
gen `var'2=`var' if year==1983 & month==1
egen `var'3=max(`var'2)
replace `var'=`var'3 if year==1982
drop `var'2 `var'3  
}    

save factor1.dta, replace

use factor1.dta, clear
collapse (mean) salvador, by (month year)
gen dummy = 1
save factor1_salvador_temp.dta, replace

use factor1.dta, clear
collapse (mean) rio_jan, by (month year)
gen dummy = 2
save factor1_rio_jan_temp.dta, replace

use factor1.dta, clear
collapse (mean) sao_pau, by (month year)
gen dummy = 3
save factor1_sao_pau_temp.dta, replace

use factor1.dta, clear
collapse (mean) por_ale, by (month year)
gen dummy = 4
save factor1_por_ale_temp.dta, replace

use factor1.dta, clear
collapse (mean) bel_hor, by (month year)
gen dummy = 5
save factor1_bel_hor_temp.dta, replace

use factor1.dta, clear
collapse (mean) recife, by (month year)
gen dummy = 6
save factor1_recife_temp.dta, replace

use factor1_salvador_temp.dta, clear
append using factor1_rio_jan_temp.dta
append using factor1_sao_pau_temp.dta
append using factor1_por_ale_temp.dta
append using factor1_bel_hor_temp.dta
append using factor1_recife_temp.dta


gen peso =.
replace peso = salvador if dummy==1
replace peso = rio_jan if dummy==2
replace peso = sao_pau	if dummy==3
replace peso = por_ale if dummy==4
replace peso = bel_hor if dummy==5
replace peso = recife if dummy==6

gen metro_area = .
replace metro_area = 26 if dummy == 6
replace metro_area = 29 if dummy == 1
replace metro_area = 31 if dummy == 5
replace metro_area = 33 if dummy == 2
replace metro_area = 35 if dummy == 3
replace metro_area = 43 if dummy == 4

label define metro_area_l 26"recife" 29"salvador" 31"belo horizonte" 33"rio de janeiro" 35"sao paulo" 43"porto alegre"
label values metro_area metro_area_l

drop bel_hor sao_pau recife salvador por_ale rio_jan dummy

erase factor1_rio_jan_temp.dta
erase factor1_sao_pau_temp.dta
erase factor1_por_ale_temp.dta
erase factor1_bel_hor_temp.dta
erase factor1_recife_temp.dta
erase factor1_salvador_temp.dta
erase factor1.dta

sort year month metro_area


save factor2.dta, replace









