********************************************************
/* 		Merge por trimestre y anho 
			ENE 2000II - 2004IV
		Elaborado por Karina Acevedo			 */
*******************************************************

clear all
set more off

global in "${surveysFolder}\BID_Karina Acevedo\Mexico ENE - ENOE\ENE" 
log using "$in\Do\merge_ene", replace

**************************
***periodo 2001 - 2004****
**************************

foreach z in 01 02 03 04 {
foreach x in 1 2 3 4 {
cd "$in\20`z'\\`x'\Data_orig"
	use "ene`x'`z'.dta", clear
		sort a_met ent con v_sel hog h_mud 
		save, replace
	use "hog-`x'`z'.dta", clear
		sort a_met ent con v_sel hog h_mud
		save, replace
	
* Merge por trimestre
	
	use "ene`x'`z'.dta", clear
	merge a_met ent con v_sel hog h_mud using "hog-`x'`z'.dta"  
	tab _merge
	drop _merge
	save "$in\20`z'\\`x'\Data_modificada\ene`x'`z'.dta", replace
}
}	

*************************
***Anho 2000*************
*************************
foreach z in 00 {
foreach x in 2 3 4 {
cd "$in\20`z'\\`x'\Data_orig"
	use "ene`x'`z'.dta", clear
		sort a_met ent con v_sel hog h_mud 
		save, replace
	use "hog-`x'`z'.dta", clear
		sort a_met ent con v_sel hog h_mud
		save, replace
	
* Merge por trimestre
	
	use "ene`x'`z'.dta", clear
	merge a_met ent con v_sel hog h_mud using "hog-`x'`z'.dta"  
	tab _merge
	drop _merge
	save "$in\20`z'\\`x'\Data_modificada\ene`x'`z'.dta", replace
}
}	

**************************************
*****Anhos 91, 95, 96, 97, 98, 99*****
**************************************

foreach z in 95 96 97 98 99 {
cd "$in\19`z'\2\Data_orig"
	use "ene2`z'.dta", clear
		sort a_met ent con v_sel hog h_mud 
		save, replace
	use "hog-2`z'.dta", clear
		sort a_met ent con v_sel hog h_mud
		save, replace
	
* Merge por trimestre
	
	use "ene2`z'.dta", clear
	merge a_met ent con v_sel hog h_mud using "hog-2`z'.dta"  
	tab _merge
	drop _merge
	save "$in\19`z'\2\Data_modificada\ene2`z'.dta", replace
}
log close
