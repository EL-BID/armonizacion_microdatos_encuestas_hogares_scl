*Yessenia Loayza
*octubre, 2013

clear all
set more off
cd "Y:\survey\PER\ENAHO\2012\a\data_orig\"

**************************************************************************************
						/*Identificador de hogar*/
**************************************************************************************

 #delimit ;
 *modulo de vivienda;
use "enaho01-2012-612.dta", clear;
cap drop id*;
cap egen idh_ch=concat(conglome vivienda hogar);
keep idh_ch p612 p612n;
reshape wide p612, i(idh_ch) j(p612n); 
sort idh_ch;
save "enaho01-2012-612_reshape.dta", replace;

global BASES "enaho01-2012-100
enaho01-2012-200
enaho01a-2012-300
enaho01a-2012-400
enaho01a-2012-500
enaho01-2012-612
sumaria-2012
enaho01b-2012-2";

#delimit ;
foreach base in $BASES{;
	use `base'.dta;
	cap drop if p203==0; /* tab p203 p217*/
	cap drop idh_ch;
	sort conglome vivienda hogar;
	egen idh_ch=concat(conglome vivienda hogar);
	sort idh_ch;
	saveold "`base'_modific.dta", replace;
	};
	
**************************************************************************************
						/*Identificador de persona*/
**************************************************************************************

 #delimit ;
 
 global BASES2 "enaho01-2012-200
enaho01a-2012-300
enaho01a-2012-400
enaho01a-2012-500
enaho01b-2012-2";

 #delimit ;
foreach base in $BASES2{;
	use "`base'_modific.dta", clear ;
	cap drop idp_ci;
	sort conglome vivienda hogar codperso;
	egen idp_ci=concat(conglome vivienda hogar codperso);
	sort idp_ci;
	save "`base'_modific.dta", replace;
	};

**************************************************************************************
						/*MERGE*/
**************************************************************************************


clear
use "enaho01-2012-100_modific.dta", clear
keep if result==1 | result==2
sort idh_ch

merge 1:m idh_ch using "enaho01-2012-200_modific.dta"
drop _merge
sort idp_ci

merge 1:1 idp_ci using "enaho01a-2012-400_modific.dta"
drop _merge
sort idp_ci

merge 1:1 idp_ci using "enaho01a-2012-300_modific.dta"
drop _merge
sort idh_ch

merge 1:1 idp_ci using "enaho01a-2012-500_modific.dta"
drop _merge
sort idh_ch

merge m:m idh_ch using "enaho01b-2012-2_modific.dta"
drop _merge p32-p45_2
sort idh_ch


merge m:m idh_ch using "sumaria-2012_modific.dta"
drop _merge
sort idh_ch


merge m:m idh_ch using "enaho01-2012-612_reshape.dta"
drop _merge
sort idh_ch

drop idh* idp*
saveold "Y:\survey\PER\ENAHO\2012\a\data_merge\PER_2012a.dta", replace
 
