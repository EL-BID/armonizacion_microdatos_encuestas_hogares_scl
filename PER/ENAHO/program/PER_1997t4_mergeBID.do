*Yessenia Loayza
*Mayo, 2013

clear all
set more off
cd "Y:\survey\PER\ENAHO\1997\t4\data_orig"

**************************************************************************************
						/*Identificador de hogar*/
**************************************************************************************
/*
 #delimit ;
 
global BASES "ena97401.dta 
ena97402.dta 
ena974-300.dta
ena974-400.dta
ena974-500.dta
sumaria-1997-iv";

#delimit ;
foreach base in $BASES{;
	use `base';
	sort con viv hog;
	save `base', replace;
	};

**************************************************************************************
						/*Identificador de persona*/
**************************************************************************************
clear
 #delimit ;
 
 global BASES2 "ena97402.dta
 ena974-300.dta
 ena974-400.dta
 ena974-500.dta
 ";

 #delimit ;
foreach base in $BASES2{;
	use `base', clear ;
	cap gen cod_per=codperso;
	cap gen cod_per=p300n;
	cap gen cod_per=p400n;
	cap gen cod_per=codpers2;
	sort con viv hog cod_per;
	tostring cod_per, replace;
	save `base', replace;
	};

**************************************************************************************
						/*MERGE*/
**************************************************************************************

*/
clear
use "ena97401.dta", clear
keep if resencue==1 | resencue==2
joinby con viv hog using "ena97402.dta", unmatched(both)
tab _merge
drop _merge

joinby con viv hog cod_per using "ena974-300.dta", unmatched(both)
tab _merge
drop _merge

joinby con viv hog cod_per using "ena974-400.dta", unmatched(both)
tab _merge
drop _merge

joinby con viv hog cod_per using "ena974-500.dta", unmatched(both)
tab _merge
drop _merge

joinby con viv hog using "sumaria-1997-iv.dta", unmatched(both)
tab _merge
drop _merge

save "Y:\survey\PER\ENAHO\1997\t4\data_merge\PER_1997t4.dta", replace

