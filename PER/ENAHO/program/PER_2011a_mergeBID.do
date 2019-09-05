*ELaboracion: Yessenia Loayza
*Última actualización: Marcela G. Rubio
clear all
set more off
cd "\\Sdssrv03\surveys\survey\PER\ENAHO\2011\a\data_orig\"

**************************************************************************************
						/*Identificador de hogar*/
**************************************************************************************

 #delimit ;
 *modulo de vivienda;
use "enaho01-2011-612.dta", clear;
cap drop id*;
cap egen idh_ch=concat(conglome vivienda hogar);
keep idh_ch p612 p612n;
reshape wide p612, i(idh_ch) j(p612n); 
sort idh_ch;
save "enaho01-2011-612_reshape.dta", replace;

/*
 global BASES "enaho01-2011-100.dta 
 enaho01-2011-200.dta 
enaho01a-2011-300.dta
enaho01A-2011-400.dta
enaho01A-2011-500.dta
enaho01-2011-612.dta
sumaria-2011.dta
enaho01b-2011-2.dta";


 #delimit ;
foreach base in $BASES{;
	use `base';
	cap egen idh_ch=concat(conglome vivienda hogar);
	cap egen idp_ci=concat(conglome vivienda hogar codperso);
	sort idh_ch;
	save `base', replace;
	};
	
	

 
**************************************************************************************
						/*Identificador de persona*/
**************************************************************************************

 #delimit ;
 
 global BASES "enaho01-2011-200.dta 
enaho01a-2011-300.dta
enaho01A-2011-400.dta
enaho01A-2011-500.dta
enaho01b-2011-2.dta";

 #delimit ;
foreach base in $BASES{;
	use `base', clear ;
	cap egen idp_ci=concat(conglome vivienda hogar);
	cap drop idp_c;
	cap drop idp_ci;
	cap egen idp_ci=concat(conglome vivienda hogar codperso);
	save `base', replace;
	};
	*/
	

use "enaho01-2011-200.dta", clear
keep if p217==.

//Educacion
merge 1:1 idp_ci using "enaho01a-2011-300.dta"
// yl: personas de 3 y mas (p212)
tab _merge
drop _merge
sort idh_ch

//Empleo
merge 1:1 idp_ci using "enaho01a-2011-500.dta"
//yl: se unen solo las personas de 14 y mas que fueron entrevistadas
// en el modulo de empleo (p214)
tab _merge
drop _merge
sort idh_ch

//Salud
merge 1:1  idp_ci using "enaho01a-2011-400.dta"
tab _merge
//yl: se unen solo las personas a las que corresponde el modulo de salud (p213)
drop _merge
sort idh_ch


//Vivienda
merge m:1 idh_ch using "enaho01-2011-100.dta"
tab _merge
drop _merge
sort idh_ch

//percepcion del hogar: variable etnia
merge m:1 idh_ch using "enaho01b-2011-2.dta"
tab _merge
drop _merge  p32-p45_2
sort idh_ch

//Agregados de ingresos anualizados
merge idh_ch using "sumaria-2011.dta"
keep if result==1 | result==2
tab _merge
drop _merge
sort idh_ch
/*Nota: se deja a unicamente las encuestas que estan completamente contestadas o incompletas
se eliminan el resto de encuestas */

merge m:m idh_ch using "enaho01-2011-612_reshape.dta"
drop _merge
sort idh_ch

drop idh* idp*
save "M:\survey\PER\ENAHO\2011\a\data_merge\PER_2011a.dta", replace
 

 
