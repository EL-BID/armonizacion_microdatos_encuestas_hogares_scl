*Yessenia Loayza
*Febrero, 2013

clear all
set more off
cd "${surveysFolder}\survey\PER\ENAHO\2003\a\data_orig"

**************************************************************************************
						/*Identificador de hogar*/
**************************************************************************************

 #delimit ;
 
global BASES "enaho01-2003-100.dta
enaho01-2003-200.dta
enaho01a-2003-300.dta
enaho01a-2003-400.dta
enaho01a-2003-500.dta
enaho01-2003-612.dta
enaho01b-2003-3.dta
sumaria-2003.dta";

#delimit ;
foreach base in $BASES{;
	use `base';
	cap drop if p203==0;
	cap drop idh_ch;
	sort conglome vivienda hogar;
	cap egen idh_ch=concat(conglome vivienda hogar);
	sort idh_ch;
	save `base', replace;
	};
	
* Marcela Rubio: Reshape base durable goods	 

cd "${surveysFolder}\survey\PER\ENAHO\2003\a\data_orig"
use enaho01-2003-612.dta, clear

keep idh_ch p612 p612n 

reshape wide p612, i(idh_ch) j(p612n)
sort idh_ch

/*
1	radio
2	tv. blanco y negro
3	tv. a color
4	refrigeradora/congeladora
5	máquina de coser
6	equipo de sonido
7	video grabadora
8	lavadora
9	plancha
10	cocina a gas
11	cocina a kerosene
12	horno microondas
13	licuadora
14	bicicleta
15	auto, camioneta
16	triciclo
17	motocicleta
18	camión
19	mototaxi
20	computadora
21	otro
22	otro
*/

label var p6121 "Su hogar tiene: radio"
label var p6122 "Su hogar tiene: tv. blanco y negro"
label var p6123 "Su hogar tiene: tv. a color"
label var p6124 "Su hogar tiene: refrigeradora/congeladora"
label var p6125 "Su hogar tiene: máquina de coser"
label var p6126 "Su hogar tiene: equipo de sonido"
label var p6127 "Su hogar tiene: video grabadora"
label var p6128 "Su hogar tiene: lavadora"
label var p6129 "Su hogar tiene: plancha"
label var p61210 "Su hogar tiene: cocina a gas"
label var p61211 "Su hogar tiene: cocina a kerosene"
label var p61212 "Su hogar tiene: horno microondas"
label var p61213 "Su hogar tiene: licuadora"
label var p61214 "Su hogar tiene: bicicleta"
label var p61215 "Su hogar tiene: auto, camioneta"
label var p61216 "Su hogar tiene: triciclo"
label var p61217 "Su hogar tiene: motocicleta"
label var p61218 "Su hogar tiene: camión"
label var p61219 "Su hogar tiene: mototaxi"
label var p61220 "Su hogar tiene: computadora"
label var p61221 "Su hogar tiene: otro"
label var p61222 "Su hogar tiene: otro"

saveold enaho01-2003-612_reshape.dta, replace
	
**************************************************************************************
						/*Identificador de persona*/
**************************************************************************************

 #delimit ;
 
 global BASES2 "enaho01-2003-200.dta
 enaho01a-2003-300.dta
enaho01a-2003-400.dta
enaho01a-2003-500.dta
enaho01b-2003-3.dta";

 #delimit ;
foreach base in $BASES2{;
	use `base', clear ;
	cap drop idp_ci;
	sort conglome vivienda hogar codperso;
	cap egen idp_ci=concat(conglome vivienda hogar codperso);
	sort idp_ci;
	save `base', replace;
	};

**************************************************************************************
						/*MERGE*/
**************************************************************************************


clear
use "enaho01-2003-100.dta", clear
keep if result==1 | result==2
sort idh_ch

merge 1:m idh_ch using "enaho01-2003-200.dta"
drop _merge
sort idp_ci


merge 1:1 idp_ci using "enaho01a-2003-300.dta"
drop _merge
sort idp_ci

merge 1:1 idp_ci using "enaho01a-2003-400.dta"
drop _merge
sort idp_ci

merge 1:1 idp_ci using "enaho01a-2003-500.dta"
drop _merge
sort idp_ci

merge 1:1 idp_ci using "enaho01b-2003-3.dta"
drop _merge p32-p45_2
sort idh_ch

merge m:m idh_ch using "sumaria-2003.dta"
drop _merge
sort idh_ch

merge m:1 idh_ch using "enaho01-2003-612_reshape.dta"
drop _merge
sort idh_ch


drop idh* idp*
save "${surveysFolder}\survey\PER\ENAHO\2003\a\data_merge\PER_2003a.dta", replace

 
