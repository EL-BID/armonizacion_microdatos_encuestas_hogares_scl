*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre 29, 2013

*** MERGE COLOMBIA ECH 2003 ****
*------------------------------*

clear
set more off
local anio = 2003
local anioab="03" 
local ruta ="${surveysFolder}\survey\COL\ECH\\`anio'\"
local m7   ="`ruta'm7\data_orig\"
local m8   ="`ruta'm8\data_orig\"
local m9   ="`ruta'm9\data_orig\"
local t3   ="`ruta't3\data_orig\"
local out  ="`ruta't3\data_merge\"

*1 - Conversion Bases de datos
*------------------------------

foreach zona in C R {

if "`zona'"=="C" {
local x "ca_"
}
if "`zona'"=="R" {
local x "re_"
}

*** VIVIENDA
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T01`zona'.dct", clear
keep if `x'TIPOD=="01"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 
saveold `m`i''col`zona'`anioab'vivi.dta,  replace
}

use "`m7'col`zona'`anioab'vivi.dta", clear
append using "`m8'col`zona'`anioab'vivi.dta"
append using "`m9'col`zona'`anioab'vivi.dta"
sort  `x'IDENT mes_c1 
saveold `t3'col`zona'`anioab'vivi.dta, replace


*** EDUCACION
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T10`zona'.dct", clear
keep if `x'TIPOD=="10"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 
saveold `m`i''col`zona'`anioab'edu.dta,  replace
}
use "`m7'col`zona'`anioab'edu.dta", clear
append using "`m8'col`zona'`anioab'edu.dta"
append using "`m9'col`zona'`anioab'edu.dta"
sort  `x'IDENT mes_c1 
saveold "`t3'col`zona'`anioab'edu.dta", replace

*** FUERZA DE TRABAJO
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T50`zona'.dct", clear
keep if `x'TIPOD=="50"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 `x'1_T50_ 
saveold `m`i''col`zona'`anioab'ft.dta,  replace
}
use "`m7'col`zona'`anioab'ft.dta", clear
append using "`m8'col`zona'`anioab'ft.dta"
append using "`m9'col`zona'`anioab'ft.dta"
sort  `x'IDENT mes_c1 `x'1_T50_
saveold "`t3'col`zona'`anioab'ft.dta", replace

*** OCUPADOS EMPLEO PRINCIPAL
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T60`zona'.dct", clear
keep if `x'TIPOD=="60"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 `x'1_T60_
saveold `m`i''col`zona'`anioab'oep.dta,  replace
}

use "`m7'col`zona'`anioab'oep.dta", clear
append using "`m8'col`zona'`anioab'oep.dta"
append using "`m9'col`zona'`anioab'oep.dta"
sort  `x'IDENT mes_c1 `x'1_T60_
saveold "`t3'col`zona'`anioab'oep.dta", replace

*** OCUPADOS SUBEMPLEO
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T61`zona'.dct", clear
keep if `x'TIPOD=="61"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 `x'1_T61_
saveold `m`i''col`zona'`anioab'os.dta,  replace
}
use "`m7'col`zona'`anioab'os.dta", clear
append using "`m8'col`zona'`anioab'os.dta"
append using "`m9'col`zona'`anioab'os.dta"
sort  `x'IDENT mes_c1 `x'1_T61_
saveold "`t3'col`zona'`anioab'os.dta", replace

*** OCUPADOS EMPLEO SECUNDARIOS
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T62`zona'.dct", clear
keep if `x'TIPOD=="62"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 `x'1_T62_
saveold `m`i''col`zona'`anioab'oes.dta,  replace
}
use "`m7'col`zona'`anioab'oes.dta", clear
append using "`m8'col`zona'`anioab'oes.dta"
append using "`m9'col`zona'`anioab'oes.dta"
sort  `x'IDENT mes_c1 `x'1_T62_
saveold "`t3'col`zona'`anioab'oes.dta", replace

*** DESOCUPADOS 
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T70`zona'.dct", clear
keep if `x'TIPOD=="70"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 `x'1_T70_ 
saveold `m`i''col`zona'`anioab'des.dta,  replace
}
use "`m7'col`zona'`anioab'des.dta", clear
append using "`m8'col`zona'`anioab'des.dta"
append using "`m9'col`zona'`anioab'des.dta"
sort  `x'IDENT mes_c1 `x'1_T70_ 
saveold "`t3'col`zona'`anioab'des.dta", replace

*** INACTIVOS
forvalues i=7/9 {
infile using "`m`i''col`anioab'_T80`zona'.dct", clear
keep if `x'TIPOD=="80"
gen mes_c1=`i'
sort  `x'IDENT mes_c1 `x'1_T80_
saveold `m`i''col`zona'`anioab'ina.dta,  replace
}
use "`m7'col`zona'`anioab'ina.dta", clear
append using "`m8'col`zona'`anioab'ina.dta"
append using "`m9'col`zona'`anioab'ina.dta"
sort  `x'IDENT mes_c1 `x'1_T80_
saveold "`t3'col`zona'`anioab'ina.dta", replace


*2 - Merge de los diferentes modulos
*-----------------------------------
use `t3'col`zona'`anioab'vivi.dta, clear
merge `x'IDENT mes_c1 using `t3'col`zona'`anioab'edu.dta,  
tab _merge
drop _merge
rename `x'1_T10_ `x'1_T50_
sort  `x'IDENT mes_c1 `x'1_T50_

merge `x'IDENT mes_c1 `x'1_T50_ using `t3'col`zona'`anioab'ft.dta
tab _merge
drop _merge
rename `x'1_T50_ `x'1_T60_
sort  `x'IDENT mes_c1 `x'1_T60_

merge `x'IDENT mes_c1 `x'1_T60_ using `t3'col`zona'`anioab'oep.dta
tab _merge
drop _merge
rename `x'1_T60_ `x'1_T61_
sort  `x'IDENT mes_c1 `x'1_T61_

merge `x'IDENT mes_c1 `x'1_T61_ using `t3'col`zona'`anioab'os.dta
tab _merge
drop _merge
rename `x'1_T61_ `x'1_T62_
sort  `x'IDENT mes_c1 `x'1_T62_

merge `x'IDENT mes_c1 `x'1_T62_ using `t3'col`zona'`anioab'oes.dta
tab _merge
drop _merge
rename `x'1_T62_ `x'1_T70_
sort  `x'IDENT mes_c1 `x'1_T70_

merge `x'IDENT mes_c1 `x'1_T70_ using `t3'col`zona'`anioab'des.dta
tab _merge
drop _merge
rename `x'1_T70_ `x'1_T80_
sort  `x'IDENT mes_c1 `x'1_T80_

merge `x'IDENT mes_c1 `x'1_T80_ using `t3'col`zona'`anioab'ina.dta
tab _merge
drop _merge
gen factor=real(`x'FACTEXP)
drop `x'FACTEXP
ren factor `x'FACTEXP

rename	`x'TIPOD	pTIPOD
rename	`x'IDENT	pIDENT
rename	`x'1i_T01	p1i_T01
rename	`x'2i_T01	p2i_T01
rename	`x'3i_T01	p3i_T01
rename	`x'4i_T011	p4i_T011
rename	`x'4i_T012	p4i_T012
rename	`x'4i_T013	p4i_T013
rename	`x'4i_T014	p4i_T014
rename	`x'4i_T015	p4i_T015
rename	`x'1_T01_	p1_T01_
rename	`x'2_T01_	p2_T01_
rename	`x'3_T01_	p3_T01_
rename	`x'4_T01_	p4_T01_
rename	`x'5_T01_	p5_T01_
rename	`x'6_T01_	p6_T01_
rename	`x'7_T01_	p7_T01_
rename	`x'8_T01_1	p8_T01_1
rename	`x'8_T01_2	p8_T01_2
rename	`x'8_T01_3	p8_T01_3
rename	`x'8_T01_4	p8_T01_4
rename	`x'8_T01_5	p8_T01_5
rename	`x'8_T01_6	p8_T01_6
rename	`x'8_T01_7	p8_T01_7
rename	`x'1_T80_	p1_T80_
rename	`x'3_T10_	p3_T10_
rename	`x'4_T10_	p4_T10_
rename	`x'5_T10_	p5_T10_
rename	`x'6_T10_	p6_T10_
rename	`x'7_T10_	p7_T10_
rename	`x'8_T10_	p8_T10_
rename	`x'9_T10_	p9_T10_
rename	`x'10_T10	p10_T10
rename	`x'11_T50	p11_T50
rename	`x'12_T50	p12_T50
rename	`x'13_T50	p13_T50
rename	`x'14_T50	p14_T50
rename	`x'15_T501	p15_T501
rename	`x'15_T502	p15_T502
rename	`x'15_T503	p15_T503
rename	`x'15_T504	p15_T504
rename	`x'16_T50	p16_T50
rename	`x'17_T50	p17_T50
rename	`x'18_T50	p18_T50
rename	`x'19_T50	p19_T50
rename	`x'20_T50	p20_T50
rename	`x'21_T50	p21_T50
rename	`x'22_T50	p22_T50
rename	`x'23_T50	p23_T50
rename	`x'24_T60	p24_T60
rename	`x'26_T60	p26_T60
rename	`x'27_T60	p27_T60
rename	`x'28_T60	p28_T60
rename	`x'29_T601	p29_T601
rename	`x'29_T602	p29_T602
rename	`x'30_T601	p30_T601
rename	`x'30_T602	p30_T602
rename	`x'30_T603	p30_T603
rename	`x'30_T604	p30_T604
rename	`x'30_T605	p30_T605
rename	`x'30_T606	p30_T606
rename	`x'31_T60	p31_T60
rename	`x'32_T601	p32_T601
rename	`x'32_T602	p32_T602
rename	`x'33_T601	p33_T601
rename	`x'33_T602	p33_T602
rename	`x'33_T603	p33_T603
rename	`x'34_T61	p34_T61
rename	`x'35_T611	p35_T611
rename	`x'35_T612	p35_T612
rename	`x'36_T611	p36_T611
rename	`x'36_T612	p36_T612
rename	`x'37_T62	p37_T62
rename	`x'38_T62	p38_T62
rename	`x'39_T621	p39_T621
rename	`x'39_T622	p39_T622
rename	`x'39_T623	p39_T623
rename	`x'39_T624	p39_T624
rename	`x'40_T62	p40_T62
rename	`x'41_T62	p41_T62
rename	`x'42_T62	p42_T62
rename	`x'43_T62	p43_T62
rename	`x'44_T62	p44_T62
rename	`x'45_T62	p45_T62
rename	`x'46_T621	p46_T621
rename	`x'46_T622	p46_T622
rename	`x'46_T623	p46_T623
rename	`x'46_T624	p46_T624
rename	`x'46_T625	p46_T625
rename	`x'46_T626	p46_T626
rename	`x'46_T627	p46_T627
rename	`x'46_T628	p46_T628
rename	`x'46_T629	p46_T629
rename	`x'47_T62	p47_T62
rename	`x'48_T62	p48_T62
rename	`x'49_T70	p49_T70
rename	`x'50_T70	p50_T70
rename	`x'51_T70	p51_T70
rename	`x'52_T70	p52_T70
rename	`x'53_T70	p53_T70
rename	`x'54_T70	p54_T70
rename	`x'55_T70	p55_T70
rename	`x'56_T70	p56_T70
rename	`x'57_T70	p57_T70
rename	`x'58_T701	p58_T701
rename	`x'58_T702	p58_T702
rename	`x'58_T703	p58_T703
rename	`x'59_T701	p59_T701
rename	`x'59_T702	p59_T702
rename	`x'59_T703	p59_T703
rename	`x'60_T80	p60_T80
rename	`x'61_T80	p61_T80
rename	`x'62_T80	p62_T80
rename	`x'63_T80	p63_T80
rename	`x'64_T80	p64_T80
rename	`x'65_T801	p65_T801
rename	`x'65_T802	p65_T802
rename	`x'65_T803	p65_T803
rename	`x'66_T801	p66_T801
rename	`x'66_T802	p66_T802
rename	`x'66_T803	p66_T803
rename	`x'FACTEXP	FACTEXP
saveold "`out'COL_`anio't3`x'.dta", replace
}

clear
use "`out'COL_`anio't3ca_.dta", clear
gen area=1
rename p31_T60 p31_T60cab
rename p27_T60 p27_T60cab
append using "`out'COL_`anio't3re_.dta"
replace area= 0 if area!=1
rename p31_T60 p31_T60res
rename p27_T60 p27_T60res
replace FACTEXP=FACTEXP/3
saveold "`out'COL_`anio't3.dta", replace 







