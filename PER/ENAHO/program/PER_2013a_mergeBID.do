
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENAHO
Round: a
Autores: Mayra Sáenz E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
Fecha última modificación:25 de Agosto de 2014

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

clear all
set more off
global ruta ="Z:\survey\PER\ENAHO\2013\a\data_orig"
/*MS: No es necesario generar los identificadores.
local db $ruta\404-Modulo01\enaho01_2013_100 $ruta\404-Modulo02\enaho01_2013_200 $ruta\404-Modulo03\404-Modulo03\Enaho01a-2013-300 $ruta\404-Modulo04\404-Modulo04\Enaho01a-2013-400 $ruta\404-Modulo05\404-Modulo05\enaho01a-2013-500
foreach x of local db {
use "`x'", clear
if "`x'" != "$ruta\404-Modulo01\enaho01_2013_100" {
    sort conglome vivienda hogar codperso
	egen idp_ci=concat(conglome vivienda hogar codperso)
	sort idp_ci
	}

else if "`x'" != "$ruta\404-Modulo01\enaho01_2013_100"{
gen idp_ci=.
}
    sort conglome vivienda hogar
	egen idh_ch=concat(conglome vivienda hogar)
	sort idh_ch
	saveold "`x'_1", replace
}
*/
*Equipamiento del hogar
use "$ruta\404-Modulo18\404-Modulo18\Enaho01_2013_612.dta", clear
keep conglome vivienda hogar p612 p612n
reshape wide p612, i(conglome vivienda hogar) j(p612n)
saveold "$ruta\404-Modulo18\404-Modulo18\Enaho01_2013_612_1.dta", replace


clear
use "$ruta\404-Modulo01\enaho01_2013_100", clear
keep if result==1 | result==2
sort conglome vivienda hogar
merge 1:m conglome vivienda hogar using "$ruta\404-Modulo02\enaho01_2013_200"
drop _merge
sort conglome vivienda hogar codperso

merge 1:1 conglome vivienda hogar codperso using "$ruta\404-Modulo03\404-Modulo03\Enaho01a-2013-300"
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\404-Modulo04\404-Modulo04\Enaho01a-2013-400", force
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\404-Modulo05\404-Modulo05\enaho01a-2013-500", force
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\404-Modulo85\404-Modulo85\Enaho01b-2013-2.dta", force
drop _merge


sort conglome vivienda hogar
merge m:m conglome vivienda hogar  using "$ruta\404-Modulo34\404-Modulo34\SUMARIA-2013.DTA", force
drop _merge

sort conglome vivienda hogar
merge m:m conglome vivienda hogar using "$ruta\404-Modulo18\404-Modulo18\Enaho01_2013_612_1.dta"
drop _merge

capture drop if p203 ==0

saveold "Z:\survey\PER\ENAHO\2013\a\data_merge\PER_2013a.dta", replace
 
