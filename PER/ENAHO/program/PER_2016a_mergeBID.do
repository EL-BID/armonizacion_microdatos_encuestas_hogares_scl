
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pais: Perú
Encuesta: ENAHO
Round: a
Autores: Marcela G. Rubio E-mail: marcelarubio28@gmail.com - mrubio@iadb.org
Fecha última modificación: Stephanie González - stephaniego@iadb.org  

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
clear all
set more off
global ruta = "${surveysFolder}\\survey\PER\ENAHO\2017\a\data_orig\"
global out ="${surveysFolder}\survey\PER\ENAHO\2017\a\data_merge\"

*Equipamiento del hogar
use "$ruta\546-Modulo18\enaho01-2016-612.dta" , clear
keep conglome vivienda hogar p612 p612n
reshape wide p612, i(conglome vivienda hogar) j(p612n)

/*
1	radio
2	tv. a color
3	tv. blanco y negro
4	equipo de sonido
5	dvd
6	video grabadora
7	computadora
8	plancha
9	licuadora
10	cocina a gas
11	cocina a kerosene
12	refrigeradora/congeladora
13	lavadora
14	horno microondas
15	m�quina de coser
16	bicicleta
17	auto, camioneta
18	motocicleta
19	triciclo
20	mototaxi
21	cami�n
22	otro
23	otro
24	otro
25	otro
26	otro
*/

label var p6121 "Su hogar tiene: radio"
label var p6122 "Su hogar tiene: tv. a color"
label var p6123 "Su hogar tiene: tv. blanco y negro"
label var p6124 "Su hogar tiene: equipo de sonido"
label var p6125 "Su hogar tiene: dvd"
label var p6126 "Su hogar tiene: video grabadora"
label var p6127 "Su hogar tiene: computadora"
label var p6128 "Su hogar tiene: plancha"
label var p6129 "Su hogar tiene: licuadora"
label var p61210 "Su hogar tiene: cocina a gas"
label var p61211 "Su hogar tiene: cocina a kerosene"
label var p61212 "Su hogar tiene: refrigeradora/congeladora"
label var p61213 "Su hogar tiene: lavadora"
label var p61214 "Su hogar tiene: horno microondas"
label var p61215 "Su hogar tiene: m�quina de coser"
label var p61216 "Su hogar tiene: bicicleta"
label var p61217 "Su hogar tiene: auto, camioneta"
label var p61218 "Su hogar tiene: motocicleta"
label var p61219 "Su hogar tiene: triciclo"
label var p61220 "Su hogar tiene: mototaxi"
label var p61221 "Su hogar tiene: cami�n"
label var p61222 "Su hogar tiene: otro"
label var p61223 "Su hogar tiene: otro"
label var p61224 "Su hogar tiene: otro"
label var p61225 "Su hogar tiene: otro"
label var p61226 "Su hogar tiene: otro"

saveold "$ruta\546-Modulo18\enaho01-2016-612_1.dta", replace

* Sort de bases
use "$ruta\546-Modulo02\enaho01-2016-200.dta", clear
duplicates report conglome vivienda hogar codperso
sort conglome vivienda hogar codperso
saveold "$ruta\546-Modulo02\enaho01-2016-200.dta", replace


use "$ruta\546-Modulo03\enaho01a-2016-300.dta", clear
duplicates report conglome vivienda hogar codperso
sort conglome vivienda hogar codperso
saveold "$ruta\546-Modulo03\enaho01a-2016-300.dta", replace


use "$ruta\546-Modulo04\enaho01a-2016-400.dta", clear
duplicates report conglome vivienda hogar codperso
sort conglome vivienda hogar codperso
saveold "$ruta\546-Modulo04\enaho01a-2016-400.dta", replace


use "$ruta\546-Modulo05\enaho01a-2016-500.dta", clear
duplicates report conglome vivienda hogar codperso
sort conglome vivienda hogar codperso
saveold "$ruta\546-Modulo05\enaho01a-2016-500.dta", replace


use "$ruta\546-Modulo85\enaho01b-2016-2.dta", clear
duplicates report conglome vivienda hogar codperso
sort conglome vivienda hogar codperso
saveold "$ruta\546-Modulo85\enaho01b-2016-2.dta", replace


use "$ruta\546-Modulo34\sumaria-2016.dta", clear
duplicates report conglome vivienda hogar
sort conglome vivienda hogar 
saveold "$ruta\546-Modulo34\sumaria-2016.dta", replace


use "$ruta\546-Modulo18\enaho01-2016-612_1.dta", clear
duplicates report conglome vivienda hogar
sort conglome vivienda hogar
saveold "$ruta\546-Modulo18\enaho01-2016-612_1.dta", replace

* Merge de bases de datos
clear
use "$ruta\546-Modulo01\enaho01-2016-100.dta", clear
keep if result==1 | result==2
sort conglome vivienda hogar
merge 1:m conglome vivienda hogar using "$ruta\546-Modulo04\enaho01a-2016-400.dta", force
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\546-Modulo02\enaho01-2016-200.dta"
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\546-Modulo03\enaho01a-2016-300.dta", force
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\546-Modulo05\enaho01a-2016-500.dta", force
drop _merge

sort conglome vivienda hogar codperso
merge 1:1 conglome vivienda hogar codperso using "$ruta\546-Modulo85\enaho01b-2016-2.dta", force
drop _merge

sort conglome vivienda hogar
merge m:1 conglome vivienda hogar  using "$ruta\546-Modulo34\sumaria-2016.dta", force
drop _merge

sort conglome vivienda hogar
merge m:1 conglome vivienda hogar using "$ruta\546-Modulo18\enaho01-2016-612_1.dta"
drop _merge

capture drop if p203 ==0

saveold "$out\PER_2016a.dta", replace
 
