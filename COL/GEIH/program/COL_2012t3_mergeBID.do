*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Enero, 2014

*** MERGE COLOMBIA GEIH 2012 ****
*------------------------------*	

clear
set more off
local anio = 2012
local anioab ="12" 	
local ruta "\\sdssrv03\Surveys\survey\COL\GEIH\\`anio'\"
local m7 ="`ruta'm7\data_orig\" 
local m8 ="`ruta'm8\data_orig\" 
local m9 ="`ruta'm9\data_orig\" 
local t3   ="`ruta't3\data_orig\"
local out  ="`ruta't3\data_merge\"


*1. Bases anuales con homologacion de ingresos
/*----------------------------------------------
clear
use "\\sdssrv03\Surveys\survey\COL\GEIH\2012\a\data_orig\personas 2012.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
merge idh using "\\sdssrv03\Surveys\survey\COL\GEIH\2012\a\data_orig\hogares 2012.dta"
tab _merge
drop _merge
egen id =concat (directorio secuencia_p orden)
sort id
destring mes, replace
keep if mes>=7 & mes<=9
keep  id impa- fex_c nper- fex_dpto_c
save "Y:\survey\COL\GEIH\2012\a\data_merge\pov.dta", replace

*2. Append entre meses
*------------------------

foreach zona in cabecera resto {

*Personas
use "`m7'\`zona' - características generales (personas).dta", clear
append using "`m8'\`zona' - características generales (personas).dta"
append using "`m9'\`zona' - características generales (personas).dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_personas.dta", replace

*Desocupados
use "`m7'\`zona' - desocupados.dta", clear
append using "`m8'\`zona' - desocupados.dta"
append using "`m9'\`zona' - desocupados.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_desocupados.dta", replace
				
*Fuerza Trabajo
use "`m7'\`zona' - fuerza de trabajo.dta", clear
append using "`m8'\`zona' - fuerza de trabajo.dta"
append using "`m9'\`zona' - fuerza de trabajo.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_ft.dta", replace

*Inactivos
use "`m7'\`zona' - inactivos.dta", clear
append using "`m8'\`zona' - inactivos.dta"
append using "`m9'\`zona' - inactivos.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_inactivos.dta", replace

*ocupados
use "`m7'\`zona' - ocupados.dta", clear
append using "`m8'\`zona' - ocupados.dta"
append using "`m9'\`zona' - ocupados.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_ocupados.dta", replace

*otrasactv
use "`m7'\`zona' - otras actividades y ayudas en la semana.dta", clear
append using "`m8'\`zona' - otras actividades y ayudas en la semana.dta"
append using "`m9'\`zona' - otras actividades y ayudas en la semana.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_otrasactv.dta", replace

*otros ingresos
use "`m7'\`zona' - otros ingresos.dta", clear
append using "`m8'\`zona' - otros ingresos.dta"
append using "`m9'\`zona' - otros ingresos.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_otrosing.dta", replace
 
*Vivienda y Hogares
use "`m7'\`zona' - vivienda y hogares.dta", clear
append using "`m8'\`zona' - vivienda y hogares.dta"
append using "`m9'\`zona' - vivienda y hogares.dta"
egen idh = concat(directorio secuencia_p)
sort idh
saveold "`t3'col_`zona'_viv.dta", replace
}


*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
use `t3'col_`zona'_personas.dta, clear
merge id using `t3'col_`zona'_desocupados.dta  
tab _merge
drop _merge
sort id

merge  id using `t3'col_`zona'_ft.dta 
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_inactivos.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_ocupados.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_otrasactv.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_otrosing.dta
tab _merge
drop _merge
egen idh = concat(directorio secuencia_p)
sort idh

merge idh using `t3'col_`zona'_viv.dta
tab _merge
drop _merge 
sort id
saveold "`out'COL_`anio't3`zona'.dta", replace
}
*/
*4. Append zonas
*---------------
*En la base de resto hay dos preguntas adicionales que implican hacer los siguientes ajustes para unificar los nombres de las variables

clear
use "`out'COL_`anio't3cabecera.dta"
drop p5210s150 fex_c area /*solo esta en area urbana*/
rename ft fturbana
append using "`out'COL_`anio't3resto.dta"
replace fex_c_2012=fex_c_2012/3
sort id

merge id using "Y:\survey\COL\GEIH\2012\a\data_merge\pov.dta"
tab _merge
drop _merge

saveold "`out'COL_`anio't3.dta", replace






















