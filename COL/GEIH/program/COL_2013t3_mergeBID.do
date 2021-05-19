*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Enero, 2014

*** MERGE COLOMBIA GEIH 2013 ****
*------------------------------*	

clear
set more off
local anio = 2013
local anioab ="13" 	
local ruta "${surveysFolder}\survey\COL\GEIH\\`anio'\"
local m7 ="`ruta'm7\data_orig\" 
local m8 ="`ruta'm8\data_orig\" 
local m9 ="`ruta'm9\data_orig\" 
local t3   ="`ruta't3\data_orig\"
local out  ="`ruta't3\data_merge\"


*1. Bases anuales con homologacion de ingresos
*----------------------------------------------

clear
use "${surveysFolder}\survey\COL\GEIH\2013\a\data_orig\anual_homologado DANE\personas_2013.dta", clear
egen idh=concat(directorio secuencia_p)
sort idh
merge m:1 idh using "${surveysFolder}\survey\COL\GEIH\2013\a\data_orig\anual_homologado DANE\hogares_2013.dta"
drop _merge
egen id =concat (directorio secuencia_p orden)
sort id
save "${surveysFolder}\survey\COL\GEIH\2013\a\data_merge\pov_anual.dta", replace
destring mes, replace
keep if mes>=7 & mes<=9
keep  id impa- fex_c nper- fex_dpto_c
save "${surveysFolder}\survey\COL\GEIH\2013\a\data_merge\pov_t3.dta", replace

*2. Append entre meses
*------------------------

foreach zona in cabecera resto {

*Personas
use "`m7'\`zona'_caracter_sticas_generales_personas_.dta", clear
append using "`m8'\`zona'_caracter_sticas_generales_personas_.dta"
append using "`m9'\`zona'_caracter_sticas_generales_personas_.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_personas.dta", replace

*Desocupados
use "`m7'\`zona'_desocupados.dta", clear
append using "`m8'\`zona'_desocupados.dta"
append using "`m9'\`zona'_desocupados.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_desocupados.dta", replace
				
*Fuerza Trabajo
use "`m7'\`zona'_fuerza_de_trabajo.dta", clear
append using "`m8'\`zona'_fuerza_de_trabajo.dta"
append using "`m9'\`zona'_fuerza_de_trabajo.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_ft.dta", replace

*Inactivos
use "`m7'\`zona'_inactivos.dta", clear
append using "`m8'\`zona'_inactivos.dta"
append using "`m9'\`zona'_inactivos.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_inactivos.dta", replace

*ocupados
use "`m7'\`zona'_ocupados.dta", clear
append using "`m8'\`zona'_ocupados.dta"
append using "`m9'\`zona'_ocupados.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_ocupados.dta", replace

*otrasactv
use "`m7'\`zona'_otras_actividades_y_ayudas_en_la_semana.dta", clear
append using "`m8'\`zona'_otras_actividades_y_ayudas_en_la_semana.dta"
append using "`m9'\`zona'_otras_actividades_y_ayudas_en_la_semana.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_otrasactv.dta", replace

*otros ingresos
use "`m7'\`zona'_otros_ingresos.dta", clear
append using "`m8'\`zona'_otros_ingresos.dta"
append using "`m9'\`zona'_otros_ingresos.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_otrosing.dta", replace
 
*Vivienda y Hogares
use "`m7'\`zona'_vivienda_y_hogares.dta", clear
append using "`m8'\`zona'_vivienda_y_hogares.dta"
append using "`m9'\`zona'_vivienda_y_hogares.dta"
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

*4. Append zonas
*---------------
*En la base de resto hay dos preguntas adicionales que implican hacer los siguientes ajustes para unificar los nombres de las variables


clear
use "`out'COL_`anio't3cabecera.dta"
drop area /*solo esta en area urbana*/
rename ft fturbana
append using "`out'COL_`anio't3resto.dta"
replace fex_c_2011=fex_c_2011/3
sort id

merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2013\a\data_merge\pov_t3.dta"
drop _merge

saveold "`out'COL_`anio't3.dta", replace























