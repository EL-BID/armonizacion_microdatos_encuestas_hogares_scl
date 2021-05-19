*Elaboración: Marcela G. Rubio (marcelarubio28@gmail.com | mrubio@iadb.org)
*Mayo, 2016
* Added ETNIA module. Cesar Lins (SCL/GDI) Marzo 2021

*** MERGE COLOMBIA GEIH 2015 ****
*------------------------------*	

clear
set more off
local anio = 2015
local ronda1 a
local ronda2 t3	
local ruta "${surveysFolder}\survey\COL\GEIH\\`anio'\"
local m7 ="`ruta'\`ronda1'\data_orig\m7\" 
local m8 ="`ruta'\`ronda1'\data_orig\m8\" 
local m9 ="`ruta'\`ronda1'\data_orig\m9\" 
local t3 ="`ruta'\`ronda2'\data_orig\"
local out ="`ruta'\`ronda2'\data_merge\"


*1. Bases anuales con homologacion de ingresos : NO ESTÁN DISPONIBLES AÚN
*----------------------------------------------

clear
use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\personas 2015.dta", clear
merge m:1 directorio secuencia_p using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\hogares 2015.dta", force
drop _merge
merge 1:1 directorio secuencia_p orden using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\ETNIA15.dta", keep(match master)
drop _merge

egen id =concat (directorio secuencia_p orden)
sort id
saveold "`ruta'\`ronda1'\data_merge\pov_anual.dta", replace
destring mes, replace
keep if mes>=7 & mes<=9
keep  id impa-iof6 impaes-fex_dpto nper-id
saveold "`ruta'\`ronda1'\data_merge\pov_t3.dta", replace


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
*/

** Módulo de migración 

* Sección incluida por SCL/MIG Fernando Morales 

use "`m7'\Julio_mig.dta", clear
foreach v of varlist _all {
	local lowname=lower("`v'")
	cap: rename `v' `lowname'
}

append using "`m8'\Agosto_mig.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	cap: rename `v' `lowname'
}

append using "`m9'\Septiembre_mig.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	cap: rename `v' `lowname'
}

egen id = concat(directorio secuencia_p orden)
sort id
saveold "`out'\COL_`anio't3migracion.dta", replace


*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
use `t3'col_`zona'_personas.dta, clear
merge 1:1 id using `t3'col_`zona'_desocupados.dta  
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_ft.dta 
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_inactivos.dta
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_ocupados.dta
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_otrasactv.dta
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_otrosing.dta
drop _merge
egen idh = concat(directorio secuencia_p)
sort idh

merge m:1 idh using `t3'col_`zona'_viv.dta
drop _merge 
sort id
saveold "`out'COL_`anio't3`zona'.dta", replace
}

*4. Append zonas
*---------------

clear
use "${surveysFolder}\survey\COL\GEIH\2015\t3\data_merge\COL_2015t3cabecera.dta", clear
append using "${surveysFolder}\survey\COL\GEIH\2015\t3\data_merge\COL_2015t3resto.dta" 
merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2015\t3\data_merge\COL_2015t3migracion.dta", nogen
replace fex_c_2011=fex_c_2011/3
sort id

merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2015\a\data_merge\pov_t3.dta"
drop _merge

saveold "${surveysFolder}\survey\COL\GEIH\2015\t3\data_merge\COL_2015t3.dta", replace























