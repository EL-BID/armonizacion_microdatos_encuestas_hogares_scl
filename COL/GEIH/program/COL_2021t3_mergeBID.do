
/*
Autor: Eric Torres
Ultima version: Nathalia Maya
Mayo, 2021 Version Stata 16 */

*** MERGE COLOMBIA GEIH 2021 (t3) ****
*------------------------------*	

clear
set more off
local anio =2021
local ronda1 a
local ronda2 t3
local ruta "${surveysFolder}\survey\COL\GEIH\\`anio'\"
local m7 ="`ruta'\`ronda1'\data_orig\m7\" 
local m8 ="`ruta'\`ronda1'\data_orig\m8\" 
local m9 ="`ruta'\`ronda1'\data_orig\m9\" 
local t3 ="`ruta'\`ronda2'\data_orig\"
local out ="`ruta'\`ronda2'\data_merge\"


*1. Bases anuales con homologacion de ingresos:
*----------------------------------------------

clear
use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Hogares.dta", clear

merge 1:m directorio secuencia_p using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Personas.dta", force
drop _merge
egen id =concat (directorio secuencia_p orden)
sort id
saveold "`ruta'\`ronda1'\data_merge\pov_anual.dta", replace
destring mes, replace
keep if mes>=7 & mes<=9

keep  impaes- id impa-iof6 nper-fex_c dominio
save "`ruta'\`ronda1'\data_merge\pov_t3.dta", replace

*2. Append entre meses
*------------------------

foreach zona in Cabecera Resto {

*Personas
use "`m7'\`zona' - Características generales (Personas).dta", clear
append using "`m8'\`zona' - Características generales (Personas).dta"
append using "`m9'\`zona' - Características generales (Personas).dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t3'col_`zona'_personas.dta", replace

*Desocupados
use "`m7'\`zona' - Desocupados.dta", clear
append using "`m8'\`zona' - Desocupados.dta"
append using "`m9'\`zona' - Desocupados.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t3'col_`zona'_desocupados.dta", replace
				
*Fuerza Trabajo
use "`m7'\`zona' - Fuerza de trabajo.dta", clear
append using "`m8'\`zona' - Fuerza de trabajo.dta"
append using "`m9'\`zona' - Fuerza de trabajo.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t3'col_`zona'_ft.dta", replace

*Inactivos
use "`m7'\`zona' - Inactivos.dta", clear
append using "`m8'\`zona' - Inactivos.dta"
append using "`m9'\`zona' - Inactivos.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t3'col_`zona'_inactivos.dta", replace

*ocupados
use "`m7'\`zona' - Ocupados.dta", clear
append using "`m8'\`zona' - Ocupados.dta"
append using "`m9'\`zona' - Ocupados.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t3'col_`zona'_ocupados.dta", replace

*otrasactv
use "`m7'\`zona' - Otras actividades y ayudas en la semana.dta", clear
append using "`m8'\`zona' - Otras actividades y ayudas en la semana.dta"
append using "`m9'\`zona' - Otras actividades y ayudas en la semana.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
*rename clase CLASE
sort id
saveold "`t3'col_`zona'_otrasactv.dta", replace

*otros ingresos
use "`m7'\`zona' - Otros ingresos.dta", clear
append using "`m8'\`zona' - Otros ingresos.dta"
append using "`m9'\`zona' - Otros ingresos.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
*rename clase CLASE
sort id
saveold "`t3'col_`zona'_otrosing.dta", replace
 
*Vivienda y Hogares
use "`m7'\`zona' - Vivienda y Hogares.dta", clear
append using "`m8'\`zona' - Vivienda y Hogares.dta"
append using "`m9'\`zona' - Vivienda y Hogares.dta"
egen idh = concat(DIRECTORIO SECUENCIA_P)
*rename clase CLASE
sort idh
saveold "`t3'col_`zona'_viv.dta", replace
}


** Módulo de migración 

* Sección incluida por SCL/MIG Fernando Morales 

use "`m7'\Julio_mig.dta", clear
append using "`m8'\Agosto_mig.dta"
append using "`m9'\Septiembre_mig.dta"

ren (Mes Directorio Secuencia_p Orden Fex_c_2011) (MES DIRECTORIO SECUENCIA_P ORDEN fex_c_2011)
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`out'\COL_`anio't3migracion.dta", replace



*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
use "`t3'\col_`zona'_personas.dta", clear
merge 1:1 id using "`t3'\col_`zona'_desocupados.dta"  
drop _merge
sort id

merge 1:1 id using "`t3'\col_`zona'_ft.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\col_`zona'_inactivos.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\col_`zona'_ocupados.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\col_`zona'_otrasactv.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\col_`zona'_otrosing.dta"
drop _merge
egen idh = concat(DIRECTORIO SECUENCIA_P)
sort idh

merge m:1 idh using "`t3'\col_`zona'_viv.dta"
drop _merge 
sort id
saveold "`out'COL_`anio't3`zona'.dta", replace
}



*4. Append zonas
*---------------

clear
use "${surveysFolder}\survey\COL\GEIH\2021\t3\data_merge\COL_2021t3cabecera.dta", clear
append using "${surveysFolder}\survey\COL\GEIH\2021\t3\data_merge\COL_2021t3resto.dta" 
merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2021\t3\data_merge\COL_2021t3migracion.dta", nogen
replace fex_c_2011=fex_c_2011/3
sort id

merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2021\a\data_merge\pov_t3.dta"
drop _merge

foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}

saveold "${surveysFolder}\survey\COL\GEIH\2021\t3\data_merge\COL_2021t3.dta", replace
















