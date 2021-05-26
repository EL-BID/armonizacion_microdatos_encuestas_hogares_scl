/*
Autor: Marcela G. Rubio - Email: marcelarubio28@gmail.com | mrubio@iadb.org
Siguiente version: Angela Lopez(alop@iadb.org)
Modificado por Carolina Hernández para 2020 (jhernandez@colmex.mx)
Octubre, 2020 Version Stata 14 */

*** MERGE COLOMBIA GEIH 2018****
*------------------------------*	

clear all
set more off
local anio =2019
local ronda1 a
local ronda2 tmovil(0507)	
local ruta "${surveysFolder}\Users\CAROLINA\OneDrive - El Colegio de México A.C\Escritorio\sdssrv03\Surveys\Survey\COL\GEIH\\`anio'\"
local m1 ="`ruta'\`ronda1'\data_orig\m5\" 
local m2 ="`ruta'\`ronda1'\data_orig\m6\" 
local m3 ="`ruta'\`ronda1'\data_orig\m7\" 
local tmovil ="`ruta'\`ronda2'\data_orig\"
local out ="`ruta'\`ronda2'\data_merge\"


tempfile  personasm1 personasm2 personasm3  desocupadosm1 desocupadosm2 desocupadosm3 ftm1 ftm2 ftm3 inactivosm1 inactivosm2 inactivosm3 ///
ocupadosm1 ocupadosm2 ocupadosm3 otrasactm1 otrasactm2 otrasactm3  otrosingm1 otrosingm2 otrosingm3 vivm1 vivm2 vivm3 pobreza_personas ///
pobreza_hogares

tempvar s_directorio s_secuencia_p s_orden


*1. Bases anuales con homologacion de ingresos
*----------------------------------------------
use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Hogares.dta", clear
rename *, lower
save `pobreza_hogares', replace

use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Personas.dta", clear
rename *, lower
save `pobreza_personas', replace

use `pobreza_hogares', clear
merge 1:m directorio secuencia_p using `pobreza_personas', force
drop _merge

egen id =concat (directorio secuencia_p orden)
sort id
destring mes, replace
keep if mes>=5 & mes<=7


saveold "`tmovil'COL_pov_`ronda2'.dta", replace


*2. Append entre meses y merge de los  modulos trimestrales por zona
*--------------------------------------------------------------------


foreach zona in Cabecera Resto{

*Personas
*--------
*dejo bases en mismo formato
use "`m1'\`zona' - Características generales (Personas).dta", clear
rename *, lower
save `personasm1', replace
use "`m2'\`zona' - Características generales (Personas).dta", clear
rename *, lower
save `personasm2', replace
use "`m3'\`zona' - Características generales (Personas).dta", clear
rename *, lower
save `personasm3', replace
*merge de bases
use `personasm1', clear
append using `personasm2'
append using `personasm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
saveold "`tmovil'col_`zona'_personas.dta", replace


*Desocupados
*--------
*dejo bases en mismo formato
use "`m1'\`zona' - Desocupados.dta", clear
rename *, lower
save `desocupadosm1', replace
use  "`m2'\`zona' - Desocupados.dta", clear
rename *, lower
save `desocupadosm2', replace
use  "`m3'\`zona' - Desocupados.dta", clear
rename *, lower
save `desocupadosm3', replace
*merge de bases
use `desocupadosm1', clear
append using `desocupadosm2'
append using `desocupadosm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
sort id
saveold "`tmovil'col_`zona'_desocupados.dta", replace

				
*Fuerza Trabajo
*--------------
*dejo bases en mismo formato
use "`m1'\`zona' - Fuerza de trabajo.dta", clear
rename *, lower
save `ftm1', replace
use "`m2'\`zona' - Fuerza de trabajo.dta", clear
rename *, lower
save `ftm2', replace
use "`m3'\`zona' - Fuerza de trabajo.dta", clear
rename *, lower
save `ftm3', replace
*merge de bases
use `ftm1', clear
append using `ftm2'
append using `ftm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
sort id
saveold "`tmovil'col_`zona'_ft.dta", replace


*Inactivos
*--------------
*dejo bases en mismo formato
use "`m1'\`zona' - Inactivos.dta", clear
rename *, lower
save `inactivosm1', replace
use "`m2'\`zona' - Inactivos.dta", clear
rename *, lower
save `inactivosm2', replace
use "`m3'\`zona' - Inactivos.dta", clear
rename *, lower
save `inactivosm3', replace
*merge de bases
use `inactivosm1', clear
append using `inactivosm2'
append using `inactivosm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
sort id
saveold "`tmovil'col_`zona'_inactivos.dta", replace

 
*ocupados
*--------------
*dejo bases en mismo formato
use "`m1'\`zona' - Ocupados.dta", clear
rename *, lower
save `ocupadosm1', replace
use "`m2'\`zona' - Ocupados.dta", clear
rename *, lower
save `ocupadosm2', replace
use "`m3'\`zona' - Ocupados.dta", clear
rename *, lower
save `ocupadosm3', replace
*merge de bases
use `ocupadosm1', clear
append using `ocupadosm2'
append using `ocupadosm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
sort id
saveold "`tmovil'col_`zona'_ocupados.dta", replace


*otrasactv
*--------------
*dejo bases en mismo formato
use "`m1'\`zona' - Otras actividades y ayudas en la semana.dta", clear
rename *, lower
save `otrasactm1', replace
use "`m2'\`zona' - Otras actividades y ayudas en la semana.dta", clear
rename *, lower
save `otrasactm2', replace
use "`m3'\`zona' - Otras actividades y ayudas en la semana.dta", clear
rename *, lower
save `otrasactm3', replace
*merge de bases
use `otrasactm1', clear
append using `otrasactm2'
append using `otrasactm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
sort id
saveold "`tmovil'col_`zona'_otrasactv.dta", replace


*otros ingresos
*--------------
*dejo bases en mismo formato
use "`m1'\`zona' - Otros ingresos.dta", clear
rename *, lower
save `otrosingm1', replace
use "`m2'\`zona' - Otros ingresos.dta", clear
rename *, lower
save `otrosingm2', replace
use "`m3'\`zona' - Otros ingresos.dta", clear
rename *, lower
save `otrosingm3', replace
*merge de bases
use `otrosingm1', clear
append using `otrosingm2'
append using `otrosingm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
gen `s_orden'  = string(orden,"%02.0f")
egen id = concat(`s_directorio' `s_secuencia_p' `s_orden')
sort id
saveold "`tmovil'col_`zona'_otrosing.dta", replace


*Vivienda y Hogares
*--------------
*dejo bases en mismo formato
use "`m1'\`zona' - Vivienda y Hogares.dta", clear
rename *, lower
save `vivm1', replace
use "`m2'\`zona' - Vivienda y Hogares.dta", clear
rename *, lower
save `vivm2', replace
use "`m3'\`zona' - Vivienda y Hogares.dta", clear
rename *, lower
save `vivm3', replace
*merge de bases
use `vivm1', clear
append using `vivm2'
append using `vivm3'
*id con mismo formato
gen `s_directorio'  = string(directorio,"%07.0f")
gen `s_secuencia_p'  = string(secuencia_p,"%02.0f")
egen idh = concat(`s_directorio' `s_secuencia_p')
sort idh
saveold "`tmovil'col_`zona'_viv.dta", replace


*Merge de los 8 modulos trimestrales por zona
use "`tmovil'col_`zona'_ft.dta", clear
merge m:1 id using "`tmovil'col_`zona'_ocupados.dta", force
drop _merge
sort id

merge m:1 id using "`tmovil'col_`zona'_desocupados.dta", force
drop _merge
sort id

merge m:1 id using "`tmovil'col_`zona'_inactivos.dta", force
drop _merge
sort id 

merge 1:m id using "`tmovil'col_`zona'_personas.dta", force
drop _merge
sort id 

merge 1:1 id using "`tmovil'col_`zona'_otrasactv.dta", force
drop _merge
sort id 

merge 1:1 id using "`tmovil'col_`zona'_otrosing.dta", force
drop _merge
egen idh = concat(`s_directorio' `s_secuencia_p')
sort idh

merge m:1 idh using "`tmovil'col_`zona'_viv.dta", force
drop _merge 
sort id

saveold "`tmovil'COL_`anio'tmovil(0507)`zona'.dta", replace
}

*3. Append zonas
*---------------
use "`tmovil'COL_`anio'`ronda2'Cabecera.dta", clear
append using "`tmovil'COL_`anio'`ronda2'Resto.dta" 
replace fex_c_2011=fex_c_2011/3
sort id
saveold "`out'COL_`anio'`ronda2'.dta", replace

**Nota** Completo base con datos de -Medición de Pobreza Monetaria y Desigualdad-
*Dejo bases de pobreza -hogares y personas- en el mismo formato antes de unir

merge 1:m directorio secuencia_p orden using "`tmovil'COL_pov_`ronda2'.dta", force
drop _merge
saveold "`out'COL_`anio'`ronda2'.dta", replace


