*********************************************
****** Merge modulos de Vivienda y Hogar ****
*********************************************

* Última modificación: Eric Torres
* Fecha: 14/03/2023

clear all
set more off 
global ruta = "${surveysFolder}"
 
local PAIS ECU
local ENCUESTA ENEMDU
local ANIO 2022
local RONDA m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANIO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\data_orig\\" 
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\\data_merge\\`PAIS'_`ANIO'`RONDA'.dta"

capture log close
log using "`log_file'", replace 

* Sort de base
import delimited "`base_in'\enemdu_persona_2022_12.csv", clear
duplicates report area estrato upm  vivienda hogar p01
sort area estrato upm vivienda hogar p01
saveold "`base_in'\miembros.dta", version(12) replace

* Merge de base de hogar con base individual
import delimited "`base_in'\enemdu_vivienda_hogar_2022_12.csv", clear
duplicates report area estrato upm  vivienda hogar
sort area estrato upm vivienda hogar 
saveold "`base_in'\hogares.dta", version(12) replace

merge 1:m area estrato upm  vivienda hogar using "`base_in'\miembros.dta", force
drop _merge
destring fexp ingpc,  dpcomma replace
destring *, replace
saveold "`base_out'", version(12) replace





