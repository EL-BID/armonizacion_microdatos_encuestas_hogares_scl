*********************************************
****** Merge modulos de Vivienda y Hogar ****
*********************************************

* Última modificación: Alvaro Altamirano; e-mail: alvaroalt@iadb.org
* Fecha: 19 de junio de 2018

clear all
set more off 
global ruta = "${surveysFolder}"
 
local PAIS ECU
local ENCUESTA ENEMDU
local ANIO 2017
local RONDA m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANIO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\data_orig\\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\\data_merge\\`PAIS'_`ANIO'`RONDA'.dta"

capture log close
log using "`log_file'", replace 

* Sort de base
use "`base_in'\201712_EnemduBDD_15anios.dta", clear

duplicates report area ciudad conglomerado zona sector panelm vivienda hogar p01
destring conglomerado zona sector panelm, replace
sort area ciudad conglomerado zona sector panelm vivienda hogar p01
saveold "`base_in'\miembros.dta", version(12) replace

* Merge de base de hogar con base individual
use "`base_in'\201712_EnemduBDD_viviendahogar.dta", clear

duplicates report area ciudad conglomerado zona sector panelm vivienda hogar 
sort area area ciudad conglomerado zona sector panelm vivienda hogar 
saveold "`base_in'\hogares.dta", version(12) replace

merge 1:m area ciudad conglomerado zona sector panelm vivienda hogar using "`base_in'\miembros.dta", force
drop _merge
saveold "`base_out'", version(12) replace 

