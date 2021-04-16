*********************************************
****** Merge modulos de Vivienda y Hogar ****
******      Julio 2015           ****
*********************************************

*Name: Marcela G. Rubio
*Date: Julio, 2015


clear all
set more off

local PAIS ECU
local ENCUESTA ENEMDU
local ANO "2002"
local RONDA m11

global ruta = "${surveysFolder}\survey\ECU\ENEMDU\2002\m11\data_mecovi\"
global out = "${surveysFolder}\survey\ECU\ENEMDU\2002\m11\data_merge\"


use "$ruta\viv12_2002.dta", clear
duplicates report ciudad zona sector vivienda hogar
egen id = concat(ciudad zona sector vivienda hogar)
duplicates tag id, gen(copies)
* drop casos duplicados
drop if copies==3 | copies==6
drop if id=="180150011011031" & copies==1 & vo==1
drop if id=="180150011011021" & copies==1 & vo==1
drop if id=="180150011011011" & copies==1 & vo==1
sort id

saveold "$ruta\viv12_2002_altered.dta", replace

use "$ruta\per12_2002.dta", clear

duplicates report ciudad zona sector vivienda hogar persona

egen id = concat(ciudad zona sector vivienda hogar)
merge m:1 id using "$ruta\viv12_2002_altered.dta"

saveold "$out\`PAIS'_`ANO'`RONDA'.dta", replace
