*Usando stata v. 12
clear

*Alvaro Altamirano 10/07/2019

global ruta = "${surveysFolder}"

local PAIS HTI
local ENCUESTA DHS
local ANO "2016_2017"
local ronda m11_m4

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 

set more off

 /*Merge */
use "`base_in'\HTIR70FL_corta.dta", clear //base de mujeres
sort v001 v002 v003

merge m:m v001 v002 v003 using "`base_in'\HTMR70FL_corta.dta" //base de hogares
merge m:1 v001 v002 using "`base_in'\HTHR70FL.dta", gen(household) //base de hombres
keep if household==3

save "`base_out'", replace

