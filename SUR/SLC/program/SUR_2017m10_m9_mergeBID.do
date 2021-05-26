*********************************************
****** Merge modulos de Vivienda y Hogar ****
*********************************************

* Elaborado por: Alvaro Altamirano; e-mail: alvaroalt@iadb.org
* Fecha: junio de 2018

clear all
set more off 

local PAIS SUR
local ENCUESTA SLC
local ANIO 2017
local RONDA m10_m9

local ruta = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\data_orig"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\\data_merge\\`PAIS'_`ANIO'`RONDA'.dta"

* Sort de base
use "`ruta'\Households.dta", clear
duplicates report hhid 
sort hhid 
saveold "`ruta'\Households.dta", replace

* Merge de base de hogar con base individual
use "`ruta'\Individuals.dta", clear

duplicates report hhid memberid
sort hhid memberid

merge m:1 hhid using "`ruta'\Households.dta"
drop _merge

saveold "`base_out'", version(12) replace 
