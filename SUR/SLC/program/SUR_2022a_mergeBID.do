*********************************************
****** Merge modulos de Vivienda y Hogar ****
*********************************************

* Elaborado por: Eric Torres; e-mail: erict@iadb.org / etorresram@gmail.com
* Fecha: Octubre de 2022

clear all
set more off 

local PAIS SUR
local ENCUESTA SLC
local ANIO 2022
local RONDA a

local ruta = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\data_orig"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\\data_merge\\`PAIS'_`ANIO'`RONDA'.dta"

* Sort de base
use "`ruta'\2022 RT001_Housing_plus.dta", clear
duplicates report hhid 
sort hhid 
saveold "`ruta'\2022 RT001_Housing_plus.dta", replace

* Merge de base de hogar con base individual
use "`ruta'\2022 RT002_Persons.dta", clear

duplicates report hhid memberid
sort hhid memberid

merge m:1 hhid using "`ruta'\2022 RT001_Housing_plus.dta"
drop _merge

*This step helps us to save de database in older Stata formats without errors, changing the strL variables to str244.
recast str244 q03_21 q09_02_desc_1 q09_02_desc_2 q09_02_desc_3 q09_17 q09_18 q09_20 q09_40, force

rename *, lower

saveold "`base_out'", version(12) replace 
