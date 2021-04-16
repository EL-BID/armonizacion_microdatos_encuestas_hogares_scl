*********************************************
****** Merge modulos de Vivienda y Hogar ****
*********************************************

* Modificado por: Stephanie González; e-mail: stephaniego@iadb.org
* Fecha: 5 de mayo de 2017

clear all
set more off 
 global ruta = "${surveysFolder}"
 
local PAIS ECU
local ENCUESTA ENEMDU
local ANIO 2016
local RONDA m12

local ruta = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\data_orig"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\\data_merge\\`PAIS'_`ANIO'`RONDA'.dta"

* Sort de base
use "`ruta'\201612_enemdubdd_vivivendahogar.dta", clear
duplicates report area ciudad zona sector panelm vivienda hogar 
sort area ciudad zona sector panelm vivienda hogar 
saveold "`ruta'\201612_enemdubdd_vivivendahogar.dta", replace


* Merge de base de hogar con base individual
*use "`ruta'\201512_enemdubdd_15anios.dta", clear
use "`ruta'\122016_enemdubdd_completa.dta", clear

duplicates report area ciudad zona sector panelm vivienda hogar p01
sort area ciudad zona sector panelm vivienda hogar p01

merge m:1 area ciudad zona sector panelm vivienda hogar using "`ruta'\201612_enemdubdd_vivivendahogar.dta"
drop _merge

saveold "`base_out'", version(12) replace 

