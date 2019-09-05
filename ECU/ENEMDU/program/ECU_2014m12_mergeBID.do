*********************************************
****** Merge modulos de Vivienda y Hogar ****
*********************************************

* Elaborado por: Marcela G. Rubio; e-mail: mrubio@iadb.org
* Fecha: 30 de abril de 2015

clear all
set more off 

local PAIS ECU
local ENCUESTA ENEMDU
local ANIO 2014
local RONDA m12

local ruta = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\data_orig"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANIO'\\`RONDA'\\data_merge\\`PAIS'_`ANIO'`RONDA'.dta"

* Sort de base
use "`ruta'\201412_enemdubdd_viviendahogar.dta", clear
duplicates report area ciudad zona sector panelm vivienda hogar 
sort area ciudad zona sector panelm vivienda hogar 
saveold "`ruta'\201412_enemdubdd_viviendahogar.dta", replace


* Merge de base de hogar con base individual
use "`ruta'\201412_enemdubdd_15anios.dta", clear

duplicates report area ciudad zona sector panelm vivienda hogar p01
sort area ciudad zona sector panelm vivienda hogar p01

merge m:1 area ciudad zona sector panelm vivienda hogar using "`ruta'\201412_enemdubdd_viviendahogar.dta"
drop _merge

saveold "`base_out'", replace 
