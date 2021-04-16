/*
Autor: -
Modificado por Carolina Hernández para 2020 (jhernandez@colmex.mx)
Noviembre, 2020 Version Stata 14 */

*** MERGE ARGENTINA EPHC 2020****
*------------------------------*	
* MERGE
* ARGENTINA EPH. CONTINUA. Sem. 1 2020

clear all
set more off
local anio =2020
local anio2 =20
local ronda t2
local ruta "${surveysFolder}\Users\CAROLINA\OneDrive - El Colegio de México A.C\Escritorio\Armonización\sdssrv03\Surveys\Survey\ARG\EPHC\\`anio'\"
local t ="`ruta'\`ronda'\data_orig\" 
local out1 ="`ruta'\`ronda'\data_merge\"
local out2 ="`ruta'\s1\data_orig\"

local log_file = "$ruta\Harmonized\ARG\EPHC\log\ARG_`anio'`ronda'_mergeBID.log"



*1. Importo bases de hogares e individuos
*----------------------------------------------

* Base agregada de individuos 


import delimited "`t'\\usu_individual_`ronda'`anio2'.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado
save "`t'\usu_individual_`ronda'`anio2'.dta", replace

* Base agregada de hogares 
import delimited "`t'\\usu_hogar_`ronda'`anio2'.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado
save "`t'\usu_hogar_`ronda'`anio2'.dta", replace


* Merge de hogares e individuos

merge codusu nro_hogar aglomerado using "`t'\usu_individual_`ronda'`anio2'.dta"
sort codusu nro_hogar aglomerado

tab _merge


save "`out1'\ARG_`anio'`ronda'.dta", replace
save "`out2'\ARG_`anio'`ronda'.dta", replace
clear

