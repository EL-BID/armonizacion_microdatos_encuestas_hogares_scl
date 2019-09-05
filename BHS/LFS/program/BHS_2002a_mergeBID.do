* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\\BHS\LFS\\2002\\a\\data_orig"

local PAIS BHS
local ENCUESTA LFS
local ANO "2002"
local ronda a

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bahamas
Encuesta: LFS
Round: a
Autores: Melany Gualavisi - Email: mayras@iadb.org
Última versión: 
Fecha última modificación: 6/17/2015

							SCL/LMK - IADB
****************************************************************************/

	*CONFORMACIÓN BASE ÚNICA BAHAMAS 2012*


use  "$ruta\Bahamas_LFS_2002_housing.dta", clear
capture drop household_no2
gen household_no2  = string(household_no,"%02.0f")
capture drop id
egen id=concat(island enum1 household_no2)
sort id
save "$ruta\Bahamas_LFS_2002_housing.dta", replace

use "$ruta\Bahamas_LFS_2002_individual.dta", clear
rename ED enum1
rename hhno household_no
capture drop household_no2
gen household_no2  = string(household_no,"%02.0f")
capture drop id
egen id=concat(island enum1 household_no2)
sort id
merge id using "$ruta\Bahamas_LFS_2002_housing.dta"
tab _merge
keep if _merge==3
drop _merge


saveold "`base_out'", replace

