* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\\BHS\LFS\\2004\\a\\data_orig"

local PAIS BHS
local ENCUESTA LFS
local ANO "2004"
local ronda a

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"



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


use  "$ruta\Bahamas_LFS_2004_housing.dta", clear
capture drop sup_dist2
gen sup_dist2  = string(sup_dist,"%03.0f")
capture drop enum_dist2
gen enum_dist2  = string(enum_dist,"%02.0f")
capture drop id
egen id=concat(island sup_dist2 enum_dist2 settlement hhno)
sort id
save "$ruta\Bahamas_LFS_2004_housing.dta", replace

use "$ruta\Bahamas_LFS_2004_individual.dta", clear
gen sup_dist2  = string(sup_dist,"%03.0f")
gen enum_dist2  = string(enum_dist,"%02.0f")
egen id=concat(island sup_dist2 enum_dist2 settlement hhno)
sort id
merge id using "$ruta\Bahamas_LFS_2004_housing.dta"
tab _merge
keep if _merge==3
drop _merge


saveold "`base_out'", replace

