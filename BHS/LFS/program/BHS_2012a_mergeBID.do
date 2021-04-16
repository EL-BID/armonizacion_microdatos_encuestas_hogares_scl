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
 
global ruta = "${surveysFolder}\\survey\\BHS\LFS\\2012\\a\\data_orig"

local PAIS BHS
local ENCUESTA LFS
local ANO "2012"
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


use  "$ruta\Bahamas_LFS_2012_housing.dta", clear
sort island hhid
save "$ruta\Bahamas_LFS_2012_housing.dta", replace

use "$ruta\Bahamas_LFS_2012_individual.dta", clear
sort island hhid
merge island hhid using "$ruta\Bahamas_LFS_2012_housing.dta"
tab _merge
drop _merge


saveold "`base_out'", replace

