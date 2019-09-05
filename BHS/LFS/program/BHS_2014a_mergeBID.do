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
 
global ruta = "\\Sdssrv03\surveys\\survey\\BHS\LFS\\2014\\a\\data_orig"

local PAIS BHS
local ENCUESTA LFS
local ANO "2014"
local ronda a

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bahamas
Encuesta: LFS
Round: a
Autores: Melany Gualavisi - Email: melanyg@iadb.org
Última versión: 
Fecha última modificación: 12/22/2015

							SCL/LMK - IADB
****************************************************************************/

	*CONFORMACIÓN BASE ÚNICA BAHAMAS 2012*


use  "$ruta\Bahamas_LFS_2014_housing.dta", clear
sort island hhno
drop _merge
save "$ruta\Bahamas_LFS_2014_housing.dta", replace

use "$ruta\Bahamas_LFS_2014_individual.dta", clear
sort island hhno
merge island hhno using "$ruta\Bahamas_LFS_2014_housing.dta"
tab _merge
drop _merge


saveold "`base_out'", replace

