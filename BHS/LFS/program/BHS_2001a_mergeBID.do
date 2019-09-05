* (Versi�n Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\\BHS\LFS\\2001\\a\\data_orig"

local PAIS BHS
local ENCUESTA LFS
local ANO "2001"
local ronda a

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Bahamas
Encuesta: LFS
Round: a
Autores: Melany Gualavisi - Email: mayras@iadb.org
�ltima versi�n: 
Fecha �ltima modificaci�n: 6/17/2015

							SCL/LMK - IADB
****************************************************************************/

	*CONFORMACI�N BASE �NICA BAHAMAS 2012*


use  "$ruta\Bahamas_LFS_2001_housing.dta", clear
capture drop id
egen id=concat(island hhno)
sort id
save "$ruta\Bahamas_LFS_2001_housing.dta", replace

use "$ruta\Bahamas_LFS_2001_individual.dta", clear
capture drop id
egen id=concat(island hhno)
sort id
merge id using "$ruta\Bahamas_LFS_2001_housing.dta"
tab _merge
keep if _merge==3
drop _merge


saveold "`base_out'", replace

