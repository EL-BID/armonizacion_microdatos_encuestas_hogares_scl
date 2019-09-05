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
 
global ruta = "\\Sdssrv03\surveys\\survey\PRY\EPH\2016\m10_m12\data_orig"

local PAIS PRY
local ENCUESTA EPH
local ANO "2016"
local ronda m10_m12

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Paraguay
Encuesta: EPH 
Round: Octubre-Diciembre
Autores:
Versión 2016: Daniela Zuluaga
Última versión: Daniela Zuluaga - Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Julio de 2016

							SCL/SCL - IADB
****************************************************************************/


forvalues i = 1/2 {
use "$ruta\r0`i'_eph2016.dta", clear
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save, replace
}



/*Unifico los modulos de interés para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\r02_eph2016.dta", clear
merge m:1 upm nvivi nhoga using "$ruta\r01_eph2016.dta"
drop _merge
sort upm nvivi nhoga l02

merge m:1 upm nvivi nhoga using "$ruta\ingrefam_eph_2016.dta", force
drop _merge
sort upm nvivi nhoga


saveold "`base_out'", v(12) replace


