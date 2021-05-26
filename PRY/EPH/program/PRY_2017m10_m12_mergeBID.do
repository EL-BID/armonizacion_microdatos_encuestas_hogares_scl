* (Versi󮠓tata 12)
clear
set more off

*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor 򮩣amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\PRY\EPH\2017\m10_m12\data_orig"

local PAIS PRY
local ENCUESTA EPH
local ANO "2017"
local ronda m10_m12

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa Paraguay
Encuesta: EPH 
Round: Octubre-Diciembre
Autores:
Versión: Daniela Zuluaga
Última versión: Alvaro Altamirano - Email: alvaroalt@iadb.org
Fecha de última modificación - Junio de 2018

							SCL/SCL - IADB
****************************************************************************/


forvalues i = 1/2 {
use "$ruta\r0`i'_eph2017.dta", clear
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save, replace
}

/*Unifico los modulos de inter고para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\r02_eph2017.dta", clear
merge m:1 upm nvivi nhoga using "$ruta\r01_eph2017.dta"
drop _merge
sort upm nvivi nhoga l02

merge m:1 upm nvivi nhoga using "$ruta\ingrefam_eph_2017.dta", force
drop _merge
sort upm nvivi nhoga


saveold "`base_out'", v(12) replace


