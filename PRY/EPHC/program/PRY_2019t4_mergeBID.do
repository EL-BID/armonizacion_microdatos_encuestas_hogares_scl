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
 
global ruta = "${surveysFolder}\\survey\PRY\EPHC\2019\t4\data_orig"

local PAIS PRY
local ENCUESTA EPHC
local ANO "2019"
local ronda t4

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa Paraguay
Encuesta: EPHC 
Round: Octubre-Diciembre
Autores:
Versión: Alvaro Altamirano
Última versión: Daniela Zuluaga - Email: danielazut@iadb.org
Fecha de última modificación - Octubre de 2020

							SCL/SCL - IADB
****************************************************************************/

/*Nota: Tuve que descargar el módulo de vivienda del siguiente link:
https://www.dgeec.gov.py/datos/encuestas/eph/Vivienda/
(porque la página de descarga de la nueva base solo trae informaciones del mercado laboral - https://www.dgeec.gov.py/microdatos/ - )
*/

*Hago rename de las bases descargadas y luego las sorteo
forvalues i = 1/3 {
use "$ruta\r0`i'_ephc2019.dta", clear
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save, replace
}

/*Unifico los modulos de inter고para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\reg02_ephc_t4_2019.dta", clear
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
merge m:1 upm nvivi nhoga using "$ruta\r01_ephc2019.dta"
drop _merge
sort upm nvivi nhoga l02

merge m:1 upm nvivi nhoga using "$ruta\r03_ephc2019.dta", force
drop _merge
sort upm nvivi nhoga


*Merge con variables educ que no estaban dentro de base de mercado laboral, descargadas de: 
*https://www.dgeec.gov.py/datos/encuestas/eph/Poblacion/
merge m:m upm nvivi nhoga l02 using "$ruta\r02_ephc2019.dta", force

drop _merge
sort upm nvivi nhoga

saveold "`base_out'", v(12) replace
