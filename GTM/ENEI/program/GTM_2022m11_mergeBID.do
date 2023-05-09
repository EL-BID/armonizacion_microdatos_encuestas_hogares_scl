* (Version Stata 17)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*

* en el 2023 No se corrió desde servidor, sino desde una computadora personal en local. Se cambiaron las rutas.
*global surveysFolder"C:\Users\jilli\IADB_2023\Harmonizacion_encuestas\surveys"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2022"
local ronda m11

global ruta = "${surveysFolder}\\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_orig"
*global ruta = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig"
display "$ruta"

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
*local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
display "`log_file'"

local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"
*local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"
display "`base_out'"

capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Octubre
Autores: Mélany Gualavisí
Última versión: Jillie Chang -jilliec@iadb.org
Fecha última modificación: abril 2023

							SCL/LMK - IADB
****************************************************************************/

*1. Import original dataset Excel/SPSS and save it as dta
*Excel: import excel "$ruta\ENEI-1-2022_HOGARES.xlsx", sheet("Sheet1") firstrow clear case(lower)

import spss "$ruta\hogares.sav" , clear
label values * // se remueven labels porque hay conflicto con base a nivel de peronas
save "$ruta\ENEI-1-2022_HOGARES.dta", replace 

import spss "$ruta\personas.sav",clear
save "$ruta\ENEI-1-2022_PERSONAS.dta", replace 

*2. Merge

use "$ruta\ENEI-1-2022_PERSONAS.dta", clear
sort hogar_num
merge m:1 hogar_num using "$ruta\ENEI-1-2022_HOGARES.dta"
tab _merge
drop _merge
rename *, lower
saveold "`base_out'", version(12) replace

log close

*lista de variables
*ds,alpha
*comparación de bases 2021 y 2022
*cf _all using "C:\Users\jilli\IADB_2023\Harmonizacion_encuestas\surveys\survey\GTM\ENEI\2021\m10\data_merge\GTM_2021m10.dta", verbose

