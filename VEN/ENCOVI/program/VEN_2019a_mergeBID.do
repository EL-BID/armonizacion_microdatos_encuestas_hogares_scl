* (Versión Stata 14)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}"
local PAIS VEN
local ENCUESTA ENCOVI
local ANO "2019"
local ronda a 

local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"                  
capture log close


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: ENCOVI
Round: a
Autores: Lina Arias lm.arias405@uniandes.edu.co
Fecha última modificación: Junio 2020

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
*Nota: Hay 76 personas que no tienen hogar. Estas personas se borran de la muestra*

use "$ruta\survey\VEN\ENCOVI\2019\a\data_orig\personas_VI-IX_XIV_XV.dta", clear

*hago el merge con viviendas
mmerge interview__key interview__id  using "\$ruta\survey\VEN\ENCOVI\2019\a\data_orig\hogares_II-V_XIII.dta", t(n:1)

drop if _merge==1
compress

drop _merge
rename lp31_ci lp31

save "`base_out'", replace
