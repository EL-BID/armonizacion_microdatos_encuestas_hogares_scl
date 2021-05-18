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
 


local PAIS GTM
local ENCUESTA ENCOVI
local ANO "2014"
local ronda m3_m8

global ruta = "${surveysFolder}\survey\GTM\ENCOVI\2014\m3_m8\data_orig\"
local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENCOVI
Round: Anual
Última versión: Marcela G. Rubio - Email: mrubio@iadb.org, marcelarubio28@gmail.com
Última modificación: Daniela Zuluaga -Email: danielazu2iadb.org da.zuluaga@hotmail.com
Fecha última modificación: Octubre de 2017

							SCL/LMK - IADB
*******************************************************************************/

clear
set more off






/*
use "$ruta\otros_gtos_mes_pasado.dta", clear 

/* Código de los programas que corresponden al último número de las variables

           1 asistencia alimentar
           2 mi comedor seguro
           3 insumos agrícolas
           4 jovenes protagonista
           5 vaso de atol
           6 alimentación escolar
           7 bono de transporte e
           8 becas escolares
           9 bolsas escolares
          10 programa de salud
          11 programa mi bono seg
          12 mi bolsa segura
          13 programa adulto mayo
          14 otro
*/


foreach var of varlist p03c01 p03c02 p03c03a p03c03b p03c04 p03c05 p03c06 p03c07 p03c08a p03c08b p03c09 p03c10 p03c11 p03c12 p03c13a p03c13b p03c14 p03c15 p03c16 dia_enc mes_enc a_enc {
rename `var' `var'_ 
}

reshape wide p03c01_ p03c02_ p03c03a_ p03c03b_ p03c04_ p03c05_ p03c06_ p03c07_ p03c08a_ p03c08b_ p03c09_ p03c10_ p03c11_ p03c12_ p03c13a_ p03c13b_ p03c14_ p03c15_ p03c16_ dia_enc_ mes_enc_ a_enc_, i(region depto area numhog factor pobreza thogar upm) j(id_beneficio) 

saveold "$ruta\asistencia_social_reshape.dta"
*/


*Modificación Mayra Sáenz (Mayo 16,2016): Se incluye el módulo de equipamiento del hogar

/*
use "$ruta\equipamientos_del_hogar.dta", clear 
duplicates	report	numhog

foreach var of varlist p01i01a-a_enc {
rename `var' `var'_ 
}

reshape wide p01i01a_ p01i02a_ p01i03a_ p01i04a_ p01i05a_ dia_enc_ mes_enc_ a_enc_, i(region depto area numhog factor pobreza thogar upm) j(id_equipa) 

saveold "$ruta\equipamientos_del_hogar_reshape.dta"
*/



use "$ruta\seguridad_ciudadana.dta", clear

merge m:1 numhog using "$ruta\asistencia_social_reshape.dta"
drop _merge

merge m:1 numhog using "$ruta\agrega_consumo.dta"
drop _merge

merge m:1 numhog using "$ruta\equipamientos_del_hogar_reshape.dta"
drop _merge


*DZ Octubre 2014: Se agregan las variables restantes del modulo de hogares para completar variables de housing**
merge m:1 numhog using "$ruta\hogares.dta"
drop _merge

compress
saveold "`base_out'", replace

