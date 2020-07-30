* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS BRA
local ENCUESTA PNADC
local ANO "2017"
local ronda t1
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\"
                        
capture log close
log using "`log_file'", replace 


/*************************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Brasil
Encuesta: PNADC ///La PNAD continua reemplaza a la PNAD tradicional a partir de 2016!
Round: anual, trimestres 1-4
Autores: Alvaro Altamirano / alvaroalt@iadb.org
Fecha última modificación: Julio 2018
							
**************************************************************************************/
**************************************************************************************/

/*Estas bases de datos fueron descargadas el 27 de junio de 2018*/

*1 - Conversion Bases de datos

infile using "`base_in'input_2017.do", using("`base_in'PNADC_2017_entr1_20180426.txt")
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

/*la base -ni el diccionario- ya no se divide más entre personas y domicilios,
 es decir, ya no hay necesidad de hacer merge como con la PNAD tradicional*/
 
 *Versión 12 no acepta labels con más de 79 caracteres
 foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}

compress
saveold "`base_out'BRA_2017t1.dta", v(12) replace

