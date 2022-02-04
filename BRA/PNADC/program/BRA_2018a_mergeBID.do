* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
/*==================================================
project:       PNADC - Brasil
Author:        Angela Lopez 
E-email:       alop@iadb.org - ar.lopez@uniandes.edu.co
url:           
Dependencies:  SLC/EDU
----------------------------------------------------
Creation Date:    25 Jun 2019 - 10:57:54            
==================================================*/

/*==================================================
              0: Program set up
==================================================*/
version 15.1
drop _all

global input  "${surveysFolder}\survey\BRA\PNADC\2018\a\data_orig"
global output "${surveysFolder}\survey\BRA\PNADC\2018\a\data_merge" 

local anos 2018
/*==================================================
              1: txt. to .dta 
==================================================*/

       infile using "${surveysFolder}\survey\BRA\PNADC\2018\a\data_orig\input_2018.do", using("${surveysFolder}\survey\BRA\PNADC\2018\a\data_orig\PNADC_2018_visita1.txt")
		save   "${output}\PNADC_`anos'_visita1.dta", replace

/*==================================================
              2: append bases 
==================================================*/


use "${output}\PNADC_2018_visita1.dta"

foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
 *Versión 12 no acepta labels con más de 79 caracteres
 foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
/*==================================================
              3: Guardo base anual 
==================================================*/

*upa estrato v1008 v1016 v1027

compress
save   "${output}\BRA_`anos'a.dta", replace
exit



