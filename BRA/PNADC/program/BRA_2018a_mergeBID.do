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

global input  "\\Sdssrv03\surveys\survey\BRA\PNADC\2018\a\data_orig"
global output "\\Sdssrv03\surveys\survey\BRA\PNADC\2018\a\data_merge" 
global final  "\\Sdssrv03\surveys\harmonized\BRA\PNADC\data_arm" 

/*==================================================
              1: txt. to .dta 
==================================================*/

local trimestre 01 02 03 04 
local anos 2018

foreach ano of local anos {
	foreach trim of local trimestre {
		clear
		* Se debe utilizar el input publicado por el IBGE para el formato de la base:
		infile using "${input}\input_2018.do", using("${input}\PNADC_`trim'`ano'.txt")
		save   "${output}\PNADC_`trim'`ano'.dta", replace
		clear
	}
}


       infile using "\\Sdssrv03\surveys\survey\BRA\PNADC\2018\a\data_orig\input_visita_2018.do", using("\\Sdssrv03\surveys\survey\BRA\PNADC\2018\a\data_orig\PNADC_2018_visita1.txt")
		save   "${output}\PNADC_`ano'_visita.dta", replace

/*==================================================
              2: append bases 
==================================================*/
local trimestre 02 03 04 
local anos 2018

use "${output}\PNADC_012018.dta"

foreach ano of local anos {
	foreach trim of local trimestre {
		append using "${output}\PNADC_`trim'`ano'.dta"
	}
}

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
save   "${output}\PNADC_`anos'a.dta", replace
save   "${final}\BRA_2018a_BID.dta", replace
exit



