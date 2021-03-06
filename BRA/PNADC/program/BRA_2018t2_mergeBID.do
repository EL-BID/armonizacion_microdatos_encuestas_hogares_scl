
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

*Updated by Alvaro Altamirano on June 2020:
	*Use the following python code to translate IBGE's SAS import dicts to STATA import dicts
	/*import pandas as pd
	import os
	import csv
	anio= 2019
	path = r'${surveysFolder}\Users\ALVAROALT\Desktop'
	os.chdir(path)
	input = pd.read_csv(r'input_PNADC_{}.txt'.format(anio), header=None, encoding = 'latin-1')
	input[0] = input[0].str.replace(r"([\@])(\d+)", r"_column (0\2)").astype('str')
	input[0] = input[0].str.replace(r"\/\*", '"').astype('str')
	input[0] = input[0].str.replace(r"\*\/", '"').astype('str')
	input[0] = input[0].str.replace(r"\$", '').astype('str')
	input[0] = input[0].str.replace(r"(\d+)(\.)", r"%\1g").astype('str')
	input[0].to_csv(r'input_{}_(stata)dict.txt'.format(anio), header=False, index=False, quoting=csv.QUOTE_NONE)
	*/


local pais BRA	
local ano 2020
local ronda t2
	
	
global input  "${surveysFolder}\survey\BRA\PNADC\2020\t2\data_orig"
global output "${surveysFolder}\survey\BRA\PNADC\2020\t2\data_merge" 


/*==================================================
              1: txt. to .dta 
==================================================*/

infile using "${input}\input_2020t.do", using("${input}\PNADC_022020.txt")
		save   "${output}\PNADC_`ano'`ronda'.dta", replace

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

compress
save   "${output}\PNADC_`ano'`ronda'.dta", replace
exit


*_______________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


