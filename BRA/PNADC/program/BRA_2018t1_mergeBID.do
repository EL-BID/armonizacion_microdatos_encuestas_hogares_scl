/*==================================================
project:       Integrar 4 trimestres PNADC
Author:        Angela Lopez 
E-email:       alop@iadb.org - ar.lopez@uniandes.edu.co
url:           
Dependencies:  
----------------------------------------------------
Creation Date:    25 Jun 2019 - 10:57:54
Modification Date:   
Do-file version:    01
References:          
Output:             
==================================================*/

/*==================================================
              0: Program set up
==================================================*/
version 15.1
drop _all

global input "\\Sdssrv03\surveys\survey\BRA\PNADC\2018\t1\data_orig"
global output "\\Sdssrv03\surveys\survey\BRA\PNADC\2018\t1\data_merge" 
global final  "\\Sdssrv03\surveys\harmonized\BRA\PNADC\data_arm" 
/*==================================================
              1: Convierto txt. a .Dta 
==================================================*/

*local trimestre 01 02 03 04 
local anos 2018
local ronda t1
foreach ano of local anos {
	*foreach trim of local trimestre {
		
		clear
		* Se debe utilizar el input publicado por el IBGE para el formato de la base:
		infile using "${input}\input_visita_2018.do", using("${input}\PNADC_`ano'_visita1.txt")
		save   "${output}\PNADC_`ano'`ronda'_BID.dta", replace
		clear
	
}

/*==================================================
              2: other cleaning
==================================================*/

local ano 2018
local ronda t1

use "${output}\PNADC_`ano'`ronda'_BID.dta"


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
              3: Guardo base trimestral 
==================================================*/

*upa estrato v1008 v1016 v1027

compress

save   "${output}\BRA_2018t1_BID.dta", replace
exit

/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:


