************************************
*** ENCUESTA DE NIVELES DE VIDA  ***
***         AÑO 2008		 ***
************************************

set more off

** MERGE

** HOUSEHOLDS

** Hogares

use hogar.dta, clear

** Viviendas 
merge llaveviv using vivienda.dta
tab _merge
drop _merge
sort llavehog


** Personas
merge llavehog using 03social.dta
tab _merge
drop _merge
sort llavehog

merge llavehog using personas.dta
tab _merge
drop _merge
sort llavehog


save pan08_env.dta, replace

