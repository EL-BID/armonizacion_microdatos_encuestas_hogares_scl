set mem 200m
set more off

****Colombia ECV 2008*****

********************************************************************
*****	UNION DE LOS ARCHIVOS 					****
********************************************************************


***************************************************
*****	Adecuando los archivos a unir		***
***************************************************

cd "Z:\Colombia\2008\ECV\Datos Originales\Stata" 

use viviendas.dta, clear
egen llave=group(periodo directorio)
sort llave
save viviendas.dta, replace

use hogares.dta,clear
egen llave=group(periodo directorio)
sort llave
save hogares.dta,replace

merge llave using viviendas.dta
tab _merge
drop _merge

sort periodo directorio secuencia_encuesta
egen llave_h=group(periodo directorio secuencia_encuesta)
sort llave_h
save "Z:\Colombia\2008\ECV\Data\col08_ecv.dta", replace


use personas.dta, clear
egen llave_h=group(periodo directorio secuencia_p)
sort llave_h
save personas.dta,replace

merge llave_h using "Z:\Colombia\2008\ECV\Data\col08_ecv.dta"
tab _merge
drop _merge

compress
save "Z:\Colombia\2008\ECV\Data\col08_ecv.dta", replace


