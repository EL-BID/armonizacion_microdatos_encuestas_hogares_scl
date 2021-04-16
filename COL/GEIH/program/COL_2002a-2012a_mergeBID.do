*ELaboracion: Yessenia Loayza
*Fecha: Febrero, 2014

*Elaborado para unir las bases anuales de Colombia 2002-2012 (estas no se usan en la armonizacion)

clear
global survey = "ECH" /*Seleccionar el nombre de la encuesta: GEIH o ECH */
forvalues i=2003/2003 { /*Seleccionar los años GEIH=2008/2012 ; ECH=2002/2005*/

use "${surveysFolder}\survey\COL\\${survey}\\`i'\a\data_orig\personas `i'.dta", clear
if `i'>=2006 & `i'<=2012 {
egen idh_ch=concat(directorio secuencia_p)
}
if `i'>=2002 & `i'<=2005 {
gen idh_ch=llave_hog
if `i'==2003 {
destring idh_ch, replace force
}
}

sort idh_ch
merge idh_ch using "${surveysFolder}\survey\COL\\${survey}\\`i'\a\data_orig\hogares`i'.dta"
tab _merge
drop _merge
save "${surveysFolder}\survey\COL\\${survey}\\`i'\\a\data_merge\COL_`i'a.dta", replace
}
