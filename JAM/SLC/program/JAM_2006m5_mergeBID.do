
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
 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Jamaica
Encuesta: JSLC
Round: Mayo 2000
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Diciembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
*Merge antiguo Sin referencia.
/*
clear all
set mem 500m
set more off
cd "${surveysFolder}\Jamaica\2006\JSLC\Datos Originales\slc2006"

use rec004

merge 1:1 serial ind using rec005
drop _merge


merge 1:1 serial ind using rec011
drop _merge

merge 1:1 serial ind using rec006
drop _merge



merge 1:1 serial ind using rec038
drop _merge	

save slc2006, replace


use rec022
destring oft, replace
/*
0   			 	      15  	 1.6%  
1  Daily 	    		  28  	 2.9%  
2  Weekly				  14  	 1.4%  
3  Fortnightly 			  112 	  11.6%  
4  Monthly				  286 	  29.6%  
5  Quarterly			  49  	 5.1%  
6  Half a yearly		  60  	 6.2%  
7  Yearly				  135 	  14.0%  
8  Occassionally		  73  	 7.5%  
9  Only when requested	  38  	 3.9%  
97  Never				  15  	 1.6%  
99					      142 	  14.7%  
*/
destring amt, replace
gen remesas_ci=.
replace remesas_ci=amt*30 if oft==1
replace remesas_ci=amt*4.3 if oft==2
replace remesas_ci=amt*2 if oft==3
replace remesas_ci=amt if oft==4
replace remesas_ci=amt/3 if oft==5
replace remesas_ci=amt/6 if oft==5
replace remesas_ci=amt/12 if oft==5

collapse (sum) remesas_ci, by(serial ind)

save remesas.dta, replace

use slc2006

merge 1:1 serial ind using remesas.dta
drop _merge

foreach num of numlist 19 20 21  23 37 {
	merge m:1 serial using rec0`num'
	drop _merge
	}
	
	
merge m:1 serial using rec001
drop _merge 


save "${surveysFolder}\Jamaica\2006\JSLC\Data\slc2006.dta", replace
save "${surveysFolder}\ARM\JAM\2006\JSLC\Orig_data\slc2006.dta", replace
*/


* La base que tiene los datos de desnutrición es la rec009.dta
clear all
global path = "${surveysFolder}\BID\JAM\Jam SLC"
use "$path\SLC\2006\m5\data_orig\slc2006\rec009.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
sort serial
saveold "$path\SLC\2006\m5\data_orig\slc2006\rec009.dta", replace





clear all
global path = "${surveysFolder}\BID\JAM\Jam SLC"
use "$path\SLC\2006\m5\data_orig\slc2006.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
sort serial

merge m:m serial using  "$path\SLC\2006\m5\data_orig\STATA anual con agregados de Consumo\annual06.dta"
tab _merge
drop _merge
merge m:m serial ind using  "$path\SLC\2006\m5\data_orig\slc2006\rec009.dta"

ds, has(type string)
    foreach var of varlist `r(varlist)' {
        replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"N")
        destring `var', replace
    }

tab _merge
drop _merge


saveold "$path\SLC\2006\m5\data_merge\JAM_2006m5.dta", replace










