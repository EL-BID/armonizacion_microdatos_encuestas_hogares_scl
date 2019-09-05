
* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Jamaica
Encuesta: JSLC
Round: Mayo 1996
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Diciembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/



clear all
foreach file in annual edtotals exp96 food gifts heads hhsize homegift housing laborf meals noncons nutr rec001 rec002 rec003 rec004 rec005 rec006 rec007 rec008 rec009 rec010 rec011 rec012 rec013 rec014 rec015 rec016 rec017 rec018 rec019 rec020 rec021 rec022 rec023 rec024 rec025 rec026 rec027 rec028 rec029 rec030 rec031 rec032 rec032a rec033 rec034 {
	usespss using "Y:\survey\JAM\SLC\1996\m5\data_orig\spss\\`file'.sav"
	foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
saveold "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\\`file'.dta", replace
clear all
set more off
}


                                      	

* serial


*Hogares
*Se excluyen estas bases, además se debe analizar si requieren un reshape. No se utilizan estos datos para el cálculo.
*food gifts homegift meals noncons rec021  rec023 rec024 rec026 rec027 rec031 rec032
*rec033
* Análisis de la variable que servirá como identificador: serial
clear all
foreach t in annual exp96 heads hhsize housing rec001 rec022 rec029 rec030 rec032a rec033{
	set more off
	use "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\\`t'.dta"
	di "`t'"
	duplicates report serial
		}




*Merge hogares
* Las que tienen sólo serial
clear all
use "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\annual.dta"
foreach t in exp96 heads hhsize housing rec001 rec022 rec029 rec030 rec032a rec033 {
	merge m:m serial  using "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\\`t'.dta", force
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\hogares.dta", replace



*serial e ind
serial ind

*Creación de los identificadores de personas
clear all
foreach t in nutr  rec012 rec013 rec014 rec015 rec016 rec017 rec018 rec019 rec020 rec025  rec028 rec034{
	set more off
	use "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\\`t'.dta"
	tostring ind, replace
    gen identif = "0"+ind if length(ind)==1
    replace identif = ind if length(ind)==2
	capture	egen id = concat(serial identif)
	destring id, replace
	sort id
	
	di "`t'"
	duplicates report id
	codebook id
	saveold "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\\`t'id.dta", replace
		}


*Esta base tiene caracteres no num'ericos
 use "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\nutrid.dta", clear
 gen id1= id if id ~=".0."

replace id = id1
drop id1
destring id, replace
drop if serial ==.
 saveold "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\nutrid.dta", replace



*Merge personas
clear all
use "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\rec034id.dta"
*rec002 rec003 rec004 rec005 rec006 rec007 rec008 rec009 rec010 rec011
drop if id ==.

foreach t in nutrid rec012id rec013id rec014id rec015id rec016id rec017id rec018id rec019id rec020id rec025id  rec028id  {
	merge m:m id using "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\\`t'.dta", force
	di "`t'"
	drop if id ==.
	drop if _merge ==2
	tab _merge
	
	drop _merge
	}
sort serial
saveold "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\personas.dta", replace

merge m:m serial using  "Y:\survey\JAM\SLC\1996\m5\data_orig\stata\hogares.dta"
drop if _merge ==2
tab _merge
drop _merge
saveold "Y:\survey\JAM\SLC\1996\m5\data_merge\JAM_1996m5.dta", replace



