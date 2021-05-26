
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
Round: Noviembre 1990
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

foreach file in annual90  povline90 rec00190 rec00290 rec00390 rec00490 rec00590 rec00690 rec00790 rec00890 rec00990 rec01090 rec01190 rec01290 rec01390 rec01490 rec01590 rec01690 rec01790 rec01890 rec01990 rec02090 rec02190 rec02290 rec02390 rec02490 rec02590 rec02690 rec02890 rec02990 rec03090  rec03190  rec03290  rec03390  rec03490  rec03590{
	usespss using "${surveysFolder}\survey\JAM\SLC\1990\m11\data_orig\spss\\`file'.sav"
	foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
saveold "${surveysFolder}\survey\JAM\SLC\1990\m11\data_orig\stata\\`file'.dta", replace
clear all
set more off
}


                                      	

*serial


*Hogares
* Análisis de la variable que servirá como identificador: serial
clear all
*rec02890 rec02990 rec03090 rec03190 rec03390 rec03490 (esta base tiene algo de ingresos, se debe identificar qué es.

foreach t in rec00190     rec03290      rec03590 {
	set more off
	use "${surveysFolder}\survey\JAM\SLC\1990\m11\data_orig\stata\\`t'.dta"
	di "`t'"
	duplicates report serial
		}




*Merge hogares
* Las que tienen sólo serial. Se excluyen las referentes al 
clear all
global path = "${surveysFolder}\survey\JAM"


use "$path\SLC\1990\m11\data_orig\stata\annual90.dta"

foreach t in rec00190     rec03290      rec03590 {
	merge m:m serial using "$path\SLC\1990\m11\data_orig\stata\\`t'.dta"
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "$path\SLC\1990\m11\data_orig\stata\hogares.dta", replace



*serial y person
clear all
global path = "${surveysFolder}\survey\JAM"



clear all
foreach t in povline90 rec00290 rec00390 rec00490 rec00590 rec00690 rec00790 rec00890 rec00990 rec01090 rec01190 rec01290 rec01390 rec01490 rec01590  rec01690 rec01790 rec01890 rec01990 rec02090 rec02190 rec02290 rec02390 rec02490 rec02590 rec02690 {
	set more off
	use "${surveysFolder}\survey\JAM\SLC\1990\m11\data_orig\stata\\`t'.dta"
	capture	egen id = concat(serial person)
	destring id, replace
	sort id
	
	di "`t'"
	duplicates report id
	codebook id
	drop if id ==.
	saveold "${surveysFolder}\survey\JAM\SLC\1990\m11\data_orig\stata\\`t'id.dta", replace
		}


* Se debe incluir posteriormente rec00790id en donde se encuentra las razones para no asistir a la escuela, también
* rec00690id rec00890id rec00990id rec01090id rec01190id rec01290id rec01390id rec01490id rec01590id  rec01690id rec01790id rec01890id rec01990id rec02090id rec02190id rec02290id rec02390id rec02490id rec02590id 
*rec00390id rec00490id rec00590id 
*Merge personas
clear all
global path = "${surveysFolder}\survey\JAM"
use "$path\SLC\1990\m11\data_orig\stata\rec00290id.dta"
foreach t in povline90id rec00690id rec00890id rec00990id rec01090id rec01190id rec01290id rec01390id rec01490id rec01590id  rec01690id rec01790id rec01890id rec01990id rec02090id rec02190id rec02290id rec02390id rec02490id rec02590id rec00390id rec00490id rec00590id {
	merge m:m id using "$path\SLC\1990\m11\data_orig\stata\\`t'.dta", force
	di "`t'"
	drop if _merge ==2
	tab _merge
	
	drop _merge
	}
sort serial
saveold "$path\SLC\1990\m11\data_orig\stata\personas.dta", replace
merge m:m serial person using "$path\SLC\1990\m11\data_orig\stata\\rec02690id.dta", force
drop if _merge ==2
drop _merge
merge m:m serial using  "$path\SLC\1990\m11\data_orig\stata\hogares.dta"
tab _merge
drop _merge
saveold "$path\SLC\1990\m11\data_merge\JAM_1990m11.dta", replace



