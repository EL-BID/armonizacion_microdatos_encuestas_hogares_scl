

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
Round: Noviembre 1994
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
foreach file in annual94 heads94 hhsize94 housing94 noncons94 notfood94 povline94 rec00194 rec00294 rec00394 rec00494 rec00594 rec00694 rec00794 rec00894 rec00994 rec01094 rec01194 rec01294 rec01394 rec01494 rec01594 rec01694 rec01794 rec01894 rec01994 rec02094 rec02194 rec02294 rec02394 rec02494 rec02594 rec02694 rec02794 rec02894 rec02994 rec03094 rec03194 thomfood94 thousexp94 tnoncons94 tnotfood94  toilets94 totfood94 totgifts94 totmeals94	{
	usespss using "Y:\survey\JAM\SLC\1994\m11\data_orig\spss\\`file'.sav"
	foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
saveold "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\\`file'.dta", replace
clear all
set more off
}


                                      	

*serial


*Hogares
*Se excluyen estas bases, además se debe analizar si requieren un reshape. No se utilizan estos datos para el cálculo.
*noncons94 notfood94 rec01094 rec01294 rec01394 rec01594 rec01694 rec02394 rec02494 rec02694 rec02894 rec02994
* Análisis de la variable que servirá como identificador: serial
clear all
foreach t in annual94 hhsize94 housing94 rec00194 rec00994 rec01794 rec02094 rec02194 rec02294 rec02594 rec02794 rec03094 thomfood94 thousexp94 tnoncons94 tnotfood94  toilets94 totfood94 totgifts94 totmeals94 {
	set more off
	use "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\\`t'.dta"
	di "`t'"
	duplicates report serial
		}




*Merge hogares
* Las que tienen sólo serial
clear all
use "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\annual94.dta"
foreach t in hhsize94 housing94 rec00194 rec00994 rec01794 rec02094 rec02194 rec02294 rec02594 rec02794 rec03094 thomfood94 thousexp94 tnoncons94 tnotfood94  toilets94 totfood94 totgifts94 totmeals94 {
	merge m:m serial using "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\\`t'.dta"
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\hogares.dta", replace



*serial e ind

*Merge personas
clear all
use "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\heads94.dta"
foreach t in povline94 rec00294 rec00394 rec00494 rec00594 rec00694 rec00794 rec00894 rec01194 rec01494 rec01894 rec01994 rec03194 {
	merge m:m serial ind using "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\\`t'.dta"
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\personas.dta", replace

merge m:m serial using  "Y:\survey\JAM\SLC\1994\m11\data_orig\stata\hogares.dta"
tab _merge
drop _merge
saveold "Y:\survey\JAM\SLC\1994\m11\data_merge\JAM_1994m11.dta", replace



