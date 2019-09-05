
* (Versi�n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Jamaica
Encuesta: JSLC
Round: Mayo 1998
Autores:
Versi�n 2013: Mayra S�enz
�ltima versi�n: Mayra S�enz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha �ltima modificaci�n: 10 de Diciembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/

clear all
use "Y:\survey\JAM\SLC\1998\m5\data_orig\jam98_jslc.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
sort serial

merge m:m serial using  "Y:\survey\JAM\SLC\1998\m5\data_orig\stata\annual.dta"
tab _merge
drop _merge
saveold "Y:\survey\JAM\SLC\1998\m5\data_merge\JAM_1998m5.dta", replace



