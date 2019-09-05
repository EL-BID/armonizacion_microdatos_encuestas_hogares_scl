
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

clear all
global path = "D:\BID\JAM\Jam SLC"
use "$path\SLC\2005\m5\data_merge\JAM_2005m5.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
sort serial

merge m:m serial using  "$path\SLC\2005\m5\data_orig\STATA anual con agregados de Consumo\annual05.dta"
tab _merge
drop _merge
ds, has(type string)
    foreach var of varlist `r(varlist)' {
        replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"N")
        destring `var', replace
    }



saveold "Y:\survey\JAM\SLC\2005\m5\data_merge\JAM_2005m5.dta", replace



