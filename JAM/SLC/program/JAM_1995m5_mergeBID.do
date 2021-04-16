
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
Round: Mayo 1995
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
foreach file in annual95 edtotals95 eld95 food95 gifts95 heads95 hheads95 hhsize95 hhsizes95 homegift95 housexp95 housing95 meals95 noncons95 povline95 rec00195 rec00295 rec00395 rec00495 rec00595 rec00695 rec00795 rec00895 rec00995 rec01095 rec01195 rec01295 rec01395 rec01495 rec01595 rec01695 rec01795 rec01895 rec01995 rec02095 rec02195 rec02295 rec02395 rec02495 rec02595 rec02695 rec02795 rec02895 rec02995 rec03095 rec03195 rec03295 thomfood95 thousexp95 tnoncons95 tnotfood95 totfood95 totgifts95 totmeals95 {
	usespss using "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\spss\\`file'.sav"
	foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
saveold "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\\`file'.dta", replace
clear all
set more off
}


                                      	

*serial


*Hogares
*Se excluyen estas bases, además se debe analizar si requieren un reshape. No se utilizan estos datos para el cálculo.
*noncons95 notfood95 rec01095 rec01295 rec01395 rec01595 rec01695 rec02395 rec02495 rec02695 rec02895 rec02995
* Análisis de la variable que servirá como identificador: serial
clear all
foreach t in annual95 hhsize95 housing95 rec00195 rec00995 rec01795 rec02095 rec02195 rec02295 rec02595 rec02795 rec03095 thomfood95 thousexp95 tnoncons95 tnotfood95 totfood95 totgifts95 totmeals95 {
	set more off
	use "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\\`t'.dta"
	di "`t'"
	duplicates report serial
		}




*Merge hogares
* Las que tienen sólo serial
*requieren reshape para próximos merge rec00995 rec01795 rec02095 rec02295 rec02595 rec03095
clear all
use "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\annual95.dta"
foreach t in hhsize95 housing95 rec00195    rec02195  rec02795  thomfood95 thousexp95 tnoncons95 tnotfood95  totfood95 totgifts95 totmeals95 {
	merge m:m serial using "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\\`t'.dta"
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\hogares.dta", replace



*serial e ind

*Merge personas
clear all
use "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\heads95.dta"
rename indiv ind
foreach t in povline95 rec00295 rec00395 rec00495 rec00595 rec00695 rec00795 rec00895 rec01195 {
	merge m:m serial ind using "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\\`t'.dta"
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\personas.dta", replace

merge m:m serial using  "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_orig\stata\hogares.dta"
tab _merge
drop _merge
saveold "${surveysFolder}\BID\JAM\Jam SLC\SLC\1995\m5\data_merge\JAM_1995m5.dta", replace



