

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
Round: Mayo 2001
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
*global path = "Y:\survey\JAM"

foreach file in annual annual_01 hhsize hhsizes homegift housexp rec001 rec002 rec002_01 rec003 rec003_01 rec004 rec004_01 rec005 rec005_01 rec006 rec006_01 rec006_1 rec007 rec007_01 rec008 rec008_01 rec009 rec009_01 rec009_1 rec010 rec010_01 rec011 rec011_01 rec012 rec012_01 rec013 rec013_01 rec014 rec014_01 rec015 rec015_01 rec016 rec016_01 rec017 rec017_01 rec018 rec018_01 rec019 rec019_01 rec020 rec020_01 rec021 rec021_01 rec022 rec022_01 rec023 rec023_01 rec024 rec024_01 rec025 rec025_01 rec026 rec026_01 rec027 rec027_01 rec027_1 rec028 rec028_01 rec029 rec029_01 rec030 rec030_01 rec031 rec031_01 rec032 rec032_01 rec033 rec033_01 rec034 rec034_01 rec035 rec035_01 thomfood thousexp tnoncons tnotfood totfood totgifts totmeals  {
	usespss using "$path\SLC\2001\m5\data_orig\spss\\`file'.sav"
	*usespss using "Y:\survey\JAM\SLC\2001\m5\data_orig\spss\\`file'.sav"
	foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
saveold "$path\SLC\2001\m5\data_orig\stata\\`file'.dta", replace
*saveold "Y:\survey\JAM\SLC\2001\m5\data_orig\stata\\`file'.dta", replace
clear all
set more off
}
                                      	

* serial

*housing meals noncons notfood povline2001 povline

*rec004 rec004_01  rec005_01 rec006 rec006_01 rec006_1 rec007 rec007_01 rec008 rec008_01 rec009 rec009_01 rec009_1 rec010 rec010_01 rec011 rec011_01 rec012 rec012_01 rec013 rec013_01 rec014 rec014_01 rec015 rec015_01 rec016 rec016_01 rec017 rec017_01 rec018 rec018_01 rec019 rec019_01 rec020 rec020_01 rec021 rec021_01 rec022 rec022_01 rec023 rec023_01 rec024 rec024_01 rec025 rec025_01 rec026 rec026_01 rec027 rec027_01 rec027_1 rec028 rec028_01 rec029 rec029_01 rec030 rec030_01 rec031 rec031_01 rec032 rec032_01 rec033 rec033_01 rec034 rec034_01 rec035 rec035_01 thomfood thousexp tnoncons tnotfood totfood totgifts totmeals  
*hogares
*annual rec001 rec034
*Personas
*rec002 rec003 rec005 rec006 rec007 rec008 rec009 rec010 rec011 rec012 rec025 rec029 rec030 rec032 rec035

*Hogares
*Se excluyen estas bases, además se debe analizar si requieren un reshape. No se utilizan estos datos para el cálculo.
*food gifts homegift meals noncons rec021  rec023 rec024 rec026 rec027 rec031 rec032
*rec033
* Análisis de la variable que servirá como identificador: serial
clear all
global path = "D:\BID\JAM\Jam SLC"
foreach t in annual rec001 rec034{
	set more off
	use "$path\SLC\2001\m5\data_orig\stata\\`t'.dta"
	di "`t'"
	duplicates report serial
		}




*Merge hogares
* Las que tienen sólo serial
clear all
global path = "D:\BID\JAM\Jam SLC"
use "$path\SLC\2001\m5\data_orig\stata\annual.dta"
foreach t in rec001 rec034 {
	merge m:m serial  using "$path\SLC\2001\m5\data_orig\stata\\`t'.dta", force
	di "`t'"
	tab _merge
	drop _merge
	}
sort serial
saveold "$path\SLC\2001\m5\data_orig\stata\hogares.dta", replace



*serial e ind


*Creación de los identificadores de personas
clear all
global path = "D:\BID\JAM\Jam SLC"
foreach t in rec002 rec003 rec005 rec006 rec007 rec008 rec009 rec010 rec011 rec012 rec025 rec029 rec030 rec032 rec035 {
	set more off
	use "$path\SLC\2001\m5\data_orig\stata\\`t'.dta"
	tostring ind, replace
    gen identif = "0"+ind if length(ind)==1
    replace identif = ind if length(ind)==2
	capture	egen id = concat(serial identif)
	destring id, replace
	sort id
	
	di "`t'"
	duplicates report id
	codebook id
	saveold "$path\SLC\2001\m5\data_orig\stata\\`t'id.dta", replace
		}




*Merge personas
clear all
global path = "D:\BID\JAM\Jam SLC"
use "$path\SLC\2001\m5\data_orig\stata\rec002id.dta"
drop if id ==.

foreach t in rec002id rec003id rec005id rec006id rec007id rec008id rec009id rec010id rec011id rec012id rec025id rec029id rec030id rec032id rec035id  {
	merge m:m id using "$path\SLC\2001\m5\data_orig\stata\\`t'.dta", force
	di "`t'"
	drop if id ==.
	drop if _merge ==2
	tab _merge
	
	drop _merge
	}
sort serial
saveold "$path\SLC\2001\m5\data_orig\stata\personas.dta", replace

merge m:m serial using  "$path\SLC\2001\m5\data_orig\stata\hogares.dta"
drop if _merge ==2
tab _merge
drop _merge
ds, has(type string)
    foreach var of varlist `r(varlist)' {
        replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"N")
        destring `var', replace
    }

saveold "$path\SLC\2001\m5\data_merge\JAM_2001m5.dta", replace



