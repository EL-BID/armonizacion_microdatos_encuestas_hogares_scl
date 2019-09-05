
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
País: Paraguay
Encuesta: EPH
Round: Octubre-Diciembre
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 4 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
clear

* MERGE PARAGUAY 2007

* Households
/*
use "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r01_07.dta", clear
sort upm nvivi nhoga
save "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r01_07.dta", replace

use "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\ingrefam_2_04_08.dta", clear
sort upm nvivi nhoga
save "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\ingrefam_2_04_08.dta", replace

* Individuals

use "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r02_07.dta", clear
sort upm nvivi nhoga l02 
save "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r02_07.dta", replace

use "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r03_07.dta", clear
sort upm nvivi nhoga l02 
save "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r03_07.dta", replace

use "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\ingresos_individuales_2007.dta", clear
sort upm nvivi nhoga l02 
save "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\ingresos_individuales_2007.dta", replace

use "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r02_07.dta", clear

merge upm nvivi nhoga using "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r03_07.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 


merge upm nvivi nhoga using "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\ingresos_individuales_2007.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\ingrefam_2_04_08.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 
*/

use Y:\survey\PRY\EPH\2007\m10_m12\data_merge\pry07.dta 
merge upm nvivi nhoga using "Y:\survey\PRY\EPH\2007\m10_m12\data_orig\Stata\r03_07.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02 

save "Y:\survey\PRY\EPH\2007\m10_m12\data_merge\PRY_2007m10_m12.dta",replace



