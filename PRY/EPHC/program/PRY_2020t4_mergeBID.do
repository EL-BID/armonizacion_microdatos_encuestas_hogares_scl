* (Versi󮠓tata 12)
clear
set more off

*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor 򮩣amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\PRY\EPHC\2020\t4\data_orig"

local PAIS PRY
local ENCUESTA EPHC
local ANO "2019"
local ronda t4

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"


log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa Paraguay
Encuesta: EPHC 
Round: Octubre-Diciembre 2020
Autores:
Versión: Marta Luzes
Última versión: Alvaro Altamirano o Daniela Zuluaga - Email: danielazut@iadb.org
Fecha de última modificación - Junio 2021
							SCL/SCL - IADB
****************************************************************************/

/*Nota ML:Para el año del 2020 t4 solo hay disponible en la pagina web del INE
una sección de la encuesta, la sección 6. EMPLEO E INGRESO LABORAL. El módulo vivienda y población 
se encuentra en este link :
https://www.ine.gov.py/datos/encuestas/eph/
*/


*Hago rename de las bases descargadas y luego las sorteo

forvalues i = 1/2 {
importsav "$ruta\reg0`i'_ephc2020.sav"
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\reg0`i'_ephc2020.dta", replace
}

/*Importo la base de Ingreso familiar*/
importsav "$ruta\ingrefam_ephc2020.sav"
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\ingrefam_ephc2020.dta", replace


/*Importo el 4 Trimestre*/

importsav "$ruta\REG02_EPHC_T4-2020.SAV"
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\reg02_ephc_t4_2020.dta", replace


/*Unifico los modulos de inter고para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\reg02_ephc_t4_2020.dta", clear

cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
merge m:1 upm nvivi nhoga using "$ruta\reg01_ephc2020.dta"
drop _merge
sort upm nvivi nhoga l02

merge m:1 upm nvivi nhoga using "$ruta\ingrefam_ephc2020.dta", force
drop _merge
sort upm nvivi nhoga


*Merge con variables educ que no estaban dentro de base de mercado laboral, descargadas de: 
*https://www.ine.gov.py/datos/encuestas/eph/
merge m:m upm nvivi nhoga l02 using "$ruta\reg02_ephc2020.dta", force

drop _merge
sort upm nvivi nhoga

saveold "`base_out'", v(12) replace

