* (Version Stata 17)

clear all
set more off
capture log close

*________________________________________________________________________________________________________________*

* Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
* utilizar un loop)
* Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
* Se tiene acceso al servidor 򮩣amente al interior del BID.
* El servidor contiene las bases de datos MECOVI.
*________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\PRY\EPHC\2021\t4\data_orig"

local PAIS PRY
local ENCUESTA EPHC
local ANO "2021"
local ronda t4

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"


log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pais: Paraguay
Encuesta: EPHC 
Round: t4 2021
Autores:
Versión: Marta Luzes
Última versión: Agustina Thailinger SCL/EDU
Fecha de última modificación: marzo 2022
							   SCL - IADB
****************************************************************************/

*Convierto las bases descargadas a dta, les hago rename y las sorteo:

/*Vivienda e inventario de bienes duraderos*/
import spss "$ruta\46b95reg01_ephc2021.sav", clear
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\vivienda_ephc2021.dta", replace

/*Ingreso familiar*/
import spss "$ruta\fdb0cingrefam_ephc2021.sav", clear
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\ingrefam_ephc2021.dta", replace

/*Poblcion*/
import spss "$ruta\7c01ereg02_ephc2021.sav", clear
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\poblacion_ephc2021.dta", replace

/*4to trimestre*/
import spss "$ruta\REG02_EPHC_4to Trim 2021.SAV", clear
rename *, lower
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save "$ruta\reg02_ephc_t4_2020.dta", replace

/*Unifico los modulos de interes para el sociometro: vivienda, ingresos y personas*/
 
use "$ruta\reg02_ephc_t4_2020.dta", clear

cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
merge m:1 upm nvivi nhoga using "$ruta\vivienda_ephc2021.dta"
drop _merge
sort upm nvivi nhoga l02

merge m:1 upm nvivi nhoga using "$ruta\ingrefam_ephc2020.dta", force
drop _merge
sort upm nvivi nhoga

merge m:m upm nvivi nhoga l02 using "$ruta\poblacion_ephc2021.dta", force

saveold "`base_out'", v(12) replace

log close
