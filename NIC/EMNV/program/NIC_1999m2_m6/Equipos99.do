

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
 


global ruta = "${surveysFolder}"

local PAIS NIC
local ENCUESTA EMNV
local ANO "1999"
local ronda m2_m6

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: EMNV
Round: Febrero-Junio
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear



****************
****refrig_ch***
****************
gen refrig_chaux=1 if (s9ep00 ==4 & s9ep1 == 1)
replace refrig_chaux=0 if (s9ep00 ==4 & s9ep1 == 2)

****************
****auto_ch****
****************
gen auto_chaux=.
replace auto_chaux=1 if (s9ep00 ==22 & s9ep1 == 1)
replace auto_chaux=0 if (s9ep00 ==22 & s9ep1 == 2)

****************
****compu_ch****
****************

gen compu_chaux=1 if (s9ep00 ==21 & s9ep1 == 1)
replace compu_chaux=0 if (s9ep00 ==21 & s9ep1 == 2)

****************************************************************************************************
/*CREAMOS EL ID_HOGAR DE LA MISMA FORMA QUE ESTA CREADO EN LAS BANANAS DEL '98 (Solo para equipos)*/
sort i00

gen first=0
quietly by i00: replace first=1 if _n==1

gen vectoru=1
gen double idy=sum(vectoru) if first==1
recode idy .=0

capture drop id_hogar
egen double id_hogar=sum(idy), by (i00)

drop first vectoru idy
sort id_hogar
*****************************************************************************************************

by id_hogar: egen refrig_ch=max(refrig_chaux)
by id_hogar: egen auto_ch=max(auto_chaux)
by id_hogar: egen compu_ch=max(compu_chaux)
sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
save "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\Equipos99.dta", replace
