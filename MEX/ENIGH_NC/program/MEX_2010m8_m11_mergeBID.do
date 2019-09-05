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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENIGH\2010\m8_m11\nueva_construccion\data_orig\STATA"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2010"
local ronda m8_m11

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\nueva_construccion\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\nueva_construccion\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (Nueva construcción)
Round: Agosto-Noviembre
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/


// MERGE

*nivel de personas, normal
use Poblacion.dta,clear
	sort  folioviv foliohog numren
	save "poblacion.dta", replace
	
*nivel de hogar 
use Hogares.dta,clear
	sort  folioviv foliohog 
	save "hogares.dta", replace

	
*nivel de personas, (hacer reshape )
use Trabajos.dta,clear
	keep folioviv foliohog numren numtrab  trapais subor indep personal pago contrato tipocontr htrab cuo scian clas_emp tam_emp no_ing tiene_suel pres_*
	egen per = concat(folioviv foliohog numren)
	rename pres_1 pres_0
	reshape wide trapais subor indep personal pago contrato tipocontr htrab cuo  scian clas_emp tam_emp no_ing tiene_suel pres_*, i(per) j(numtrab) string
	rename pres_01 pres_11
	rename pres_02 pres_12
	
	
	foreach var of varlist trapais1 subor1 indep1 personal1 pago1 contrato1 tipocontr1 htrab1 cuo1 scian1 clas_emp1 tam_emp1 no_ing1 tiene_suel1 ///
	pres_11 pres_21 pres_31 pres_41 pres_51 pres_61 pres_71 pres_81 pres_91 pres_101 pres_111 pres_121 pres_131 pres_141 pres_151 pres_161 pres_171 pres_181 pres_191 pres_201 {
	    label var `var' "`var' del primer trabajo"
	}
	foreach var of varlist trapais2 subor2 indep2 personal2 pago2 contrato2 tipocontr2 htrab2 cuo2 scian2 clas_emp2 tam_emp2 no_ing2 tiene_suel2 ///
	pres_12 pres_22 pres_32 pres_42 pres_52 pres_62 pres_72 pres_82 pres_92 pres_102 pres_112 pres_122 pres_132 pres_142 pres_152 pres_162 pres_172 pres_182 pres_192 pres_202 {
	    label var `var' "`var' del segundo trabajo"
	}
		
	drop per
	sort  folioviv foliohog numren
	save "trabajos_reshape.dta", replace

*nivel de personas, la clave indica el tipo de ingresos que tiene una persona)
*la persona uno tiene tres tipos de ingresos, hacer un reshape

use Ingresos.dta,clear
	sort  folioviv foliohog numren
	egen per = concat(folioviv foliohog numren)
	egen ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)
	drop  mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 ing_tri ing_1 ing_2 ing_3 ing_4 ing_5 ing_6
	rename ing_mens ing_1
	reshape wide ing_1, i(per) j(clave)string
	drop per
	sort  folioviv foliohog  numren
	save "ingresos_reshape.dta", replace
	
//MERGE
clear
use poblacion.dta
merge folioviv foliohog using hogares.dta
	tab _merge
		drop _merge
	sort  folioviv foliohog numren

merge  folioviv foliohog numren using trabajos_reshape.dta
	tab _merge
	*Faltan observaciones en using data porque la base trabajos solo contiene a las personas que tienen trabajo no a todas (no todos los miembros del hogar)
	drop _merge
	sort  folioviv foliohog numren
	
merge  folioviv foliohog numren using ingresos_reshape.dta
	tab _merge
	*Faltan observaciones en using data porque la base ingresos solo contiene a las personas que tienen un ingreso no a todas
	drop _merge
	sort  folioviv foliohog numren	
	
	
	
save "`base_out'", replace


log close



