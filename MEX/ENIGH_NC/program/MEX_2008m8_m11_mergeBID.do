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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENIGH\2008\m8_m11\nueva_construccion\data_orig\STATA"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2008"
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

	use "$ruta\trabajos.dta",clear
	keep folioviv foliohog numren  trapais numtrab subor indep personal pago contrato tipocontr htrab otro_trab otra_act cmo scian clas_emp tam_emp p49 tiene_suel pres_*
	egen per = concat(folioviv foliohog numren)
	rename pres_1 pres_0
	tostring numtrab, replace
	reshape wide trapais subor indep personal pago contrato tipocontr htrab otro_trab otra_act cmo scian clas_emp tam_emp p49 tiene_suel pres_*, i(per) j(numtrab) string
	rename pres_01 pres_11
	rename pres_02 pres_12
	
	
	foreach var of varlist trapais1 subor1 indep1 personal1 pago1 contrato1 tipocontr1 htrab1 otro_trab1 otra_act1 cmo1 scian1 clas_emp1 tam_emp1 p491 tiene_suel1 ///
	pres_11 pres_21 pres_31 pres_41 pres_51 pres_61 pres_71 pres_81 pres_91 pres_101 pres_111 pres_121 pres_131 pres_141 pres_151 pres_161 pres_171 pres_181 pres_191 pres_201 {
	    label var `var' "`var' del primer trabajo"
	}
	foreach var of varlist trapais2 subor2 indep2 personal2 pago2 contrato2 tipocontr2 htrab2 otro_trab2 otra_act2 cmo2 scian2 clas_emp2 tam_emp2 p492 tiene_suel2 ///
	pres_12 pres_22 pres_32 pres_42 pres_52 pres_62 pres_72 pres_82 pres_92 pres_102 pres_112 pres_122 pres_132 pres_142 pres_152 pres_162 pres_172 pres_182 pres_192 pres_202 {
	    label var `var' "`var' del segundo trabajo"
	}
		
	drop per
	sort  folioviv foliohog numren
	save "$ruta\trabajos_reshape.dta", replace

*nivel de personas, la clave indica el tipo de ingresos que tiene una persona)
*la persona uno tiene tres tipos de ingresos, hacer un reshape

use "$ruta\ingresos.dta",clear
	sort  folioviv foliohog numren
	egen per = concat(folioviv foliohog numren)
	egen ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)
	drop  mes_1 mes_2 mes_3 mes_4 mes_5 mes_6 ing_tri ing_1 ing_2 ing_3 ing_4 ing_5 ing_6
	rename ing_mens ing_1
	reshape wide ing_1, i(per) j(clave)string
	drop per
	sort  folioviv foliohog  numren
	save "$ruta\ingresos_reshape.dta", replace
	
	
/*
merge folio using ubicacion.dta		// BASE DE DATOS NO DISPONIBLE EN LA PAG. DEL INEGI
	tab _merge			// CONTIENE LAS VARIABLES AGEB, UPM, Y OTRAS
	drop if _merge==2  		// RELACIONADAS A LA UBICACION DE LA VIVIENDA
	sort folio num_ren
*/


use "$ruta\gastos.dta",clear
	sort  folioviv foliohog 
	keep if clave == "G003"
	keep folioviv foliohog clave gas_tri
	egen per = concat(folioviv foliohog)
	reshape wide gas_tri, i(per) j(clave)string
	drop per
		sort  folioviv foliohog  
	save "$ruta\gastos_reshape.dta", replace

// MERGE

use "$ruta\poblacio.dta",clear
	sort folioviv foliohog
	
merge m:m folioviv foliohog using "$ruta\hogares.dta"
	tab _merge
	drop _merge
	sort folioviv foliohog

merge m:m folioviv foliohog using "$ruta\concen.dta"
	tab _merge
	drop _merge
	sort folioviv foliohog
	
merge m:m folioviv foliohog using "$ruta\trabajos_reshape.dta"
	tab _merge
	drop _merge
	sort folioviv foliohog numren
	
merge m:m folioviv foliohog using "$ruta\ingresos_reshape.dta"
	tab _merge
	drop _merge
	sort folioviv foliohog numren

merge m:m folioviv foliohog using "$ruta\gastos_reshape.dta"
	tab _merge
	drop _merge
	sort folioviv foliohog 


destring, replace

	
save "`base_out'", replace


log close



/* Bases de datos no incluidas
	1. Ingresos.dta
	2. Nomon.dta
	3. Gastos.dta
	4. Eroga.dta */
	
* Nota sobre la actualización de la serie 2000 - 2005 *	

/*

El Instituto Nacional de Estadística, Geografía e Informática (INEGI), en 
un esfuerzo por fortalecer el servicio público de información estadística 
presenta las Bases de Datos de la Encuesta Nacional de Ingresos y Gastos 
de los Hogares para 2000, 2002, 2004, y 2005, Armonizadas de acuerdo con 
la Conciliación Demográfica, con la intención de dar respuesta a los 
requerimientos de aquellos usuarios especializados, con un interés particular 
en el análisis de los microdatos, que permiten un conocimiento más detallado 
del monto, la estructura y la distribución de los ingresos de los hogares y 
del destino de los gastos del hogar en bienes de consumo duradero y no 
duradero. También se obtiene información sobre la infraestructura de las 
viviendas, la composición familiar de los hogares, así como de la actividad 
económica de cada uno de sus miembros. 

Esta encuesta proporciona información a nivel nacional tanto para el conjunto 
de localidades de 2 500 y más habitantes, como para el de aquellas con menos 
de 2 500 habitantes. 

Las cifras que se incluyen para el periodo 2000-2005 han sido sometidas a un 
proceso de armonización acorde con las cifras de la Conciliación Demográfica 
realizada conjuntamente por el Consejo Nacional de Población, El Colegio de 
México y el INEGI. 

A la par del ejercicio de armonización, se llevó a cabo una revisión de la 
información captada por las cuatro últimas ENIGH, con el propósito de uniformarlas. 
Este ejercicio permitió homologar las bases de datos en conjunto, y no una a una 
de manera aislada. En particular, en la ENIGH 2004 se corrigieron pequeños errores 
en la construcción de algunas variables. 

Es importante mencionar que con las nuevas bases de datos 2000-2005 de las ENIGH sí 
es posible analizar la evolución de los ingresos y de los gastos de los hogares 
mexicanos en el periodo. 

Las bases de datos están conformadas por 10 archivos: siete de la base de datos (DBF), 
tres de catálogos (pdf), incluyendo uno con la estructura de la base de datos (pdf). 
La información detallada puede consultarse en la descripción de la base de datos (pdf). 
Para bajar las bases de datos de la ENIGH, sólo tiene que descargarlas en un directorio 
de su equipo y desempacarlo. Para el manejo de las bases de datos, y considerando el 
volumen de información, se recomienda utilizar software como Fox o Access. 

Por medio de esta encuesta y de la difusión de sus resultados, tanto en productos impresos 
y electrónicos, como en bases de datos, el INEGI consolida su compromiso con la población 
de brindar información que contribuya al conocimiento de la realidad sociodemográfica y 
económica del país. 

Conviene señalar que estas bases de datos, en formato de disco compacto, estarán a su 
disposición en los próximos días en los Centros de Información del INEGI 
 

Fuente:
http://www.inegi.gob.mx/est/contenidos/espanol/sistemas/enigh/bd/default.asp
Consultado: Abril, 2007

*/
