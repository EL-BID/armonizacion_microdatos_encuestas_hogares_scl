* (Versión Stata 12)
clear
set more off
*______________________________________________________________________________________________________*

	 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la 
	 *posibilidad de utilizar un loop)
	 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
	 * Se tiene acceso al servidor únicamente al interior del BID.
	 * El servidor contiene las bases de datos MECOVI.
	 * Las DHS son encuestas cuya población objetivo son las madres de 15-49 años y sus respectivos hijos.
*______________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS PER
local ENCUESTA DHS
local ANO "2000"
local ronda m7_m11 


local base_out  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"


                        
/*______________________________________________________________________________________________________*
						 BASES DE DATOS DE ENCUESTAS DE SALUD - SOCIOMETRO 
	País: Perú
	Encuesta: DHS
	Round: m3_m12
	Versiones anteriores: Mayra Sáenz   E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
						  Marcela Rubio E-mail: marcelarubio28@gmail.com - mrubio@IADB.ORG
	Última versión: Mayra Sáenz   E-mail: saenzmayra.a@gmail.com - mayras@iadb.org		
	Fecha última modificación: Marzo 16, 2015
									SCL - IADB*/
*______________________________________________________________________________________________________*

*No se puede generar el identificador de niños porque todos los valores de la línea de niño en el hogar
*está como missing.

/*


*Generación de indicador en la base de madres e hijos
use "$ruta\survey\PER\DHS\2000\m7_m11\data_orig\PEBR41FL.dta", clear
sort v001 v002 v003
tostring v001 v002 v003, replace
g z3 = "000"
g z2 = "00"
g z1 = "0"

foreach x of varlist v001 v002 v003 {
g l`x' = length(`x')
}

egen v11 = concat(z1 v001) if lv001 ==3
egen v12 = concat(z2 v001) if lv001 ==2
egen v13 = concat(z3 v001) if lv001 ==1

egen v21 = concat(z2 v002) if lv002 ==2
egen v22 = concat(z1 v002) if lv002 ==3


egen v31 = concat(z1 v003) if lv003 ==1

g v001aux = v001 if lv001 ==4
replace v001aux = v11 if lv001 ==3
replace v001aux = v12 if lv001 ==2
replace v001aux = v13 if lv001 ==1

g v002aux = v002 if lv002 ==4
replace v002aux = v21 if lv002 ==2
replace v002aux = v22 if lv002 ==3

g v003aux = v003 if lv003 ==2
replace v003aux = v31 if lv003 ==1



egen idhogar = concat(v001aux v002aux v003aux)
label var idhogar "Identificador de la madre en el hogar"

sort idhogar
drop z3 z2 z1 lv001 lv002 lv003 v11 v12 v13 v21 v22 v31 v001aux v002aux v003aux
saveold "$ruta\survey\PER\DHS\2000\m7_m11\data_orig\PEBR41FL_MADRE.DTA", replace



*Generación de indicador en la base de hogares
use "$ruta\survey\PER\DHS\2000\m7_m11\data_orig\PEPR41FL.DTA", clear
sort hv001 hv002 hvidx

tostring hv001 hv002 hvidx, replace
g z3 = "000"
g z2 = "00"
g z1 = "0"

foreach x of varlist hv001 hv002 hvidx {
g l`x' = length(`x')
}

sum lhv001 lhv002 lhvidx

egen v11 = concat(z1 hv001) if lhv001 ==3
egen v12 = concat(z2 hv001) if lhv001 ==2
egen v13 = concat(z3 hv001) if lhv001 ==1

egen v21 = concat(z2 hv002) if lhv002 ==2
egen v22 = concat(z1 hv002) if lhv002 ==3

egen v31 = concat(z1 hvidx) if lhvidx ==1

g hv001aux = hv001 if lhv001 ==4
replace hv001aux = v11 if lhv001 ==3
replace hv001aux = v12 if lhv001 ==2
replace hv001aux = v13 if lhv001 ==1

g hv002aux = hv002 if lhv002 ==4
replace hv002aux = v21 if lhv002 ==2
replace hv002aux = v22 if lhv002 ==3

g hvidxaux = hvidx if lhvidx ==2
replace hvidxaux = v31 if lhvidx ==1

egen idhogar = concat(hv001aux hv002aux hvidxaux)

sort idhogar

merge m:m idhogar using "$ruta\survey\PER\DHS\2000\m7_m11\data_orig\PEBR41FL_MADRE.DTA"






