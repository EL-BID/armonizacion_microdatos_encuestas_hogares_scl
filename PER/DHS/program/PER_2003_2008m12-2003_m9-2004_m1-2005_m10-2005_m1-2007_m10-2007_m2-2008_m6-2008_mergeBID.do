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
local ANO "2003_2008"
local ronda m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008


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


*Generación de indicador en la base de madres e hijos
use "$ruta\survey\PER\DHS\2003_2008\m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008\data_orig\Version2\PEBR5ADT.DTA", clear
sort v001 v002 v003
sum v001 v002 v003
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

egen v21 = concat(z2 v002) if lv002 ==1
egen v22 = concat(z1 v002) if lv002 ==2

egen v31 = concat(z1 v003) if lv003 ==1

g v001aux = v001 if lv001 ==4
replace v001aux = v11 if v001aux ==""
replace v001aux = v12 if v001aux ==""
replace v001aux = v13 if v001aux ==""

g v002aux = v002 if lv002 ==3
replace v002aux = v21 if v002aux ==""
replace v002aux = v22 if v002aux ==""

g v003aux = v003 if lv003 ==2
replace v003aux = v31 if v003aux ==""


g lninho = "00" if b16 ==0
replace lninho = "01" if b16 ==1
replace lninho = "02" if b16 ==2
replace lninho = "03" if b16 ==3
replace lninho = "04" if b16 ==4
replace lninho = "05" if b16 ==5
replace lninho = "06" if b16 ==6
replace lninho = "07" if b16 ==7
replace lninho = "08" if b16 ==8
replace lninho = "09" if b16 ==9
replace lninho = "10" if b16 ==10
replace lninho = "11" if b16 ==11
replace lninho = "12" if b16 ==12
replace lninho = "13" if b16 ==13
replace lninho = "14" if b16 ==14
replace lninho = "15" if b16 ==15
replace lninho = "16" if b16 ==16
replace lninho = "17" if b16 ==17
replace lninho = "18" if b16 ==18
replace lninho = "19" if b16 ==19
replace lninho = "20" if b16 ==20
replace lninho = "21" if b16 ==21


/*
egen idhogar1 = concat(v001aux v002aux v003aux)
label var idhogar1 "Identificador de la madre en el hogar"
*/


egen idhogar = concat(v001aux v002aux lninho)
label var idhogar "Identificador del niño en el hogar"

drop z3 z2 z1 lv001 lv002 lv003 v11 v12 v13 v21 v22 v31 v001aux v002aux v003aux lninho

saveold "$ruta\survey\PER\DHS\2003_2008\m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008\data_orig\Version2\PEBR5ADT_MADRE.DTA", replace



*Generación de indicador en la base de hogares
use "$ruta\survey\PER\DHS\2003_2008\m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008\data_orig\Version2\PEPR51FL.dta", clear
sort hv001 hv002 hvidx
sum hv001 hv002 hvidx

tostring hv001 hv002 hvidx, replace
g z3 = "000"
g z2 = "00"
g z1 = "0"

foreach x of varlist hv001 hv002 hvidx {
g l`x' = length(`x')
}

egen v11 = concat(z1 hv001) if lhv001 ==3
egen v12 = concat(z2 hv001) if lhv001 ==2
egen v13 = concat(z3 hv001) if lhv001 ==1

egen v21 = concat(z2 hv002) if lhv002 ==1
egen v22 = concat(z1 hv002) if lhv002 ==2

egen v31 = concat(z1 hvidx) if lhvidx ==1

g hv001aux = hv001 if lhv001 ==4
replace hv001aux = v11 if hv001aux ==""
replace hv001aux = v12 if hv001aux ==""
replace hv001aux = v13 if hv001aux ==""

g hv002aux = hv002 if lhv002 ==3
replace hv002aux = v21 if hv002aux ==""
replace hv002aux = v22 if hv002aux ==""

g hvidxaux = hvidx if lhvidx ==2
replace hvidxaux = v31 if hvidxaux ==""




egen idhogar = concat(hv001aux hv002aux hvidxaux)


*Como es base de niños toca pegar sólo con niños
merge m:m idhogar using "$ruta\survey\PER\DHS\2003_2008\m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008\data_orig\Version2\PEBR5ADT_MADRE.DTA"


g idh = substr(idhogar, 1,7)

g laux1=length(idh)

rename idhogar idhogar1

g idhogar = idh if laux1 ==7

sort idhogar

*saveold "$ruta\survey\PER\DHS\2012\m3_m12\data_orig\PEPR6IFL_HOGAR.DTA", replace

drop _merge idh laux1

saveold "$ruta\survey\PER\DHS\2003_2008\m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008\data_merge\PER_2003_2008m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008.dta", replace

