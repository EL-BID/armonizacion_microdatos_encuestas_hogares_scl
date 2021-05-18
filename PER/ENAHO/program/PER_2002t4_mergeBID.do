* Peru
clear
set more off
cd "${surveysFolder}\survey\PER\ENAHO\2002\t4\data_orig\"

*Creacion del archivo principal de la ENAHO 4o trimestre del 2002

* Los modulos 2.-"Ingreso del productor agropecuario", 3.- "Modulo Comunal" y 
*4.- "Ingreso del productor agropecuario, y las desagregaciones de gastos 
* no se incluyen en este archivo debido a que el INEI imputa los resultados de 
* estos modulos a traves del archivo "SUMARIA". Los archivos no incluidos y 
* el resto de los archivos originales se encuentran en esta carpeta.

* No se incluye a los archivos
*1b-2.dta (Gobernabilidad y Democracia)
*1-700 
*1-800b (Part Ciudadana),

*
* La informacion del archivo SUMARIA también se encuentra en el archivo final.

 
******************************
***Merge archivo de hogares***
******************************

use 1-100.dta, clear
merge conglome vivienda hogar using sumaria.dta
tab _merge
drop _merge
sort conglome vivienda hogar 
merge conglome vivienda hogar using 1-800a.dta
tab _merge
drop _merge
sort conglome vivienda hogar
compress
save peru02-iv-hogares_modif.dta,replace

*******************************
***Merge archivo de personas***
*******************************

*Merge archivo de personas
use 1-200.dta,clear
merge conglome vivienda hogar codperso using 1a-300.dta
sort conglome vivienda hogar codperso
tab _merge
drop _merge

merge conglome vivienda hogar codperso using 1a-400.dta
sort conglome vivienda hogar codperso
tab _merge
drop _merge
merge conglome vivienda hogar codperso using 1a-500.dta
sort conglome vivienda hogar codperso
tab _merge
drop _merge
merge conglome vivienda hogar codperso using 1b-1.dta
sort conglome vivienda hogar codperso
tab _merge
drop _merge

sort conglome vivienda hogar codperso
merge conglome vivienda hogar using peru02-iv-hogares_modif.dta
compress
save "${surveysFolder}\survey\PER\ENAHO\2002\t4\data_merge\PER_2002t4.dta", replace

