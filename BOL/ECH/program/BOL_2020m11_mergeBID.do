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

global in = "${surveysFolder}\survey\BOL\ECH\2020\m11\data_orig\"
global out = "${surveysFolder}\survey\BOL\ECH\2020\m11\data_merge\"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11_m12
Autores:
Versión 2016: Mayra Sáenz
Versión 2019: Stephanie González Rubio
Versión 2020: 

Última versión: 


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
/*
* Este módulo no se tiene en 2020

use "$in\EH2020_equipamiento.dta", clear

rename s09c_14 posee_
rename s09c_15 nro_
rename s09c_16 anios_ 
rename s09c_17 monto_ 

* reshape del módulo
reshape wide posee nro anios monto, i(folio factor) j(item)

/*
           1 juego de living 
           2 cocina (a gas, eléctrica, etc.)
           3 refrigerador o freezer
           4 computadora (laptop o tablet pc, etc)
           5 radio o radiograbador
           6 minicomponente o equipo de sonido
           7 televisor
           8 lavadora de ropa
           9 motocicleta (para uso del hogar)
          10 automóvil (para uso del hogar)
*/

label var nro_1 "juego de living"

foreach x in nro anios monto posee {
label var `x'_1 "juego de living"
label var `x'_2 "cocina (a gas, eléctrica, etc.)"
label var `x'_3 "horno microondas/ microondas"
label var `x'_4 "refrigerador o freezer"
label var `x'_5 "computadora (laptop o tablet pc, etc)"
label var `x'_6 "radio o radiograbador"
label var `x'_7 "minicomponente o equipo de sonido"
label var `x'_8 "televisor"
label var `x'_9 "lavadora de ropa"
label var `x'_10 "motocicleta (para uso del hogar)"
label var `x'_11 "automóvil (para uso del hogar)"
}

duplicates report folio
		  
sort folio
save "$in\eh2018_gastos_equipamiento_reshape.dta", replace
*/
use "$in\EH2020_GastosAlimentarios.dta", clear

rename s08a_01  s09a_
rename s08a_02  s09b_
rename s08a_03a s09c_
rename s08a_03b s09d_
rename s08a_04  s09e_

rename producto item

keep folio depto area item s09a_ s09b_ s09c_ s09d_ s09e_ upm estrato factor

reshape wide s09a_ s09b_ s09c_ s09d_ s09e_ , i(folio factor) j(item)
*pasó de 75 items en 2019 a 57 items en 2020
foreach i of numlist 1/57 {
label var s09a_`i' "¿En el último mes en su hogar compraron, consiguieron o consumieron.."
label var s09b_`i' "¿Con qué frecuencia compra ....?"
label var s09c_`i' "Generalmente, ¿qué cantidad de .... compra ?"
label var s09d_`i' "Unidad de medida"
label var s09e_`i' "¿Cuánto gasta por comprar esta cantidad?"

}


duplicates report folio
sort folio
save "$in\EH2020_gastos_alimentarios_reshape.dta", replace

use "$in\EH2020_Persona.dta", clear
sort folio nro
save, replace

use "$in\EH2020_Vivienda.dta", clear
sort folio 
save, replace

	*use "$in\eh2018_gastosnoalimentarios", clear (Este módulo no se tiene para 2020)
	*sort folio 
	*save, replace

* Merge
use "$in\EH2020_Persona.dta", clear

merge m:1 folio using "$in\EH2020_Vivienda.dta", force
drop _merge

	*merge m:1 folio using "$in\eh2018_gastos_equipamiento_reshape.dta", force
	*drop _merge

merge m:1 folio using "$in\EH2020_gastos_alimentarios_reshape.dta", force
drop _merge

	*merge m:1 folio using "$in\eh2018_gastosnoalimentarios", force
	*drop _merge

saveold "$out\BOL_2020m11.dta", replace
