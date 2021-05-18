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


global in = "${surveysFolder}\survey\BOL\ECH\2018\m11\data_orig\"
global out = "${surveysFolder}\survey\BOL\ECH\2018\m11\data_merge\"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11_m12
Autores:
Versión 2016: Mayra Sáenz
Versión 2019: Stephanie González Rubio (Julio 18th, 2019)
Última modificación: Cesar Lins (2021/03/09)

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/

*El nombre del módulo y el de las variables cambia par
use "$in\eh2018_equipamiento.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

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

use "$in\eh2018_gastosalimentarios.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

rename s09a_01  s09a_
rename s09a_02  s09b_
rename s09a_03a s09c_
rename s09a_03b s09d_
rename s09a_04  s09e_
rename s09a_05  s09f_
rename s09a_06a s09g_
rename s09a_06b s09h_
rename s09a_07  s09i_
rename s09a_08  s09j_
rename s09a_09  s09k_

rename producto item

reshape wide s09a_ s09b_ s09c_ s09d_ s09e_ s09f_ s09g_ s09h_ s09i_ s09j_ s09k_, i(folio factor) j(item)

foreach i of numlist 1/75 {
label var s09a_`i' "¿En el último mes en su hogar compraron, consiguieron o consumieron.."
label var s09b_`i' "¿Con qué frecuencia compra ....?"
label var s09c_`i' "Generalmente, ¿qué cantidad de .... compra ?"
label var s09d_`i' "Unidad de medida"
label var s09e_`i' "¿Cuánto gasta por comprar esta cantidad?"
label var s09f_`i' "¿Con qué frecuencia consume .... de lo que ud. produce o vende?"
label var s09g_`i' "Gral ¿qué cantidad de .... consume de lo que usted mismo produce o vende?"
label var s09h_`i' "Unidad de medida"
label var s09i_`i' "Si tuviera que comprar esa cantidad de (....) en el mercado, ¿cuánto pagaría?"
label var s09j_`i' "¿El hogar recibió ... en el último mes como pago en especie ... o regalo?"
label var s09k_`i' "¿Cuánto pagaría si tuviera que comprar esa cantidad de .... en el mercado?"
}
/*
1 pan corriente
2 pan especial
3 galletas
4 productos de pasteleria (torta, empanad
5 productos de pasteler?a frita
6 arroz
7 maíz
8 quinua
9 fideo
10 harina (trigo, ma?z, etc.)
11 otros cereales (avena, cereales en hoju
12 carne de res sin hueso (cortes especial
13 carne de res con hueso (con fibras, de
14 carne de res molida (corriente/ especia
15 carne de pollo (entero, trozado)
16 carne fresca de cerdo entero o cortes e
17 carne fresca de ganado ovino por piezas
18 carne de llama fresca
19 embutidos (salchicha, chorizo, carnes f
20 menudencias res,cordero, cerdo,pollo (h
21 charque, chalona (de cualquier animal)
22 pescados frescos (s?balo, pejerrey, tru
23 pescados y alimentos marinos en conserv
24 leche l?quida
25 leche en polvo
26 yogurt
27 otros productos lacteos
28 quesos
29 productos l?cteos no de leche de vaca (
30 huevos
31 aceite comestible
32 mantequilla
33 manteca, margarina
34 pl?tano
35 manzana
36 papaya
37 mandarina
38 naranja
39 uva
40 durazno
41 sandia
42 otras frutas, pi?a, lim?n, mango, pera,
43 tomate
44 cebolla
45 zanahoria
46 lechuga
47 choclo
48 otras verduras(zapallo, vainitas, pimen
49 conjunto de verduras picadas/surtido de
50 papa
51 yuca/mandioca
52 tuberculos secos (chu?o, tunta)
53 legumbres secas (frejol/poroto)
54 lenteja
55 man?
56 productos preparados, procesados (chu?o
57 otros productos oleaginosas.(chia, amar
58 az?car granulada
59 mermeladas y jaleas
60 miel de abeja, miel de ca?a
61 chocolates
62 caramelos/dulces, gomas de mascar
63 endulsantes artificiales, variedad de e
64 sal
65 aj? en vaina seco
66 especias, salsas, condimentos, aderezos
67 caf?
68 t?
69 hoja de coca
70 polvos a base de chocolate (toddy, choc
71 hierbas naturales (manzanilla, eucalipt
72 bebida gaseosa en botella/lata
73 jugos de frutas y hortalizas en vaso, j
74 agua natural envasada
75 vino, cerveza, destilados (singani, wis
*/

duplicates report folio
sort folio
save "$in\eh2018_gastos_alimentarios_reshape.dta", replace

use "$in\eh2018_persona.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

sort folio nro
save, replace

use "$in\eh2018_vivienda.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

sort folio 
save, replace

use "$in\eh2018_gastosnoalimentarios", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

sort folio 
save, replace

* Merge

use "$in\eh2018_persona.dta", clear

merge m:1 folio using "$in\eh2018_vivienda.dta", force
drop _merge

merge m:1 folio using "$in\eh2018_gastos_equipamiento_reshape.dta", force
drop _merge

merge m:1 folio using "$in\eh2018_gastos_alimentarios_reshape.dta", force
drop _merge

merge m:1 folio using "$in\eh2018_gastosnoalimentarios", force
drop _merge

*Modificación Cesar Lins - Feb 2021 / saveold didn't work because labels are too long
save "$out\BOL_2018m11.dta", replace
