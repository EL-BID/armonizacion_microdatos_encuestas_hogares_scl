
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


global in = "${surveysFolder}\survey\BOL\ECH\2012\m11\data_orig\"
global out = "${surveysFolder}\survey\BOL\ECH\2012\m11\data_merge\"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11
Autores:
Versión 2016: Mayra Sáenz
Última versión: Noviembre 4, 2016


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/

use "$in\bolivia_equipamiento_2012.dta", clear

rename s7_16 posee_
rename s7_17 nro_
rename s7_18 anios_ 
rename s7_19 monto_ 
rename d_equip item

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
label var `x'_3 "refrigerador o freezer"
label var `x'_4 "computadora (laptop o tablet pc, etc)"
label var `x'_5 "radio o radiograbador"
label var `x'_6 "minicomponente o equipo de sonido"
label var `x'_7 "televisor"
label var `x'_8 "lavadora de ropa"
label var `x'_9 "motocicleta (para uso del hogar)"
label var `x'_10 "automóvil (para uso del hogar)"
}

duplicates report folio
		  
sort folio
saveold "$in\bolivia_equipamiento_2012_reshape.dta", replace

use "$in\bolivia_gastosalimentarios_2012.dta", clear

rename s7_04   s7a_
rename s7_05   s7b_
rename s7_06a  s7c_
rename s7_07   s7d_
rename s7_08   s7e_
rename s7_09a   s7f_
rename s7_10   s7g_
rename s7_11   s7h_
rename s7_12   s7i_

rename s7_06b   s7j_
rename s7_09b   s7k_


rename s7_04cod item

reshape wide s7a s7b s7c s7d s7e s7f s7g s7h s7i s7j s7k, i(folio factor) j(item)

foreach i of numlist 1/66 {
label var s7a_`i' "¿En el último mes en su hogar compraron, consiguieron o consumieron.."
label var s7b_`i' "¿Con qué frecuencia compra ....?"
label var s7c_`i' "Generalmente, ¿qué cantidad de .... compra ?"
label var s7d_`i' "¿Cuánto gasta por comprar esta cantidad?"
label var s7e_`i' "¿Con qué frecuencia consume .... de lo que ud. produce o vende?"
label var s7f_`i' "Generalmente, ¿qué cantidad de .... consume de lo que usted mismo produce o vende?"
label var s7g_`i' "Si tuviera que comprar esa cantidad de (....) en el mercado, ¿cuánto pagaría?"
label var s7h_`i' "¿El hogar recibió (....) en el último mes como pago en especie, trueque, donación o regalo?"
label var s7i_`i' "¿Cuánto pagaría si tuviera que comprar esa cantidad de .... en el mercado?"
label var s7j_`i' "Unidad de medida de lo que generalmente compra"
label var s7k_`i' "Unidad de medida consume/produce"

}



/*
1	Pan
2	Galletas de agua, saladas, dulces
3	Arroz
4	Maíz en grano
5	Trigo en grano
6	Quinua
7	Fideo
8	Harina de trigo y/o maíz
9	Otros cereales (Avena, hojuelas, etc.)
10	Carne de pollo (entero, trozado)
11	Menudencias de Pollo (patas, cabezas, corazón, mollejas,etc.)
12	Carne de res (molida, blanda, cortes especiales)
13	Carne de res con hueso (con fibras, de segunda, tercera)
14	Carne de cordero
15   Carne de cerdo                                                                                                                                                   
16   Charque, chalona (de cualquier animal) Embutidos (salchicha, chorizo, carnes frías,
18   Menudencias (hígado, corazón, etc.)                                                                      
19   Otras carnes (llama, conejo, jochi, etc.)
20   Pescados frescos (sábalo, pejerrey, blanquillo, etc.)                                                                                                  
21   Sardinas, Atún
22   Otros pescados (secos, en lata, mariscos, etc.)
23   Aceite comestible                                                                                                                                               
24   Margarina, manteca y/o cebo                                                                                                                                 
25   Leche líquida                                                                                                                                                
26   Leche en polvo                                                                                                                                               
27   Queso            
28	Huevos
29	Otros productos lácteos (mantequilla, yogurt, requesón, etc.)
30	Cebollas
31	Tomate
32	Zanahoria
33	Arvejas frescas
34	Habas frescas
35	Choclo
36	Lechuga, acelga
37	Locoto, pimentón, perejil
38	Otras verduras frescas (nabo, espinaca, etc)
39	Papa
40	Chuño (seco, remojado)
41	Yuca
42	Oca
43	Otros tubérculos (papaliza, camote, etc.)
44   Maní, lentejas, porotos
45   Plátano de comer/banano/guineo                                                                                                                     
46   Plátano de cocinar/postre                                                                                                                                    
47   Naranja                                                                                                                                                              
48   Mandarina                                                                                                                                                       
49   Limón                                                                                                                                                                     
50   Papaya                                                                                                                                                                     
51   Manzana                                                                                                                                                                       
52   Otras frutas frescas (piña, lima, pomelo, etc.)
53   Azúcar                                                                                                                                                                   
54   Mermeladas y jaleas
55   Miel de caña y abeja                                                                                            
56   Refrescos en polvo y postres en polvo
57   Otros endulzantes (chancaca, sacarina, etc)
58	Te, café, mate, hierba mate, sultana												
59	Cocoa, Toddy, Chocolike												
60	Hojas de coca				
61   Sal                                                                                                                                                                        
62   Ají en vaina, seco                                                                                                                                               
63   Condimentos y sazonadores (ajinomoto, caldos en cubitos, etc.)
64   Gaseosa en botella
65   Jugos en botella y/o cartón
66   Bebidas alcohólicas (cerveza, etc.)
*/

duplicates report folio
sort folio, stable
save "$in\bolivia_gastosalimentarios_2012_reshape.dta", replace


use "$in\bolivia_gastosnoalimentarios_2012.dta", clear
sort folio, stable
save, replace

use "$in\bolivia_vivienda_2012.dta", clear
sort folio, stable
save, replace

use "$in\bolivia_personas_2012.dta", clear
sort folio nro1a, stable
save, replace

* Merge

use  "$in\bolivia_personas_2012.dta", clear

merge m:1 folio using  "$in\bolivia_vivienda_2012.dta", force
drop _merge

merge m:1 folio using  "$in\bolivia_equipamiento_2012_reshape.dta", force
drop _merge

merge m:1 folio using  "$in\bolivia_gastosnoalimentarios_2012.dta", force
drop _merge

merge m:1 folio using  "$in\bolivia_gastosalimentarios_2012_reshape.dta", force
drop _merge


saveold "$out\BOL_2012m11.dta", replace


*Modificación Cesar Lins - Feb 2021 / saveold didn't work because labels are too long
save "$out\BOL_2012m11.dta", replace
