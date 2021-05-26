
* (Versión Stata 12)
clear
set more off


* Marcela G. Rubio 
* Última versión: 2021/03/09 (Cesar Lins)


global in = "${surveysFolder}\survey\BOL\ECH\2013\m11\data_original\"
global out = "${surveysFolder}\survey\BOL\ECH\2013\m11\data_merge\"

*Modificación Mayra Sáenz - Noviembre 2016, se incluye el módulo de equipamiento y de gastos alimentarios

use "$in\bolivia_equipamiento_2013.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

rename s8_18 posee_
rename s8_19 nro_
rename s8_20 anios_ 
rename s8_21 monto_ 
rename s8_18cod item

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
saveold "$in\bolivia_equipamiento_2013_reshape.dta", replace

use "$in\bolivia_gastosalimentarios_2013.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

rename s8_06   s8a_
rename s8_07   s8b_
rename s8_08a  s8c_
rename s8_09   s8d_
rename s8_10   s8e_
rename s8_11a  s8f_
rename s8_12   s8g_
rename s8_13   s8h_
rename s8_14   s8i_

rename s8_08b   s8j_
rename s8_11b   s8k_


rename s8_06cod item

reshape wide s8a s8b s8c s8d s8e s8f s8g s8h s8i s8j s8k, i(folio factor) j(item)

foreach i of numlist 1/66 {
label var s8a_`i' "¿En el último mes en su hogar compraron, consiguieron o consumieron.."
label var s8b_`i' "¿Con qué frecuencia compra ....?"
label var s8c_`i' "Generalmente, ¿qué cantidad de .... compra ?"
label var s8d_`i' "¿Cuánto gasta por comprar esta cantidad?"
label var s8e_`i' "¿Con qué frecuencia consume .... de lo que ud. produce o vende?"
label var s8f_`i' "Generalmente, ¿qué cantidad de .... consume de lo que usted mismo produce o vende?"
label var s8g_`i' "Si tuviera que comprar esa cantidad de (....) en el mercado, ¿cuánto pagaría?"
label var s8h_`i' "¿El hogar recibió (....) en el último mes como pago en especie, trueque, donación o regalo?"
label var s8i_`i' "¿Cuánto pagaría si tuviera que comprar esa cantidad de .... en el mercado?"
label var s8j_`i' "Unidad de medida de lo que generalmente compra"
label var s8k_`i' "Unidad de medida consume/produce"

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
sort folio
save "$in\bolivia_gastosalimentarios_2013_reshape.dta", replace


use "$in\bolivia_gastosnoalimentarios_2013.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

sort folio
save, replace

use "$in\bolivia_vivienda_2013.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

sort folio
save, replace

use "$in\bolivia_personas_2013.dta", clear

*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

sort folio nro2a
save, replace

* Merge

use  "$in\bolivia_personas_2013.dta", clear
*Modificación Cesar Lins - Feb 2021, data was updated by INE and some variable names changed to UPPERCASE
rename *, lower

merge m:1 folio using  "$in\bolivia_vivienda_2013.dta", force
drop _merge

merge m:1 folio using  "$in\bolivia_equipamiento_2013_reshape.dta", force
drop _merge

merge m:1 folio using  "$in\bolivia_gastosnoalimentarios_2013.dta", force
drop _merge

merge m:1 folio using  "$in\bolivia_gastosalimentarios_2013_reshape.dta", force
drop _merge


*Modificación Cesar Lins - Feb 2021 / saveold didn't work because labels are too long
save "$out\BOL_2013m11.dta", replace


