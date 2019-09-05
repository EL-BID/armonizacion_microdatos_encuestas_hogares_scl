
* (Versi�n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*


global in = "\\Sdssrv03\surveys\survey\BOL\ECH\2012\m11\data_orig\"
global out = "\\Sdssrv03\surveys\survey\BOL\ECH\2012\m11\data_merge\"



/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Bolivia
Encuesta: ECH
Round: m11
Autores:
Versi�n 2016: Mayra S�enz
�ltima versi�n: Noviembre 4, 2016


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

* reshape del m�dulo
reshape wide posee nro anios monto, i(folio factor) j(item)

/*
           1 juego de living 
           2 cocina (a gas, el�ctrica, etc.)
           3 refrigerador o freezer
           4 computadora (laptop o tablet pc, etc)
           5 radio o radiograbador
           6 minicomponente o equipo de sonido
           7 televisor
           8 lavadora de ropa
           9 motocicleta (para uso del hogar)
          10 autom�vil (para uso del hogar)
*/

label var nro_1 "juego de living"

foreach x in nro anios monto posee {
label var `x'_1 "juego de living"
label var `x'_2 "cocina (a gas, el�ctrica, etc.)"
label var `x'_3 "refrigerador o freezer"
label var `x'_4 "computadora (laptop o tablet pc, etc)"
label var `x'_5 "radio o radiograbador"
label var `x'_6 "minicomponente o equipo de sonido"
label var `x'_7 "televisor"
label var `x'_8 "lavadora de ropa"
label var `x'_9 "motocicleta (para uso del hogar)"
label var `x'_10 "autom�vil (para uso del hogar)"
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
label var s7a_`i' "�En el �ltimo mes en su hogar compraron, consiguieron o consumieron.."
label var s7b_`i' "�Con qu� frecuencia compra ....?"
label var s7c_`i' "Generalmente, �qu� cantidad de .... compra ?"
label var s7d_`i' "�Cu�nto gasta por comprar esta cantidad?"
label var s7e_`i' "�Con qu� frecuencia consume .... de lo que ud. produce o vende?"
label var s7f_`i' "Generalmente, �qu� cantidad de .... consume de lo que usted mismo produce o vende?"
label var s7g_`i' "Si tuviera que comprar esa cantidad de (....) en el mercado, �cu�nto pagar�a?"
label var s7h_`i' "�El hogar recibi� (....) en el �ltimo mes como pago en especie, trueque, donaci�n o regalo?"
label var s7i_`i' "�Cu�nto pagar�a si tuviera que comprar esa cantidad de .... en el mercado?"
label var s7j_`i' "Unidad de medida de lo que generalmente compra"
label var s7k_`i' "Unidad de medida consume/produce"

}



/*
1	Pan
2	Galletas de agua, saladas, dulces
3	Arroz
4	Ma�z en grano
5	Trigo en grano
6	Quinua
7	Fideo
8	Harina de trigo y/o ma�z
9	Otros cereales (Avena, hojuelas, etc.)
10	Carne de pollo (entero, trozado)
11	Menudencias de Pollo (patas, cabezas, coraz�n, mollejas,etc.)
12	Carne de res (molida, blanda, cortes especiales)
13	Carne de res con hueso (con fibras, de segunda, tercera)
14	Carne de cordero
15   Carne de cerdo                                                                                                                                                   
16   Charque, chalona (de cualquier animal) Embutidos (salchicha, chorizo, carnes fr�as,
18   Menudencias (h�gado, coraz�n, etc.)                                                                      
19   Otras carnes (llama, conejo, jochi, etc.)
20   Pescados frescos (s�balo, pejerrey, blanquillo, etc.)                                                                                                  
21   Sardinas, At�n
22   Otros pescados (secos, en lata, mariscos, etc.)
23   Aceite comestible                                                                                                                                               
24   Margarina, manteca y/o cebo                                                                                                                                 
25   Leche l�quida                                                                                                                                                
26   Leche en polvo                                                                                                                                               
27   Queso            
28	Huevos
29	Otros productos l�cteos (mantequilla, yogurt, reques�n, etc.)
30	Cebollas
31	Tomate
32	Zanahoria
33	Arvejas frescas
34	Habas frescas
35	Choclo
36	Lechuga, acelga
37	Locoto, piment�n, perejil
38	Otras verduras frescas (nabo, espinaca, etc)
39	Papa
40	Chu�o (seco, remojado)
41	Yuca
42	Oca
43	Otros tub�rculos (papaliza, camote, etc.)
44   Man�, lentejas, porotos
45   Pl�tano de comer/banano/guineo                                                                                                                     
46   Pl�tano de cocinar/postre                                                                                                                                    
47   Naranja                                                                                                                                                              
48   Mandarina                                                                                                                                                       
49   Lim�n                                                                                                                                                                     
50   Papaya                                                                                                                                                                     
51   Manzana                                                                                                                                                                       
52   Otras frutas frescas (pi�a, lima, pomelo, etc.)
53   Az�car                                                                                                                                                                   
54   Mermeladas y jaleas
55   Miel de ca�a y abeja                                                                                            
56   Refrescos en polvo y postres en polvo
57   Otros endulzantes (chancaca, sacarina, etc)
58	Te, caf�, mate, hierba mate, sultana												
59	Cocoa, Toddy, Chocolike												
60	Hojas de coca				
61   Sal                                                                                                                                                                        
62   Aj� en vaina, seco                                                                                                                                               
63   Condimentos y sazonadores (ajinomoto, caldos en cubitos, etc.)
64   Gaseosa en botella
65   Jugos en botella y/o cart�n
66   Bebidas alcoh�licas (cerveza, etc.)
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


