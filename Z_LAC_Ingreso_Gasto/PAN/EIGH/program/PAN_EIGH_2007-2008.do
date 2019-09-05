clear
clear all
clear matrix
set more off
set matsize 2000


/*************************************************************************
 *************************************************************************			       	
	            Inter-American Development Bank

1) Elaborado por: Daniela Zuluaga Gordillo - SCL
			       danielazu@iadb.org

2) Fecha: Marzo 2017

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los choques climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático

5) País: Panamá

6) Encuesta: Encuesta de Ingresos y Gastos de los hagares-EIGH

7) Ano: 2007-2008

8) Inputs: 	$RAW\eigh2007_resumen_ingresos_gastos_constante.dta 
            $RAW\personas_ingresos_constante.dta
			$RAW\gastos_08a18_alimentos_constante.dta
 
 
6) Outputs: $data_arm\PAN_EIGH_2007-2008.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: Se trabaja con los valores netos 
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\PAN\EIGH\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\PAN\EIGH\data_arm"

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\eigh2007_resumen_ingresos_gastos_constante.dta" , clear 
	

          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

rename y110 ing_lab_mon

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*
rename y113 ing_ren_mon

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

/*Para generar el ingreso monetario por transferencias es necesario utilizar la 
base de datos de personas, para lograr separar las transferencias privadas de las
publicas*/

preserve

use "$RAW\personas_ingresos_constante.dta" , clear 

/*el monto recibido por las becas debe dividirse en 3 dado que la pregunta relacionada con 
las transferencias por becas hace referencia a los 3 meses anteriores*/

replace becasdin= becasdin/3
replace becasesp=becasesp/3

/*el monto recibido por subsidios, primas e indemnizaciones debe dividirse en 12 
dado que la pregunta relacionada con las transferencias por becas hace referencia 
a los 12 meses anteriores*/


replace subsidios= subsidios/12
replace prima=prima/12
replace indemdin=indemdin/12
replace indemesp=indemesp/12
replace ayudadin=ayudadin/12
replace ayudaesp= ayudaesp/12
replace otrastrandin=otrastrandin/12
replace otrastranesp= otrastranesp/12
replace fondo= fondo/12
replace remesasesp=remesasesp/12

**agrupo todas las variables de transferencias por hogar**



rename codjubila codjubilacion

global trans_dinero " pension1 pension2din jubilacion becasdin subsidios prima indemdin ayudadin otrastrandin fondo "

foreach var in $trans_dinero  {
egen gob_mon_`var'= rowtotal(`var') if (cod`var'<=1 | cod`var'==2)
egen pri_mon_`var'= rowtotal(`var') if (cod`var'==0| cod`var'>=3)
}


egen ing_trspri_mon=rowtotal(pri_mon*), missing

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

egen ing_trsgob_mon=rowtotal(gob_mon*), missing

global trans_especie " pension2esp becasesp indemesp ayudaesp otrastranesp "

foreach var in $trans_especie  {
egen gob_nomon`var' = rowtotal(`var') if (cod`var'<=1 | cod`var'==2)
egen pri_nomon`var'= rowtotal(`var') if (cod`var'==0| cod`var'>=3) 
}

egen ing_trsgob_nomon=rowtotal(gob_nomon*), missing
egen ing_trspri_nomon=rowtotal(pri_nomon*), missing


**Para conservar los missing values**
global miss "ing_trsgob_mon ing_trspri_mon ing_trsgob_nomon ing_trspri_nomon remesasesp"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}


collapse (sum) ing_trsgob_mon ing_trspri_mon ing_trsgob_nomon ing_trspri_nomon remesasesp (count) ing_trsgob_mon_miss ing_trspri_mon_miss ing_trsgob_nomon_miss ing_trspri_nomon_miss remesasesp_miss , by(llaveviv llavehog)

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}
**Se junta esta base con la base de resumen de ingresos y gastos**

tempfile transferencias
	save `transferencias' , replace
	restore 
	
	mmerge llaveviv llavehog using `transferencias' , t(1:1) 

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen ing_ct_mon=.

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

rename y1145 ing_rem_mon

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

rename y115 ing_otro_mon 

*--------------------*
* INGRESO MONETARIO  *  
*--------------------*

egen ing_mon= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing


         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*


**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

egen  ing_lab_nomon = rowtotal(y121 y122 y1232  y1234 y1235), missing

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

egen ing_ren_nomon= rowtotal(y1233 y125), missing

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

**ing_trspri_nomon**

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

**ing_trsgob_nomon**

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

gen ing_ct_nomon=.

*--------------------------------------------------*
* INGRESO NO MONETARIO POR REMESAS INTERNACIONALES * 
*--------------------------------------------------*

gen ing_rem_nomon= remesasesp

*------------------------------------------------------------------*
* INGRESO NO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------------------*

gen ing_otro_nomon=.

*----------------------------*
* INGRESO NO MONETARIO TOTAL * 
*----------------------------*

egen ing_nomon= rowtotal( ing_lab_nomon ing_ren_nomon  ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon), missing

          *=======================================================*
          * 1.3. INGRESO CORRIENTE TOTAL (MONETARIO+NO MONETARIO) *
          *=======================================================*


**Composicion del ingreso total**

*-----------------------*
* INGRESO LABORAL       * 
*-----------------------*

egen ing_lab = rowtotal(ing_lab_mon ing_lab_nomon), missing

*-----------------------------------*
* INGRESO POR RENTA DE LA PROPIEDAD * 
*-----------------------------------*

egen ing_ren= rowtotal (ing_ren_mon ing_ren_nomon), missing 

*---------------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS LOCALES * 
*---------------------------------------------*

egen ing_trspri= rowtotal(ing_trspri_mon ing_trspri_nomon), missing

*-------------------------------------*
* INGRESO POR REMESAS INTERNACIONALES * 
*-------------------------------------*

egen ing_rem= rowtotal(ing_rem_mon ing_rem_nomon), missing

*-------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS * 
*-------------------------------------*

egen ing_tpriv= rowtotal(ing_trspri ing_rem), missing

*-------------------------------------------------------------*
* INGRESO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-------------------------------------------------------------*

egen ing_ct= rowtotal(ing_ct_mon ing_ct_nomon), missing

*-------------------------------------------*
* INGRESO POR OTRAS TRANSFERENCIAS PUBLICAS * 
*-------------------------------------------*

egen ing_trsgob= rowtotal(ing_trsgob_mon ing_trsgob_nomon), missing

*-------------------------------------------*
* INGRESO POR TRANSFERENCIAS DEL GOBIERNO   * 
*-------------------------------------------*

egen ing_tpub= rowtotal(ing_trsgob ing_ct), missing


*------------------------------------------------------*
* INGRESO  POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------*

egen ing_otro= rowtotal(ing_otro_mon ing_otro_nomon), missing

*--------------------------*
* INGRESO CORRIENTE TOTAL  *  
*--------------------------*

egen ict= rowtotal(ing_mon ing_nomon), missing


** Etiquetas Variables Monetarias**

label var ing_mon "Ingreso monetario del hogar"
label var ing_lab_mon "Ingreso monetario laboral del hogar"
label var ing_ren_mon "Ingreso monetario por renta de la propiedad del hogar"
label var ing_trspri_mon "Ingreso monetario por transferencias privadas del hogar"
label var ing_ct_mon "Ingreso monetario del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob_mon "Ingreso monetario por transferencias publicas del hogar"
label var ing_rem_mon "Ingreso monetario del hogar por remesas internacionales"
label var ing_otro_mon "Otros ingresos monetarios/ ingresos extraordinarios del hogar" 

** Etiquetas Variables No Monetarias**

label var ing_nomon "Ingreso no monetario del hogar"
label var ing_lab_nomon "Ingreso no monetario laboral del hogar"
label var ing_ren_nomon "Ingreso no monetario por renta de la propiedad del hogar"
label var ing_trspri_nomon "Ingreso no monetario por transferencias privadas del hogar"
label var ing_ct_nomon "Ingreso no monetario del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob_nomon "Ingreso no monetario del hogar por otras transferencias del gobierno"
label var ing_rem_nomon "Ingreso no monetario del hogar por remesas internacionales"
label var ing_otro_nomon "Otros ingresos no monetarios/ ingresos extraordinarios del hogar" 

** Etiquetas Variables Agregadas**

label var ict "Ingreso corriente total"
label var ing_lab "Ingreso laboral del hogar"
label var ing_ren "Ingreso por renta de la propiedad del hogar"
label var ing_trspri "Ingreso por transferencias privadas locales del hogar"
label var ing_rem "Ingreso del hogar por remesas internacionales"
label var ing_tpriv "Ingreso del hogar por transferencias privadas"
label var ing_ct "Ingreso del hogar por transferencias de dinero del gobierno (cct-uct)"
label var ing_trsgob "Ingreso del hogar por otras transferencias del gobierno"
label var ing_tpub "Ingreso del hogar por transferencias del gobierno"
label var ing_otro "Otros ingresos/ ingresos extraordinarios del hogar" 



/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	
	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
		
preserve

/*Es necesario usar la base de datos de gastos del hogar para desagregar los gastos
de electricidad, agua y combustibles para cocinar*/

use "$RAW\gastos_08a18_alimentos_constante.dta" , clear 

destring codigo, replace
		 
*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

bys llaveviv llavehog: gen gasto_alihogar= gpromedio if (codigo>=190101 & codigo<=200109) 

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

bys llaveviv llavehog: gen gasto_alifuera= gpromedio if (codigo>=200201 & codigo<=200363) 


*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

bys llaveviv llavehog: gen gasto_alta= gpromedio if (codigo==200501)

**Para conservar los missing values**
global mis "gasto_alifuera gasto_alihogar gasto_alta"
foreach var in $mis {
egen `var'_mis= count(`var') if `var'!=.
}
collapse (sum) gasto_alifuera gasto_alihogar gasto_alta (count) gasto_alifuera_mis gasto_alihogar_mis gasto_alta_mis, by (llaveviv llavehog)

foreach var in $mis {
replace `var'=. if `var'_mis==0
drop `var'_mis
}


	tempfile alimento
	save `alimento' , replace
	
/*se vuelve a la base de datos inicial, para posteriormente hacer el merge con las
variables creadas en la base de hogares*/

restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge llaveviv llavehog  using `alimento' , t(1:1)

drop _merge


**Se le agrega el gasto no monetario en alimentos a el rubro de alimentos consumidos dentro del hogar**

egen aux= rowtotal(gasto_alihogar z19b2 z19b3 z19b4), missing
replace gasto_alihogar=aux
drop aux
*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

egen gasto_veca= rowtotal(z12a z12b2 z12b3), missing

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*

egen gasto_viv= rowtotal(z08a z08b2 z08b3 z08b5), missing

preserve

/*Es necesario usar la base de datos de gastos del hogar para desagregar los gastos
de electricidad, agua y combustibles para cocinar*/

use "$RAW\gastos_08a18_alimentos_constante.dta" , clear 

*---------------*
* GASTO EN AGUA *
*---------------*

bys llaveviv llavehog: gen gasto_vag= gpromedio if codigo=="080201"


*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

bys llaveviv llavehog: gen gasto_vele= gpromedio if codigo=="080401"


*-------------------*
* GASTO EN KEROSENE *
*-------------------*

bys llaveviv llavehog: gen gasto_vk= gpromedio if codigo=="090104"


*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

/*Se interpreta el gasto en tanque de gas como gasto en gas licuado de petroleo*/ 

bys llaveviv llavehog: gen gasto_vlp= gpromedio if codigo=="090101"

*---------------*
* GASTO EN LENA *
*---------------*

bys llaveviv llavehog: gen gasto_vle= gpromedio if codigo=="090102"

*-----------------*
* GASTO EN CARBON *
*-----------------*

bys llaveviv llavehog: gen gasto_vca= gpromedio if codigo=="090103"


**Para conservar los missing values**
global mi "gasto_vag gasto_vele gasto_vca gasto_vle gasto_vk gasto_vlp"
foreach var in $mi {
egen `var'_mi= count(`var') if `var'!=.
}


collapse (sum) gasto_vag gasto_vele gasto_vca gasto_vle gasto_vk gasto_vlp (count) gasto_vag_mi gasto_vele_mi gasto_vca_mi gasto_vle_mi gasto_vk_mi gasto_vlp_mi, by (llaveviv llavehog)

foreach var in $mi {
replace `var'=. if `var'_mi==0
drop `var'_mi
}

	tempfile servicios
	save `servicios' , replace
	
/*se vuelve a la base de datos inicial, para posteriormente hacer el merge con las
variables creadas en la base de hogares*/

restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge llaveviv llavehog  using `servicios' , t(1:1)

drop _merge

/*Ahora se calculan los otros gastos en vivienda y servicios de conservación, excluyendo
los rubros desagregados*/

egen aux= rowtotal( gasto_vag gasto_vele gasto_vk gasto_vlp gasto_vle gasto_vca), missing

gen gasto_vcon=gasto_viv-aux if aux!=.
replace gasto_vcon=gasto_viv if aux==.
drop aux

**Se crean variables de vivienda, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*-------------------*
* GASTO EN PETROLEO *
*-------------------*

gen gasto_vp=.

*-------------------*
* GASTO EN GASOLINA *
*-------------------*

gen gasto_vgas=.

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*

gen gasto_vgn=.

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi=.

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 

*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

gen gasto_vot= .



*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

 egen gasto_ens=rowtotal(z10a z11a z10b2 z10b3 z11b2 z11b3), missing

*----------------*
* GASTO EN SALUD *
*----------------*

egen gasto_sal= rowtotal(z14a z14b2 z14b3), missing

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans= rowtotal(z17a1 z17b12 z17b13), missing

preserve

/*Es necesario usar la base de datos de gastos del hogar para desagregar los gastos
de transporte*/

use "$RAW\gastos_08a18_alimentos_constante.dta" , clear 

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga= gpromedio if (codigo=="200503"| codigo=="200504")

*---------------------------------*
* GASTO EN GAS LICUADO-TRANSPORTE * 
*---------------------------------*

gen gasto_tlp= gpromedio if codigo=="200506"

*-----------------*
* GASTO EN DIESEL * 
*-----------------*

gen gasto_tdie= gpromedio if codigo=="200505"


**Para conservar los missing values**
global m "gasto_tga gasto_tdie gasto_tlp"
foreach var in $m {
egen `var'_m= count(`var') if `var'!=.
}

collapse(sum) gasto_tga gasto_tdie gasto_tlp (count) gasto_tga_m gasto_tdie_m gasto_tlp_m, by (llaveviv llavehog)

foreach var in $m {
replace `var'=. if `var'_m==0
drop `var'_m
}

	tempfile trans
	save `trans' , replace
	
/*se vuelve a la base de datos inicial, para posteriormente hacer el merge con las
variables creadas en la base de hogares*/

restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge llaveviv llavehog  using `trans' , t(1:1)

drop _merge


*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*---------------------------------*

egen gasto_tserv= rowtotal(z17a12), missing

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

egen gasto_tman= rowtotal(z17a13), missing

**Se le resta a este rubro el gasto en combustibles**

egen combustibles= rowtotal(gasto_tga gasto_tdie gasto_tlp), missing
gen aux= gasto_tman-combustibles if combustibles!=.
replace aux=gasto_tman if combustibles==.
replace gasto_tman=aux
drop aux


*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

egen gasto_tadq= rowtotal(z17a11), missing


*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

/*Ahora se calculan los otros gastos en transporte, excluyendo
los rubros ya desagregados*/

egen aux= rowtotal( gasto_tga gasto_tlp gasto_tdie gasto_tserv gasto_tadq gasto_tman), missing

gen gasto_totros=gasto_trans-aux
drop aux

*---------------------------------*
* GASTO EN GAS ALCOHOL-TRANSPORTE *
*---------------------------------*

gen gasto_talc=.

*------------------------------------------------*
* GASTO EN GAS NATURAL COMPRIMIDO GNC-TRANSPORTE * 
*------------------------------------------------*

gen gasto_tgnc=. 


*----------------------------------------*
* GASTO EN OTROS COMBUSTIBLES-TRANSPORTE * 
*----------------------------------------*

gen gasto_totcomb= . 


*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*

egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb), missing


*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

egen gasto_com= rowtotal(z17a2 z17b22 z17b23), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

egen gasto_edre= rowtotal(z15a z16a z15b2 z15b3 z16b2 z16b3), missing

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	

**Se identifica la parte del pago en especie (z02c que corresponde al resto de remuneraciones en especie)

egen aux= rowtotal(z08b3 z10b3 z11b3 z12b3 z14b3 z17b13 z17b23 z16b3 z15b3 z13b3 z18b3 z19b3), missing

gen resto_especie= z02c-aux if aux!=.
replace resto_especie= z02c if aux==.

/*se remplaza z02c por la variable resto_especie que corresponde al resto de remuneraciones en especie. Se hace este procedimiento dado que 
las variables z08b3 z10b3 z11b3 z12b3 z14b3 z17b13 z17b23 z16b3 z15b3 z13b3 z18b3 z19b3 ya fueron agregadas previamente*/

replace z02c=resto_especie

egen gasto_otros= rowtotal(z13a z18a z13b2 z13b3 z18b2 z18b3 z02d z02c), missing


*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

egen gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros), missing

**Etiquetas variables de gasto**

label var gct "Gasto corriente total"
label var gasto_ali "Gasto en alimentos, bebidas y tabaco"
label var gasto_alihogar "Gasto en alimentos consumidos en el hogar"
label var gasto_alifuera "Gasto en alimentos consumidos fuera del hogar"
label var gasto_alta "Gasto en alcohol y tabaco"
label var gasto_veca "Gasto en vestido y calzado"
label var gasto_viv "Gasto en vivienda, servicios de conservacion y combustibles"
label var gasto_vcon "Gasto en vivienda y servicios de conservacion"
label var gasto_vk "Gasto en kerosene para uso domestico"
label var gasto_vleca "Gasto en lena y carbon para uso domestico"
label var gasto_vle "Gasto en lena para uso domestico"
label var gasto_vca "Gasto en carbon para uso domestico"
label var gasto_vlp "Gasto en gas licuado de petroleo para uso domestico"
label var gasto_vdi "Gasto en diesel para uso domestico"
label var gasto_vp "Gasto en petroleo para uso domestico"
label var gasto_vgas "Gasto en gasolina para uso domestico"
label var gasto_vpgk "Gasto en petroleo, gasolina y kerosene para uso domestico"
label var gasto_vot "Gasto en otros combustibles para uso domestico"
label var gasto_vele "Gasto en electricidad"
label var gasto_vgn "Gasto en gas natural"
label var gasto_vag "Gasto en agua"
label var gasto_ens "Gasto en muebles, enseres y mantenimiento de la vivienda"
label var gasto_sal "Gasto en salud"
label var gasto_trans "Gasto en transporte"
label var gasto_tserv "Gasto en servicios de transporte"
label var gasto_tga "Gasto en gasolina para transporte"
label var gasto_tdie "Gasto en diesel y gas para transporte"
label var gasto_tgnc "Gasto en gas natural comprimido para transporte"
label var gasto_tcomb "Gasto en combustible para transporte"
label var gasto_tlp "Gasto en gas licuado de petroleo para transporte"
label var gasto_talc "Gasto en alcohol para transporte"
label var gasto_totcomb "Gasto en otros combustibles para transporte"
label var gasto_tman "Gasto en reparacion y conservacion de vehiculos"
label var gasto_tadq "Gasto en adquisicion de vehiculos"
label var gasto_totros "Otros gastos en transporte"
label var gasto_com "Gasto en comunicaciones"
label var gasto_edre "Gasto en educacion y recreacion"
label var gasto_otros "Gasto en otros bienes y servicios"

 
keep llaveviv llavehog factor personas gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**
** Dado que la encuesta se realizo en los 6 ultimos meses del 2007 y los primeros 6 del 2008 se escoge 2008 por ser el mas reciente*
gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "PAN"
label var pais "Pais" 
gen anio=2008
label var anio "Anio de la encuesta" 
gen encuesta="EIGH"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename personas miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename factor factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros llaveviv llavehog

		  
saveold "$data_arm\PAN_EIGH_2007-2008.dta" , replace v(12)
