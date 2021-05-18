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

2) Fecha: Abril 2017

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los choques climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático

5) País: Costa Rica

6) Encuesta: Encuesta de Ingresos y Gastos de los hagares-EIGH

7) Ano: 2013

8) Inputs: 	$RAW\enigh_2013_base_gastos.dta 
            $RAW\enigh_2013_base_hogares.dta 
            $RAW\enigh_2013_base_personas.dta 
 
 
6) Outputs: $data_arm\CRI_ENIGH_2013.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: Se trabaja con los valores netos 
Alrededor del 1% de las becas provienen de instituciones sin fines de lucro,
este rubro se ubicó en transferencias privadas, al igual que las incapacidades y seguros
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\CRI\ENIGH\RAW"
	global data_arm  "${surveysFolder}\harmonized\Z_LAC_Ingreso_Gasto\CRI\ENIGH\data_arm"

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\enigh_2013_base_hogares.dta" , clear 
	

         
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

egen ing_lab_mon = rowtotal(h136_salario_neto_monet h140_ganancias_autonomo h176), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

rename h155 ing_ren_mon

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

egen ing_trspri_mon= rowtotal(h158 h160 h162 h165 h166 h168 h169 h170 h171 h172 h173 h174), missing

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

rename h163 ing_ct_mon

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

egen ing_trsgob_mon= rowtotal(h157 h159 h161 h164), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

egen ing_rem_mon= rowtotal(h156 h167), missing

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

gen ing_otro_mon=.

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

egen ing_lab_nomon = rowtotal(h137 h141 h177), missing

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

**Valor Locativo imputado neto**

rename h184 ing_ren_nomon 

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

egen ing_trspri_nomon= rowtotal(h178 h179), missing

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

gen ing_ct_nomon=.

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PUBLICAS * 
*--------------------------------------------------*

gen ing_trsgob_nomon= .

*--------------------------------------------------*
* INGRESO NO MONETARIO POR REMESAS INTERNACIONALES * 
*--------------------------------------------------*

gen ing_rem_nomon= .

*------------------------------------------------------------------*
* INGRESO NO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------------------*

gen ing_otro_nomon=.

*----------------------------*
* INGRESO NO MONETARIO TOTAL * 
*----------------------------*

egen double ing_nomon= rowtotal(ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon), missing

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
		  

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

rename h231 gasto_alihogar

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

rename h275 gasto_alifuera 

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

**se incluye en este rubro las bebidas alcoholicas consumidas dentro y fuera del hogar**

egen gasto_alta=rowtotal (h234 h276), missing 

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing


*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

rename h237 gasto_veca

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*

**la variable gasto_viv incluye el alquiler imputado**

gen gasto_viv=h243

/*se desagrega la variable gasto_viv para obtener los gastos específicos en agua,
electricidad y combustibles*/

preserve

/*Se usa la base de datos de gastos para calcular los gastos específicos en agua,
electricidad y combustibles*/

use "$RAW\enigh_2013_base_gastos.dta", clear

destring articulo, replace

*---------------*
* GASTO EN AGUA *
*---------------*

gen gasto_vag= gasto_mes if (articulo>=1650 & articulo<=1659) & tipo_gasto==1

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

gen gasto_vele= gasto_mes if ( articulo==1630| articulo==1631) & tipo_gasto==1

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

**Canfin es equivalente a Kerosene**

gen gasto_vk= gasto_mes if ( articulo==1634) & tipo_gasto==1


*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

gen gasto_vlp= gasto_mes if ( articulo==1632| articulo==1633) & tipo_gasto==1

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle= gasto_mes if ( articulo==1636) & tipo_gasto==1

*-----------------*
* GASTO EN CARBON *
*-----------------*

gen gasto_vca= gasto_mes if (articulo==1594 | articulo==16350) & tipo_gasto==1

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 

*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

gen gasto_vot= gasto_mes if ( articulo==1649) & tipo_gasto==1

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*	

gen gasto_vgn=.

*-------------------*
* GASTO EN PETROLEO *
*-------------------*	

gen gasto_vp=.

*-------------------*
* GASTO EN GASOLINA *
*-------------------*		  
		  
gen gasto_vgas=.

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi=.

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing


**Se agregan las variables construidas a nivel de hogar**

**Para conservar los missing values**
global miss "gasto_vag gasto_vele gasto_vca gasto_vlp gasto_vle gasto_vk gasto_vot gasto_vgn gasto_vleca gasto_vp gasto_vgas gasto_vdi gasto_vpgk"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}

collapse (sum) gasto_vag gasto_vele gasto_vca gasto_vlp gasto_vle gasto_vk gasto_vot gasto_vgn gasto_vleca gasto_vp gasto_vgas gasto_vdi gasto_vpgk (count) gasto_vag_miss gasto_vele_miss gasto_vca_miss gasto_vlp_miss gasto_vle_miss gasto_vk_miss gasto_vot_miss gasto_vgn_miss gasto_vleca_miss gasto_vp_miss gasto_vgas_miss gasto_vdi_miss gasto_vpgk_miss, by (upm_consec id_vivienda id_hogar)

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}

destring id_hogar, replace

	tempfile servicios
	save `servicios' , replace
	restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge upm_consec id_vivienda id_hogar using `servicios' , t(1:1)

*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

**se crea la variable gasto_vcon**

egen aux= rowtotal(gasto_vag gasto_vele gasto_vca gasto_vlp gasto_vle gasto_vk gasto_vot), missing
gen gasto_vcon= gasto_viv-aux if aux!=.
replace gasto_vcon=gasto_viv if aux==.
drop aux
 

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

rename h251 gasto_ens

*----------------*
* GASTO EN SALUD *
*----------------*

rename  h255 gasto_sal

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

rename h260 gasto_trans

preserve

/*Se usa la base de datos de gastos para calcular los gastos específicos en
 combustibles*/

use "$RAW\enigh_2013_base_gastos.dta", clear

destring articulo, replace

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga= gasto_mes if (articulo>=1534 & articulo<=1536) & tipo_gasto==1

*---------------------------------*
* GASTO EN GAS LICUADO-TRANSPORTE * 
*---------------------------------*

gen gasto_tlp= gasto_mes if (articulo==1533) & tipo_gasto==1

*-----------------*
* GASTO EN DIESEL * 
*-----------------*

gen gasto_tdie= gasto_mes if (articulo==1532) & tipo_gasto==1

/*gasto en aceites lubricantes (este rubro se le suma al rubro de reparación y
conservacion de vehículos más adelante) */

gen lubricantes= gasto_mes if (tipo_gasto==1) & (articulo>=3600 & articulo<=3605) | (articulo==3610)

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

gen gasto_tserv= gasto_mes if tipo_gasto==1  & ((articulo>=1540 & articulo<=1547) | (articulo==1877) | (articulo>=4650 & articulo<=4659) | (articulo==4693) | (articulo==4694)) 


*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

gen gasto_totros= gasto_mes if tipo_gasto==1  & ((articulo==1549) | (articulo==4669) | (articulo==4679) | (articulo==4689) | (articulo==4709))


**Se agregan las variables construidas a nivel de hogar**

**Para conservar los missing values**

global missi "gasto_tga gasto_tlp gasto_tdie lubricantes gasto_totros gasto_tserv"
foreach var in $missi {
egen `var'_missi= count(`var') if `var'!=.
}

collapse (sum) gasto_tga gasto_tlp gasto_tdie lubricantes gasto_totros gasto_tserv (count) gasto_tga_missi gasto_tlp_missi gasto_tdie_missi lubricantes_missi gasto_totros_missi gasto_tserv_missi,  by (upm_consec id_vivienda id_hogar)

foreach var in $missi {
replace `var'=. if `var'_missi==0
drop `var'_missi
}

destring id_hogar, replace

	tempfile transporte
	save `transporte' , replace
	restore 
	
**Se agregan las variables construidas a la base de datos de hogar**

mmerge upm_consec id_vivienda id_hogar using `transporte' , t(1:1)


*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

egen gasto_tman=rowtotal(lubricantes h257), missing


*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

rename h256 gasto_tadq

**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

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

rename h264 gasto_com 

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

egen gasto_edre= rowtotal(h270 h274), missing

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	

/*el gasto de hoteles y restaurantes se agrega al rubro de otros bienes y servicios,
sinembargo antes de hacerlo se le restan las variables de gasto en alimentos y bebidas alcoholicas
consumidas fuera del hogar*/

egen aux= rowtotal(gasto_alifuera h276), missing
gen aux2= h278-aux

egen gasto_otros= rowtotal(h285 aux2)
drop aux
drop aux2

*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

egen gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros)

** Etiquetas variables de gasto**

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

 
keep h078 upm_consec id_vivienda id_hogar factor_expansion gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**

gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "CRI"
label var pais "Pais" 
gen anio=2013
label var anio "Anio de la encuesta" 
gen encuesta="ENIGH"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename h078 miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros upm_consec id_vivienda id_hogar

		  
saveold "$data_arm\CRI_ENIGH_2013.dta" , replace v(12)




