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

5) País: Colombia

6) Encuesta: Encuesta Nacional de Calidad de Vida-ENCV

7) Ano: 2014

8) Inputs: 	$RAW\gastos del hogar.dta 
            $RAW\fuerza de trabajo.dta
			$RAW\tenencia y financiacion de la vivienda que ocupa el hogar.dta
			$RAW\condiciones de vida del hogar y tenencia de bienes.dta
			$RAW\servicios del hogar.dta
			$RAW\salud.dta
			$RAW\educacion.dta
			$RAW\atencion integral menores de 5 anos.dta
			$RAW\caracteristicas y composicion del hogar.dta
           
 
6) Outputs: $data_arm\COL_ENCV_2014.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: 
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\COL\ENCV\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\COL\ENCV\data_arm"

	
	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\fuerza de trabajo.dta" , clear 
	
**Se hace el merge con la base de caracteristicas y composicion del hogar**



mmerge directorio secuencia_encuesta secuencia_p orden using "$RAW\caracteristicas y composicion del hogar.dta", t(1:1)


	
**Se crea la variable de miembros del hogar a partir de la relación con el jefe de hogar**

gen relacion=.
replace relacion=1 if  p6051==1
replace relacion=2 if  p6051==2
replace relacion=3 if  p6051==3
replace relacion=4 if (p6051>=4 &  p6051<=9) | (p6051==14)
replace relacion=5 if  (p6051== 11) | (p6051== 12) | (p6051==13)
replace relacion=6 if  p6051==10

label variable relacion "Relacion con el jefe del hogar"
label define relacion 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion 6 "Empleado/a domestico/a", add
label value relacion relacion_ci

**Los miembros del hogar es la suma de individuos que componen el hogar, excluyenndo empleados domesticos y otros no parientes**

gen miembros= .

replace miembros=1 if relacion<5

**Se crea la cantidad de miembros en el hogar**

bys directorio secuencia_p: egen cant_miembros=sum(miembros)

drop if _merge==2

          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**	

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

/*Dado que las preguntas de ingreso se encuentran mensualizadas y las preguntas
relacionadas con primas de servicios, vacaciones y bonificaciones se encuentran 
anualizadas, se procede a mensualizar dichos valores*/

forvalues i= 1(1)5 {
gen p1087s`i'a1_mes= (p1087s`i'a1)/12
}


egen  ing_lab_mon= rowtotal ( p8624 p8626s1 p8628s1 p8630s1 p8631s1 p1087s1a1_mes p1087s2a1_mes p1087s3a1_mes p1087s4a1_mes p1087s5a1_mes p8636s1 p8640s1 p6750 ), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

**Se mensualizan las variables asociadas a renta de la propiedad**

replace p8652s1= p8652s1/12



egen  ing_ren_mon= rowtotal(p8646s1 p8652s1), missing
	
*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

/*Se incluyen las transferencias recibidas en becas de educacion provenientes de la
base de educacion*/

preserve

/*Se usa la base de educacion para establecer los ingresos del hogar por subsidios de educacion*/

use "$RAW\educacion.dta" , clear 


**Se mensualizan las variables

replace p8610s1= p8610s1 if p8610s2==1
replace p8610s1= p8610s1/2 if p8610s2==2
replace p8610s1= p8610s1/6 if p8610s2==3
replace p8610s1= p8610s1/12 if p8610s2==4

replace p8612s1= p8612s1 if p8612s2==1
replace p8612s1= p8612s1/2 if p8612s2==2
replace p8612s1= p8612s1/6 if p8612s2==3
replace p8612s1= p8612s1/12 if p8612s2==4


gen beca_pub= p8610s1 if (p6229==2 | p6229==3 | p6229==6 | p6229==4 | p6229==5)
gen beca_pri= p8610s1 if (p6229==1  | p6229==7 | p6229==8)

gen sub_pub=p8612s1 if (p6238==2 | p6238==3  | p6238==6  | p6238==4 | p6238==5)
gen sub_pri=p8612s1 if (p6238==1| p6238==7 | p6238==8)

**Para conservar los missing values**
global miss "beca_pub beca_pri sub_pub sub_pri"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}


collapse (sum) beca_pub beca_pri sub_pub sub_pri (count) beca_pub_miss beca_pri_miss sub_pub_miss sub_pri_miss, by (directorio secuencia_p) 

foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}
tempfile subsidio 
save `subsidio' , replace

restore 

**Se hace el merge con la base de fuerza de trabajo**

mmerge directorio secuencia_p  using `subsidio' , t(n:n) 
	
drop _merge


**se mensualizan las variables asociadas a transferencias privadas**

replace p8650s1= p8650s1/12


/* Dado que el 93% de las personas afirmaron
que la ayuda monetario que recibieron provenía del interior del país (categoría 1), la categoría " ambas" (categoría 3)
se le asignó al dinero proveniente del interior y no al dinero proveniente del exterior (categoria 2)*/

**Dado que las variables beca_pri y sub_pri se encuentra a nivel de hogar, se encuentra el otro monto de la transferencia a nivel de hogar**

bys directorio secuencia_p: gen aux_trspri_mon= p8650s1 if  (p8650s1a1==1 | p8650s1a1==3)

egen ing_trspri_mon= rowtotal (aux_trspri_mon beca_pri sub_pri), missing

drop aux_trspri_mon

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

preserve

/*Se usa la base de caracteristicas y composicion del hogar para establecer el
ingreso por programas sociales del gobierno*/

use "$RAW\condiciones de vida del hogar y tenencia de bienes.dta" , clear 

**se mensualizan las variables asociadas a este rubro**

replace p5191s1a1= p5191s1a1/12
replace p5191s2a1= p5191s2a1/12

rename p5191s1a1 ing_ct_mon

/*en esta base de datos la secuencia_encuesta es equivalente a la secuencia_p de la
base de hogares, por lo cual se renombra secuencia_encuesta a secuencia_p*/

drop secuencia_p
rename secuencia_encuesta secuencia_p

keep directorio secuencia_p ing_ct_mon p5191s2a1

/*la variable p5191s2a1 se emplea mas adelante para la creación del ingreso no monetario,*
pues esta corresponde al ingreso en especie por programas sociales*/

tempfile programas sociales 
save `programas sociales' , replace

restore 

**se hace el merge de la base de temporal programas sociales con la base fuerza de trabajo**

mmerge directorio secuencia_p using `programas sociales' , t(n:1) 
	
drop _merge


*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

**se mensualizan las variables asociadas a este rubro**

replace p8648s1= p8648s1/12

egen aux_ing_trsgob_mon= rowtotal(p8642s1 p8644s1 p8648s1), missing
bys directorio secuencia_p: egen trsgob =sum(aux_ing_trsgob_mon), m

egen ing_trsgob_mon= rowtotal(trsgob beca_pub sub_pub), missing

drop aux_ing_trsgob_mon trsgob

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= p8650s1 if  (p8650s1a1==2)


*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

gen ing_otro_mon=.

*-------------------*
* INGRESO MONETARIO *  
*-------------------*



/*Antes de continuar, es necesario agregar todas las variables individuales para 
poder tener un agregado a nivel del hogar, dado que el ingreso monetario es la suma
de las variables creadas previamente, no es posible sumarlas sin que todas se 
encuentren agregadas por hogar*/

preserve 

/*Para los ingresos del hogar, unicamente se considera la suma de los ingresos de aquellos individuos que conforman el hogar,
de esta manera, no se considera en la suma aquellos ingresos de empleados domesticos u otros no parientes. Se pone un missing value a todos los ingresos de estas personas
para que la suma final del ingreso del hogar no se vea afectada*/

global nomiembros "ing_lab_mon ing_ren_mon ing_trspri_mon ing_trsgob_mon ing_rem_mon ing_otro_mon "

foreach var in $nomiembros {
replace `var'=. if miembros==.
}

**Para conservar los missing values**
global mis "ing_lab_mon ing_ren_mon ing_trspri_mon ing_trsgob_mon ing_rem_mon ing_otro_mon"
foreach var in $mis {
egen `var'_mis= count(`var') if `var'!=.
}

collapse (sum)  ing_lab_mon ing_ren_mon ing_rem_mon ing_otro_mon (count) ing_lab_mon_mis ing_ren_mon_mis ing_trspri_mon_mis ing_trsgob_mon_mis ing_rem_mon_mis ing_otro_mon_mis , by(directorio secuencia_p)

foreach var in $mis {
replace `var'=. if `var'_mis==0
drop `var'_mis
}

tempfile ingreso monetario 
save `ingreso monetario' , replace

restore 

**se eliminan las variables de interés que se encuentran a nivel de individuo**

drop ing_lab_mon ing_ren_mon ing_rem_mon ing_otro_mon 


**se hace el merge de la base temporal de ingreso monetario con la base de fuerza de trabajo**

mmerge directorio secuencia_p using `ingreso monetario' , t(n:1) 
	
drop _merge

egen ing_mon= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing



         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*


**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

preserve

/*Para considerar el autoconsumo como parte de los ingresos no monetarios, es necesario 
construir dicha variable a partir de la base de datos de gastos del hogar*/

use "$RAW\gastos del hogar.dta" , clear 

**Es necesario mensualizar todas las variables de gasto que sean semanales, trimestrales  y anuales**

**Variables semanales**

forvalues i= 1/23 {
gen sem_1autoc`i'=(p8900s`i'a2)*4.33
}

forvalues i= 1/12 {
gen sem_2`i'autoc`i'=(p8905s`i'a2)*4.33
}
 
 
 egen autoc_sem=rowtotal(sem_*), missing
 
**Variables trimestrales**

 forvalues i= 1/10 {
gen trim_autoc`i'=(p8915s`i'a2)/3
}

 egen autoc_trim=rowtotal(trim_*), missing
 
**Variables anuales**

 forvalues i= 1/8 {
gen an_1autoc`i'=(p8920s`i'a2)/12
}

 
 forvalues i= 10/21 {
gen an_2autoc`i'=(p8920s`i'a2)/12
}

 forvalues i= 23/25 {
gen an_3autoc`i'=(p8920s`i'a2)/12
}
 
  egen autoc_anu=rowtotal(an_*), missing
  
**se genera la suma de autoconsumo para las variables mensuales**

  forvalues i= 1/12 {
gen mes_1autoc`i'=p8910s`i'a2
}

 forvalues i= 16/16 {
gen mes_2autoc`i'=(p8910s`i'a2)/12
}
   egen autoc_men=rowtotal(mes_*), missing
   
   
egen autoconsumo= rowtotal (autoc_sem autoc_men autoc_trim autoc_anu), missing


drop secuencia_p
rename secuencia_encuesta secuencia_p

keep directorio secuencia_p autoconsumo

tempfile autoconsumo
save `autoconsumo' , replace

restore 

/*se hace el merge de la base de tenencia y financiacion de la vivienda que ocupa el hogar
con la base de fuerza de trabajo*/

mmerge directorio secuencia_p using `autoconsumo' , t(n:1) 
	
drop _merge

egen  ing_lab_nomon = rowtotal(p6595s1 p6605s1 p6623s1 p6615s1), missing

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*
preserve

/*Se usa la base de tenencia y financiacion de la vivienda que ocupa el hogar
para establecer el ingreso no monetario por renta de la propiedad (estimacion 
por alquiler de la vivienda imputado)*/

use "$RAW\tenencia y financiacion de la vivienda que ocupa el hogar.dta" , clear 


gen ing_ren_nomon= p5130

drop secuencia_p
rename secuencia_encuesta secuencia_p

keep directorio secuencia_p ing_ren_nomon

tempfile renta no monetario 
save `renta no monetario' , replace

restore 

/*se hace el merge de la base de tenencia y financiacion de la vivienda que ocupa el hogar
con la base de fuerza de trabajo*/

mmerge directorio secuencia_p using `renta no monetario' , t(n:1) 
	
drop _merge

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

gen ing_trspri_nomon= .

*--------------------------------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------------------*

rename p5191s2a1 ing_ct_nomon

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

*-----------------------*
* INGRESO NO MONETARIO  * 
*-----------------------*

/*Antes de continuar, es necesario agregar todas las variables individuales para 
poder tener un agregado a nivel del hogar, dado que el ingreso no monetario del hogar es la suma
de las variables creadas previamente, no es posible agregarlas sin que todas se 
encuentren agregadas por hogar*/

preserve 

/*Para los ingresos del hogar, unicamente se considera la suma de los ingresos de aquellos individuos que conforman el hogar,
de esta manera, no se considera en la suma aquellos ingresos de empleados domesticos u otros no parientes. Se pone un missing value a todos los ingresos de estas personas
para que la suma final del ingreso del hogar no se vea afectada*/

global nomiembro "ing_lab_nomon "

foreach var in $nomiembro {
replace `var'=. if miembro==.
}

collapse (sum) ing_lab_nomon (count) nonmiss=ing_lab_nomon, by(directorio secuencia_p)
replace ing_lab_nomon=. if nonmiss==0
drop nonmiss


tempfile ingreso no monetario 
save `ingreso no monetario' , replace

restore 

**se eliminan las variables de interés que se encuentran a nivel de individuo

drop ing_lab_nomon 

**se hace el merge de la base temporal de ingreso no monetario laboral con la base de fuerza de trabajo**

mmerge directorio secuencia_p using `ingreso no monetario' , t(n:1) 
	
drop _merge

**Se le agrega al ingreso no monetario el autoconsumo**

egen aux= rowtotal(ing_lab_nomon autoconsumo), missing

replace ing_lab_nomon=aux
drop aux

egen  ing_nomon= rowtotal(ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon), missing




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

keep cant_miembros  directorio secuencia_p secuencia_encuesta  ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub

bys directorio secuencia_p: gen id= _n

keep if id==1

/*Para lograr hacer el merge con las otras bases mas adelante, es necesario que 
la secuencia_encuesta de la base de fuerza de trabajo corresponda con la de las
bases de hogares, por esta razon se realiza el siguiente procedimiento*/

replace secuencia_encuesta= secuencia_p
replace secuencia_p=1



saveold "$RAW\ingresos del hogar.dta", replace v(12)


/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	

		  
/*Para los calculos siguientes, es necesario utilizar 3 bases de datos, por lo 
cual se hace el merge entre estas*/

use "$RAW\gastos del hogar.dta" , clear 

mmerge directorio secuencia_encuesta using "$RAW\servicios del hogar.dta", t(1:1)

drop _merge
	 
mmerge directorio secuencia_encuesta using "$RAW\ingresos del hogar.dta", t(1:1)

drop _merge

mmerge directorio secuencia_encuesta using "$RAW\tenencia y financiacion de la vivienda que ocupa el hogar.dta", t(1:1)

drop _merge

**Es necesario mensualizar todas las variables de gasto que sean semanales, trimestrales  y anuales**

**Variables semanales**

global semanal " p8730s1 p8731s1 p8732s1 p8733s1 p8734s1 p8735s1 p8736s1 p8737s1 p8738s1 p8739s1 p8740s1 p8741s1 p8742s1 p8743s1 p8744s1 p8745s1 p8746s1 p8747s1 p8748s1 p8749s1 p8750s1 p8751s1 p5683s1 p8754s1 p8755s1 p8756s1 p8757s1 p8758s1 p8759s1 p8760s1 p8761s1 p8762s1 p8763s1 p8764s1 p5684s1 p8861s1 p8900s1a2 p8900s2a2 p8900s3a2 p8900s4a2 p8900s5a2 p8900s6a2 p8900s7a2 p8900s8a2 p8900s9a2 p8900s10a2 p8900s11a2 p8900s12a2 p8900s13a2 p8900s14a2 p8900s15a2 p8900s16a2 p8900s17a2 p8900s18a2 p8900s19a2 p8900s20a2 p8900s21a2 p8900s22a2 p8900s23a2 p8905s1a2 p8905s2a2 p8905s3a2 p8905s4a2 p8905s5a2 p8905s6a2 p8905s7a2 p8905s8a2 p8905s9a2 p8905s10a2 p8905s11a2 p8905s12a2"      

foreach var in $semanal {
replace `var'= `var'*4.33
}



**Variables trimestrales**

global trimestral " p8780s1 p8781s1 p8782s1 p8783s1 p8784s1 p8785s1 p8786s1 p8787s1 p8788s1 p8869s1 p8866s1 p8915s1a2 p8915s2a2 p8915s3a2 p8915s4a2 p8915s5a2 p8915s6a2 p8915s7a2 p8915s8a2 p8915s9a2 p8915s10a2"

foreach var in $trimestral {
replace `var'= `var'/3
}


**Variables anuales**

global anual "p8789s1 p8790s1 p8791s1 p8792s1 p8793s1 p8794s1 p8795s1 p8796s1 p8797s1 p8798s1 p8799s1 p8800s1 p8801s1 p8802s1 p8803s1 p8804s1 p8805s1 p8806s1 p8807s1 p8808s1 p8809s1 p5686s1 p8870s1 p8871s1 p8867s1 p8920s1a2 p8920s2a2 p8920s3a2 p8920s4a2 p8920s5a2 p8920s6a2 p8920s7a2 p8920s8a2 p8920s10a2 p8920s11a2 p8920s12a2 p8920s13a2 p8920s14a2 p8920s15a2 p8920s16a2 p8920s17a2 p8920s18a2 p8920s19a2 p8920s20a2 p8920s21a2 p8920s23a2 p8920s24a2 p8920s25a2"
foreach var in $anual {
replace `var'= `var'/12
}




          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
		 
 

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

egen gasto_alihogar= rowtotal(p8730s1 p8731s1 p8732s1 p8733s1 p8734s1 p8735s1 p8736s1 p8737s1 p8738s1 p8739s1 p8740s1 p8741s1 p8742s1 p8743s1 p8744s1 p8745s1 p8746s1 p8747s1 p8748s1 p8749s1 p8750s1 p8751s1 p5683s1 p8900s1a2 p8900s2a2 p8900s3a2 p8900s4a2 p8900s5a2 p8900s6a2 p8900s7a2 p8900s8a2 p8900s9a2 p8900s10a2 p8900s11a2 p8900s12a2 p8900s13a2 p8900s14a2 p8900s15a2 p8900s16a2 p8900s17a2 p8900s18a2 p8900s19a2 p8900s20a2 p8900s21a2 p8900s22a2 p8900s23a2), missing


*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

egen gasto_alifuera=rowtotal(p8763s1 p8905s10a2), missing


*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

egen gasto_alta = rowtotal(p8754s1 p8757s1 p8905s1a2 p8905s4a2), missing

*------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS *
*------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

 egen gasto_veca= rowtotal(p8760s1 p8769s1 p8771s1 p8780s1 p8781s1 p8782s1  p8783s1 p8905s7a2 p8910s4a2 p8910s6a2 p8915s1a2 p8915s2a2 p8915s3a2 p8915s4a2 ), missing

*----------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION *
*----------------------------------------------*

/*se divide el valor del pago de los servicios de conservacion en los meses para los que corresponde este pago,
con el fin de obtener un valor mensual*/

replace p5044= p5044/p5044s1
replace p5034= p5034/p5034s1

egen gasto_vcon= rowtotal(p5044 p5034 p5650 p5140 p8920s2a2 p8920s10a2 p8798s1 p8800s1 p8920s12a2), missing

*---------------*
* GASTO EN AGUA *
*---------------*

/*se divide el valor del pago de los servicios de acueducto en los meses para los que corresponde este pago,
con el fin de obtener un valor mensual*/

replace p5067= p5067/p5067s1
gen gasto_vag= p5067  

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

/*se divide el valor del pago de la electricidad en los meses para los que corresponde este pago,
con el fin de obtener un valor mensual*/

replace p5018= p5018/p5018s1

gen gasto_vele= p5018 

*------------------------------------------------*
* GASTO EN PETROLEO, GASOLINA, KEROSENE, ALCOHOL *
*------------------------------------------------*

gen gasto_vpgk= p8540 if p8536==3


*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

gen gasto_vlp= p8540 if p8536==4


*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle= p8540 if p8536==6

*-----------------*
* GASTO EN CARBON *
*-----------------*

gen gasto_vca= p8540 if p8536==5

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 


*--------------------------------------------*
* OTROS GASTOS EN COMBUSTIBLES PARA CALENTAR *
*--------------------------------------------*

gen gasto_vot= p8540 if p8536==7

**Se crean variables de vivienda, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

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

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

gen gasto_vk=.

*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

egen gasto_viv= rowtotal(gasto_vcon gasto_vag gasto_vele gasto_vpgk gasto_vlp gasto_vle gasto_vca gasto_vot), missing

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

egen gasto_ens= rowtotal(p8755s1 p8764s1 p8767s1 p8770s1 p8774s1 p8789s1 p8791s1 p8792s1 p8793s1 p8786s1 p8790s1 p8905s2a2 p8905s11a2 p8910s2a2 p8910s5a2 p8910s9a2 p8915s7a2 p8920s1a2 p8920s3a2 p8920s4a2 p8920s5a2 ), missing

*----------------*
* GASTO EN SALUD *
*----------------*
preserve

/*Se usa la base de salud para establecer el gasto del hogar en salud*/

use "$RAW\salud.dta" , clear 

/*se remplazan los valores de secuencia_encuesta por los de secuencia_p para que 
dicha variable corresponda con los valores de secuencia_encuesta de las bases de hogares*/ 

replace secuencia_encuesta= secuencia_p
drop secuencia_p


**Se mensualizan las variables anualizadas**

replace p8558s1a1= p8558s1a1/12 
replace p8558s2a1=p8558s2a1/12
replace p802= p802/12

egen gasto_sal= rowtotal( p8551 p119 p6154s2a1  p6154s3a1 p6154s4a1 p6154s5a1 p6154s6a1  p6154s11a1  p6154s8a1  p6154s9a1 p8558s1a1 p8558s2a1 p802), missing

**Para conservar los missing values**
global mi "gasto_sal p6154s7a1"
foreach var in $mi {
egen `var'_mi= count(`var') if `var'!=.
}
collapse (sum) gasto_sal p6154s7a1 (count) gasto_sal_mi p6154s7a1_mi , by (directorio secuencia_encuesta) 

foreach var in $mi {
replace `var'=. if `var'_mi==0
drop `var'_mi
}


tempfile salud 
save `salud' , replace

restore 

**Se hace el merge con la base de hogares**

mmerge directorio secuencia_encuesta using `salud' , t(1:1) 
	
drop _merge

*--------------------*
* GASTO EN EDUCACION *
*--------------------*
preserve

/*Se usa la base de educacion (para ninos mayores de 5 anos) para establecer el gasto del hogar en educacion*/

use "$RAW\educacion.dta" , clear 

/*se remplazan los valores de secuencia_encuesta por los de secuencia_p para que 
dicha variable corresponda con los valores de secuencia_encuesta de las bases de hogares*/ 

replace secuencia_encuesta= secuencia_p
drop secuencia_p


**Se mensualizan las variables diarias**

replace p6180s1= p6180s1*30
replace p6180s2= p6180s2*30
**Se mensualizan las variables anuales**

replace p8594s1= p8594s1/12
replace p8596s1=p8596s1/12
replace p8598s1=p8598s1/12

**se mensualizan el resto de variables**

replace p8610s1= p8610s1 if p8610s2==1
replace p8610s1= p8610s1/2 if p8610s2==2
replace p8610s1= p8610s1/6 if p8610s2==3
replace p8610s1= p8610s1/12 if p8610s2==4

replace p8612s1= p8612s1 if p8612s2==1
replace p8612s1= p8612s1/2 if p8612s2==2
replace p8612s1= p8612s1/6 if p8612s2==3
replace p8612s1= p8612s1/12 if p8612s2==4


gen alimento_escolar= p6180s1 if p6180==1 
replace alimento_escolar= p6180s2+p6180s1 if p6180==1 & p6180s1==0

egen educacion1= rowtotal(p8594s1 p8596s1 p8598s1 p8600s1 p8604s1 p8606s1 p8608s1 p8610s1 p8612s1), missing

**Para conservar los missing values**
global missing "educacion1 alimento_escolar p8602s1"
foreach var in $missing {
egen `var'_missing= count(`var') if `var'!=.
}

collapse (sum) educacion1 alimento_escolar p8602s1 (count) educacion1_missing alimento_escolar_missing p8602s1_missing , by (directorio secuencia_encuesta) 

foreach var in $missing {
replace `var'=. if `var'_missing==0
drop `var'_missing
}

tempfile edu 
save `edu' , replace

restore 

**Se hace el merge con la base de hogares**

mmerge directorio secuencia_encuesta using `edu' , t(1:1) 
	
drop _merge

preserve

/*Se usa la base de atencion integral para ninos menores de 5 anos para establecer el gasto del hogar en educacion-
para este grupo de la poblacion*/

use "$RAW\atencion integral menores de 5 anos.dta" , clear 

/*se remplazan los valores de secuencia_encuesta por los de secuencia_p para que 
dicha variable corresponda con los valores de secuencia_encuesta de las bases de hogares*/ 

replace secuencia_encuesta= secuencia_p
drop secuencia_p


**Se mensualizan las variables diarias**


replace p774s1= p774s1*30
replace p774s2 = p774s2 *30
replace p774s3= p774s3*30
replace p776s1= p776s1*30
replace p776s2 = p776s2 *30
replace p776s3= p776s3*30

**Se mensualizan las variables anuales**

replace p6169s1= p6169s1/12
replace p6171s1a1=p6171s1a1/12
replace p6171s2a1 =p6171s2a1/12
replace p6171s3a1=p6171s3a1/12

gen alimento1= p774s1 if p774==1 | p774==2
replace alimento1= p774s3 if p774==3
replace alimento1= p774s2 if p774==2 & p774s1==0

gen alimento2= p776s1 if p776==1 | p776==2
replace alimento2= p776s3 if p774==3
replace alimento2= p776s2 if p776==2 & p776s1==0


egen alimento_escolar2= rowtotal( p8574s1 alimento1 alimento2), missing


egen educacion2= rowtotal(p6169s1 p6171s1a1  p6171s2a1  p6171s3a1 p8570s1 p8576s1), missing

**Para conservar los missing values**
global missi "educacion2 p8572s1 alimento_escolar2"
foreach var in $missi {
egen `var'_missi= count(`var') if `var'!=.
}


collapse (sum) educacion2 p8572s1 alimento_escolar2 (count) educacion2_missi p8572s1_missi alimento_escolar2_missi , by (directorio secuencia_encuesta) 

foreach var in $missi {
replace `var'=. if `var'_missi==0
drop `var'_missi
}


tempfile edu2 
save `edu2' , replace

restore 

**Se hace el merge con la base de hogares**

mmerge directorio secuencia_encuesta using `edu2' , t(1:1) 
	
drop _merge

/*DADO QUE EN LOS RUBROS DE EDUCACION SE OBSERVAN GASTOS DE ALIMENTACION EN LOS PLANTELES EDUCATIVOS,
ESTOS RUBROS SE INCLUYEN EN ALIMENTOS FUERA DEL HOGAR*/

egen aux_gasto_alifuera= rowtotal( gasto_alifuera alimento_escolar alimento_escolar2), missing
replace gasto_alifuera=aux_gasto_alifuera
drop aux_gasto_alifuera

**Se reemplaza la variable gasto_ali considerando el nuevo cambio de la variable gasto_alifuera
drop gasto_ali
egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*---------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*---------------------------------*

egen gasto_tcomb= rowtotal(p8758s1 p8905s5a2), missing

*---------------------------------------------*
* GASTO EN TRANSPORTE SERVICIOS DE TRANSPORTE *
*---------------------------------------------*

egen gasto_tserv= rowtotal (p8756s1 p8787s1 p8795s1 p6154s7a1 p8602s1 p8905s3a2 p8915s8a2 p8920s7a2 p8572s1), missing


*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

egen gasto_totros= rowtotal(p8759s1 p8905s6a2 p8905s12a2 p8920s11a2 p8799s1 ), missing


*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

egen gasto_tman=rowtotal(p8784s1 p8915s5a2), missing


*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

egen gasto_tadq= rowtotal(p8796s1 p8920s8a2), missing  

**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga=.

*---------------------------------*
* GASTO EN GAS LICUADO-TRANSPORTE * 
*---------------------------------*

gen gasto_tlp=.

*-----------------*
* GASTO EN DIESEL * 
*-----------------*

gen gasto_tdie=.

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


*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans= rowtotal (gasto_tserv gasto_tcomb gasto_totros gasto_tman gasto_tadq), missing


*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

/*se divide el valor del pago por servicio de telefono fijo en los meses para los que corresponde este pago,
con el fin de obtener un valor mensual*/

replace p5330= p5330/p5330s1

egen gasto_com= rowtotal( p5684s1 p8775s1 p8777s1 p8765s1 p8808s1 p5330 p8910s10a2 p8910s12a2 p8910s16a2 p8920s20a2), missing  



*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

**Se suma el gasto en educacion y el gasto en recreación**

egen gasto_rec= rowtotal (p8761s1 p8762s1 p8773s1 p8785s1 p8788s1 p8869s1 p8805s1 p8871s1 p8870s1  p8809s1 p8807s1 p8806s1 p5686s1 p8905s8a2 p8905s9a2 p8910s8a2 p8915s6a2 p8915s9a2 p8915s10a2 p8920s17a2 p8920s18a2 p8920s19a2 p8920s21a2 p8920s23a2 p8920s24a2 p8920s25a2), missing 

egen gasto_edre= rowtotal(gasto_rec educacion1 educacion2), missing


*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*

egen gasto_otros= rowtotal(p8768s1 p8766s1 p8772s1 p8794s1  p8801s1 p8802s1 p8910s1a2 p8910s3a2 p8910s7a2 p8920s6a2 p8920s13a2 p8920s14a2 ), missing  
 
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

 
keep directorio secuencia_encuesta secuencia_p fex_c cant_miembros gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**

gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "COL"
label var pais "Pais" 
gen anio=2014
label var anio "Anio de la encuesta" 
gen encuesta="ENCV"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename cant_miembros miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename fex_c factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros directorio secuencia_p secuencia_encuesta

		  
saveold "$data_arm\COL_ENCV_2014_2.dta" , replace v(12)


	
	
