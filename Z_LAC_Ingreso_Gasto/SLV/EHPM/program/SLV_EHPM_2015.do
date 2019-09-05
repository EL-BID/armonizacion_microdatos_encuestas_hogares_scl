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

2) Fecha: Mayo 2017

3) Objetivo: Armonizar encuestas de hogares en América Latina y el Caribe para 
             analizar el riesgo climático
			 
4) Pregunta de 
   investigación: Cómo mejores programas de protección social pueden ayudar
                  a los hogares a hacer frente a los choques climáticos y a 
				  proteger el desarrollo socioeconómico en la región frente 
				  a los impactos del cambio climático

5) País: El Salvador

6) Encuesta: Encuesta de Hogares de Propositos Multiples- EHPM

7) Ano: 2015

8) Inputs: 	$RAW\SLV_2015a.dta 
            $RAW\sec08a.dta
			$RAW\sec08b.dta
			$RAW\sec08c.dta
			$RAW\sec08d.dta
 
 
6) Outputs: $data_arm\SLV_EHPM_2015.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:

global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\SLV\EHPM\RAW"
global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\SLV\EHPM\data_arm"
	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/

*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\SLV_2015a.dta" , clear 
	

 preserve 
 
 use "$RAW\SLV_2015a.dta" , clear
 
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*

**Composicion del ingreso monetario**
 
*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r424=r424*30 	if r423==1
replace r424=r424*4.3 	if r423==2
replace r424=r424*2 	if r423==3
replace r424=r424 		if r423==4 | r423==5

replace r42501a= r42501a*r42501b/12 
replace r42502a= r42502a*r42502b/12 
replace r42503a= r42503a*r42503b/12 
replace r42504a= r42504a*r42504b/12 
replace r42511a= r42511a*r42511b/12 
replace r44511=  r44511/12

*Para el caso de los trabajadores independientes, se le restan los costos para obtener sus ingresos netos**
replace r429= r429*-1
egen ing_neto= rowtotal(r428 r429), missing
replace ing_neto=0 if ing_neto<0

**Se mensualizan los valores correpsondientes al ingreso neto de los independientes**

replace ing_neto=ing_neto*30 if r427==1
replace ing_neto=ing_neto*4.3 if r427==2
replace ing_neto=ing_neto*2 if r427==3
replace ing_neto=ing_neto if r427==4 | r427==9
replace ing_neto=ing_neto/2 if r427==5
replace ing_neto=ing_neto/3 if r427==6
replace ing_neto=ing_neto/6 if r427==7
replace ing_neto=ing_neto/12 if r427==8

**Se mensualizan las otras variables asociadas al ingreso monetario**

replace r43501a=r43501a*r43501b/12 
replace r43502a=r43502a*r43502b/12 
replace r43503a=r43503a*r43503b/12 
replace r43504a=r43504a*r43504b/12 
replace r43511a=r43511a*r43511b/12 


egen ing_lab_mon = rowtotal(r424 r42501a r42502a r42503a r42504a r42511a ing_neto r43501a r43502a r43503a r43504a r43511a r434 r44511), missing

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r44404a  =r44404a*r44404b/12
replace r44405a  =r44405a*r44405b/12
replace r44406a =r44406a*r44406b/12
replace r44508=r44508/12

egen ing_ren_mon=rowtotal(r44404a r44405a r44406a r44508), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r44402a= r44402a*r44402b/12
replace r44410a = r44410a*r44410b/12
replace r44411a= r44411a*r44411b/12
replace r44501= r44501/12
replace r44502= r44502/12
replace r44503= r44503/12
replace r44504= r44504/12
replace r44505= r44505/12
replace r44507= r44507/12
replace r44408a=r44408a*r44408b/12
replace r44403a=r44403a*r44403b/12
replace r44510 =r44510/12

egen ing_trspri_mon= rowtotal(r44402a r44410a r44411a r44501 r44502 r44503 r44504 r44505 r44507 r44408a r44403a r44510), missing

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r44506 =r44506/12


egen ing_ct_mon= rowtotal(r44506), missing

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r44407a= r44407a*r44407b/12
replace r44409a= r44409a*r44409b/12

egen ing_trsgob_mon= rowtotal(r44407a r44409a), missing

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r44401a= r44401a*r44401b/12
replace r44509= r44509/12


egen ing_rem_mon= rowtotal(r44401a r44509 ), missing

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r44512 =r44512/12

gen ing_otro_mon= r44512

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

**Se mensualizan las variables asociadas a este rubro**

replace r42505a=r42505a*r42505b/12 
replace r42506a=r42506a*r42506b/12 
replace r42507a=r42507a*r42507b/12 
replace r42508a=r42508a*r42508b/12 
replace r42509a=r42509a*r42509b/12 
replace r42510a=r42510a*r42510b/12 
replace r42512a=r42512a*r42512b/12 

replace r43505a=r43505a*r43505b/12 
replace r43506a=r43506a*r43506b/12 
replace r43507a=r43507a*r43507b/12 
replace r43508a=r43508a*r43508b/12 
replace r43509a=r43509a*r43509b/12 
replace r43510a=r43510a*r43510b/12 
replace r43512a=r43512a*r43512b/12 


egen ing_lab_nomon = rowtotal(r42505a r42506a r42507a r42508a r42509a r42510a r42511a  r43505a r43506a r43507a r43508a r43509a r43510a r43511a r431), missing


*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

**Se mensualizan las variables asociadas a este rubro**

replace r706e=r706e/12
gen ing_trspri_nomon=r706e

*-----------------------*
* INGRESO NO MONETARIO  * 
*-----------------------*

egen  ing_nomon= rowtotal( ing_lab_nomon  ing_trspri_nomon), missing


**Se crea la variable de miembros del hogar a partir de la relación con el jefe de hogar**

gen relacion=.
replace relacion=1 if  r103==1
replace relacion=2 if  r103==2
replace relacion=3 if  r103==3
replace relacion=4 if  r103>=4 &  r103<=9
replace relacion=5 if  r103==10 
replace relacion=6 if  r103==11

label variable relacion "Relacion con el jefe del hogar"
label define relacion 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion 6 "Empleado/a domestico/a", add
label value relacion relacion_ci

**Los miembros del hogar es la suma de individuos que componen el hogar, excluyenndo empleados domesticos y otros no parientes**

gen miembros= .

replace miembros=1 if relacion<5

/*Para los ingresos del hogar, unicamente se considera la suma de los ingresos de aquellos individuos que conforman el hogar,
de esta manera, no se considera en la suma aquellos ingresos de empleados domesticos u otros no parientes. Se pone un missing value a todos los ingresos de estas personas
para que la suma final del ingreso del hogar no se vea afectada*/

global nomiembros "ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon"

foreach var in $nomiembros {
replace `var'=. if miembros==.
}
**Se deja una observacion agregada por hogar**

**Para conservar los missing values**
global miss " ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon  ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon"
foreach var in $miss {
egen `var'_miss= count(`var') if `var'!=.
}


collapse (sum)  miembros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon  ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon (mean) ing_trspri_nomon (count) ing_mon_miss ing_lab_mon_miss ing_ren_mon_miss ing_trspri_mon_miss ing_ct_mon_miss ing_trsgob_mon_miss ing_rem_mon_miss ing_otro_mon_miss ing_nomon_miss ing_lab_nomon_miss, by(idboleta)


foreach var in $miss {
replace `var'=. if `var'_miss==0
drop `var'_miss
}

/*Se hace el merge con la base anterior a nivel del hogar, para sumarle a los ingresos aquellos
ingresos que se encuentran a nivel del hogar*/

tempfile ingresos
	save `ingresos' , replace
	restore 

mmerge idboleta using `ingresos' , t(n:1)
drop _merge

***Se le suman a las variables pertinentes los ingresos a nivel del hogar: autoconsumo y beneficios de programas sociales**


*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

** Se asume que para aquellos programas en los que no se pregunta por la frecuencia, su frecuencia es anual**

replace r902=r902/12
replace r907= r907/12


/*Se asume que en el caso de los bonos, la frecuencia equivalente a 1 y 2 corresponde
a 1 vez al ano y a dos veces al ano respectivamente*/

replace r904=5 if r904otr==2
replace r905=r905/2 if r904==2 
replace r905=r905/3 if r904==3
replace r905=r905/12 if r904==4 
replace r905=r905/6 if r904==5

replace r910=r910*30 if r909==1
replace r910=r910*4.33 if r909==2
replace r910=r910*2 if r909==3
replace r910=r910/3 if r909==5
replace r910=r910/6 if r909==6
replace r910=r910/12 if r909==7

replace r910_02=r910*30 if r909_02==1
replace r910_02=r910*4.33 if r909_02==2
replace r910_02=r910*2 if r909_02==3
replace r910_02=r910/3 if r909_02==5
replace r910_02=r910/6 if r909_02==6
replace r910_02=r910/12 if r909_02==7

replace r913=r913/12 if r912==1
replace r913=r913/6 if r912==1
replace r913=r913/4 if r912==1

replace r915=r915/12

replace r918=r918/2 if r917==2

egen aux= rowtotal(ing_trsgob_mon r905 r902 r907 r910 r910_02 r913 r915 r918 ), missing
replace ing_trsgob_mon= aux
drop aux


*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

replace r921=r921/2 if r920==2
replace r921=r921/3 if r920==3
**Se asume que la otra frecuencia--(1) es equivalente a una vez al ano.
replace r921=r921/12 if r920==4

replace r924=r924/2 if r923==2
replace r924=r924/3 if r923==3
**Se asume que la otra frecuencia--(1) es equivalente a una vez al ano.
replace r924=r924/12 if r923==4

egen aux= rowtotal(ing_ct_mon r921 r924), missing
replace ing_ct_mon= aux
drop aux
*--------------------*
* INGRESO MONETARIO  *  
*--------------------*

egen aux= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon), missing
replace ing_mon=aux
drop aux



*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

gen ing_ren_nomon = .


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

*-----------------------*
* INGRESO NO MONETARIO  * 
*-----------------------*

egen aux= rowtotal(ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ), missing
replace ing_nomon= aux
drop aux

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


**Se deja una sola observacion por hogar y las variables necesarias**

bys idboleta: gen id=_n
keep if id==1

keep idboleta miembros ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro ing_tpriv ing_tpub

**Se guarda la base de datos de ingresos**

saveold "$RAW\ingresos.dta" , replace v(12)



/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	**Se agrega la variable construida**

	
          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
		  
*Se utilizan las bases ubicadas en la carpeta RAW*

	use "$RAW\SLV_2015a.dta" , clear 
	

	
**Se encuentra el gasto en transporte con motivo de estudio**
bys idboleta: egen transporte_estudio=sum(r224a2), missing
bys idboleta: egen aux= max(transporte_estudio)
replace transporte_estudio= aux
drop aux

bys idboleta: egen transporte_estudio_otro=sum(r224a4), missing
bys idboleta: egen aux= max(transporte_estudio_otro)
replace transporte_estudio_otro= aux
drop aux

egen transporte_educacion= rowtotal(transporte_estudio transporte_estudio_otro), missing


**Se encuentra el gasto en transporte con motivo de empleo**
replace r45101=r45101*r45001
replace r45101=r45101*4.33

bys idboleta: egen transporte_empleo=sum(r45101), missing
bys idboleta: egen aux= max(transporte_empleo)
replace transporte_empleo= aux
drop aux


	/*Teniendo en cuenta que a partir de este punto unicamente se van a utilizar variables
	que se encuentran a nivel de hogar, se deja una unica obervacion por hogar*/

bys folio lote idboleta: gen id=_n
keep if id==1

**Se hace el merge con la base de datos de ingresos**

mmerge idboleta using "$RAW\ingresos.dta", t(1:1)

	**Se usa la base de datos de consumo de alimentos en el hogar**	  
preserve

	use "$RAW\sec08a.dta" , clear 
	
	/*	____________________________________________________________________________
Se usan las 4 bases de datos de gastos del hogar. En cada una de ellas
Se crea una variable por cada rubro o tipo de gasto para posteriormente ubicarlas
en las categorías especificadas. Al final, Se juntan las 4 bases de datos para 
conformar una base unica que contenga las variables de interes.
	____________________________________________________________________________	
*/


*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

bys idboleta: egen gasto_alihogar= sum(gastomensual) 


**Se dejan unicamente las variables de interés y una observación por hogar**

bys idboleta: gen id=_n
keep if id==1

keep idboleta gasto_alihogar

**Se hace el merge con la base anterior**

tempfile gastos_alimentos
	save `gastos_alimentos' , replace
	restore 

mmerge idboleta using `gastos_alimentos' , t(1:1)
drop _merge

preserve


**Se usa la base de datos de gastos mensuales, se construye una variable por cada tipo de gasto**

use "$RAW\sec08b.dta" , clear 

bys idboleta: egen otros= sum(gastomensual) if r808a==1 | r808a==6 | r808a==11 | r808a==12 | r808a==13 | r808a==14
bys idboleta: egen aux= max(otros)
replace otros= aux
drop aux

bys idboleta: egen enseres= sum(gastomensual) if r808a==2 | r808a==3
bys idboleta: egen aux= max(enseres)
replace enseres= aux
drop aux

bys idboleta: egen cultura= sum(gastomensual) if r808a==4 | r808a==5
bys idboleta: egen aux= max(cultura)
replace cultura= aux
drop aux

bys idboleta: egen ropa= sum(gastomensual) if r808a==7
bys idboleta: egen aux= max(ropa)
replace ropa= aux
drop aux

bys idboleta: egen combustible= sum(gastomensual) if r808a==8
bys idboleta: egen aux= max(combustible)
replace combustible= aux
drop aux

bys idboleta: egen servicio_trans= sum(gastomensual) if r808a==9
bys idboleta: egen aux= max(servicio_trans)
replace servicio_trans= aux
drop aux

bys idboleta: egen renta= sum(gastomensual) if r808a==10
bys idboleta: egen aux= max(renta)
replace renta= aux
drop aux

**Se dejan unicamente las variables de interés y una observación por hogar**

bys idboleta: gen id=_n
keep if id==1

keep idboleta otros ropa enseres renta combustible servicio_trans cultura

**Se hace el merge con la base anterior**

tempfile gastos_mes
	save `gastos_mes' , replace
	restore 

mmerge idboleta using `gastos_mes' , t(1:1)
drop _merge

preserve

**Se usa la base de datos de gastos semestrales, se construye una variable por cada tipo de gasto**

use "$RAW\sec08c.dta" , clear 

bys idboleta: egen ropa2= sum(gastomensual) if r808a==1 | r808a==2
bys idboleta: egen aux= max(ropa2)
replace ropa2= aux
drop aux

bys idboleta: egen enseres2= sum(gastomensual) if r808a==3 | r808a==4 | r808a==5| r808a==6 | r808a==10 | r808a==11
bys idboleta: egen aux= max(enseres2)
replace enseres2= aux
drop aux

bys idboleta: egen reparacion_vehiculo= sum(gastomensual) if r808a==7
bys idboleta: egen aux= max(reparacion_vehiculo)
replace reparacion_vehiculo= aux
drop aux

bys idboleta: egen compra_vehiculo= sum(gastomensual) if r808a==8
bys idboleta: egen aux= max(compra_vehiculo)
replace compra_vehiculo= aux
drop aux

bys idboleta: egen otros_transporte= sum(gastomensual) if r808a==9
bys idboleta: egen aux= max(otros_transporte)
replace otros_transporte= aux
drop aux

bys idboleta: egen servicio_trans2= sum(gastomensual) if r808a==13 | r808a==14
bys idboleta: egen aux= max(servicio_trans2)
replace servicio_trans2= aux
drop aux

bys idboleta: egen otros2= sum(gastomensual) if r808a==15
bys idboleta: egen aux= max(otros2)
replace otros2= aux
drop aux
	
**Se dejan unicamente las variables de interés y una observación por hogar**

bys idboleta: gen id=_n
keep if id==1

keep idboleta otros2 ropa2 enseres2 servicio_trans2 otros_transporte compra_vehiculo reparacion_vehiculo

**Se hace el merge con la base anterior de gastos**

tempfile gastos_semestral
	save `gastos_semestral' , replace
	restore 

mmerge idboleta using `gastos_semestral' , t(1:1)
drop _merge

preserve

**Se usa la base de datos de gastos anuales, se construye una variable por cada tipo de gasto**

use "$RAW\sec08d.dta" , clear 

bys idboleta: egen otros3= sum(gastomensual) if r808a==4 |  r808a==6 |  r808a==8 |  r808a==9 |  r808a==10
bys idboleta: egen aux= max(otros3)
replace otros3= aux
drop aux

**Se dejan unicamente las variables de interés y una observación por hogar**

bys idboleta: gen id=_n
keep if id==1

keep idboleta otros3

**Se hace el merge con la base anterior de gastos**

tempfile gastos_anual
	save `gastos_anual' , replace
	restore 

mmerge idboleta using `gastos_anual' , t(1:1)
drop _merge


*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

gen gasto_alifuera=.

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

gen gasto_alta=.

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing


*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

egen gasto_veca= rowtotal(ropa ropa2), missing

*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

egen gasto_viv=rowtotal(r33101b r33101b1 r33101b2 r33102b r33104b r33107b r33106b r33103b r33105b renta r33116b r33115b r308c r33117b ), missing

*---------------*
* GASTO EN AGUA *
*---------------*

egen gasto_vag= rowtotal(r33101b r33101b1 r33101b2 ), missing

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

**Se incluye gasto en carga de bateria**

egen gasto_vele= rowtotal(r33102b r33107b), missing

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

gen gasto_vk= r33103b 


*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

**El gas propano es equivalente al gas licuado de petroleo**

gen gasto_vlp=r33104b 

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle= r33106b 


*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

gen gasto_vot= r33105b


*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

**se crea la variable gasto_vcon**

egen gasto_vcon=rowtotal(renta r33116b r33115b r308c r33117b), missing

**Se crean variables de vivienda, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

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

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*	

gen gasto_vgn=.

*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing

*----------------*
* GASTO EN CARBON*
*----------------*

gen gasto_vca=.

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 


*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

egen gasto_ens= rowtotal(enseres enseres2), missing

*----------------*
* GASTO EN SALUD *
*----------------*

gen gasto_sal= gmsa

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans= rowtotal(combustible servicio_trans servicio_trans2 otros_transporte reparacion_vehiculo compra_vehiculo transporte_empleo transporte_educacion), missing

*------------------------------*
* GASTO EN COMBUSTIBLE-TRANSPORTE *
*------------------------------*

gen gasto_tcomb=combustible 

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

egen gasto_tserv= rowtotal(servicio_trans servicio_trans2 transporte_empleo transporte_educacion), missing

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

gen gasto_totros= otros_transporte

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

gen gasto_tman=reparacion_vehiculo 

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

gen gasto_tadq= compra_vehiculo

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


*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

egen gasto_com= rowtotal( r33113b r33112b r33111b r33110b r33109b r33108b), missing

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

**Se le resta el gasto en transporte por motivo de estudio al gasto en educacion**

gen gasto_edu=gmed
replace gasto_edu= gmed-transporte_educacion if transporte_educacion!=.


egen gasto_edre= rowtotal(gasto_edu cultura), missing

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	

**Se le resta el gasto en transporte por motivo de empleo al gasto en empleo**

gen gasto_empleo= gmem
replace gasto_empleo= gmem-transporte_empleo if transporte_empleo!=.
egen gasto_otros= rowtotal(otros otros2 otros3 gasto_empleo)

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

 
keep idboleta miembros fac00 gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**

gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "SLV"
label var pais "Pais" 
gen anio=2015
label var anio "Anio de la encuesta" 
gen encuesta="ENGHO"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename miembros miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename fac00 factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros idboleta

		  	  
saveold "$data_arm\SLV_EHPM_2015.dta" , replace v(12)
