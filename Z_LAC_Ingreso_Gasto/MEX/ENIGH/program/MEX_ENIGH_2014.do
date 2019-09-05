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

5) País: Mexico

6) Encuesta: Encuesta Nacional de Ingresos y Gastos de los Hogares-ENIGH

7) Ano: 2014

8) Inputs: 	$RAW\concentrado.dta 
            $RAW\gastohogar.dta  
            $RAW\gastopersona.dta  
            $RAW\ingresos.dta  
            $RAW\trabajos.dta   
 
6) Outputs: $data_arm\MEX_ENIGH_2014.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)
				  
8) Notas: Se replica dofile de Comeval- medicion de la pobreza_2014
          Se deflactan los rubros a pesos del 2014
				
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\MEX\ENIGH\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\MEX\ENIGH\data_arm"


/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/


          *=================================*
          * 1.0. INGRESO MONETARIO          *
          *=================================*

/*Para la construcción del ingreso corriente del hogar es necesario utilizar
información sobre la condición de ocupación y los ingresos de los individuos.
Se utiliza la información contenida en la base "trabajo.dta" para identificar a
la población ocupada que declara tener como prestación laboral aguinaldo, ya sea 
por su trabajo principal o secundario, a fin de incorporar los ingresos por este 
concepto en la medición*/


use "$RAW\trabajos.dta", clear

keep folioviv foliohog numren id_trabajo pres_2
destring pres_2 id_trabajo, replace
reshape wide pres_2, i(folioviv foliohog numren) j(id_trabajo)

gen trab=1

label var trab "Población con al menos un empleo"

gen aguinaldo1=.
replace aguinaldo1=1 if pres_21==2
recode aguinaldo1 (.=0)

gen aguinaldo2=.
replace aguinaldo2=1 if pres_22==2 
recode aguinaldo2 (.=0)

label var aguinaldo1 "Aguinaldo trabajo principal"
label define aguinaldo 0 "No dispone de aguinaldo" 1 "Dispone de aguinaldo"					   
label value aguinaldo1 aguinaldo

label var aguinaldo2 "Aguinaldo trabajo secundario"
label value aguinaldo2 aguinaldo

keep folioviv foliohog numren aguinaldo1 aguinaldo2 trab

sort folioviv foliohog numren 

saveold "$RAW\aguinaldo.dta", replace v(12)


*Ahora se incorpora a la base de ingresos*

use "$RAW\ingresos.dta", clear

sort  folioviv foliohog numren

merge  folioviv foliohog numren using "$RAW\aguinaldo.dta"

tab _merge
drop _merge

sort folioviv foliohog numren

/*eliminar si la clave de ingreso es el aguinaldo del ejercicio 2013 y
aguinaldo 1 o 2 es igual a O, es decir, la persona no dispone de aguinaldo*/

drop if (clave=="P009" & aguinaldo1!=1)
drop if (clave=="P016" & aguinaldo2!=1)


/*Una vez realizado lo anterior, se procede a deflactar el ingreso monetario recibido
por los hogares a precios de agosto de 2014. Para ello, se utilizan las 
variables meses, las cuales toman los valores 2 a 10 e indican el mes en
que se recibió el ingreso respectivo*/

*Definición de los deflactores 2014*

scalar	dic13	=	0.9829863009	
scalar	ene14	=	0.9917752429	
scalar	feb14	=	0.9942876285	
scalar	mar14	=	0.9970115834	
scalar	abr14	=	0.9951515365	
scalar	may14	=	0.9919691814	
scalar	jun14	=	0.9936881821	
scalar	jul14	=	0.9964209524	
scalar	ago14	=	1.0000000000	
scalar	sep14	=	1.0044165095	
scalar	oct14	=	1.0099702040	
scalar	nov14	=	1.0181156226	
scalar	dic14	=	1.0186836881	

destring mes_*, replace

replace ing_6=ing_6/feb14 if mes_6==2
replace ing_6=ing_6/mar14 if mes_6==3
replace ing_6=ing_6/abr14 if mes_6==4
replace ing_6=ing_6/may14 if mes_6==5

replace ing_5=ing_5/mar14 if mes_5==3
replace ing_5=ing_5/abr14 if mes_5==4
replace ing_5=ing_5/may14 if mes_5==5
replace ing_5=ing_5/jun14 if mes_5==6

replace ing_4=ing_4/abr14 if mes_4==4
replace ing_4=ing_4/may14 if mes_4==5
replace ing_4=ing_4/jun14 if mes_4==6
replace ing_4=ing_4/jul14 if mes_4==7

replace ing_3=ing_3/may14 if mes_3==5
replace ing_3=ing_3/jun14 if mes_3==6
replace ing_3=ing_3/jul14 if mes_3==7
replace ing_3=ing_3/ago14 if mes_3==8

replace ing_2=ing_2/jun14 if mes_2==6
replace ing_2=ing_2/jul14 if mes_2==7
replace ing_2=ing_2/ago14 if mes_2==8
replace ing_2=ing_2/sep14 if mes_2==9

replace ing_1=ing_1/jul14 if mes_1==7
replace ing_1=ing_1/ago14 if mes_1==8
replace ing_1=ing_1/sep14 if mes_1==9
replace ing_1=ing_1/oct14 if mes_1==10


/*Se deflactan las claves P008 y P015 (Reparto de utilidades) 
y P009 y P016 (aguinaldo) con los deflactores de mayo a agosto 2014 
y de diciembre de 2013 a agosto 2014, respectivamente y se obtiene el promedio mensual*/

replace ing_1=(ing_1/may14)/12 if clave=="P008" | clave=="P015"
replace ing_1=(ing_1/dic13)/12 if clave=="P009" | clave=="P016"

recode ing_2 ing_3 ing_4 ing_5 ing_6 (0=.) if clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016"

/**Una vez realizada la deflactación, se procede a obtener el 
ingreso mensual promedio en los últimos seis meses, para 
cada persona y clave de ingreso*/

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)


          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*
		  
**Composicion del ingreso monetario**
		  
*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

gen double ing_lab_mon=ing_mens if (clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") | (clave>="P018" & clave<="P022") | (clave>="P067" & clave<="P081")

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

gen double ing_ren_mon=ing_mens if (clave>="P023" & clave<="P031")

*----------------------------------------------------*
* INGRESOS POR TRASNFERENCIAS PRIVADAS
*----------------------------------------------------*

gen double ing_trspri_mon=ing_mens if (clave>="P034" & clave<="P037") | (clave>="P039" & clave<="P040")

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

gen double ing_ct_mon=ing_mens if (clave>="P042" & clave<="P048")

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

gen double ing_trsgob_mon=ing_mens if (clave=="P032") | (clave=="P038")

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen double ing_rem_mon=ing_mens if (clave=="P041") | (clave=="P033")

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

gen ing_otro_mon=.

*-------------------*
* INGRESO MONETARIO * 
*-------------------*

gen double ing_mon=ing_mens if (clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016")| (clave>="P018" & clave<="P048") | (clave>="P067" & clave<="P081")


**Se estima el total de ingresos de cada  hogar**

collapse (sum) ing_mon ing_lab_mon ing_ren_mon ing_ct_mon ing_trsgob_mon ing_trspri_mon ing_rem_mon ing_otro_mon, by(folioviv foliohog)
							 
sort folioviv foliohog

saveold "$RAW\ingreso_deflactado.dta", replace v(12)



         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*


**Se obtiene el ingreso no monetario a partir del gasto no monetario**

use "$RAW\gastohogar.dta", clear
gen base=1
append using "$RAW\gastopersona.dta"
recode base (.=2)

label var base "Origen del monto"
label define base 1 "Monto del hogar" 2 "Monto de personas"
label value base base

/*En el caso de la información de gasto no monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la octava posición del 
folio de la vivienda. En primer lugar se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folioviv,8,1))

*Definición de los deflactores*

*Rubro 1.1 semanal, Alimentos*
scalar d11w07=	0.9901303261	
scalar d11w08=	1.0000000000	
scalar d11w09=	1.0095550367	
scalar d11w10=	1.0117409376	
scalar d11w11=	1.0192922318	

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco*
scalar d12w07=	0.9953130285	
scalar d12w08=	1.0000000000	
scalar d12w09=	1.0018441864	
scalar d12w10=	1.0031971266	
scalar d12w11=	1.0026011887	

*Rubro 2 trimestral, Vestido, calzado y accesorios*
scalar d2t05=	0.9937183977	
scalar d2t06=	0.9952148881	
scalar d2t07=	0.9984303614	
scalar d2t08=	1.0044224187	

*Rubro 3 mensual, Vivienda*
scalar d3m07=	0.9997777670	
scalar d3m08=	1.0000000000	
scalar d3m09=	1.0003671675	
scalar d3m10=	1.0150731919	
scalar d3m11=	1.0376346683	

*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar*
scalar d42m07=	0.9971952310	
scalar d42m08=	1.0000000000	
scalar d42m09=	1.0002691884	
scalar d42m10=	0.9973081165	
scalar d42m11=	0.9989492971	

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar*
scalar d42t05=	0.9977191352	
scalar d42t06=	0.9980635806	
scalar d42t07=	0.9991548065	
scalar d42t08=	0.9991924349	

*Rubro 4.1 semestral, Muebles y aparatos dómesticos*
scalar d41s02=	0.9952434028	
scalar d41s03=	0.9949403103	
scalar d41s04=	0.9946616608	
scalar d41s05=	0.9951032633	

*Rubro 5.1 trimestral, Salud*
scalar d51t05=	0.9944286156	
scalar d51t06=	0.9972835892	
scalar d51t07=	0.9995669908	
scalar d51t08=	1.0015184188	

*Rubro 6.1.1 semanal, Transporte público urbano*
scalar d611w07=	0.9987640919	
scalar d611w08=	1.0000000000	
scalar d611w09=	1.0008100209	
scalar d611w10=	1.0015448852	
scalar d611w11=	1.0027807933	

*Rubro 6 mensual, Transporte*
scalar d6m07=	0.9996805033	
scalar d6m08=	1.0000000000	
scalar d6m09=	1.0023265911	
scalar d6m10=	1.0054068667	
scalar d6m11=	1.0051119467	

*Rubro 6 semestral, Transporte*
scalar d6s02=	0.9903700427	
scalar d6s03=	0.9935445288	
scalar d6s04=	0.9964773444	
scalar d6s05=	0.9991370859	

*Rubro 7 mensual, Educación y esparcimiento*
scalar d7m07=	0.9998214971	
scalar d7m08=	1.0000000000	
scalar d7m09=	1.0121560472	
scalar d7m10=	1.0141642048	
scalar d7m11=	1.0154762011	

*Rubro 2.3 mensual, Accesorios y cuidados del vestido*
scalar d23m07=	0.9931651984	
scalar d23m08=	1.0000000000	
scalar d23m09=	1.0016681244	
scalar d23m10=	1.0051576603	
scalar d23m11=	1.0059240958	

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido*
scalar d23t05=	0.9945267486	
scalar d23t06=	0.9963391432	
scalar d23t07=	0.9982777743	
scalar d23t08=	1.0022752616	

*INPC semestral*
scalar dINPCs02=	0.9947548441	
scalar dINPCs03=	0.9957069060	
scalar dINPCs04=	0.9969410603	
scalar dINPCs05=	0.9994108382	

*Una vez definidos los deflactores, se seleccionan los rubros*

gen double gasnomon=gas_nm_tri/3

gen esp=1 if tipo_gasto=="G4"
gen reg=1 if tipo_gasto=="G5"
replace reg=1 if tipo_gasto=="G6"
gen autoc=1 if tipo_gasto=="G3"
gen est_alq=1 if tipo_gasto=="G7"

*Control para la frecuencia de los regalos recibidos por el hogar*
drop if ((frecu>="5" & frecu<="6") | frecu=="" | frecu=="0") & base==1 & tipo_gasto=="G5"

*Control para la frecuencia de los regalos recibidos por persona;

drop if ((frecu>="11" & frecu<="12") | frecu=="") & base==2 & tipo_gasto=="G5"

*------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS *
*------------------------------*

gen gasto_ali_nm=gasnomon if (clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247")

replace gasto_ali_nm=gasto_ali_nm/d11w08 if decena==0
replace gasto_ali_nm=gasto_ali_nm/d11w08 if decena==1
replace gasto_ali_nm=gasto_ali_nm/d11w08 if decena==2
replace gasto_ali_nm=gasto_ali_nm/d11w09 if decena==3
replace gasto_ali_nm=gasto_ali_nm/d11w09 if decena==4
replace gasto_ali_nm=gasto_ali_nm/d11w09 if decena==5
replace gasto_ali_nm=gasto_ali_nm/d11w10 if decena==6
replace gasto_ali_nm=gasto_ali_nm/d11w10 if decena==7
replace gasto_ali_nm=gasto_ali_nm/d11w10 if decena==8
replace gasto_ali_nm=gasto_ali_nm/d11w11 if decena==9

**division del gasto en alimentos**

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

gen gasto_alihogar_nm=gasnomon if (clave>="A001" & clave<="A222") | (clave=="A242")

replace gasto_alihogar_nm=gasto_alihogar_nm/d11w08 if decena==0
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w08 if decena==1
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w08 if decena==2
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w09 if decena==3
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w09 if decena==4
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w09 if decena==5
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w10 if decena==6
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w10 if decena==7
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w10 if decena==8
replace gasto_alihogar_nm=gasto_alihogar_nm/d11w11 if decena==9

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

gen gasto_alifuera_nm=gasnomon if (clave>="A243" & clave<="A247")

replace gasto_alifuera_nm=gasto_alifuera_nm/d11w08 if decena==0
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w08 if decena==1
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w08 if decena==2
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w09 if decena==3
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w09 if decena==4
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w09 if decena==5
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w10 if decena==6
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w10 if decena==7
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w10 if decena==8
replace gasto_alifuera_nm=gasto_alifuera_nm/d11w11 if decena==9

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

gen gasto_alta_nm=gasnomon if (clave>="A223" & clave<="A241")

replace gasto_alta_nm=gasto_alta_nm/d12w08 if decena==0
replace gasto_alta_nm=gasto_alta_nm/d12w08 if decena==1
replace gasto_alta_nm=gasto_alta_nm/d12w08 if decena==2
replace gasto_alta_nm=gasto_alta_nm/d12w09 if decena==3
replace gasto_alta_nm=gasto_alta_nm/d12w09 if decena==4
replace gasto_alta_nm=gasto_alta_nm/d12w09 if decena==5
replace gasto_alta_nm=gasto_alta_nm/d12w10 if decena==6
replace gasto_alta_nm=gasto_alta_nm/d12w10 if decena==7
replace gasto_alta_nm=gasto_alta_nm/d12w10 if decena==8
replace gasto_alta_nm=gasto_alta_nm/d12w11 if decena==9

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

gen gasto_veca_nm=gasnomon if (clave>="H001" & clave<="H122") |(clave=="H136")

replace gasto_veca_nm=gasto_veca_nm/d2t05 if decena==0
replace gasto_veca_nm=gasto_veca_nm/d2t05 if decena==1
replace gasto_veca_nm=gasto_veca_nm/d2t06 if decena==2
replace gasto_veca_nm=gasto_veca_nm/d2t06 if decena==3
replace gasto_veca_nm=gasto_veca_nm/d2t06 if decena==4
replace gasto_veca_nm=gasto_veca_nm/d2t07 if decena==5
replace gasto_veca_nm=gasto_veca_nm/d2t07 if decena==6
replace gasto_veca_nm=gasto_veca_nm/d2t07 if decena==7
replace gasto_veca_nm=gasto_veca_nm/d2t08 if decena==8
replace gasto_veca_nm=gasto_veca_nm/d2t08 if decena==9


*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

gen gasto_viv_nm=gasnomon if (clave>="G001" & clave<="G016") | (clave>="R001" & clave<="R004") | (clave=="R013")

replace gasto_viv_nm=gasto_viv_nm/d3m07 if decena==0
replace gasto_viv_nm=gasto_viv_nm/d3m07 if decena==1
replace gasto_viv_nm=gasto_viv_nm/d3m08 if decena==2
replace gasto_viv_nm=gasto_viv_nm/d3m08 if decena==3
replace gasto_viv_nm=gasto_viv_nm/d3m08 if decena==4
replace gasto_viv_nm=gasto_viv_nm/d3m09 if decena==5
replace gasto_viv_nm=gasto_viv_nm/d3m09 if decena==6
replace gasto_viv_nm=gasto_viv_nm/d3m09 if decena==7
replace gasto_viv_nm=gasto_viv_nm/d3m10 if decena==8
replace gasto_viv_nm=gasto_viv_nm/d3m10 if decena==9


**Division del gasto en vivienda, servicios de conservacion y combustible**

*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

gen gasto_vcon_nm=gasnomon if (clave>="G001" & clave<="G008") |(clave=="R004") | (clave=="R013")

replace gasto_vcon_nm=gasto_vcon_nm/d3m07 if decena==0
replace gasto_vcon_nm=gasto_vcon_nm/d3m07 if decena==1
replace gasto_vcon_nm=gasto_vcon_nm/d3m08 if decena==2
replace gasto_vcon_nm=gasto_vcon_nm/d3m08 if decena==3
replace gasto_vcon_nm=gasto_vcon_nm/d3m08 if decena==4
replace gasto_vcon_nm=gasto_vcon_nm/d3m09 if decena==5
replace gasto_vcon_nm=gasto_vcon_nm/d3m09 if decena==6
replace gasto_vcon_nm=gasto_vcon_nm/d3m09 if decena==7
replace gasto_vcon_nm=gasto_vcon_nm/d3m10 if decena==8
replace gasto_vcon_nm=gasto_vcon_nm/d3m10 if decena==9

*---------------*
* GASTO EN AGUA *
*---------------*

gen gasto_vag_nm=gasnomon if (clave=="R002")

replace gasto_vag_nm=gasto_vag_nm/d3m07 if decena==0
replace gasto_vag_nm=gasto_vag_nm/d3m07 if decena==1
replace gasto_vag_nm=gasto_vag_nm/d3m08 if decena==2
replace gasto_vag_nm=gasto_vag_nm/d3m08 if decena==3
replace gasto_vag_nm=gasto_vag_nm/d3m08 if decena==4
replace gasto_vag_nm=gasto_vag_nm/d3m09 if decena==5
replace gasto_vag_nm=gasto_vag_nm/d3m09 if decena==6
replace gasto_vag_nm=gasto_vag_nm/d3m09 if decena==7
replace gasto_vag_nm=gasto_vag_nm/d3m10 if decena==8
replace gasto_vag_nm=gasto_vag_nm/d3m10 if decena==9

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

gen gasto_vele_nm=gasnomon if (clave=="R001")

replace gasto_vele_nm=gasto_vele_nm/d3m07 if decena==0
replace gasto_vele_nm=gasto_vele_nm/d3m07 if decena==1
replace gasto_vele_nm=gasto_vele_nm/d3m08 if decena==2
replace gasto_vele_nm=gasto_vele_nm/d3m08 if decena==3
replace gasto_vele_nm=gasto_vele_nm/d3m08 if decena==4
replace gasto_vele_nm=gasto_vele_nm/d3m09 if decena==5
replace gasto_vele_nm=gasto_vele_nm/d3m09 if decena==6
replace gasto_vele_nm=gasto_vele_nm/d3m09 if decena==7
replace gasto_vele_nm=gasto_vele_nm/d3m10 if decena==8
replace gasto_vele_nm=gasto_vele_nm/d3m10 if decena==9

*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

gen gasto_vlp_nm=gasnomon if (clave=="G009")

replace gasto_vlp_nm=gasto_vlp_nm/d3m07 if decena==0
replace gasto_vlp_nm=gasto_vlp_nm/d3m07 if decena==1
replace gasto_vlp_nm=gasto_vlp_nm/d3m08 if decena==2
replace gasto_vlp_nm=gasto_vlp_nm/d3m08 if decena==3
replace gasto_vlp_nm=gasto_vlp_nm/d3m08 if decena==4
replace gasto_vlp_nm=gasto_vlp_nm/d3m09 if decena==5
replace gasto_vlp_nm=gasto_vlp_nm/d3m09 if decena==6
replace gasto_vlp_nm=gasto_vlp_nm/d3m09 if decena==7
replace gasto_vlp_nm=gasto_vlp_nm/d3m10 if decena==8
replace gasto_vlp_nm=gasto_vlp_nm/d3m10 if decena==9

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*

gen gasto_vgn_nm=gasnomon if (clave=="R003")

replace gasto_vgn_nm=gasto_vgn_nm/d3m07 if decena==0
replace gasto_vgn_nm=gasto_vgn_nm/d3m07 if decena==1
replace gasto_vgn_nm=gasto_vgn_nm/d3m08 if decena==2
replace gasto_vgn_nm=gasto_vgn_nm/d3m08 if decena==3
replace gasto_vgn_nm=gasto_vgn_nm/d3m08 if decena==4
replace gasto_vgn_nm=gasto_vgn_nm/d3m09 if decena==5
replace gasto_vgn_nm=gasto_vgn_nm/d3m09 if decena==6
replace gasto_vgn_nm=gasto_vgn_nm/d3m09 if decena==7
replace gasto_vgn_nm=gasto_vgn_nm/d3m10 if decena==8
replace gasto_vgn_nm=gasto_vgn_nm/d3m10 if decena==9

*-----------------*
* GASTO EN CARBON *
*-----------------*	

gen gasto_vca_nm=gasnomon if (clave=="G012")

replace gasto_vca_nm=gasto_vca_nm/d3m07 if decena==0
replace gasto_vca_nm=gasto_vca_nm/d3m07 if decena==1
replace gasto_vca_nm=gasto_vca_nm/d3m08 if decena==2
replace gasto_vca_nm=gasto_vca_nm/d3m08 if decena==3
replace gasto_vca_nm=gasto_vca_nm/d3m08 if decena==4
replace gasto_vca_nm=gasto_vca_nm/d3m09 if decena==5
replace gasto_vca_nm=gasto_vca_nm/d3m09 if decena==6
replace gasto_vca_nm=gasto_vca_nm/d3m09 if decena==7
replace gasto_vca_nm=gasto_vca_nm/d3m10 if decena==8
replace gasto_vca_nm=gasto_vca_nm/d3m10 if decena==9

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle_nm=gasnomon if (clave=="G013")

replace gasto_vle_nm=gasto_vle_nm/d3m07 if decena==0
replace gasto_vle_nm=gasto_vle_nm/d3m07 if decena==1
replace gasto_vle_nm=gasto_vle_nm/d3m08 if decena==2
replace gasto_vle_nm=gasto_vle_nm/d3m08 if decena==3
replace gasto_vle_nm=gasto_vle_nm/d3m08 if decena==4
replace gasto_vle_nm=gasto_vle_nm/d3m09 if decena==5
replace gasto_vle_nm=gasto_vle_nm/d3m09 if decena==6
replace gasto_vle_nm=gasto_vle_nm/d3m09 if decena==7
replace gasto_vle_nm=gasto_vle_nm/d3m10 if decena==8
replace gasto_vle_nm=gasto_vle_nm/d3m10 if decena==9

*-------------------*
* GASTO EN PETROLEO *
*-------------------*

gen gasto_vp_nm=gasnomon if (clave=="G010")

replace gasto_vp_nm=gasto_vp_nm/d3m07 if decena==0
replace gasto_vp_nm=gasto_vp_nm/d3m07 if decena==1
replace gasto_vp_nm=gasto_vp_nm/d3m08 if decena==2
replace gasto_vp_nm=gasto_vp_nm/d3m08 if decena==3
replace gasto_vp_nm=gasto_vp_nm/d3m08 if decena==4
replace gasto_vp_nm=gasto_vp_nm/d3m09 if decena==5
replace gasto_vp_nm=gasto_vp_nm/d3m09 if decena==6
replace gasto_vp_nm=gasto_vp_nm/d3m09 if decena==7
replace gasto_vp_nm=gasto_vp_nm/d3m10 if decena==8
replace gasto_vp_nm=gasto_vp_nm/d3m10 if decena==9

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi_nm=gasnomon if (clave=="G011")

replace gasto_vdi_nm=gasto_vdi_nm/d3m07 if decena==0
replace gasto_vdi_nm=gasto_vdi_nm/d3m07 if decena==1
replace gasto_vdi_nm=gasto_vdi_nm/d3m08 if decena==2
replace gasto_vdi_nm=gasto_vdi_nm/d3m08 if decena==3
replace gasto_vdi_nm=gasto_vdi_nm/d3m08 if decena==4
replace gasto_vdi_nm=gasto_vdi_nm/d3m09 if decena==5
replace gasto_vdi_nm=gasto_vdi_nm/d3m09 if decena==6
replace gasto_vdi_nm=gasto_vdi_nm/d3m09 if decena==7
replace gasto_vdi_nm=gasto_vdi_nm/d3m10 if decena==8
replace gasto_vdi_nm=gasto_vdi_nm/d3m10 if decena==9


*-------------------------------------------*
* GASTO EN OTROS COMBUSTIBLES PARA CALENTAR *
*-------------------------------------------*

gen gasto_vot_nm=gasnomon if (clave>="G014" & clave<="G016")

replace gasto_vot_nm=gasto_vot_nm/d3m07 if decena==0
replace gasto_vot_nm=gasto_vot_nm/d3m07 if decena==1
replace gasto_vot_nm=gasto_vot_nm/d3m08 if decena==2
replace gasto_vot_nm=gasto_vot_nm/d3m08 if decena==3
replace gasto_vot_nm=gasto_vot_nm/d3m08 if decena==4
replace gasto_vot_nm=gasto_vot_nm/d3m09 if decena==5
replace gasto_vot_nm=gasto_vot_nm/d3m09 if decena==6
replace gasto_vot_nm=gasto_vot_nm/d3m09 if decena==7
replace gasto_vot_nm=gasto_vot_nm/d3m10 if decena==8
replace gasto_vot_nm=gasto_vot_nm/d3m10 if decena==9


*--------------------------------*
* GASTO EN ARTICULOS DE LIMPIEZA *
*--------------------------------*

gen gasto_lim_nm=gasnomon if (clave>="C001" & clave<="C024")

replace gasto_lim_nm=gasto_lim_nm/d42m07 if decena==0
replace gasto_lim_nm=gasto_lim_nm/d42m07 if decena==1
replace gasto_lim_nm=gasto_lim_nm/d42m08 if decena==2
replace gasto_lim_nm=gasto_lim_nm/d42m08 if decena==3
replace gasto_lim_nm=gasto_lim_nm/d42m08 if decena==4
replace gasto_lim_nm=gasto_lim_nm/d42m09 if decena==5
replace gasto_lim_nm=gasto_lim_nm/d42m09 if decena==6
replace gasto_lim_nm=gasto_lim_nm/d42m09 if decena==7
replace gasto_lim_nm=gasto_lim_nm/d42m10 if decena==8
replace gasto_lim_nm=gasto_lim_nm/d42m10 if decena==9

*--------------------------------*
* GASTO EN CRISTALERIA Y BLANCOS *
*--------------------------------*

gen gasto_cris_nm=gasnomon if (clave>="I001" & clave<="I026")

replace gasto_cris_nm=gasto_cris_nm/d42t05 if decena==0
replace gasto_cris_nm=gasto_cris_nm/d42t05 if decena==1
replace gasto_cris_nm=gasto_cris_nm/d42t06 if decena==2
replace gasto_cris_nm=gasto_cris_nm/d42t06 if decena==3
replace gasto_cris_nm=gasto_cris_nm/d42t06 if decena==4
replace gasto_cris_nm=gasto_cris_nm/d42t07 if decena==5
replace gasto_cris_nm=gasto_cris_nm/d42t07 if decena==6
replace gasto_cris_nm=gasto_cris_nm/d42t07 if decena==7
replace gasto_cris_nm=gasto_cris_nm/d42t08 if decena==8
replace gasto_cris_nm=gasto_cris_nm/d42t08 if decena==9

*----------------------------*
* GASTO EN MUEBLES Y ENSERES *
*----------------------------*

gen gasto_ens_nm=gasnomon if (clave>="K001" & clave<="K037")

replace gasto_ens_nm=gasto_ens_nm/d41s02 if decena==0
replace gasto_ens_nm=gasto_ens_nm/d41s02 if decena==1
replace gasto_ens_nm=gasto_ens_nm/d41s03 if decena==2
replace gasto_ens_nm=gasto_ens_nm/d41s03 if decena==3
replace gasto_ens_nm=gasto_ens_nm/d41s03 if decena==4
replace gasto_ens_nm=gasto_ens_nm/d41s04 if decena==5
replace gasto_ens_nm=gasto_ens_nm/d41s04 if decena==6
replace gasto_ens_nm=gasto_ens_nm/d41s04 if decena==7
replace gasto_ens_nm=gasto_ens_nm/d41s05 if decena==8
replace gasto_ens_nm=gasto_ens_nm/d41s05 if decena==9

*----------------*
* GASTO EN SALUD *
*----------------*

gen gasto_sal_nm=gasnomon if (clave>="J001" & clave<="J072")

replace gasto_sal_nm=gasto_sal_nm/d51t05 if decena==0
replace gasto_sal_nm=gasto_sal_nm/d51t05 if decena==1
replace gasto_sal_nm=gasto_sal_nm/d51t06 if decena==2
replace gasto_sal_nm=gasto_sal_nm/d51t06 if decena==3
replace gasto_sal_nm=gasto_sal_nm/d51t06 if decena==4
replace gasto_sal_nm=gasto_sal_nm/d51t07 if decena==5
replace gasto_sal_nm=gasto_sal_nm/d51t07 if decena==6
replace gasto_sal_nm=gasto_sal_nm/d51t07 if decena==7
replace gasto_sal_nm=gasto_sal_nm/d51t08 if decena==8
replace gasto_sal_nm=gasto_sal_nm/d51t08 if decena==9


*-----------------------------*
* GASTO EN TRANSPORTE PUBLICO *
*-----------------------------*

gen gasto_tpub_nm=gasnomon if (clave>="B001" & clave<="B007")

replace gasto_tpub_nm=gasto_tpub_nm/d611w08 if decena==0
replace gasto_tpub_nm=gasto_tpub_nm/d611w08 if decena==1
replace gasto_tpub_nm=gasto_tpub_nm/d611w08 if decena==2
replace gasto_tpub_nm=gasto_tpub_nm/d611w09 if decena==3
replace gasto_tpub_nm=gasto_tpub_nm/d611w09 if decena==4
replace gasto_tpub_nm=gasto_tpub_nm/d611w09 if decena==5
replace gasto_tpub_nm=gasto_tpub_nm/d611w10 if decena==6
replace gasto_tpub_nm=gasto_tpub_nm/d611w10 if decena==7
replace gasto_tpub_nm=gasto_tpub_nm/d611w10 if decena==8
replace gasto_tpub_nm=gasto_tpub_nm/d611w11 if decena==9

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

gen gasto_tserv_nm=gasnomon if (clave>="M001" & clave<="M004") |(clave=="M006") 

replace gasto_tserv_nm=gasto_tserv_nm/d6s02 if decena==0
replace gasto_tserv_nm=gasto_tserv_nm/d6s02 if decena==1
replace gasto_tserv_nm=gasto_tserv_nm/d6s03 if decena==2
replace gasto_tserv_nm=gasto_tserv_nm/d6s03 if decena==3
replace gasto_tserv_nm=gasto_tserv_nm/d6s03 if decena==4
replace gasto_tserv_nm=gasto_tserv_nm/d6s04 if decena==5
replace gasto_tserv_nm=gasto_tserv_nm/d6s04 if decena==6
replace gasto_tserv_nm=gasto_tserv_nm/d6s04 if decena==7
replace gasto_tserv_nm=gasto_tserv_nm/d6s05 if decena==8
replace gasto_tserv_nm=gasto_tserv_nm/d6s05 if decena==9

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga_nm=gasnomon if (clave>="F007" & clave<="F008")

replace gasto_tga_nm=gasto_tga_nm/d6s02 if decena==0
replace gasto_tga_nm=gasto_tga_nm/d6s02 if decena==1
replace gasto_tga_nm=gasto_tga_nm/d6s03 if decena==2
replace gasto_tga_nm=gasto_tga_nm/d6s03 if decena==3
replace gasto_tga_nm=gasto_tga_nm/d6s03 if decena==4
replace gasto_tga_nm=gasto_tga_nm/d6s04 if decena==5
replace gasto_tga_nm=gasto_tga_nm/d6s04 if decena==6
replace gasto_tga_nm=gasto_tga_nm/d6s04 if decena==7
replace gasto_tga_nm=gasto_tga_nm/d6s05 if decena==8
replace gasto_tga_nm=gasto_tga_nm/d6s05 if decena==9

*----------------------------------*
* GASTO EN DIESEL Y GAS-TRANSPORTE *
*----------------------------------*

gen gasto_tdie_nm=gasnomon if (clave=="F009")

replace gasto_tdie_nm=gasto_tdie_nm/d6s02 if decena==0
replace gasto_tdie_nm=gasto_tdie_nm/d6s02 if decena==1
replace gasto_tdie_nm=gasto_tdie_nm/d6s03 if decena==2
replace gasto_tdie_nm=gasto_tdie_nm/d6s03 if decena==3
replace gasto_tdie_nm=gasto_tdie_nm/d6s03 if decena==4
replace gasto_tdie_nm=gasto_tdie_nm/d6s04 if decena==5
replace gasto_tdie_nm=gasto_tdie_nm/d6s04 if decena==6
replace gasto_tdie_nm=gasto_tdie_nm/d6s04 if decena==7
replace gasto_tdie_nm=gasto_tdie_nm/d6s05 if decena==8
replace gasto_tdie_nm=gasto_tdie_nm/d6s05 if decena==9


*--------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS  *
*--------------------------------------------------*	 

gen gasto_tman_nm=gasnomon if (clave>="F010" & clave<="F014") | (clave>="M012" & clave<="M018")

replace gasto_tman_nm=gasto_tman_nm/d6s02 if decena==0
replace gasto_tman_nm=gasto_tman_nm/d6s02 if decena==1
replace gasto_tman_nm=gasto_tman_nm/d6s03 if decena==2
replace gasto_tman_nm=gasto_tman_nm/d6s03 if decena==3
replace gasto_tman_nm=gasto_tman_nm/d6s03 if decena==4
replace gasto_tman_nm=gasto_tman_nm/d6s04 if decena==5
replace gasto_tman_nm=gasto_tman_nm/d6s04 if decena==6
replace gasto_tman_nm=gasto_tman_nm/d6s04 if decena==7
replace gasto_tman_nm=gasto_tman_nm/d6s05 if decena==8
replace gasto_tman_nm=gasto_tman_nm/d6s05 if decena==9

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*	

gen gasto_tadq_nm=gasnomon if (clave>="M007" & clave<="M011")

replace gasto_tadq_nm=gasto_tadq_nm/d6s02 if decena==0
replace gasto_tadq_nm=gasto_tadq_nm/d6s02 if decena==1
replace gasto_tadq_nm=gasto_tadq_nm/d6s03 if decena==2
replace gasto_tadq_nm=gasto_tadq_nm/d6s03 if decena==3
replace gasto_tadq_nm=gasto_tadq_nm/d6s03 if decena==4
replace gasto_tadq_nm=gasto_tadq_nm/d6s04 if decena==5
replace gasto_tadq_nm=gasto_tadq_nm/d6s04 if decena==6
replace gasto_tadq_nm=gasto_tadq_nm/d6s04 if decena==7
replace gasto_tadq_nm=gasto_tadq_nm/d6s05 if decena==8
replace gasto_tadq_nm=gasto_tadq_nm/d6s05 if decena==9

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

gen gasto_totros_nm=gasnomon if (clave=="M005")

replace gasto_totros_nm=gasto_totros_nm/d6s02 if decena==0
replace gasto_totros_nm=gasto_totros_nm/d6s02 if decena==1
replace gasto_totros_nm=gasto_totros_nm/d6s03 if decena==2
replace gasto_totros_nm=gasto_totros_nm/d6s03 if decena==3
replace gasto_totros_nm=gasto_totros_nm/d6s03 if decena==4
replace gasto_totros_nm=gasto_totros_nm/d6s04 if decena==5
replace gasto_totros_nm=gasto_totros_nm/d6s04 if decena==6
replace gasto_totros_nm=gasto_totros_nm/d6s04 if decena==7
replace gasto_totros_nm=gasto_totros_nm/d6s05 if decena==8
replace gasto_totros_nm=gasto_totros_nm/d6s05 if decena==9

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

gen gasto_com_nm=gasnomon if (clave>="F001" & clave<="F006") | (clave>="R005" & clave<="R008")| (clave>="R010" & clave<="R011")

replace gasto_com_nm=gasto_com_nm/d6m07 if decena==0
replace gasto_com_nm=gasto_com_nm/d6m07 if decena==1
replace gasto_com_nm=gasto_com_nm/d6m08 if decena==2
replace gasto_com_nm=gasto_com_nm/d6m08 if decena==3
replace gasto_com_nm=gasto_com_nm/d6m08 if decena==4
replace gasto_com_nm=gasto_com_nm/d6m09 if decena==5
replace gasto_com_nm=gasto_com_nm/d6m09 if decena==6
replace gasto_com_nm=gasto_com_nm/d6m09 if decena==7
replace gasto_com_nm=gasto_com_nm/d6m10 if decena==8
replace gasto_com_nm=gasto_com_nm/d6m10 if decena==9

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*	

gen gasto_edre_nm=gasnomon if (clave>="E001" & clave<="E012") | (clave>="E014" & clave<="E034")| (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005") | clave=="R009"

replace gasto_edre_nm=gasto_edre_nm/d7m07 if decena==0
replace gasto_edre_nm=gasto_edre_nm/d7m07 if decena==1
replace gasto_edre_nm=gasto_edre_nm/d7m08 if decena==2
replace gasto_edre_nm=gasto_edre_nm/d7m08 if decena==3
replace gasto_edre_nm=gasto_edre_nm/d7m08 if decena==4
replace gasto_edre_nm=gasto_edre_nm/d7m09 if decena==5
replace gasto_edre_nm=gasto_edre_nm/d7m09 if decena==6
replace gasto_edre_nm=gasto_edre_nm/d7m09 if decena==7
replace gasto_edre_nm=gasto_edre_nm/d7m10 if decena==8
replace gasto_edre_nm=gasto_edre_nm/d7m10 if decena==9

**Es necesario deflactar el gasto en transporte escolar y despues agregarlo a la categoría de gasto en transporte**

**Gasto en transporte escolar**

gen gasto_escolar_nm=gasnomon if (clave=="E013")

replace gasto_escolar_nm=gasto_escolar_nm/d7m07 if decena==0
replace gasto_escolar_nm=gasto_escolar_nm/d7m07 if decena==1
replace gasto_escolar_nm=gasto_escolar_nm/d7m08 if decena==2
replace gasto_escolar_nm=gasto_escolar_nm/d7m08 if decena==3
replace gasto_escolar_nm=gasto_escolar_nm/d7m08 if decena==4
replace gasto_escolar_nm=gasto_escolar_nm/d7m09 if decena==5
replace gasto_escolar_nm=gasto_escolar_nm/d7m09 if decena==6
replace gasto_escolar_nm=gasto_escolar_nm/d7m09 if decena==7
replace gasto_escolar_nm=gasto_escolar_nm/d7m10 if decena==8
replace gasto_escolar_nm=gasto_escolar_nm/d7m10 if decena==9

*---------------------------*
* GASTO EN CUIDADO PERSONAL *
*---------------------------*

gen gasto_cuip_nm=gasnomon if (clave>="D001" & clave<="D026") | (clave=="H132")

replace gasto_cuip_nm=gasto_cuip_nm/d23m07 if decena==0
replace gasto_cuip_nm=gasto_cuip_nm/d23m07 if decena==1
replace gasto_cuip_nm=gasto_cuip_nm/d23m08 if decena==2
replace gasto_cuip_nm=gasto_cuip_nm/d23m08 if decena==3
replace gasto_cuip_nm=gasto_cuip_nm/d23m08 if decena==4
replace gasto_cuip_nm=gasto_cuip_nm/d23m09 if decena==5
replace gasto_cuip_nm=gasto_cuip_nm/d23m09 if decena==6
replace gasto_cuip_nm=gasto_cuip_nm/d23m09 if decena==7
replace gasto_cuip_nm=gasto_cuip_nm/d23m10 if decena==8
replace gasto_cuip_nm=gasto_cuip_nm/d23m10 if decena==9

*---------------------------------*
* GASTO EN ACCESORIOS PERSONALES  *
*---------------------------------*

gen gasto_accp_nm=gasnomon if (clave>="H123" & clave<="H131") | (clave=="H133")

replace gasto_accp_nm=gasto_accp_nm/d23t05 if decena==0
replace gasto_accp_nm=gasto_accp_nm/d23t05 if decena==1
replace gasto_accp_nm=gasto_accp_nm/d23t06 if decena==2
replace gasto_accp_nm=gasto_accp_nm/d23t06 if decena==3
replace gasto_accp_nm=gasto_accp_nm/d23t06 if decena==4
replace gasto_accp_nm=gasto_accp_nm/d23t07 if decena==5
replace gasto_accp_nm=gasto_accp_nm/d23t07 if decena==6
replace gasto_accp_nm=gasto_accp_nm/d23t07 if decena==7
replace gasto_accp_nm=gasto_accp_nm/d23t08 if decena==8
replace gasto_accp_nm=gasto_accp_nm/d23t08 if decena==9

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*

gen gasto_otros_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T915") | (clave=="R012")

replace gasto_otros_nm=gasto_otros_nm/dINPCs02 if decena==0
replace gasto_otros_nm=gasto_otros_nm/dINPCs02 if decena==1
replace gasto_otros_nm=gasto_otros_nm/dINPCs03 if decena==2
replace gasto_otros_nm=gasto_otros_nm/dINPCs03 if decena==3
replace gasto_otros_nm=gasto_otros_nm/dINPCs03 if decena==4
replace gasto_otros_nm=gasto_otros_nm/dINPCs04 if decena==5
replace gasto_otros_nm=gasto_otros_nm/dINPCs04 if decena==6
replace gasto_otros_nm=gasto_otros_nm/dINPCs04 if decena==7
replace gasto_otros_nm=gasto_otros_nm/dINPCs05 if decena==8
replace gasto_otros_nm=gasto_otros_nm/dINPCs05 if decena==9



**Despues de tener todos los rubros debidamente deflactados y mensualizados, se agrupan algunos rubros en categorías más grandes**

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

**Se agregan las categorias de muebles y enseres, articulos de limpieza y gasto en cristaleria y blancos**

egen double aux= rsum(gasto_ens_nm gasto_lim_nm gasto_cris_nm)
replace gasto_ens_nm= aux
drop aux

**Se agrega el transporte escolar a la categoría de servicios de transporte**

egen double aux= rsum(gasto_tserv_nm gasto_escolar_nm)
replace gasto_tserv_nm= aux
drop aux

**Se agregan las categorías de gasto en cuidado personal y accesorios personales a la categoría de gasto en otros bienes y servicios**

egen double aux= rsum(gasto_otros_nm gasto_accp_nm gasto_cuip_nm)
replace gasto_otros_nm=aux
drop aux

**Se agrega la categoría de gasto en transporte publico a la de servicios de transporte**

egen double aux = rsum(gasto_tserv_nm gasto_tpub_nm)

replace gasto_tserv_nm=aux
drop aux


**se crea el gasto total en transporte**

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans_nm= rsum(gasto_tserv_nm gasto_tga_nm gasto_tdie_nm gasto_tman_nm gasto_tadq_nm gasto_totros_nm)


saveold "$RAW\ingresonomonetario_def14.dta", replace  v(12)

use "$RAW\ingresonomonetario_def14.dta", clear

/*Construcción de la base de pagos en especie a partir de la base 
de gasto no monetario*/

keep if esp==1

collapse (sum) *_nm, by(folioviv foliohog)

global gastos1 " gasto_ali_nm gasto_alihogar_nm gasto_alifuera_nm gasto_alta_nm gasto_veca_nm gasto_viv_nm gasto_vcon_nm gasto_vlp_nm gasto_vp_nm gasto_vdi_nm gasto_vca_nm gasto_vle_nm gasto_vot_nm gasto_vele_nm gasto_vag_nm gasto_vgn_nm gasto_ens_nm gasto_sal_nm gasto_trans_nm gasto_tserv_nm gasto_tga_nm gasto_tdie_nm gasto_tman_nm gasto_tadq_nm gasto_totros_nm gasto_com_nm gasto_edre_nm gasto_otros_nm "
foreach var in $gastos1 {
rename `var' `var'e
}
	
sort  folioviv foliohog

saveold "$RAW\esp_def14.dta", replace v(12)

use "$RAW\ingresonomonetario_def14.dta", clear

*Construcción de base de regalos a partir de la base no monetaria*

keep if reg==1

collapse (sum) *_nm, by(folioviv foliohog)


foreach var in $gastos1 {
rename `var' `var'r
}
sort folioviv foliohog

saveold "$RAW\reg_def14.dta", replace v(12)

use "$RAW\ingresonomonetario_def14.dta", clear

/*Construcción de la base de autoconsumo a partir de la base 
de gasto no monetario*/

keep if autoc==1

collapse (sum) *_nm, by(folioviv foliohog)

foreach var in $gastos1 {
rename `var' `var'ac
}

sort  folioviv foliohog

saveold "$RAW\autoc_def14.dta", replace v(12)
		  
use "$RAW\ingresonomonetario_def14.dta", clear

/*Construcción de la base de estimacion del alquiler a partir de la base 
de gasto no monetario*/

keep if est_alq==1

gen ealq_nm=gasnomon if (clave>="G102" & clave<="G106")

collapse (sum) ealq_nm, by(folioviv foliohog)

sort  folioviv foliohog

saveold "$RAW\est_alqdef14.dta", replace v(12)		  
		
		
**Se juntan todas las bases de datos no monetarias y monetarias**		
		
use "$RAW\concentrado.dta", clear

keep  folioviv foliohog tam_loc factor tot_integ est_dis upm ubica_geo

*Incorporación de la base de ingreso monetario deflactado*

sort folioviv foliohog

merge  folioviv foliohog using "$RAW\ingreso_deflactado.dta"
tab _merge
drop _merge

global ing "ing_mon ing_lab_mon ing_ren_mon ing_ct_mon ing_trsgob_mon ing_trspri_mon ing_rem_mon"

foreach var in $ing {
replace `var'=0 if  `var'==.
}

*Incorporación de la base de ingreso no monetario deflactado: pago en especie*

sort  folioviv foliohog

merge  folioviv foliohog using "$RAW\esp_def14.dta"
tab _merge
drop _merge

*Incorporación de la base de ingreso no monetario deflactado: regalos en especie*

sort folioviv foliohog

merge  folioviv foliohog using "$RAW\reg_def14.dta"
tab _merge
drop _merge

*Incorporación de la base de ingreso no monetario deflactado: autoconsumo*

sort folioviv foliohog

merge  folioviv foliohog using "$RAW\autoc_def14.dta"
tab _merge
drop _merge

*Incorporación de la base de ingreso no monetario deflactado: estimacion del alquiler*

sort folioviv foliohog

merge  folioviv foliohog using "$RAW\est_alqdef14.dta"
tab _merge
drop _merge


gen rururb=1 if tam_loc=="4"
replace rururb=0 if tam_loc<="3"
label define rururb 1 "Rural" 0 "Urbano"
label value rururb rururb

**Generar valores finales de las variables**

egen double pago_esp=rowtotal(gasto_alihogar_nme gasto_alifuera_nme gasto_alta_nme gasto_veca_nme gasto_vcon_nme gasto_vlp_nme gasto_vp_nme gasto_vdi_nme gasto_vca_nme gasto_vle_nme gasto_vot_nme gasto_vele_nme gasto_vag_nme gasto_vgn_nme gasto_ens_nme gasto_sal_nme gasto_tserv_nme gasto_tga_nme gasto_tdie_nme gasto_tman_nme gasto_tadq_nme gasto_totros_nme gasto_com_nme gasto_edre_nme gasto_otros_nme)

egen double reg_esp=rowtotal (gasto_alihogar_nmr gasto_alifuera_nmr gasto_alta_nmr gasto_veca_nmr gasto_vcon_nmr gasto_vlp_nmr gasto_vp_nmr gasto_vdi_nmr gasto_vca_nmr gasto_vle_nmr gasto_vot_nmr gasto_vele_nmr gasto_vag_nmr gasto_vgn_nmr gasto_ens_nmr gasto_sal_nmr gasto_tserv_nmr gasto_tga_nmr gasto_tdie_nmr gasto_tman_nmr gasto_tadq_nmr gasto_totros_nmr gasto_com_nmr gasto_edre_nmr gasto_otros_nmr)

egen double pago_autoc=rowtotal (gasto_alihogar_nmac gasto_alifuera_nmac gasto_alta_nmac gasto_veca_nmac gasto_vcon_nmac gasto_vlp_nmac gasto_vp_nmac gasto_vdi_nmac gasto_vca_nmac gasto_vle_nmac gasto_vot_nmac gasto_vele_nmac gasto_vag_nmac gasto_vgn_nmac gasto_ens_nmac gasto_sal_nmac gasto_tserv_nmac gasto_tga_nmac gasto_tdie_nmac gasto_tman_nmac gasto_tadq_nmac gasto_totros_nmac gasto_com_nmac gasto_edre_nmac gasto_otros_nmac)

**Composicion del ingreso no monetario**

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

egen double ing_lab_nomon = rowtotal(pago_esp pago_autoc)

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

**El gasto imputado por estimacion del alquiler no se deflacta**

gen ing_ren_nomon= ealq_nm 

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

gen double ing_trspri_nomon=reg_esp

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

egen double ing_nomon= rowtotal(ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon)

sort folioviv foliohog



          *=======================================================*
          * 1.3. INGRESO CORRIENTE TOTAL (MONETARIO+NO MONETARIO) *
          *=======================================================*


		  
**Composicion del ingreso total**

*-----------------------*
* INGRESO LABORAL       * 
*-----------------------*

egen double ing_lab = rsum(ing_lab_mon ing_lab_nomon)

*-----------------------------------*
* INGRESO POR RENTA DE LA PROPIEDAD * 
*-----------------------------------*

egen double ing_ren= rsum (ing_ren_mon ing_ren_nomon)

*---------------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS LOCALES * 
*---------------------------------------------*

egen double ing_trspri= rsum( ing_trspri_mon ing_trspri_nomon)

*-------------------------------------*
* INGRESO POR REMESAS INTERNACIONALES * 
*-------------------------------------*

egen double ing_rem= rsum(ing_rem_mon ing_rem_nomon) 

*-------------------------------------*
* INGRESO POR TRANSFERENCIAS PRIVADAS * 
*-------------------------------------*

egen double ing_tpriv= rsum(ing_trspri ing_rem) 

*--------------------------------------------------------------*
* INGRESO  POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*--------------------------------------------------------------*

egen double ing_ct= rsum(ing_ct_mon ing_ct_nomon) 

*----------------------------------------------------------*
* INGRESO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*----------------------------------------------------------*

egen double ing_trsgob= rsum(ing_trsgob_mon ing_trsgob_nomon)

*-------------------------------------------*
* INGRESO POR TRANSFERENCIAS DEL GOBIERNO   * 
*-------------------------------------------*

egen double ing_tpub= rsum(ing_trsgob ing_ct)

*------------------------------------------------------*
* INGRESO  POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*------------------------------------------------------*

egen double ing_otro= rsum(ing_otro_mon ing_otro_nomon)


*--------------------------*
* INGRESO CORRIENTE TOTAL  *  
*--------------------------*

egen double ict= rsum(ing_mon ing_nomon)




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

saveold "$RAW\ingresotot14.dta", replace v(12)

/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
          *=====================*
          * 2.0.GASTO MONETARIO *
          *=========+===========*	

**gasto monetario deflactado a pesos de agosto de 2014**


use "$RAW\gastohogar.dta", clear
gen base=1
append using "$RAW\gastopersona.dta"
recode base (.=2)

label var base "Origen del monto"
label define base 1 "Monto del hogar" 2 "Monto de personas"
label value base base

/*En el caso de la información de gasto monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la octava posición del 
folio de la vivienda. En primer lugar se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folioviv,8,1))

*Definición de los deflactores*

*Rubro 1.1 semanal, Alimentos*
scalar d11w07=	0.9901303261	
scalar d11w08=	1.0000000000	
scalar d11w09=	1.0095550367	
scalar d11w10=	1.0117409376	
scalar d11w11=	1.0192922318	

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco*
scalar d12w07=	0.9953130285	
scalar d12w08=	1.0000000000	
scalar d12w09=	1.0018441864	
scalar d12w10=	1.0031971266	
scalar d12w11=	1.0026011887	

*Rubro 2 trimestral, Vestido, calzado y accesorios*
scalar d2t05=	0.9937183977	
scalar d2t06=	0.9952148881	
scalar d2t07=	0.9984303614	
scalar d2t08=	1.0044224187	

*Rubro 3 mensual, Vivienda*
scalar d3m07=	0.9997777670	
scalar d3m08=	1.0000000000	
scalar d3m09=	1.0003671675	
scalar d3m10=	1.0150731919	
scalar d3m11=	1.0376346683	

*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar*
scalar d42m07=	0.9971952310	
scalar d42m08=	1.0000000000	
scalar d42m09=	1.0002691884	
scalar d42m10=	0.9973081165	
scalar d42m11=	0.9989492971	

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar*
scalar d42t05=	0.9977191352	
scalar d42t06=	0.9980635806	
scalar d42t07=	0.9991548065	
scalar d42t08=	0.9991924349	

*Rubro 4.1 semestral, Muebles y aparatos dómesticos*
scalar d41s02=	0.9952434028	
scalar d41s03=	0.9949403103	
scalar d41s04=	0.9946616608	
scalar d41s05=	0.9951032633	

*Rubro 5.1 trimestral, Salud*
scalar d51t05=	0.9944286156	
scalar d51t06=	0.9972835892	
scalar d51t07=	0.9995669908	
scalar d51t08=	1.0015184188	

*Rubro 6.1.1 semanal, Transporte público urbano*
scalar d611w07=	0.9987640919	
scalar d611w08=	1.0000000000	
scalar d611w09=	1.0008100209	
scalar d611w10=	1.0015448852	
scalar d611w11=	1.0027807933	

*Rubro 6 mensual, Transporte*
scalar d6m07=	0.9996805033	
scalar d6m08=	1.0000000000	
scalar d6m09=	1.0023265911	
scalar d6m10=	1.0054068667	
scalar d6m11=	1.0051119467	

*Rubro 6 semestral, Transporte*
scalar d6s02=	0.9903700427	
scalar d6s03=	0.9935445288	
scalar d6s04=	0.9964773444	
scalar d6s05=	0.9991370859	

*Rubro 7 mensual, Educación y esparcimiento*
scalar d7m07=	0.9998214971	
scalar d7m08=	1.0000000000	
scalar d7m09=	1.0121560472	
scalar d7m10=	1.0141642048	
scalar d7m11=	1.0154762011	

*Rubro 2.3 mensual, Accesorios y cuidados del vestido*
scalar d23m07=	0.9931651984	
scalar d23m08=	1.0000000000	
scalar d23m09=	1.0016681244	
scalar d23m10=	1.0051576603	
scalar d23m11=	1.0059240958	

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido*
scalar d23t05=	0.9945267486	
scalar d23t06=	0.9963391432	
scalar d23t07=	0.9982777743	
scalar d23t08=	1.0022752616	

*INPC semestral*
scalar dINPCs02=	0.9947548441	
scalar dINPCs03=	0.9957069060	
scalar dINPCs04=	0.9969410603	
scalar dINPCs05=	0.9994108382	

*Una vez definidos los deflactores, se seleccionan los rubros*

gen double gasmon=gasto_tri/3

**Solo se trabaja con los gastos monetarios en bienes y servicios para el hogar**

gen hogar=1 if tipo_gasto=="G1"

drop if tipo_gasto=="G3" | tipo_gasto=="G7" | tipo_gasto=="G4" | tipo_gasto=="G5" | tipo_gasto=="G6" | tipo_gasto=="G2"

*------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS *
*------------------------------*

gen gasto_ali_m=gasmon if (clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247") 

replace gasto_ali_m=gasto_ali_m/d11w08 if decena==0
replace gasto_ali_m=gasto_ali_m/d11w08 if decena==1
replace gasto_ali_m=gasto_ali_m/d11w08 if decena==2
replace gasto_ali_m=gasto_ali_m/d11w09 if decena==3
replace gasto_ali_m=gasto_ali_m/d11w09 if decena==4
replace gasto_ali_m=gasto_ali_m/d11w09 if decena==5
replace gasto_ali_m=gasto_ali_m/d11w10 if decena==6
replace gasto_ali_m=gasto_ali_m/d11w10 if decena==7
replace gasto_ali_m=gasto_ali_m/d11w10 if decena==8
replace gasto_ali_m=gasto_ali_m/d11w11 if decena==9

**division del gasto en alimentos**

*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

gen gasto_alihogar_m=gasmon if (clave>="A001" & clave<="A222") | (clave=="A242")

replace gasto_alihogar_m=gasto_alihogar_m/d11w08 if decena==0
replace gasto_alihogar_m=gasto_alihogar_m/d11w08 if decena==1
replace gasto_alihogar_m=gasto_alihogar_m/d11w08 if decena==2
replace gasto_alihogar_m=gasto_alihogar_m/d11w09 if decena==3
replace gasto_alihogar_m=gasto_alihogar_m/d11w09 if decena==4
replace gasto_alihogar_m=gasto_alihogar_m/d11w09 if decena==5
replace gasto_alihogar_m=gasto_alihogar_m/d11w10 if decena==6
replace gasto_alihogar_m=gasto_alihogar_m/d11w10 if decena==7
replace gasto_alihogar_m=gasto_alihogar_m/d11w10 if decena==8
replace gasto_alihogar_m=gasto_alihogar_m/d11w11 if decena==9

*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

gen gasto_alifuera_m=gasmon if (clave>="A243" & clave<="A247")

replace gasto_alifuera_m=gasto_alifuera_m/d11w08 if decena==0
replace gasto_alifuera_m=gasto_alifuera_m/d11w08 if decena==1
replace gasto_alifuera_m=gasto_alifuera_m/d11w08 if decena==2
replace gasto_alifuera_m=gasto_alifuera_m/d11w09 if decena==3
replace gasto_alifuera_m=gasto_alifuera_m/d11w09 if decena==4
replace gasto_alifuera_m=gasto_alifuera_m/d11w09 if decena==5
replace gasto_alifuera_m=gasto_alifuera_m/d11w10 if decena==6
replace gasto_alifuera_m=gasto_alifuera_m/d11w10 if decena==7
replace gasto_alifuera_m=gasto_alifuera_m/d11w10 if decena==8
replace gasto_alifuera_m=gasto_alifuera_m/d11w11 if decena==9

*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

gen gasto_alta_m=gasmon if (clave>="A223" & clave<="A241")

replace gasto_alta_m=gasto_alta_m/d12w08 if decena==0
replace gasto_alta_m=gasto_alta_m/d12w08 if decena==1
replace gasto_alta_m=gasto_alta_m/d12w08 if decena==2
replace gasto_alta_m=gasto_alta_m/d12w09 if decena==3
replace gasto_alta_m=gasto_alta_m/d12w09 if decena==4
replace gasto_alta_m=gasto_alta_m/d12w09 if decena==5
replace gasto_alta_m=gasto_alta_m/d12w10 if decena==6
replace gasto_alta_m=gasto_alta_m/d12w10 if decena==7
replace gasto_alta_m=gasto_alta_m/d12w10 if decena==8
replace gasto_alta_m=gasto_alta_m/d12w11 if decena==9

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

gen gasto_veca_m=gasmon if (clave>="H001" & clave<="H122") |(clave=="H136")

replace gasto_veca_m=gasto_veca_m/d2t05 if decena==0
replace gasto_veca_m=gasto_veca_m/d2t05 if decena==1
replace gasto_veca_m=gasto_veca_m/d2t06 if decena==2
replace gasto_veca_m=gasto_veca_m/d2t06 if decena==3
replace gasto_veca_m=gasto_veca_m/d2t06 if decena==4
replace gasto_veca_m=gasto_veca_m/d2t07 if decena==5
replace gasto_veca_m=gasto_veca_m/d2t07 if decena==6
replace gasto_veca_m=gasto_veca_m/d2t07 if decena==7
replace gasto_veca_m=gasto_veca_m/d2t08 if decena==8
replace gasto_veca_m=gasto_veca_m/d2t08 if decena==9


*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

**SE INCLUYE EL GASTO EN ALQUILER DE VIVIENDA Y ALQUILER IMPUTADO (G101-G108)**

gen gasto_viv_m=gasmon if (clave>="G001" & clave<="G016") | (clave>="R001" & clave<="R004") | (clave=="R013") |(clave>="G101" & clave<="G106")

replace gasto_viv_m=gasto_viv_m/d3m07 if decena==0
replace gasto_viv_m=gasto_viv_m/d3m07 if decena==1
replace gasto_viv_m=gasto_viv_m/d3m08 if decena==2
replace gasto_viv_m=gasto_viv_m/d3m08 if decena==3
replace gasto_viv_m=gasto_viv_m/d3m08 if decena==4
replace gasto_viv_m=gasto_viv_m/d3m09 if decena==5
replace gasto_viv_m=gasto_viv_m/d3m09 if decena==6
replace gasto_viv_m=gasto_viv_m/d3m09 if decena==7
replace gasto_viv_m=gasto_viv_m/d3m10 if decena==8
replace gasto_viv_m=gasto_viv_m/d3m10 if decena==9

**Division del gasto en vivienda, servicios de conservacion y combustible**

*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

gen gasto_vcon_m=gasmon if (clave>="G001" & clave<="G008") |(clave=="R004") | (clave=="R013") |(clave>="G101" & clave<="G106")

replace gasto_vcon_m=gasto_vcon_m/d3m07 if decena==0
replace gasto_vcon_m=gasto_vcon_m/d3m07 if decena==1
replace gasto_vcon_m=gasto_vcon_m/d3m08 if decena==2
replace gasto_vcon_m=gasto_vcon_m/d3m08 if decena==3
replace gasto_vcon_m=gasto_vcon_m/d3m08 if decena==4
replace gasto_vcon_m=gasto_vcon_m/d3m09 if decena==5
replace gasto_vcon_m=gasto_vcon_m/d3m09 if decena==6
replace gasto_vcon_m=gasto_vcon_m/d3m09 if decena==7
replace gasto_vcon_m=gasto_vcon_m/d3m10 if decena==8
replace gasto_vcon_m=gasto_vcon_m/d3m10 if decena==9

*---------------*
* GASTO EN AGUA *
*---------------*

gen gasto_vag_m=gasmon if (clave=="R002")

replace gasto_vag_m=gasto_vag_m/d3m07 if decena==0
replace gasto_vag_m=gasto_vag_m/d3m07 if decena==1
replace gasto_vag_m=gasto_vag_m/d3m08 if decena==2
replace gasto_vag_m=gasto_vag_m/d3m08 if decena==3
replace gasto_vag_m=gasto_vag_m/d3m08 if decena==4
replace gasto_vag_m=gasto_vag_m/d3m09 if decena==5
replace gasto_vag_m=gasto_vag_m/d3m09 if decena==6
replace gasto_vag_m=gasto_vag_m/d3m09 if decena==7
replace gasto_vag_m=gasto_vag_m/d3m10 if decena==8
replace gasto_vag_m=gasto_vag_m/d3m10 if decena==9

*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

gen gasto_vele_m=gasmon if (clave=="R001")

replace gasto_vele_m=gasto_vele_m/d3m07 if decena==0
replace gasto_vele_m=gasto_vele_m/d3m07 if decena==1
replace gasto_vele_m=gasto_vele_m/d3m08 if decena==2
replace gasto_vele_m=gasto_vele_m/d3m08 if decena==3
replace gasto_vele_m=gasto_vele_m/d3m08 if decena==4
replace gasto_vele_m=gasto_vele_m/d3m09 if decena==5
replace gasto_vele_m=gasto_vele_m/d3m09 if decena==6
replace gasto_vele_m=gasto_vele_m/d3m09 if decena==7
replace gasto_vele_m=gasto_vele_m/d3m10 if decena==8
replace gasto_vele_m=gasto_vele_m/d3m10 if decena==9

*----------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO *
*----------------------------------*

gen gasto_vlp_m=gasmon if (clave=="G009")

replace gasto_vlp_m=gasto_vlp_m/d3m07 if decena==0
replace gasto_vlp_m=gasto_vlp_m/d3m07 if decena==1
replace gasto_vlp_m=gasto_vlp_m/d3m08 if decena==2
replace gasto_vlp_m=gasto_vlp_m/d3m08 if decena==3
replace gasto_vlp_m=gasto_vlp_m/d3m08 if decena==4
replace gasto_vlp_m=gasto_vlp_m/d3m09 if decena==5
replace gasto_vlp_m=gasto_vlp_m/d3m09 if decena==6
replace gasto_vlp_m=gasto_vlp_m/d3m09 if decena==7
replace gasto_vlp_m=gasto_vlp_m/d3m10 if decena==8
replace gasto_vlp_m=gasto_vlp_m/d3m10 if decena==9

*----------------------*
* GASTO EN GAS NATURAL *
*----------------------*

gen gasto_vgn_m=gasmon if (clave=="R003")

replace gasto_vgn_m=gasto_vgn_m/d3m07 if decena==0
replace gasto_vgn_m=gasto_vgn_m/d3m07 if decena==1
replace gasto_vgn_m=gasto_vgn_m/d3m08 if decena==2
replace gasto_vgn_m=gasto_vgn_m/d3m08 if decena==3
replace gasto_vgn_m=gasto_vgn_m/d3m08 if decena==4
replace gasto_vgn_m=gasto_vgn_m/d3m09 if decena==5
replace gasto_vgn_m=gasto_vgn_m/d3m09 if decena==6
replace gasto_vgn_m=gasto_vgn_m/d3m09 if decena==7
replace gasto_vgn_m=gasto_vgn_m/d3m10 if decena==8
replace gasto_vgn_m=gasto_vgn_m/d3m10 if decena==9

*-----------------*
* GASTO EN CARBON *
*-----------------*	

gen gasto_vca_m=gasmon if (clave=="G012")

replace gasto_vca_m=gasto_vca_m/d3m07 if decena==0
replace gasto_vca_m=gasto_vca_m/d3m07 if decena==1
replace gasto_vca_m=gasto_vca_m/d3m08 if decena==2
replace gasto_vca_m=gasto_vca_m/d3m08 if decena==3
replace gasto_vca_m=gasto_vca_m/d3m08 if decena==4
replace gasto_vca_m=gasto_vca_m/d3m09 if decena==5
replace gasto_vca_m=gasto_vca_m/d3m09 if decena==6
replace gasto_vca_m=gasto_vca_m/d3m09 if decena==7
replace gasto_vca_m=gasto_vca_m/d3m10 if decena==8
replace gasto_vca_m=gasto_vca_m/d3m10 if decena==9

*---------------*
* GASTO EN LENA *
*---------------*

gen gasto_vle_m=gasmon if (clave=="G013")

replace gasto_vle_m=gasto_vle_m/d3m07 if decena==0
replace gasto_vle_m=gasto_vle_m/d3m07 if decena==1
replace gasto_vle_m=gasto_vle_m/d3m08 if decena==2
replace gasto_vle_m=gasto_vle_m/d3m08 if decena==3
replace gasto_vle_m=gasto_vle_m/d3m08 if decena==4
replace gasto_vle_m=gasto_vle_m/d3m09 if decena==5
replace gasto_vle_m=gasto_vle_m/d3m09 if decena==6
replace gasto_vle_m=gasto_vle_m/d3m09 if decena==7
replace gasto_vle_m=gasto_vle_m/d3m10 if decena==8
replace gasto_vle_m=gasto_vle_m/d3m10 if decena==9

*-------------------*
* GASTO EN PETROLEO *
*-------------------*

gen gasto_vp_m=gasmon if (clave=="G010")

replace gasto_vp_m=gasto_vp_m/d3m07 if decena==0
replace gasto_vp_m=gasto_vp_m/d3m07 if decena==1
replace gasto_vp_m=gasto_vp_m/d3m08 if decena==2
replace gasto_vp_m=gasto_vp_m/d3m08 if decena==3
replace gasto_vp_m=gasto_vp_m/d3m08 if decena==4
replace gasto_vp_m=gasto_vp_m/d3m09 if decena==5
replace gasto_vp_m=gasto_vp_m/d3m09 if decena==6
replace gasto_vp_m=gasto_vp_m/d3m09 if decena==7
replace gasto_vp_m=gasto_vp_m/d3m10 if decena==8
replace gasto_vp_m=gasto_vp_m/d3m10 if decena==9

*-----------------*
* GASTO EN DIESEL *
*-----------------*

gen gasto_vdi_m=gasmon if (clave=="G011")

replace gasto_vdi_m=gasto_vdi_m/d3m07 if decena==0
replace gasto_vdi_m=gasto_vdi_m/d3m07 if decena==1
replace gasto_vdi_m=gasto_vdi_m/d3m08 if decena==2
replace gasto_vdi_m=gasto_vdi_m/d3m08 if decena==3
replace gasto_vdi_m=gasto_vdi_m/d3m08 if decena==4
replace gasto_vdi_m=gasto_vdi_m/d3m09 if decena==5
replace gasto_vdi_m=gasto_vdi_m/d3m09 if decena==6
replace gasto_vdi_m=gasto_vdi_m/d3m09 if decena==7
replace gasto_vdi_m=gasto_vdi_m/d3m10 if decena==8
replace gasto_vdi_m=gasto_vdi_m/d3m10 if decena==9

*-------------------------------------------*
* GASTO EN OTROS COMBUSTIBLES PARA CALENTAR *
*-------------------------------------------*

gen gasto_vot_m=gasmon if (clave>="G014" & clave<="G016")

replace gasto_vot_m=gasto_vot_m/d3m07 if decena==0
replace gasto_vot_m=gasto_vot_m/d3m07 if decena==1
replace gasto_vot_m=gasto_vot_m/d3m08 if decena==2
replace gasto_vot_m=gasto_vot_m/d3m08 if decena==3
replace gasto_vot_m=gasto_vot_m/d3m08 if decena==4
replace gasto_vot_m=gasto_vot_m/d3m09 if decena==5
replace gasto_vot_m=gasto_vot_m/d3m09 if decena==6
replace gasto_vot_m=gasto_vot_m/d3m09 if decena==7
replace gasto_vot_m=gasto_vot_m/d3m10 if decena==8
replace gasto_vot_m=gasto_vot_m/d3m10 if decena==9


*--------------------------------*
* GASTO EN ARTICULOS DE LIMPIEZA *
*--------------------------------*

gen gasto_lim_m=gasmon if (clave>="C001" & clave<="C024")

replace gasto_lim_m=gasto_lim_m/d42m07 if decena==0
replace gasto_lim_m=gasto_lim_m/d42m07 if decena==1
replace gasto_lim_m=gasto_lim_m/d42m08 if decena==2
replace gasto_lim_m=gasto_lim_m/d42m08 if decena==3
replace gasto_lim_m=gasto_lim_m/d42m08 if decena==4
replace gasto_lim_m=gasto_lim_m/d42m09 if decena==5
replace gasto_lim_m=gasto_lim_m/d42m09 if decena==6
replace gasto_lim_m=gasto_lim_m/d42m09 if decena==7
replace gasto_lim_m=gasto_lim_m/d42m10 if decena==8
replace gasto_lim_m=gasto_lim_m/d42m10 if decena==9

*--------------------------------*
* GASTO EN CRISTALERIA Y BLANCOS *
*--------------------------------*

gen gasto_cris_m=gasmon if (clave>="I001" & clave<="I026")

replace gasto_cris_m=gasto_cris_m/d42t05 if decena==0
replace gasto_cris_m=gasto_cris_m/d42t05 if decena==1
replace gasto_cris_m=gasto_cris_m/d42t06 if decena==2
replace gasto_cris_m=gasto_cris_m/d42t06 if decena==3
replace gasto_cris_m=gasto_cris_m/d42t06 if decena==4
replace gasto_cris_m=gasto_cris_m/d42t07 if decena==5
replace gasto_cris_m=gasto_cris_m/d42t07 if decena==6
replace gasto_cris_m=gasto_cris_m/d42t07 if decena==7
replace gasto_cris_m=gasto_cris_m/d42t08 if decena==8
replace gasto_cris_m=gasto_cris_m/d42t08 if decena==9

*----------------------------*
* GASTO EN MUEBLES Y ENSERES *
*----------------------------*

gen gasto_ens_m=gasmon if (clave>="K001" & clave<="K037")

replace gasto_ens_m=gasto_ens_m/d41s02 if decena==0
replace gasto_ens_m=gasto_ens_m/d41s02 if decena==1
replace gasto_ens_m=gasto_ens_m/d41s03 if decena==2
replace gasto_ens_m=gasto_ens_m/d41s03 if decena==3
replace gasto_ens_m=gasto_ens_m/d41s03 if decena==4
replace gasto_ens_m=gasto_ens_m/d41s04 if decena==5
replace gasto_ens_m=gasto_ens_m/d41s04 if decena==6
replace gasto_ens_m=gasto_ens_m/d41s04 if decena==7
replace gasto_ens_m=gasto_ens_m/d41s05 if decena==8
replace gasto_ens_m=gasto_ens_m/d41s05 if decena==9

*----------------*
* GASTO EN SALUD *
*----------------*

gen gasto_sal_m=gasmon if (clave>="J001" & clave<="J072")

replace gasto_sal_m=gasto_sal_m/d51t05 if decena==0
replace gasto_sal_m=gasto_sal_m/d51t05 if decena==1
replace gasto_sal_m=gasto_sal_m/d51t06 if decena==2
replace gasto_sal_m=gasto_sal_m/d51t06 if decena==3
replace gasto_sal_m=gasto_sal_m/d51t06 if decena==4
replace gasto_sal_m=gasto_sal_m/d51t07 if decena==5
replace gasto_sal_m=gasto_sal_m/d51t07 if decena==6
replace gasto_sal_m=gasto_sal_m/d51t07 if decena==7
replace gasto_sal_m=gasto_sal_m/d51t08 if decena==8
replace gasto_sal_m=gasto_sal_m/d51t08 if decena==9


*-----------------------------*
* GASTO EN TRANSPORTE PUBLICO *
*-----------------------------*

gen gasto_tpub_m=gasmon if (clave>="B001" & clave<="B007")

replace gasto_tpub_m=gasto_tpub_m/d611w08 if decena==0
replace gasto_tpub_m=gasto_tpub_m/d611w08 if decena==1
replace gasto_tpub_m=gasto_tpub_m/d611w08 if decena==2
replace gasto_tpub_m=gasto_tpub_m/d611w09 if decena==3
replace gasto_tpub_m=gasto_tpub_m/d611w09 if decena==4
replace gasto_tpub_m=gasto_tpub_m/d611w09 if decena==5
replace gasto_tpub_m=gasto_tpub_m/d611w10 if decena==6
replace gasto_tpub_m=gasto_tpub_m/d611w10 if decena==7
replace gasto_tpub_m=gasto_tpub_m/d611w10 if decena==8
replace gasto_tpub_m=gasto_tpub_m/d611w11 if decena==9

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

gen gasto_tserv_m=gasmon if (clave>="M001" & clave<="M004") |(clave=="M006") 

replace gasto_tserv_m=gasto_tserv_m/d6s02 if decena==0
replace gasto_tserv_m=gasto_tserv_m/d6s02 if decena==1
replace gasto_tserv_m=gasto_tserv_m/d6s03 if decena==2
replace gasto_tserv_m=gasto_tserv_m/d6s03 if decena==3
replace gasto_tserv_m=gasto_tserv_m/d6s03 if decena==4
replace gasto_tserv_m=gasto_tserv_m/d6s04 if decena==5
replace gasto_tserv_m=gasto_tserv_m/d6s04 if decena==6
replace gasto_tserv_m=gasto_tserv_m/d6s04 if decena==7
replace gasto_tserv_m=gasto_tserv_m/d6s05 if decena==8
replace gasto_tserv_m=gasto_tserv_m/d6s05 if decena==9

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

gen gasto_tga_m=gasmon if (clave>="F007" & clave<="F008")

replace gasto_tga_m=gasto_tga_m/d6s02 if decena==0
replace gasto_tga_m=gasto_tga_m/d6s02 if decena==1
replace gasto_tga_m=gasto_tga_m/d6s03 if decena==2
replace gasto_tga_m=gasto_tga_m/d6s03 if decena==3
replace gasto_tga_m=gasto_tga_m/d6s03 if decena==4
replace gasto_tga_m=gasto_tga_m/d6s04 if decena==5
replace gasto_tga_m=gasto_tga_m/d6s04 if decena==6
replace gasto_tga_m=gasto_tga_m/d6s04 if decena==7
replace gasto_tga_m=gasto_tga_m/d6s05 if decena==8
replace gasto_tga_m=gasto_tga_m/d6s05 if decena==9

*----------------------------------*
* GASTO EN DIESEL Y GAS-TRANSPORTE *
*----------------------------------*

gen gasto_tdie_m=gasmon if (clave=="F009")

replace gasto_tdie_m=gasto_tdie_m/d6s02 if decena==0
replace gasto_tdie_m=gasto_tdie_m/d6s02 if decena==1
replace gasto_tdie_m=gasto_tdie_m/d6s03 if decena==2
replace gasto_tdie_m=gasto_tdie_m/d6s03 if decena==3
replace gasto_tdie_m=gasto_tdie_m/d6s03 if decena==4
replace gasto_tdie_m=gasto_tdie_m/d6s04 if decena==5
replace gasto_tdie_m=gasto_tdie_m/d6s04 if decena==6
replace gasto_tdie_m=gasto_tdie_m/d6s04 if decena==7
replace gasto_tdie_m=gasto_tdie_m/d6s05 if decena==8
replace gasto_tdie_m=gasto_tdie_m/d6s05 if decena==9


*--------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS  *
*--------------------------------------------------*	 

gen gasto_tman_m=gasmon if (clave>="F010" & clave<="F014") | (clave>="M012" & clave<="M018")

replace gasto_tman_m=gasto_tman_m/d6s02 if decena==0
replace gasto_tman_m=gasto_tman_m/d6s02 if decena==1
replace gasto_tman_m=gasto_tman_m/d6s03 if decena==2
replace gasto_tman_m=gasto_tman_m/d6s03 if decena==3
replace gasto_tman_m=gasto_tman_m/d6s03 if decena==4
replace gasto_tman_m=gasto_tman_m/d6s04 if decena==5
replace gasto_tman_m=gasto_tman_m/d6s04 if decena==6
replace gasto_tman_m=gasto_tman_m/d6s04 if decena==7
replace gasto_tman_m=gasto_tman_m/d6s05 if decena==8
replace gasto_tman_m=gasto_tman_m/d6s05 if decena==9

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*	

gen gasto_tadq_m=gasmon if (clave>="M007" & clave<="M011")

replace gasto_tadq_m=gasto_tadq_m/d6s02 if decena==0
replace gasto_tadq_m=gasto_tadq_m/d6s02 if decena==1
replace gasto_tadq_m=gasto_tadq_m/d6s03 if decena==2
replace gasto_tadq_m=gasto_tadq_m/d6s03 if decena==3
replace gasto_tadq_m=gasto_tadq_m/d6s03 if decena==4
replace gasto_tadq_m=gasto_tadq_m/d6s04 if decena==5
replace gasto_tadq_m=gasto_tadq_m/d6s04 if decena==6
replace gasto_tadq_m=gasto_tadq_m/d6s04 if decena==7
replace gasto_tadq_m=gasto_tadq_m/d6s05 if decena==8
replace gasto_tadq_m=gasto_tadq_m/d6s05 if decena==9

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

gen gasto_totros_m=gasmon if (clave=="M005")

replace gasto_totros_m=gasto_totros_m/d6s02 if decena==0
replace gasto_totros_m=gasto_totros_m/d6s02 if decena==1
replace gasto_totros_m=gasto_totros_m/d6s03 if decena==2
replace gasto_totros_m=gasto_totros_m/d6s03 if decena==3
replace gasto_totros_m=gasto_totros_m/d6s03 if decena==4
replace gasto_totros_m=gasto_totros_m/d6s04 if decena==5
replace gasto_totros_m=gasto_totros_m/d6s04 if decena==6
replace gasto_totros_m=gasto_totros_m/d6s04 if decena==7
replace gasto_totros_m=gasto_totros_m/d6s05 if decena==8
replace gasto_totros_m=gasto_totros_m/d6s05 if decena==9

*-------------------------*
* GASTO EN COMUNICACIONES *
*-------------------------*

gen gasto_com_m=gasmon if (clave>="F001" & clave<="F006") | (clave>="R005" & clave<="R008")| (clave>="R010" & clave<="R011")

replace gasto_com_m=gasto_com_m/d6m07 if decena==0
replace gasto_com_m=gasto_com_m/d6m07 if decena==1
replace gasto_com_m=gasto_com_m/d6m08 if decena==2
replace gasto_com_m=gasto_com_m/d6m08 if decena==3
replace gasto_com_m=gasto_com_m/d6m08 if decena==4
replace gasto_com_m=gasto_com_m/d6m09 if decena==5
replace gasto_com_m=gasto_com_m/d6m09 if decena==6
replace gasto_com_m=gasto_com_m/d6m09 if decena==7
replace gasto_com_m=gasto_com_m/d6m10 if decena==8
replace gasto_com_m=gasto_com_m/d6m10 if decena==9

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*	

gen gasto_edre_m=gasmon if (clave>="E001" & clave<="E012") | (clave>="E014" & clave<="E034")| (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005") | clave=="R009"

replace gasto_edre_m=gasto_edre_m/d7m07 if decena==0
replace gasto_edre_m=gasto_edre_m/d7m07 if decena==1
replace gasto_edre_m=gasto_edre_m/d7m08 if decena==2
replace gasto_edre_m=gasto_edre_m/d7m08 if decena==3
replace gasto_edre_m=gasto_edre_m/d7m08 if decena==4
replace gasto_edre_m=gasto_edre_m/d7m09 if decena==5
replace gasto_edre_m=gasto_edre_m/d7m09 if decena==6
replace gasto_edre_m=gasto_edre_m/d7m09 if decena==7
replace gasto_edre_m=gasto_edre_m/d7m10 if decena==8
replace gasto_edre_m=gasto_edre_m/d7m10 if decena==9

**Es necesario deflactar el gasto en transporte escolar y despues agregarlo a la categoría de gasto en transporte**

**Gasto en transporte escolar**

gen gasto_escolar_m=gasmon if (clave=="E013")

replace gasto_escolar_m=gasto_escolar_m/d7m07 if decena==0
replace gasto_escolar_m=gasto_escolar_m/d7m07 if decena==1
replace gasto_escolar_m=gasto_escolar_m/d7m08 if decena==2
replace gasto_escolar_m=gasto_escolar_m/d7m08 if decena==3
replace gasto_escolar_m=gasto_escolar_m/d7m08 if decena==4
replace gasto_escolar_m=gasto_escolar_m/d7m09 if decena==5
replace gasto_escolar_m=gasto_escolar_m/d7m09 if decena==6
replace gasto_escolar_m=gasto_escolar_m/d7m09 if decena==7
replace gasto_escolar_m=gasto_escolar_m/d7m10 if decena==8
replace gasto_escolar_m=gasto_escolar_m/d7m10 if decena==9

*---------------------------*
* GASTO EN CUIDADO PERSONAL *
*---------------------------*

gen gasto_cuip_m=gasmon if (clave>="D001" & clave<="D026") | (clave=="H132")

replace gasto_cuip_m=gasto_cuip_m/d23m07 if decena==0
replace gasto_cuip_m=gasto_cuip_m/d23m07 if decena==1
replace gasto_cuip_m=gasto_cuip_m/d23m08 if decena==2
replace gasto_cuip_m=gasto_cuip_m/d23m08 if decena==3
replace gasto_cuip_m=gasto_cuip_m/d23m08 if decena==4
replace gasto_cuip_m=gasto_cuip_m/d23m09 if decena==5
replace gasto_cuip_m=gasto_cuip_m/d23m09 if decena==6
replace gasto_cuip_m=gasto_cuip_m/d23m09 if decena==7
replace gasto_cuip_m=gasto_cuip_m/d23m10 if decena==8
replace gasto_cuip_m=gasto_cuip_m/d23m10 if decena==9

*---------------------------------*
* GASTO EN ACCESORIOS PERSONALES  *
*---------------------------------*

gen gasto_accp_m=gasmon if (clave>="H123" & clave<="H131") | (clave=="H133")

replace gasto_accp_m=gasto_accp_m/d23t05 if decena==0
replace gasto_accp_m=gasto_accp_m/d23t05 if decena==1
replace gasto_accp_m=gasto_accp_m/d23t06 if decena==2
replace gasto_accp_m=gasto_accp_m/d23t06 if decena==3
replace gasto_accp_m=gasto_accp_m/d23t06 if decena==4
replace gasto_accp_m=gasto_accp_m/d23t07 if decena==5
replace gasto_accp_m=gasto_accp_m/d23t07 if decena==6
replace gasto_accp_m=gasto_accp_m/d23t07 if decena==7
replace gasto_accp_m=gasto_accp_m/d23t08 if decena==8
replace gasto_accp_m=gasto_accp_m/d23t08 if decena==9

*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*

gen gasto_otros_m=gasmon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T915") | (clave=="R012")

replace gasto_otros_m=gasto_otros_m/dINPCs02 if decena==0
replace gasto_otros_m=gasto_otros_m/dINPCs02 if decena==1
replace gasto_otros_m=gasto_otros_m/dINPCs03 if decena==2
replace gasto_otros_m=gasto_otros_m/dINPCs03 if decena==3
replace gasto_otros_m=gasto_otros_m/dINPCs03 if decena==4
replace gasto_otros_m=gasto_otros_m/dINPCs04 if decena==5
replace gasto_otros_m=gasto_otros_m/dINPCs04 if decena==6
replace gasto_otros_m=gasto_otros_m/dINPCs04 if decena==7
replace gasto_otros_m=gasto_otros_m/dINPCs05 if decena==8
replace gasto_otros_m=gasto_otros_m/dINPCs05 if decena==9


**Despues de tener todos los rubros debidamente deflactados y mensualizados, se agrupan algunos rubros en categorías más grandes**

*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

**Se agregan las categorias de muebles y enseres, articulos de limpieza y gasto en cristaleria y blancos**

egen double aux= rsum(gasto_ens_m gasto_lim_m gasto_cris_m)
replace gasto_ens_m= aux
drop aux

**Se agrega el transporte escolar a la categoría de servicios de transporte**

egen double aux= rsum(gasto_tserv_m gasto_escolar_m)
replace gasto_tserv_m= aux
drop aux

**Se agregan las categorías de gasto en cuidado personal y accesorios personales a la categoría de gasto en otros bienes y servicios**

egen double aux= rsum(gasto_otros_m gasto_accp_m gasto_cuip_m)
replace gasto_otros_m=aux
drop aux

**Se agrega la categoría de gasto en transporte publico a la de servicios de transporte**

egen double aux = rsum(gasto_tserv_m gasto_tpub_m)

replace gasto_tserv_m=aux
drop aux


**se crea el gasto total en transporte**

*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

egen gasto_trans_m= rsum(gasto_tserv_m gasto_tga_m gasto_tdie_m gasto_tman_m gasto_tadq_m gasto_totros_m)


sort folioviv foliohog

saveold "$RAW\gastomonetario_def14.dta", replace  v(12)

use "$RAW\gastomonetario_def14.dta", clear

/*Construcción de la base de gasto monetario en bienes y servicios para el 
hogar partir de la base de gasto monetario*/

keep if hogar==1

collapse (sum) *_m, by(folioviv foliohog)

global gastos2 "gasto_ali_m gasto_alihogar_m gasto_alifuera_m gasto_alta_m gasto_veca_m gasto_viv_m gasto_vcon_m gasto_vlp_m gasto_vp_m gasto_vdi_m gasto_vca_m gasto_vle_m gasto_vot_m gasto_vele_m gasto_vag_m gasto_vgn_m gasto_ens_m gasto_sal_m gasto_trans_m gasto_tserv_m gasto_tga_m gasto_tdie_m gasto_tman_m gasto_tadq_m gasto_totros_m gasto_com_m gasto_edre_m gasto_otros_m"
foreach var in $gastos2 {
rename `var' `var'hogar
}

sort  folioviv foliohog

saveold "$RAW\hogar_def14.dta", replace v(12)


          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*
		  

use "$RAW\ingresotot14.dta", clear
sort  folioviv foliohog

/*Incorporación de la base de gasto monetario deflactado: gasto de los hogares
en bienes y servicios para el hogar*/

sort  folioviv foliohog

merge  folioviv foliohog using "$RAW\hogar_def14.dta"
tab _merge
drop _merge


egen double gastom_hogar=rowtotal (gasto_alihogar_mhogar gasto_alifuera_mhogar gasto_alta_mhogar gasto_veca_mhogar gasto_vcon_mhogar gasto_vlp_mhogar gasto_vp_mhogar gasto_vdi_mhogar gasto_vca_mhogar gasto_vle_mhogar gasto_vot_mhogar gasto_vele_mhogar gasto_vag_mhogar gasto_vgn_mhogar gasto_ens_mhogar gasto_sal_mhogar gasto_tserv_mhogar gasto_tga_mhogar gasto_tdie_mhogar gasto_tman_mhogar gasto_tadq_mhogar gasto_totros_mhogar gasto_com_mhogar gasto_edre_mhogar gasto_otros_mhogar)
gen double gasto_mon=gastom_hogar


*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

global gastos3 "gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_viv gasto_vcon gasto_vlp gasto_vp gasto_vdi gasto_vca gasto_vle gasto_vot gasto_vele gasto_vag gasto_vgn gasto_ens gasto_sal gasto_trans gasto_tserv gasto_tga gasto_tdie gasto_tman gasto_tadq gasto_totros gasto_com gasto_edre gasto_otros"
foreach var in $gastos3 {
egen `var'= rowtotal(`var'_mhogar `var'_nmac `var'_nmr `var'_nme)
}



**Se crean variables de vivienda, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

gen gasto_vk=.

*-------------------*
* GASTO EN GASOLINA *
*-------------------*

gen gasto_vgas=.


*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rsum(gasto_vp gasto_vgas gasto_vk)

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca=rsum(gasto_vle gasto_vca)



**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*---------------------------------------------*
* GASTO EN GAS LICUADO DE PETROLEO-TRANSPORTE *
*---------------------------------------------*

gen gasto_tlp=.


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

egen gasto_tcomb=rowtotal(gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb)

*------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS *
*------------------------------*
egen double aux=rsum(gasto_alihogar gasto_alifuera gasto_alta)
replace gasto_ali=aux
drop aux


egen double gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros)

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

sort folioviv foliohog

 
keep folioviv foliohog factor_hog tot_integ  gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**

gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "MEX"
label var pais "Pais" 
gen anio=2014
label var anio "Anio de la encuesta" 
gen encuesta="ENHO"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename tot_integ miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename factor_hog factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros folioviv foliohog


saveold "$data_arm\MEX_ENIGH_2014.dta", replace v(12)

 
	


