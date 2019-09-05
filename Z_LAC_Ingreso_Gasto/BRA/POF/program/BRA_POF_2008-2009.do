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

5) País: Brasil

6) Encuesta: Pesquisa de Orçamentos Familiares 2008-2009- POF

7) Ano: 2008-2009

8) Inputs: 	$RAW\T_RENDIMENTOS_S.dta 
            $RAW\T_OUTROS_RECI_S.dta
			$RAW\T_DESPESA_12MESES_S.dta
			$RAW\T_DESPESA_90DIAS_S.dta
			$RAW\T_DESPESA_INDIVIDUAL_S.dta
			$RAW\T_DESPESA_VEICULO_S.dta
			$RAW\T_CADERNETA_DESPESA_S.dta
			$RAW\T_CADERNETA_DESPESA_S.dta
			$RAW\T_OUTRAS_DESPESAS_S.dta
			$RAW\T_ALUGUEL_ESTIMADO_S.dta
			$RAW\T_SERVICO_DOMS_S.dta
			$RAW\T_MORADOR_S.dta
 
 
6) Outputs: $data_arm\BRA_POF_2008-2009.dta 							

7) Version: Stata 14.0 (pero se guardan las bases en formato 12.0)

8) Notas: Se trabaja con los valores deflactados y brutos
          Para todas aquellas variables que se construyen, cuando no hay registro para ese
		  ingreso o gasto especifico, se asume que este es cero (dada la composicion de la base)
		  y por lo tanto el missing value que se genera se remplaza por cero.
				  		
				
 *************************************************************************
 *************************************************************************/
	
*Directorios:
	global RAW    "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\BRA\POF\RAW"
	global data_arm  "\\Sdssrv03\surveys\harmonized\Z_LAC_Ingreso_Gasto\BRA\POF\data_arm"

	
/*	____________________________________________________________________________

	      ************** 1. INGRESO DE LOS HOGARES **************
	____________________________________________________________________________	
*/


          *=================================*
          * 1.0. APPEND                     *
          *=================================*
		  
		  
*Se utilizan las bases ubicadas en la carpeta RAW*

/*Se hace un append entre las dos bases de datos que contienen ingresos para facilitar
la construccion de los ingresos del hogar*/	

	use "$RAW\T_RENDIMENTOS_S.dta" , clear 
	
**Se crea el identificador de la base**

gen base=1	
	
	append using "$RAW\T_OUTROS_RECI_S.dta" 

replace base=2 if base==.
	
	
/*Se crea la variable id que permite identificar los codigos de los ingresos.
Esta variable id se compone de 5 digitos que corresponden  a los 2 digitos de la variable "número do quadro"
y a los 3 primeros digitos de la variable "código do item"*/

**Se extraen los tres primeros digitos de la variable "código do item"**

gen item= substr( cod_item,1,3 ) 

egen id= concat(num_quadro item)

destring id, replace


preserve

**Se crea la variable de miembros del hogar a partir de la relación con el jefe de hogar**

use "$RAW\T_MORADOR_S.dta", clear

gen relacion=.
replace relacion=1 if  cod_rel_pess_refe_uc ==1
replace relacion=2 if  cod_rel_pess_refe_uc ==2
replace relacion=3 if  cod_rel_pess_refe_uc ==3
replace relacion=4 if  cod_rel_pess_refe_uc ==4 |  cod_rel_pess_refe_uc  ==5
replace relacion=5 if  cod_rel_pess_refe_uc >=6 & cod_rel_pess_refe_uc <=8


label variable relacion "Relacion con el jefe del hogar"
label define relacion 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 
label define relacion 6 "Empleado/a domestico/a", add
label value relacion relacion_ci

**Los miembros del hogar es la suma de individuos que componen el hogar, excluyenndo empleados domesticos y otros no parientes**

gen aux= .
replace aux=1 if relacion<5

bys cod_uf num_seq num_dv cod_domc: egen miembros= sum(aux)

**Se deja una sola observación por hogar y la variable relevante**
bys cod_uf num_seq num_dv cod_domc : gen contador=_n
keep if contador==1

keep cod_uf num_seq num_dv cod_domc miembros 

**Se hace el merge con la base de ingresos**

tempfile miembros
	save `miembros' , replace
	restore 

mmerge cod_uf num_seq num_dv cod_domc using `miembros' , t(n:1)
drop _merge
      
          *=================================*
          * 1.1. INGRESO MONETARIO          *
          *=================================*



**Composicion del ingreso monetario**


 /*Se ajustan los nombres de las variables de ingresos en las
dos bases para unificarlas*/

replace val_recebido_corrigido=val_renm_bruto_corrigido if val_recebido_corrigido==.

replace num_meses_recebe=num_meses_resgate if num_meses_recebe==.

**Se dividen las variables de ingreso y deducciones sobre  la frecuencia**

replace val_recebido_corrigido=val_recebido_corrigido*num_meses_recebe if num_meses_recebe!=0

replace num_ordem_inform =num_ord_inform if num_ordem_inform==""

/*Se genera la variable ingreso que corresponde al valor del ultimo ingreso recibido (deflactado)*/

gen ingreso= val_recebido_corrigido/12

*---------------------------------*
* INGRESO MONETARIO LABORAL       * 
*---------------------------------*

bys  cod_uf num_seq num_dv cod_domc :egen aux= sum(ingreso) if ( (id>=53001 & id<=53006) | (id>=54015 & id<=54018) | (id==54020) | (id>=54036 & id<=54038) | (id>=55001 & id<=55003) | (id==55011) | (id==55026) | (id==55035) | (id>=55037 & id<=55043) | (id==55045) | (id>=55062 & id<=55063))
bys  cod_uf num_seq num_dv cod_domc :egen ing_lab_mon= max(aux)
drop aux

replace ing_lab_mon = 0 if ing_lab_mon==.

*---------------------------------------------*
* INGRESO MONETARIO POR RENTA DE LA PROPIEDAD * 
*---------------------------------------------*

bys  cod_uf num_seq num_dv cod_domc: egen aux= sum(ingreso) if ( (id>=54008 & id<=54009) | (id>=54033 & id<=54034) | (id==55004) | (id==55007) | (id==55009) | (id>=55012 & id<=55013) | (id>=55015 & id<=55016) | (id>=55019 & id<=55021) | (id==55024) | (id>=55027 & id<=55028) | (id==55032) | (id==55034) | (id==55036) | (id>=55046 & id<=55052) | (id==55054) )
bys  cod_uf num_seq num_dv cod_domc:egen ing_ren_mon= max(aux)
drop aux

replace ing_ren_mon = 0 if ing_ren_mon==.

*-----------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*-----------------------------------------------*

bys  cod_uf num_seq num_dv cod_domc: egen aux= sum(ingreso) if ( (id==54005) | (id==54023) | (id==55025) | (id==55033) | (id==55066) | (id==54007) | (id==54026) | (id==54032) | (id==55030) | (id>=55055 & id<=55059))
bys  cod_uf num_seq num_dv cod_domc:egen ing_trspri_mon= max(aux)
drop aux

replace ing_trspri_mon = 0 if ing_trspri_mon==.

*-----------------------------------------------------------------------*
* INGRESO MONETARIO POR TRANSFERENCIAS DE DINERO DEL GOBIERNO (CCT-UCT) * 
*-----------------------------------------------------------------------*

bys  cod_uf num_seq num_dv cod_domc: egen aux= sum(ingreso) if ( (id>=54010 & id<=54012)  | (id==54024) | (id>=54001 & id<=54002) | (id>=55022 & id<=55023) | (id==55064) | (id>=54013 & id<=54014) | (id==54006) | (id==54022))
bys  cod_uf num_seq num_dv cod_domc:egen ing_ct_mon= max(aux)
drop aux

replace ing_ct_mon = 0 if ing_ct_mon==.

*--------------------------------------------------------------------*
* INGRESO MONETARIO POR OTRAS TRANSFERENCIAS DEL GOBIERNO (PUBLICAS) * 
*--------------------------------------------------------------------*

bys  cod_uf num_seq num_dv cod_domc: egen aux= sum(ingreso) if ( (id>=54003 & id<=54004) | (id==55065) | (id==54019) | (id==54021) | (id==54025) | (id>=54027 & id<=54031) | (id==54035) | (id==55017) | (id==55018) | (id==55029) | (id==55031) )
bys  cod_uf num_seq num_dv cod_domc:egen ing_trsgob_mon= max(aux)
drop aux


**Se crean las condiciones para la variación patrimonial**

/*Variação Patrimonial – Somatório dos códigos 55005 a 55006, 55008, 55010, 55014, 55044 + para cada informante as seguintes diferenças:
Código 57001 – Código 56001 (só considerar quando a diferença for > 0)
Código 57002 – Código 56002 (só considerar quando a diferença for > 0)
Código 57003 – Código 56003 (só considerar quando a diferença for > 0)
Código 57004 – Código 56004 (só considerar quando a diferença for > 0)*/


forvalues i=1/4{
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: gen  aux5700`i'= ingreso if id==5700`i'
replace aux5700`i'=0 if aux5700`i'==.
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform:egen condicion5700`i'=max(aux5700`i')
 }	
 
forvalues i=1/4{
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: gen  aux5600`i'= ingreso if id==5600`i'
replace aux5600`i'=0 if aux5600`i'==.
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform:egen condicion5600`i'=max(aux5600`i')
 }

forvalues i=1/4{
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform:gen  condicion`i'= condicion5700`i'- condicion5600`i'
 }
gen condicion=.
replace condicion=1 if condicion1>0| condicion2>0 | condicion3>0 | condicion4>0
replace condicion=0 if condicion==.

replace ing_trsgob_mon = 0 if ing_trsgob_mon==.

*-----------------------------------------------*
* INGRESO MONETARIO POR REMESAS INTERNACIONALES * 
*-----------------------------------------------*

gen ing_rem_mon= .

*---------------------------------------------------------------*
* INGRESO MONETARIO POR INGRESOS EXTRAORDINARIOS-OTROS INGRESOS *  
*---------------------------------------------------------------*

bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: egen suma1= sum(ingreso) if  (id==55005) 
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: egen suma2= sum(ingreso) if  (id==55006)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: egen suma3= sum(ingreso) if  (id==55008)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: egen suma4= sum(ingreso) if  (id==55010)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: egen suma5= sum(ingreso) if  (id==55014)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: egen suma6= sum(ingreso) if  (id==55044)



bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: gen diferencia1= condicion1 if condicion1>0 &  (id==57001)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: gen diferencia2= condicion2 if condicion2>0 &  (id==57002)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: gen diferencia3= condicion3 if condicion3>0 &  (id==57003)
bys  cod_uf num_seq num_dv cod_domc num_uc num_ordem_inform: gen diferencia4= condicion4 if condicion4>0 &  (id==57004)


egen aux3= rowtotal(suma1 suma2 suma3 suma4 suma5 suma6 diferencia1 diferencia2 diferencia3 diferencia4), missing
bys  cod_uf num_seq num_dv cod_domc:egen ing_otro_mon= sum(aux3)

replace ing_otro_mon= 0 if ing_otro_mon==.

*--------------------*
* INGRESO MONETARIO  *  
*--------------------*

egen aux= rowtotal(ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_otro_mon), missing
bys  cod_uf num_seq num_dv cod_domc :egen ing_mon= max(aux)
drop aux

**Se deja una sola observación por hogar y unidad de consumo,ademas de las variables relevantes**
bys cod_uf num_seq num_dv cod_domc : gen contador=_n
keep if contador==1

keep cod_uf num_seq num_dv cod_domc  ing_lab_mon ing_ren_mon ing_trspri_mon ing_trsgob_mon ing_ct_mon ing_rem_mon ing_otro_mon ing_mon fator_expansao2 miembros


preserve
**Se utilizan las bases de datos de gastos para obtener el ingreso no monetario**

         *=================================*
         * 1.2. INGRESO NO MONETARIO       *
         *=================================*

		 
	*=================================*
    *  1.2.1 APPEND                   *
    *=================================*
	
	
*Se utilizan las bases ubicadas en la carpeta RAW*

/*Se hace un append entre las  bases de datos que contienen los gastos para facilitar
la construccion por rubros de los gastos del hogar y el ingreso monetario*/

* 1.Gastos anuales *

use "$RAW\T_DESPESA_12MESES_S.dta" , clear 
	
**Se crea el identificador de la base**

gen base=1

* 2.Gastos trimestrales *
	
append using "$RAW\T_DESPESA_90DIAS_S.dta" 

replace base=2 if base==. 

* 3.Gastos individuales *
	
append using "$RAW\T_DESPESA_INDIVIDUAL_S.dta" 

replace base=3 if base==.

* 4.Gastos de vehiculo *

append using "$RAW\T_DESPESA_VEICULO_S.dta" 

replace base=4 if base==.

* 5.Cuadernillo de gastos *

append using "$RAW\T_CADERNETA_DESPESA_S.dta" 

replace base=5 if base==.

* 6. Otros Gastos *

append using "$RAW\T_OUTRAS_DESPESAS_S.dta" 

replace base=6 if base==.

* 7. Alquiler estimado*

append using "$RAW\T_ALUGUEL_ESTIMADO_S.dta" 

replace base=7 if base==.

* 8. Servicios domesticos*

append using "$RAW\T_SERVICO_DOMS_S.dta" 

replace base=8 if base==.


**Se generan las etiquetas para identificar las bases de datos**


label define base 1 "Despesa_12meses" 2 "Despesa_90dias" 3 "Despesa_individual" 4 "Despesa_veiculo" 5 "Despesa_Caderneta" 6 "Outras_Despesas" 7 "Aluguel_estimado" 8 "Servico_domestico"
label values base base

gen gasto= val_despesa_corrigido

**Se mensualizan las variable gasto según la periodicidad del código al cual perteneza**
replace gasto=gasto/3 if num_quadro==6
replace gasto=gasto/3 if num_quadro==7
replace gasto=gasto/3 if num_quadro==8
replace gasto=gasto/3 if num_quadro==9
**Se mensualiza el gasto para los gastos anuales (Despesa 12 meses) Codigo 10-13 **
replace gasto= gasto* qtd_meses if qtd_meses!=0 & base==1 
replace gasto=gasto/12 if base==1
**Se mensualiza el gasto para el alquiler estimado (Aluguel Estimado) Codigo 10**
replace gasto= gasto* qtd_meses if qtd_meses!=0 & base==7 
replace gasto=gasto/12 if base==7
**Se mensualizan las variable gasto según la periodicidad del código al cual perteneza**
replace gasto=gasto/12 if (num_quadro>=15 & num_quadro<=18)
**Se mensualiza el gasto para el servicio domestido (Servico Domestico) Codigo 19**
replace gasto= gasto* qtd_meses if qtd_meses!=0 & base==8 
replace gasto=gasto/12 if base==8
**Se mensualizan las variable gasto según la periodicidad del código al cual perteneza**
replace gasto=gasto*4.33 if (num_quadro>=22 & num_quadro<=27)
replace gasto=gasto/1 if (num_quadro>=28 & num_quadro<=30)
replace gasto=gasto/3 if (num_quadro>=31 & num_quadro<=44)
replace gasto=gasto/12 if (num_quadro>=45 & num_quadro<=51)
replace gasto=gasto*4.33 if (num_quadro>=63 & num_quadro<=69)

	
/*Se crea la variable id que permite identificar los codigos de los ingresos.
Esta variable id se compone de 5 digitos que corresponden  a los 2 digitos de la variable "número do quadro"
y a los 3 primeros digitos de la variable "código do item"*/

**Se extraen los tres primeros digitos de la variable "código do item"**

gen item= substr( cod_item,1,3 ) 

egen id= concat(num_quadro item)

destring id, replace

/*Para los "número do quadro" entre 06 y 09, el id queda de 4 dígitos, pues cuando se convierte a string el 0 que antepone a los numeros
6,7, 8 y 9 desaparece. De esta manera para estos cuadros, el id comienza por el numero 6, 7, 8 o 9 respectivamente, por ejemplo (6001, 7001, 8001 y 9001)*/

**Para la base de "$RAW\T_CADERNETA_DESPESA_S.dta" se usa la variable "prod_num_quadro_grupo_pro" para la construccion de el id**

egen aux= concat(prod_num_quadro_grupo_pro item) 
destring aux, replace
replace id=aux if base==5
drop aux

destring id, replace

/*Para el ingreso no monetario se considera unicamente el gasto cuando la forma de adquisicion es alguna de las siguientes:
(07) Donation, (08) Business Withdrawal, (09) Trade, (10) Self-Produced and (11) Other*/

/*No se consideran los códigos   47001-47999 (excepto 47023)  ya que son gastos de capital y
no propiamente gastos de consumo del hogar*/

drop if id>=47001 & id<=47022| (id>=47024 & id<=47999)

*---------------------------------*
* INGRESO NO MONETARIO LABORAL    * 
*---------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id!=10090 & (cod_obtencao==8 | cod_obtencao==9 | cod_obtencao==10 | cod_obtencao==11) 
bys cod_uf num_seq num_dv cod_domc :egen ing_lab_nomon= max(aux)
drop aux

replace ing_lab_nom=0 if ing_lab_nomon==.

*------------------------------------------------*
* INGRESO NO MONETARIO POR RENTA DE LA PROPIEDAD * 
*------------------------------------------------*

/*Se crea considerando la estructura propuesta por la encuesta: 

2 – Subtract from the referred value of code 10090 the values of all monetary expenses
(when means of acquisition is set between 1 and 6) in the following codes:
Code of chart 8 – 08001 to 08017, 08019 to 08072 and 08077 to 08999
Code of chart 10 – 10005, 10010, 10012 to 10013
Code of chart 12 – 12005 to 12025, 12030 to 12033 and 12999
Consider only when difference is positive (> 0)*/

**Se genera la variable de alquiler estimado**

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==10090
bys cod_uf num_seq num_dv cod_domc :egen alquiler_estimado= max(aux)
drop aux

**Se genera la suma de los codigos (monetarios) que deben restarsele al alquiler estimado**

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if (cod_obtencao>=1 & cod_obtencao<=6) & ((id>=8001 & id<=8017)| (id>=8019 & id<=8072) | (id>=8077& id<=8999) | (id==10005) | (id==10010) | (id==10012)| (id==10013) | (id>=12005 & id<=12025) | (id>=12030 & id<=12033) | (id==12999) )
bys cod_uf num_seq num_dv cod_domc :egen resta_monetaria= max(aux)
drop aux

**Se genera el ingreso no  monetario por renta de la propiedad**
gen ing_ren_nomon= alquiler_estimado-resta_monetaria

replace ing_ren_nomon = alquiler_estimado if resta_monetaria==.
replace ing_ren_nomon=0 if ing_ren_nomon<0

replace ing_ren_nomon=0 if ing_ren_nomon==.

*--------------------------------------------------*
* INGRESO NO MONETARIO POR TRANSFERENCIAS PRIVADAS * 
*--------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id!=10090 & (cod_obtencao==7)
bys cod_uf num_seq num_dv cod_domc :egen ing_trspri_nomon= max(aux)
drop aux

replace ing_trspri_nomon=0 if ing_trspri_nomon==.

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

egen ing_nomon= rowtotal( ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon), missing

**Se deja una sola observación por hogar y unidad de consumo,ademas de las variables relevantes**
bys cod_uf num_seq num_dv cod_domc : gen contador=_n
keep if contador==1

keep cod_uf num_seq num_dv cod_domc  ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon  ing_nomon 

**Se hace el merge con la base de ingresos monetarios**

tempfile ingreso_nomonetario
	save `ingreso_nomonetario' , replace
	restore 

mmerge cod_uf num_seq num_dv cod_domc using `ingreso_nomonetario' , t(1:1)
drop _merge


          *=======================================================*
          * 1.3. INGRESO CORRIENTE TOTAL (MONETARIO+NO MONETARIO) *
          *=======================================================*

**Se verifica que las variables creadas no tengan missing values (Ver notas al comienzo del dofile)**

replace ing_lab_mon=0 if ing_lab_mon==.
replace ing_ren_mon = 0 if ing_ren_mon==.
replace ing_trspri_mon = 0 if ing_trspri_mon==.
replace ing_ct_mon = 0 if ing_ct_mon==.
replace ing_trsgob_mon = 0 if ing_trsgob_mon==.
replace ing_otro_mon= 0 if ing_otro_mon==.
replace ing_mon= 0 if ing_mon==.

replace ing_lab_nomon=0 if ing_lab_nomon==.
replace ing_ren_nomon = 0 if ing_ren_nomon==.
replace ing_trspri_nomon = 0 if ing_trspri_nomon==.
replace ing_nomon= 0 if ing_nomon==.

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

preserve
/*	____________________________________________________________________________

	       ************** 2. GASTO DE LOS HOGARES **************
	____________________________________________________________________________	
*/
	
	
	
	      *=================================*
          * 2.0. APPEND                     *
          *=================================*
	

*Se utilizan las bases ubicadas en la carpeta RAW para obtener los ingresos del hogar*

/*Se hace un append entre las  bases de datos que contienen los gastos para facilitar
la construccion por rubros de los gastos del hogar. Se replica el procedimiento del ingreso no monetario*/

*Se utilizan las bases ubicadas en la carpeta RAW*

/*Se hace un append entre las  bases de datos que contienen los gastos para facilitar
la construccion por rubros de los gastos del hogar y el ingreso monetario*/

* 1.Gastos anuales *

use "$RAW\T_DESPESA_12MESES_S.dta" , clear 
	
**Se crea el identificador de la base**

gen base=1

* 2.Gastos trimestrales *
	
append using "$RAW\T_DESPESA_90DIAS_S.dta" 

replace base=2 if base==. 

* 3.Gastos individuales *
	
append using "$RAW\T_DESPESA_INDIVIDUAL_S.dta" 

replace base=3 if base==.

* 4.Gastos de vehiculo *

append using "$RAW\T_DESPESA_VEICULO_S.dta" 

replace base=4 if base==.

* 5.Cuadernillo de gastos *

append using "$RAW\T_CADERNETA_DESPESA_S.dta" 

replace base=5 if base==.

* 6. Otros Gastos *

append using "$RAW\T_OUTRAS_DESPESAS_S.dta" 

replace base=6 if base==.

* 7. Alquiler estimado*

append using "$RAW\T_ALUGUEL_ESTIMADO_S.dta" 

replace base=7 if base==.

* 8. Servicios domesticos*

append using "$RAW\T_SERVICO_DOMS_S.dta" 

replace base=8 if base==.


**Se generan las etiquetas para identificar las bases de datos**


label define base 1 "Despesa_12meses" 2 "Despesa_90dias" 3 "Despesa_individual" 4 "Despesa_veiculo" 5 "Despesa_Caderneta" 6 "Outras_Despesas" 7 "Aluguel_estimado" 8 "Servico_domestico"
label values base base

gen gasto= val_despesa_corrigido

**Se mensualizan las variable gasto según la periodicidad del código al cual perteneza**
replace gasto=gasto/3 if num_quadro==6
replace gasto=gasto/3 if num_quadro==7
replace gasto=gasto/3 if num_quadro==8
replace gasto=gasto/3 if num_quadro==9
**Se mensualiza el gasto para los gastos anuales (Despesa 12 meses) Codigo 10-13 **
replace gasto= gasto* qtd_meses if qtd_meses!=0 & base==1 
replace gasto=gasto/12 if base==1
**Se mensualiza el gasto para el alquiler estimado (Aluguel Estimado) Codigo 10**
replace gasto= gasto* qtd_meses if qtd_meses!=0 & base==7 
replace gasto=gasto/12 if base==7
**Se mensualizan las variable gasto según la periodicidad del código al cual perteneza**
replace gasto=gasto/12 if (num_quadro>=15 & num_quadro<=18)
**Se mensualiza el gasto para el servicio domestido (Servico Domestico) Codigo 19**
replace gasto= gasto* qtd_meses if qtd_meses!=0 & base==8 
replace gasto=gasto/12 if base==8
**Se mensualizan las variable gasto según la periodicidad del código al cual perteneza**
replace gasto=gasto*4.33 if (num_quadro>=22 & num_quadro<=27)
replace gasto=gasto/1 if (num_quadro>=28 & num_quadro<=30)
replace gasto=gasto/3 if (num_quadro>=31 & num_quadro<=44)
replace gasto=gasto/12 if (num_quadro>=45 & num_quadro<=51)
replace gasto=gasto*4.33 if (num_quadro>=63 & num_quadro<=69)

	
/*Se crea la variable id que permite identificar los codigos de los ingresos.
Esta variable id se compone de 5 digitos que corresponden  a los 2 digitos de la variable "número do quadro"
y a los 3 primeros digitos de la variable "código do item"*/

**Se extraen los tres primeros digitos de la variable "código do item"**

gen item= substr( cod_item,1,3 ) 

egen id= concat(num_quadro item)

destring id, replace

/*Para los "número do quadro" entre 06 y 09, el id queda de 4 dígitos, pues cuando se convierte a string el 0 que antepone a los numeros
6,7, 8 y 9 desaparece. De esta manera para estos cuadros, el id comienza por el numero 6, 7, 8 o 9 respectivamente, por ejemplo (6001, 7001, 8001 y 9001)*/

**Para la base de "$RAW\T_CADERNETA_DESPESA_S.dta" se usa la variable "prod_num_quadro_grupo_pro" para la construccion de el id**

egen aux= concat(prod_num_quadro_grupo_pro item) 
destring aux, replace
replace id=aux if base==5
drop aux

destring id, replace


/*No se consideran los códigos   -  Imóveis de uso ocasional – codigos (47005 a 47007, 47009 a 47020, 47025, 47999)  ya que son gastos de capital y
no propiamente gastos de consumo del hogar*/

**	Se consideran únicamente aquellos gastos de consumo del hogar o de la unidad de consumo, es decir, se eliminan aquellos gastos para otras unidades de consumo**

keep if cod_obtencao!=2 & cod_obtencao!=4 & cod_obtencao!=6


          *============================================*
          * 2.1.GASTO TOTAL POR CLASIFICACION DE GASTO *
          *============================================*


*-----------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS EN EL HOGAR *
*-----------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if  ((prod_num_quadro_grupo_pro!=86) & (prod_num_quadro_grupo_pro!=87) & (prod_num_quadro_grupo_pro!=88) & (prod_num_quadro_grupo_pro!=89) & (prod_num_quadro_grupo_pro!=83) & (prod_num_quadro_grupo_pro!=.)) 
bys cod_uf num_seq num_dv cod_domc :egen gasto_alihogar= max(aux)
drop aux

replace gasto_alihogar=0 if gasto_alihogar==.



*---------------------------------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS CONSUMIDOS FUERA DEL HOGAR *
*---------------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ( (num_quadro==24) |(id==41006) | (id==48033) | (id==49026))
bys cod_uf num_seq num_dv cod_domc :egen gasto_alifuera= max(aux)
drop aux

replace gasto_alifuera=0 if gasto_alifuera==.


*---------------------------------------*
* GASTO EN BEBIDAS ALCOHOLICAS Y TABACO *
*---------------------------------------*

**se incluye en este rubro las bebidas alcoholicas consumidas dentro y fuera del hogar**

**Se genera la variable de bebidas alcoholicas consumidas en el hogar para considerar más adelante en la construccion del gasto en bebidas alcoholicas y tabaco**

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if  prod_num_quadro_grupo_pro==83
bys cod_uf num_seq num_dv cod_domc :egen gasto_bebidas_alcoholicas_h= max(aux)
drop aux

replace gasto_bebidas_alcoholicas_h=0 if gasto_bebidas_alcoholicas_h==.

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=25001 & id<=25018) | (id==25999) | (id==38053) | (id==38067)| (id==38069) | (id==38070))
bys cod_uf num_seq num_dv cod_domc :egen gasto_tabaco= max(aux)
drop aux

replace gasto_tabaco=0 if gasto_tabaco==.


egen gasto_alta=rowtotal (gasto_tabaco gasto_bebidas_alcoholicas_h), missing

*-------------------------------*
* GASTO EN ALIMENTOS Y BEBIDAS  *
*-------------------------------*

egen gasto_ali= rowtotal(gasto_alihogar gasto_alifuera gasto_alta), missing

*----------------------------*
* GASTO EN VESTIDO Y CALZADO *
*----------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=34001 & id<=34033) | (id==34999) | (id>=35001 & id<=35059) | (id==35999) | (id>=36001 & id<=36048) | (id==36999) | (id>=38001 & id<=38052) | (id>=38054 & id<=38066) | (id==38068) | (id==38999) | (id>=38071 & id<=38072) | (id>=37001 & id<=37002) | (id==37008) | (id==37009) | (id==37014) | (id==37015) | (id>=37017 & id<=37019) | (id==37021) | (id>=37025 & id<=37030) | (id>=37034 & id<=37037) | (id==37044) | (id==37999) | (id==40001) | (id==40022) | (id>=46001 & id<=46008) | (id>=46010 & id<=46011) | (id==46999) )  
bys cod_uf num_seq num_dv cod_domc :egen gasto_veca= max(aux)
drop aux

replace gasto_veca=0 if gasto_veca==.


*-------------------------------------------------------------*
* GASTO EN VIVIENDA, SERVICIOS DE CONSERVACION Y COMBUSTIBLES *
*-------------------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if (( id==6001 ) | (id==6002) | (id==6003)| (id==07001) | (id==7004) | (id==7005)| (id>=7005 & id<=7011) | (id==6999)| (id==12003) | (id>=12006 & id<=12023) | (id==12025) | (id>=12030 & id<=12034) | (id==12999) | (id==10001) | (id==10003) | (id==10004) | (id==10006) | (id==10008) | (id==10009) | (id==10090))
bys cod_uf num_seq num_dv cod_domc :egen gasto_viv= max(aux)
drop aux

replace gasto_viv=0 if gasto_viv==.


*---------------*
* GASTO EN AGUA *
*---------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==6001 
bys cod_uf num_seq num_dv cod_domc :egen gasto_vag= max(aux)
drop aux

replace gasto_vag=0 if gasto_vag==.



*-----------------------*
* GASTO EN ELECTRICIDAD *
*-----------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==6002 
bys cod_uf num_seq num_dv cod_domc :egen gasto_vele= max(aux)
drop aux

replace gasto_vele=0 if gasto_vele==.


*-------------------------*
* GASTO EN GAS EN BOTELLA *
*-------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==7001) | (id==7004))
bys cod_uf num_seq num_dv cod_domc :egen gasto_vlp= max(aux)
drop aux

replace gasto_vlp=0 if gasto_vlp==.

*-------------------------*
* GASTO EN GAS NATURAL *
*-------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if (id==6003)
bys cod_uf num_seq num_dv cod_domc :egen gasto_vgn= max(aux)
drop aux

replace gasto_vgn=0 if gasto_vgn==.

*-------------------*
* GASTO EN KEROSENE *
*-------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==7005
bys cod_uf num_seq num_dv cod_domc :egen gasto_vk= max(aux)
drop aux

replace gasto_vk=0 if gasto_vk==.


*-------------------*
* GASTO EN GASOLINA *
*-------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==7009
bys cod_uf num_seq num_dv cod_domc :egen gasto_vgas= max(aux)
drop aux

replace gasto_vgas=0 if gasto_vgas==.


*---------------*
* GASTO EN LENA *
*---------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==7011
bys cod_uf num_seq num_dv cod_domc :egen gasto_vle= max(aux)
drop aux

replace gasto_vle=0 if gasto_vle==.



*-----------------*
* GASTO EN CARBON *
*-----------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==7010
bys cod_uf num_seq num_dv cod_domc :egen gasto_vca= max(aux)
drop aux

replace gasto_vca=0 if gasto_vca==.

*------------------------*
* GASTO EN LENA Y CARBON *
*------------------------*

egen gasto_vleca= rowtotal( gasto_vca gasto_vle), missing 

*-----------------*
* GASTO EN DIESEL *
*-----------------*

**Gasto en oleo diesel**

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if id==7008
bys cod_uf num_seq num_dv cod_domc :egen gasto_vdi= max(aux)
drop aux


replace gasto_vdi=0 if gasto_vdi==.

*------------------------------*
* OTROS GASTOS EN COMBUSTIBLES *
*------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if (id==7007) | (id==7006)
bys cod_uf num_seq num_dv cod_domc :egen gasto_vot= max(aux)
drop aux

replace gasto_vot=0 if gasto_vot==.


*-----------------------------------------------------*
* OTROS GASTOS EN VIVIENDA, SERVICIOS DE CONSERVACION *
*-----------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==6999)| (id==12003) | (id>=12006 & id<=12023) | (id==12025) | (id>=12030 & id<=12034) | (id==12999) | (id==10001) | (id==10003) | (id==10004) | (id==10006) | (id==10008) | (id==10009) | (id==10090))
bys cod_uf num_seq num_dv cod_domc :egen gasto_vcon= max(aux)
drop aux

replace gasto_vcon=0 if gasto_vcon==.


**Se crean variables de vivienda, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**

*-------------------*
* GASTO EN PETROLEO *
*-------------------*

gen gasto_vp=.


*----------------------------------------*
* GASTO EN PETROLEO, GASOLINA Y KEROSENE *
*----------------------------------------*

egen gasto_vpgk=rowtotal(gasto_vp gasto_vgas gasto_vk), missing


*----------------------------------------------------------*
* GASTO EN MUEBLES, ENSERES Y MANTENIMIENTO DE LA VIVIENDA *
*----------------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==7002) | (id==7003) |(id==7999) | (id>=8001 & id<=8068) | (id>=8070 & id<=8079) | (id==8999) | (id>=19001 & id<=19030) |(id==19099) | (id==86057) | (id==86083) |(id==86090) ///
| (id>=86001 & id<=86008) | (id==86011) | (id>=86014 & id<=86027) | (id>=86030 & id<=86034) | (id==86042) | (id==86045) | (id==86056) | (id==86058) | (id>=86060 & id<=86061) | (id>=86063 & id<=86066) | (id>=86069 & id<=86071) | (id==86073) | (id==86075) | (id==86077) ///
| (id==86086) |(id>=86092 & id<=86095) |(id>=15001 & id<=15124)  | (id>=15128 & id<=15148) | (id>=15150 & id<=15164) | (id==15166) | (id>=15168 & id<=15171) | (id==15173) |  (id==15175) |  (id==15177) | (id>=15180 & id<=15183) | (id>=15185 & id<=15197) | (id==15206) | (id>=15208 & id<=15999) | (id>=16016 & id<=16017) | (id==16023) | (id>=17001 & id<=17091) | (id==17999) | (id>=18001 & id<=18999) | (id>=37003 & id<=37006) | (id>=37010 & id<=37013) /// 
| (id==37016)| (id==37020)| (id==37023) | (id==37024) | (id>=37031 & id<=37033) | (id>=37038 & id<=37040) | (id==37043) | (id>=37045 & id<=37047) | (id>=39001 & id<=39999) | (id==40014) | (id>=40019 & id<=40020)| (id==86009) | (id==86010) | (id==86012) | (id==86013) | (id==86028) | (id==86029) | (id>=86035 & id<=86041) ///
| (id==86043) | (id==86044) | (id>=86046 & id<=86055)| (id==86059) | (id==86062) | (id==86067) | (id==86068) | (id==86072) | (id==86074) | (id==86076) | (id>=86078 & id<=86082) | (id==86084) | (id==86085) | (id>= 86087 & id<=86089) | (id==86091) | (id>=9001 &  id<=9999))

bys cod_uf num_seq num_dv cod_domc :egen gasto_ens= max(aux)
drop aux

replace gasto_ens=0 if gasto_ens==.

*----------------*
* GASTO EN SALUD *
*----------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=29001 & id<=29313) | (id==29999) | (id>=42001 & id<=42999))
bys cod_uf num_seq num_dv cod_domc :egen gasto_sal= max(aux)
drop aux

replace gasto_sal=0 if gasto_sal==.


*---------------------*
* GASTO EN TRANSPORTE *
*---------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==23007) | (id==23028) | (id==23006)  | (id>=23001 & id<=23005 ) | (id>=23010 & id<=23012) | (id>=23014 & id<=23016) | (id>=23020 & id<=23021) | (id==23023) | (id==23025) | (id==23026) | (id==23999) | (id==48034) | (id>=41001 & id<=41005) | (id>=41008 & id<=41017) | (id>=41020 & id<=41022) | (id>=41040 & id<=41042) | (id==41999) ///
| (id==23008) | (id==23009) | (id==23013) | (id>=23017 & id<=23018) | (id==23022) | (id==50003) | (id>=50005 & id<=50008) | (id==50010) | (id==50013) | (id==50017) | (id==50999) | (id==23019) | (id>=43001 & id<=43008) | (id>=43011 & id<=43013) | (id==43017) | (id==43019) | (id==43027) | (id==43029) | (id==43035) | (id==43040) | (id==43042) | (id==43999) | (id==50012) | (id>=51001 & id<=51023) | (id==51999))
bys cod_uf num_seq num_dv cod_domc :egen gasto_trans= max(aux)
drop aux

replace gasto_trans=0 if gasto_trans==.

*------------------------------*
* GASTO EN GASOLINA-TRANSPORTE *
*------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==23007) | (id==23028))
bys cod_uf num_seq num_dv cod_domc :egen gasto_tga= max(aux)
drop aux

replace gasto_tga=0 if gasto_tga==.

*---------------------------------*
* GASTO EN ALCOHOL-TRANSPORTE * 
*---------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if (id==23006) 
bys cod_uf num_seq num_dv cod_domc :egen gasto_talc= max(aux)
drop aux

replace gasto_talc=0 if gasto_talc==.

*----------------------------------*
* GASTO EN SERVICIOS DE TRANSPORTE *
*----------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ( (id>=23001 & id<=23005 ) | (id>=23010 & id<=23012) | (id>=23014 & id<=23016) | (id>=23020 & id<=23021) | (id==23023) | (id==23025) | (id==23026) | (id==23999) | (id==48034) | (id>=41001 & id<=41005) | (id>=41008 & id<=41017) | (id>=41020 & id<=41022) | (id>=41040 & id<=41042) | (id==41999))
bys cod_uf num_seq num_dv cod_domc :egen gasto_tserv= max(aux)
drop aux

replace gasto_tserv=0 if gasto_tserv==.

*----------------------------*
* OTROS GASTOS EN TRANSPORTE *
*----------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==23008) | (id==23009) | (id==23013) | (id>=23017 & id<=23018) | (id==23022) | (id==50003) | (id>=50005 & id<=50008) | (id==50010) | (id==50013) | (id==50017) | (id==50999) ) 
bys cod_uf num_seq num_dv cod_domc :egen gasto_totros= max(aux)
drop aux

replace gasto_totros=0 if gasto_totros==.

*-------------------------------------------------*
* GASTO EN REPARACION Y CONSERVACION DE VEHICULOS *
*-------------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==23019) | (id>=43001 & id<=43008) | (id>=43011 & id<=43013) | (id==43017) | (id==43019) | (id==43027) | (id==43029) | (id==43035) | (id==43040) | (id==43042) | (id==43999)) 
bys cod_uf num_seq num_dv cod_domc :egen gasto_tman= max(aux)
drop aux

replace gasto_tman=0 if gasto_tman==.

*--------------------------------------------*
* GASTO EN COMPRA Y ADQUISICION DE VEHICULOS *
*--------------------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==50012) | (id>=51001 & id<=51023) | (id==51999) ) 
bys cod_uf num_seq num_dv cod_domc :egen gasto_tadq= max(aux)
drop aux

replace gasto_tadq=0 if gasto_tadq==.

**Se crean variables de transporte, unas agregadas y otras como valores perdidos ya que no hay suficiente informacion**


*---------------------------------*
* GASTO EN GAS LICUADO-TRANSPORTE * 
*---------------------------------*

gen gasto_tlp=.

*-----------------*
* GASTO EN DIESEL * 
*-----------------*

gen gasto_tdie=.


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

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id==6004) | (id>=28023 & id<=28024) | (id>=6008 & id<=6011) | (id>=6013 & id<=6015) | (id==6005) | (id==6007) | (id==6012) | (id>=22001 & id<=22005) | (id==22999) ) 
bys cod_uf num_seq num_dv cod_domc :egen gasto_com= max(aux)
drop aux

replace gasto_com=0 if gasto_com==.

*---------------------------------*
* GASTO EN EDUCACION Y RECREACION *
*---------------------------------*

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=49001 & id<=49025) | (id>=49027 & id<=49999) | (id==28055) | (id==48003) | (id==48013) | (id==48036) | (id==32001) | (id==32002) | (id==32999))
bys cod_uf num_seq num_dv cod_domc :egen gasto_educacion= max(aux)
drop aux

replace gasto_educacion=0 if gasto_educacion==.

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=33001 & id<=33999) | (id==46009) | (id==46012) | (id==46013) | (id>=27001 & id<=27999) | (id==32004) | (id==32005) | (id>=28001 & id<=28022) | (id>=28025 & id<=28054) | (id>=28056 & id<=28999) | (id==41026) | (id>=41033 & id<=41039) | (id==45014) | (id>=45016 & id<=45018) ///
| (id>=15125 & id<=15127) | (id==15167) | (id==15178) | (id==15179) | (id==15184) | (id==15200) | (id==15207) | (id>=16001 & id<=16015) | (id>=16020 & id<=16022) | (id>=16028 & id<=16034) | (id>=16036 & id<=16038) | (id==16040) | (id>=37041 & id<=37042) )
bys cod_uf num_seq num_dv cod_domc :egen gasto_recreacion= max(aux)
drop aux

replace gasto_recreacion=0 if gasto_recreacion==.

egen gasto_edre= rowtotal( gasto_educacion gasto_recreacion), missing


*-----------------------------------*
* GASTO EN OTROS BIENES Y SERVICIOS *
*-----------------------------------*	

bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=45001 & id<=45013)| (id==45999) | (id>=44001 & id<=44021) |(id==44999) | (id>=31001 & id<=31999) | (id>=30001 & id<=30999) | (id>=89001 & id<=89015) | (id==12005) | (id==12035) | (id==41007) | (id==41018) | (id==41019) | (id>=41023 & id<=41025) | (id==47023) | (id>=11081 & id<=11084) | (id==12004) | (id>=13001 & id<=13999) | (id==16018) | (id==16019) | (id>=16024 & id<=16027) ///
| (id==16039) | (id==16999) | (id==32006) | (id==32015) | (id==37007) | (id==37022) | (id>=40002 & id<=40013) | (id>=40015 & id<=40018) | (id==40021) | (id>=40023 & id<=40026) | (id==40999)  | (id>=87001 & id<=87010) | (id>=87012 & id<=87015) | (id==88001) | (id>=26001 & id<=26999) )
bys cod_uf num_seq num_dv cod_domc :egen gasto_otros= max(aux)
drop aux

replace gasto_otros=0 if gasto_otros==.

**Codigo no incluidos**
bys cod_uf num_seq num_dv cod_domc : egen aux= sum(gasto) if ((id>=47005 & id<=47007) |  (id>=47009 & id<=47020) | (id==47025) | (id==47999))
bys cod_uf num_seq num_dv cod_domc :egen no_incluido= max(aux)
drop aux

*-----------------------*
* GASTO CORRIENTE TOTAL *
*-----------------------*

egen gct= rowtotal (gasto_alihogar gasto_alifuera gasto_alta gasto_veca gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vpgk gasto_vlp gasto_vdi gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_com gasto_edre gasto_otros), missing

**Se deja una sola observación por hogar y unidad de consumo,ademas de las variables relevantes**
bys cod_uf num_seq num_dv cod_domc : gen contador=_n
keep if contador==1

keep cod_uf num_seq num_dv cod_domc fator_expansao2  gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros 


**Se hace el merge con la base de ingresos**

tempfile gastos
	save `gastos' , replace
	restore 

mmerge cod_uf num_seq num_dv cod_domc  using `gastos' , t(1:1)
drop _merge

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

 
keep cod_uf num_seq num_dv cod_domc fator_expansao2 miembros gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros ///
ing_tpriv ing_tpub ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_ct ing_trsgob ing_rem ing_otro

**Se generan variables de identificacion del hogar, pais, anio y encuesta**
**Dado que esta encuesta se realizó entre el 19 de mayo de 2008 y el 18 de mayo de 2009, por lo cual se escoge el 2008 como el anio de la encuesta, pues tuvo la mayoria de meses.*
gen cod_hogar=_n
label var cod_hogar "Codigo del hogar" 
gen pais= "BRA"
label var pais "Pais" 
gen anio=2008
label var anio "Anio de la encuesta" 
gen encuesta="POF"
label var encuesta "Encuesta" 

**Se renombran variables relevantes**
rename miembros miembros_hogar
label var miembros_hogar "Cantidad de miembros en el hogar" 
rename fator_expansao2 factor_expansion
label var factor_expansion "Factor de Expansion" 

order  pais anio encuesta cod_hogar  miembros_hogar factor_expansion ing_mon ing_lab_mon ing_ren_mon ing_trspri_mon ing_ct_mon ing_trsgob_mon ing_rem_mon ing_otro_mon ing_nomon ing_lab_nomon ing_ren_nomon ing_trspri_nomon ing_ct_nomon ing_trsgob_nomon ing_rem_nomon ing_otro_nomon ict ing_lab ing_ren ing_trspri ing_rem ing_tpriv ing_ct ing_trsgob ing_tpub ing_otro ///
gct gasto_ali gasto_alihogar gasto_alifuera gasto_alta gasto_veca  gasto_viv  gasto_vgn gasto_vag gasto_vele gasto_vleca gasto_vle gasto_vca	 gasto_vlp	 gasto_vdi	 gasto_vp 	gasto_vgas	 gasto_vpgk gasto_vk gasto_vot gasto_vcon gasto_ens gasto_sal gasto_tadq gasto_tman gasto_totros gasto_tserv gasto_tcomb gasto_tga	gasto_tlp gasto_tdie gasto_tgnc	gasto_talc	gasto_totcomb gasto_trans gasto_com gasto_edre gasto_otros cod_uf num_seq num_dv cod_domc
	  
saveold "$data_arm\BRA_POF_2008-2009.dta" , replace v(12)
