* (Versión stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor òn©£amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHC
local ANO "2020"
local ronda s2 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
cap log using "`log_file'", replace 

cap log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Paî³º Argentina
Encuesta: EPHC
Round: IISem-2019
Autores: 
Versión 2012: Yessenia Loaysa
 Alvaro Altamirano (LMK/SCL) - Email: alvaroalt@iadb.org, 24 de junio de 2020
Última versión: Cesar Lins (SCL/GDI) Marzo 2021

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=4


	************
	* region_c *
	************
	
gen region_c=.
replace region_c=1  if (aglomerado>=2 & aglomerado<=3) | (aglomerado>=33 & aglomerado<=34) | (aglomerado==38) /*Buenos Aires */
replace region_c=2  if aglomerado==22                          /*Catamarca*/
replace region_c=3  if aglomerado==8                           /*Chaco*/
replace region_c=4  if aglomerado==9 | aglomerado==91          /*Chubut*/
replace region_c=5  if aglomerado==32                          /*Ciudad de Buenos Aires*/
replace region_c=6  if aglomerado==13 | aglomerado==36         /*Córdoba*/
replace region_c=7  if aglomerado==12                          /*Corrientes*/
replace region_c=8  if aglomerado==6 | aglomerado==14          /*Entre Ríos*/
replace region_c=9  if aglomerado==15                          /*Formosa*/
replace region_c=10 if aglomerado==19                          /*Jujuy*/
replace region_c=11 if aglomerado==30                          /*La pampa*/
replace region_c=12 if aglomerado==25                          /*La Rioja*/
replace region_c=13 if aglomerado==10                          /*Mendoza*/
replace region_c=14 if aglomerado==7                           /*Misiones*/
replace region_c=15 if aglomerado==17                          /*Neuquen*/
replace region_c=16 if aglomerado==93                          /*Rî¯ Negro*/ 
replace region_c=17 if aglomerado==23                          /*Salta*/
replace region_c=18 if aglomerado==27                          /*San Juan*/ 
replace region_c=19 if aglomerado==26                          /*San Luis*/
replace region_c=20 if aglomerado==20                          /*Santa Cruz*/
replace region_c=21 if aglomerado>=4 & aglomerado<=5           /*Santa Fe*/
replace region_c=22 if aglomerado==18                          /*Santiago de Estero*/
replace region_c=23 if aglomerado==31                          /*Tierra del Fuego*/
replace region_c=24 if aglomerado==29                          /*Tucuman*/

	label define region_c     ///
	1"Buenos Aires"           ///	
	2"Catamarca"              ///
	3"Chaco"                  /// 
	4"Chubut"                 ///
	5"Ciudad de Buenos Aires" ///
	6"Córdoba"                ///
	7"Corrientes"             ///
	8"Entre Ríos"             ///
	9"Formosa"                ///
	10"Jujuy"                 ///
	11"La Pampa"              ///
	12"La Rioja"              ///
	13"Mendoza"               ///
	14"Misiones"              ///
	15"Neuquon"               ///
	16"Río Negro"             ///
	17"Salta"                 ///
	18"San Juan"              ///
	19"San Luis"              ///
	20"Santa Cruz"            ///
	21"Santa Fe"              ///
	22"Santiago del Estero"   ///
	23"Tierra del Fuego"      ///
	24"Tucumán"                       
   label value region_c region_c
   label var region_c "division politico-administrativa, provincia"
   
	*******************************************
	*Factor de expansion del hogar (factor_ch)*
	*******************************************

	gen factor_ch=pondera
	label var factor_ch "Factor de expansion del hogar"
	
	*******************************************
	*unidad primaria de muestreo*
	*******************************************	
	gen upm_ci=aglomerado

	*******************************************
	*estrato*
	*******************************************	
	
	gen estrato_ci=.
	

		*************************
		***VARIABLES DEL HOGAR***
		*************************
		
	******************
	*idh_ch (idhogar)*
	******************
	
	sort codusu aglomerado nro_hogar trimestre
	egen idh_ch=group(codusu aglomerado nro_hogar trimestre)

	********
	*idp_ci*
	********
	
	gen idp_ci=componente
	
	******
	*zona*
	******
	*NOTA: sigue siendo Urbana: 29 aglomerados
	
	gen zona_c=1
	
	******
	*pais*
	******

	gen str3 pais_c="ARG"

	******
	*anio*
	******
	
	gen anio_c=2020


	**********
	*semestre*
	**********

	gen semestre_c=2
	
	*************
	*relacion_ci*
	*************
	
	gen relacion_ci=1 if ch03==1
	replace relacion_ci=2 if ch03==2
	replace relacion_ci=3 if ch03==3
	replace relacion_ci=4 if ch03>=4 & ch03<=9
	replace relacion_ci=5 if ch03==10 
	*replace relacion_ci=6 if componente==51 /* no se clasifica mas de esta forma desde 2013 t3)*/
	
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4  "Otros parientes" 5 "Otros no parientes"
label variable relacion_ci "Relacion con el jefe del hogar"



			****************************
			***VARIABLES DEMOGRAFICAS***
			****************************

	***********
	*factor_ci* 
	***********

	gen factor_ci=pondera
	

	*********
	*sexo_ci*
	*********
	
	capture gen sexo_ci=ch04
	drop if sexo_ci>2 | sexo_ci<1 
	
	*********
	*edad_ci*
	*********
	
	capture gen edad_ci=ch06
	replace edad_ci=0 if edad_ci==-1
	replace edad_ci=98 if edad_ci>=98
	
	**************
	*Estado Civil*
	**************
	
	recode ch07 (1 2=2 "Union formal o informal") (3=3 "Divorciado o separado") (4=4 "Viudo") (5=1 "Soltero") (else=.), gen(civil_ci) 
	label variable civil_ci "Estado civil"
	
	
	*********
	*jefe_ci*
	*********

	gen jefe_ci=(relacion_ci==1)
	
	**************
	*nconyuges_ch*
	**************

	by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
	

	***********
	*nhijos_ch*
	***********
	
	by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
	
	**************
	*notropari_ch*
	**************

	by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
	
	****************
	*notronopari_ch*
	****************
	
	by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
	

	************
	*nempdom_ch*
	************

	*NOTA: a traves de la relacion de parentesco no es posible identificar a los empleados domesticos
	*Se pregunta a parte si el individuo presta servicios domesticos. No obstante, no se sabe si pertenecen
	*al hogar encuestado directamente, por ello se aproxima a esta medida usando la relacion de parentesco
	gen empldom_ci=0
	replace empldom_ci=1 if pp04b1==1
	
	by idh_ch, sort: egen nempdom_ch=sum(empldom_ci==1) if relacion_ci==5	  
	

	*************
	*clasehog_ch*
	*************

	gen byte clasehog_ch=0
		**** unipersonal
	replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
		**** nuclear (child with or without spouse but without other relatives)
	replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0
		**** nuclear (spouse with or without children but without other relatives)
	replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0
		**** ampliado
	replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
		**** compuesto (some relatives plus non relative)
	replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
		**** corresidente
	replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

	**************
	*nmiembros_ch*
	**************

	by idh_ch, sort: egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5)
	
	*************
	*nmayor21_ch*
	*************

	by idh_ch, sort: egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))

	*************
	*nmenor21_ch*
	*************

	by idh_ch, sort: egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))

	*************
	*nmayor65_ch*
	*************

	by idh_ch, sort: egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65 & edad_ci!=.))

	************
	*nmenor6_ch*
	************

	by idh_ch, sort: egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))

	************
	*nmenor1_ch*
	************

	by idh_ch, sort: egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))

	************
	*miembros_ci
	************
	
	gen miembros_ci=(relacion_ci>=1 & relacion_ci<5) 

	*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
	***afroind_ci***
	***************
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 
	

			***********************************
			***VARIABLES DEL MERCADO LABORAL***
			***********************************

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if estado==1
replace condocup_ci=2 if estado==2
replace condocup_ci=3 if estado==3 & edad_ci>=10
replace condocup_ci=. if estado == 0
replace condocup_ci=4 if estado == 4
label var condocup_ci "Condicion de ocupación ¤e acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
	
	
************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)

	*************
	*desalent_ci*
	*************
	*ANTERIOR: gen desalent_ci=(pea1_ci~=1 & (p01==2 & p07==2) & p08==4) 
	*p08==4 razon de no busqueda es que cree que no encontrara trabajo
	***********
	*Se toman las personas que reportan haberse cansado de buscar y que pertenecen a la PET

	
	gen desalent_ci=.

*Note: Se deberî¡ incrementar la categoria 4 "hay poco trabajo en esta epoca de añ¯¢

	*************	
	*horaspri_ci*
	*************
	destring pp3e_tot, ignore(",") replace
	gen horaspri_ci=pp3e_tot
	replace horaspri_ci=. if pp3e_tot==999
	
	************* 
	*horastot_ci*
	*************
	destring pp3f_tot, ignore(",") replace
	gen otrashoras=pp3f_tot if pp3f_tot!=999
	
	egen horastot_ci=rsum(horaspri_ci otrashoras), missing 
	replace horastot_ci=. if horaspri_ci==. & otrashoras==.
	

	***********
	*subemp_ci*
	***********

	gen subemp_ci=0
	replace subemp_ci=1 if pp03g==1 & (horastot_ci>=1 & horastot_ci<=30) & emp_ci==1
	replace subemp_ci =. if emp_ci ==.
	
	/*tab subemp_ci
replace subemp_ci=1 if intensi==1 | intensi==2
recode subemp_ci .=0 if intensi==3 | intensi==4 | intensi==5
tab subemp_ci
*/
	*Note: Se corrige y se consideran las horas totales

	***************
	*tiempoparc_ci*
	***************
	
	gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30) & (pp03g==2 & emp_ci==1)
	replace tiempoparc_ci=. if emp_ci==0
	
	
	**************
	*categopri_ci*
	**************
	
	gen categopri_ci=cat_ocup if emp_ci==1
	replace categopri_ci=. if categopri_ci<1 | categopri_ci>4
	

	**************
	*categosec_ci*
	**************

	gen categosec_ci=.
	

	*************
	*contrato_ci*
	*************
	
*	gen contrato_ci=.

	***********
	*segsoc_ci* 
	*********We don't consider the people that declare to have 
	*aguinaldo (4), vacaciones (8), Vacaciones y Aguinaldo (12), Indemnizacion (32),
	*Indemnizacion y Aguinaldo (36)	indemnización ¹ Vacaciones (40) and Indemnización¬ 
	*vacaciones y aguinaldo (44).
	*AHORA:
	*Sigue siendo solo para empleados (categopri_ci==3) solo se incluyen jubilaciones PERO LO QUE SE QUIERE VER ES 
	*AFILIACION A SALUD!!! 
	***********/
	
*	gen segsoc_ci=(categopri_ci==3 & pp07h==1) 
*	replace segsoc_ci=. if emp_ci~=1
	
*Note: se debe considerar t tambien?

	*************
	*nempleos_ci*
	*************
	
	gen nempleos_ci=pp03d
	replace nempleos_ci=1 if pp03c==1
	replace nempleos_ci=. if emp_ci!=1
	

	*************
	*firmapeq_ci* 
	*************
	
*	gen firmapeq_ci=0 if emp_ci==1
*	replace firmapeq_ci=1 if pp04c>=1 & pp04c<=5 & emp_ci==1
	

	*************
	*spublico_ci* 
	*************
	
	gen spublico_ci=(pp04a==1 & emp_ci==1)
	replace spublico_ci=. if pp04a==0 
	

	**********
	*Ocupa_ci*
	**********
	*NOTA: desde 2001 hay otra clasificacion, pero debe estudiarse como hacer las agrupaciones para la 
	*construccion de la variable tal como esta propuesta para la armonizacion.

tostring pp04d_cod, replace
gen ocup1=substr(pp04d_cod,1,2)
gen ocup2=substr(pp04d_cod,3,1)
gen ocup3=substr(pp04d_cod,4,1)
gen ocup4=substr(pp04d_cod,5,1)

destring ocup1 ocup2 ocup3 ocup4, replace
/*
lab var ocup1 "patron"
lab var ocup2 "asalariados"
lab var ocup3 "cuenta_propia"
lab var ocup4 "sin_salario"
*/

*************
***ocupa_ci**
*************
capture drop ocupa_ci	
gen ocupa_ci=1 if ocup4>=1 & ocup4<=2
replace ocupa_ci=2 if ocup1>=0 & ocup1<=7 & ocupa_ci !=1
replace ocupa_ci=3 if (ocup1==10 | ocup1==11 | ocup1==20) & ocupa_ci !=1
replace ocupa_ci=4 if ocup1>=30 & ocup1<=33 & ocupa_ci !=1
replace ocupa_ci=5 if (ocup1>=36 & ocup1<=47 | ocup1>=52 & ocup1<=58) & ocupa_ci !=1
replace ocupa_ci=6 if ocup1>=60 & ocup1<=65 & ocupa_ci !=1
replace ocupa_ci=7 if ocup1>=70 & ocup1<=92 & ocupa_ci !=1
replace ocupa_ci=8 if ocup1>=48 & ocup1<=49 & ocupa_ci !=1
replace ocupa_ci=9 if (ocup1==34 | ocup1==35 | ocup1==50 | ocup1==51)  & ocupa_ci !=1
replace ocupa_ci=. if estado !=1 & ocup1==99
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio" ///
4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas"    ///
7 "obreros no agricolas, conductores de maq y ss de transporte" 8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
	

	*********
	*rama_ci*
	*********

*2015, 12 MLO cambio la clasificacion y por lo tanto debe cambiarse la codificacion
destring pp04b_cod, replace 	

gen rama_ci = .
replace rama_ci = 1 if ((pp04b_cod==1)|(pp04b_cod>=101 &  pp04b_cod<=300)) & emp_ci==1
replace rama_ci = 2 if ((pp04b_cod>=500 & pp04b_cod<=900) ) & emp_ci==1
replace rama_ci = 3 if ((pp04b_cod>=1000 & pp04b_cod<=3300) |  pp04b_cod==10 |  pp04b_cod==19  |  pp04b_cod==20 |  pp04b_cod==21 |  pp04b_cod==26) & emp_ci==1
replace rama_ci = 4 if ((pp04b_cod>=3500 & pp04b_cod<=3900)) & emp_ci==1
replace rama_ci = 5 if ((pp04b_cod==4000)) & emp_ci==1
replace rama_ci = 6 if ((pp04b_cod>=4500 & pp04b_cod<4900) | pp04b_cod==48 | (pp04b_cod>=5500 & pp04b_cod<=5602)) & emp_ci==1
replace rama_ci = 7 if ((pp04b_cod>=4900 & pp04b_cod<=5300) | pp04b_cod==49) & emp_ci==1
replace rama_ci = 8 if ((pp04b_cod>=6400 & pp04b_cod<=8200) ) & emp_ci==1
replace rama_ci = 9 if ((pp04b_cod>=5800 & pp04b_cod<=6300) |(pp04b_cod>=8300 &  pp04b_cod<=9900) | pp04b_cod==85 | pp04b_cod==63 | pp04b_cod==61  | pp04b_cod==58) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


	
	
	************
	*durades_ci*
	************
	*Esta variable se capturo como categorica por ello se crea como missing. Sin embargo, se 
	*guarda la informacion en una nueva variable durades1_ci
	
	gen durades_ci=.
	
	gen durades1_ci=pp10a
	replace durades1_ci=. if pp10a==0 | pp10a==9
		
	***************
	*antiguedad_ci*
	***************
	*NOTA: antes la variable era continua, ahora esta en intervalos

	*Para emp domesticos (continua)
	gen ant_m=pp04b3_mes
	replace ant_m=. if pp04b3_mes==-1 | pp04b3_mes==99 |pp04b3_mes<0
	gen ant_a=pp04b3_ano 
	replace ant_a=. if pp04b3_ano==-1 | pp04b3_ano==99 |pp04b3_ano<0 
	replace ant_m=ant_m/12
	egen antiguedad1=rsum(ant_a ant_m), missing
	replace antiguedad1=. if pp04b3_mes==0 & pp04b3_ano==0 	

	*Para trabajadores familiares (continua)
	gen ant_mc=pp05b2_mes
	replace ant_mc=. if pp05b2_mes==-1 | pp05b2_mes==99 |pp05b2_mes<0
	gen ant_ac=pp05b2_ano
	replace ant_ac=. if pp05b2_ano==-1 | pp05b2_ano==99 |pp05b2_ano<0 
	replace ant_mc=ant_mc/12
	egen antiguedad2=rsum(ant_a ant_m), missing
	replace antiguedad2=. if pp05b2_mes==0 & pp05b2_ano==0 	

	*Para Empleados y obreros
	*SE USA EL VALOR MEDIO DE CADA RANGO DE ANIOS PARA CONSTRUIR LOS ANIOS DE ANTIGUEDAD, (SE CREA TRUNCADA)*

	gen antiguedad3=0 if pp07a==1
	replace antiguedad3=0.17 if pp07a==2
	*[1-3 meses---2/12=0.17]*
	replace antiguedad3=0.33 if pp07a==3
	*[3-6 meses---4/12=0.33]*
	replace antiguedad3=0.75 if pp07a==4
	*[6m- 1a---9/12=0.75]*
	replace antiguedad3=3 if pp07a==5
	*[1-5---3=0.33]*
	replace antiguedad3=5 if pp07a==6
	*[mas de 5]*
	replace antiguedad3=. if pp07a==0 | pp07a==9
	
	
	*Para trabajadores Independientes
	gen antiguedad4     = 0    if pp05h==1
	replace antiguedad4 = 2/12 if pp05h==2
	replace antiguedad4 = 4/12 if pp05h==3
	replace antiguedad4 = 9/12 if pp05h==4
	replace antiguedad4 = 3    if pp05h==5
	replace antiguedad4 = 5    if pp05h==6
	replace antiguedad4 = .    if pp05h==0 | pp05h==9
	
	*Agregando
	gen antiguedad_ci=	antiguedad1 if antiguedad1!=.
	replace antiguedad_ci= antiguedad2 if antiguedad2!=.
	replace antiguedad_ci= antiguedad3 if antiguedad3!=.
	replace antiguedad_ci= antiguedad4 if antiguedad4!=.
*Note: A los empleados e independientes se les esta dejando un mâ¹©mo de 5 añ¯³ de antiguedad.
	

			**************************
			***VARIABLES DE INGRESO***
			**************************
	        
	***********
	*ylmpri_ci*
	***********

	*NOTA: se hace la suma de todos los ingresos monetarios por el trabajo principal.
	*Pero en general correspone a la variable p21. Esos chequeos estan en la version anterior de este
	*programa
		
	gen ylmpri_ci=.
	replace ylmpri_ci=p21 if p21>=0 & p21!=. & emp_ci==1
			
	*************
	*nrylmpri_ci*
	*************

	gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
	
	************
	*ylnmpri_ci*
	************
	*En esta encuesta se pregunta si recibe pago no monetario, pero no se pregunta cual es el vr de esto
	gen ylnmpri_ci=.
	

	***************
	***ylmsec_ci***
	***************

	gen ylmsec_ci=.
	

	****************
	***ylnmsec_ci***
	****************

	gen ylnmsec_ci=.
	

	*************
	*ylmotros_ci*
	*************


	gen ylmotros_ci=tot_p12 if tot_p12>=0
	
	 
	******************
	***ylnmotros_ci***
	******************

	gen ylnmotros_ci=.
	

	********
	*ylm_ci*
	********

	/***********
	Para crear el ingreso laboral anteriormente se sumaban los ingresos provenientes de trabajar como
	asalariado, cuenta propista y patron. Pero en este caso son excluyentes y a pesar de que en la encuesta
	trimestral existe una variable para los ingresos de las demas ocupaciones (tot_p12) en la encuesta 
	semestral no aparece. No se que hacer con el monto de aguinaldo, bonificaciones no habituales y monto retrocativos*/
	
	egen ylm_ci=rsum(ylmpri_ci ylmotros_ci), missing
	replace ylm_ci=. if ylmpri_ci==. &  ylmotros_ci==.
	

	*********
	*ylnm_ci*
	*********
	
	gen ylnm_ci=.
	label var ylnm_ci "Ingreso laboral NO monetario total"  	

	*********
	*ynlm_ci*
	*********
	
	gen ynlm_ci=.
	replace ynlm_ci = t_vi
	replace ynlm_ci=. if ynlm_ci<0

	
	
	**********
	*ynlnm_ci*
	**********
	
	gen ynlnm_ci=.
	

		**********************
		***HOUSEHOLD INCOME***
		**********************

	*************
	*nrylmpri_ch*
	*************
	*Creating a Flag label for those households where someone has a ylmpri_ci as missing

	by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
	replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
	replace nrylmpri_ch=. if nrylmpri_ch==.


	********
	*ylm_ch*
	********

	by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

	*********
	*ylnm_ch*
	*********
	
	gen ylnm_ch=.

	**********
	*ylmnr_ch*
	**********
	
	by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
	replace ylmnr_ch=. if nrylmpri_ch==1

	*********
	*ynlm_ch*
	*********
	
	by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

	**********
	*ynlnm_ch*
	**********
	
	gen ynlnm_ch=.

	********
	***NA***
	********
	gen rentaimp_ch=.

	gen autocons_ci=.

	gen autocons_ch=.


	************
	*remesas_ci*
	************
	
	gen remesas_ci=.

	
	************
	*remesas_ch*
	************
	
	gen remesas_ch=.


	*************
	*ylmhopri_ci*
	*************
	
	gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri_ci)
	replace ylmhopri_ci=. if ylmhopri_ci<=0


	**********
	*ylmho_ci*
	**********

	gen ylmho_ci=ylm_ci/(horastot_ci*4.3)


			****************************
			***VARIABLES DE EDUCACION***
			****************************


	*********
	*aedu_ci* 
	*********
	*NOTA: Como terciario, universitario y posgrado tienen una duración ¶ariable se supone 
	*que terciario completo implica 3 anios de educacion adicional a la secundaria, 5 adicionales a universitario 
	*y 2 a postgrado. Esto solo se basa en el criterio de que la modas de finalización ¥stos niveles suelen
	*ser estas. ESTO SE DEBE DISCUTIR 
/*
	gen aedu_ci=0 if (ch10==0 | ch10==3) 
	replace aedu=0 if ch12==1 & ch13==2
	replace aedu=0 if ch12==2 & ch14=="00" & ch13==2
	replace aedu=0 if ch12==3 & ch14=="00" & ch13==2

	replace aedu=1 if ch12==2 & ch14=="01" & ch13==2
	replace aedu=1 if ch12==3 & ch14=="01" & ch13==2
	replace aedu=2 if ch12==2 & ch14=="02" & ch13==2
	replace aedu=2 if ch12==3 & ch14=="02" & ch13==2
	replace aedu=3 if ch12==2 & ch14=="03" & ch13==2
	replace aedu=3 if ch12==3 & ch14=="03" & ch13==2
	replace aedu=4 if ch12==2 & ch14=="04" & ch13==2
	replace aedu=4 if ch12==3 & ch14=="04" & ch13==2
	replace aedu=5 if ch12==2 & ch14=="05" & ch13==2
	replace aedu=5 if ch12==3 & ch14=="05" & ch13==2
	replace aedu=6 if ch12==2 & ch14=="06" & ch13==2
	replace aedu=6 if ch12==3 & ch14=="06" & ch13==2
	replace aedu=7 if ch12==2 & ch14=="07" & ch13==2
	replace aedu=7 if ch12==2 & ch13==1
	replace aedu=7 if ch12==3 & ch14=="07" & ch13==2
	replace aedu=8 if ch12==3 & ch14=="08" & ch13==2
	replace aedu=8 if ch12==4 & ch14=="01" & ch13==2
	replace aedu=9 if ch12==3 & ch14=="09" & ch13==2
	replace aedu=9 if ch12==3 & ch13==1
	replace aedu=9 if ch12==4 & ch14=="02" & ch13==2
	replace aedu=10 if ch12==4 & ch14=="03" & ch13==2 
	replace aedu=10 if ch12==5 & ch14=="01" & ch13==2
	replace aedu=11 if ch12==4 & ch14=="04" & ch13==2
	replace aedu=11 if ch12==5 & ch14=="02" & ch13==2
	replace aedu=12 if ch12==4 & ch14=="05" & ch13==2
	replace aedu=12 if ch12==4 & ch13==1
	replace aedu=12 if ch12==5 & ch14=="03" & ch13==2
	replace aedu=12 if ch12==5 & ch13==1
	replace aedu=13 if ch12==4 & ch14=="06" & ch13==2
	replace aedu=13 if ch12==6 & ch14=="01" & ch13==2
	replace aedu=13 if ch12==7 & ch14=="01" & ch13==2
	replace aedu=14 if ch12==6 & ch14=="02" & ch13==2
	replace aedu=14 if ch12==7 & ch14=="02" & ch13==2
	replace aedu=15 if ch12==7 & ch14=="03" & ch13==2
	replace aedu=15 if ch12==6 & ch13==1
	replace aedu=16 if ch12==7 & ch14=="04" & ch13==2
	replace aedu=17 if ch12==7 & ch13==1
	replace aedu=18 if ch12==7 & ch13==1
	replace aedu=19 if ch12==8 & ch13==1
	replace aedu=. if ch12==9 | ch12==99

*/
* Mod. 8/2015 Ivan Bonacelli EDU/SLC
   *Ajustando variables
	replace ch11=. if ch11==9
	replace ch12=. if ch12==99 | ch12==9 // El ò¬´©mo condicional es para sacar a la población £on educación ¥special
	replace ch13=. if ch13==9
	destring ch14, replace
	replace ch14=. if ch14==98 | ch14==99
	
	
	*Variable de Añ¯ ¤e Educación	
	
	gen aedu_ci=.
	
	*Para quienes no terminaron el ò¬´©mo nivel educativo al que asistió
	replace aedu_ci=0 if (ch10==0 | ch10==3) // Cero añ¯³ de educación °ara aquellos que no ha asistido nunca a ninguna instituciones y los menores de 2 añ¯³n
	replace aedu_ci=0 if ch12==1 // Prescolar
	replace aedu_ci=ch14 if ch12==2 | ch12==3 & ch13==2
	replace aedu_ci=ch14+7 if ch12==4 & ch13==2
	replace aedu_ci=ch14+9 if ch12==5 & ch13==2
	replace aedu_ci=ch14+12 if ch12==6 & ch13==2
	replace aedu_ci=ch14+12 if ch12==7 & ch13==2
	replace aedu_ci=ch14+17 if ch12==8 & ch13==2
		
	*Para quienes terminaron el ò¬´©mo nivel educativo al que asistió
	replace aedu_ci=7 if ch12==2 & ch13==1
	replace aedu_ci=9 if ch12==3 & ch13==1
	replace aedu_ci=12 if ch12==4 & ch13==1
	replace aedu_ci=12 if ch12==5 & ch13==1
	replace aedu_ci=17 if ch12==6 & ch13==1
	replace aedu_ci=17 if ch12==7 & ch13==1
	replace aedu_ci=19 if ch12==8 & ch13==1
	
	*Imputando para los que tenemos certeza del nivel educativo mâ³ alto alcanzado
	replace aedu_ci=7 if nivel_ed==2 & aedu_ci==.
	replace aedu_ci=12 if nivel_ed==4 & aedu_ci==.
	replace aedu_ci=17 if nivel_ed==6 & aedu_ci==.
	replace aedu_ci=0 if nivel_ed==7 & aedu_ci==.
	
	label var aedu_ci "Anios de educacion aprobados" 	

	**********
	*eduno_ci* 
	**********
	
	gen eduno_ci=(nivel_ed==7)
	
	**********
	*edupi_ci*
	**********
	
	gen edupi_ci=(nivel_ed==1)

	**********
	*edupc_ci* 
	**********
	
	gen edupc_ci=(nivel_ed==2)

	**********
	*edusi_ci*
	**********
	
	gen edusi_ci=(nivel_ed==3)

	**********
	*edusc_ci*
	**********
	
	gen edusc_ci=(nivel_ed==4)

	**********
	*eduui_ci*
	**********
	
	gen eduui_ci=(nivel_ed==5)

	**********
	*eduuc_ci*
	**********
	
	gen eduuc_ci=(nivel_ed==6)

	***********
	*edus1i_ci*
	***********

	gen byte edus1i_ci=.

	***********
	*edus1c_ci*
	***********
	*edus1c_ci: primer ciclo de secundaria completa
	gen byte edus1c_ci=(aedu_ci==9)

	***********
	*edus2i_ci*
	***********

	gen byte edus2i_ci=.

	***********
	*edus2c_ci*
	***********

	gen byte edus2c_ci=.

	***********
	*edupre_ci*
	***********

	*Cambio realizado 11/7/2016 por Ivân Bornacelly - Creando la dummy que identifica a la población ±ue asiste a Educación rescolar
	gen byte edupre_ci=.
	replace edupre_ci=1 if ch12==1
	recode edupre_ci(.=0)

	************
	*asispre_ci*
	************
	*Nueva variable incoporada 01/11/2017 por Ivân Bornacelly
	g asispre_ci=.
	replace asispre_ci=1 if ch10==1 & ch12==1 & ch06>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"
	
	**********
	*eduac_ci*
	**********
	gen byte eduac_ci=.
	

/** Mod. 8/2015 Ivan Bonacelli EDU/SLC
Nota: Queda alrededor de 8% de la muestra que tiene asignado un nivel educativo incompleto a la que no se le puede asignar añ¯³ de educaciónEsta parte está¡­â³ adelante y por ahora el impacto mâ³ visible es que la población £on educación ¥special se les asigna de manera directa 3 añ¯³ de educación £uando no deberî¢n ser contabilizados. Se sugiere que mientras evaluamos el tema de la imputación ¤e los añ¯³ de Educación °ara Argentina se desactive la segunda lî¯¥a del siguiente có¤©§o:*/


replace eduno_ci=1	if aedu_ci==0 
*replace aedu_ci=3 	if aedu_ci==. & edupi_ci==1
replace aedu_ci=7 	if aedu_ci==. & edupc_ci==1
replace aedu_ci=9 	if aedu_ci==. & edusi_ci==1
replace aedu_ci=12 	if aedu_ci==. & edusc_ci==1
replace aedu_ci=14 	if aedu_ci==. & eduui_ci==1
replace aedu_ci=18 	if aedu_ci==. & eduuc_ci==1

	***********
	*asiste_ci*
	***********
	
	gen asiste_ci=(ch10==1)

	*************
	*pqnoasis_ci*
	*************

	gen pqnoasis_ci=.
	label var pqnoasis_ci "Razones para no asistir a la escuela"
	
	**Daniela Zuluaga- Enero 2019: Se agrega la variable pqnoasis1_ci**
	
	**************
	*pqnoasis1_ci*
	**************
	
	gen pqnoasis1_ci=. 

	***********
	*repite_ci*
	***********

	gen repite_ci=.

	**************
	*repiteult_ci*
	**************

	gen repiteult_ci=.


	***********
	*edupub_ci*
	***********
	gen edupub_ci =.
	replace edupub_ci = 1 if ch11==1
	replace edupub_ci = 0 if ch11==2 
	
	*************
	***tecnica_ci**
	*************
	* Cambio Angela López noviembre 2019
	gen     tecnica_ci= .
	replace tecnica_ci= 1 if ch12==6

	gen     universidad_ci= .
	replace universidad_ci= 1 if ch12==6
*Ch12==1 corresponde a jardín/preescolar, no hay variable técnica.
/*
replace tecnica_ci=1 if ch12==1
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"
*/

		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************


	************
	*aguared_ch*
	************

	gen aguared_ch=.
	replace aguared_ch=(iv7==1)
	replace aguared_ch=. if iv7==9


	*************
	*aguadist_ch*
	*************

	gen aguadist_ch=.
	replace aguadist_ch=(iv6==1 | iv6==2)
	replace aguadist_ch=. if iv6==9 | iv6==.

	*************
	*aguamala_ch*
	*************

	gen aguamala_ch=(iv7==4)
	replace aguamala_ch=. if iv7==9

	*************
	*aguamide_ch*
	*************

	gen aguamide_ch=.

	********
	*luz_ch*
	********
	*En la nueva encuesta no se encontro si se pregunta por instalacion electrica
	gen luz_ch=.

	************
	*luzmide_ch*
	************

	gen luzmide_ch=.


	************
	*combust_ch*
	************

	gen combust_ch=0
	replace combust_ch=1 if ii8==1 | ii8==2 
	replace combust_ch=. if ii8==0 | ii8==9

	*********
	*bano_ch*
	*********

	gen bano_ch=0
	replace bano_ch=1 if ii9!=4
 	replace bano_ch=. if ii9==9 | ii9==0


	***********
	*banoex_ch*
	***********

	gen banoex_ch=0
	replace banoex_ch=1 if ii9==1
	replace banoex_ch=. if ii9==0 | ii9==9


	*********
	*des1_ch*
	*********


	gen des1_ch=.
	replace des1_ch=0 if bano_ch==0
	replace des1_ch=1 if iv11==1 | iv11==2
	replace des1_ch=2 if iv11==3 | iv11==4
	replace des1_ch=. if iv11==0 | iv11==9


	*********
	*des2_ch*
	*********

	gen des2_ch=.
	replace des2_ch=0 if bano_ch==0
	replace des2_ch=1 if iv11==1 | iv11==2 | iv11==3 | iv11==4
	replace des2_ch=. if iv11==0 | iv11==9


	*********
	*piso_ch*
	*********
    * Modificaciones Daniela Zuluaga Junio 2017: se agrega la opcion piso_ch=2 if iv3==4 (otros). Esta opcion antes indicaba missing value y no se estaba teniendo en cuenta.
	gen piso_ch=0 	
	replace piso_ch=1 if iv3==1 | iv3==2
	replace piso_ch=2 if iv3==4
	replace piso_ch=. if iv3==0 | iv3==9 
	label var piso_ch "Materiales de construcción del piso"  
	label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
	label val piso_ch piso_ch


	**********
	*pared_ch*
	**********

	gen pared_ch=.
	label var pared_ch "Materiales de construcción de las paredes"


	**********
	*techo_ch*
	**********
*Modificación SGR Julio 2019
	gen techo_ch=0 if  iv4==6 | iv4==7
	replace techo_ch=1  if iv4>=1 & iv4<=5
    replace techo_ch=1  if iv4==9 & iv5==1
	replace techo_ch=2  if iv4==9 & iv5==2
	label var techo_ch "Materiales de construcción del techo" 
	label def techo_ch 1"Materiales permanentes"  0"Materiales no permanentes" 2 "Otros Materiales"
	label val techo_ch techo_ch 
	
	**********
	*resid_ch*
	**********

	gen resid_ch =.
 
	*********
	*dorm_ch*
	*********
	gen dorm_ch=ii2 if ii3==2
	replace dorm_ch=. if ii2==99

	************
	*cuartos_ch*
	************

	gen cuartos_ch=ii1
	replace cuartos_ch=. if ii1==99


	***********
	*cocina_ch*
	***********

	gen cocina_ch=0 
	replace cocina_ch=1 if ii4_1==1
	replace cocina_ch=. if ii4_1==0 | ii4_1==9


	**********
	*telef_ch*
	**********

	gen telef_ch=.


	***********
	*refrig_ch*
	***********

	gen refrig_ch=.


	**********
	*freez_ch*
	**********

	gen freez_ch=.
	

	*********
	*auto_ch*
	*********

	gen auto_ch=.
	

	**********
	*compu_ch*
	**********

	gen compu_ch=.


	*************
	*internet_ch*
	*************

	gen internet_ch=.


	********
	*cel_ch*
	********

	gen cel_ch=.

	**********
	*vivi1_ch*
	**********

	gen vivi1_ch=1 if iv1==1
	replace vivi1_ch=2 if iv1==2
	replace vivi1_ch=3 if iv1==3 | iv1==4 | iv1==5 | iv1==6



	**********
	*vivi2_ch*
	**********

	gen vivi2_ch=0
	replace vivi2_ch=1 if iv1==1 | iv1==2


	*************
	*viviprop_ch*
	*************
	*NOTA: aqui se genera una variable parecida, pues no se puede saber si es propia total o parcialmente pagada
	gen viviprop_ch=.
	
	gen viviprop_ch1=0 if ii7==3
	replace viviprop_ch1=1 if ii7==1 | ii7==2
	replace viviprop_ch1=3 if ii7>=4 & ii7<=9 
	replace viviprop_ch1=. if ii7==99

	************
	*vivitit_ch*
	************

	gen vivitit_ch=.

	************
	*vivialq_ch*
	************

	gen vivialq_ch=.


	***************
	*vivialqimp_ch*
	***************
	gen vivialqimp_ch=.
	
	**Daniela Zuluaga- Enero 2019: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
	*********************
    ***aguamejorada_ch***
    *********************
	gen  aguamejorada_ch = 1 if iv7 == 1  | iv7 ==2 | iv7 ==3 //No se utiliza la pregunta de ubicación del grifo porque no se detallan las fuentes de agua
	replace aguamejorada_ch = 0 if iv7 == 4
		
	*********************
    ***banomejorado_ch***
    *********************
   gen  banomejorado_ch = 1 if (iv8 == 1 & (iv10 == 1 | iv10 == 2)  & (iv11==1 | iv11==2 | iv11==3) & ii9 == 1)
   replace banomejorado_ch = 0 if (iv8 == 1 & (iv10 == 1 | iv10 == 2)  & (iv11==1 | iv11==2 | iv11==3) & (ii9 == 2| ii9==3)) | (iv8 == 1 & (iv10 == 1 | iv10 == 2 | iv10 == 3) & (iv11==4) & (ii9 == 1 | ii9 == 2 | ii9 ==3)) | (iv8 == 1 & iv10 == 3 & (ii9 == 1 | ii9 == 2 | ii9 ==3)) | iv8 == 2
	
	
	
	gen byte muestra_92=(aglomerado==32 | aglomera==33 | aglomera==6 | aglomera==9 | aglomera==19 | aglomera==23 | aglomera==26 | aglomera==30 | aglomera==26 | aglomera==30 | aglomera==13 | aglomera==10 | aglomera==4| aglomera==29)
	

/************************************************************************************************************
* 3. Creación ¤e nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

/* https://www.indec.gob.ar/uploads/informesdeprensa/eph_pobreza_01_18.pdf pÃ¡gina 10. 
calculo: Canasta BÃ¡sica Total promedio del hogar pobre/TamaÃ±o promedio del hogar pobre en adulto equivalente,
Canasta BÃ¡sica Alimentaria promedio del hogar indigente/TamaÃ±o promedio del hogar indigente en adulto equivalente */ 


*promedio julio - diciembre 2020:  https://www.indec.gob.ar/uploads/informesdeprensa/eph_pobreza_02_2082FA92E916.pdf

*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci =15167

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =6201
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if pp07h==1 
replace cotizando_ci=0 if pp07h==2 
replace cotizando_ci=. if pp07h==0
recode cotizando_ci .=0 if (estado==1 & (categopri_ci==1 | categopri_ci==2))| (estado==1 & (categosec_ci==1 | categosec_ci==2))  /*independiente que no cotiza en primera/segunda ocupacion*/ 
replace cotizando_ci =0 if estado==2										/* desocupados no cotizan*/
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
recode afiliado_ci .=0 if pea_ci==1 & desemp_ci==0
label var afiliado_ci "Afiliado a la Seguridad Social"


****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************
* No hay variable de firmo o no contrato, solo tipo de trabajo. MGD 06/17/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if pp07c==2 & categopri_ci==3
replace tipocontrato_ci=2 if pp07c==1 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
* MLO 2013, 03
gen cesante_ci=1 if pp10d==1 /* ha trabajado anteriormente*/
*gen cesante_ci=1 if pp10d==2
recode cesante_ci .=0 if condocup_ci==2
* No todos los desempleados respondieron si han trabajado antes
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************
gen tamemp_ci=1 if (pp04c >= 1 & pp04c <= 5)
replace tamemp_ci=1 if (pp04c == 99 & pp04c99 == 1)

replace tamemp_ci=2 if pp04c>5 & pp04c<=8
replace tamemp_ci=2 if (pp04c == 99 & pp04c99 == 2)
replace tamemp_ci=3 if pp04c>8 & pp04c<12 & pp04c!=. & pp04c!=99
replace tamemp_ci=3 if pp04c == 99 & pp04c99 == 3

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

gen pension_ci=1 if (v2_m>0 & v2_m<.) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen aguinpen=v21_m/12 if v2_m>0 & v2_m!=.

egen ypen_ci=rsum(v2_m aguinpen), missing
replace ypen_ci=. if ypen_ci<0
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen byte pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**salmm_ci***
*************

* https://www.boletinoficial.gob.ar/detalleAviso/primera/213533/20190815 *
*Salario mínimo fijado en enero 2020
gen salmm_ci=16875
label var salmm_ci "Salario minimo legal"


******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if cat_inac==1
replace categoinac_ci=2 if cat_inac==3
replace categoinac_ci=4 if cat_inac==4
recode categoinac_ci .= 4 if condocup_ci==3

label var categoinac_ci "Condición ¤e inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
	
***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

*******************
***  seguro_ci  ***
*******************

g benefdes_ci=0 if desemp_ci==1
replace benefdes_ci=1 if  v4==1 & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** yseguro_ci  ***
*******************
g ybenefdes_ci=v4_m if benefdes_ci==1 & v4_m>0
label var ybenefdes_ci "Monto de seguro de desempleo"

*Se generan las siguientes variables, para las cuales no hay informaciÃ³n en la encuesta*
gen mes_c=.
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen instcot_ci=.



******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(inlist(ch15,4,5)) if ch15!=. & ch15!=9		/* Categoria Ns./Nr. no se incluye en la variable*/
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & inlist(ch16,1,2,3)) if !inlist(ch16,6,9) & migrante_ci!=.		/* Categorias Ns./Nr. y no habia nacido no se incluyen en la variable*/
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	cap: tostring ch15_cod, replace
	gen migrantelac_ci=((ch15==4 | inlist(ch15_cod,"201","202","203","205","206","208","209","210") | ///
	inlist(ch15_cod,"211","213","214","215","216","217","218","219","220") | ///
	inlist(ch15_cod,"221","222","224","225","226","232","233","236","237") | ///
	inlist(ch15_cod,"239","240")) & migrante_ci==1) if migrante_ci!=. 
	replace migrantelac_ci=. if ch15_cod=="999" & ch15!=4
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	/* Fuente: https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/codigospaises_09.pdf */

* Variables incluidas por SCL/MIG Juan Camilo Perdomo
	
	**********************
	*** migrantiguo5_ci **
	**********************
	gen migrantiguo5_ci = 1 if inlist(ch16,1,2,3) & migrante_ci==1
	replace migrantiguo5_ci = 0 if inlist(ch16,4,5) & migrante_ci==1
	replace migrantiguo5_ci = . if inlist(ch16,6,9) | migrante_ci==0
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	****** miglac_ci *****
	**********************
	gen miglac_ci = 1 if (ch15==4 | inlist(ch15_cod,"201","202","203","205","206","207","208","209","210") | ///
	inlist(ch15_cod,"211","213","214","215","216","217","218","219","220") | ///
	inlist(ch15_cod,"221","222","224","225","226","232","233","236","237") | ///
	inlist(ch15_cod,"239","240")) & migrante_ci == 1
	replace miglac_ci = 0 if miglac_ci != 1 & migrante_ci == 1
	replace miglac_ci =. if migrante_ci == 0
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC" 
	
/*_____________________________________________________________________________________________________*/
* Asignación ¤e etiquetas e inserción ¤e variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  lî¯¥as de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación ¤e que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y dejarlo en base armonizada 
para anÃ¡lisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename pp04d_cod codocupa

compress


foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHC
local ANO "2020"
local ronda s2 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

saveold "`base_out'", version(12) replace

cap log close

