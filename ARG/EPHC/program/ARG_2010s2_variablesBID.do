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
 


global ruta = "${surveysFolder}"

local PAIS ARG
local ENCUESTA EPHC
local ANO "2010"
local ronda s2 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Argentina
Encuesta: EPHC
Round: IISem-2010
Autores: 
Version 2010: Yanira

Versión 2012: Yessenia Loaysa
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Marzo de 2013

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
replace region_c=6  if aglomerado==13 | aglomerado==36         /*Córdova*/
replace region_c=7  if aglomerado==12                          /*Corrientes*/
replace region_c=8  if aglomerado==6 | aglomerado==14          /*Entre Ríos*/
replace region_c=9  if aglomerado==15                          /*Formosa*/
replace region_c=10 if aglomerado==19                          /*Jujuy*/
replace region_c=11 if aglomerado==30                          /*La pampa*/
replace region_c=12 if aglomerado==25                          /*La Rioja*/
replace region_c=13 if aglomerado==10                          /*Mendoza*/
replace region_c=14 if aglomerado==7                           /*Misiones*/
replace region_c=15 if aglomerado==17                          /*Neuquen*/
replace region_c=16 if aglomerado==93                          /*Río Negro*/ 
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
	*capture rename pond_sem pondera if ano4==2003

	gen factor_ch=pondera
	label var factor_ch "Factor de expansion del hogar"

		*************************
		***VARIABLES DEL HOGAR***
		*************************
		
	******************
	*idh_ch (idhogar)*
	******************

	sort codusu aglomerado nro_hogar trimestre
	egen idh_ch=group(codusu aglomerado nro_hogar trimestre)
	label variable idh_ch "ID del hogar"
	

	********
	*idp_ci*
	********
	
	gen idp_ci=componente
	label variable idp_ci "ID de la persona en el hogar"
	

	******
	*zona*
	******
	*NOTA: sigue siendo Urbana: 29 aglomerados
	
	gen zona_c=1
	label variable zona_c "Zona del pais"
	label define zona_c 1 "Urbana" 0 "Rural"
	label value zona_c zona_c


	******
	*pais*
	******

	gen str3 pais_c="ARG"
	label variable pais_c "Pais"


	******
	*anio*
	******
	
	gen anio_c=ano4
	label variable anio_c "Anio de la encuesta" 


	**********
	*semestre*
	**********
	gen semestre_c=2
	label var semestre "Semestre de la encuesta" 
	
	*************
	*relacion_ci*
	*************
	
	gen relacion_ci=1 if ch03==1
	replace relacion_ci=2 if ch03==2
	replace relacion_ci=3 if ch03==3
	replace relacion_ci=4 if ch03>=4 & ch03<=9
	replace relacion_ci=5 if ch03==10 
	label variable relacion_ci "Relacion con el jefe del hogar"
	label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 
	label define relacion_ci  5 "Otros no Parientes", add 
	label values relacion_ci relacion_ci
	


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
	label var sexo_ci "Sexo del individuo" 


	*********
	*edad_ci*
	*********
	
	capture gen edad_ci=ch06
	replace edad_ci=0 if edad_ci==-1
	replace edad_ci=98 if edad_ci>=98
	label variable edad_ci "Edad del individuo"


	
	**************
	*Estado Civil*
	**************
	
	recode ch07 (1=2) (2=2) (3=3) (4=4) (5=1) (9=.), gen(civil_ci) 
	label variable civil_ci "Estado civil"
	label define civil_ci 1 "Soltero" 2 "Union formal o informal"
	label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
	label value civil_ci civil_ci

	
	*********
	*jefe_ci*
	*********

	gen jefe_ci=(relacion_ci==1)
	label variable jefe_ci "Jefe de hogar"


	**************
	*nconyuges_ch*
	**************

	by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
	label variable nconyuges_ch "Numero de conyuges"


	***********
	*nhijos_ch*
	***********
	
	by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
	label variable nhijos_ch "Numero de hijos"

	**************
	*notropari_ch*
	**************

	by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
	label variable notropari_ch "Numero de otros familiares"	

	****************
	*notronopari_ch*
	****************
	
	by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
	label variable notronopari_ch "Numero de no familiares"


	************
	*nempdom_ch*
	************

	*NOTA: a traves de la relacion de parentesco no es posible identificar a los empleados domesticos
	*Se pregunta a parte si el individuo presta servicios domesticos. No obstante, no se sabe si pertenecen
	*al hogar encuestado directamente, por ello se aproxima a esta medida usando la relacion de parentesco
	gen empldom_ci=0
	replace empldom_ci=1 if pp04b1==1
	label var empldom_ci "El individuo es empleado domestico" 

	by idh_ch, sort: egen nempdom_ch=sum(empldom_ci==1) if relacion_ci==5	  
	label variable nempdom_ch "Numero de empleados domesticos"


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

	label variable clasehog_ch "Tipo de hogar"
	label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
	label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
	label value clasehog_ch clasehog_ch


	**************
	*nmiembros_ch*
	**************

	by idh_ch, sort: egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5)
	label variable nmiembros_ch "Numero de familiares en el hogar"


	*************
	*nmayor21_ch*
	*************

	by idh_ch, sort: egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))
	label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

	*************
	*nmenor21_ch*
	*************

	by idh_ch, sort: egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))
	label variable nmenor21_ch "Numero de familiares menores a 21 anios"

	*************
	*nmayor65_ch*
	*************

	by idh_ch, sort: egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65 & edad_ci!=.))
	label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

	************
	*nmenor6_ch*
	************

	by idh_ch, sort: egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))
	label variable nmenor6_ch "Numero de familiares menores a 6 anios"

	************
	*nmenor1_ch*
	************

	by idh_ch, sort: egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))
	label variable nmenor1_ch "Numero de familiares menores a 1 anio"

	************
	*miembros_ci
	************
	
	gen miembros_ci=(relacion_ci>=1 & relacion_ci<5) 
	label variable miembros_ci "Miembro del hogar"
	
			
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
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
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
/*
	********
	*emp_ci*
	********

	gen emp_ci =(estado==1)
    label var emp_ci "Ocupado (empleado)"


	************
	*desemp1_ci*
	************
	*Ya no se puede discriminar quienes buscaron trabajo en la semana de referencia
	*Pues aunque se pregunta esta informacion no esta en la base de datos. Todo se resume 
	*en la variable estado
	
	gen desemp1_ci=(estado==2)
	label var desemp1_ci "Desempleado que buscó empleo en el periodo de referencia"

*Note: Aunque la definición de desempleado a través de la variable estado (la busqueda de empleo 
*es el mes)encaja más en la variable desemp3, se decide ponerla en desemp1_ci para cuando toque hacer agregaciones de 
*America Latina, Argentina no tenga missing en el desempleo. 

	************
	*desemp2_ci*
	************
	*Tampoco se puede distinguir quienes estan esperando una respuesta de un empleo. No obstante, es posible
	*saber quienes esta esperando la temporada alta, pero no esta la variable en la base de datos.
		
	gen desemp2_ci=.
	label var desemp2_ci "desemp1_ci + personas que esperan respuesta a solicitud o temporada alta"


	************
	*desemp3_ci*
	************ 
		
	gen desemp3_ci=.
	label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"


	*********
	*pea1_ci* 
	*********
	
	gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
	label var pea1_ci "Población Económicamente Activa con desemp1_ci"


	*********
	*pea2_ci*
	*********
	
	gen pea2_ci=.
	label var pea2_ci "Población Económicamente Activa con desemp2_ci"	


	*********
	*pea3_ci* 
	*********
	
	gen pea3_ci=.
	label var pea3_ci "Población Económicamente Activa con desemp3_ci"

*/
	*************
	*desalent_ci*
	*************
	*ANTERIOR: gen desalent_ci=(pea1_ci~=1 & (p01==2 & p07==2) & p08==4) 
	*p08==4 razon de no busqueda es que cree que no encontrara trabajo
	***********/
	*Se toman las personas que reportan haberse cansado de buscar y que pertenecen a la PET

	gen desalent_ci=0 if edad_ci>=10
	replace desalent_ci=1 if pp02e==3 | pp02e==4 
	label var desalent_ci "Trabajadores desalentados"

*Note: Se debería incrementar la categoria 4 "hay poco trabajo en esta epoca de año"

	*************	
	*horaspri_ci*
	*************

	gen horaspri_ci=pp3e_tot
	replace horaspri_ci=. if pp3e_tot==999
	label var horaspri_ci "Horas trabajadas en la actividad principal"

	
	************* 
	*horastot_ci*
	*************

	gen otrashoras=pp3f_tot if pp3f_tot!=999
	
	egen horastot_ci=rsum(horaspri_ci otrashoras), missing 
	replace horastot_ci=. if horaspri_ci==. & otrashoras==.
	label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	

	***********
	*subemp_ci*
	***********

	* Modificacion MGD 06/24/2014: solo horas principales y se considera disponibilidad a trabajar mas horas.
	gen subemp_ci=0
	replace subemp_ci=1 if pp03g==1 & (pp03h==1 | pp03h==2) & (horaspri_ci>=1 & horaspri_ci<=30) & emp_ci==1
		label var subemp_ci "Personas en subempleo por horas"
	/*tab subemp_ci
replace subemp_ci=1 if intensi==1 | intensi==2
recode subemp_ci .=0 if intensi==3 | intensi==4 | intensi==5
tab subemp_ci*/
*Note: Se corrige y se consideran las horas totales

	***************
	*tiempoparc_ci*
	***************
	
	gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<=30) & (pp03g==2 & emp_ci==1)
	replace tiempoparc_ci=. if emp_ci==0
	label var tiempoparc_c "Personas que trabajan medio tiempo" 

	
	**************
	*categopri_ci*
	**************
	
	gen categopri_ci=cat_ocup if emp_ci==1
	replace categopri_ci=. if categopri_ci<1 | categopri_ci>4
	label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categopri_ci 3"Empleado" 4" No remunerado" , add
	label value categopri_ci categopri_ci
	label variable categopri_ci "Categoria ocupacional"


	**************
	*categosec_ci*
	**************

	gen categosec_ci=.
	label variable categosec_ci "Categoria ocupacional trabajo secundario"


	*************
	*contrato_ci*
	*************
	
	*gen contrato_ci=.
	*label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"


	***********
	*segsoc_ci* 
	***********

	
	*ANTERIOR: We don't consider the people that declare to have 
	*aguinaldo (4), vacaciones (8), Vacaciones y Aguinaldo (12), Indemnizacion (32),
	*Indemnizacion y Aguinaldo (36)	indemnización y Vacaciones (40) and Indemnización, 
	*vacaciones y aguinaldo (44).
	*AHORA:
	*Sigue siendo solo para empleados (categopri_ci==3) solo se incluyen jubilaciones PERO LO QUE SE QUIERE VER ES 
	*AFILIACION A SALUD!!! 
	***********/
	
	*gen segsoc_ci=(categopri_ci==3 & pp07h==1) 
	*replace segsoc_ci=. if emp_ci~=1
	*label var segsoc_ci "Personas que tienen seguridad social en PENSIONES por su trabajo"

*Note: se debe considerar pp07i tambien?

	*************
	*nempleos_ci*
	*************
	
	gen nempleos_ci=pp03d
	replace nempleos_ci=1 if pp03c==1
	replace nempleos_ci=. if emp_ci!=1
	label var nempleos_ci "Número de empleos" 


	*************
	*firmapeq_ci* 
	*************
	
	*gen firmapeq_ci=0 if emp_ci==1
	*replace firmapeq_ci=1 if pp04c>=1 & pp04c<=5 & emp_ci==1
	*label var firmapeq_ci "Trabajadores informales"


	*************
	*spublico_ci* 
	*************
	
	gen spublico_ci=(pp04a==1 & emp_ci==1)
	replace spublico_ci=. if pp04a==0 
	label var spublico_ci "Personas que trabajan en el sector público"


	**********
	*Ocupa_ci*
	**********
	*NOTA: desde 2001 hay otra clasificacion, pero debe estudiarse como hacer las agrupaciones para la 
	*construccion de la variable tal como esta propuesta para la armonizacion.
*************
***ocup1-4***
*************

gen ocup1=substr(pp04d_cod,1,2)
gen ocup2=substr(pp04d_cod,3,1)
gen ocup3=substr(pp04d_cod,4,1)
gen ocup4=substr(pp04d_cod,5,1)

destring ocup1 ocup2 ocup3 ocup4, replace

lab var ocup1 "patron"
lab var ocup2 "asalariados"
lab var ocup3 "cuenta_propia"
lab var ocup4 "sin_salario"

*************
***ocupa_ci**
*************

capture drop ocupa_ci	
/*
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
*/

* Modificacion MGD 06/26/2014: se reclasifica a profesionales y tecnicos y a administrativos segun la nueva clasificacion.
gen ocupa_ci=.
replace ocupa_ci=1 if (ocup1>=10 & ocup1<=20) & (ocup4==1 | ocup4==2) & emp_ci==1
replace ocupa_ci=2 if ocup1>=0 & ocup1<=7 & emp_ci==1
replace ocupa_ci=3 if (ocup1>=10 & ocup1<=20) & (ocup4==3 | ocup4==4) & emp_ci==1
replace ocupa_ci=4 if (ocup1>=30 & ocup1<=33) & emp_ci==1
replace ocupa_ci=5 if ((ocup1>=34 & ocup1<=47) | (ocup1>=50 & ocup1<=58)) &  emp_ci==1
replace ocupa_ci=6 if (ocup1>=60 & ocup1<=65) & emp_ci==1 
replace ocupa_ci=7 if (ocup1>=70 & ocup1<=92) & emp_ci==1
replace ocupa_ci=8 if (ocup1>=48 & ocup1<=49) & emp_ci==1 
replace ocupa_ci=9 if (ocup1==13 | ocup1==28 | ocup1==99) | ((ocup1>=10 & ocup1<=20) & (ocup4>=5 & ocup4<=9))   & emp_ci==1
replace ocupa_ci=. if ocup1==.
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio" ///
4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas"    ///
7 "obreros no agricolas, conductores de maq y ss de transporte" 8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

	*********
	*rama_ci*
	*********
*2014, 02 modificacion MLO para que quede en sintonia con nueva codificacion a partir de 2011.
	destring pp04b_cod, replace
	gen rama_ci=.
	replace rama_ci = 1 if ((pp04b_cod==1)|(pp04b_cod>=101 &  pp04b_cod<=500)) & emp_ci==1
	replace rama_ci = 2 if ((pp04b_cod>=10 & pp04b_cod<=14) |(pp04b_cod>=1000 &  pp04b_cod<=1400)) & emp_ci==1
	replace rama_ci = 3 if ((pp04b_cod>=15 & pp04b_cod<=36) |(pp04b_cod>=1501 &  pp04b_cod<=3609)) & emp_ci==1
	replace rama_ci = 4 if ((pp04b_cod>=37 & pp04b_cod<=41) |(pp04b_cod>=3700 &  pp04b_cod<=4100)) & emp_ci==1
	replace rama_ci = 5 if ((pp04b_cod==45) |(pp04b_cod==4500)) & emp_ci==1
	replace rama_ci = 6 if ((pp04b_cod>=50 & pp04b_cod<=55) |(pp04b_cod>=5001 &  pp04b_cod<=5503) &  pp04b_cod!=5311 & pp04b_cod!=5311) & emp_ci==1
	replace rama_ci = 7 if ((pp04b_cod>=60 & pp04b_cod<=64) |(pp04b_cod>=6001 &  pp04b_cod<=6402) & pp04b_cod!=6303 &  pp04b_cod!=6402) & emp_ci==1
	replace rama_ci = 8 if ((pp04b_cod>=65 & pp04b_cod<=70) |(pp04b_cod>=6500 &  pp04b_cod<=7499)) & emp_ci==1
	replace rama_ci = 9 if ((pp04b_cod>=71 & pp04b_cod<=95) |(pp04b_cod>=7500 &  pp04b_cod<=9900) | pp04b_cod==5311 | pp04b_cod==6303 | pp04b_cod==6402 | pp04b_cod==5311) & emp_ci==1

	/*
capture drop rama_ci
gen rama_ci = .
replace rama_ci = 1 if (pp04b_cod==1)|(pp04b_cod>=101 &  pp04b_cod<=500)
replace rama_ci = 2 if (pp04b_cod>=10 & pp04b_cod<=14) |(pp04b_cod>=1000 &  pp04b_cod<=1400)
replace rama_ci = 3 if (pp04b_cod>=15 & pp04b_cod<=37) |(pp04b_cod>=1501 &  pp04b_cod<=3700)
replace rama_ci = 4 if (pp04b_cod>=40 & pp04b_cod<=41) |(pp04b_cod>=4001 &  pp04b_cod<=4100)
replace rama_ci = 5 if (pp04b_cod==45) |(pp04b_cod==4500)
replace rama_ci = 6 if (pp04b_cod>=50 & pp04b_cod<=55) |(pp04b_cod>=5001 &  pp04b_cod<=5503)
replace rama_ci = 7 if (pp04b_cod>=60 & pp04b_cod<=64) |(pp04b_cod>=6001 &  pp04b_cod<=6402)
replace rama_ci = 8 if (pp04b_cod>=65 & pp04b_cod<=74) |(pp04b_cod>=6500 &  pp04b_cod<=7409)
replace rama_ci = 9 if (pp04b_cod>=75 & pp04b_cod<=95) |(pp04b_cod>=7501 &  pp04b_cod<=9900)
*/

	/*capture drop rama_ci
	gen rama_ci = .
	replace rama_ci = 1 if (pp04b_cod==1)|(pp04b_cod>=101 &  pp04b_cod<=500)
	replace rama_ci = 2 if (pp04b_cod>=10 & pp04b_cod<=14) |(pp04b_cod>=1000 &  pp04b_cod<=1400)
	replace rama_ci = 3 if (pp04b_cod>=15 & pp04b_cod<=37) |(pp04b_cod>=1501 &  pp04b_cod<=3700)
	replace rama_ci = 4 if (pp04b_cod>=40 & pp04b_cod<=41) |(pp04b_cod>=4001 &  pp04b_cod<=4100)
	replace rama_ci = 5 if (pp04b_cod==45) |(pp04b_cod==4500)
	replace rama_ci = 6 if (pp04b_cod>=50 & pp04b_cod<=55) |(pp04b_cod>=5001 &  pp04b_cod<=5503)
	replace rama_ci = 7 if (pp04b_cod>=60 & pp04b_cod<=64) |(pp04b_cod>=6001 &  pp04b_cod<=6402)
	replace rama_ci = 8 if (pp04b_cod>=65 & pp04b_cod<=67) |(pp04b_cod>=6500 &  pp04b_cod<=6702)
	replace rama_ci = 9 if (pp04b_cod>=70 & pp04b_cod<=95) |(pp04b_cod>=7000 &  pp04b_cod<=9900)
	label var rama_ci "Rama de actividad"
	label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
	label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
	label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
	label val rama_ci rama_ci*/



	************
	*durades_ci*
	************
	*Esta variable se capturo como categorica por ello se crea como missing. Sin embargo, se 
	*guarda la informacion en una nueva variable durades1_ci
	
	gen durades_ci=.
	label variable durades_ci "Duracion del desempleo en meses"

	gen durades1_ci=pp10a
	replace durades1_ci=. if pp10a==0 | pp10a==9
	label variable durades1_ci "Duracion del desempleo - categorica"
	label define durades1_ci 1 "menos de 1 mes"
	label define durades1_ci 2 "de 1 a 3 meses", add
	label define durades1_ci 3 "más de 3 a 6 meses", add
	label define durades1_ci 4 "más de 6 a 12 meses", add
	label define durades1_ci 5 "más de 1 año", add
	label values durades1_ci durades1_ci
 
	
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
	*CREO QUE NO ES POSIBLE CONSTRUIR LA VARIABLE ANTIGUEDAD, PERO A CONTINUACIÓN LA MEJOR MANERA DE APROXIMARLA
	*Para empleados USO EL VALOR MEDIO DE CADA RANGO DE ANIOS, LA ESTOY CREANDO TRUNCADA,
	* HAY QUE REVISAR ESTO!!!!
	* Yanira: no estoy de acuerdo con esto.  Posiblemente sea mejor convertir a discretas las 
	* antiguedades de independientes y empleados domesticos. No obstante lo dejo asi hasta consultar
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
	gen antiguedad_ci=	antiguedad1 if antiguedad1!=. & emp_ci==1
	replace antiguedad_ci= antiguedad2 if antiguedad2!=. & emp_ci==1
	replace antiguedad_ci= antiguedad3 if antiguedad3!=. & emp_ci==1
	replace antiguedad_ci= antiguedad4 if antiguedad4!=. & emp_ci==1
	label var antiguedad_ci "antiguedad laboral (anios) - aproximacion"	
*Note: A los empleados e independientes se les esta dejando un máximo de 5 años de antiguedad.
	

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
	label var ylmpri_ci "Ingreso laboral monetario actividad principal" 
			
	*************
	*nrylmpri_ci*
	*************

	gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
	label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


	************
	*ylnmpri_ci*
	************
	*En esta encuesta se pregunta si recibe pago no monetario, pero no se pregunta cual es el vr de esto
	gen ylnmpri_ci=.
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


	***************
	***ylmsec_ci***
	***************

	gen ylmsec_ci=.
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


	****************
	***ylnmsec_ci***
	****************

	gen ylnmsec_ci=.
	label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


	*************
	*ylmotros_ci*
	*************

	gen ylmotros_ci=tot_p12 if tot_p12>=0
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos"
	
	******************
	***ylnmotros_ci***
	******************

	gen ylnmotros_ci=.
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


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
	label var ylm_ci "Ingreso laboral monetario total"  


	*********
	*ylnm_ci*
	*********
	
	gen ylnm_ci=.
	label var ylnm_ci "Ingreso laboral NO monetario total"  	

	*********
	*ynlm_ci*
	*********
	
	//yl: para los que existe la variable t_vi	

	gen ynlm_ci=.
	replace ynlm_ci = t_vi
	replace ynlm_ci=. if ynlm_ci<0
	label var ynlm_ci "Ingreso no laboral monetario"  

	
	
	**********
	*ynlnm_ci*
	**********
	
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

/*

	*Para ignorar el 0.5% más alto y más bajo de los ingreso se hace lo siguiente:
	foreach var in ylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylnm_ci ynlm_ci {
	sort `var'
	cap pctile pct = `var' if `var'>=0 & `var'!=. & emp_ci==1, nq(200) genp(percent)
	cap scalar filtro1=pct in 199 
	cap scalar filtro2=pct in 1
	cap replace `var'=. if  `var'>filtro1 | `var'<filtro2
	cap scalar drop filtro1 filtro2
	cap drop pct percent
	}
	
*/

	
	
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
	label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


	********
	*ylm_ch*
	********

	by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
	label var ylm_ch "Ingreso laboral monetario del hogar"


	*********
	*ylnm_ch*
	*********
	
	gen ylnm_ch=.
	label var ylnm_ch "Ingreso laboral no monetario del hogar"


	**********
	*ylmnr_ch*
	**********
	
	by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
	replace ylmnr_ch=. if nrylmpri_ch==1
	label var ylmnr_ch "Ingreso laboral monetario del hogar"


	*********
	*ynlm_ch*
	*********
	
	by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
	label var ynlm_ch "Ingreso no laboral monetario del hogar"


	**********
	*ynlnm_ch*
	**********
	
	gen ynlnm_ch=.
	label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


	********
	***NA***
	********
	gen rentaimp_ch=.
	label var rentaimp_ch "Rentas imputadas del hogar"

	gen autocons_ci=.
	label var autocons_ci "Autoconsumo reportado por el individuo"
	
	gen autocons_ch=.
	label var autocons_ch "Autoconsumo reportado por el hogar"


	************
	*remesas_ci*
	************
	
	gen remesas_ci=.
	label var remesas_ci "Remesas mensuales reportadas por el individuo" 

	
	************
	*remesas_ch*
	************
	
	gen remesas_ch=.
	label var remesas_ch "Remesas mensuales del hogar"	


	*************
	*ylmhopri_ci*
	*************
	
	gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri_ci)
	replace ylmhopri_ci=. if ylmhopri_ci<=0
	label var ylmhopri_ci "Salario monetario de la actividad principal" 


	**********
	*ylmho_ci*
	**********

	gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
	label var ylmho_ci "Salario monetario de todas las actividades" 



			****************************
			***VARIABLES DE EDUCACION***
			****************************
* Mod. 3/2022 Pia Iocco y Agustina Thailinger EDU/SLC
* Mod. 8/2015 Ivan Bonacelli EDU/SLC
   *Ajustando variables
	replace ch10=. if ch10==9
	replace ch12=. if ch12==99 | ch12==9 // Missing a educacion especial
	replace ch13=. if ch13==9
	destring ch14, replace
	replace ch14=. if ch14==98 | ch14==99
	
	label define Ch12 1 "Jardín/preescolar" 2 "Primario" 3 "EGB" 4 "Secundario" 5 "Polimodal" 6 "Terciario" 7 "Universitario" 8 "Posgrado universitario" 9 "Educación 	especial (discapacitado)" 
	label values ch12 Ch12
	
	*Variable de Anios de Educacion
	gen aedu_ci=.
	
	*Para quienes no terminaron el ultimo nivel educativo al que asistieron
	replace aedu_ci=0 if (ch10==0 | ch10==3) // Cero anios de educación para aquellos que no han asistido nunca a ninguna institucion y los menores de 2 anios
	replace aedu_ci=0 if ch12==1 // Prescolar
	replace aedu_ci=ch14 if ch12==2 | ch12==3 & ch13==2
	replace aedu_ci=ch14+6 if ch12==4 & ch13==2
	replace aedu_ci=ch14+9 if ch12==5 & ch13==2
	replace aedu_ci=ch14+12 if ch12==6 & ch13==2
	replace aedu_ci=ch14+12 if ch12==7 & ch13==2
	replace aedu_ci=ch14+17 if ch12==8 & ch13==2
		
	*Para quienes terminaron el ultimo nivel educativo al que asistieron
	replace aedu_ci=6 if ch12==2  & ch13==1
	replace aedu_ci=9 if ch12==3  & ch13==1
	replace aedu_ci=12 if ch12==4 & ch13==1
	replace aedu_ci=12 if ch12==5 & ch13==1
	replace aedu_ci=15 if ch12==6 & ch13==1
	replace aedu_ci=17 if ch12==7 & ch13==1
	replace aedu_ci=19 if ch12==8 & ch13==1
	
	*Imputando para los que tenemos certeza del nivel educativo mas alto alcanzado
	replace aedu_ci=6 if nivel_ed==2 & aedu_ci==.
	replace aedu_ci=12 if nivel_ed==4 & aedu_ci==.
	replace aedu_ci=17 if nivel_ed==6 & aedu_ci==.
	replace aedu_ci=0 if nivel_ed==7 & aedu_ci==.

	label var aedu_ci "Anios de educacion aprobados"
	label define nivel 1 "Primario incompleto (incluye educación especial)" 2 "primario completo" 3 "Secundario incompleto" 4 "secundario completo" 5 "Superior universitario incompleto" 6 "Superior universitario completo" 7 "Sin instrucción" 9 "Ns/ Nr"  
	label values nivel_ed nivel

	//imputando valores. Esta es una opcion a la anterior

/*	replace aedu_ci=0 if ch12==2 & aedu_ci==. & ch13==1
	replace aedu_ci=0 if ch12==3 & aedu_ci==. & ch13==1
	replace aedu_ci=6 if ch12==4 & aedu_ci==. & ch13==1
	replace aedu_ci=9 if ch12==5 & aedu_ci==. & ch13==1
	replace aedu_ci=12 if ch12==6 & aedu_ci==. & ch13==1
	replace aedu_ci=12 if ch12==7 & aedu_ci==. & ch13==1
	replace aedu_ci=17 if ch12==8 & aedu_ci==. & ch13==1 */

	**********
	*eduno_ci* 
	**********
	
	gen eduno_ci=(aedu_ci==0)
	replace eduno_ci=. if aedu_ci==.
	
	**********
	*edupi_ci*
	**********
	
	gen edupi_ci=(aedu>=1 & aedu<=5)
	replace edupi_ci=. if aedu_ci==.

	**********
	*edupc_ci* 
	**********
	
	gen edupc_ci=(aedu==6)
	replace edupc_ci=. if aedu_ci==.

	**********
	*edusi_ci*
	**********
	
	gen edusi_ci=(aedu>=7 & aedu<=11)
	replace edusi_ci=. if aedu_ci==.

	**********
	*edusc_ci*
	**********
	
	gen edusc_ci=(aedu==12)
	replace edusc_ci=. if aedu_ci==.

	**********
	*eduui_ci*
	**********
	
	gen eduui_ci=(aedu_ci>12 & nivel_ed==5) // nivel superior incompleto
	replace eduui_ci=1 if aedu_ci>12 & aedu_ci<=16 & nivel_ed!=5 & nivel_ed!=6
	replace eduui_ci=. if aedu_ci==.

	**********
	*eduuc_ci*
	**********
	
	gen eduuc_ci=(aedu_ci>12 & nivel_ed==6)
	replace eduuc_ci=1 if aedu_ci>=17 & nivel_ed!=5 & nivel_ed!=6
	replace eduuc_ci=. if aedu_ci==.

	***********
	*edus1i_ci*
	***********

	gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
	replace edus1i_ci=. if aedu_ci==. 
	label variable edus1i_ci "1er ciclo de la secundaria incompleto"

	***********
	*edus1c_ci*
	***********

	gen byte edus1c_ci=(aedu_ci==9)
	replace edus1c_ci=. if aedu_ci==.
	label variable edus1c_ci "1er ciclo de la secundaria completo"

	***********
	*edus2i_ci*
	***********

	gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
	replace edus2i_ci=. if aedu_ci==.
	label variable edus2i_ci "2do ciclo de la secundaria incompleto"

	***********
	*edus2c_ci*
	***********

	gen byte edus2c_ci=(aedu==12)
	replace edus2c_ci=.  if aedu_ci==.
	label variable edus2c_ci "2do ciclo de la secundaria completo"
	
	***********
	*edupre_ci*
	***********
	gen byte edupre_ci=.

	************
	*asispre_ci*
	************
	*Nueva variable incoporada 01/11/2017 por Iván Bornacelly
	g asispre_ci=(ch10==1 & ch12==1)
	la var asispre_ci "Asiste a educacion prescolar"
	
	**********
	*eduac_ci*
	**********

	gen byte eduac_ci=.
	replace eduac_ci=1 if ch12==7 | ch12==8 
	replace eduac_ci=0 if ch12==6
	label variable eduac_ci "Superior universitario vs superior no universitario"

	***********
	*asiste_ci*
	***********
	
	gen asiste_ci=(ch10==1)
	replace asiste_ci=. if ch10==0 | ch10==9
	label variable asiste_ci "Asiste actualmente a la escuela"

	*************
	*pqnoasis_ci*
	*************

	gen pqnoasis_ci=.
	label var pqnoasis_ci "Razones para no asistir a la escuela"
	
	**************
	*pqnoasis1_ci*
	**************

	**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci**
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
	
	gen edupub_ci=.
	replace edupub_ci = 1 if ch11==1 & asiste_ci==1
	replace edupub_ci = 0 if ch11==2 & asiste_ci==1


		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************
****************
***aguared_ch***
****************
	gen aguared_ch=(iv7==1)
	replace aguared_ch=. if iv7==9


*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0


*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.
replace aguafuente_ch = 1 if iv7==1 & iv6<3
replace aguafuente_ch = 2 if iv7==1 & iv6==3
replace aguafuente_ch = 10 if iv7>1


*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch= 1 if iv6==1
replace aguadist_ch= 2 if iv6==2
replace aguadist_ch= 3 if iv6==3


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch = 9


**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9


*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10


*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7

*****************
***aguamide_ch***
*****************
generate aguamide_ch = .


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=0
replace bano_ch=6 if iv8==1
replace bano_ch=1 if iv10<3 & iv10>=1  & iv11==1
replace bano_ch=2 if iv10<3 & iv10>=1   & iv11==2
replace bano_ch=3 if iv10<3 & iv10>=1   & iv11==3
replace bano_ch=6 if iv10==3
replace bano_ch=4 if iv11==4

***************
***banoex_ch***
***************
	gen banoex_ch=0
	replace banoex_ch=1 if ii9==1
	replace banoex_ch=. if ii9==0 | ii9==9


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch =  0 if iv8==1
replace sinbano_ch = 1 if iv9==3
replace sinbano_ch = 2 if iv8==2

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9

		
	********
	*luz_ch*
	********
	*En la nueva encuesta no se encontro si se pregunta por instalacion electrica
	gen luz_ch=.
	label var luz_ch  "La principal fuente de iluminación es electricidad"


	************
	*luzmide_ch*
	************

	gen luzmide_ch=.
	label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


	************
	*combust_ch*
	************

	gen combust_ch=0
	replace combust_ch=1 if ii8==1 | ii8==2 
	replace combust_ch=. if ii8==0 | ii8==9
	label var combust_ch "Principal combustible gas o electricidad" 




	*********
	*des1_ch*
	*********

	gen des1_ch=.
	replace des1_ch=0 if bano_ch==0
	replace des1_ch=1 if iv11==1 | iv11==2
	replace des1_ch=2 if iv11==3 | iv11==4
	* Junio 2015 MGR: iv11=0 corresponde a no tiene baño por lo que no debe reemplazarse por missing ya que escribe sobre la opción 0 de des1_ch
	*replace des1_ch=. if iv11==0 | iv11==9
	label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
	label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
	label val des1_ch des1_ch
	

	*********
	*des2_ch*
	*********

	gen des2_ch=.
	replace des2_ch=0 if bano_ch==0
	replace des2_ch=1 if iv11==1 | iv11==2 | iv11==3 | iv11==4
	* Junio 2015 MGR: iv11=0 corresponde a no tiene baño por lo que no debe reemplazarse por missing ya que escribe sobre la opción 0 de des2_ch
	*replace des2_ch=. if iv11==0 | iv11==9
	replace des2_ch=. if iv11==9
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
	


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
	label var resid_ch "Método de eliminación de residuos"

 
	*********
	*dorm_ch*
	*********

	gen dorm_ch=ii2 if ii3==2
	replace dorm_ch=. if ii2==99
	label var dorm_ch "Habitaciones para dormir"
    
	
	************
	*cuartos_ch*
	************

	gen cuartos_ch=ii1
	replace cuartos_ch=. if ii1==99
	label var cuartos_ch "Habitaciones en el hogar"
 

	***********
	*cocina_ch*
	***********

	gen cocina_ch=0 
	replace cocina_ch=1 if ii4_1==1
	replace cocina_ch=. if ii4_1==0 | ii4_1==9
	label var cocina_ch "Cuarto separado y exclusivo para cocinar"
	

	**********
	*telef_ch*
	**********

	gen telef_ch=.
	label var telef_ch "El hogar tiene servicio telefónico fijo"


	***********
	*refrig_ch*
	***********

	gen refrig_ch=.
	label var refrig_ch "El hogar posee refrigerador o heladera"


	**********
	*freez_ch*
	**********

	gen freez_ch=.
	label var freez_ch "El hogar posee congelador"


	*********
	*auto_ch*
	*********

	gen auto_ch=.
	label var auto_ch "El hogar posee automovil particular"


	**********
	*compu_ch*
	**********

	gen compu_ch=.
	label var compu_ch "El hogar posee computador"


	*************
	*internet_ch*
	*************

	gen internet_ch=.
	label var internet_ch "El hogar posee conexión a Internet"


	********
	*cel_ch*
	********

	gen cel_ch=.
	label var cel_ch "El hogar tiene servicio telefonico celular"


	**********
	*vivi1_ch*
	**********

	gen vivi1_ch=1 if iv1==1
	replace vivi1_ch=2 if iv1==2
	replace vivi1_ch=3 if iv1==3 | iv1==4 | iv1==5 | iv1==6
	label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
	label val vivi1_ch vivi1_ch



	**********
	*vivi2_ch*
	**********

	gen vivi2_ch=0
	replace vivi2_ch=1 if iv1==1 | iv1==2
	label var vivi2_ch "La vivienda es casa o departamento"


	*************
	*viviprop_ch*
	*************
	*NOTA: aqui se genera una variable parecida, pues no se puede saber si es propia total o parcialmente pagada
	
	gen viviprop_ch1=0 if ii7==3
	replace viviprop_ch1=1 if ii7==1 | ii7==2
	replace viviprop_ch1=3 if ii7>=4 & ii7<=9 
	replace viviprop_ch1=. if ii7==99
	label var viviprop_ch1 "Propiedad de la vivienda"
	label def viviprop_ch1 0"Alquilada" 1"Propia" 
	label def viviprop_ch1 3"Ocupada (propia de facto)", add
	label val viviprop_ch1 viviprop_ch1


	************
	*vivitit_ch*
	************

	gen vivitit_ch=.
	label var vivitit_ch "El hogar posee un título de propiedad"

	************
	*vivialq_ch*
	************

	gen vivialq_ch=.
	label var vivialq_ch "Alquiler mensual"

	***************
	*vivialqimp_ch*
	***************
	gen vivialqimp_ch=.
	label var vivialqimp_ch "Alquiler mensual imputado"
	
	
	gen byte muestra_92=(aglomerado==32 | aglomera==33 | aglomera==6 | aglomera==9 | aglomera==19 | aglomera==23 | aglomera==26 | aglomera==30 | aglomera==26 | aglomera==30 | aglomera==13 | aglomera==10 | aglomera==4| aglomera==29)
	


/************************************************************************************************************
* Líneas de pobreza oficiales
************************************************************************************************************/

*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci = 393.05

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = 181.26

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
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
**salmm_ci***
*************

*1 = ARG 2011
* MGD 2015: promedio del semestre
gen salmm_ci= 	1700
label var salmm_ci "Salario minimo legal"

******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if cat_inac==1
replace categoinac_ci=2 if cat_inac==3
replace categoinac_ci=4 if cat_inac==4
recode categoinac_ci .= 4 if condocup_ci==3

label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
	
***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

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
	
	gen migantiguo5_ci=(migrante_ci==1 & inlist(ch16,1,2,3)) if !inlist(ch16,6,9) & migrante_ci!=. 		/* Categorias Ns./Nr. y no habia nacido no se incluyen en la variable*/
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
	gen miglac_ci = 1 if (inlist(ch15_cod,"201","202","203","205","206","207","208","209","210") | ///
	inlist(ch15_cod,"211","213","214","215","216","217","218","219","220") | ///
	inlist(ch15_cod,"221","222","224","225","226","232","233","236","237") | ///
	inlist(ch15_cod,"239","240")) & migrante_ci == 1
	replace miglac_ci = 0 if miglac_ci != 1 & migrante_ci == 1
	replace miglac_ci =. if migrante_ci == 0
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC" 


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	pqnoasis1_ci repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename pp04d_cod codocupa
rename pp04b_cod codindustria

destring codocupa codindustria, replace

compress

saveold "`base_out'", version(12) replace

log close





