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

local PAIS DOM
local ENCUESTA ENFT
local ANO "2001"
local ronda m10 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Republica Dominicana
Encuesta: ENFT
Round: m10
Autores: 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Agosto de 2013
Armonización Septiembre 2013: Mayra Sáenz
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
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c




***************
***factor_ch***
***************

gen factor_ch= factor_exp
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

sort vivienda hogar
egen idh_ch = group(vivienda hogar)
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=miembro
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if zona==1
replace zona_c=1 if zona==0
replace zona_c=0 if zona_reside==1
replace zona_c=1 if zona_reside==0

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


***************
***upm_ci***
***************

clonevar upm_ci=upm
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

clonevar estrato_ci=estrato
label variable estrato_ci "Estrato"


************
***region***
************

gen region_c=provincia
label define region_c 1 "Distrito Nacional" ///
2 "Azua" ///
3 "Bahoruco" ///
4 "Barahona" ///
5 "Dajabon" ///
6 "Duarte" ///
7 "Elias Piña" ///
8 "El Seibo" ///
9 "Espaillat" ///
10 "Independencia" ///
11 "La Altagracia" ///
12 "La Romana" ///
13 "La Vega" ///
14 "Maria Trinidad Sanchez" ///
15 "Monte Cristi" ///
16 "Pedernales" ///
17 "Peravia" ///
18 "Puerto Plata" ///
19 "Salcedo" ///
20 "Samana" ///
21 "San Cristobal" ///
22 "San Juan" ///
23 "San Pedro De Macoris" ///
24 "Sanchez Ramirez" ///
25 "Santiago" ///
26 "Santiago Rodriguez" ///
27 "Valverde" ///
28 "Monseñor Nouel" ///
29 "Monte Plata" ///
30 "Hato Mayor" ///
31 "San Jose De Ocoa" ///
32 "Santo Domingo" 
label value region_c region_c
label var region_c "Region - provincias"  


************
****pais****
************

gen str3 pais_c="DOM"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2001
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=10 
label variable mes_c "Mes de la encuesta"


*****************
***relacion_ci***
*****************

gen relacion_ci=1 if parentesco_con_jefe==1
replace relacion_ci=2 if parentesco_con_jefe==2
replace relacion_ci=3 if parentesco_con_jefe==3 | parentesco_con_jefe==4 
replace relacion_ci=4 if parentesco_con_jefe>=5 & parentesco_con_jefe<=11
replace relacion_ci=5 if parentesco_con_jefe==12

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label value relacion_ci relacion_ci


	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=sexo
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estado_civil==6
replace civil_ci=2 if estado_civil==1 | estado_civil==2 
replace civil_ci=3 if estado_civil==3 | estado_civil==4
replace civil_ci=4 if estado_civil==5

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


**************
***jefe_ci***
*************

gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"


******************
***nconyuges_ch***
******************

by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************

by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************

by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************

by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"


****************
***nempdom_ch***
****************
*NOTA: No dentro de las relaciones de parentezco no es posible identificar a los empleados domésticos
gen nempdom_ch=.
label variable nempdom_ch "Numero de empleados domesticos"


*****************
***clasehog_ch***
*****************

gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

	
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
	*** afroind_ci ***
	***************
gen afroind_ci=. 

	***************
	*** afroind_ch ***
	***************
gen afroind_ch=. 

	*******************
	*** afroind_ano_c ***
	*******************
gen afroind_ano_c=.		

	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 

	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 



************************************
	*** VARIABLES DEL MERCADO LABORAL***
	************************************
****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if ocupado==1 & edad_ci>=10
replace condocup_ci=2 if desocupado==1 & edad_ci>=10
replace condocup_ci=3 if inactivo==1  & edad_ci>=10
replace condocup_ci=4 if edad<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Variable alternativa con variables originales trabajo+aunque no trabajo tiene trabajo+busco trabajo. MGD 05/22/2014

gen condocup_ci=.
replace condocup_ci=1 if trabajo_sem_ant==1 | tuvo_act_econ_sem_ant==1 | cultivo_sem_ant==1 | elab_prod_sem_ant==1 | ayudo_fam_sem_ant==1 | cosio_lavo_sem_ant==1
replace condocup_ci=2 if (trabajo_sem_ant==2 | tuvo_act_econ_sem_ant==2 | cultivo_sem_ant==2 | elab_prod_sem_ant==2 | ayudo_fam_sem_ant==2 | cosio_lavo_sem_ant==2) & (busco_trab_mes_ant==1 | busco_trab_sem_ant==1)
recode condocup_ci (.=3) if edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

************
***emp_ci***
************

gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1

*****************
***desalent_ci***
*****************

gen desalent_ci=(motivo_no_busca_trab==10 | motivo_no_busca_trab==11)
replace desalent_ci=. if motivo_no_busca_trab==.
label var desalent_ci "Trabajadores desalentados"
*Utilizo para desalentado la opcion 10 y no la 1 como se ha hecho antes

 ***************
 ***subemp_ci***
 ***************    
 
*Horas semanales trabajadas empleo principal
gen promhora=horas_sem_ocup_princ if emp_ci==1 

*Horas semanales trabajadas empleo secundario
gen promhora1=horas_sem_ocup_secun if emp_ci==1
egen tothoras=rowtotal(promhora promhora1)
replace tothoras=. if promhora==. & promhora1==. 
replace tothoras=. if tothoras>=168

*Modificacion MGD 06/20/2014: condiciona solo a horas en ocupacion primaria.
gen subemp_ci=0
replace subemp_ci=1 if (promhora>=1 & promhora<=30) & emp_ci==1 & desea_trab_mas_horas==1
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************

gen horaspri_ci=horas_sem_ocup_princ
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras  if emp_ci==1 
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30) &  emp_ci==1 & desea_trab_mas_horas==2
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categoria_ocup_princ==6
replace categopri_ci=2 if categoria_ocup_princ==4 | categoria_ocup_princ==5
replace categopri_ci=3 if categoria_ocup_princ==1 | categoria_ocup_princ==2 | categoria_ocup_princ==3  | categoria_ocup_princ==9 | categoria_ocup_princ==10
replace categopri_ci=4 if categoria_ocup_princ==7 | categoria_ocup_princ==8
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"
*NOTA: aqui se quedan sin clasificar los jornaleros y "otros"*

/*
CATEGORIA_OCUP_PRINC	1	EMPLEADO GOBIERNO GENERAL
CATEGORIA_OCUP_PRINC	2	EMPLEADO DE EMPRESA PUBLICA
CATEGORIA_OCUP_PRINC	3	EMPLEADO EMPRESA PRIVADA
CATEGORIA_OCUP_PRINC	4	POR CUENTA PROPIA PROFESIONAL
CATEGORIA_OCUP_PRINC	5	POR CUENTA PROPIA NO PROFESIONAL
CATEGORIA_OCUP_PRINC	6	PATRON
CATEGORIA_OCUP_PRINC	7	AYUDANTE FAM. O NO FAM. NO REMUNERADO
CATEGORIA_OCUP_PRINC	8	AYUDANTE  NO FAM. NO REMUNERADO
CATEGORIA_OCUP_PRINC	9	TRABAJADOR EN PRODUCCIÓN DOMESTICA
CATEGORIA_OCUP_PRINC	10  SERVICIO DOMESTICO
*/

******************
***categosec_ci***
******************

gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & tiene_ocup_secun==2
replace nempleos_ci=2 if emp_ci==1 & tiene_ocup_secun==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if cant_pers_trab==1 | cant_pers_trab==2 
replace firmapeq_ci=0 if cant_pers_trab>=3 & cant_pers_trab!=.
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=(categoria_ocup_princ==1 |categoria_ocup_princ==2) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************
*NOTA: se contruye la variable ocupa_ci de acuerdo a CIUO88
//yessenia L

tostring ocupacion_princ, replace
gen digito = "0"
egen x = concat(digito ocupacion_princ) if length(ocupacion_princ)==2
replace ocupacion_princ=x if length(ocupacion_princ)==2
gen ocupa=real(substr(ocupacion_princ,1,2))
 
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=34)  & emp_ci==1
replace ocupa_ci=2 if (ocupa>=11 & ocupa<=13)  & emp_ci==1
replace ocupa_ci=3 if (ocupa>=41 & ocupa<=42)  & emp_ci==1
replace ocupa_ci=4 if (ocupa==52)              & emp_ci==1
replace ocupa_ci=5 if (ocupa==51)              & emp_ci==1
replace ocupa_ci=6 if (ocupa==61)              & emp_ci==1
replace ocupa_ci=7 if (ocupa>=71 & ocupa<=83)  & emp_ci==1
replace ocupa_ci=8 if (ocupa==1)               & emp_ci==1
replace ocupa_ci=9 if (ocupa>=91 & ocupa<=93)  & emp_ci==1
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************
*Nota: se contruye la variable rama_ci siguiendo a CIUU revision3 

destring rama_princ, replace
rename rama_princ ramac

gen rama_ci=.
replace rama_ci = 1 if (ramac>=11 & ramac<=50)  & emp_ci==1
replace rama_ci = 2 if (ramac>=101 & ramac<=142)  & emp_ci==1
replace rama_ci = 3 if (ramac>=151 & ramac<=372)  & emp_ci==1
replace rama_ci = 4 if (ramac>=401 & ramac<=410)  & emp_ci==1
replace rama_ci = 5 if (ramac>=451 & ramac<=455)  & emp_ci==1
replace rama_ci = 6 if (ramac>=501 & ramac<=552)  & emp_ci==1
replace rama_ci = 7 if (ramac>=601 & ramac<=642)  & emp_ci==1
replace rama_ci = 8 if (ramac>=651 & ramac<=702)  & emp_ci==1
replace rama_ci = 9 if (ramac>=711 & ramac<=999)  & emp_ci==1

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
/*
tiempo_busca_trab 
1 Menos de 1 mes
2 1 mes a menos de 2 meses
3 2 meses a menos de 3 meses.
4 3 meses a menos de 6 meses.
5 6 meses a menos de 1 año.
6 más de un año.
*Mayra Sáenz-Febrero 2014
*Se cambia la variable utilizada y se estima el número de meses como el promedio, al igual que en el caso de Jamaica.

*/
gen durades_ci=.
replace durades_ci=1  if tiempo_busca_trab==1
replace durades_ci=(1+2)/2 if tiempo_busca_trab==2
replace durades_ci=(2+3)/2 if tiempo_busca_trab==3
replace durades_ci=(3+6)/2 if tiempo_busca_trab==4
replace durades_ci=(6+12)/2 if tiempo_busca_trab==5
replace durades_ci=(12+12)/2 if tiempo_busca_trab==6

label variable durades_ci "Duracion del desempleo en meses"
label def durades_ci 1"Menos de un mes" 2"1 mes a menos de 2 meses" 3"2 meses a menos de 3 meses"
label def durades_ci 4"3 meses a menos de 6 meses" 5"6 meses a menos de 1 año" 6"Más de 1 año", add
label val durades_ci durades1_ci 

*******************
***antiguedad_ci***
*******************
destring tiempo_lab_dias, replace
gen temp1=tiempo_lab_dias/365

destring tiempo_lab_mes, replace
gen temp2=tiempo_lab_mes/12

destring tiempo_lab_anos, replace
egen antiguedad_ci= rsum(tiempo_lab_anos temp1 temp2), missing 
replace antiguedad_ci=. if emp_ci==0
replace antiguedad_ci=. if tiempo_lab_dias==. & tiempo_lab_mes==. & tiempo_lab_anos==.
label var antiguedad_ci "Antiguedad en la actividad actual en anios"
drop temp*

/* 
Nota MGD 07/03/2014: en este año la encuesta tiene valores missings en las variables de tiempo de trabajo, 
por lo que no se puede generar la variable.
*/
	
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*********
*lp_ci***
*********
*valor septiembre
gen lp_ci =1261
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
*valor septiembre
gen lpe_ci =615
label var lpe_ci "Linea de indigencia oficial del pais"


****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	

label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


**********
*busca_ci* 
**********
gen busca_ci=0 if condocup==2
replace busca_ci=1 if busco_trab_sem_ant==1 
label var busca_ci "Busco trabajo la semana anterior"
label define busca_ci 0 "No" 1 "Si"
label value busca_ci busca_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if trabajo_antes==1
replace cesante_ci=0 if trabajo_antes==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci**
*************
/*1 de 1 a 4
2 de 5 a 10
3 sw 11 a 19
4 de 20 y mas personas*/
gen tamemp_ci=1 if cant_pers_trab>0 & cant_pers_trab<=4
replace tamemp_ci=2 if cant_pers_trab>=2 & cant_pers_trab<=3
replace tamemp_ci=3 if cant_pers_trab>=4 & cant_pers_trab!=.
/*
gen tamemp_ci=1 if cant_pers_trab>0 & cant_pers_trab<=5
replace tamemp_ci=2 if cant_pers_trab>5 & cant_pers_trab<=50
replace tamemp_ci=3 if cant_pers_trab>50 & cant_pers_trab!=.*/
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

gen pension_ci=1 if monto_pension_ing_nac!=0 & monto_pension_ing_nac!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
*Modificado Mayra Sáenz -Febrero 2014
/*
gen ypen_ci=monto_pension_ing_nac
recode ypen_ci .=0
label var ypen_ci "Valor de la pension contributiva"
*/
*gen ypen_ci=monto_pension_ing_nac if recibio_ing_pension_mes ==1
* 2014, 02 vuelvo a hacer modificacion sobre cambio de Mayra. MLO
gen ypen_ci=monto_pension_ing_nac if pension_ing_nac ==1

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

**********
**tc_ci***
**********
gen tc_ci=16.66

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* DOM 2001
gen salmm_ci= 	2075.00

label var salmm_ci "Salario minimo legal"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((motivo_no_busca_trab==9) & condocup_ci==3)
replace categoinac_ci = 2 if  (motivo_no_busca_trab==6 & condocup_ci==3)
replace categoinac_ci = 3 if  (motivo_no_busca_trab==7 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
/*
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008
*/

*No se puede generar ya que no existe la variable cotizando.
gen  formal_ci=.
*replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
*replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

************************************************************************
**************************INGRESOS**************************************
************************************************************************


*1 hora, 2 día, 3 semana, 4 quincena, 5 mes
gen ymensual= 	   (ing_ocup_princ*horas_sem_ocup_princ)*4.3 if periodo_ing_ocup_princ==1
replace ymensual=  (ing_ocup_princ*dias_sem_ocup_princ)*4.3  if periodo_ing_ocup_princ==2
replace ymensual=  ing_ocup_princ*4.3   if periodo_ing_ocup_princ==3
replace ymensual=  ing_ocup_princ*2   if periodo_ing_ocup_princ==4
replace ymensual=  ing_ocup_princ     if periodo_ing_ocup_princ==5


gen pension=monto_pension_ing_nac  	    if pension_ing_nac==1
gen intereses= monto_interes_ing_nac 	if interes_ing_nac==1 
gen alquiler= monto_alquiler_ing_nac 	if alquiler_ing_nac==1
gen remesasnales=monto_remesas_ing_nac  if remesas_ing_nac==1  
gen otrosing=monto_otros_ing_nac 		if otros_ing_nac==1 
destring gobierno_ing_nac, replace
gen gobierno=monto_gobierno_ing_nac     if gobierno_ing_nac==1	

gen ymensual2=ing_ocup_secun



*REMESAS*
*Para República Dominicana hay dos módulos especiales: remesas e ingresos del exterior.
*Aquí se trabaja sobre esas variables:

*Módulo de ingresos del exterior
********************************

*No se cuenta con el tipo de cambio definido en la encuesta. Por lo tanto, se toma el valor de la
* tasa de cambio según el Banco Central de República Dominicana. 
*http://www.bancentral.gov.do/estadisticas.asp?a=Mercado_Cambiario
* El archivo en excel está disponible en el servidor surveys/ survey/ DOM/ 2001/ docs
* Tipo de cambio dólar 2001 (Promedio mensual de Octubre) = 16.66


*Dado que se necesita la información en moneda local se calcula el factor de conversión a pesos
*Si la información está en pesos se deja como está

*Modulo Ingresos del Exterior
gen pension_int=monto_ing_pension_mes	 		    if  moneda_ing_pension_mes==1
replace pension_int=monto_ing_pension_mes*16.66   	if  moneda_ing_pension_mes==2
replace pension_int=. if recibio_ing_pension_mes==2


gen interes_int=monto_ing_interes_mes	 		    if  moneda_ing_interes_mes==1
replace interes_int=monto_ing_interes_mes*16.66    if  moneda_ing_interes_mes==2
replace interes_int=. if recibio_ing_interes_mes==2


gen regalos_int= monto_equiv_regalo	if recibio_regalos ==1


*Variable semestral
gen otros_int=(monto_otros_ing_sem)/6			if moneda_otros_ing_sem==1
replace otros_int=(monto_otros_ing_sem*16.66)/6	if moneda_otros_ing_sem==2
replace otros_int=. if recibio_otros_ing_sem==2	

*Módulo de remesas
******************

*En este módulo se pregunta por el monto de remesas recibido cada mes del tercer trimestre.
* En Julio también se reportan ingresos del exterior en Bolivar y Franco
* los cuales corresponden al 0.01% y al 0.02%, respectivamente. 
* En el Banco Central de República Dominicana, en la serie histórica reportan estos tipod de moneda desde 2004. 
*Por lo tanto se generan como missings.

gen r_jul=monto_jul			    if moneda_jul==0
replace r_jul=monto_jul		    if moneda_jul==1
replace r_jul=monto_jul*16.66 	if moneda_jul==2


gen r_ago=monto_ago		    	if moneda_ago==0
replace r_ago=monto_ago		    if moneda_ago==1
replace r_ago=monto_ago*16.66  	if moneda_ago==2


gen r_sep=monto_sep 	    	if moneda_sep==0
replace r_sep=monto_sep 		if moneda_sep==1
replace r_sep=monto_sep*16.66  	if moneda_sep==2


egen remesas_trim=rsum(r_jul r_ago r_sep), missing
replace remesas_trim=. if recibio_remesa!=1 

gen remesas_prom=remesas_trim/3
replace remesas_prom=. if recibio_remesa!=1 



*Mayra Sáenz - Septiembre 2013. En las preguntas de remesas existen la pregunta acerca de la frecuencia
* con la que recibe la remesa en determinado mes, lo cual se debería tomar en cuenta para calcular el monto de
* remesas mensuales. Sin embargo, no existe el diccionario para identificar a qué frecuencia corresponde
* determinado código.


***************
***ylmpri_ci***
***************

egen ylmpri_ci=rsum(ymensual), missing
replace ylmpri_ci=. if ymensual==. 
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


****************
***ylnmpri_ci***
****************
*_____________________________________________________________________________________________________________*
* Mayra Sáenz - Septiembre 2013
* En este año existen las preguntas: pago_alimentos_op, pago_vivienda_op,pago_transporte_op,pago_otros_monto.
* Sin embargo, no se desagregan los montos. Son preguntas categóricas.
*_____________________________________________________________________________________________________________*

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

gen ylmsec_ci=ymensual2 if emp_ci==1 & tiene_ocup_secun == 1
replace ylmsec_ci=. if (ymensual2==99999) & emp_ci==1 
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************
destring pension intereses alquiler remesasnales otrosing gobierno pension_int interes_int otros_int remesas_prom, replace
egen ynlm_ci=rsum(pension intereses alquiler remesasnales otrosing gobierno pension_int interes_int otros_int remesas_prom), missing
replace ynlm_ci=. if pension==. & intereses==. & alquiler==. & remesasnales==. & otrosing==. & gobierno==. & pension_int==. & interes_int==. & otros_int==. & remesas_prom==.
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************

gen ynlnm_ci=regalos_int 
replace ynlnm_ci=. if regalos_int==.
label var ynlnm_ci "Ingreso no laboral no monetario" 
     
                                                                                                                    
************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************

*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"


**************
***ynlnm_ch***
**************
/*Modificación Mayra Sáenz-Febrero 2014
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"
*/
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


********
***NA***
********
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"



****************
***remesas_ci***
****************

*Aqui se toma el valor mensual de las remesas

gen remesas_ci=remesas_prom
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
***remesas_ch***
****************

*Aqui se toma el valor mensual de las remesas

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 



	****************************
	***VARIABLES DE EDUCACION***
	****************************


*************
***aedu_ci*** 
*************

gen	 aedu_ci=0  if (ult_nivel_alcanzado==1) | (ult_nivel_alcanzado==7)

*Primaria
replace aedu_ci=1  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==1)
replace aedu_ci=2  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==2)
replace aedu_ci=3  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==3)
replace aedu_ci=4  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==4)
replace aedu_ci=5  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==5)
replace aedu_ci=6  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==6)
replace aedu_ci=7  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==7)
replace aedu_ci=8  if (ult_nivel_alcanzado==2 & ult_ano_aprobado==8)

*Secundaria
replace aedu_ci=9  if (ult_nivel_alcanzado==3 & ult_ano_aprobado==1) 
replace aedu_ci=10 if (ult_nivel_alcanzado==3 & ult_ano_aprobado==2) 
replace aedu_ci=11 if (ult_nivel_alcanzado==3 & ult_ano_aprobado==3) 
replace aedu_ci=12 if (ult_nivel_alcanzado==3 & ult_ano_aprobado==4) 

*Técnico vocacional
replace aedu_ci=13 if (ult_nivel_alcanzado==4 & ult_ano_aprobado==1)
replace aedu_ci=14 if (ult_nivel_alcanzado==4 & ult_ano_aprobado==2)
replace aedu_ci=15 if (ult_nivel_alcanzado==4 & ult_ano_aprobado==3)
replace aedu_ci=16 if (ult_nivel_alcanzado==4 & ult_ano_aprobado==4)

*Universitario
replace aedu_ci=13 if (ult_nivel_alcanzado==5 & ult_ano_aprobado==1)
replace aedu_ci=14 if (ult_nivel_alcanzado==5 & ult_ano_aprobado==2)
replace aedu_ci=15 if (ult_nivel_alcanzado==5 & ult_ano_aprobado==3)
replace aedu_ci=16 if (ult_nivel_alcanzado==5 & ult_ano_aprobado==4)
replace aedu_ci=17 if (ult_nivel_alcanzado==5 & (ult_ano_aprobado==5 | ult_ano_aprobado==6))

*Posgrado
replace aedu_ci=18 if ult_nivel_alcanzado==6 & ult_ano_aprobado==1 
replace aedu_ci=19 if ult_nivel_alcanzado==6 & ult_ano_aprobado==2 
replace aedu_ci=.  if ult_nivel_alcanzado==.

label var aedu_ci "Anios de educacion aprobados" 


**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
label variable eduno_ci "Sin educacion"


**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<8) 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==8 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=9 & aedu_ci<12) 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12 & ult_nivel_alcanzado==3 
label variable edusc_ci "Secundaria completa"


* Mayra Sáenz - Septiembre 2013. Según el documento metodológico, para República Dominicana
* se reconoce como educación superior completa cuando alcanzan 16 o 17 años de educación formal.

/*
*************
**eduui_ci***
*************

drop eduui_ci
gen byte eduui_ci=0 if edad_ci>=4
replace eduui_ci=1 if aedu_ci>=13 & aedu_ci<=16 & ult_nivel_alcanzado==5 
replace eduui_ci=1 if aedu_ci>=13 & aedu_ci<=14 & ult_nivel_alcanzado==4 
label var eduui_ci "universitaria incompleta"

*************
**eduuc_ci***
*************

drop eduuc_ci
gen byte eduuc_ci=0 if edad_ci>=4
replace eduuc_ci=1 if aedu_ci==17 & ult_nivel_alcanzado==5 
replace eduuc_ci=1 if aedu_ci==15 & ult_nivel_alcanzado==4 
label var eduuc_ci "universitaria completa"
*/


**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<16
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=16
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edus1i_ci***
***************

gen byte edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************

gen byte edupre_ci= (ult_nivel_alcanzado==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=1 if ult_nivel_alcanzado==5 
replace eduac_ci=0 if ult_nivel_alcanzado==4
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************
generat asiste_ci=1 if tanda_asiste!=7 
replace asiste_ci=0 if tanda_asiste==7 | se_matriculo==7
replace asiste_ci=. if tanda_asiste==. & se_matriculo==.
label variable asiste_ci "Asiste actualmente a la escuela"

*Note: Se corrije en este anio (2010) la variable asiste_ci, puesto que  
// que es mejor considerar la variable asistencia y no matricula
// para mantener la homogeneización con el resto de países y acercarse mas a la def. del sociometro.



**************
***pqnoasis***
**************

gen pqnoasis_ci=motivo_no_asiste
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"Muy caro" 2"El trabajo no se lo permite" 3"Muy lejos" 4"Le fue mal"
label def pqnoasis_ci 5"Espera inicio de nuevo periodo" 6"Por incapacidad física o mental", add
label def pqnoasis_ci 7"Por edad" 8"Terminó sus estudios" 9 "otras" 10"Razones familiares" 11"Nunca lo inscribieron", add
label val pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if motivo_no_asiste==1
replace pqnoasis1_ci = 2 if motivo_no_asiste==2
replace pqnoasis1_ci = 3 if motivo_no_asiste==6  | motivo_no_asiste==10
replace pqnoasis1_ci = 6 if motivo_no_asiste==8
replace pqnoasis1_ci = 7 if motivo_no_asiste==7 
replace pqnoasis1_ci = 8 if motivo_no_asiste==3
replace pqnoasis1_ci = 9 if motivo_no_asiste==4  | motivo_no_asiste==5 | motivo_no_asiste==9 | motivo_no_asiste==11

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************

gen edupub_ci=0
replace edupub_ci=1 if tipo_centro==2
replace edupub_ci=. if tipo_centro==.
label var edupub_ci "Asiste a un centro de enseñanza público"



*************
**tecnica_ci*
*************

gen tecnica_ci=.
replace tecnica_ci=1 if ult_nivel_alcanzado==4 
replace tecnica_ci=0 if ult_nivel_alcanzado==5 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************


****************
***aguared_ch***
****************

gen aguared_ch=.
replace aguared_ch=(agua_red_publica==1)
label var aguared_ch "Acceso a fuente de agua por red"


*****************
***aguadist_ch***
*****************

gen aguadist_ch=.
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************

gen aguamala_ch=.
label var aguamala_ch "Agua unimproved según MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch=.
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch=0
replace combust_ch=1 if combustible_cocina==1 | combustible_cocina==3 | combustible_cocina==2
replace combust_ch=0 if combustible_cocina==99
label var combust_ch "Principal combustible gas o electricidad" 
*Modificado Mayra Sáenz - Febrero 2014. De acuerdo al documento metodológico 1 es sí y 0 el resto.


*************
***bano_ch***
*************

gen bano_ch=1
replace bano_ch=0 if tipo_sanitario==4 
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=.
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if tipo_sanitario==1 | tipo_sanitario==2
replace des1_ch=2 if tipo_sanitario==3 
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if tipo_sanitario==1 | tipo_sanitario==2 | tipo_sanitario==3 
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************

gen piso_ch=1 if piso!=17 & piso!=.
replace piso_ch=0 if piso==17 | piso==20
replace piso_ch=2 if piso==21
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"otros materiales"
label val piso_ch piso_ch

**************
***pared_ch***
**************

gen pared_ch=1 
replace pared_ch=0 if pared_exterior==3 | pared_exterior==18 | pared_exterior==19 | pared_exterior==20
replace pared_ch=2 if pared_exterior==21
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"otros materiales"
label val pared_ch pared_ch

**************
***techo_ch***
**************

gen techo_ch=1
replace techo_ch=0 if techo==18 
replace techo_ch=2 if techo==21
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2"otros materiales"
label val techo_ch techo_ch


**************
***resid_ch***
**************

gen resid_ch =.   
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if agua_red_publica ==1
replace aguamejorada_ch = 0 if agua_red_publica ==2
		
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if (tipo_sanitario >=1 & tipo_sanitario <=3)
replace banomejorado_ch = 0 if  tipo_sanitario == 4


*************
***dorm_ch***
*************
*Hay hogares que reportan no tener cuartos exclusivamente para dormir y cuentan con un solo espacio. Para estas 
*observaciones se cambia el 0 que tienen por 1. Porque aunque no sea exclusivo tienen un espacio para dormitorio

gen dorm_ch=cant_dormitorios
replace dorm_ch=1 if cant_dormitorios==0
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=cant_cuartos
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=0
replace telef_ch=1 if telefono==1
replace telef_ch=. if telefono==.
label var telef_ch "El hogar tiene servicio telefónico fijo"


***************
***refrig_ch***
***************

gen refrig_ch=0
replace refrig_ch=1 if  refrigerador==1
replace refrig_ch=. if  refrigerador==.
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"


*************
***auto_ch***
*************

gen auto_ch=0
replace auto_ch=1 if automovil==1
replace auto_ch=. if automovil==.
label var auto_ch "El hogar posee automovil particular"


**************
***compu_ch***
**************

gen compu_ch=0
replace compu_ch=1 if computadora==1
replace compu_ch=. if computadora==.
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************

gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"


************
***cel_ch***
************
destring celular, replace
gen cel_ch=0
replace cel_ch=1 if celular==1
replace cel_ch=. if celular==.
label var cel_ch "El hogar tiene servicio telefonico celular"



**************
***vivi1_ch***
**************

gen vivi1_ch=1 if tipo_vivienda==1 | tipo_vivienda==2 | tipo_vivienda==3
replace vivi1_ch=2 if tipo_vivienda==4 | tipo_vivienda==5 
replace vivi1_ch=3 if tipo_vivienda==6 | tipo_vivienda==7 | tipo_vivienda==8  | tipo_vivienda==9
*Modificado Mayra Sáenz - Febrero 2014 tipo de vivienda ==9 corresponde a otros no a missing.
*replace vivi1_ch=. if tipo_vivienda==9
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

gen vivi2_ch=0
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************

gen viviprop_ch=0 if tipo_tenencia==7
replace viviprop_ch=1 if tipo_tenencia==1 | tipo_tenencia==5
replace viviprop_ch=2 if tipo_tenencia==2 | tipo_tenencia==3
replace viviprop_ch=3 if tipo_tenencia==4 | tipo_tenencia==6 | tipo_tenencia==8
replace viviprop_ch=. if tipo_tenencia==9
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch = . if paga_alquiler==1
replace vivialq_ch=monto_alquiler 	  if periodo_pago_alq==2
replace vivialq_ch=monto_alquiler*4.3   if periodo_pago_alq==1
replace vivialq_ch=monto_alquiler*2   if periodo_pago_alq==3
replace vivialq_ch=monto_alquiler/12  if periodo_pago_alq==4
replace vivialq_ch=. if monto_alquiler==0
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=monto_probable_alq 
label var vivialqimp_ch "Alquiler mensual imputado"
*Modificación Mayra Sáenz - Febrero 2014
gen rentaimp_ch= vivialqimp_ch
label var rentaimp_ch "Rentas imputadas del hogar"

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(pais_reside!=647) if pais_reside!=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(migrante_ci==1 & inlist(pais_reside,63,77,83,88,97,105,169,196,211,239,242,317,325,341,345,391,493,580,586,589,770,810,845,850)) if migrante_ci!=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	/* Codigos obtenidos de la carpeta de docs originales de DOM de 2005, archivo llamado Diccionario ENFT Octubre 2005 */
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=(migrante_ci==1 & inlist(pais_reside,63,77,83,88,97,105,169,196,211,239,242,317,325,341,345,391,493,580,586,589,770,810,845,850)) if migrante_ci!=.
	replace miglac_ci = 0 if !inlist(pais_reside,63,77,83,88,97,105,169,196,211,239,242,317,325,341,345,391,493,580,586,589,770,810,845,850) & migrante_ci==1
	replace miglac_ci = . if migrante_ci==0 
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	************************** 
	** PROVINCIAS ************
	**************************

   gen ine01=.   
   replace ine01=1  if  provincia==1			/*Distrito Nacional*/
   replace ine01=2  if  provincia==2			/*Azua*/
   replace ine01=3  if  provincia==3			/*Bahoruco*/
   replace ine01=4  if  provincia==4			/*Barahona*/
   replace ine01=5  if  provincia==5		    /*Dajabon*/
   replace ine01=6  if  provincia==6			/*Duarte*/
   replace ine01=7  if  provincia==7			/*Elias Piña*/
   replace ine01=8  if  provincia==8			/*El Seibo*/
   replace ine01=9  if  provincia==9			/*Espaillat*/
   replace ine01=10 if  provincia==10			/*Independencia*/
   replace ine01=11 if  provincia==11			/*La Altagracia*/
   replace ine01=12 if  provincia==12			/*La Romana*/
   replace ine01=13 if  provincia==13			/*La Vega*/
   replace ine01=14 if  provincia==14			/*Maria Trinidad Sanchez*/
   replace ine01=15 if  provincia==15			/*Monte Cristi*/
   replace ine01=16 if  provincia==16			/*Pedernales*/
   replace ine01=17 if  provincia==17			/*Peravia*/
   replace ine01=18 if  provincia==18			/*Puerto Plata*/
   replace ine01=19 if  provincia==19			/*Salcedo*/
   replace ine01=20 if  provincia==20		    /*Samana*/
   replace ine01=21 if  provincia==21			/*San Cristobal*/
   replace ine01=22 if  provincia==22			/*San Juan*/
   replace ine01=23 if  provincia==23			/*San Pedro De Macoris*/
   replace ine01=24 if  provincia==24			/*Sanchez Ramirez*/
   replace ine01=25 if  provincia==25			/*Santiago*/
   replace ine01=26 if  provincia==26			/*Santiago Rodriguez*/
   replace ine01=27 if  provincia==27			/*Valverde*/
   replace ine01=28 if  provincia==28			/*Monseñor Nouel*/
   replace ine01=29 if  provincia==29			/*Monte Plata*/
   replace ine01=30 if  provincia==30			/*Hato Mayor*/

	label define ine01 1"Distrito Nacional" 2"Azua" 3"Bahoruco" 4"Barahona" 5"Dajabon" 6"Duarte" 7"Elias Piña" 8"El Seibo" 9"Espaillat" 10"Independencia" 11"La Altagracia" 12"La Romana" 13"La Vega" 14"Maria Trinidad Sanchez" 15"Monte Cristi" 16"Pedernales" 17"Peravia" 18"Puerto Plata" 19"Salcedo" 20"Samana" 21"San Cristobal" 22"San Juan" 23"San Pedro De Macoris" 24"Sanchez Ramirez" 25"Santiago" 26"Santiago Rodriguez" 27"Valverde" 28"Monseñor Nouel" 29"Monte Plata" 30"Hato Mayor"
	label value ine01 ine01
	label var ine01 " Primera division politico-administrativa, Provincia"	


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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close





