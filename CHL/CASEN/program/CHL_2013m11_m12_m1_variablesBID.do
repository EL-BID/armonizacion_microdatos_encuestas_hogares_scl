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

local PAIS CHL
local ENCUESTA CASEN
local ANO "2013"
local ronda m11_m12_m1

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
   
capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Chile
Encuesta: CASEN
Round: Noviembre - Diciembre
Autores: Marcela G. Rubio - Email: mrubio@iadb.org

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

	***************
	***region_c ***
	***************
	gen region_c=region
	label define region_c ///
	1"Tarapacá"                               		///
	2"Antofagasta"							  		///
	3"Atacama"                                		///
	4"Coquimbo"                               		///
	5"Valparaíso"                             		///
	6"Libertador General Bernardo Ohiggins"  		///
	7"Maule"                                  		///
	8"Bío bío"                               		///
	9"La Araucanía"                           		///
	10"Los Lagos"                             		///
	11"Aysén del General Carlos Ibañez del Campo"   ///
	12"Magallanes y de la Antártica Chilena"        ///
	13"Metropolitana de Santiago"                   ///
	14"Los Ríos"                                    /// 
	15"Arica y Parinacota"
   label value region_c region_c
   label var region_c "division politico-administrativa, region"
   
*************
* factor_ch *
*************

gen factor_ch=expr   
label var factor_ch "Factor de expansión el hogar"

*************
* idh_ch    *
*************
sort folio
egen idh_ch=group(folio) 
label var idh_ch "ID del hogar"

*************
* idp_ch    *
*************
gen idp_ci=o
label variable idp_ci "ID de la persona en el hogar"

*************
* zona_c    *
*************
gen zona_c=zona
recode zona_c (2=0)
label variable zona_c "Zona urbana vs. rural"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

*************
* pais_c    *
*************
gen pais_c="CHL"
label variable pais_c "Pais"


*************
* anio_c    *
*************
gen anio_c=2013
label variable anio_c "Anio de la encuesta"

*************
* mes_c    *
*************
tostring fecha, gen(date)
gen str mes_c = substr(date,5,2)
tostring mes, replace 
label var mes_c "Mes de la encuesta"
destring mes_c, replace

***************
* relacion_ci *
***************
gen relacion_ci=1     if pco1==1
replace relacion_ci=2 if pco1==2
replace relacion_ci=3 if pco1==3 | pco1==4 | pco1==5
replace relacion_ci=4 if pco1>=6 & pco1<=12
replace relacion_ci=5 if pco1==13
replace relacion_ci=6 if pco1==14
label var relacion_ci "Relación de parentesco con el jefe"
label def relacion_ci 1"Jefe" 2"Conyuge" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico"
label val relacion_ci relacion_ci	

		**************************
		* VARIABLES DEMOGRAFICAS *
		**************************

***************
* factor_ci   * 
***************
*gen factor_ci=expr_r20 

* modificacion MLO, 2015, m4 (con expr_r20 da una pobladcion de 13 mill en vez de 17)
gen factor_ci=expr
label variable factor_ci "Factor de expansion del individuo"

	***************
	***upm_ci***
	***************
gen upm_ci=. 

	***************
	***estrato_ci***
	***************
gen estrato_ci=.

***************
* sexo_ci     * 
***************
gen sexo_ci=sexo
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci


***************
* edad_ci     * 
***************
gen edad_ci=edad
label var edad_ci "Edad del individuo"

***************
* civil_ci    * 
***************
gen civil_ci=1     if ecivil==7
replace civil_ci=2 if ecivil==1 | ecivil==2
replace civil_ci=3 if ecivil==3 | ecivil==4 | ecivil==5
replace civil_ci=4 if ecivil==6
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

***************
* jefe_ci     * 
***************
gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"

****************
* nconyuges_ch * 
****************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

****************
* nhijos_ch    * 
****************
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

****************
* notropari_ch * 
****************
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

******************
* notronopari_ch * 
******************
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"

****************
* nempdom_ch   * 
****************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

****************
* clasehog_ch  * 
****************
gen clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

****************
* nmiembros_ch * 
****************
bysort idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=4) 
label variable nmiembros_ch "Numero de familiares en el hogar"

****************
* nmayor21_ch  * 
****************
bysort idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci>=21 & edad_ci<=98))
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

****************
* nmenor21_ch  * 
****************
bysort idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci<21))
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

****************
* nmayor65_ch  * 
****************
bysort idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci>=65))
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
* nmenor6_ch   * 
****************
bysort idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci<6))
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
* nmenor1_ch   * 
****************
bysort idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=4) & (edad_ci<1))
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
* miembros_ci   * 
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"



         ******************************
         *** VARIABLES DE DIVERSIDAD **
         ******************************
*Nathalia Maya & Antonella Pereira
*Feb 2021	

	***************
	***afroind_ci***
	***************
**Pregunta: Pueblos indígenas, pertenece usted o es descendiente de alguno de ellos? (r6) (Aimara 1; Rapa-Nui o Pascuenses 2; Quechua 3; Mapuche 4; Atacame�o (Likan-Antai) 5; Collas 6; Kawashkar o Alacalufes 7; Y�mana o Yag�n 8; Diaguita 9; No pertenece a ning�n pueblo ind�gena 10; No sabe/no responde 99)
gen afroind_ci=. 
replace afroind_ci=1 if (r6 >=1 & r6 <=9 )
replace afroind_ci=2 if r6==0
replace afroind_ci=3 if r6==10
replace afroind_ci=. if  r6==99 

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2006

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 



		*********************************
		* VARIABLES DEL MERCADO LABORAL *
		*********************************

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if (o1==1 | o2==1 | o3==1)
replace condocup_ci=2 if ((o1==2 | o2==2 | o3==2) & (o6==1))
recode condocup_ci (.=3) if edad_ci>=12 
replace condocup_ci=4 if edad<12
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

****************
* horaspri_ci  * 
****************
gen horaspri_ci=o10
replace horaspri_ci=. if o10==999 | emp_ci!=1
label var horaspri_ci "Horas totales trabajadas en la actividad principal"

****************
* horastot_ci  * 
****************
gen horastot_ci=horaspri_ci /*No existen horas totales solo act princ */
label var horastot_ci "Horas totales trabajadas en todas las actividades"

****************
* desalent_ci  * 
**************** 
gen desalent_ci=(o7r1==7 | o7r1==14)
label var desalent_ci "Trabajadores desalentados"

****************
* subemp_ci    * 
**************** 
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci<=30 & o11==1)
label var subemp_ci "Personas en subempleo por horas"
 
****************
*tiempoparc_ci * 
**************** 
gen tiempoparc_ci=(o10<=30 & (o11==1 | o11==2)) /*sigo con la misma estructura aunq creo q la opcion 2 de o11 debe ser subemp  */
replace tiempoparc_ci=. if emp_ci!=1
label var tiempoparc_c "Personas que trabajan medio tiempo" 

****************
*categopri_ci  * 
**************** 
gen categopri_ci=.
replace categopri_ci=1 if o15==1
replace categopri_ci=2 if o15==2
replace categopri_ci=3 if o15>=3 & o15<=8
replace categopri_ci=4 if o15==9
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional en la actividad principal"

****************
*categosec_ci  * 
****************

gen categosec_ci=.
replace categosec_ci=1 if o28==1
replace categosec_ci=2 if o28==2
replace categosec_ci=3 if o28>=3 & o28<=8
replace categosec_ci=4 if o28==9
replace categosec_ci=. if emp_ci!=1
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categosec_ci 3"Empleado" 4" No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


/*
****************
* contrato_ci  * 
**************** 
gen contrato_ci=(o17==1)
replace contrato_ci=. if emp_ci!=1
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

****************
* segsoc_ci    * 
**************** 
*Se toma afilición a pensiones
gen segsoc_ci=(o29==1)
replace segsoc_ci=. if emp_ci!=1
label var segsoc_ci "Personas que tienen seguro social"*/

****************
* nempleos_ci  * 
**************** 
gen nempleos_ci=1 if o27==2
replace nempleos_ci=2 if o27==1
label var nempleos_ci "Número de empleos" 
label def nempleos_ci 1"Un empleo" 2"Más de un empleo"
label val nempleos_ci nempleos_ci

/*
****************
* firmapeq_ci  * 
**************** 
gen firmapeq_ci=.
replace firmapeq_ci=1 if (o24>=1 & o24<=2) 
replace firmapeq_ci=0 if (o24>=3 & o24<=6)   
replace firmapeq_ci=. if emp_ci!=1
label var firmapeq_ci "Trabajadores informales"
label def firmapeq_ci 1"5 o menos trabajadores" 0"Más de 5 trabajadores"
label val firmapeq_ci firmapeq_ci*/
 
****************
* spublico_ci  * 
**************** 
gen spublico_ci=(o15==3 | o15==4 | o15==8)
replace spublico_ci=. if emp_ci!=1
label var spublico_ci "Personas que trabajan en el sector público"

		********************************
		* VARIABLES DE DEMANDA LABORAL *
		********************************

****************
* ocupa_ci     * 
****************

destring oficio4, replace
capture drop ocupa_ci
gen ocupa_ci=.
replace ocupa_ci=1 if (oficio4>=2111 & oficio4<=3480) & emp_ci==1
replace ocupa_ci=2 if (oficio4>=1110 & oficio4<=1319) & emp_ci==1
replace ocupa_ci=3 if (oficio4>=4110 & oficio4<=4223) & emp_ci==1
replace ocupa_ci=4 if ((oficio4>=9110 & oficio4<=9113) | (oficio4>=5210 & oficio4<=5230)) & emp_ci==1
replace ocupa_ci=5 if ((oficio4>=5111 & oficio4<=5169) | (oficio4>=9120 & oficio4<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((oficio4>=6110 & oficio4<=6210) | (oficio4>=9210 & oficio4<=9220)) & emp_ci==1
replace ocupa_ci=7 if ((oficio4>=7111 & oficio4<=8340) | (oficio4>=9311 & oficio4<=9333))  & emp_ci==1
replace ocupa_ci=8 if oficio4==110 & emp_ci==1
replace ocupa_ci=9 if oficio4==9999 
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci


****************
* rama_ci      * 
****************
/*
generat rama_ci=1 if rama1==1 | rama1==2
replace rama_ci=2 if rama1==3
replace rama_ci=3 if rama1==4
replace rama_ci=4 if rama1==5 
replace rama_ci=5 if rama1==6 
replace rama_ci=6 if rama1>=7 &  rama1<=8
replace rama_ci=7 if rama1==9
replace rama_ci=8 if rama1>=10 & rama1<=11
replace rama_ci=9 if rama1>=12 & rama1<=16
replace rama_ci=. if emp_ci!=1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
*/

destring rama4, replace

gen rama_ci=.
replace rama_ci=1 if (rama4>=100 & rama4<=599) & emp_ci==1
replace rama_ci=2 if (rama4>=1000 & rama4<=1499) & emp_ci==1
replace rama_ci=3 if (rama4>=1500 & rama4<=3799) & emp_ci==1
replace rama_ci=4 if (rama4>=4000 & rama4<=4199) & emp_ci==1
replace rama_ci=5 if (rama4>=4500 & rama4<=4599) & emp_ci==1
replace rama_ci=6 if (rama4>=5000 & rama4<=5599) & emp_ci==1
replace rama_ci=7 if (rama4>=6000 & rama4<=6499) & emp_ci==1
replace rama_ci=8 if (rama4>=6500 & rama4<=7099) & emp_ci==1
replace rama_ci=9 if (rama4>=7100 & rama4<=9990) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

		**************
		***INGRESOS***
		**************


****************
* ylmpri_ci    * 
****************
gen ylmpri_ci=yopraj 
replace ylmpri_ci=. if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

****************
* ylnmpri_ci   * 
**************** 
gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal" 

****************
* ylmsec_ci    * 
**************** 
gen ylmsec_ci=ytrabaj-yopraj  if emp_ci==1 
replace ylmsec_ci=. if ylmsec_ci==0
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
* ylnmsec_ci   * 
**************** 
gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

****************
* ylmotros_ci  * 
**************** 
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

****************
* ylnmotros_ci * 
**************** 
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

****************
* nrylmpri_ci  * 
**************** 
gen nrylmpri_ci=(emp_ci==1 & ylmpri_ci==.)
replace nrylmpri_ci=. if emp_ci!=1
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

****************
* ylm_ci       * 
**************** 
gen ylm_ci= ytrabaj 
replace ylm_ci=. if emp_ci!=1
label var ylm_ci "Ingreso laboral monetario total" 

****************
* ylnm_ci      * 
**************** 
gen ylnm_ci=.
label var ylnm_ci "Ingreso laboral NO monetario total"  

****************
* ynlm_ci      * 
**************** 
gen inglab =  ytrabaj *-1
egen ynlm_ci = rsum (yautaj inglab  ysubaj), missing
replace ynlm_ci=. if yautaj==. & inglab==. & ysubaj==. 
label var ynlm_ci "Ingreso no laboral monetario"  

/* Nota:

ytotcorh = yautcorh yaimcorh ysubh

yautaj= ingresos autonomos (la suma de todos los pagos que reciben las 
personas, provenientes tanto del trabajo como de la propiedad de los activos)

ytrabaj = ingreso laboral (Corresponden a los ingresos que obtienen las 
personas en su ocupación por concepto de sueldos y salarios, monetarios y 
en especies ganancias provenientes del trabajo independiente la auto-provisión
de bienes producidos por el hogar)

ysubaj=todos los aportes en dinero que reciben las personas y los hogares del 
Estado a través de los programas sociales.

*/

/*
****************
* ylmpri_ci    * 
****************

/*La variable de la base ya ha sido mensualizada
foreach var of varlist y0401 y0402 y0403 y0404{
gen `var'_m = `var'/12
} 
*/

*asalariados
egen aympri=rsum(y0101c y0301 y0302 y0303 y0304 y0305 y0306 y0401 y0402 y0403 y0404), missing 
replace aympri = . if y0101c==. & y0301==. & y0302==. & y0303==. & y0304==. & y0305==. & y0306==. & y0401==. & y0402==. & y0403==. & y0404==. 
replace aympri=. if emp_ci==0

*independientes
/* gen venta_m = y0901/12 esta variable ya fue mensualizada*/

/*
y0701c: independientes	principal	-	efectivo
y0901: independientes	principal	-	ventas anuales
*/

egen iympri= rsum(y0701c y0901)
replace iympri = . if y0701c ==. & y0901==.

egen ylmpri_ci=rsum(aympri iympri)
replace ylmpri_ci=. if aympri==. & iympri==.

label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

****************
* ylnmpri_ci   * 
**************** 

/*
y0501:asalariados	principal	-	alimentos y bebidas
y0502: asalariados	principal	-	vales de alimentación
y0503: asalariados	principal	-	vivienda o alojamiento
y0504: asalariados	principal	-	vehículo para uso privado
y0505: asalariados	principal	-	servicio de transporte
y0506: asalariados	principal	-	estacionamiento gratuito
y0507: asalariados	principal	-	teléfono
y0508: asalariados	principal	-	vestimenta
y0509: asalariados	principal	-	servicios de guardería o sala cuna
y0510: asalariados	principal	-	leña
y0511: asalariados	principal	-	bienes o servicios del empleador
y0512: asalariados	principal	-	otros similares en especies
y0801: independientes principal - especies
*/

*asalariados
egen aylnmpri = rsum(y0501  y0502  y0503 y0504 y0505 y0506 y0507 y0508 y0509 y0510 y0511 y0512), missing
replace aylnmpri = . if y0501==. & y0502==. &  y0503==. & y0504==. & y0505==. & y0506==. & y0507==. & y0508==. & y0509==. & y0510==. & y0511==. & y0512==. 

*independientes
gen iylnmpri = y0801

egen ylnmpri_ci= rsum(aylnmpri iylnmpri)
replace ylnmpri_ci=. if aylnmpri==. & iylnmpri==.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal" 

****************
* ylmsec_ci    * 
**************** 

egen ylmsec_ci= rsum(yosa  yosi), missing
replace ylmsec_ci=. if yosa==. & yosi==.
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
* ylnmsec_ci   * 
**************** 
gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

****************
* ylmotros_ci  * 
**************** 

* ytro: remuneración por trabajos ocasionales
* ydes: seguro de desempleo o cesantía
* yta1: trabajos de antes - asalariados
* yta2: trabajos de antes - no asalariados
* y1101: ingresos del trabajo de familiares no remunerados, desocupados e inactivos
egen ylmotros_ci= rsum(ytro ydes  yta1 yta2 y1101), missing
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

****************
* ylnmotros_ci * 
**************** 
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

****************
* nrylmpri_ci  * 

**************** 
gen nrylmpri_ci=(emp_ci==1 & ylmpri_ci==.)
replace nrylmpri_ci=. if emp_ci!=1
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

****************
* ylm_ci       * 
**************** 
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

****************
* ylnm_ci      * 
**************** 
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

****************
* ynlm_ci      * 
**************** 

/* La variable de la base ya ha sido mensualizada
foreach var of varlist yah1 yah2 yrut yre2 yre3 yac2 yids ydon ydim yotr{
gen `var'_m = `var'/12
}
*/

gen interes = yah1

gen dividendo = yah2

gen util = yrut

egen arriendo = rsum(yre1 yama  yre2   yre3), missing  

gen autocons = yac2

gen indem = yids

gen dona = ydon

gen devolu = ydim

gen otros = yotr

gen subsidio = ysub

egen familiar = rsum(yfa1 yfa2), missing

* no podemos utilizar numper o idh porque el numero actual de personas en el hogar difiere en ambos casos y al expandir la variable a nivel hogar nos da un monto diferente
gen aux = 1
bys idh_ch: egen id = sum(aux)
drop aux
gen alquiler_pc = yaimcorh/id

egen pension = rsum(y2701c yinv ymon yorf yotp ymes), missing
replace pension=. if y2701c==. &  yinv==. & ymon==. & yorf==. & yotp==. & ymes==.

egen ynlm_ci = rsum(subsidio pension arriendo interes dividendo util autocons indem dona devolu otros familiar), missing
replace ynlm_ci = . if subsidio==. & pension==. & arriendo==. & interes==. & dividendo==. & util==. & autocons==. & ///
indem==. & dona==. & devolu==. & otros==. & familiar==. 
*/

****************
* ynlnm_ci     * 
**************** 
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


****************
* nrylmpri_ch  * 
**************** 
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

****************
* ylm_ch       * 
**************** 
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

****************
* ylnm_ch      * 
**************** 
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

************
* ylmnr_ch *
************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***********
* ynlm_ch *
***********
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"

*************
* ynlnm_ch  *
*************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*****************
* ymlhopri_ci   *
*****************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 

*************
* ylmho_ci  *
*************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 

****************
* rentaimp_ch  * 
**************** 
gen rentaimp_ch=yaimhaj
label var rentaimp_ch "Rentas imputadas del hogar"

****************
* autocons_ci  * 
**************** 
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

****************
* autocons_ci  * 
**************** 
gen autocons_ch=.
label var autocons_ch "Autoconsumo del hogar"

****************
* remesas_ci   * 
**************** 
gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
* remesas_ch   * 
**************** 
gen remesas_ch=.
label var remesas_ch "Remesas mensuales del hogar"

****************
* durades_ci   * 
**************** 
gen durades_ci=o8/4.3
replace durades_ci=. if o8==999 /*| activ!=2*/
label var durades_ci "Duración del desempleo en meses"

****************
* antiguedad_ci* 
**************** 
recode o13 (2014 = 2013)
gen antiguedad_ci=(2013-o13)+1
replace antiguedad_ci=. if o13==9999 | emp_ci!=1 | antiguedad_ci>edad_ci
label var antiguedad_ci "Antiguedad en la actividad actual"

		************************
		* VARIABLES EDUCATIVAS *
		************************

***************
***asiste_ci*** 
*************** 
gen asiste_ci=(e3==1)
replace asiste_ci=. if e3==.
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci*** 
************* 
replace e6b = . if e6b == 99
replace e6a = . if e6a == 99

gen aedu_ci = .
replace aedu_ci=0              if e6a>=1 & e6a<=3    /*Nunca asistió, sala cuna. jardin infantil, prekinder/kinder */

* Para aquellos que asisten actualmente
replace aedu_ci = e6b      if inrange(e6a, 5, 6) & e3 == 1 /*Preparatoria  (Sist. antiguo) y Básica (Sist. nuevo) */
replace aedu_ci = e6b + 6 - 1  if inrange(e6a, 7, 9) & e3 == 1  /*Humanidades (Sist. antiguo) Técnica, Comercial, Industrial o Normalista (Sist. antiguo) */
replace aedu_ci = e6b + 8 - 1   if inrange(e6a, 8, 10) & e3 == 1 /*Educación Media Científico Humanística (Sist. nuevo) Educación Media Técnica Profesional (Sist. nuevo)*/           
replace aedu_ci = e6b + 12 - 1    if e6a >= 11 & e6a <= 12  & e3 == 1 /*Tecnico nivel superior completo o incompleto, profesional completo o incompleto*/
replace aedu_ci = e6b + 16 - 1  if e6a == 13 & e3 == 1   /*Posgrado*/

* Para aquellos que no asisten actualmente
replace aedu_ci = e6b      if inrange(e6a, 5, 6) & e3 == 2 /*Preparatoria  (Sist. antiguo) y Básica (Sist. nuevo) */
replace aedu_ci = e6b + 6  if inrange(e6a, 7, 9) & e3 == 2 /*Humanidades (Sist. antiguo) Técnica, Comercial, Industrial o Normalista (Sist. antiguo) */
replace aedu_ci = e6b + 8  if inrange(e6a, 8, 10) & e3 == 2/*Educación Media Científico Humanística (Sist. nuevo) Educación Media Técnica Profesional (Sist. nuevo)*/           
replace aedu_ci = e6b + 12 if e6a >= 11 & e6a <= 12 & e3 == 2  /*Tecnico nivel superior completo o incompleto, profesional completo o incompleto*/
replace aedu_ci = e6b + 16 if e6a == 13 & e3 == 2   /*Posgrado*/

* Imputando anios perdidos *

* para aquellos que no finalizaron *

replace aedu_ci = 0     if e6a==5 & aedu_ci==. & e6c != 1 //prebasica
replace aedu_ci = 0     if e6a==6 & aedu_ci==. & e6c != 1  //preparatoria
replace aedu_ci = 6     if e6a==7 & aedu_ci==. & e6c != 1 // humanidades
replace aedu_ci = 8     if e6a==8 & aedu_ci==. & e6c != 1 //media cientifico
replace aedu_ci = 6     if e6a==9 & aedu_ci==. & e6c != 1 // tp antigua
replace aedu_ci = 8     if e6a==10 & aedu_ci==.  & e6c != 1 //media tp
replace aedu_ci = 12    if (e6a==11 | e6a==12) & aedu_ci==. & e6c != 1 //profesional o tecnico nivel superior 
replace aedu_ci = 16    if e6a==13 & aedu_ci==.  & e6c != 1 // postgrado

* para aquellos que finalizaron *

replace aedu_ci = 0     if e6a==5 & aedu_ci==. & e6c == 1 //prebasica
replace aedu_ci = 6     if e6a==6 & aedu_ci==. & e6c == 1  //preparatoria
replace aedu_ci = 8     if e6a==7 & aedu_ci==. & e6c == 1 // humanidades
replace aedu_ci = 12     if e6a==8 & aedu_ci==. & e6c == 1 //media cientifico
replace aedu_ci = 8     if e6a==9 & aedu_ci==. & e6c == 1 // tp antigua
replace aedu_ci = 12     if e6a==10 & aedu_ci==.  & e6c == 1 //media tp
replace aedu_ci = 14    if e6a==11 & aedu_ci==. & e6c == 1 // tecnico nivel superior
replace aedu_ci = 16    if e6a==12 & aedu_ci==. & e6c == 1 //profesional 
replace aedu_ci = 18    if e6a==13 & aedu_ci==.  & e6c == 1 // postgrado
label var aedu_ci "Anios de educacion aprobados" 


**************
***eduno_ci***
**************
gen byte eduno_ci=aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci=aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci=aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci=aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen byte edusc_ci=aedu_ci==12
replace edusc_ci=1 if aedu_ci==13 & e6a==10 // education TP con 13 anios
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
gen byte eduui_ci=(aedu>12 & aedu_ci<17 & e6a==12) //pregrado  
replace eduui_ci=1 if (aedu>12 & aedu_ci<15 & e6a==11) // superior tecnica
replace eduui_ci=0 if aedu_ci==13 & e6a==10 // education TP con 13 anios
replace eduui_ci=0 if aedu_ci==12 & (e6a==11 | e6a==12) & e6b==. // terciaria con anios imputados
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************
gen byte eduuc_ci=((aedu_ci>=17 & e6a==12)  | (aedu_ci>=15 & e6a==11)  | (aedu_ci>=12 & e6a==13))
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
gen edus1i_ci=0 // usando los anios de educacion
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<8 
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=0 // usando los anios de educacion
replace edus1c_ci=1 if aedu_ci==8 
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=0 // usando los anios de educacion
replace edus2i_ci=1 if aedu_ci>8 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=0 // usando los anios de educacion
replace edus2c_ci=1  if aedu_ci==12 
replace edus2c_ci=1  if aedu_ci==13 & e6a==10 // modalidad educacion TP con 13 anios
replace edus2c_ci=.  if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

***************
***edupre_ci***
***************
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

****************
***asispre_ci***
****************
*Creación de la variable asistencia a preescolar por Iván Bornacelly - 01/12/17
gen asispre_ci=(e3==1 & e6a==3) // Asiste Prekinder / Kinder
la var asispre_ci "Asiste a educacion prescolar"

**************
***eduac_ci***
**************
gen eduac_ci=(e6a>=12 & e6a<=13)
replace eduac_ci=0 if (e6a==11)
replace eduac_ci=. if e6a<=10 | e6a>13 
label variable eduac_ci "Superior universitario vs superior no universitario"

*****************
***pqnoasis_ci***
*****************
gen pqnoasis_ci=e5
label var pqnoasis_ci "Razones para no asistir a la escuela"
 
#delimit ;

label define pqnoasis_ci 
           1 "ayuda en la casa o quehaceres del hogar"
           2 "embarazo, maternidad o paternidad"
           3 "tiene una discapacidad, requiere establecimiento de educacion especial"
           4 "enfermedad que lo inhabilita"
           5 "problemas familiares"
           6 "no le interesa"
           7 "termino de estudiar"
           8 "a su edad no le sirve estudiar o no conoce la manera"
           9 "dificultad economica"
          10 "trabaja o busca trabajo"
          11 "problemas de rendimiento"
          12 "expulsion o cancelacion de matricula"
          13 "no existe establecimiento cercano"
          14 "dificultad de acceso o movilizacion"
          15 "otra razon. especifique"
          99 "ns/nr";

#delimit cr
label values pqnoasis_ci pqnoasis_ci

******************
***pqnoasis1_ci***
******************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e5 ==9
replace pqnoasis1_ci = 2 if e5 ==10
replace pqnoasis1_ci = 3 if e5 ==3 | e5 ==4 | e5 ==5
replace pqnoasis1_ci = 4 if e5 ==6
replace pqnoasis1_ci = 5 if e5 ==1 | e5 ==2
replace pqnoasis1_ci = 6 if e5 ==7 
replace pqnoasis1_ci = 7 if e5 ==8 
replace pqnoasis1_ci = 8 if e5 ==13  | e5 ==14
replace pqnoasis1_ci = 9 if e5 ==11 | e5 ==12 | e5 ==15 

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado"

******************
***repiteult_ci***
******************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el último grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
replace edupub_ci=1 if inlist(e8depen, 1, 2) & asiste_ci==1 //Municipales
replace edupub_ci=0 if inlist(e8depen, 3, 4) & asiste_ci==1 // Particular pagado, particular subvencionado
label var edupub_ci "Personas que asisten a centros de enseñanza públicos"

		******************************************
		* VARIABLES DE INFRAESTRUCTURA DEL HOGAR *
		******************************************

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if v23<=3 
replace aguared_ch = 0 if v23>3
la var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
*se asume a partir de los datos y el cuestionario que agua potable solo es de red.
gen aguafconsumo_ch = 0

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch=.
replace aguafuente_ch = 1 if v23<=3 & v24<=2
replace aguafuente_ch = 2 if v23<=3 & v24>2
replace aguafuente_ch = 6 if v23==6
replace aguafuente_ch = 8 if v23==5
replace aguafuente_ch = 10 if (v23==7 | v23==8 | v23==4)
replace aguafuente_ch = 10 if aguafuente_ch ==. & jefe_ci==1


*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch=1 if v24==1
replace aguadist_ch=2 if v24==2
replace aguadist_ch=3 if v24==3


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =9

**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9
*label var aguadisp2_ch "= 9 la encuesta no pregunta si el servicio de agua es constante"


*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10
*label var aguamala_ch "= 1 si la fuente de agua no es mejorada"

*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7
*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*****************
***aguamide_ch***
*****************
gen aguamide_ch = 1 if v23<=2
replace aguamide_ch  = 0 if v23>2
label var aguamide_ch "Usan medidor para pagar consumo de agua"

*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.
replace bano_ch=0 if v25==8
replace bano_ch=1 if v25==1
replace bano_ch=2 if v25==2
replace bano_ch=3 if v25==3 | v25==4 
replace bano_ch=4 if v25==5
replace bano_ch=5 if v25==7 
replace bano_ch=6 if v25==6
replace bano_ch=6 if bano_ch ==. & jefe_ci==1

***************
***banoex_ch***
***************
generate banoex_ch=9
la var banoex_ch "El servicio sanitario es exclusivo del hogar"


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
replace sinbano_ch = 0 if v25!=8

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"



***************
* luz_ch      *
***************
gen luz_ch=(v26<=6)
replace luz_ch=. if v26==.
label var luz_ch "La principal fuente de iluminación es electricidad"

***************
* luzmide_ch  *
***************
gen luzmide_ch=(v26==1 | v22==2)
replace luzmide_ch=. if luz_ch==0
label var luzmide_ch "El hogar usa un medidor para pagar el consumo de electricidad"

***************
* combust_ch  *
***************
gen combust_ch=.
label var combust_ch "El combustible pricipal usado en el hogar es gas o electricidad"


***************
* des1_ch     *
***************
gen des1_ch=0 if bano_ch==0 
replace des1_ch=1 if v25==1 | v25==2
replace des1_ch=2 if v25==3 | v25==4
replace des1_ch=3 if v25==5 | v25==6
replace des1_ch=. if v25==.
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

***************
* des2_ch     *
***************
gen des2_ch=0 if bano_ch==0 
replace des2_ch=1 if v25>=1 & v25<=4
replace des2_ch=2 if v25==5 | v25==6 | v25==7
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

***************
* piso_ch     *
***************
gen piso_ch=0 if v4==7
replace piso_ch=1 if v4<7
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

***************
* pared_ch    *
***************
gen pared_ch=0 if v2>=4 & v2<=6
replace pared_ch=1 if v2<4
replace pared_ch=. if v2==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch

***************
* techo_ch    *
***************
gen techo_ch=0 if v6>=4 & v6<=7
replace techo_ch=1 if v6<4
replace techo_ch=. if v6==.
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes"
label val techo_ch techo_ch

***************
* resid_ch    *
***************
gen resid_ch=.
label var resid_ch "Método de eliminación de residuos"

 
***************
* dorm_ch     *
***************
gen dorm_ch=v29a 
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

***************
* cuartos_ch  *
***************
*Esta construcción es la mejor aproximación y mantiene consistencia en el tiempo. Se usa la información del hogar.

foreach var of varlist v29a v29b v29c v29d v29e {
recode `var' (99=.)
}
egen aux_cuartos=rsum(v29a v29b v29c v29d v29e), missing
gen int cuartos_ch = (aux_cuartos/v30)
label var cuartos_ch "Cantidad de habitaciones en el hogar"
drop aux_cuartos

***************
* cocina_ch  *
***************
gen cocina_ch=(v29c>=1 & v29c!=.)
replace cocina_ch=. if v29c==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar" 

***************
* telef_ch    *
***************
bysort idh_ch: egen telef_ch=sum(r13d==1)
replace telef_ch=1 if telef_ch>=1 & telef_ch!=.
replace telef_ch=. if r13d==9
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
* refrig_ch   *
***************
bysort idh_ch: egen refrig_ch=sum(r13b==1)
replace refrig_ch=1 if refrig_ch>=1 & refrig_ch!=.
replace refrig_ch=. if r13b==9
label var refrig_ch "El hogar posee heladera o refrigerador"

***************
* freez_ch    *
***************
gen freez_ch=.
label var freez_ch "El hogar posee congelador"

***************
* auto_ch     * 
***************
bysort idh_ch: egen auto_ch=sum(r10==1)
replace auto_ch=1 if auto_ch>=1 & auto_ch!=.
replace auto_ch=. if r10==9
label var auto_ch "El hogar posee automovil particular"

***************
* compu_ch    * 
***************
bysort idh_ch: egen compu_ch=sum(r13f==1)
replace compu_ch=1 if compu_ch>=1 & compu_ch!=.
label var compu_ch "El hogar posee computadora"

***************
* internet_ch * 
***************
bysort idh_ch: egen internet_ch=sum(r14a==1 | r14b==1 | r14c==1 | r14d==1 | r14e==1 | r14f==1)
replace internet_ch=1 if internet_ch>=1 & internet_ch!=.
label var internet_ch "El hogar tiene conexión a Internet"

*************
* cel_ch    * 
*************
bysort idh_ch: egen cel_ch=sum(r19>=1 & r19<=3)
replace cel_ch=1 if cel_ch>=1 &  cel_ch!=.
label var cel_ch "El hogar tiene servicio telefónico celular"

***************
* vivi1_ch    * 
*************** 
gen vivi1_ch=1 if v1>=1 & v1<=3
replace vivi1_ch=2 if v1>=4 & v1<=5
replace vivi1_ch=3 if v1>=6 & v1!=.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

***************
* vivi2_ch    * 
*************** 
gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)
label var vivi2_ch "La vivienda es una casa o un departamento"

***************
* viviprop_ch * 
*************** 
gen viviprop_ch=0     if v9>=5 & v9<=6
replace viviprop_ch=1 if v9==1 | v9==3
replace viviprop_ch=2 if v9==2 | v9==4
replace viviprop_ch=3 if v9>=7 & v9<=11
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

***************
* vivitit_ch  * 
***************
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad" 

***************
* vivialq_ch  * 
***************
gen vivialq_ch=v21
label var vivialq_ch "Alquier mensual"

***************
*vivialqimp_ch* 
***************
gen vivialqimp_ch=yaimhaj
label var vivialqimp_ch "Alquiler mensual imputado"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =  388081/4.43

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =  258720/4.43

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if o30>=1 & o30<=6
recode cotizando_ci .=0 if (activ==1 | activ==2)
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizando_ci1=.
replace cotizando_ci1=1 if o30>=1 & o30<=6
recode cotizando_ci1 .=0 if (activ==1 | activ==2 | activ==3)
label var cotizando_ci1 "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if o29==1
recode afiliado_ci .=0 
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************

*Esta encuesta sólo incluye 4 tipos de pensiones: jubilación o vejez, invalidez, montepío, pensión de orfandad y otros (y2701aj yinvaj ymonaj  yorfaj)

gen tipopen_ci=.

replace tipopen_ci = 1 if (y2701aj > 0 & y2701aj != .)
replace tipopen_ci = 2 if (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 3 if (ymonaj > 0 & ymonaj != .)
replace tipopen_ci = 4 if (yorfaj > 0 & yorfaj != .)

replace tipopen_ci = 12 if (yinvaj > 0 & yinvaj != .) | (y2701aj > 0 & y2701aj != .)
replace tipopen_ci = 13 if (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .)
replace tipopen_ci = 14 if (yinvaj > 0 & yinvaj != .) | (yorfaj > 0 & yorfaj != .)

replace tipopen_ci = 23 if (ymonaj > 0 & ymonaj != .) | (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 24 if (yorfaj > 0 & yorfaj != .) | (yinvaj > 0 & yinvaj != .)

replace tipopen_ci = 123 if (y2701aj > 0 & y2701aj != .) | (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .)

replace tipopen_ci = 1234 if (y2701aj > 0 & y2701aj != .) | (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .) | (yorfaj > 0 & yorfaj != .)


label define  t 1 "Jubilacion" 2 "Pension invalidez" 3 "Pension viudez" 4 "Orfandad" 12 " Jub y inv" 13 "Jub y viud" 14 "Jub y orfandad" 23 "Viud e inv" 24 "orfandad y inv"  123 "Jub inv viud" 1234 "Todas"
label value tipopen_ci t

label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=y27ai
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
encode o30esp, gen(coti_otros)

gen instcot_ci=.
replace instcot_ci=1 if o30==1 | o30==2
replace instcot_ci=2 if o30==3
replace instcot_ci=3 if o30==4
replace instcot_ci=4 if o30==5
replace instcot_ci=5 if coti_otro>=1 & coti_otros<=59
label define instcot_ci 1 "AFP" 2 "IPS ex-INP" 3 "CAPRE-DENA" 4 "DIPRECA" 5  "Otros"
label value instcot_ci instcot_ci
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o16==1 & categopri_ci==3
replace tipocontrato_ci=2 if (o16==2 | o16==3) & categopri_ci==3
replace tipocontrato_ci=3 if o17==3 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
 */
 * Corregido por la variable de firmo o no firmo MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if ((o17==1 | o17==2) & o16==1) & categopri_ci==3
replace tipocontrato_ci=2 if ((o17==1 | o17==2) & o16==2) & categopri_ci==3
replace tipocontrato_ci=3 if ((o17>=3 | tipocontrato_ci==.) & categopri_ci==3)
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
* MGD oct-2016: modifico razon 2 a razon 1
gen cesante_ci=1 if o4==1 & (o6==1 | o7r1<=2)
replace cesante_ci=0 if o4==2 & (o6==1 | o7r1<=2)
label var cesante_ci "Desocupado - definicion oficial del pais"	


**************
***tamemp_ci**
**************

encode o24, gen(tama)

gen tamemp_ci=1 if tama==1 | tama==2 
replace tamemp_ci=2 if tama==3 | tama==4
replace tamemp_ci=3 if tama==5 | tama==6


label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

gen tamemp_o=1 if tama==1 | tama==2 | tama==3
replace tamemp_o=2 if tama==4 
replace tamemp_o=3 if tama==5 | tama==6

label var tamemp_ci "# empleados en la empresa segun rangos-OECD"
label define tamemp_o 1 "[1-9]" 2 "[10-49]" 3 "[50 y mas]"
label value tamemp_o tamemp_o


*************
**pension_ci*
*************

egen auxpen=rsum(y2701aj yinvaj ymonaj  yorfaj yotpaj), missing
gen pension_ci=1 if auxpen>0 & auxpen!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=auxpen
replace ypen_ci=. if auxpen<0
drop auxpen
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
egen auxpens=rsum(y2009aj y2010aj), missing
gen pensionsub_ci=1 if auxpens>0 & auxpens!=.
recode pensionsub_ci .=0 
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=auxpens
replace ypensub_ci=. if auxpens<0
drop auxpens
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
* Año base 2011
gen tc_ci=395.469173224
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

*http://www.leychile.cl/Consulta/listado_n_sel?_grupo_aporte&sub=807&agr=2&comp
gen salmm_ci= 210000
label var salmm_ci "Salario minimo legal"


**************
**categoinac_ci*
**************


gen categoinac_ci=1 if o7r1==12
replace categoinac_ci=2 if o7r1==11
replace categoinac_ci=3 if o7r1==10
replace categoinac_ci=4 if (o7r1>=1 & o7r1<=9 ) | (o7r1>= 13 & o7r1<=17)

label var categoinac_ci "Condición de inactividad"
label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
label value categoinac_ci categoinac_ci
	
***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

g formal_1=cotizando_ci1

*******************
***  benefdes_ci  ***
*******************

g benefdes_ci=0 if desemp_ci==1
replace benefdes_ci=1 if  ydesaj!=. & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci  ***
*******************
g ybenefdes_ci=ydesaj if benefdes_ci==1
label var ybenefdes_ci "Monto de seguro de desempleo"

* variables que faltan crear
gen tcylmpri_ci =.
gen tcylmpri_ch =.


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(r1b==3) if r1b!=9 & !mi(r1b)
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & inlist(r2,1,2)) if migrante_ci!=. & r2!=. & r2!=9
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(inlist(r1b_p_cod,406,408,409,412,413,416,417,418,420,501,502,503,505,506,508,509,512,513) & migrante_ci==1) if migrante_ci!=. & r2_p_cod!=999
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci **
	**********************
	
	gen migrantiguo5_ci= 1 if inlist(r2,1,2) & migrante_ci==1
	replace migrantiguo5_ci = 0 if (r2 == 3 & migrante_ci == 1)
	replace migrantiguo5_ci = . if migrante_ci == 0 | r2!=. & r2==9 & r2==1
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci= 1 if inlist(r1b_p_cod,406,408,409,412,413,414,416,417,418,420,501,502,503,505,506,508,509,512,513) & migrante_ci == 1
	replace miglac_ci = 0 if miglac_ci != 1 & migrante_ci == 1
	replace miglac_ci = . if r1b_p_cod==.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	
	/* Fuente: http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/docs/Libro_de_Codigos_Casen_2015.pdf */
	
	
	**************************
	** REGIONES ************** 
	**************************
	
   gen ine01=.   
   replace ine01=1 if  region==1		/*Tarapacá*/
   replace ine01=2 if  region==2		/*Antofagasta*/
   replace ine01=3 if  region==3		/*Atacama*/
   replace ine01=4 if  region==4		/*Coquimbo*/
   replace ine01=5 if  region==5    	/*Valparaíso*/
   replace ine01=6 if  region==6		/*O'Higgins*/
   replace ine01=7 if  region==7		/*Maule*/
   replace ine01=8 if  region==8		/*Bío Bío*/
   replace ine01=9 if  region==9		/*La Araucanía*/
   replace ine01=10 if region==10		/*Los Lagos*/
   replace ine01=11 if region==11		/*Aysén*/
   replace ine01=12 if region==12		/*Magallanes y Antártica Chilena*/
   replace ine01=13 if region==13		/*Metropolitana Santiago*/
   replace ine01=14 if region==14		/*Los Ríos*/
   replace ine01=15 if region==15		/*Arica y Parinacota*/
   
	label define ine01 1"Tarapacá" 2"Antofagasta" 3"Atacama" 4"Coquimbo" 5"Valparaíso" 6"O'Higgins" 7"Maule" 8"Bío Bío" 9"La Araucanía" 10"Los Lagos" 11"Aysén" 12"Magallanes y Antártica Chilena" 13"Metropolitana Santiago" 14"Los Ríos" 15"Arica y Parinacota"
	label value ine01 ine01
	label var ine01 " Primera division politico-administrativa, región"

	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci  ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) */
rename rama4 codindustria
rename oficio4 codocupa
compress


compress


saveold "`base_out'", replace


log close

