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
 


*global ruta = "${surveysFolder}"

local PAIS CHL
local ENCUESTA CASEN
local ANO "2009"
local ronda m11_m12 

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
Autores: 
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Versión 2012: Yanira Oviedo (YO), Yessenia Loaysa (YL)
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

	***************
	***region_c ***
	***************
*YL: generacion "region_c" para los años 2009 y +. Para proyecto maps America.	

	gen region_c=region
	label define region_c ///
	1"Tarapacá"                               		///
	2"Antofagasta"							  		///
	3"Atacama"                                		///
	4"Coquimbo"                               		///
	5"Valparaíso"                             		///
	6"Libertador General Bernardo Ohiggins"  		///
	7"Maule"                                  		///
	8" Bío Bío"                               		///
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
/*Esta es la expansion que se usa en todos los años anteriores. La provincial recien aparece en el 2000*/
gen factor_ch=expr 
label var factor_ch "Factor de expansión el hogar"

*************
* idh_ch    *
*************
sort  segmento idviv hogar
egen idh_ch=group(segmento idviv hogar) 
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
gen anio_c=2009
label variable anio_c "Anio de la encuesta"

*************
* mes_ch    *
*************
gen mes_c=11
label var mes_c "Mes de la encuesta"

***************
* relacion_ci *
***************
gen relacion_ci=1 if pco1==1
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
gen factor_ci=expr
label variable factor_ci "Factor de expansion del individuo"

	***************
	***upm_ci***
	***************
gen upm_ci=. 

	***************
	***estrato_ci***
	***************
clonevar estrato_ci=estrato
label variable estrato_ci "Estrato"


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
gen civil_ci=1 if ecivil==7
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
**Pregunta: Pueblos indígenas, pertenece usted o es descendiente de alguno de ellos? (t5) (Aimara 1; Rapa-Nui o Pascuenses 2; Quechua 3; Mapuche 4; Atacame�o (Likan-Antai) 5; Collas 6; Kawashkar o Alacalufes 7; Y�mana o Yag�n 8; Diaguita 9; No pertenece a ning�n pueblo ind�gena 10)
gen afroind_ci=. 
replace afroind_ci=1 if (t5 >=1 & t5 <=9 )
replace afroind_ci=2 if t5==0
replace afroind_ci=3 if t5==10 

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
replace condocup_ci=2 if ((o1==2 | o2==2 | o3==2) & (o4==1))
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

/*
****************
* emp_ci       * 
****************
*La encuesta contempla 3 preguntas para determinar si un individuo está ocupado y existe una variable de clasificación
*No obstante sólo se venían manejando 2 preguntas. Esto debería revisarse hacia atrás!
gen emp_ci=(o1==1 | (o1==2 & o2==1)| (o2==2 & o3==1))
label var emp_ci "Ocupado (empleado)"
label def emp_ci 1"Sí" 0"No"
label val emp_ci emp_ci

****************
* desemp1_ci   * 
**************** 
*Revisar hacia atrás las definiciones de desempleo.
gen desemp1_ci=(o4==1)
label var desemp1_ci "Desempleado que buscó empleo en el periodo de referencia"

****************
* desemp2_ci   * 
**************** 
*Revisar hacia atrás las definiciones de desempleo.
gen desemp2_ci=(o4==1 | o6==6)
replace desemp2_ci=. if o4==. & o6==.
label var desemp2_ci "desemp1_ci + personas que esperan respuesta a solicitud o temporada alta"

****************
* desemp3_ci   * 
**************** 
*Revisar hacia atrás las definiciones de desempleo.
gen desemp3_ci=.
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"

****************
* pea1_ci      * 
**************** 
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1
replace pea1_ci=. if emp_ci==. & desemp1_ci==.
label var pea1_ci "Población Económicamente Activa con desemp1_ci"

****************
* pea2_ci      * 
**************** 
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1
replace pea2_ci=. if emp_ci==. & desemp2_ci==.
label var pea2_ci "Población Económicamente Activa con desemp2_ci"

****************
* pea3_ci      * 
**************** 
gen pea3_ci=.
label var pea3_ci "Población Económicamente Activa con desemp3_ci"
*/
****************
* horaspri_ci  * 
****************
gen horaspri_ci=o16
replace horaspri_ci=. if o16==999 | emp_ci!=1
label var horaspri_ci "Horas totales trabajadas en la actividad principal"

****************
* horastot_ci  * 
****************
gen horastot_ci=horaspri_ci
label var horastot_ci "Horas totales trabajadas en todas las actividades"

****************
* desalent_ci  * 
**************** 
gen desalent_ci=(o4==2 & (o6==10 | o6==15))
label var desalent_ci "Trabajadores desalentados"

****************
* subemp_ci    * 
**************** 
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci<=30 & o17==1)
label var subemp_ci "Personas en subempleo por horas"

****************
*tiempoparc_ci * 
**************** 
gen tiempoparc_ci=(o16<=30 & (o17==2 | o17==3))
replace tiempoparc_ci=. if emp_ci!=1
label var tiempoparc_c "Personas que trabajan medio tiempo" 

****************
*categopri_ci  * 
**************** 
gen categopri_ci=.
replace categopri_ci=1 if o23==1
replace categopri_ci=2 if o23==2
replace categopri_ci=3 if o23>=3 & o23<=7
replace categopri_ci=3 if o23==9
replace categopri_ci=4 if o23==8
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional en la actividad principal"

****************
*categosec_ci  * 
****************
gen categosec_ci=.
* MLO: no hay informacion para construir esta variable
label variable categosec_ci "Categoria ocupacional trabajo secundario"
/*
****************
* contrato_ci  * 
**************** 
gen contrato_ci=(o25==1|o25==2)
replace contrato_ci=. if emp_ci!=1
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

****************
* segsoc_ci    * 
**************** 
*Se toma afilición a pensiones
gen segsoc_ci=(o31==1)
replace segsoc_ci=. if emp_ci!=1
label var segsoc_ci "Personas que tienen seguro social"*/

****************
* nempleos_ci  * 
**************** 
gen nempleos_ci=1 if o30==2 
replace nempleos_ci=2 if o30==1
label var nempleos_ci "Número de empleos" 
label def nempleos_ci 1"Un empleo" 2"Más de un empleo"
label val nempleos_ci nempleos_ci
/*
****************
* firmapeq_ci  * 
**************** 
gen firmapeq_ci=1 if o14=="A" | o14=="B"
replace firmapeq_ci=0 if o14=="C" | o14=="D" | o14=="E" | o14=="F"
replace firmapeq_ci=. if o14=="X" | emp_ci!=1
label var firmapeq_ci "Trabajadores informales"
label def firmapeq_ci 1"5 o menos trabajadores" 0"Más de 5 trabajadores"
label val firmapeq_ci firmapeq_ci*/

****************
* spublico_ci  * 
**************** 
gen spublico_ci=(o23==3 | o23==4 | o23==9)
replace spublico_ci=. if emp_ci!=1
label var spublico_ci "Personas que trabajan en el sector público"

		********************************
		* VARIABLES DE DEMANDA LABORAL *
		********************************

****************
* ocupa_ci     * 
****************
* Utiliza CIUO-88 (MGD 6/16/17)
gen ocupa_ci=.
replace ocupa_ci=1 if (c_o12>=2100 & c_o12<=3480) & emp_ci==1
replace ocupa_ci=2 if (c_o12>=1100 & c_o12<=1319) & emp_ci==1
replace ocupa_ci=3 if (c_o12>=4100 & c_o12<=4223) & emp_ci==1
replace ocupa_ci=4 if ((c_o12>=9100 & c_o12<=9113) | (c_o12>=5200 & c_o12<=5230)) & emp_ci==1
replace ocupa_ci=5 if ((c_o12>=5100 & c_o12<=5169) | (c_o12>=9100 & c_o12<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((c_o12>=6100 & c_o12<=6210) | (c_o12>=9200 & c_o12<=9220)) & emp_ci==1
replace ocupa_ci=7 if ((c_o12>=7100 & c_o12<=8340) | (c_o12>=9300 & c_o12<=9333))  & emp_ci==1
replace ocupa_ci=8 if c_o12==110 & emp_ci==1
replace ocupa_ci=9 if c_o12==9999

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras", add
label value ocupa_ci ocupa_ci

****************
* rama_ci      * 
****************
/*
gen rama_ci=rama
replace rama_ci=. if emp_ci!=1 | rama_ci==0
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
*/

gen rama_ci=.
replace rama_ci=1 if (c_o13>=1000 & c_o13<=1499) & emp_ci==1
replace rama_ci=2 if (c_o13>=2000 & c_o13<=2999) & emp_ci==1
replace rama_ci=3 if (c_o13>=3000 & c_o13<=3999) & emp_ci==1
replace rama_ci=4 if (c_o13>=4000 & c_o13<=4999) & emp_ci==1
replace rama_ci=5 if (c_o13>=5000 & c_o13<=5999) & emp_ci==1
replace rama_ci=6 if (c_o13>=6000 & c_o13<=6999) & emp_ci==1
replace rama_ci=7 if (c_o13>=7000 & c_o13<=7999) & emp_ci==1
replace rama_ci=8 if (c_o13>=8000 & c_o13<=8999) & emp_ci==1
replace rama_ci=9 if (c_o13>=9000 & c_o13<=9990) & emp_ci==1




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
gen ylmsec_ci=.
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
*No es muy clara esta construcción.  No obstante, para mantener consistencia temporal se conserva la 
*construcción de la variable

* 2014, 01 Agregado MLO, no se estaba restando el ingreso laboral correctamente cueando la variable era missing
* del ingreso autonomo se resta el ingreso laboral y las transferencias del estado
gen negylm=-ylm_ci
replace negylm=0 if ylm_ci==.

egen ynlm_ci = rsum(yautaj negylm ysubaj), missing
*gen ynlm_ci = yautaj - ylm_ci + ysubaj
replace ynlm_ci=. if yautaj==. & ylm_ci==. & ysubaj==. 
label var ynlm_ci "Ingreso no laboral monetario"  

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
gen durades_ci=o7/4.3 if condocup_ci==2
replace durades_ci=. if o7==999 /*| activ!=2*/ & condocup_ci==2
label var durades_ci "Duración del desempleo"

****************
* antiguedad_ci* 
**************** 
gen antiguedad_ci=(2009-o20)+1
replace antiguedad_ci=. if o20==9999 | emp_ci!=1
label var antiguedad_ci "Antiguedad en la actividad actual"


		************************
		* VARIABLES EDUCATIVAS *
		************************

***************
***asiste_ci*** 
*************** 
gen asiste_ci = (e3 == 1)
replace asiste_ci = . if e3 == .
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci*** 
************* 
replace e7t = . if e7t == 99
replace e7c = . if e7c == 99

gen aedu_ci=.
replace aedu_ci = 0 if inlist(e7t, 1, 16) // Prescolar o Parvularia o Ninguno

* Para no asistentes:
replace aedu_ci = e7c if e7t == 2 & asiste_ci != 1 // Preparatoria (Sistema Antiguo)
replace aedu_ci = e7c if e7t == 3 & asiste_ci != 1 // Básica
replace aedu_ci = e7c + 6 if e7t == 5 & asiste_ci != 1 // Humanidades (Sistema Antiguo)
replace aedu_ci = e7c + 8 if e7t == 6 & asiste_ci != 1 // Media Científico-Humanística
replace aedu_ci = e7c + 6 if e7t == 7 & asiste_ci != 1 // Técnica, Comercial, Industrial o Normalista (Sistema Antiguo)
replace aedu_ci = e7c + 8 if e7t == 8 & asiste_ci != 1 // Media Técnica Profesional
replace aedu_ci = e7c + 12 if (e7t >= 9 & e7t <= 14) & asiste_ci != 1 // Superior
replace aedu_ci = e7c + 17 if e7t == 15 & asiste_ci != 1 // Postgrado

* Para asistentes:
replace aedu_ci = e7c - 1 if e7t == 2 & asiste_ci == 1 // Preparatoria (Sistema Antiguo)
replace aedu_ci = e7c - 1 if e7t == 3 & asiste_ci == 1 // Básica
replace aedu_ci = e7c + 6 - 1 if e7t == 5 & asiste_ci == 1 // Humanidades (Sistema Antiguo)
replace aedu_ci = e7c + 8 - 1 if e7t == 6 & asiste_ci == 1 // Media Científico-Humanística
replace aedu_ci = e7c + 6 - 1 if e7t == 7 & asiste_ci == 1 // Técnica, Comercial, Industrial o Normalista (Sistema Antiguo)
replace aedu_ci = e7c + 8 - 1 if e7t == 8 & asiste_ci == 1 // Media Técnica Profesional
replace aedu_ci = e7c + 12 - 1 if (e7t >= 9 & e7t <= 14) & asiste_ci == 1 // Superior
replace aedu_ci = e7c + 17 - 1 if e7t == 15 & asiste_ci == 1 // Postgrado
label var aedu_ci "Anios de educacion aprobados" 

**************
***eduno_ci***
**************
gen byte eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == .
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci = (aedu_ci > 0 & aedu_ci < 6)
replace edupi_ci = . if aedu_ci == .
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************
gen byte edupc_ci = (aedu_ci == 6)
replace edupc_ci = . if aedu_ci == .
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci = (aedu_ci > 6 & aedu_ci < 12)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
/* 
Se considera con edusc tambien a aquellos que 
reporten educacion media tecnica con 13 anios
de educación ya que no es un terciario.
*/
gen byte edusc_ci = (aedu_ci == 12) | (aedu_ci == 13 & e7t == 8)
replace edusc_ci = . if aedu_ci == .
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
/*
En esta encuesta puede discriminarse entre terciario y universitario completo
o incompleto. 
*/

gen byte eduui_ci = (aedu_ci > 12 & inlist(e7t, 9, 11, 13)) //tecnica, profesional o universitaria incompleta.
replace eduui_ci = . if aedu_ci == .
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************
gen byte eduuc_ci = (aedu_ci > 12 & inlist(e7t, 10, 12, 14, 15)) //tecnica, profesional o universitaria completa o postgrado.
replace eduuc_ci = . if aedu_ci == .
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
gen edus1i_ci = (aedu_ci > 6 & aedu_ci < 8) 
replace edus1i_ci = . if aedu_ci == .
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci = (aedu_ci == 8) 
replace edus1c_ci = . if aedu_ci == . 
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci = (aedu_ci > 8 & aedu_ci < 12)
replace edus2i_ci = . if aedu_ci == .
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci = (aedu_ci == 12) | (aedu_ci == 13 & e7t == 8)
replace edus2c_ci = .  if aedu_ci == .
label variable edus2c_ci "2do ciclo de la secundaria completo"

***************
***edupre_ci***
***************
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
*Creación de la variable asistencia a preescolar por Iván Bornacelly - 01/12/17
g asispre_ci=.
replace asispre_ci = 1 if (e3 == 1 & e7t == 1)
la var asispre_ci "Asiste a educacion prescolar"

**************
***eduac_ci***
**************
gen eduac_ci = (e7t >= 11 & e7t <= 15) // Superior universitario
replace eduac_ci = 0 if inlist(e7t, 9, 10) // Formacion técnica
label variable eduac_ci "Superior universitario vs superior no universitario"

****************
**pqnoasis_ci***
****************
*Modificado Mayra Sáenz Junio, 2016: antes se generaba como missing, la e5 es para personas de 7 a 40 años
* y la variable e4 es para niños de 0 a 6 años.

gen pqnoasis_ci = e5
label var pqnoasis_ci "Razones para no asistir a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e5 == 3
replace pqnoasis1_ci = 2 if e5 == 4
replace pqnoasis1_ci = 3 if e5 == 6 | e5 == 10 | e5 == 13 | e5 == 14
replace pqnoasis1_ci = 4 if e5 == 9
replace pqnoasis1_ci = 5 if e5 == 5 | e5 == 7 | e5 == 8
replace pqnoasis1_ci = 6 if e5 == 17
replace pqnoasis1_ci = 7 if e5 == 18
replace pqnoasis1_ci = 8 if e5 == 1  | e5 == 2  
replace pqnoasis1_ci = 9 if e5 == 11 | e5 == 12 | e5 == 15 | e5 == 16 | e5 == 19 | e5 == 20

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
replace edupub_ci=1 if inlist(e9, 1) & asiste_ci==1 //Municipales
replace edupub_ci=0 if inlist(e9, 2, 4) & asiste_ci==1 // Particular pagado, particular subvencionado
label var edupub_ci "Personas que asisten a centros de enseñanza públicos"



		******************************************
		* VARIABLES DE INFRAESTRUCTURA DEL HOGAR *
		******************************************

***************
* aguared_ch  *
***************
gen aguared_ch=(v8==1 | v8==2 | v8==3)
replace aguared_ch=. if v8==.
label var aguared_ch "Acceso a fuente de agua por red"

***************
* aguadist_ch *
***************
*Es la mejor aproximación
gen aguadist_ch=v9
replace aguadist_ch=. if v9==9
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la vivienda" 2"Afuera de la vivienda" 3"La acarrean"
label val aguadist_ch aguadist_ch

***************
* aguamala_ch *
***************
gen aguamala_ch=(v8>=5 & v8<=6)
replace aguamala_ch=. if v8==.
label var aguamala_ch "La principal fuente de agua es unimproved según MDG"

***************
* aguamide_ch *
***************
gen aguamide_ch=(v8==1 |v8==2)
replace aguamide_ch=. if aguared_ch==.
label var aguamide_ch "El hogar usa un medidor para pagar por su consumo de agua"

***************
* luz_ch      *
***************
gen luz_ch=(v11<=6)
label var luz_ch "La principal fuente de iluminación es electricidad"

***************
* luzmide_ch  *
***************
gen luzmide_ch=(v11==1 | v11==2)
replace luzmide_ch=. if luz_ch==0
label var luzmide_ch "El hogar usa un medidor para pagar el consumo de electricidad"

***************
* combust_ch  *
***************
gen combust_ch=.
label var combust_ch "El combustible pricipal usado en el hogar es gas o electricidad"

***************
* bano_ch     *
***************
gen bano_ch=(v10<=6)
replace bano_ch=. if v10==.
label var bano_ch "El hogar tiene algún tipo de servicio higiénico"

***************
* banoex_ch   *
***************
gen banoex_ch=.
label var banoex_ch "El servicio higiénico es de uso exclusivo del hogar"

***************
* des1_ch     *
***************
gen des1_ch=0 if bano_ch==0 | v10==7
replace des1_ch=1 if v10==1 | v10==2
replace des1_ch=2 if v10==3 | v10==4
replace des1_ch=3 if v10==5 | v10==6
replace des1_ch=. if v10==.
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

***************
* des2_ch     *
***************
gen des2_ch=0 if bano_ch==0 | v10==7
replace des2_ch=1 if v10>=1 & v10<=4
replace des2_ch=2 if v10==5 | v10==6
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

***************
* piso_ch     *
***************
gen piso_ch=0 if v13a==5
replace piso_ch=1 if v13a<5
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

***************
* pared_ch    *
***************
gen pared_ch=0 if v12a>=4 & v12a<=7
replace pared_ch=1 if v12a<4
replace pared_ch=2 if v12a==8
replace pared_ch=. if v12a==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch

***************
* techo_ch    *
***************
gen techo_ch=0 if v14a>=4 & v14a<=6
replace techo_ch=1 if v14a<4
replace techo_ch=. if v14a==.
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes"
label val techo_ch techo_ch

***************
* resid_ch    *
***************
gen resid_ch=.
label var resid_ch "Método de eliminación de residuos"

 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v8 >=1 & v8 <=4)
replace aguamejorada_ch = 0 if (v8 >=5 & v8 <=6) | v9 ==3

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v10 >=1 & v10 <=4) 
replace banomejorado_ch = 0 if  (v10 >=5 & v10 <=7)


***************
* dorm_ch     *
***************
gen dorm_ch=v7a 
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

***************
* cuartos_ch  *
***************
*Esta construcción es la mejor aproximación y mantiene consistencia en el tiempo. No obstante, podría estarse
*sumando doble vez algunas habitaciones que son de uso múltiple. Además, se usa la información de la vivienda y
*no la del hogar porque está está incompleta!
egen cuartos_ch=rsum(v7a v7b v7c v7d v7e v7f v7g), missing
replace cuartos_ch=. if v7a==. & v7b==. & v7c==. & v7d==. & v7e==. & v7f==. & v7g==.
label var cuartos_ch "Cantidad de habitaciones en el hogar"

***************
* cocina_ch  *
***************
gen cocina_ch=(v7f==1| v7f==2)
replace cocina_ch=. if v7f==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar" 

***************
* telef_ch    *
***************
bysort idh_ch: egen telef_ch=sum(r11d==1)
replace telef_ch=1 if telef_ch>=1
replace telef_ch=. if r11d==9
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
* refrig_ch   *
***************
bysort idh_ch: egen refrig_ch=sum(r11b==1)
replace refrig_ch=1 if refrig_ch>=1
replace refrig_ch=. if r11b==9
label var refrig_ch "El hogar posee heladera o refrigerador"

***************
* freez_ch    *
***************
gen freez_ch=.
label var freez_ch "El hogar posee congelador"

***************
* auto_ch     * 
***************
bysort idh_ch: egen auto_ch=sum(r10a>=1 & r10a<9)
replace auto_ch=1 if auto_ch>=1
replace auto_ch=. if r10a==9
label var auto_ch "El hogar posee automovil particular"

***************
* compu_ch    * 
***************
bysort idh_ch: egen compu_ch=sum(r12>=1 & r12!=.)
replace compu_ch=1 if compu_ch>=1
label var compu_ch "El hogar posee computadora"

***************
* internet_ch * 
***************
bysort idh_ch: egen internet_ch=sum(r13a>=1 & r13a<=4)
replace internet_ch=1 if internet_ch>=1
label var internet_ch "El hogar tiene conexión a Internet"

*************
* cel_ch    * 
*************
bysort idh_ch: egen cel_ch=sum(r14==1 | r14==2)
replace cel_ch=1 if cel_ch>=1
label var cel_ch "El hogar tiene servicio telefónico celular"

***************
* vivi1_ch    * 
*************** 
gen vivi1_ch=1 if v17==1 | v17==2
replace vivi1_ch=2 if v17==3
replace vivi1_ch=3 if v17>3 & v17!=.
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
gen viviprop_ch=0 if v18==5 | v18==6
replace viviprop_ch=1 if v18==1 | v18==3
replace viviprop_ch=2 if v18==2 | v18==4
replace viviprop_ch=3 if v18>=7 & v18<=10
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
gen vivialq_ch=. 
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

gen lp_ci =.
replace lp_ci= 64134  if zona_c==1  /*urbana*/
replace lp_ci= 43242  if zona_c==0	/*rural*/

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 32067    if zona_c==1  /*urbana*/
replace lpe_ci= 24710    if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if o32>=1 & o32<=6
recode cotizando_ci .=0 if (activ==1 | activ==2)
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if o31==1
recode afiliado_ci .=0 
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************

gen tipopen_ci=.

replace tipopen_ci = 1 if (yjubaj > 0 & yjubaj != .)
replace tipopen_ci = 1 if (yvitaj > 0 & yvitaj != .)
replace tipopen_ci = 2 if (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 3 if (ymonaj > 0 & ymonaj != .)
replace tipopen_ci = 4 if (yorfaj > 0 & yorfaj != .)

replace tipopen_ci = 12 if (yinvaj > 0 & yinvaj != .) | (yjubaj > 0 & yjubaj != .)
replace tipopen_ci = 13 if (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .)
replace tipopen_ci = 14 if (yinvaj > 0 & yinvaj != .) | (yorfaj > 0 & yorfaj != .)

replace tipopen_ci = 23 if (ymonaj > 0 & ymonaj != .) | (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 23 if (yvitaj > 0 & yvitaj != .) | (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 24 if (yorfaj > 0 & yorfaj != .) | (yinvaj > 0 & yinvaj != .)

replace tipopen_ci = 123 if (yjubaj > 0 & yjubaj != .) | (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .)

replace tipopen_ci = 1234 if (yjubaj > 0 & yjubaj != .) | (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .) | (yorfaj > 0 & yorfaj != .)


label define  t 1 "Jubilacion" 2 "Pension invalidez" 3 "Pension viudez" 12 " Jub y inv" 13 "Jub y viud" 14 "viudez y orfandad" 23 "Viud e inv" 24 "orfandad y inv"  123 "Todas"
label value tipopen_ci t

label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.

label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=o32 if o32<=5
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o26==1 & categopri_ci==3
replace tipocontrato_ci=2 if (o26==2 | o26==3 ) & categopri_ci==3
replace tipocontrato_ci=3 if o25==3 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
 * Corregido por la variable de firmo o no firmo MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if ((o25==1 | o25==2) & o26==1) & categopri_ci==3
replace tipocontrato_ci=2 if ((o25==1 | o25==2) & (o26>=2 & o26<=5)) & categopri_ci==3
replace tipocontrato_ci=3 if (o25>=3 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************
gen cesante_ci=1 if o8==1
replace cesante_ci=0 if o8==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


**************
***tamemp_ci**
**************

gen tamemp_ci=1 if o14=="A" | o14=="B"
replace tamemp_ci=2 if o14=="C" | o14=="D"
replace tamemp_ci=3 if o14=="E" | o14=="F"

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci


*************
**pension_ci*
*************
egen auxpen=rsum(yjubaj yvitaj yinvaj ymonaj yorfaj), missing
*gen pension_ci=1 if auxpen>0
*Modificación Mayra Sáenz - Septiembre 2014
gen pension_ci=1 if auxpen>0  & auxpen!=.
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

egen auxpens=rsum(ypensaj ypresaj), missing
destring auxpens, replace

gen pensionsub_ci=1 if auxpens>0 & auxpens!=.
recode pensionsub_ci .=0 
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen  ypensub_ci=auxpens
replace ypensub_ci=. if auxpens<0
drop auxpens
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=501.4427273
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* CHL 2009
gen salmm_ci= 	165000
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************


gen tecnica_ci=.
replace tecnica_ci=1 if e7t==9 | e7t==10
recode tecnica_ci .=0
label var tecnica_ci "1=formacion terciaria tecnica"

****************
**categoinac_ci*
****************

gen categoinac_ci=1 if o6==18
replace categoinac_ci=2 if o6==17
replace categoinac_ci=3 if o6==7
replace categoinac_ci=4 if o6==1 | o6==2 | o6==3 | o6==4 | o6==5 | o6==6 | o6==8 | o6==9 | o6==10| o6==11| o6==12 | o6==13 | o6==14 | o6==15 | o6==16 | o6==19 | o6==20 | o6==21

label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	

***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

* variables que faltan crear

gen tcylmpri_ci =.
gen tcylmpri_ch =.


******************************
*** VARIABLES DE MIGRACION ***
******************************

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	
	
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
	
	
	**************************
	** PROVINCIAS ************
	**************************
		
   gen ine02=.   
   replace ine02=11 if provincia==11			/*Iquique*/
   replace ine02=14 if provincia==14			/*Tamarugal*/
   replace ine02=21 if provincia==21			/*Antofagasta*/
   replace ine02=22 if provincia==22		    /*El Loa*/
   replace ine02=23 if provincia==23			/*Tocopilla*/
   replace ine02=31 if provincia==31			/*Copiapó*/
   replace ine02=32 if provincia==32			/*Chañaral*/
   replace ine02=33 if provincia==33			/*Huasco*/
   replace ine02=41 if provincia==41			/*Elqui*/
   replace ine02=42 if provincia==42			/*Choapa*/
   replace ine02=43 if provincia==43			/*Limarí*/
   replace ine02=51 if provincia==51			/*Valparaíso*/
   replace ine02=53 if provincia==53	    	/*Los Andes*/
   replace ine02=54 if provincia==54			/*Petorca*/
   replace ine02=55 if provincia==55			/*Quillota*/
   replace ine02=56 if provincia==56			/*San Antonio*/
   replace ine02=57 if provincia==57			/*San Felipe*/   
   replace ine02=61 if provincia==61			/*Cachapoal*/
   replace ine02=62 if provincia==62			/*Cardenal Caro*/
   replace ine02=63 if provincia==63			/*Colchagua*/
   replace ine02=71 if provincia==71			/*Talca*/
   replace ine02=72 if provincia==72			/*Cauquenes*/
   replace ine02=73 if provincia==73			/*Curicó*/
   replace ine02=74 if provincia==74	    	/*Linares*/
   replace ine02=81 if provincia==81			/*Concepción*/
   replace ine02=82 if provincia==82			/*Arauco*/
   replace ine02=83 if provincia==83			/*Bio Bío*/
   replace ine02=84 if provincia==84			/*Ñuble*/
   replace ine02=91 if provincia==91			/*Cautín*/
   replace ine02=92 if provincia==92			/*Malleco*/
   replace ine02=101 if provincia==101			/*Llanquihue*/
   replace ine02=102 if provincia==102			/*Chiloé*/
   replace ine02=103 if provincia==103			/*Osorno*/
   replace ine02=104 if provincia==104			/*Palena*/
   replace ine02=111 if provincia==111			/*Cohaique*/
   replace ine02=112 if provincia==112	    	/*Aysén*/
   replace ine02=113 if provincia==113			/*Capitán Prat*/
   replace ine02=114 if provincia==114			/*General Carrera*/
   replace ine02=121 if provincia==121			/*Magallanes*/
   replace ine02=122 if provincia==122			/*Antártica*/
   replace ine02=123 if provincia==123			/*Tierra del Fuego*/
   replace ine02=124 if provincia==124			/*Última Esperanza*/
   replace ine02=131 if provincia==131			/*Santiago*/
   replace ine02=132 if provincia==132			/*Cordillera*/
   replace ine02=133 if provincia==133			/*Chacabuco*/
   replace ine02=134 if provincia==134			/*Maipo*/
   replace ine02=135 if provincia==135			/*Melipilla*/
   replace ine02=136 if provincia==136			/*Talagante*/
   replace ine02=141 if provincia==141			/*Valdivia*/
   replace ine02=142 if provincia==142			/*Ranco*/
   replace ine02=151 if provincia==151			/*Arica*/
   replace ine02=152 if provincia==152			/*Parinatoca*/

	label define ine02 11"Iquique" 14"Tamarugal" 21"Antofagasta" 22"El Loa" 23"Tocopilla" 31"Copiapó" 32"Chañaral" 33"Huasco" 41"Elqui" 42"Choapa" 43"Limarí" 51"Valparaíso" 53"Los Andes" 54"Petorca" 55"Quillota" 56"San Antonio" 57"San Felipe" 61"Cachapoal" 62"Cardenal Caro" 63"Colchagua" 71"Talca" 72"Cauquenes" 73"Curicó" 74"Linares" 81"Concepción" 82"Arauco" 83"Bio Bío" 84"Ñuble" 91"Cautín" 92"Malleco" 101"Llanquihue" 102"Chiloé" 103"Osorno" 104"Palena" 111"Cohaique" 112"Aisén" 113"Capitán Prat" 114"General Carrera" 121"Magallanes" 122"Antártica" 123"Tierra del Fuego" 124"Última Esperanza" 131"Santiago" 132"Cordillera" 133"Chacabuco" 134"Maipo" 135"Melipilla" 136"Talagante" 141"Valdivia" 142"Ranco" 151"Arica" 152"Parinatoca"
	label value ine02 ine02
	label var ine02 " Segunda division politico-administrativa, Provincia"


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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

rename c_o13 codindustria
rename c_o12 codocupa
rename cviv calgobviv
compress


saveold "`base_out'", replace


log close


