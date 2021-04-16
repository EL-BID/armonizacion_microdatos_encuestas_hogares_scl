
* (Versión Stata 13)
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

local PAIS COL
local ENCUESTA ECH
local ANO "2005"
local ronda t3 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Colombia
Encuesta: ECH
Round: t3
Autores: Yanira Oviedo
Generación nuevas variables LMK: Andres Felipe Sanchez, Laura Oliveri, Yessenia Loayza
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: noviembre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

***************
***region_c ***
***************
gen region_c=DPTO
label define region_c       /// 
	5  "Antioquia"	        ///
	8  "Atlántico"	        ///
	11 "Bogotá, D.C"	    ///
	13 "Bolívar" 	        ///
	15 "Boyacá"	            ///
	17 "Caldas"	            ///
	18 "Caquetá"	        ///
	19 "Cauca"	            ///
	20 "Cesár"	            ///
	23 "Córdoba"	        ///
	25 "Cundinamarca"       ///
	27 "Chocó"	            ///
	41 "Huila"	            ///
	44 "La Guajira"	        ///
	47 "Magdalena"	        ///
	50 "Meta"	            ///
	52 "Nariño"	            ///
	54 "Norte de Santander"	///
	63 "Quindío"	        ///
	66 "Risaralda"	        ///
	68 "Santander"	        ///
	70 "Sucre"	            ///
	73 "Tolima"	            ///
	76 "Valle"	
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************
* Region_BID *
************
gen region_BID_c=3
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*
***************
***factor_ch***
***************
gen factor_ch=FACTOR
label variable factor_ch "Factor de expansion del hogar"
*
***************
****idh_ch*****
***************
egen idh_ch=concat(LLAVE_H CABECERA)
label variable idh_ch "ID del hogar"
*
*************
****idp_ci****
**************
*
gen idp_ci=ORDEN
label variable idp_ci "ID de la persona en el hogar"
*
**********
***zona***
**********
*
gen byte zona_c=0 if CABECERA==0
replace zona_c=1 if CABECERA==1
*
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c
*
************
****pais****
************

gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2005
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=MES

label variable mes_c "Mes de la encuesta"
label define mes_c 7"Julio" 8"Agosto" 9"Septiembre"
label value mes_c mes_c

***************
***factor_ci***
***************

gen factor_ci=FACTOR
label variable factor_ci "Factor de expansion del individuo"


		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************


*****************
***relacion_ci***
*****************

gen relacion_ci=1 if C3==1
replace relacion_ci=2 if C3==2
replace relacion_ci=3 if C3==3
replace relacion_ci=4 if C3>=4 & C3<=9
replace relacion_ci=5 if C3==10 | C3==11 | C3==13 | C3==14 | C3==15
replace relacion_ci=6 if C3==12

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

**********
***sexo***
**********
gen sexo_ci=C4
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=C5
label variable edad_ci "Edad del individuo"

*************
** raza_ci **
*************
gen raza_ci=.

*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if C6==5
replace civil_ci=2 if C6==1 | C6==2 
replace civil_ci=3 if C6==4 
replace civil_ci=4 if C6==3

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

by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
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
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
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
bys idh_ch: egen nmayor21_ch = sum((relacion_ci >= 1 & relacion_ci <= 4) & edad_ci >= 21 & edad_ci!=.)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************
bys idh_ch: egen nmayor65_ch = sum((relacion_ci >= 1 & relacion_ci <= 4) & edad_ci >= 65 & edad_ci!=.)
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
* 2014, 01, MLO modificado segun documento metodologico (los huespedes quedan fuera del hogar)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"


		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************

****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if  F12 == 1  | F13==1 | F14==1 | F15==1
replace condocup_ci=2 if (F16==1 | (F16==2 & F17==1 & F18==1)) & F23==1
recode condocup_ci .=3 
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

*********
*lp25_ci
*********
gen lp25_ci = 90622.06

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci =144995.3

label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 161841 if CABECERA==1 /*cabecera*/
replace lp_ci= 95965 if CABECERA==2  /*resto*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 65154 if CABECERA==1 /*cabecera*/
replace lpe_ci= 53285 if CABECERA==2  /*resto*/
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
* COL 2005
gen salmm_ci= 	12716.6666666667*22
label var salmm_ci "Salario minimo legal"

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "1 Cotizante a la Seguridad Social"

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
label define instpen_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, Ecopetrol etc)" 4 "Fondo Subsidiado (Prosperar,etc.)" 
label value instpen_ci instpen_ci

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
gen cesante_ci=1 if D53==2
replace cesante_ci=0 if D53==1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*************
*tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"
* Y.L. -> No se puede distinguir superior universitaria de no universitaria

****************
*categoinac_ci**
***************
gen categoinac_ci=. 
label var categoinac_ci "Condición de inactividad"
label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos"
label value categoinac_ci categoinac_ci
/*Y.L. en la ECH No se puede clasificar a todos los inactivos.
Ademas, la pregunta F18/p18 no considera la categoria jubilado. */

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
***formal_ci***
*************
gen formal_ci=(cotizando_ci==1)


*****************
***desalent_ci***
*****************
gen desalent_ci=(F18==5)
replace desalent_ci=. if F18==.
label var desalent_ci "Trabajadores desalentados"


***************
***subemp_ci***
***************

*Horas semanales trabajadas normalmente*
gen promhora= O34 if emp_ci==1 
replace promhora=. if O34==998 | O34==999

*Horas adicionales en la semana pasada*
*gen hora_ad= O36B
*replace hora_ad=. if O36B==99
*replace hora_ad=. if O36A==2

*Horas semanales trabajo secundario*
gen promhora1= O39
replace promhora1=. if O39==99

*Total horas
egen tothoras=rowtotal(promhora promhora1), m
replace tothoras=. if tothoras>=168

gen subemp_ci=0
replace subemp_ci=1 if tothoras<=30  & emp_ci==1 & O40==1
replace subemp_ci =. if emp_ci ==.
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=O34 
replace horaspri_ci=. if O34==998 | O34==999
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
gen tiempoparc_ci=(horastot_ci<30 & O40==2)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 


******************
***categopri_ci***
******************
*Y.L -> ha diferencias en esta pregunta entre cabecera y resto en el formulario
gen categopri_ci=.
*cabecera
replace categopri_ci=1 if O27==5
replace categopri_ci=2 if O27==4 
replace categopri_ci=3 if O27==1 | O27==2 |O27==3 
replace categopri_ci=4 if O27==6 
replace categopri_ci=0 if O27==7 | O27==8
replace categopri_ci=. if emp_ci==0

*resto
gen temp=.
replace temp=1 if O27==6
replace temp=2 if O27==5 
replace temp=3 if O27==1 | O27==2 |O27==3 
replace temp=4 if O27==7 
replace temp=0 if O27==4| O27==8
replace temp=. if emp_ci==0
replace categopri_ci=temp if CABECERA==0
drop temp
*Y.L -> coloco la categoria 4 en otros para hacerlo comprarable a la serie.
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"


******************
***categosec_ci***
******************
gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"
/*No se puede estimar*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & O37==2
replace nempleos_ci=2 if emp_ci==1 & O37==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 


*****************
***spublico_ci***
*****************

gen spublico_ci=(O27==2) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************
gen ocupa_ci=.
replace ocupa_ci=1 if (O24>=1 & O24<=19) & emp_ci==1  
replace ocupa_ci=2 if (O24>=20 & O24<=21) & emp_ci==1
replace ocupa_ci=3 if (O24>=30 & O24<=39) & emp_ci==1
replace ocupa_ci=4 if (O24>=40 & O24<=49) & emp_ci==1
replace ocupa_ci=5 if (O24>=50 & O24<=59) & emp_ci==1
replace ocupa_ci=6 if (O24>=60 & O24<=64) & emp_ci==1
replace ocupa_ci=7 if (O24>=70 & O24<=98) & emp_ci==1
replace ocupa_ci=9 if (O24==0 | O24==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (O26>=1 & O26<=5) & emp_ci==1   
replace rama_ci=2 if (O26>=10 & O26<=14) & emp_ci==1
replace rama_ci=3 if (O26>=15 & O26<=37) & emp_ci==1
replace rama_ci=4 if (O26>=40 & O26<=41) & emp_ci==1
replace rama_ci=5 if (O26==45) & emp_ci==1
replace rama_ci=6 if (O26>=50 & O26<=55) & emp_ci==1
replace rama_ci=7 if (O26>=60 & O26<=64) & emp_ci==1
replace rama_ci=8 if (O26>=65 & O26<=71) & emp_ci==1
replace rama_ci=9 if (O26>=72 & O26<=99) & emp_ci==1
replace rama_ci=. if emp_ci==0

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


****************
***durades_ci***
****************
gen durades_ci= D49/4.2
replace durades_ci=. if D49==998 | D49==999
label variable durades_ci "Duracion del desempleo en meses"
 
*******************
***antiguedad_ci***
*******************
gen antiguedad_ci= . 
label var antiguedad_ci "Antiguedad en la actividad actual en anios"


			**************
			***INGRESOS***
			**************

gen ymensual 	= O28 
replace ymensual=. if O28==98 | O28==99

gen yhorasext 	= .

gen yvivienda 	= O30_VALOR 
replace yvivienda=. if O30_VALOR==98 | O30==2

gen yalimen 	= O29_VALOR
replace yalimen =. if O29_VALOR==98 | O29==2

gen ytrans 	= .

gen yespecie 	= .

gen ynetoind 	= O31
replace ynetoind=. if O31==98 | O31==99

gen ysecund	= O38
replace ysecund=. if O38==98 | O38==99

gen yarrien	= O32A
replace yarrien=. if O32A==98 | O32A==99

gen ypension	= O32B
replace ypension=. if O32B==98 | O32B==99

gen yayudafam	= O33A/12
replace yayudafam=. if O33A==98 | O33A==99 

gen yintereses	= O33B/12
replace yintereses =. if O33B==98 | O33B==99

gen yotros= O33C/12
replace yotros=. if O33C==98 | O33C==99



*Ingresos desocupados
gen ytrabajo_des=D58A
replace ytrabajo_des=. if D58A==98 | D58A==99

gen yarrien_des =D58B
replace yarrien_des=. if D58B==98 | D58B==99

gen ypension_des= D58C
replace ypension_des =. if D58C==98 | D58C==99

gen yayudafam_des = D59A/12
replace yayudafam_des=. if D59A==98 | D59A==99 

gen yintereses_des	= D59B/12
replace yintereses_des =. if D59B==98 | D59B==99

gen yotros_des= D59C/12
replace yotros_des=. if D59C==98 | D59C==99



*Ingresos inactivos
gen ytrabajo_ina=I65A
replace ytrabajo_ina=. if I65A==98 | I65A==99

gen yarrien_ina=I65B
replace yarrien_ina=. if I65B==98 | I65B==99

gen ypension_ina= I65C
replace ypension_ina=. if I65C==98 | I65C==99

gen yayudafam_ina= I66A/12
replace yayudafam_ina=. if I66A==98 | I66A==99 

gen yintereses_ina	= I66B/12
replace yintereses_ina=. if I66B==98 | I66B==99

gen yotros_ina= I66C/12
replace yotros_ina=. if I66C==98 | I66C==99


gen yremesas	= .



***************
***ylmpri_ci***
***************

egen ylmpri_ci=rsum(ymensual ynetoind)
replace ylmpri_ci=. if ymensual == . & ynetoind==.  
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

egen ylnmpri_ci=rsum(yvivienda yalimen ytrans yespecie)
replace ylnmpri_ci=. if yvivienda==. & yalimen==. & ytrans==. & yespecie==. 
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

gen ylmsec_ci=ysecund if emp_ci==1 
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

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(yarrien ypension yayudafam yintereses yotros ytrabajo_des yarrien_des ypension_des yayudafam_des yintereses_des yotros_des ytrabajo_ina yarrien_ina ypension_ina yayudafam_ina yintereses_ina yotros_ina)
replace ynlm_ci=. if yarrien==. & ypension==. & yayudafam==. & yintereses==. & yotros==. & ytrabajo_des==. & yarrien_des==. & ypension_des==. & yayudafam_des==. & yintereses_des==. & yotros_des==. & ytrabajo_ina==. & yarrien_ina==. & ypension_ina==. & yayudafam_ina==. & yintereses_ina==. & yotros_ina==. 
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ylnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


*************
**pension_ci*
*************

*Y.L. -> genero pension nuevamente
egen aux_p=rsum( ypension ypension_des ypension_ina), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
*  ypen_ci  *
*************
*Y.L. -> genero ypen_ci nuevamente
egen ypen_ci=rowtotal( ypension ypension_des ypension_ina), missing
replace ypen_ci= . if  ypension==9999999999 | ypension_des==9999999999 | ypension_ina==9999999999
replace ypen_ci=. if pension_ci==0 |  pension_ci<1000
*gen ypen_ci= p7500s2a1
*replace ypen_ci= . if p7500s2a1==9999999999
*replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension contributiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************

*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del hogar"


**************
***ynlnm_ch***
**************

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



****************
***remesas_ci***
****************

*Aqui se toma el valor mensual de las remesas

gen remesas_ci=yremesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 


****************
***remesas_ch***
****************

*Aqui se toma el valor mensual de las remesas

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
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

*Generando el nivel educativo

gen NIVEL=0 	if C10==100
replace NIVEL=1	if C10==200 | C10==201 
replace NIVEL=2 if C10>=300 & C10<=305
replace NIVEL=3	if C10>=400 & C10<=409
replace NIVEL=3 if C10>=610 & C10<=613
replace NIVEL=4	if C10>=500 & C10<=515
replace NIVEL=.	if C10==999

label var NIVEL "NIVEL EDUCATIVO MÁS ALTO ALCANZADO"
label def NIVEL 0"Ninguno" 1"Preescolar" 2"Primaria" 3"Secundaria" 4"Superior"
label val NIVEL NIVEL


*************
***aedu_ci***
*************
*Yessenia Loayza (may 2014) -> corrigo variable aedu_ci
gen byte aedu_ci=.
*** Ninguno o preescolar
replace aedu_ci=0 if C10<=300
*** primaria 
replace aedu_ci=1 if C10==301 
replace aedu_ci=2 if C10==302 
replace aedu_ci=3 if C10==303 
replace aedu_ci=4 if C10==304 
replace aedu_ci=5 if C10==305 | C10==400
*** secundaria 
replace aedu_ci=6  if C10==406 
replace aedu_ci=7  if C10==407 
replace aedu_ci=8  if C10==408 
replace aedu_ci=9  if C10==409 
replace aedu_ci=10 if C10==610 
replace aedu_ci=11 if C10==611 | C10==500  
replace aedu_ci=12 if C10==612 /* Y.L -> 12 años de educacion secundaria "son muy pocos"*/
replace aedu_ci=13 if C10==613 /* Y.L -> 13 años de educacion secundaria "son muy pocos"*/

*** superior o universitario  *** 
replace aedu_ci=12 if C10==501
replace aedu_ci=13 if C10==502
replace aedu_ci=14 if C10==503
replace aedu_ci=15 if C10==504
replace aedu_ci=16 if C10==505
replace aedu_ci=17 if C10==506
replace aedu_ci=18 if C10==507
replace aedu_ci=19 if C10==508
replace aedu_ci=20 if C10==509
replace aedu_ci=21 if C10==510
replace aedu_ci=22 if C10==511
replace aedu_ci=23 if C10==512
replace aedu_ci=24 if C10==513
replace aedu_ci=25 if C10==514
replace aedu_ci=26 if C10==515
replace aedu_ci=.  if C10==999

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
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<5) 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==5 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************
gen byte edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=6 & aedu_ci<11) 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************
gen byte edusc_ci=0
replace edusc_ci=1 if (aedu_ci>=11 & aedu_ci<=13) & NIVEL==3 
label variable edusc_ci "Secundaria completa"


**************
***eduui_ci***
**************
*Y.L - > Para la educación superior no es posible saber cuantos anios dura el ciclo esta es una aprox.
gen byte eduui_ci=(aedu_ci>11 & aedu_ci<16)
label variable eduui_ci "Superior incompleto"

***************
***eduuc_ci***
***************
*Y.L. -> Para la educación superior no es posible saber cuantos anios dura el ciclo esta es una aprox.
gen byte eduuc_ci= (aedu_ci>=16 & aedu_ci!=.)
label variable eduuc_ci "Superior completo"


***************
***edus1i_ci***
***************
gen byte edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>=6 & aedu_ci<9)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci==10 
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if (aedu_ci>=11 & aedu_ci<=13) 
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************
gen byte edupre_ci=(C10==201)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if C8==1
replace asiste_ci=0 if C8==2
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis=.
label var pqnoasis "Razones para no asistir a la escuela"

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
replace edupub_ci=1 if C9==1
replace edupub_ci=. if C9==.
label var edupub_ci "Asiste a un centro de ensenanza público"


	**********************************
	**** VARIABLES DE LA VIVIENDA ****
	**********************************


****************
***aguared_ch***
****************
gen aguared_ch=(V4D==1)
replace aguared_ch=. if V4D==.
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
*gen aguamala_ch=(VA6==5 | VA6==6)
*Y.L. cambio categoria para hacerla comparable con GEIH
gen aguamala_ch=(VA6==5 | VA6==9)
replace aguamala_ch=. if VA6==.
label var aguamala_ch "Agua unimproved según MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************
gen luz_ch=0 if V4A!=.
replace luz_ch=1 if V4A==1 
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************
gen combust_ch=0 if VA7!=.
replace combust_ch=1 if  VA7==1 | VA7==3 | VA7==4
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************
gen bano_ch=1
replace bano_ch=0 if VA3==6
replace bano_ch=. if VA3==.
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************
gen banoex_ch=0 if VA4!=.
replace banoex_ch=1 if VA4==1
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************
gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if VA3==1 | VA3==2
replace des1_ch=2 if VA3==3 | VA3==4
replace des1_ch=3 if VA3==5
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if VA3==1 | VA3==2 | VA3==3 | VA3==4
replace des2_ch=3 if VA3==5
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************
gen piso_ch=1 if V3!=1 & V3!=.
replace piso_ch=0 if V3==1
replace piso_ch=. if V3==.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch=0 
replace pared_ch=1 if V2==1 | V2==2 | V2==3 | V2==5
replace pared_ch=. if V2==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch


**************
***techo_ch***
**************
gen techo_ch=.
label var techo_ch "Materiales de construcción del techo"

**************
***resid_ch***
**************
gen resid_ch =0    if VA5==4
replace resid_ch=1 if VA5==3
replace resid_ch=2 if VA5==1 | VA5==2
replace resid_ch=3 if VA5==5
replace resid_ch=. if VA5==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

 
*************
***dorm_ch***
*************
gen dorm_ch=VA2
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************
gen cuartos_ch=VA1
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************
gen telef_ch=.
label var telef_ch "El hogar tiene servicio telefónico fijo"


***************
***refrig_ch***
***************
gen refrig_ch=0
replace refrig_ch=1 if  VA10C==1
replace refrig_ch=. if  VA10C==.
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"


*************
***auto_ch***
*************

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"


**************
***compu_ch***
**************
gen compu_ch=0
replace compu_ch=1 if VA10H==1
replace compu_ch=. if VA10H==.
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************
*Y.L.-> elimino categoria 3
gen internet_ch=0
replace internet_ch=1 if VA10I==1
replace internet_ch=. if VA10I==.
label var internet_ch "El hogar posee conexión a Internet"


************
***cel_ch***
************
gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefonico celular"


**************
***vivi1_ch***
**************
gen vivi1_ch=1     if V1==1
replace vivi1_ch=2 if V1==2
replace vivi1_ch=3 if V1==3 | V1==4 | V1==5 
replace vivi1_ch=. if V1==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


**************
***vivi2_ch***
**************

gen vivi2_ch=0
replace vivi2_ch=1 if V1==1 | V1==2
replace vivi2_ch=. if V1==.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************
*Y.L -> incremento categoria 2
gen viviprop_ch=0 if VA8==3
replace viviprop_ch=1 if VA8==1 
replace viviprop_ch=2 if VA8==2
replace viviprop_ch=3 if VA8==4 |VA8==5 |VA8==6
replace viviprop_ch=. if VA8==.
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
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
gen vivialq_ch=VA9 if VA9>=10000
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"

gen  tcylmpri_ci =.
gen tcylmpri_ch=.
gen pqnoasis_ci=.

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* También se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch	notropari_ch notronopari_ch	nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci lp25_ci lp4_ci	lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first


qui destring $var, replace
set more off
compress
do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"
saveold "`base_out'", replace
log close



