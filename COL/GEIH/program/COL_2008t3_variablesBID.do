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
local ENCUESTA GEIH
local ANO "2008"
local ronda t3 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Colombia
Encuesta: GEIH
Round: t3
Autores: Yanira Oviedo, Yessenia Loayza (desloay@hotmail.com)
Generación nuevas variables LMK: Andres Felipe Sanchez, Laura Oliveri, Yessenia Loayza
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: noviembre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

*YL: generacion "region_c" para proyecto maps America.	
gen region_c=real(i_dpto)
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
gen region_BID_c=.
replace region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************
*NOTA: este factor fue dividido entre 3 tal como lo señala la instrucción del DANE al unir 3 meses.
*Esto se hizo en el último do file para MECOVI
gen factor_ch=fex_c
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************
sort  directorio secuencia_p
egen idh_ch = group(directorio secuencia_p)
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************
gen idp_ci=orden
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
destring clase, replace
gen byte zona_c=0 if clase==2
replace zona_c=1 if clase==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


************
****pais****
************
gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2008
label variable anio_c "Anio de la encuesta"

tostring periodo, replace
gen mes_c=real(substr(periodo, 5,2))

destring mes_c, replace
***************
***factor_ci***
***************
*NOTA: este factor fue dividido entre 3 tal como lo señala la instrucción del DANE al unir 3 meses.
*Esto se hizo en el último do file para MECOVI

gen factor_ci=fex_c
label variable factor_ci "Factor de expansion del individuo"

	***************
	***upm_ci***
	***************
gen upm_ci=. 

	***************
	***estrato_ci***
	***************
gen estrato_ci=.

		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************

*****************
***relacion_ci***
*****************
gen relacion_ci=1 if p6050==1
replace relacion_ci=2 if p6050==2
replace relacion_ci=3 if p6050==3
replace relacion_ci=4 if p6050==4 | p6050==5
replace relacion_ci=5 if p6050==7 | p6050==8 | p6050==9
replace relacion_ci=6 if p6050==6

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

**********
***sexo***
**********
gen sexo_ci=p6020
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=p6040
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 if p6070==6
replace civil_ci=2 if p6070==1 | p6070==2 | p6070==3 
replace civil_ci=3 if p6070==4 
replace civil_ci=4 if p6070==5

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
* 2014,01 modificacion segun doc metodologico
*gen miembros_ci=(relacion_ci<6)
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 
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

		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= 193701 if clase==1 /*cabecera*/
replace lp_ci= 115703 if clase==2  /*resto*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci= 83746 if clase==1 /*cabecera*/
replace lpe_ci= 69134 if clase==2  /*resto*/
label var lpe_ci "Linea de indigencia oficial del pais"


*************
**salmm_ci***
*************
* COL 2008
gen salmm_ci= 	15383.3333333333*30
label var salmm_ci "Salario minimo legal"

****************
****condocup_ci*
****************
destring oficio, replace
gen condocup_ci=.
replace condocup_ci=1 if oci==1
replace condocup_ci=2 if dsi==1
replace condocup_ci=3 if ini==1
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=(p6090==1) /*afiliacion para todas las personas*/
replace afiliado_ci=. if p6090==.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if p6920==1
replace cotizando_ci=0 if p6920==2 | (condocup_ci==2 & p6920!=1)
label var cotizando_ci "1 Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=p6930
label var instcot_ci "institución a la cual cotiza"
label define instcot_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, Ecopetrol etc)" 4 "Fondo Subsidiado (Prosperar,etc.)" 
label value instcot_ci instpen_ci

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if p6460==1 & condocup_ci==1
replace tipocontrato_ci=2 if p6460==2 & condocup_ci==1
replace tipocontrato_ci=3 if p6450==1 & condocup_ci==1
replace tipocontrato_ci=3 if p6440==2 & condocup_ci==1
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
gen cesante_ci=1 if p7310==2
replace cesante_ci=0 if p7310==1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
gen tamemp_ci=.
replace tamemp_ci=1 if p6870>=1 &p6870<=3
replace tamemp_ci=2 if p6870>=4 &p6870<=7
replace tamemp_ci=3 if p6870>=8 &p6870<=9
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
* MGD 11/30/2015: No se toman en cuenta los missings en el ingreso
replace p7500s2a1= . if p7500s2a1==9999999999 | p7500s2a1==9999 | p7500s2a1==99999 | p7500s2a1==98
gen pension_ci=1 if p7500s2a1>0 & p7500s2a1<.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

*************
*ypen_ci*
*************
gen ypen_ci=p7500s2a1
replace ypen_ci= . if p7500s2a1==9999999999 | p7500s2a1==98 | p7500s2a1==99
replace ypen_ci=.  if pension_ci==0
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
*tecnica_ci**
*************
gen tecnica_ci=(p6220==3)
label var tecnica_ci "1=formacion terciaria tecnica"

****************
*categoinac_ci**
***************
gen categoinac_ci=. 
* Modificacion MLO 2015,abr (la p7450 pregunta a desocupados)
replace categoinac_ci=2 if p6240==3 & condocup_ci==3
replace categoinac_ci=3 if p6240==4 & condocup_ci==3
recode categoinac_ci . =4 if condocup_ci==3

/*replace categoinac_ci=1 if p7450==5
replace categoinac_ci=2 if p7450==2
replace categoinac_ci=3 if p7450==3
replace categoinac_ci=4 if p7450==1 |  p7450==4 | (p7450>=6 & p7450<=9) | p7450==0
*/label var categoinac_ci "Condición de inactividad"
label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos"
label value categoinac_ci categoinac_ci

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
gen desalent_ci=(p6310==5)
replace desalent_ci=. if p6310==.
label var desalent_ci "Trabajadores desalentados"

***************
***subemp_ci***
***************
gen subemp_ci=0
gen promhora= p6800 if emp_ci==1 
*P6800: horas semanales trabajadas normalmente*
gen promhora1= p7045 
*P7045: horas semanales trabajo secundario*

egen tothoras=rowtotal(promhora promhora1), m
replace tothoras=. if promhora==. & promhora1==. 
replace tothoras=. if tothoras>=168
replace subemp_ci=1 if tothoras<=30  & emp_ci==1 & p7090==1
replace subemp_ci =. if emp_ci ==.
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=p6800 
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
gen tiempoparc_ci=(horastot_ci<30 & p7090==2)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
gen categopri_ci=.
replace categopri_ci=1 if p6430==5
replace categopri_ci=2 if p6430==4 
replace categopri_ci=3 if p6430==1 | p6430==2 |p6430==3 
replace categopri_ci=4 if p6430==6 | p6430==7
replace categopri_ci=0 if p6430==8 | p6430==9
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"


******************
***categosec_ci***
******************
gen categosec_ci=.
replace categosec_ci=1 if p7050==5
replace categosec_ci=2 if p7050==4 
replace categosec_ci=3 if p7050==1 | p7050==2 |p7050==3 
replace categosec_ci=4 if p7050==6 | p7050==7
replace categosec_ci=0 if p7050==8 | p7050==9
replace categosec_ci=. if emp_ci==0

label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & p7040==2
replace nempleos_ci=2 if emp_ci==1 & p7040==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 

/*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if p6870==1 | p6870==2 |p6870==3
replace firmapeq_ci=0 if p6870>=4 & p6870!=.
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=(p6430==2| p7050==2 ) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (oficio>=1 & oficio<=19) & emp_ci==1  
replace ocupa_ci=2 if (oficio>=20 & oficio<=21) & emp_ci==1
replace ocupa_ci=3 if (oficio>=30 & oficio<=39) & emp_ci==1
replace ocupa_ci=4 if (oficio>=40 & oficio<=49) & emp_ci==1
replace ocupa_ci=5 if (oficio>=50 & oficio<=59) & emp_ci==1
replace ocupa_ci=6 if (oficio>=60 & oficio<=64) & emp_ci==1
replace ocupa_ci=7 if (oficio>=70 & oficio<=98) & emp_ci==1
replace ocupa_ci=9 if (oficio==0 | oficio==99) & emp_ci==1
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

gen rama2d_orig=rama2d
destring rama2d, replace

gen rama_ci=.
replace rama_ci=1 if (rama2d>=1 & rama2d<=5) & emp_ci==1   
replace rama_ci=2 if (rama2d>=10 & rama2d<=14) & emp_ci==1
replace rama_ci=3 if (rama2d>=15 & rama2d<=37) & emp_ci==1
replace rama_ci=4 if (rama2d>=40 & rama2d<=41) & emp_ci==1
replace rama_ci=5 if (rama2d==45) & emp_ci==1
replace rama_ci=6 if (rama2d>=50 & rama2d<=55) & emp_ci==1
replace rama_ci=7 if (rama2d>=60 & rama2d<=64) & emp_ci==1
replace rama_ci=8 if (rama2d>=65 & rama2d<=71) & emp_ci==1
replace rama_ci=9 if (rama2d>=72 & rama2d<=99) & emp_ci==1
replace rama_ci=. if emp_ci==0

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


****************
***durades_ci***
****************
gen durades_ci= p7250/4.3
*replace durades_ci=62 if p7250==998
replace durades_ci=. if p7250==999 | p7250==998
label variable durades_ci "Duracion del desempleo en meses"
 
*******************
***antiguedad_ci***
*******************

gen antiguedad_ci= p6426/12 
replace antiguedad_ci=. if emp_ci==0 | p6426==999
label var antiguedad_ci "Antiguedad en la actividad actual en anios"





			**************
			***INGRESOS***
			**************
/*		
foreach var in p6500 p6510s1 p6600s1 p6590s1 p6610s1 p6620s1  p6585s1a1 p6585s2a1 p6585s3a1 p6585s4a1 p6580s1 { 
replace `var'=. if `var'<=999 & `var'!=0
}

foreach var in p6630s1a1 p6630s2a1 p6630s3a1 p6630s4a1 p6630s6a1 {
replace `var'=. if `var'<=999 & `var'!=0
}

foreach var in p6750 p550 p7070 p7500s1a1 p7500s2a1 p7500s3a1 {
replace `var'=. if `var'<=999 & `var'!=0
}

foreach var in p7510s1a1 p7510s2a1 p7510s3a1 p7510s4a1 p7510s5a1 p7510s6a1 {
replace `var'=. if `var'<=999 & `var'!=0
}

gen ymensual 	= p6500
gen yhorasext 	= p6510s1
gen yvivienda 	= p6600s1
gen yalimen 	= p6590s1
gen ytrans 		= p6610s1
gen yespecie 	= p6620s1 
gen ysubsi1		= p6585s1a1
gen ysubsi2		= p6585s2a1
gen ysubsi3		= p6585s3a1
gen ysubsi4		= p6585s4a1
gen yprimas		= p6545s1
gen yprimboni	= p6580s1

gen yprimser	= p6630s1a1/12
gen yprimnav 	= p6630s2a1/12
gen yprimvac	= p6630s3a1/12
gen yprimvia	= p6630s4a1/12
gen yprimbono	= p6630s6a1/12

recode p6760 (0=0.5)
replace p6760=. if p6760>=98
gen ynetoind 	= p6750/p6760
gen ycosecha 	= p550/12
gen ysecund		= p7070
gen yarrien		= p7500s1a1
gen ypension	= p7500s2a1
gen yjubila		= p7500s3a1

gen yayudafam	= p7510s1a1/12
gen yayudainst	= p7510s3a1/12
gen yayudainste	= p7510s4a1/12
gen yintereses	= p7510s5a1/12
gen ycesantia	= p7510s6a1/12
*/
gen yremesas	= p7510s2a1/12
***************
***ylmpri_ci***
***************
	egen 	ylmpri_ci = rsum(impa impaes)
	replace ylmpri_ci = . if impa==. & impaes==.
	la var 	ylmpri_ci "Ingreso laboral monetario actividad principal" 

*****************
***nrylmpri_ci***
*****************
	g nrylmpri_ci = (ylmpri_ci == . & emp_ci == 1)
	la var nrylmpri_ci "ID no respuesta ingreso de la actividad principal"  

****************
***ylnmpri_ci***
****************
	egen ylnmpri_ci = rsum(ie iees)
	replace ylnmpri_ci=. if ie==. & iees==.
	/*YL -> Nota: "ie" and "iees"corresponden al ingreso por especie de la act principal*/
	la var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

***************
***ylmsec_ci***
***************
	egen ylmsec_ci = rsum(isa isaes)
	replace ylmsec_ci=. if isa==. & isaes==.
	la var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************
	g ylnmsec_ci = . /*No se pregunta ingreso por especies para act secundaria */
	la var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

*****************
***ylmotros_ci***
*****************
	egen ylmotros_ci= rowtotal(imdi imdies), m
	la var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

******************
***ylnmotros_ci***
******************
	g ylnmotros_ci = .
	la var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

************
***ylm_ci***
************
	egen ylm_ci = rowtotal(ylmpri_ci ylmsec_ci ylmotros_ci), m
	*YL -> Incremento el ingreso laboral de inactivos & desocupados
	la var ylm_ci "Ingreso laboral monetario total"  

*************
***ylnm_ci***
*************
	egen ylnm_ci = rowtotal(ylnmpri_ci ylnmsec_ci ylnmotros_ci), m
	la var ylnm_ci "Ingreso laboral NO monetario total"  

*************
***ynlm_ci***
*************
	egen ynlm_ci = rowtotal(iof1 iof2 iof3 iof6 iof1es iof2es iof3es iof6es), m
	la var ynlm_ci "Ingreso no laboral monetario"  

**************
***ylnm_ci***
**************
	g ynlnm_ci = .
	la var ynlnm_ci "Ingreso no laboral no monetario" 


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

** Label de variables relevantes ***			
			
lab var	p6160 "¿Sabe leer y escribir?	"
lab def p6160 1 "Si" 2 "No" 	
lab val p6160 p6160		
lab var	p6170 "¿Actualmente asiste al preescolar, escuela, colegio o universidad?	"
lab def p6170 1 "Si" 2 "No" 	
lab val p6170 p6170		
lab var	p6175 "El establecimiento al que asiste ¿es oficial?	"
lab def p6175 1 "Si" 2 "No"	
lab val p6175 p6175		
lab var	p6210 "¿Cuál es el nivel educativo más alto alcanzado por  y el último año o grado aprobado en este nivel?	"
lab def p6210 1 "Ninguno" 2 "Preescolar"  3 "Básica primaria (1o - 5o) " 4  "Básica secundaria (6o - 9o) " 5  "Media (10o - 13o)" 6 " Superior o universitaria"  9 "No sabe, no informa "	
lab val p6210 p6210	p6210 p6210 p6210 p6210 p6210	
lab var p6210s1	" Grados "
lab var	p6220 "¿Cuál es el título o diploma de mayor nivel educativo que ha recibido?	"
lab def p6220 1 "Niguno" 2 "Bachiller" 3 "Técnico o tecnológico" 4 "Universitario" 5 "Postgrado" 9 "No sabe, no informa"	
lab val p6220 p6220	p6220 p6220 p6220 p6220 	

*************
***aedu_ci***
*************


/* Missing values para no sabe no responde en maximo nivel educativo y  
último anio aprobado. */
replace p6210s1=. if p6210s1==99
replace p6210=.   if p6210==9
replace p6220 = . if p6220 == 9

gen byte aedu_ci = .

* 0 anios de educacion
replace aedu_ci = 0 if p6210 == 1 | p6210 == 2 // Ninguno o prescolar.
replace aedu_ci = 0 if p6210 == 3 & p6210s1 == 0 // Primario cero anios.


*Primaria
replace aedu_ci = 1 if p6210 == 3 & p6210s1 == 1
replace aedu_ci = 2 if p6210 == 3 & p6210s1 == 2
replace aedu_ci = 3 if p6210 == 3 & p6210s1 == 3
replace aedu_ci = 4 if p6210 == 3 & p6210s1 == 4
replace aedu_ci = 5 if p6210 == 3 & p6210s1 == 5
replace aedu_ci = 5 if p6210 == 4 & p6210s1 == 0 // 0 anios secudnaria.

*Secundaria
replace aedu_ci = 6 if p6210 == 4 & p6210s1 == 6
replace aedu_ci = 7 if p6210 == 4 & p6210s1 == 7
replace aedu_ci = 8 if p6210 == 4 & p6210s1 == 8
replace aedu_ci = 9 if p6210 == 4 & p6210s1 == 9
replace aedu_ci = 10 if p6210 == 5 & p6210s1 == 10
replace aedu_ci = 11 if p6210 == 5 & p6210s1 == 11
replace aedu_ci = 11 if p6210 == 6 & p6210s1 == 0
replace aedu_ci = 12 if p6210 == 5 & p6210s1 == 12
replace aedu_ci = 13 if p6210 == 5 & p6210s1 == 13

*Superior o universitaria
replace aedu_ci = 11 + p6210s1 if p6210 == 6

* Missing 
replace aedu_ci = . if p6210 == . 



**************
***eduno_ci***
**************

gen byte eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == .
label variable eduno_ci "Sin educacion"


**************
***edupi_ci***
**************

gen byte edupi_ci = (aedu_ci >= 1 & aedu_ci < 5) 
replace edupi_ci = . if aedu_ci == .
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci = (aedu_ci == 5) 
replace edupc_ci = . if aedu_ci == . 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci = (aedu_ci >= 6 & aedu_ci < 11) 
replace edusi_ci = . if aedu_ci == . 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci = (aedu_ci == 11)
replace edusc_ci = . if aedu_ci == .
label variable edusc_ci "Secundaria completa"


**************
***eduui_ci***
**************

/* Se restringe por maximo nivel de titulacion alcanzado */
g byte eduui_ci = (aedu_ci > 11 & p6220 < 3)
replace eduui_ci = . if aedu_ci == . 
label variable eduui_ci "Superior incompleto"


***************
***eduuc_ci***
***************

g byte eduuc_ci = (aedu_ci > 11 & p6220 > 2)
replace eduuc_ci = . if aedu_ci == .
label variable eduuc_ci "Superior completo"


***************
***edus1i_ci***
***************
gen byte edus1i_ci = (aedu_ci >= 6 & aedu_ci < 9)
replace edus1i_ci = . if aedu_ci == . 
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci = (aedu_ci == 9)
replace edus1c_ci = . if aedu_ci == .
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci = (aedu_ci == 10) 
replace edus2i_ci = . if aedu_ci == .
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci = (aedu_ci == 11)
replace edus2c_ci = . if aedu_ci == . 
label variable edus2c_ci "2do ciclo de la secundaria completo"


***************
***edupre_ci***
***************

g byte edupre_ci = .
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci**
***************

g asispre_ci = (p6170 == 1 & p6210 == 2 & p6210s1 < 2)
la var asispre_ci "Asiste a educación prescolar"
	
**************
***eduac_ci***
**************

/* No se puede calcular ya que solo tenemos la diferenciacion para los 
que finalizaron cada nivel. */
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci = 1 if p6170 == 1
replace asiste_ci = 0 if p6170 == 2
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"

**************
*pqnoasis1_ci*
**************

g pqnoasis1_ci = .

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

gen edupub_ci=.
replace edupub_ci = 1 if (p6175 == 1 & p6170 == 1)
replace edupub_ci = 0 if (p6175 == 2 & p6170 == 1)
label var edupub_ci "Asiste a un centro de enseñanza público"



		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if p4030s5==1 
replace aguared_ch = 0 if p4030s5==2
la var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 1 if p5050==1 
replace aguafconsumo_ch = 2 if p5050==7 
replace aguafconsumo_ch = 3 if p5050==10 
replace aguafconsumo_ch = 5 if p5050==5 
replace aguafconsumo_ch = 6 if p5050==8 
replace aguafconsumo_ch = 7 if p5050==2
replace aguafconsumo_ch = 8 if p5050==6  
replace aguafconsumo_ch = 9 if (p5050==4 | p5050==9)
replace aguafconsumo_ch = 10 if (p5050==3| p5050==2)

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.
replace aguafuente_ch = 1 if p5050==1 
replace aguafuente_ch = 2 if p5050==7 
replace aguafuente_ch = 3 if p5050==10 
replace aguafuente_ch = 5 if p5050==5 
replace aguafuente_ch = 6 if p5050==8 
replace aguafuente_ch = 7 if p5050==2
replace aguafuente_ch = 8 if p5050==6  
replace aguafuente_ch = 9 if (p5050==4 | p5050==9)
replace aguafuente_ch = 10 if (p5050==3 | p5050==2)


*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch=1 if (p5050==1 | p5050==2)
replace aguadist_ch=0 if p5050>2


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =.
replace aguadisp1_ch = 1 if p4040==1
replace aguadisp1_ch = 0 if p4040==2


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
generate aguamide_ch = .
label var aguamide_ch "Usan medidor para pagar consumo de agua"


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.
replace bano_ch=0 if p5020==6
replace bano_ch=1 if p5020==1
replace bano_ch=2 if p5020==2
replace bano_ch=4 if p5020==5
replace bano_ch=6 if p5020==3 | p5020 ==4

***************
***banoex_ch***
***************
generate banoex_ch=.
replace banoex_ch = 1 if p5030==1
replace banoex_ch = 0 if p5030==2
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
replace sinbano_ch = 0 if p5020<6

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"
		


************
***luz_ch***
************
g luz_ch = p4030s1 == 1 
replace luz_ch=. if p4030s1==.
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************
g combust_ch = (p5080 == 1 | p5080 == 3 | p5080 == 4)
replace combust_ch =. if p5080==.
label var combust_ch "Principal combustible gas o electricidad" 



*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if p5020==1 | p5020==2
replace des1_ch=2 if p5020==3 | p5020==4
replace des1_ch=3 if p5020==5
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if p5020==1 | p5020==2 | p5020==3 | p5020==4
replace des2_ch=3 if p5020==5
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************
g piso_ch = (p4020 != 1 & p4020 != .)
replace piso_ch = . if p4020 == .
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch= (p4010>=1 & p4010<=3)
replace pared_ch=. if p4010==.
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
gen resid_ch =0    if p5040==1
replace resid_ch=1 if p5040==4
replace resid_ch=2 if p5040==2 | p5040==3
replace resid_ch=3 if p5040==5
replace resid_ch=. if p5040==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


*************
***dorm_ch***
*************

gen dorm_ch=p5010
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=p5000
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************
g cocina_ch = 0 if p5070 >= 2 & p5070 <= 6
replace cocina_ch = 1 if p5070 == 1
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************
g telef_ch = p5210s1 == 1
replace telef_ch = . if p5210s1 == .
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************
g refrig_ch = p5210s5 == 1
replace refrig_ch = . if p5210s5 == .
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************
gen freez_ch=.
label var freez_ch "El hogar posee congelador"


*************
***auto_ch***
*************
g auto_ch = p5210s22 == 1
replace auto_ch = . if p5210s22 == .
label var auto_ch "El hogar posee automovil particular"

**************
***compu_ch***
**************
g compu_ch = p5210s16 == 1
replace compu_ch = . if p5210s16 == .
label var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************
g internet_ch = p5210s3 == 1
replace internet_ch = . if p5210s3 == . 
label var internet_ch "El hogar posee conexión a Internet"

************
***cel_ch***
************
/*
gen temp1=(p5220==1)
replace temp1=. if p5220==.

bysort idh_ch: egen temp2=sum(temp1)
replace temp2=. if p5220==.

gen cel_ch=0
replace cel_ch=1 if temp2>=1 & temp2!=.
replace cel_ch=. if p5220==.
label var cel_ch "El hogar tiene servicio telefonico celular"
drop temp* 
*/
gen cel_ch=.
**************
***vivi1_ch***
**************
gen vivi1_ch=1     if p4000==1
replace vivi1_ch=2 if p4000==2
replace vivi1_ch=3 if p4000==3 | p4000==4 | p4000==5 | p4000==6
replace vivi1_ch=. if p4000==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

*************
***vivi2_ch***
*************
g vivi2_ch = (p4000 == 1 | p4000 == 2)
replace vivi2_ch = . if p4000 == .
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
gen viviprop_ch=0     if p5090==3
replace viviprop_ch=1 if p5090==1
replace viviprop_ch=2 if p5090==2
replace viviprop_ch=3 if p5090==4 |p5090==5 |p5090==6
replace viviprop_ch=. if p5090==.
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************
gen vivitit_ch=.
replace vivitit_ch=1 if p5120==1
replace vivitit_ch=0 if p5120==2
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
gen vivialq_ch=p5140 if p5140>=10000
label var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=p5130 if p5130>=10000
label var vivialqimp_ch "Alquiler mensual imputado"

gen  tcylmpri_ci =.
gen tcylmpri_ch=.

drop aux

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

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
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress

foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}

saveold "`base_out'", replace


log close



