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
local ANO "2010"
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
Generación nuevas variables LMK: Andres Felipe , Laura Oliveri, Yessenia Loayza
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: noviembre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear


***************
***region_c ***
***************
*YL: generacion "region_c" para los años 2009 y +. Para proyecto maps America.

egen idhtemp = concat(directorio secuencia_p)
gen departamento=real(i_dpto)
bys idhtemp: egen auxreg=max(departamento) 

gen region_c=real(i_dpto)
replace region_c= auxreg if region_c==. /*Se coloca la variable region a los <10 porque variable orginal no la asigna*/
label define region_c   /// /*Falta confirmar esta categoria. No encuentro la codificacion oficial, asigno analizando las distribubiones con datos oficiales*/
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
*confirmo dpa en http://190.25.231.237/dvpbuscar/dvpbuscar.html
drop idhtemp departamento idhtemp auxreg

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
sort directorio secuencia_p
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
gen zona_c=0 if clase==2
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
gen anio_c=2010
label variable anio_c "Anio de la encuesta"

gen mes_c=real(mes)
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
by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65 & edad_ci!=.)
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
gen miembros_ci=(relacion_ci<=4)
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

		
****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if oci==1
replace condocup_ci=2 if dsi==1
replace condocup_ci=3 if ini==1
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= 207005 if clase==1 /*cabecera*/
replace lp_ci= 123502 if clase==2  /*resto*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci= 87401 if clase==1 /*cabecera*/
replace lpe_ci= 71392 if clase==2  /*resto*/
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
*2014, 10 modificacion MLO
gen salmm_ci= 17166.6666666667*30	
label var salmm_ci "Salario minimo legal"

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
label define instpen_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, Ecopetrol etc)" 4 "Fondo Subsidiado (Prosperar,etc.)" 
label value instpen_ci instpen_ci

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
replace ypen_ci= . if p7500s2a1==9999999999  | p7500s2a1==98 | p7500s2a1==98 
replace ypen_ci=. if pension_ci==0
*YL -> no estoy segura si 9999 es valor perdido.
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

/*
replace categoinac_ci=1 if p7450==5
replace categoinac_ci=2 if p7450==2
replace categoinac_ci=3 if p7450==3
replace categoinac_ci=4 if p7450==1 |  p7450==4 | (p7450>=6 & p7450<=9) | p7450==0*/
label var categoinac_ci "Condición de inactividad"
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

/*
************
***emp_ci***
************
gen emp_ci=0
replace emp_ci=1 if p6440!=.
label var emp_ci "Ocupado (empleado)"


****************
***desemp3_ci***
****************
gen byte desemp3_ci=(p7250!=.)
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"

*/

*****************
***desalent_ci***
*****************
gen desalent_ci=(p6310==5)
replace desalent_ci=. if p6310==.
label var desalent_ci "Trabajadores desalentados"

***************
***subemp_ci***
***************
gen promhora= p6800 if emp_ci==1 
*p6800: horas semanales trabajadas normalmente*
gen promhora1= p7045 
*p7045: horas semanales trabajo secundario*

egen tothoras=rowtotal(promhora promhora1), m
replace tothoras=. if promhora==. & promhora1==. 
replace tothoras=. if tothoras>=168

gen subemp_ci=0
replace subemp_ci=1 if tothoras<=30  & emp_ci==1 & p7090==1
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
destring oficio, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (oficio>=1 & oficio<=19) & emp_ci==1  
replace ocupa_ci=2 if (oficio>=20 & oficio<=21) & emp_ci==1
replace ocupa_ci=3 if (oficio>=30 & oficio<=39) & emp_ci==1
replace ocupa_ci=4 if (oficio>=40 & oficio<=49) & emp_ci==1
replace ocupa_ci=5 if (oficio>=50 & oficio<=59) & emp_ci==1
replace ocupa_ci=6 if (oficio>=60 & oficio<=64) & emp_ci==1
replace ocupa_ci=7 if (oficio>=70 & oficio<=98) & emp_ci==1
replace ocupa_ci=9 if (oficio==00 | oficio==99) & emp_ci==1
replace ocupa_ci=. if emp_ci==0
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

*************
***rama_ci***
*************
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
foreach var in p6500 p6510s1 p6600s1 p6590s1 p6610s1 p6620s1 p6585s1a1 p6585s2a1 p6585s3a1 p6585s4a1 p6580s1 { 
replace `var'=. if `var'<=999 & `var'!=0
}

foreach var in p6630s1a1 p6630s2a1 p6630s3a1 p6630s4a1 p6630s6a1 {
replace `var'=. if `var'<=999 & `var'!=0
}

foreach var in p6750 p550 p7070 p7500s1a1 p7500s2a1 p7500s3a1 {
replace `var'=. if `var'<=999 & `var'!=0
}

foreach var in p7510s1a1 p7510s2a1 p7510s3a1 p7510s5a1 p7510s6a1 {
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

gen yprimser	= p6630s1a1
gen yprimnav 	= p6630s2a1
gen yprimvac	= p6630s3a1
gen yprimvia	= p6630s4a1
gen yprimbono	= p6630s6a1

recode p6760 (0=0.5)
replace p6760=. if p6760>=98
gen ynetoind 	= p6750/p6760
gen ycosecha 	= p550/12
gen ysecund		= p7070
gen yarrien		= p7500s1a1
gen ypension	= p7500s2a1
gen yjubila		= p7500s3a1

gen yayudafam	= p7510s1a1/12
gen yremesas	= p7510s2a1/12
gen yayudainst	= p7510s3a1/12
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

** Se incorporan modificaciones a sintaxis de variable aedu_ci según revisión hecha por Iván Bornacelly SCL/EDU **

replace p6210s1=. if p6210s1==99
replace p6210=.   if p6210==9

gen aedu_ci=.
* IB: Estaban quedando con missing aquellas personas que tienen educación prescolar y están en el grado 1.
/*
replace aedu_ci=0 if p6210==1 & p6210s1==0 
replace aedu_ci=0 if p6210==2 & p6210s1==0 
replace aedu_ci=0 if p6210==3 & p6210s1==0 
*/
replace aedu_ci=0 if p6210==1 
replace aedu_ci=0 if p6210==2 
replace aedu_ci=0 if p6210==3 & p6210s1==0 

*Primaria
replace aedu_ci=1 if p6210==2 & p6210s1==1 
replace aedu_ci=1 if p6210==3 & p6210s1==1
replace aedu_ci=2 if p6210==3 & p6210s1==2
replace aedu_ci=3 if p6210==3 & p6210s1==3
replace aedu_ci=4 if p6210==3 & p6210s1==4
replace aedu_ci=5 if p6210==3 & p6210s1==5
replace aedu_ci=5 if p6210==4 & p6210s1==0

*Secundaria
replace aedu_ci=6 if p6210==4 & p6210s1==6
replace aedu_ci=7 if p6210==4 & p6210s1==7
replace aedu_ci=8 if p6210==4 & p6210s1==8
replace aedu_ci=9 if p6210==4 & p6210s1==9
replace aedu_ci=10 if p6210==5 & p6210s1==10
replace aedu_ci=11 if p6210==5 & p6210s1==11
replace aedu_ci=11 if p6210==6 & p6210s1==0
replace aedu_ci=12 if p6210==5 & p6210s1==12
replace aedu_ci=13 if p6210==5 & p6210s1==13

*Superior
replace aedu_ci = 11+p6210s1 if p6210==6 
label var aedu_ci "Anios de educacion aprobados" 

**************
***eduno_ci***
**************
gen eduno_ci=0
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
replace edusc_ci=1 if (aedu_ci>=11 & aedu_ci<=13) & p6210==5 
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
*Para la educación superior no es posible saber cuantos anios dura el ciclo
*por ello se hace una aproximación a través de titulación
g byte eduui_ci = (aedu_ci > 11 & aedu_ci!=. & p6210 == 6 & (p6220 == 1 | p6220 == 2))
label variable eduui_ci "Superior incompleto"

***************
***eduuc_ci***
***************
*Para la educación superior no es posible saber cuantos anios dura el ciclo
*por ello se hace una aproximación a través de titulación
g byte eduuc_ci = ((aedu_ci > 11 & aedu_ci!=.) & p6210 == 6 & (p6220 == 3 | p6220 == 4 | p6220 == 5))
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
replace edus2c_ci=1 if (aedu_ci>=11 & aedu_ci<=13) & p6220==2
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************
g byte edupre_ci =(p6210s1==1 & p6210==2)
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci**
***************
*Variable creada por Iván Bornacelly - 01/16/2017
	g asispre_ci=.
	replace asispre_ci=1 if p6210s1==0 & p6210==2 & p6170==1
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educación prescolar"
	
**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
gen asiste_ci=.
replace asiste_ci=1 if p6170==1
replace asiste_ci=0 if p6170==2
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************
gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

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
replace edupub_ci=1 if p6175==1
replace edupub_ci=0 if p6175==2
label var edupub_ci "Asiste a un centro de enseñanza público"



		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************


****************
***aguared_ch***
****************
* MGR Jil, 2015: corrección en sintáxis, la variable se reemplaza por missing
/*
gen aguared_ch=(p4030s5==1)
replace aguared_ch=.
*/

gen aguared_ch=(p4030s5==1)
replace aguared_ch=. if p4030s5==.
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=.
label val aguadist_ch aguadist_ch

*****************
***aguamala_ch***
*****************
gen aguamala_ch=(p5050==5 | p5050==6)
replace aguamala_ch=. if p5050==.
label var aguamala_ch "Agua unimproved según MDG" 

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

************
***luz_ch***
************
gen luz_ch=p4030s1==1 
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
***bano_ch***
*************
g bano_ch = p5020 != 6
replace bano_ch = . if p5020 == .
label var bano_ch "El hogar tiene servicio sanitario"

***************
***banoex_ch***
***************
g banoex_ch = p5030 == 1
replace banoex_ch = . if bano_ch == 0
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

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
replace des2_ch=2 if p5020==5
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
*g pared_ch = (p4010 >= 1 & p4010 <= 7)
*Y.L. hago comparable a ECH
g pared_ch = (p4010==1 | p4010==2 | p4010==3 | p4010==5)
replace pared_ch = . if p4010 == .
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


 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (p5050 >=1 & p5050 <=5) | p5050==7
replace aguamejorada_ch = 0 if  p5050 ==6 | (p5050 >=8 & p5050 <=10) 

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if ( p5020 >=1 &  p5020 <=4) & p5030 ==1
replace banomejorado_ch = 0 if (( p5020 >=1 &  p5020 <=4) & p5030 ==2) | ( p5020 >=5 &  p5020 <=6)
 
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
gen cocina_ch=0 if p5070>=2 & p5070<=6
replace cocina_ch=1 if p5070==1
replace cocina_ch=. if p5070==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
gen telef_ch=0
replace telef_ch=1 if p5210s1==1
replace telef_ch=. if p5210s1==.
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************
gen refrig_ch=0
replace refrig_ch=1 if  p5210s5==1
replace refrig_ch=. if  p5210s5==.
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
replace auto_ch=1 if p5210s22==1
replace auto_ch=. if p5210s22==.
label var auto_ch "El hogar posee automovil particular"

**************
***compu_ch***
**************
gen compu_ch=0
replace compu_ch=1 if p5210s16==1
replace compu_ch=. if p5210s16==.
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
gen temp1=(p5220==1)
replace temp1=. if p5220==.

bysort idh_ch: egen temp2=sum(temp1)
replace temp2=. if p5220==.

gen cel_ch=0
replace cel_ch=1 if temp2>=1 & temp2!=.
replace cel_ch=. if p5220==.
label var cel_ch "El hogar tiene servicio telefonico celular"
drop temp*


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

**************
***vivi2_ch***
**************
gen vivi2_ch=0
replace vivi2_ch=1 if p4000==1 | p4000==2
replace vivi2_ch=. if p4000==.
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
gen viviprop_ch=0 if p5090==3
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
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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





