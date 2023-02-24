* (Versión Stata 14)
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
local ANO "2016"
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
Autores: Marcela G. Rubio
Ultima version: Alvaro J. Altamirano M. (alvaroalt@iadb.org)
Ultima modificacion: Daniela Zuluaga
Fecha última modificación: Octubre 2017

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

***************
***region_c ***
***************
gen region_c=real(dpto)
label define region_c       /// 
	5  "Antioquia"	        ///
	8  "Atlantico"	        ///
	11 "Bogota, D.C"	    ///
	13 "Bolivar" 	        ///
	15 "Boyace"	            ///
	17 "Caldas"	            ///
	18 "Caqueta"	        ///
	19 "Cauca"	            ///
	20 "Cesar"	            ///
	23 "Cordoba"	        ///
	25 "Cundinamarca"       ///
	27 "Choco"	            ///
	41 "Huila"	            ///
	44 "La Guajira"	        ///
	47 "Magdalena"	        ///
	50 "Meta"	            ///
	52 "Narino"	            ///
	54 "Norte de Santander"	///
	63 "Quindio"	        ///
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
label define region_BID_c 1 "CentroamÃ©rica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************
g factor_ch=fex_c_2011
la var factor_ch "Factor de expansiÃ³n del hogar"

***************
****idh_ch*****
***************
gen idh_ch = idh
la var idh_ch "ID del hogar"

**************
****idp_ci****
**************
g idp_ci=orden
la var idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
destring clase, replace
g zona_c = clase == 1
la var zona_c "Zona del paÃ­s"
la de zona_c 1 "Urbana" 0 "Rural"
la val zona_c zona_c

************
****pais****
************
g str3 pais_c = "COL"
la var pais_c "PaÃ­s"

**********
***anio***
**********
g anio_c = 2016
la var anio_c "AÃ±o de la encuesta"

destring mes, replace
gen mes_c=mes
	
***************
***factor_ci***
***************
* YL -> El factor fue dividido para 3 porque se unieron los 3 meses (en do-file de merge).
g factor_ci=fex_c_2011
la var factor_ci "Factor de expansiÃ³n del individuo"

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
	g 		relacion_ci = 1 if p6050 == 1
	replace relacion_ci = 2 if p6050 == 2
	replace relacion_ci = 3 if p6050 == 3
	replace relacion_ci = 4 if p6050 == 4 | p6050 == 5
	replace relacion_ci = 5 if p6050 == 7 | p6050 == 8 | p6050 == 9
	replace relacion_ci = 6 if p6050 == 6
	la var relacion_ci "RelaciÃ³n con el jefe del hogar"
	la de relacion_ci 	1 "Jefe/a" 				///
						2 "Esposo/a" 			///
						3 "Hijo/a" 				///
						4 "Otros parientes" 	///
						5 "Otros no parientes" 	///
						6 "Empleado/a domÃ©stico/a"
	la val relacion_ci relacion_ci

**********
***sexo***
**********
	g sexo_ci = p6020
	la var sexo_ci "Sexo del individuo" 
	la define sexo_ci 1 "Hombre" 2 "Mujer"
	la val sexo_ci sexo_ci

**********
***edad***
**********
	g edad_ci = p6040
	la var edad_ci "Edad del individuo (aÃ±os)"

*****************
****civil_ci*****
*****************
	g 		civil_ci = .
	replace civil_ci = 1 if p6070 == 6
	replace civil_ci = 2 if p6070==1 | p6070==2 | p6070==3
	replace civil_ci = 3 if p6070==4 
	replace civil_ci = 4 if p6070==5
	la var civil_ci "Estado civil"
	la de civil_ci 	1 "Soltero" 				///
					2 "UniÃ³n formal o informal" ///
					3 "Divorciado o separado" 	///
					4 "Viudo"
	la val civil_ci civil_ci

**************
***jefe_ci***
*************
	g jefe_ci = relacion_ci == 1
	la var jefe_ci "Jefe de hogar"

******************
***nconyuges_ch***
******************
	bys idh_ch: egen nconyuges_ch = sum(relacion_ci == 2)
	la var nconyuges_ch "NÃºmero de cÃ³nyuges"

***************
***nhijos_ch***
***************
	bys idh_ch: egen nhijos_ch = sum(relacion_ci == 3)
	la var nhijos_ch "NÃºmero de hijos"

******************
***notropari_ch***
******************
	bys idh_ch: egen notropari_ch = sum(relacion_ci == 4)
	la var notropari_ch "NÃºmero de otros familiares"

********************
***notronopari_ch***
********************
	bys idh_ch: egen notronopari_ch = sum(relacion_ci == 5)
	la var notronopari_ch "NÃºmero de no familiares"

****************
***nempdom_ch***
****************
	bys idh_ch: egen nempdom_ch = sum(relacion_ci == 6)
	la var nempdom_ch "NÃºmero de empleados domÃ©sticos"

*****************
***clasehog_ch***
*****************
	g byte clasehog_ch = 0
**** unipersonal
	replace clasehog_ch = 1 if nhijos_ch == 0 & nconyuges_ch == 0 & notropari_ch == 0 & notronopari_ch == 0
**** nuclear (child with or without spouse but without other relatives)
	replace clasehog_ch = 2 if (nhijos_ch > 0 | nconyuges_ch > 0) & (notropari_ch == 0 & notronopari_ch == 0)
**** ampliado
	replace clasehog_ch = 3 if notropari_ch > 0 & notronopari_ch == 0
**** compuesto (some relatives plus non relative)
	replace clasehog_ch = 4 if ((nconyuges_ch > 0 | nhijos_ch > 0 | notropari_ch > 0) & (notronopari_ch > 0))
**** corresidente
	replace clasehog_ch = 5 if nhijos_ch == 0 & nconyuges_ch == 0 & notropari_ch == 0 & notronopari_ch > 0
	
	la variable clasehog_ch "Tipo de hogar"
	la de clasehog_ch 	1 "Unipersonal" 	///
						2 "Nuclear" 		///
						3 "Ampliado" 		///
						4 "Compuesto" 		///
						5 "Corresidente"
	la val clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************
	bys idh_ch: egen nmiembros_ch = sum(relacion_ci >= 1 & relacion_ci <= 4)
	la var nmiembros_ch "Número de familiares en el hogar"

*****************
***nmayor21_ch***
*****************
	bys idh_ch: egen nmayor21_ch = sum((relacion_ci >= 1 & relacion_ci <= 4) & edad_ci >= 21 & edad_ci!=.)
	la var nmayor21_ch "Número de familiares mayores a 21 años"

*****************
***nmenor21_ch***
*****************
	bys idh_ch: egen nmenor21_ch = sum((relacion_ci> = 1 & relacion_ci< = 4) & edad_ci < 21)
	la var nmenor21_ch "Número de familiares menores a 21 años"

*****************
***nmayor65_ch***
*****************
	bys idh_ch: egen nmayor65_ch = sum((relacion_ci >= 1 & relacion_ci <= 4) & edad_ci >= 65 & edad_ci!=.)
	la var nmayor65_ch "Número de familiares mayores a 65 años"

****************
***nmenor6_ch***
****************
	bys idh_ch: egen nmenor6_ch = sum((relacion_ci >= 1 & relacion_ci <= 4) & edad_ci < 6)
	la var nmenor6_ch "Número de familiares menores a 6 años"

****************
***nmenor1_ch***
****************
	bys idh_ch: egen nmenor1_ch = sum((relacion_ci >= 1 & relacion_ci <= 4) & edad_ci < 1)
	la var nmenor1_ch "Número de familiares menores a 1 año"

****************
***miembros_ci***
****************
	g miembros_ci = (relacion_ci <= 4)
	la var miembros_ci "Miembro del hogar"


******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 

	***************
	***afroind_ci***
	***************
**Pregunta: De acuerdo con su cultura, pueblo o rasgos físicos, … es o se reconoce como:(P6080) (1- Indigena 2- Gitano - Rom 3- Raizal del archipiélago de San Andrés y providencia 4- Palenquero de San basilio o descendiente 5- Negro(a), mulato(a), Afrocolombiano(a) o Afrodescendiente 6- Ninguno de los anteriores (mestizo, blanco, etc)) 
gen afroind_ci=. 
replace afroind_ci=1  if P6080 == 1 
replace afroind_ci=2 if P6080 == 3 | P6080 == 4 | P6080 == 5
replace afroind_ci=3 if P6080 == 2 | P6080 == 6
replace afroind_ci=. if P6080 ==.
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
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
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
*Nota: la pregunta p6920 se realiza a todos los ocupados (asalariados/independientes)

********************
*** instcot_ci *****
********************
gen instcot_ci=p6930
label var instcot_ci "institución a la cual cotiza"
label define instcot_ci 1 "Fondo privado" 2 "ISS, Cajanal" 3 "Regímenes especiales (FFMM, Ecopetrol etc)" 4 "Fondo Subsidiado (Prosperar,etc.)" 
label value instcot_ci instcot_ci

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*********
*lp_ci***
*********
gen lp_ci =lp
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =li
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*http://obiee.banrep.gov.co/analytics/saw.dll?Go&Path=/shared/Consulta%20Series%20Estadisticas%20desde%20Excel/1.%20Salarios/1.1%20Salario%20minimo%20legal%20en%20Colombia/1.1.1%20Serie%20historica&Options=rdf&NQUser=salarios&NQPassword=salarios&lang=es
gen salmm_ci= 689455
label var salmm_ci "Salario minimo legal"

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
replace tamemp_ci=1 if p6870>=1 & p6870<=3
replace tamemp_ci=2 if p6870>=4 & p6870<=7
replace tamemp_ci=3 if p6870>=8 & p6870<=9
label var tamemp_ci "# empleados en la empresa"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

*MGD30/11/2015: segun el documento metodologico se genera condicionada a si recibe ingreso por pension.  Varía ligeramente el resultado.
* La serie hacia atrás se genera condicionando al ingreso no con la variable binaria.
gen pension_ci=1 if p7500s2a1>0 & p7500s2a1!=.
recode pension_ci .=0 
*Yl -> ok que incluya a los que tienen codigo 98 (si recieben pero no se sabe el valor)
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
replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
*DZ Octubre 2017-Se crea la variable de pension subsidiada*
gen pensionsub_ci=(p1661s3==1)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
*DZ Octubre 2017-Se crea la variable valor de la pension subsidiada*

gen ypensub_ci=  p1661s3a1
replace ypensub_ci=. if p1661s3a1==98
replace ypensub_ci= ypensub_ci/12
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
 

*************
*tecnica_ci**
*************
gen tecnica_ci=(p6220==3)
label var tecnica_ci "1=formacion terciaria tecnica"

****************
*categoinac_ci**
****************
gen categoinac_ci=. 
replace categoinac_ci=1 if p7450==5 & condocup_ci==3
recode categoinac_ci .=2 if p7450==2 | (p6240==3 & condocup_ci==3)
recode categoinac_ci .=3 if p7450==3 | (p6240==4 & condocup_ci==3)
recode  categoinac_ci .=4 if condocup_ci==3

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

*****************
***desalent_ci***
*****************
g desalent_ci = p6310 == 5
replace desalent_ci = . if p6310 == .
la var desalent_ci "Trabajador desalentado"

***************
***subemp_ci***
***************
g promhora = p6800 if emp_ci==1 
*p6800: horas semanales trabajadas normalmente*
g promhora1 = p7045 
*p7045: horas semanales trabajo secundario*
egen tothoras = rowtotal(promhora promhora1), m
replace tothoras = . if promhora == . & promhora1 == . 
replace tothoras = . if tothoras >= 168

g subemp_ci = 0
replace subemp_ci = 1 if tothoras <= 30  & emp_ci == 1 & p7090 == 1
replace subemp_ci = . if emp_ci == .
la var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************
	g horaspri_ci = p6800 
	replace horaspri_ci = . if emp_ci == 0
	la var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
	g horastot_ci = tothoras  if emp_ci == 1 
	la var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*******************
***tiempoparc_ci***
*******************
	g tiempoparc_ci = (horastot_ci < 30 & p7090 == 2)
	replace tiempoparc_ci = . if emp_ci == 0
	la var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
	g categopri_ci = .
	replace categopri_ci = 1 if p6430 == 5
	replace categopri_ci = 2 if p6430 == 4 
	replace categopri_ci = 3 if p6430 == 1 | p6430 == 2 | p6430 == 3 
	replace categopri_ci = 4 if p6430 == 6 | p6430 == 7
	replace categopri_ci = 0 if p6430 == 8 | p6430==9
	replace categopri_ci = . if emp_ci == 0
	la de categopri_ci 	0 "Otro" 			///
						1 "Patrón" 			///
						2 "Cuenta propia" 	///
						3 "Empleado" 	 	///
						4 "No remunerado"
	la val categopri_ci categopri_ci
	la var categopri_ci "Categoría ocupacional"

******************
***categosec_ci***
******************
	g categosec_ci = .
	replace categosec_ci = 1 if p7050 == 5
	replace categosec_ci = 2 if p7050 == 4 
	replace categosec_ci = 3 if p7050 == 1 | p7050 == 2 | p7050 == 3 
	replace categosec_ci = 4 if p7050 == 6 | p7050 == 7
	replace categosec_ci = 0 if p7050 == 8 | p7050 == 9
	replace categosec_ci = . if emp_ci == 0
	la de categosec_ci 	0 "Otro" 			///
						1 "Patrón" 			///
						2 "Cuenta propia" 	///
						3 "Empleado" 		///
						4 "No remunerado"
	la val categosec_ci categosec_ci
	la var categosec_ci "Categoría ocupacional trabajo secundario"

*****************
***nempleos_ci***
*****************
	g nempleos_ci = .
	replace nempleos_ci = 1 if emp_ci == 1 & p7040 == 2
	replace nempleos_ci = 2 if emp_ci == 1 & p7040 == 1
	la var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************
	g firmapeq_ci = (p6870 == 1 | p6870 == 2 | p6870 == 3)
	replace firmapeq_ci = . if emp_ci == 0
	la var firmapeq_ci "Trabajadores informales"
	*/
*****************
***spublico_ci***
*****************
	g spublico_ci = (p6430 == 2 | p7050 ==2) 
	replace spublico_ci = . if emp_ci == 0 
	la var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
	destring oficio, replace
	g ocupa_ci=.
	replace ocupa_ci = 1 if oficio >= 1  & oficio <= 19 & emp_ci == 1  
	replace ocupa_ci = 2 if oficio >= 20 & oficio <= 21 & emp_ci == 1
	replace ocupa_ci = 3 if oficio >= 30 & oficio <= 39 & emp_ci == 1
	replace ocupa_ci = 4 if oficio >= 40 & oficio <= 49 & emp_ci == 1
	replace ocupa_ci = 5 if oficio >= 50 & oficio <= 59 & emp_ci == 1
	replace ocupa_ci = 6 if oficio >= 60 & oficio <= 64 & emp_ci == 1
	replace ocupa_ci = 7 if oficio >= 70 & oficio <= 98 & emp_ci == 1  
	replace ocupa_ci = 9 if oficio == 0  | oficio == 99 & emp_ci == 1
 
	la var ocupa_ci "Ocupación laboral"
	la de ocupa_ci 	1 "Profesional y técnico"																///
					2 "Director o funcionario superior"														///
					3 "Personal administrativo y nivel intermedio"											///
					4 "Comerciante o vendedor"																///
					5 "Trabajador en servicios"																///
					6 "Trabajador agrícola y afines"														///
					7 "Obrero no agrícola, conductores de máquinas y vehículos de transporte y similares" 	///
					8 "Fuerzas armadas" 																	///
					9 "Otras ocupaciones"
	la val ocupa_ci ocupa_ci


*************
***rama_ci***
*************
	destring rama2d, replace
	g rama_ci = .
	replace rama_ci = 1 if rama2d >=  1 & rama2d <=  5
	replace rama_ci = 2 if rama2d >= 10 & rama2d <= 14
	replace rama_ci = 3 if rama2d >= 15 & rama2d <= 37
	replace rama_ci = 4 if rama2d >= 40 & rama2d <= 41
	replace rama_ci = 5 if rama2d == 45
	replace rama_ci = 6 if rama2d >= 50 & rama2d <= 55
	replace rama_ci = 7 if rama2d >= 60 & rama2d <= 64
	replace rama_ci = 8 if rama2d >= 65 & rama2d <= 71
	replace rama_ci = 9 if rama2d >= 72 & rama2d <= 99
	replace rama_ci = . if emp_ci == 0
	
	la var rama_ci "Rama de actividad"
	la def rama_ci 	1 "Agricultura, caza, silvicultura y pesca"				///
					2 "Explotación de minas y canteras"						///
					3 "Industrias manufactureras"							///
					4 "Electricidad, gas y agua"							///
					5 "Construcción"										///
					6 "Comercio, restaurantes y hoteles"					///
					7 "Transporte y almacenamiento"							///
					8 "Establecimientos financieros, seguros e inmuebles" 	///
					9 "Servicios sociales y comunales"
	la val rama_ci rama_ci

****************
***durades_ci***
****************
	g durades_ci = p7250 / 4.3
	replace durades_ci = . if p7250 == 999 
	la var durades_ci "Duración del desempleo en meses"
 
*******************
***antiguedad_ci***
*******************
	g antiguedad_ci = p6426 / 12 
	replace antiguedad_ci = . if emp_ci == 0 | p6426 == 999
	la var antiguedad_ci "Antiguedad en la actividad actual en años"

			**************
			***INGRESOS***
			**************
/*
	foreach var in p6500 p6510s1 p6590s1 p6600s1 p6610s1 p6620s1	    ///
		p6585s1a1 p6585s2a1 p6585s3a1 p6585s4a1	p6545s1	p6580s1		    ///
		p6630s1a1 p6630s2a1 p6630s3a1 p6630s4a1 p6630s6a1 			    ///
		p6750 p550 p7070 p7500s1a1 p7500s2a1 p7500s3a1 	p7422s1		    ///
		p7510s1a1 p7510s2a1 p7510s3a1 p7510s5a1 p7510s6a1 p7070	 p7472s1 ///
	{ 
		replace `var' = . if `var' < =99 & `var' != 0
	}
*asalariados activida principal
	g ymensual	 = p6500
	g yhorasext  = p6510s1 if p6510s2==2
	
	g yalimen	 = p6590s1 
	g yvivienda  = p6600s1
	g ytrans	 = p6610s1
	g yespecie 	 = p6620s1 
	g ysubsi1	 = p6585s1a1 if p6585s1a2==2
	g ysubsi2	 = p6585s2a1 if p6585s2a2==2
	g ysubsi3	 = p6585s3a1 if p6585s3a2==2
	g ysubsi4	 = p6585s4a1 if p6585s4a2==2
	
	g yprimas	 = p6545s1   if p6545s2==2
	g yprimboni	 = p6580s1   if p6580s2==2
	g yprimser	 = p6630s1a1
	g yprimnav 	 = p6630s2a1
	g yprimvac	 = p6630s3a1
	g yprimvia	 = p6630s4a1
	g yprimbono	 = p6630s6a1
*Independientes actividad principal
	recode p6760 (0=0.5) // JLC: ¿Por qué? -> YL: pienso que es para justificar que gana porque trabajo al menos 15 dias. si se deja 0 es como ganar sin trabajar y no se sabria cuantos dias exactamente, es proxy.
	replace p6760 = . if p6760 >= 98
	g ynetoind 	 = p6750 / p6760
	g ycosecha 	 = p550 / 12
*Actividad secundaria	
	g ysecund	 = p7070
*desocupados
	g ydesoc     =p7422s1
*Inactivos
	g yinactiv   =p7472s1 
*Otros Ingresos
	g yarrien	 = p7500s1a1
	g ypension	 = p7500s2a1
	g yjubila	 = p7500s3a1
	g yayudafam	 = p7510s1a1 / 12
	g yremesas	 = p7510s2a1 / 12
	g yayudainst = p7510s3a1 / 12
	g yintereses = p7510s5a1 / 12
	g ycesantia	 = p7510s6a1 / 12
*/

***************
***ylmpri_ci***
***************
	egen 	ylmpri_ci = rsum(impa impaes), m
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
	egen ylnmpri_ci = rsum(ie iees), m
	replace ylnmpri_ci=. if ie==. & iees==.
	/*YL -> Nota: "ie" and "iees"corresponden al ingreso por especie de la act principal*/
	la var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

***************
***ylmsec_ci***
***************
	egen ylmsec_ci = rsum(isa isaes), m
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
	egen ynlm_ci = rowtotal(iof1 iof2  iof3h iof3i iof6 iof1es iof2es  iof3hes iof3ies iof6es), m
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
	bys idh_ch: egen nrylmpri_ch = sum(nrylmpri_ci) if miembros_ci == 1
	replace nrylmpri_ch = 1 if nrylmpri_ch > 0 & nrylmpri_ch < .
	replace nrylmpri_ch = . if nrylmpri_ch == .
	la var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

*******************
****** ylm_ch *****
***** ylnm_ch *****
***** ynlm_ch *****
*******************

	foreach i in lm lnm nlm {
		bys idh_ch: egen y`i'_ch = sum(y`i'_ci) if miembros_ci == 1
	}
	la var ylm_ch 	"Ingreso laboral monetario del hogar"
	la var ylnm_ch 	"Ingreso laboral no monetario del hogar"
	la var ynlm_ch 	"Ingreso no laboral monetario del hogar"
	
****************
*** ylmnr_ch ***
****************
	bys idh_ch: egen ylmnr_ch = sum(ylm_ci) if miembros_ci == 1
	replace ylmnr_ch = . if nrylmpri_ch == 1
	la var ylmnr_ch "Ingreso laboral monetario del hogar"
	
**************
***ynlnm_ch***
**************
	g ynlnm_ch = .
	la var ynlnm_ch "Ingreso no laboral no monetario del hogar"

********
***NA***
********
	g rentaimp_ch = .
	la var rentaimp_ch "Rentas imputadas del hogar"

	g autocons_ci = .
	la var autocons_ci "Autoconsumo reportado por el individuo"

	g autocons_ch = .
	la var autocons_ch "Autoconsumo reportado por el hogar"

****************
***remesas_ci***
****************
	g remesas_ci = p7510s2a1/12 if p7510s2a1>9999 & p7510s2a1!=.
	la var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
***remesas_ch***
****************
	bys idh_ch: egen remesas_ch = sum(remesas_ci) if miembros_ci == 1
	la var remesas_ch "Remesas mensuales del hogar" 

*****************
***ylhopri_ci ***
*****************
	g ylmhopri_ci = ylmpri_ci / (horaspri_ci * 4.3)
	la var ylmhopri_ci "Salario monetario de la actividad principal" 

***************
***ylmho_ci ***
***************
	g ylmho_ci = ylm_ci / (horastot_ci * 4.3)
	la var ylmho_ci "Salario monetario de todas las actividades" 

			****************************
			***VARIABLES DE EDUCACION***
			****************************
			
**************
***aedu_ci****
**************

/* Missing values para no sabe no responde en maximo nivel educativo y  
   último anio aprobado.
*/			
    replace p6210s1=. if p6210s1==99
	replace p6210=.   if p6210==9
	replace p6220=. if p6220==9 

	g aedu_ci = . 
* 0 anios de educacion
	replace aedu_ci = 0 if p6210 == 1 | p6210 == 2  // Ninguno o prescolar.
	replace aedu_ci = 0 if p6210 == 3 & p6210s1 == 0 // Primaria cero anios.

*Primaria
	replace aedu_ci = 1 if p6210 == 3 & p6210s1 == 1
	replace aedu_ci = 2 if p6210 == 3 & p6210s1 == 2
	replace aedu_ci = 3 if p6210 == 3 & p6210s1 == 3
	replace aedu_ci = 4 if p6210 == 3 & p6210s1 == 4
	replace aedu_ci = 5 if p6210 == 3 & p6210s1 == 5
	replace aedu_ci = 5 if p6210 == 4 & p6210s1 == 0 // 0 anios secundaria.

*Secundaria
	replace aedu_ci = 6  if p6210 == 4 & p6210s1 == 6
	replace aedu_ci = 7  if p6210 == 4 & p6210s1 == 7
	replace aedu_ci = 8  if p6210 == 4 & p6210s1 == 8
	replace aedu_ci = 9  if p6210 == 4 & p6210s1 == 9
	replace aedu_ci = 10 if p6210 == 5 & p6210s1 == 10
	replace aedu_ci = 11 if p6210 == 5 & p6210s1 == 11
	replace aedu_ci = 11 if p6210 == 6 & p6210s1 == 0
	replace aedu_ci = 12 if p6210 == 5 & p6210s1 == 12
	replace aedu_ci = 13 if p6210 == 5 & p6210s1 == 13

*Superior
	replace aedu_ci = 11 + p6210s1 if p6210 == 6
	
* Missing 
	replace aedu_ci = . if p6210 == . 

**************
***eduno_ci***
**************

	g byte eduno_ci = (aedu_ci == 0)
	replace eduno_ci = . if aedu_ci == .
	la var eduno_ci "Sin educación"

**************
***edupi_ci***
**************

	g byte edupi_ci = (aedu_ci >= 1 & aedu_ci < 5) 
	replace edupi_ci = . if aedu_ci == .
	la var edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

	g byte edupc_ci = (aedu_ci == 5) 
	replace edupc_ci = . if aedu_ci == .
	la var edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

	g byte edusi_ci = (aedu_ci >= 6 & aedu_ci < 11) 
	replace edusi_ci = . if aedu_ci == .
	la var edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

	g byte edusc_ci = (aedu_ci == 11)
	replace edusc_ci = . if aedu_ci == . 
	la var edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************

* Se restringe por maximo nivel de titulación alcanzado
	g byte eduui_ci = (aedu_ci > 11 & p6220 < 3) 
	replace eduui_ci = . if aedu_ci == . 
	la var eduui_ci "Superior incompleto"

**************
***eduuc_ci***
**************

	g byte eduuc_ci = (aedu_ci > 11 & p6220 > 2)
	replace eduuc_ci = . if aedu_ci == .
	la var eduuc_ci "Superior completo"

***************
***edus1i_ci***
***************

	g byte edus1i_ci = (aedu_ci >= 6 & aedu_ci < 9)
	replace edus1i_ci = . if aedu_ci == .
	la var edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

	g byte edus1c_ci = (aedu_ci == 9)
	replace edus1c_ci = . if aedu_ci == . 
	la var edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

	g byte edus2i_ci = (aedu_ci == 10) 
	replace edus2i_ci = . if aedu_ci == . 
	la var edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

	g byte edus2c_ci = (aedu_ci == 11)
	replace edus2c_ci = . if aedu_ci == .
	la var edus2c_ci "2do ciclo de la secundaria completo"

***************
***edupre_ci***
***************

	g byte edupre_ci = .
	la var edupre_ci "Educación preescolar"

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
	g byte eduac_ci = .
	la var eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

	g asiste_ci = 1 if p6170 == 1
	replace asiste_ci = 0 if p6170 == 2
	la var asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

	g pqnoasis_ci = .
	la var pqnoasis_ci "Razones para no asistir a la escuela"
	
		
**************
*pqnoasis1_ci*
**************

	g pqnoasis1_ci = .

***************
***repite_ci***
***************

	g repite_ci = .
	la var repite_ci "Ha repetido al menos un grado"

******************
***repiteult_ci***
******************

	g repiteult_ci = .
	la var repiteult "Ha repetido el último grado"

***************
***edupub_ci***
***************

	g edupub_ci = 1 if p6175 == 1 & p6170 == 1
	replace edupub_ci = 0 if p6175 == 2 & p6170 == 1
	la var edupub_ci "Asiste a un centro de enseñanza público"

		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if p4030s5==1 
replace aguared_ch = 2 if p4030s5==2
la var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 1 if p5050==1
replace aguafconsumo_ch = 3 if p5050==10
replace aguafconsumo_ch = 4 if p5050==3
replace aguafconsumo_ch = 5 if p5050==5
replace aguafconsumo_ch = 6 if p5050==8
replace aguafconsumo_ch = 8 if p5050==6
replace aguafconsumo_ch = 10 if (p5050==4 | p5050==7)
replace aguafconsumo_ch = 7 if p5050==2
replace aguafconsumo_ch = 9 if (p5050==4 | p5050==9)
*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.
replace aguafuente_ch = 1 if p5050==1
replace aguafuente_ch = 3 if p5050==10
replace aguafuente_ch = 4 if p5050==3
replace aguafuente_ch = 5 if p5050==5
replace aguafuente_ch = 6 if p5050==8
replace aguafuente_ch = 8 if p5050==6
replace aguafuente_ch = 10 if (p5050==4 | p5050==7)
replace aguafuente_ch = 7 if p5050==2
replace aguafuente_ch = 9 if (p5050==4 | p5050==9)


*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch=1 if (p5050==1 | p5050==2)
replace aguadist_ch=2 if p5050==3
replace aguadist_ch=3 if p5050>=4


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
replace bano_ch=3 if p5020==4
replace bano_ch=4 if (p5020==3 | p5020==5)

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
replace banomejorado_ch =1 if bano_ch<=3
replace banomejorado_ch =0 if bano_ch>=4 & bano_ch!=6

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
	la var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************
	g luzmide_ch = .
	la var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
	g combust_ch = (p5080 == 1 | p5080 == 3 | p5080 == 4)
	replace combust_ch =. if p5080==.
	la var combust_ch "Principal combustible gas o electricidad" 

*************
***des1_ch***
*************
	g des1_ch = .
	replace des1_ch = 0 if bano_ch == 0
	replace des1_ch = 1 if p5020 == 1 | p5020 == 2
	replace des1_ch = 2 if p5020 == 3 | p5020 == 4
	replace des1_ch = 3 if p5020 == 5
	la var des1_ch "Tipo de desague inadecuado (unimproved) según MDG"
	la def des1_ch 	0 "No tiene servicio sanitario" 				///
					1 "Conectado a red general o cámara séptica" 	///
					2 "Letrina o conectado a pozo ciego" 			///
					3 "Desemboca en río o calle"
	la val des1_ch des1_ch

*************
***des2_ch***
*************
	g des2_ch = .
	replace des2_ch = 0 if bano_ch == 0
	replace des2_ch = 1 if p5020 == 1 | p5020 == 2 | p5020 == 3 | p5020 == 4
	replace des2_ch = 2 if p5020 == 5 
	la var des2_ch "Tipo de desague sin incluir definición MDG"
	la def des2_ch 	0 "No tiene servicio sanitario" 								///
					1 "Conectado a red general, cámara pséptica, pozo o letrina" 	///
					2 "Cualquier otro caso"
	la val des2_ch des2_ch

*************
***piso_ch***
*************
	g piso_ch = (p4020 != 1 & p4020 != .)
	replace piso_ch = . if p4020 == .
	la var piso_ch "Materiales de construcción del piso"  
	la def piso_ch 	0 "Piso de tierra" 			///
					1 "Materiales permanentes"
	la val piso_ch piso_ch

**************
***pared_ch***
**************

	g pared_ch = (p4010 >= 1 & p4010 <= 3)
	replace pared_ch = . if p4010 == .
	la var pared_ch "Materiales de construcción de las paredes"
	la de pared_ch 0"No permanentes" 1"Permanentes"
	la val pared_ch pared_ch

**************
***techo_ch***
**************
	g techo_ch = .
	la var techo_ch "Materiales de construcción del techo"

**************
***resid_ch***
**************
	g resid_ch = 0		 if p5040 == 1
	replace resid_ch = 1 if p5040 == 4
	replace resid_ch = 2 if p5040 == 2 | p5040 == 3
	replace resid_ch = 3 if p5040 == 5
	replace resid_ch = . if p5040 == .
	la var resid_ch "Método de eliminación de residuos"
	la de resid_ch 	0 "Recolección pública o privada" 	///
					1 "Quemados o enterrados" 			///
					2 "Tirados a un espacio abierto" 	///
					3 "Otros"
	la val resid_ch resid_ch
	

*************
***dorm_ch***
*************
	g dorm_ch = p5010
	la var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
	g cuartos_ch = p5000
	la var cuartos_ch "Habitaciones en el hogar"

***************
***cocina_ch***
***************

	g cocina_ch = 0 if p5070 >= 2 & p5070 <= 6
	replace cocina_ch = 1 if p5070 == 1
	la var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
	g telef_ch = p5210s1 == 1
	replace telef_ch = . if p5210s1 == .
	la var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************
	g refrig_ch = p5210s5 == 1
	replace refrig_ch = . if p5210s5 == .
	la var refrig_ch "El hogar posee refrigerador o heladera"

**************
***freez_ch***
**************
	g freez_ch = .
	la var freez_ch "El hogar posee congelador"

*************
***auto_ch***
*************
	g auto_ch = p5210s22 == 1
	replace auto_ch = . if p5210s22 == .
	la var auto_ch "El hogar posee automóvil particular"

**************
***compu_ch***
**************
	g compu_ch = p5210s16 == 1
	replace compu_ch = . if p5210s16 == .
	la var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************
	g internet_ch = p5210s3 == 1
	replace internet_ch = . if p5210s3 == . 
	la var internet_ch "El hogar posee conexión a Internet"

************
***cel_ch***
************
	g cel_ch = 0
	replace cel_ch = p5220==1
	replace cel_ch = . if p5220 == .
	la var cel_ch "El hogar tiene servicio telefónico celular"


**************
***vivi1_ch***
**************
	g vivi1_ch = 1     	 if p4000 == 1
	replace vivi1_ch = 2 if p4000 == 2
	replace vivi1_ch = 3 if p4000 == 3 | p4000 == 4 | p4000 == 5 | p4000 == 6
	replace vivi1_ch = . if p4000 == .
	la var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	la de vivi1_ch 	1 "Casa" ///
					2 "Departamento" ///
					3 "Otros"
	la val vivi1_ch vivi1_ch

**************
***vivi2_ch***
**************
	g vivi2_ch = (p4000 == 1 | p4000 == 2)
	replace vivi2_ch = . if p4000 == .
	la var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
	g viviprop_ch = 0 if p5090 == 3
	replace viviprop_ch = 1 if p5090 == 1
	replace viviprop_ch = 2 if p5090 == 2
	replace viviprop_ch = 3 if p5090 == 4 | p5090 == 5 | p5090 == 6
	replace viviprop_ch = . if p5090 == .
	la var viviprop_ch "Propiedad de la vivienda"
	la de viviprop_ch 	0 "Alquilada" 					///
						1 "Propia y totalmente pagada" 	///
						2 "Propia y en proceso de pago" ///
						3 "Ocupada (propia de facto)" 
	la val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************
	g vivitit_ch = .
	la var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
	g vivialq_ch = p5140 if p5140 >= 10000
	la var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
	g vivialqimp_ch = p5130 if p5130 >= 10000 
	la var vivialqimp_ch "Alquiler mensual imputado"

	gen  tcylmpri_ci =.
    gen tcylmpri_ch=.
	
*******************
*** benefdes_ci ***
*******************
*  No es seguro es subsidio
g benefdes_ci=0 if desemp_ci==1
replace benefdes_ci=1 if  p9460==1 & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci **
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(p6074==2 & p756==3) if p6074!=. & p756!=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & inlist(p755,2,3)) if migrante_ci!=. & p755!=1
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=(migrante_ci==1 & inlist(p755,2,3)) if migrante_ci!=. & p755!=1
	replace migrantiguo5_ci = 0 if p755 == 4 & migrante_ci==1 & migrante_ci!=. & p755!=1
	replace migrantiguo5_ci = . if migrante_ci==0
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"

******************************
* Variables SPH - PMTC y PNC *
******************************

* PTMC: Mis familias en acción
*		Jóvenes en acción
* PNC: 	Colombia mayor

* Ingreso del hogar
egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)
drop ingreso_total

* Personas que reciben transferencias monetarias condicionadas
**Se mensualizan las variables correspondientes**
gen ptmc1= p1661s1a1/12
gen ptmc2=p1661s2a1/12
egen aux = rowtotal(ptmc1 ptmc2), m
bys idh_ch: egen ing_ptmc = sum(aux)
drop aux ptmc1 ptmc2

gen percibe_ptmc_ci=(p1661s1==1| p1661s2 ==1)
bys idh_ch: egen ptmc_ch=max(percibe_ptmc_ci) // nivel hogar 
replace ptmc_ch  = ((percibe_ptmc_ci==1)| (ing_ptmc>0 & ing_ptmc!=.))
replace ing_ptmc=. if y_hog==.

* Adultos mayores 
gen mayor64_ci=(edad>64 & edad!=.)

* Personas que reciben pensiones
**Se mensualizan las variables correspondientes**
gen pens=p1661s3a1 /12
bys idh_ch: egen ing_pension = sum(pens)
gen pnc_ci=(p1661s3==1)

replace pnc_ci = ((pnc_ci==1) | (ing_pension>0 & ing_pension!=.))
replace ing_pension=. if y_hog==.

*ingreso neto del hogar
gen y_pc     = y_hog / nmiembros_ch 
gen y_pc_net = (y_hog - ing_ptmc - ing_pension) / nmiembros_ch

lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

lab def pnc_ci 1 "Beneficiario PNC" 0 "No beneficiario PNC"
lab val pnc_ci pnc_ci

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

*_____________________________________________________________________________________________________*

*  Pobres extremos, pobres moderados, vulnerables y no pobres 
* con base en ingreso neto (Sin transferencias)
* y líneas de pobreza internacionales
gen     grupo_int = 1 if (y_pc_net<lp31_ci)
replace grupo_int = 2 if (y_pc_net>=lp31_ci & y_pc_net<(lp31_ci*1.6))
replace grupo_int = 3 if (y_pc_net>=(lp31_ci*1.6) & y_pc_net<(lp31_ci*4))
replace grupo_int = 4 if (y_pc_net>=(lp31_ci*4) & y_pc_net<.)

tab grupo_int, gen(gpo_ingneto)

* Crear interacción entre recibirla la PTMC y el gpo de ingreso
gen ptmc_ingneto1 = 0
replace ptmc_ingneto1 = 1 if ptmc_ch == 1 & gpo_ingneto1 == 1

gen ptmc_ingneto2 = 0
replace ptmc_ingneto2 = 1 if ptmc_ch == 1 & gpo_ingneto2 == 1

gen ptmc_ingneto3 = 0
replace ptmc_ingneto3 = 1 if ptmc_ch == 1 & gpo_ingneto3 == 1

gen ptmc_ingneto4 = 0
replace ptmc_ingneto4 = 1 if ptmc_ch == 1 & gpo_ingneto4 == 1

lab def grupo_int 1 "Pobre extremo" 2 "Pobre moderado" 3 "Vulnerable" 4 "No pobre"
lab val grupo_int grupo_int


/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/
destring idh_ch, replace

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
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first



compress


saveold "`base_out'", replace


log close





