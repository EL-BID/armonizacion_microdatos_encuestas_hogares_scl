
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

local PAIS GTM
local ENCUESTA ENEI
local ANO "2002"
local ronda m10_m11 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: m10_m11
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

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
*** region_c **
***************
gen region_c=.
label var region_c "Division politico-administrativa"

***************
***factor_ch***
***************

gen factor_ch= factorh
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

gen idh_ch = hogar
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=id
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
/*
              1    Urbano metropolitano
              2    Resto urbano
              3    Resto rural

*/			  
gen byte zona_c=0 if dominio==3
replace zona_c=1 if dominio==1 | dominio ==2

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


************
****pais****
************

gen str3 pais_c="GTM"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2002
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=11 
label variable mes_c "Mes de la encuesta"



*****************
***relacion_ci***
*****************
/*
p03a02    RELACION DE PARENTESCO                                             13
              1    Jefe o jefa del hogar
              2    Esposo (a)  o  compañero (a)
              3    Hijo o hija
              4    Yerno o nuera
              5    Nieto o nieta
              6    Padre o madre
              7    Suegro o suegra
              8    Hermano o hermana
              9    Cuñado o cuñada
             10    Otro pariente
             11    Empleado (a)  o  doméstica (a)
             12    Pensionista o huesped
             13    Otro no pariente
*/


gen relacion_ci=1 if p03a02==1
replace relacion_ci=2 if p03a02==2
replace relacion_ci=3 if p03a02==3 
replace relacion_ci=4 if p03a02>=4 & p03a02<=10
replace relacion_ci=5 if p03a02==12 | p03a02==13
replace relacion_ci=6 if p03a02==11

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci


	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************

gen factor_ci=factorpe
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=ppa02
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=ppa03
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************
/*

p03a03    ESTADO CIVIL O CONYUGAL                                            14
              1    Unido o unida
              2    Casado o casada
              3    Separado (a) de matrimonio
              4    Separado (a) de union
              5    Divorciado o divorciada
              6    Viudo o viuda
              7    Soltero o soltera
 
*/


gen civil_ci=.
replace civil_ci=1 if p03a03==7
replace civil_ci=2 if p03a03==1 | p03a03==2 
replace civil_ci=3 if p03a03==3 | p03a03==4 | p03a03==5
replace civil_ci=4 if p03a03==6

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

gen miembros_ci=(relacion_ci<6)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña


/*           1    "Kiché" 
              2    "Qeqchí" 
              3    "Kaqchikel" 
              4    "Mam" 
              5    "Garifuna" 
              6    "Ladino" 
              7    "Extranjero" 
              8    "Achi" 
              9    "Acateco" 
             10    "Awacateco"
             11    "Qanjobal" 
             12    "Chorti" 
             13    "Chuj" 
             14    "Jacalteco" 
             15    "Pocomchi" 
             16    "Pocomam" 
             17    "Tzutujil" 
             18    "Xinca" 
             19    "Popti" 
             20    "Calchiteco" 
             21    "Ixil" 
			 */

gen raza_ci=.
replace raza_ci= 1 if (p03a04 >=1 & p03a04 <=4) | (p03a04 >=8 & p03a04 <=98) 
replace raza_ci= 2 if (p03a04 ==5) & raza_ci==.
replace raza_ci= 3 if (p03a04 ==6 | p03a04 ==7) & raza_ci==.
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"		 
			 
gen raza_aux=.
replace raza_aux= 1 if (p03a04 >=1 & p03a04 <=4) | (p03a04 >=8 & p03a04 <=98) 
replace raza_aux= 4 if (p03a04 ==5) & raza_aux==.
replace raza_aux= 3 if (p03a04 ==7| p03a04==6) & raza_aux==.
bys idh_ch: gen aux=raza_aux if p03a02==1
bys idh_ch: egen aux1 = max(aux)
replace raza_aux=aux1 if (raza_aux ==. & (p03a02 ==3 | p03a02==5))  
replace raza_aux=3 if raza_aux==. 
drop aux aux1
label define raza_aux 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 4 "Afro-indígena"
label value raza_aux raza_aux 
label value raza_aux raza_aux
label var raza_aux "Raza o etnia del individuo"

gen raza_idioma_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_aux==1 | raza_aux==4
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_aux==2 | raza_aux==4
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 
			 

	************************************
	*** VARIABLES DEL MERCADO LABORAL***
	************************************
*************
**salmm_ci***
*************

*1 = GUA 2002
gen salmm_ci= 	862.5

label var salmm_ci "Salario minimo legal"



*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

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

****************
*instcot_ci*****
****************
gen instcot_ci=p05a25
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if (trabaja==1)
replace condocup_ci=2 if (bttotal==1 | btpasf==1) & trabaja!=1
recode condocup_ci .=3 if inactivo==1
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci
*/

* No se utilizan variables creadas sino las originales de la base para homologar con la serie anterior. 05/19/2014 MGD
* Toma en cuentra trabajo+unque no trabajoa tiene trabajo y busqueda.
* No hay menores porque la base esta solo para individuos >= a 7 anios.

* MGR: Modifico serie en base a correcciones Laura Castrillo: delimitar la condición de edad para que no tome los missing en caso que existan
gen condocup_ci=.
replace condocup_ci=1 if (p04a02==1 | p04a06==1)
replace condocup_ci=2 if (p04a02==2 & p04a06==2) & (p04a08==1 | p04a09==1)
recode condocup_ci .=3 if edad_ci>=7 & edad_ci!=.
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if p05a25==1
recode afiliado_ci .=0 if p05a25!=1
replace afiliado_ci=0 if condocup_ci==2
label var afiliado_ci "Afiliado a la Seguridad Social"

*************
*cesante_ci* 
*************
* Correccion MGD 07/17/2014: mal generada la variable, se consideraba a inactivos p07a01 y no desempleados.
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if p06a06==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"


*************
**pension_ci*
*************

gen pension_ci=1 if p10a02b>0 & p10a02b!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=p10a02b/3 if p10a02b>0 & p10a02b!=.

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"

*****************
***desalent_ci***
*****************

gen desalent_ci=(p04a11 ==2 | p04a11 ==3)
replace desalent_ci=. if p04a11 ==.
label var desalent_ci "Trabajadores desalentados"

 ***************
 ***subemp_ci***
 ***************    
 * Horas trabajadas a la semana en el primer empleo
 /*
Lunes     p05a31a         
Martes    p05a31b        
Miércoles p05a31c      
Jueves    p05a31d       
Viernes   p05a31e        
Sábado    p05a31f          
Domingo   p05a31g          
 */       
 *Total de horas a la semana en la actividad principal
 egen horpri = rsum(p05a31a  p05a31b  p05a31c p05a31d p05a31e p05a31f p05a31g) if emp_ci==1, missing
 
 *Total de horas a la semana en la actividad secundaria
 gen horsec = p05b09 if emp_ci==1

egen tothoras=rowtotal(horpri horsec)
replace tothoras=. if horpri==. & horsec==. 
replace tothoras=. if tothoras>=168

* Modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). MGD 06/19/2014
g subemp_ci=0 
replace subemp_ci=1 if emp_ci==1 & (horpri>=1 & horpri<=30) & p05d01==1 & p05d05==1
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************

gen horaspri_ci=horpri
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

* MGR: Modifico serie en base a correcciones Laura Castrillo: se debe utilizar horaspri en lugar de horastot como había sido generada antes
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30) &  emp_ci==1 & p05d01==2
replace tiempoparc_ci=. if emp_ci!=1 | horaspri_ci==. 
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if p05a08 ==6 
replace categopri_ci=2 if p05a08 == 5 
replace categopri_ci=3 if (p05a08 ==1 | p05a08 ==2 | p05a08 ==3 | p05a08 ==4) 
replace categopri_ci=4 if (p05a08 ==7 | p05a08 ==8) 


label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

/*
p05a08    CATEGORIA OCUPACIONAL                                              41
              1    Empleado (a) del gobierno
              2    Empleado (a) privado (a)
              3    Jornalero (a) o peon
              4    Empleado (a) doméstico (a)
              5    Trabajador (a) por cuenta propia
              6    Patrón (a) empleador (a) socio (a)
              7    Trabajador familiar sin pago
              8    Trabajador no familiar sin pago
*/


******************
***categosec_ci***
******************

/*
p05c06     CATEGORIA OCUPACIONAL                                            
              1    Empleado (a) del gobierno
              2    Empleado (a) privado (a)
              3    Jornalero (a) o peon
              4    Empleado (a) doméstico (a)
              5    Trabajador (a) por cuenta propia
              6    Patrón (a) empleador (a) socio (a)
              7    Trabajador familiar sin pago
              8    Trabajador no familiar sin pago
*/




gen categosec_ci=.
replace categosec_ci=1 if p05c06  ==6 
replace categosec_ci=2 if p05c06  == 5
replace categosec_ci=3 if (p05c06  ==1 | p05c06  ==2 | p05c06  ==3 | p05c06  ==4) 
replace categosec_ci=4 if (p05c06  ==7 | p05c06  ==8) 


label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p05a10a ==1 & p05a09 ==1) & categopri_ci==3
replace tipocontrato_ci=2 if (p05a10a ==2 & p05a09 ==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p05a09 ==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & p05a01==1
replace nempleos_ci=2 if emp_ci==1 & (p05a01==2 | p05a01==3)
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci= .
replace firmapeq_ci= 1 if p05a30==1 |  p05a30==2
replace firmapeq_ci = 0 if p05a30>=3 &  p05a30<=7
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************
gen spublico_ci=(p05a08 ==1) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************
/*
p05a02    OCUPACIÓN A 2 DIGITOS                                              35
              1    Fuerzas Armadas
             11    Miembros de la Admon. Pública y Gerentes de Empresas
             12    Directores de Empresa
             13    Gerentes de Empresa
             21    Profesionales de las Ciencias Físicas y Afines
             22    Prof. de las Ciencias Biológicas, la Medicina y la Salud
             23    Profesionales de la Enseñanza
             24    Especialistas en Admon., Admon. de empresas y afines
             31    Tec. y Prof. de las Ciencias Físicas, Quím e Ing
             32    Profesionales de la Medicina y la Salud
             33    Instructores
             34    Otros Técnicos y Profesionales
             41    Oficinistas
             42    Empleados de trato directo con el público
             51    Trab. de Servicios Personales, de Proteccion y Seguridad
             52    Modelos, Vendedores y demostradores
             61    Agricultores y trab. calificados agropecuarios y pesqueros
             62    Trabajadores Agropecuarios y Pesqueros de subsistencia
             63    Agricultores de cultivo de café
             64    Administrador de finca de café
             71    Oficiales y oper. de indus. extractivas y de la construc.
             72    Oficiales y oper. de la metalurgia, la construc. Mecánica
             73    Mecánicos de precisión, artesanos y oper. de artes gráf
             74    Operarios y artesanos de artes mecánicas y otros oficios
             75    Supervisores de línea en maquiladora
             76    Operarios de maquiladora
             81    Operador de instalaciones fijas y afines
             82    Operador de máquinas y montadores
             83    Conductor de vehículos y oper. de equipos pesados móviles
             91    Trabajadores no calificados de ventas y servicios
             92    Peones agropecuarios, forestales, pesqueros y afines
             93    Peones mineros, de la cons. la ind. manufac. y el trans
             94    Peones agrícolas del café.
             99    Ocupación no bien especificada

*/

gen ocupa_ci=.
replace ocupa_ci=1 if (p05a02 >=21 & p05a02 <=35) & emp_ci==1
replace ocupa_ci=2 if (p05a02 >=11 & p05a02 <=19) & emp_ci==1
replace ocupa_ci=3 if (p05a02 >=41 & p05a02 <=49) & emp_ci==1
replace ocupa_ci=4 if (p05a02 ==52 | p05a02 ==95) & emp_ci==1
replace ocupa_ci=5 if (p05a02 ==51 | p05a02 ==53 | p05a02 ==50 | p05a02 ==54 | p05a02 ==91) & emp_ci==1
replace ocupa_ci=6 if ((p05a02 >=61 & p05a02 <=67) | p05a02 ==92) & emp_ci==1
replace ocupa_ci=7 if ((p05a02 >=71 & p05a02 <=86) | p05a02 ==93) & emp_ci==1
replace ocupa_ci=8 if (p05a02 >=1 & p05a02 <=3) & emp_ci==1
replace ocupa_ci=9 if (p05a02 ==94 | p05a02 ==96 | p05a02 ==99) & emp_ci==1
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
replace rama_ci = 1 if p05a03>=1 & p05a03<=5
replace rama_ci = 2 if p05a03>=10 & p05a03<=14
replace rama_ci = 3 if p05a03>=15 & p05a03<=37
replace rama_ci = 4 if p05a03>=40 & p05a03<=41
replace rama_ci = 5 if p05a03==45
replace rama_ci = 6 if p05a03>=50 & p05a03<=55
replace rama_ci = 7 if p05a03>=60 & p05a03<=64
replace rama_ci = 8 if p05a03>=65 & p05a03<=74
replace rama_ci = 9 if p05a03>=75 & p05a03<=99

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

************
*durades_ci*
************
*Cuántas semanas hace que dejó de trabajar
* MGD 04/02/2015: la variable correcta es p06a02 cuantas semanas lleva buscando trabajo
gen durades_ci=.
*replace durades_ci= (p06a07/4.3)
replace durades_ci= (p06a02/4.3) 
replace durades_ci= 0.23 if p06a02==0   
label variable durades_ci "Duracion del desempleo en meses"


*******************
***antiguedad_ci***
*******************
gen antiguedad_ci= p05a35
replace antiguedad_ci=. if emp_ci==0
replace antiguedad_ci=. if p05a35==. 
label var antiguedad_ci "Antiguedad en la actividad actual en anios"


*******************
***tamemp_ci***
*******************
/*
p05a30:
           1 1 persona
           2 de  2  a  5
           3 de  6  a  10
           4 de  11  a  30
           5 de  31  a  50
           6 de  51  a  100
           7 de  101  a  más
*/

*Guatemala Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if p05a30==1 | p05a30==2
replace tamemp_ci = 2 if (p05a30>=3 & p05a30<=5)
replace tamemp_ci = 3 if (p05a30>=6 & p05a30<=7)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************
/*

p04a02    QUE HIZO LA SEMANA  PASADA                                         
              1    Trabajar
              2    Buscar trabajo
              3    Estudiar
              4    Quehaceres del hogar
              5    Incapacitado
              6    Jubilado o pensionado
              7    Rentista
              8    Enfermos / convalecientes
             98    Otra actividad
*/



gen categoinac_ci =1 if (p04a02==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (p04a02==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (p04a02==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

************************************************************************
**************************INGRESOS**************************************
************************************************************************

local var = "p05a14 p05a15b p05a16b p05a17b p05a18c p05a19b p05a20b  p05a21b p05b05 p05b06b p05b07b p10b01b p10b02b p10b03b p10b04b p05a23 p05b08 p10a01b p10a03b p10a04b p10a05b p10a06b p10a07b p10a08b p10a09b p10a10b p10d01b" 
foreach x of local var {
recode `x'  (99999=.) (999999=.)
}


*Ingresos del trabajo principal
gen ymensual= 	p05a14
gen bono14 = p05a15b/12
gen aguinaldo=p05a16b/12
gen vacaciones = p05a17b/12

*Ingresos en especies trabajo principal
gen ropa = (p05a18c*p05a18b)/12
gen alimentos=p05a19b
gen vivienda1=p05a20b
gen transporte=p05a21b 
  
* Ingresos actividad secundaria
gen ymensual2=p05b05 
gen prestaciones2= p05b06b
*Bonos, aguinaldo, vacaciones
gen bonaguivac= p05b07b/12

*Otros ingresos del trabajo distintos a los declarados
gen incentivo =p10b01b/12
gen vtas =p10b02b/12
gen otrostrabajos =p10b03b/12
gen otrostrabajos2 =p10b04b/12

*Ingreso mensual neto act. principal
gen ymensualneto= p05a23
*Ingreso mensual neto act. secundaria
gen ymensualneto2= p05b08

*Ingresos distintos al trabajo (en el último trimestre)
gen alquiler = p10a01b/3
gen jubilacion = p10a02b/3
gen ayudas = p10a03b/3
gen remesas1 = p10a04b/3
gen intereses = p10a05b/3
gen beca= p10a06b /3
gen pension = p10a07b /3
gen indemni = p10a08b/3
gen herencia = p10a09b/3
gen bonos = p10a10b/3

*Autoconsumo
gen autocons= p10d01b 




***************
***ylmpri_ci***
***************
egen ylmpri_ci=rsum(ymensual bono14 aguinaldo vacaciones), missing
replace ylmpri_ci=. if ymensual==. & bono14==.& aguinaldo==. & vacaciones==. 
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

egen ylnmpri_ci=rsum(ropa alimentos vivienda1 transporte), missing
replace ylnmpri_ci=. if alimentos==. & vivienda1==. & transporte==. & ropa==. 
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   



***************
***ylmsec_ci***
***************

egen ylmsec_ci= rsum(ymensual2 prestaciones2 bonaguivac) if emp_ci==1 , missing
replace ylmsec_ci=. if (ymensual2==. & prestaciones2==. & bonaguivac==.) & emp_ci==1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************
egen ylmotros_ci=rsum(incentivo vtas otrostrabajos otrostrabajos2), missing
replace ylmotros_ci =. if (incentivo==. & vtas==. & otrostrabajos==. & otrostrabajos2==.)
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


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(alquiler jubilacion ayudas remesas1 intereses beca pension indemni herencia bonos), missing
replace ynlm_ci=. if alquiler==. & jubilacion==. & ayudas==. & remesas1==. & intereses==. & beca==. & pension==. & indemni==. & herencia==. & bonos==.
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
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

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
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

gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

gen autocons_ci=autocons
label var autocons_ci "Autoconsumo reportado por el individuo"

by idh_ch, sort: egen numper=sum(miembros_ci) if miembros_ci==1, missing
gen autocons_ch= autocons/numper
label var autocons_ch "Autoconsumo reportado por el hogar"



****************
***remesas_ci***
****************
gen remesas_ci=remesas1
label var remesas_ci "Remesas mensuales reportadas por el individuo" 


****************
***remesas_ch***
****************

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
	
	/*
p03a09a NIVEL EDUCATIVO MAS ALTO APROBADO                                  
              1    Ninguno
              2    Preparatoria
              3    Educación  adultos
              4    Primaria
              5    Secundaria
              6    Superior
              7    Postgrado
             98    Otro,  ¿cual ?
p03a09b   GRADO MAS ALTO APROBADO                                          
       */
	

*************
***aedu_ci*** 
*************
gen aedu_ci=.
replace	 aedu_ci=0  if (p03a09a==1) 

*Primaria
replace aedu_ci=1  if (p03a09a==4 & p03a09b==1)
replace aedu_ci=2  if (p03a09a==4 & p03a09b==2)
replace aedu_ci=3  if (p03a09a==4 & p03a09b==3)
replace aedu_ci=4  if (p03a09a==4 & p03a09b==4)
replace aedu_ci=5  if (p03a09a==4 & p03a09b==5)
replace aedu_ci=6  if (p03a09a==4 & p03a09b==6)


*Secundaria
replace aedu_ci=7  if (p03a09a==5 & p03a09b==1) 
replace aedu_ci=8 if (p03a09a==5 & p03a09b==2) 
replace aedu_ci=9 if (p03a09a==5 & p03a09b==3) 
replace aedu_ci=10 if (p03a09a==5 & p03a09b==4) 
replace aedu_ci=11 if (p03a09a==5 & p03a09b==5) 
replace aedu_ci=12 if (p03a09a==5 & p03a09b==6) 

*Superior
replace aedu_ci=13 if (p03a09a==6 & p03a09b==1)
replace aedu_ci=14 if (p03a09a==6 & p03a09b==2)
replace aedu_ci=15 if (p03a09a==6 & p03a09b==3)
replace aedu_ci=16 if (p03a09a==6 & p03a09b==4)
replace aedu_ci=17 if (p03a09a==6 & (p03a09b==5 | p03a09b==6))

*Postgrado
replace aedu_ci=18 if p03a09a==7 & p03a09b==1 
replace aedu_ci=19 if p03a09a==7 & p03a09b==2 
replace aedu_ci=20 if p03a09a==7 & p03a09b==3 
replace aedu_ci=21 if p03a09a==7 & p03a09b==4
replace aedu_ci=22 if p03a09a==7 & (p03a09b==5 | p03a09b==6)
replace aedu_ci=.  if p03a09a==.

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
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<12) 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12 
label variable edusc_ci "Secundaria completa"


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
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edus1i_ci***
***************
gen edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>6 & aedu_ci<9)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************

gen byte edupre_ci= .
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

* Modificaciones Marcela Rubio Septiembre 2014: variable ha sido generada incorrecamente

/*
generat asiste_ci=.
replace asiste_ci=1 if p03a07==1 
replace asiste_ci=0 if p03a07==2
label variable asiste_ci "Asiste actualmente a la escuela"
*/

gen asiste_ci=0 if ppa03>=7 & ppa03~=. 
replace asiste_ci=1 if p03a07==1
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=p03a08
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1    "Enfermedad" ///
              2    "Falta de maestro" ///
              3    "Oficios de la casa" ///
              4    "Huelga magisterial" ///
              5    "Falta de dinero" ///
              6    "Trabajo" ///
              7    "No le interesa" ///
              8    "Mal tiempo" ///
              9    "Embarazo" ///
             10    "Migración temporal" ///
             11    "No tiene quien lo lleve" ///
             12    "Cerró pensum" ///
             13    "Problemas personales o familiares" ///
             98    "Otra causa"
label val pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==5
replace pqnoasis1_ci= 2 if  pqnoasis_ci==6
replace pqnoasis1_ci= 3 if  pqnoasis_ci==13 | pqnoasis_ci==1
replace pqnoasis1_ci= 4 if  pqnoasis_ci==7
replace pqnoasis1_ci= 5 if  pqnoasis_ci==3 | pqnoasis_ci==9
replace pqnoasis1_ci= 6 if  pqnoasis_ci==12
replace pqnoasis1_ci= 8 if  pqnoasis_ci==11 
replace pqnoasis1_ci= 9 if  pqnoasis_ci==98 | pqnoasis_ci==10 | pqnoasis_ci==8 | pqnoasis_ci==4 | pqnoasis_ci==2

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
gen edupub_ci=.
label var edupub_ci "Asiste a un centro de enseñanza público"

*************
**tecnica_ci*
*************
gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************


****************
***aguared_ch***
****************
gen aguared_ch=.
replace aguared_ch=(p02a03a==1)
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
gen luz_ch=(p02a03b==1)
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch=.
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************
gen bano_ch=(p02a03c ==1)
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
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

* MGR Jul, 2015. Variable había sido generada como missing, puede generarse parcialmente 
gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if p02a03c==1
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

gen piso_ch=.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch

**************
***pared_ch***
**************

gen pared_ch=.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch


**************
***techo_ch***
**************

gen techo_ch=.
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes"
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
g       aguamejorada_ch = 1 if p02a03a ==1 // Esta pregunta no es comparable con el resto de anios porque no desagrega las categorias de respuesta
replace aguamejorada_ch = 0 if p02a03a ==2
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if p02a03c == 1 // Esta pregunta no es comparable con el resto de anio porque no desagrega las categorias de respuesta
replace banomejorado_ch = 0 if p02a03c == 2

 
*************
***dorm_ch***
*************
gen dorm_ch=p02a06
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************
* Se considera la pregunta que se refiere al número de cuartos a disposición del hogar.
gen cuartos_ch=p02a05
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************
gen telef_ch=(p02a03d ==1)
label var telef_ch "El hogar tiene servicio telefónico fijo"


***************
***refrig_ch***
***************

gen refrig_ch=.
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

gen compu_ch=.
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************

gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"


************
***cel_ch***
************

gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefonico celular"



**************
***vivi1_ch***
**************

gen vivi1_ch=.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

gen vivi2_ch=.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************
/*
P02A01    Forma de tenencia de la vivienda                                   12
              1    Propia totalmente pagada
              2    Propia pagandose a plazos
              3    Heredada o donada
              4    Alquilada?
              5    Cedida o prestada?
             98    Otra forma, ¿cual?
*/

gen viviprop_ch=0 if p02a01==4
replace viviprop_ch=1 if p02a01==1
replace viviprop_ch=2 if p02a01==2
replace viviprop_ch=3 if p02a01==3 | p02a01==5
replace viviprop_ch=. if p02a01==. 
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2 "Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch


****************
***vivitit_ch***
****************
/*
 
P02A02    Documento de propiedad de la vivienda                              13
              1    Ninguno
              2    Recibo o factura
              3    Escritura o título sin registrar
              4    Escritura en trámite
              5    Título registrado
              6    Documento o posesión
              7    Documento o acta municipal
*/
gen vivitit_ch= (p02a02 ~=1)  
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch = . 
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_idioma_ci  id_ind_ci id_afro_ci raza_ci  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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
