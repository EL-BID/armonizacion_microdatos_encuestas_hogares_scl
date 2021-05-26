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

local PAIS JAM
local ENCUESTA LFS
local ANO "2003"
local ronda m4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Jamaica
Encuesta: LFS
Round: Abril, 2003
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
****************************************************************************/


use `base_in', clear

* **Inclusión Mayra Sáenz - Julio 2013- Cambiar los nombres a minúsculas
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}


qui destring _all, replace

*====================================================================================================================================*
*                                                    VARIABLES DEL HOGAR                                                             *
*====================================================================================================================================*

*************************
*  factor de expansión  *
*************************

* El rfact1 es el que expande a la población a términos reales.
* El rfact2/3 son de otras secciones de la encuesta.
gen factor_ch=rfact1
label var factor_ch "Factor de expansion del hogar"



***********
* Region_c *
************
*Modificación Mayra Sáenz - Julio 2013
gen region_c=  par

label define region_c  ///
           1 "Kingston" ///
           2 "St Andrew" ///
           3 "St Thomas" ///
           4 "Portland" ///
           5 "St Mary" ///
           6 "St Ann" ///
           7 "Trelawny" ///
           8 "St James" ///
           9 "Hanover" ///
          10 "Westmoreland" ///
          11 "St Elizabeth" ///
          12 "Manchester" ///
          13 "Clarendon" ///
          14 "St Catherine"
	    
label value region_c region_c
label var region_c "División política, parroquias"

**************
* Región BID *
**************
gen region_BID_c=.
replace region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


**************************
* identificador del hogar*
**************************

ren  hhold idh_ch
label var idh_ch "ID del hogar"

****************************
* identificador de persona *
****************************

ren indiv idp_ci
label var idp_ci "ID de la persona en el hogar"


***************************
* zona urbana 1 o zona rural 0*
***************************
gen zona_c=1 if ur1 == 1 | ur1 == 2
replace zona_c = 0 if ur1 ==3

**********************************
* país***************************

gen pais_c="JAM"
label variable pais_c "Pais"

****************************
* anio de la encuesta ******

gen anio_c=2003
label variable anio_c "Anio de la encuesta"


***********************
*  mes de la encuesta *
***********************

gen mes_c=4
label variable mes_c "Mes de la encuesta"
label define mes_c 4 "Abril" 
label value mes_c mes_c


*******************************
*relación con el jefe de hogar*
*******************************

*Modificado Mayra Sáenz Julio 2013
*Antes no se incluía la categoría 4, pero se debe incluir en otros parientes ya que hace referencia al
* yerno o nuera del jefe de hogar.
/*
relat


head			1
spouse			2
child			3
child's spouse	4
grandchild		5
parent hd/sp	6
other relative	7
helper			8
other			9

*/

destring relat, replace
gen relacion_ci=.
replace relacion_ci=1 if relat==1
replace relacion_ci=2 if relat==2
replace relacion_ci=3 if relat==3
replace relacion_ci=4 if relat==4 |relat==5 | relat==6 | relat==7 
replace relacion_ci=5 if relat==9 
replace relacion_ci=6 if relat==8

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

*====================================================================================================================================*
*                                                          VARIABLES DEMOGRAFICAS                                                    *
*====================================================================================================================================*

****************************************
*factor de expansión a nivel individual*
****************************************

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"
********
* sexo *
********

ren sex sexo_ci
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci


******
*edad*
******

gen  edad_ci = age
replace edad_ci=. if age==98 | age==99 
label variable edad_ci "Edad del individuo"

**************
*estado civil*
**************

gen civil_ci=.
label variable civil_ci "Estado civil"

**************
*Raza*
**************

gen raza_ci=.
*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

***************
*jefe de hogar*
***************

gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"


******************
***nconyuges_ch***
******************

by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Número de conyuges"

***************
***nhijos_ch***
***************

by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Número de hijos"

******************
***notropari_ch***
******************

by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Número de otros familiares"

********************
***notronopari_ch***
********************

by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Número de no familiares"


****************
***nempdom_ch***
****************

by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Número de empleados domésticos"

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

label variable clasehog_ch "tipo de hogar"
label define clasehog_ch 1 " unipersonal" 2 "nuclear" 3 "ampliado" 
label define clasehog_ch 4 "compuesto" 5 " corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Número de familiares en el hogar"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "miembro del hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Número de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Número de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Número de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Número de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Número de familiares menores a 1 anio"



*====================================================================================================================================*
*                                                          VARIABLES DEL MERCADO LABORAL                                              *
*====================================================================================================================================*
************************
*Condición de ocupación*
************************
/*
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
gen condocup_ci =.
*Ocupado*
replace condocup_ci=1 if q21==1 | q21==2 |  q22==1  | q23==1 
*Desocupado*
replace condocup_ci=2 if (q21==3 | (q43>=1 & q43<=7) |q44==3 | q44==6 | (q41>=1 & q41<=6) |  (q42 >=1 & q42 <=7)) & condocup_ci==.
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) 
*menores que PET
*replace condocup_ci=4 if edad_ci<14 
replace condocup_ci=4 if edad_ci<10 

label define condocup_ci 1"Ocupados" 2"Desocupados" 3"Inactivos" 4"Menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
*Se modifico segun la edad minima de la encuesta de 14 anios y la sintaxis. MGD 06/10/2014
gen condocup_ci =.
replace condocup_ci=1 if q21==1 | q21==2 |  q22==1  | q23==1
replace condocup_ci=2 if condocup_ci!=1 & (((q22==2  | q23==2) & q21==3) | (q24==1 & (q25==1 | q25==2)))  
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) & edad_ci>=14
replace condocup_ci=4 if edad_ci<14 
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
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

***********
* Cesante *
***********
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if q45==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

****************
*cotizando_ci***
****************
gen cotizando_ci=.

label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

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

**************
**ypen_ci*
**************
gen ypen_ci=. /* No preguntan ingresos por esta fuente */
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

***************
***tecnica_ci**
***************
gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"

***************
***pension_ci**
***************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*********
*lp_ci***
*********
gen lp_ci=57475.52 if ur1==1
replace lp_ci=54776.47 if ur1==2
replace lp_ci=51005.90 if ur1==3
label var lp_ci "Linea de pobreza local (Adulto Equivalente)"

*********
*lpe_ci***
*********
gen lpe_ci=35850.42 if ur1==1
replace lpe_ci=35277.04 if ur1==2
replace lpe_ci=34937.63 if ur1==3
label var lpe_ci "Linea de pobreza extrema local (Adulto Equivalente)"

*********
*salmm_ci***
*********
gen salmm_ci=2400*4.3
label var salmm_ci "Salario Minimo en dolares de Jamaica"

/*
********
*emp_ci*
********

gen emp_ci=1 if q22==1 | q21==1 | q21==2 | q23==1 
replace emp_ci =0 if emp_ci==. 
label var emp_ci "Ocupado (empleado)"

************
*desemp1_ci*
************

gen desemp1_ci=1 if q43==1
replace desemp1_ci=0 if desemp1_ci==.
label var desemp1_ci "Desempleado que buscó empleo en el periodo de referencia"
************
*desemp2_ci*
************

gen desemp2_ci=1 if desemp1_ci==1 | q44==3 | q44==6
replace desemp2_ci=0 if desemp2_ci==.
label var desemp2_ci "desemp1_ci + personas que esperan respuesta a solicitud o temporada alta"
************
*desemp3_ci*
************

gen desemp3_ci=1 if (q41>=1 & q41<9) | desemp2_ci==1
replace desemp3_ci=0 if desemp3_ci==.
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"
*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1
label var pea1_ci "Poblacion economicamente activa utilizando la definicion 'desemp1'"


*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1
label var pea2_ci "Población Económicamente Activa con desemp2_ci"


*************
***pea3_ci***
*************
gen pea3_ci=1 if emp_ci==1 |desemp3_ci==1
label var pea3_ci "Población Económicamente Activa con desemp3_ci"
*/
*************
*desalent_ci*
*************

gen desalent_ci=1 if q44==1 | q44==4
replace desalent_ci=0 if desalent_ci==.
label var desalent_ci "Trabajadores desalentados"

****************************************************
*horas totales trabajadas en la actividad principal*
****************************************************

/*sólo se pregunta cuantas horas se trabaja usualmente a la semana. no se pregunta por
actividad principal sino por todo. no es posible identificar las horas dedicadas a la actividad principal*/
gen horaspri_ci=.
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

**********************************************************
***Horas totales trabajadas en la actividad secundaria.***
**********************************************************
* No distingue actividad secundaria
gen horassec_ci=.
label var horassec_ci "Horas trabajadas semanalmente en el trabajo secundario"


***************************************************
*horas totales trabajadas en todas las actividades*
***************************************************

gen horastot_ci=q33
replace horastot_ci=. if horastot_ci==99
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"


***********
*subemp_ci*
***********

gen subemp_ci=1 if q34==1 & q33<30
replace subemp_ci=0 if subemp_ci==.
label var subemp_ci "Personas en subempleo por horas"

***************
*tiempoparc_ci*
***************

gen tiempoparc_ci=1 if q33<30 & q34==3
replace tiempoparc_ci=0 if tiempoparc_ci==.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

**************
*categopri_ci*
**************
/*
q323
 local govt. employe					1
other govt agencies employee			2
private sector employee					3
unpaid family worker					4
employer								5
own account worker						6
not stated								9
*/

gen categopri_ci=.
replace categopri_ci=1 if q323==5
replace categopri_ci=2 if q323==6
replace categopri_ci=3 if q323==1 | q323==2 | q323==3
replace categopri_ci=4 if q323==4

label define categopri_ci 1"Patron" 2"Cuenta propia" 
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

gen contrato_ci=.
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

***********
*segsoc_ci*
***********

gen segsoc_ci=.
label var segsoc_ci "Personas que tienen seguridad social en salud por su trabajo"

*************
*nempleos_ci*
*************

gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if q37>=2 & q37<.
label var nempleos_ci "Número de empleos" 
/*
*************
*firmapeq_ci*
*************

gen firmapeq_ci=.
replace firmapeq_ci=1 if q324==1 | q324 ==2
replace firmapeq_ci=0 if (q324>=3 & q324<=5)
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores formales"
*/

*************
*spublico_ci*
*************
/*
q323
 local govt. employe					1
other govt agencies employee			2
private sector employee					3
unpaid family worker					4
employer								5
own account worker						6
not stated								9
*/
gen spublico_ci=.
replace spublico_ci=1 if q323==1 | q323==2
replace spublico_ci=0 if spublico_ci==.
label var spublico_ci "Personas que trabajan en el sector público"


***************************************
*ocupación laboral actividad principal*
***************************************
gen ocupa=string(q38m)
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=. if ocupa_ci==0
drop ocupa
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


**********************************
*rama laboral actividad principal*
**********************************
/*
gen rama1=string(q39m)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 
replace rama_ci=. if emp_ci==0
drop rama1
*/
g rama_ci=.
replace rama_ci=1 if (q39m>=100 & q39m<=999) & emp_ci==1
replace rama_ci=2 if (q39m>=1000 & q39m<=1999) & emp_ci==1
replace rama_ci=3 if (q39m>=2000 & q39m<=3499) & emp_ci==1
replace rama_ci=4 if (q39m>=4000 & q39m<=4599) & emp_ci==1
replace rama_ci=5 if (q39m>=5000 & q39m<=5999) & emp_ci==1
replace rama_ci=6 if (q39m>=6000 & q39m<=6999) & emp_ci==1
replace rama_ci=7 if (q39m>=7000 & q39m<=7499) & emp_ci==1
replace rama_ci=8 if (q39m>=8000 & q39m<=8499) & emp_ci==1
replace rama_ci=9 if (q39m>=8300 & q39m<=9990) & emp_ci==1

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


************
*durades_ci*
************

/*
pregunta q41

1. Less than 1 month; 
2.1m but less than 3m; 
3. 3m but less than 6m; 
4. 6m but less than 9m; 
5. 9m but less than 12 m; 
6. 12 m but less than 24m; 
7. 2 years and over y 
99. Not stated

*/


************
*durades_ci*
************
* MGD04/09/2015: cambian las categorias.
gen durades_ci=.
replace durades_ci=0.5 if q41==1
replace durades_ci=(1+3)/2 if q41==2
replace durades_ci=(3+6)/2 if q41==3
replace durades_ci=(6+9)/2 if q41==4
replace durades_ci=(9+12)/2 if q41==5
replace durades_ci=(12+24)/2 if q41==6
replace durades_ci=(24+36)/2 if q41==7




***************
*antiguedad_ci*
***************

* Correccion MGD 7/30/2014: ae cambiaron los promedios de acuerdo a la temporalidad correcta de la variable.
gen antiguedad_ci=.
replace antiguedad_ci=((1+3)/2)/12 if q311==1
replace antiguedad_ci=((3+6)/2)/12 if q311==2
replace antiguedad_ci=((6+9)/2)/12 if q311==3
replace antiguedad_ci=((9+12)/2)/12 if q311==4
replace antiguedad_ci=((12+24)/2)/12 if q311==5
replace antiguedad_ci=((24+60)/2)/12 if q311==6
replace antiguedad_ci=((60+72)/2)/12 if q311==7


************************
*tamemp_ci*
************************

gen tamemp_ci=1 if q324==1 | q324==2
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if q324==3 
*Empresas grandes
replace tamemp_ci=3 if q324==4 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=rfact1]

************************
*categoinac_ci*
************************

gen categoinac_ci = 1 if (q55 ==7 & condocup_ci==3)
replace categoinac_ci = 2 if (q21 == 5 & condocup_ci==3)
replace categoinac_ci = 3 if (q21 == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"


*******************
***formal***
*******************

* no hay informacion para crear la variable
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"

                    **************
					***INGRESOS***
					**************


***********************************************
*ingreso laboral monetario actividad principal*
***********************************************

/*no hay manera de encontrar ingreso de actividad principal, secundaria. solamente el total. 
tampoco se pregunta por pago en especie.*/

gen ylmpri_ci=.
label var  ylmpri_ci "Ingreso laboral monetario actividad principal"

**************************************************
*ingreso laboral no monetario actividad principal*
**************************************************

gen ylnmpri_ci=.
label var ylnmpri "Ingreso laboral no monetario act. principal"

*********************************************************************
*identificador de no respuesta del ingreso de la actividad principal*
*********************************************************************

gen nrylmpri_ci=.
label var nrylmpri_ci "Identificador de No Respuesta (NR) del ingreso de la actividad principal"


************************************************
*ingreso laboral monetario actividad secundaria*
************************************************

gen ylmsec_ci=.
label var ylmsec_ci "Ingreso laboral monetario actividad secundaria"


***************************************************
*ingreso laboral no monetario actividad secundaria*
***************************************************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"


*****************************************
*ingreso laboral monetario mensual total*
*****************************************
gen ylm_ci=.
replace ylm_ci=q325e if q325a==4
replace ylm_ci=q325e*4.2 if q325a==1
replace ylm_ci=q325e/12 if q325a==7
label var  ylm_ci "Ingreso laboral monetario mensual total"

************************************
*ingreso laboral no monetario total*
************************************

gen ylnm_ci=.
label var ylnm_ci "Ingreso laboral no monetario total"

******************************************
*ingreso laboral monetario otros trabajos*
******************************************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario otros trabajos"

*********************************************
*ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral no monetario otros trabajos"

******************************
*ingreso no laboral monetario*
******************************

gen ynlm_ci=.
replace ynlm_ci=q325o if q325b==4
replace ynlm_ci=q325o*4.2 if q325b==1
replace ynlm_ci=q325o/12 if q325b==7
label var ynlm_ci "Ingreso no laboral monetario (otras fuentes)"
 

*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario"

************
*remesas_ci*
************

gen remesas_ci=.
label var remesas_ci "Remesas reportadas por el individuo"


**********************************************************************
*identificador de los hogares en donde alguno de los miembros no sabe*
*	       no responde el ingreso de la actividad principal          *
**********************************************************************

gen nrylmpri_ch=.
label var nrylmpri_ch "Id. hogar si algun miembro no sabe o no respond el ingreso act. principal"

**************************************************
*identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del Hogar"


***********
**ynlm_ch**
***********

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del Hogar"

***************
*** ylnm_ch ***
***************

gen ylnm_ch=.
label var ylnm_ch  "Ingreso laboral no monetario del Hogar"

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del Hogar considera No respuesta"

************
*remesas_ch*
************

gen remesas_ch=.
label var remesas_ch "Remesas del Hogar"

***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 

*************
*rentaimp_ch*
*************

gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"


*************
*autocons_ch*
*************

gen autocons_ch=.
label var autocons_ch "Autoconsumo del Hogar"

*************
*autocons_ci*
*************

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

*************
*ylmhopri_ci*
*************

gen ylmhopri_ci=.
label var ylmhopri_ci "Salario  monetario de la actividad principal"

**********
*ylmho_ci*
**********

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario  monetario de todas las actividades"




*====================================================================================================================================*
*                                                   VARIABLES DE EDUCACIÓN
*====================================================================================================================================*	

/*
__________________________________________________________________________________________________________

+++++++++++++++++++++++++++++++++++++++++++++++++++++++
NOTA METODOLÓGICA DEL CÁLCULO DE EDUCACIÓN EN JAMAICA
+++++++++++++++++++++++++++++++++++++++++++++++++++++++
Mayra Sáenz-Julio 2013

*Años de educacion adicional a primaria.
En Jamaica el sistema de Educación es distinto al resto de América Latina.

*La primaria tiene seis años (1-6 grado).

*La secundaria puede extenderse a más de 7 años y se divide en lower level (7-9 grado) y 
upper level (10-11 grado) y para aprobar la secundaria deben rendir determinados exámenes que
se encuentran en distintas categorías.

Así, CXC basic, JSC 5 SSC;  CXC Gen, GCE 'O' 1-2 ;  CXC Gen, GCE 'O' 3-4;  CXC Gen, GCE 'O' 5+
Estos cuatro exámenes corresponden a los Caribbean Examination Council's O'Level (Nivel Ordinario) school leaving
examinations - Basic or General Proficiency levels. Los cuales están calificados del 1 al 6, en donde 1=
pasar con distinción, 2= pasar con créditos , 3= pasar con nivel satisfactorio, 4 o más = 'basic level' pass.

Al finalizar el grado 11 pueden optar por extender su educación secundaria hasta dos años más. Esta extensión
se denomina 'Sixth Form', la misma que se divide en upper sixth (grado 13) y lower sixth (grado 12).
Esta extensión también se conoce como 'advanced post secondary program', al final del cual se debe rendir los 
exámenes CAPE (Caribbean Advanced Proficiency Exams), los mismos que equivalen a los GCE (General Certificate
Education) A-level examinations que eran estándar hasta el año 2003.

* La educación terciaria completa sólo puede ser alcanzada en la universidad y corresponde a los que declaran 
´degree´ en la pregunta: What is the highest academic examination that you have / has passed.

Además, esta base considera sólo a las personas mayores de 15 años.
__________________________________________________________________________________________________________
*/

*********
*aedu_ci*
*********

gen aedu_ci=.
* años de primaria de los ocupados/desocupados/inactivos
replace aedu_ci= 1 if q320 == 2 | q421== 2 | q514==2
replace aedu_ci= 2 if q320 == 3 | q421== 3 | q514==3
replace aedu_ci= 3 if q320 == 4 | q421== 4 | q514==4
replace aedu_ci= 4 if q320 == 5 | q421== 5 | q514==5
replace aedu_ci= 5 if q320 == 6 | q421== 6 | q514==6
replace aedu_ci= 6 if q320 == 7 | q421== 7 | q514==7

* años de secundaria de los ocupados/desocupados/inactivos
replace aedu_ci= 7 if q321 == 2 | q422== 2 | q515==2
replace aedu_ci= 8 if q321 == 3 | q422== 3 | q515==3
replace aedu_ci= 9 if q321 == 4 | q422== 4 | q515==4
replace aedu_ci= 10 if q321 == 5 | q422== 5 | q515==5
replace aedu_ci= 11 if q321 == 6 | q422== 6 | q515==6
replace aedu_ci= 12 if q321 == 7 | q422== 7 | q515==7
replace aedu_ci= 13 if q321 == 8 | q422== 8 | q515==8

label var aedu_ci "Años de educación"

gen exam = .
replace exam = 1 if  (q322==1) | (q423==1) | (q516 ==1)
replace exam = 2 if  (q322==2) | (q423==2) | (q516 ==2)
replace exam = 3 if  (q322==3) | (q423==3) | (q516 ==3)
replace exam = 4 if  (q322==4) | (q423==4) | (q516 ==4)
replace exam = 5 if  (q322==5) | (q423==5) | (q516 ==5)
replace exam = 6 if  (q322==6) | (q423==6) | (q516 ==6)
replace exam = 7 if  (q322==7) | (q423==7) | (q516 ==7)
replace exam = 8 if  (q322==8) | (q423==8) | (q516 ==8)
replace exam = 9 if  (q322==9) | (q423==9) | (q516 ==9)
replace exam = 99 if  (q322==99) | (q423==99) | (q516 ==99)
recode exam (99=.)
 
label define exam 1 "none" 2 "CXC basic, JSC 5 SSC" 3 "CXC Gen, GCE 'O' 1-2" 4 "CXC Gen, GCE 'O' 3-4" ///  
  5 "CXC Gen, GCE 'O' 5+" 6 "GCE 'A' 1-2label defi" 7 "CAPElabel define GCE" 8 "Degree" 9 "other" ///
  99 "not stated"
label value exam exam
label var exam "Highest (academic) examination"


**********
*eduno_ci*
**********

gen eduno_ci=1 if aedu_ci==0
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.
label variable eduno_ci "Cero anios de educacion"

**********
*edupi_ci*
**********
gen edupi_ci=0 if aedu_ci!=.
replace edupi_ci=1 if (aedu_ci >=1 & aedu_ci<6)
label variable edupi_ci "Primaria incompleta"


**********
*edupc_ci*
**********
gen edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if edupc_ci==. & aedu_ci != .
label variable edupc_ci "Primaria completa"

**********
*edusi_ci*
**********
gen edusi_ci=0 if aedu_ci!=.
replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<11)
label variable edusi_ci "Secundaria incompleta"

**********
*edusc_ci*
**********
/*
Para el caso de Jamaica, tendrán secundaria completa los que tengan más de once años de educación
pero que NO declaren tener título universitario (serían los de terciaria completa). Además. que
hayan rendido los exámenes de culminación de secundaria ya sea cualquiera de las ordinarias CXC gen, GCE 'O'
o las avanzadas GCE 'A' , CAPE. Por otro lado, tendrán cero los que tengan menos de once años de educación.

A PESAR DE QUE ESTA SINTAXIS ES MAS PRECISA, AL HACER UN TAB NO COINCIDE CON EL NÙMERO DE CASOS DE LA VARIABLE
DE AÑOS DE ESCOLARIDAD.

gen edusc_ci=1 if ((aedu_ci>=11)&(exam!=8))& aedu_ci!=. 
replace edusc_ci=1 if (exam >=2 & exam<=7) & ((aedu_ci>=11)& aedu_ci!=.) & edusc_ci !=1
replace edusc_ci=0 if (aedu_ci<11) & (edusc_ci == .)
label variable edusc_ci "Secundaria completa"
*/
gen edusc_ci=1 if aedu_ci>=11 & aedu_ci!=.
replace edusc_ci=0 if aedu_ci<11
label variable edusc_ci "Secundaria completa"


**********
*edus1i_ci*
***********
gen edus1i_ci=0 if aedu_ci!=.
replace edus1i_ci=1 if (aedu_ci>=7 & aedu_ci<9)& aedu_ci!=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***********
*edus1c_ci*
***********
gen edus1c_ci=0 if aedu_ci!=.
replace edus1c_ci=1 if aedu_ci==9 & aedu_ci!=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***********
*edus2i_ci*
***********
gen edus2i_ci=0 if aedu_ci!=.
replace edus2i_ci=1 if aedu_ci==10 
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***********
*edus2c_ci*
***********
gen edus2c_ci=0 if aedu_ci!=.
replace edus2c_ci=1 if aedu_ci>=11 & aedu_ci!=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**********
*eduui_ci*
**********

gen eduui_ci=.
label variable eduui_ci "Superior incompleto"
**********
*eduuc_ci*
**********
/* Se podría considerar a los que respondieron degree, sin embargo, al evaluar esta variable con ese criterio se 
evidencian individuos que no acaban ni la secundaria y responden degree.

gen eduuc_ci=0 if aedu_ci!=.
replace eduuc_ci=1 if exam == 8
label variable eduuc_ci "Superior completo"
*/

gen eduuc_ci=.
label variable eduuc_ci "Superior completo"


************************
***Educacion preescolar.
************************
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************************************************************************
***Educación terciaria académica versus educación terciaria no-académica***
***************************************************************************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************************************************************************
***Personas que actualmente asisten a centros de enseñanza.
***************************************************************************
gen asiste_ci=1 if q21==5
replace asiste_ci=0 if asiste_ci==.
label variable asiste_ci "Asiste actualmente a la escuela"

***************************************************************************
***Razones para no asistir a la escuela.***
***************************************************************************
gen pqnoasis_ci=.
label variable pqnoasis_ci  " Razón por que no asiste a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

******************************************************
*Personas que han repetido al menos un año o grado.***
******************************************************

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado o año"

******************************************************
***Personas que han repetido el ultimo grado.
******************************************************

gen repiteult_ci=.
label var repite_ci "Personas que han repetido el último grado"

********************************************************
***Personas que asisten a centros de enseñanza publicos.
********************************************************
gen edupub_ci=.
label var edupub_ci "Personas asisten a centros de enseñanza públicos"





/*sólo es posible determinar los anios de educación hasta finalizar la secundaria. posteriormente se pregunta sobre 
los exámenes cxc pero no es posible determinar el nivel educativo con estos


fin encuesta lfs
*/

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
	
	*Esta base de datos no tiene módulo de vivienda.
	
	****************
	***aguared_ch***
	****************
	gen aguared_ch=.
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
	label var luz_ch "La principal fuente de iluminación es electricidad"	
	
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
	
	gen bano_ch=.
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
	
	gen des2_ch=.
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
	
	*************
	***piso_ch***
	*************
	gen piso_ch=.
	label var piso_ch "Materiales de construcción del piso"	
	
	**************
	***pared_ch***
	**************
	gen pared_ch=.
	label var pared_ch "Materiales de construcción de las paredes"	
	
	**************
	***techo_ch***
	**************
	
	gen techo_ch=.
	label var techo_ch "Materiales de construcción del techo"	
	
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
      g       aguamejorada_ch = .
		
      *********************
      ***banomejorado_ch***
      *********************
      g       banomejorado_ch = .
	 
	*************
	***dorm_ch***
	*************
	
	gen dorm_ch=.
	label var dorm_ch "Habitaciones para dormir"	
	
	
	****************
	***cuartos_ch***
	****************
	
	gen cuartos_ch=.
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
	
	**************
	***vivi2_ch***
	**************
	
	gen vivi2_ch=.
	label var vivi2_ch "La vivienda es casa o departamento"
	
	
	*****************
	***viviprop_ch***
	*****************
	gen viviprop_ch=.
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
	gen vivialq_ch=.
	label var vivialq_ch "Alquiler mensual"

	*******************
	***vivialqimp_ch***
	*******************
	gen vivialqimp_ch=.
	label var vivialqimp_ch "Alquiler mensual imputado"

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para anÃ¡lisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
clonevar  codocupa = q38m
clonevar codindustria = q39m

compress


saveold "`base_out'", replace


log close



















































