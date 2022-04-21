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
local ANO "1994"
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
Round: Abril, 1994
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 21 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*2014, 09 Mayra Sáenz revisar el factor de expansión, genra a más de 2000 individuos por hogar.
* ATENCION!!! la base original tiene problemas, no se cruzan condocup_ci y categopri_ci
****************************************************************************/


use `base_in', clear


qui destring _all, replace



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

***********
* INE01 *
************
gen ine01=  par

**************
* Región BID *
**************
gen region_BID_c=.
replace region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*************************
*  Factor de expansión  *
*************************

gen factor_ch=1
label var factor_ch "Factor de expansion del hogar"


**************************
* Identificador del hogar*
**************************

ren  hhld idh_ch

****************************
* Identificador de persona *
****************************

ren indv idp_ci


***************************
* Zona urbana o zona rural*
***************************

gen zona_c=.


**********************************
* País***************************

gen pais_c="JAM"


****************************
* Anio de la encuesta ******

gen anio_c=1994

***********************
*  Mes de la encuesta *
***********************

gen mes_c=4


*******************************
*Relación con el jefe de hogar*
*******************************


/*
reln


           1 head
           2 spouse
           3 child
           4 child_spouse
           5 grand_child
           6 parent
           7 other_rel
           8 helper
           9 non_relative


*/

gen relacion_ci=.
replace relacion_ci=1 if reln==1
replace relacion_ci=2 if reln==2
replace relacion_ci=3 if reln==3
replace relacion_ci=4 if reln==4 |reln==5 | reln==6 | reln==7 
replace relacion_ci=5 if reln==9 
replace relacion_ci=6 if reln==8

****************************************
*Factor de expansión a nivel individual*
****************************************

gen factor_ci=factor_ch

********
* Sexo *
********

ren sex sexo_ci

******
*Edad*
******

ren age edad_ci

**************
*Estado civil*
**************

gen civil_ci=.

**************
*Raza*
**************

gen raza_ci=.
*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

***************
*Jefe de hogar*
***************

gen jefe_ci=(relacion_ci==1)



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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"



***************************************
*Ocupación laboral actividad principal*
***************************************
gen ocupa=string(q38)
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=. if ocupa_ci==0
drop ocupa

**********************************
*Rama laboral actividad principal*
**********************************

gen rama1=string(q39)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 
drop rama1



****************************************************
*Horas totales trabajadas en la actividad principal*
****************************************************

/*Sólo se pregunta cuantas horas se trabaja usualmente a la semana. No se pregunta por
actividad principal sino por todo. No es posible identificar las horas dedicadas a la actividad principal*/
gen horaspri_ci=.

**********************************************************
***Horas totales trabajadas en la actividad secundaria.***
**********************************************************
* No distingue actividad secundaria
gen horassec_ci=.
label var horassec_ci "Horas trabajadas semanalmente en el trabajo secundario"

***************************************************
*Horas totales trabajadas en todas las actividades*
***************************************************

gen horastot_ci=.
replace horastot_ci=(1+8)/2 if q33==1
replace horastot_ci=(9+16)/2 if q33==2
replace horastot_ci=(17+24)/2 if q33==3
replace horastot_ci=(25+32)/2 if q33==4
replace horastot_ci=(33+40)/2 if q33==5
replace horastot_ci=(41+49)/2 if q33==6
replace horastot_ci=(49+56)/2 if q33==7
replace horastot_ci=. if horastot_ci==99

label var horastot_ci "Horas totales trabajadas en todas las actividades"

***********************************************
*Ingreso laboral monetario actividad principal*
***********************************************


gen ylmpri_ci=.


**************************************************
*Ingreso laboral no monetario actividad principal*
**************************************************

gen ylnmpri_ci=.


************************************************
*Ingreso laboral monetario actividad secundaria*
************************************************

gen ylmsec_ci=.


***************************************************
*Ingreso laboral no monetario actividad secundaria*
***************************************************

gen ylnmsec_ci=.

******************************************
*Ingreso laboral monetario otros trabajos*
******************************************

gen ylmotros_ci=.


*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*********************************************************************
*Identificador de No Respuesta del ingreso de la actividad principal*
*********************************************************************

gen nrylmpri_ci=.

*****************************************************************
*Identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*****************************************
*Ingreso laboral monetario mensual total*
*****************************************

gen ylm_ci=.

replace ylm_ci=q324a if q324b==4
replace ylm_ci=q324a*4.2 if q324b==1
replace ylm_ci=q324a/12 if q324b==7
label var ylm_ci "Ingreso laboral monetario total"


************************************
*Ingreso laboral no monetario total*
************************************

gen ylnm_ci=.

******************************
*Ingreso no laboral monetario*
******************************
recode q324c (9999999=.)
gen ynlm_ci=.
replace ynlm_ci=q324c if q324d==4
replace ynlm_ci=q324c*4.2 if q324d==1
replace ynlm_ci=q324c/12 if q324d==7
label var ynlm_ci "Ingreso no laboral monetario (otras fuentes)"


*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario"


**********************************************************************
*Identificador de los hogares en donde alguno de los miembros no sabe*
*	       no responde el ingreso de la actividad principal          *
**********************************************************************

gen nrylmpri_ch=.

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.


*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

*************
*ylmhopri_ci*
*************

gen ylmphopri_ci=.

**********
*ylmho_ci*
**********

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)


*************
*rentaimp_ch*
*************

gen rentaimp_ch=.

*************
*autocons_ci*
*************

gen autocons_ci=.

*************
*autocons_ch*
*************

gen autocons_ch=.



************
*remesas_ci*
************

gen remesas_ci=.

************
*remesas_ch*
************

gen remesas_ch=.

*************
*ylmhopri_ci*
*************

gen ylmhopri_ci=.
label var ylmhopri_ci "Salario  monetario de la actividad principal"

***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 

***************
*** ylnm_ch ***
***************

gen ylnm_ch=.
label var ylnm_ch  "Ingreso laboral no monetario del Hogar"

************
*durades_ci*
************

gen durades_ci=.
replace durades_ci=0.5 if q41==1
replace durades_ci=(1+3)/2 if q41==2
replace durades_ci=(3+6)/2 if q41==3
replace durades_ci=(6+9)/2 if q41==4
replace durades_ci=(9+12)/2 if q41==5
replace durades_ci=(12+24)/2 if q41==6 | q41==7 | q41==8




***************
*antiguedad_ci*
***************
gen antiguedad_ci=.

replace antiguedad_ci=0.5/12 if q310==1
replace antiguedad_ci=((1+3)/2)/12 if q310==2
replace antiguedad_ci=((3+6)/2)/12 if q310==3
replace antiguedad_ci=((6+9)/2)/12 if q310==4
replace antiguedad_ci=((9+12)/2)/12 if q310==5
replace antiguedad_ci=((12+24)/2)/12 if q310>=6 & q310!=.

************************
*Condición de ocupación*
************************
*2014, 01 MLO cambio del limite de edad de condocup_ci a 10+
gen condocup_ci =.
* REVISAR SINTAXIS, ESTA MAL- NO HAY DICCIONARIO
/*
replace condocup_ci=1 if q211==1 | q211==2 |  q22==1  | q23==1 | q32==1
replace condocup_ci=2 if (q211==3 | (q43>=1 & q43<=7) |q44==3 | q44==6 | (q41>=1 & q41<=6) |  (q42 >=1 & q42 <=7)) & condocup_ci==.
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2) 
*replace condocup_ci=4 if edad_ci<14 
replace condocup_ci=4 if edad_ci<10 */

label define condocup_ci 1 "Ocupados" 2 "Desocupados" 3 "Inactivos" 4 "Menor que 14", modify
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
gen lp_ci=.
*gen lp_ci=22637.75 if area==1
*replace lp_ci=21574.68 if area==2
*replace lp_ci=20089.57 if area==3
label var lp_ci "Linea de pobreza local (Adulto Equivalente)"

*********
*lpe_ci***
*********
gen lpe_ci=.
*gen lpe_ci=. if area==1
*replace lpe_ci=. if area==2
*replace lpe_ci=. if area==3
label var lpe_ci "Linea de pobreza extrema local (Adulto Equivalente)"

*********
*salmm_ci***
*********
gen salmm_ci=.
label var salmm_ci "Salario Minimo en dolares de Jamaica"

/*
********
*emp_ci*
********

  
*Generalmente se utiliza la pregunta q21, pero esta 
* base de datos no tiene esa variable. Se utiliza la q32
* que se refiere a que si ha trabajado regularmente los 'ultimos 5 días.

gen emp_ci=1 if q32==1 |  q22==1  | q23==1 
replace emp_ci =0 if emp_ci==. 

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
*Esta variable toma valores de 7 u 8, pero esos
* no están en la codificación.
*La última categoría es 6 que indica de 12 meses o más.
gen desemp3_ci=1 if (q41>=1 & q41<7) | desemp2_ci==1
replace desemp3_ci=0 if desemp3_ci==.
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"
*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1


*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea2_ci=1 if emp_ci==1 |desemp2_ci==1

*************
***pea3_ci***
*************
gen pea3_ci=1 if emp_ci==1 |desemp3_ci==1
*/

*************
*desalent_ci*
*************
gen desalent_ci=1 if q44==1 | q44==4
replace desalent_ci=0 if desalent_ci==.
label var desalent_ci "Trabajadores desalentados"

***********
*subemp_ci*
***********

gen subemp_ci=1 if q34==1 & horastot_ci<30
replace subemp_ci=0 if subemp_ci==.
***************
*tiempoparc_ci*
***************

gen tiempoparc_ci=1 if horastot_ci<30 & q35==3
replace tiempoparc_ci=0 if tiempoparc_ci==.
**************
*categopri_ci*
**************
/*
 local govt. employe					1
other govt agencies employee			2
private sector employee					3
unpaid family worker					4
employer								5
own account worker						6
not stated								9
*/

gen categopri_ci=.
replace categopri_ci=1 if  q322==5
replace categopri_ci=2 if  q322==6
replace categopri_ci=3 if  q322==1 | q322==2 | q322==3
replace categopri_ci=4 if  q322==4

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

**************
*categosec_ci*
**************

gen categosec_ci=.

*************
*contrato_ci*
*************

gen contrato_ci=.

***********
*segsoc_ci*
***********

gen segsoc_ci=.

*************
*nempleos_ci*
*************

gen nempleos_ci=.
/*
*************
*firmapeq_ci*
*************

gen firmapeq_ci=.
label var firmapeq_ci "Trabajadores formales"
*/
*************
*spublico_ci*
*************
gen spublico_ci=.
replace spublico_ci=1 if  q322==1 |  q322==2
replace spublico_ci=0 if spublico_ci==.
label var spublico_ci "Personas que trabajan en el sector público"

************************
*tamemp_ci*
************************

gen tamemp_ci=.
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

************************
*categoinac_ci*
************************

gen categoinac_ci = 1 if (q55 ==7 & condocup_ci==3)
replace categoinac_ci = 2 if (q211 == 5 & condocup_ci==3)
replace categoinac_ci = 3 if (q211 == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************

* no hay informacion para crear la variable
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"


*********
*aedu_ci*
*********

*ocupados
gen aedu_ci=.
replace aedu_ci=q319 if q319<99
replace aedu_ci=q320 if q320>0 & q320<99
   
*desocupados
replace aedu_ci=q421 if q421<99
replace aedu_ci=q422 if q422>0 & q422<99

*inactivos
replace aedu_ci=q514 if q514<99
replace aedu_ci=q515 if q515>0 & q515<99
label var aedu_ci "Años de educación"

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
evidencian individuos que no terminan la secundaria y responden degree.

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
*Generalmente se utiliza la q21 pero en esta base no hay la variable
* que se refiere a las actividades realizadas el mes anterior.
gen asiste_ci=.
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



compress


saveold "`base_out'", replace


log close
