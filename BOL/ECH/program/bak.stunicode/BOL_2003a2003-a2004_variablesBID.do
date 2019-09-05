
* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
global ruta = "\\Sdssrv03\surveys"

local PAIS BOL
local ENCUESTA ECH
local ANO "2003"
local ronda a2003-a2004 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\\survey\BOL\ECH\2003_2004\a2003_a2004\data_orig\BOL_2003_2004a2003-a2004.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: a2003_a2004 
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 4 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=3

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************

gen factor_ch=.
replace factor_ch = fe4_red
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************
sort folio
tostring folio,g(temp)
gen idh_ch = temp
label variable idh_ch "ID del hogar"
drop temp

*************
****idp_ci****
**************

gen idp_ci=nro1
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=0 	if urb_rur==2
replace zona_c=1 	if urb_rur==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


**************
***region_c***
**************

gen region_c=.
label var region_c "Region" 


************
****pais****
************

gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2003
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=11
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*
relacion:
	1	jefe o jefa del hogar
	2	esposa/o o conviviente
	3	hijo/a o entenado/a
	4	yerno o nuera
	5	hermano/a o cuñado/a
	6	padres o suegros
	8	nieto/a
	9	otros parientes
	10	otros no parientes
	11	empleada del hogar

*/

gen relacion_ci=.
replace relacion_ci=1 if relacion==1
replace relacion_ci=2 if relacion==2
replace relacion_ci=3 if relacion==3
replace relacion_ci=4 if relacion>=4 & relacion<=9
replace relacion_ci=5 if relacion==10 
replace relacion_ci=6 if relacion==11

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

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci = a1_02

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci= a1_03
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************

/*
a1_12:
           1 soltero(a)
           2 casado(a)
           3 conviviente /concubino
           4 separado(a)
           5 divorciado(a)
           6 viudo(a)
*/

gen civil_ci=.
replace civil_ci=1 		if a1_12==1
replace civil_ci=2 		if a1_12==2 | a1_12==3
replace civil_ci=3 		if a1_12==4 | a1_12==5
replace civil_ci=4 		if a1_12==6

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

*************
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

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .

gen raza_ci=.
replace raza_ci= 1 if  (a1_11 >=1 & a1_11 <=6)
replace raza_ci= 1 if (a1_08==1 | a1_08==2 | a1_08== 4 |a1_08== 5) & raza_ci==.
replace raza_ci= 3 if (a1_11  ==7) 
replace raza_ci= 3 if (a1_08==3 | a1_08==6 | a1_08== 7 | a1_08== 8)& raza_ci==.
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 



************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 
 

*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= 339.818367483678 if ciudad==1 & urb_rur==1
replace lp_ci= 340.954350982598 if ciudad==2 & urb_rur==1
replace lp_ci= 355.707495807127 if ciudad==3 & urb_rur==1
replace lp_ci= 310.042906501388 if ciudad==4 & urb_rur==1
replace lp_ci= 285.153467025503 if ciudad==5 & urb_rur==1
replace lp_ci= 355.707495807127 if ciudad==6 & urb_rur==1
replace lp_ci= 357.028886351092 if ciudad==7 & urb_rur==1
replace lp_ci= 357.028886351092 if ciudad==8 & urb_rur==1
replace lp_ci= 284.222365593378 if ciudad==10 & urb_rur==1
replace lp_ci= 357.028886351092 if ciudad==9 & urb_rur==1

replace lp_ci=  270.53   if  urb_rur==2




label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 171.608275579257 if ciudad==1 & urb_rur==1
replace lpe_ci= 189.570619146325 if ciudad==2 & urb_rur==1
replace lpe_ci= 179.632285382599 if ciudad==3 & urb_rur==1
replace lpe_ci= 172.383856014772 if ciudad==4 & urb_rur==1
replace lpe_ci= 158.54532766618 if ciudad==5 & urb_rur==1
replace lpe_ci= 179.632285382599 if ciudad==6 & urb_rur==1
replace lpe_ci= 181.370674266355 if ciudad==7 & urb_rur==1
replace lpe_ci= 181.370674266355 if ciudad==8 & urb_rur==1
replace lpe_ci= 172.52297591518 if ciudad==10 & urb_rur==1
replace lpe_ci= 181.370674266355 if ciudad==9 & urb_rur==1

replace lpe_ci=    154.20   if  urb_rur==2



label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 2003
gen salmm_ci= 	440.00


label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if  a4_22a==1 | (a4_34a  >0 & a4_34a  !=.)
recode cotizando_ci .=0 if condact>=2 & condact<=4
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci= 1 if  a4_46b ==1	
*replace afiliado_ci =1 if s5_71b==1
recode afiliado_ci .=0 if condact>=2 & condact<=4
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.


replace tipopen_ci=1 if a5_01c2>0 &  a5_01c2~=.
replace tipopen_ci=2 if a5_01f2>0 & a5_01f2~=.
replace tipopen_ci=3 if a5_01d2>0 & a5_01d2~=.
replace tipopen_ci=4 if a5_01e2>0 & a5_01e2~=. 
replace tipopen_ci=12 if (a5_01c2>0 & a5_01f2>0) & (a5_01c2~=. & a5_01f2~=.)
replace tipopen_ci=13 if (a5_01c2>0 & a5_01d2>0) & (a5_01c2~=. & a5_01d2~=.)
replace tipopen_ci=23 if (a5_01f2>0 & a5_01d2>0) & (a5_01f2~=. & a5_01d2~=.)
replace tipopen_ci=123 if (a5_01c2>0 & a5_01f2>0 & a5_01d2>0) & (a5_01c2~=. & a5_01f2~=. & a5_01d2~=.)
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
gen instcot_ci=. 



****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if condact==2
replace condocup_ci=2 if condact==3 | condact==4
replace condocup_ci=3 if (condact==5 | condact==6) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Homologacion toda la serie 05/22/2014 MGD

gen condocup_ci=.

*Mod. MLO 2015,10: se consideran otras causas excepcionales 
*replace condocup_ci=1 if a4_01==1 | a4_02<=6  | a4_03==1
replace condocup_ci=1 if a4_01==1 | a4_02<=6 | (a4_03>=1 & a4_03<=7)
*replace condocup_ci=2 if (a4_01==2 | a4_02==7 | a4_03>1) & (a4_05==1) & (a4_04==1)
replace condocup_ci=2 if (a4_01==2 | a4_02==7 | a4_03>7) & (a4_05==1) & (a4_04==1)

*2015,10 MLO la encuesta pregunta a partir de 7 años (no 10)
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci



*************
*cesante_ci* 
*************

gen cesante_ci=1 if  a4_09==1 & condocup_ci==2
* 2014, 03 Modificacion siguiente linea MLO
replace cesante_ci=0 if a4_09==2 & condocup_ci==2
*replace cesante_ci=0 if  a4_09==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if a4_19>=1 & a4_19<=5
replace tamemp_ci=2 if a4_19>=6 & a4_19<=49
replace tamemp_ci=3 if a4_19>49 & a4_19!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
egen aux_p=rsum(a5_01c2 a5_01d2 a5_01e2 a5_01f2), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
*Mod. MLO 2014,07,21 modificacion a valorea mensuales * en este momento era 1 pago anual
gen aux_ps= a5_03e1/12  if  a5_03e2==1 & a5_03e1 >1 & a5_03e1 !=. 
gen byte pensionsub_ci=1 if aux_ps>0 & aux_ps!=.
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring aux_ps, replace
gen ypensub_ci=aux_ps

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

gen desalent_ci=(emp_ci==0 & (a4_08==3 | a4_08==4))
replace desalent_ci=. if emp_ci==.

*****************
***horaspri_ci***
*****************
*Esta variable está definida para una semana, sin embargo, se creo para el mes. Se deja la programación inicial sin
*habilitar y se construye una nueva

/* dias a la semana */
* a4_20a

/* Horas diarias */
* a4_20b1

*gen horassem =  a4_20b1* a4_20a
*gen horaspri_ci= horassem*4.3


gen horaspri_ci= a4_20b1* a4_20a

replace horaspri_ci=. if a4_20b1==. & a4_20a==.
replace horaspri_ci=. if emp_ci~=1

*drop horassem

*gen horassem =  a4_40hrs* a4_39

*gen horassec_ci= horassem*4.3

gen horassec_ci=a4_40hrs* a4_39
replace horassec_ci=. if a4_40hrs==. & a4_39==.
replace horassec_ci=. if emp_ci~=1

*drop horassem

*****************
***horastot_ci***
*****************

egen horastot_ci= rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = . if horaspri_ci == . & horassec_ci == .

***************
***subemp_ci***
***************
/*
gen subemp_ci=.
replace subemp_ci = 1 if a4_42 == 1 & horastot_ci <= 129
replace subemp_ci = 0 if a4_42 == 2 & emp_ci == 1
*/

* Segun definicion del documento metodologico: horas de la actividad principal y si esta disponible a trabajar mas horas. MGD 06/18/2014
* Se podria considerar a las dos alternativas: desea trabajar y esta dispuesto a trabajar.
gen subemp_ci=0
*replace subemp_ci=1 if a4_43==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (a4_43==1 & a4_42==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.
*replace tiempoparc_ci = 1 if a4_42 == 2 & horastot_ci <= 30 & emp_ci == 1
*replace tiempoparc_ci = 0 if a4_42 == 2 & emp_ci == 1 & horastot_ci >30
*Mod. MLO 2015, 10
replace tiempoparc_ci=(a4_42==2 & horaspri_ci<30 & emp_ci == 1)
replace tiempoparc_ci=. if emp_ci==0
******************
***categopri_ci***
******************

/*
a4_13:
	1	obrero(a)
	2	empleado
	3	trabajador(a) por cuenta propia
	4	patrón, socio o empleador que si recibe salario
	5	patrón, socio o empleador que no recibe salario
	6	cooperativista de producción
	7	trabajador(a) familiar o aprendiz sin remuneración
	8	empleada(o) del hogar
*/


gen categopri_ci=.
replace categopri_ci=1 if a4_13>=4 & a4_13<=6
replace categopri_ci=2 if a4_13==3
replace categopri_ci=3 if a4_13==1 | a4_13==2 | a4_13==8
replace categopri_ci=4 if a4_13==7
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if a4_38>=4 & a4_38<=6
replace categosec_ci=2 if a4_38==3
replace categosec_ci=3 if a4_38==1 | a4_38==2 | a4_38==8
replace categosec_ci=4 if a4_38==7

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if a4_14==3 & categopri_ci==3
replace tipocontrato_ci=2 if a4_14==1 & categopri_ci==3
replace tipocontrato_ci=3 if ((a4_14==2 | a4_14==4) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & a4_36==1

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if  a4_19>=1 & a4_19<=5 /*1 a 5 personas*/
replace firmapeq_ci=0 if  a4_19>=6 & a4_19!=. /*más de 5 personas*/
*/
*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=.
replace spublico_ci=1 if a4_15==1
replace spublico_ci=0 if a4_15==2
replace spublico_ci=. if emp_ci~=1

**************
***ocupa_ci***
**************
/*
ceob_1:
	1	dirección en la adm. pública y empresas
	2	profesionales, científicos e intelectuales
	3	técnicos y profesionales de apoyo
	4	empleados de oficina
	5	servicios y vendedores del comercio
	6	agricultura, pecuaria, agropecuaria y pesca
	7	industria extractiva, construcción, ind. manufact.	y	otros	o
	8	operadores de instalaciones y maquinarias
	9	trabajadores no calificados
	10	fuerzas armadas
*/

* Modificacion MGD 07/24/2014: clasificacion CIUO -88
g aux = substr(cod_oc,1,3)
destring aux, replace
gen ocupa_ci=.
replace ocupa_ci=1 if ((aux>=210 & aux<=348) | (aux>=21 & aux<=34)) & emp_ci==1
replace ocupa_ci=2 if ((aux>=110 & aux<=131) |  aux==11) & emp_ci==1
replace ocupa_ci=3 if ((aux>=410 & aux<=422) |  aux==41 |  aux==42) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911) | aux==52 | aux==91) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=621) | (aux>=920 & aux<=921) | aux==61) & emp_ci==1
replace ocupa_ci=7 if ((aux>=710 & aux<=851) | (aux>=930 & aux<=933) | aux==71 | aux==81 | aux==93)& emp_ci==1
replace ocupa_ci=8 if (aux>=0 & aux<=9) & emp_ci==1

drop aux

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"

/*NA: No se puede estandarizar ya que no se distingue entre dos categorias:
comerciantes y vendedores y trabajadores en servicios*/

*************
***rama_ci***
*************
/*
cpaeb_1:
	1	a.	agricultura, ganaderia, caza y silvicultura
	2	b.	pesca
	3	c.	explotacion de minas y canteras
	4	d.	industria manufacturera
	5	e.	produccion y distribucion de energia electrica, gas y agu
	6	f.	construccion
	7	g.	comercio por mayor y menor; rep vehiculos motos efectos p
	8	h.	servicio de hoteles y restaurantes
	9	i.	transporte, almacenamiento y comunicaciones
	10	j.	intermediacion financiera
	11	k.	servicios inmobiliarios, empresariales y de alquiler
	12	l.	administracion publica, defensa y seguridad soc obligator
	13	m.	educacion
	14	n.	servicios sociales y de salud
	15	o.	servicios comunitarios, sociales y personales
	16	p.	servicio de hogares privados que contratan serv domestico
	17	q.	servicio de organizaciones y organos extraterritoriales
	99	no	sabe/no responde
*/


gen rama_ci=.
replace rama_ci=1 if cpaeb_1>=1 & cpaeb_1<=2 & emp_ci==1
replace rama_ci=2 if cpaeb_1==3 & emp_ci==1
replace rama_ci=3 if cpaeb_1==4 & emp_ci==1
replace rama_ci=4 if cpaeb_1==5 & emp_ci==1
replace rama_ci=5 if cpaeb_1==6 & emp_ci==1
replace rama_ci=6 if cpaeb_1>=7 & cpaeb_1<=8 & emp_ci==1 
replace rama_ci=7 if cpaeb_1==9 & emp_ci==1
replace rama_ci=8 if cpaeb_1>=10 & cpaeb_1<=11 & emp_ci==1
replace rama_ci=9 if cpaeb_1>=12 & cpaeb_1<=17 & emp_ci==1


****************
***durades_ci***
****************
/*En meses
a4_10b:
           2 semana
           4 mes
           6 año
*/

gen durades_ci=.
replace durades_ci=a4_10a/4.3  if a4_10b==2
replace durades_ci=a4_10a      if a4_10b==4
replace durades_ci=a4_10a*12   if a4_10b==6

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (a4_07==3 & condocup_ci==3)
replace categoinac_ci = 2 if  (a4_07==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (a4_07==2 & condocup_ci==3)
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
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

*Yessenia: 
*Note: En este ano existen varios componentes del ingreso que no se consideran antes ni despues. 
*Para que sea homogeneo a traves del tiempo deberian eliminarse los que no estan en las encuestas del resto de anios.

*************************
*********LABORAL*********
*************************

/*
a4_23b:
           1 bs.
           2 $us 	-	 

a4_23c:
           1 diario
           2 semanal
           3 quincenal
           4 mensual
           5 trimestral
           6 semestral
           7 anual
*/

*******************
* salario liquido *
*******************

gen a4_23a2 = .
replace a4_23a2 = a4_23a 		if a4_23b==1
replace a4_23a2 = a4_23a*7.50 	if a4_23b==2

gen yliquido = .
replace yliquido= a4_23a2*30	if a4_23c==1
replace yliquido= a4_23a2*4.3	if a4_23c==2
replace yliquido= a4_23a2*2		if a4_23c==3
replace yliquido= a4_23a2		if a4_23c==4
replace yliquido= a4_23a2/3		if a4_23c==5
replace yliquido= a4_23a2/6		if a4_23c==6
replace yliquido= a4_23a2/12	if a4_23c==7

**************
* comisiones *
**************

* a4_24a1 a4_24a2 a4_24a3

gen a4_24a12 = .
replace a4_24a12 = a4_24a1 		if a4_24a2==1
replace a4_24a12 = a4_24a1*7.50	if a4_24a2==2

gen ycomisio = .
replace ycomisio= a4_24a12*30	if a4_24a3==1
replace ycomisio= a4_24a12*4.3	if a4_24a3==2
replace ycomisio= a4_24a12*2	if a4_24a3==3
replace ycomisio= a4_24a12		if a4_24a3==4
replace ycomisio= a4_24a12/3	if a4_24a3==5
replace ycomisio= a4_24a12/6	if a4_24a3==6
replace ycomisio= a4_24a12/12	if a4_24a3==7



************
* propinas *
************

gen a4_24b12 = .
replace a4_24b12 = a4_24b1 		if a4_24b2==1
replace a4_24b12 = a4_24b1*7.50	if a4_24b2==2

gen ypropinas= .
replace ypropinas= a4_24b12*30	if a4_24b3==1
replace ypropinas= a4_24b12*4.3	if a4_24b3==2
replace ypropinas= a4_24b12*2	if a4_24b3==3
replace ypropinas= a4_24b12		if a4_24b3==4
replace ypropinas= a4_24b12/3	if a4_24b3==5
replace ypropinas= a4_24b12/6	if a4_24b3==6
replace ypropinas= a4_24b12/12	if a4_24b3==7


***************
* refrigerios *
***************

gen a4_24c12 = .
replace a4_24c12 = a4_24c1 		if a4_24c2==1
replace a4_24c12 = a4_24c1*7.50	if a4_24c2==2

gen yrefrige= .
replace yrefrige= a4_24c12*30	if a4_24c3==1
replace yrefrige= a4_24c12*4.3	if a4_24c3==2
replace yrefrige= a4_24c12*2	if a4_24c3==3
replace yrefrige= a4_24c12		if a4_24c3==4
replace yrefrige= a4_24c12/3	if a4_24c3==5
replace yrefrige= a4_24c12/6	if a4_24c3==6
replace yrefrige= a4_24c12/12	if a4_24c3==7


****************
* horas extras *
****************

gen a4_24d12 = .
replace a4_24d12 = a4_24d1 		if a4_24d2==1
replace a4_24d12 = a4_24d1*7.50	if a4_24d2==2

gen yhrsextr= .
replace yhrsextr= a4_24d12*30	if a4_24d3==1
replace yhrsextr= a4_24d12*4.3	if a4_24d3==2
replace yhrsextr= a4_24d12*2	if a4_24d3==3
replace yhrsextr= a4_24d12		if a4_24d3==4
replace yhrsextr= a4_24d12/3	if a4_24d3==5
replace yhrsextr= a4_24d12/6	if a4_24d3==6
replace yhrsextr= a4_24d12/12	if a4_24d3==7


*********
* prima *
*********

gen yprima = .
replace yprima = a4_26a1 		if a4_26a2==1
replace yprima = a4_26a1*7.50	if a4_26a2==2

*************
* aguinaldo *
*************

gen yaguina = .
replace yaguina = a4_26b1 		if a4_26b2==1
replace yaguina = a4_26b1*7.50	if a4_26b2==2

*************
* alimentos *
*************

gen yalimen = .
replace yalimen= a4_27a3*30		if a4_27a2==1 & a4_27a1==1
replace yalimen= a4_27a3*4.3	if a4_27a2==2 & a4_27a1==1
replace yalimen= a4_27a3*2		if a4_27a2==3 & a4_27a1==1
replace yalimen= a4_27a3		if a4_27a2==4 & a4_27a1==1
replace yalimen= a4_27a3/3		if a4_27a2==5 & a4_27a1==1
replace yalimen= a4_27a3/6		if a4_27a2==6 & a4_27a1==1
replace yalimen= a4_27a3/12		if a4_27a2==7 & a4_27a1==1

**************
* transporte *
**************

gen ytranspo = .
replace ytranspo= a4_27b3*30	if a4_27b2==1 & a4_27b1==1
replace ytranspo= a4_27b3*4.3	if a4_27b2==2 & a4_27b1==1
replace ytranspo= a4_27b3*2		if a4_27b2==3 & a4_27b1==1
replace ytranspo= a4_27b3		if a4_27b2==4 & a4_27b1==1
replace ytranspo= a4_27b3/3		if a4_27b2==5 & a4_27b1==1
replace ytranspo= a4_27b3/6		if a4_27b2==6 & a4_27b1==1
replace ytranspo= a4_27b3/12	if a4_27b2==7 & a4_27b1==1

**************
* vestimenta *
**************

gen yvesti = .
replace yvesti= a4_27c3*30		if a4_27c2==1 & a4_27c1==1
replace yvesti= a4_27c3*4.3		if a4_27c2==2 & a4_27c1==1
replace yvesti= a4_27c3*2		if a4_27c2==3 & a4_27c1==1
replace yvesti= a4_27c3			if a4_27c2==4 & a4_27c1==1
replace yvesti= a4_27c3/3		if a4_27c2==5 & a4_27c1==1
replace yvesti= a4_27c3/6		if a4_27c2==6 & a4_27c1==1
replace yvesti= a4_27c3/12		if a4_27c2==7 & a4_27c1==1

************
* vivienda *
************

gen yvivien = .
replace yvivien= a4_27d3*30		if a4_27d2==1 & a4_27d1==1
replace yvivien= a4_27d3*4.3	if a4_27d2==2 & a4_27d1==1
replace yvivien= a4_27d3*2		if a4_27d2==3 & a4_27d1==1
replace yvivien= a4_27d3		if a4_27d2==4 & a4_27d1==1
replace yvivien= a4_27d3/3		if a4_27d2==5 & a4_27d1==1
replace yvivien= a4_27d3/6		if a4_27d2==6 & a4_27d1==1
replace yvivien= a4_27d3/12		if a4_27d2==7 & a4_27d1==1


*************
* guarderia *
*************

gen yguarde = .
replace yguarde= a4_27e3*30		if a4_27e2==1 & a4_27e1==1
replace yguarde= a4_27e3*4.3	if a4_27e2==2 & a4_27e1==1
replace yguarde= a4_27e3*2		if a4_27e2==3 & a4_27e1==1
replace yguarde= a4_27e3		if a4_27e2==4 & a4_27e1==1
replace yguarde= a4_27e3/3		if a4_27e2==5 & a4_27e1==1
replace yguarde= a4_27e3/6		if a4_27e2==6 & a4_27e1==1
replace yguarde= a4_27e3/12		if a4_27e2==7 & a4_27e1==1

*******************
* ingreso act. pr *
*******************
*Yessenia: cambio la var a4_35a por la var a4_29a
gen a4_35a2 = .
replace a4_35a2 = a4_35a 		if a4_35b==1
replace a4_35a2 = a4_35a*7.50 	if a4_35b==2

gen yactpri = .
replace yactpri= a4_35a2*30		if a4_35c==1
replace yactpri= a4_35a2*4.3	if a4_35c==2
replace yactpri= a4_35a2*2		if a4_35c==3
replace yactpri= a4_35a2		if a4_35c==4
replace yactpri= a4_35a2/3		if a4_35c==5
replace yactpri= a4_35a2/6		if a4_35c==6
replace yactpri= a4_35a2/12		if a4_35c==7

********************
* salario liquido2 *
********************

gen a4_41a2 = .
replace a4_41a2 = a4_41a 		if a4_41b==1
replace a4_41a2 = a4_41a*7.50 	if a4_41b==2

gen yliquido2 = .
replace yliquido2= a4_41a2*30	if a4_41c==1
replace yliquido2= a4_41a2*4.3	if a4_41c==2
replace yliquido2= a4_41a2*2	if a4_41c==3
replace yliquido2= a4_41a2		if a4_41c==4
replace yliquido2= a4_41a2/3	if a4_41c==5
replace yliquido2= a4_41a2/6	if a4_41c==6
replace yliquido2= a4_41a2/12	if a4_41c==7


*************************
******NO-LABORAL*********
*************************

*************
* intereses *
*************

gen yinteres = .
replace yinteres = a5_01a2		if a5_01a3==1 & a5_01a1==1
replace yinteres = a5_01a2*7.50	if a5_01a3==2 & a5_01a1==1

**************
* alquileres *
**************

gen yalqui = .
replace yalqui = a5_01b2		if a5_01b3==1 & a5_01b1==1
replace yalqui = a5_01b2*7.50	if a5_01b3==2 & a5_01b1==1

**************
* jubilacion *
**************

gen yjubi = .
replace yjubi = a5_01c2			if a5_01c3==1 & a5_01c1==1
replace yjubi = a5_01c2*7.50	if a5_01c3==2 & a5_01c1==1

**************
* benemerito *
**************

gen ybene = .
replace ybene = a5_01d2			if a5_01d3==1 & a5_01d1==1
replace ybene = a5_01d2*7.50		if a5_01d3==2 & a5_01d1==1

*************
* invalidez *
*************

gen yinvali = .
replace yinvali = a5_01e2		if a5_01e3==1 & a5_01e1==1
replace yinvali = a5_01e2*7.50		if a5_01e3==2 & a5_01e1==1

**********
* viudez *
**********

gen yviudez = .
replace yviudez = a5_01f2		if a5_01f3==1 & a5_01f1==1
replace yviudez = a5_01f2*7.50		if a5_01f3==2 & a5_01f1==1


****************
* otras rentas *
****************

gen yotren = .
replace yotren = a5_01g2		if a5_01g3==1 & a5_01g1==1
replace yotren = a5_01g2*7.50		if a5_01g3==2 & a5_01g1==1

************************
* alquileres agricolas *
************************

*Yessenia: divido para doce e inserto condicion & a5_02a2==1 (distingo lo que se recibio en esepcie)

gen yalqagri = .
replace yalqagri = a5_02a3/12		 if a5_02a4==1 & a5_02a2==1
replace yalqagri = (a5_02a3*7.50)/12 if a5_02a4==2 & a5_02a2==1

gen yalqagri_nm = .
replace yalqagri_nm = a5_02a3/12		 if a5_02a4==1 & a5_02a2==2
replace yalqagri_nm = (a5_02a3*7.50)/12 if a5_02a4==2 & a5_02a2==2

**************
* dividendos *
**************

*Yessenia: divido para doce e inserto condicion & a5_02b2==1
gen ydivi = .
replace ydivi = a5_02b3/12			if a5_02b4==1 & a5_02b2==1         
replace ydivi = (a5_02b3*7.50)/12	if a5_02b4==2 & a5_02b2==1 

gen ydivi_nm = .
replace ydivi_nm = a5_02b3/12			if a5_02b4==1 & a5_02b2==2         
replace ydivi_nm = (a5_02b3*7.50)/12	if a5_02b4==2 & a5_02b2==2 

*************************
* alquileres maquinaria *
*************************
*Yessenia: divido para doce e inserto condicion & a5_02c2==1

gen yalqmaqui = .
replace yalqmaqui = a5_02c3/12		    if a5_02c4==1 & a5_02c2==1
replace yalqmaqui = (a5_02c3*7.50)/12	if a5_02c4==2 & a5_02c2==1

gen yalqmaqui_nm = .
replace yalqmaqui_nm = a5_02c3/12		    if a5_02c4==1 & a5_02c2==2
replace yalqmaqui_nm = (a5_02c3*7.50)/12	if a5_02c4==2 & a5_02c2==2


******************
* indem. trabajo *
******************

*Yessenia: divido para doce 
gen yindtr = .
replace yindtr = a5_03a1/12		    if a5_03a2==1
replace yindtr = (a5_03a1*7.50)/12	if a5_03a2==2

******************
* indem. seguros *
******************
*Yessenia: divido para doce 
gen yindseg = .
replace yindseg = a5_03b1/12		if a5_03b2==1
replace yindseg = (a5_03b1*7.50)/12	if a5_03b2==2

************
* herencia *
************
*Yessenia: divido para doce 
gen yheren = .
replace yheren = a5_03c1/12		     if a5_03c2==1
replace yheren = (a5_03c1*7.50)/12	 if a5_03c2==2

***********
* pasunak *
***********
*Yessenia: divido para doce 
gen ypasu = .
replace ypasu = a5_03d1/12			if a5_03d2==1
replace ypasu = (a5_03d1*7.50)/12	if a5_03d2==2


***********
* bonosol *
***********
*Yessenia: divido para doce 
gen ybono = .
replace ybono = a5_03e1/12			if a5_03e2==1
replace ybono = (a5_03e1*7.50)/12	if a5_03e2==2


******************
* otros ingresos *
******************
*Yessenia: divido para doce 
gen yotring = .
replace yotring = a5_03f1/12		if a5_03f2==1
replace yotring = (a5_03f1*7.50)/12	if a5_03f2==2


*******************
* asist. familiar *
*******************

gen a5_04a12 = .
replace a5_04a12 = a5_04a1 		    if a5_04a2==1
replace a5_04a12 = a5_04a1*7.50 	if a5_04a2==2

gen yasistfam = .
replace yasistfam= a5_04a12*30		if a5_04a3==1
replace yasistfam= a5_04a12*4.3		if a5_04a3==2
replace yasistfam= a5_04a12*2		if a5_04a3==3
replace yasistfam= a5_04a12		if a5_04a3==4
replace yasistfam= a5_04a12/3		if a5_04a3==5
replace yasistfam= a5_04a12/6		if a5_04a3==6
replace yasistfam= a5_04a12/12		if a5_04a3==7


*********************
* Trans. monetarias *
*********************

gen a5_04b12 = .
replace a5_04b12 = a5_04b1 		    if a5_04b2==1
replace a5_04b12 = a5_04b1*7.50 	if a5_04b2==2

gen ytransmon = .
replace ytransmon= a5_04b12*30		if a5_04b3==1
replace ytransmon= a5_04b12*4.3		if a5_04b3==2
replace ytransmon= a5_04b12*2	 	if a5_04b3==3
replace ytransmon= a5_04b12		    if a5_04b3==4
replace ytransmon= a5_04b12/3		if a5_04b3==5
replace ytransmon= a5_04b12/6		if a5_04b3==6
replace ytransmon= a5_04b12/12		if a5_04b3==7



**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

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


***********
* remesas *
***********

gen a5_04c12 = .
replace a5_04c12 = a5_04c1 		    if a5_04c2==1
replace a5_04c12 = a5_04c1*7.50 	if a5_04c2==2

gen yremesas = .
replace yremesas= a5_04c12*30		if a5_04c3==1
replace yremesas= a5_04c12*4.3		if a5_04c3==2
replace yremesas= a5_04c12*2		if a5_04c3==3
replace yremesas= a5_04c12		    if a5_04c3==4
replace yremesas= a5_04c12/3		if a5_04c3==5
replace yremesas= a5_04c12/6		if a5_04c3==6
replace yremesas= a5_04c12/12		if a5_04c3==7


*************
* inversion *
*************

gen yinvers = .
replace yinvers =  a5_05a2/12		if a5_05a3==1
replace yinvers = (a5_05a2*7.50)/12	if a5_05a3==2

*************
* hipotecas *
*************

gen yhipotec = .
replace yhipotec =  a5_05b2/12		    if a5_05b3==1
replace yhipotec = (a5_05b2*7.50)/12	if a5_05b3==2

*********
* bonos *
*********

gen ybonos = .
replace ybonos =  a5_05c2/12		if a5_05c3==1
replace ybonos = (a5_05c2*7.50)/12	if a5_05c3==2

*************
* prestamos *
*************

gen ypresta = .
replace ypresta =  a5_05d2/12		if a5_05d3==1
replace ypresta = (a5_05d2*7.50)/12	if a5_05d3==2

************
* prestata *
************

gen yprestata = .
replace yprestata =  a5_05e2/12		if a5_05e3==1
replace yprestata = (a5_05e2*7.50)/12	if a5_05e3==2

*************
* inmuebles *
*************

gen yinmueb = .
replace yinmueb =  a5_06a3/12		if a5_06a4==1
replace yinmueb = (a5_06a3*7.50)/12	if a5_06a4==2

***********************
* propiedades rurales *
***********************

gen yinmrur = .
replace yinmrur =  a5_06b3/12		if a5_06b4==1
replace yinmrur = (a5_06b3*7.50)/12	if a5_06b4==2

*************
* vehiculos *
*************

gen yvehi = .
replace yvehi =  a5_06c3/12		if a5_06c4==1
replace yvehi = (a5_06c3*7.50)/12	if a5_06c4==2


*********************
* electrodomesticos *
*********************

gen yelec = .
replace yelec =  a5_06d3/12		if a5_06d4==1
replace yelec = (a5_06d3*7.50)/12	if a5_06d4==2


***********
* muebles *
***********

gen ymuebles = .
replace ymuebles =  a5_06e3/12		if a5_06e4==1
replace ymuebles = (a5_06e3*7.50)/12	if a5_06e4==2

*********
* joyas *
*********

gen yjoyas = .
replace yjoyas =  a5_06f3/12		if a5_06f4==1
replace yjoyas = (a5_06f3*7.50)/12	if a5_06f4==2


/* 
ylm:
yliquido 
ycomisio 
ypropinas 
yhrsextr 
yprima 
yaguina
yactpri 
yliquido2

ylnm:
yrefrige 
yalimen 
ytranspo 
yvesti 
yvivien 
yguarde */




***************
***ylmpri_ci***
***************

egen ylmpri_ci=rsum(yliquido ycomisio ypropinas yhrsextr yprima yaguina yactpri), missing
replace ylmpri_ci=. if yliquido ==. & ycomisio ==. &  ypropinas ==. & yhrsextr ==. & yprima ==. &  yaguina ==. &  yactpri==.  
replace ylmpri_ci=. if emp_ci~=1
replace ylmpri_ci=0 if categopri_ci==4


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

egen ylnmprid=rsum(yrefrige yalimen ytranspo yvesti yvivien yguarde), missing
replace ylnmprid=. if yrefrige ==. & yalimen==. & ytranspo==. & yvesti==. & yvivien==. & yguarde==. 
replace ylnmprid=0 if categopri_ci==4


*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1

***************
***ylmsec_ci***
***************

gen ylmsec_ci= yliquido2 
replace ylmsec_ci=. if emp_ci~=1
replace ylmsec_ci=0 if categosec_ci==4

******************
****ylnmsec_ci****
******************

gen ylnmsec_ci= .
replace ylnmsec_ci=. if emp_ci==0


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

 
/* 

ynlm:

yinteres 
yalqui 
yjubi 
ybene 
yinvali 
yviudez 
yotren  
yalqagri 
ydivi 
yalqmaqui  
yindtr  
yindseg 
yheren 
ypasu 
ybono  
yotring  
yasistfam 
ytransmon 
yremesas 
yinvers 
yhipotec 
ybonos 
ypresta 
yprestata 
yinmueb 
yinmrur 
yvehi 
yelec 
ymuebles 
yjoyas */



*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(yinteres yalqui yjubi ybene yinvali yviudez yotren yalqagri ydivi yalqmaqui yindtr yindseg yheren ypasu ybono yotring yasistfam ytransmon yremesas yinvers yhipotec ybonos ypresta yprestata yinmueb yinmrur yvehi yelec ymuebles yjoyas), missing
replace ynlm_ci=. if 	yinteres==. & yalqui==. & yjubi==. & ybene==. & yinvali==. & yviudez==. & yotren==. & yalqagri==. & ydivi==. & yalqmaqui==. & yindtr==. & yindseg==. & ///
			yheren==. & ypasu==. & ybono==. & yotring==. & yasistfam==. & ytransmon==. & yremesas==. & yinvers==. & yhipotec==. & ybonos==. & ///
			ypresta==. & yprestata==. & yinmueb==. & yinmrur==. & yvehi==. & yelec==. & ymuebles==. & yjoyas==. 

**************
***ynlnm_ci***
**************
*Yessenia: distingo el ynl monetario y no monetario.

egen ynlnm_ci= rsum(yalqagri_nm ydivi_nm yalqmaqui_nm), missing
replace ynlnm_ci=. if yalqagri_nm==. & ydivi_nm==. & yalqmaqui_nm==.

*****************
***remesas_ci***
*****************

gen remesas_ci=yremesas

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


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=.


*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch= a6_06a
replace rentaimp_ch=rentaimp_ch*7.50 if a6_06b==2


*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)


****************************
***VARIABLES DE EDUCACION***
****************************

/*En esta sección es sólo para los mayores a los 5 años de edad*/

/*
a3_02a:
          11 ninguno
          12 curso de alfabetización
          13 educación pre-escolar
          14 básico (1 a 5 años)
          15 intermedio (1 a 3 años)
          16 medio (1 a 4 años)
          17 primaria (1 a 8 años)
          18 secundaria (1 a 4 años)
          19 educación básica de adultos (eba)
          20 centro de educación media de adultos (cema)
          21 normal
          22 universidad  pública (licenciatura)
          23 universidad  privada (licenciatura)
          24 postgrado, maestría
          25 técnico de universidad
          26 técnico de instituto
          27 institutos de formación militar y policial
          28 otros cursos
*/


gen byte aedu_ci=.

replace aedu_ci=0 if a3_02a==11 | a3_02a==12 | a3_02a==13

replace aedu_ci=1 if (a3_02a==14 | a3_02a==17) & a3_02b==1
replace aedu_ci=2 if (a3_02a==14 | a3_02a==17) & a3_02b==2
replace aedu_ci=3 if (a3_02a==14 | a3_02a==17) & a3_02b==3
replace aedu_ci=4 if (a3_02a==14 | a3_02a==17) & a3_02b==4
replace aedu_ci=5 if (a3_02a==14 | a3_02a==17) & a3_02b==5

replace aedu_ci=6 if (a3_02a==17 & a3_02b==6) | (a3_02a==15 & a3_02b==1)
replace aedu_ci=7 if (a3_02a==17 & a3_02b==7) | (a3_02a==15 & a3_02b==2)
replace aedu_ci=8 if (a3_02a==17 & a3_02b==8) | (a3_02a==15 & a3_02b==3)

replace aedu_ci=9 if (a3_02a==16 | a3_02a==18) & a3_02b==1
replace aedu_ci=10 if (a3_02a==16 | a3_02a==18) & a3_02b==2
replace aedu_ci=11 if (a3_02a==16 | a3_02a==18) & a3_02b==3
replace aedu_ci=12 if (a3_02a==16 | a3_02a==18) & a3_02b==4

replace aedu_ci=13 if (a3_02a>=21 & a3_02a<=27 & a3_02a~=24) & a3_02b==1
replace aedu_ci=14 if (a3_02a>=21 & a3_02a<=27 & a3_02a~=24) & a3_02b==2
replace aedu_ci=15 if (a3_02a>=21 & a3_02a<=27 & a3_02a~=24) & a3_02b==3
replace aedu_ci=16 if (a3_02a>=21 & a3_02a<=27 & a3_02a~=24) & a3_02b==4

replace aedu_ci=16 if (a3_02a==21 | a3_02a==25 | a3_02a==26) & a3_02b==5
replace aedu_ci=17 if (a3_02a==22 | a3_02a==23 | a3_02a==27) & a3_02b==5

replace aedu_ci=16 if (a3_02a==21 | a3_02a==25 | a3_02a==26) & a3_02b==8
replace aedu_ci=17 if (a3_02a==22 | a3_02a==23 | a3_02a==27) & a3_02b==8

replace aedu_ci=18 if a3_02a==24 & a3_02b==1
replace aedu_ci=19 if a3_02a==24 & a3_02b==2



**************
***eduno_ci***
**************

gen byte eduno_ci=(a3_02a==11 | a3_02a==12 | a3_02a==13) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu>=1 & aedu_ci<=4)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==5)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=6 & aedu_ci<=11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(aedu_ci>=6 & aedu_ci<=7)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==8)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci>=9 & aedu_ci<=11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & a3_02b<8)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & a3_02b==8) | (aedu_ci>=17 & aedu_ci<.)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(a3_02a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (a3_02a==22 | a3_02a==23 | a3_02a==24| a3_02a==25)
replace eduac_ci=0 if (a3_02a==21 | (a3_02a>=26 & a3_02a<=27))
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

*gen asiste_ci=(a3_08==1)

* LMC (introducido por YL): Se cambia la forma de cálculo porque se deben considerar los rangos de edad lcm dic2013
*Modificación Mayra Sáenz Enero-2017: Se genera la dummy de acuerdo al documento metodológico.
gen asiste_ci= a3_04==1
/*
gen asiste_ci= 1 if a3_04==1
replace asiste_ci = 0 if a3_04==2*/
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=a3_09 

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************

gen     pqnoasis1_ci = 1 if a3_09 ==2
replace pqnoasis1_ci = 2 if a3_09 ==3
replace pqnoasis1_ci = 3 if a3_09 ==4 
replace pqnoasis1_ci = 4 if a3_09 ==8
replace pqnoasis1_ci = 5 if a3_09 ==9 
replace pqnoasis1_ci = 6 if a3_09 ==6 
replace pqnoasis1_ci = 7 if a3_09 ==7  
replace pqnoasis1_ci = 8 if a3_09 ==5
replace pqnoasis1_ci = 9 if a3_09 ==1  | a3_09 ==10

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci



 
***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.


***************
***edupub_ci***
***************
/*Sobre los que se matricularon ese año*/

gen edupub_ci=(a3_06==2 | a3_06==3)
replace edupub_ci=. if a3_06==.

**************
***tecnica_ci*
**************

gen tecnica_ci=.
replace tecnica_ci=1 if a3_02a==25 | a3_02a==26
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

/*
a6_16:
           1 cañeria de red
           2 pileta publica
           3 carro repartidor (aguatero)
           4 pozo o noria con bomba
           5 pozo o noria sin bomba
           6 rio, vertiente o acequia
           7 lago, laguna o curiche
           8 otra
*/


gen aguared_ch=(a6_16==1)
replace aguared_ch=. if a6_16==.

*Mayra Saenz Julio 2013 
/*
a6_19:
           1 por cañería dentro de la vivienda
           2 por cañeria fuera de la vivienda, pero dentro del lote
           3 por cañeria fuera del lote o terreno
           4 no se distribuye por cañería
*/

gen aguadist_ch=1 if a6_19==1
replace aguadist_ch=2 if a6_19==2
replace aguadist_ch=3 if a6_19==3
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


gen aguamala_ch=(a6_16==6 | a6_16==7)
replace aguamala_ch=. if a6_16==.
label var aguamala_ch "Agua unimproved según MDG" 



/*NA*/

gen aguamide_ch=.
/*NA*/

/* a6_14 == 1*/

gen luz_ch=(a6_14==1)


gen luzmide_ch=.
/*NA*/

/*
a6_24:
           1 leña
           2 guano, bosta o taquia
           3 kerosene
           4 gas licuado (garrafa)
           5 gas natural por red (cañeria)
           6 otro
           7 electricidad
           8 no utiliza
*/

gen combust_ch= (a6_24==5 | a6_24== 7)

*****************************************
/*
a6_21:
           1 si tiene
           2 no tiene
a6_22:
           1 privado
           2 compartido
           3 no tiene baño
*/

gen bano_ch=(a6_21==1)

gen banoex_ch=(a6_22==1)
replace banoex_ch=. if a6_22==3

******************************************

/*
a6_23:
           1 alcantarillado
           2 camara septica
           3 pozo ciego
           4 superficie (calle, quebrada o río)
           5 no tiene baño

*/
gen des1_ch=.
replace des1_ch=0 if a6_23==0
replace des1_ch=1 if a6_23==1 | a6_23==2
replace des1_ch=2 if a6_23==3
replace des1_ch=3 if a6_23==4
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if a6_23==1 | a6_23==2 | a6_23==3 
replace des2_ch=2 if a6_23==4
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch



/*
a6_10:
           1 tierra
           2 tablon de madera
           3 machimbre o parket
           4 alfombra o tapìzon
           5 cemento
           6 mosaico, baldosa o ceramica
           7 ladrillo
           8 otro
*/


gen piso_ch=0 if a6_10==1 
replace piso_ch=1 if a6_10>=2 & a6_10<=7 
replace piso_ch=2 if a6_10==8


/*
a6_07:
           1 ladrillo, bloques de cemento u hormigón
           2 adobe o tapial
           3 tabique o quinche
           4 piedra
           5 madera
           6 caña, palma o troncos
           7 otro

*/


gen pared_ch=0 if a6_07 ==6
replace pared_ch=1 if a6_07==1 | a6_07==2 | a6_07==3 | a6_07==4 | a6_07==5
replace pared_ch=2 if a6_07==7

/*
a6_09:
           1 calamina o plancha
           2 tejas (cemento, arcilla, fibrocemento)
           3 losa de hormigon armado
           4 paja, caña, palmo o barro
           5 otro

*/

gen techo_ch=0 if a6_09==4
replace techo_ch=1 if a6_09>=1 & a6_09<=3
replace techo_ch=2 if a6_09==5

/*
a6_26:
           1 la tira al río
           2 la quema
           3 la tira en un terreno baldio a la calle
           4 la entierra
           5 la deposita al basurero público o contenedor
           6 utiliza el servicio público de recolección (carro basurero)
           7 otro
*/


gen resid_ch =0    if a6_26  ==6
replace resid_ch=1 if a6_26  ==4 | a6_26  ==2
replace resid_ch=2 if a6_26  ==1 | a6_26  ==3
replace resid_ch=3 if a6_26  ==5
replace resid_ch=. if a6_26  ==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen     aguamejorada_ch = 1 if (a6_16 >= 1 &  a6_16 <=2) | (a6_16 >= 4 &  a6_16 <=5)
replace aguamejorada_ch = 0 if  a6_16 == 3 | (a6_16 >= 6 &  a6_16 <=8)
		
		
*********************
***banomejorado_ch***
*********************
gen     banomejorado_ch = 1 if (a6_21 == 1 & (a6_23 >= 1 & a6_23 <=3) & a6_22 == 1)
replace banomejorado_ch = 0 if (a6_21 == 1 & (a6_23 >= 1 & a6_23 <=3) & a6_22 == 2) | a6_22 ==3 | a6_21  ==2 | a6_23 == 5 | (a6_21  == 1  & (a6_23 >= 4 & a6_23 <=5))
		

gen dorm_ch= a6_12

gen cuartos_ch=a6_11

gen cocina_ch=(a6_13==1)

gen telef_ch=.
/*NA en la base, si existe la pregunta*/


gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/

/*
a6_02:
           1 propia
           2 alquilada
           3 en contrato anticretico
           4 en contrato mixto
           5 cedida por servicios
           6 cedida por parientes o amigos
           7 otra
*/

gen vivi1_ch=.


gen vivi2_ch=.

gen viviprop_ch=0 	if a6_02==2
replace viviprop_ch=1 	if a6_02==1
replace viviprop_ch=2 	if a6_02==3 
replace viviprop_ch=3 	if a6_02>=4 & a6_02<=7

gen vivitit_ch=.
/*NA*/

/*
a6_03a - monto
a6_03b - moneda
*/

gen vivialq_ch=a6_03a if viviprop_ch==0
replace vivialq_ch=vivialq_ch*7.45 if a6_03b==2

/*
a6_06a - monto
a6_06b - moneda
*/

gen vivialqimp_ch=a6_06a
replace vivialqimp_ch=vivialqimp_ch*7.45 if a6_06b==2


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



