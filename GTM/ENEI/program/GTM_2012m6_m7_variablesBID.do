
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
local ANO "2012"
local ronda m6_m7 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: m6_m7
Autores: Yesenia Loayza
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

****************
* region_BID_c *
****************
gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
*	region_c  * 
***************

gen region_c=dominio
label var region_c "division politico-administrativa, dominio estudio encuesta"


***************
***factor_ch***
***************

gen factor_ch= factor
label variable factor_ch "Factor de expansion del hogar"


******************************
*	idh_ch
******************************
g idh_ch= num_hog
la var idh_ch "Household ID"


******************************
*	idp_cI
******************************
g idp_ci= cp
la var idp_ci "Individual ID"

**********
***zona***
**********

recode dominio (3=0 Rural) (1 2=1 Urbano) ,g(zona_c)
label variable zona_c "Zona del pais"


************
****pais****
************

gen str3 pais_c="GTM"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2012
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=.
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

******************************
*	relacion_ci 
******************************
/*
ppa05:
          
           1 jefe(a) del hogar
           2 esposo(a) o compañero(a)
           3 hijo (a)
           4 yerno o nuera
           5 nieto(a)
           6 padreo o madre
           7 suegro(a)
           8 hermano (a)
           9 cuñado (a)
          10 otro(a) pariente
          11 empleado en casa particular
          12 pensionista o húesped
		  13 Otro no pariente

*/


recode ppa05 (4/10=4 "Otros parientes") (12 13 =5 "Otros no parientes") (11=6 "Empleado/a domestico/a"), g(relacion_ci)
la de relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a", add
label variable relacion_ci "Relacion con el jefe del hogar"


	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************

gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

***************
***upm_ci***
***************

clonevar upm_ci=upm
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

gen estrato_ci=.
label variable estrato_ci "Estrato"

**********
***sexo***
**********
g sexo_ci=ppa02
la de sexo_ci 1 "Hombre" 2 "Mujer"
la val sexo_ci sexo_ci

**********
***edad***
**********
g edad_ci= ppa03


******************************
*	civil_ci 
******************************
/*
 1 unido(a)
           2 casado(a)
           3 separado(a) de matrimonio
           4 separado(a) de union
           5 divorciado (a)
           6 viudo (a)
           7 soltero
           8 menor de 12 años

		   */
recode ppa09 (7=1 Single) (1 2=2 "Formal or informal union") (3 4 5=3 "Divorced or separated") (6=4 Widowed) (else=.), g (civil_ci)
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal", modify
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , modify
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



         ******************************
         *** VARIABLES DE DIVERSIDAD **
         ******************************
*Nathalia Maya & Antonella Pereira
*Feb 2021	

	***************
	***afroind_ci***
	***************
**Pregunta: ppa06 (1 xinca; 2 garifuna; 3 ladino; 4 extranjero; 5 maya) 
gen afroind_ci=. 
replace afroind_ci=1  if ppa06 == 1 | ppa06 ==2 | ppa06 ==5
replace afroind_ci=3 if ppa06 ==3 
replace afroind_ci=9 if ppa06 ==4
replace afroind_ci=. if ppa06 ==.

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2010

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
*************
**salmm_ci***
*************

*1 = GUA 2012
*gen salmm_ci= 1882.00
gen salmm_ci= 2040 /*30 dias*/
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
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if ocupados ==1
replace condocup_ci=2 if desocupados==1
replace condocup_ci =3 if inactivos==1
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7", modify
label value condocup_ci condocup_ci
*/
* No se utilizan variables creadas sino las originales de la base para homologar con la serie anterior. 05/19/2014 MGD
* Toma en cuentra trabajo+unque no trabajoa tiene trabajo y busqueda.

* MGR: Modifico serie en base a correcciones Laura Castrillo: delimitar la condición de edad para que no tome los missing en caso que existan
gen condocup_ci=.
replace condocup_ci=1 if p04a02==1 | p04a06==1
replace condocup_ci=2 if (p04a02==2 & p04a06==2) & (p04b01==1 | p04b02==1)
recode condocup_ci .=3 if edad_ci>=7 & edad_ci!=.
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

****************
*cotizando_ci***
****************
* MGD 03/2016: a partir de 2011 se deja de generar cotizando a pesar de que si existe la variable
* SGR 05/10: se modifica line 1
*gen cotizando_ci=1 if p04c25a==1 & p04c25b>0 & p04c25a!=.
gen cotizando_ci=1 if p04c25a==1 & p04c25b>0 & p04c25b!=.
recode cotizando_ci .=0 if condocup_ci==1 | condocup_ci==2
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if p04c25a==1
replace afiliado_ci=0 if condocup_ci==2
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=p04c25a
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=p04c25a
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 





*************
*cesante_ci* 
*************
* Correccion MGD 07/17/2014: mal generada la variable, se consideraba a inactivos p04g01 y no desempleados.
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if p04f06==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*************
**pension_ci*
*************

gen pension_ci=1 if p05a05b>0 & p05a05b!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=p05a05b/3 if p05a05b>0 & p05a05b!=.

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

g desalent_ci=(emp_ci==0 & (p04b05>=3 & p04b05<=5))
replace desalent_ci=. if p04b04==.
replace desalent_ci=1 if emp_ci==0 & p04b04>=8 & p04b04<=11
label var desalent_ci "Trabajadores desalentados"
   


*****************
***horaspri_ci***
*****************
egen horaspri_ci=rsum(p04c28a p04c28b p04c28c p04c28d p04c28e p04c28f p04c28g) if emp_ci==1, missing
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
g hsext=p04c11b/4.3
/*
egen horastot_ci=rsum(horaspri_ci p04d16 hsext) if emp_ci==1 & p04d01==1, missing  /*adding secondary employment and extra time*/
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
*/
*Horas totales sin horas extra. MGD 06/19/2014
egen horastot_ci=rsum(horaspri_ci p04d16) if emp_ci==1, missing /*adding secondary employment and extra time*/
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

******************************
*	subemp_ci
******************************
/*
* Modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). MGD 06/19/2014
g subemp_ci=0 
replace subemp_ci=1 if emp_ci==1 & horastot_ci<=30 & p04e01==1 & p04e05==1
label var subemp_ci "Personas en subempleo por horas"
*/
* Modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). MGD 06/19/2014
* No se consideran las horas extra trabajadas para el calculo de horas.
g subemp_ci=0 
replace subemp_ci=1 if emp_ci==1 & horaspri_ci<=30 & p04e01==1 & p04e05==1
label var subemp_ci "Personas en subempleo por horas"


*******************
***tiempoparc_ci***
*******************

* MGR: Modifico serie en base a correcciones Laura Castrillo: se debe utilizar horaspri en lugar de horastot como había sido generada antes
g tiempoparc_ci=(emp_ci==1 & p04e01==2 & (horaspri_ci>=1 & horaspri_ci<30))
replace tiempoparc_ci=. if emp_ci!=1 | p04e01==. | horaspri_ci==.
label var tiempoparc_ci "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************

recode p04c06 (6 8=1 Patron) (5 7=2 CuentataPropia) (1/4=3 Employee) (9=4 "Unpaid family worker") (else=.),g ( categopri_ci )
note: categopri_ci=4 includes unpaid workers in general though the label says "unpaid FAMILY worker"

label define categopri_ci 1"Patron" 2"Cuenta propia", modify 
label define categopri_ci 3"Empleado" 4" No remunerado" , modify
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

/*

p04c06:
           1 empleado(a) del gobierno
           2 empleado(a) privada(o)
           3 jornalero(a) o peón(a)
           4 empleado(a) en casa particular
           5 trabajador(a) por cuenta propia no agricola
           6 patrón(a) empleador(a), socio(a) no agricola
           7 trabajador(a) por cuenta propia agricola
           8 patrón(a) empleador(a), socio(a) agricola
           9 trabajador(a) no renumerado(a)
		  
*/

******************
***categosec_ci***
******************

/*
p05b04 En este segundo trabajo usted es? 103
1 Empleado(a) del gobierno
2 Empleado(a) privado(a)
3 Jornalero(a) o peón
4 Empleado(a) doméstico(a)
5 Trabajador(a) por cuenta propia no agrícola
6 Patrón(a) empleador(a), socio(a) no agrícola
7 Trabajador(a) por cuenta propia agrícola
8 Patrón(a) empleador(a) socio(a) agrícola
9 Trabajador familiar sin pago
*/

recode p04d05 (6 8=1 Patron) (5 7=2 CuentataPropia) (1/4=3 Employee) (9=4 "Unpaid family worker") (else=.),g ( categosec_ci )
note: categosec_ci=4 includes unpaid workers in general though the label says "unpaid FAMILY worker"

label define categosec_ci 1"Patron" 2"Cuenta propia", modify 
label define categosec_ci 3"Empleado" 4" No remunerado" , modify
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p04c08a==1 & p04c07==1) & categopri_ci==3
replace tipocontrato_ci=2 if (p04c08a==2 & p04c07==1)  & categopri_ci==3
replace tipocontrato_ci=3 if (p04c07==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & p04c01==1
replace nempleos_ci=2 if emp_ci==1 & (p04c01>=2)
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 

/*
******************************
*	firmapeq_ci
******************************
g firmapeq_ci=(p04c05>=1 & p04c05<=5) if emp_c==1
replace firmapeq_ci=. if emp_ci!=1 | p04c05==.
la var firmapeq_ci "Trabajadores formales. 1=5 o menos trabajadores"
*/
******************************
*	spublico_ci 
******************************
gen spublico_ci=(p04c06 ==1) 
replace spublico_ci=. if emp_ci==0 
la var spublico_ci "Trabaja en sector publico"

*************
**ocupa_ci***
*************

gen ocupa_ci=.
replace ocupa_ci=1 if (p04c02b_2d >=21 & p04c02b_2d <=35) & emp_ci==1
replace ocupa_ci=2 if (p04c02b_2d >=11 & p04c02b_2d <=14) & emp_ci==1
replace ocupa_ci=3 if (p04c02b_2d >=41 & p04c02b_2d <=44) & emp_ci==1
replace ocupa_ci=4 if (p04c02b_2d ==52 | p04c02b_2d ==95) & emp_ci==1
replace ocupa_ci=5 if (p04c02b_2d ==51 | (p04c02b_2d >=53 & p04c02b_2d <=54) | p04c02b_2d ==91) & emp_ci==1
replace ocupa_ci=6 if ((p04c02b_2d >=61 & p04c02b_2d <=63) | p04c02b_2d ==92) & emp_ci==1
replace ocupa_ci=7 if ((p04c02b_2d >=71 & p04c02b_2d <=83) | p04c02b_2d ==93) & emp_ci==1
replace ocupa_ci=8 if (p04c02b_2d >=1 & p04c02b_2d <=3) & emp_ci==1
replace ocupa_ci=9 if (p04c02b_2d ==94 | p04c02b_2d ==96) & emp_ci==1
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

*************
**rama_ci****
*************

gen rama_ci=.
replace rama_ci=1 if p04c04b_2d >=1 & p04c04b_2d <=3
replace rama_ci=2 if p04c04b_2d >=5 & p04c04b_2d <=9
replace rama_ci=3 if p04c04b_2d >=10 & p04c04b_2d <=33
replace rama_ci=4 if p04c04b_2d >=35 & p04c04b_2d <=39
replace rama_ci=5 if p04c04b_2d >=41 & p04c04b_2d <=43
replace rama_ci=6 if (p04c04b_2d >=45 & p04c04b_2d <=47) | (p04c04b_2d >=55 & p04c04b_2d <=56)
replace rama_ci=7 if (p04c04b_2d >=49 & p04c04b_2d <=53) | p04c04b_2d ==61 
replace rama_ci=8 if p04c04b_2d >=64 & p04c04b_2d <=68
replace rama_ci=9 if (p04c04b_2d >=69 & p04c04b_2d <=99) | (p04c04b_2d >=58 & p04c04b_2d <=60) | (p04c04b_2d >=62 & p04c04b_2d <=63)
label var rama_ci "Rama de actividad"
label val rama_ci rama_ci


************
*durades_ci*
************
* MGD 04/02/2015: variable correcta p04f01
gen durades_ci=.
*replace durades_ci= (p04f08/4.3) 
replace durades_ci= (p04f01/4.3) 
replace durades_ci=0.23 if p04f01==0
label variable durades_ci "Duracion del desempleo en meses"

******************
**antiguedad_ci***
******************
gen mesaanio= (p04c30b/12)

egen antiguedad_ci=rsum( p04c30a mesaanio) if emp_ci==1, m 
*2014,05 modificado por MLO
*recode antiguedad_ci .=0 


*******************
***tamemp_ci***
*******************
       
*Guatemala Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if p04c05>=1 & p04c05<=5
replace tamemp_ci = 2 if (p04c05>=6 & p04c05<=50)
replace tamemp_ci = 3 if (p04c05>50) & p04c05!=.

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp_ci1 = 1 if p04c05>1 & p04c05<=5
replace tamemp_ci1 = 2 if (p04c05>=6 & p04c05<=50)
replace tamemp_ci1 = 3 if (p04c05>50) & p04c05!=.

gen tamemp_o = 1 if p04c05>=1 & p04c05<=9
replace tamemp_o = 2 if (p04c05>=10 & p04c05<50)
replace tamemp_o = 3 if (p04c05>=50) & p04c05!=.

label define tamemp_o 1 "[1-9]" 2 "[10-49]" 3 "[50 y mas]"
label value tamemp_o tamemp_o
label var tamemp_o "Tamaño de empresa-OECD"


*******************
***categoinac_ci***
*******************

/*

p04a02:
           1 trabajar
           2 buscar trabajo
           3 estudiar
           4 quehaceres del hogar
           5 jubilado(a) o pensionado(a)
           6 rentista
           7 enfermo(a) o convaleciente
           8 capacidades diferentes
           9 cuidado de niños(as)
          10 cuidado de adultos(as) mayores
          11 cuidado de enfermo(a)
          98 otro

*/


gen categoinac_ci =1 if (p04a02==5 & condocup_ci==3)
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

***************
***ylmpri_ci***
***************


foreach var of varlist p04c13b p04c14b p04c15b p04c16b p04c17b p04c21b{ 
g `var'tdp=`var'/12 
}
*egen ylmpri_ci= rsum(p04c10 p04c11c p04c12b *tdp), missing



*MLO cambio esta restricccion: * Se excluye a las variables p04c22 p04c23 que tienen que ver con las ganancias netas de la actividad.
/*Employee. Salario bruto mensual en Quetzales. p04c10
Otros: ganancia mensual en actividad no agricola y agricola. p04c22 p04c23
*/

egen ylmpri_ci= rsum(p04c10 p04c11c p04c12b *tdp p04c22 p04c23), missing
* MLO 2014, 08 incorporacion de ganancias netas de otras actividades antes excluidas.
* se observa en los datos que los que responden missing en  p04c22 (ingreso del mes anterior) tienen valor en p04c23(ingreso promedio mensual) 
* >>supuesto: en caso de missing en ingreso neto de otras actividades en l mes se utiliza el valor promedio indicado en la variable p04c23
* de esta forma se esta incluyendo el ingreso de los no asalariados (patrones y cuenta propia) dentro del ingreso no laboral que hasta ahora cubria solo a asalariados

notes:ylmpri_ci is montly wage in Quetzales
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*****************
***nrylmpri_ci***
*****************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


****************
***ylnmpri_ci***
****************

egen ylnmpri_ci=rsum(p04c18b p04c19b p04c20b), missing
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************
destring (p04d06 p04d09b), replace
foreach var of varlist p04d08b p04d10b p04d11b{
g `var'tdpsec=`var'/12
}
egen ylmsec_ci=rsum(p04d06 p04d09b *tdpsec), missing
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

g ylnmsec_ci=p04d07b
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
egen ylm_ci= rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"
*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  
 

*ModificaciÃ³n SGR la variable correcta es p05a18b, la p05a17b es el total
*egen rem=rsum( p05a17b p05a18b p05a19b), missing
egen rem=rsum( p05a18b p05a19b p05a20b), missing

foreach var of varlist p05a01b p05a02b p05a03b p05a04b p05a05b p05a06b p05a07b rem{
g `var'tdp3=`var'/3
}

foreach var of varlist p05a08b p05a09b p05a10b p05a11b p05a12b p05a13b p05a14b p05a15b{
g `var'tdp3=`var'/12
}

egen ynlm_ci=rsum(*tdp3*), missing
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

******************************
*	autocons_ci 
******************************
g autocons_ci=p05a20b
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing
la var autocons_ch "Autoconsumo del Hogar"

****************
***remesas_ci***
****************
g remesas_ci=remtdp3
la var remesas_ci "Cash remittances from abroad"


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


******************************************************************************
*	EDUCATION 
******************************************************************************

******************************
*	asiste_ci: Definida aqui como inscriptos en plantel educativo en el presente anio escolar  OK
******************************
g asiste_ci=(p03a02==1)
replace asiste_ci=. if p03a02==.
notes: asiste is defined as enrolled in the current school year
*******************************************
*	aedu_ci: Anios de educacion COMPLETADOS
*******************************************
/*
 p03a03
 Preprimaria  |1       
     Primaria |2      
       Básico |3      
Diversificado |4     
     Superior |5       
     Maestría |6     
    Doctorado |7      
*/

*  Se fija la condición "& p03a04==0" porque en el formulario se señala que si aún no ha aprobado ningún 
* año (en determinado nivel) se escriba 0. Entonces, por ejemplo, se entiende que una persona que indique
* estar en secundaria sin haber aprobado ningún nivel todavía, entonces tendría 6 años de educación.

/* Corrección Marcela Rubio Septiembre 2014: el cuestionario cambió de 2011 y 2012 y el último ya no incluye la categoría de Ningún nivel
pero se debe tomar en cuenta la pregunta anterior la cual es Asiste o asistió a algún pabel educativo y aquellos que contestaron que 
Nunca asistieron deben caer bajo la categoría Ninguno o 0 años de educación

*/
gen aedu_ci=.
replace	 aedu_ci=0  if (p03a03==1) | (p03a03==2 & p03a04==0) | p03a02==3
*replace	 aedu_ci=0  if (p03a03==1) | (p03a03==2 & p03a04==0)

*Primaria
replace aedu_ci=1  if (p03a03==2 & p03a04==1)
replace aedu_ci=2  if (p03a03==2 & p03a04==2)
replace aedu_ci=3  if (p03a03==2 & p03a04==3)
replace aedu_ci=4  if (p03a03==2 & p03a04==4)
replace aedu_ci=5  if (p03a03==2 & p03a04==5)
replace aedu_ci=6  if (p03a03==2 & p03a04==6) | (p03a03==3 & p03a04==0)


*Secundaria
replace aedu_ci=7  if (p03a03==3 & p03a04==1) 
replace aedu_ci=8 if (p03a03==3 & p03a04==2) 
replace aedu_ci=9 if (p03a03==3 & p03a04==3) | (p03a03==4 & p03a04==0)
replace aedu_ci=10 if (p03a03==4 & p03a04==1) 
replace aedu_ci=11 if (p03a03==4 & p03a04==2) 
replace aedu_ci=12 if (p03a03==4 & p03a04==3) | (p03a03==5 & p03a04==0)

*Superior
replace aedu_ci=13 if (p03a03==5 & p03a04==1)
replace aedu_ci=14 if (p03a03==5 & p03a04==2)
replace aedu_ci=15 if (p03a03==5 & p03a04==3)
replace aedu_ci=16 if (p03a03==5 & p03a04==4)
replace aedu_ci=17 if (p03a03==5 & (p03a04==5 | p03a04==6)) | ((p03a03==6 | p03a03==7)& p03a04==0)

*Postgrado
replace aedu_ci=18 if (p03a03==6 | p03a03==7) & p03a04==1 
replace aedu_ci=19 if (p03a03==6 | p03a03==7) & p03a04==2 
replace aedu_ci=20 if (p03a03==6 | p03a03==7) & p03a04==3 
replace aedu_ci=21 if (p03a03==6 | p03a03==7) & p03a04==4
replace aedu_ci=22 if (p03a03==6 | p03a03==7) & p03a04==5
replace aedu_ci=23 if (p03a03==6 | p03a03==7) & p03a04==6
*replace aedu_ci=.  if p03a03==. | p03a04 ==.
replace aedu_ci=.  if p03a03==. & p03a04 ==. & p03a02==.

label var aedu_ci "Anios de educacion aprobados"


******************************
*	eduno_ci
******************************
g byte eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
la var eduno_ci "Personas sin educacion. Excluye preescolar"
******************************
*	edupi_ci 
******************************
g byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
la var edupi_ci "Personas que no han completado Primaria"
******************************
*	edupc_ci 
******************************
g byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
la var edupc_ci "Primaria Completa"
******************************
*	edusi_ci 
******************************
g byte edusi_ci=(aedu_ci>6 & aedu_ci<11)
replace edusi_ci=. if aedu_ci==.
la var edusi_ci "Secundaria Incompleta"
******************************
*	edusc_ci 
******************************
g byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
la var edusc_ci "Secundaria Completa"
******************************
*	edus1i_ci 
******************************
g byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
la var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"
******************************
*	edus1c_ci 
******************************
g byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
la var edus1c_ci "1er ciclo de Educacion Secundaria Completo"
******************************
*	edus2i_ci 
******************************
g byte edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
la var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"
******************************
*	edus2c_ci 
******************************
g byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
la var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media
******************************
*	eduui_ci 
******************************
g byte eduui_ci=(aedu_ci>11 & aedu_ci<17) 
replace eduui_ci=. if aedu_ci==.
la var eduui_ci "Universitaria o Terciaria Incompleta"
******************************
*	eduuc_ci 
******************************
g byte eduuc_ci=(aedu_ci>=17)
replace eduuc_ci=. if aedu_ci==.
la var eduuc_ci "Universitaria o Terciaria Completa"
******************************
*	edupre_ci 
******************************
g byte edupre_ci=0 if aedu_ci ~=.
replace edupre_ci=1 if p03a03==1 & aedu_ci ~=.
la var edupre_ci "Asiste a Educacion preescolar"
******************************
*	asispre_ci
******************************
g byte asispre_ci=.
**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

******************************
*	pqnoasis_ci 
******************************
g pqnoasis_ci=. /*NA*/

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

******************************
*	repite_ci 
******************************
g repite_ci=.  /*NA*/
******************************
*	repiteult_ci 
******************************
g repiteult_ci=. /*NA*/
******************************
*	edupub_ci 
******************************
g edupub_ci=.
replace edupub_ci=1 if p03a03==1 
replace edupub_ci=0 if p03a03==2
la var edupub_ci "Personas que asisten a centros de ensenanza publicos"

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
g aguared_ch=0
replace  aguared_ch=1 if p02a05a==1| p02b03==1|p02b03==2


*****************
***aguadist_ch***
*****************
/*No se genera. Entiendo que la distribucion seria de interes solo para viviendas con acceso a red. Si la fuente principal de agua es 
de lluvia, rio, o lago, pienso que no deberia ser clasificada aqui como "fuera de la vivienda" sino en "agua mala". De cualquier
forma el cuestionario de Guatemala no permite distinguir si la principal fuente de agua esta a menos o mas de 100 metros 

aguadist_ch: Ubicación de la principal fuente de agua
1 Adentro de la casa
2 Afuera de la casa pero adentro del terreno (o a menos de 100mts de distancia)
3 Afuera de la casa y afuera del terreno (o a más de 100mts de distancia)

                         11559         1  tubería (red) dentro de la vivienda
                          1651         2  tubería (red) fuera de la vivienda
                           336         3  chorro público
                          2135         4  pozo perforado público o privado
                          1294         5  río, lago o manantial
                           100         6  camión cisterna
                           268         7  agua de lluvia
                           576        98  otra fuente, ¿cuál? */

gen aguadist_ch= 1 if  p02b03 ==1
replace aguadist_ch= 2 if  p02b03 ==2
replace aguadist_ch= 3 if  p02b03 >=3 & p02b03 <=9
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************
g aguamala_ch=0
replace aguamala_ch=1 if p02b03==7|p02b03==5
label var aguamala_ch "Agua unimproved según MDG" 


*****************
***aguamide_ch***
*****************
recode p02a05e (1=1 Sí) (else=0 No), g (aguamide_ch)
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************
g luz_ch=0
replace luz_ch=1 if p02b05==1 | p02a05c==1
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************
recode p02a05f (1=1 Sí) (else=0 No), g (luzmide_ch)
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

recode p02b05 (1/3=1 Sí) (else=0 No), g(combust_ch)
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

recode p02b06 (1 2 4=1 Sí) (else=0 No), g(bano_ch)
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=.
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


******************************
*	des1_ch
******************************
g des1_ch=0 if p02b06==5
replace des1_ch=1 if p02a05b==1 | p02b06==1 | p02b06==2
replace des1_ch=2 if p02b06==4
replace des1_ch=3 if p02b06==3

/*
des1_ch Tipo de desagüe incluyendo la definición de "Unimproved" del MDG
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general o a una cámara séptica
2 El desagüe está conectado a un pozo ciego o es una letrina.
3 El desagüe se comunica con la superficie: desemboca en un río o en la calle.*/
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

recode des1_ch (1 2=1) (3=2), g (des2_ch)

/*
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general, a una cámara o fosa séptica, o a un pozo ciego o letrina.
2 Cualquier otro*/
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

recode p02a04 (7=0 "Dirt floor")(1/6=1 "Permanent materials")(98=2 "Other materials"),g (piso_ch)
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales", modify
label val piso_ch piso_ch

**************
***pared_ch***
**************

recode p02a02 (7 8=0 "Non-permanent materials")(1/6=1 "Permanent materials")(98=2 "Other materials"),g (pared_ch)
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales", modify
label val pared_ch pared_ch


**************
***techo_ch***
**************

recode p02a03 (5=0 "Non-permanent materials")(1/4 =1 "Permanent materials")(98=2 "Other materials"),g (techo_ch)
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 1"Otros materiales", modify
label val techo_ch techo_ch


**************
***resid_ch***
**************

recode p02b08 (1 2=0 "Recolección pública o privada")(3 4=1 "Quemados o enterrados")(5=2 Tirados)(else=3 Otros),g (resid_ch)
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados", modify
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", modify
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (p02b03 >=1 & p02b03 <=4) | p02b03 ==7
replace aguamejorada_ch = 0 if (p02b03 >=5 & p02b03 <=6) | p02b03 ==8 | p02b03 ==98
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (p02b06 >=1 & p02b06 <=4)
replace banomejorado_ch = 0 if  p02b06 == 5  

*************
***dorm_ch***
*************

g dorm_ch= p02b02
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************
g cuartos_ch=p02b01
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************
g telef_ch=(p02b07a==1)
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

g internet_ch=(p02b07c==1)
label var internet_ch "El hogar posee conexión a Internet"


************
***cel_ch***
************

g cel_ch=(p02b07b==1)
label var cel_ch "El hogar tiene servicio telefonico celular"



**************
***vivi1_ch***
**************

recode p02a01 (1=1 House) (2=2 Apartment) (3/8=3 Other) ,g ( vivi1_ch)
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros", modify
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

g vivi2_ch=(p02a01 >=1 & p02a01 <=2)
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************
recode p02a07 (3=0 Alquilada) (1=1 "Propia y totalmente pagada") (2=2 "Propia y en proceso de pago") (4=3 Ocupada) ,g ( viviprop_ch)
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2 "Propia y en proceso de pago", modify
label def viviprop_ch 3"Ocupada (propia de facto)", modify
label val viviprop_ch viviprop_ch


****************
***vivitit_ch***
****************
gen vivitit_ch= .
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

g vivialq_ch= p02a09 if viviprop_ch==0
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch= p02a08
label var vivialqimp_ch "Alquiler mensual imputado"

*******************
***  benefdes_ci***
*******************

g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
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

	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first




compress


saveold "`base_out'", replace


log close











