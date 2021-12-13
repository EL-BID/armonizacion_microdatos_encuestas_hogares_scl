*(Versión Stata 14)
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

local PAIS VEN
local ENCUESTA ENCOVI
local ANO "2019"
local ronda a 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: ENCOVI
Round: a
Autores: Lina Arias -Email: lm.arias405@uniandes.edu.co
Fecha última modificación: Julio 2020

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use "`base_in'", clear


cap destring, replace

************
****pais****
************
gen str pais_c="VEN"

**********
***anio***
**********
gen anio_c=2020

*********
***mes***
*********
gen mes_c=.

**********
***zona***
**********

gen zona_c=.
label define zona 0 "Rural" 1 "Urbana"
label value zona zona

****************
*** idh_ch ***
****************


sort interview__id
egen idh_ch=group(interview__id)


gen idp_ci=Miembro__id


***************
***factor_c***
***************
gen factor_ci=pondera
label var factor_ci "Factor de Expansion del Individuo"

gen factor_ch=pondera
label var factor_ch "Factor de expansion del Hogar"


***************
***upm_ci***
***************

clonevar upm_ci=SEGMENTO
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

gen estrato_ci=.
label variable estrato_ci "Estrato"

***********
* Region_c *
************

gen region_c=  entidad
label define region_c  ///
1	"Distrito Federal"  ///
2	"Amazonas " ///
3	"Anzoategui"  ///
4	"Apure " ///
5	"Aragua " ///
6	"Barinas " ///
7	"Bolívar " ///
8	"Carabobo " ///
9	"Cojedes " ///
10	"Delta Amacuro"  ///
11	"Falcón"  ///
12	"Guárico"  ///
13	"Lara"  ///
14	"Mérida"  ///
15	"Miranda"  ///
16	"Monagas"  ///
17	"Nueva Esparta"  /// 
18	"Portuguesa"  ///
19	"Sucre"  ///
20	"Táchira"  ///
21	"Trujillo"  ///
22	"Yaracuy"  ///
23	"Zulia"  ///
24	"Vargas" 
	    
label value region_c region_c
label var region_c " Primera División política - Entidades Federativas"

************************
*** region según BID ***
************************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if s6q2==1
replace relacion_ci=2 if s6q2==2
replace relacion_ci=3 if s6q2==3 | s6q2==4
replace relacion_ci=4 if s6q2>=5 & s6q2<=11 /* Otros familiares */
replace relacion_ci=5 if s6q2==12 
replace relacion_ci=6 if s6q2==13
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico (inc fam Serv. Dom.)"
label value relacion_ci relacion_ci
*16 missing

****************************
***VARIABLES DEMOGRAFICAS***
****************************


**********
***sexo***
**********
gen sexo_ci=s6q3
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci
*16 obs missing 

**********
***edad***
**********
** Generating Edad

gen edad_ci=s6q5
label var edad_ci "Edad del Individuo"

*****************
***civil_ci***
*****************
gen byte civil_ci=.
replace civil_ci=1 if s6q13  ==8
replace civil_ci=2 if (s6q13  >=1  & s6q13<=4)
replace civil_ci=3 if s6q13  ==5 | s6q13==6
replace civil_ci=4 if s6q13  ==7
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

************
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


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
	*** afroind_ci ***
	***************
gen afroind_ci=. 

	***************
	*** afroind_ch ***
	***************
gen afroind_ch=. 

	*******************
	*** afroind_ano_c ***
	*******************
gen afroind_ano_c=.		

	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 

	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************
**************
***ocupa_ci***
**************
*La clasificación no corresponde a la clasificación estandarizada de ocupación tp47
tab s9q13
tab s9q14 s9q13

gen ocupa_ci=.
replace ocupa_ci=1 if s9q13>=2 & s9q13<4
replace ocupa_ci=2 if s9q13==1
replace ocupa_ci=3 if s9q13==4
replace ocupa_ci=4 if s9q13==5 
replace ocupa_ci=6 if s9q13==6
replace ocupa_ci=7 if s9q13>=7 & s9q13<9
replace ocupa_ci=8 if s9q13==10
replace ocupa_ci=8 if s9q13==9
label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6"TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8"FUERZAS ARMADAS" 9"OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

*5 missing que debieron responder pero no respondieron

*****************
***horastot_ci***
*****************
//tab s9q16, mi

recode s9q18 (98=.) (99=.)

gen byte horastot_ci=.
replace horastot_ci=s9q18 if s9q18>=0 & s9q18 <168 //fixed, it was mistakenly using s9q16, which is horaspri_ci
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

*27 obs que debieron responder 
************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (s9q1==1 | (s9q2==1 | s9q2==2) | (s9q3==1))
replace condocup_ci=2 if (s9q1==2 | (s9q2==3) | s9q3==2) & (s9q6==1 | s9q7==1 | s9q8==1 | s9q8==2)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2 & edad_ci!=.a
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"



******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if s9q15==5 & condocup_ci==1
replace categopri_ci=2 if (s9q15==6 | s9q15==7 | s9q15==9)  & condocup_ci==1
replace categopri_ci=3 if (s9q15 >=1 & s9q15  <=4)   & condocup_ci==1
replace categopri_ci=4 if s9q15 ==8 & condocup_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" //5 obs que son missing en la pregunta original s9q15
label values categopri_ci categopri_ci

*5 missing que debieron responder pero no respondieron

****************
*afiliado_ci****
****************

gen afiliado_ci=0     if condocup_ci==1 | condocup_ci==2 
replace afiliado_ci=1 if (s9q20__1==1 | s9q22__1==1 | s8q18==1 | s8q18==2 | s9q28__2==1) & afiliado_ci==0

label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************

gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if s9q36==1 & cotizando_ci==0
 
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 


********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=.
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=.
*Empresas grandes
replace tamemp_ci=.
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

*****************
*categoinac_ci***
*****************
gen categoinac_ci = .
replace categoinac_ci = 1 if ((s9q12==6) & condocup_ci==3)
replace categoinac_ci = 2 if ((s9q12==1) & condocup_ci==3)
replace categoinac_ci = 3 if ((s9q12==3) & condocup_ci==3)
replace categoinac_ci = 4 if ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "Jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label values categoinac_ci categoinac_ci
tab s9q12 categoinac_ci if condocup_ci==3, mi

**los missing se dejan en la categoria otros.
 
*************
**pension_ci*
*************
*DZ se agrega la variable pension contributiva*
gen pension_ci=.
replace pension_ci=1 if (s9q28__2==1 | s9q28__3==1 | s9q28_petro==1)
replace pension_ci=0 if pension_ci==.
label var pension_ci "1=Recibe pension contributiva"
**se deja como cero las personas que responden que no reciben.  
*************
*ypen_ci*
*************
*DZ se agrega la variable valor de la pension contributiva*

egen ypen_ci= rsum(ing_pe2 ing_pe3 ing_pet) if s9q28__2==1  | s9q28__3==1 | s9q28_petro==1
label var ypen_ci "Valor de la pension contributiva"


***************
*pensionsub_ci*
***************
*DZ Octubre 2017-Se crea la variable de pension subsidiada-Gran Mision en amor mayor

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=.
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
/*La encuesta fue levantada entre noviembre 2019 y abril 2020, se toman los dos periodos para el salario dependiendo de la fecha de la encuesta
Se toma el salario de octubre 2019 y enero 2020, http://www.sistematemis.com.ve/Salarios-Minimos-Venezuela?action=  */
tempvar ano
gen `ano'=substr(fechae, 1, 4)
tab `ano'
gen salmm_ci=250000.00 if `ano'=="2020"
replace salmm_ci=150000.00 if `ano'=="2019" 
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
tab s7q11

gen tecnica_ci=(s7q11==7)
label var tecnica_ci "=1 formacion terciaria tecnica"	

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
***horaspri_ci***
*****************

gen byte horaspri_ci=.
replace horaspri_ci=s9q16  if s9q16>=0 & s9q16<168
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"


****************
***durades_ci***
****************
* Variable categórica s9q8
gen durades_ci = .


*****************
***desalent_ci***
*****************
gen desalent_ci=(s9q11 ==1 | s9q11 ==3 | s9q11 ==5 | s9q11 ==6) 
label var desalent_ci "Trabajadores desalentados: personas que creen que por alguna razón no conseguirán trabajo"
label define desalent_ci 1 "Trabajador desalentado" 0 "No es trabajador desalentado" 
label values desalent_ci desalent_ci 


***************
***subemp_ci***
***************
/*No es posible construir esta variable ya que no se realizo esta pregunta*
*/

gen subemp_ci=.
label var subemp_ci "Personas que trabajan 30 horas a la semana o menos y están dispuestas a trabajar más"
label define subemp_ci 1 "Subempleado " 0 "No subempleado" 
label values subemp_ci subemp_ci 

*******************
***tiempoparc_ci***
*******************
*No se puede construir esta variable por la misma razón que no se pudo construir la variable anterior, no se realizo la pregunta*

gen tiempoparc_ci=.


*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if s9q14==1
replace rama_ci=2 if s9q14==2
replace rama_ci=3 if s9q14==3
replace rama_ci=4 if s9q14==4
replace rama_ci=5 if s9q14==5
replace rama_ci=6 if s9q14==6
replace rama_ci=7 if s9q14==7
replace rama_ci=8 if s9q14==8
replace rama_ci=9 if s9q14==9 | s9q14==10
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6"Comercio al por mayor y menor, restaurantes, hoteles" 7"Transporte y almacenamiento" 8"Establecimientos financieros, seguros, bienes inmuebles" 9"Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************
***categosec_ci***
******************
gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"



*****************
***nempleos_ci***
*****************
capture drop nempleos_ci
gen byte nempleos_ci=.
label var nempleos "Numero de empleos"

*****************
***spublico_ci***
*****************
gen byte spublico_ci=.
replace spublico_ci=1 if emp_ci==1 & (s9q15 ==1 )
replace spublico_ci=0 if emp_ci==1 & (s9q15>1) 
label var spublico "Personas que trabajan en el sector publico"

****************
***ylmpri_ci ***
****************
*ESTA INCLUIDO EL INGRESO POR TODOS LOS TRABAJOS REALIZADOS. 
recode ing1 (99=.) (98=.)
egen ylmpri_ci= rowtotal(ylabor_asa ylabor_patron ylabor_ctapro), m
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

*******************
*** ylmhopri_ci ***
*******************
gen ylmhopri_ci=.
replace ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


***************
***ylmsec_ci***
***************
gen ylmsec_ci=.
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

******************
*** ylmotros_ci***
******************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso Laboral Monetario Otros Trabajos"

*****************
*** ylnmpri_ci***
*****************
*ESTA INCLUIDO EL INGRESO POR TODOS LOS TRABAJOS REALIZADOS. 
gen ylnmpri_ci=ylabor_asaesp
label var ylnmpri_ci "Ingreso Laboral NO Monetario de la Actividad Principal"

***************
***ylmsec_ci***
***************
gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso Laboral NO Monetario de la Actividad Secundaria"

******************
***ylnmotros_ci***
******************
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso Laboral NO Monetario Otros Trabajos"

************
***ylm_ci***
************
egen ylm_ci= rsum(ylmpri_ci ylmsec_ci),m
replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci), m
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

*************
***ynlm_ci***
*************
*ESTO NO ES NECESARIO PORQUE EL MONTO DE LAS PENSIONES SOLO LO REGISTRAN UNA VEZ.
/*recode tmhp52bs (98=.) (99=.)
gen pensiones=pmhp55bs if pmhp55bs>99
*La cifra declarada en tmhp45bs (incluida en ylmpri_ci) a veces se duplica en las preguntas pmhp55bs* relacionadas con pensiones porque los mayores a 40 años que aún trabajan declaran el mismo monto en los dos
g aux = tmhp52bs-pensiones

replace pensiones = pensiones - tmhp52bs  if aux== 0 // se hace este ajuste para evitar duplicar las cifras.
*/
gen ynlm_ci= ing_nlabor_sri 
label var ynlm_ci "Ingreso NO Laboral Monetario"

*************
***ynlnm_ci***
*************
gen ynlnm_ci= .
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

******************
*** tcylmpri_ci***
******************
gen tcylmpri_ci=.
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

*******************
*** autocons_ci ***
*******************
egen autocons_ci=rsum(ing_i ing_r), m
label var autocons_ci "Autoconsumo Individual"

*****************
***remesas_ci***
*****************
gen remesas_ci=ing_ex4
label var remesas_ci "Remesas Individuales"



************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************
capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembros_ci==1
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

******************
*** tcylmpri_ch***
******************
gen tcylmpri_ch=.


*************
*** ylm_ch***
*************
egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

****************
*** ylmnr_ch ***
****************
egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

***************
*** ylnm_ch ***
***************
egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

***************
*** ynlm_ch ***
***************
egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"


****************
*** ynlnm_ch ***
****************
egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"


***************
***ylmho_ci ***
***************
gen ylmho_ci=.
replace ylmho_ci=ylm_ci/(horastot*4.3)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"



*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch=rentai
label var rentaimp_ch "Rentas imputadas del hogar"


*******************
*** autocons_ch ***
*******************

egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

******************
*** remesas_ch ***
******************
egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"


*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la actividad actual"


replace ylnm_ch=. if ylnm_ci==.
replace ynlnm_ch=. if ynlnm_ci==.
replace autocons_ch=. if autocons_ci==.
replace remesas_ch=. if remesas_ci==.
replace ylm_ch =. if miembros_ci==0
replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0

****************************
***VARIABLES DE EDUCACION***
****************************

***************
***asiste_ci***
***************
gen byte asiste_ci=.
replace asiste_ci=1 if s7q3 ==1 
replace asiste_ci=0 if s7q3 ==2 
label var asiste "Personas que actualmente asisten a centros de enseñanza"


***************
***/aedu_ci****
***************

recode s7q11* s7q4*  (99=.) (98=.)
recode s7q11b (7/2014=7)

gen byte aedu_ci=.
replace aedu_ci=0                 if s7q11==1 | s7q11==2
replace aedu_ci=s7q11a           if s7q11==3

replace aedu_ci=s7q11a+9           if s7q11==4
replace aedu_ci=s7q11b+9 if s7q11==4 & aedu_ci==. //reportaron el grado donde tenian que reportar los anhos de educacion superior

replace aedu_ci=s7q11a           if s7q11==5

replace aedu_ci=s7q11a+6         if s7q11==6
replace aedu_ci=s7q11b+6 if s7q11==6 & aedu_ci==.

replace aedu_ci = 12 + s7q11b if s7q11ba==1 & (s7q11==7 | s7q11==8) // técnico (TSU) | Universitario
replace aedu_ci = 17 + s7q11b if s7q11ba==1 & s7q11==9 // postgrado
replace aedu_ci = 12 + s7q11c*0.5 if s7q11ba==2 & (s7q11==7 | s7q11==8) // técnico (TSU) | Universitario
replace aedu_ci = 17 + s7q11c*0.5 if s7q11ba==2 & s7q11==9 //posgrado
replace aedu_ci = 12 + s7q11d*0.25 if s7q11ba==3 & (s7q11==7 | s7q11==8) // técnico (TSU) | Universitario
replace aedu_ci = 17 + s7q11d*0.25 if s7q11ba==3 & s7q11==9 //posgrado

**para los que tienen missing en el regimen de estudio
replace aedu_ci = 12 + s7q11b if (s7q11==7 | s7q11==8) & aedu_ci==. // técnico (TSU) | Universitario
replace aedu_ci = 17 + s7q11b if s7q11==9 & aedu_ci==. // postgrado
replace aedu_ci = 12 + s7q11c*0.5 if (s7q11==7 | s7q11==8) & aedu_ci==. // técnico (TSU) | Universitario
replace aedu_ci = 17 + s7q11c*0.5 if s7q11==9 & aedu_ci==. //posgrado
replace aedu_ci = 12 + s7q11d*0.25 if (s7q11==7 | s7q11==8) & aedu_ci==. // técnico (TSU) | Universitario
replace aedu_ci = 17 + s7q11d*0.25 if s7q11==9 & aedu_ci==. //posgrado

label variable aedu_ci "Años de Educacion"

							
**************
***eduno_ci*** // ningún nivel de instrucción
**************
gen eduno_ci=.
replace eduno=1 if s7q11==1 // ningún nivel, asiste y no asiste
replace eduno=0 if s7q11>1 // preescolar para arriba, asiste y no asiste
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

***************
***edupre_ci***
***************
gen edupre_ci=.
replace edupre=1 if s7q11==2 // preescolar asiste y no asiste
replace edupre=0 if s7q11==1 | s7q11>2 // ningún nivel y básica o más, asiste o no asiste
label var edupre_ci "Educacion preescolar"

**************
***edupi_ci***
**************
gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu_ci>=6 & aedu_ci!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

**************
***edupc_ci***
**************
gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu_ci!=.) 
label var edupc_ci "1 = personas que han completado el nivel primario"

**************
***edusi_ci***
**************
gen edusi_ci=.
replace edusi=1 if aedu_ci>6 & aedu_ci<12
replace edusi=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=11 & aedu_ci!=.)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

**************
***edusc_ci***
**************
gen edusc_ci=.
replace edusc=1 if aedu_ci==12 
replace edusc=0 if (aedu_ci>=0 & aedu_ci<12) 
label var edusc_ci "1 = personas que han completado el nivel secundario"


**************
***eduui_ci***
**************


gen eduui_ci= (aedu_ci>12 & aedu_ci<17)
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

***************
***eduuc_ci***
***************
gen byte eduuc_ci= (aedu_ci>=17)
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"

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


**************
***eduac_ci***
**************
gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "


***************
***asispre_ci**
***************
g asispre_ci=.
replace asispre_ci=1 if asiste_ci==1 & (s7q4==2)
replace asispre_ci=1 if asiste_ci==1 & (s7q4==1) & (edad_ci<=5)
recode asispre_ci (.=0)
la var asispre_ci "Asiste a educacion prescolar"
	


***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************
***repiteult_ci***
******************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"


**************
***pqnoasis***
**************
gen byte pqnoasis_ci=.
replace pqnoasis=s7q2 if s7q2>1 & s7q2<9
replace pqnoasis=10 if s7q2==9
replace pqnoasis=11 if s7q2==10 |  s7q2==11
replace pqnoasis=13 if s7q2==14
replace pqnoasis=14 if s7q2==15
replace pqnoasis=15 if s7q2==16 | s7q2==1
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Culmino sus estudios" 2 "Escuela distante" 3 "Escuela cerrada" 4 "Muchos paros/inasistencia de maestros" 5 "Costo de los útiles" 6 "Costo de los uniformes" 7 "Enfermedad/Discapacidad " 8 "Tiene que trabajar " 9 "No quiso seguir estudiando "  10 " Inseguridad al asistir al centro educat " 11 "Discriminación o violencia" 12 "Por embarazo/cuidar los hijos" 13 "Tiene que ayudar en tareas del hogar " 14 "No lo considera importante " 15 "otros"
label values pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci ==5 & pqnoasis_ci==6
replace pqnoasis1_ci = 2 if pqnoasis_ci ==8
replace pqnoasis1_ci = 3 if pqnoasis_ci ==7  
replace pqnoasis1_ci = 4 if pqnoasis_ci ==14 | pqnoasis_ci==9
replace pqnoasis1_ci = 5 if pqnoasis_ci ==13 | pqnoasis_ci==12
replace pqnoasis1_ci = 6 if pqnoasis_ci ==1
replace pqnoasis1_ci = 8 if pqnoasis_ci ==2  | pqnoasis_ci ==3
replace pqnoasis1_ci = 9 if pqnoasis_ci ==15 | pqnoasis_ci==11 | pqnoasis_ci==10 | pqnoasis_ci==4 | pqnoasis_ci==3

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch = (s5q13__1==1)
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=.

*****************
***aguamala_ch***
*****************
gen aguamala_ch=.

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.

************
***luz_ch***
************

gen luz_ch=.
label var luz_ch "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************
gen luzmide_ch=.

****************
***combust_ch***
****************
gen combust_ch=.

*************
***bano_ch***
*************
gen bano_ch=.
replace bano_ch = 1 if s4q9>=1 & s4q9<=4
replace bano_ch = 0 if s4q9==5

label var bano_ch "El hogar tiene algun tipo de servicio higienico"

***************
***banoex_ch***
***************

gen banoex_ch=1 if s5q2==1
replace banoex_ch=0 if s5q2==2
label var banoex_ch "El servicio higiénico es de uso exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if s4q9==5
replace des1_ch=1 if s4q9==1 | s4q9==2 
replace des1_ch=2 if s4q9==4 | s4q9==3
label var des1_ch "Tipo de desagüe incluyendo definición de unimproved del MDG"

*************
***des2_ch***
*************


gen des2_ch=.
replace des2_ch=0 if s4q9==5
replace des2_ch=1 if s4q9==1 | s4q9==2 | s4q9==3 | s4q9==4
label var des2_ch "Tipo de desagüe sin incluir la definición de unimproved de los MDG"

*************
***piso_ch***
*************

gen piso_ch=.
replace piso_ch=0 if s4q1==3
replace piso_ch=1 if s4q1==1 | s4q1==2 
replace piso_ch=2 if s4q1==4 

label var piso_ch "Material predominante en el piso de la casa"
label define piso_ch 0 "Tierra" 1 "Materiales permanentes" 2 "Otros"
label values piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch=.
replace pared_ch=0 if s4q2==8
replace pared_ch=1 if s4q2>=1 & s4q2<=5
replace pared_ch=2 if s4q2==7 | s4q2==6

label var pared_ch "Materiales de construcción de las paredes"
label define pared_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales"
label values pared_ch pared_ch

**************
***techo_ch***
**************

gen techo_ch=.
replace techo_ch=0 if s4q3==5
replace techo_ch=1 if s4q3>=1 & s4q3<=4
*No hay categorías que pueden clasificarse en "Otros materiales"

label var techo_ch "Material de construcción del techo"
label define techo_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales" 
label values techo_ch techo_ch

**************
***resid_ch***
**************
gen resid_ch=.
*Se elimina la pregunta en 2016
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
recode s5q1 (98=.) (99=.)
gen dorm_ch=s5q1 
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

****************
***cuartos_ch***
****************

gen cuartos_ch=.
label var cuartos_ch "Cantidad de habitaciones en el hogar"
	
***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************

gen telef_ch=(s5q6__12==1)
label var telef_ch "Hogar tiene servicio telefónico fijo"
label values telef_ch telef_ch

***************
***refrig_ch***
***************

gen refrig_ch=.
replace refrig_ch=1 if s5q6__1 ==1
replace refrig_ch=0 if s5q6__1 ==0
label var refrig_ch "El hogar posee heladera o refrigerador"
label define refrig_ch 0 "No" 1 "Sí"
label values refrig_ch refrig_ch

**************
***freez_ch***
**************
gen freez_ch=.

*************
***auto_ch***
*************
gen auto_ch=(s5q4a >=1 & s5q4a <5)
label var auto_ch "El hogar posee automóvil particular"
label define auto_ch 0 "No" 1 "Sí"
label values auto_ch auto_ch

**************
***compu_ch***
**************

gen compu_ch=.
replace compu_ch=1 if s5q6__4 ==1
replace compu_ch=0 if s5q6__4 ==0
label var compu_ch "El hogar posee computadora"
label define compu_ch 0 "No" 1 "Sí"
label values compu_ch compu_ch


*****************
***internet_ch***
*****************

gen internet_ch=.
replace internet_ch=1 if s5q6__5==1
replace internet_ch=0 if s5q6__5==0
label var internet_ch "El hogar posee conexión a internet"
label define internet_ch 0 "No" 1 "Sí"
label values internet_ch internet_ch

************
***cel_ch***
************
gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefónico celular"
label define cel_ch 0 "No" 1 "Sí"
label values cel_ch cel_ch

**************
***vivi1_ch***
**************
gen vivi1_ch=.
replace vivi1_ch = 1 if s4q4 ==1 | s4q4 ==2
replace vivi1_ch = 2 if s4q4 ==3
replace vivi1_ch = 3 if s4q4 >=4 & s4q4 <=7
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label define vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros"
label values vivi1_ch vivi1_ch


**************
***vivi2_ch***
**************

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3
label var vivi2_ch "La vivienda en la que reside el hogar es una casa o un departamento"
label define vivi2_ch 0 "No" 1 "Sí"
label values vivi2_ch vivi2_ch


*****************
***viviprop_ch***
*****************
*Revisar la categor'ia 3
gen viviprop_ch=.
replace viviprop_ch=0 if s5q7==3 | s5q7==4
replace viviprop_ch=1 if s5q7==1
replace viviprop_ch=2 if s5q7==2
replace viviprop_ch=3 if s5q7>=4 & s5q7  <=10
label var viviprop_ch "Propiedad de la vivienda"
label define viviprop_ch 0 "Alquilada" 1 "Propia y totalmente pagada" 2 "Propia en proceso de pago" 3 "Ocupada (propia de facto)"
label values viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************
gen vivitit_ch=.
replace vivitit_ch=1 if (s5q12>=1 & s5q12<=2) 
replace vivitit_ch=0 if s5q12>2 
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
*s5q8a.LA PREGUNTA HACE REFERENCIA A CUÁNTO PAGA POR ALQUILER O CRÉDITO HIPOTECARIO, ENTONCES SE CONDICIONA SOLO A LOS QUE REPORTAN QUE LA VIVIENDA ES ALQUILADA. 
gen vivialq_ch=.
replace vivialq_ch=Tgasto_alquiler if viviprop_ch==0
label var vivialq_ch "Alquiler mensual de la vivienda"

*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=rentai

*******************
***  benefdes_ci  ***
*******************

g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci  ***
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
	
	gen migrante_ci=(s6q6!=1) if (s6q6!=.a | s6q6!=.)
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(migrante_ci==1 & (inlist(s6q6,2,3,4,5,6,7,8) | inlist(s6q6_os,"Haití","Nicaragua","República Dominicana"))) if migrante_ci!=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen miglac_ci=(migrante_ci==1 & (inlist(s6q6,2,3,4,5,6,7,8) | inlist(s6q6_os,"Haití","Nicaragua","República Dominicana"))) if migrante_ci!=.
	replace miglac_ci = 0 if miglac_ci != 1 & migrante_ci == 1
	replace miglac_ci = . if migrante_ci == 0
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"

	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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
salmm_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first


foreach var of varlist  lp19_ci lp31_ci lp5_ci {

		
		format `var' %18.4f
		}

compress

**********
***anio***
**********
replace anio_c=2019

save "`base_out'", replace


log close

