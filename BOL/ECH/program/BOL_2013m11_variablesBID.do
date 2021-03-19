
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
local ANO "2013"
local ronda m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11
Autores: Marcela Rubio (03/03/2015)
Última versión: 2021/03/09 (Cesar Lins)

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use "`base_in'", clear


	****************
	* region_BID_c *
	****************
	
gen region_BID_c=3

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
	************
	* region_c *
	************

gen region_c=real(substr(folio,1,1))

label define region_c ///
1"Chuquisaca"         ///     
2"La Paz"             ///
3"Cochabamba"         ///
4"Oruro"              ///
5"Potosí"             ///
6"Tarija"             ///
7"Santa Cruz"         ///
8"Beni"               ///
9"Pando"              
label value region_c region_c
label var region_c "division politica, estados"

***************
***factor_ch***
***************
gen factor_ch= factor
label variable factor_ch "Factor de expansion del hogar"

	***************
	***upm_ci***
	***************
gen upm_ci=upm
	***************
	***estrato_ci***
	***************
gen estrato_ci=estrato

***************
****idh_ch*****
***************
sort folio
gen idh_ch = folio
destring idh_ch, replace
format idh_ch %14.0g
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************
gen idp_ci= nro2a
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 	if area==2
replace zona_c=1 	if area==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

label variable zona_c "Zona del pais"


************
****pais****
************
gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2013
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=11
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if  s2_05==1
replace relacion_ci=2 if  s2_05==2
replace relacion_ci=3 if  s2_05==3
replace relacion_ci=4 if  s2_05>=4 &  s2_05<=9
replace relacion_ci=5 if  s2_05==10 |  s2_05==12 
replace relacion_ci=6 if  s2_05==11

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
gen sexo_ci = s2_02
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci= s2_03 
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 		if s2_10==1
replace civil_ci=2 		if s2_10==2 | s2_10==3
replace civil_ci=3 		if s2_10==4 | s2_10==5
replace civil_ci=4 		if s2_10==6
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

*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021										

	***************
	***afroind_ci***
	***************
**Pregunta: como boliviano o boliviana, pertenece a una nación o pueblo indígena? (s3a_2a) (PERTENCE 1, NO PERTENECE 2, NO SOY BOLIVIANO/BOLIVIANA 3)
**Pregunta: a qué nación o pueblo pertenece? (s3a_02b) (AFROBOLIVIANO 001 - TODAS LAS OTRAS CATEGORIAS SON INDIGENAS)

gen afroind_ci=. 
replace afroind_ci=1  if s3a_02a==1 & s3a_02b!="001"
replace afroind_ci=2 if s3a_02b=="001"
replace afroind_ci=3 if s3a_02a ==2 
replace afroind_ci=9 if s3a_02a==3 


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = sum(afroind_jefe), by(idh_ch) 

drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2013


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
* Líneas de pobreza oficiales
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =z
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =zext

label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
* http://www.economiayfinanzas.gob.bo/index.php?opcion=com_contenido&ver=contenido&id=2811&id_item=&seccion=306&categoria=446

gen salmm_ci= 1200
label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci= s6_52b==1	
recode afiliado_ci .=0  if condact>=1 & condact<=3
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************

gen tipopen_ci=.
replace tipopen_ci=1 if s7_01a>0 &  s7_01a!=.
replace tipopen_ci=2 if s7_01d>0 & s7_01d!=.
replace tipopen_ci=3 if s7_01b>0 & s7_01b!=.
replace tipopen_ci=4 if s7_01c>0 & s7_01c!=. 
replace tipopen_ci=12 if (s7_01a>0 & s7_01d>0) & (s7_01a!=. & s7_01d!=.)
replace tipopen_ci=13 if (s7_01a>0 & s7_01b>0) & (s7_01a!=. & s7_01b!=.)
replace tipopen_ci=23 if (s7_01d>0 & s7_01b>0) & (s7_01d!=. & s7_01b!=.)
replace tipopen_ci=123 if (s7_01a>0 & s7_01d>0 & s7_01b>0) & (s7_01a!=. & s7_01d!=. & s7_01b!=.)
label define tipopen_ci 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci tipopen_ci
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

gen condocup_ci=.
*Mod. MLO 2015,10: se consideran otras causas excepcionales 
*replace condocup_ci=1 if s6_01==1 | s6_02<=6 | s6_03==1
replace condocup_ci=1 if s6_01==1 | s6_02<=6 | (s6_03>=1 & s6_03<=7)
*replace condocup_ci=2 if (s6_01==2 | s6_02==7 | s6_03>1) & s6_05==1 & s6_04==1 
replace condocup_ci=2 if (s6_01==2 | s6_02==7 | s6_03>7) & s6_05==1 & s6_04==1 
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci = 1 if condocup_ci==2 & s6_07==1
replace cesante_ci = 0 if condocup_ci==2 & s6_07==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if s6_20>=1 & s6_20<=5
replace tamemp_ci=2 if s6_20>=6 & s6_20<=49
replace tamemp_ci=3 if s6_20>49 & s6_20!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*Bolivia micro 1 a 4 pequeña 5 a 14 Mediana 15-40 Grande mas 41
gen tamemp=.
replace tamemp=1 if s6_20>=1 & s6_20<=4
replace tamemp=2 if s6_20>=5 & s6_20<=14
replace tamemp=3 if s6_20>=15 & s6_20<=40
replace tamemp=4 if s6_20>=41 & s6_20!=.
label var tamemp "# empleados en la empresa segun rangos"
label define tamemp 1 "Micro" 2 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp tamemp


*************
**pension_ci*
*************

egen aux_p=rsum(s7_01a s7_01b s7_01c s7_01d), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************
*11/4/2015 MGD: no considerar ceros
recode aux_p (0=.)
gen ypen_ci=aux_p 
*recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci = (s7_01ea==1)  
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen  ypensub_ci=s7_01eb if s7_01eb>1 & s7_01eb!=. 
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
	
/* Esta sección es para los residentes habituales del hogar mayores a 7 años. Sin embargo, las variables construidas 
por el centro de estadística tienen en cuenta a la población con 10 años o más. Esto no es un problema dado que el 
programa para generar los indicadores de sociómetro restrige  todo a 15 o más años para que haya comparabilidad entre
países
*/

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
gen desalent_ci=(emp_ci==0 & (s6_10==3 | s6_10==4))
replace desalent_ci=. if emp_ci==.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************

  *s6_23m: 23b. cuantas horas en promedio trabaja al dia .. ? (minutos)
  *s6_23h: 23a. cuantas horas en promedio trabaja al dia .. ? (horas)
  *s6_22: 22. cuantos dias a la semana trabaja

gen aux_min= s6_23m/60
egen horas_min= rsum(s6_23h aux_min)
gen horaspri_ci = horas_min*s6_22
replace horaspri_ci=. if s6_22==. | s6_23h==. | s6_23m==.
replace horaspri_ci=. if emp_ci!=1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
/*
s6_39a:  39a. cuantos dias trabajÓ la semana anterior ?
s6_39b1: 39b. cuantas horas promedio al dia trabajÓ la semana anterior ?
s6_39b2: 39b. cuantos minutos promedio al dia trabajÓ la semana anterior ?
*/

gen aux_min2=s6_39b2/60
egen horas_min2=rsum(s6_39b1 aux_min2)
gen horassec_ci= horas_min2*s6_39a 
replace horassec_ci=. if s6_39a==. | s6_39b1==. | s6_39b2==.
replace horassec_ci=. if emp_ci!=1

egen horastot_ci= rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = . if horaspri_ci == . & horassec_ci == .
replace horassec_ci=. if emp_ci~=1

***************
***subemp_ci***
***************

* Se considera subempleo visible: quiere trabajar mas horas y esta disponible. 
gen subemp_ci=0
replace subemp_ci=1 if (s6_46==1 & s6_47==1)  & horaspri_ci <= 30 & emp_ci==1

label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
*Mod. MLO 2015, 10
replace tiempoparc_ci=(s6_46==2 & horaspri_ci<30 & emp_ci == 1)
replace tiempoparc_ci=. if emp_ci==0
*replace tiempoparc_ci=1 if s6_46==2 & horastot_ci<=30 & emp_ci == 1
*replace tiempoparc_ci=0 if s6_46==2 & emp_ci == 1 & horastot_ci>30
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
gen categopri_ci=.
replace categopri_ci=1 if s6_16>=4 & s6_16<=6
replace categopri_ci=2 if s6_16==3
replace categopri_ci=3 if s6_16==1 | s6_16==2 | s6_16==8
replace categopri_ci=4 if s6_16==7
replace categopri_ci=. if emp_ci~=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
gen categosec_ci=.
replace categosec_ci=1 if s6_36>=4 & s6_36<=6
replace categosec_ci=2 if s6_36==3
replace categosec_ci=3 if s6_36==1 | s6_36==2 | s6_36==8
replace categosec_ci=4 if s6_36==7
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if s6_21==3 & categopri_ci==3
replace tipocontrato_ci=2 if s6_21==1 & categopri_ci==3
*replace tipocontrato_ci=3 if ((s6_21==2 | s6_21==4) | tipocontrato_ci==.) & categopri_ci==3
*Mod. MLO 2015,10
replace tipocontrato_ci=3 if ((s6_21==2 | s6_21==4 | s6_21==5) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s6_34==1
label var nempleos_ci "Número de empleos" 
label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
label value nempleos_ci nempleos_ci

				
/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
replace firmapeq_ci=1 if  s6_20>=1 & s6_20<=5 
replace firmapeq_ci=0 if  s6_20>=6 & s6_20!=.
label var firmapeq_ci "Trabajadores informales"
 */
 
*****************
***spublico_ci***
*****************
gen spublico_ci=.
replace spublico_ci=1 if s6_17==1
replace spublico_ci=0 if s6_17==2 | s6_17==3 | s6_17==4
replace spublico_ci=. if emp_ci~=1
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
*cob_op:
*NA: No se puede estandarizar ya que no se distingue entre dos categorias:
*comerciantes y vendedores y trabajadores en servicios 

* Modificacion MGD 07/24/2014: clasificacion CIUO -08
*rename  aux
gen aux=cods6_11b
destring aux, replace
gen ocupa_ci=.
replace ocupa_ci=1 if ((aux>=210 & aux<=352) | (aux>=20 & aux<=35))& emp_ci==1
replace ocupa_ci=2 if ((aux>=110 & aux<=149) | (aux>=10 & aux<=14)) & emp_ci==1
replace ocupa_ci=3 if ((aux>=410 & aux<=449) | (aux>=40 & aux<=44))& emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | aux==52 | (aux>=950 & aux<=952)) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=530 & aux<=549) | aux==51 | (aux>=910 & aux<=912) | (aux>=960 & aux<=962)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=639) | (aux>=71 & aux<=83) | aux==61 | aux==62 | aux==92 | aux==921) & emp_ci==1
replace ocupa_ci=7 if ((aux>=710 & aux<=839) | (aux>=930 & aux<=949) | aux==93 | aux==94)& emp_ci==1
replace ocupa_ci=8 if (aux>=0 & aux<=5) & emp_ci==1
drop aux
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"

*************
***rama_ci***
*************
/*
caeb_op:
0 Agricultura,Ganadería,Caza,Pesca y Silv
1 Explotación de Minas y Canteras
2 Industria Manufacturera
3 Suministro de electricidad,gas,vapor y
4 Suministro de agua, evac. de aguas res,
5 Construcción
6 Venta por mayor y menor,reparación de a
7 Transporte y Almacenamiento
8 Actividades de alojamiento y servicio d
9 Informaciones y Comunicaciones
10 Intermediación Financiera y Seguros
11 Actividades inmobiliarias
12 Servicios Profesionales y Técnicos
13 Actividades de Servicios Administrativo
14 Adm. Pública, Defensa y Seguridad Socia
15 Servicios de Educación
16 Servicios de Salud y Asistencia Social
17 Actividades artisticas,entretenimiento
18 Otras actividades de servicios
19 Actividades de Hogares Privados
20 Servicio de Organismos Extraterritorial
99 NS/NR
*/ 

gen rama_ci=.
replace rama_ci=1 if caeb_op==0 & emp_ci==1
replace rama_ci=2 if caeb_op==1 & emp_ci==1
replace rama_ci=3 if caeb_op==2 & emp_ci==1
replace rama_ci=4 if (caeb_op==3 | caeb_op==4) & emp_ci==1
replace rama_ci=5 if caeb_op==5 & emp_ci==1
replace rama_ci=6 if (caeb_op>=6 & caeb_op<=8) & emp_ci==1 
replace rama_ci=7 if caeb_op==7 & emp_ci==1
replace rama_ci=8 if (caeb_op>=10 & caeb_op<=11) & emp_ci==1
replace rama_ci=9 if (caeb_op==9 | (caeb_op>=12 & caeb_op<=20)) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
gen durades_ci=.
replace durades_ci=s6_08a/4.3  if s6_08b==2
replace durades_ci=s6_08a       if s6_08b==4
replace durades_ci=s6_08a*12   if s6_08b==8
label variable durades_ci "Duracion del desempleo en meses"

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.	
replace antiguedad_ci=s6_14a/52.14  	if s6_14b==2 & emp_ci==1
replace antiguedad_ci=s6_14a/12   if s6_14b==4 & emp_ci==1
replace antiguedad_ci=s6_14a	   	if s6_14b==8 & emp_ci==1
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***categoinac_ci***
*******************
*Modificacion MLO, 2015 m4 (se cambió s5_14 por s6_09)
gen categoinac_ci =1 if (s6_09==3 & condocup_ci==3)
replace categoinac_ci = 2 if  ( s6_09==1 & condocup_ci==3)
replace categoinac_ci = 3 if  ( s6_09==2 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringe a ocupados solamente*/
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


**************
***INGRESOS***
**************

*************************
*********LABORAL*********
*************************
/*
s2_38f:
1  diario
2  semanal
3  quincenal
4  mensual
8  anual
*/

*******************
* salario líquido *
*******************
gen yliquido = .
replace yliquido= s6_25a*30		if s6_25b==1
replace yliquido= s6_25a*4.3	if s6_25b==2
replace yliquido= s6_25a*2		if s6_25b==3
replace yliquido= s6_25a		if s6_25b==4
replace yliquido= s6_25a/2		if s6_25b==5
replace yliquido= s6_25a/3		if s6_25b==6
replace yliquido= s6_25a/6		if s6_25b==7
replace yliquido= s6_25a/12		if s6_25b==8

**************
* comisiones *
**************

gen ycomisio = .
replace ycomisio= s6_27a1*30	if s6_27a2==1
replace ycomisio= s6_27a1*4.3	if s6_27a2==2
replace ycomisio= s6_27a1*2		if s6_27a2==3
replace ycomisio= s6_27a1		if s6_27a2==4
replace ycomisio= s6_27a1/2		if s6_27a2==5
replace ycomisio= s6_27a1/3		if s6_27a2==6
replace ycomisio= s6_27a1/6 	if s6_27a2==7
replace ycomisio= s6_27a1/12 	if s6_27a2==8

****************
* horas extras *
****************
gen yhrsextr= .
replace yhrsextr= s6_27b1*30	if s6_27b2==1
replace yhrsextr= s6_27b1*4.3	if s6_27b2==2
replace yhrsextr= s6_27b1*2		if s6_27b2==3
replace yhrsextr= s6_27b1		if s6_27b2==4
replace yhrsextr= s6_27b1/2		if s6_27b2==5
replace yhrsextr= s6_27b1/3		if s6_27b2==6
replace yhrsextr= s6_27b1/6	    if s6_27b2==7
replace yhrsextr= s6_27b1/12	if s6_27b2==8

*********
* prima *
*********

gen yprima = .
replace yprima = s6_26a/12

*************
* aguinaldo *
*************

gen yaguina = .
replace yaguina = s6_26b/12

*************
* alimentos *
*************
gen yalimen = .
replace yalimen= s6_30a3*30		if s6_30a2==1 & s6_30a1==1
replace yalimen= s6_30a3*4.3	if s6_30a2==2 & s6_30a1==1
replace yalimen= s6_30a3*2		if s6_30a2==3 & s6_30a1==1
replace yalimen= s6_30a3		if s6_30a2==4 & s6_30a1==1
replace yalimen= s6_30a3/2		if s6_30a2==5 & s6_30a1==1
replace yalimen= s6_30a3/3		if s6_30a2==6 & s6_30a1==1
replace yalimen= s6_30a3/6		if s6_30a2==7 & s6_30a1==1
replace yalimen= s6_30a3/12		if s6_30a2==8 & s6_30a1==1

**************
* transporte *
**************

gen ytranspo = .
replace ytranspo= s6_30b3*30	if s6_30b2==1 & s6_30b1==1
replace ytranspo= s6_30b3*4.3	if s6_30b2==2 & s6_30b1==1
replace ytranspo= s6_30b3*2		if s6_30b2==3 & s6_30b1==1
replace ytranspo= s6_30b3		if s6_30b2==4 & s6_30b1==1
replace ytranspo= s6_30b3/2		if s6_30b2==5 & s6_30b1==1
replace ytranspo= s6_30b3/3		if s6_30b2==6 & s6_30b1==1
replace ytranspo= s6_30b3/6		if s6_30b2==7 & s6_30b1==1
replace ytranspo= s6_30b3/12	if s6_30b2==8 & s6_30b1==1

**************
* vestimenta *
**************

gen yvesti = .
replace yvesti= s6_30c3*30		if s6_30c2==1 & s6_30c1==1
replace yvesti= s6_30c3*4.3		if s6_30c2==2 & s6_30c1==1
replace yvesti= s6_30c3*2		if s6_30c2==3 & s6_30c1==1
replace yvesti= s6_30c3			if s6_30c2==4 & s6_30c1==1
replace yvesti= s6_30c3/2		if s6_30c2==5 & s6_30c1==1
replace yvesti= s6_30c3/3		if s6_30c2==6 & s6_30c1==1
replace yvesti= s6_30c3/6		if s6_30c2==7 & s6_30c1==1
replace yvesti= s6_30c3/12		if s6_30c2==8 & s6_30c1==1

************
* vivienda *
************

gen yvivien = .
replace yvivien= s6_30d3*30		if s6_30d2==1 & s6_30d1==1
replace yvivien= s6_30d3*4.3	if s6_30d2==2 & s6_30d1==1
replace yvivien= s6_30d3*2		if s6_30d2==3 & s6_30d1==1
replace yvivien= s6_30d3		if s6_30d2==4 & s6_30d1==1
replace yvivien= s6_30d3/2		if s6_30d2==5 & s6_30d1==1
replace yvivien= s6_30d3/3		if s6_30d2==6 & s6_30d1==1
replace yvivien= s6_30d3/6		if s6_30d2==7 & s6_30d1==1
replace yvivien= s6_30d3/12		if s6_30d2==8 & s6_30d1==1


*************
* otros *
*************

gen yotros = .
replace yotros= s6_30e3*30		if s6_30e2==1 & s6_30e1==1
replace yotros= s6_30e3*4.3	    if s6_30e2==2 & s6_30e1==1
replace yotros= s6_30e3*2	    if s6_30e2==3 & s6_30e1==1
replace yotros= s6_30e3		    if s6_30e2==4 & s6_30e1==1
replace yotros= s6_30e3/2	    if s6_30e2==5 & s6_30e1==1
replace yotros= s6_30e3/3		if s6_30e2==6 & s6_30e1==1
replace yotros= s6_30e3/6		if s6_30e2==7 & s6_30e1==1
replace yotros= s6_30e3/12		if s6_30e2==8 & s6_30e1==1


**********************************
* ingreso act. pr independientes *
**********************************
*Aquí se tiene en cuenta el monto de dinero que les queda a los independientes para el uso del hogar
gen yactpri = .
replace yactpri= s6_33a*30		if s6_33b==1
replace yactpri= s6_33a*4.3		if s6_33b==2
replace yactpri= s6_33a*2		if s6_33b==3
replace yactpri= s6_33a			if s6_33b==4
replace yactpri= s6_33a/2		if s6_33b==5
replace yactpri= s6_33a/3		if s6_33b==6
replace yactpri= s6_33a/6		if s6_33b==7
replace yactpri= s6_33a/12		if s6_33b==8

*********************
* salario liquido 2 *
*********************
/* No hay las categorias de anual
  1  diario
  2  semanal
  3  quincenal
  4  mensual
  5  bimestral
  6  trimestral
  7  semestral
*/

gen yliquido2 = .
replace yliquido2= s6_41a*30	if s6_41b==1
replace yliquido2= s6_41a*4.3	if s6_41b==2
replace yliquido2= s6_41a*2		if s6_41b==3
replace yliquido2= s6_41a		if s6_41b==4
replace yliquido2= s6_41a/2		if s6_41b==5
replace yliquido2= s6_41a/3		if s6_41b==6
replace yliquido2= s6_41a/3		if s6_41b==7

*****************
* Horas extra 2 *
*****************


gen yhrsextr2 = .
replace yhrsextr2=s6_42a2/12 if s6_42a1==1


***************************************
* alimentos, transporte y vestimenta2 *
***************************************


gen yalimen2 = .
replace yalimen2= s6_42b2/12	if s6_42b1==1


**************
* vivienda 2 *
**************

gen yvivien2= .
replace yvivien2= s6_42c2/12	if s6_42c1==1


*************************
******NO-LABORAL*********
*************************

*************
* intereses *
*************
gen yinteres = .
replace yinteres = s7_02a	

**************
* alquileres *
**************

gen yalqui = .
replace yalqui = s7_02b		


****************
* otras rentas *
****************

gen yotren = .
replace yotren = s7_02c	

**************
* jubilacion *
**************

gen yjubi = .
replace yjubi = s7_01a

**************
* benemerito *
**************

gen ybene = .
replace ybene = s7_01b

*************
* invalidez *
*************

gen yinvali = .
replace yinvali = s7_01c

**********
* viudez *
**********

gen yviudez = .
replace yviudez = s7_01d



************************
* alquileres agricolas *
************************

gen yalqagri = .
replace yalqagri =  s7_03a/12		


**************
* dividendos *
**************

gen ydivi = .
replace ydivi =  s7_03b/12


*************************
* alquileres maquinaria *
*************************

gen yalqmaqui = .
replace yalqmaqui = s7_03c/12

 
******************
* indem. trabajo *
******************

gen yindtr = .
replace yindtr =  s7_04a/12


******************
* indem. seguros *
******************

gen yindseg = .
replace yindseg = s7_04b/12


******************
* renta dignidad *
******************

gen ybono = .
replace ybono = s7_01eb

******************
* otros ingresos *
******************

gen yotring = .
replace yotring = s7_04c/12


*******************
* asist. familiar *
*******************
/*
  2  semanal
  3  quincenal
  4  mensual
  5  bimestral
  6  trimestral
  7  semestral
  8  anual
*/
* No hay la categoria de diario en s7_05a2
gen yasistfam = .
replace yasistfam= s7_05a1*4.3		if s7_05a2==2
replace yasistfam= s7_05a1*2		if s7_05a2==3
replace yasistfam= s7_05a1			if s7_05a2==4
replace yasistfam= s7_05a1/2		if s7_05a2==5
replace yasistfam= s7_05a1/3		if s7_05a2==6
replace yasistfam= s7_05a1/6		if s7_05a2==7
replace yasistfam= s7_05a1/12		if s7_05a2==8


*********************
* Trans. monetarias *
*********************
* No hay la categoria de diario en s7_05b2

gen ytransmon = .
replace ytransmon= s7_05b1*4.3		if s7_05b2==2
replace ytransmon= s7_05b1*2		if s7_05b2==3
replace ytransmon= s7_05b1			if s7_05b2==4
replace ytransmon= s7_05b1/2		if s7_05b2==5
replace ytransmon= s7_05b1/3		if s7_05b2==6
replace ytransmon= s7_05b1/6		if s7_05b2==7
replace ytransmon= s7_05b1/12		if s7_05b2==8

***********
* remesas *
***********
/*
    MONEDA

A. BOLIVIANOS 
B. EUROS
C. DÓLARES
D. PESOS ARGENTINOS
E. REALES
F. PESOS CHILENOS
G. OTRO

http://www.bcb.gob.bo/?q=indicadores/cotizaciones
Al 31 de diciembre de 2013
*/

gen s6_112= .
replace s6_112 =  s7_09a 			if s7_09b== "A" /*bolivianos*/
replace s6_112 =  s7_09a*9.46795    if s7_09b== "B" /*euro*/
replace s6_112 =  s7_09a*6.86		if s7_09b== "C" /*dolar*/
replace s6_112 =  s7_09a*1.05220    if s7_09b== "D" /*peso argentino*/
replace s6_112 =  s7_09a*2.90721   	if s7_09b== "E" /*real*/
replace s6_112 =  s7_09a*0.01305	if s7_09b== "F" /*peso chileno*/
replace s6_112 =  s7_09a*2.45088   	if s7_09b== "G" /*soles*/

* se suman remesas monetarias y en especie
egen rem = rsum(s7_10  s6_112)

gen yremesas = .
replace yremesas= rem*4.3		if s7_07==2
replace yremesas= rem*2		    if s7_07==3
replace yremesas= rem			if s7_07==4
replace yremesas= rem/2			if s7_07==5
replace yremesas= rem/3			if s7_07==6
replace yremesas= rem/6			if s7_07==7
replace yremesas= rem/12		if s7_07==8

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

egen ylmpri_ci=rsum(yliquido ycomisio yhrsextr yprima yaguina yactpri), missing
replace ylmpri_ci=. if yliquido ==. & ycomisio ==. &  yhrsextr ==. & yprima ==. &  yaguina ==. &  yactpri==.  
replace ylmpri_ci=. if emp_ci~=1
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


******************
*** ylnmpri_ci ***
******************

egen ylnmprid=rsum(yalimen ytranspo yvesti yvivien yotros), missing
replace ylnmprid=. if yalimen==. & ytranspo==. & yvesti==. & yvivien==. & yotros==.   
replace ylnmprid=0 if categopri_ci==4

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

egen ylmsec_ci= rsum(yliquido2 yhrsextr2), missing
replace ylmsec_ci=. if emp_ci~=1 & yhrsextr2==. & yliquido2 ==.
replace ylmsec_ci=0 if categosec_ci==4
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


******************
****ylnmsec_ci****
******************

egen ylnmsec_ci=rsum(yalimen2  yvivien2), missing
replace ylnmsec_ci=. if yalimen2==.  & yvivien2==.  
replace ylnmsec_ci=0 if categosec_ci==4
replace ylnmsec_ci=. if emp_ci==0
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

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


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


 
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

egen ynlm_ci=rsum(yinteres yalqui yjubi ybene yinvali yviudez yotren yalqagri ydivi yalqmaqui yindtr yindseg ybono yotring yasistfam ytransmon yremesas ), missing
replace ynlm_ci=. if 	yinteres==. & yalqui==. & yjubi==. & ybene==. & yinvali==. & yviudez==. & yotren==. & yalqagri==. & ydivi==. & yalqmaqui==. & yindtr==. & yindseg==. & ///
			ybono==. & yotring==. & yasistfam==. & ytransmon==. & yremesas==. 
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


*****************
***remesas_ci***
*****************

gen remesas_ci=yremesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 




************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

**************
*** ylm_ch ***
**************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

****************
*** ylmnr_ch ***
****************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

*******************
*** remesas_ch ***
*******************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 

***************
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"

****************
*** ynlnm_ch ***
****************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*******************
*** autocons_ci ***
*******************
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

*******************
*** autocons_ch ***
*******************
gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"

*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch= .
label var rentaimp_ch "Rentas imputadas del hogar"

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

/*En esta sección es sólo para los mayores a los 5 años de edad*/

/*
 s5_02a

CERO AÑOS DE EDUCACIÓN
          11 ninguno
          12 curso de alfabetización
          13 educación pre-escolar
		  37 otros cursos (menor a 1 año)

PRIMARIA
          14 antiguo-básico (1 a 5 años)
          15 antiguo-intermedio (1 a 3 años)
          16 antiguo-medio (1 a 4 años)
          17 anterior-primaria (1 a 8 años)
		  19 actual-primaria (1 a 6 años)
          
SECUNDARIA
		  18 anterior-secundaria (1 a 4 años)
          20 actual-secundaria (1 a 6 años)
          
SUPERIOR
          28 normal
          29 universidad  pública (licenciatura)
          30 universidad privada (licenciatura)
          31 posgrado diplomado
          32 postgrado maestría
          33 postgrado doctorado
          34 técnico de universidad
          35 técnico de instituto (mayor o igual a un año)
          36 institutos de formacion militar y policial

		  
*No se consideran por no ser educación formal sino "Alternativa" o "No formal"
		  21 educacion básica de adultos (eba)
          22 centro de educación media de adultos (cema)
          23 educacion juvenil alternativa (eja)
          24 educación primaria de adultos(epa)
          25 educación secundaria de adultos (esa)
          26 educación técnica de adultos (eta)
          27 educación especial		  
*/

gen aedu_ci = .

* Ninguno o preescolar
replace aedu_ci = 0 if s5_02a==11 | s5_02a==12 | s5_02a==13

*Primaria & Secundaria
replace aedu_ci = 1 if s5_02b==1 & (s5_02a==14 | s5_02a==17 | s5_02a==19)
replace aedu_ci = 2 if s5_02b==2 & (s5_02a==14 | s5_02a==17 | s5_02a==19)
replace aedu_ci = 3 if s5_02b==3 & (s5_02a==14 | s5_02a==17 | s5_02a==19)
replace aedu_ci = 4 if s5_02b==4 & (s5_02a==14 | s5_02a==17 | s5_02a==19)
replace aedu_ci = 5 if s5_02b==5 & (s5_02a==14 | s5_02a==17 | s5_02a==19)
replace aedu_ci = 6 if (s5_02b==6 & (s5_02a==17 | s5_02a==19)) |  (s5_02b==1 & s5_02a==15)
replace aedu_ci = 7 if (s5_02b==7 & s5_02a==17) |  (s5_02b==2 & s5_02a==15) | (s5_02b==1 & s5_02a==20) 
replace aedu_ci = 8 if (s5_02b==8 & s5_02a==17) |  (s5_02b==3 & s5_02a==15) | (s5_02b==2 & s5_02a==20)
replace aedu_ci = 9 if (s5_02b==1 & s5_02a==16) |  (s5_02b==1 & s5_02a==18) | (s5_02b==3 & s5_02a==20)
replace aedu_ci = 10 if (s5_02b==2 & s5_02a==16) |  (s5_02b==2 & s5_02a==18) | (s5_02b==4 & s5_02a==20)
replace aedu_ci = 11 if (s5_02b==3 & s5_02a==16) |  (s5_02b==3 & s5_02a==18) | (s5_02b==5 & s5_02a==20)
replace aedu_ci = 12 if (s5_02b==4 & s5_02a==16) |  (s5_02b==4 & s5_02a==18) | (s5_02b==6 & s5_02a==20)

* Superior

replace aedu_ci = 13 if s5_02b==1 & (s5_02a==28 |  s5_02a==29 | s5_02a==30 |  s5_02a==34 | s5_02a==35 | s5_02a==36)
replace aedu_ci = 14 if s5_02b==2 & (s5_02a==28 |  s5_02a==29 | s5_02a==30 |  s5_02a==34 | s5_02a==35 | s5_02a==36)
replace aedu_ci = 15 if s5_02b==3 & (s5_02a==28 |  s5_02a==29 | s5_02a==30 |  s5_02a==34 | s5_02a==35 | s5_02a==36)
replace aedu_ci = 16 if s5_02b==4 & (s5_02a==28 |  s5_02a==29 | s5_02a==30 |  s5_02a==34 | s5_02a==35 | s5_02a==36)
replace aedu_ci = 17 if (s5_02b>=5 & s5_02b<=8) & (s5_02a==28 |  s5_02a==29 | s5_02a==30 |  s5_02a==29 | s5_02a==30 | s5_02a==34 | s5_02a==35 | s5_02a==36)


replace aedu_ci = 18 if s5_02b==1 & (s5_02a==31 |  s5_02a==32 | s5_02a==33)
replace aedu_ci = 19 if s5_02b==2 & (s5_02a==31 |  s5_02a==32 | s5_02a==33)
replace aedu_ci = 20 if s5_02b==3 & (s5_02a==31 |  s5_02a==32 | s5_02a==33)
replace aedu_ci = 21 if (s5_02b>=4 & s5_02b<=8) & (s5_02a==31 |  s5_02a==32)
replace aedu_ci = 21 if (s5_02b==4 & s5_02a==33)
replace aedu_ci = 22 if (s5_02b==5 & s5_02a==33)
replace aedu_ci = 22 if (s5_02b==8 & s5_02a==33)

**************
***eduno_ci***
**************

gen byte eduno_ci= 1 if aedu_ci == 0
replace eduno_ci= 0 if aedu_ci > 0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<=5)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=7 & aedu_ci<=11)
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
* Se incorpora la restricción s5_02b<8 para que sea comparable con los otros años LCM dic 2013

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & s5_02b<8)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & s5_02b==8) | (aedu_ci>=17 & aedu_ci<.)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa"

***************
***edupre_ci***
***************

gen byte edupre_ci=(s5_02a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
*Variable añadida por Iván Bornacelly - 01/12/2017
	g asispre_ci=.	
	replace asispre_ci=1 if s5_04==1 & s5_05a==13
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"
	
**************
***eduac_ci***
**************

* Se cambia para universidad completa o más 
gen byte eduac_ci=.
replace eduac_ci=1 if (s5_02a>=30 & s5_02a<=35 | s5_02a==37)
replace eduac_ci=0 if (s5_02a==29 | s5_02a>=36 )
label variable eduac_ci "Superior universitario vs superior no universitario"
/*cambio de eduuc_ci de LCM introcucido por YL solo para este año.
YL: No estoy segura de aceptar esta definicion pero la copio para hacerla comparable con
los otros años*/

***************
***asiste_ci***
***************

*(introducido por YL): Se cambia la forma de cálculo porque se deben considerar los rangos de edad 
*Modificación Mayra Sáenz Enero-2017: Se genera la dummy de acuerdo al documento metodológico.
gen asiste_ci= s5_04==1
/* 
gen asiste_ci= 1 if s5_04==1
replace asiste_ci = 0 if s5_04==2*/
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=s5_11 
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"vacación/receso" 2"falta de dinero" 3"por trabajo" 4"por enfermedad/accidente/discapacidad"
label def pqnoasis_ci 5"los establecimientos son distantes" 6"culminó sus estudios" 7"edad temprana/ edad avanzada", add
label def pqnoasis_ci 8"falta de interés" 9"labores de casa/ embarazo/cuidado de niños/as" 10"otra", add
label val pqnoasis_ci pqnoasis 


**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
gen    pqnoasis1_ci = 1 if s5_11==2
replace pqnoasis1_ci = 2 if s5_11==3
replace pqnoasis1_ci = 3 if s5_11==4 
replace pqnoasis1_ci = 4 if s5_11==8
replace pqnoasis1_ci = 5 if s5_11==9 
replace pqnoasis1_ci = 6 if s5_11==6 
replace pqnoasis1_ci = 7 if s5_11==7  
replace pqnoasis1_ci = 8 if s5_11==5
replace pqnoasis1_ci = 9 if s5_11==1  | s5_11==10

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

*En este año no se hace pregunta sobre si individuo repitió año
gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************

/*
s5_09:	   
 1 fiscal - pÚblico
 2 pÚblico de convenio
 3 particular - privado
*/

gen edupub_ci=(s5_09==1 | s5_09==2)
replace edupub_ci=. if s5_09==.
label var edupub_ci "Asiste a un centro de ensenanza público"

**************
***tecnica_ci*
**************

gen tecnica_ci = (s5_02a==34 | s5_02a==35)
label var tecnica_ci "1=formacion terciaria tecnica"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************


****************
***aguared_ch***
****************

/*s1_09:	
1 cañería de red dentro de la vivienda?
2 cañería de red fuera de la vivienda, pero dentro del lote
3 pileta pública
4 pozo entubado o perforado?
5 pozo excavado protegido?
6 pozo excavado no protegido
7 manantial o vertiente protegida?
8 río-acequia-vertiente no protegida?
9 agua embotellada?
10 carro repartidor (aguatero)
11 otro? (especifique)
*/

gen aguared_ch=(s1_09==1 | s1_09==2 | s1_09==3)
replace aguared_ch=. if s1_09==.
label var aguared_ch "Acceso a fuente de agua por red"


****************
***aguared_ch***
****************

gen aguadist_ch=1 if s1_09==1
replace aguadist_ch=2 if s1_09==2
replace aguadist_ch=3 if (s1_09==3 | s1_09==10)
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************

gen aguamala_ch=(s1_09==6 | s1_09==7 | s1_09==8)
replace aguamala_ch=. if s1_09==.
label var aguamala_ch "Agua unimproved según MDG" 

*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch=(s1_14==1)
replace luz_ch =. if   s1_14== .
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch= (s1_20==4 | s1_20==6)
replace combust_ch = . if s1_20==.
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

gen bano_ch= (s1_11>=1 &  s1_11<=3)
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=(s1_13==1)
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************
/*
s1_12
1 a la red de alcantarillado?
2 a una cámara séptica?
3 a un pozo de absorción?
4 a la superficie (calle/quebrada/rio)
5 otro
6 no sabe
*/

gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if s1_12==1 | s1_12==2
replace des1_ch=2 if s1_12==3
replace des1_ch=3 if s1_12==4
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if s1_12==1 | s1_12==2 | s1_12==3 
replace des2_ch=2 if s1_12==4 | s1_12==5
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

gen piso_ch=0 if s1_08==1 
replace piso_ch=1 if  s1_08>=2 &  s1_08<=7 
replace piso_ch=2 if  s1_08==8
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales"
label val piso_ch piso_ch


**************
***pared_ch***
**************

gen pared_ch=0 if s1_05 ==6
replace pared_ch=1 if s1_05>=1 & s1_05<=5
replace pared_ch=2 if s1_05==7
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val pared_ch pared_ch


**************
***techo_ch***
**************

gen techo_ch=0 if s1_07==4
replace techo_ch=1 if s1_07>=1 & s1_07<=3
replace techo_ch=2 if s1_07==5
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val techo_ch techo_ch

**************
***resid_ch***
**************

gen resid_ch =0 if s1_16==5 | s1_16==6
replace resid_ch=1 if s1_16==2 | s1_16==4
replace resid_ch=2 if s1_16==1 | s1_16==3
replace resid_ch=3 if s1_16==7
replace resid_ch=. if s1_16==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if (s1_09 >= 1 &  s1_09 <=6)
replace aguamejorada_ch = 0 if (s1_09 >= 7 &  s1_09 <=11)
		
		
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if ((s1_11 >= 1 & s1_11 <=2) & (s1_12 >= 1 & s1_12 <=3) & s1_13== 1)
replace banomejorado_ch = 0 if ((s1_11 >= 1 & s1_11 <=2) & (s1_12 >= 1 & s1_12 <=3) & s1_13== 2) | (s1_11 >= 3 & s1_11 <= 6)  | ((s1_11 >= 1 & s1_11 <=2)  & (s1_12 >= 4 & s1_12 <=5))


*************
***dorm_ch***
*************

gen dorm_ch= s1_23 
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************

*Esta pregunta no incluye baño, cocina, lavandería, garage, depósito o negocio 
gen cuartos_ch=s1_22
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************

gen cocina_ch=(s1_19==1)
replace cocina_ch = . if  s1_19==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=(s1_24==1)
replace telef_ch = . if s1_24==.
label var telef_ch "El hogar tiene servicio telefónico fijo"


******************
***refrig_ch***
******************
*Modificado Mayra Sáenz - Noviembre, 2016. Antes se generaban como missings
gen refrig_ch= posee_3==1
label var refrig_ch "El hogar posee refrigerador o heladera"

******************
***freez_ch***
******************
gen freez_ch=.
label var freez_ch "El hogar posee congelador"

******************
***auto_ch***
******************
*Modificado Mayra Sáenz - Noviembre, 2016. Antes se generaban como missings
gen auto_ch=posee_10==1
label var auto_ch "El hogar posee automovil particular"

******************
***compu_ch***
******************
*Modificado Mayra Sáenz - Noviembre, 2016. Antes se generaban como missings
gen compu_ch=posee_4==1
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************
gen internet_ch=(s1_26==1)
replace internet_ch = .   if  s1_26== .
label var internet_ch "El hogar posee conexión a Internet"



************
***cel_ch***
************

gen cel_ch= .
label var cel_ch "El hogar tiene servicio telefonico celular"


**************
***vivi1_ch***
**************

gen vivi1_ch=1 if s1_01==1
replace vivi1_ch=2 if s1_01==3
replace vivi1_ch=3 if s1_01==2 | (s1_01>=4 & s1_01<=6)
replace vivi1_ch=. if s1_01==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

*************
***vivi2_ch**
*************

gen vivi2_ch= (vivi1_ch==1 | vivi1_ch==2)
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************

*Se crea una variable parecida, pero con otro nombre

gen viviprop_ch=0 if s1_02==1
replace viviprop_ch=1 if s1_02==2
replace viviprop_ch=2 if s1_02==3 
replace viviprop_ch=3 if s1_02==4 | s1_02==5 | s1_02==6 
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia" 
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

gen vivialq_ch= s1_03
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=s1_04
label var vivialqimp_ch "Alquiler mensual imputado"

*******************
*** benefdes_ci ***
*******************
g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"




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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) 
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 */
rename cods6_11b codocupa
rename cods6_12b codindustria

compress


*Modificación Cesar Lins - Feb 2021 / saveold didn't work because labels are too long
save "`base_out'", replace


log close


