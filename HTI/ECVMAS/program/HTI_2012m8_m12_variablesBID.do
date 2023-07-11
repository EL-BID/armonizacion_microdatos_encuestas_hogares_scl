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

local PAIS HTI
local ENCUESTA ECVMAS
local ANO "2012"
local ronda m8_m12 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Haití
Encuesta: ECVMAS
Round: m8-m12, 2012
Autores:
Alvaro Altamirano Montoya
Última versión: Alvaro Altamirano - Email: alvaroalt@iadb.org, ajaltamiranomontoya@gmail.com
Fecha última modificación: 24 de Octubre de 2017

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
****************************************************************************/

use `base_in', clear

***********
* Region_c *
************
rename dept region_c

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
ren  poids_finaux factor_ch
label variable factor_ch "Factor de expansion del hogar"

**************************
* Identificador del hogar*
**************************

gen idh_ch=hh_id2new 
sort idh*
label variable idh_ch "ID del hogar"

****************************
* Identificador de persona *
****************************
sort hh_id2new i_id3
egen idp_ci=group(idh_ch i_id3)
label variable idp_ci "ID de la persona en el hogar"

***************************
* Zona urbana o zona rural*
***************************
gen zona_c=.
replace zona_c=1 if milieu==1 |  milieu==2
replace zona_c=0 if milieu==3
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

*************************
* País					*
*************************

gen pais_c="HTI"
label variable pais_c "Pais"

****************************
* Anio de la encuesta ******

gen anio_c=2012
label variable anio_c "Anio de la encuesta"

***********************
*  Mes de la encuesta *
***********************

gen mes_c=8
label variable mes_c "Mes de la encuesta"



*******************************
*Relación con el jefe de hogar*
*******************************
/*
lien de parenté:
           1  chef de ménage
           2  epoux(se)/conjoint(e)
           3  fils/fille
           4  petit-fils/petite-fille
           5  papa/maman
           6  beau-père/belle-mère
           7  beau-fils/belle-fille
           8  frère/soeur
           9  oncle/tante
          10  neveu/nièce
          11  cousin/cousine
          12  autre parent
          13  personnel de maison
          14  domestique = restavèk
          15  autre
*/

gen relacion_ci=.
replace relacion_ci=1 if hh_e07==1
replace relacion_ci=2 if hh_e07==2
replace relacion_ci=3 if hh_e07==3
replace relacion_ci=4 if hh_e07==4 | hh_e07==5 | hh_e07==6 | hh_e07==7 | hh_e07==8 | hh_e07==9 | hh_e07==10 | hh_e07==11 | hh_e07==12
replace relacion_ci=5 if hh_e07==15
replace relacion_ci=6 if hh_e07==13 | hh_e07==14

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci


****************************************
*Factor de expansión a nivel individual*
****************************************

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"


**********
***sexo***
**********

gen sexo_ci=hh_e05  
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=hh_e06b 
label variable edad_ci "Edad del individuo"

**************
***civil_ci***
**************
gen civil_ci=1 if i_h02==4
replace civil_ci=2 if i_h02==1 | i_h02==2 | i_h02==3
replace civil_ci=3 if i_h02==5 | i_h02==6 | i_h02==7
replace civil_ci=4 if i_h02==8
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
gen miembros_ci=(relacion_ci<=4)
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


		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if i_i02==1 | (i_i05a>=1 & i_i05a<=6) | i_i05b==1 | (i_i03aa==1 | i_i03ab==1 | i_i03ad==1 | i_i03ae==1 | i_i03af==1 | i_i03ag==1 | i_i03ah==1 | i_i03ai==1 | i_i03aj==1 | i_i03ak==1 | i_i03al==1 | i_i03am==1)
replace condocup_ci=2 if (condocup_ci!=1 & i_n01a==1) & (i_n04==1 | i_n04==2)
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label define condocup_ci 1 "ocupados" 2 "desocupados" 3 "inactivos" 4 "menor de edad (10)"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

************
***emp_ci***
************

gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 | desemp_ci==1

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (i_j13==1 | i_j13==2 | i_j13==3) & cotizando_ci==0 
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*cotizapri_ci***
****************
*La pregunta es hecha solamente para la actividad principal
gen cotizapri_ci=cotizando_ci==1
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=cotizando_ci==1
label var cotizasec_ci "Cotizante a la Seguridad Social en actividad sec."

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


/*No hay información de valores de beneficio pensional*/
*************
**pension_ci*
*************
gen pension_ci=. 
label var pension_ci "1=Recibe pension contributiva"

*************
*   ypen_ci *
*************
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
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
*A falta de consulta sobre si ha trabajado antes, usamos la pregunta I4.
*ha trabajado antes pero ahorita está desocupado
generate cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if i_i04==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
*La base no incluye líneas de pobreza, el informe trabaja sobre líneas de pobreza subjetivas y relativas al estilo europeo de "60% abajo de la mediana". 
*Ver página  72-74: http://www.ihsi.ht/pdf/ecvmas/analyse/IHSI_DIAL_Rapport%20complet_11072014.pdf
*Ver también variable hh_v04

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

gen rama_ci=.
replace rama_ci=1 if i_j03a==1 & emp_ci==1
replace rama_ci=2 if i_j03a==2 & emp_ci==1
replace rama_ci=3 if i_j03a==3 & emp_ci==1
replace rama_ci=4 if (i_j03a==4 | i_j03a==5) & emp_ci==1
replace rama_ci=5 if i_j03a==6 & emp_ci==1
replace rama_ci=6 if (i_j03a==7 | i_j03a==9) & emp_ci==1
replace rama_ci=7 if (i_j03a==8 | i_j03a==10) & emp_ci==1
replace rama_ci=8 if (i_j03a==11 | i_j03a==12) & emp_ci==1
replace rama_ci=9 if (i_j03a==13 | i_j03a==14 | i_j03a==15 | i_j03a==16 | i_j03a==17 | i_j03a==18 | i_j03a==19 | i_j03a==20) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras"
label def rama_ci 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio, restaurantes y hoteles" 7 "Transporte y almacenamiento", add
label def rama_ci 8 "Establecimientos financieros, seguros e inmuebles" 9 "Servicios sociales y comunales", add
label val rama_ci rama_ci


* rama secundaria
gen ramasec_ci=.
replace ramasec_ci=1 if i_k03==1 & emp_ci==1
replace ramasec_ci=2 if i_k03==2 & emp_ci==1
replace ramasec_ci=3 if i_k03==3 & emp_ci==1
replace ramasec_ci=4 if (i_k03==4 | i_k03==5) & emp_ci==1
replace ramasec_ci=5 if i_k03==6 & emp_ci==1
replace ramasec_ci=6 if (i_k03==7 | i_k03==9) & emp_ci==1
replace ramasec_ci=7 if (i_k03==8 | i_k03==10) & emp_ci==1
replace ramasec_ci=8 if (i_j03a==11 | i_k03==12) & emp_ci==1
replace ramasec_ci=9 if (i_k03==13 | i_k03==14 | i_k03==15 | i_k03==16 | i_k03==17 | i_k03==18 | i_k03==19 | i_k03==20) & emp_ci==1
label var ramasec_ci "Rama de actividad"
label def ramasec_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras"
label def ramasec_ci 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio, restaurantes y hoteles" 7 "Transporte y almacenamiento", add
label def ramasec_ci 8 "Establecimientos financieros, seguros e inmuebles" 9 "Servicios sociales y comunales", add
label val ramasec_ci ramasec_ci


*************
**salmm_ci***
*************
*Alvaro AM: Fuente: Ver último párrafo de la página 140 del informe de DIAL: http://www.ihsi.ht/pdf/ecvmas/analyse/IHSI_DIAL_Rapport%20complet_11072014.pdf
generat salmm_ci=.
replace salmm_ci=300*22
label var salmm_ci "Salario minimo legal"


*****************
***desalent_ci***
*****************
gen desalent_ci=0 if i_n01a!=.
replace desalent = 1 if i_n02==2 | i_n02==11

******************************
*	subemp_ci
******************************
egen tothoras = rsum(i_j16h i_k11h), missing

gen subemp_ci=(tothoras<30 & i_j19a==1 & i_j19b==1)  // menos de 30h, quiere, "y está disponible" para trabajar más horas! Estas últimas dos preguntas solo están para la activ. princ.
*Observación: Ellos usan el umbral de 35h. Ver pregunta J17 del cuestionario. 
label var subemp_ci "Dispuestas a trabajar mas, pero trabajan 30hs o menos(semana)"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.

*****************
***horaspri_ci***
*****************

gen horaspri_ci=i_j16h
replace horaspri_ci=. if emp_ci==0 | i_j16h>144

*****************
***horastot_ci***
*****************
gen horastot_ci=tothoras
replace horastot_ci=. if emp_ci==0

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if i_j02==7
replace categopri_ci=2 if i_j02==8
replace categopri_ci=3 if i_j02==1 | i_j02==2 | i_j02==3  | i_j02==4 | i_j02==5 
replace categopri_ci=4 if i_j02==9 | i_j02==10 | i_j02==11
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1 "Patron" 2 "Cuenta propia" 
label define categopri_ci 3 "Empleado" 4 " Trabajador no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if i_k02==7
replace categosec_ci=2 if i_k02==8
replace categosec_ci=3 if i_k02==1 | i_k02==2 | i_k02==3  | i_k02==4 | i_k02==5 
replace categosec_ci=4 if i_k02==9 | i_k02==10 | i_k02==11
replace categosec_ci=. if emp_ci==0

label define categosec_ci 1 "Patron" 2 "Cuenta propia" 
label define categosec_ci 3 "Empleado" 4 "Trabajador no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
replace tipocontrato_ci=1 if i_j11==1
replace tipocontrato_ci=2 if i_j11==2 | i_j11==3
replace tipocontrato_ci=3 if i_j11==4 | i_j11==5 | i_j11==6 | i_j11==7

label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

***************
***segsoc_ci***
***************
/*gen segsoc_ci=.
label variable segsoc_ci "Tiene Seguro Social"
*/
*****************
***nempleos_ci***
*****************
gen nempleos_ci=1 if i_k01b==2
replace nempleos_ci=2 if i_k01b==1
replace nempleos_ci=. if emp_ci==0

*****************
***tamfirma_ci***
*****************
*Estructura de tamano de firma adhoc, debido a las opciones de la pregunta en esta base.
gen tamfirma_ci=.
replace tamfirma_ci=1 if i_j07b==1
replace tamfirma_ci=2 if i_j07b==2
replace tamfirma_ci=3 if i_j07b==3 | i_j07b==4
replace tamfirma_ci=4 if i_j07b==5 | i_j07b==6 | i_j07b==7

replace tamfirma_ci=. if emp_ci!=1
label var tamfirma_ci "Tamano de la firma"
label def tamfirma_ci 1 "1 trabajador" 2 "2 a 5 trabajadores" 3 "6 a 50 trabajadores" 4 "51 trabajadores o más"
label val tamfirma_ci tamfirma_ci

*****************
***spublico_ci***
*****************
*Nota: En el año 2009 no se pregunta acerca del tipo de establecimiento en que trabaja
gen spublico_ci=(i_j04==1 | i_j04==2)
label var spublico_ci "Trabajador del sector público"

**************
***ocupa_ci***
**************
/*En la base las clasificaron los oficios por sus nombres (nom du métier), y la variable está como string***

*Se utiliza ciuo88
tab s5p14
tostring s5p14, replace force
gen digito = "0"
egen x = concat(digito s5p14) if length(s5p14)==3
replace s5p14=x if length(s5p14)==3
gen ocupa= real(substr(s5p14,1,2))
*/
gen ocupa_ci=.
/*
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=34)  & emp_ci==1
replace ocupa_ci=2 if (ocupa>=11 & ocupa<=13)  & emp_ci==1
replace ocupa_ci=3 if (ocupa>=41 & ocupa<=42)  & emp_ci==1
replace ocupa_ci=4 if (ocupa==52)              & emp_ci==1
replace ocupa_ci=5 if (ocupa==51)              & emp_ci==1
replace ocupa_ci=6 if (ocupa>=61 & ocupa<=62)  & emp_ci==1
replace ocupa_ci=7 if (ocupa>=71 & ocupa<=83)  & emp_ci==1
replace ocupa_ci=8 if (ocupa==1)               & emp_ci==1
replace ocupa_ci=9 if (ocupa>=91 & ocupa<=94)  & emp_ci==1
*/
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


****************
***durades_ci***
****************
gen durades_ci= . 
label variable durades_ci "Duracion del desempleo en meses- Intervalos"
label def durades_ci 1 "Menos de un mes" 2 "1 mes a menos de 2 meses" 3 "2 meses a menos de 3 meses"
label def durades_ci 4 "3 meses a menos de 6 meses" 5 "6 meses a menos de 1 año" 6"Más de 1 año", add
label val durades_ci durades1_ci 

*******************
***antiguedad_ci***
*******************
tostring i_j09b, g(stringaux)
gen anoinicio=substr(stringaux, -4,.) 
destring anoinicio, replace
replace anoinicio=. if anoinicio<0
gen antiguedad_ci=(2012-anoinicio) if anoinicio!=.
label var antiguedad_ci "Antiguedad en el empleo (en años)"

*************
*tamemp_ci***
*************
gen tamemp_ci=. 
label var tamemp_ci "# empleados en la empresa de la actividad principal"

*******************
***categoinac_ci*** 
*******************
/*No hay categoría de pensionados u otros*/
gen categoinac_ci =.
/*
replace categoinac_ci = 2 if  (s5p10==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s5p10==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
*/
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
gen byte formal_ci=cotizando_ci
label var formal_ci "1=afiliado o cotizante / PEA"

************************************************************************
**************************INGRESOS**************************************
************************************************************************

****************
***ylmpri_ci ***
****************
*Obs. No publicaron la pregunta j24, que desagrega ingresos entre ocupaciones y tipos de remuneración/ingresos,
*En este caso trabajamos con los ingresos agregados que la encuesta ya trae.
*El instituto de estadísticas menciona que los módulos 67, 68 y 71 desagregan los ingresos laborales para el empleo actual
*actividad principal y actividad secundaria, pero las bases descargables no los incluyen (ver: http://www.ihsi.ht/produit_enq_nat_ecvmas.html)

*Actividad principal
recode i_j25 (-9 -8 =.)

gen ylmpri_ci=i_j25
replace ylmpri_ci=. if i_j25==. | emp_ci==0
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 
*Nota: no se esta considerando el autoconsumo como ingreso

***************
***ylmsec_ci***
***************
*Actividad secundaria
recode i_k15 (-9 -8 =.)

gen ylmsec_ci=i_k15
replace ylmsec_ci=. if i_k15==. | emp_ci==0
replace ylmsec_ci=0 if categosec_ci==4
label var ylmsec_ci "Ingreso laboral monetario actividad secundaria" 
*Nota: no se esta considerando el autoconsumo como ingreso

****************
***ylnmsec_ci***
****************
gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


****************
***ylnmpri_ci***
****************
gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"  


gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal" 

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

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total" 

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************
gen tcylmpri_ci=.

*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.  
label var ylnm_ci "Ingreso laboral NO monetario total"  

*************
***ynlm_ci***
*************

gen ynlm_ci=.
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

**************
***ynlnm_ch***
**************

gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

**********************************
*** remesas_ci & remesas_ch ***
**********************************
merge m:m idh_ch using "$ruta\survey\HTI\ECVMAS\2012\m8_m12\data_orig\ECVMAS 2012\2_ECVMAS_BASE DE DONNEES\Modulo Remesas.dta", gen(mergeremesas)
*Tipo de cambio en 2012: 0.02378, dollars x gourdes. Para explicación del dollar hatiano, ver: https://www.vagabondjourney.com/what-is-a-haitian-dollar/
recode hh_r09b (-9 -8 =.)
gen remesas_ci=.
replace remesas_ci=hh_r09b if hh_r09c==1
replace remesas_ci=hh_r09b/5 if hh_r09c==2
replace remesas_ci=hh_r09b*0.02378 if hh_r09c==3
replace remesas_ci=remesas_ci/12
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

bys idh_ch: egen remesas_ch=sum(remesas_ci)
label var remesas_ch "Remesas mensuales del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm_ch=rsum(ynlm remesas_ch)
replace ynlm_ch=. if ynlm==. 
drop ynlm


*******************
*** autocons_ci ***
*******************
*Las preguntas de autoconsumo no constan en la base
gen autocons_ci= .
label var autocons_ci "Autoconsumo reportado por el individuo"


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
replace autocons_ch =. if autocons_ch==0
label var autocons_ch "Autoconsumo reportado por el hogar"

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=.

*/
*****************
***ylmhopri_ci ***
*****************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0

***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
replace ylmho_ci=. if ylmho_ci<=0


****************************
***VARIABLES DE EDUCACION*** 
****************************

/*
Para la construcción de aedu_ci se utilizan:

	// Para mayores de 10 anios:

		- i_h10 para los niveles 2 a 16 que abarcan la totalidad de primaria hasta 
		secundaria.
		- i_h11 para complementar con i_h11 == 17 que hace referencia a estudios superiores. 
		i_h11 contiene los diplomas obtenidos. Se imputan los anios que corresponden tomando 
		el limite inferior de cada categoria si se tiene información en la encuesta y 
		en caso contrario se le adiciona 1 anio a los anios del nivel de educación anterior.
		Ej: un invidivio con diploma de formación profesional declarado obtiene 13 anios de educacion 
		por secundaria completa más 1 por diploma de centro de formacion profesional.
		
	// Para menores de 10 anios:
	
		- Al no contar con información de último grado aprobado (la pregunta hace referenfcia
		a último nivel alcanzado) se imputan los anios correspondientes al nivel de educación 
		anterior.
		
*/

*************
***aedu_ci*** 
*************

gen aedu_ci = . 
// Respondientes mayores de 10 anios anios
replace aedu_ci = 0 if inlist(i_h10, 1, 2) // Ninguno, Preescolar
replace aedu_ci = i_h10 - 2 if (i_h10 > 2 & i_h10 <= 16) // Primaria, Secundaria, retorica , filosofía

// Superior se asigna por titulo?
replace aedu_ci = 13 + 3 if i_h11 == 7 // Diploma universitario (3-4 anios?)
replace aedu_ci = 13 + 3 + 1 if i_h11 == 8 // Diploma maestría (1 - 2 anios)
replace aedu_ci = 13 + 1 if i_h11 == 9 // Diploma universitario DEA? (1 anio)
replace aedu_ci = 13 + 3 + 1 + 1 if i_h11 == 10 // Doctorado
replace aedu_ci = 13 + 1 if i_h11 == 11 // Formacion Profesional

// Respondientes menores de 10 anios ultimo nivel alcanzado, se imputan los anios del nivel anterior.
replace aedu_ci = 0 if hh_e15 == 3 // Primaria
replace aedu_ci = 6 if hh_e15 == 4 // Secundaria

**************
***eduno_ci***
**************

gen byte eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == .
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci = (aedu_ci > 0 & aedu_ci < 6)
replace edupi_ci = . if aedu_ci == .
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci = (aedu_ci == 6)
replace edupc_ci = . if aedu_ci == .
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci = (aedu_ci > 6 & aedu_ci < 13)
replace edusi_ci = . if aedu_ci == .
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci = (aedu_ci == 13)
replace edusc_ci = . if aedu_ci == .
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci = (aedu_ci > 6 & aedu_ci < 9)
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

gen byte edus2i_ci = (aedu_ci > 9 & aedu_ci < 13)
replace edus2i_ci = . if aedu_ci == .
label variable edus2i_ci "2do ciclo de la secundaria incompleto"
***************
***edus2c_ci***
***************

gen byte edus2c_ci = (aedu_ci == 13)
replace edus2c_ci = . if aedu_ci == .
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci = (aedu_ci > 13 & aedu_ci < 17)
replace eduui_ci = . if aedu_ci == .
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci = (aedu_ci >= 17)
replace eduuc_ci = . if aedu_ci == .
label variable eduuc_ci "Universitaria incompleta o mas"

***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************

gen byte asispre_ci = .
label variable asispre_ci "Asiste a preescolar"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
gen asiste_ci = (hh_e16a == 1)
replace asiste_ci = 1 if i_h08a == 1
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************
// Al tener muy pocas obsercavaciones para cada grupo va a missing
gen pqnoasis_ci = .
label var pqnoasis_ci "Razones para no asistir"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g pqnoasis1_ci = .

***************
***repite_ci***
***************
gen repite_ci=.

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"

***************
***edupub_ci***
***************

gen edupub_ci = (hh_e16b == 1 | i_h09 == 1)
replace edupub_ci = . if asiste_ci == 0 
label var edupub_ci "Asiste a un centro de enseñanza público"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************



/* Existe la siguiente distribución de fuentes de agua, pero la misma no permite decir dónde se encuentra la fuente, suponer sería riesgoso me parece.
1. robinet privé - dinepa
2. fontaine publique
3. puit artésien / forage
4. puit protégé
5. source d'eau protégé
6. eau de pluie
7. kiosque (vendeur d'eau traitée)
8. eau traitée (camion, bouteille, sach
9. puit non protégé
10. source d'eau non protégée
11. eau non traitée (camion, bouteille
12. eau de surface (rivière, lac, mare
*/

/*
replace aguadist_ch=1 if hh_c10a==1
replace aguadist_ch=2 if hh_c10a==2 
replace aguadist_ch=3 if hh_c10a>=3 & hh_c10a<=10
replace aguadist_ch=. if hh_c10a==98 | hh_c10a==99
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1 "Dentro de la vivienda" 2 "Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3 "Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch
*/


****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if hh_c10b==1 
replace aguared_ch = 0 if hh_c10b!=1
la var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 1 if hh_c10a==1 
replace aguafconsumo_ch = 2 if hh_c10a==2 
replace aguafconsumo_ch = 4 if hh_c10a==3 |hh_c10a== 4
replace aguafconsumo_ch = 5 if hh_c10a==6
replace aguafconsumo_ch = 7 if hh_c10a==5 | hh_c10a==7 |hh_c10a==8
replace aguafconsumo_ch = 8 if hh_c10a==12
replace aguafconsumo_ch = 9 if hh_c10a==10 | hh_c10a==11 | hh_c10a==9
replace aguafconsumo_ch = 10 if hh_c10a == -8 |hh_c10a == -9 

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.
replace aguafuente_ch = 1 if hh_c10b==1 
replace aguafuente_ch = 2 if hh_c10b==2 
replace aguafuente_ch = 4 if hh_c10b==3|hh_c10b== 4
replace aguafuente_ch = 5 if hh_c10b==6 
replace aguafuente_ch = 7 if hh_c10b==5 | hh_c10b==7 | hh_c10b==8 
replace aguafuente_ch = 8 if hh_c10b==12
replace aguafuente_ch = 9 if hh_c10b==10 | hh_c10b==11 |hh_c10b==9
replace aguafuente_ch = 10 if  hh_c10b == -8 |hh_c10b == -9 


*************
*aguadist_ch*
*************
gen aguadist_ch=0



**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =9



**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9



*************
*aguamala_ch*  
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10
*label var aguamala_ch "= 1 si la fuente de agua no es mejorada"

*****************
*aguamejorada_ch*  
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7
*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*****************
***aguamide_ch***
*****************
generate aguamide_ch = .



*****************
*bano_ch         *  
*****************
gen bano_ch=.
replace bano_ch=1 if hh_d02==1 & hh_c11==1
replace bano_ch=2 if hh_d02==1 & hh_c11==2
replace bano_ch=3 if (hh_d02==2 | hh_d02==3) & (hh_c11!=3 | hh_c11!=4 |hh_c11!=5|hh_c11!=6)
replace bano_ch=4 if hh_c11==3 | hh_c11==4|hh_c11==5|hh_c11==6
replace bano_ch=5 if (hh_d02==4 | hh_d02==5 | hh_d02 ==6) & (hh_c11!=3 | hh_c11!=4 |hh_c11!=5|hh_c11!=6)
replace bano_ch=6 if bano_ch ==. & jefe_ci==1
replace bano_ch=0 if hh_d02==7 

***************
***banoex_ch***
***************
generate banoex_ch=.
replace banoex_ch = 1 if hh_d02==2 | hh_d02==4
replace banoex_ch = 0 if hh_d02==3 | hh_d02==5 | hh_d02==7
replace banoex_ch = 9 if hh_d02==1 | hh_d02==6
* does not ask about exclusivity for those with wcs or holes *


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

************
*sinbano_ch*
************
gen sinbano_ch = 0
replace sinbano_ch = 3 if hh_d02==7


*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9

************
***luz_ch***
************

gen luz_ch=(hh_c09a<4 & hh_c09a!=.)
label var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************

gen luzmide_ch=(hh_c09a<3 & hh_c09a!=.)
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
gen combust_ch= (hh_d01==2 | hh_d01==3 | hh_d01==5 | hh_d01==6 & hh_d01!=.)
label var combust_ch "Principal combustible gas o electricidad" 



*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if hh_c11==5 | hh_c11==6
replace des1_ch=1 if hh_c11==1 /*Red o pozo septico*/
replace des1_ch=2 if hh_c11==2 /*Letrina */
replace des1_ch=3 if hh_c11==4 | hh_c11==5 | hh_c11==6 /*Superficie*/
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0 "No tiene servicio sanitario" 1 "Conectado a red general o cámara séptica"
label def des1_ch 2 "Letrina o conectado a pozo ciego" 3 "Desemboca en río o calle", add
label val des1_ch des1_ch

*************
***des2_ch***
*************
gen des2_ch=.
replace des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2
replace des2_ch=2 if des1_ch==3
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0 "No tiene servicio sanitario" 1 "Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2 "Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************
gen piso_ch=.
replace piso_ch=0 if hh_c03==2
replace piso_ch=1 if hh_c03!=2 & hh_c03!=.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0 "Piso de tierra" 1 "Materiales permanentes"
label val piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch=.
replace pared_ch=0 if hh_c01==1 | hh_c01==2 | hh_c01==4 | hh_c01==5 | hh_c01==7
replace pared_ch=1 if hh_c01==3 | hh_c01==6
replace pared_ch=. if hh_c01==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0 "No permanentes" 1 "Permanentes"
label val pared_ch pared_ch

**************
***techo_ch***
**************
gen techo_ch=.
replace techo_ch=0 if hh_c02==1 | hh_c02==4 | hh_c02==6
replace techo_ch=1 if hh_c02==2 | hh_c02==3 | hh_c02==5
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0 "No permanentes" 1 "Permanentes"
label val techo_ch techo_ch

**************
***resid_ch***
**************
recode hh_c12 (-9=.)
gen resid_ch=.
replace resid_ch=0 if hh_c12==1 | hh_c12==2
replace resid_ch=1 if hh_c12==8 | hh_c12==9
replace resid_ch=2 if hh_c12==3 | hh_c12==4 | hh_c12==6 | hh_c12==7
replace resid_ch=3 if hh_c12==5
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0 "Recolección pública o privada" 1 "Quemados o enterrados"
label def resid_ch 2 "Tirados a un espacio abierto" 3 "Otros", add
label val resid_ch resid_ch



*************
***dorm_ch***
*************
gen dorm_ch=hh_c04a
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
gen  cuartos_ch =hh_c04b+hh_c04a
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

gen cel_ch=.

***************
***refrig_ch***
***************
gen refrig_ch = .
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

*************
***compu_ch***
**************
gen compu_ch=.
label var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************
gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"

**************
***vivi1_ch***
**************
gen vivi1_ch=.
replace vivi1_ch= 1 if hh_b16a<6
replace vivi1_ch= 2 if hh_b16a==6
replace vivi1_ch= 3 if  hh_b16a==8 | hh_b16a==9 | hh_b16a==10
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros"
label val vivi1_ch vivi1_ch

*************
***vivi2_ch***
*************
gen vivi2_ch =.
replace vivi2_ch = 1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch = 2 if vivi1_ch==3 
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
gen viviprop_ch =.
replace viviprop_ch = 0 if hh_c05==2 | hh_c05==3 
replace viviprop_ch = 1 if hh_c05==1
replace viviprop_ch = 2 if hh_c05==2
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0 "Alquilada" 1 "Propia y totalmente pagada" 2 "propia y en proceso de pago"
label def viviprop_ch 3 "Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch = .
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
* Alquiler en gourdes 
recode hh_c07a (-9 -8 =.)
g auxalqui = hh_c07a if  hh_c07b==1 
replace auxalqui = hh_c07a/5 if  hh_c07b==2
replace auxalqui = hh_c07a*0.02378 if  hh_c07b==3
rename auxalqui vivialq_ch
label var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"




**** MGD 2017:Borro missings en sexo_ci
keep if sexo_ci!=.


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=inlist(i_h03a,2,3,4) if i_h03a!=. & i_h03a!=-9
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=inlist(i_h03a,2) if i_h03a!=. & i_h03a!=-9
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	/* Encuesta pregunta sobre años viviendo en este lugar, no sabemos si pudo vivir en Honduras y mudarse de ciudad */
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=inlist(i_h03a,2) if i_h03a!=. & i_h03a!=-9
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first


compress


saveold "`base_out'", replace


log close
