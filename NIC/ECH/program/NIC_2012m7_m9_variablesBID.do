
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

local PAIS NIC
local ENCUESTA ECH
local ANO "2012"
local ronda m7_m9

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: ECH
Round: Julio-Septiembre
Autores: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 9 de abril de 2014

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear
	

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013	
gen region_c= s01p01
label define region_c  ///
          5  "Nueva Segovia" ///
          10 "Jinotega" ///
          20 "Madriz" ///
          25 "Estelí" ///
          30 "Chinandega" ///
          35 "León" ///
          40 "Matagalpa" ///
          50 "Boaco" ///
          55 "Managua" ///
          60 "Masaya" ///
          65 "Chontales" ///
          70 "Granada" ///
          75 "Carazo" ///
          80 "Rivas" ///
          85 "Río San Juan" ///
          91 "Raan" ///
          93 "Raas"		  
label value region_c region_c
label var region_c "División política, departamentos"

***************
***factor_ch***
***************
gen factor_ch=fajustexproyeccion
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
***************
gen idh_ch = id
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************
gen idp_ci=s07p00
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen zona_c=0 if s01p06==2
replace zona_c=1 if s01p06==1
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************
gen str3 pais_c="NIC"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2012
label variable anio_c "Anio de la encuesta"

gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" | pais=="PAN" | pais=="DOM" | pais=="CRI" | pais=="BLZ" | pais=="GTM" | pais=="SLV" | pais=="HON" | pais=="NIC"
replace region_BID_c=2 if pais=="BAH" | pais=="BAR" | pais=="GUY" | pais=="JAM" | pais=="SUR" | pais=="TOT"
replace region_BID_c=3 if pais=="ECU" | pais=="COL" | pais=="PER" | pais=="VEN" | pais=="BOL" 
replace region_BID_c=4 if pais=="ARG" | pais=="BRA" | pais=="CHL" | pais=="PRY" | pais=="URU" 
replace region_BID_c=5 if pais=="HAI"
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

gen mes_c= .
***************
***factor_ci***
***************
gen factor_ci=fajustexproyeccion
label variable factor_ci "Factor de expansion del individuo"


		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************

*****************
***relacion_ci***
*****************
gen relacion_ci=1     if s07p08==1
replace relacion_ci=2 if s07p08==2
replace relacion_ci=3 if s07p08==3
replace relacion_ci=4 if s07p08>=4 & s07p08<=8
replace relacion_ci=5 if s07p08==9
label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

**********
***sexo***
**********
gen sexo_ci=s07p09
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=s07p10 if s07p10<=97
label variable edad_ci "Edad del individuo"

**************
***civil_ci***
**************
gen civil_ci=.
replace civil_ci=1 if s07p18==7
replace civil_ci=2 if s07p18==2 | s07p18==3
replace civil_ci=3 if s07p18==4 | s07p18==5
replace civil_ci=4 if s07p18==6
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

*No se clasifican a los niños 1 menor de 12 años = s07p18==1 

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
replace nempdom_ch=.
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


		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if po==1
replace condocup_ci=2 if pda==1
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14 /*A parir del 2010 la PET asciende a 14*/
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
* Alternativa 2 con variables originales tomando en cuenta la definicion de armonizacion MGD 06/05/2014
gen condocup_ci=.
*replace condocup_ci=1 if s11p01==1 | (s11p02>=1 & s11p02<=9) | (s11p04>=1 & s11p04<=4) | (s11p03==1 & s11p06<4)
*2014, 10 Modificacion MLO
replace condocup_ci=1 if s11p01==1 | (s11p02>=1 & s11p02<=9) | (s11p04>=1 & s11p04<=4) |s11p04==7 | s11p04==8 | s11p05==1| (s11p03==1 & (s11p06<4 & s11p06!=.))
replace condocup_ci=2 if condocup_ci!=1 & s11p07==1 & s11p10==1
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen afiliado_ci=(s07p12==1 | s07p12==2 | s07p12==4) 
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (s11p33a==1) & cotizando_ci==0 /*solo a ocupados*/
label var cotizando_ci "Cotizante a la Seguridad Social"

* Formalidad sin restringir PEA
gen cotizando_ci1=0     if condocup_ci>=1 & condocup_ci<=3
replace cotizando_ci1=1 if (s11p33a==1) & cotizando_ci1==0 /*solo a ocupados*/
label var cotizando_ci1 "Cotizante a la Seguridad Social"

****************
*cotizapri_ci***
****************
gen cotizapri_ci=.
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=.
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

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci= (s12p54_44a==1) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*   ypen_ci *
*************
recode s12p54_44 (9999999999=.)
gen ypen_ci=s12p54_44 if pension_ci==1
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
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if s11p15==1 & condocup_ci==2
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


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
egen meansalmm =mean(salmin)
generat salmm_ci=salmin
replace salmm_ci=meansalmm if salmin==.
label var salmm_ci "Salario minimo legal"

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
replace pea_ci=1 if emp_ci==1 |desemp_ci==1

*****************
***desalent_ci***
*****************
gen desalent_ci=0 if s11p14!=.
replace desalent_ci = 1 if s11p14==4 | s11p14==10
replace desalent_ci=. if emp_ci==1
label var desalent_ci "Trabajadores desalentados"

***************
***subemp_ci***
***************
gen subemp_ci=0 
replace subemp_ci=1 if  (s11p39c>=1 & s11p39c<=30)  & emp_ci==1 & s11p43==1
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************
* 2014, 10 Modificación MLO, se reemplaza por horas habituales para mantener consistencia con serie anterior
*s11p39a (horas semana pasada ocup ppal) s11p41a (horas habituales ocup ppal)
/*gen horaspri_ci=s11p39a 
replace horaspri_ci=. if s11p39a==999
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"
*/
gen horaspri_ci=s11p39a if s11p40==1
replace horaspri_ci=s11p41a if s11p40==3
replace horaspri_ci=. if s11p39a==999 &  s11p40==1
replace horaspri_ci=. if s11p41a==999 &  s11p40==3
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
*s11p39c (horas semana pasada totales) s11p41c (horas habituales ocup ppal)
/*
gen horastot_ci=s11p39c
replace horastot_ci=. if s11p39c==999
replace horastot_ci=. if emp_ci==0
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
*/
gen horastot_ci=s11p39c if s11p40==1
replace horastot_ci=s11p41c if s11p40==3
replace horastot_ci=. if s11p39c==999 &  s11p40==1
replace horastot_ci=. if s11p41c==999 &  s11p40==3
replace horastot_ci=. if emp_ci==0
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*******************
***tiempoparc_ci***
*******************
generat tiempoparc_ci= 0 if emp_ci==1
replace tiempoparc_ci=1 if (horastot_ci>=1 & horastot_ci<=30) & s11p43==2
replace tiempoparc_ci=. if emp_ci==0 | emp_ci ==.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
generat categopri_ci=.
replace categopri_ci=1 if s11p27==3
replace categopri_ci=2 if s11p27==4 
replace categopri_ci=3 if s11p27==1 | s11p27==2 | s11p27==5
replace categopri_ci=4 if s11p27==6 | s11p27==7
replace categopri_ci=. if emp_ci==0
label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************
generat categosec_ci=.
replace categosec_ci=1 if s11p37==3
replace categosec_ci=2 if s11p37==4 
replace categosec_ci=3 if s11p37==1 | s11p37==2 | s11p37==5
replace categosec_ci=4 if s11p37==6 | s11p37==7
replace categosec_ci=. if emp_ci==0
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
replace tipocontrato_ci=1 if (s11p31==1)             & categopri_ci==3
replace tipocontrato_ci=2 if (s11p31==2)             & categopri_ci==3
replace tipocontrato_ci=3 if ((s11p31>=3 & s11p31<=4) | tipocontrato_c==.)  & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


***************
***segsoc_ci***
***************
gen segsoc_ci=.
replace segsoc_ci= 1 if s11p33a==1
replace segsoc_ci= 0 if s11p33a==2
label var segsoc_ci "Personas que tienen seguridad social en salud por su trabajo"

*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & s11p34==2
replace nempleos_ci=2 if emp_ci==1 & s11p34==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
replace firmapeq_ci=1 if s11p33==1 | s11p33==2 
replace firmapeq_ci=0 if s11p33>=3 & s11p33<=7
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************
gen spublico_ci=0 if (s11p29>=5 & s11p29!=.) & emp_ci==1
replace spublico_ci=1 if s11p29<=4
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
*Se utiliza ciuo88

tab s11p24
tostring s11p24, replace force
gen digito = "0"
egen x = concat(digito s11p24) if length(s11p24)==3
replace s11p24=x if length(s11p24)==3
gen ocupa= real(substr(s11p24,1,2))

gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=34)  & emp_ci==1
replace ocupa_ci=2 if (ocupa>=11 & ocupa<=13)  & emp_ci==1
replace ocupa_ci=3 if (ocupa>=41 & ocupa<=42)  & emp_ci==1
replace ocupa_ci=4 if (ocupa==52)              & emp_ci==1
replace ocupa_ci=5 if (ocupa==51)              & emp_ci==1
replace ocupa_ci=6 if (ocupa>=61 & ocupa<=62)  & emp_ci==1
replace ocupa_ci=7 if (ocupa>=71 & ocupa<=83)  & emp_ci==1
replace ocupa_ci=8 if (ocupa==1)               & emp_ci==1
replace ocupa_ci=9 if (ocupa>=91 & ocupa<=93)  & emp_ci==1
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
*Se utiliza clasificacion CUAEN

tostring s11p26, replace force
egen y = concat(digito s11p26) if length(s11p26)==3
replace s11p26=y if length(s11p26)==3
gen rama=real(substr(s11p26,1,2))

gen rama_ci=.
replace rama_ci = 1 if rama>=1 & rama<=5
replace rama_ci = 2 if rama>=10 & rama<=14
replace rama_ci = 3 if rama>=15 & rama<=37
replace rama_ci = 4 if rama>=40 & rama<=41
replace rama_ci = 5 if rama==45
replace rama_ci = 6 if rama>=50 & rama<=55
replace rama_ci = 7 if rama>=60 & rama<=64
replace rama_ci = 8 if rama>=65 & rama<=71
replace rama_ci = 9 if rama>=72 & rama<=99
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
drop digito x y ocupa rama 
*/

** Ultimo cambio MGD 04/29/2014
gen rama_ci=.
replace rama_ci=1 if (s11p26>=100& s11p26<=599) & emp_ci==1
replace rama_ci=2 if (s11p26>=1010 & s11p26<=1499) & emp_ci==1
replace rama_ci=3 if (s11p26>=1510 & s11p26<=3729) & emp_ci==1
replace rama_ci=4 if (s11p26>=4010 & s11p26<=4199) & emp_ci==1
replace rama_ci=5 if (s11p26>=4510 & s11p26<=4559) & emp_ci==1
replace rama_ci=6 if (s11p26>=5010 & s11p26<=5529) & emp_ci==1
replace rama_ci=7 if (s11p26>=6010 & s11p26<=6429) & emp_ci==1
replace rama_ci=8 if (s11p26>=6510 & s11p26<=7029) & emp_ci==1
replace rama_ci=9 if (s11p26>=7111 & s11p26<=9900) & emp_ci==1


label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
*Se pregunta pero en rangos, luego, no es comparable con los demás países
/*
s11p09:
           1 menos de 1 mes
           2 1 a 3 meses
           3 4 a 6 meses
           4 7 a 12 meses
           5 más de 12 meses hasta 2 años
           9 ignorado
*/

*Mayra Sáenz - Abril 2014
*Se genera como el promedio para mantener la misma metodología 
*aplicada en el resto de países.

gen durades_ci=.
replace durades_ci = 0.5 if s11p09 ==1
replace durades_ci = (1+3)/2 if s11p09 ==2
replace durades_ci = (4+6)/2 if s11p09 ==3
replace durades_ci = (7+12)/2 if s11p09 ==4
replace durades_ci = (13+24)/2 if s11p09 ==5
label variable durades_ci "Duracion del desempleo en meses"
 
*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*************
*tamemp_ci
*************
*Nicaragua Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50
/*
s11p33:
           1 1 persona
           2 2 a 5 personas
           3 6 a 9 personas
           4 10 a 20 personas
           5 21 a 50 personas
           6 51 a 100 personas
           7 101 y más personas
           9 ignorado

*/
gen tamemp_ci = 1 if s11p33>=1 & s11p33<=2
replace tamemp_ci = 2 if (s11p33>=3 & s11p33<=5)
replace tamemp_ci = 3 if (s11p33>=6 & s11p33<=7)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp_o = 1 if s11p33>=1 & s11p33<=3
replace tamemp_o = 2 if (s11p33>=4 & s11p33<=5)
replace tamemp_o = 3 if (s11p33>=6 & s11p33<=7)

label define tamemp_o 1 "[1-9]" 2 "[10-49]" 3 "[50 y mas]"
label value tamemp_o tamemp_o
label var tamemp_o "Tamaño de empresa-OECD"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((s11p12 ==3 | s11p12 ==6 ) & condocup_ci==3)
replace categoinac_ci = 2 if  (s11p12 ==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s11p12 ==4 & condocup_ci==3)
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

* Formalidad sin restringir a PEA
g formal_1 = cotizando_ci1
			**************
			***INGRESOS***
			**************
for var  s12p49_1p01- s12p54_51: recode X (9999999999=.)

/////////////////////////////////////
/// Ingreso de los Asalariados //////
////////////////////////////////////

* Sueldos y otros beneficios de los empleados
gen ymensual=s12p49_1p11  if emp_ci==1  /*Act. Principal- MS: es el mismo valor de la s12p49b*/
replace ymensual= .       if s12p49==. | s12p49==2 | s12p49==3
*Inclusión Mayra Sáenz - Abril 2014
g ymensual_ = .  /*Act. Principal Cuenta propia*/
replace ymensual_ = s12p49c1*22   if s12p49c2 ==1
replace ymensual_ = s12p49c1*4.3  if s12p49c2 ==2
replace ymensual_ = s12p49c1*2    if s12p49c2 ==3
replace ymensual_ = s12p49c1      if s12p49c2 ==4
replace ymensual_ =.              if s12p49c2 ==9

*Inclusión Mayra Sáenz - Abril 2014
g ymensual_1 = .  /*Act. Principal Trabajadores ocasionales*/
replace ymensual_1 = s12p49a1*22   if s12p49a2 ==1
replace ymensual_1 = s12p49a1*4.3  if s12p49a2 ==2
replace ymensual_1 = s12p49a1*2    if s12p49a2 ==3
replace ymensual_1 = s12p49a1      if s12p49a2 ==4
replace ymensual_1 =.              if s12p49a2 ==9



gen ymensual2=s12p49_1s11 if emp_ci==1 /*Act. Secundaria MS: es el mismo valor de la s12p49c*/
replace ymensual2= .      if s12p49==. | s12p49==2 | s12p49==3
*Inclusión Mayra Sáenz - Abril 2014
g ymensual2_ = .  /*Act. Principal Cuenta propia*/
replace ymensual2_ = s12p49c3*22  if s12p49c4 ==1
replace ymensual2_ = s12p49c3*4.3 if s12p49c4 ==2
replace ymensual2_ = s12p49c3*2   if s12p49c4 ==3
replace ymensual2_ = s12p49c3     if s12p49c4 ==4
replace ymensual2_ =.             if s12p49c4 ==9


*Inclusión Mayra Sáenz - Abril 2014
g ymensual2_1 = .  /*Act. Principal Trabajadores ocasionales*/
replace ymensual2_1 = s12p49a3*22   if s12p49a4 ==1
replace ymensual2_1 = s12p49a3*4.3  if s12p49a4 ==2
replace ymensual2_1 = s12p49a3*2    if s12p49a4 ==3
replace ymensual2_1 = s12p49a3      if s12p49a4 ==4
replace ymensual2_1 =.              if s12p49a4 ==9


*Aguinaldo, Bono y vacaciones
gen aguinaldo=s12p49_2p12/s12p49_2mp12 		if s12p49_2mp12!=99 & s12p49_2p12a==1 /*Act. Principal*/
*Modificación Mayra Sáenz Abril 2014 
g aguinaldo_= s12p49b_p5                      if s12p49b_p5a ==1 /*Act. Principal Trab. Ocasionales*/
gen aguinaldo2= s12p49_2s12/s12p49_2ms12 	if s12p49_2ms12!=99 & s12p49_2s12a==1 /*Act. Secundaria*/
gen bono= s12p49_2p13/s12p49_2mp13			if s12p49_2mp13!=99 & s12p49_2p13a==1 /*Act. Principal*/
gen bono2= s12p49_2s13/s12p49_2ms13			if s12p49_2ms13!=99 & s12p49_2s13a==1 /*Act. Secundaria*/

gen vacaciones=s12p49_2p14/s12p49_2mp14		if s12p49_2mp14!=99 & s12p49_2p14a==1
gen vacaciones2=s12p49_2s14/s12p49_2ms14	if s12p49_2ms14!=99 & s12p49_2s14a==1
*Modificación Mayra Sáenz Abril 2014 
g vacaciones_= s12p49b_p7                      if s12p49b_p7a ==1 /*Act. Principal Trab. Ocasionales*/

*Ingresos en especie
gen ynomonetario=s12p49_3p23 if s12p49==2
gen ynomonetario2=s12p49_3s23 if s12p49==2

/// Ingreso de los Independientes //////
gen yindmonet=s12p50_3p01 if s12p50==1
replace yindmonet =. if s12p50==. | s12p50==2

gen yindmonet2=s12p50_3s01 if s12p50==1
replace yindmonet2 =. if s12p50==.  | s12p50==2
replace yindmonet2 =. if s12p50_3s01==2222222222

*Autoconsumo 
gen yindnom=s12p50_2p01 
gen yindnom2=s12p50_2s01

//////////////////////////////////////////////
//////////  Ingresos No laborales    /////////
//////////////////////////////////////////////

gen yrentaprop=s12p51_30
replace yrentaprop =. if s12p51_30==9999999999

egen miss=rowmiss(s12p51_24- s12p51_29)
replace yrentaprop=. if miss==6
drop miss


gen yrentainmuebles=s12p53_39
replace yrentainmuebles=. if s12p53_39==.

gen ytransferencias=s12p54_51
replace ytransferencias=. if  s12p54_51==9999999999

gen yremesas=s12p54_41

*Modificación Mayra Sáenz Abril 2014 - Se incluyen las transferencias, producción agrícola, pecuaria
*pesquera, forestal y del patio.

*Transferencias del gobierno- Se pregunta sólo al jefe del hogar
g refrig   = s12p55_52p5 if s12p55_52p1 ==1
g almuerzo = s12p55_53p5 if s12p55_53p1 ==1
g mochila  = s12p55_54p5 if s12p55_54p1 ==1
g otro     = s12p55_55p5 if s12p55_55p1 ==1

*Producción Agrícola 12 meses *Existe alguien que gasta mas de los que gana 
g gtosagric = s12p63_82*(-1) if s12p57 ==1
egen ingagric1 = rsum(s12p59_77 s12p60_77 s12p61_77 s12p62_77 gtosagric)  if s12p57 ==1, missing 
g ingagric = ingagric1/12
replace ingagric=. if ingagric<0
*g autoconsag = s12p60_77 if s12p57 ==1

*Producción Pecuaria 12 meses
g gtospecua = s12p69_106* (-1) if s12p64 ==1
egen ingpecua1 = rsum(s12p66_99 s12p67_99  s12p68_99 gtospecua)   if s12p64 ==1, missing 
g ingpecua = ingpecua1/12
replace ingpecua=. if ingpecua<0
*Producción Pesquera 12 meses
g gtospesque = s12p73*(-1) if s12p72 ==1
egen ingpesque1 = rsum(s12p71 gtospesque)         if s12p70 ==1, missing 
g ingpesque = ingpesque1/12
replace ingpesque=. if ingpesque<0

*Producción Forestal 12 meses
g gtosfores= s12p77*(-1) if s12p76==1
egen ingfores1 = rsum(s12p75 s12p75a gtosfores)   if s12p74 ==1, missing 
g ingfores = ingfores1/12
replace ingfores=. if ingfores<0

*Producción Patio 12 meses
g gtospatio= s12p89*(-1) if s12p88==1
egen ingpatio1 = rsum(s12p80_111 s12p81_111 s12p83_116 s12p84_116 s12p86_120 s12p87_120 gtospatio)   if s12p78 ==1, missing 
g ingpatio = ingpatio1/12
replace ingpatio=. if ingpatio<0


***************
***ylmpri_ci***
***************
egen ylmpri_ci=rsum(ymensual aguinaldo bono vacaciones yindmonet ymensual_ ymensual_1 aguinaldo_ vacaciones_), missing
replace ylmpri_ci=. if (ymensual==. & aguinaldo==. & bono==. & vacaciones==. &  yindmonet==.) | emp_ci==0
*replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

****************
***ylnmpri_ci***
****************
egen ylnmpri_ci= rsum(ynomonetario yindnom), missing
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

***************
***ylmsec_ci***
***************
egen ylmsec_ci=rsum(ymensual2 aguinaldo2 bono2 vacaciones2 yindmonet2 ymensual2_ ymensual2_1)
replace ylmsec_ci=. if (ymensual2==. & aguinaldo2==. & bono2==. & vacaciones2==. &  yindmonet2==.) | emp_ci==0
*replace ylmsec_ci=0 if categosec_ci==4
replace ylmsec_ci=. if emp_ci==1 & s11p34==2
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************
*Modificación Mayra Sáenz - Abril 2014 
*Se incluye el autoconsumo
egen ylnmsec_ci= rsum(ynomonetario2 yindnom2), missing
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
egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==. & ylmotros_ci==. 
label var ylm_ci "Ingreso laboral monetario total"  

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

*************
***ynlm_ci***
*************
replace ingagric=. if ingagric<0
egen ynlm_ci=rsum(yrentaprop yrentainmuebles ytransferencias refrig almuerzo mochila otro ingagric ingpecua ingpesque ingfores ingpatio), missing
replace ynlm_ci=. if yrentaprop==. & yrentainmuebles==. & ytransferencias==. 
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
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

**************
*** ylm_ch ***
**************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"


**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

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

*****************
***rentaimp_ch***
*****************
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

*****************
***autocons_ci***
*****************
egen autocons_ci= rsum(yindnom yindnom2), missing
label var autocons_ci "Autoconsumo reportado por el individuo"

*****************
***autocons_ch***
*****************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
la var autocons_ch "Autoconsumo del Hogar"


****************
***remesas_ci***
****************
gen remesas_ci=yremesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
***remesas_ch***
****************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
label var remesas_ch "Remesas mensuales del hogar" 



			****************************
			***VARIABLES DE EDUCACION***
			****************************

gen aedu_ci=.
replace aedu_ci=0 if s07p15a==0 | s07p15a==1
*Primaria
replace aedu_ci=s07p15b 	if s07p15a==2 & s07p15b!=99
*Secundaria
replace aedu_ci=6+s07p15b   if s07p15a==3 & s07p15b!=99
*Superior o universitaria
replace aedu_ci=11+s07p15b   if s07p15a>=4 & s07p15a<=7 & s07p15b!=99
*Post-grado
replace aedu_ci=16+s07p15b   if (s07p15a==8 | s07p15a==9) & s07p15b!=99
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
gen edupi_ci=0
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen edupc_ci=0
replace edupc_ci=1 if aedu_ci==6 
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<11) 
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen edusc_ci=0
replace edusc_ci=1 if aedu_ci==11 
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
*Se trabaja con incompleto par universitaria, pues no es posible determinar esto para los programas técnicos
gen eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<16
label variable eduui_ci "Superior incompleto"

**************
***eduuc_ci***
**************
*Para la educación superior no es posible saber cuantos anios dura el ciclo
*por ello se hace una aproximación a través de titulación
gen eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=16
label variable eduuc_ci "Superior completo"

***************
***edus1i_ci***
***************
gen edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************
gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

**************
***eduac_ci***
**************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
gen asiste_ci=.
replace asiste_ci=1 if s07p16==1
replace asiste_ci=0 if s07p16==2
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
** creada por Angela Lopez Mayo 2019

g       pqnoasis1_ci = 1 if s07p17==7
replace pqnoasis1_ci = 2 if s07p17==9
replace pqnoasis1_ci = 3 if s07p17==2 | s07p17==13
replace pqnoasis1_ci = 4 if s07p17==12
replace pqnoasis1_ci = 5 if s07p17== 5 | s07p17== 6
replace pqnoasis1_ci = 6 if s07p17==10
replace pqnoasis1_ci = 7 if s07p17==8
replace pqnoasis1_ci = 8 if s07p17==3 | s07p17==4 
replace pqnoasis1_ci = 9 if s07p17==1 | s07p17==11 | s07p17==99
 
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
***tecnica_ci**
*************
gen tecnica_ci=(s07p15a==6)
label var tecnica_ci "=1 formacion terciaria tecnica"	


		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************

*En esta encuesta no hay información sobre la infraestructura del hogar!

****************
***aguared_ch***
****************
gen aguared_ch=.
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=.
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

*********
*raza_ci*
*********
* En este año no se puede identificar una variable que haga referencia a los grupos étnicos.
gen raza_ci=.
gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

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


