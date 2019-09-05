
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

local PAIS NIC
local ENCUESTA EMNV
local ANO "2009"
local ronda m7_m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: EMNV
Round: Julio-Octubre
Autores:Yesenia Loayza, 2012
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Septiembre de 2013

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
gen region_c= i01

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

gen factor_ch=factor 

***************
****idh_ch*****
***************
egen id_hogar = group(i00)
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************
gen idp_ci=s2p00
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen zona_c=i06
replace zona_c=0 if i06==2
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
gen anio_c=2009
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

gen mes_c= mesent

***************
***factor_ci***
***************
gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"
//Nota: se utiliza el mismo factor para hogares e individuos para hacerlo homogeneo con los otros países


		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************

*****************
***relacion_ci***
*****************

gen relacion_ci=s2p4
replace relacion_ci=4 if s2p4>=5 & s2p4<=8
replace relacion_ci=5 if s2p4==9 | s2p4==11
replace relacion_ci=6 if s2p4==10
label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci


**********
***sexo***
**********

gen sexo_ci=s2p5  
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=s2p2a 
label variable edad_ci "Edad del individuo"

**************
***civil_ci***
**************
gen civil_ci=1 if s2p7==6 | s2p7==7
replace civil_ci=2 if s2p7==1 | s2p7==2
replace civil_ci=3 if s2p7==3 | s2p7==4
replace civil_ci=4 if s2p7==5 
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

		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
****************
****condocup_ci*
****************
/*
gen buscotrab =((s5p8a>=1 & s5p8a<=8) | (s5p8b>=1 & s5p8b<=8) | (s5p8c>=1 & s5p8c<=8) | (s5p8d>=1 & s5p8d<=8))

gen condocup_ci=.
replace condocup_ci=1 if  s5p1==1 | (s5p2>=1 & s5p2<=10) |  s5p3==1 
replace condocup_ci=2 if  s5p7==1 & buscotrab==1
replace condocup_ci=3 if  condocup_ci!=1 &  condocup_ci!=2
replace condocup_ci=4 if  edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
drop buscotrab
*/

* Alternativa 2 con variables originales tomando en cuenta la definicion de armonizacion MGD 06/05/2014
gen condocup_ci=.
*2014, 10 Modificacion MLO (desactivo la opcion si contesta ninguno en p2)
*replace condocup_ci=1 if s5p1==1 | (s5p2>=1 & s5p2<=10) | (s5p4>=1 & s5p4<=4) | (s5p3==1 & s5p6<4)
replace condocup_ci=1 if s5p1==1 | (s5p2>=1 & s5p2<10) | (s5p4>=1 & s5p4<=4) | s5p4==7 | s5p4==8 | s5p5==1 | (s5p3==1 & (s5p6<4 & s5p6!=.))
replace condocup_ci=2 if condocup_ci!=1 & s5p7==1 & s5p11==1
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
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
replace pea_ci=1 if emp_ci==1 |desemp_ci==1

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=. /*No se prgunta*/
label var cotizando_ci "Cotizante a la Seguridad Social"

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
gen pension_ci=(i_pensi>=1 & i_pensi!=.) /*No viene la variable orginal, solo esta */
label var pension_ci "1=Recibe pension contributiva"


*************
*   ypen_ci *
*************
gen ypen_ci=i_pensi if pension_ci==1
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
replace cesante_ci=1 if s5p12==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =linea_ge 
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =linea_ex 
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

** Ultimo cambio MGD 04/29/2014
gen rama_ci=.
replace rama_ci=1 if (s5p15>=100& s5p15<=599) & emp_ci==1
replace rama_ci=2 if (s5p15>=1010 & s5p15<=1499) & emp_ci==1
replace rama_ci=3 if (s5p15>=1510 & s5p15<=3729) & emp_ci==1
replace rama_ci=4 if (s5p15>=4010 & s5p15<=4199) & emp_ci==1
replace rama_ci=5 if (s5p15>=4510 & s5p15<=4559) & emp_ci==1
replace rama_ci=6 if (s5p15>=5010 & s5p15<=5529) & emp_ci==1
replace rama_ci=7 if (s5p15>=6010 & s5p15<=6429) & emp_ci==1
replace rama_ci=8 if (s5p15>=6510 & s5p15<=7029) & emp_ci==1
replace rama_ci=9 if (s5p15>=7111 & s5p15<=9800) & emp_ci==1


label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

*************
**salmm_ci***
*************
/*generat salmm_ci=.
replace salmm_ci= 1573.10+2437.5/2 if rama_ci==1
replace salmm_ci= 2879             if rama_ci==2
replace salmm_ci= 2155.5+2556.7/2  if rama_ci==3
replace salmm_ci= 2904.4           if rama_ci==4  
replace salmm_ci= 3587.5           if rama_ci==5 
replace salmm_ci= 2940.4           if rama_ci==6 | rama_ci==7
replace salmm_ci= 3587.5           if rama_ci==8
replace salmm_ci= 2247.3+1999.1/2  if rama_ci==9
replace salmm_ci=(1573.10+2437.5+2879+2155.5+2556.7+2904.4+3587.5+2940.4+2940.4+3587.5+2247.3+1999.1)/12 if salmm_ci==. 
*/
* 2015 MGD: actualizan salarios minimos segun la ctividad

generat salmm_ci=.
replace salmm_ci= 2004.3  if rama_ci==1
replace salmm_ci= 2878.8  if rama_ci==2
replace salmm_ci= 2356.05 if rama_ci==3
replace salmm_ci= 2940.3  if rama_ci==4  
replace salmm_ci= 3595.2  if rama_ci==5 
replace salmm_ci= 2940.3  if rama_ci==6 | rama_ci==7
replace salmm_ci= 3587.5  if rama_ci==8
replace salmm_ci= 2247.3  if rama_ci==9
replace salmm_ci= 2534.93 if salmm_ci==. 
label var salmm_ci "Salario minimo legal"


*****************
***desalent_ci***
*****************
gen desalent_ci=0 if s5p9!=.
replace desalent = 1 if s5p9==4 | s5p9==10


******************************
*	subemp_ci
******************************

egen tothoras = rsum(s5p17 s5p32), missing
*No está la variable s5p50 (desea trabajar mas horas y esta disponible para hacerlo)

gen subemp_ci=.
label var subemp_ci "Dispuestas a trabajar mas, pero trabajan 30hs o menos(semana)"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.

*****************
***horaspri_ci***
*****************

gen horaspri_ci=s5p17
replace horaspri_ci=. if emp_ci==0 | s5p17>168

*****************
***horastot_ci***
*****************
gen horastot_ci=tothoras
replace horastot_ci=. if emp_ci==0

******************
***categopri_ci***
******************

* Modificación Marcela G. Rubio - Diciembre 2014
* Los trabajadores miembros de cooperativas deben incluirse en la categoría 2

/*
gen categopri_ci=.
replace categopri_ci=1 if s5p18==3
replace categopri_ci=2 if s5p18==4
replace categopri_ci=3 if s5p18==1 | s5p18==2 | s5p18==5
replace categopri_ci=4 if s5p18==6 | s5p18==7
replace categopri_ci=. if emp_ci==0
*/

gen categopri_ci=.
replace categopri_ci=1 if s5p18==3
replace categopri_ci=2 if s5p18==4 | s5p18==5
replace categopri_ci=3 if s5p18==1 | s5p18==2 
replace categopri_ci=4 if s5p18==6 | s5p18==7
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1 "Patron" 2 "Cuenta propia" 
label define categopri_ci 3 "Empleado" 4 " Trabajador no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if s5p33==3
replace categosec_ci=2 if s5p33==4
replace categosec_ci=3 if s5p33==1 | s5p33==2 | s5p33==5
replace categosec_ci=4 if s5p33==6 | s5p33==7
replace categosec_ci=. if emp_ci==0
label define categosec_ci 1"Patron" 2 "Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Trabajador no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

***************
***segsoc_ci***
***************
gen segsoc_ci=.
label variable segsoc_ci "Tiene Seguro Social"

*****************
***nempleos_ci***
*****************
gen nempleos_ci=1 if s5p28==2
replace nempleos_ci=2 if s5p28==1
replace nempleos_ci=. if emp_ci==0

*****************
***tamfirma_ci***
*****************
*Nota: En el año 2009 no existe la pregunta "cuántas personas incluida usted trabajan regularmente en esta empresa"
*Por lo tanto no se puede obtnere el número de empleados que trabajan en la empresa
*gen firmapeq_ci=. 

*****************
***spublico_ci***
*****************
*Nota: En el año 2009 no se pregunta acerca del tipo de establecimiento en que trabaja
gen spublico_ci=.

**************
***ocupa_ci***
**************
*Se utiliza ciuo88
tab s5p14
tostring s5p14, replace force
gen digito = "0"
egen x = concat(digito s5p14) if length(s5p14)==3
replace s5p14=x if length(s5p14)==3
gen ocupa= real(substr(s5p14,1,2))

gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=34)  & emp_ci==1
replace ocupa_ci=2 if (ocupa>=11 & ocupa<=13)  & emp_ci==1
replace ocupa_ci=3 if (ocupa>=41 & ocupa<=42)  & emp_ci==1
replace ocupa_ci=4 if (ocupa==52)              & emp_ci==1
replace ocupa_ci=5 if (ocupa==51)              & emp_ci==1
replace ocupa_ci=6 if (ocupa>=61 & ocupa<=62)  & emp_ci==1
replace ocupa_ci=7 if (ocupa>=71 & ocupa<=83)  & emp_ci==1
replace ocupa_ci=8 if (ocupa==1)               & emp_ci==1
replace ocupa_ci=9 if (ocupa>=91 & ocupa<=94)  & emp_ci==1
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************
*Se utiliza clasificacion CUAEN

tostring s5p15, replace force
egen y = concat(digito s5p15) if length(s5p15)==3
replace s5p15=y if length(s5p15)==3
gen rama=real(substr(s5p15,1,2))


drop digito x y ocupa rama 

****************
***durades_ci***
****************
*Nota: en el año 2009 no es posible calcularlo.
//Se eliminó la pregunta del formulario.

gen durades_ci= . 
label variable durades_ci "Duracion del desempleo en meses- Intervalos"
label def durades_ci 1"Menos de un mes" 2"1 mes a menos de 2 meses" 3"2 meses a menos de 3 meses"
label def durades_ci 4"3 meses a menos de 6 meses" 5"6 meses a menos de 1 año" 6"Más de 1 año", add
label val durades_ci durades1_ci 

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
*Nota: Se elimina la pregunta "cuanto tiempo en total tiene de trabajar en.."
*Por tanto en 2009 #N/A
/*MGD 9/25/2014: mala la varibale en la base, se la deja como missing.
gen antiguedad_ci=s5p16a if s5p16b==4
replace antiguedad_ci=s5p16a/12 if s5p16b==3
replace antiguedad_ci=s5p16a/51.6 if s5p16b==2
replace antiguedad_ci=s5p16a/365 if s5p16b==1
label var antiguedad_ci "Antiguedad en la actividad actual en anios"*/

*************
*tamemp_ci***
*************
gen tamemp_ci=. /*No se prgunta*/
label var tamemp_ci "# empleados en la empresa de la actividad principal"


*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((s5p10==3 | s5p10==6) & condocup_ci==3)
replace categoinac_ci = 2 if  (s5p10==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s5p10==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
/*gen formal=1 if cotizando_ci==1

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
*/

*Modificación Mayra Sáenz - Septiembre 2014
*Afiliado y cotizando están como missing values.
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"


************************************************************************
**************************INGRESOS**************************************
************************************************************************

****************
***ylmpri_ci ***
****************

*para empleados
gen salario= .
replace salario=s5p19a*30 if s5p19b==1
replace salario=s5p19a*4.3  if s5p19b==2
replace salario=s5p19a*2  if s5p19b==3 | s5p19b==4
replace salario=s5p19a    if s5p19b==5
replace salario=s5p19a/3  if s5p19b==6
replace salario=s5p19a/6  if s5p19b==7
replace salario=s5p19a/12 if s5p19b==8
replace salario=.         if emp_ci==0 
replace salario=.         if s5p19a==9999998 

*para patron o cuenta propia
gen ganancia = .
replace ganancia = s5p26a*30 if  s5p26b==1
replace ganancia = s5p26a*4.3 if  s5p26b==2
replace ganancia = s5p26a*2 if  s5p26b==3 | s5p26b==4
replace ganancia = s5p26a if  s5p26b==5
replace ganancia = s5p26a/3 if  s5p26b==6
replace ganancia = s5p26a/6 if  s5p26b==7
replace ganancia = s5p26a/12 if  s5p26b==8
replace ganancia =. if emp_ci==0


*Comision = comision + horas extras + propinas + otros (promedio del mes)
gen comision   =  s5p20b         if s5p20a==1 
replace comision =. if s5p20b==999999 | s5p20b== 999998
*Decimo tercer sueldo lo da anual pero se detalla a cuantos meses corresponde.
gen dectercero =  s5p21b/s5p21c  if s5p21a==1 
gen alimentos  =  s5p22b         if s5p22a==1 | s5p22a==2 
gen vivienda   = s5p23b          if s5p23a==1 | s5p23a==2
gen transporte = s5p24b          if s5p24a==1 | s5p24a==2
*Se pregunta cuantas veces recibio ingresos por uniforme al año
generat uniforme = s5p25b/12      if (s5p25a==1 | s5p25a==2) & s5p25c==1
replace uniforme = (s5p25b*2)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==2
replace uniforme = (s5p25b*3)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==3
replace uniforme = (s5p25b*4.3)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==4
replace uniforme = (s5p25b*5)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==5
replace uniforme = (s5p25b*6)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==6
replace uniforme = (s5p25b*7)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==7
replace uniforme = (s5p25b*8)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==8
replace uniforme = (s5p25b*9)/12  if (s5p25a==1 | s5p25a==2) & s5p25c==9
replace uniforme = (s5p25b*10)/12 if (s5p25a==1 | s5p25a==2) & s5p25c==10
replace uniforme = (s5p25b*11)/12 if (s5p25a==1 | s5p25a==2) & s5p25c==11
replace uniforme =  s5p25b        if (s5p25a==1 | s5p25a==2) & s5p25c==12

egen ylmpri_ci=rsum(salario ganancia comision dectercero)
replace ylmpri_ci=. if (salario==. & comision==. & dectercero==. & ganancia==.) | emp_ci==0
replace ylmpri_ci=. if  ylmpri_ci==9999998
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 
*Nota: no se esta considerando el autoconsumo como ingreso

****************
***ylnmpri_ci***
****************
egen ylnmpri_ci=rsum(alimentos vivienda transporte uniforme)
replace ylnmpri_ci=. if (alimentos==. & vivienda==. & transporte==. & uniforme==.) | emp_ci==0
replace ylnmpri_ci=. if  ylnmpri_ci==9999998
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal" 

***************
***ylmsec_ci***
***************

*para empleados
gen salariosec= .
replace salariosec=s5p34a*30 if s5p34b==1
replace salariosec=s5p34a*4.3  if s5p34b==2
replace salariosec=s5p34a*2  if s5p34b==3 | s5p34b==4
replace salariosec=s5p34a    if s5p34b==5
replace salariosec=s5p34a/3  if s5p34b==6
replace salariosec=s5p34a/6  if s5p34b==7
replace salariosec=s5p34a/12 if s5p34b==8
replace salariosec=.         if emp_ci==0 
replace salariosec=.         if salariosec==0 //existe una observación que tiene cero ingreso//

*para patron, cuenta propia o miemro cooperativa
gen gananciasec = .
replace gananciasec = s5p41a*30 if  s5p41b==1
replace gananciasec = s5p41a*4.3  if  s5p41b==2
replace gananciasec = s5p41a*2  if  s5p41b==3 | s5p41b==4
replace gananciasec = s5p41a    if  s5p41b==5
replace gananciasec = s5p41a/3  if  s5p41b==6
replace gananciasec = s5p41a/6  if  s5p41b==7
replace gananciasec = s5p41a/12 if  s5p41b==8
replace gananciasec =.          if emp_ci==0
replace gananciasec =.          if s5p41a==999998

*Comision = comision + horas extras + propinas + otros (promedio del mes)
gen comisionsec   =  s5p35b         if s5p35a==1
replace comisionsec=. if s5p35b==999999 
*Decimo tercer sueldo lo da anual pero se detalla a cuantos meses corresponde.
gen dectercerosec =  s5p36b/s5p36c  if s5p36a==1 

gen alimentossec  =  s5p37b         if s5p37a==1 | s5p37a==2 
replace alimentossec=. if s5p37b== 999999

gen viviendasec   =  s5p38b         if s5p38a==1 | s5p38a==2
replace viviendasec=.               if s5p38b==999999

gen transportesec =  s5p39b         if s5p39a==1 | s5p39a==2
*Se pregunta cuantas veces recibio ingresos por uniforme al año
generat uniformesec = s5p40b/12      if (s5p40a==1 | s5p40a==2) & s5p40c==1
replace uniformesec = (s5p40b*2)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==2
replace uniformesec = (s5p40b*3)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==3
replace uniformesec = (s5p40b*4.3)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==4
replace uniformesec = (s5p40b*5)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==5
replace uniformesec = (s5p40b*6)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==6
replace uniformesec = (s5p40b*7)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==7
replace uniformesec = (s5p40b*8)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==8
replace uniformesec = (s5p40b*9)/12  if (s5p40a==1 | s5p40a==2) & s5p40c==9
replace uniformesec = (s5p40b*10)/12 if (s5p40a==1 | s5p40a==2) & s5p40c==10
replace uniformesec = (s5p40b*11)/12 if (s5p40a==1 | s5p40a==2) & s5p40c==11
replace uniformesec =  s5p40b        if (s5p40a==1 | s5p40a==2) & s5p40c==12

egen ylmsec_ci=rsum(salariosec gananciasec comisionsec dectercerosec)
replace ylmsec_ci=. if (salariosec==. & comisionsec==. & dectercerosec==. & gananciasec==.) | emp_ci==0
replace ylmsec_ci=. if  ylmsec_ci==9999998 | ylmsec_ci==1999998
replace ylmsec_ci=0 if categosec_ci==4
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************

egen ylnmsec_ci=rsum(alimentossec viviendasec transportesec uniformesec)
replace ylnmsec_ci=. if (alimentossec==. & viviendasec==. & transportesec==. & uniformesec==.) | emp_ci==0
replace ylnmsec_ci=. if  ylnmsec_ci==1999998 |  ylnmsec_ci==999999

drop salario* comision* dectercero* alimentos* vivienda* transporte* uniforme*

****************
***ylnmpri_ci***
****************

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

gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

gen remesas_ch=.
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

gen autoc     = s5p27b*30 if s5p27a==1 & s5p27c==1
replace autoc = s5p27b*4.3  if s5p27a==1 & s5p27c==2
replace autoc = s5p27b*2  if s5p27a==1 & (s5p27c==3 | s5p27c==4)
replace autoc = s5p27b    if s5p27a==1 & s5p27c==5
replace autoc = s5p27b/3  if s5p27a==1 & s5p27c==6
replace autoc = s5p27b/6  if s5p27a==1 & s5p27c==7
replace autoc = s5p27b/12 if s5p27a==1 & s5p27c==8

gen autocsec     = s5p42b*30 if s5p42a==1 & s5p42c==1
replace autocsec = s5p42b*4.3  if s5p42a==1 & s5p42c==2
replace autocsec = s5p42b*2  if s5p42a==1 & (s5p42c==3 | s5p42c==4)
replace autocsec = s5p42b    if s5p42a==1 & s5p42c==5
replace autocsec = s5p42b/3  if s5p42a==1 & s5p42c==6
replace autocsec = s5p42b/6  if s5p42a==1 & s5p42c==7
replace autocsec = s5p42b/12 if s5p42a==1 & s5p42c==8

egen autocons_ci= rsum(autoc autocsec)
replace autocons_ci=. if (autoc==. & autocsec==.) | emp_ci==0
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

gen rentaimp_ch=s1p14a
replace rentaimp_ch=. if s1p14a==99999 | s1p14a==99998
*Nota: se estan perdiendo las observaciones de s1p14b por que estan en dólares

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
*Parece que los dsatos del módulo de educación presentaran alguna inconsistencia
*************
***aedu_ci*** 
*************

/*
gen aedu_ci=.
replace aedu_ci=0         if s4p12a==0 | s4p12a==1
replace aedu_ci=2         if s4p12b==1 & s4p12a==2 
replace aedu_ci=4         if s4p12b==2 & s4p12a==2 
replace aedu_ci=6         if s4p12b==3 & s4p12a==2 
replace aedu_ci=s4p12b    if s4p12a==3
replace aedu_ci=6+s4p12b  if s4p12a==4
replace aedu_ci=11+s4p12b if s4p12a>=5 & s4p12a<=9
replace aedu_ci=16+s4p12b if s4p12a==10 &  s4p12a==11
replace aedu_ci=.         if s4p12a==98 | s4p12a==99
*No se a que se refiere la educacion especial (en el 2010 ya no existe esta categoria)
label var aedu_ci "Anios de educacion aprobados" 
*/

* Modificación Marcela Rubio Septiembre 2014: cambio en sintaxis

gen aedu_ci=.
replace aedu_ci=0 if s4p12a==0 | s4p12a
replace aedu_ci= s4p12b if s4p12a==3
replace aedu_ci= s4p12b+6 if s4p12a==4 
replace aedu_ci= s4p12b+11 if (s4p12a>=5 & s4p12a<=9)
replace aedu_ci= s4p12b+16 if (s4p12a==10 | s4p12a==11)


**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

/*
gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"
*/

** Modificación Marcela Rubio Septiembre 2014: cambio en sintaxis ya que educ secundaria completa es 11 años en NIC

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"
***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

/*
gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"
*/

* Modificación Marcela Rubio Septiembre 2014: corrección en sintaxis ya que educación terciaria en NIC comienza despues de 11 años

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<17
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
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************
*No considero la asistencia de los niños menores de 7 años

gen asiste_ci= (s4p15==1 & edad_ci>=7 & edad!=.)
*Mayra Sáenz Mayo, 2015: Se incluye la asistencia de los menores de 7.
replace asiste_ci=1 if (s4p1 ==3 | s4p1 ==4) & edad_ci<7 
label variable asiste_ci "Asiste actualmente a la escuela"

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

**************
***pqnoasis***
**************
*Se escoje solo para los mayores 7 de años ya que para los menores de 7 existen otras categorias
gen pqnoasis_ci=s4p16
label var pqnoasis_ci "Razones para no asistir"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if s4p14==14
replace pqnoasis1_ci = 2 if s4p14==5
replace pqnoasis1_ci = 3 if s4p14==13 | s4p14==16
replace pqnoasis1_ci = 4 if s4p14==2
replace pqnoasis1_ci = 5 if s4p14==4  | s4p14==11 | s4p14==12 
replace pqnoasis1_ci = 6 if s4p14==3
replace pqnoasis1_ci = 7 if s4p14==1
replace pqnoasis1_ci = 8 if s4p14==6  | s4p14==7  | s4p14==8 | s4p14==9
replace pqnoasis1_ci = 9 if s4p14==10 | s4p14==15

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci



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

gen edupub_ci=(s4p18==1 | s4p18==2) & (edad_ci>=7 & edad!=.)
replace edupub_ci=. if edad_ci<7 | (s4p18==8 | s4p18==9)
label var edupub_ci "Asiste a un centro de enseñanza público"

*************
***tecnica_ci**
*************
gen tecnica_ci=(s4p12a==8)
label var tecnica_ci "=1 formacion terciaria tecnica"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

****************
***aguared_ch***
****************
gen aguared_ch=.
replace aguared_ch=(s1p15a==1 | s1p15a==2)
replace aguared_ch=. if s1p15a==98 | s1p15a==99
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=0
replace aguadist_ch=1 if s1p15a==1
replace aguadist_ch=2 if s1p15a==2 
replace aguadist_ch=3 if s1p15a>=3 & s1p15a<=10
replace aguadist_ch=. if s1p15a==98 | s1p15a==99
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch

*****************
***aguamala_ch***
*****************
gen aguamala_ch=(s1p15a>=3 & s1p15a<=10)
replace aguamala_ch=. if s1p15a==98 | s1p15a==99

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.

************
***luz_ch***
************

gen luz_ch=(s1p21==1)
label var luz_ch  "La principal fuente de iluminación es electricidad"
replace luz_ch=. if s1p21==98 | s1p21==98
****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
gen combust_ch= (s1p25==2 | s1p25==4 | s1p25==5)
replace combust_ch=. if s1p25==8 | s1p25==9
label var combust_ch "Principal combustible gas o electricidad" 

*************
***bano_ch***
*************

gen bano_ch=(s1p18!=6)
replace bano_ch=. if s1p18==8 | s1p18==9
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
replace des1_ch=0 if s1p18==6
replace des1_ch=1 if s1p18==3 | s1p18==4 /*Red o pozo septico*/
replace des1_ch=2 if s1p18==1 | s1p18==2 /*Letrina */
replace des1_ch=3 if s1p18==5 /*Superficie*/
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************
gen des2_ch=.
replace des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2
replace des2_ch=2 if des1_ch==3
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************
gen piso_ch=.
replace piso_ch=0 if s1p5==5
replace piso_ch=1 if s1p5>=1 & s1p5<=4
replace piso_ch=2 if s1p5==6
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch


**************
***pared_ch***
**************
gen pared_ch=.
replace pared_ch=0 if s1p4>=12 & s1p4<=15
replace pared_ch=1 if s1p4>=1 & s1p4<=11
replace pared_ch=2 if s1p4==16
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch

**************
***techo_ch***
**************
gen techo_ch=.
replace techo_ch=0 if s1p6==2 |s1p6==5 |s1p6==6
replace techo_ch=1 if s1p6==1 | s1p7==3 | s1p7==4
replace techo_ch=2 if s1p6==7
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes"
label val techo_ch techo_ch

**************
***resid_ch***
**************

gen resid_ch=.
replace resid_ch=0 if s1p19==1 | s1p19==2
replace resid_ch=1 if s1p19==3 | s1p19==4
replace resid_ch=2 if s1p19==5 | s1p19==6
replace resid_ch=2 if s1p19>=7 & s1p19<=9
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
gen dorm_ch=s1p10
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
gen  cuartos_ch =s1p9
replace cuartos_ch=. if  s1p9==99
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
replace telef_ch=1 if s1p28d==1 
replace telef_ch=0 if s1p28d==2 
label var telef_ch "El hogar tiene servicio telefónico fijo"

gen cel_ch=  (s1p28e==1)

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
replace internet_ch=1 if s1p28c ==1
replace internet_ch=0 if s1p28c ==2
label var internet_ch "El hogar posee conexión a Internet"

**************
***vivi1_ch***
**************
gen vivi1_ch=.
replace vivi1_ch= 1 if s1p3==1 | s1p3==2
replace vivi1_ch= 2 if s1p3==3 | s1p3==4
replace vivi1_ch= 3 if  s1p3>=5 & s1p3<=7
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
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
replace viviprop_ch = 0 if s1p11==1 
replace viviprop_ch = 1 if s1p11==3 | s1p1==4
replace viviprop_ch = 2 if s1p11==2
replace viviprop_ch = 3 if s1p11>=5 & s1p11<=7
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2 "propia y enproceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch = .
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************

gen vivialq_ch= s1p12a  /* falta la parte reportada en dolares */
label var vivialq_ch "Alquiler mensual"
*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch= s1p14a /* falta la parte reportada en dolares */
replace vivialqimp_ch= . if s1p14a==99998 | s1p14a==99999
label var vivialqimp_ch "Alquiler mensual imputado"

*********
*raza_ci*
*********
*En 2009, no consta la pregunta acerca del grupo étnico.
gen raza_ci=.
gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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



