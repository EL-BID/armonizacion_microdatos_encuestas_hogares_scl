
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
 


*global ruta = "\\Sdssrv03\surveys"

local PAIS CRI
local ENCUESTA ENAHO
local ANO "2010"
local ronda m7 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Costa Rica
Encuesta: ENAHO
Round: m7
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 1 de octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=1 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

capture destring provincia canton distrito, replace

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013	
/*gen region_c=provincia

label define region_c  ///
1	"San José" ///
2	"Alajuela" ///
3	"Cartago" ///
4	"Heredia" ///
5	"Guanacaste" ///
6	"Puntarenas" ///
7	"Limón" 
      
           
label value region_c region_c
label var region_c "División política, provincias"*/

*Inclusión Mayra Sáenz - Julio 2014	
gen region_c=region

label define region_c  ///
	       1 "Central" ///
           2 "Chorotega" ///
           3 "Pacífico central" ///
           4 "Brunca" ///
           5 "Huetar Atlántica" ///
           6 "Huetar Norte"
label value region_c region_c
label var region_c "División política, region de planificacion"


***************
***factor_ch***
***************
gen factor_ch=factor
label var factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
***************
/*sort provincia canton distrito segmento estructura parte vivienda hogar
egen idh_ch = group(provincia canton distrito segmento estructura parte vivienda hogar)
label var idh_ch "ID del hogar"
*/
destring segmento cuestionario hogar, replace
sort segmento cuestionario hogar
egen idh_ch = group(segmento cuestionario hogar)
duplicates report idh_ch
label var idh_ch "ID del hogar"


*************
****idp_ci****
**************
gen idp_ci=linea
label var idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen zona_c=0 if zona==2
replace zona_c=1 if zona==1
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************
gen str3 pais_c="CRI"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2010
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=7
label variable mes_c "Mes de la encuesta"
label define mes_c 7 "Julio" 
label value mes_c mes_c

*****************
***relacion_ci***
*****************
gen relacion_ci=1 		if a3==1
replace relacion_ci=2 	if a3==2
replace relacion_ci=3 	if a3==3
replace relacion_ci=4 	if a3>=4 & a3<=9 
replace relacion_ci=5 	if a3==10 | a3==12
replace relacion_ci=6 	if a3==11
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
gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci=a4
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=a5
replace edad_ci=. if a5==98 | a5==99 
label variable edad_ci "Edad del individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
*En este año  no se dispone de esta variable.
gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo"  


**************
***civil_ci***
**************
gen civil_ci=.
replace civil_ci=1 if a26==6
replace civil_ci=2 if a26==1 | a26==2
replace civil_ci=3 if a26==3 | a26==4
replace civil_ci=4 if a26==5
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
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"



		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci = lp

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = cba

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=1 if (a10>=1 & a10<=3) | a13==1
recode cotizando_ci .=0 
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
replace tipopen_ci=a12
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=a11
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if b1==1 | (b2 >= 1 & b2 <= 8) | b3==1 | (b5 >= 1 & b5 <= 5) & estabili != 30 /*ocupado frecuente*/
replace condocup_ci=1 if b1==1 | (b2 >= 1 & b2 <= 8) | b3==1 | (b5 >= 1 & b5 <= 5) & estabili == 30 /*ocupado estacional*/
replace condocup_ci=2 if (b6== 8 | b6== 9) & ((b7a== 1 | b7b== 2 | b7c== 3 | b7d== 4 | b7e== 5 | b7f==6 | b7g == 7 | b7h == 08 | b7i == 09 | b7j == 10 | b7k== 11) | (b8==1 | b8==2 | b8==3)) & g6==1 /*desempleado cesante*/
replace condocup_ci=2 if (b6== 8 | b6==9 ) & ((b7a== 1 | b7b== 2 | b7c== 3 | b7d== 4 | b7e== 5 | b7f== 6 | b7g== 7 | b7h ==08 | b7i ==09 | b7j== 10 | b7k== 11) | (b8==1 | b8==2 | b8==3)) & g6==2 /*desempleado primera vez*/
recode condocup_ci .=3 if  edad_ci>=12
replace condocup_ci=4 if  edad_ci<12

/*esta version no permitia determinar la situacion de las personas entre 12 y 15
replace condocup_ci=1 if condact==11 | condact==12
replace condocup_ci=2 if condact==21 | condact==22
replace condocup_ci=3 if condact>=31 & condact<=34 & edad_ci>=10
replace condocup_ci=4 if  edad_ci<10*/
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
* MGD 12/4/2015: condicionado a que este desocupado
gen cesante_ci=1 if g6==1 & condocup_ci==2
recode cesante_ci .=0 if condocup_ci==2
* No todos los desempleados respondieron si han trabajado antes
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
**pension_ci*
*************

gen pension_ci=1 if h9j == 1 
replace pension_ci =0 if h9j == 2 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************
recode h9j1 (99999999=.)
/*
1 mes
           2 bimestre
           3 trimestre
           4 cuatrimestre
           6 semestre
           8 año
           9 ignorado
*/

gen ypen_ci=h9j1 if  h9j2 ==1
replace ypen_ci=h9j1/2 if  h9j2 ==2
replace ypen_ci=h9j1/3 if  h9j2 ==3
replace ypen_ci=h9j1/4 if  h9j2 ==4
replace ypen_ci=h9j1/6 if  h9j2 ==6
replace ypen_ci=h9j1/12 if  h9j2 ==8
replace ypen_ci=. if  h9j2 ==9

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen byte pensionsub_ci= 1 if h9e==1
replace pensionsub_ci =0 if h9e==2
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen aux_ps= h9e1 if h9e2==1
replace aux_ps = . if h9e1==9999999
destring aux_ps, replace
gen  ypensub_ci=aux_ps
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**salmm_ci***
*************

* 2010 *Salario minimo OIT
* 2015 MGD cambio por datos oficiales M. Trabajo segun zona urbana y rural
gen salmm_ci= 	187043 if zona_c==0
replace salmm_ci =234981.3 if zona_c==1
label var salmm_ci "Salario minimo legal"


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
gen desalent_ci=(emp_ci==0 & (b8==5 | b8==6 |b8==7 |b8==8))
replace desalent_ci=. if condactre==.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=c2a1 
replace horaspri_ci=. if c2a1==999
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horassec_ci***
*****************
gen horassec_ci=c2b1 
replace horassec_ci=. if c2b1==999
replace horassec_ci=. if emp_ci==0
label var horassec_ci "Horas trabajadas semanalmente en el trabajo secundario"

*****************
***horastot_ci***
*****************
egen horastot_ci=rsum(horaspri_ci horassec_ci), missing 
replace horastot_ci=. if horaspri_ci==. & horassec_ci==.
replace horastot_ci=. if emp_ci==0
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

***************
***subemp_ci***
***************
* Modificacion MGD 06/24/2014: mal generada la variable, se corrije con las variables desea trabajar mas horas y disponibilidad.
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci<=30 & c3a==1 & (c3b==1 | c3b==2)) & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=(horastot_ci<=30 & c1==2)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
gen categopri_ci=.
replace categopri_ci=1 if posiempagrupri==21
replace categopri_ci=2 if posiempagrupri==22 
replace categopri_ci=3 if posiempagrupri==11 | posiempagrupri==12 
replace categopri_ci=4 if posiempagrupri==13 
replace categopri_ci=. if emp_ci==0
label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************
gen categosec_ci=.
replace categosec_ci=1 if posiempagrusec==21
replace categosec_ci=2 if posiempagrusec==22 
replace categosec_ci=3 if posiempagrusec==11 | posiempagrusec==12 
replace categosec_ci=4 if posiempagrusec==13 
replace categosec_ci=. if emp_ci==0
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if e1==1 & categopri_ci==3
replace tipocontrato_ci=2 if (e1>=2 & e1<=5) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & c1==1
replace nempleos_ci=2 if emp_ci==1 & c1==2
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
replace firmapeq_ci=1 if c10<=5
replace firmapeq_ci=0 if c10>5 & c10<99
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************
gen spublico_ci=.
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
* Utiliza la COCR (clasificación propia)
gen ocupa_ci=.
replace ocupa_ci=1 if c9a>=2111 & c9a<=3492 & emp_ci==1
replace ocupa_ci=2 if c9a>=1111 & c9a<=1133 & emp_ci==1
replace ocupa_ci=3 if c9a>=4111 & c9a<=4224 & emp_ci==1
replace ocupa_ci=4 if ((c9a>=5210 & c9a<=5222) | (c9a>=9111 & c9a<=9115)) & emp_ci==1
replace ocupa_ci=5 if ((c9a>=5110 & c9a<=5169) | (c9a>=9120 & c9a<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((c9a>=6111 & c9a<=6210) | (c9a>=9211 & c9a<=9233)) & emp_ci==1
replace ocupa_ci=7 if ((c9a>=7111 & c9a<=8330) | (c9a>=9311 & c9a<=9334)) & emp_ci==1
replace ocupa_ci=9 if c9a>=9800 & emp_ci==1
replace ocupa_ci=. if emp_ci==0
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
replace rama_ci=1 if (c8>=1110 & c8<=5002)   & emp_ci==1
replace rama_ci=2 if (c8>=10101 & c8<=14292) & emp_ci==1
replace rama_ci=3 if (c8>=15111 & c8<=37203) & emp_ci==1
replace rama_ci=4 if (c8>=40100 & c8<=41000) & emp_ci==1
replace rama_ci=5 if (c8>=45100 & c8<=45500) & emp_ci==1
replace rama_ci=6 if (c8>=50101 & c8<=55300) & emp_ci==1
replace rama_ci=7 if (c8>=60100 & c8<=64203) & emp_ci==1
replace rama_ci=8 if (c8>=65110 & c8<=70200) & emp_ci==1
replace rama_ci=9 if (c8>=71111 & c8<=99999) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
/*
           1 un mes o menos
           2 más de un mes a tres meses
           3 más de tres meses a seis meses
           4 más de seis meses a un año
           5 más de un año a tres años
           6 más de tres años
           9 ignorado
*/


gen durades_ci=.
*un mes o menos
replace durades_ci=(1+4.3)/2/4.3 if g2==1
*MÁS de 1 a 3 meses
replace durades_ci=(1+3)/2 if g2==2
*MÁS de 3 a 6 meses
replace durades_ci=(3+6)/2 if g2==3
*MÁS de 6 a 12 meses
replace durades_ci=(6+12)/2 if g2==4
*MÁS de 1 año a 3 años
replace durades_ci=(12+36)/2 if g2==5
*MÁS de 3 años
replace durades_ci=(36+48)/2 if g2==6
label variable durades_ci "Duracion del desempleo en meses"

*******************
***antiguedad_ci***
*******************
* Correccion MGD 08/05/2014: generacion de variable que estaba como missing.
* Independientes
g aux0=d4a
g aux1=d4b*30
g aux2=d4c*365
egen ind=rsum(aux0 aux1 aux2), m
replace ind=ind/365

* Asalariados
g aux3=e3a
g aux4=e3b*30
g aux5=e3c*365
egen asal=rsum(aux3 aux4 aux5), m
replace asal=asal/365

egen antiguedad_ci=rsum(ind asal), m
replace antiguedad_ci=. if antiguedad_ci==99
replace antiguedad_ci=. if antiguedad_ci>edad_ci
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***tamemp_ci***
*******************
         
*Costa Rica Pequeña 1 a 5, Mediana 6 a 19, Grande Más de 19
/*
gen tamemp_ci = 1 if (c10>=1 & c10<=5)
replace tamemp_ci = 2 if (c10>=6 & c10<=19)
replace tamemp_ci = 3 if (c10>19)
*/
* 2014, 06 Modificacion MLO, según los codigos del formulario
gen tamemp_ci = 1 if (c10>=1 & c10<=5)
replace tamemp_ci = 2 if (c10>=6 & c10<=10)
replace tamemp_ci = 3 if (c10>10 & c10!=.)
replace tamemp_ci = . if c10==99 /*missing*/

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************
/*

raznoacteco:
           1 pensionado o jubilado
           2 rentista
           3 asistencia a centro de estudios
           4 obligaciones del propio hogar
           5 con discapacidad o enfermedad
           6 otro motivo

*/


gen categoinac_ci = 1 if (raznoacteco == 1 & condocup_ci==3)
replace categoinac_ci = 2 if (raznoacteco == 3 & condocup_ci==3)
replace categoinac_ci = 3 if (raznoacteco == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"



*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

gen byte formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

					**************
					***INGRESOS***
					**************
					
*foreach v of varlist d13b d13c d13d d13e d12 d13a d13c1 d14a d16* d17 c13f d15a d17 d14* d15* e12b e13d1 e15a e11b e14a1 e14b1 e14c1 e14d1 e13a1 e13b1 e13c1 e13e1 f8a f8b f10a f10* f11* h9a1 h9b1 h9c1 h9d1 h9e1 h9f1 h9g1 h9h1 h9i1 h9j1 h9k1 h9l1 h9m1 h9o1 h10c {
foreach v of varlist _all{
capture recode `v' (99999999=.) 
*sum `v'
*return list
}  
***************
***ylmpri_ci***
***************
/*
gen ganancia=c13g  			if c13g1==1
replace ganancia=c13g/2  	if c13g1==2
replace ganancia=c13g/3  	if c13g1==3
replace ganancia=c13g/4  	if c13g1==4
replace ganancia=c13g/6  	if c13g1==6
replace ganancia=c13g/12  	if c13g1==8
replace ganancia=.		  	if c13g1==9

gen salario=e12a

egen ylmpri_ci=rsum(ganancia salario), missing
replace ylmpri_ci=. if ganancia==. & salario==.
replace ylmpri_ci=0 if categopri_ci==4
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=. if ylmpri_ci==1.00e+08
*/



*Se utilizan las variables que vienen en la base
*Modificación Mayra Sáenz - Abril 2014
*Se replica la sintaxis del instituto de estadística


g inmpif=.
replace inmpif=d13e if (d13d==1 & (01110<=c8<=01300 | 0150<=c8<=05002 | 10101<=c8<=22219) | 23100<=c8<=37203 | 40100<=c8<=40300 | 50101<=c8<=50102 | 50301<=c8<=50402 | 50500<=c8<=51900 | 52000<=c8<=52590 | 55201<=c8<=55300 | c8==0) & (c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==1)
 

/*g inmpia =.
replace inmpia =d16d/d16d1 if 0<d16d1<8 & d12==3 & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==3 & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace inmpia=d16d/12 if d16d1==8 & d12==3  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==3 & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace inmpia=. if (d16d==99999999 | d16d1== 9) & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==3 & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace inmpia=d16d if (d16d==0 & d16d1== .)  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==3 & (01110<=c8<=01300 | 01501<=c8<=05002)))

g inmpina=.
replace inmpina=d15c if d15a==1 & d15b==1 & (c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==2 & ((10101<=c8<=22219 | 23100<=c8<=37203 | 40100<=c8<=40300 | 50101<=c8<=50102 | 50301<=c8<=50402 | 50500<=c8<=51900 | 2000<=c8<=52590 | 55201<=c8<=55300)))

g spif=.
replace spif=d13b if d13a==1 & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & d12==1))

g gpina=.
replace gpina=d14a if d12==2 & d14a>0 & ((c12==1 | ((c12==4 | 5) & c13d==2) & (10101<=c8<=22219 | 23100<=c8<=37203 | 40100<=c8<=40300 | 50101<=c8<=50102 | 50301<=c8<=50402 | 50500<=c8<=51900 | 52000<=c8<=52590 | 55201<=c8<=55300 | 01401<=c8<=01403 | 22220<=c8<=22302 | 41000<=c8<=45100) | 45201<=c8<=45302 | 45400<=c8<=45500 | 50201<=c8<=50202 | c8==50403 | c8==51910 | 52601<=c8<=52606 | c8==55101 | c8==55102 | c8==55000 | 60100<=c8<=99000 | c8==0) )
replace gpina=d17 if d12==2 & (gpina==99999999 | gpina==0 | gpina==.) & d17~= .  & ((c12==1 | ((c12==4 | 5) & c13d==2) & (10101<=c8<=22219 | 23100<=c8<=37203 | 40100<=c8<=40300 | 50101<=c8<=50102 | 50301<=c8<=50402 | 50500<=c8<=51900 | 52000<=c8<=52590 | 55201<=c8<=55300 | 01401<=c8<=01403 | 22220<=c8<=22302 | 41000<=c8<=45100) | 45201<=c8<=45302 | 45400<=c8<=45500 | 50201<=c8<=50202 | c8==50403 | c8==51910 | 52601<=c8<=52606 | c8==55101 | c8==55102 | c8==55000 | 60100<=c8<=99000 | c8==0) )
replace gpina=d14a if d12==2 & d14a==99999999 & d17==0  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (10101<=c8<=22219 | 23100<=c8<=37203 | 40100<=c8<=40300 | 50101<=c8<=50102 | 50301<=c8<=50402 | 50500<=c8<=51900 | 52000<=c8<=52590 | 55201<=c8<=55300 | 01401<=c8<=01403 | 22220<=c8<=22302 | 41000<=c8<=45100) | 45201<=c8<=45302 | 45400<=c8<=45500 | 50201<=c8<=50202 | c8==50403 | c8==51910 | 52601<=c8<=52606 | c8==55101 | c8==55102 | c8==55000 | 60100<=c8<=99000 | c8==0) )
replace gpina=d18 if d3==1 | d3==2 & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (10101<=c8<=22219 | 23100<=c8<=37203 | 40100<=c8<=40300 | 50101<=c8<=50102 | 50301<=c8<=50402 | 50500<=c8<=51900 | 52000<=c8<=52590 | 55201<=c8<=55300 | 01401<=c8<=01403 | 22220<=c8<=22302 | 41000<=c8<=45100) | 45201<=c8<=45302 | 45400<=c8<=45500 | 50201<=c8<=50202 | c8==50403 | c8==51910 | 52601<=c8<=52606 | c8==55101 | c8==55102 | c8==55000 | 60100<=c8<=99000 | c8==0) )


g gpsfi =.
replace gpsfi=c13g/c13g1 if 0< c13g1 <8  & (c12==4 & c13a==2 & c13b==1 & c13c==1 & c13d==1)
replace gpsfi=c13g/12 if c13g1==8 & (c12==4 & c13a==2 & c13b==1 & c13c==1 & c13d==1)
replace gpsfi = . if c13g==99999999 | c13g1==9  & (c12==4 & c13a==2 & c13b==1 & c13c==1 & c13d==1)


g gpia=.
replace gpia=d16a/d16a1 if d12==3 & 0<d16a1<8  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace gpia=d16a/12    if d12==3 & d16a1==8  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace gpia=d16a       if d12==3 & (d16a==0 & d16a1== .) & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace gpia=. if d12==3 & (d16a==99999999 | d16a1==9)  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace gpia=d17 if d12==3 & (gpia==99999999 | gpia==. | gpia==0) & d17~= .  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (01110<=c8<=01300 | 01501<=c8<=05002)))
replace gpia=d18 if d3==1 | d3==2  & ((c12==1 | ((c12==4 | c12==5) & c13d==2) & (01110<=c8<=01300 | 01501<=c8<=05002)))

g gpif=.
replace gpif=d13c/d13c1 if 0<d13c1<8  & ((c12==1 | ((c12==4 | 5) & c13d==2) & d12==1 ))
replace gpif=d13c/12    if d13c1==8  & ((c12==1 | ((c12==4 | 5) & c13d==2) & d12==1 ))
replace gpif=. if (d13c==99999999 | d13c1==9)  & ((c12==1 | ((c12==4 | 5) & c13d==2) & d12==1 ))
replace gpif=d13c if (d13c==0 | d13c1== .)  & ((c12==1 | ((c12==4 | 5) & c13d==2) & d12==1 ))
replace gpif=d17 if (gpif==99999999 | gpif== 0 | gpif==.) & d17 ~= .  & ((c12==1 | ((c12==4 | 5) & c13d==2) & d12==1 ))
replace gpif=. if (d13c==99999999 | d13c1==9) & d17==0 & ((c12==1 | ((c12==4 | 5) & c13d==2) & d12==1 ))
*/

/*
g spmn_b =.
replace spmn_b = e12a*0.9083 if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==3 & e12a<=619000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a*0.8083+61900  if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==3 & 619000<e12a<=929000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a*0.7583+108400 if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==3 & e12a>929000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a*0.9083 if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==4  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==3 & e12a<=619000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a*0.9+61900 if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==3 & 619000<e12a<=929000 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b =e12a*0.85+108400 if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==3 & e12a>929000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==4 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a if 1<e12a<99999999 & e12b==2 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a+e12c if 1<e12a<99999999 & e12b==3 & e10c==5  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a if e12a==99999999 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = . if e12c==99999999 & e12b==3  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a*0.9083 if e12a==0 & e10a==1 & e10b==3 & e15a<=619000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a*0.8083+61900  if e12a==0 & e10a==1 & e10b==3 & 619000<e15a<=929000 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a*0.7583+108400 if e12a==0 & e10a==1 & e10b==3 & e15a>929000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a*0.9083 if e12a==0 & e10a==1 & e10b==4 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a if e12a==0 & e10a==2 & e10b==3 & e15a<=619000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a*0.9+61900 if e12a==0 & e10a==2 & e10b==3 & 619000<e15a<=929000  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a*0.85+108400 if e12a==0 & e10a==2 & e10b==3 & e15a>929000 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a if e12a==0 & e10a==2 & e10b==4  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = . if e12a==0 & e15a==99999999  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15b/e15b1 if (e12a==1 | e8a==1) & 0<e15b1<8  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15b/12 if (e12a==1 | e8a==1) & e15b1==8  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = . if (e12a==1 | e8a==1) & (e15b==99999999 | e15b1==9)  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
*/


/*
* sin multiplicar por el coeficiente de subdeclaración
g spmn_b =.
replace spmn_b = e12a               if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==3 & e12a!=. &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a               if 1<e12a<99999999 & e12b==1 & e10a==1 & e10b==4                        &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a               if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==3 & e12a!=. &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a               if 1<e12a<99999999 & e12b==1 & e10a==2 & e10b==4                        &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a               if 1<e12a<99999999 & e12b==2                                            &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a+e12c          if 1<e12a<99999999 & e12b==3 & e10c==5                                  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e12a               if e12a==99999999                                                       &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = .                  if e12c==99999999  & e12b==3                                            &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a               if e12a==0 & e10a==1 & e10b==3 & e15a!=. &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a               if e12a==0 & e10a==1 & e10b==4 &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a               if e12a==0 & e10a==2 & e10b==3 & e15a!=.  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15a               if e12a==0 & e10a==2 & e10b==4  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = .                  if e12a==0 & e15a==99999999  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15b/e15b1         if (e12a==1 | e8a==1) & 0<e15b1<8  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = e15b/12            if (e12a==1 | e8a==1) & e15b1==8  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace spmn_b = .                  if (e12a==1 | e8a==1) & (e15b==99999999 | e15b1==9)  &((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )


g ipsp =.
replace ipsp =e11b if e11a==1 & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))  

g asp =.
replace asp= e14a1/12 if e14a==1 & e14a2==8  & ( (c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace asp = e14a1/e14a2 if e14a==1 & e14a2<8  & ( (c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace asp = . if e14a==1 & (e14a2==9 | e14a1==99999999)  & ( (c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
g sesp =.
replace sesp= e14b1/12 if e14b==3 & e14b2==8 & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))
replace sesp = e14b1/e14b2 if e14b==3 & e14b2<8  & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))
replace sesp = . if e14b==3 & (e14b2==9 | e14b1==99999999)  & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))
g bsp =.
replace bsp = e14c1/12 if e14c==5 & e14c2==8 & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))
replace bsp = e14c1/e14c2 if e14c==5 & e14c2<8  & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))
replace bsp = . if e14c==5 & (e14c2==9 | e14c1==99999999) & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))
g oisp=.
replace oisp= e14d1/12 if e14d==7 & e14d2==8  & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace oisp = e14d1/e14d2 if e14d==7 & e14d2<8  & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
replace oisp = . if e14d==7 & (e14d2==9 | e14d1==99999999)  & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1) )
g spnma=. 
replace spnma = e13a1 if e13a==1 & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))  
g spnmv =.
replace spnmv = e13b1 if e13b==3 & ((c12==2 | c12==3) | ((c12==4 | 5) & c13a==1))

g spnmt=. 
replace spnmt = e13c1 if e13c==5 & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))  
g spnmvh =.
replace spnmvh = e13d1 if e13d ==7 & ((c12==2 | c12==3) | ((c12==4 | c12==5) & c13a==1))  
g spnmo =.
replace spnmo = e13e1 if e13e==1 & ((c12==2 | c12==3) | ((c12==4 | 5) & c13a==1))  

g gsi =. 
replace gsi= f6a/f6a1 if 0<f6a1<8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace gsi= f6a/12   if f6a1==8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace gsi= . if (f6a1==9 | f6a==99999999)  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace gsi =0 if (f6a1== . & f6a==0)  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace gsi= f6a/f6a1 if 0<f6a1<8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
replace gsi= f6a/12   if f6a1==8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
replace gsi= . if (f6a1==9 | f6a==99999999)  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
replace gsi =0 if (f6a1== . & f6a==0)  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))


g inmsi =.
replace inmsi = f6b/f6b1 if 0<f6b1<8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace inmsi = f6b/12   if f6b1==8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace inmsi = . if (f6b==99999999 | f6b1==9)  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace inmsi = 0 if (f6b==0 & f6b1== .) & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2)))
replace inmsi = f6b/f6b1 if 0<f6b1<8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
replace inmsi = f6b/12   if f6b1==8  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
replace inmsi = . if (f6b==99999999 | f6b1==9)  & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
replace inmsi = 0 if (f6b==0 & f6b1== .) & ((c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20== . & e17== .)))
*/


/*
g ssmn=.
replace ssmn = f8a*0.9083 if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a==1 & f7b ==3 & f8a <=619000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a*0.8083+61900 if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a==1 & f7b==3 & 619000 < f8a <= 929000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a*0.7583+108400 if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==1 & f7b ==3 & f8a > 929000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a*0.9083 if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==1 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==2 & f7b ==3 & f8a <=619000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a*0.9+61900 if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==2 & f7b ==3 & 619000 < f8a <= 929000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a*0.85+108400 if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==2 & f7b ==3 & f8a> 929000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==2 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==2  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a+f8c if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==3 & f7c==5  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = 99999999 if (f5==3 | f5==4) & f8a ==99999999 | (f8c == 99999999 & f8b ==3) | (f9b==99999999 & f9a==1)   & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a*0.9083 if (f5==3 | f5==4) & f8a ==0 & f7a ==1 & f7b ==3 & f8a <=619000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a*0.8083+61900  if (f5==3 | f5==4) & f8a ==0 & f7a ==1 & f7b ==3 & 619000 < f8a <=929000 & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a*0.7583+108400 if (f5==3 | f5==4) & f8a ==0 & f7a ==1 & f7b ==3 & f8a > 929000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a*0.9083 if (f5==3 | f5==4) & f8a ==0 & f7a ==1 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a if (f5==3 | f5==4) & f8a ==0 & f7a ==2 & f7b ==3 & f8a <=619000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a*0.9+61900 if (f5==3 | f5==4) & f8a ==0 & f7a ==2 & f7b ==3 & 619000 < f8a <=929000 & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a*0.85+108400 if (f5==3 | f5==4) & f8a ==0 & f7a ==2 & f7b ==3 & f8a > 929000  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a if (f5==3 | f5==4) & f8a ==0 & f7a ==2 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = 99999999 if (f5==3 | f5==4) & f8a ==0 & f10a == 99999999  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10b/f10b1 if (f8a ==1|(f4==4 | f4==5)) & f10b>0 & f10b1 <8  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10b/12 if (f8a ==1|(f4==4 | f4==5)) & f10b1==8  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = 99999999 if (f8a ==1|(f4==4 | f4==5)) & (f10b1==9 | f10b ==99999999) & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
*/


/*
*Sin considerar el coeficiente de subdeclaración
g ssmn=.
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a==1 & f7b ==3 & f8a !=.  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==1 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==2 & f7b ==3 & f8a !=.  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==1 & f7a ==2 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==2  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f8a+f8c if (f5==3 | f5==4) & (f8a>2 & f8a<99999999)& f8b==3 & f7c==5  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = 99999999 if (f5==3 | f5==4) & f8a ==99999999 | (f8c == 99999999 & f8b ==3) | (f9b==99999999 & f9a==1)   & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a if (f5==3 | f5==4) & f8a ==0 & f7a ==1 & f7b ==3 & f8a !=.  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a if (f5==3 | f5==4) & f8a ==0 & f7a ==1 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a if (f5==3 | f5==4) & f8a ==0 & f7a ==2 & f7b ==3 & f8a !=.  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10a if (f5==3 | f5==4) & f8a ==0 & f7a ==2 & f7b ==4  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = 99999999 if (f5==3 | f5==4) & f8a ==0 & f10a == 99999999  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10b/f10b1 if (f8a ==1|(f4==4 | f4==5)) & f10b>0 & f10b1 <8  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = f10b/12 if (f8a ==1|(f4==4 | f4==5)) & f10b1==8  & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))
replace ssmn = 99999999 if (f8a ==1|(f4==4 | f4==5)) & (f10b1==9 | f10b ==99999999) & (c1==2 & (d20==1 | e17==1) & (f5==1 | f5==2) | (c1==2 & d20==. & e17== .))

g ssnm =.
replace ssnm = f9b if f9a==1 & (f5==3 | f5==4) & ((c1==2 & (d20==1 | e17==1)) | (c1==2 & d20== . & e17== .)) 

/* Ya constan en la base original
g irp = .
replace irp = rsum(ia ii id ib), missing
g ia= .
replace ia= h9a1/h9a2 if h9a==1 & 0 < h9a2 <8 & a5> 14 
replace ia= h9a1/12 if h9a==1 & h9a2==8 & a5> 14  
replace ia =.   if h9a==1 & h9a1==99999999 & a5> 14  

g ii=.
replace ii= h9b1/h9b2 if h9b==1 & 0 < h9b2 <8 & a5> 14  
replace ii= h9b1/12 if h9b==1 & h9b2==8 & a5> 14  
replace ii =. if h9b==1 & h9b1==99999999 & a5> 14  

g id=.
replace id = h9c1/h9c2 if h9c==1 & 0 < h9c2 <8 & a5> 14  
replace id = h9c1/12 if h9c==1 & h9c2==8 & a5> 14  
replace id =. if h9c==1 & h9c1==99999999 & a5> 14  

g ib = .
replace ib= h9d1/h9d2 if h9d==1 & 0 < h9d2 <8 & a5> 14  
replace ib= h9d1/12 if h9d==1 & h9d2==8 & a5> 14  
replace ib =. if h9d==1 & h9d1==99999999 & a5> 14  

g trnc=.
replace trnc= h9e1/h9e2 if h9e==1 & 0 < h9e2 <8 & a5> 14  
replace trnc= h9e1/12 if h9e==1 & h9e2==8 & a5> 14 
replace trnc =. if h9e==1 & h9e1==99999999 & a5> 14  
 
g timas=.
replace timas= h9f1/h9f2 if h9f==1 & 0 < h9f2 <8 & a5> 14  
replace timas= h9f1/12 if h9f==1 & h9f2==8 & a5> 14 
replace timas= . if h9f==1 & h9f1==99999999 & a5> 14 

g ts=.
replace ts=h9g1/h9g2 if h9g==1 & 0 < h9g2 <8 & a5> 14  
replace ts= h9g1/12 if h9g==1 & h9g2==8 & a5> 14 
replace ts =. if h9g==1 & h9g1==99999999 & a5> 14 

g tbc=.
replace tbc= h9h1/h9h2 if h9h==1 & 0 < h9h2 <8 & a5> 14  
replace tbc= h9h1/12 if h9h==1 & h9h2==8 & a5> 14 
replace tbc =. if h9h==1 & h9h1==99999999 & a5> 14  
 
g tpa=.
replace tpa= h9i1/h9i2 if h9i==1 & 0 < h9i2 <8 & a5> 14 
replace tpa= h9i1/12 if h9i==1 & h9i2==8 & a5> 14  
replace tpa =. if h9i==1 & h9i1==99999999 & a5> 14 

g tpn=.
replace tpn=h9j1/h9j2 if h9j==1 & 0 < h9j2 <8 & a5> 14  
replace tpn=h9j1/12 if h9j==1 & h9j2==8 & a5> 14  
replace tpn=tpn =.if h9j==1 & h9j1==99999999 & a5> 14  
 
g tpe=.
replace tpe = h9k1/h9k2 if h9k==1 & 0 < h9k2 <8 & a5> 14  
replace tpe = h9k1/12  if h9k==1 & h9k2==8 & a5> 14 
replace tpe = . if h9k==1 & h9k1==99999999 & a5> 14  
 
g tap =.
replace tap= h9l1/h9l2 if h9l==1 & 0 < h9l2 <8 & a5> 14  
replace tap =h9l1/12   if h9l==1 & h9l2==8 & a5> 14  
replace tap =. if h9l==1 & h9l1==99999999 & a5> 14  

g te=.
replace te= h9m1/h9m2 if h9m==1 & 0 < h9m2 <8 & a5> 14  
replace te = h9m1/12 if h9m==1 & h9m2==8 & a5> 14 
replace te = . if h9m==1 & h9m1==99999999 & a5> 14  
 
g tdp =.
replace tdp =h9n1/h9n2 if h9n==1 & 0 < h9n2 <8 & a5> 14  
replace tdp= h9n1/12 if h9n==1 & h9n2==8 & a5> 14  
replace tdp =. if h9n==1 & h9n1==99999999 & a5> 14  

g ot =.
replace ot= h9o1/h9o2 if h9o==1 & 0 < h9o2 <8 & a5> 14  
replace ot =h9o1/12   if h9o==1 & h9o2==8 & a5> 14  
replace ot = . if h9o==1 & h9o1==99999999 & a5> 14  

g tnm=.
replace tnm= h10c/h10c1 if h10a==1 & h10c1>0 & h10c1<8 & linea==1  
replace tnm = h10c/12 if h10a==1 & h10c1==8 & linea==1  
replace tnm =. if h10a==1 & h10c==99999999 & linea==1 
*/

egen ylmpri_ci=  rsum(spif gpif gpina gpsfi gpia gpif spmn_b ipsp asp sesp bsp oisp), missing
replace ylmpri_ci= . if ithn == 99999999 
replace ylmpri_ci= 0 if ithn == 0
label var  ylmpri_ci "Ingreso laboral monetario actividad principal"
*/

egen ingnomonet= rsum(inmpif inmpina inmpia spnma spnmv spnmt spnmvh spnmo), missing
replace ingnomonet = ingnomonet*-1

egen ylmpri_ci = rsum(ipnt ingnomonet), missing
replace ylmpri_ci = . if ipnt ==.
sum ylmpri_ci
label var  ylmpri_ci "Ingreso laboral monetario actividad principal"

****************
***ylnmpri_ci***
****************
/*for var e13a1 e13b1 e13c1 e13d1 e13e1 e14a1 e14b1 e14c1 e14d1: replace X=. if X==99999999

gen temp1=e14a1  		if e14a2==1
replace temp1=e14a1/2  	if e14a2==2
replace temp1=e14a1/3  	if e14a2==3
replace temp1=e14a1/4  	if e14a2==4
replace temp1=e14a1/6  	if e14a2==6
replace temp1=e14a1/12 	if e14a2==8
replace temp1=.		  	if e14a2==9

gen temp2=e14b1  		if e14b2==1
replace temp2=e14b1/2  	if e14b2==2
replace temp2=e14b1/3  	if e14b2==3
replace temp2=e14b1/4  	if e14b2==4
replace temp2=e14b1/6  	if e14b2==6
replace temp2=e14b1/12 	if e14b2==8
replace temp2=.		  	if e14b2==9

gen temp3=e14c1  		if e14c2==1
replace temp3=e14c1/2  	if e14c2==2
replace temp3=e14c1/3  	if e14c2==3
replace temp3=e14c1/4  	if e14c2==4
replace temp3=e14c1/6  	if e14c2==6
replace temp3=e14c1/12 	if e14c2==8
replace temp3=.		  	if e14c2==9

gen temp4=e14d1  		if e14d2==1
replace temp4=e14d1/2  	if e14d2==2
replace temp4=e14d1/3  	if e14d2==3
replace temp4=e14d1/4  	if e14d2==4
replace temp4=e14d1/6  	if e14d2==6
replace temp4=e14d1/12 	if e14d2==8
replace temp4=.		  	if e14d2==9

egen ylnmpri_ci=rsum(e13a1 e13b1 e13c1 e13d1 e13e1 temp1 temp2 temp3 temp4), missing
egen perd=rowmiss(e13a1 e13b1 e13c1 e13d1 e13e1 temp1 temp2 temp3 temp4)
replace ylnmpri_ci=. if perd==9
drop temp* perd
*/
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
egen ylnmpri_ci= rsum(inmpif inmpina inmpia spnma spnmv spnmt spnmvh spnmo), missing
replace ylnmpri_ci= . if ipnt == .
label var ylnmpri "Ingreso laboral no monetario act. principal"



*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=.
***********************************************************************************************
**TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

*********************************************************************************
**TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
*********************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"



***************
***ylmsec_ci***
***************
/*gen ganancia2=f6a	  		if f6a1==1
replace ganancia2=f6a/2  	if f6a1==2
replace ganancia2=f6a/3  	if f6a1==3
replace ganancia2=f6a/4  	if f6a1==4
replace ganancia2=f6a/6  	if f6a1==6
replace ganancia2=f6a/12  	if f6a1==8
replace ganancia2=.		  	if f6a1==9

gen salario2=f8a

egen ylmsec_ci=rsum(ganancia2 salario2), missing
replace ylmsec_ci=. if ganancia2==. & salario2==.
replace ylmsec_ci=0 if categosec_ci==4
replace ylmsec_ci=. if emp_ci==0
replace ylmsec_ci=. if ylmsec_ci==1.00e+08
*/
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
egen ingnomonets= rsum(inmsi ssnm), missing
replace ingnomonets = ingnomonets*-1

egen ylmsec_ci = rsum(isnt ingnomonets), missing
replace ylmsec_ci = . if isnt ==.
sum ylmsec_ci

*egen ylmsec_ci= rsum(gsi ssmn), missing
*replace ylmsec_ci= . if ithn == 99999999 
*replace ylmsec_ci= 0 if ithn == 0
*label var ylmsec_ci "Ingreso laboral monetario actividad secundaria"


****************
***ylnmsec_ci***
****************
*gen ylnmsec_ci=f9b
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
egen ylnmsec_ci= rsum(inmsi ssnm), missing
replace ylnmsec_ci= . if ithn == 99999999 
replace ylnmsec_ci= 0 if ithn == 0
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"

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

gen ylmotros_ci=.
gen ylnmotros_ci=.

*************
***ynlm_ci***
*************
*Para este año se dispone de información sobre: alquileres, intereses, dividendos, beneficios de cooperativas,
*pensiones, IMAS, subsidios, becas, pensión alimenticia, jubilaciones nacionales, jubilaciones internacionales, 
*aguinaldos, remesas, transferencias y otras transferencias.
/*
*Montos:
ren h9a1 h9a3
ren h9b1 h9a4
ren h9c1 h9a5
ren h9d1 h9a6
ren h9e1 h9a7
ren h9f1 h9a8
ren h9g1 h9a9
ren h9h1 h9a10
ren h9i1 h9a11
ren h9j1 h9a12
ren h9k1 h9a13
ren h9l1 h9a14
ren h9m1 h9a15
ren h9n1 h9a16
ren h9o1 h9a17

*Frecuencias
ren h9a2 h9b3
ren h9b2 h9b4
ren h9c2 h9b5
ren h9d2 h9b6
ren h9e2 h9b7
ren h9f2 h9b8
ren h9g2 h9b9
ren h9h2 h9b10
ren h9i2 h9b11
ren h9j2 h9b12
ren h9k2 h9b13
ren h9l2 h9b14
ren h9m2 h9b15
ren h9n2 h9b16
ren h9o2 h9b17


forvalues var=3(1)17{
	recode h9a`var' (99999999=.)
	gen ing_`var'=h9a`var'	 		if h9b`var'==1
	replace ing_`var'=h9a`var'/2 	if h9b`var'==2
	replace ing_`var'=h9a`var'/3 	if h9b`var'==3
	replace ing_`var'=h9a`var'/4 	if h9b`var'==4
	replace ing_`var'=h9a`var'/6 	if h9b`var'==6
	replace ing_`var'=h9a`var'/12 	if h9b`var'==8
	replace ing_`var'=.			 	if h9b`var'==9
		}	

egen ynlm_ci=rsum(ing_3-ing_17), missing
*/
*Modificación Mayra Sáenz - Abril 2014
*Se da preferencia al agregado oficial 
*Ingreso por renta de la propiedad, transferencias monetarias
egen ynlm_ci=rsum(irp ttm), missing
replace ynlm_ci= . if ithn == 99999999 
replace ynlm_ci= 0 if ithn == 0
label var ynlm_ci "Ingreso no laboral monetario (otras fuentes)"
**************
***ynlnm_ci***
**************
*gen ynlnm_ci=.
**Modificación Mayra Sáenz - Abril 2014
*Transferencias no monetarias.
gen ynlnm_ci=tnm
replace ynlnm_ci= . if ithn == 99999999 
replace ynlnm_ci= 0 if ithn == 0
label var ynlnm_ci "Ingreso no laboral no monetario"

****************
***remesas_ci***
****************
*gen remesas_ci=ing_15
*drop ing_3-ing_15
*Modificación Mayra Sáenz - Abril 2014
*Transferencias del extranjero
g remesas_ci= te
replace remesas_ci= . if ithn == 99999999 
replace remesas_ci= 0 if ithn == 0
label var remesas_ci "Remesas reportadas por el individuo"

*******************
*** nrylmpri_ch ***
*******************
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
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

***************




*** ylnm_ch ***
***************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

****************
***remesas_ch***
****************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing

* N/A*
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"
gen autocons_ch=.
label var autocons_ch "Autoconsumo del Hogar"

***********************************************************
***AUTOCONS_CI : Autoconsumo reportado por el individuo.
************************************************************
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"


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

gen aedu_ci=.
replace aedu_ci=0 if a15==0 | a15==1 

*Primaria
replace aedu_ci=1 if a15==11 
replace aedu_ci=2 if a15==12
replace aedu_ci=3 if a15==13
replace aedu_ci=4 if a15==14
replace aedu_ci=5 if a15==15
replace aedu_ci=6 if a15==16

*Secundaria (académica y técnica)
replace aedu_ci=7 if a15==21 | a15==31
replace aedu_ci=8 if a15==22 | a15==32
replace aedu_ci=9 if a15==23 | a15==33
replace aedu_ci=10 if a15==24 | a15==34
replace aedu_ci=11 if a15==25 | a15==26 | a15==35  
replace aedu_ci=12 if a15==36 | a15==37

*Superior (universitario o para-universitario)
replace aedu_ci=13 if a15==41 | a15==51
replace aedu_ci=14 if a15==42 | a15==52
replace aedu_ci=15 if a15==43 | a15==53
replace aedu_ci=16 if a15==54 
replace aedu_ci=17 if a15==55
replace aedu_ci=18 if a15==56

*Postgrado
replace aedu_ci=19 if a15==71 | a15==81
replace aedu_ci=20 if a15==72 | a15==82
replace aedu_ci=21 if a15==73 | a15==83
replace aedu_ci=22 if a15==74 | a15==84

**************
***eduno_ci***
**************
/*gen eduno_ci=0
replace eduno_ci=1 if a15==0 | a15==1 
label variable eduno_ci "Cero anios de educacion"*/

gen eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
label variable eduno_ci "Cero anios de educacion"


**************
***edupi_ci***
**************
/*gen edupi_ci=0
replace edupi_ci=1 if (a15>=11 & a15<16)
label variable edupi_ci "Primaria incompleta"*/

gen edupi_ci=0
replace edupi_ci=1 if aedu_ci>=1 & aedu_ci<6
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
/*gen edupc_ci=0
replace edupc_ci=1 if a15==16
label variable edupc_ci "Primaria completa"*/

gen edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
/*gen edusi_ci=0
replace edusi_ci=1 if (a15>=21 & a15<=26) 
replace edusi_ci=1 if (a15>=31 & a15<=35) 
label variable edusi_ci "Secundaria incompleta"*/

gen edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<11
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
/*gen edusc_ci=0
replace edusc_ci=1 if  a15==36 | a15==37 
label variable edusc_ci "Secundaria completa"*/

gen edusc_ci=0
replace edusc_ci=1 if  (aedu_ci==11 | aedu_ci==12) & a15!=35
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************
gen edus1i_ci=0
replace edus1i_ci=1 if (a15>=21 & a15<=22)
replace edus1i_ci=1 if (a15>=31 & a15<=32)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"


***************
***edus1c_ci***
***************
gen edus1c_ci=0
replace edus1c_ci=1 if a15==23 | a15==33
label variable edus1c_ci "1er ciclo de la secundaria completo"


***************
***edus2i_ci***
***************
gen edus2i_ci=0
replace edus2i_ci=1 if (a15==24 | a15 == 25 | a15 == 26)
replace edus2i_ci=1 if (a15>=34 & a15<=35)
label variable edus2i_ci "2do ciclo de la secundaria incompleto"


***************
***edus2c_ci***
***************
gen edus2c_ci=0
replace edus2c_ci=1 if a15==36 | a15==37
label variable edus2c_ci "2do ciclo de la secundaria completo"


**************
***eduui_ci***
**************
gen eduui_ci=0
replace eduui_ci=1 if (a15>=41 & a15<=43)
replace eduui_ci=1 if (a15>=51 & a15<=54)
label variable eduui_ci "Superior incompleto"


***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if a15>=55
label variable eduuc_ci "Superior completo"

*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if a15>=41 & a15<=43
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************
gen edupre_ci=.
replace edupre_ci=(a15==1)
label variable edupre_ci "Educacion preescolar"

****************
***asispre_ci***
****************
*Variable agregada por Iván Bornacelly - 01/16/2017
	g asispre_ci=.
	replace asispre_ci=1 if (a14==1 | a14==2) & a5>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"
	
**************
***eduac_ci***
**************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
gen asiste_ci=.
replace asiste_ci=1 if a14>=1 & a14<=8
replace asiste_ci=0 if a14==0
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************
gen pqnoasis_ci=a18
label define pqnoasis_ci 1  "tiene que trabajar" 2  "prefiere trabajar" 3  "tiene que cuidar niños, ancianos u otras personas" ///
4  "tiene que ayudar en oficios domésticos  " 5  "no puede pagar los estudios  " 6  "problemas de acceso al sistema escolar" ///
7  "le cuesta el estudio" 8  "no está interesado en el aprendizaje formal" 9  "embarazo o matrimonio" 10  "enfermedad o discapacidad" ///
11  "no tiene edad" 12  "falta ganar pruebas del mep; exámenes de admisión.  " 13  "otro" 99  "ignorado"

label value pqnoasis_ci pqnoasis_ci
label variable pqnoasis_ci  " Razón por que no asiste a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if a18==5
replace pqnoasis1_ci = 2 if a18==1
replace pqnoasis1_ci = 3 if a18==7 | a18==10
replace pqnoasis1_ci = 4 if a18==2 | a18==8
replace pqnoasis1_ci = 5 if a18==3 | a18==4 | a18==9
replace pqnoasis1_ci = 7 if a18==11 
replace pqnoasis1_ci = 8 if a18==6
replace pqnoasis1_ci = 9 if a18==12 | a18==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un grado o año"

******************************************************************
***18._REPITEULT_CI : Personas que han repetido el ultimo grado.
******************************************************************

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el último grado"
***************
***edupub_ci***
***************
gen edupub_ci=0
replace edupub_ci=1 if a16==1 
replace edupub_ci=. if a16==.
label var edupub_ci "Personas asisten a centros de enseñanza públicos"



						**********************************
						**** VARIABLES DE LA VIVIENDA ****
						**********************************

****************
***aguared_ch***
****************
gen aguared_ch=.
replace aguared_ch=1 if v11>=1 & v11<=3
replace aguared_ch=0 if v11==0

*****************
***aguadist_ch***
*****************
gen aguadist_ch=.
replace aguadist_ch=1 if v11==1
replace aguadist_ch=2 if v11==2
replace aguadist_ch=3 if v11==3

*****************
***aguamala_ch***
*****************
/*
***NOTA***
 ____________________________________________________________________________________________________________________________
| Toma el valor de 1 cuando la principal fuente de agua es "unimproved" y 0 caso contrario, por lo cual asigna 1             |
| cuando la pregunta v11 es igual a 6 o 7, pero la v11 sólo toma valores de 0,1,2,3 y 9.                                     |
| Debería tomar el valor de 1 cuando v11 es igual a 0.                                                                       |
|____________________________________________________________________________________________________________________________|
*/


gen aguamala_ch=.
replace aguamala_ch=0 if v11>=1 & v11<=5
replace aguamala_ch=1 if v11==0
label var aguamala_ch "Principal fuente de agua es unimproved"


*****************
***aguamide_ch***
*****************
gen aguamide_ch=.

************
***luz_ch***
************
gen luz_ch=.
replace luz_ch=1 if v15>=1 & v15<=5
replace luz_ch=0 if v15==0| v15==6 

****************
***luzmide_ch***
****************
gen luzmide_ch=.

****************
***combust_ch***
****************
gen combust_ch=.
replace combust_ch=1 if  v16==1 | v16==2
replace combust_ch=0 if  v16==3 | v16==4

*************
***bano_ch***
*************
gen bano_ch=.
replace bano_ch=1 if v14a==1
replace bano_ch=0 if v14a==2

***************
***banoex_ch***
***************
gen banoex_ch=.
replace banoex_ch=1 if v14b==1
replace banoex_ch=0 if v14b==2

*************
***des1_ch***
*************
gen des1_ch=.
replace des1_ch=0 if v13a==0
replace des1_ch=1 if v13a==1 |v13a==2 |v13a==3
replace des1_ch=2 if v13a==4 

*************
***des2_ch***
*************

***NOTA***
/*
____________________________________________________________________________________________________________________________
|Según la definición de variables del sociómetro, esta variable debería asignar el valor de 2 sólo cuando el hogar tiene    |
|'otro tipo de desague'. Sin embargo, asigna 2 cuando tiene 'otro tipo de desague' Y cuando tiene 'pozo ciego o letrina',   |
| lo cual,debería estar con el código 1.                                                                                    | 
|___________________________________________________________________________________________________________________________|
*/
gen des2_ch=0 if v13a==0
replace des2_ch =1 if v13a==1 |v13a==2 |v13a==3 | v13a==4 
replace des2_ch=2 if v13a==5 
label var des2_ch "Tipo de desague sin incluir unimproved"



*************
***piso_ch***
*************

* Modificaciones Marcela Rubio Septiembre 2014

/*
gen piso_ch=.
replace piso_ch=0 if v6==5
replace piso_ch=1 if v6>=1 & v6<=3
replace piso_ch=2 if v6==4 | v6 ==5 
label define piso_ch 0 "Piso de tierra" 1 "Materiales permanentes" 2 "Otros materiales"
label value piso_ch piso_ch
label var piso_ch "Materiales de construcción del piso"
*/

gen piso_ch=.
replace piso_ch=0 if v6==0
replace piso_ch=1 if v6>=1 & v6<=3
replace piso_ch=2 if v6==4 | v6 ==5 
label define piso_ch 0 "Piso de tierra" 1 "Materiales permanentes" 2 "Otros materiales"
label value piso_ch piso_ch
label var piso_ch "Materiales de construcción del piso"

**************
***pared_ch***
**************
gen pared_ch=.
replace pared_ch=0 if v3==7 |v3==0
replace pared_ch=1 if v3>=1  & v3<=6
replace pared_ch=2 if v3==8

**************
***techo_ch***
**************
gen techo_ch=.
replace techo_ch=0 if v4==0
replace techo_ch=1 if v4>=1 & v4<=3
replace techo_ch=2 if v4==4 |v4==5

**************
***resid_ch***
**************
gen resid_ch=.
replace resid_ch=0 if v17==1
replace resid_ch=1 if v17==2 |v17==3
replace resid_ch=2 if v17==4 |v17==5
replace resid_ch=3 if v17==6

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v12 >=1 & v12 <=5)
replace aguamejorada_ch = 0 if (v12 >=6 & v12 <=7)
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  ((v13a >=1 & v13a <=4) & v13b ==1)
replace banomejorado_ch = 0 if  ((v13a >=1 & v13a <=4) & v13b ==2) | (v13a ==5 | v13a ==0) |   v14a == 2

*************
***dorm_ch***
*************
gen dorm_ch=.
replace dorm_ch=v8 if v8>=0 & v8<=10

****************
***cuartos_ch***
****************
gen cuartos_ch=.
replace cuartos_ch=v9 if v9>=1 & v9<=20

***************
***cocina_ch***
***************
gen cocina_ch=.

*************
**telef_ch***
*************
gen telef_ch=.
replace telef_ch=1 if  v18b==3
replace telef_ch=0 if  v18b==4
label var telef_ch "El hogar tiene servicio telefónico"

**************
**refrig_ch***
**************
gen refrig_ch=.
replace refrig_ch=1 if  v18c==5
replace refrig_ch=0 if  v18c==6
label var refrig_ch "El hogar posee heladera o refrigerador"

*************
**freez_ch***
*************
gen freez_ch=.
label var freez_ch "El hogar posee freezer o congelador"

************
**auto_ch***
************
gen auto_ch=.
replace auto_ch=1 if  v18h==7
replace auto_ch=0 if  v18h==8
label var auto_ch "El hogar posee automóvil particular"


*************
**compu_ch***
*************
gen compu_ch=.
replace compu_ch=1 if  v18f==3
replace compu_ch=0 if  v18f==4
label var compu_ch "El hogar posee computadora"

****************
**internet_ch***
****************
gen internet_ch=.
replace internet_ch=1 if  v19==1
replace internet_ch=0 if  v19==2

***********
**cel_ch***
***********
gen cel_ch=.
replace cel_ch=1 if  v18a==1
replace cel_ch=0 if  v18a==2

*************
**vivi1_ch***
*************
gen vivi1_ch=.
replace vivi1_ch=1 if  v1==1 |v1==2
replace vivi1_ch=2 if  v1==3 |v1==4
replace vivi1_ch=3 if  v1==5 |v1==6 | v1==7

*************
**vivi2_ch***
*************
gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

****************
**viviprop_ch***
****************
gen viviprop_ch=.
replace viviprop_ch=0 if v2a==3
replace viviprop_ch=1 if v2a==1
replace viviprop_ch=2 if v2a==2
replace viviprop_ch=3 if v2a==4 | v2a==5

****************
**vivitit_ch***
****************
gen vivitit_ch=.

***************
**vivialq_ch***
***************
gen vivialq_ch=.
replace vivialq_ch=v2a1 	if v2a == 3
replace vivialq_ch=. 		if v2a1==99999999

***************
**vivialq_ch***
***************
gen vivialqimp_ch=.
replace vivialq_ch=v2b 	
replace vivialq_ch=. 		if v2b==99999999

/***************************
* DISCAPACIDAD
***************************/
*Daniela Zuluaga Feb 2020:
*Con base a elaboración Mariana Pinzón y M.Antonella Pereira

gen dis_ci = 0
recode dis_ci nonmiss=. if a7a>=. & a7b>=.
recode dis_ci nonmiss=. if inlist(.,a7a,a7b)

foreach i in a b {
forvalues j=1/7 {
replace dis_ci=1 if a7`i'==`j'
}
}
lab def dis_ci 1 "Con Discapacidad" 0 "Sin Discapacidad"
lab val dis_ci dis_ci

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

rename c8 codindustria 
rename c9a codocupa
compress


saveold "`base_out'", replace


log close


