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

local PAIS PAN
local ENCUESTA EH
local ANO "2001"
local ronda m8

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Panama
Encuesta: EH
Round: Agosto
Autores: 
Versión 2010: autor no identificado
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 10 de Octubre de 2013
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/


use `base_in', clear

destring _all, replace

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"



************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

destring prov, replace
gen region_c=  prov

label define region_c  ///
1	"Bocas del Toro" ///
2	"Coclé" ///
3	"Colón" ///
4	"Chiriquí" ///
5	"Darién" ///
6	"Herrera" ///
7	"Los Santos" ///
8	"Panamá" ///
9	"Veraguas" ///
10	"Kuna Yala" ///
11	"Emberá" ///
12	"Ngäbe-Buglé"		  
label value region_c region_c
label var region_c "División política, provincias"

***************
***factor_ci***
***************

gen factor_ci=fac15
label variable factor_ci "Factor de expansion del individuo"


***************
****idh_ch*****
**************
egen idh_ch =group(prov dist corre estra unidad cuest hogar regireco areareco upm_rec)
label variable idh_ch "ID del hogar"


replace p1=1 if p1==3 & idh_ch==8236
replace p1=1 if p1==2 & idh_ch==8237
*************
****idp_ci****
**************

gen idp_ci=nper
label variable idp_ci "ID de la persona en el hogar"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if p1==1
replace relacion_ci=2 if p1==2
replace relacion_ci=3 if p1==3
replace relacion_ci=4 if p1==4 
replace relacion_ci=5 if p1==6
replace relacion_ci=6 if p1==5

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
label value relacion_ci relacion_ci

***************
***factor_ch***
***************

**NOTE need to create new variable factorjefe, save dataset, then egen new variable factor_ch**

gen factorjefe=factor_ci if relacion_ci==1
by idh_ch, sort: egen factor_ch=sum(factorjefe)

label variable factor_ch "Factor de expansion del hogar"
drop factorjefe


**********
***zona***
**********

gen byte zona_c=0 if areareco==2
replace zona_c=1 if areareco==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="PAN"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2001
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=8
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo" 6 " Junio" 7 "Julio" 8 "Agosto" 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre"
label value mes_c mes_c

****************************
***VARIABLES DEMOGRAFICAS***
****************************


**********
***sexo***
**********

gen sexo_ci=p2
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=p3
label variable edad_ci "Edad del individuo"


*****************
***estcivil_ci***
*****************
**NOTE estado civil NO EXISTE en este base de datos**

gen civil_ci=.
replace civil_ci=1 if p4==7 | p4==8
replace civil_ci=2 if p4==1 | p4==4
replace civil_ci=3 if p4==2 | p4==3 | p4==5
replace civil_ci=4 if p4==6
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci estcivil_ci


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

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0)   /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
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
**Pregunta: ¿Se considera usted indígena? (indi_rec) (1 - no indígena; 2 - indígena)
**No se identifica a personas afrodescendientes. Todos los no-indígenas se categorizan como "otro". 
**En el 2011 se convierte en la EHPM (no solo EH) 

	***************
	***afroind_ci***
	***************
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
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

clonevar sumactiv = p10_17 
/*
gen condocup_ci=.
replace condocup_ci=1 if sumactiv >= 1 & sumactiv <= 5
replace condocup_ci=2 if  sumactiv == 6 
replace condocup_ci=3 if  sumactiv >= 7 &  sumactiv <= 14 |  sumactiv == 0
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/

* Alternativa 2: segun la codificacion de la variable p10_17. MGD 06/06/2014
gen condocup_ci=.
replace condocup_ci=1 if p10_17>= 1 & p10_17<= 5 
replace condocup_ci=2 if  p10_17==6 | p10_17==7 | (((p10_17>=9 & p10_17<=11) | p10_17==14) & p18==1) 
recode condocup_ci .=3 if  edad_ci>=10
recode condocup_ci .=4 if  edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

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

/*
************
***emp_ci***
************

gen byte emp_ci=0
replace emp_ci=1 if p10_17>=1 & p10_17<=5

/*Se esta considerando empleado a aquellos que contestan "trabajando, trabajos informales, o trabajo familiar*/

****************
***desemp1_ci***
****************

gen desemp1_ci=(emp_ci==0 & p10_17==6)

****************
***desemp2_ci*** 
****************

gen desemp2_ci=(emp_ci==0 & (p10_17==6 | p10_17==7))

****************
***desemp3_ci***
****************

gen desemp3_ci=(emp_ci==0 & (p10_17==6 | p10_17==7 | p18==1))

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

gen pea3_ci=0
replace pea3_ci=1 if emp_ci==1 | desemp3_ci==1*/

*****************
***horaspri_ci***
*****************

gen horaspri_ci=p36 if p36>0 & p36<99
replace horaspri_ci=. if emp_ci==0

*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(p36 p41) if p36<99 & p36>0 , missing
replace horastot_ci=horaspri_ci if p41==99 | p41==0
replace horastot_ci=. if (p36==0 & p41==0) | (p36==99 | p41==99)
replace horastot_ci=. if emp_ci==0

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & sumactiv==8)

***************
***subemp_ci***
***************
gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & p43==1 & horaspri_ci<=30)

* Alternativa considerando disponibilidad. MGD 06/19/2014
gen subemp_ci1=0
replace subemp_ci1=1 if (emp_ci==1 & p43==1 & horaspri_ci<=30) & (p45==1 | (p46>=1 & p46<=3))



*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.
replace tiempoparc_ci=1 if p43==2 & horastot<=30
replace tiempoparc_ci=0 if p43==1 | (p43==2 & horastot>30 & horastot<.)


************************************
*** VARIABLES DE DEMANDA LABORAL***
************************************

**************
***ocupa_ci***
**************

**NOTE: No encontre fuerzas armadas en la lista. **

* Modificacion MGD 07/22/2014: se utiliza la clasificacion CNO de Panama
g ocup1=p26
destring ocup1, replace

g ocupa_ci=.
replace ocupa_ci=1 if (ocup1>=140 & ocup1<=662) & emp_ci==1
replace ocupa_ci=2 if (ocup1>=1 & ocup1<=139) & emp_ci==1
replace ocupa_ci=3 if (ocup1>=663 & ocup1<=817) & emp_ci==1
replace ocupa_ci=4 if ((ocup1>=880 & ocup1<=917) | (ocup1>=1555 & ocup1<=1565)) & emp_ci==1
replace ocupa_ci=5 if ((ocup1>=818 & ocup1<=879) | (ocup1>=1566 & ocup1<=1608)) & emp_ci==1
replace ocupa_ci=6 if (ocup1>=918 & ocup1<=1000)  & emp_ci==1
replace ocupa_ci=7 if ((ocup1>=1001 & ocup1<=1554) | (ocup1>=1609 & ocup1<=1649)) & emp_ci==1
replace ocupa_ci=8 if  (ocup1>=1650 & ocup1<=1651)& emp_ci==1
replace ocupa_ci=9 if (ocup1>=1652 & ocup1<=1653 ) & emp_ci==1

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
replace rama_ci=1 if p28>=111 & p28<=502 & emp_ci==1
replace rama_ci=2 if p28>=1320 & p28<=1429 & emp_ci==1
replace rama_ci=3 if p28>=1511 & p28<=3720 & emp_ci==1
replace rama_ci=4 if p28>=4010 & p28<=4100 & emp_ci==1
replace rama_ci=5 if p28>=4510 & p28<=4550 & emp_ci==1
replace rama_ci=6 if p28>=5110 & p28<=5530 & emp_ci==1
replace rama_ci=7 if p28>=6010 & p28<=6420 & emp_ci==1
replace rama_ci=8 if p28>=6511 & p28<=7020 & emp_ci==1
replace rama_ci=9 if p28>=7111 & p28<=9900 & emp_ci==1
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

/* Diccionario del 2000 
0: No aplicable
111 al 202: Agricultura ganadería, caza y silvicultura
501 al 502: Pesca
1320 al 1429: Explotación de minas y canteras
1511 al 3720: Industrias manufactureras
4010 al 4100: Suministro de electricidad, gas y agua
4510 al 4550: Construcción
5110 al 5390: Comercio al por mayor y al por menor; reparación de vehículos, automotores, motocicletas, efectos
personales y enseres domésticos
5510 al 5520: Hoteles y restaurantes
6010 al 6420: Transporte, almacenamiento y comunicaciones
6511 al 6720: Intermediación financiera
7011 al 7499: Actividades inmobiliarias, empresariales y de alquiler
7511 al 7530: Administración pública y defensa; planes de seguridad social de afiliación obligatoria
8010 al 8090: Enseñanza
8511 al 8532: Actividades de servicios sociales y de salud
9000 al 9309: Otras actividades comunitarias, sociales y personales de servicios
9500: Hogares privados con servicio doméstico
9800: Organizaciones y órganos extraterritoriales
9900: Actividades no bien especificadas*/

******************
***categopri_ci***
******************

**check to see that Trabajadores Familiares are not receiving salary**

gen categopri_ci=.
replace categopri_ci=1 if p30==6 & emp_ci==1
replace categopri_ci=2 if ( p30==5  | p30==8) & emp_ci==1
replace categopri_ci=3 if (p30==1 | p30==2 | p30==3 | p30==4) & emp_ci==1
replace categopri_ci=4 if p30==7 & emp_ci==1
*MLO: agregue la condicion que sea ocupado
label define categopri_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional en la actividad principal"


******************
***categosec_ci***
******************

**solo preguntaron si el segundo trabajo es agropecuario/artesanal o no, y cuanto gana**
*no es posbile crear esta variable*

gen categosec_ci=.
label define categosec_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional en la actividad secundaria"

/******************
***contrato_ci***
*****************

gen contrato_ci=.
replace contrato_ci=1 if p31==2 | p31==3 | p31==4 /* contratos permanentes o transitorios */
replace contrato_ci=0 if p31==5 | p31==1 /* sin contrato o trabajadores permanentes */

***************
***segsoc_ci***
***************

**no es posible crear esta variable**

gen segsoc_ci=.*/


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & (p37==1 | p37==2)

**nunca preguntan por el numero de trabajos distintos. solo preguntan por trabajo principal y secundario**

*****************
***tamfirma_ci***
*****************

gen tamfirma_ci=.
replace tamfirma_ci=0 if p29==1 /* menos de 5 */
replace tamfirma_ci=1 if p29>=2 & p29<=5 /* mas de 5 */


*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci=1 if p30==1
replace spublico_ci=0 if p30>=2 & p30<=8


****************
***durades_ci***
****************
gen d=p21-200 if p21>=201 & p21<299
replace d=0.5 if p21==100

gen durades_ci= .
replace durades_ci=d
*replace durades_ci=. if emp_ci==1


*******************
***antiguedad_ci***
*******************
gen m=p33-100 if p33>=100 & p33<=111
gen a=p33-200 if p33>=201 & p33<299

gen antiguedad_ci=.
replace antiguedad_ci=a
replace antiguedad_ci=m/12 if a==.

drop a m d

******************************************************************
*********************        INGRESOS         ********************
******************************************************************

****************************
***ylmpri_ci & ylmpri1_ci***
****************************

gen ylmpri_ci=p351 if p351>0 & p351<9999 & categopri==3
replace ylmpri_ci=p353 if p353>0 & p353<9999 & (categopri==1 | categopri==2)
replace ylmpri_ci=0 if categopri==4
replace ylmpri_ci=. if emp_ci==0

gen agui=p47f if p47f>0 & p47f<9999

egen ylmpri1_ci=rsum(ylmpri_ci agui), missing
replace ylmpri1_ci=. if ylmpri_ci==. & agui==.
replace ylmpri1_ci=. if emp_ci==0
replace ylmpri1_ci=. if ylmpri_ci==. & (p352==0 | p352==9999)

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

**solo para personas empleadas, else missing**

gen nrylmpri_ci=((p351==9999 | p353==9999) & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0


***************
***ylmsec_ci***
***************
gen ylmsec_ci=p42 if p42>0 & p42<9999  
replace ylmsec_ci=. if emp_ci==0


****************
***ylnmsec_ci***
****************

g ylnmsec_ci=.
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


**************
*** ylm_ci ***
**************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylm1_ci= rsum(ylmpri1_ci ylmsec_ci), missing
replace ylm1_ci=. if ylmpri1_ci==. & ylmsec_ci==.

*******************
***   ylnm_ci   ***
*******************

gen ylnmpri_ci=p352 if p352>0 & p352<9999
gen ylnm_ci=ylnmpri_ci
*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario"
***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 
*************
***ynlm_ci***
*************

gen jub=p47a if p47a>0 & p47a<9999

gen ayfam=p47b if p47b>0 & p47b<9999

gen alqui=p47c if p47c>0 & p47c<9999

gen loter=p47d if p47d>0 & p47d<9999

gen becas=p47e if p47e>0 & p47e<9999

gen agro=p47g if p47g>0 & p47g<9999

gen otroy=p47h if p47h>0 & p47h<9999

egen ynlme1= rsum(jub ayfam alqui loter becas agro otroy) if emp_ci==1, missing
replace ynlme1=. if (jub==. & ayfam==. & alqui==. & becas==. & loter==. & agro==. & otroy==. & emp_ci==1)

egen ynlmd1= rsum(jub ayfam alqui loter becas agro agui otroy) if emp_ci==0, missing
replace ynlmd1=. if (jub==. & ayfam==. & alqui==. & becas==. & loter==. & agro==. & agui==. & otroy==. & emp_ci==0)

egen ynlm1_ci=rsum(ynlme1 ynlmd1), missing
replace ynlm1_ci=. if ynlme1==. & ynlmd1==.

egen ynlme= rsum(jub ayfam alqui loter becas otroy) if emp_ci==1, missing
replace ynlme=. if (jub==. & ayfam==. & alqui==. & becas==. & loter==. & otroy==. & emp_ci==1)

egen ynlmd= rsum(jub ayfam alqui loter becas agui otroy) if emp_ci==0, missing
replace ynlmd=. if (jub==. & ayfam==. & alqui==. & becas==. & loter==. & agui==. & otroy==. & emp_ci==0)

egen ynlm_ci=rsum(ynlme ynlmd), missing
replace ynlm_ci=. if ynlme==. & ynlmd==.

drop jub alqui loter becas agro otroy ayfam agui ynlme ynlmd ynlme1 ynlmd1
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

*by idh_ch, sort: egen nrylmpri1_ch=sum(nrylmpri1_ci) if miembros_ci==1
*replace nrylmpri1_ch=1 if nrylmpri1_ch>0 & nrylmpri1_ch<.
*replace nrylmpri1_ch=. if nrylmpri1_ch==.


************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing 

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
*replace ylmnr_ch=. if nrylmpri_ch==1

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
*replace ylmnr1_ch=. if nrylmpri1_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

******************
*** remesas_ch ***
******************
gen remesas_ci=.
gen remesas_ch=.


***************
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm1_ch=sum(ynlm1_ci) if miembros_ci
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci


*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

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

gen rentaimp_ch=.

******************************
***ylhopri_ci & ylhopri1_ci***
******************************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

**************************
***ylmho_ci & ylm1ho_ci***
**************************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)

*** HOUSING ***

****************
***aguared_ch***
****************
gen aguared_ch =.
label var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = .

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.

*************
*aguadist_ch*
*************
gen aguadist_ch=.

**************
*aguadisp1_ch*
**************
gen aguadisp1_ch = 9 


**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9


*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = .

*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch =.
*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.

***************
***banoex_ch***
***************
gen banoex_ch=9
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch=.


************
*sinbano_ch*
************
gen sinbano_ch = .
*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch =9

gen luz_ch=.

gen luzmide_ch=.

gen combust_ch=.

gen des1_ch=.

gen des2_ch=.

gen piso_ch=.

gen pared_ch=.

gen techo_ch=.

gen resid_ch=. 


gen dorm_ch=.

gen cuartos_ch=.

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.

gen vivi2_ch=.

gen viviprop_ch=.

gen vivitit_ch=.

gen vivialq_ch=.

gen vivialqimp_ch=.

** EDUCACION **

gen asiste_ci=.
replace asiste_ci=1 if p7==1
replace asiste_ci=0 if p7==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

gen pqnoasis_ci=p7a if p7a>0
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p7a==3
replace pqnoasis1_ci = 2 if p7a==2
replace pqnoasis1_ci = 5 if p7a==4
replace pqnoasis1_ci = 8 if p7a==1
replace pqnoasis1_ci = 9 if p7a==5

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

gen edupub_ci=.
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"



gen grado=p8-10 if p8>=11 & p8<=16
replace grado=p8-20 if p8>=21 & p8<=26
replace grado=p8-30 if p8>=31 & p8<=33
replace grado=p8-40 if p8>=41 & p8<=49
replace grado=p8-50 if p8>=51 & p8<=53
replace grado=0 if p8==50

gen nivel=0 if p8==50
replace nivel=1 if p8>=11 & p8<=16
replace nivel=2 if p8>=21 & p8<=26
replace nivel=3 if p8>=31 & p8<=33
replace nivel=4 if p8>=41 & p8<=49
replace nivel=5 if p8>=51 & p8<=53

gen aedu_ci=0 if nivel==0
replace aedu_ci=grado if nivel==1
replace aedu_ci=grado+6 if nivel==2 | nivel==3
replace aedu_ci=grado+12 if nivel==4 | nivel==5

gen eduno_ci=.
replace eduno_ci=0 if aedu>0 & aedu<.
replace eduno_ci=1 if aedu==0

gen edupi_ci=.
replace edupi_ci=1 if aedu>=1 & aedu<6
replace edupi_ci=0 if aedu>=6 & aedu<.

gen edupc_ci=.
replace edupc_ci=1 if aedu==6
replace edupc_ci=0 if (aedu>=1 & aedu<6) | (aedu>6 & aedu<.)

gen edusi_ci=.
replace edusi_ci=1 if aedu>6 & aedu<12
replace edusi_ci=0 if (aedu>=1 & aedu<=6) | (aedu>=12 & aedu<.)

gen edusc_ci=.
replace edusc_ci=1 if aedu==12
replace edusc_ci=0 if (aedu>=1 & aedu<12) | (aedu>12 & aedu<.)

gen eduui_ci=.
replace eduui_ci=1 if aedu>12 & aedu<17
replace eduui_ci=0 if (aedu>=1 & aedu<=12) | (aedu>=17 & aedu<.)

gen eduuc_ci=.
replace eduuc_ci=1 if aedu>=17 & aedu<.
replace eduuc_ci=0 if (aedu>=1 & aedu<17) 

gen edus1i_ci=.
replace edus1i_ci=0 if edusi_ci==1 | edusc_ci==1
replace edus1i_ci=1 if aedu>=7 & aedu<=8

gen edus1c_ci=.
replace edus1c_ci=0 if edusi_ci==1 | edusc_ci==1
replace edus1c_ci=1 if aedu==9

gen edus2i_ci=.
replace edus2i_ci=0 if edusi_ci==1 | edusc_ci==1
replace edus2i_ci=1 if aedu==10 | aedu==11

gen edus2c_ci=.
replace edus2c_ci=0 if edusi_ci==1 | edusc_ci==1
replace edus2c_ci=1 if aedu==12


gen edupre_ci=.

gen eduac_ci=.
replace eduac_ci=0 if nivel==5
replace eduac_ci=1 if nivel==4

drop nivel grado

*****************************************
*** PANAMA 2001	- ENCUESTA DE HOGARES ***	
*****************************************

 destring, replace

 rename p2 sexo
 rename p3 edad
 destring areareco, gen(area)
 rename fac15_e factor

 tab area [w=factor]

/*
p1
Parentesco
 1. Jefes
 2. Cónyuge del jefe
 3. Hijo o hija (incluyendo los hijos adoptivos o de crianza)
 4. Otro pariente (hermanos, nietos, tíos, sobrinos, abuelos, cuñados, padres, suegros, etc.)
 5. Servicio doméstico (sirvientes, cocineras, cocineras, conductores, niñeras, etc.)
 6. Otras personas no parientes del jefe (huéspedes y las familias de esas personas)
*/

 gen	 incl=1 if (p1>=1 & p1<=6) 
 replace incl=0 if (p1==5)

 sort prov dist corre estra unidad cuest hogar nper

** Years of education
/*
ASISTE (P7)
7. ¿Asiste a la escuela actualmente?

NIVEDUC (P8)
8. ¿Grado o año más alto aprobó?

11. Primaria 1 año	41. Universitaria 1 año
12. Primaria 2 años	42. Universitaria 2 años
13. Primaria 3 años	43. Universitaria 3 años
14. Primaria 4 años	44. Universitaria 4 años
15. Primaria 5 años	45. Universitaria 5 años
16. Primaria 6 años	46. Universitaria 6 años
21. Secundaria 1 año	47. Universitaria 7 años
22. Secundaria 2 años	48. Universitaria 8 años
23. Secundaria 3 años	49. Universitaria 9 años
24. Secundaria 4 años	51. No Universitaria 1 año
25. Secundaria 5 años	52. No Universitaria 2 años
26. Secundaria 6 años	53. No Universitaria 3 años
31. Vocacional 1 año	50. Ningún grado
32. Vocacional 2 años	0. No aplicable (menores de 5 años)
33. Vocacional 3 años
*/

 rename p7 asiste
 rename p8 niveduc

 gen anoest=.
 replace anoest=0  if niveduc==50 
 replace anoest=1  if niveduc==11
 replace anoest=2  if niveduc==12
 replace anoest=3  if niveduc==13
 replace anoest=4  if niveduc==14
 replace anoest=5  if niveduc==15
 replace anoest=6  if niveduc==16
 replace anoest=7  if niveduc==21 | niveduc==31
 replace anoest=8  if niveduc==22 | niveduc==32
 replace anoest=9  if niveduc==23 | niveduc==33
 replace anoest=10 if niveduc==24
 replace anoest=11 if niveduc==25
 replace anoest=12 if niveduc==26
 replace anoest=13 if niveduc==41 | niveduc==51
 replace anoest=14 if niveduc==42 | niveduc==52
 replace anoest=15 if niveduc==43 | niveduc==53
 replace anoest=16 if niveduc==44
 replace anoest=17 if niveduc==45
 replace anoest=18 if niveduc==46
 replace anoest=19 if niveduc==47
 replace anoest=20 if niveduc==48
 replace anoest=21 if niveduc==49
 replace anoest=99 if niveduc==0 | anoest==.
 

** Economic Active Population

* NATIONAL DEFINITION 
	
 gen peaa=0
 replace peaa=1 if    ((sumactiv>=1 & sumactiv<=3) | sumactiv==5) | (sumactiv==4 & (p25>=100 & p25<=199))

* DESOCUPADOS
 gen desocup=0
 replace desocup=1 if ( (sumactiv>=6  & sumactiv<=8) 		      & (p25>=100 & p25<=199))
 replace desocup=2 if (((sumactiv>=9  & sumactiv<=11) | sumactiv==14) & (p25>=100 & p25<=199)) & p18==1
 replace desocup=3 if (((sumactiv>=9  & sumactiv<=11) | sumactiv==14) & (p25>=100 & p25<=199)) & p18==2 & p19==1
 replace desocup=4 if ( (sumactiv>=6  & sumactiv<=8) 		      & (p25==999))
 replace desocup=5 if (((sumactiv>=10 & sumactiv<=11) | sumactiv==14) & (p25==999)) 	       & p18==1
 replace desocup=6 if (((sumactiv>=10 & sumactiv<=11) | sumactiv==14) & (p25==999))            & p18==2 & p19==1

 replace peaa=2 if (desocup>=1 & desocup<=6)
 
 replace peaa=3 if peaa==0 & (edad>=10 & edad<99)

* INTERNATIONAL DEFINITION

 gen	 peaaint=0
 replace peaaint=1 if    ((sumactiv>=1 & sumactiv<=3) | sumactiv==5) | (sumactiv==4 & (p25>=100 & p25<=199))

 gen desocupint=0
 replace desocupint=1 if ( (sumactiv>=6  & sumactiv<=7) 		 & (p25>=100 & p25<=199))
 replace desocupint=2 if (((sumactiv>=9  & sumactiv<=11) | sumactiv==14) & (p25>=100 & p25<=199)) & p18==1
 replace desocupint=3 if ( (sumactiv>=6  & sumactiv<=7) 		 & (p25==999))
 replace desocupint=4 if (((sumactiv>=10 & sumactiv<=11) | sumactiv==14) & (p25==999)) 		  & p18==1

 replace peaaint=2 if (desocupint>=1 & desocupint<=4)
 
 replace peaaint=3 if peaaint==0 & (edad>=10 & edad<99)

 destring p30 p26 p28, replace

 rename p30 categ
 rename p28 rama
 rename p26 ocup
 


************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*
ASISTE (P7)
7. ¿Asiste a la escuela actualmente?

NIVEDUC (P8)
8. ¿Grado o año más alto aprobó?

11. Primaria 1 año		41. Universitaria 1 año
12. Primaria 2 años		42. Universitaria 2 años
13. Primaria 3 años		43. Universitaria 3 años
14. Primaria 4 años		44. Universitaria 4 años
15. Primaria 5 años		45. Universitaria 5 años
16. Primaria 6 años		46. Universitaria 6 años
21. Secundaria 1 año		47. Universitaria 7 años
22. Secundaria 2 años		48. Universitaria 8 años
23. Secundaria 3 años		49. Universitaria 9 años
24. Secundaria 4 años		51. No Universitaria 1 año
25. Secundaria 5 años		52. No Universitaria 2 años
26. Secundaria 6 años		53. No Universitaria 3 años
31. Vocacional 1 año		50. Ningún grado
32. Vocacional 2 años		0. No aplicable (menores de 5 años)
33. Vocacional 3 años
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if  (edad>=6 & edad<=11) & (asiste==1 | asiste==2)
 replace NERP=1 if  (edad>=6 & edad<=11) & (asiste==1 & (niveduc==50|(niveduc>=11 & niveduc<=15)))
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if  (edad>=12 & edad<=17) & (asiste==1 | asiste==2)
 replace NERS=1 if  (edad>=12 & edad<=17) & (asiste==1 & ((niveduc>=16 & niveduc<=25) |(niveduc>=31 & niveduc<=32)))

* ISCED 3
* 2o Nivel de enseñanza media

 gen	 NERS2=0 if  (edad>=15 & edad<=17) & (asiste==1 | asiste==2)
 replace NERS2=1 if  (edad>=15 & edad<=17) & (asiste==1) & ((niveduc>=23 & niveduc<=25))
	
*Literacy Rate of 15-24 Years Old*
* More than 5 years of education

 gen	 LIT=0 if  (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if  (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)


*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if   (asiste==1 & (niveduc==50|(niveduc>=11 & niveduc<=15)))
 gen sec=1  if   (asiste==1 & ((niveduc>=16 & niveduc<=25) |(niveduc>=31 & niveduc<=32)))
 gen ter=1  if   (asiste==1 & (niveduc==26 | niveduc==33 | ((niveduc>=41 & niveduc<=45) | (niveduc>=51 & niveduc<=53))))
	      
** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  


** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
  /*
  categ (p30)							rama (p28)
  30. ¿Dónde usted trabaja o trabajó lo hizo como..?		28. ¿A qué se dedica el negocio...?
   1. Empleado del gobierno
   2. Empleado de empresa privada
   3. Empleado de la comisión del canal o sitios de defensa
   4. Servicio doméstico
   5. Por cuenta propia
   6. Patrono (dueño)
   7. Trabajador familiar
   8. Miembro de una cooperativa de producción
  */

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ>=1 & categ<=3) & peaa==1 & (rama>=1010 & rama<=9800)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ>=1 & categ<=3) & peaa==1 & (rama>=1010 & rama<=9800) & sexo==2
	
*Gender-With domestic servants*

 gen 	WENAS2=0 if   (edad>=15 & edad<=64) & (categ>=1 & categ<=4) & peaa==1 & (rama>=1010 & rama<=9800)
 replace WENAS2=1 if  (edad>=15 & edad<=64) & (categ>=1 & categ<=4) & peaa==1 & (rama>=1010 & rama<=9800) & sexo==2

 * RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

* National Definition
 gen	 tasadeso=0 if peaa==1
 replace tasadeso=1 if peaa==2

* International Definition
 gen	 tda=0 if peaaint==1
 replace tda=1 if peaaint==2

/*
  ** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

  noisily display "Unemployment Rate 15 to 24. National Definition"
  global variable UNMPLYMENT15	
  gen UNMPLYMENT15=0     if  (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
  replace UNMPLYMENT15=1 if  (edad>=15 & edad<=24) & tasadeso==1 
*/

* International Definition

 gen	 UNMPLYMENT15=0 if  (edad>=15 & edad<=24) &  (tda==0 | tda==1) 
 replace UNMPLYMENT15=1 if  (edad>=15 & edad<=24) &  (tda==1)

*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

** CCA 19. Proportion of children under 15 who are working
* Includes population 12 to 14 years-old

 gen	 CHILDREN=0 if  (edad>=12 & edad<=14) 
 replace CHILDREN=1 if  (edad>=12 & edad<=14) & (peaa==1)

** Disconnected Youths

 gen	 DISCONN=0 if  (edad>=15 & edad<=24) &  (sumactiv>=1 & sumactiv<=14)
 replace DISCONN=1 if  (edad>=15 & edad<=24) &  (sumactiv==8 | sumactiv==9 | sumactiv==13 | sumactiv==14)

*** Rezago escolar

 gen	 rezago=0	if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7

 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==17
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen 	 REZ=0 if  (edad>=7 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if  (edad>=7 & edad<=17) & (rezago==1)

* Primary completion rate [15 - 24 years of age]

 gen 	 PRIMCOMP=0 if (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)

* Average years of education of the population 15+


 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))

 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=17) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=17) & (anoest>=0 & anoest<99)

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

* PAN 2001

gen salmm_ci= . /*239.20*/
replace salmm_ci= 219.6 if rama_ci==1
replace salmm_ci= 262.4 if rama_ci==2
replace salmm_ci= 236 if rama_ci==3
replace salmm_ci= 254.4 if rama_ci==4
replace salmm_ci= 319.2 if rama_ci==5
replace salmm_ci= 236 if rama_ci==6
replace salmm_ci= 252 if rama_ci==7
replace salmm_ci= 289.6 if rama_ci==8
replace salmm_ci= 277.07 if rama_ci==9
replace salmm_ci= 260.7 if salmm_ci==.

label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= . if zona_c==1 & dist==1 /* Cdad. Panamá*/
replace lp_ci= . if zona_c==1 & dist==3 /* Zona urbana districto san miguelito*/
replace lp_ci= . if ((dist!=1 & dist!=3) & zona_c==1) | zona_c==0  /* resto urbano o rural*/


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =.
replace lpe_ci= . if zona_c==1 & dist==1 /* Cdad. Panamá*/
replace lpe_ci= . if zona_c==1 & dist==3 /* Zona urbana districto san miguelito*/
replace lpe_ci= . if ((dist!=1 & dist!=3) & zona_c==1) | zona_c==0  /* resto urbano o rural*/

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************


gen afiliado_ci=.	
replace afiliado_ci =1 if p5==1
recode afiliado_ci .=0 if sumactiv >= 1 & sumactiv <= 6
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

****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p31==1 | p31==4) & categopri_ci==3
replace tipocontrato_ci=2 if (p31==2 | p31==3) & categopri_ci==3
replace tipocontrato_ci=3 if (p31==5 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************
* MGD 12/4/2015: se corrige con la variable correcta p25, inclusion de ceros con p25 >100. 
gen cesante_ci=1 if p25>100 & p25!=999 & condocup_ci==2
*gen cesante_ci=1 if p27==999 
recode cesante_ci .=0 if condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"		

*******************
***formal***
*******************
* David Cornejo Febrero 2023: no existe la variable cotizante por tanto tampoco formalidad

gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"

*************
*tamemp_ci
*************
gen tamemp_ci=1 if p29==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p29==2 | p29==3 | p29==4
*Empresas grandes
replace tamemp_ci=3 if p29==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

*************
*categoinac_ci
*************

gen categoinac_ci=1 if p10_17==9
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if p10_17==10
*Quehaceres del Hogar
replace categoinac_ci=3 if p10_17==11
*Otra razon
replace categoinac_ci=4 if p10_17==12 | p10_17==13 | p10_17==14
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo

*************
**pension_ci*
*************
replace p47a=. if p47a==9999
egen aux_p=rsum(p47a), missing
gen pension_ci=1 if aux_p>0 & aux_p!=. & aux_p!=999999
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

egen ypen_ci=rsum(p47a), missing
*2014, 02 mlo
*replace ypen_ci=.
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


*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if   niveduc==31 |   niveduc==32|   niveduc==33 
recode tecnica_ci .=0
label var tecnica_ci "1=formacion terciaria tecnica"


ren ocup ocup_old
*******************
*** SALUD  ***
*******************

*******************
*** cobsalud_ci ***
*******************

gen cobsalud_ci=.

label var cobsalud_ci "Tiene cobertura de salud"
label define cobsalud_ci 0 "No" 1 "Si" 
label value cobsalud_ci cobsalud_ci

************************
*** tipocobsalud_ci  ***
************************

gen tipocobsalud_ci=.

label var tipocobsalud_ci "Tipo cobertura de salud"
lab def tipocobsalud_ci 0"Sin cobertura" 1 "Publico" 2"Privado/otros" 
lab val tipocobsalud_ci tipocobsalud_ci


*********************
*** probsalud_ci  ***
*********************
* Nota: se pregunta si tuvieron problemas de salud en último mes. 

gen probsalud_ci=.

label var probsalud_ci "Tuvo algún problema de salud en los ultimos días"
lab def probsalud_ci 0 "No" 1 "Si"
lab val probsalud_ci probsalud_ci


*********************
*** distancia_ci  ***
*********************
gen distancia_ci=.

label var distancia_ci "Dificultad de acceso a salud por distancia"
lab def distancia_ci 0 "No" 1 "Si"
lab val distancia_ci distancia_ci

*****************
*** costo_ci  ***
*****************
gen costo_ci=.

label var costo_ci "Dificultad de acceso a salud por costo"
lab def costo_ci 0 "No" 1 "Si"
lab val costo_ci costo_ci

********************
*** atencion_ci  ***
********************
gen atencion_ci=.

label var atencion_ci "Dificultad de acceso a salud por problemas de atencion"
lab def atencion_ci 0 "No" 1 "Si"
lab val atencion_ci atencion_ci

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
	
	**********************
	*** migrantiguo5_ci **
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.


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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close






