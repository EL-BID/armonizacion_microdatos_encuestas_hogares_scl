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

local PAIS HND
local ENCUESTA EPHPM
local ANO "2019"
local ronda m6 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m6
Autores: Daniela Zuluaga
Última versión: Daniela Zuluaga - Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Marzo de 2019

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
* no disponible base septiembre hasta el momento
****************************************************************************/


use "`base_in'", clear

foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
* Region_c *
************
gen region_c= depto
destring region_c,replace

label define region_c  ///
           1 "Atlantida" ///
           2 "Colon" ///
           3 "Comayagua" ///
           4 "Copan" ///
           5 "Cortes" ///
           6 "Choluteca" ///
           7 "El Paraiso" ///
           8 "Francisco Morazan" ///
           9 "Gracias a Dios" ///
          10 "Intibuca" ///
          11 "Islas de la bahia" ///
          12 "La paz" ///
          13 "Lempira" ///
          14 "Ocotepeque" ///
          15 "Olancho" ///
          16 "Santa Barbara " ///
          17 "Valle" ///
          18 "Yoro"
 
label value region_c region_c
label var region_c "Division política, departamentos"


************
** ine01  ** 
************
gen ine01= depto
destring ine01,replace

label define ine01  ///
           1 "Atlantida" ///
           2 "Colon" ///
           3 "Comayagua" ///
           4 "Copan" ///
           5 "Cortes" ///
           6 "Choluteca" ///
           7 "El Paraiso" ///
           8 "Francisco Morazan" ///
           9 "Gracias a Dios" ///
          10 "Intibuca" ///
          11 "Islas de la bahia" ///
          12 "La paz" ///
          13 "Lempira" ///
          14 "Ocotepeque" ///
          15 "Olancho" ///
          16 "Santa Barbara " ///
          17 "Valle" ///
          18 "Yoro"
 
label value ine01 ine01
label var ine01 "Division administrativa, departamentos"


***********
*factor_ch*
***********
gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"

********
*idh_ch*
********

* se clona variable ya que de la forma tradicional se generan hogares de 52 miembros en lugar de 16 miembros como lo indica la variable.
clonevar idh_ch = hogar
format hogar %14.0g
format idh_ch %14.0g

********
*idp_ci*
********
gen idp_ci=nper 

********
*zona_c*
********
gen zona_c=1 if domi==1 | domi==2 | domi==3 
replace zona_c=0 if domi==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

********
*pais_c*
********
gen pais_c="HND"

********
*anio_c*
********
gen anio_c=2019

*******
*mes_c*
*******

gen mes_c= 6
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre" 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo" 6 "Junio" 7 "Julio" 8 "Agosto"
label value mes_c mes_c


*************
*relacion_ci*
*************
gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
replace relacion_ci=4 if rela_j>=5 & rela_j<= 8 
replace relacion_ci=5 if rela_j==9
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci



	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***********
*factor_ci*
***********
gen factor_ci=factor_ch

***************
***upm_ci***
***************

gen upm_ci=dominio
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

gen estrato_ci=.
label variable estrato_ci "Estrato"

*********
*sexo_ci*
*********
gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

*********
*edad_ci*
*********
gen edad_ci=edad 
label var edad_ci "Edad del Individuo"
drop edad

**********
*civil_ci*
**********
gen civil_ci=.
replace civil_ci=1 if civil==5
replace civil_ci=2 if civil==1 | civil==6
replace civil_ci=3 if civil==3 | civil==4
replace civil_ci=4 if civil==2
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
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

******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 


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

****************
****condocup_ci*
****************
/*
gen condocup_ci=condact
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Comprobacion con variables originales.  Se considera ocupado a quienes estan en trabajos no remunerados. 5/28/2014 MGD
* La edad minima de la encuesta se cambia a 5 anios.

g condocup_ci=.
replace condocup_ci=1 if (cp501==1 | cp504==1 | cp505==1)
replace condocup_ci=2 if (cp501==2 | cp504==2 | cp505==2) & (cp510==1 | cp510==1) 
recode condocup_ci (.=3) if edad_ci>=5
recode condocup_ci (.=4) if edad_ci<5
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci  1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
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


**************
*categopri_ci*
**************
gen categopri_ci=.
replace categopri_ci=1 if cp526==7 | cp526==6 
replace categopri_ci=2 if cp526==5 | cp526==4 
replace categopri_ci=3 if cp526==1  | cp526==2  | cp526==3 | cp526==10
replace categopri_ci=4 if cp526==8 | cp526==9 | cp526==11
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

*************
*horaspri_ci*
*************
egen horaspri_ci=rowtotal( cp522_dom cp522_lun cp522_mar cp522_mie cp522_jue cp522_vie cp522_sab), m
replace horaspri_ci=. if horaspri_ci>168

************
*horastot_ci
************
egen horassec_ci=rowtotal( cp539dom cp539lun cp539mar cp539mie cp539jue cp539vie cp539sab ), m
replace horassec_ci=. if horassec_ci>168
egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0
replace horastot_ci = . if horastot_ci>168

****************
* categosec_ci *
****************
gen categosec_ci=.
replace categosec_ci=1 if cp542==7
replace categosec_ci=2 if cp542==5 | cp542==6 
replace categosec_ci=3 if cp542==1  | cp542==2  | cp542==3 | cp542==4 | cp542==10
replace categosec_ci=4 if cp542==8 | cp542==9 | cp542==11
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci


************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=cp514/30   if cp514tiempo==1
replace durades_ci=cp514/4.3  if cp514tiempo==2
replace durades_ci=cp514      if cp514tiempo==3
label var durades "Duracion del Desempleo (en meses)"

***************
*antiguedad_ci*
***************
*Las variables para generar la variable de antiguedad son ce33 y ce33_tiempo, sin embargo,
*las variables que están en la base se encuentran como valores perdidos.
generat antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*************
*desalent_ci*
*************
gen desalent_ci=.
replace desalent_ci=1 if cp511==6
replace desalent_ci=0 if cp511!=6 & cp511!=.

***********
*subemp_ci*
***********
/*
gen subemp_ci=.
replace subemp_ci=0 if emp==0 | emp==1
replace subemp_ci=1 if horastot<30 & ce120==1
label var subemp_ci "Trabajadores subempleados"
*/
* MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & cp551==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

***************
*tiempoparc_ci*
***************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp==0 | emp==1
replace tiempoparc_ci=1 if horastot<30 
label var tiempoparc_ci "Trabajadores a medio tiempo"

*************
*nempleos_ci*
*************
generat nempleos_ci=1 if cp535==2
replace nempleos_ci=2 if cp535==1
replace nempleos_ci=. if emp_ci==0
/*
**************
*firmpapeq_ci*
**************
gen firmapeq_ci=0 if (ce32_cantidad>5 & ce32_cantidad<99999)
replace firmapeq_ci=1 if (ce32_cantidad<=5 & ce32_cantidad!=0)
*/

*************
*spublico_ci*
*************
gen spublico_ci=1 if cp526==1 
replace spublico_ci=0 if cp526!=1 

**********
*ocupa_ci*
**********
gen oc= ce425cod
tostring oc, replace
gen labor=substr(oc,1,4)
destring labor, replace 
drop oc

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999 & emp_ci==1
replace ocupa_ci=2 if labor>=1000 & labor<=1999 & emp_ci==1
replace ocupa_ci=3 if labor>=4000 & labor<=4999 & emp_ci==1
replace ocupa_ci=4 if ((labor>=5200 & labor<=5299) | (labor>=9100 & labor<=9119)) & emp_ci==1
replace ocupa_ci=5 if ((labor>=5100 & labor<=5199) | (labor>=9120 & labor<=9169)) & emp_ci==1
replace ocupa_ci=6 if ((labor>=6000 & labor<=6999) | (labor>=9210 & labor<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((labor>=7000 & labor<=8999) | (labor>=9311 & labor<=9333)) & emp_ci==1
replace ocupa_ci=8 if labor>0 & labor<=9999 & emp_ci==1 & ocupa_ci==.
replace ocupa_ci=9 if labor==9999 & emp_ci==1
drop labor 

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

*********
*rama_ci*
*********
*DZ Agosto 2019: Se pasa directamente de la clasificación CIIU Rev2.0 a CIIU Rev 4.0. No existe una tabla de equivalencia como tal en el INE.
gen rama_ci=.
replace  rama_ci=1 if ramaop==1 & emp_ci==1
replace  rama_ci=2 if ramaop==2 & emp_ci==1
replace  rama_ci=3 if ramaop==3 & emp_ci==1
replace  rama_ci=4 if ramaop==4 | ramaop==5 & emp_ci==1
replace  rama_ci=5 if ramaop==6 & emp_ci==1
replace  rama_ci=6 if ramaop==7 | ramaop==9 & emp_ci==1
replace  rama_ci=7 if ramaop==8 | ramaop==10 & emp_ci==1
replace  rama_ci=8 if ((ramaop>=11 & ramaop<=14) & (emp_ci==1))
replace  rama_ci=9 if ((ramaop>=15 & ramaop<=21) & (emp_ci==1))
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


* rama secundaria
gen ramasec_ci=.
replace  ramasec_ci=1 if ramaos==1 & emp_ci==1
replace  ramasec_ci=2 if ramaos==2 & emp_ci==1
replace  ramasec_ci=3 if ramaos==3 & emp_ci==1
replace  ramasec_ci=4 if ramaos==4 | ramaos==5 & emp_ci==1
replace  ramasec_ci=5 if ramaos==6 & emp_ci==1
replace  ramasec_ci=6 if ramaos==7 | ramaos==9 & emp_ci==1
replace  ramasec_ci=7 if ramaos==8 | ramaos==10 & emp_ci==1
replace  ramasec_ci=8 if ((ramaos>=11 & ramaos<=14) & (emp_ci==1))
replace  ramasec_ci=9 if ((ramaos>=15 & ramaos<=21) & (emp_ci==1))
label var ramasec_ci "Rama de actividad"
label def ramasec_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def ramasec_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def ramasec_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val ramasec_ci ramasec_ci

*************
**salmm_ci***
*************
* HON 2019
gen salmm_ci= 9443.24
label var salmm_ci "Salario minimo legal"

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (cp517_1>=1 & cp517_1<=5) 
replace cotizando_ci=1 if (cp517_2>=1 & cp517_2<=5) & cotizando_ci==.
replace cotizando_ci=1 if (cp517_3>=1 & cp517_3<=5) & cotizando_ci==.
replace cotizando_ci=1 if (cp517_4>=1 & cp517_4<=5) & cotizando_ci==.
recode cotizando_ci .=0 if condact>=1 & condact<=2
/*independiente que no cotiza en primera/segunda ocupacion*/ 
/* desocupados no cotizan*/
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

* Formalidad sin restringir a PEA
gen cotizando_ci1=.
replace cotizando_ci1=1 if (cp517_1>=1 & cp517_1<=5) 
replace cotizando_ci1=1 if (cp517_2>=1 & cp517_2<=5) & cotizando_ci==.
replace cotizando_ci1=1 if (cp517_3>=1 & cp517_3<=5) & cotizando_ci==.
replace cotizando_ci1=1 if (cp517_4>=1 & cp517_4<=5) & cotizando_ci==.
recode cotizando_ci1 .=0 if condact>=1 & condact<=3


*****************
*tipocontrato_ci*
*****************
/*
        CE34B-433 ¿Esta trabajando bajo |
                              contrato? |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
        1. Contrato indiviudal temporal |        652       11.06       11.06
2. Contrato individual permanente/acuer |      2,555       43.33       54.38
                  3. Contrato colectivo |         48        0.81       55.20
                      4. Acuerdo verbal |      2,581       43.77       98.97
                 9. No sabe/no responde |         61        1.03      100.00
----------------------------------------+-----------------------------------
                                  Total |      5,897      100.00
 
*/
/*
gen tipocontrato_ci=.
replace tipocontrato_ci= 1 if ce34==2 |ce34==3 
replace tipocontrato_ci= 2 if ce34==1
replace tipocontrato_ci= 3 if ce34==4

* MOdificacion condicionada a categopri==3 MGD
gen tipocontrato_ci=.
replace tipocontrato_ci= 1 if (ce434==2 | ce434==3 ) & categopri_ci==3
replace tipocontrato_ci= 2 if ce434==1 & categopri_ci==3
replace tipocontrato_ci= 3 if (ce434==4  & tipocontrato_ci==.) & categopri_ci==3 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
gen tipocontrato_ci=.

*************
*cesante_ci* 
*************

gen cesante_ci=1 if cp515==1 & condocup_ci==2
replace cesante_ci=0 if cp515==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci = 1 if (cp525cuantos>=1 & cp525cuantos<=5)
replace tamemp_ci = 2 if (cp525cuantos>=6 & cp525cuantos<=50)
replace tamemp_ci = 3 if (cp525cuantos>50) & cp525cuantos!=.
replace tamemp_ci=. if  cp525cuantos>=9999
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp = 1 if (cp525cuantos>=1 & cp525cuantos<=4)
replace tamemp = 2 if (cp525cuantos>=5 & cp525cuantos<=14)
replace tamemp = 3 if (cp525cuantos>=15) & cp525cuantos<=40
replace tamemp=. if  cp525cuantos>=41 & cp525cuantos!=.

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
*DZ Marzo 2019: Se genera la variable como missing value ya que (cp517) tiene múltiple respuesta, y no se puede decidir arbitrariamente a cual de las instituciones se asocia*
gen instcot_ci=.
/*
replace instcot_ci=1 if ce433_1==1
replace instcot_ci=2 if ce433_2==1
replace instcot_ci=3 if ce433_3==1
replace instcot_ci=4 if ce433_4==1
replace instcot_ci=5 if ce433_5==1
replace instcot_ci=6 if ce433_6==1
replace instcot_ci=7 if ce433_7==1
replace instcot_ci=8 if ce433_8==1
replace instcot_ci=9 if ce433_9==1
replace instcot_ci=10 if ce433_10==1


label define instcot_ci 1 "rap" 2 "injupemp" 3 "inprema" 4"ipm" 5 "ihss" 6 "Fondo privado de pensiones" 7 "Seguro medico privado" 8 "Gremio o asociacion de trabajadores" 9 "Ninguna de las anteriores" 10 "Otro"
label value instcot_ci instcot_ci  
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 
*/
****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

*************
**ypen_ci*
*************
/*gen pension1= oih1_lps/3 
gen jubilacion1= oih2_lps/3
egen ypen_ci=rsum(pension1 jubilacion1), missing
label var ypen_ci "Valor de la pension contributiva"*/

* DZ Sep 2017: corrección se le agrega la pensión y jubilación recibida en usd**  
gen pension1= oih01_lps/3 if oih01_lps!=0
gen pension1_us= (oih01_us*cambio)/3 if oih01_us!=0
gen jubilacion1= oih02_lps/3 if oih02_lps!=0
gen jubilacion1_us= (oih02_us*cambio)/3 if oih02_us!=0
egen ypen_ci=rowtotal(pension1 pension1_us jubilacion1 jubilacion1_us), missing
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************

gen pension_ci=1 if ypen_ci!=0 & ypen_ci!=.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

*gen pensionsub_ci=1 if ypensub_ci!=. 
* 2014, 01 Revision MLO
gen pensionsub_ci=1 if ypensub_ci!=.  & ypensub_ci!=0
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
*DZ OCT 2017: Hogares por debajo del precio de la canasta basica de consumo. Nota: en la EHPM no incluyeron información de la canasta de consumo en los cuadros de pobreza
gen lp_ci=  3592.85  if zona_c==1
replace lp_ci= 1809.58 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"


*********
*lpe_ci**
*********

*DZ OCT 2017: Hogares por debajo del precio de la canasta basica de alimentos.
*2019
gen lpe_ci = 1796.42 if zona_c==1
replace lpe_ci= 1355.49 if zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((cp512 ==2 | cp512==3) & condocup_ci==3)
replace categoinac_ci = 2 if  (cp512==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (cp512==6 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
/*
gen formal_ci=1 if afiliado_ci==1 & condocup_ci==1 
label var formal_ci "Formal"
*/
*Modificación Mayra Sáenz- Febrero 2014

capture gen formal=1 if cotizando_ci==1
gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

* Formalidad sin restringir a PEA
g formal_1= cotizando_ci1


************************************************************************
**************************INGRESOS**************************************
************************************************************************

*Daniela Zuluaga- Noviembre 2017: Se deciden reemplazar las variables del ingreso laboral (Monetario y no Monetario) por las que ya están construidas en la base original**

***************
***ylmpri_ci***
***************
egen ylmpri_ci=rowtotal(ysmop ycmop), missing
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

*****************
***nrylmpri_ci***
*****************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


************
*ylnmpri_ci*
************
egen ylnmpri_ci=rowtotal(yseop yceop), missing
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********
egen ylmsec_ci=rowtotal(ysmos ycmos), missing
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
*ylnmsec_ci*
************
egen ylnmsec_ci=rowtotal(yseos yceos), missing
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"


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

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"

*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************
* DZ MAR 2019: corrección cambio de categorías respecto al anio anterior**
*1	Pensión  yhpens 
gen jubi = oih01_lps/3
gen jubid = (oih01_us*cambio)/3     
*2	Jubilación yhjubi 
gen pens = oih02_lps/3
gen pensd = (oih02_us*cambio)/3  
*3	Alquileres yhalqu  
gen alqui = oih03_lps/3   
gen alquid = (oih03_us*cambio)/3                                                                                                                                    
*4	Descuentos por la 3a edad 
gen desc3ed = oih04/3                                    
*5	Pensión por divorcio 
gen pendiv= oih05_lps/3 
gen pendivd= (oih05_us*cambio)/3                                               
*7	Ayudas familiares yhayuf  
gen ayuf=oih06_lps/3 
gen ayufd=(oih06_us*cambio)/3 
*especies
gen ayufes=oih06_lps_esp/3
gen ayufesd= (oih06_us_esp*cambio)/3                                           
*8	Ayudas particulares  yhayup
gen ayup= oih07_lps/3
gen ayupd=(oih07_us*cambio)/3
*especies
gen ayupes=oih07_lps_esp/3
gen ayupesd= (oih07_us_esp*cambio)/3                                     
*9	Bolsón PRAF -utiles escolares
gen bolspra= oih09/3
*10	Uniformes escolares
gen meresc=oih10/3                                                                                                   
*11	Becas  
gen beca= oih11/3 
*12	Remesas del exterior yhreme 
gen remesa= oih12_lps/3 
gen remesad=(oih12_us*cambio)/3
*especies
gen remesp=oih12_lps_esp/3
gen remespd= (oih12_us_esp*cambio)/3
*13 Bono vida mejor
gen bonvm= oih13/3
*14 Bono a personas con capacidades especiales 	
gen bonpce=oih14/3 
*15	Alimento solidario/Bolsa solidaria(Adulto Mayor)
gen alis= oih15/3 
*16	Otros programas del gobierno  
gen otbon= oih16_lps/3 
gen otbond=(oih16_us*cambio)/3
*Otros programas del gobierno en especie
gen otbone= oih16_lps_esp/3 
gen otboned=(oih16_us_esp*cambio)/3
*17	Otros:  
gen otros= oih17_lps/3 
gen otrosd=(oih17_us*cambio)/3                                                              
* otros en especies       
gen otrose=oih17_lps_esp/3
gen otrosed=(oih17_us_esp*cambio)/3


egen ynlm_ci=rsum(jubi jubid pens pensd alqui alquid desc3ed   pendiv pendivd ayuf ayufd  ayup ayupd bonpce meresc bolspra beca remesa remesad bonvm  alis  otbon otbond otbone otboned otros otrosd), missing
label var ynlm_ci "Ingreso No Laboral Monetario"


**************
***ynlnm_ci***
**************
egen ynlnm_ci=rsum(ayufes ayufesd ayupes ayupesd remesp remespd otrose otrosed), missing
replace ynlnm_ci=. if ayufes==. & ayufesd==. & ayupes==. & ayupesd==. & remesp==. & remespd==. & otrose==. & otrosed==. 
label var ynlnm_ci "Ingreso No Laboral No Monetario" 



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

by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing 
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"



********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

***********************
*** autoconsumo_ci ***
***********************

**DZ Noviembre 2017: Se genera el autoconsumo con variables originales
gen autoconsumop_ci=cp532
replace autoconsumop_ci=0 if cp532 ==. & edad_ci>4 & (categopri==1 | categopri==2) & (emp_ci==1) 
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=cp532
replace autoconsumos_ci=0 if cp532==. & edad_ci>4 & (categosec==1 | categosec==2) & cp535==1 
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"
egen autocons_ci=rsum(autoconsumop_ci autoconsumos_ci), missing
replace autocons_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autocons_ci "Autoconsumo Individual (Trabajadores Independientes)"


******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing 
la var autocons_ch "Autoconsumo del Hogar"

****************
***remesas_ci***
****************

egen remesas_ci=rsum(remesa remesad remesp remespd), missing
label var remesas_ci "Remesas Individuales"


****************
***remesas_ch***
****************

bys idh_ch: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 



*****************
***ylmhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 

******************
*Ingreso Nacional*
******************
gen yoficial_ch=ytothg 
label var yoficial_ch "Ingreso del hogar total generado por el país"

gen ypeoficial_ch=yperhg
label var yoficial_ch "Ingreso per cápita generado por el país"

**************************INGRESOS-TRANSFERENCIAS**************************************

* Daniela Zuluaga-Noviembre 2017: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

***************
***trapri_ci***
***************
egen trapri_ci= rowtotal(remesas_ci pendiv pendivd ayuf ayufd ayufes ayufesd ayup ayupd ayupes ayupesd), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

***************
***progpub_ci***
***************
egen progpub_ci= rowtotal(bonvm bolspra bonpce), missing
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas" 

***************
***progpub_ch***
***************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci otbon otbond otbone otboned meresc desc3ed beca ypensub_ci), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(alqui alquid ), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(otros otrosd otrose otrosed), missing
label var otros_ci "Otros Ingresos" 

***************
***otros_ch***
***************
bys idh_ch: egen otros_ch=sum(otros_ci) if miembros_ci==1, missing
label var otros_ch "Otros Ingresos del hogar" 

***************
***ypen_ch***
***************
bys idh_ch: egen ypen_ch=sum(ypen_ci) if miembros_ci==1, missing
label var ypen_ch "Ingresos del hogar por jubilaciones y pensiones contributivas" 


***************
***ytotal_ci***
***************
egen ytotal_ci= rowtotal (ylm_ci ylnm_ci trapri_ci trapub_ci capital_ci otros_ci ypen_ci), missing
label var ytotal_ci "Ingreso total individual" 

***************
***ytotal_ch***
***************
egen ytotal_ch=rowtotal(ylm_ch  ylnm_ch  trapri_ch  trapub_ch  capital_ch  otros_ch  ypen_ch) if miembros_ci==1, missing
label var ytotal_ch "Ingreso total del hogar"

***************
***ytotalpc_ch***
***************
gen ytotalpc_ch=(ytotal_ch/nmiembros_ch) if miembros_ci==1
label var ytotalpc_ch "Ingreso per capita del hogar"




***************
***quintil_ci***
***************
xtile quintil_ci=ytotalpc_ch if ytotalpc_ch>0 & ytotalpc_ch!=. [pw=(factor_ch)], nq(5)
label var quintil_ci "Quintil de ingreso"
label define quintil_ci 1 "Quintil 1" 2 "Quintil 2" 3 "Quintil 3" 4 "Quintil 4" 5 "Quintil 5"
label values quintil_ci quintil_ci


*****************
*	Educación   *
*****************
*Modificado por Agustina Thailinger EDU/SCL Mayo 2022

*rename *, lower

***************
***asiste_ci***
***************
*DZ Mar 2019:Se agrega centro de educación temprana**
generat asiste_ci=.
replace asiste_ci=1 if cp405==1 | cp401==1
replace asiste_ci=0 if cp405==2 | cp401==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci***
*************
*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU	
replace cp407=. if cp407==99
replace cp412=. if cp412==99
replace cp410=. if cp410>9
replace cp417=. if cp417>9

*Para quienes ya no asisten:
gen aedu_ci=.
replace aedu_ci=0 if cp407>=1 & cp407<=3
replace aedu_ci=cp410 if cp407==4 
replace aedu_ci=cp410+6 if cp407==5
replace aedu_ci=cp410+9 if cp407==6
replace aedu_ci=cp410+11 if cp407==7 | cp407==8 | cp407==9
replace aedu_ci=cp410+11+4 if cp407==10

*Para quienes asisten actualmente:
*DZ Jul 2017: Cambio de categoria respecto al anio anterior
replace aedu_ci=0 if cp412==1 | cp412==2 | cp412==3
replace aedu_ci=cp417-1 if cp412==4
replace aedu_ci=cp417+6-1 if cp412==5
replace aedu_ci=cp417+9-1 if cp412==6
replace aedu_ci=cp417+11-1 if cp412==7 | cp412==8 | cp412==9
replace aedu_ci=cp417+11+4-1 if cp412==10
label var aedu_ci "Años de educacion aprobados"	
		
// imputando los años perdidos
replace aedu_ci=0 if (cp407==1 | cp407==2 | cp407==3) & cp410==. // alfabetizacion 
replace aedu_ci=0 if (cp412==2 | cp412==3) & cp417==. // alfabetizacion
replace aedu_ci=0 if (cp407==4 & cp410==.) | (cp412==4 & cp417==.) // educacion basica
replace aedu_ci=6 if (cp407==5 & cp410==.) | (cp412==5 & cp417==.) // ciclo comun
replace aedu_ci=9 if (cp407==6 & cp410==.) | (cp412==6 & cp417==.) // diversificado
replace aedu_ci=11 if (inlist(cp407,7, 8,9) & cp410==.) | (inlist(cp412,7,8,9) & cp417==.) // terciaria
replace aedu_ci=15 if (cp407==10 & cp410==.) | (cp412==10 & cp417==.) // postgrado
 
**************
***eduno_ci***
**************
g byte eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
la var eduno_ci "Personas sin educacion. Excluye preescolar"

**************
***edupi_ci*** 
**************
g byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
la var edupi_ci "Personas que no han completado Primaria"

**************
***edupc_ci*** 
**************
g byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
la var edupc_ci "Primaria Completa"

**************
***edusi_ci*** 
**************
g byte edusi_ci=(aedu_ci>6 & aedu_ci<=10)
replace edusi_ci=. if aedu_ci==.
la var edusi_ci "Secundaria Incompleta"

**************
***edusc_ci***
**************
g byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
la var edusc_ci "Secundaria Completa"

***************
***edus1i_ci*** 
***************
g byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
la var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"

***************
***edus1c_ci*** 
***************
g byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
la var edus1c_ci "1er ciclo de Educacion Secundaria Completo"

***************
***edus2i_ci*** 
***************
g byte edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
la var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

***************
***edus2c_ci*** 
***************
g byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
la var edus2c_ci "2do ciclo de Educacion Secundaria Completo"

**************
***eduui_ci*** 
**************
g byte eduui_ci=(aedu>=12 & aedu<=15) & cp409==2 // no finalizó estudios
replace eduui_ci=1 if (aedu>=12 & aedu<=15) & cp409==.
replace eduui_ci=. if aedu_ci==. 
la var eduui_ci "Universitaria o Terciaria Incompleta"

**************
***eduuc_ci*** 
**************
g byte eduuc_ci=(aedu>=12 & aedu<=15) & cp409==1
replace eduuc_ci=1 if aedu_ci>15
replace eduuc_ci=. if aedu_ci==.
la var eduuc_ci "Universitaria o Terciaria Completa"

***************
***edupre_ci***
***************
g byte edupre_ci=.
la var edupre_ci "Tiene Educacion preescolar"

***************
***asipre_ci***
***************
gen byte asispre_ci=(cp401==1 & cp412==3) 
la var asispre_ci "Asiste a educacion prescolar"

**************
***pqnoasis*** 
**************
gen pqnoasis_ci=cp406 
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

***************
***repite_ci*** 
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************
***repiteult_ci*** 
******************
gen repiteult_ci=.
replace repiteult_ci=1 if cp415==1
replace repiteult_ci=0 if cp415==2
label var repiteult_ci "Personas que están repetiendo el ultimo grado"

***************
***edupub_ci*** 
***************
gen edupub_ci=.
replace edupub_ci=1 if (cp418==1 | cp418==2 | cp418==3 | cp418==4 | cp418==8  | cp418==10) & cp405==1
replace edupub_ci=0 if (cp418==5 | cp418==6 | cp418==7 | cp418==9 | cp418==11 | cp418==12) & cp405==1
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci= 1 if (cp407==9 | cp407==10 | cp412==9| cp412==10)
replace eduac_ci= 0 if (cp407==7 | cp407==8 ) | (cp412==7| cp412==8)
label variable eduac_ci "Superior universitario vs superior no universitario"

******************
***pqnoasis1_ci***
******************
*DZ Noviembre 2017: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz
g       pqnoasis1_ci = 1 if pqnoasis_ci==7
replace pqnoasis1_ci = 2 if pqnoasis_ci==11
replace pqnoasis1_ci = 3 if pqnoasis_ci==6
replace pqnoasis1_ci = 4 if pqnoasis_ci==3
replace pqnoasis1_ci = 5 if pqnoasis_ci==4 | pqnoasis_ci==10
replace pqnoasis1_ci = 6 if pqnoasis_ci==2
replace pqnoasis1_ci = 7 if pqnoasis_ci==8 | pqnoasis_ci==9
replace pqnoasis1_ci = 8 if pqnoasis_ci==5
replace pqnoasis1_ci = 9 if pqnoasis_ci==1 | pqnoasis_ci==12

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
************
*aguared_ch*
************
gen aguared_ch=.
replace aguared_ch=1 if dh204==1
replace aguared_ch=0 if dh204==2 

*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch=1 if dv106==1
replace aguadist_ch=2 if dv106==2
replace aguadist_ch=3 if (dv106==3| dv106==4)

*************
*aguamala_ch*
*************
gen aguamala_ch=.
replace aguamala_ch=1 if dv105>=5 & dv105<=8
replace aguamala_ch=0 if dv105>=1 & dv105<=4

*************
*aguamide_ch*
*************
gen aguamide_ch=.

********
*luz_ch*
********
gen luz_ch=1 if  dv107==1 |  dv107==2 |  dv107==3 
replace luz_ch=0 if  dv107>=4 &   dv107<=8

************
*luzmide_ch*
************
gen luzmide_ch=.

************
*combust_ch*
************
gen combust_ch=1 if dh203==3 | dh203==2 | dh203==4
replace combust_ch=0 if dh203==5 | dh203==1

*********
*bano_ch*
*********
gen bano_ch=.
replace bano_ch=1 if dh204==1
replace bano_ch=0 if dh204==2

gen banoex_ch=.
replace banoex_ch=1 if dh206==1
replace banoex_ch=0 if dh206==2

* DZ Jul 2017: corrección nueva categoría respecto al anio anterior**
gen des1_ch=.
replace des1_ch=0 if dh204==2
replace des1_ch=1 if (dh205==1|dh205==2)
replace des1_ch=2 if (dh205==5|dh205==6|dh205==7 |dh205==8)
replace des1_ch=3 if (dh205==3|dh205==4)
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

* MGR Jul 2015: corrección sintáxis

/*	
gen des2_ch=.
replace des2_ch=1 if (dh205==1|dh205==2|dh205==3)
replace des2_ch=2 if (dh205==4|dh205==5|dh205==6|dh205==7|dh205==7)
replace des2_ch=0 if dh204==2
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
*/

* DZ Jul 2017: corrección nueva categoría respecto al anio anterior**
gen des2_ch=.
replace des2_ch=1 if (dh205==1|dh205==2|dh205==5|dh205==6|dh205==7 |dh205==8)
replace des2_ch=2 if (dh205==4|dh205==3|dh205==8)
replace des2_ch=0 if dh204==2
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
	
gen piso_ch=.
replace piso_ch=0 if dv103==7
replace piso_ch=1 if dv103>=1 & dv103<=6 
replace piso_ch=2 if dv103==8 

gen techo_ch=.
replace techo_ch=0 if dv104==6 | dv104==7
replace techo_ch=1 if dv104>=1 & dv104<=5
replace techo_ch=2 if dv104==8| dv104==9 | dv104==10

* DZ Jul 2017: corrección nueva categoría respecto al anio anterior**
gen pared_ch=.
replace pared_ch=0 if dv102>=6 & dv102<=7
replace pared_ch=1 if dv102>=1 & dv102<=5
replace pared_ch=2 if dv102==8

label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros"
label val pared_ch pared_ch
	
gen resid_ch=.
replace resid_ch=0 if ( dv108 ==1| dv108 ==3)
replace resid_ch=1 if ( dv108 ==4| dv108 ==6)
replace resid_ch=2 if ( dv108 ==2| dv108 ==7)
replace resid_ch=3 if ( dv108 ==5| dv108 ==8)

gen dorm_ch=.
replace dorm_ch=dv112

gen cuartos_ch=.
replace cuartos_ch=dv111

***********
*cocina_ch*
***********
gen cocina_ch=(dh201==1)
replace cocina_ch=. if dh201==.

**********
*telef_ch*
**********

gen telef_ch=((dh207_7>=1 & dh207_7<=2) | (dh207_8>=1 & dh207_8<=6))

***********
*regrig_ch*
***********
gen refrig_ch=(dh207_1>=1 & dh207_1<=11)

**********
*freez_ch*
**********
gen freez_ch=.

*********
*auto_ch*
*********
* DZ Jul 2017: corrección de categoría respecto al anio anterior**
gen auto_ch= (dh207_9>=1 & dh207_9<=6)

**********
*compu_ch*
**********
* DZ Jul 2017: corrección de categoría respecto al anio anterior**
gen compu_ch=(dh207_12>=1 & dh207_12<=6)

*************
*internet_ch*
*************

gen internet_ch=(at03==1 & at05_2==1)
replace internet_ch=. if at03==. & at05_2==.

********
*cel_ch*
********
gen cel_ch=(at09)
replace cel_ch=. if at09==.

**********
*vivi1_ch*
**********
gen vivi1_ch=.
replace vivi1_ch=1 if dv101==1 | dv101==2
replace vivi1_ch=2 if dv101==4
replace vivi1_ch=3 if dv101==5 | dv101==3 | dv101==7
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

**********
*vivi2_ch*
**********
gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

*************
*viviprop_ch*
*************
gen viviprop_ch=.
replace viviprop_ch=0 if dv109==1
replace viviprop_ch=1 if dv109==3
replace viviprop_ch=2 if dv109==2
replace viviprop_ch=3 if (dv109==4 | dv109==5 | dv109==6 | dv109==7)
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch
	
******************************
*	vivitit_ch
******************************
gen vivitit_ch=.
replace vivitit_ch=1 if dv110==1 | dv110==2
replace vivitit_ch=0 if dv110==3
label var vivitit_ch "El hogar posee un titulo de propiedad"


******************************
*	vivialq_ch
******************************
gen vivialq_ch=.
replace vivialq_ch= dv107pago
label var vivialq_ch "Alquiler mensual"
*Renta = Monto de la renta mensual de la vivienda

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
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

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g aguamejorada_ch = 1 if (dv105 >=1 & dv105 <=4) | dv105==8
replace aguamejorada_ch = 0 if (dv105 >=5 & dv105 <=7) | (dv105 >=9 & dv105 <=10)

*********************
***banomejorado_ch***
*********************
g banomejorado_ch = 1 if ( dh204 ==1 & ((dh205 >=1 & dh205 <=2) | (dh205 >=5 & dh205 <=8)) & dh206 ==1)
replace banomejorado_ch = 0 if ( dh204 ==1 & ((dh205 >=1 & dh205 <=2) | (dh205 >=5 & dh205 <=8)) & dh206 ==2) | (dh205 >=3 & dh205 <=4) | (dh204==2)


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=.
	label var migrante_ci "=1 si es migrante"
	/* Base con error en la pregunta de migrante, no se puede rescatar */
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	/* Encuesta pregunta sobre años viviendo en este lugar, no sabemos si pudo vivir en Honduras y mudarse de ciudad */
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
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

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
trapri_ci trapri_ch progpub_ci progpub_ch trapub_ci  trapub_ch capital_ci capital_ch otros_ci otros_ch ypen_ch ytotal_ci  ytotal_ch ytotalpc_ch quintil_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
clonevar codocupa = ce425cod 
clonevar codindustria = ce428cod

compress

**DZ Agosto 2019: Se truncan labels de las variables para poder guardarlos en una versión antigua de stata**
foreach i of varlist _all { 
local longlabel: var label `i' 
local shortlabel = substr("`longlabel'",1,79) 
label var `i' "`shortlabel'"
}
saveold "`base_out'", version(12) replace


log close





