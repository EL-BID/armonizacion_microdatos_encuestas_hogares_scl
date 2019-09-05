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

local PAIS HND
local ENCUESTA EPHPM
local ANO "2014"
local ronda m6 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m6
Autores: Marcela G. Rubio
Última versión: Daniela Zuluaga (DZ) - Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017

			  
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


*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

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
gen anio_c=2014

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
replace condocup_ci=1 if (ce401==1 | ce402==1 | ce403==1)
replace condocup_ci=2 if (ce401==2 | ce402==2 | ce403==2) & (ce405==1 | ce406==1) 
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
replace categopri_ci=1 if ce432==10 | ce432==11 | ce432==6 | ce432==7
replace categopri_ci=2 if ce432==4 | ce432==5   | ce432==8  | ce432==9 
replace categopri_ci=3 if ce432==1  | ce432==2  | ce432==3 
replace categopri_ci=4 if ce432==12 | ce432==13
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

*************
*horaspri_ci*
*************
gen horaspri_ci=ce429
replace horaspri_ci=. if ce429>168

************
*horastot_ci
************
gen horassec_ci=ce453
replace horassec_ci=. if ce453 >168
egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0
replace horastot_ci = . if horastot_ci>168

****************
* categosec_ci *
****************
gen categosec_ci=.
replace categosec_ci=1 if (ce456==6 | ce456==7 | ce456==10 | ce456==11)
replace categosec_ci=2 if (ce456==4 | ce456==5 | ce456==8 | ce456==9)
replace categosec_ci=3 if (ce456>=1 & ce456<=3)
replace categosec_ci=4 if (ce456==12| ce456==13)
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci


************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=ce411/30   if ce411tiempo==1
replace durades_ci=ce411/4.3  if ce411tiempo==2
replace durades_ci=ce411      if ce411tiempo==3
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
replace desalent_ci=1 if ce409==6
replace desalent_ci=0 if ce409!=6 & ce409!=.

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
replace subemp_ci=1 if horaspri_ci<=30 & ce472==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

***************
*tiempoparc_ci*
***************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp==0 | emp==1
replace tiempoparc_ci=1 if horastot<30 & ce472==2
label var tiempoparc_ci "Trabajadores a medio tiempo"

*************
*nempleos_ci*
*************
generat nempleos_ci=1 if ce448==2
replace nempleos_ci=2 if ce448==1
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
gen spublico_ci=1 if ce432==1 
replace spublico_ci=0 if ce432!=1 

**********
*ocupa_ci*
**********
tostring ce425cod , replace
gen labor=substr(ce425cod,1,4)
destring labor, replace 

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
*DZ Agosto 2019: Se reemplaza la viariable rama por ramaop (creada originalmente en la base)
/*
gen rama_ci=.
replace  rama_ci=1 if (ce428cod>=111001 & ce428cod<=950010) & emp_ci==1
replace  rama_ci=2 if (ce428cod>=1010000 & ce428cod<=1430017) & emp_ci==1
replace  rama_ci=3 if (ce428cod>=1511000 & ce428cod<=3910075) & emp_ci==1
replace  rama_ci=4 if (ce428cod>=4010001 & ce428cod<=4340002) & emp_ci==1
replace  rama_ci=5 if (ce428cod>=4500028 & ce428cod<=4820020) & emp_ci==1
replace  rama_ci=6 if (ce428cod>=5002005 & ce428cod<=5939057) & emp_ci==1
replace  rama_ci=7 if (ce428cod>=6003003 & ce428cod<=6499003) & emp_ci==1
replace  rama_ci=8 if (ce428cod>=6500008 & ce428cod<=7020023) & emp_ci==1
replace  rama_ci=9 if (ce428cod>=7111000 & ce428cod<=9900027) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
*/

gen rama_ci=.
replace  rama_ci=1 if ramaop==1 & emp_ci==1
replace  rama_ci=2 if ramaop==2 & emp_ci==1
replace  rama_ci=3 if ramaop==3 & emp_ci==1
replace  rama_ci=4 if ramaop==4 & emp_ci==1
replace  rama_ci=5 if ramaop==5 & emp_ci==1
replace  rama_ci=6 if ramaop==6 & emp_ci==1
replace  rama_ci=7 if ramaop==7 & emp_ci==1
replace  rama_ci=8 if ramaop==8 & emp_ci==1
replace  rama_ci=9 if ramaop==9 & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

*************
**salmm_ci***
*************
* HON 2014
gen salmm_ci= 7311.29275
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
replace cotizando_ci=1 if (ce423_1==1 | ce423_2==2 | ce423_3==3 | ce423_4==4 | ce423_5==5 | ce423_6==6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
/*independiente que no cotiza en primera/segunda ocupacion*/ 
/* desocupados no cotizan*/
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

* Formalidad sin restringir a PEA
gen cotizando_ci1=.
replace cotizando_ci1=1 if (ce423_1==1 | ce423_2==2 | ce423_3==3 | ce423_4==4 | ce423_5==5 | ce423_6==6)
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
*/
* MOdificacion condicionada a categopri==3 MGD
gen tipocontrato_ci=.
replace tipocontrato_ci= 1 if (ce433==2 | ce433==3 ) & categopri_ci==3
replace tipocontrato_ci= 2 if ce433==1 & categopri_ci==3
replace tipocontrato_ci= 3 if (ce433==4  & tipocontrato_ci==.) & categopri_ci==3 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if ce412==1 & condocup_ci==2
replace cesante_ci=0 if ce412==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci = 1 if (ce431_cantidad>=1 & ce431_cantidad<=5)
replace tamemp_ci = 2 if (ce431_cantidad>=6 & ce431_cantidad<=50)
replace tamemp_ci = 3 if (ce431_cantidad>50) & ce431_cantidad!=.
replace tamemp_ci=. if  ce431_cantidad>=9999
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp = 1 if (ce431_cantidad>=1 & ce431_cantidad<=4)
replace tamemp = 2 if (ce431_cantidad>=5 & ce431_cantidad<=14)
replace tamemp = 3 if (ce431_cantidad>=15) & ce431_cantidad<=40
replace tamemp=. if  ce431_cantidad>=41 & ce431_cantidad!=.

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=1 if ce423_1==1
replace instcot_ci=2 if ce423_2==2
replace instcot_ci=3 if ce423_3==3
replace instcot_ci=4 if ce423_4==4
replace instcot_ci=5 if ce423_5==5
replace instcot_ci=6 if ce423_6==6
replace instcot_ci=7 if ce423_7==7
replace instcot_ci=8 if ce423_8==8
replace instcot_ci=9 if ce423_9==9
replace instcot_ci=10 if ce423_10==10
replace instcot_ci=11 if ce423_11==11

label define instcot_ci 1 "rap" 2 "injupemp" 3 "inprema" 4"ipm" 5 "ihss" 6 "Fondo privado de pensiones" 7 "Seguro medico privado" 8 "Sindicato" 9 "Gremio o asociacion de trabajadores" 10 "Ninguna de las anteriores" 11 "Otro"
label value instcot_ci instcot_ci  
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

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
gen pension1= oih1_lps/3 if oih1_lps!=0
gen pension1_us= (oih1_us*cambio)/3 if oih1_us!=0
gen jubilacion1= oih2_lps/3 if oih2_lps!=0
gen jubilacion1_us= (oih2_us*cambio)/3 if oih2_us!=0
egen ypen_ci=rsum(pension1 pension1_us jubilacion1 jubilacion1_us), missing
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

*DZ Octubre 2017- Se modifica la variable ypensub_ci, se genera sumando bonos praf para mayores de 64 y bono ciudadano de oro. 
*gen ypensub_ci=oih10 if oih10>0
gen bono_praf_adulto=oih10 if edad_ci>=65 & edad_ci!=.
gen bono_ciudadano_oro=(oih16_us*cambio)
egen ypensub_ci=rowtotal(bono_praf_adulto oih16_lps bono_ciudadano_oro), m
replace ypensub_ci=ypensub_ci/3
drop bono_praf_adulto
drop bono_ciudadano_oro
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

*DZ Jul 2017: Hogares por debajo del precio de la canasta basica de consumo.
gen lp_ci= 3140.1 if zona_c==1
replace lp_ci= 1631.4 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

*DZ Jul 2017: Hogares por debajo del precio de la canasta basica de alimentos.
gen lpe_ci = 1570.1 if zona_c==1
replace lpe_ci= 1222.0 if zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((ce407 ==2 | ce407==3) & condocup_ci==3)
replace categoinac_ci = 2 if  (ce407==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (ce407==6 & condocup_ci==3)
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

*1	Pensión  yhpens 
gen jubi = oih1_lps/3
gen jubid = (oih1_us*cambio)/3     
*2	Jubilación yhjubi 
gen pens = oih2_lps/3
gen pensd = (oih2_us*cambio)/3  
*3	Alquileres yhalqu  
gen alqui = oih3_lps/3   
gen alquid = (oih3_us*cambio)/3                                                                                                                                    
*4	Descuentos por la 3a edad 
gen desc3ed = oih4/3                                    
*5	Subsidio de la ENEE 
gen subs= oih5/3
*6	Intereses bancarios  
gen intbanc= oih6_lps/3 
gen intband= (oih6_us*cambio)/3                                               
*7	Pensión por divorcio  
gen pendiv=oih7_lps/3 
gen pendivd=(oih7_us*cambio)/3                                            
*8	Ayudas familiares yhayuf
gen ayuf= oih8_lps/3
gen ayufd=(oih8_us*cambio)/3

*especies
gen ayufes=oih8_esp_lps/3
gen ayufesd= (oih8_esp_us*cambio)/3

*9	Ayudas particulares  yhayup
gen ayup=oih9_lps/3
gen ayupd=(oih9_us*cambio)/3
   *especies
gen ayupes=oih9_esp_lps/3
gen ayupesd=( oih9_esp_us*cambio)/3                                            
*10	Bonos PRAF 
gen bonpra=oih10/3                                                                  
*11	Merienda escolar 
gen meresc= oih11/3                                       
*12	Bolsón PRAF 
gen bolspra=oih12/3
*13	Becas 
gen beca= oih13_lps/3 
gen becad=(oih13_us*cambio)/3
*14	Remesas del exterior yhreme 
gen remesa= oih14_lps/3
gen remesad= (oih14_us*cambio)/3
*especies
gen remesp=oih14_esp_lps/3
gen remespd= (oih14_esp_us*cambio)/3
*15	Bono 10 Mil
gen bon10= oih15_lps/3 
gen bon10d=(oih15_us*cambio)/3
*16	Bono Ciudadano de Oro 
gen bonci=oih16_lps/3 
gen boncid=(oih16_us*cambio)/3
*17	Otros bonos 
gen otbon= oih17_lps/3 
gen otbond=(oih17_us*cambio)/3
*18	Otros:  
gen otros= oih18_lps/3 
gen otrosd=(oih18_us*cambio)/3                                                              
* otros en especies       
gen otrose=oih18_esp_lps/3
gen otrosed=(oih18_esp_us*cambio)/3


egen ynlm_ci=rsum(jubi jubid pens pensd alqui alquid desc3ed subs intbanc intband pendiv pendivd ayuf ayufd  ayup ayupd bonpra meresc bolspra beca becad remesa remesad bon10 bon10d bonci boncid otbon otbond otros otrosd), missing
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
gen autoconsumop_ci=ce447 
replace autoconsumop_ci=0 if ce447 ==. & edad_ci>4 & (categopri==1 | categopri==2) & (emp_ci==1) 
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=ce471
replace autoconsumos_ci=0 if ce471==. & edad_ci>4 & (categosec==1 | categosec==2) & ce448==1 
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

**************************INGRESOS-TRANSFERENCIAS**************************************

* Daniela Zuluaga-Noviembre  2017: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

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

****************
***progpub_ci***
****************
/*Se suman unicamente los beneficicarios del bonopraf con edad_ci<65 dado que los beneficiarios de edad_ci>=65 ya fueron incluidos en la variable ypensub_ci.
para esto se genera una variable auxiliar*/

gen aux=bonpra if edad_ci<65
egen progpub_ci= rowtotal(bon10 bon10d bolspra aux), missing
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas"
drop aux

****************
***progpub_ch***
****************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci otbon otbond subs meresc ypensub_ci desc3ed beca becad), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

****************
***capital_ci***
****************
egen capital_ci= rowtotal(alqui alquid intbanc intband), missing
label var capital_ci "Ingreso por renta del capital" 

****************
***capital_ch***
****************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

*****************
***otros_ci***
*****************
egen otros_ci= rowtotal(otros otrosd otrose otrosed), missing
label var otros_ci "Otros Ingresos" 

*****************
***otros_ch***
*****************
bys idh_ch: egen otros_ch=sum(otros_ci) if miembros_ci==1, missing
label var otros_ch "Otros Ingresos del hogar" 

*************
***ypen_ch***
*************
bys idh_ch: egen ypen_ch=sum(ypen_ci) if miembros_ci==1, missing
label var ypen_ch "Ingresos del hogar por jubilaciones y pensiones contributivas" 


*************
***ytotal_ci***
*************
egen ytotal_ci= rowtotal (ylm_ci ylnm_ci trapri_ci trapub_ci capital_ci otros_ci ypen_ci), missing
label var ytotal_ci "Ingreso total individual" 

*************
***ytotal_ch***
*************
egen ytotal_ch=rowtotal(ylm_ch  ylnm_ch  trapri_ch  trapub_ch  capital_ch  otros_ch  ypen_ch) if miembros_ci==1, missing
label var ytotal_ch "Ingreso total del hogar"

***************
***ytotalpc_ch***
***************
gen ytotalpc_ch=(ytotal_ch/nmiembros_ch) if miembros_ci==1
label var ytotalpc_ch "Ingreso per capita del hogar"


****************
***quintil_ci***
****************
xtile quintil_ci=ytotalpc_ch[fw=round(factor_ch)], nq(5)
label var quintil_ci "Quintil de ingreso"
label define quintil_ci 1 "Quintil 1" 2 "Quintil 2" 3 "Quintil 3" 4 "Quintil 4" 5 "Quintil 5"
label values quintil_ci quintil_ci


******************************************************************************
*	Educación
*****************************************************************************

************
* asiste_ci*
************

generat asiste_ci=.
replace asiste_ci=1 if ed103==1
replace asiste_ci=0 if ed103==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"


	*************
	***aedu_ci***
	*************
		
* Años de educacion aprobados **
/*replace ed108 =. if ed108 >9
replace ed115=. if ed115>9

** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if ed105>=1 & ed105<=3
replace aedu_ci=ed108  if ed105==4 
replace aedu_ci=ed108 +6 if ed105==5 | ed105==6
replace aedu_ci=ed108 +12 if ed105==7 | ed105==8 | ed105==9
replace aedu_ci=ed108 +17 if ed105==10
** para quienes asisten actualmente
replace aedu_ci=0 if ed110==1 | ed110==2 | ed110==3 
replace aedu_ci=ed115-1 if ed110==4
replace aedu_ci=ed115+6-1 if ed110==5 | ed110==6
replace aedu_ci=ed115+12-1 if ed110==7 | ed110==8 | ed110==9
replace aedu_ci=ed115+17-1 if ed110==10
label var aedu_ci "Años de educacion aprobados"	*/

*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU	
* Años de educacion aprobados **
replace ed108=. if ed108>9
replace ed115=. if ed115>9

** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if ed105>=1 & ed105<=3
replace aedu_ci=ed108 if ed105==4 
replace aedu_ci=ed108+6 if ed105==5
replace aedu_ci=ed108+9 if ed105==6
replace aedu_ci=ed108+12 if ed105==7 | ed105==8 | ed105==9
replace aedu_ci=ed108+17 if ed105==10 

** para quienes asisten actualmente
replace aedu_ci=0 if ed110==1 | ed110==2 | ed110==3 
replace aedu_ci=ed115-1 if ed110==4
replace aedu_ci=ed115+6-1 if ed110==5
replace aedu_ci=ed115+9-1 if ed110==6
replace aedu_ci=ed115+12-1 if ed110==7 | ed110==8 | ed110==9
replace aedu_ci=ed115+17-1 if ed110==10
label var aedu_ci "Años de educacion aprobados"	
		
		
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
g byte edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edusi_ci=. if aedu_ci==.
la var edusi_ci "Secundaria Incompleta"
******************************
*	edusc_ci 
******************************
g byte edusc_ci=(aedu_ci==12)
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
g byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
la var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"
******************************
*	edus2c_ci 
******************************
g byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
la var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media
******************************
*	eduui_ci 
******************************
g byte eduui_ci=(aedu_ci>12 & aedu_ci<17) 
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
g byte edupre_ci=.
replace edupre_ci=1 if ((ed105==3 | ed110==3) & aedu_ci ~=.)
replace edupre_ci=0 if (edupre_ci~=1 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

***************
***asipre_ci***
***************

g asispre_ci=.
replace asispre_ci=1 if ed103==1 & ed110==3 & edad_ci>=4
recode asispre_ci (.=0)
la var asispre_ci "Asiste a educacion prescolar"

******************************
*	pqnoasis 
******************************
ren ed104 pqnoasis_ci
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

******************************
*	repite_ci 
******************************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************************
*	repiteult_ci 
******************************
gen repiteult_ci=.
replace repiteult_ci=1 if ed113==1
replace repiteult_ci=0 if ed113==2
label var repiteult_ci "Personas que están repetiendo el ultimo grado"

******************************
*	edupub_ci 
******************************
gen edupub_ci=.
/*
Esta pregunta no se incluye porque el concepto de esta variable es para aquellas personas que actualmente
están estudiando en un centro educativo público.
replace edupub_ci=1 if (ed109==1|ed109==2|ed109==3|ed109==4|ed109==7|ed109==8|ed109==13)
replace edupub_ci=0 if (ed109==5|ed109==6|ed109==9|ed109==10|ed109==11|ed109==12)
*/
replace edupub_ci=1 if (ed116==1|ed116==2|ed116==3|ed116==4|ed116==7|ed116==8)
replace edupub_ci=0 if (ed116==5|ed116==6|ed116==9|ed116==10|ed116==11|ed116==12)
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if ed105==7 | ed110==7
replace tecnica_ci=0 if tecnica_ci ~=1 & ( ed105!=99 & ed110!=99)
label var tecnica_ci "1=formacion terciaria tecnica"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci= 0 if tecnica_ci ==1
replace eduac_ci=1 if eduuc_ci ==1 | eduui_ci ==1
label variable eduac_ci "Superior universitario vs superior no universitario"


**DZ Noviembre 2017: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
*****************
***pqnoasis1_ci***
*****************
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
replace aguared_ch=1 if dv05==1
replace aguared_ch=0 if dv05==2 

*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch=1 if dv07==1
replace aguadist_ch=2 if dv07==2
replace aguadist_ch=3 if (dv07==3| dv07==4)

*************
*aguamala_ch*
*************
gen aguamala_ch=.
replace aguamala_ch=1 if dv06>=5 & dv06<=8
replace aguamala_ch=0 if dv06>=1 & dv06<=4

*************
*aguamide_ch*
*************
gen aguamide_ch=.

********
*luz_ch*
********
gen luz_ch=1 if  dv08==1 |  dv08==2 |  dv08==3 
replace luz_ch=0 if  dv08>=4 &   dv08<=8

************
*luzmide_ch*
************
gen luzmide_ch=.

************
*combust_ch*
************
gen combust_ch=1 if dh04==3 | dh04==2 | dh04==4
replace combust_ch=0 if dh04==5 | dh04==1

*********
*bano_ch*
*********
gen bano_ch=.
replace bano_ch=1 if dh05==1
replace bano_ch=0 if dh05==2

gen banoex_ch=.
replace banoex_ch=1 if dh07==1
replace banoex_ch=0 if dh07==2

gen des1_ch=.
replace des1_ch=0 if dh05==2
replace des1_ch=1 if (dh06==1|dh06==2)
replace des1_ch=2 if (dh06==5|dh06==6|dh06==7)
replace des1_ch=3 if (dh06==3|dh06==4)
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

* MGR Jul 2015: corrección sintáxis

/*	
gen des2_ch=.
replace des2_ch=1 if (dh06==1|dh06==2|dh06==3)
replace des2_ch=2 if (dh06==4|dh06==5|dh06==6|dh06==7|dh06==7)
replace des2_ch=0 if dh05==2
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
*/

gen des2_ch=.
replace des2_ch=1 if (dh06==1|dh06==2|dh06==5|dh06==6|dh06==7)
replace des2_ch=2 if (dh06==4|dh06==3|dh06==8)
replace des2_ch=0 if dh05==2
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
	
gen piso_ch=.
replace piso_ch=0 if dv03==7
replace piso_ch=1 if dv03>=1 & dv03<=6 
replace piso_ch=2 if dv03==8 

gen techo_ch=.
replace techo_ch=0 if dv04==6 | dv04==7
replace techo_ch=1 if dv04>=1 & dv04<=5
replace techo_ch=2 if dv04==8| dv04==9 | dv04==10

gen pared_ch=.
replace pared_ch=0 if dv02>=5 & dv02<=6
replace pared_ch=1 if dv02>=1 & dv02<=4
replace pared_ch=2 if dv02==7

label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros"
label val pared_ch pared_ch
	
gen resid_ch=.
replace resid_ch=0 if ( dv09 ==1| dv09 ==3)
replace resid_ch=1 if ( dv09 ==4| dv09 ==6)
replace resid_ch=2 if ( dv09 ==2| dv09 ==7)
replace resid_ch=3 if ( dv09 ==5| dv09 ==8)

gen dorm_ch=.
replace dorm_ch=dh01 

gen cuartos_ch=.
replace cuartos_ch=dv13

***********
*cocina_ch*
***********
gen cocina_ch=(dh02==1)
replace cocina_ch=. if dh02==.

**********
*telef_ch*
**********

gen telef_ch=(dh08_7==1 | dh08_8==1)

***********
*regrig_ch*
***********
gen refrig_ch=(dh08_1==1)

**********
*freez_ch*
**********
gen freez_ch=.

*********
*auto_ch*
*********
gen auto_ch=(dh08_9==1 | dh08_10==1)

**********
*compu_ch*
**********
gen compu_ch=(dh08_14==1)

*************
*internet_ch*
*************
gen internet_ch=(at306_1==1)
replace internet_ch=. if at306_1==.

********
*cel_ch*
********
gen cel_ch=(at308==1)
replace cel_ch=. if at308==.

**********
*vivi1_ch*
**********
gen vivi1_ch=.
replace vivi1_ch=1 if dv01==1 | dv01==2
replace vivi1_ch=2 if dv01==4
replace vivi1_ch=3 if dv01==5 | dv01==3 | dv01==7
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
replace viviprop_ch=0 if dv11==1
replace viviprop_ch=1 if dv11==3
replace viviprop_ch=2 if dv11==2
replace viviprop_ch=3 if (dv11==4 | dv11==5 | dv11==6 | dv11==7)
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch
	
gen vivitit_ch=.

gen vivialq_ch=.
replace vivialq_ch=dv12       if dv12moneda==1 
replace vivialq_ch=dv12*cambio if dv12moneda==2
replace vivialq_ch=. if dv12==9999999999

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
g aguamejorada_ch = 1 if (dv06 >=1 & dv06 <=4) | dv06==8
replace aguamejorada_ch = 0 if (dv06 >=5 & dv06 <=7) | (dv06 >=9 & dv06 <=10)

*********************
***banomejorado_ch***
*********************
g banomejorado_ch = 1 if ( dh05 ==1 & ((dh06 >=1 & dh06 <=2) | (dh06 >=5 & dh06 <=8)) & dh07 ==1)
replace banomejorado_ch = 0 if ( dh05 ==1 & ((dh06 >=1 & dh06 <=2) | (dh06 >=5 & dh06 <=8)) & dh07 ==2) | (dh06 >=3 & dh06 <=4) | (dh05==2)

	
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
trapri_ci trapri_ch progpub_ci progpub_ch trapub_ci  trapub_ch capital_ci capital_ch otros_ci otros_ch ypen_ch ytotal_ci  ytotal_ch ytotalpc_ch quintil_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close


