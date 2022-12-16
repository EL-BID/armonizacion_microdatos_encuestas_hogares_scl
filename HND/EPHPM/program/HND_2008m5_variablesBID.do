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
local ANO "2008"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Yessenia Loaysa (abr-2013)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
RAM 27/05/2010
*Inclusión Mayra Sáenz - Julio 2013
****************************************************************************/


use "`base_in'", clear


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
*Inclusión Mayra Sáenz - Julio 2013

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

***********
*factor_ch*
***********

gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"


********
*idh_ch*
********
destring hogar, replace

gen double idh_ch=hogar
format idh_ch %20.0g 

********
*idp_ci*
********

gen idp_ci=Nper

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

gen anio_c=2008

*******
*mes_c*
*******

gen mes_c=5


*************
*relacion_ci*
*************

gen relacion_ci=.
replace relacion_ci=1 if Rela_j==1
replace relacion_ci=2 if Rela_j==2
replace relacion_ci=3 if Rela_j==3 | Rela_j==4
*MGR: correción en sintáxis 5/2016
*replace relacion_ci=4 if Rela_j==5 | Rela_j==6 | Rela_j== 8 
*replace relacion_ci=5 if Rela_j==7 | Rela_j==9 | Rela_j==11
replace relacion_ci=4 if Rela_j==5 | Rela_j==6 | Rela_j==7 | Rela_j== 8 
replace relacion_ci=5 if Rela_j==9 | Rela_j==11
replace relacion_ci=6 if Rela_j==10
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

gen sexo_ci=Sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

*********
*edad_ci*
*********

gen edad_ci=Edad if Edad <99
label var edad_ci "Edad del Individuo"
drop Edad

**********
*civil_ci*
**********

gen civil_ci=.
gen Civil=est_civil
replace civil_ci=1 if Civil==5
replace civil_ci=2 if Civil==1 | Civil==6
replace civil_ci=3 if Civil==3 | Civil==4
replace civil_ci=4 if Civil==2
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



         ******************************
         *** VARIABLES DE DIVERSIDAD **
         ******************************
*Nathalia Maya & Antonella Pereira
*Feb 2021	

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
replace condocup_ci=1 if (p34==1 | p35==1 | p36==1)
replace condocup_ci=2 if (p34==2 | p35==2 | p36==2) & (p38==1 | p39==1) 
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


/*2009


. tab  p66

      p66. En la O.P. Ud trabaja como: |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
     1. Empleado(a) u obrero(a) público |      2,495        6.41        6.41
     2. Empleado(a) u obrero(a) privado |     14,716       37.78       44.18
            3. Empleado(a) Doméstico(a) |      1,030        2.64       46.83
4. Miembro de cooperativa de producción |         14        0.04       46.86
5. Cuenta propia que no contrata mano d |      8,112       20.83       67.69
6. Cuenta Propia que contrata mano de o |      1,300        3.34       71.03
            7. Empleador o socio activo |        832        2.14       73.16
              8. Miembro de cooperativa |         38        0.10       73.26
9. Cuenta propia que no contrata mano d |      3,389        8.70       81.96
10. Cuenta propia que contrata mano de  |      2,261        5.80       87.76
         11. Patrón o socio de la finca |        107        0.27       88.04
  12. Trabajador familiar no remunerado |      4,486       11.52       99.56
           13. Trabajador no remunerado |        173        0.44      100.00
----------------------------------------+-----------------------------------
                                  Total |     38,953      100.00
*/
gen categopri_ci=.
replace categopri_ci=1 if		p66==10 | p66==11 | p66==6 | p66==7
replace categopri_ci=2 if		p66==5  | p66==9 | p66==4 | p66==8
replace categopri_ci=3 if 		p66==1  | p66==2  | p66==3
replace categopri_ci=4 if  		p66==12 | p66==13  
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

**************
*categosec_ci*
**************

/* CATEGOSEC_CI
   Ocupacion Secundaria,
   

 P462 En la ocupación de... ¿Ud trabaja |
                                  como: |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
     1. Empleado(a) u obrero(a) público |        115        1.20        1.20
     2. Empleado(a) u obrero(a) privado |      1,897       19.84       21.05
            3. Empleado(a) Doméstico(a) |         27        0.28       21.33
4. Miembro de cooperativa de producción |          7        0.07       21.40
5. Cuenta propia que no contrata mano d |      1,831       19.15       40.55
6. Cuenta Propia que contrata mano de o |        167        1.75       42.30
            7. Empleador o socio activo |        101        1.06       43.36
              8. Miembro de cooperativa |         20        0.21       43.57
9. Cuenta propia que no contrata mano d |      2,020       21.13       64.70
10. Cuenta propia que contrata mano de  |      1,310       13.70       78.40
         11. Patrón o socio de la finca |         52        0.54       78.94
  12. Trabajador familiar no remunerado |      1,945       20.35       99.29
           13. Trabajador no remunerado |         68        0.71      100.00
----------------------------------------+-----------------------------------
                                  Total |      9,560      100.00
 	
*/

gen categosec_ci=.
replace categosec_ci=1 if (p96==7 | p96==11)
replace categosec_ci=2 if (p96==4 | p96==5 | p96==6 | p96==8 | p96==9 | p96==10)
replace categosec_ci=3 if (p96==1 | p96==2 | p96==3)
replace categosec_ci=4 if (p96==12| p96==13)
recode categopri_ci (1=2) if (p96==6 | p96==10) &  p109==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci



*************
*horaspri_ci*
*************

gen horaspri_ci=p65
replace horaspri_ci=. if horaspri_ci>168



************
*horastot_ci
************
gen horassec_ci= p95
replace horassec_ci=. if  p95>168

egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0

replace horastot_ci = . if horastot_ci>168


************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p44a/30   if p44b==1
replace durades_ci=p44a/4.3  if p44b==2
replace durades_ci=p44a      if p44b==3
label var durades_ci "Duracion del Desempleo (en meses)"


***************
*antiguedad_ci*
***************
gen antiguedad_ci=.
replace antiguedad_ci=p68a/365           if p68b==1
replace antiguedad_ci=p68a/51.6            if p68b==2
replace antiguedad_ci=p68a/12            if p68b==3
replace antiguedad_ci=p68a               if p68b==4
*Mayra Sáenz Octubre 2013/ Se incluye a los cuenta propia
replace antiguedad_ci=p81a/365           if p81b==1
replace antiguedad_ci=p81a/51.6            if p81b==2
replace antiguedad_ci=p81a/12            if p81b==3
replace antiguedad_ci=p81a               if p81b==4
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*************
*desalent_ci*
*************

gen desalent_ci=(p42==6)
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

***********
*subemp_ci*
***********
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & p119==1
label var subemp_ci "Trabajadores subempleados"
*/
* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p119==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"


***************
*tiempoparc_ci*
***************

gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p119==2
label var tiempoparc_ci "Trabajadores a medio tiempo"
*************
*nempleos_ci*
*************

gen nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p89==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"
/*
**************
*firmpapeq_ci*
**************
*Asalariados, cuenta propia, y cuenta propia actividades no agrícolas de la actividad principal
gen firmapeq_ci=0 if (p67b>5 & p67b<99999) | (p80b>5 & p80b<99999) | (p88b >5 & p88b <99999)
replace firmapeq_ci=1 if (p67b<=5 & p67b!=0) | (p80b<=5 & p80b!=0) | (p88b <=5 & p88b !=0)
*/
*************
*spublico_ci*
*************

gen spublico_ci=1 if p66==1 
replace spublico_ci=0 if p66!=1 

*************
***rama_ci***
*************

gen rama_ci=.
replace  rama_ci=1 if (p63>=111001 & p63<=751305) & emp_ci==1
replace  rama_ci=2 if (p63>=1010000 & p63<=1429034) & emp_ci==1
replace  rama_ci=3 if (p63>=1511000 & p63<=3910075) & emp_ci==1
replace  rama_ci=4 if (p63>=4010001 & p63<=4215003) & emp_ci==1
replace  rama_ci=5 if (p63>=4500028 & p63<=4550002) & emp_ci==1
replace  rama_ci=6 if (p63>=5010000 & p63<=5939035) & emp_ci==1
replace  rama_ci=7 if (p63>=6003003 & p63<=6499003) & emp_ci==1
replace  rama_ci=8 if (p63>=6500008 & p63<=7020019) & emp_ci==1
replace  rama_ci=9 if (p63>=7111000 & p63<=9900027) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
**********
*ocupa_ci*
**********

tostring p60, replace
replace p60 = "0" + p60 if length(p60)==6
gen labor=substr(p60,1,4)
destring labor, replace 

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999 & emp_ci==1
replace ocupa_ci=2 if labor>=1000 & labor<=1999 & emp_ci==1
replace ocupa_ci=3 if labor>=4000 & labor<=4999 & emp_ci==1
replace ocupa_ci=4 if ((labor>=5200 & labor<=5299) | (labor>=9100 & labor<=9119)) & emp_ci==1
replace ocupa_ci=5 if ((labor>=5100 & labor<=5199) | (labor>=9120 & labor<=9169)) & emp_ci==1
replace ocupa_ci=6 if ((labor>=6000 & labor<=6999) | (labor>=9210 & labor<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((labor>=7000 & labor<=8999) | (labor>=9311 & labor<9410)) & emp_ci==1
replace ocupa_ci=8 if labor>0 & labor<=999 & emp_ci==1
replace ocupa_ci=9 if (labor==9999 | labor==9410 | labor==9500) & emp_ci==1
drop labor 

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


**********
**tc_ci***
**********
gen tc_ci=19.03
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HON 2008
gen salmm_ci= 	3428.40
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
replace cotizando_ci=1 if (p58_1>=1 & p58_1<=6) |  (p58_2>=1 & p58_2<=6) |  (p58_3>=1 & p58_3<=6) |  (p58_4>=1 & p58_4<=6) |  (p58_5>=1 & p58_5<=6) |  (p58_6>=1 & p58_6<=6) |  (p58_7>=1 & p58_7<=6) |  (p58_8>=1 & p58_8<=6) |  (p58_9>=1 & p58_9<=6) |  (p58_10>=1 & p58_10<=6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci


foreach i of num 1/11{
replace p58_`i'=0 if p58_`i'==.
}
egen aux=concat(p58*)
destring aux, replace
tostring(aux),replace force
drop aux
*****************
*tipocontrato_ci*
*****************
/*
recode p69 (1=1) (3=2 3) (nonmissing=.), gen(tipocontrato_ci)
label drop tipocontrato_ci
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1"Permanente/indefinido" 2"Temporal" 3"Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
* Modificacion: la variable se crea solamente considerando firma de contrato sin temporalidad. MGD 06/16/2014
g tipocontrato_ci=.
replace tipocontrato_ci=1 if (p69==1 & p70==2) & categopri_ci==3
replace tipocontrato_ci=2 if (p69==1 & p70==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p69==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************

gen cesante_ci=p48 if p48==1 & condocup_ci==2
replace cesante_ci=0 if p48==2 & condocup_ci==2
replace cesante_ci=. if p48==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	

*************
*tamemp_ci
*************
foreach var in p67b p80b p88b  {
recode `var' (9999999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (p67b>=1 & p67b<=5) | (p80b>=1 & p80b<=5) | (p88b >=1 & p88b <=5)
replace tamemp_ci = 2 if (p67b>=6 & p67b<=50) | (p80b>=6 & p80b<=50) | (p88b >=6 & p88b <=50)
replace tamemp_ci = 3 if (p67b>50 & p67b~=.) | (p80b>50 & p80b~=.) | (p88b >50 & p88b ~=.)
replace tamemp_ci = . if p67b==.  & p80b==. &  p88b==.
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande", modify
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

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

*************
**ypen_ci*
*************
egen ypen_ci=rsum(pension jubilacion), missing
label var ypen_ci "Valor de la pension contributiva"

****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*************
**pension_ci*
*************

gen pension_ci=1 if ypen_ci!=0 & ypen_ci!=.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************

**DZ Octubre 2017,PRAF corresponde al programa de transferencias monetarias condicionadas, por lo cual hay más beneficiarios aparte de los adultos mayores. Se restringe esta variable.
gen ypensub_ci=bonpraf if edad_ci>=65
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=ypensub_ci!=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
capture drop lp_ci
*gen lp_ci =Pobreza==2
*Modificación Mayra Sáenz - Marzo 2014
gen lp_ci =.
replace lp_ci = 2406.9 if zona_c == 1
replace lp_ci = 1260.3 if zona_c == 0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

*gen lpe_ci =Pobreza==3
*Modificación Mayra Sáenz - Marzo 2014
g lpe_ci =.
replace lpe_ci = 1203.5 if zona_c == 1
replace lpe_ci = 944 if zona_c == 0
label var lpe_ci "Linea de indigencia oficial del pais"

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((p40 ==2 | p40==3) & condocup_ci==3)
replace categoinac_ci = 2 if  (p40==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (p40==6 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "Jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 
label value categoinac_ci categoinac_ci


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

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"
*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

 
*************
***ynlm_ci***
*************
*Las variables originales ya proporcional el dato en meses.
egen ynlm_ci=rsum(pension jubilacion alquileres destos_tra_edad subsidio_enee bono_80 int_bancarios pens_divorcio ayud_fam ayudpart_efec bonpraf merescolar bolspraf becas remesaext_efec otrosbon otros), missing
replace ynlm_ci=. if pension==. & jubilacion==. & alquileres==. & destos_tra_edad==. & bono_80==. & subsidio_enee==. & int_bancarios==. & pens_divorcio==. & ayud_fam==. & ayudpart_efec==. & bonpraf==. & merescolar==. & bolspraf==. & becas==. & remesaext_efec==. & otrosbon==. & otros==.  
/* Mayra Sáenz - Octubre 2013. No se utiliza la variable original yotrf porque incluye los ingresos no laborales en especies.
gen ynlm_ci=yotrf
label var ynlm_ci "Ingreso No Laboral Monetario"
*/

**************
***ynlnm_ci***
**************
gen ynlnm_ci=. 
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

****************
*autoconsumo_ci*
****************
gen autoconsumop_ci=p87 if p87 >=0 
replace autoconsumop_ci=0 if p87==. & edad_ci>4 & (categopri==1 | categopri==2) & (p34==1 | p35==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p117 if p117 >=0 
replace autoconsumos_ci=0 if p117==. & edad_ci>4 & (categosec==1 | categosec==2) & p89==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autocons_ci=rsum(autoconsumop_ci autoconsumos_ci), missing
replace autocons_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autocons_ci "Autoconsumo Individual (Trabajadores Independientes)"


******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing 
la var autocons_ch "Autoconsumo del Hogar"


************
*remesas_ci*
************
gen remesas_ci=remesaext_efec
replace remesas_ci=. if remesaext_efec==.


****************
***remesas_ch***
****************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing 
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

* Daniela Zuluaga-Noviembre 2017: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

***************
***trapri_ci***
***************
egen trapri_ci= rowtotal(remesas_ci pens_divorcio ayud_fam ayudpart_efec ), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

***************
***progpub_ci***
***************
gen aux=bonpraf if edad_ci<65
egen progpub_ci= rowtotal( bolspraf aux), missing
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas"
drop aux

***************
***progpub_ch***
***************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci ypensub_ci destos_tra_edad subsidio_enee bono_80 merescolar otrosbon becas ), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(alquileres int_bancarios), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(otros), missing
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

***************
***asiste_ci***
***************
gen asiste_ci = .
replace asiste_ci = 1 if p03 == 1
replace asiste_ci = 0 if p03 == 2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci***
*************
*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU	
replace p05 = . if p05 == 99
replace p11 = . if p11 == 99
replace p08 = . if p08 > 9
replace p15 = . if p15 > 9

* Para quienes ya no asisten:
gen aedu_ci = .
replace aedu_ci = 0 if  (p05 >=1 & p05 <= 3) // Ninguno, Ctro alfab, pre-básica
replace aedu_ci = p08 if p05 == 4 // Básica 
replace aedu_ci = p08 + 6  if p05 == 5 // Ciclo básico
replace aedu_ci = p08 + 9 if p05 == 6 // Diversificado
replace aedu_ci = p08 + 11 if (p05 == 7 | p05 == 8 | p05 == 9) // Técnico Sup, Sup no univ, univ
replace aedu_ci = p08 + 11 + 4  if p05 == 10 // Post-grado

* Para quienes asisten actualmente:
replace aedu_ci = 0 if (p11==2 | p11==3) // Ctro alfab, pre-básica
replace aedu_ci = p15 - 1 if p11==4 & p15 >0 // Básica
replace aedu_ci = p15 + 6 - 1 if p11 == 5 // Ciclo común 
replace aedu_ci = p15 + 9 - 1 if p11 == 6 // Diversificado
replace aedu_ci = p15 + 11 - 1 if (p11 == 7 | p11 == 8 | p11 == 9) // Superior
replace aedu_ci = p15 + 11 + 4 - 1 if p11 == 10 // Post-grado

* Imputación de anios perdidos
replace aedu_ci = 0 if (p05 == 1 | p05 == 2 | p05 == 3 & p08 == .)  ///
				| (p11 == 2 | p11 == 3 & p15 == .) // Ninguno, Ctro alafab , Pre-básica
replace aedu_ci = 0 if  (p05 == 4 & p08 == .) | (p11 == 4 & p15 == .) // Educación básica
replace aedu_ci = 6 if (p05 == 5 & p08 == .) | (p11 == 5 & p15 == .) // Ciclo Común
replace aedu_ci = 9 if (p05 == 6 & p08 == .) | (p11 == 6 & p15== .) // Diversificado
replace aedu_ci = 11 if (inlist(p05, 7, 8, 9) & p08==.) | (inlist(p11, 7, 8, 9) & p15 == .) // Terciaria
replace aedu_ci = 15 if (p05 == 10 & p08 == .) | (p11 == 10 & p15 == .) // Post-grado
label var aedu_ci "Años de educacion aprobados"	


**************
***eduno_ci***
**************
g byte eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == .
la var eduno_ci "Personas sin educacion. Excluye preescolar"

**************
***edupi_ci*** 
**************
g byte edupi_ci = (aedu_ci >= 1 & aedu_ci < 6)
replace edupi_ci = . if aedu_ci == .
la var edupi_ci "Personas que no han completado Primaria"

**************
***edupc_ci*** 
**************
g byte edupc_ci = (aedu_ci == 6)
replace edupc_ci = . if aedu_ci == .
la var edupc_ci "Primaria Completa"

**************
***edusi_ci*** 
**************
g byte edusi_ci = (aedu_ci > 6 & aedu_ci <= 10)
replace edusi_ci = . if aedu_ci == .
la var edusi_ci "Secundaria Incompleta"

**************
***edusc_ci***
**************
g byte edusc_ci = (aedu_ci == 11)
replace edusc_ci = . if aedu_ci == .
la var edusc_ci "Secundaria Completa"

***************
***edus1i_ci*** 
***************
g byte edus1i_ci = (aedu_ci > 6 & aedu_ci < 9)
replace edus1i_ci = . if aedu_ci == .
la var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"

***************
***edus1c_ci*** 
***************
g byte edus1c_ci = (aedu_ci == 9)
replace edus1c_ci = . if aedu_ci == .
la var edus1c_ci "1er ciclo de Educacion Secundaria Completo"

***************
***edus2i_ci*** 
***************
g byte edus2i_ci=(aedu_ci > 9 & aedu_ci < 11)
replace edus2i_ci = . if aedu_ci == .
la var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

***************
***edus2c_ci*** 
***************
g byte edus2c_ci = (aedu_ci == 11)
replace edus2c_ci = . if aedu_ci == .
la var edus2c_ci "2do ciclo de Educacion Secundaria Completo"

**************
***eduui_ci*** 
**************
g byte eduui_ci = (aedu_ci >= 12 & aedu_ci <= 15) & p07 == 2 // no finalizó estudios
replace eduui_ci = 1 if (aedu_ci >= 12 & aedu_ci <= 15) & p07 == .
replace eduui_ci = . if aedu_ci == .
la var eduui_ci "Universitaria o Terciaria Incompleta"

**************
***eduuc_ci*** 
**************
g byte eduuc_ci = (aedu_ci >= 12 & aedu_ci <= 15) & p07 == 1
replace eduuc_ci = 1 if aedu_ci > 15
replace eduuc_ci = . if aedu_ci == .
la var eduuc_ci "Universitaria o Terciaria Completa"

***************
***edupre_ci***
***************
g byte edupre_ci=.
la var edupre_ci "Educacion preescolar"

***************
***asipre_ci***
***************
g asispre_ci=.
la var asispre_ci "Asiste a educacion prescolar"

*****************
***pqnoasis_ci***
*****************
gen pqnoasis_ci = p04
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"


***************
***repite_ci*** 
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************
***repiteult_ci*** 
******************
gen repiteult_ci = .
replace repiteult_ci = 1 if p13 == 1
replace repiteult_ci = 0 if p13 == 2
label var repiteult_ci "Personas que han repetido el ultimo grado"


***************
***edupub_ci*** 
***************
gen edupub_ci=.
replace edupub_ci=1 if (p16 == 1 | p16 == 2 | p16 == 3 | p16 == 4| p16 == 8 | p16 == 11 | p16 == 13) & p03 == 1
replace edupub_ci=0 if (p16 == 5 | p16 == 6 | p16 == 7 | p16 == 9 | p16 == 10 | p16 == 12) & p03 == 1
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci= 1 if (p05 == 9 | p05 == 10 | p11 == 9| p11 == 10) // univ o post-grado
replace eduac_ci= 0 if (p05 == 7 | p05 == 8 ) | (p11 == 7| p11 == 8) // tecnico sup o sup no unviersitario
label variable eduac_ci "Superior universitario vs superior no universitario"
	

******************
***pqnoasis1_ci***
******************
**DZ Noviembre 2017: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
g       pqnoasis1_ci = 1 if pqnoasis_ci == 7
replace pqnoasis1_ci = 2 if pqnoasis_ci == 11
replace pqnoasis1_ci = 3 if pqnoasis_ci == 6
replace pqnoasis1_ci = 4 if pqnoasis_ci == 3
replace pqnoasis1_ci = 5 if pqnoasis_ci == 4 | pqnoasis_ci == 10
replace pqnoasis1_ci = 6 if pqnoasis_ci == 2
replace pqnoasis1_ci = 7 if pqnoasis_ci == 8 | pqnoasis_ci == 9
replace pqnoasis1_ci = 8 if pqnoasis_ci == 5
replace pqnoasis1_ci = 9 if pqnoasis_ci == 1 | pqnoasis_ci == 12

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

********************************************************************************************************************
**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
gen aguared_ch=.
replace aguared_ch=1 if v05==1
replace aguared_ch=0 if v05==2 

gen aguadist_ch=.
replace aguadist_ch=1 if v09==1
replace aguadist_ch=2 if (v09==2 | v09==3)
replace aguadist_ch=3 if v09==4

gen aguamala_ch=.
replace aguamala_ch=1 if v06>=5 & v06<=8
replace aguamala_ch=0 if v06>=1 & v06<=4

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if (v10==1|v10==2|v10==3)
replace luz_ch=0 if (v10>=4 & v10<=8)


gen luzmide_ch=.


* La base no dispone de la pregunta h4
gen combust_ch=.

gen pared_ch=.
replace pared_ch=0 if v02>=5 & v02<=6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7

gen bano_ch=.
replace bano_ch=1 if h5==1
replace bano_ch=0 if h5==2

gen banoex_ch=.
replace banoex_ch=1 if h7==1
replace banoex_ch=0 if h7==2

gen des1_ch=.
replace des1_ch=0 if h5==2
replace des1_ch=1 if (h6==1|h6==2)
replace des1_ch=2 if (h6==5|h6==6|h6==7)
replace des1_ch=3 if (h6==3|h6==4)

* MGR Jul, 2015: corrección en sintáxis

/*
gen des2_ch=.
replace des2_ch=1 if (h6==1|h6==2|h6==3)
replace des2_ch=2 if (h6==4|h6==5|h6==6|h6==7|h6==8)
replace des2_ch=0 if h5==2
*/

gen des2_ch=.
replace des2_ch=1 if (h6==1|h6==2|h6==5|h6==6|h6==7|h6==8)
replace des2_ch=2 if (h6==4|h6==3)
replace des2_ch=0 if h5==2

gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8 | v09==9

gen resid_ch=.
replace resid_ch=0 if (v11==1|v11==3)
replace resid_ch=1 if (v11==4|v11==6)
replace resid_ch=2 if (v11==2|v11==7)
replace resid_ch=3 if (v11==5|v11==8)

gen dorm_ch=.
replace dorm_ch=h1 if h1>=0 

gen cuartos_ch=.
replace cuartos_ch=v16 if v16>=0 


gen cocina_ch=.
replace cocina_ch=1 if (h3==1)
replace cocina_ch=0 if (h3==2)
*********************************

gen telef_ch=.
replace telef_ch=1 if (h8_07==1 | h8_08==1)
replace telef_ch=0 if (h8_07==2 & h8_08==2)

gen refrig_ch=.
replace refrig_ch=1 if h8_01==1
replace refrig_ch=0 if h8_01==2

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if (h8_09==1 | h8_10==1)
replace auto_ch=0 if (h8_09==2 & h8_10==2)

gen compu_ch=.
replace compu_ch=1 if h8_14==1
replace compu_ch=0 if h8_14==2

/*
**** If any household member has accessed ****
**** the internet from home, the variable ****
**** receives a value of 1                ****
gen internet_ch_=.
replace internet_ch_=1 if p31_1==1
by hogar, sort: egen internet_ch = max(internet_ch_)
drop internet_ch_
replace internet_ch=0 if internet_ch==.
*/

gen internet_ch=.
replace internet_ch=1 if p31_1==1
replace internet_ch=0 if internet_ch==.
/*
**** If any household member has celular  ****
**** phone, the variable receives a value ****
**** of 1                                 ****
gen cel_ch_=.
replace cel_ch_=1 if p33==1
by hogar, sort: egen cel_ch = max(cel_ch_)
drop cel_ch_
replace cel_ch=0 if cel_ch==.
*/

gen cel_ch=.
replace cel_ch=1 if p33==1
replace cel_ch=0 if cel_ch==.

gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=8) | v01==2 | v01==3

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if v14==1
replace viviprop_ch=1 if v14==3
replace viviprop_ch=2 if v14==2
replace viviprop_ch=3 if (v14==4 | v14==5 | v14==6 | v14==7)

gen vivitit_ch=.
replace vivitit_ch=1 if v17==1
replace vivitit_ch=0 if v17==2

/* Tipo de cambio lempiras por dolares = 19.03 
   Variable cambio en la Base de otros Ingresos */

gen vivialq_ch=.
replace vivialq_ch=v15 if v15m==1
replace vivialq_ch=v15*19.03 if v15m==2

****  ****
gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v06 >=1 & v06 <=4) | v06==8
replace aguamejorada_ch = 0 if (v06 >=5 & v06 <=7) | v06==9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if ( h5 ==1 & ((h6 >=1 & h6 <=2) | (h6 >=5 & h6 <=8)) & h7 ==1)
replace banomejorado_ch = 0 if ( h5 ==1 & ((h6 >=1 & h6 <=2) | (h6 >=5 & h6 <=8)) & h7 ==2) | (h6>=3 & h6 <=4) | (h5==2)


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
	*** migrantelac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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
trapri_ci trapri_ch progpub_ci progpub_ch trapub_ci  trapub_ch capital_ci capital_ch otros_ci otros_ch ypen_ch ytotal_ci  ytotal_ch ytotalpc_ch quintil_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close



