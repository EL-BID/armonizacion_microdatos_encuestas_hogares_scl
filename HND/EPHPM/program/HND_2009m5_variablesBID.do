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
local ANO "2009"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
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

	****************
	* region_c *
	****************
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

gen idh_ch=hogar
*format idh_ch %20.0g 

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

gen anio_c=2009

*******
*mes_c*
*******
cap drop mes_c
gen mes_c=""
tostring fecha, replace
replace mes_c =substr(fecha,2,2) if length(fecha)==7
replace mes_c =substr(fecha,3,2) if length(fecha)==8
destring mes, replace
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre" 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo" 6 "Junio" 7 "Julio" 8 "Agosto"
label value mes_c mes_c



*************
*relacion_ci*
*************

gen relacion_ci=.
replace relacion_ci=1 if Rela_j==1
replace relacion_ci=2 if Rela_j==2
replace relacion_ci=3 if Rela_j==3 | Rela_j==4
replace relacion_ci=4 if Rela_j>=5 & Rela_j<= 8 
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
label define sexo_ci 1 "Masculino" 2 "Femenino"
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
replace condocup_ci=1 if (p401==1 | p402==1 | p403==1)
replace condocup_ci=2 if (p401==2 | p402==2 | p403==2) & (p405==1 | p406==1) 
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


. tab  p431

      p431. En la O.P. Ud trabaja como: |      Freq.     Percent        Cum.
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
replace categopri_ci=1 if		p431==10 | p431==11 | p431==6 | p431==7
replace categopri_ci=2 if		p431==5  | p431==9 | p431==4 | p431==8
replace categopri_ci=3 if 		p431==1  | p431==2  | p431==3
replace categopri_ci=4 if  		p431==12 | p431==13  
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
replace categosec_ci=1 if (p462==7 | p462==11)
replace categosec_ci=2 if (p462==4 | p462==5 | p462==6 | p462==8 | p462==9 | p462==10)
replace categosec_ci=3 if (p462==1 | p462==2 | p462==3)
replace categosec_ci=4 if (p462==12| p462==13)
recode categopri_ci (1=2) if (p462==6 | p462==10) &  p476==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

*****************
***horaspri_ci***
*****************

gen horaspri_ci=p430
replace horaspri_ci=. if horaspri_ci>168


*****************
***horastot_ci***
*****************
gen horassec_ci=p461
replace horassec_ci=. if p461>168

egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0

replace horastot_ci = . if horastot_ci>168

************
*durades_ci*
************

gen durades_ci=.
replace durades_ci=p411_cant/30      if p411_frec==1
replace durades_ci=p411_cant/4.3     if p411_frec==2
replace durades_ci=p411_cant         if p411_frec==3
label var durades "Duracion del Desempleo (en meses)"

***************
*antiguedad_ci*
***************
gen antiguedad_ci=.
replace antiguedad_ci=p433_cant/365           if  p433_tiempo==1
replace antiguedad_ci=p433_cant/51.6            if  p433_tiempo==2
replace antiguedad_ci=p433_cant/12            if  p433_tiempo==3
replace antiguedad_ci=p433_cant               if  p433_tiempo==4

replace antiguedad_ci=p447_cant/365           if  p447_tiempo==1
replace antiguedad_ci=p447_cant/51.6            if  p447_tiempo==2
replace antiguedad_ci=p447_cant/12            if  p447_tiempo==3
replace antiguedad_ci=p447_cant               if  p447_tiempo==4

replace antiguedad_ci=.         if p447_cant==99 | p433_cant==99

label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"


*************
*desalent_ci*
*************

gen desalent_ci=.
replace desalent_ci=1 if p409==6
replace desalent_ci=0 if p409!=6 & p409!=.

***********
*subemp_ci*
***********
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & p486==1
label var subemp "Trabajadores subempleados"
*/
* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p486==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

***************
*tiempoparc_ci*
***************

gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p486==2
label var tiempoparc_ci "Trabajadores a medio tiempo"

*************
*nempleos_ci*
*************

gen nempleos_ci=1 if p455==2
replace nempleos_ci=2 if p455==1
replace nempleos_ci=. if emp_ci==0

/*
**************
*firmpapeq_ci*
**************

*Asalariados, cuenta propia, y cuenta propia actividades no agrícolas de la actividad principal
gen firmapeq_ci=0 if (p432_cant>5 & p432_cant<99999) | (p446_cant>5 & p446_cant<99999) | (p454_cant>5 & p454_cant<99999)
replace firmapeq_ci=1 if (p432_cant<=5 & p432_cant!=0) | (p446_cant<=5 & p446_cant!=0) | (p454_cant<=5 & p454_cant!=0)
*/
*************
*spublico_ci*
*************
gen spublico_ci=1 if p431==1 
replace spublico_ci=0 if p431!=1 
*************
***rama_ci***
*************
gen rama_ci=.
replace  rama_ci=1 if (p428>=111001 & p428<=950010) & emp_ci==1
replace  rama_ci=2 if (p428>=1010000 & p428<=1430017) & emp_ci==1
replace  rama_ci=3 if (p428>=1511000 & p428<=3910075) & emp_ci==1
replace  rama_ci=4 if (p428>=4010001 & p428<=4340002) & emp_ci==1
replace  rama_ci=5 if (p428>=4500028 & p428<=4820020) & emp_ci==1
replace  rama_ci=6 if (p428>=5010000 & p428<=5939057) & emp_ci==1
replace  rama_ci=7 if (p428>=6003003 & p428<=6499003) & emp_ci==1
replace  rama_ci=8 if (p428>=6500008 & p428<=7020023) & emp_ci==1
replace  rama_ci=9 if (p428>=7111000 & p428<=9900027) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

**********
*
*
**********
tostring p425, replace
replace p425 = "0" + p425 if length(p425)==6
gen labor=substr(p425,1,4)
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
* HND2009: promedio urbano-rural
gen salmm_ci= 	4777.50
label var salmm_ci "Salario minimo legal"

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
*****************************************************************************************************/

tostring p423_1 p423_2 ,replace
*gen ce23_lmk=substr(p423_1,1,1) 
gen aux_a=substr(p423_1,1,1) if p423_1!="9999999999" & p423_1!="."
gen aux_b=substr(p423_1,2,1) if p423_1!="9999999999" & p423_1!="."
gen aux_c=substr(p423_1,3,1) if p423_1!="9999999999" & p423_1!="."
gen aux_d=substr(p423_1,4,1) if p423_1!="9999999999" & p423_1!="."
gen aux_e=substr(p423_1,5,1) if p423_1!="9999999999" & p423_1!="."
gen aux_f=substr(p423_1,6,1) if p423_1!="9999999999" & p423_1!="."
destring aux_*, replace
*destring p423_1 p423_2 ce23_lmk,replace
*replace ce23_lmk=. if ce23_lmk==0
*replace ce23_lmk=10 if p423_1==10
*replace ce23_lmk=11 if p423_2==1

gen cotizando_ci=.
replace cotizando_ci=1 if (aux_a>=1 & aux_a<=6) |  (aux_b>=1 & aux_b<=6)| (aux_c>=1 & aux_c<=6)| (aux_d>=1 & aux_d<=6) | (aux_e>=1 & aux_e<=6) | (aux_f>=1 & aux_f<=6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

*****************
*tipocontrato_ci*
*****************
/*
recode p434 (1=1) (2=3) (nonmissing=.), gen(tipocontrato_ci)
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

* Modificacion: la variable se crea solamente considerando firma de contrato sin temporalidad. MGD 06/16/2014
g tipocontrato_ci=.
replace tipocontrato_ci=1 if (p434==1 & p435==2) & categopri_ci==3
replace tipocontrato_ci=2 if (p434==1 & p435==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p434==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=p412 if p412==1 & condocup_ci==2
replace cesante_ci=0 if p412==2 & condocup_ci==2
replace cesante_ci=. if p412==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	


*************
*tamemp_ci
*************
foreach var in p432_cant p446_cant p454_cant {
recode `var' (9999999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (p432_cant>=1 & p432_cant<=5) | (p446_cant>=1 & p446_cant<=5) | (p454_cant>=1 & p454_cant<=5)
replace tamemp_ci = 2 if (p432_cant>=6 & p432_cant<=50) | (p446_cant>=6 & p446_cant<=50) | (p454_cant>=6 & p454_cant<=50)
replace tamemp_ci = 3 if (p432_cant>50 & p432_cant~=.) | (p446_cant>50 & p446_cant~=.) | (p454_cant>50 & p454_cant~=.)
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
egen ypen_ci=rsum(pension_esp pension_efe jubilacion_esp jubilacion_efe), missing
label var ypen_ci "Valor de la pension contributiva"

****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 


*************
**pension_ci*
*************

gen pension_ci=1 if ypen_ci!=. & ypen_ci!=0
recode pension_ci .=0

label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************
**DZ Octubre 2017,PRAF corresponde al programa de transferencias monetarias condicionadas, por lo cual hay más beneficiarios aparte de los adultos mayores. Se restringe esta variable.
egen ypensub_ci=rowtotal(bonpraf_esp bonpraf_efe) if edad_ci>=65, missing
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
gen lp_ci =.
replace lp_ci = 2547.6 if zona_c == 1
replace lp_ci = 1341.7 if zona_c == 0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci = 1273.78 if zona_c == 1
replace lpe_ci = 1004.98 if zona_c == 0
label var lpe_ci "Linea de indigencia oficial del pais"
*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((p407 ==2 | p407==3) & condocup_ci==3)
replace categoinac_ci = 2 if  (p407==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (p407==6 & condocup_ci==3)
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
*Las variables originales ya proporcionan el dato en meses.


egen ynlm_ci=rsum( pension_efe jubilacion_efe alquileres_efe destos_3edad_efe subs_enee_efe int_bancarios_efe pens_divorcio_efe ayud_fam_efe ayud_part_efe bonpraf_efe meresc_efe bolspraf_efe becas_efe remesaext_efe otbon_efe otros_efe ymensual17), missing
replace ynlm_ci=. if (pension_efe==. & jubilacion_efe==. &  alquileres_efe==. &  destos_3edad_efe==. &  subs_enee_efe==. &  int_bancarios_efe==. &  pens_divorcio_efe==. &  ayud_fam_efe==. &  ayud_part_efe==. &  bonpraf_efe==. &  meresc_efe==. &  bolspraf_efe==. &  becas_efe==. &  remesaext_efe==. & otbon_efe==. &  otros_efe==. & ymensual17==.)
label var ynlm_ci "Ingreso No Laboral Monetario" 

/* Mayra Sáenz - Octubre 2013. No se utiliza la variable original yotrf porque incluye los ingresos no laborales en especies.
gen ynlm_ci=yotrf
label var ynlm_ci "Ingreso No Laboral Monetario"
*/

**************
***ynlnm_ci***
**************
*Las variables originales ya proporcionan el dato en meses.
egen ynlnm_ci=rsum(pension_esp jubilacion_esp alquileres_esp destos_3edad_esp subs_enee_esp int_bancarios_esp pens_divorcio_esp ayud_fam_esp ayud_part_esp bonpraf_esp meresc_esp bolspraf_esp becas_esp remesaext_esp otbon_esp otros_esp), missing
replace ynlnm_ci=. if (pension_esp==. & jubilacion_esp==. &  alquileres_esp==. &  destos_3edad_esp==. &  subs_enee_esp==. &  int_bancarios_esp==. &  pens_divorcio_esp==. &  ayud_fam_esp==. &  ayud_part_esp==. &  bonpraf_esp==. &  meresc_esp==. &  bolspraf_esp==. &  becas_esp==. &  remesaext_esp==. & otbon_esp==. &  otros_esp==.)
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
gen autoconsumop_ci=p453 if  p453>=0  
replace autoconsumop_ci=0 if p453==. & edad_ci>4 & (categopri==1 | categopri==2) & (emp_ci==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p484 if p484>=0 
replace autoconsumos_ci=0 if  p484==. & edad_ci>4 & (categosec==1 | categosec==2) &  p455==1
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
egen remesas_ci=rsum(remesaext_esp remesaext_efe), missing
replace remesas_ci=. if remesaext_esp==. & remesaext_efe==.


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

* Daniela Zuluaga-Noviembre  2017: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

***************
***trapri_ci***
***************
egen trapri_ci= rowtotal(remesas_ci pens_divorcio_esp pens_divorcio_efe ayud_fam_esp ayud_fam_efe ayud_part_esp ayud_part_efe ), missing
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

egen aux=rowtotal(bonpraf_esp bonpraf_efe) if edad_ci<65, missing
egen progpub_ci= rowtotal(bolspraf_esp bolspraf_efe aux), missing
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
egen trapub_ci= rowtotal(progpub_ci ypensub_ci destos_3edad_esp destos_3edad_efe subs_enee_esp subs_enee_efe meresc_esp meresc_efe  becas_esp becas_efe otbon_esp otbon_efe), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

****************
***capital_ci***
****************
egen capital_ci= rowtotal(alquileres_esp alquileres_efe int_bancarios_esp int_bancarios_efe), missing
label var capital_ci "Ingreso por renta del capital" 

****************
***capital_ch***
****************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

*****************
***otros_ci***
*****************
egen otros_ci= rowtotal(otros_esp otros_efe ymensual17), missing
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

*****************
*	Educación   *
*****************

************
* asiste_ci*
************
gen asiste_ci=.
replace asiste_ci = 1 if p103 == 1
replace asiste_ci = 0 if p103 == 2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci***
*************	
* Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU
replace p105 = . if p105 == 99
replace p111 = . if p111 == 99
replace p108 = . if p108 > 9
replace p115 = . if p115 > 9

* Para quienes ya no asisten:
gen aedu_ci=.
replace aedu_ci = 0 if  (p105 >= 1 & p105 <= 3) // Ninguno, Ctro alafab , Pre-básica
replace aedu_ci = p108 if p105 == 4 // Básica
replace aedu_ci = p108 + 6 if p105 == 5  // Ciclo Básico
replace aedu_ci = p108 + 9 if p105 == 6 // Diversificado
replace aedu_ci = p108 + 11 if (p105 == 7 | p105 == 8 | p105 == 9) // Tecnico Sup, Sup no univ, univ
replace aedu_ci = p108 + 11 + 4 if p105 == 10 // Post-grado

* Para quienes asisten actualmente:
replace aedu_ci = 0 if (p111 == 2 | p111 == 3)  // Ctro alfab, pre-básica 
replace aedu_ci = p115 - 1 if p111 == 4 // Básica
replace aedu_ci = p115 + 6 - 1 if p111 == 5  // Ciclo Común 
replace aedu_ci = p115 + 9 - 1 if p111 == 6 // Diversificado
replace aedu_ci = p115 + 11 - 1 if (p111 == 7 | p111 == 8 | p111 == 9) // Superior
replace aedu_ci = p115 + 11 + 4 - 1 if p111 == 10 // Post-grado

* Imputación de anios perdidos:
replace aedu_ci = 0 if (p105 == 1 | p105 == 2 | p105 == 3 & p108 == .)  ///
				| (p111 == 2 | p111 == 3 & p115 == .) // Ninguno, Ctro alafab , Pre-básica
replace aedu_ci = 0 if  (p105 == 4 & p108 == .) | (p111 == 4 & p115 == .) // Educación básica
replace aedu_ci = 6 if (p105 == 5 & p108 == .) | (p111 == 5 & p115 == .) // Ciclo Común
replace aedu_ci = 9 if (p105 == 6 & p108 == .) | (p111 == 6 & p115== .) // Diversificado
replace aedu_ci = 11 if (inlist(p105, 7, 8, 9) & p108==.) | (inlist(p111, 7, 8, 9) & p115 == .) // Terciaria
replace aedu_ci = 15 if (p105 == 10 & p108 == .) | (p111 == 10 & p115 == .) // Post-grado
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
g byte eduui_ci = (aedu_ci >= 12 & aedu_ci <= 15) & p107 == 2 // no finalizó estudios
replace eduui_ci = 1 if (aedu_ci >= 12 & aedu_ci <= 15) & p107 == .
replace eduui_ci = . if aedu_ci == .
la var eduui_ci "Universitaria o Terciaria Incompleta"

**************
***eduuc_ci*** 
**************
g byte eduuc_ci = (aedu_ci >= 12 & aedu_ci <= 15) & p107 == 1
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
gen byte asispre_ci= (p103 == 1 & p118 == 3) // Asiste a pre-básica
la var asispre_ci "Asiste a educacion prescolar"


*****************
***pqnoasis_ci***
*****************
gen pqnoasis_ci = p104
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
replace repiteult_ci = 1 if p113 == 1
replace repiteult_ci = 0 if p113 == 2
label var repiteult_ci "Personas que están repetiendo el ultimo grado"

***************
***edupub_ci*** 
***************
gen edupub_ci=.
replace edupub_ci = 1 if (p116 == 1 | p116 == 2 | p116 == 3 | p116 == 4 | p116 == 8  | p116 == 11 | p116 == 13) & p103 == 1
replace edupub_ci = 0 if (p116 == 5 | p116 == 6 | p116 == 7 | p116 == 9 | p116 == 10 | p116 == 12) & p103 == 1
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

**************
***eduac_ci***
**************
gen byte eduac_ci = .
replace eduac_ci= 1 if (p105 == 9 | p105 == 10 | p111 == 9| p111 == 10) // univ o post-grado
replace eduac_ci= 0 if (p105 == 7 | p105 == 8 ) | (p111 == 7| p111 == 8) // tecnico sup o sup no unviersitario
label variable eduac_ci "Superior universitario vs superior no universitario"

******************
***pqnoasis1_ci***
******************
**DZ Noviembre 2017: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
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

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if v06<=2 
replace aguared_ch = 0 if v06>2
la var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0



*****************
*aguafuente_ch*
*****************
gen aguafuente_ch = 1 if v06<=2 & v09<=2
replace aguafuente_ch = 2 if (v06<=2 & v09>2) | v06==8
replace aguafuente_ch = 6 if v06==6
replace aguafuente_ch = 7 if v06==7
replace aguafuente_ch = 8 if v06==5
replace aguafuente_ch = 9 if v06==9
replace aguafuente_ch = 10 if  v06==10 |v06==3 | v06==4

*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch= 1 if v09==1
replace aguadist_ch= 2 if v09==2
replace aguadist_ch= 3 if v09==3|v09 ==4

**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =.
replace aguadisp1_ch = 1 if v07==1
replace aguadisp1_ch = 0 if v07==2

**************
*aguadisp2_ch*
**************
gen aguadisp2_ch =.
replace aguadisp2_ch = 1 if v08_2<=3 & v08_3<12
replace aguadisp2_ch = 2 if v08_2>3 & v08_3>=12
replace aguadisp2_ch = 3 if v07==1

*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10


*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7 



*****************
***aguamide_ch***
*****************
gen aguamide_ch =.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.
replace bano_ch=0 if h05==2
replace bano_ch=1 if h06==1
replace bano_ch=2 if h06==2
replace bano_ch=3 if ( h06==6 | h06==7)
replace bano_ch=4 if (h06==3 | h06==4)
replace bano_ch=6 if h06==8 | h06==5 

***************
***banoex_ch***
***************
generate banoex_ch=.
replace banoex_ch = 9 if h05==2
replace banoex_ch = 1 if h07==1
replace banoex_ch = 0 if h07==2
la var banoex_ch "El servicio sanitario es exclusivo del hogar"


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6


************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch = 0 if h05==1

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"

********
*luz_ch*
********


gen luz_ch=1 if v10==1 |v10==2 |v10==3 
replace luz_ch=0 if v10>=4 & v10<=8

************
*luzmide_ch*
************

gen luzmide_ch=.


************
*combust_ch*
************

gen combust_ch=1 if h04==3 | h04==2 | h04==4
replace combust_ch=0 if h04==5 | h04==1




gen des1_ch=.
replace des1_ch=0 if h05==2
replace des1_ch=1 if (h06==1|h06==2)
replace des1_ch=2 if (h06==5|h06==6|h06==7)
replace des1_ch=3 if (h06==3|h06==4)

* MGR Jul, 2015: corrección en sintáxis 

/*
gen des2_ch=.
replace des2_ch=1 if (h06==1|h06==2|h06==3)
replace des2_ch=2 if (h06==4|h06==5|h06==6|h06==7 | h06==8)
replace des2_ch=0 if h05==2
*/
gen des2_ch=.
replace des2_ch=1 if (h06==1|h06==2|h06==5|h06==6|h06==7)
replace des2_ch=2 if (h06==4|h06==3|h06==8)
replace des2_ch=0 if h05==2

gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8 

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8| v04==9 | v04==10

gen pared_ch=.
replace pared_ch=0 if v02>=5 & v02<=6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7

gen resid_ch=.
replace resid_ch=0 if (v11==1|v11==3)
replace resid_ch=1 if (v11==4|v11==6)
replace resid_ch=2 if (v11==2|v11==7)
replace resid_ch=3 if (v11==5|v11==8)

gen dorm_ch=.
replace dorm_ch=h01 if h01>=0 

gen cuartos_ch=.
replace cuartos_ch=v16 if v16>=0 



***********
*cocina_ch*
***********

gen cocina_ch=.


*Las variables de bienes durables constan en el formulario pero no están en la base de datos.

**********
*telef_ch*
**********

gen telef_ch=.

***********
*regrig_ch*
***********

gen refrig_ch=.

**********
*freez_ch*
**********

gen freez_ch=.

*********
*auto_ch*
*********

gen auto_ch=.

**********
*compu_ch*
**********

gen compu_ch=.

*************
*internet_ch*
*************

gen internet_ch=(p306_1==1)

********
*cel_ch*
********

gen cel_ch=(p308==1)

**********
*vivi1_ch*
**********


gen vivi1_ch=.
replace vivi1_ch=1 if v01==1 | v01==2
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=7) | v01==3 


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
replace viviprop_ch=0 if v14==1
replace viviprop_ch=1 if v14==3
replace viviprop_ch=2 if v14==2
replace viviprop_ch=3 if (v14==4 | v14==5 | v14==6 | v14==7)

gen vivitit_ch=.
replace vivitit_ch=1 if v17==1
replace vivitit_ch=0 if v17==2

/* Tipo de cambio lempiras por dolares = 19.92
   Variable cambio en la Base de otros Ingresos */

gen vivialq_ch=.
replace vivialq_ch=v15 if v15m==1
replace vivialq_ch=v15*19.92 if v15m==2
*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"


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
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close




