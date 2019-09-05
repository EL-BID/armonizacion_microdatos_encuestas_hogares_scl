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
local ANO "2010"
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
Mayra Sáenz (Julio 2013)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

use "`base_in'", clear


*************************************************************************
*************************           HONDURAS                *************
************************* MAYO 2010 (No se dispone de Sept10)************
*************************************************************************

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
gen idh_ch=hogar

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
gen anio_c=2010

*******
*mes_c*
*******
cap drop mes_c
gen mes_c= mes 
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
replace relacion_ci=5 if rela_j==9 | rela_j==11
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
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

*********
*edad_ci*
*********
gen edad_ci=edad if edad <99
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
replace condocup_ci=1 if (ce01==1 | ce02==1 | ce03==1)
replace condocup_ci=2 if (ce01==2 | ce02==2 | ce03==2) & (ce05==1 | ce06==1) 
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
/*2010
ce31:
	1	1. empleado(a) u obrero(a) público
	2	2. empleado(a) u obrero(a) privado
	3	3. empleado(a) doméstico(a)
	4	4. miembro de cooperativa de producción
	5	5. cuenta propia que no contrata mano de obra temporal
	6	6. cuenta propia que contrata mano de obra temporal
	7	7. empleador o socio activo
	8	8. miembro de cooperativa
	9	9. cuenta propia que no contrata mano de obra temporal
	10	10. cuenta propia que contrata mano de obra temporal
	11	11. patrón o socio de la finca
	12	12. trabajador familiar no remunerado
	13	13. trabajador no remunerado
*/
gen categopri_ci=.
replace categopri_ci=1 if ce31==10 | ce31==11 | ce31==6 | ce31==7
replace categopri_ci=2 if ce31==5  | ce31==9 | ce31==4 | ce31==8
replace categopri_ci=3 if ce31==1  | ce31==2  | ce31==3
replace categopri_ci=4 if ce31==12 | ce31==13  
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

*************
***rama_ci***
*************
gen rama_ci=.
replace  rama_ci=1 if (ce28cod>=111001 & ce28cod<=950010) & emp_ci==1
replace  rama_ci=2 if (ce28cod>=1010000 & ce28cod<=1430017) & emp_ci==1
replace  rama_ci=3 if (ce28cod>=1511000 & ce28cod<=3910075) & emp_ci==1
replace  rama_ci=4 if (ce28cod>=4010001 & ce28cod<=4340002) & emp_ci==1
replace  rama_ci=5 if (ce28cod>=4500028 & ce28cod<=4820020) & emp_ci==1
replace  rama_ci=6 if (ce28cod>=5010000 & ce28cod<=5939057) & emp_ci==1
replace  rama_ci=7 if (ce28cod>=6003003 & ce28cod<=6499003) & emp_ci==1
replace  rama_ci=8 if (ce28cod>=6500008 & ce28cod<=7020023) & emp_ci==1
replace  rama_ci=9 if (ce28cod>=7111000 & ce28cod<=9900027) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

*************
*horaspri_ci*
*************
gen horaspri_ci=ce30
replace horaspri_ci=. if horaspri_ci>168

************
*horastot_ci
************
gen horassec_ci=ce77
replace horassec_ci=. if ce77>168

egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0

replace horastot_ci = . if horastot_ci>168


****************
* categosec_ci *
****************
gen categosec_ci=.
replace categosec_ci=1 if (ce79==6 | ce79==7 | ce79==10 | ce79==11)
replace categosec_ci=2 if (ce79==4 | ce79==5 | ce79==8 | ce79==9)
replace categosec_ci=3 if (ce79>=1 & ce79<=3)
replace categosec_ci=4 if (ce79==12| ce79==13)
recode categosec_ci (1=2) if (ce79==6 | ce79==10) & ce110==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=ce11/30      if ce11tiempo==1
replace durades_ci=ce11/4.3  if ce11tiempo==2
replace durades_ci=ce11               if ce11tiempo==3
label var durades_ci "Duracion del Desempleo (en meses)"

***************
*antiguedad_ci*
***************


/*
generat antiguedad_ci=.
replace antiguedad_ci=ce33/(365/12)     if ce33_tiempo==1 /* modifico esta var, revisar hacia atras*/
replace antiguedad_ci=ce33/((365/7)/12) if ce33_tiempo==2
replace antiguedad_ci=ce33              if ce33_tiempo==3
replace antiguedad_ci=ce33*12           if ce33_tiempo==4
replace antiguedad_ci=.                 if ce33==99
label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"
*/
*Modificación Mayra Sáenz - octubre 2013. La variable es en años.
* Modificacion MGD 07/18/2014: faltaba incluir la antiguedad de los cuenta propia.
* Asalariados
generat antiguedad_ci=.
replace antiguedad_ci=ce33/365  if ce33_tiempo==1
replace antiguedad_ci=ce33/51.6 if ce33_tiempo==2
replace antiguedad_ci=ce33/12   if ce33_tiempo==3
replace antiguedad_ci=ce33      if ce33_tiempo==4
* Cuenta propia
replace antiguedad_ci=ce64/365  if ce64_tiempo==1
replace antiguedad_ci=ce64/51.6 if ce64_tiempo==2
replace antiguedad_ci=ce64/12   if ce64_tiempo==3
replace antiguedad_ci=ce64      if ce64_tiempo==4

replace antiguedad_ci=.         if ce33==99 | ce64==99
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*************
*desalent_ci*
*************
gen desalent_ci=.
replace desalent_ci=1 if ce09==6
replace desalent_ci=0 if ce09!=6 & ce09!=.

***********
*subemp_ci*
***********
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & ce120==1
label var subemp_ci "Trabajadores subempleados"
*/

* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & ce120==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

***************
*tiempoparc_ci*
***************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & ce120==2
label var tiempoparc_ci "Trabajadores a medio tiempo"

*************
*nempleos_ci*
*************
generat nempleos_ci=1 if ce72==2
replace nempleos_ci=2 if ce72==1
replace nempleos_ci=. if emp_ci==0
/*
**************
*firmpapeq_ci*
**************
*Asalariados, cuenta propia, y cuenta propia actividades no agrícolas de la actividad principal
gen firmapeq_ci=0 if (ce32_cantidad>5 & ce32_cantidad<99999) | (ce63_cantidad>5 & ce63_cantidad<99999) | (ce71_cantidad>5 & ce71_cantidad<99999)
replace firmapeq_ci=1 if (ce32_cantidad<=5 & ce32_cantidad!=0) | (ce63_cantidad<=5 & ce63_cantidad!=0) | (ce71_cantidad<=5 & ce71_cantidad!=0)
*/
*************
*spublico_ci*
*************
gen spublico_ci=1 if ce31==1 
replace spublico_ci=0 if ce31!=1 


**********
*ocupa_ci*
**********
tostring ce25, replace
replace ce25 = "0" + ce25 if length(ce25)==6
gen labor=substr(ce25,1,4)
destring labor, replace 

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999 & emp_ci==1
replace ocupa_ci=2 if labor>=1000 & labor<=1999 & emp_ci==1
replace ocupa_ci=3 if labor>=4000 & labor<=4999 & emp_ci==1
replace ocupa_ci=4 if ((labor>=5200 & labor<=5299) | (labor>=9100 & labor<=9119)) & emp_ci==1
replace ocupa_ci=5 if ((labor>=5100 & labor<=5199) | (labor>=9120 & labor<=9169)) & emp_ci==1
replace ocupa_ci=6 if ((labor>=6000 & labor<=6999) | (labor>=9210 & labor<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((labor>=7000 & labor<=8999) | (labor>=9311 & labor<=9410)) & emp_ci==1
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
* HON 2010
gen salmm_ci= 	4949.4
label var salmm_ci "Salario minimo legal"

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
* correccion
tostring ce23 ,replace

gen aux_a=substr(ce23,1,1) if ce23!="9999999999" & ce23!="."
gen aux_b=substr(ce23,2,1) if ce23!="9999999999" & ce23!="."
gen aux_c=substr(ce23,3,1) if ce23!="9999999999" & ce23!="."
gen aux_d=substr(ce23,4,1) if ce23!="9999999999" & ce23!="."
gen aux_e=substr(ce23,5,1) if ce23!="9999999999" & ce23!="."
gen aux_f=substr(ce23,6,1) if ce23!="9999999999" & ce23!="."
gen aux_g=substr(ce23,7,1) if ce23!="9999999999" & ce23!="."
destring aux_*, replace
	
gen cotizando_ci=.
replace cotizando_ci=1 if (aux_a>=1 & aux_a<=6) |  (aux_b>=1 & aux_b<=6)| (aux_c>=1 & aux_c<=6)| (aux_d>=1 & aux_d<=6) | (aux_e>=1 & aux_e<=6) | (aux_f>=1 & aux_f<=6) | (aux_g>=1 & aux_g<=6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

*****************
*tipocontrato_ci*
*****************
/*
recode ce82 (1=1) (2=3) (nonmissing=.), gen(tipocontrato_ci)
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

* Modificacion: la variable se cre aocn el tipo de ocntrato para la ocupacion secundaria.
* Las variables correctas son ce34 y ce35.  MGD 06/16/2014
g tipocontrato_ci=.
replace tipocontrato_ci=1 if (ce34==1 & ce35==2) & categopri_ci==3
replace tipocontrato_ci=2 if (ce34==1 & ce35==1) & categopri_ci==3
replace tipocontrato_ci=3 if (ce34==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=ce12 if ce12==1 & condocup_ci==2
replace cesante_ci=0 if ce12==2 & condocup_ci==2
replace cesante_ci=. if ce12==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	


*************
*tamemp_ci
*************
foreach var in ce32_cantidad ce63_cantidad ce71_cantidad {
recode `var' (99999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (ce32_cantidad>=1 & ce32_cantidad<=5) | (ce63_cantidad>=1 & ce63_cantidad<=5) | (ce71_cantidad>=1 & ce71_cantidad<=5)
replace tamemp_ci = 2 if (ce32_cantidad>=6 & ce32_cantidad<=50) | (ce63_cantidad>=6 & ce63_cantidad<=50) | (ce71_cantidad>=6 & ce71_cantidad<=50)
replace tamemp_ci = 3 if (ce32_cantidad>50 & ce32_cantidad~=.) | (ce63_cantidad>50 & ce63_cantidad~=.) | (ce71_cantidad>50 & ce71_cantidad~=.)
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande", modify
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
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
egen ypen_ci=rsum(pension jubilacion), missing
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
gen lp_ci =.
replace lp_ci = 2647.1 if zona_c == 1
replace lp_ci = 1379 if zona_c == 0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 1323.5 if zona_c == 1
replace lpe_ci= 1033.0 if zona_c == 0
label var lpe_ci "Linea de indigencia oficial del pais"

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((ce07 ==2 | ce07==3) & condocup_ci==3)
replace categoinac_ci = 2 if  (ce07==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (ce07==6 & condocup_ci==3)
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
                                                     

egen ynlm_ci=rsum( pension jubilacion alquileres destos_tra_edad subsidio_enee int_bancarios pens_divorcio ayud_fam_efectivo ayudpart_efec remesaext_efec ayudinst_efec bonpraf merescolar bolspraf becas bontecn otrosbon otros), missing
replace ynlm_ci=. if pension==. & jubilacion==. & alquileres==. & destos_tra_edad==. & subsidio_enee==. & int_bancarios==. & pens_divorcio==. & ayud_fam_efectivo==. & ayudpart_efec==. & remesaext_efec==. & ayudinst_efec==. & bonpraf==. & merescolar==. & bolspraf==. & becas==. & bontecn==. & otrosbon==. & otros==.  
/* Mayra Sáenz - Octubre 2013. No se utiliza la variable original yotrf porque incluye los ingresos no laborales en especies.
gen ynlm_ci=yotrf
label var ynlm_ci "Ingreso No Laboral Monetario"
*/

**************
***ynlnm_ci***
**************
egen ynlnm_ci=rsum(ayudfam_especie ayudpart_esp remesaext_esp ayudinst_esp), missing
replace ynlnm_ci =. if ayudfam_especie==. & ayudpart_esp==. & remesaext_esp==. & ayudinst_esp==.
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
gen autoconsumop_ci=ce70 if  ce70>=0 
replace autoconsumop_ci=0 if ce70==. & edad_ci>4 & (categopri==1 | categopri==2) & (emp_ci==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=ce118 if ce118>=0  
replace autoconsumos_ci=0 if  ce118==. & edad_ci>4 & (categosec==1 | categosec==2) &  ce72==1
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
egen remesas_ci=rsum(remesaext_esp remesaext_efec), missing
replace remesas_ci=. if remesaext_esp==. & remesaext_efec==.


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
egen trapri_ci= rowtotal(remesas_ci pens_divorcio ayud_fam_efectivo ayudfam_especie  ayudpart_efec ayudpart_esp), missing
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

gen aux=bonpraf if edad_ci<65
egen progpub_ci= rowtotal( bolspraf aux), missing
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
egen trapub_ci= rowtotal(progpub_ci bontecn merescolar ypensub_ci becas destos_tra_edad otrosbon ayudinst_esp ayudinst_efec subsidio_enee), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

****************
***capital_ci***
****************
egen capital_ci= rowtotal(alquileres int_bancarios), missing
label var capital_ci "Ingreso por renta del capital" 

****************
***capital_ch***
****************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

*****************
***otros_ci***
*****************
egen otros_ci= rowtotal(otros), missing
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
replace asiste_ci=1 if ed03==1
replace asiste_ci=0 if ed03==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"




	*************
	***aedu_ci***
	*************
		/*
		1 1. ninguno
           2 2. programa de alfabetizacion
           3 3. pre-básica
           4 4. básica
           5 5. ciclo comun
           6 6. diversificado
           7 7. tecnico superior
           8 8. superior no universitaria
           9 9. superior universitaria
          10 10. post-grado
          99 99. no sabe/no responde
*/
		* Años de educacion aprobados **
/*replace ed08=. if ed08>9
replace ed15=. if ed15>9

** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if ed05>=1 & ed05<=3
replace aedu_ci=ed08 if ed05==4 
replace aedu_ci=ed08+6 if ed05==5 | ed05==6
replace aedu_ci=ed08+12 if ed05==7 | ed05==8 | ed05==9
replace aedu_ci=ed08+17 if ed05==10
** para quienes asisten actualmente
replace aedu_ci=0 if ed10==1 | ed10==2 | ed10==3 
replace aedu_ci=ed15-1 if ed10==4
replace aedu_ci=ed15+6-1 if ed10==5 | ed10==6
replace aedu_ci=ed15+12-1 if ed10==7 | ed10==8 | ed10==9
replace aedu_ci=ed15+17-1 if ed10==10
label var aedu_ci "Años de educacion aprobados"	*/

*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU	

* Años de educacion aprobados **
		
replace ed08=. if ed08>9
replace ed15=. if ed15>9

** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if ed05>=1 & ed05<=3
replace aedu_ci=ed08 if ed05==4 
replace aedu_ci=ed08+6 if ed05==5
replace aedu_ci=ed08+9 if ed05==6
replace aedu_ci=ed08+12 if ed05==7 | ed05==8 | ed05==9
replace aedu_ci=ed08+17 if ed05==10

** para quienes asisten actualmente
replace aedu_ci=0 if ed10==1 | ed10==2 | ed10==3 
replace aedu_ci=ed15-1 if ed10==4
replace aedu_ci=ed15+6-1 if ed10==5  
replace aedu_ci=ed15+9-1 if ed10==6
replace aedu_ci=ed15+12-1 if ed10==7 | ed10==8 | ed10==9
replace aedu_ci=ed15+17-1 if ed10==10
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
replace edupre_ci=1 if ((ed05==3 | ed10==3) & aedu_ci ~=.)
replace edupre_ci=0 if (edupre_ci~=1 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

***************
***asipre_ci***
***************

g asispre_ci=.
replace asispre_ci=1 if ed03==1 & ed10==3 & edad_ci>=4
recode asispre_ci (.=0)
la var asispre_ci "Asiste a educacion prescolar"

******************************
*	pqnoasis 
******************************
ren ed04 pqnoasis_ci
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
replace repiteult_ci=1 if ed13==1
replace repiteult_ci=0 if ed13==2
label var repiteult_ci "Personas que están repetiendo el ultimo grado"

******************************
*	edupub_ci 
******************************
gen edupub_ci=.
/*Mayra Sáenz Octubre 2013
replace edupub_ci=1 if (ed09==1|ed09==2|ed09==3|ed09==4|ed09==7|ed09==8|ed09==13)
replace edupub_ci=0 if (ed09==5|ed09==6|ed09==9|ed09==10|ed09==11|ed09==12)
*/
replace edupub_ci=1 if (ed16==1|ed16==2|ed16==3|ed16==4|ed16==7|ed16==8|ed16==13)
replace edupub_ci=0 if (ed16==5|ed16==6|ed16==9|ed16==10|ed16==11|ed16==12)
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if ed05==7 | ed10==7
replace tecnica_ci=0 if tecnica_ci ~=1 & ( ed05!=99 & ed10!=99)
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
replace aguadist_ch=1 if dv09==1
replace aguadist_ch=2 if (dv09==2 | dv09==3)
replace aguadist_ch=3 if dv09==4

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
gen luz_ch=1 if dv10==1 |dv10==2 |dv10==3 
replace luz_ch=0 if dv10>=4 & dv10<=8

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


* MGR Jul, 2015: corrección en sintáxis 

/*	
gen des2_ch=.
replace des2_ch=1 if (dh06==1|dh06==2|dh06==3)
replace des2_ch=2 if (dh06==4|dh06==5|dh06==6|dh06==7 |dh06==8)
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
replace resid_ch=0 if (dv11==1|dv11==3)
replace resid_ch=1 if (dv11==4|dv11==6)
replace resid_ch=2 if (dv11==2|dv11==7)
replace resid_ch=3 if (dv11==5|dv11==8)

gen dorm_ch=.
replace dorm_ch=dh01 if dh01>=0 

gen cuartos_ch=.
replace cuartos_ch=dv16 if dv16>=0 

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
gen auto_ch=(dh08_9==1 | dh08_10 ==1)

**********
*compu_ch*
**********
gen compu_ch=(dh08_14==1)

*************
*internet_ch*
*************
gen internet_ch=(at04==1)
replace internet_ch=. if at04==. | at04==9

********
*cel_ch*
********
gen cel_ch=(at08==1)
replace cel_ch=. if at08==.

**********
*vivi1_ch*
**********
gen vivi1_ch=.
replace vivi1_ch=1 if dv01==1 | dv01==2
replace vivi1_ch=2 if dv01==4
replace vivi1_ch=3 if (dv01>=5 & dv01<=7) | dv01==3 
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
replace viviprop_ch=0 if dv14==1
replace viviprop_ch=1 if dv14==3
replace viviprop_ch=2 if dv14==2
replace viviprop_ch=3 if (dv14==4 | dv14==5 | dv14==6 | dv14==7)
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch
	
gen vivitit_ch=.
replace vivitit_ch=1 if dv17==1
replace vivitit_ch=0 if dv17==2

gen vivialq_ch=.
replace vivialq_ch=dv15       if dv15moneda==1 /*solo hay lempiras*/
replace vivialq_ch=dv15*19.92 if dv15moneda==2

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"
	
**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g  aguamejorada_ch = 1 if (dv06 >=1 & dv06 <=4) | dv06==8
replace aguamejorada_ch = 0 if (dv06 >=5 & dv06 <=7) | (dv06 >=9 & dv06 <=10)

*********************
***banomejorado_ch***
*********************
g banomejorado_ch = 1 if ( dh05 ==1 & ((dh06 >=1 & dh06 <=2) | (dh06 >=5 & dh06 <=8)) & dh07 ==1)
replace banomejorado_ch = 0 if ( dh05 ==1 & ((dh06 >=1 & dh06 <=2) | (dh06 >=5 & dh06 <=8)) & dh07 ==2) | (dh06 >=3 & dh06 <=4) | (dh05==2)




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
trapri_ci trapri_ch progpub_ci progpub_ch trapub_ci  trapub_ch capital_ci capital_ch otros_ci otros_ch ypen_ch ytotal_ci  ytotal_ch ytotalpc_ch quintil_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close
