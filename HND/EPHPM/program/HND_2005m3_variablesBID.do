
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
local ANO "2005"
local ronda m3

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m3
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*****                            HONDURAS 2005 - MARZO                                                *****
*****                EPHPM 2005 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                  	       personas                                             ***** 
*****                                          hogares                                                *****
*** Revised March, 2008 (by tede) ***

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
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  depto
label define region_c  1 "1. atlantida" ///
           2 "2. colón" ///
           3 "3. comayagua" ///
           4 "4. copán" ///
           5 "5. cortés" ///
           6 "6. choluteca" ///
           7 "7. el paraíso" ///
           8 "8. francisco morazan" ///
           9 "9. gracias a dios" ///
          10 "10. intibuca" ///
          11 "11. islas de la bahía" ///
          12 "12. la paz" ///
          13 "13. lempira" ///
          14 "14. ocotepeque" ///
          15 "15. olancho" ///
          16 "16. santa bárbara" ///
          17 "17. valle" ///
          18 "18. yoro" 

label var region_c "División política"

**********
***raza***
**********
gen raza_ci= .
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

g raza_idioma_ci =.

************
****pais****
************

gen pais_c="HND"

**********
***anio***
**********

gen anio_c=2005

*********
***mes***
*********

gen mes_c=3
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"
label value mes_c mes_c

***************
****idh_ch*****
***************

replace numhog=1 if numhog==.
egen idh_ch=group(hogar depto domi ur numhog) 

***************
**factor_ch****
***************
gen factor_ch=factor

**********
***zona***
**********
gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4
replace zona_c=0 if domi==5
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

***************
*relacion_ci  *
***************

gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j==7 | rela_j== 8 
replace relacion_ci=5 if rela_j==9 | rela_j==11
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion relacion

***************
*idp_ci       *	
***************
gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"

	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
****factor_ci**
***************

gen factor_ci=factor_ch
**********
***sexo***
**********
gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=edad if edad<99
label var edad_ci "Edad del Individuo"

*****************
***civil_ci***
*****************

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
****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=condact
replace condocup_ci=4 if condact == 4 | edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Comprobacion con variables originales.  Se considera ocupado a quienes estan en trabajos no remunerados. 5/28/2014 MGD
* La edad minima de la encuesta se cambia a 5 anios.

g condocup_ci=.
replace condocup_ci=1 if (p17==1 | p18==1 | p19==1)
replace condocup_ci=2 if (p17==2 | p18==2 | p19==2) & (p21==1) 
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


*****************
***desalent_ci***
*****************
gen desalent_ci=.
replace desalent_ci=0 if emp_ci==1
replace desalent_ci=1 if p23 ==6
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*****************
***horaspri_ci***
*****************
gen horaspri_ci= p42 if  p42<=168 
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p65 if p65<168


*****************
***horastot_ci***
*****************
egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0

label var horassec "Horas totales trabajadas en todas las Actividades"



***************
*subemp_ci    *	
***************
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot_ci<30 & p84==1
label var subemp_ci "Trabajadores subempleados"
tab subemp_ci [w=int(factor_ci)]

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p27/(365/12)      if p27a==1
replace durades_ci=p27/((365/7)/12)  if p27a==2
replace durades_ci=p27               if p27a==3
label var durades_ci "Duracion del Desempleo (en meses)"
 

*******************
***antiguedad_ci***
*******************
gen b= p45/365 if p45a == 1
replace b= p45/51.6 if p45a ==2
replace b= p45/12 if p45a ==3
replace b= p45 if p45a ==4

gen antiguedad_ci=b if emp_ci==1
replace antiguedad_ci=. if p45a==. & b==.
drop b
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot_ci<30 & p84==2
label var tiempoparc_ci "Trabajadores a medio tiempo"
*****************
***nempleos_ci***
*****************
*gen segsoc_ci=.
*label var segsoc "Personas que cuentan con seguro social"
gen nempleos_ci=.
replace nempleos_ci=p61
/* MLO: modificacion 2013
gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if emp_ci==1 & p58==1
replace nempleos=0 if emp_ci==0*/
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"

/*
*************
*firmapeq_ci*
*************

*Asalariados y cuenta propia de la actividad principal
gen firmapeq_ci=0 if (p52a>5 & p52a<99999) | (p60a>5 & p60a<99999) 
replace firmapeq_ci=1 if (p52a<=5 & p52a!=0) | (p60a<=5 & p60a!=0) 
 */

*****************
***spublico_ci***
*****************

gen spublico_ci= 1 if  p43==1
replace spublico_ci= 0 if  p43 > 1 &  p43 <= 13
label var spublico_ci "Personas que trabajan en el sector publico"


*************
**ocupa_ci***
*************
gen p39a1=p39a
tostring p39a1, replace
replace p39a1 = "0" + p39a1 if length(p39a1)==6
gen labor=substr(p39a1,1,4)
destring labor, replace 

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999 & emp_ci==1
replace ocupa_ci=2 if labor>=1000 & labor<=1999 & emp_ci==1
replace ocupa_ci=3 if labor>=4000 & labor<=4999 & emp_ci==1
replace ocupa_ci=4 if ((labor>=5200 & labor<=5299) | (labor>=9100 & labor<=9119)) & emp_ci==1
replace ocupa_ci=5 if ((labor>=5100 & labor<=5199) | (labor>=9120 & labor<=9191)) & emp_ci==1
replace ocupa_ci=6 if ((labor>=6000 & labor<=6999) | (labor>=9210 & labor<=9231)) & emp_ci==1
replace ocupa_ci=7 if ((labor>=7000 & labor<=8999) | (labor>=9300 & labor<9410)) & emp_ci==1
replace ocupa_ci=8 if labor>0 & labor<=999 & emp_ci==1
replace ocupa_ci=9 if (labor==9999 | labor==9410 | labor==9500 | labor==5430 | labor==5520 | labor==5020) & emp_ci==1
drop labor 

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci


*************
***rama_ci***
*************
*** ocupacion principal ***

gen rama_ci=ramaop
replace rama_ci=. if ramaop==10 | ramaop==11 | emp_ci==0
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
******************
***categopri_ci***
******************

gen categopri_ci=1 if p43==7| p43==6 | p43==10 | p43==11  /* p43 for March 2005 */
replace categopri_ci=2 if p43==4 | p43==5 | p43==8 | p43==9 
replace categopri_ci=3 if p43==1 | p43==2 | p43==3
replace categopri_ci=4 if p43==12 | p43==13
recode categopri_ci (1=2) if (p43==6 | p43==10) &  p51 ==4
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************
***categosec_ci***
******************

gen categosec_ci=1 if p66==7 | p66==6 | p66==10 | p66==11 /* p66 for March 2005 */
replace categosec_ci=2 if p66==4 | p66==5 | p66==8 | p66==9
replace categosec_ci=3 if p66==1 | p66==2 | p66==3
replace categosec_ci=4 if p66==12 | p66==13
recode categosec_ci (1=2) if (p66==6 | p66==10) &  p74==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
/*YL ->linea estimada (no encuentro las originales) correspond al mes de febrero*/
generat lp_ci =1549.44 if zona_c==1
replace lp_ci = 861.88 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (p09l1c2==1|p09l2c2==1|p09l3c2==1|p09l4c2==1|p09l5c2==1) 
recode cotizando_ci . = 0 if condact==1 | condact==2

label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	

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
 
*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if p28 ==1 & condocup_ci==2
replace cesante_ci=0 if p28 ==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

foreach var in p52a p60a   {
recode `var' (99999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (p52a>=1 & p52a<=5) | (p60a>=1 & p60a<=5) 
replace tamemp_ci = 2 if (p52a>=6 & p52a<=50) | (p60a>=6 & p60a<=50)
replace tamemp_ci = 3 if (p52a>50 & p52a~=.) | (p60a>50 & p60a~=.) 
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande", modify
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"


*************
*ypen_ci*
*************
**Se transforman los dolares a lempiras**
foreach i in p87l01c2  p87l02c2 {
gen `i'2 = `i'*19.00 
replace `i'= `i'2
}  
drop p87l01c22 p87l02c22

*DZ Noviembre 2017: Se incluye pension y jubilacion en usd
egen ypen_ci=rowtotal(p87l01c1  p87l01c2 p87l02c1 p87l02c2 ), missing
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci =0
replace pension_ci=1 if ypen_ci!=. & ypen_ci!=0
label var pension_ci "1=Recibe pension contributiva"


****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen byte pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci= 19.03


label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2005: mod 2015 MGD salario segun rama
gen salmm_ci=. 	/*2515.10*/
replace salmm_ci=2108.4 if rama_ci==1
replace salmm_ci=2275.2 if rama_ci==2 | rama_ci==3 | rama_ci==5 | rama_ci==6 | rama_ci==9
replace salmm_ci=2376.15 if rama_ci==7 
replace salmm_ci=2654.63 if rama_ci==8
replace salmm_ci=2352.71 if rama_ci==4 | salmm_ci==.
label var salmm_ci "Salario minimo legal"


*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((p24 ==1 | p24==2) & condocup_ci==3)
replace categoinac_ci = 2 if  (p24==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (p24==5 & condocup_ci==3)
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
egen ylmpri_ci=rowtotal(ysmop ycmop yagmop ygamop) if (ysmop>=0 & ycmop>=0 & yagmop>=0 & ygamop>=0), missing
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
egen ylnmpri_ci=rowtotal(yseop yceop yageop ygaeop ) if (yseop>=0 & yceop>=0 & yageop>=0 & ygaeop>=0), missing
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********
egen ylmsec_ci=rowtotal(ysmos ycmos yagmos ygamos) if (ysmos>=0 & ycmos>=0 & yagmos>=0 & ygamos>=0), missing
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
*ylnmsec_ci*
************
egen ylnmsec_ci=rowtotal(yseos yceos yageos ygaeos) if (yseos>=0 & yceos>=0 & yageos>=0 & ygaeos>=0), missing
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
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a enero 2005: 18.88 
                                                                     febrero 2005: 18.95 
								     marzo 2005: 19.00 
								     promedio 3 meses: 18.94 */
/*
p87l01c2	p87-pension (vlr. del mes pasado efvo us$)
p87l02c2	p87-jubilación (vlr. del mes pasado efvo us$)
p87l03c2	p87-alquileres (vlr. del mes pasado efvo us$)
p87l05c2	p87-intereses bco (vlr. del mes pasado efvo us$)
p87l06c2	p87-remesas del exterior (vlr. del mes pasado efvo us$)
p87l07c2	p87-pensión x divorcio (vlr. del mes pasado efvo us$)
p87l08c2	p87-ayudas familiares (vlr. del mes pasado efvo us$)
p87l09c2	p87-ayudas particulares (vlr. del mes pasado efvo us$)
p87l11c2	p87-prestaciones laborales (vlr. del mes pasado efvo us$)
p87l12c2	p87-otros (vlr. del mes pasado efvo us$)

*/

*Conversión de otros ingresos en dólares a lempiras . Tipo de cambio 19.03
foreach i in  p87l03c2 p87l05c2 p87l06c2	p87l07c2 p87l08c2 p87l09c2	p87l11c2 p87l12c2 {
gen `i'_ = `i'*19.00 
replace `i'= `i'_
}  
/*
Otros ingresos en lempiras	
p87l01c1	p87-pension (vlr. del mes pasado efvo lps)
p87l02c1	p87-jubilación (vlr. del mes pasado efvo lps)
p87l03c1	p87-alquileres (vlr. del mes pasado efvo lps)
p87l04c1	p87-subsidios (vlr. del mes pasado efvo lps)
p87l05c1	p87-intereses bco (vlr. del mes pasado efvo lps)
p87l06c1	p87-remesas del exterior (vlr. del mes pasado efvo lps)
p87l07c1	p87-pensión x divorcio (vlr. del mes pasado efvo lps)
p87l08c1	p87-ayudas familiares (vlr. del mes pasado efvo lps)
p87l09c1	p87-ayudas particulares (vlr. del mes pasado efvo lps)
p87l10c1	p87-bonos (vlr. del mes pasado efvo lps)
p87l11c1	p87-prestaciones laborales (vlr. del mes pasado efvo lps)
p87l12c1	p87-otros (vlr. del mes pasado efvo lps)
p87l12c3	p87-otros (vlr. del mes pasado esp lps)
*/

egen ynlm_ci=rsum(p87l01c1 p87l02c1 p87l03c1 p87l04c1 p87l05c1 p87l06c1 p87l07c1 p87l08c1 p87l09c1 p87l10c1 p87l12c1 p87l01c2 p87l02c2 p87l03c2 p87l05c2 p87l06c2	p87l07c2 p87l08c2 p87l09c2 p87l12c2), missing
label var ynlm_ci "Ingreso No Laboral Monetario"

**************
***ynlnm_ci***
**************
/*
 * Ingresos en especies dólares	
p87l06c4	p87-remesas del exterior (vlr. del mes pasado esp us$)
p87l09c4	p87-ayudas particulares (vlr. del mes pasado esp us$)
p87l08c4	p87-ayudas familiares (vlr. del mes pasado esp us$)
p87l12c4	p87-otros (vlr. del mes pasado esp us$)

*/

*Conversión de otros ingresos en dólares a lempiras . Tipo de cambio 19.00
foreach i in p87l06c4 p87l09c4 p87l08c4 p87l12c4 {
gen `i'_ = `i'*19.00 
replace `i'= `i'_
}  

/*
Ingresos en especies Lempiras	
p87l06c3	p87-remesas del exterior (vlr. del mes pasado esp lps)
p87l09c3	p87-ayudas particulares (vlr. del mes pasado esp lps)
p87l08c3	p87-ayudas familiares (vlr. del mes pasado esp lps)
p87l12c3	p87-otros (vlr. del mes pasado esp lps)
*/
egen ynlnm_ci=rsum(p87l06c3 p87l09c3 p87l08c3 p87l12c3 p87l06c4 p87l09c4 p87l08c4 p87l12c4), missing

label var ynlnm_ci "Ingreso no laboral no monetario" 


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
*** autocons_ci ***
***********************

gen autoconsumop_ci=p59 if p59<99999 & p59>=0 /* p59 for 2005 */ 
replace autoconsumop_ci=0 if p59==. & edad_ci>4 & (categopri==1 | categopri==2) & (p17==1 | p18==1) /* p59 for 2005 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p82 if p82<99999 & p82>=0 /* p82 for 2005 */ 
replace autoconsumos_ci=0 if p82==. & edad_ci>4 & (categosec==1 | categosec==2) & p61==1 
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
*Los valores en dólares ya fueron convertidos a lempiras para el cálculo del ingreso no laboral monetario.
egen remesas_ci=rsum(p87l06c1	p87l06c2	p87l06c3	p87l06c4), missing
label var remesas_ci "Remesas Individuales"

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
egen trapri_ci= rowtotal(remesas_ci p87l07c1 p87l07c2  p87l08c1 p87l08c2 p87l08c3 p87l08c4 p87l09c1 p87l09c2 p87l09c3 p87l09c4 ), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

***************
***progpub_ci***
***************
gen progpub_ci= .
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas" 

***************
***progpub_ch***
***************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci p87l04c1 p87l10c1  ypensub_ci), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(p87l03c1 p87l03c2 p87l05c1 p87l05c2), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(p87l12c1 p87l12c2 p87l12c3 p87l12c4 ), missing
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

/*NOTA: El ingreso total aquí construido difiere de la variable de ingreso total de la base original . Esto ocurre ya que al agregar algunas de las variables
desagregadas en la base, no coinciden con las variables agregadas que están construidas en la base original (para los ingresos no laborales), no hay información 
suficiente para saber cómo se agregaron dichas variables en la base original;
En particular, hay inconsistencia con la periodicidad de las variables de otros ingresos.En todo caso, construimos los ingresos no laborales con las variables desagregadas, 
teniendo como base la periodicidad específicada en el cuestionario.
LA DIFERENCIA ES MENOR AL 10% 
No se consideran ingresos laborales negativos*/

******************************************************************************
*	Educación
*****************************************************************************

************
* asiste_ci*
************
gen asiste_ci=.
replace asiste_ci=1 if p02==1
replace asiste_ci=0 if p02==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

/*
* Años de educacion aprobados **
replace p04b=. if p04b>9
replace p05b=. if p05b>9
** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if p04a>=1 & p04a<=3
replace aedu_ci=p04b if p04a==4 
replace aedu_ci=p04b+6 if p04a==5 | p04a==6
* MGR Aug, 2015: correción en sintáxis
*replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+12 if p04a==7 | p04a==8 | p04a==9 /*se incluye superior no universitaria*/
*replace aedu_ci=p04b+17 if p04a==9
replace aedu_ci=p04b+17 if p04a==10
** para quienes asisten actualmente
/*
replace aedu_ci=0 if p05a==1 | p05a==2
replace aedu_ci=p05b-1 if p05a==3
replace aedu_ci=p05b+6-1 if p05a==4 | p05a==5
replace aedu_ci=p05b+12-1 if p05a==6 | p05a==7
replace aedu_ci=p05b+17-1 if p05a==8
label var aedu_ci "Años de educacion aprobados"
*/
replace aedu_ci=0 if p05a==2 | p05a==3
replace aedu_ci=p05b-1 if p05a==4
replace aedu_ci=p05b+6-1 if p05a==5 | p05a==6
replace aedu_ci=p05b+12-1 if p05a==7 | p05a==8 | p05a==9 
replace aedu_ci=p05b+17-1 if p05a==10
label var aedu_ci "Años de educacion aprobados"

*/

*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU

***************
*aedu_ci      *	
***************

* Años de educacion aprobados **
replace p04b=. if p04b>9
replace p05b=. if p05b>9
** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if p04a>=1 & p04a<=3
replace aedu_ci=p04b if p04a==4 
replace aedu_ci=p04b+6 if p04a==5 
replace aedu_ci=p04b+9 if p04a==6
* MGR Aug, 2015: correción en sintáxis
*replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+12 if p04a==7 | p04a==8 | p04a==9 /*se incluye superior no universitaria*/
*replace aedu_ci=p04b+17 if p04a==9
replace aedu_ci=p04b+17 if p04a==10
** para quienes asisten actualmente
/*
replace aedu_ci=0 if p05a==1 | p05a==2
replace aedu_ci=p05b-1 if p05a==3
replace aedu_ci=p05b+6-1 if p05a==4 | p05a==5
replace aedu_ci=p05b+12-1 if p05a==6 | p05a==7
replace aedu_ci=p05b+17-1 if p05a==8
label var aedu_ci "Años de educacion aprobados"
*/

replace aedu_ci=0 if p05a==2 | p05a==3
replace aedu_ci=p05b-1 if p05a==4
replace aedu_ci=p05b+6-1 if p05a==5 
replace aedu_ci=p05b+9-1 if p05a==6
replace aedu_ci=p05b+12-1 if p05a==7 | p05a==8 | p05a==9 
replace aedu_ci=p05b+17-1 if p05a==10
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
replace edupre_ci=1 if ((p04a==3 | p05a==3) & aedu_ci ~=.)
replace edupre_ci=0 if (edupre_ci~=1 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

***************
***asispre_ci***
***************
*Creación de la variable asistencia a preescolar por Iván Bornacelly - 08/09/18
	g asispre_ci=.
	la var asispre_ci "Asiste a educacion prescolar"	

******************************
*	pqnoasis 
******************************
rename p03 pqnoasis_ci
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

******************************
*	repite_ci 
******************************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

***************
*repiteult_ci *	
***************

gen repiteult_ci=.
replace repiteult_ci=1 if p05c==1
replace repiteult_ci=0 if p05c==2
label var repiteult_ci "Personas que han repetido el ultimo grado"


******************************
*	edupub_ci 
******************************

gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p04a==7 | p05a==7
replace tecnica_ci=0 if tecnica_ci ~=1 & ( p04a!=99 & p05a!=99)
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
replace pqnoasis1_ci = 2 if pqnoasis_ci==3
replace pqnoasis1_ci = 3 if pqnoasis_ci==6
replace pqnoasis1_ci = 4 if pqnoasis_ci==2
replace pqnoasis1_ci = 5 if pqnoasis_ci==4  | pqnoasis_ci==10
replace pqnoasis1_ci = 6 if pqnoasis_ci==1
replace pqnoasis1_ci = 7 if pqnoasis_ci==8  | pqnoasis_ci==9
replace pqnoasis1_ci = 8 if pqnoasis_ci==5
replace pqnoasis1_ci = 9 if pqnoasis_ci==11 | pqnoasis_ci==12

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

************
*aguared_ch*
************

gen aguared_ch=.
replace aguared_ch=1 if v05b==1 | v05b==2
replace aguared_ch=0 if v05b>=3 &  v05b<=8

*************
*aguadist_ch*
*************

gen aguadist_ch=.
replace aguadist_ch=1 if v05e==1
replace aguadist_ch=2 if (v05e==2 | v05e==3)
replace aguadist_ch=3 if v05e==4

*************
*aguamala_ch*
*************

gen aguamala_ch=.
replace aguamala_ch=1 if v05b>=5 & v05b<=8
replace aguamala_ch=0 if v05b>=1 & v05b<=4

*************
*aguamide_ch*
*************


gen aguamide_ch=.

********
*luz_ch*
********


gen luz_ch=1 if v07==1 |v07==2 |v07==3 
replace luz_ch=0 if v07>=4 & v07<=8

************
*luzmide_ch*
************

gen luzmide_ch=.


************
*combust_ch*
************

gen combust_ch=.

*********
*bano_ch*
*********

gen bano_ch=.
replace bano_ch=1 if v06a==1
replace bano_ch=0 if v06a==2

gen banoex_ch=.
replace banoex_ch=1 if v06c==1
replace banoex_ch=0 if v06c==2

gen des1_ch=.
replace des1_ch=0 if v06a==2
replace des1_ch=1 if (v06b==1|v06b==2)
replace des1_ch=2 if (v06b==5|v06b==6|v06b==7)
replace des1_ch=3 if (v06b==3|v06b==4)

gen des2_ch=.
replace des2_ch=1 if (v06b==1|v06b==2|v06b==3)
replace des2_ch=2 if (v06b==4|v06b==5|v06b==6|v06b==7)
replace des2_ch=0 if v06a==2

gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8 


gen pared_ch=.
replace pared_ch=0 if v02==5 | v02==6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8| v04==9 | v04==10

gen resid_ch=.
replace resid_ch=0 if (v08a==1|v08a==3)
replace resid_ch=1 if (v08a==4|v08a==6)
replace resid_ch=2 if (v08a==2|v08a==7)
replace resid_ch=3 if (v08a==5|v08a==8)

gen dorm_ch=.
replace dorm_ch=v16b if v16b>=0 

gen cuartos_ch=.
replace cuartos_ch=v16a if v16a>=0 



***********
*cocina_ch*
***********

gen cocina_ch=.

**********
*telef_ch*
**********

gen telef_ch=.
replace telef_ch=1 if v13f==1 
replace telef_ch=0 if v13f==2

***********
*regrig_ch*
***********

gen refrig_ch=.
replace refrig_ch=1 if v13a==1 /* v13a for 2005 */
replace refrig_ch=0 if v13a==2

***************
**freez_ch    *
***************

gen freez_ch=.

***************
**auto_ch     *
***************

gen auto_ch=.
replace auto_ch=1 if v13h==1 /* v13h for 2005 */
replace auto_ch=0 if v13h==2

***************
**compu_ch    *
***************

gen compu_ch=.
replace compu_ch=1 if v13k==1 /* v13k for 2005 */
replace compu_ch=0 if v13k==2

***************
**internet_ch *
***************

gen internet_ch=.

***************
**cel_ch      *
***************

gen cel_ch=.
replace cel_ch=1 if v13g==1 /* v13g for 2005 */
replace cel_ch=0 if v13g==2

***************
**vivi1_ch    *
***************

gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=8) | v01==2 | v01==3

***************
**vivi2_ch    *
***************

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

***************
**viviprop_ch *
***************

gen viviprop_ch=.
replace viviprop_ch=0 if v14==5 /* v14 for 2005 */
replace viviprop_ch=1 if v14==1
replace viviprop_ch=2 if v14==4
replace viviprop_ch=3 if v14==6 | v14==2 | v14==3 | v14==7 /* v14==7 included for 2005 */

***************
**vivitit_ch  *
***************

gen vivitit_ch=.
replace vivitit_ch=1 if v17a==1 /* v17a */
replace vivitit_ch=0 if v17a==2


***************
**vivialq_ch  *
***************
/*
gen vivialq_ch=.
replace vivialq_ch=v15b if v15b<99999 & v14==5 /* v15b & v14 for 2005 */

* replace vivialq_ch=v10c/17.73 if v10c<99999 & v10b==2 *


*/
gen vivialq_ch=.
replace vivialq_ch=v15b if v15a==1
replace vivialq_ch=v15b*19.00 if v15a==2


***************
*vivialqimp_ch*
***************

gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v05b >=1 & v05b <=4)
replace aguamejorada_ch = 0 if (v05b >=5 & v05b <=8)

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=7)) & v06c ==1)
replace banomejorado_ch = 0 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=7)) & v06c ==2) | (v06b >=3 & v06b <=4) | (v06a==2)


**otras variables**
gen id_ind_ci = .
gen id_afro_ci = .

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
