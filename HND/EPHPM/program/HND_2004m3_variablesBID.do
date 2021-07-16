


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
local ANO "2004"
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
Round: m5
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*****                            HONDURAS 2004 - MARZO                                                *****
*****                EPHPM 2004 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                  	       personas                                               ***** 
*****                                          hogares                                                *****

****************************************************************************/

clear all
set more off
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
************
****pais****
************

gen pais_c="HND"

**********
***anio***
**********

gen anio_c=2004

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
*idp_ci       *	
***************

gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"

*****************
***relacion_ci***
*****************
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

	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
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

		  ******************************
          *** VARIABLES DE DIVERSIDAD **
          ******************************
*Nathalia Maya & Antonella Pereira
*Julio 2021	

	
	
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
replace condocup_ci=1 if (p12==1 | p13==1 | p14==1)
replace condocup_ci=2 if (p12==2 | p13==2 | p14==2) & (p16a==1) 
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
replace desalent_ci=1 if p17==5
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

******************
***categopri_ci***
******************
gen categopri_ci=1 if p29a==7| p29a==6 | p29a==11  | p29a==10/* p29a for March 2004 */
replace categopri_ci=2 if p29a==4 | p29a==5  | (p29a>=8 & p29a<=9)
replace categopri_ci=3 if p29a==1 | p29a==2 | p29a==3
replace categopri_ci=4 if p29a==12 | p29a==13
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci


*****************
***horaspri_ci***
*****************
gen horaspri_ci=p24 if p24<=168
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"


******************
***categosec_ci***
******************
gen categosec_ci=1 if p41a==7 |  p41a==6 | p41a==11 | p41a==10/* p41a for March 2004 */
replace categosec_ci=2 if p41a==4 | p41a==5  | (p41a>=8 & p41a<=9)
replace categosec_ci=3 if p41a==1 | p41a==2 | p41a==3
replace categosec_ci=4 if p41a==12 | p41a==13
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

*****************
***horastot_ci***
*****************
gen horastot_ci=thoras 
replace horastot_ci=. if thoras==.
replace horastot_ci = . if horastot_ci>168

label var horastot_ci "Horas totales trabajadas en todas las Actividades"


***************
***subemp_ci***
***************
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot_ci<30 & p49==1
label var subemp_ci "Trabajadores subempleados"
*/
* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p49==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p21b
replace durades_ci=0.5 if p21a ==1 & p21b ==.
label var durades_ci "Duracion del Desempleo (en meses)"


*******************
***antiguedad_ci***
*******************

gen b=p27b/12
egen antiguedad_ci=rsum(p27a b) if emp_ci==1, missing
replace antiguedad_ci=. if p27a==. & b==.
drop b
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"


*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p49==2
label var tiempoparc_ci "Trabajadores a medio tiempo"

*****************
***nempleos_ci***
*****************
gen nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p37==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"


*****************
***tamfirma_ci***
*****************
gen tamfirma_ci=1 if p28a==2
replace tamfirma_ci=0 if p28a==1
label var tamfirma_ci "Trabajadores formales: 1 = + de 10 empleados"
/*
*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p28b>=1 & p28b<=5
replace firmapeq_ci=0 if p28b>=6 & p28b<9999
*/
*****************
***spublico_ci***
*****************

gen spublico_ci= 1 if  p29a==1
replace spublico_ci= 0 if  p29a!=1
label var spublico_ci "Personas que trabajan en el sector publico"

*************
**ocupa_ci***
*************
gen ocupa_ci =.
replace ocupa_ci=1 if p25a >=1 & p25a <=1955
replace ocupa_ci=2 if p25a >=1993 & p25a <=2545
replace ocupa_ci=3 if p25a >=2569 & p25a <=3484
replace ocupa_ci=4 if p25a >=3505 & p25a <=3940
replace ocupa_ci=5 if p25a >=9073 & p25a <=9988 
replace ocupa_ci=6 if p25a >=3962 & p25a <=4541
replace ocupa_ci=7 if (p25a >=4561 & p25a <=9059) 
*recode ocupa_ci (5=8) if (p25a ==9193) 
*replace ocupa_ci=9 
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci



***********************
*** rama_ci         ***
***********************

gen rama_ci=.
replace  rama_ci=1 if (p26a>=111 & p26a<=500) & emp_ci==1
replace  rama_ci=2 if (p26a>=1010 & p26a<=1429) & emp_ci==1
replace  rama_ci=3 if (p26a>=1511 & p26a<=3720) & emp_ci==1
replace  rama_ci=4 if (p26a>=4010 & p26a<=4100) & emp_ci==1
replace  rama_ci=5 if (p26a>=4510 & p26a<=4550) & emp_ci==1
replace  rama_ci=6 if (p26a>=5010 & p26a<=5520) & emp_ci==1
replace  rama_ci=7 if (p26a>=6010 & p26a<=6420) & emp_ci==1
replace  rama_ci=8 if (p26a>=6511 & p26a<=7020) & emp_ci==1
replace  rama_ci=9 if (p26a>=7111 & p26a<=9900) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
/*YL ->linea estimada (no encuentro las originales) correspond al mes de febrero*/
generat lp_ci =1446.60 if zona_c==1
replace lp_ci = 804.67 if zona_c==0
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
gen cesante_ci=1 if p22==1 & condocup_ci==2
replace cesante_ci=0 if p22==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.

gen tamemp_ci = 1 if (p28b>=1 & p28b<=5)
replace tamemp_ci = 2 if (p28b>=6 & p28b<=50)
replace tamemp_ci = 3 if (p28b>50 & p28b!=.)
replace tamemp_ci=. if  p28b>=9999
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"



*************
*ypen_ci*
*************
**Se transforman los dolares a lempiras**
foreach i in p5202c02  p5204c02 {
gen `i'2 = `i'*18.0662 
replace `i'= `i'2
}  
drop p5202c022 p5204c022

*DZ Noviembre 2017: Se incluye pension y jubilacion en usd
egen ypen_ci=rowtotal(p5201c02 p5203c02 p5202c02 p5204c02), missing
label var ypen_ci "Valor de la pension contributiva"



*************
**pension_ci*
*************

gen pension_ci =.
replace pension_ci=1 if ypen_ci!=. & ypen_ci!=0
replace pension_ci=0 if ypen_ci==0 | ypen_ci==.
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
gen tc_ci= .

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2004: mod 2015 MGD salario segun rama
gen salmm_ci=. 	
replace salmm_ci=1911.75 if rama_ci==1
replace salmm_ci=2062.5 if rama_ci==2 | rama_ci==3 | rama_ci==5 | rama_ci==6 | rama_ci==9
replace salmm_ci=2151.75 if rama_ci==7 
replace salmm_ci=2421.38 if rama_ci==8
replace salmm_ci=2225.77 if rama_ci==4 | salmm_ci==.
label var salmm_ci "Salario minimo legal"

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((p18 ==1 | p18==2) & condocup_ci==3)
replace categoinac_ci = 2 if  (p18==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (p18==5 & condocup_ci==3)
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
gen formal_ci=.
label var formal_ci "Formal"


************************************************************************
**************************INGRESOS**************************************
************************************************************************
*Daniela Zuluaga- Noviembre 2017: Se deciden reemplazar las variables del ingreso laboral (Monetario y no Monetario) por las que ya están construidas en la base original**

*NOTA: NO SE CONSIDERAN INGRESOS MENORES A CERO*

***************
***ylmpri_ci***
***************
egen ylmpri_ci=rowtotal(ysmop ycmop yagmop ygamop) if (ysmop>=0 & ycmop>=0 &yagmop>=0 &ygamop>=0) , missing
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
egen ylmsec_ci=rowtotal(ysmos ycmos yagmos ygamos) if (ysmos>=0 & ycmos >=0 & yagmos>=0 & ygamos>=0), missing
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
*ylnmsec_ci*
************
egen ylnmsec_ci=rowtotal(yseos yceos yageos ygaeos) if (yseos>=0 & yceos >=0 & yageos>=0 & ygaeos>=0), missing
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
* Tipo de cambio utilizado en la base 18.0662

/*
p5210c02	p5210c02. remesas del exterior en efectivo (us$)? ult. meses
p5202c02	p5202c02.pension ? ult. meses en dolares
p5204c02	p5204c02.jubilaciones ? ult. meses en dolares
p5206c02	p5206c02.alquiler ? ult. meses en dolares

*/

*Conversión de otros ingresos en dólares a lempiras . Tipo de cambio 18.0662
foreach i in p5210c02 p5206c02 {
gen `i'_ = `i'*18.0662
replace `i'= `i'_
}
drop *_  
/*
Otros ingresos en lempiras	
p5201c02	p5201c02.pension ? ult. meses en lempiras
p5203c02	p5203c02.jubilaciones ? ult. meses en lempiras
p5205c02	p5205c02.alquiler ? ult. meses en lempiras
p5207c02	p5207c02.subsidios (3ra. edad, luz, etc.) ? ult. meses
p5208c02	p5208c02.intereses bancarios ? ult. meses
p5213c02	p5213c02.pension por divorcio? ult. meses
p5214c02	p5214c02.ayudas familiares en efectivo ? ult. mes
p5216c02	p5216c02.ayudas particulares ? ult. mes
p5217c02	p5217c02.bonos ? ult. mes
p5218c02	p5218c02.prestaciones laborales ? ult. mes
p5219c02	p5219c02.otros.? ult. mes
p5209c02

*/

egen ynlm_ci=rsum(p5201c02 p5203c02 p5205c02 p5207c02 p5208c02 p5209c02 p5213c02 p5214c02 p5216c02 p5217c02 p5219c02  p5210c02 p5202c02 p5204c02 p5206c02), missing
label var ynlm_ci "Ingreso No Laboral Monetario"

**************
***ynlnm_ci***
**************
/*
Ingresos en especies dólares	
p5212c02	p5212c02.remesas del exterior en especie (us$)? ult. m
*/

*Conversión de otros ingresos en dólares a lempiras . Tipo de cambio 18.0662
foreach i in p5212c02 {
gen `i'_ = `i'*18.0662 
replace `i'= `i'_
}  
drop *_ 
/*
Ingresos en especies Lempiras	
p5211c02	p5211c02.remesas del exterior en especie (lps.)? ult.
p5215c02	p5215c02.ayudas familiares en especie ? ult. mes

*/
egen ynlnm_ci=rsum(p5212c02 p5211c02 p5215c02), missing

label var ynlnm_ci "Ingreso no laboral no monetario" 
/*

***ypenju2***
gen ypenju2=p5201c03/3 if p5201c03>=0 /* p5201c03 for 2004 */ 
replace ypenju2=p5201c02 if p5201c02>=0 & (p5201c03==. | p5201c03==0) /* p5201c02 for 2004 */
* pensión en dólares ypend (transformado a lempiras en la variable ypen)

***yjub2***
gen yjub2=p5203c03/3 if  p5203c03>=0 /*  p5203c03 for 2004 */
replace  yjub2=p5203c02 if p5203c02>=0 & (p5203c03==. | p5203c03==0) /* p5203c02 for 2004 */
* jubilación en dólares yjubd (transformado a lempiras en la variable ypen)

***yalquile2***
gen yalquile2=p5205c03/3 if  p5205c03>=0 /*  p5205c03 for 2004 */
replace yalquile2= p5205c02 if  p5205c02>=0 & (p5205c03==. | p5205c03==0) /*  p5205c02 for 2004 */
* alquileres en dólares
gen yalquile2d=(p5206c03/3)*18.24 if  p5206c03>=0 /*  p5206c03 for 2004 */
replace yalquile2d= (p5206c02)*18.24 if  p5206c02>=0 & (p5206c03==. | p5206c03==0) /*  p5206c02 for 2004 */

***ysubsi2*****
gen ysubsi2=p5207c03/3 if p5207c03>=0 /* p5207c03 for 2004 */
replace ysubsi2= p5207c02 if  p5205c02>=0 & (p5207c03==. | p5207c03==0) /*  p5207c02 for 2004 */

***ybonos2*****
gen ybonos2=p5217c03/3 if p5217c03>=0 /*  p5217c03 for 2004 */
replace ybonos2=p5217c02 if  p5217c02>=0 & (p5217c03==. | p5217c03==0) /* p5217c02 for 2004 */

***yremesa2*****
gen yremesa2=p5209c03/3 if p5209c03>=0 /* p5209c03 for 2004 */
replace yremesa2=p5209c02 if p5209c02>=0 & (p5209c03==. | p5209c03==0) /* p5209c02 for 2004 */ 
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a enero 2004: 17.98 
                                                                     febrero 2004: 18.06 
								     marzo 2004: 18.14 
								     promedio 3 meses: 18.24 */
***yremesad2****
gen yremesad2=p5210c03/3*18.24 if p5210c03>=0 /* p5210c03 for 2004 */
replace yremesad2=p5210c02*18.24 if p5210c02>=0 & (p5210c03==. | p5210c03==0) /* p5210c02 for 2004 */

***yayuda2******
gen yayuda2=p5214c03/3 if p5214c03>=0 /* p5214c03 for 2004 */
replace yayuda2=p5214c02 if p5214c02>=0 & (p5214c03==. | p5214c03==0) /* p5214c02 for 2004*/ 

***yayupar2*****
gen yayupar2=p5216c03/3 if p5216c03>=0 /* p5216c03 for 2004 */ 
replace yayupar2=p5216c02 if p5216c02>=0 & (p5216c03==. | p5216c03==0) /* p5216c02 for 2004 */

***yotros2******
gen yotros2=p5219c03/3 if p5219c03>=0 /*  p5219c03 for 2004 */ 
replace yotros2= p5219c02 if p5219c02>=0 & (p5219c03==. | p5219c02==0) /*  p5219c02 for 2004 */

***interes2*****
gen interes2=p5208c03/3 if p5208c03>=0 /* p5208c03 for 2004 */
replace interes2=p5208c02 if p5208c02>=0 & (p5208c03==. | p5208c03==0) /* p5208c02 for 2004 */

***prestlab2****
gen prestlab2=p5218c03/3 if p5218c03>=0 /* p5218c03 for 2004 */ 
replace prestlab2=p5218c02 if p5218c02>=0 & (p5218c03==. | p5218c03==0) /* p5218c02 for 2004 */

***yremerasde***
gen yremesade=(p5212c03/3)*18.24 if p5212c03>=0 /* p5212c03 for 2004 */ 
replace yremesade=p5212c02*18.24 if p5212c02>=0 & (p5212c03==. | p5212c03==0) /* p5212c02 for 2004 */

***yremerase****
gen yremesae=p5211c03/3 if p5211c03>=0 /* p5211c03 for 2004 */
replace yremesae=p5211c02 if p5211c02>=0 & (p5211c03==. | p5211c03==0) /* p5211c02 for 2004 */

***yayudae******
gen yayudae=p5215c03/3 if p5215c03>=0 /* p5215c03 for 2004 */
replace yayudae=p5215c02 if p5215c02>=0 & (p5215c03==. | p5215c03==0) /* p5215c02 for 2004 */

/* No estan las ayudas de particulares en especie:*/

/* No estan las herencias */

*Hay pension por divorcio: p5213c03 & p5213c02 for 2004 */ 
/*
***ypdiv********
gen ypdiv=p5213c03/3 if p5213c03>=0
replace ypdiv=p5213c02 if p5213c02>=0 & (p5213c03==. | p5213c03==0)

sum ypdiv if ypdiv>=0


egen ynlm_ci=rsum(ypenju2 yjub2 yjubd ysubsi2 yalquile2 yalquile2d ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypdiv), missing
replace ynlm_ci=. if ypenju2==. & yjubd==. & ysubsi2==. & yalquile2==. & yalquile2d==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypdiv
replace ynlm_ci=0 if p5201c03==. & p5201c02==. & p5203c03==. & p5203c02==. & p5205c03==. & p5205c02==. & p5207c03==. & p5207c02==. & p5217c03==. & p5217c02==. & p5209c03==. & p5209c02==. & p5210c03==. & p5210c02==. & p5214c03==. & p5214c02==. & p5216c03==. & p5216c02==. & p5219c03==. & p5219c02==. & p5208c02==. & p5208c02==. & p5218c03==. & p5218c02==.  & p5213c03==. & p5213c02==. 
label var ynlm_ci "Ingreso No Laboral Monetario"

*/



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
gen autoconsumop_ci=p36 if p36<99999 & p36>=0 /* p36 for 2004 */ 
replace autoconsumop_ci=0 if p36==. & edad>4 & (categopri==1 | categopri==2) & (p12==1 | p13==1) /* p36 for 2004 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p48 if p48<99999 & p48>=0 /* p48 for 2004 */
replace autoconsumos_ci=0 if p48==. & edad>4 & (categosec==1 | categosec==2) & p37==1 
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

egen remesas_ci=rsum(p5209c02	p5210c02	p5211c02	p5212c02), missing
replace remesas_ci=0 if p5209c02==0 &	p5210c02==0 &	p5211c02==0 & p5212c02==0  
label var remesas_ci "Remesas Individuales (monetario + especies)"


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
egen trapri_ci= rowtotal(remesas_ci p5216c02 p5215c02 p5214c02 p5213c02 ), missing
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
egen trapub_ci= rowtotal(progpub_ci p5217c02 p5207c02 ypensub_ci), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(p5205c02 p5206c02 p5208c02  ), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(p5219c02), missing
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
LA DIFERENCIA ES MENOR AL  5%

Además, esta base contiene ingresos laborales negativos, los cuales no estamos considerando*/ 

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

* Años de educacion aprobados **
replace p04b=. if p04b>9
replace p05b=. if p05b>9
** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if p04a>=1 & p04a<=3
replace aedu_ci=p04b if p04a==4 
replace aedu_ci=p04b+6 if p04a==5 | p04a==6
replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+17 if p04a==9
** para quienes asisten actualmente
* MGR Aug, 2015: se corrige sintáxis
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
replace aedu_ci=p05b+12-1 if p05a==7 | p05a==8
replace aedu_ci=p05b+17-1 if p05a==9
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
replace edupre_ci=1 if ((p05a==3 | p04a==3) & aedu_ci ~=.)
replace edupre_ci=0 if (edupre_ci~=1 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"


******************************
*	pqnoasis 
******************************
gen pqnoasis_ci=p03
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
replace repiteult_ci=1 if p05c==1
replace repiteult_ci=0 if p05c==2
label var repiteult_ci "Personas que han repetido el ultimo grado"

******************************
*	edupub_ci 
******************************
g edupub_ci=.
la var edupub_ci "Personas que asisten a centros de ensenanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p04a==7 | p05a==7
replace tecnica_ci=0 if tecnica_ci ~=1 & ( p05a!=99 & p04a!=99)
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
g       pqnoasis1_ci = 1 if p03==10
replace pqnoasis1_ci = 2 if p03==3
replace pqnoasis1_ci = 3 if p03==6  | p03==7
replace pqnoasis1_ci = 4 if p03==2
replace pqnoasis1_ci = 5 if p03==4  | p03==8
replace pqnoasis1_ci = 6 if p03==1
replace pqnoasis1_ci = 7 if p03==9  | p03==11
replace pqnoasis1_ci = 8 if p03==5
replace pqnoasis1_ci = 9 if p03==12 | p03==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

* MGR Jul, 2015: la base disponible no asigna a todos los miembros del hogar  el valor de la variable de vivienda, únicamente al jefe.
* Ya que no contamos con bases originales para hacer merge nuevamente, expandimos variables de vivienda a nivel de hogar. 

foreach var of varlist v01 v02 v03 v04 v05a v05b v05c v05d1 v05d2 v05e v06a v06b v06c v07 v08 v09 v10a v10b v10c v11a v11b v11c v11d ///
v11e v11f v11g v11h v11i v11j v12a v12b v13a v13b1 v13b2 {
bys idh_ch: egen `var'_hog = max(`var') 
}

* MGR Jul, 2015: se modifican todas las variables de vivienda utilizando la variable expandida al hogar en lugar de la variable original


************
*aguared_ch*
************
/*
gen aguared_ch=.
replace aguared_ch=1 if v05a==1
replace aguared_ch=0 if v05a==2 
*/

gen aguared_ch=.
replace aguared_ch=1 if v05a_hog==1
replace aguared_ch=0 if v05a_hog==2 

*************
*aguadist_ch*
*************

/*
gen aguadist_ch=.
replace aguadist_ch=1 if v05e==1
replace aguadist_ch=2 if (v05e==2 | v05e==3)
replace aguadist_ch=3 if v05e==4
*/

gen aguadist_ch=.
replace aguadist_ch=1 if v05e_hog==1
replace aguadist_ch=2 if (v05e_hog==2 | v05e_hog==3)
replace aguadist_ch=3 if v05e_hog==4

*************
*aguamala_ch*
*************

/*
gen aguamala_ch=.
replace aguamala_ch=1 if v05b>=5 & v05b<=8
replace aguamala_ch=0 if v05b>=1 & v05b<=4
*/

gen aguamala_ch=.
replace aguamala_ch=1 if v05b_hog>=5 & v05b_hog<=8
replace aguamala_ch=0 if v05b_hog>=1 & v05b_hog<=4

*************
*aguamide_ch*
*************


gen aguamide_ch=.

********
*luz_ch*
********

/*
gen luz_ch=1 if v07==1 |v07==2 |v07==3 
replace luz_ch=0 if v07>=4 & v07<=8
*/

gen luz_ch=1 if v07_hog==1 |v07_hog==2 |v07_hog==3 
replace luz_ch=0 if v07_hog>=4 & v07_hog<=8

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

/*
gen bano_ch=.
replace bano_ch=1 if v06a==1
replace bano_ch=0 if v06a==2
*/

gen bano_ch=.
replace bano_ch=1 if v06a_hog==1
replace bano_ch=0 if v06a_hog==2

/*
gen banoex_ch=.
replace banoex_ch=1 if  v06c==1
replace banoex_ch=0 if  v06c==2
*/

gen banoex_ch=.
replace banoex_ch=1 if  v06c_hog==1
replace banoex_ch=0 if  v06c_hog==2

/*
gen des1_ch=.
replace des1_ch=0 if v06a==2
replace des1_ch=1 if ( v06b==1| v06b==2)
replace des1_ch=2 if ( v06b==5| v06b==6| v06b==7)
replace des1_ch=3 if ( v06b==3| v06b==4)
*/

gen des1_ch=.
replace des1_ch=0 if v06a_hog==2
replace des1_ch=1 if ( v06b_hog==1| v06b_hog==2)
replace des1_ch=2 if ( v06b_hog==5| v06b_hog==6| v06b_hog==7)
replace des1_ch=3 if ( v06b_hog==3| v06b_hog==4)

/*
gen des2_ch=.
replace des2_ch=1 if (v06b==1|v06b==2|v06b==3)
replace des2_ch=2 if (v06b==4|v06b==5|v06b==6|v06b==7)
replace des2_ch=0 if v06a==2
*/

gen des2_ch=.
replace des2_ch=1 if des1_ch==1 | des1_ch==2
replace des2_ch=2 if des1_ch==3
replace des2_ch=0 if v06a_hog==2

/*
gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8 
*/

gen piso_ch=.
replace piso_ch=0 if v03_hog==7
replace piso_ch=1 if v03_hog>=1 & v03_hog<=6 
replace piso_ch=2 if v03_hog==8 

/*
gen pared_ch=.
replace pared_ch=0 if v02==5 | v02==6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7
*/

gen pared_ch=.
replace pared_ch=0 if v02_hog==5 | v02_hog==6
replace pared_ch=1 if v02_hog>=1 & v02_hog<=4
replace pared_ch=2 if v02_hog==7

/*
gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8
*/

gen techo_ch=.
replace techo_ch=0 if v04_hog==6 | v04_hog==7
replace techo_ch=1 if v04_hog>=1 & v04_hog<=5
replace techo_ch=2 if v04_hog==8

/*
gen resid_ch=.
replace resid_ch=0 if (v08==1|v08==3)
replace resid_ch=1 if (v08==4|v08==6)
replace resid_ch=2 if (v08==2|v08==7)
replace resid_ch=3 if (v08==5|v08==8)
*/

gen resid_ch=.
replace resid_ch=0 if (v08_hog==1|v08_hog==3)
replace resid_ch=1 if (v08_hog==4|v08_hog==6)
replace resid_ch=2 if (v08_hog==2|v08_hog==7)
replace resid_ch=3 if (v08_hog==5|v08_hog==8)

/*
gen dorm_ch=.
replace dorm_ch=v12b if v12b<99 
*/

gen dorm_ch=.
replace dorm_ch=v12b_hog if v12b_hog<99 

/*
gen cuartos_ch=.
replace cuartos_ch=v12a if v12a<99 
*/

gen cuartos_ch=.
replace cuartos_ch=v12a_hog if v12a_hog<99 

***********
*cocina_ch*
***********

/*
gen cocina_ch=.
replace cocina_ch = 1 if v11b ==1
replace cocina_ch = 0 if v11b ==2
*/

gen cocina_ch=.
replace cocina_ch = 1 if v11b_hog ==1
replace cocina_ch = 0 if v11b_hog ==2

**********
*telef_ch*
**********

/*
gen telef_ch=.
replace telef_ch = 1 if v11f ==1
replace telef_ch = 0 if v11f ==2
*/

gen telef_ch=.
replace telef_ch = 1 if v11f_hog ==1
replace telef_ch = 0 if v11f_hog ==2

***********
*regrig_ch*
***********

/*
gen refrig_ch=.
replace refrig_ch=1 if v11a ==1
replace refrig_ch=0 if v11a ==2
*/

gen refrig_ch=.
replace refrig_ch=1 if v11a_hog ==1
replace refrig_ch=0 if v11a_hog ==2

**********
*freez_ch*
**********

gen freez_ch=.

*********
*auto_ch*
*********

/*
gen auto_ch=.
replace auto_ch=1 if v11h ==1
replace auto_ch=0 if v11h ==2
*/

gen auto_ch=.
replace auto_ch=1 if v11h_hog ==1
replace auto_ch=0 if v11h_hog ==2

**********
*compu_ch*
**********

/*
gen compu_ch=.
replace compu_ch=1 if v11i ==1
replace compu_ch=0 if v11i ==2
*/

gen compu_ch=.
replace compu_ch=1 if v11i_hog ==1
replace compu_ch=0 if v11i_hog ==2

*************
*internet_ch*
*************

/*
gen internet_ch=.
replace internet_ch=1 if p10 ==1
replace internet_ch=0 if p10 ==2
*/

gen internet_ch=.
replace internet_ch=1 if p10 ==1
replace internet_ch=0 if p10 ==2

********
*cel_ch*
********

/*
gen cel_ch=.
replace cel_ch=1 if v11g ==1
replace cel_ch=0 if v11g ==2
*/

gen cel_ch=.
replace cel_ch=1 if v11g_hog ==1
replace cel_ch=0 if v11g_hog ==2

**********
*vivi1_ch*
**********

/*
gen vivi1_ch=.
replace vivi1_ch=1 if v01==1 | v01==2
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=8) | v01==3 
*/

gen vivi1_ch=.
replace vivi1_ch=1 if v01_hog==1 | v01_hog==2
replace vivi1_ch=2 if v01_hog==4
replace vivi1_ch=3 if (v01_hog>=5 & v01_hog<=8) | v01==3 

**********
*vivi2_ch*
**********

/*
gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3
*/

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

*************
*viviprop_ch*
*************

/*
gen viviprop_ch=.
replace viviprop_ch=0 if v10a==5

replace viviprop_ch=1 if v10a==1
replace viviprop_ch=2 if v10a==4
replace viviprop_ch=3 if (v10a==2 | v10a==3 | v10a==6)
*/

gen viviprop_ch=.
replace viviprop_ch=0 if v10a_hog==5
replace viviprop_ch=1 if v10a_hog==1
replace viviprop_ch=2 if v10a_hog==4
replace viviprop_ch=3 if (v10a_hog==2 | v10a_hog==3 | v10a_hog==6)

/*
gen vivitit_ch=.
replace vivitit_ch=1 if v13a==1
replace vivitit_ch=0 if v13a==2
*/

gen vivitit_ch=.
replace vivitit_ch=1 if v13a_hog==1
replace vivitit_ch=0 if v13a_hog==2


/*
gen vivialq_ch=.
replace vivialq_ch=v10c if v10b==1 & viviprop_ch==0
replace vivialq_ch=v10c*18.24 if v10b==2 & viviprop_ch==0
*/

gen vivialq_ch=.
replace vivialq_ch=v10c_hog if v10b_hog==1 & viviprop_ch==0
replace vivialq_ch=v10c_hog*18.24 if v10b_hog==2 & viviprop_ch==0

gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g  aguamejorada_ch = 1 if (v05b >=1 & v05b <=4)
replace aguamejorada_ch = 0 if (v05b >=5 & v05b <=8)

*********************
***banomejorado_ch***
*********************
g banomejorado_ch = 1 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=7)) & v06c ==1)
replace banomejorado_ch = 0 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=7)) & v06c ==2) | (v06b >=3 & v06b <=4) | (v06a==2)

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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close

