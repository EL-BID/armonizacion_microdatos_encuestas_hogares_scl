

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
local ANO "2006"
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
Autores: Revised March, 2008 (by tede) 
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
 
label var region_c "División política"


************
****pais****
************

gen pais_c="HND"

**********
***anio***
**********

gen anio_c=2006

*********
***mes***
*********

gen mes_c=5
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

	***************************
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
label define sexo_ci 1 "Hombre" 2 "Mujer"
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
gen condocup_ci=.
replace condocup_ci=condact
replace condocup_ci=4 if condact == 4 | edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
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
replace desalent_ci=0 if condocup_ci==3
replace desalent_ci=1 if p23==6
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*****************
***horaspri_ci***
*****************
gen horaspri_ci= p42 if  p42<=168
 label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"
 gen horassec_ci= p72 if  p72<168
 
*****************
***horastot_ci***
*****************
egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci=. if horaspri_ci==. & horassec_ci==.
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0
label var horastot_ci "Horas totales trabajadas en todas las Actividades"

***************
***subemp_ci***
********
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot_ci<30 &  p97==1
label var subemp_ci "Trabajadores subempleados"

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p26/(365/12)      if p26a==1
replace durades_ci=p26/((365/7)/12)  if p26a==2
replace durades_ci=p26               if p26a==3
label var durades_ci "Duracion del Desempleo (en meses)"
 
*******************
***antiguedad_ci***
*******************
gen b=  p46/365 if  p46a == 1
 replace b=  p46/52 if  p46a ==2
 replace b=  p46/12 if  p46a ==3
 replace b=  p46 if  p46a ==4
 
 gen antiguedad_ci=b if emp_ci==1
 replace antiguedad_ci=. if p46a==. & b==.
 drop b
 label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"
 

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p97==2
label var tiempoparc_ci "Trabajadores a medio tiempo"

*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if  p67==1
replace nempleos_ci=2 if  p67==2
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"
/*
*************
*firmapeq_ci*
*************
*Asalariados, cuenta propia, y cuenta propia de la actividad principal
gen firmapeq_ci=0 if (p58a>5 & p58a<99999) | (p66a>5 & p66a<99999) 
replace firmapeq_ci=1 if (p58a<=5 & p58a!=0) | (p66a<=5 & p66a!=0) 
 */
 
*****************
***spublico_ci***
*****************
gen spublico_ci=1 if p43==1
replace spublico_ci=0 if p43!=1 

label var spublico_ci "Personas que trabajan en el sector publico"


*************
**ocupa_ci***
*************
tostring  p39a, replace
replace  p39a = "0" +  p39a if length( p39a)==6
gen labor=substr( p39a,1,4)
destring labor, replace 

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999
replace ocupa_ci=2 if labor>=1000 & labor<=1999
replace ocupa_ci=3 if labor>=4000 & labor<=4999
replace ocupa_ci=4 if labor>=5200 & labor<=5999
replace ocupa_ci=5 if labor>=5000 & labor<=5199
replace ocupa_ci=6 if labor>=6000 & labor<=6999
replace ocupa_ci=7 if labor>=7000 & labor<=8999
replace ocupa_ci=8 if labor>=0 & labor<=999
replace ocupa_ci=9 if (labor>=9000 & labor<=9996) | labor==9999
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

gen categopri_ci=1 if p43==7| p43==6 | p43==10| p43==11 /* p43 for March 2005 */
replace categopri_ci=2 if p43==4 | p43==5 | p43==8 | p43==9
replace categopri_ci=3 if p43==1 | p43==2 | p43==3
replace categopri_ci=4 if p43==12 | p43==13
recode categopri_ci (1=2) if (p43==6 | p43==10) &  p57 ==4
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************
***categosec_ci***
******************

gen categosec_ci=1 if p73==7| p73==6 | p73==10 | p73==11 /* p73 for March 2005 */
replace categosec_ci=2 if p73==4 | p73==5 | p73==8 | p73==9
replace categosec_ci=3 if p73==1 | p73==2 | p73==3
replace categosec_ci=4 if p73==12 | p73==13
recode categosec_ci (1=2) if (p73==6 | p73==10) &  p87==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci = 1775.1 if zona_c == 1
replace lp_ci = 895.1 if zona_c == 0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci = 887.55 if zona_c == 1
replace lpe_ci = 670.47 if zona_c == 0
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if p68_01==1 | p68_02==1 | p68_03==1 | p68_04==1 | p68_05==1 | p68_06==1 | p68_07==1 | p68_08==1 | p68_09==1 | p68_10==1 
recode cotizando_ci .=0 if condact==1 | condact==2
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
replace tipocontrato_ci=1 if p47==1 
replace tipocontrato_ci=3 if p47==2 | p47==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if p27 ==1 & condocup_ci==2
replace cesante_ci=0 if p27 ==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

foreach var in p58a p66a {
recode `var' (99999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (p58a>=1 & p58a<=5) | (p66a>=1 & p66a<=5) 
replace tamemp_ci = 2 if (p58a>=6 & p58a<=50) | (p66a>=6 & p66a<=50) 
replace tamemp_ci = 3 if (p58a>50 & p58a~=.) | (p66a>50 & p66a~=.) 
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande", modify
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*************
*ypen_ci*
*************
*DZ, Noviembre 2017: Se incluye tambien la pension y jubilacion en usd.
gen pen=p00l01c5/3 
gen pen_usd=(p00l01c6*19.03)/3 
gen jubi=p00l02c5/3
gen jubi_usd=(p00l02c6*19.03)/3
egen ypen_ci=rowtotal(pen pen_usd jubi jubi_usd), missing
label var ypen_ci "Valor de la pension contributiva"


*************
**pension_ci*
*************
gen pension_ci=(ypen_ci>0 & ypen_ci!=.)
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

* HON 2006
gen salmm_ci= 2759.70
label var salmm_ci "Salario minimo legal"


*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((p22 ==1 | p22==2) & condocup_ci==3)
replace categoinac_ci = 2 if  (p22==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (p22==5 & condocup_ci==3)
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
egen ylmpri_ci=rowtotal(ysmop ycmop yagmop ygamop ycrmop) if (ysmop>=0 & ycmop>=0 & yagmop>=0 & ygamop>=0 & ycrmop>=0), missing
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
egen ylnmpri_ci=rowtotal(yseop yceop yageop ygaeop ) if (yseop>=0 & yceop>=0 & yageop>=0 & ygaeop>=0) , missing
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********
egen ylmsec_ci=rowtotal(ysmos ycmos yagmos ygamos ycrmos) if (ysmos >=0 & ycmos>=0 & yagmos>=0 & ygamos>=0 & ycrmos>=0), missing
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
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a 
                                                                     marzo 2006: 19.03 
								     abril 2006: 19.03
								     mayo 2006:  19.03
								     promedio 3 meses: 19.03 */                                                     

foreach i in p00l01c5	p00l02c5	p00l03c5	p00l04c5	p00l05c5	p00l06c5	p00l07c5	p00l08c5	p00l09c5	p00l10c5	p00l11c5	p00l12c5	p00l01c6	p00l02c6	p00l03c6	p00l05c6	p00l06c6	p00l07c6	p00l08c6	p00l09c6	p00l11c6 ///
p00l12c6	p00l06c7	p00l08c7	p00l09c7	p00l12c7	p00l06c8	p00l08c8	p00l09c8	p00l12c8 {
gen y_`i' = `i'/3
}  

*Se aplica la tasa de cambio:

foreach i in y_p00l01c6 y_p00l02c6 y_p00l03c6 y_p00l05c6 y_p00l06c6 y_p00l07c6 y_p00l08c6 y_p00l09c6 y_p00l11c6 y_p00l12c6 y_p00l06c8 y_p00l08c8 y_p00l09c8 y_p00l12c8 {
replace `i' = `i'*19.03
}

/*
p00l01c5        double %10.0g                 p00-pension (vlr. de ult 3 meses efvo lps)
p00l02c5        double %10.0g                 p00-jubilación (vlr. de ult 3 meses efvo lps)
p00l03c5        double %10.0g                 p00-alquileres (vlr. de ult 3 meses efvo lps)
p00l04c5        double %10.0g                 p00-subsidios (vlr. de ult 3 meses efvo lps)
p00l05c5        float  %9.0g                  p00-intereses bco (vlr. de ult 3 meses efvo lps)
p00l06c5        double %10.0g                 p00-remesas del exterior (vlr. de ult 3 meses efvo lps)
p00l07c5        long   %12.0g                 p00-pensión x divorcio (vlr. de ult 3 meses efvo lps)
p00l08c5        float  %9.0g                  p00-ayudas familiares (vlr. de ult 3 meses efvo lps)
p00l09c5        int    %8.0g                  p00-ayudas particulares (vlr. de ult 3 meses efvo lps)
p00l10c5        long   %12.0g                 p00-bonos (vlr. de ult 3 meses efvo lps)
p00l11c5        long   %12.0g                 p00-prestaciones laborales (vlr. de ult 3 meses efvo lps)
p00l12c5        long   %12.0g                 p00-otros (vlr. de ult 3 meses efvo lps)

p00l01c6        int    %8.0g                  p00-pension (vlr. de ult 3 meses efvo us$)
p00l02c6        int    %8.0g                  p00-jubilación (vlr. de ult 3 meses efvo us$)
p00l03c6        int    %8.0g                  p00-alquileres (vlr. de ult 3 meses efvo us$)
p00l05c6        int    %8.0g                  p00-intereses bco (vlr. de ult 3 meses efvo us$)
p00l06c6        long   %12.0g                 p00-remesas del exterior (vlr. de ult 3 meses efvo us$)
p00l07c6        int    %8.0g                  p00-pensión x divorcio (vlr. de ult 3 meses efvo us$)
p00l08c6        int    %8.0g                  p00-ayudas familiares (vlr. de ult 3 meses efvo us$)
p00l09c6        int    %8.0g                  p00-ayudas particulares (vlr. de ult 3 meses efvo us$)
p00l11c6        int    %8.0g                  p00-prestaciones laborales (vlr. de ult 3 meses efvo us$)
p00l12c6        int    %8.0g                  p00-otros (vlr. de ult 3 meses efvo us$)

p00l06c7        int    %8.0g                  p00-remesas del exterior (vlr. de ult 3 meses esp lps)
p00l08c7        float  %9.0g                  p00-ayudas familiares (vlr. de ult 3 meses esp lps)
p00l09c7        int    %8.0g                  p00-ayudas particulares (vlr. de ult 3 meses esp lps)
p00l12c7        double %10.0g                 p00-otros (vlr. de ult 3 meses esp lps)

p00l06c8        int    %8.0g                  p00-remesas del exterior (vlr. de ult 3 meses esp us$)
p00l08c8        int    %8.0g                  p00-ayudas familiares (vlr. de ult 3 meses esp us$)
p00l09c8        int    %8.0g                  p00-ayudas particulares (vlr. de ult 3 meses esp us$)
p00l12c8        int    %8.0g                  p00-otros (vlr. de ult 3 meses esp us$)
*/


egen ynlm_ci=rsum(y_p00l01c5 y_p00l02c5	y_p00l03c5	y_p00l04c5	y_p00l05c5	y_p00l06c5	y_p00l07c5	y_p00l08c5	y_p00l09c5	y_p00l10c5	y_p00l12c5	y_p00l01c6	y_p00l02c6	y_p00l03c6	y_p00l05c6	y_p00l06c6	y_p00l07c6	y_p00l08c6	y_p00l09c6	 y_p00l12c6), missing
label var ynlm_ci "Ingreso No Laboral Monetario"

**************
***ynlnm_ci***
**************
egen ynlnm_ci=rsum(y_p00l06c7	y_p00l08c7	y_p00l09c7	y_p00l12c7	y_p00l06c8	y_p00l08c8	y_p00l09c8	y_p00l12c8), missing

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

gen autoconsumop_ci=p65 if p65<99999 & p65>=0 /* p65 for May 2006 */
replace autoconsumop_ci=0 if p65==. & edad>4 & (categopri==1 | categopri==2) & (p16==1 | p17==1) /* p64 for May 2006 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p95 if p95<99999 & p95>=0 /* p95 for May 2006 */ 
replace autoconsumos_ci=0 if p95==. & edad>4 & (categosec==1 | categosec==2) & p67==1 
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
*Los valores en dólares ya fueron convertidos a lempiras para el cálculo del ingreso no laboral monetario.
egen remesas_ci=rsum(y_p00l06c5 y_p00l06c6 y_p00l06c7 y_p00l06c8 ), missing

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
egen trapri_ci= rowtotal(remesas_ci y_p00l07c5 y_p00l08c5 y_p00l09c5 y_p00l07c6 y_p00l08c6 y_p00l09c6 y_p00l08c7 y_p00l09c7 y_p00l08c8 y_p00l09c8), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

****************
***progpub_ci***
****************
gen progpub_ci= .
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas"


****************
***progpub_ch***
****************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci ypensub_ci y_p00l10c5 y_p00l04c5 ), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

****************
***capital_ci***
****************
egen capital_ci= rowtotal(y_p00l03c5 y_p00l03c6 y_p00l05c5 y_p00l05c6), missing
label var capital_ci "Ingreso por renta del capital" 

****************
***capital_ch***
****************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

*****************
***otros_ci***
*****************
egen otros_ci= rowtotal(y_p00l12c5 y_p00l12c6 y_p00l12c7 y_p00l12c8), missing
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

/*NOTA: El ingreso total aquí construido difiere de la variable de ingreso total de la base original . Esto ocurre ya que al agregar algunas de las variables
desagregadas en la base, no coinciden con las variables agregadas que están construidas en la base original (para los ingresos no laborales), no hay información 
suficiente para saber cómo se agregaron dichas variables en la base original;
En particular, hay inconsistencia con la periodicidad de las variables de otros ingresos.En todo caso, construimos los ingresos no laborales con las variables desagregadas, 
teniendo como base la periodicidad específicada en el cuestionario.
LA DIFERENCIA ES MENOR AL 3% 
No se consideran ingresos laborales negativos*/

******************************************************************************
*	Educación
*****************************************************************************

************
* asiste_ci*
************

gen asiste_ci=.
replace asiste_ci=1 if  p02 ==1 /* p019 for Sept 2006 */
replace asiste_ci=0 if  p02 ==2
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
replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+17 if p04a==9
** para quienes asisten actualmente
replace aedu_ci=0 if p05a==1 | p05a==2
replace aedu_ci=p05b-1 if p05a==3
replace aedu_ci=p05b+6-1 if p05a==4 | p05a==5
replace aedu_ci=p05b+12-1 if p05a==6 | p05a==7
replace aedu_ci=p05b+17-1 if p05a==8
label var aedu_ci "Años de educacion aprobados"
*/


*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU
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
replace aedu_ci=0 if p05a==2 | p05a==3
replace aedu_ci=p05b-1 if p05a==4
replace aedu_ci=p05b+6-1 if p05a==5
replace aedu_ci=p05b+9-1 if p05a==6
replace aedu_ci=p05b+12-1 if p05a==7 | p05a==8 | p05a==10
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
gen pqnoasis_ci=p03
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
replace edupub_ci=1 if (p08==1|p08==2|p08==3|p08==4|p08==7)
replace edupub_ci=0 if (p08==5|p08==6|p08==8|p08==9)
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
g       pqnoasis1_ci = 1 if p03==7
replace pqnoasis1_ci = 2 if p03==11
replace pqnoasis1_ci = 3 if p03==6
replace pqnoasis1_ci = 4 if p03==3
replace pqnoasis1_ci = 5 if p03==4  | p03==10
replace pqnoasis1_ci = 6 if p03==2
replace pqnoasis1_ci = 7 if p03==8  | p03==9
replace pqnoasis1_ci = 8 if p03==5
replace pqnoasis1_ci = 9 if p03==1 | p03==12

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
replace des2_ch=2 if (v06b==4|v06b==5|v06b==6|v06b==7 |v06b==8)
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
replace dorm_ch=v15b if v15b>=0 

gen cuartos_ch=.
replace cuartos_ch=v15a if v15a>=0 



***********
*cocina_ch*
***********

gen cocina_ch=.

**********
*telef_ch*
**********
gen telef_ch=.
replace telef_ch=1 if v12f==1 | v12g==1 
replace telef_ch=0 if v12f==2 | v12g==2  


***********
*regrig_ch*
***********

gen refrig_ch=.
replace refrig_ch=1 if v12a==1 
replace refrig_ch=0 if v12a==2

***************
**freez_ch    *
***************

gen freez_ch=.

***************
**auto_ch     *
***************

gen auto_ch=.
replace auto_ch=1 if v12h==1 
replace auto_ch=0 if v12h==2

***************
**compu_ch    *
***************

gen compu_ch=.
replace compu_ch=1 if v12k==1 
replace compu_ch=0 if v12k==2

***************
**internet_ch *
***************

gen internet_ch=.

***************
**cel_ch      *
***************

gen cel_ch=.

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
replace viviprop_ch=0 if v13==5 
replace viviprop_ch=1 if v13==1
replace viviprop_ch=2 if v13==4
replace viviprop_ch=3 if v13==6 | v13==2 | v13==3 | v13==7 /* v13==7 included for 2005 */

***************
**vivitit_ch  *
***************

gen vivitit_ch=.
replace vivitit_ch=1 if v16a==1 
replace vivitit_ch=0 if v16a==2


***************
**vivialq_ch  *
***************
gen vivialq_ch=.
replace vivialq_ch=v14b if v14a==1
replace vivialq_ch=v14b*19.03 if v14a==2


***************
*vivialqimp_ch*
***************

gen vivialqimp_ch=.


**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v05b >=1 & v05b <=4) | v05b==8
replace aguamejorada_ch = 0 if (v05b >=5 & v05b <=7) | v05b==9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=8)) & v06c ==1)
replace banomejorado_ch = 0 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=8)) & v06c ==2) | (v06b >=3 & v06b <=4) | (v06a==2)

**Otras Variables**


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
