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
local ANO "1996"
local ronda m9

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m9
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización 4 de Octubre de 2013: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
 opened on:  16 Aug 2006, 12:39:18
 Modificación marzo 2013: Andres Felipe Sanchez
Email: andressa@iadb.org, anfesanz@gmail.com


*******************************************************************************************************
******                                    HONDURAS 1996                                              **
******            EPHPM 1996 (Encuesta Permenente de Hogares de Propositos Multiples                 **
******                                    6.428 hogares                                              **
******                                   33.172 personas                                             **
*******************************************************************************************************
****************************************************************************/

use "`base_in'", clear
/*foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
*/
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

gen region_c=  DEPTO
label define region_c 1 "Atlántida" 2 "Colón" 3 "Comayagua" ///
4 "Copán" 5 "Cortés" 6 "Choluteca" 7 "El Paraíso" 8 "Francisco de Morazán" ///
10 "Intibuca" 12 "La Paz" 13 "Lempira" 14 "Ocotepeque" 15 "Olancho" 16 "Santa Bárbara" ///
17 "Valle" 18 "Yoro"
label var region_c "División política"


***************
***factor_ch***
***************
gen factor_ch=FACTOREX

***************
****idh_ch*****
*************** 
egen idh_ch=group(DOMINIO SEGMENTO VIVIENDA HOGAR)

*************
****idp_ci****
**************
gen idp_ci=ORDENPER
label var idp_ci "Identificador Individual dentro del Hogar"
           
**********
***zona***
**********
gen zona_c=1 if DOMINIO==1 | DOMINIO==2 | DOMINIO==3 | DOMINIO==4
replace zona_c=0 if DOMINIO==5
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

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

************
****pais****
************
gen pais_c="HND"

**********
***anio***
**********
gen anio_c=1996

*********
***mes***
*********
gen mes_c=9
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"
label value mes_c mes_c


*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if PARENTCO==1
replace relacion_ci=2 if PARENTCO==2
replace relacion_ci=3 if PARENTCO==3 
replace relacion_ci=4 if PARENTCO==4 | PARENTCO==5 | PARENTCO==6
replace relacion_ci=5 if PARENTCO==7
replace relacion_ci=6 if PARENTCO==8
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "Conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
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
gen sexo_ci=SEXO
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=EDAD if EDAD<99
label var edad_ci "Edad del Individuo"

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 if ESTCIVIL==5
replace civil_ci=2 if ESTCIVIL==1 | ESTCIVIL==6
replace civil_ci=3 if ESTCIVIL==3 | ESTCIVIL==4
replace civil_ci=4 if ESTCIVIL==2
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
recode CONDACT (1=1) (2 3=2) (4/9=3), gen(condocup_ci)
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Comprobacion con variables originales.  Se considera ocupado a quienes estan en trabajos no remunerados.

g condocup_ci=.
replace condocup_ci=1 if (TRABAJO==1 | TIENETRB==1 | REALITRB==1)
replace condocup_ci=2 if (TRABAJO==6 | TIENETRB==6 | REALITRB==6) & (BUSCOSEM==1 | BUSCOMES==1) 
recode condocup_ci (.=3) if edad_ci>=10
recode condocup_ci (.=4) if edad_ci<10
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

replace desalent_ci=0 if pea_ci==1

replace desalent_ci=1 if PQNOBUS==5

label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 



******************
***categopri_ci***
******************
gen categopri_ci=1 if CATEG==7| CATEG==6
replace categopri_ci=2 if CATEG==4 | CATEG==5 
replace categopri_ci=3 if CATEG==1 | CATEG==2 | CATEG==3
replace categopri_ci=4 if CATEG==8
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci


*****************
***horaspri_ci***
*****************
gen horaspri_ci=HRSOCUPP if  HRSOCUPP<99 &  HRSOCUPP>0

label var horaspri "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen horassec_ci=HRSOCUPS if  HRSOCUPS<99 &  HRSOCUPS>0


******************
***categosec_ci***
******************
gen categosec_ci=.


*****************
***horastot_ci***
*****************
gen horastot_ci=HRSTRAB  if HRSTRAB~=99 &  HRSTRAB>0
replace  horastot=. if HRSTRAB==. 
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las actividades"


***************
***subemp_ci***
***************
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & DESTRB==1
label var subemp_ci "Trabajadores subempleados"
*/
* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & DESEA==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"


************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=TPOBUS if TPOBUS<99 & TPOBUS>0
replace durades=0.5 if CTPOBUS==1 & (TPOBUS==0  | TPOBUS==.)
label var durades "Duracion del Desempleo (en meses)"

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=ANOSTRAB if emp_ci==1 & ANOSTRAB<99 & ANOSTRAB>0
replace antiguedad_ci=0.5 if TPOTRABA==1 & emp_ci==1 & antiguedad_ci==.
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.

replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1

replace tiempoparc_ci=1 if horastot<30 & DESTRB==6

label var tiempoparc_ci "Trabajadores a medio tiempo" 

*****************
***nempleos_ci***
*****************

gen nempleos_ci=1 if emp_ci==1

replace nempleos=2 if emp_ci==1 & OTROTRAB==1

replace nempleos_ci=. if emp_ci==0

label var nempleos "Numero de empleos"


*****************
***tamfirma_ci***
*****************
gen tamfirma_ci=.

replace tamfirma=1 if TAMAEST==2

replace tamfirma=0 if TAMAEST==1

label var tamfirma "Trabajadores formales: 1 = + de 10 empleados"

/*
******************************
*	firmapeq_ci
******************************
g firmapeq_ci=(TAMAEST==1 & (PERSTRAB>=1 & PERSTRAB<=5)) if emp_c==1
replace firmapeq_ci=. if emp_ci!=1 | PERSTRAB==.
la var firmapeq_ci "Trabajadores formales. 1=5 o menos trabajadores"*/

*****************
***spublico_ci***
*****************
gen spublico_ci= 1 if CATEG == 1
replace spublico_ci= 0 if CATEG !=1
label var spublico "Personas que trabajan en el sector publico"

label var spublico "Personas que trabajan en el sector publico"


*************
**ocupa_ci***
*************
gen ocupa_ci=.
replace ocupa_ci=1 if (OCUPAC >0 & OCUPAC <=950) & emp_ci ==1
replace ocupa_ci=2 if OCUPAC >=1000 & OCUPAC <=1290
replace ocupa_ci=3 if OCUPAC >=2000 & OCUPAC <=2790
replace ocupa_ci=4 if OCUPAC >=3000 & OCUPAC <=3390
replace ocupa_ci=5 if OCUPAC >=9000 & OCUPAC <=9810 
replace ocupa_ci=6 if OCUPAC >=4000 & OCUPAC <=4520
replace ocupa_ci=7 if (OCUPAC >=5000 & OCUPAC <=8150) 
*replace ocupa_ci=8
*replace ocupa_ci=9 
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci


*************
***rama_ci***
*************
gen rama_ci=.
replace  rama_ci=1 if (RAMACT>=1110 & RAMACT<=1302) & emp_ci==1
replace  rama_ci=2 if (RAMACT>=2100 & RAMACT<=2909) & emp_ci==1
replace  rama_ci=3 if (RAMACT>=3111 & RAMACT<=3909) & emp_ci==1
replace  rama_ci=4 if (RAMACT>=4101 & RAMACT<=4200) & emp_ci==1
replace  rama_ci=5 if RAMACT==5000 & emp_ci==1
replace  rama_ci=6 if (RAMACT>=6100 & RAMACT<=6320) & emp_ci==1
replace  rama_ci=7 if (RAMACT>=7111 & RAMACT<=7200) & emp_ci==1
replace  rama_ci=8 if (RAMACT>=8101 & RAMACT<=8329) & emp_ci==1
replace  rama_ci=9 if (RAMACT>=9100 & RAMACT<=9600) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

**********
**tc_ci***
**********
gen tc_ci=12.93
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HON 1996
gen salmm_ci= 	665.16
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
label var cotizando_ci "Cotizante a la Seguridad Social"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"

*************
*tamemp_ci
*************
gen tamemp_ci =.
replace tamemp_ci=1 if (TAMAEST==1 & (PERSTRAB>=1 & PERSTRAB<=5))
replace tamemp_ci=2 if (TAMAEST==1 & (PERSTRAB>=6 & PERSTRAB<=9))
replace tamemp_ci=3 if (TAMAEST==2)
label var tamemp_ci "# empleados en la empresa"
label define tamemp_ci  1 "Pequeñas" 2 "Medianas" 3 "Grandes"
label value tamemp_ci tamemp_ci
*************
*cesante_ci* 
*************
gen cesante_ci=TRABANT if TRABANT==1 & condocup_ci==2
replace cesante_ci=0 if TRABANT==6 & condocup_ci==2
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	

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
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"
****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 


*************
**pension_ci*
*************

gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
/*YL ->linea estimada (no encuentro las originales) correspond al mes de agosto*/
generat lp_ci=763.20 if zona_c==1
replace lp_ci=424.53 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if (ACTIVID==1 & condocup_ci==3)
replace categoinac_ci = 2 if  (ACTIVID==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (ACTIVID==4 & condocup_ci==3)
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

***************
***ylmpri_ci***
***************

gen ylmpri_ci=.
replace ylmpri_ci=YOCUPP if YOCUPP<99999 & EDAD>9 & YOCUPP>0
replace ylmpri_ci=GANOCUPP if GANOCUPP<99999 & EDAD>9 & GANOCUPP>0
replace ylmpri_ci=0 if YOCUPP==0 & GANOCUPP==0 & EDAD>9 & (TRABAJO==1 | REALITR==1)
replace ylmpri_ci=0 if CATEG==8 & EDAD>9 & (TRABAJO==1 | REALITR==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"


*****************
***nrylmpri_ci***
*****************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


****************
***ylnmpri_ci***
****************

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************
gen ylmsec_ci=.

replace ylmsec_ci=YOCUPS if YOCUPS<99999 & EDAD>9 & YOCUPS>0

replace ylmsec_ci=GANOCUPS if GANOCUPS<99999 & EDAD>9 & GANOCUPS>0

replace ylmsec_ci=0 if YOCUPS==0 & GANOCUPS==0 & EDAD>9 & OTROTR==1

label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"


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
* Los ingresos no laborales monetarios constan en el formulario, pero no en la base de datos.
gen ynlm_ci=.
label var ynlm_ci "Ingreso no laboral monetario" 


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
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

******************************
*	autocons_ci 
******************************
g autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing 
la var autocons_ch "Autoconsumo del Hogar"

****************
***remesas_ci***
****************
g remesas_ci=.
la var remesas_ci "Cash remittances from abroad"


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
gen trapri_ci= .
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
gen trapub_ci= .
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
gen capital_ci= .
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
gen otros_ci= .
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

******************************************************************************
*	Educación
******************************************************************************


gen asiste_ci=.

replace asiste_ci=1 if ASISTE==1

replace asiste_ci=0 if ASISTE==6

label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"


* Años de educacion aprobados **
replace GRADO=. if GRADO==9

gen aedu_ci=.

replace aedu_ci=0 if (NIVEL>=1 & NIVEL<=3) | (NIVEL==4 & GRADO==0)

*consistent approach*
replace aedu_ci=GRADO if NIVEL==4 & GRADO>=1

replace aedu_ci=GRADO+6 if (NIVEL==5 | NIVEL==6) & GRADO>=0

replace aedu_ci=GRADO+12 if (NIVEL==7 | NIVEL==8) & GRADO>=0

*replace aedu_ci=GRADO+16 if NIVEL==9
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
replace edupre_ci=1 if (NIVEL==2 & aedu_ci ~=.)
replace edupre_ci=0 if (NIVEL~=2 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

******************************
*	pqnoasis 
******************************
g pqnoasis_ci=. /*NA*/
******************************
*	repite_ci 
******************************
g repite_ci=.  /*NA*/
******************************
*	repiteult_ci 
******************************
g repiteult_ci=. /*NA*/
******************************
*	edupub_ci 
******************************
g edupub_ci=.
la var edupub_ci "Personas que asisten a centros de ensenanza publicos"

*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=NIVEL==6|NIVEL==7
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

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
g       pqnoasis1_ci =.

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
gen aguared_ch=0
replace aguared_ch = 1 if AGUAA ==1 | AGUAA==2

gen aguadist_ch=.

replace aguadist_ch=1 if AGUAB==6

replace aguadist_ch=2 if AGUAB==7 

replace aguadist_ch=3 if AGUAB==8

gen aguamala_ch=.

replace aguamala_ch=1 if AGUAA>=4 & AGUAA<=5

replace aguamala_ch=0 if AGUAA>=1 & AGUAA<=3

gen aguamide_ch=.

gen luz_ch=.

replace luz_ch=1 if LUZ==1 | LUZ==2 | LUZ==3

replace luz_ch=0 if LUZ==4

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.

replace bano_ch=1 if SERVSANA==1 | SERVSANA==2

replace bano_ch=0 if SERVSANA==3

gen banoex_ch=.

replace banoex=1 if USOSERV==7

replace banoex=0 if USOSERV==8

gen des1_ch=.
replace des1_ch=0 if SERVSANA==3
replace des1_ch=1 if SERVSANB==4 | SERVSANB==5
replace des1_ch=2 if SERVSANB==2 | SERVSANB==6

/*
des1_ch Tipo de desagüe incluyendo la definición de "Unimproved" del MDG
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general o a una cámara séptica
2 El desagüe está conectado a un pozo ciego o es una letrina.
3 El desagüe se comunica con la superficie: desemboca en un río o en la calle.*/
* No hay casos para la categoría 3.

label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


gen des2_ch=.
replace  des2_ch=0 if  des1_ch==0
replace  des2_ch=1 if des1_ch==1 | des1_ch==2

/*
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general, a una cámara o fosa séptica, o a un pozo ciego o letrina.
2 Cualquier otro*/
* No hay casos para la categoría 2.
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch




gen piso_ch=.

replace piso_ch=0 if PISO==5

replace piso_ch=1 if PISO>=1 & PISO<=4 

*replace piso_ch=2 if PISO==

gen pared_ch=.

replace pared_ch=0 if PAREDES==4 | PAREDES==5

replace pared_ch=1 if PAREDES>=1 & PAREDES<=3

*replace pared_ch=2 if PAREDES==6

gen techo_ch=.

gen resid_ch=.

gen dorm_ch=.

replace dorm_ch=NRODORM 


gen cuartos_ch=.

replace cuartos_ch=NROHAB

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

replace vivi2_ch=1 if TIPOVIV==1

replace vivi2_ch=0 if TIPOVIV>=2 & TIPOVIV<=6

gen viviprop_ch=.

replace viviprop_ch=0 if TENENCIA==3

replace viviprop_ch=1 if TENENCIA==1

replace viviprop_ch=2 if TENENCIA==2

replace viviprop_ch=3 if TENENCIA==4 | TENENCIA==5 | TENENCIA==6

gen vivitit_ch=.

gen vivialq_ch=.

replace vivialq_ch=PAGOMENS if PAGOMENS>0 & PAGOMENS<9999 & viviprop_ch==0


gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g   aguamejorada_ch = 1 if (AGUAA >=1 & AGUAA <=3)
replace aguamejorada_ch = 0 if (AGUAA >=4 & AGUAA <=5)

*********************
***banomejorado_ch***
*********************
g  banomejorado_ch = 1 if  (SERVSANA >=1 & SERVSANA <=2) & (SERVSANB >=4 & SERVSANB <=6) & USOSERV ==7
replace banomejorado_ch = 0 if ((SERVSANA >=1 & SERVSANA <=2) & (SERVSANB >=4 & SERVSANB <=6) & USOSERV ==8) | ((SERVSANA >=1 & SERVSANA <=2) & (SERVSANB !=4 | SERVSANB !=5 | SERVSANB !=6)) | SERVSANA ==3
	


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






