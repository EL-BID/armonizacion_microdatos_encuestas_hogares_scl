

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
local ANO "2002"
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
Round: m9
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Diciembre de 2017
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*******************************************************************************************************
*****                                  HONDURAS 2002 - MAYO                                           *
*****                EPHPM 2002 (Encuesta Permanente de Hogares de Propositos Multiples)              *
*****                                  104.907 personas                                               *
*****                                   21.189 hogares                                                *
*******************************************************************************************************
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
***************
***factor_ch***
***************
gen factor_ch=factor

***************
****idh_ch*****
*************** 
egen idh_ch=group(hogar depto domi estrato numhog)
                
**********
***zona***
**********
gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4
replace zona_c=0 if domi==5
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

************
****pais****
************
gen pais_c="HND"

**********
***anio***
**********
gen anio_c=2002

*********
***mes***
*********
gen mes_c=5
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"
label value mes_c mes_c

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 
replace relacion_ci=4 if rela_j==4 | rela_j==5 | rela_j==6 
replace relacion_ci=5 if rela_j==7 | rela_j==10
replace relacion_ci=6 if rela_j==8
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
/*
civil:
           1 01. casado
           2 02. viudo
           3 03. divorciado
           4 04. separado
           5 05. soltero
           6 06. union libre
           9 09. ns/nr

		   */
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
gen desalent_ci=(emp_ci==0 & (p26==5))
replace desalent_ci=. if p26==.
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

******************
***categopri_ci***
******************
 /* Queda un hogar sin jefe: idh=2241 */
gen categopri_ci=1 if p34==7| p34==6
replace categopri_ci=2 if p34==4 | p34==5 
replace categopri_ci=3 if p34==1 | p34==2 | p34==3
replace categopri_ci=4 if p34==8 | p34==9
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

*****************
***horaspri_ci***
*****************
gen horaspri_ci=p19a if p19a<=168
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p19b if p19b<168

*****************
***horastot_ci***
*****************
gen horastot_ci=p19c  if p19c~=99 &  p19c>0
replace  horastot_ci=. if p19c==. 
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las actividades"

******************
***categosec_ci***
******************
gen categosec_ci=1 if p40==7 | p40==6
replace categosec_ci=2 if p40==4 | p40==5 
replace categosec_ci=3 if p40==1 | p40==2 | p40==3
replace categosec_ci=4 if p40==8 | p40==9 
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci
 
***************
***subemp_ci***
***************
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & p20==1
label var subemp_ci "Trabajadores subempleados"

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p25b
replace durades_ci=0.5 if p25a==1 & p25b==.
label var durades_ci "Duracion del Desempleo (en meses)"


*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=p32b if emp_ci==1
replace antiguedad_ci=0.5 if p32a==1
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"


*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p20==2
label var tiempoparc_ci "Trabajadores a medio tiempo"
*****************
***nempleos_ci***
*****************

gen nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p17a==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"


*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p33b>=1 & p33b<=5
replace firmapeq_ci=0 if p33b>=6 & p33b<9999

*****************
***spublico_ci***
*****************
gen spublico_ci= 1 if  p34==1
replace spublico_ci= 0 if  p34 > 1 &  p34 <= 9
label var spublico_ci "Personas que trabajan en el sector publico"

*************
**ocupa_ci***
*************
/*
tostring p30, replace force
gen digito1 = "0"
gen digito2 = "00"
gen digito3 = "000"
egen x1 = concat(digito1 p30) if length(p30)==3 
egen x2 = concat(digito2 p30) if length(p30)==2
egen x3 = concat(digito3 p30) if ((p30!=".") & length(p30)==1)
replace p30=x1 if length(p30)==3
replace p30=x2 if length(p30)==2
replace p30=x3 if length(p30)==1 & p30!="."
gen x =real(substr(p30,4,1))
gen ocupa= real(substr(p30,1,1))
replace ocupa =. if x ==.
drop digito* x*

ocupaop:
           0 00. profesionales, tecnicos y peoa
           1 01. directores gerentes y administ. grales.
           2 02. empleados de oficina
           3 03. comerciantes y vendedores
           4 04. agricultores, ganaderos y trab. agrop.
           5 05. conductores de transporte
           6 06. trab. ind. textil, albañileria, mecanica, etc.
           7 07. trab. area grafica, quim., alimentos, etc.
           8 08. operador de carga y almacenaje
           9 09. ocupacion de los servicios
          10 10. no sabe, no responde 
          11 11. busca trabajo por primera vez 

*/

gen ocupa_ci =.
replace ocupa_ci=1 if p30 >=1 & p30 <=1955
replace ocupa_ci=2 if p30 >=1993 & p30 <=2545
replace ocupa_ci=3 if p30 >=2569 & p30 <=3484
replace ocupa_ci=4 if p30 >=3505 & p30 <=3940
replace ocupa_ci=5 if p30 >=9073 & p30 <=9988 
replace ocupa_ci=6 if p30 >=3962 & p30 <=4541
replace ocupa_ci=7 if (p30 >=4561 & p30 <=9059) 
*recode ocupa_ci (5=8) if (p30 ==9193) 
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
gen rama_ci=rama
replace rama_ci=. if rama==10 |rama==11 | emp_ci==0
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
/*YL ->linea estimada (no encuentro las originales) correspond al mes de agosto*/
gen lp_ci =.
replace lp_ci = 1386.24 if zona_c == 1
replace lp_ci =  771.10 if zona_c == 0
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

gen cesante_ci=1 if p21==1 & condocup_ci==2
replace cesante_ci=0 if p21==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamfirma_ci=.
gen tamemp_ci = 1 if (p33b>=1 & p33b<=5)
replace tamemp_ci = 2 if (p33b>=6 & p33b<=50)
replace tamemp_ci = 3 if (p33b>50)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"


*************
*ypen_ci*
*************
gen ypen_ci=v46aum
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
gen tc_ci=17.07

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2002
gen salmm_ci= 	1810.20

label var salmm_ci "Salario minimo legal"


*******************
***categoinac_ci***
*******************
/*
p27:
           1 1. jubilado o pensionista
           2 2. rentista
           3 3. estudiante
           4 4. realiza los quehaceres del hogar
           5 5. incapacitado permanentemente
           6 6. incapacitado temporalmente
           7 7. otros
           9 9. ns/nr
*/
gen categoinac_ci =1 if (p27==1 & condocup_ci==3)
replace categoinac_ci = 2 if  (p27==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (p27==4 & condocup_ci==3)
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
*v46aum v46bum v46cum v46dum v46eum v46fum v46gum v46hum v46ium v46jum v46kum
/*

v46aum          float   %9.0g                 p46aum. pensiones o jubilaciones ? ult. meses
v46bum          float   %9.0g                 p46bum. alquieres ? ult. meses
v46cum          float   %9.0g                 p46cum. subsidios? ult. meses
v46dum          float   %9.0g                 p46dum. intereses bancarios? ult. meses
v46eum          float   %9.0g                 p46eum. remesas del exterior? ult. meses
v46fum          float   %9.0g                 p46fum. ayudas familiares? ult. meses
v46gum          float   %9.0g                 p46gum. ayudas particula.? ult. meses
v46hum          float   %9.0g                 p46hum. bonos.? ult. meses
v46ium          float   %9.0g                 p46ium. prestaciones laborales.? ult. meses
v46jum          float   %9.0g                 p46jum. herencias.? ult. meses
v46kum          float   %9.0g                 p46kum. otros.? ult. meses

*/
gen ypenju2=v46aum 
gen yalquile2=v46bum 
gen ysubsi2=v46cum 
gen ybonos2=v46hum
gen yremesa2=v46eum
gen yayuda2=v46fum
gen yayupar2=v46gum
gen interes2=v46dum
gen prestlab2=v46ium 
gen herencia2=v46jum
gen yotros2=v46ku3m

egen ynlm_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 interes2 yotros2), missing
/*replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & interes2==. & prestlab2==. & herencia2==. & yotros2==.*/ 
label var ynlm_ci "Ingreso No Laboral Monetario"

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
gen autoconsumop_ci=p39/6 if p39<99999 & p39>=0 
replace autoconsumop_ci=0 if p39==. & edad>4 & (categopri==1 | categopri==2) & emp_ci==1 
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"


gen autoconsumos_ci=p45/6 if p45<99999 & p45>=0 
replace autoconsumos_ci=0 if p45==. & edad>4 & (categosec==1 | categosec==2) & emp_c==1
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
gen remesas_ci=yremesa2
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
egen trapri_ci= rowtotal(remesas_ci yayuda2 yayupar2), missing
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
egen trapub_ci= rowtotal(progpub_ci ypensub_ci ybonos2 ysubsi2), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(interes2 yalquile2), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(yotros2), missing
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
LA DIFERENCIA ES MENOR AL 5% */

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
replace p03b=. if p03b==99
gen aedu_ci=.
replace aedu_ci=0 if p03a>=1 & p03a<=3
replace aedu_ci=p03b if p03a==5
replace aedu_ci=p03b+6 if p03a==6 | p03a==7
replace aedu_ci=p03b+12 if p03a==8 | p03a==9
replace aedu_ci=p03b+17 if p03a==10
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
replace edupre_ci=1 if (p03a==2 & aedu_ci ~=.)
replace edupre_ci=0 if (p03a~=2 & aedu_ci ~=.)
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
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p03a==8 
replace tecnica_ci=0 if p03a!=8
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
g       pqnoasis1_ci =.

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=0
replace aguared_ch = 1 if v04b ==1 | v04b==2

gen aguadist_ch=.

replace aguadist_ch=1 if v04c==1

replace aguadist_ch=2 if v04c==2  | v04c==3

replace aguadist_ch=3 if v04c==4

gen aguamala_ch=.

replace aguamala_ch=1 if v04b>=4 & v04b<=5

replace aguamala_ch=0 if v04b>=1 & v04b<=3

gen aguamide_ch=.

gen luz_ch=.

replace luz_ch=1 if v06==1 | v06==2 | v06==3

replace luz_ch=0 if v06>=4 & v06<=7

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.
replace bano_ch=1 if v05a>=1 & v05a<=5
replace bano_ch=0 if v05a==6


gen banoex_ch=.
replace banoex=1 if v05b==1
replace banoex=0 if v05b==2

gen des1_ch=.
replace des1_ch=0 if v05a==6
replace des1_ch=1 if v05a==1| v05a==2
replace des1_ch=2 if v05a>=3 & v05a<=5
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch
/*
des1_ch Tipo de desagüe incluyendo la definición de "Unimproved" del MDG
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general o a una cámara séptica
2 El desagüe está conectado a un pozo ciego o es una letrina.
3 El desagüe se comunica con la superficie: desemboca en un río o en la calle.*/
* No hay casos para la categoría 3.


gen des2_ch=.
replace  des2_ch=0 if  des1_ch==0
replace  des2_ch=1 if des1_ch==1 | des1_ch==2
replace  des2_ch=2 if v05a==9

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

replace piso_ch=0 if v03==6

replace piso_ch=1 if v03>=1 & v03<=5 

replace piso_ch=2 if v03==7

gen pared_ch=.

replace pared_ch=0 if v02==4 | v02==5

replace pared_ch=1 if v02>=1 & v02<=3

replace pared_ch=2 if v02==6

gen techo_ch=.


gen resid_ch=.
replace resid_ch=0 if v07==1
replace resid_ch=1 if v07==3 | v07==4
replace resid_ch=2 if v07==2 | v07==5
replace resid_ch=3 if v07==6 

gen dorm_ch=.
replace dorm_ch= v11b if  v11b>=0 

gen cuartos_ch=.
replace cuartos_ch=v11a if v11a>=0 

gen cocina_ch=.

gen telef_ch=.
replace telef_ch = 1 if v10d==1
replace telef_ch = 0 if v10d==2

gen refrig_ch=.
replace refrig_ch = 1 if v10a==1
replace refrig_ch = 0 if v10a==2

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.
* La variable es la v01, pero no se puede distinguir entre casa o departamento.


gen vivi2_ch=.

replace vivi2_ch=1 if v01==1

replace vivi2_ch=0 if v01>=2 & v01<=7

/*
v09a
1 01. propietario y esta pagada totalmente
           2 02.propietario y la esta pagando
           3 03. alquilada
           4 04. cedida sin pago
           5 05. recuperada legalizada
           6 06. recuperada sin legalizar
           9 09. ns/nr


*/		   
gen viviprop_ch=.
replace viviprop_ch=0 if v09a==3
replace viviprop_ch=1 if v09a==1
replace viviprop_ch=2 if v09a==2
replace viviprop_ch=3 if v09a==4 | v09a==5 | v09a==6

gen vivitit_ch=.
gen vivialq_ch=.
replace vivialq_ch=v09b if viviprop_ch==0

gen vivialqimp_ch=.


**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g  aguamejorada_ch = 1 if (v04b >=1 & v04b <=4)
replace aguamejorada_ch = 0 if (v04b >=5 & v04b <=7)

*********************
***banomejorado_ch***
*********************
g  banomejorado_ch = 1 if ( v05a >=1 & v05a <=5) & v05b ==1
replace banomejorado_ch = 0 if ((v05a >=1 & v05a <=5) & v05b ==2) | (v05a==6)

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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
