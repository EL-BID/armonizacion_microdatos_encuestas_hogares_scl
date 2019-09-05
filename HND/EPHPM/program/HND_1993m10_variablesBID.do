
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
local ANO "1993"
local ronda m10 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m10
Autores:Mayra Sáenz
Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización 4 de Octubre de 2013: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


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
label define region_c 1 "Atlántida" 2 "Colón" 3 "Comayagua" ///
4 "Copán" 5 "Cortés" 6 "Choluteca" 7 "El Paraíso" 8 "Francisco de Morazán" ///
10 "Intibuca" 12 "La Paz" 13 "Lempira" 14 "Ocotepeque" 15 "Olancho" 16 "Santa Bárbara" ///
17 "Valle" 18 "Yoro"
label var region_c "División política"


***************
***factor_ch***
***************

gen factor_ch=factor


***************
****idh_ch*****
*************** 
egen idh_ch=group(reg depto munic domi vivi hogar)

*************
****idp_ci****
**************
gen idp_ci=nperhog
label var idp_ci "Identificador Individual dentro del Hogar"

                
**********
***zona***
**********

* Para crear la variable zona se debe utilizar el primer dígito de la variable domi. Sin embargo, los valores difieren
* significativamente de aquellos de años anteriores. Se genera como valor perdido hasta revisar.
* Fuente: http://63.161.65.190/nada/index.php/catalog/10/vargrp/VG2
/*
tostring domi, replace
gen dominio = substr(domi, 1,1)
destring dominio, replace


gen zona_c=1 if dominio==1 | dominio==2 | dominio==3 
replace  zona_c=0 if dominio==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c
*/


gen zona_c=.

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
gen anio_c=1993

*********
***mes***
*********
gen mes_c=10

label define mes_c 10 "Octubre" 
label value mes_c mes_c


*****************
***relacion_ci***
*****************

gen parentco= rela_j

gen relacion_ci=.
replace  relacion_ci=1 if parentco==1
replace  relacion_ci=2 if parentco==2
replace  relacion_ci=3 if parentco==3 
replace  relacion_ci=4 if parentco==4 | parentco==5 
replace  relacion_ci=5 if parentco==6
replace  relacion_ci=6 if parentco==7
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "Conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

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
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=edad if edad<99
label var edad_ci "Edad del Individuo"
drop edad

*****************
***civil_ci***
*****************
gen civil_ci=.
replace  civil_ci=1 if civil==5
replace  civil_ci=2 if civil==1 | civil==6
replace  civil_ci=3 if civil==3 | civil==4
replace  civil_ci=4 if civil==2
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
recode activida (11 12=1) (21 22=2) (33/38=3), gen(condocup_ci)
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Comprobacion con variables originales.  Se considera ocupado a quienes estan en trabajos no remunerados.

g condocup_ci=.
replace condocup_ci=1 if (p6==1 | p7==1 | p8==1)
replace condocup_ci=2 if (p6==6 | p7==6 | p8==6) & (p16==1 | p17==1) 
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
replace  desalent=0 if emp_ci==1
replace  desalent=1 if p20==5
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 


******************
***categopri_ci***
******************
gen categopri_ci=1 if p28==7 | p28==6
replace  categopri_ci=2 if p28==4 | p28==5 
replace  categopri_ci=3 if p28==1 | p28==2 | p28==3
replace  categopri_ci=4 if p28==8
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci



*****************
***horaspri_ci***
*****************
gen horaspri_ci=p11_1 if p11_1<99 & p11_1>0
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen horassec_ci=p11_2 if p11_2<99 & p11_2>0
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Secundaria"


******************
***categosec_ci***
******************

gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia", modify 
label define categosec_ci 3"Empleado" 4" No remunerado" , modify
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***horastot_ci***
*****************

gen horastot_ci=p12  if p12~=99 &  p12>0
replace  horastot=. if p12==. 
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

***************
***subemp_ci***
***************
/*
gen subemp_ci=0 
replace subemp_ci=1 if emp_ci==1 & horastot_ci<=30 & p13==1
replace subemp_ci =. if emp_ci ==0
label var subemp_ci "Personas en subempleo por horas"
*/

* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p13==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

************
*durades_ci*
************
gen durades_ci=.
replace  durades_ci=p19_2 if p19_2<99 & p19_2>0
replace  durades_ci=0.5 if p19_1==1 & (p19_2==0 | p19_2==.)
label var durades_ci "Duracion del Desempleo (en meses)"

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=p26_2 if emp_ci==1 & p26_2<99 & p26_2>0
replace  antiguedad_ci=0.5 if p26_1==1 & antiguedad_ci==. & emp==1 
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*******************
***tiempoparc_ci***
*******************

g tiempoparc_ci=(emp_ci==1 & p13==6 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci!=1 | p13==. |horastot_ci==.
label var tiempoparc_ci "Personas que trabajan medio tiempo" 

*****************
***nempleos_ci***
*****************
gen nempleos_ci=1 if emp_ci==1
replace  nempleos_ci=2 if emp_ci==1 & p10==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"

*****************
***tamfirma_ci***
*****************

gen tamfirma_ci=.
replace  tamfirma=1 if p27_1==2
replace  tamfirma=0 if p27_1==1
label var tamfirma "Trabajadores formales: 1 = + de 10 empleados"

/*
******************************
*	firmapeq_ci
******************************
g firmapeq_ci=(p27_1==1 & (p27_2>=1 & p27_2<=5)) if emp_c==1
replace firmapeq_ci=. if emp_ci!=1 | p27_2==.
la var firmapeq_ci "Trabajadores formales. 1=5 o menos trabajadores"*/

*****************
***spublico_ci***
*****************
gen spublico_ci= 1 if p28 == 1
replace spublico_ci= 0 if p28  !=1
label var spublico "Personas que trabajan en el sector publico"

*************
**ocupa_ci***
*************

gen ocupa_ci=.
replace ocupa_ci=1 if ocupagen >=0 & ocupagen <=950
replace ocupa_ci=2 if ocupagen >=1000 & ocupagen <=1290
replace ocupa_ci=3 if ocupagen >=2000 & ocupagen <=2790
replace ocupa_ci=4 if ocupagen >=3000 & ocupagen <=3390
replace ocupa_ci=5 if ocupagen >=9000 & ocupagen <=9810 
replace ocupa_ci=6 if ocupagen >=4000 & ocupagen <=4520
replace ocupa_ci=7 if (ocupagen >=5000 & ocupagen <=8150) 
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
* Nota MGD 9/22/2014: se genero la variable como missing ya que presenta un salto en las clasificaciones a pesar de estar bien generada.
gen rama_ci=.
/*replace  rama_ci=1 if (p25_2>=64 & p25_2<=448) & emp_ci==1
replace  rama_ci=2 if (p25_2>=505 & p25_2<=824) & emp_ci==1
replace  rama_ci=3 if (p25_2>=883 & p25_2<=6015) & emp_ci==1
replace  rama_ci=4 if (p25_2>=6049 & p25_2<=6178) & emp_ci==1
replace  rama_ci=5 if (p25_2>=6238 & p25_2<=6288) & emp_ci==1
replace  rama_ci=6 if (p25_2>=6301 & p25_2<=6493) & emp_ci==1
replace  rama_ci=7 if (p25_2>=6553 & p25_2<=7315) & emp_ci==1
replace  rama_ci=8 if (p25_2>=7372 & p25_2<=8067) & emp_ci==1
replace  rama_ci=9 if (p25_2>=8128 & p25_2<=9833) & emp_ci==1*/
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

gen salmm_ci= 	salmin
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
*cesante_ci* 
*************
generat cesante_ci=0 if p23==6 & condocup_ci==2
replace cesante_ci=1 if p23==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

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

****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*************
**ypen_ci*
*************
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"


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
/*YL ->linea estimada (no encuentro las originales) correspond al mes de septiembre*/
generat lp_ci=340.80 if zona_c==1
replace lp_ci=189.57 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"


*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"


*************
*tamemp_ci
*************
gen tamemp_ci =.
replace tamemp_ci=1 if (p27_1==1 & (p27_2>=1 & p27_2<=5))
replace tamemp_ci=2 if (p27_1==1 & (p27_2>=6 & p27_2<=9))
replace tamemp_ci=3 if (p27_1==2)
label var tamemp_ci "# empleados en la empresa"
label define tamemp_ci  1 "Pequeñas" 2 "Medianas" 3 "Grandes"
label value tamemp_ci tamemp_ci

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if (activida==33 & condocup_ci==3)
replace categoinac_ci = 2 if  (activida==35 & condocup_ci==3)
replace categoinac_ci = 3 if  (activida==36 & condocup_ci==3)
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


recode p29_2 p31_2 p29_6 p31_4 (99999 =.)

/*
gen ylmpri_ci=.
replace  ylmpri_ci=p29_2 if p29_2<99999 & p29_2>0 & emp_ci ==1
replace  ylmpri_ci=p31_2 if p31_2<99999 & p31_2>0 & emp_ci ==1
replace  ylmpri_ci=0 if p29_2==0 & p31_2==0 & edad_ci>9 & emp_ci ==1
replace  ylmpri_ci=0 if p28==8 & edad_ci>9 & emp_ci ==1
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"
*/

* Modificaciones Marcela Rubio Septiembre 2014

egen ylmpri_ci= rsum(p29_2  p31_2) if edad>9 & p11_1 >0, missing


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

/*
gen ylmsec_ci=.
replace  ylmsec_ci=p29_6 if p29_6<99999 & edad_ci>9 & p29_6>0
replace  ylmsec_ci=p31_4 if p31_4<99999 & edad_ci>9 & p31_4>0
replace  ylmsec_ci=0 if p29_6==0 & p31_4==0 & edad_ci>9 & p10==1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 
*/

* Modificaciones Marcela Rubio Septiembre 2014

egen ylmsec_ci = rsum(p29_6  p31_4) if edad>9 & p11_2>0, missing

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
replace  asiste_ci=1 if p4==1
replace  asiste_ci=0 if p4==6
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

 * Años de educacion aprobados **
gen grado = p5_2
gen nivel =p5_1

gen aedu_ci=.

replace  aedu_ci=0 if (nivel>=1 & nivel<=3) | (nivel==4 & grado==0)
replace  aedu_ci=grado if nivel==4 & grado>=1
replace  aedu_ci=grado+6 if (nivel==5 | nivel==6) & grado>=0
replace  aedu_ci=grado+12 if (nivel==7 | nivel==8) & grado>=0
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
replace edupre_ci=1 if (nivel==2 & aedu_ci ~=.)
replace edupre_ci=0 if (nivel~=2 & aedu_ci ~=.)
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
replace tecnica_ci=nivel==6|nivel==7
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
replace aguared_ch = 1 if pv4a ==1 | pv4a==2

gen aguadist_ch=.
replace  aguadist_ch=1 if pv4b==6
replace  aguadist_ch=2 if pv4b==7 
replace  aguadist_ch=3 if pv4b==8


gen aguamala_ch=.
replace  aguamala_ch=1 if pv4a>=4 & pv4a<=5
replace  aguamala_ch=0 if pv4a>=1 & pv4a<=3

gen aguamide_ch=.

gen luz_ch=.
replace  luz_ch=1 if pv6==1 | pv6==2 | pv6==3
replace  luz_ch=0 if pv6==4 


gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.
replace  bano_ch=1 if pv5a==1 | pv5a==2
replace  bano_ch=0 if pv5a==3

gen banoex_ch=.
replace  banoex=1 if pv5c==7
replace  banoex=0 if pv5c==8

gen des1_ch=.
replace des1_ch=0 if pv5a==3
replace des1_ch=1 if pv5b==4 | pv5b==5
replace des1_ch=2 if pv5a==2 | pv5b==6

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
replace  piso_ch=0 if  pv3==5
replace  piso_ch=1 if  pv3>=1 &  pv3<=4 
* No hay casos para la categoría 2.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales", modify
label val piso_ch piso_ch


gen pared_ch=.
replace  pared_ch=0 if  pv2==4 |  pv2==5
replace  pared_ch=1 if  pv2>=1 &  pv2<=3
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales", modify
label val pared_ch pared_ch


gen techo_ch=.

gen resid_ch=.
 
gen dorm_ch=.

replace  dorm_ch=pv8b if pv8b>=0 & pv8b<99

gen cuartos_ch=.

replace  cuartos_ch=pv8a if pv8a>=0 & pv8a<99

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.
 
gen auto_ch=.

gen compu_ch=.
 
gen internet_ch=.

gen cel_ch=.

*Mayra Sáenz 2013. Existe la variable pero no se puede distinguir entre casa y departamento.
gen vivi1_ch=.

gen vivi2_ch=.
replace  vivi2_ch=1 if pv1==1
replace  vivi2_ch=0 if pv1>=2 & pv1<=6

gen viviprop_ch=.
replace  viviprop_ch=0 if  pv7a==3
replace  viviprop_ch=1 if  pv7a==1
replace  viviprop_ch=2 if  pv7a==2
replace  viviprop_ch=3 if  pv7a==4 |  pv7a==5 |  pv7a==6

gen vivitit_ch=.

gen vivialq_ch=.

replace  vivialq_ch=pv7a if pv7a>0 & pv7a<9999 & viviprop_ch==0
gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g  aguamejorada_ch = 1 if (pv4a >=1 & pv4a <=3)
replace aguamejorada_ch = 0 if (pv4a >=4 & pv4a <=5)

*********************
***banomejorado_ch***
*********************
g  banomejorado_ch = 1 if (pv5a >=1 & pv5a <=2) & (pv5b >=4 & pv5b <=6) & pv5c ==7
replace banomejorado_ch = 0 if ((pv5a >=1 & pv5a <=2) & (pv5b >=4 & pv5b <=6) & pv5c ==8) | ((pv5a>=1 & pv5a <=2) & (pv5b !=4 | pv5b !=5 | pv5b !=6)) | pv5a ==3
		



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

