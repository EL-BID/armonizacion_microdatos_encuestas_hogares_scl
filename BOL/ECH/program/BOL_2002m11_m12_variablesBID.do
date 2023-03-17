
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

local PAIS BOL
local ENCUESTA ECH
local ANO "2002"
local ronda m11_m12 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11
Autores: 
Última versión: Nathalia Maya  E-mail: sandramay@iadb.org 
Fecha última modificación: 25 de agosto de 2022

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

	****************
	* region_BID_c *
	****************
	
gen region_BID_c=3

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

	************
	* region_c *
	************
*destring depto, gen(region_c)
gen region_c= depto
label define region_c ///
1"Chuquisaca"         ///     
2"La Paz"             ///
3"Cochabamba"         ///
4"Oruro"              ///
5"Potosí"             ///
6"Tarija"             ///
7"Santa Cruz"         ///
8"Beni"               ///
9"Pando"
label values region_c region_c              
clonevar ine01 = region_c


***************
***factor_ch***
***************
gen factor_ch= factor
label variable factor_ch "Factor de expansion del hogar"


	***************
	***upm_ci***
	***************
gen upm_ci=upm
	***************
	***estrato_ci**
	***************
gen estrato_ci=. //No es claro si la variable "estrato" corresponde a un estrato muestral

***************
****idh_ch*****
**************
sort folio gconsum
egen idh_ch=group(folio gconsum)
format idh_ch %20.0f
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci=nro1
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=0 if urbrur==2
replace zona_c=1 if urbrur==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2002
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=11
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if s106==1
replace relacion_ci=2 if s106==2
replace relacion_ci=3 if s106==3
replace relacion_ci=4 if s106>=4 & s106<=8
replace relacion_ci=5 if s106==10 | s106==11
replace relacion_ci=6 if s106==9

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=s102

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=s103
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if s110==1
replace civil_ci=2 if s110==2 | s110==3
replace civil_ci=3 if s110==4 | s110==5
replace civil_ci=4 if s110==6

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
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
			
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				

	***************
	***afroind_ci***
	***************
**Pregunta: ¿Se considera perteneciente a alguno de los siguientes pueblos originarios / indígenas...

gen afroind_ci=. 
replace afroind_ci=1 if s111a!=7
replace afroind_ci=3 if s111a==7
replace afroind_ci=9 if edad_ci<12 


	***************
	***afroind_ch***
	***************
gen afroind_jefe=.
replace afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2001 //De 1999 a 2000 se indluía la categoría "Raza negra", a partir de 2001 no se incluye esta categoría

	*************
	***dis_ci***
	**************
gen dis_ci = .
gen dis_ch =.


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= 335.602829200798 if ciudad==1 & urb_rur==1
replace lp_ci= 327.041438265403 if ciudad==2 & urb_rur==1
replace lp_ci= 351.294848612139 if ciudad==3 & urb_rur==1
replace lp_ci= 297.391359793426 if ciudad==4 & urb_rur==1
replace lp_ci= 273.517553636224 if ciudad==5 & urb_rur==1
replace lp_ci= 351.294848612139 if ciudad==6 & urb_rur==1
replace lp_ci= 343.870440308143 if ciudad==7 & urb_rur==1
replace lp_ci= 343.870440308143 if ciudad==8 & urb_rur==1
replace lp_ci= 272.151357187525 if ciudad==10 & urb_rur==1
replace lp_ci= 343.870440308143 if ciudad==9 & urb_rur==1


replace lp_ci=   233.39    if  urb_rur==2

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 169.479428746403 if ciudad==1 & urb_rur==1
replace lpe_ci= 181.835039675564 if ciudad==2 & urb_rur==1
replace lpe_ci= 177.40389854913 if ciudad==3 & urb_rur==1
replace lpe_ci= 165.349596045145 if ciudad==4 & urb_rur==1
replace lpe_ci= 152.07575982174 if ciudad==5 & urb_rur==1
replace lpe_ci= 177.40389854913 if ciudad==6 & urb_rur==1
replace lpe_ci= 174.686183676537 if ciudad==7 & urb_rur==1
replace lpe_ci= 174.686183676537 if ciudad==8 & urb_rur==1
replace lpe_ci= 165.195873812828 if ciudad==10 & urb_rur==1
replace lpe_ci= 174.686183676537 if ciudad==9 & urb_rur==1


replace lpe_ci=     133.03    if  urb_rur==2



label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 2002
gen salmm_ci= 	430.00


label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.

*recode cotizando_ci .=0 if condact>=1 & condact<=3
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci= 1 if  s550b ==1	
*replace afiliado_ci =1 if s5_71b==1
recode afiliado_ci .=0 if condact>=2 & condact<=4
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.


replace tipopen_ci=1 if s602c>0 &  s602c~=.
replace tipopen_ci=2 if s602f>0 & s602f~=.
replace tipopen_ci=3 if s602d>0 & s602d~=.
replace tipopen_ci=4 if s602e>0 & s602e~=. 
replace tipopen_ci=12 if (s602c>0 & s602f>0) & (s602c~=. & s602f~=.)
replace tipopen_ci=13 if (s602c>0 & s602d>0) & (s602c~=. & s602d~=.)
replace tipopen_ci=23 if (s602f>0 & s602d>0) & (s602f~=. & s602d~=.)
replace tipopen_ci=123 if (s602c>0 & s602f>0 & s602d>0) & (s602c~=. & s602f~=. & s602d~=.)
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
gen instcot_ci=. 


****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if condact==2
replace condocup_ci=2 if condact==3 | condact==4
replace condocup_ci=3 if (condact==5 | condact==6) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Homologacion toda la serie 05/22/2014 MGD

gen condocup_ci=.

*Mod. MLO 2015,10: se consideran otras causas excepcionales 
*replace condocup_ci=1 if s501==1 | s502<=6  | s503a==1
replace condocup_ci=1 if s501==1 | s502<=6 | (s503a>=1 & s503a<=7)
*replace condocup_ci=2 if (s501==2 | s502==7 | s503a>1) & (s505==1 | s506==1) & (s504==1)
replace condocup_ci=2 if (s501==2 | s502==7 | s503a>7) & (s505==1 | s506==1) & (s504==1)

*2015,10 MLO la encuesta pregunta a partir de 7 años (no 10)
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if  s510==1 & condocup_ci==2
* 2014, 03 Modificacion siguiente linea MLO
replace cesante_ci=0 if s510==2 & condocup_ci==2
*replace cesante_ci=0 if  s510==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if s523==1 | s523==2
replace tamemp_ci=2 if s523>=3 & s523<=6
replace tamemp_ci=3 if s523>=7 & s523<=8
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

egen aux_p=rsum(s602c s602d s602e s602f s602g), missing
gen pension_ci=1 if aux_p>0 & aux_p!=. 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
ta s604d
gen aux_ps= s604d/12  if s604d >0 & s604d !=. 
gen byte pensionsub_ci=1 if aux_ps>0 & aux_ps!=.
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring aux_ps, replace
gen  ypensub_ci=aux_ps

label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
	

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & s505==2 & (s509a==3 | s509a==4))
replace desalent_ci=. if emp_ci==.

*****************
***horaspri_ci***
*****************

gen horas1=s524min/60

egen horaspri_ci=rsum(s524hrs horas1), missing
replace horaspri_ci=. if s524hrs==. & horas1==.
replace horaspri_ci=horaspri_ci*s524a
replace horaspri_ci=. if emp_ci~=1

drop horas1


*****************
***horastot_ci***
*****************

gen horas2=s538min/60

egen horassec=rsum(s538hrs horas2), missing
replace horassec=. if s538hrs==. & horas2==.
replace horassec=horassec*s538a
replace horassec=. if emp_ci~=1

egen horastot_ci=rsum(horaspri_ci horassec), missing
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horas2
drop horassec

***************
***subemp_ci***
***************
/*NA*/

* Si hay como generar la variable.  MGD 06/18/2014
* Segun definicion del documento metodologico: horas de la actividad principal y si esta disponible a trabajar mas horas. MGD 06/18/2014
* Se podria considerar a las dos alternativas: desea trabajar y esta dispuesto a trabajar.
gen subemp_ci=0
*replace subemp_ci=1 if s545==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (s545==1 & s544==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************

*Incorporacion MLO, 2015,10 
gen tiempoparc_ci=(horaspri_ci<30 & s544==2)
replace tiempoparc_ci=. if emp_ci==0

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if s517>=4 & s517<=6
replace categopri_ci=2 if s517==3
replace categopri_ci=3 if s517==1 | s517==2 | s517==8
replace categopri_ci=4 if s517==7
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if s535>=4 & s535<=6
replace categosec_ci=2 if s535==3
replace categosec_ci=3 if s535==1 | s535==2 | s535==8
replace categosec_ci=4 if s535==7

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"



*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if s518==3 & categopri_ci==3
replace tipocontrato_ci=2 if s518==1 & categopri_ci==3
replace tipocontrato_ci=3 if ((s518==2 | s518==4) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s532==1

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if s523>=1 & s523<=2 /*1 a 4 personas*/
replace firmapeq_ci=0 if s523>=3 & s523<=8 /*5 o más personas*/
*/

*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=.
replace spublico_ci=1 if s519==2
replace spublico_ci=0 if s519==1
replace spublico_ci=. if emp_ci~=1


**************
***ocupa_ci***
**************
* Nota MGD 07/25/2014: no hay como separar entre servicios y comercio; tampoco es posible desagregar la categoria 9.  
* Es por esto que no hay categoria 4 de comercio y la categoria 9 sube respecto a la serie posterior. No es comparable se deja como missing.
gen ocupa_ci=.
/*replace ocupa_ci=1 if (cob_p==2 | cob_p==3) & emp_ci==1
replace ocupa_ci=2 if cob_p==1 & emp_ci==1
replace ocupa_ci=3 if cob_p==4 & emp_ci==1
replace ocupa_ci=5 if cob_p==5 & emp_ci==1
replace ocupa_ci=6 if cob_p==6 & emp_ci==1
replace ocupa_ci=7 if (cob_p==7 | cob_p==8)  & emp_ci==1
replace ocupa_ci=8 if cob_p==0 & emp_ci==1
replace ocupa_ci=9 if cob_p==9 & emp_ci==1*/


*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if caeb_p>=1 & caeb_p<=2 & emp_ci==1
replace rama_ci=2 if caeb_p==3 & emp_ci==1
replace rama_ci=3 if caeb_p==4 & emp_ci==1
replace rama_ci=4 if caeb_p==5 & emp_ci==1
replace rama_ci=5 if caeb_p==6 & emp_ci==1
replace rama_ci=6 if caeb_p>=7 & caeb_p<=8 & emp_ci==1 
replace rama_ci=7 if caeb_p==9 & emp_ci==1
replace rama_ci=8 if caeb_p>=10 & caeb_p<=11 & emp_ci==1
replace rama_ci=9 if caeb_p>=12 & caeb_p<=17 & emp_ci==1


****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=.
replace durades_ci=s548a/30   if s548b==1
replace durades_ci=s548a/4.3  if s548b==2
replace durades_ci=s548a/2    if s548b==3
replace durades_ci=s548a      if s548b==4
replace durades_ci=s548a*6    if s548b==5
replace durades_ci=s548a*12   if s548b==6



*******************
***antiguedad_ci***
*******************
/*En años*/

gen antiguedad_ci=.
replace antiguedad_ci=s515a/(4.3*12) if s515b==2 & emp_ci==1
replace antiguedad_ci=s515a/12 if s515b==4 & emp_ci==1
replace antiguedad_ci=s515a if s515b==6 & emp_ci==1

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (s508a==3 & condocup_ci==3)
replace categoinac_ci = 2 if  (s508a==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s508a==2 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 


*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************

*Para los trabajadores dependientes

*Ingreso basico
gen ypridb=.
replace ypridb=s526a*30 if s526b==1 
replace ypridb=s526a*4.3 if s526b==2 
replace ypridb=s526a*2 if s526b==3 
replace ypridb=s526a if s526b==4 
replace ypridb=s526a/6 if s526b==5
replace ypridb=s526a/12 if s526b==6

replace ypridb=0 if categopri_ci==4 

*Ingresos extras
local sub="a b c"
foreach i of local sub {
gen ypriex`i'=.
replace ypriex`i'=s527`i'2/12 if s527`i'1==1
replace ypriex`i'=0 if s527`i'1==2
}

egen ypridbd=rsum(ypridb ypriexa ypriexb ypriexc), missing
replace ypridbd=. if ypridb==. & ypriexa==. & ypriexb==. & ypriexc==.

*Para los trabajadores independientes 

gen yprijbi=.
replace yprijbi=s531a*30 if s531b==1 
replace yprijbi=s531a*4.3 if s531b==2 
replace yprijbi=s531a*2 if s531b==3 
replace yprijbi=s531a if s531b==4 
replace yprijbi=s531a/6 if s531b==5
replace yprijbi=s531a/12 if s531b==6


*Ingreso laboral monetario para todos

egen ylmpri_ci=rsum(yprijbi ypridbd), missing
replace ylmpri_ci=. if ypridbd==. & yprijbi==. 
replace ylmpri_ci=. if emp_ci~=1


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

*Ingreso laboral no monetario de los dependientes

local nnn="a b c d e"
foreach i of local nnn {

gen especie`i'=.
replace especie`i'=s528`i'3*30  if s528`i'2==1
replace especie`i'=s528`i'3*4.3 if s528`i'2==2
replace especie`i'=s528`i'3*2   if s528`i'2==3
replace especie`i'=s528`i'3     if s528`i'2==4
replace especie`i'=s528`i'3/3   if s528`i'2==5
replace especie`i'=s528`i'3/6   if s528`i'2==6
replace especie`i'=s528`i'3/12   if s528`i'2==7
replace especie`i'=0 if s528`i'1==2
}

egen ylnmprid=rsum(especiea especieb especiec especied especiee), missing
replace ylnmprid=. if especiea==. &  especieb==. & especiec==. & especied==. & especiee==. 
replace ylnmprid=0 if categopri_ci==4


*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1


***************
***ylmsec_ci***
***************

*Para los trabajadores dependientes

*Ingreso basico
gen ysecb=.
replace ysecb=s540a*30 if s540b==1 
replace ysecb=s540a*4.3 if s540b==2 
replace ysecb=s540a*2 if s540b==3 
replace ysecb=s540a if s540b==4 
replace ysecb=s540a/6 if s540b==5
replace ysecb=s540a/12 if s540b==6

replace ysecb=0 if categosec_ci==4 

*Ingresos extras

gen yxsa=.
replace yxsa=s541a2/12 if s541a1==1
replace yxsa=0 if s541a1==2


egen ysecbd=rsum(ysecb yxsa), missing
replace ysecbd=. if ysecb==. & yxsa==. 


*Para los trabajadores independientes

gen ysecjbi=.
replace ysecjbi=s543a*30 if s543b==1 
replace ysecjbi=s543a*4.3 if s543b==2 
replace ysecjbi=s543a*2 if s543b==3 
replace ysecjbi=s543a if s543b==4 
replace ysecjbi=s543a/6 if s543b==5
replace ysecjbi=s543a/12 if s543b==6

*Ingreso laboral monetario para todos

egen ylmsec_ci=rsum(ysecjbi ysecbd), missing
replace ylmsec_ci=. if ysecjbi==. & ysecbd==.
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

*Ingreso laboral no monetario de los dependientes

local nun="b c"
foreach i of local nun {

gen especiesec`i'=.
replace especiesec`i'=s541`i'2/12  if s541`i'1==1
replace especiesec`i'=0 if s541`i'1==2
}


egen ylnmsecd=rsum(especiesecb especiesecc), missing
replace ylnmsecd=. if especiesecb==. &  especiesecc==. 
replace ylnmsecd=0 if categosec_ci==4
replace ylnmsecd=. if emp_ci~=1

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmseci=.

*Ingreso laboral no monetario para todos

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci), missing
replace ylnmsec_ci=. if ylnmsecd==. & ylnmseci==.
replace ylnmsec_ci=. if emp_ci==0


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

*Alquileres, dividendos, etc.
gen transl1=s601a/12
gen transl2=s601b/12
gen transl3=s601c/12

egen transltot=rsum(transl1 transl2 transl3), missing
replace transltot=. if transl1==. & transl2==. & transl3==. 


*Rentas mensuales
egen rentastot=rsum(s602a s602b s602c s602d s602e s602f s602g s602h1), missing
replace rentastot=. if s602a==. & s602b==. & s602c==. & s602d==. & s602e==. & s602f==. & s602h1==. 


* Otros ingresos no laborales

foreach i of local sub {

gen otrosnl`i'=.
replace otrosnl`i'=s603`i'1*30  if s603`i'2==1
replace otrosnl`i'=s603`i'1*4.3 if s603`i'2==2
replace otrosnl`i'=s603`i'1*2   if s603`i'2==3
replace otrosnl`i'=s603`i'1     if s603`i'2==4
replace otrosnl`i'=s603`i'1/6   if s603`i'2==5
replace otrosnl`i'=s603`i'1/12  if s603`i'2==6

}

egen yotrosnl=rsum(otrosnla otrosnlb otrosnlc), missing
replace yotrosnl=. if otrosnla==. &  otrosnlb==. & otrosnlc==. 


egen yotrosnl2=rsum(s604a s604b s604c s604d s604e), missing
replace yotrosnl2=. if s604a==. & s604b==. & s604c==. & s604d==. & s604e==. 
replace yotrosnl2=yotrosnl2/12

*Ingresos no laborales totales monetarios

egen ynlm_ci=rsum(transltot rentastot yotrosnl yotrosnl2), missing
replace ynlm_ci=. if transltot==. & rentastot==. & yotrosnl==. & yotrosnl2==.


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.

**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

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



*****************
***remesas_ci***
*****************

gen remesas_ci=otrosnlc

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


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=.

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=s804a
replace rentaimp_ch=rentaimp_ch*7.45 if s804b==2


*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)



****************************
***VARIABLES DE EDUCACION***
****************************

/*En esta sección es sólo para los mayores a los 5 años de edad*/

gen byte aedu_ci=.

replace aedu_ci=0 if s402a==11 | s402a==12 | s402a==13

replace aedu_ci=1 if (s402a==14 | s402a==17) & s402b==1
replace aedu_ci=2 if (s402a==14 | s402a==17) & s402b==2
replace aedu_ci=3 if (s402a==14 | s402a==17) & s402b==3
replace aedu_ci=4 if (s402a==14 | s402a==17) & s402b==4
replace aedu_ci=5 if (s402a==14 | s402a==17) & s402b==5

replace aedu_ci=6 if (s402a==17 & s402b==6) | (s402a==15 & s402b==1)
replace aedu_ci=7 if (s402a==17 & s402b==7) | (s402a==15 & s402b==2)
replace aedu_ci=8 if (s402a==17 & s402b==8) | (s402a==15 & s402b==3)

replace aedu_ci=9 if (s402a==16 | s402a==18) & s402b==1
replace aedu_ci=10 if (s402a==16 | s402a==18) & s402b==2
replace aedu_ci=11 if (s402a==16 | s402a==18) & s402b==3
replace aedu_ci=12 if (s402a==16 | s402a==18) & s402b==4

replace aedu_ci=13 if (s402a>=21 & s402a<=27 & s402a~=24) & s402b==1
replace aedu_ci=14 if (s402a>=21 & s402a<=27 & s402a~=24) & s402b==2
replace aedu_ci=15 if (s402a>=21 & s402a<=27 & s402a~=24) & s402b==3
replace aedu_ci=16 if (s402a>=21 & s402a<=27 & s402a~=24) & s402b==4

replace aedu_ci=16 if (s402a==21 | s402a==25 | s402a==26) & s402b==5
replace aedu_ci=17 if (s402a==22 | s402a==23 | s402a==27) & s402b==5

replace aedu_ci=16 if (s402a==21 | s402a==25 | s402a==26) & s402b==8
replace aedu_ci=17 if (s402a==22 | s402a==23 | s402a==27) & s402b==8

replace aedu_ci=18 if s402a==24 & s402b==1
replace aedu_ci=19 if s402a==24 & s402b==2



**************
***eduno_ci***
**************

gen byte eduno_ci=(s402a==11 | s402a==12 | s402a==13) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu>=1 & aedu_ci<=4)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==5)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=6 & aedu_ci<=11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(aedu_ci>=6 & aedu_ci<=7)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==8)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci>=9 & aedu_ci<=11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & s402b<8)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & s402b==8) | (aedu_ci>=17 & aedu_ci<.)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(s402a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (s402a==22 | s402a==23 | s402a==24 | s402a==25)
replace eduac_ci=0 if (s402a==21 | (s402a>=26 & s402a<=27))
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(s404==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=s408a if s407==2


**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
gen      pqnoasis1_ci = 1 if s408a ==2
replace pqnoasis1_ci = 2 if s408a==3
replace pqnoasis1_ci = 3 if s408a==4 | s408a==12
replace pqnoasis1_ci = 4 if s408a==8
replace pqnoasis1_ci = 5 if s408a==9 | s408a==10
replace pqnoasis1_ci = 6 if s408a==6 
replace pqnoasis1_ci = 7 if s408a==7 | s408a==11 
replace pqnoasis1_ci = 8 if s408a==5
replace pqnoasis1_ci = 9 if s408a==1  | s408a==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


 
***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.


***************
***edupub_ci***
***************
/*Sobre los que se matricularon ese año*/

gen edupub_ci=(s406a==2)
replace edupub_ci=. if s406a==.

**************
***tecnica_ci*
**************

gen tecnica_ci=.
replace tecnica_ci=1 if s402a==25 | s402a==26
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

* MGR Jul, 2015: algunas variables de vivienda habían sido generadas como missing ya que no estaban disponibles debido a error al hacer merge.

gen aguared_ch=(s809a==1)

gen aguadist_ch=.

gen aguamala_ch=(s809a==6 | s809a==7)
replace aguamala_ch=. if s809a==.
label var aguamala_ch "Agua unimproved según MDG" 


gen aguamide_ch=.
/*NA*/

gen luz_ch= (s817 ==1)


gen luzmide_ch=.
/*NA*/

gen combust_ch=(s820==4 | s820==5 | s820==7)


gen bano_ch=(s814==1)

gen banoex_ch=(s815==1)
replace banoex_ch=. if s814==2

gen des1_ch=0 if s814==2
replace des1_ch=1 if s816>=1 & s816<=2
replace des1_ch=2 if s816==3
replace des1_ch=3 if s816==4

/*
gen des2_ch=0 if s814==2
replace des2_ch=1 if s816>=1 & s816<=2
replace des2_ch=2 if s816==3
*/
* MGR Jul, 2015 corrección sintáxis
gen des2_ch=0 if s814==2
replace des2_ch=1 if s816>=1 & s816<=3
replace des2_ch=2 if s816==4

gen piso_ch=0 if s808a==1
replace piso_ch=1 if s808a>=2 & s808a<=7
replace piso_ch=2 if s808a==8

gen pared_ch=0 if s805a==2 | s805a==3 | s805a==6
replace pared_ch=1 if s805a==1 | s805a==4 | s805a==5
replace pared_ch=2 if s805a==7

gen techo_ch=0 if s807a==4
replace techo_ch=1 if s807a>=1 & s807a<=3
replace techo_ch=2 if s807a==5

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen    aguamejorada_ch = 1 if (s809a >= 1 &  s809a <=4)
replace aguamejorada_ch = 0 if (s809a >= 5 &  s809a <=8)
		
*********************
***banomejorado_ch***
*********************
gen    banomejorado_ch = 1 if (s814 == 1 & (s816 >= 1 & s816 <=3) & s815 == 1)
replace banomejorado_ch = 0 if (s814 == 1 & (s816 >= 1 & s816 <=3) & s815 == 2) | s814 ==2 | (s814 == 1  & s816 == 4)
	
	
gen dorm_ch=s823

gen cuartos_ch=s822

gen cocina_ch= (s819==1)
gen telef_ch=.
/*NA en la base, si existe la pregunta*/


gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=1 if s801==1
replace vivi1_ch=2 if s801==2
replace vivi1_ch=3 if s801>=3 & s801<=5


gen vivi2_ch=(s801<=2)

gen viviprop_ch=0 if s802a==1
replace viviprop_ch=1 if s802a==2
replace viviprop_ch=2 if s802a==3
replace viviprop_ch=3 if s802a>=4 & s802a<=8

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=s803a if viviprop_ch==0
replace vivialq_ch=vivialq_ch*7.45 if s803b==2

gen vivialqimp_ch=s804a
replace vivialqimp_ch=vivialqimp_ch*7.45 if s804b==2

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(s201==3) if s201!=. 	
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & s203cod==2) if migrante_ci!=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
* Variables incluidas por SCL/MIG Juan Camilo Perdomo
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci = 1 if s203cod==2 & migrante_ci==1 
	replace migrantiguo5_ci = 0 if s203cod == 1 & migrante_ci==1 
	replace migrantiguo5_ci = . if migrante_ci!=1 
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
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
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) 
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 
rename s512a codocupa
rename s512b codindustria*/

rename cob_p codocupa
rename caeb_p codindustria

compress


saveold "`base_out'", replace


log close



