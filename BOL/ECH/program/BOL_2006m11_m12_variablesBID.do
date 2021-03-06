
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
local ANO "2006"
local ronda m11_m12 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11_m12
Autores: 
Última versión: Melany Gualavisi melanyg@iadb.org
Fecha última modificación: 04/11/2014

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

***************
***factor_ch***
***************

gen factor_ch=.
replace factor_ch = factor
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

sort Folio
tostring Folio,g(temp)
gen idh_ch = temp
label variable idh_ch "ID del hogar"
drop temp


*************
****idp_ci****
**************

gen idp_ci= NRO1
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 	if Urb_Rur==2
replace zona_c=1 	if Urb_Rur==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c



**************
***region_c***
**************

gen region_c=.
label var region_c "Region" 


************
****pais****
************

gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2006
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=11
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*
s1_05:
           1 Jefe o jefa del hogar
           2 Esposa(o) o conviviente
           3 Hijo(a) o entenado(a)
           4 Yerno o nuera
           5 Hermano(a) o Cuñado(a)
           6 Padres
           7 Suegros
           8 Nieto o nieta
           9 Otro pariente
          10 Otro que no es pariente
          11 Empleada(o) del hogar cama adentro
          12 Pariente de la empleada(o) del hogar
*/

gen relacion_ci=.
replace relacion_ci=1 if s1_05==1
replace relacion_ci=2 if s1_05==2
replace relacion_ci=3 if s1_05==3
replace relacion_ci=4 if s1_05>=4 & s1_05<=9
replace relacion_ci=5 if s1_05==10 | s1_05==12 
replace relacion_ci=6 if s1_05==11

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

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci = s1_02

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci= s1_03
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************

/*
s1_11:
           1 Soltero(a)
           2 Casado(a)
           3 Conviviente /Concubino(a)
           4 Separado(a)
           5 Divorciado(a)
           6 Viudo(a)
*/

gen civil_ci=.
replace civil_ci=1 		if s1_11==1
replace civil_ci=2 		if s1_11==2 | s1_11==3
replace civil_ci=3 		if s1_11==4 | s1_11==5
replace civil_ci=4 		if s1_11==6

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
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

	***************
	***afroind_ci***
	***************
**Pregunta: pertenencia a algun pueblo originario o indigena? (s1_10) (quechua 1; aymara 2; guarani 3; chiquitano 4; moje�o 5; otro 6; ninguno 7) NO AFRODESCENDENTS 

gen afroind_ci=. 
replace afroind_ci=1  if S1_10!=7  
replace afroind_ci=2 if S1_10==0 
replace afroind_ci=3 if S1_10==7
replace afroind_ci=. if S1_10==.
replace afroind_ci=9 if S1_10==. & edad_ci<12


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2005

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
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 
*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 399.578840884648 if ciudad1==1 |  ID01==1 & zona_c==1
replace lp_ci= 385.470605878233 if ciudad1==2 |  ID01==2 & zona_c==1
replace lp_ci= 418.262232030231 if ciudad1==3 |  ID01==3 & zona_c==1
replace lp_ci= 350.523249440621 if ciudad1==4 |  ID01==4 & zona_c==1
replace lp_ci= 322.384153144916 if ciudad1==5 |  ID01==5 & zona_c==1
replace lp_ci= 418.262232030231 if ciudad1==6 |  ID01==6 & zona_c==1
replace lp_ci= 422.14604252096 if ciudad1==7  |  ID01==7 & zona_c==1
replace lp_ci= 422.14604252096 if ciudad1==8  |  ID01==8 & zona_c==1
replace lp_ci= 313.342580371434 if ciudad1==10 |  ID01==9 & zona_c==1
replace lp_ci= 422.14604252096 if ciudad1==9 & zona_c==1
replace lp_ci=  294.00   if  zona_c==0


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 201.787314646747 if ciudad1==1 |  ID01==1 & zona_c==1
replace lpe_ci= 214.321656868298 if ciudad1==2 |  ID01==2 & zona_c==1
replace lpe_ci= 211.222427175267 if ciudad1==3 |  ID01==3 & zona_c==1
replace lpe_ci= 194.890926688986 if ciudad1==4 |  ID01==4 & zona_c==1
replace lpe_ci= 179.245589148573 if ciudad1==5 |  ID01==5 & zona_c==1
replace lpe_ci= 211.222427175267 if ciudad1==6 |  ID01==6 & zona_c==1
replace lpe_ci= 214.450189600648 if ciudad1==7 |  ID01==7 & zona_c==1
replace lpe_ci= 214.450189600648 if ciudad1==8 |  ID01==8 & zona_c==1
replace lpe_ci= 190.198946285461 if ciudad1==10 |  ID01==9 & zona_c==1
replace lpe_ci= 214.450189600648 if ciudad1==9  & zona_c==1
replace lpe_ci=   167.58   if  zona_c==0





label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 2006
gen salmm_ci= 	500.00


label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci= 1 if  s5_54 ==1	
*replace afiliado_ci =1 if s5_71b==1
recode afiliado_ci .=0  if condact>=1 & condact<=3
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.

replace tipopen_ci=1 if s6_01a>0 &  s6_01a~=.
replace tipopen_ci=2 if s6_01d>0 & s6_01d~=.
replace tipopen_ci=3 if s6_01b>0 & s6_01b~=.
replace tipopen_ci=4 if s6_01c>0 & s6_01c~=. 
replace tipopen_ci=12 if (s6_01a>0 & s6_01d>0) & (s6_01a~=. & s6_01d~=.)
replace tipopen_ci=13 if (s6_01a>0 & s6_01b>0) & (s6_01a~=. & s6_01b~=.)
replace tipopen_ci=23 if (s6_01d>0 & s6_01b>0) & (s6_01d~=. & s6_01b~=.)
replace tipopen_ci=123 if (s6_01a>0 & s6_01d>0 & s6_01b>0) & (s6_01a~=. & s6_01d~=. & s6_01b~=.)
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
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact==2 | condact==3
replace condocup_ci=3 if (condact==4 | condact==5) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Homologacion toda la serie 05/27/2014 MGD

gen condocup_ci=.
*Mod. MLO 2015,10: se consideran otras causas excepcionales 
*replace condocup_ci=1 if s5_01==1 | s5_02<=6  | s5_03a==1
replace condocup_ci=1 if s5_01==1 | s5_02<=6 | (s5_03a>=1 & s5_03a<=7)
*replace condocup_ci=2 if (s5_01==2 | s5_02==7 | s5_03>1) & (s5_05==1) & (s5_04==1)
replace condocup_ci=2 if (s5_01==2 | s5_02==7 | s5_03a>7) & (s5_05==1) & (s5_04==1)
*2015,10 MLO la encuesta pregunta a partir de 7 años (no 10)
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if s5_07==1 & condocup_ci==2
* 2014, 03 Modificacion siguiente linea MLO
replace cesante_ci=0 if s5_07==2 & condocup_ci==2
*replace cesante_ci=0 if s5_07==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if s5_26>=1 & s5_26<=5
replace tamemp_ci=2 if s5_26>=6 & s5_26<=49
replace tamemp_ci=3 if s5_26>49 & s5_26!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
egen aux_p=rsum(s6_01a s6_01b s6_01c s6_01d), missing
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
*2014,07 MLO: el pago es 1800 anuales, se mensualiza

gen aux_ps= s6_05c1/12 if s6_05c2=="A" & s6_05c1>1 & s6_05c1!=. 
replace aux_ps =(s6_05c1*8.1)/12 if s6_05c2=="B"  & s6_05c1>1 & s6_05c1!=. 
gen byte pensionsub_ci=1 if aux_ps>0 & aux_ps!=.
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
*07/2014 MLO: modifique esta parte de la sintaxis porque se generaba como missing
*destring aux_ps, replace
gen ypensub_ci=aux_ps

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

gen desalent_ci=(emp_ci==0 & (s5_15==3 | s5_15==4))
replace desalent_ci=. if emp_ci==.
label var desalent_ci "Trabajadores desalentados"


*****************
***horaspri_ci***
*****************

*NOTA: se la variable se está creando como horas trabajadas al mes. Sin embargo, la definición indica que es 
*a la semana.  Por lo tanto, se cambia la programación y debe revisarse en los programas de años anteriores

*Días a la semana s5_27a
*Horas diarias s5_27b 
*gen horassem =  s5_27b* s5_27a
*gen horaspri_ci= horassem*4.3

gen horaspri_ci=s5_27b* s5_27a
replace horaspri_ci=. if s5_27b==. & s5_27a==.
replace horaspri_ci=. if emp_ci~=1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"


*****************
***horastot_ci***
*****************
*Nota: para el segundo trabajo también se mensualiza. Por tanto, se construye la programación para los datos 
*semanales
*drop horassem
*gen horassem =  s5_43a*s5_43b1
*gen horassec_ci= horassem*4.3

gen horassec_ci= s5_43a*s5_43b1
replace horassec_ci=. if s5_43a==. | s5_43b1==.
replace horassec_ci=. if emp_ci~=1

egen horastot_ci= rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = . if horaspri_ci == . & horassec_ci == .
replace horassec_ci=. if emp_ci~=1


***************
***subemp_ci***
***************
*NOTA: aquí fue necesario ajustar el cambio de horas
*gen subemp_ci=.
*replace subemp_ci = 1 if s5_65 == 1 & horastot_ci <= 129
*replace subemp_ci = 0 if s5_65 == 2 & emp_ci == 1
/*
gen subemp_ci=.
replace subemp_ci=1 if s5_65== 1 & horastot_ci <= 30
replace subemp_ci=0 if s5_65== 2 & emp_ci == 1
replace subemp_ci=. if s5_65==. | horastot_ci==.
label var subemp_ci "Personas en subempleo por horas"
*/

* Se considera subempleo visible: quiere trabajar mas horas y esta disponible. MGD 06/18/2014
gen subemp_ci=0
*replace subemp_ci=1 if s5_66==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (s5_66==1 & s5_65==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
*NOTA: aquí fue necesario ajustar el cambio de horas
*gen tiempoparc_ci=.
*replace tiempoparc_ci = 1 		if  s5_65 == 2 & horastot_ci <= 129
*replace tiempoparc_ci = 0 		if  s5_65 == 2 & emp_ci == 1 & horastot_ci > 129

gen tiempoparc_ci=.
*replace tiempoparc_ci=1 if s5_65==2 & horastot_ci<30
*replace tiempoparc_ci=0 if s5_65==2 & emp_ci == 1 & horastot_ci>30

*Mod. MLO 2015, 10
replace tiempoparc_ci=(s5_65==2 & horaspri_ci<30 & emp_ci == 1)
replace tiempoparc_ci=. if emp_ci==0

label var tiempoparc_c "Personas que trabajan medio tiempo" 


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if s5_21>=4 & s5_21<=6
replace categopri_ci=2 if s5_21==3
replace categopri_ci=3 if s5_21==1 | s5_21==2 | s5_21==8
replace categopri_ci=4 if s5_21==7
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"



******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if s5_40>=4 & s5_40<=6
replace categosec_ci=2 if s5_40==3
replace categosec_ci=3 if s5_40==1 | s5_40==2 | s5_40==8
replace categosec_ci=4 if s5_40==7

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if s5_22==3 & categopri_ci==3
replace tipocontrato_ci=2 if s5_22==1 & categopri_ci==3
replace tipocontrato_ci=3 if ((s5_22==2 | s5_22==4) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s5_37==1
label var nempleos_ci "Número de empleos" 

/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
replace firmapeq_ci=1 if  s5_26>=1 &  s5_26<=5 
replace firmapeq_ci=0 if  s5_26>=6 &  s5_26!=.  
label var firmapeq_ci "Trabajadores informales"
*/

*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=.
replace spublico_ci=1 if s5_23==1
replace spublico_ci=0 if s5_23==2
replace spublico_ci=. if emp_ci~=1
label var spublico_ci "Personas que trabajan en el sector público"



**************
***ocupa_ci***
**************
*cob_op:
*NA: No se puede estandarizar ya que no se distingue entre dos categorias:
*comerciantes y vendedores y trabajadores en servicios 

* Modificacion MGD 07/24/2014: clasificacion CIUO -88
g aux = substr(s5_16cod,1,3)
destring aux, replace
gen ocupa_ci=.
replace ocupa_ci=1 if ((aux>=210 & aux<=348) | (aux>=21 & aux<=34)) & emp_ci==1
replace ocupa_ci=2 if ((aux>=110 & aux<=131) |  aux==11) & emp_ci==1
replace ocupa_ci=3 if ((aux>=410 & aux<=422) |  aux==41 |  aux==42) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911) | aux==52 | aux==91) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=621) | (aux>=920 & aux<=921) | aux==92) & emp_ci==1
replace ocupa_ci=7 if ((aux>=710 & aux<=851) | (aux>=930 & aux<=933) | aux==71 | aux==81 | aux==93)& emp_ci==1
replace ocupa_ci=8 if (aux>=0 & aux<=8) & emp_ci==1

drop aux

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************
/*
caeb_op:
           1 Agricultura, Ganadería y Caza
           2 Silvicultura y Pesca
           3 Explotación de Minas y Canteras
           4 Industria Manufacturera
           5 Prod. y Distr. de Electricidad, Gas y Agua
           6 Construcción
           7 Venta y Reparaciones
           8 Hoteles y Restaurantes
           9 Transporte, Almacenamiento, Comunicaciones
          10 Intermediación Financiera
          11 Serv. Inmobiliarios, Empresariales y de Alquiler
          12 Adm. Pública, Defensa y Seguridad Social
          13 Educación
          14 Servicios Sociales y de Salud
          15 Servicios Comunitarios y Personales
          16 Hogares Privados
          17 Organismos Extraterritoriales
*/


gen rama_ci=.
replace rama_ci=1 if caeb_op>=1 & caeb_op<=2 & emp_ci==1
replace rama_ci=2 if caeb_op==3 & emp_ci==1
replace rama_ci=3 if caeb_op==4 & emp_ci==1
replace rama_ci=4 if caeb_op==5 & emp_ci==1
replace rama_ci=5 if caeb_op==6 & emp_ci==1
replace rama_ci=6 if caeb_op>=7 & caeb_op<=8 & emp_ci==1 
replace rama_ci=7 if caeb_op==9 & emp_ci==1
replace rama_ci=8 if caeb_op>=10 & caeb_op<=11 & emp_ci==1
replace rama_ci=9 if caeb_op>=12 & caeb_op<=17 & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


****************
***durades_ci***
****************

gen durades_ci=.
replace durades_ci=s5_13a/4.3  if s5_13b==2
replace durades_ci=s5_13a      if s5_13b==4
replace durades_ci=s5_13a*12   if s5_13b==8
label variable durades_ci "Duracion del desempleo en meses"



*******************
***antiguedad_ci***
*******************
recode s5_19a (99=.)
gen antiguedad_ci=.
replace antiguedad_ci=s5_19a/52  	if s5_19b==2 & emp_ci==1
replace antiguedad_ci=s5_19a/12      	if s5_19b==4 & emp_ci==1
replace antiguedad_ci=s5_19a	   	if s5_19b==8 & emp_ci==1
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (s5_14==3 & condocup_ci==3)
replace categoinac_ci = 2 if  (s5_14==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s5_14==2 & condocup_ci==3)
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

**************
***INGRESOS***
**************


*************************
*********LABORAL*********
*************************

/*
s5_29b:
           1 bs.
           2 $us 	-	 

s5_29c:
           1 diario
           2 semanal
           3 quincenal
           4 mensual
           5 trimestral
           6 semestral
           7 anual
*/

*******************
* salario liquido *
*******************

gen s5_29a2 = .
replace s5_29a2 = s5_29a 		if s5_29b=="A"
replace s5_29a2 = s5_29a*8.1 	if s5_29b=="B"

gen yliquido = .
replace yliquido= s5_29a2*30	if s5_29c==1
replace yliquido= s5_29a2*4.3	if s5_29c==2
replace yliquido= s5_29a2*2		if s5_29c==3
replace yliquido= s5_29a2		if s5_29c==4
replace yliquido= s5_29a2/2		if s5_29c==5
replace yliquido= s5_29a2/3		if s5_29c==6
replace yliquido= s5_29a2/6 	if s5_29c==7
replace yliquido= s5_29a2/12	if s5_29c==8

**************
* comisiones *
**************

gen ycomisio = .
replace ycomisio= s5_31a1*30	if s5_31a2==1
replace ycomisio= s5_31a1*4.3	if s5_31a2==2
replace ycomisio= s5_31a1*2		if s5_31a2==3
replace ycomisio= s5_31a1		if s5_31a2==4
replace ycomisio= s5_31a1/2		if s5_31a2==5
replace ycomisio= s5_31a1/3		if s5_31a2==6
replace ycomisio= s5_31a1/6 	if s5_31a2==7
replace ycomisio= s5_31a1/12	if s5_31a2==8


****************
* horas extras *
****************

gen yhrsextr= .
replace yhrsextr= s5_31b1*30	if s5_31b2==1
replace yhrsextr= s5_31b1*4.3	if s5_31b2==2
replace yhrsextr= s5_31b1*2		if s5_31b2==3
replace yhrsextr= s5_31b1		if s5_31b2==4
replace yhrsextr= s5_31b1/2		if s5_31b2==5
replace yhrsextr= s5_31b1/3		if s5_31b2==6
replace yhrsextr= s5_31b1/6 	if s5_31b2==7
replace yhrsextr= s5_31b1/12	if s5_31b2==8


*********
* prima *
*********
*Esto tiene un periodo de referencia de los últimos 12 meses. Hay que revisarlo para los años anteriores 
*La programación anterior queda inactiva
*gen yprima = .
*replace yprima = s5_30a1 		if s5_30a2=="A"
*replace yprima = s5_30a1*8.1	if s5_30a2=="B"

gen yprima = .
replace yprima = (s5_30a1)/12 		if s5_30a2=="A"
replace yprima = (s5_30a1*8.1)/12		if s5_30a2=="B"


*************
* aguinaldo *
*************
*Esto tiene un periodo de referencia de los últimos 12 meses. Hay que revisarlo para los años anteriores 
*La programación anterior queda inactiva
*gen yaguina = .
*replace yaguina = s5_30b1 		if s5_30b2=="A"
*replace yaguina = s5_30b1*8.1	if s5_30b2=="B"

gen yaguina = .
replace yaguina = (s5_30b1)/12 		if s5_30b2=="A"
replace yaguina = (s5_30b1*8.1)/12 	if s5_30b2=="B"


*************
* alimentos *
*************

gen yalimen = .
replace yalimen= s5_33a3*30		if s5_33a2==1 & s5_33a1==1
replace yalimen= s5_33a3*4.3	if s5_33a2==2 & s5_33a1==1
replace yalimen= s5_33a3*2		if s5_33a2==3 & s5_33a1==1
replace yalimen= s5_33a3		if s5_33a2==4 & s5_33a1==1
replace yalimen= s5_33a3/2		if s5_33a2==5 & s5_33a1==1
replace yalimen= s5_33a3/3		if s5_33a2==6 & s5_33a1==1
replace yalimen= s5_33a3/6		if s5_33a2==7 & s5_33a1==1
replace yalimen= s5_33a3/12		if s5_33a2==8 & s5_33a1==1

**************
* transporte *
**************
* No hay la categoria bimestral (5)
gen ytranspo = .
replace ytranspo= s5_33b3*30	if s5_33b2==1 & s5_33b1==1
replace ytranspo= s5_33b3*4.3	if s5_33b2==2 & s5_33b1==1
replace ytranspo= s5_33b3*2		if s5_33b2==3 & s5_33b1==1
replace ytranspo= s5_33b3		if s5_33b2==4 & s5_33b1==1
replace ytranspo= s5_33b3/3		if s5_33b2==6 & s5_33b1==1
replace ytranspo= s5_33b3/6		if s5_33b2==7 & s5_33b1==1
replace ytranspo= s5_33b3/12	if s5_33b2==8 & s5_33b1==1

**************
* vestimenta *
**************

gen yvesti = .
replace yvesti= s5_33c3*30		if s5_33c2==1 & s5_33c1==1
replace yvesti= s5_33c3*4.3		if s5_33c2==2 & s5_33c1==1
replace yvesti= s5_33c3*2		if s5_33c2==3 & s5_33c1==1
replace yvesti= s5_33c3			if s5_33c2==4 & s5_33c1==1
replace yvesti= s5_33c3/2		if s5_33c2==5 & s5_33c1==1
replace yvesti= s5_33c3/3		if s5_33c2==6 & s5_33c1==1
replace yvesti= s5_33c3/6		if s5_33c2==7 & s5_33c1==1
replace yvesti= s5_33c3/12		if s5_33c2==8 & s5_33c1==1

************
* vivienda *
************
* No hay bimestral ni semestral (5 y 7)
gen yvivien = .
replace yvivien= s5_33d3*30		if s5_33d2==1 & s5_33d1==1
replace yvivien= s5_33d3*4.3	if s5_33d2==2 & s5_33d1==1
replace yvivien= s5_33d3*2		if s5_33d2==3 & s5_33d1==1
replace yvivien= s5_33d3		if s5_33d2==4 & s5_33d1==1
replace yvivien= s5_33d3/3		if s5_33d2==6 & s5_33d1==1
replace yvivien= s5_33d3/12		if s5_33d2==8 & s5_33d1==1


*************
* guarderia *
*************
* No hay la categoria semestral
gen yguarde = .
replace yguarde= s5_33e3*30		if s5_33e2==1 & s5_33e1==1
replace yguarde= s5_33e3*4.3	if s5_33e2==2 & s5_33e1==1
replace yguarde= s5_33e3*2		if s5_33e2==3 & s5_33e1==1
replace yguarde= s5_33e3		if s5_33e2==4 & s5_33e1==1
replace yguarde= s5_33e3/2		if s5_33e2==5 & s5_33e1==1
replace yguarde= s5_33e3/3		if s5_33e2==6 & s5_33e1==1
replace yguarde= s5_33e3/12		if s5_33e2==8 & s5_33e1==1

**********************************
* ingreso act. pr independientes *
**********************************

gen s5_36a2 = .
replace s5_36a2 = s5_36a 		if s5_36b=="A"
replace s5_36a2 = s5_36a*8.1 		if s5_36b=="B"

gen yactpri = .
replace yactpri= s5_36a2*30		if s5_36c==1
replace yactpri= s5_36a2*4.3	if s5_36c==2
replace yactpri= s5_36a2*2		if s5_36c==3
replace yactpri= s5_36a2		if s5_36c==4
replace yactpri= s5_36a2/2		if s5_36c==5
replace yactpri= s5_36a2/3		if s5_36c==6
replace yactpri= s5_36a2/6		if s5_36c==7
replace yactpri= s5_36a2/12		if s5_36c==8

********************
* salario liquido2 *
********************

gen s5_45a2 = .
replace s5_45a2 = s5_45a 		if s5_45b=="A"
replace s5_45a2 = s5_45a*8.1 	if s5_45b=="B"

gen yliquido2 = .
replace yliquido2= s5_45a2*30		if s5_45c==1
replace yliquido2= s5_45a2*4.3		if s5_45c==2
replace yliquido2= s5_45a2*2		if s5_45c==3
replace yliquido2= s5_45a2		    if s5_45c==4
replace yliquido2= s5_45a2/2		if s5_45c==5
replace yliquido2= s5_45a2/3		if s5_45c==6
replace yliquido2= s5_45a2/6		if s5_45c==7
replace yliquido2= s5_45a2/12		if s5_45c==8


*****************
* Horas extra 2 *
*****************
*Para el empleo secundario se pregunta por los últimos 12 meses. Por eso se divide entre 12. Se debe 
*revisar esto para los años anteriores

gen yhrsextr2 = .
replace yhrsextr2=s5_46a2/12 if s5_46a1==1


***************************************
* alimentos, transporte y vestimenta2 *
***************************************
*Para el empleo secundario se pregunta por los últimos 12 meses. Por eso se divide entre 12. Se debe 
*revisar esto para los años anteriores

gen yalimen2 = .
replace yalimen2= s5_46b2/12		if s5_46b1==1


**************
* vivienda 2 *
**************
*Para el empleo secundario se pregunta por los últimos 12 meses. Por eso se divide entre 12. Se debe 
*revisar esto para los años anteriores
gen yvivien2= .
replace yvivien2= s5_46c2/12 if s5_46c1==1


*************************
******NO-LABORAL*********
*************************

*************
* intereses *
*************

gen yinteres = .
replace yinteres = s6_02a1		if s6_02a2=="A" 
replace yinteres = s6_02a1*8.1		if s6_02a2=="B" 

**************
* alquileres *
**************

gen yalqui = .
replace yalqui = s6_02b1		if s6_02b2=="A" 
replace yalqui = s6_02b1*8.1		if s6_02b2=="B" 

**************
* jubilacion *
**************

gen yjubi = .
replace yjubi = s6_01a

**************
* benemerito *
**************

gen ybene = .
replace ybene = s6_01b

*************
* invalidez *
*************

gen yinvali = .
replace yinvali = s6_01c

**********
* viudez *
**********

gen yviudez = .
replace yviudez = s6_01d


****************
* otras rentas *
****************

gen yotren = .
replace yotren = s6_02c1		if s6_02c2=="A" 
replace yotren = s6_02c1*8.1		if s6_02c2=="B" 


************************
* alquileres agricolas *
************************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.

gen yalqagri = .
replace yalqagri = (s6_03a1)/12		if s6_03a2=="A" 
replace yalqagri = (s6_03a1*8.1)/12	if s6_03a2=="B" 


**************
* dividendos *
**************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.

gen ydivi = .
replace ydivi =  (s6_03b1)/12		if s6_03b2=="A" 
replace ydivi =  (s6_03b1*8.1)/12		if s6_03b2=="B" 


*************************
* alquileres maquinaria *
*************************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yalqmaqui = .
replace yalqmaqui = (s6_03c1)/12		if s6_03c2=="A" 
replace yalqmaqui = (s6_03c1*8.1)/12		if s6_03c2=="B" 
 

******************
* indem. trabajo *
******************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yindtr = .
replace yindtr = (s6_05a1)/12		if s6_05a2=="A"
replace yindtr = (s6_05a1*8.1)/12		if s6_05a2=="B"


******************
* indem. seguros *
******************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yindseg = .
replace yindseg = (s6_05b1)/12		if s6_05b2=="A"
replace yindseg = (s6_05b1*8.1)/12	if s6_05b2=="B"


***********
* bonosol *
***********

*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen ybono = .
replace ybono = (s6_05c1)/12			if s6_05c2=="A"
replace ybono = (s6_05c1*8.1)/12		if s6_05c2=="B"


******************
* otros ingresos *
******************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yotring = .
replace yotring = (s6_05d1)/12		if s6_05d2=="A"
replace yotring = (s6_05d1*8.1)/12	if s6_05d2=="B"


*******************
* asist. familiar *
*******************
/*
 2  semanal
 4  mensual
 6  trimestral
 7  semestral
 8  anual
*/
gen s6_04a12 = .
replace s6_04a12 = s6_04a1 		if s6_04a2=="A"
replace s6_04a12 = s6_04a1*8.1 		if s6_04a2=="B"

gen yasistfam = .
replace yasistfam= s6_04a12*4.3		if s6_04a3==2
replace yasistfam= s6_04a12			if s6_04a3==4
replace yasistfam= s6_04a12/3		if s6_04a3==6
replace yasistfam= s6_04a12/6		if s6_04a3==7
replace yasistfam= s6_04a12/12		if s6_04a3==8


*********************
* Trans. monetarias *
*********************

gen s6_04b12 = .
replace s6_04b12 = s6_04b1 		if s6_04b2=="A"
replace s6_04b12 = s6_04b1*8.1 		if s6_04b2=="B"

gen ytransmon = .
replace ytransmon= s6_04b12*4.3		if s6_04b3==2
replace ytransmon= s6_04b12*2		if s6_04b3==3
replace ytransmon= s6_04b12		    if s6_04b3==4
replace ytransmon= s6_04b12/2		if s6_04b3==5
replace ytransmon= s6_04b12/3		if s6_04b3==6
replace ytransmon= s6_04b12/6		if s6_04b3==7
replace ytransmon= s6_04b12/12		if s6_04b3==8

***********
* remesas *
***********

gen s6_04c12 = .
replace s6_04c12 = s6_04c1 		if s6_04c2=="A"
replace s6_04c12 = s6_04c1*8.1 		if s6_04c2=="B"

* No hay categoria diaria.
gen yremesas = .
replace yremesas= s6_04c12*4.3		if s6_04c3==2
replace yremesas= s6_04c12*2		if s6_04c3==3
replace yremesas= s6_04c12		    if s6_04c3==4
replace yremesas= s6_04c12/2		if s6_04c3==5
replace yremesas= s6_04c12/3		if s6_04c3==6
replace yremesas= s6_04c12/6		if s6_04c3==7
replace yremesas= s6_04c12/12		if s6_04c3==8


/* 
ylm:
yliquido 
ycomisio 
ypropinas 
yhrsextr 
yprima 
yaguina
yactpri 
yliquido2

ylnm:
yrefrige 
yalimen 
ytranspo 
yvesti 
yvivien 
yguarde */




***************
***ylmpri_ci***
***************

egen ylmpri_ci=rsum(yliquido ycomisio yhrsextr yprima yaguina yactpri), missing
replace ylmpri_ci=. if yliquido ==. & ycomisio ==. &  yhrsextr ==. & yprima ==. &  yaguina ==. &  yactpri==.  
replace ylmpri_ci=. if emp_ci~=1
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


******************
*** ylnmpri_ci ***
******************
*Aqui se venían contando ingresos del segundo empleo. Por ello, se cambia la programación. Se debe revisar desde cuando
*se han generado estos ingresos así.
*egen ylnmprid=rsum(yalimen ytranspo yvesti yvivien yguarde yalimen2 yhrsextr2 yvivien2), missing
*replace ylnmprid=. if yalimen==. & ytranspo==. & yvesti==. & yvivien==. & yguarde==. & yalimen2==. & yhrsextr2==. & yvivien2==.  
*replace ylnmprid=0 if categopri_ci==4

egen ylnmprid=rsum(yalimen ytranspo yvesti yvivien yguarde), missing
replace ylnmprid=. if yalimen==. & ytranspo==. & yvesti==. & yvivien==. & yguarde==.   
replace ylnmprid=0 if categopri_ci==4

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

gen ylmsec_ci= yliquido2 
replace ylmsec_ci=. if emp_ci~=1
replace ylmsec_ci=0 if categosec_ci==4
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


******************
****ylnmsec_ci****
******************

egen ylnmsec_ci=rsum(yalimen2 yhrsextr2 yvivien2), missing
replace ylnmsec_ci=. if yalimen2==. & yhrsextr2==. & yvivien2==.  
replace ylnmsec_ci=0 if categosec_ci==4
replace ylnmsec_ci=. if emp_ci==0
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

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

 
/* 

ynlm:

yinteres 
yalqui 
yjubi 
ybene 
yinvali 
yviudez 
yotren  
yalqagri 
ydivi 
yalqmaqui  
yindtr  
yindseg 
yheren 
ypasu 
ybono  
yotring  
yasistfam 
ytransmon 
yremesas 
yinvers 
yhipotec 
ybonos 
ypresta 
yprestata 
yinmueb 
yinmrur 
yvehi 
yelec 
ymuebles 
yjoyas */



*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(yinteres yalqui yjubi ybene yinvali yviudez yotren yalqagri ydivi yalqmaqui yindtr yindseg ybono yotring yasistfam ytransmon yremesas ), missing
replace ynlm_ci=. if 	yinteres==. & yalqui==. & yjubi==. & ybene==. & yinvali==. & yviudez==. & yotren==. & yalqagri==. & ydivi==. & yalqmaqui==. & yindtr==. & yindseg==. & ///
			ybono==. & yotring==. & yasistfam==. & ytransmon==. & yremesas==. 
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 



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
***remesas_ci***
*****************

gen remesas_ci=yremesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 



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


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"


*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"


****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


*******************
*** autocons_ci ***
*******************

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"


*******************
*** autocons_ch ***
*******************

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"


*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch= s8_04a
replace rentaimp_ch=rentaimp_ch*8.1 if s8_04b=="B"
label var rentaimp_ch "Rentas imputadas del hogar"


*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 



****************************
***VARIABLES DE EDUCACION***
****************************

/*En esta sección es sólo para los mayores a los 5 años de edad*/

/*
s4_02a:
          11 Ninguno
          12 Curso de alfabetización
          13 Educación pre-escolar
          14 Básico (1 a 5 años)
          15 Intermedio (1 a 3 años)
          16 Medio (1 a 4 años)
          17 Primaria (1 a 8 años)
          18 Secundaria (1 a 4 años)
          19 Educación básica de adultos (EBA)
          20 Centro de educación media de adultos (CEMA)
          21 Educación juvenil alternativa(EJA)
          22 Educación primaria de adultos(EPA)
          23 Eduación secundaria de adultos(ESA)
          24 Normal
          25 Universidad  pública(Licenciatura)
          26 Universidad  privada (Licenciatura)
          27 Postgrado (Diplomado, especialidad, maestría, doctorado)
          28 Técnico de universidad
          29 Técnico de instituto(Duración mayor o igual a 1 año)
          30 Institutos de formación militar y policial
          31 Otros cursos(Duración menor a 1 año)
*/


gen byte aedu_ci=.

replace aedu_ci=0 if s4_02a==11 | s4_02a==12 | s4_02a==13

replace aedu_ci=1 if (s4_02a==14 | s4_02a==17) & s4_02b==1
replace aedu_ci=2 if (s4_02a==14 | s4_02a==17) & s4_02b==2
replace aedu_ci=3 if (s4_02a==14 | s4_02a==17) & s4_02b==3
replace aedu_ci=4 if (s4_02a==14 | s4_02a==17) & s4_02b==4
replace aedu_ci=5 if (s4_02a==14 | s4_02a==17) & s4_02b==5

replace aedu_ci=6 if (s4_02a==17 & s4_02b==6) | (s4_02a==15 & s4_02b==1)
replace aedu_ci=7 if (s4_02a==17 & s4_02b==7) | (s4_02a==15 & s4_02b==2)
replace aedu_ci=8 if (s4_02a==17 & s4_02b==8) | (s4_02a==15 & s4_02b==3)

replace aedu_ci=9 if (s4_02a==16 | s4_02a==18) & s4_02b==1
replace aedu_ci=10 if (s4_02a==16 | s4_02a==18) & s4_02b==2
replace aedu_ci=11 if (s4_02a==16 | s4_02a==18) & s4_02b==3
replace aedu_ci=12 if (s4_02a==16 | s4_02a==18) & s4_02b==4

replace aedu_ci=13 if (s4_02a>=24 & s4_02a<=30 & s4_02a~=27) & s4_02b==1
replace aedu_ci=14 if (s4_02a>=24 & s4_02a<=30 & s4_02a~=27) & s4_02b==2
replace aedu_ci=15 if (s4_02a>=24 & s4_02a<=30 & s4_02a~=27) & s4_02b==3
replace aedu_ci=16 if (s4_02a>=24 & s4_02a<=30 & s4_02a~=27) & s4_02b==4
replace aedu_ci=17 if (s4_02a==24 | s4_02a==28 | s4_02a==29) & (s4_02b>=5 & s4_02b<=8)
replace aedu_ci=17 if (s4_02a==25 | s4_02a==26 | s4_02a==30) & (s4_02b>=5 & s4_02b<=8)

replace aedu_ci=18 if s4_02a==27 & s4_02b==1
replace aedu_ci=19 if s4_02a==27 & s4_02b==2
replace aedu_ci=20 if s4_02a==27 & s4_02b==3
replace aedu_ci=21 if s4_02a==27 & s4_02b==4
replace aedu_ci=22 if s4_02a==27 & s4_02b==5

************Cambios
replace aedu_ci=19 if s4_02a==27 & s4_02b==8
************Cambios

label var aedu_ci "Anios de educacion aprobados" 


**************
***eduno_ci***
**************

gen byte eduno_ci=(s4_02a==11 | s4_02a==12 | s4_02a==13) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu>=1 & aedu_ci<=5)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=7 & aedu_ci<=11)
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

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & s4_02b<8)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & s4_02b==8) | (aedu_ci>=17 & aedu_ci<.)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"

***************
***edupre_ci***
***************

gen byte edupre_ci=(s4_02a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"



**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (s4_02a==25 | s4_02a==26 | s4_02a==27 | s4_02a==28)
replace eduac_ci=0 if (s4_02a==24 | (s4_02a>=29 & s4_02a<=30))
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

*gen asiste_ci=(s4_08==1)
*LCM (introducido por YL): Se cambia la forma de cálculo porque se deben considerar los rangos de edad lcm dic2013
*LCM (introducido por YL): Se cambia la forma de cálculo porque se deben considerar los rangos de edad lcm dic2013
*Modificación Mayra Sáenz Enero-2017: Se genera la dummy de acuerdo al documento metodológico.
gen asiste_ci= s4_04==1
/*
gen asiste_ci= 1 if s4_04==1
replace asiste_ci = 0 if s4_04==2*/
label variable asiste_ci "Asiste actualmente a la escuela"

***************
***asispre_ci***
***************
*Variable añadida por Iván Bornacelly - 01/12/2017
g asispre_ci=.	
replace asispre_ci=1 if s4_04==1 & s4_05a==13
recode asispre_ci (.=0)
la var asispre_ci "Asiste a educacion prescolar"

**************
***pqnoasis***
**************

gen pqnoasis_ci=S4_09
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"vacación/receso" 2"falta de dinero" 3"por trabajo" 4"por enfermedad/accidente/discapacidad"
label def pqnoasis_ci 5"los establecimientos son distantes" 6"culminó sus estudios" 7"edad temprana/ edad avanzada", add
label def pqnoasis_ci 8"falta de interés" 9"labores de casa/ embarazo/cuidado de niños/as" 10"otra", add
label val pqnoasis_ci pqnoasis_ci 

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************

gen      pqnoasis1_ci = 1 if S4_09==2
replace pqnoasis1_ci = 2 if S4_09==3
replace pqnoasis1_ci = 3 if S4_09==4 
replace pqnoasis1_ci = 4 if S4_09==8
replace pqnoasis1_ci = 5 if S4_09==9 
replace pqnoasis1_ci = 6 if S4_09==6 
replace pqnoasis1_ci = 7 if S4_09==7  
replace pqnoasis1_ci = 8 if S4_09==5
replace pqnoasis1_ci = 9 if S4_09==1  | S4_09==10

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

 
***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=(s4_07==1)
replace repiteult_ci=. if s4_07==.
label var repiteult "Ha repetido el último grado"



***************
***edupub_ci***
***************
/*Sobre los que se matricularon ese año*/
/*
s4_06:
           1 Particular / privado
           2 Fiscal / público
           3 Público de Convenio
*/


gen edupub_ci=(s4_06==2 | s4_06==3)
replace edupub_ci=. if s4_06==.
label var edupub_ci "Asiste a un centro de ensenanza público"


**************
***tecnica_ci*
**************

gen tecnica_ci=.
replace tecnica_ci=1 if s4_02a==28 | s4_02a==29
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

****************
***aguared_ch***
****************
/*
*s8_10:

gen aguared_ch=(s8_10==1 | s8_10==2)
replace aguared_ch=. if s8_10==.
label var aguared_ch "Acceso a fuente de agua por red"


* Modificación Mayra Sáenz Julio 2013
* Mejor se debe utilizar esta variable


s8_09


           1 cañería de red
           2 pileta pública
           3 carro repartidor (aguatero)
           4 pozo o noria con bomba
           5 pozo o noria sin bomba
           6 río/vertiente/acequia
           7 lago/laguna/curiche
           8 otro
*/

gen aguared_ch=(s8_09==1)
replace aguared_ch=. if s8_09==.
label var aguared_ch "Acceso a fuente de agua por red"


****************
***aguadist_ch***
****************
/*
s8_10:
           1 por cañería dentro de la vivienda
           2 por cañeria fuera de la vivienda, pero dentro del lote o ter
           3 no se distribuye por cañería
           8 ns/nr


*/

gen aguadist_ch=1 if s8_10==1
replace aguadist_ch=2 if s8_10==2
replace aguadist_ch=3 if s8_10==3
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch

*****************
***aguamala_ch***
*****************

gen aguamala_ch=(s8_09==6 | s8_09==7)
replace aguamala_ch=. if s8_09==.
label var aguamala_ch "Agua unimproved según MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

************
***luz_ch***
************

gen luz_ch=(s8_15==1)
replace luz_ch =. if  s8_15== .
label var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch= (s8_18==5 | s8_18== 7)
replace combust_ch = . if s8_18==.
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

gen bano_ch= (s8_12==1)
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=(s8_13==1)
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if s8_14==1 | s8_14==2
replace des1_ch=2 if s8_14==3
replace des1_ch=3 if s8_14==4
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if s8_14==1 | s8_14==2 | s8_14==3 
replace des2_ch=3 if s8_14==4
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

gen piso_ch=0 if  s8_08==1 
replace piso_ch=1 if  s8_08>=2 &  s8_08<=7 
replace piso_ch=2 if  s8_08==8
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes"
label val piso_ch piso_ch


**************
***pared_ch***
**************

gen pared_ch=0 if s8_05 ==6
replace pared_ch=1 if s8_05==1 | s8_05==2 | s8_05==3 | s8_05==4 | s8_05==5
replace pared_ch=2 if s8_05==7
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch


**************
***techo_ch***
**************

gen techo_ch=0 if s8_07==4
replace techo_ch=1 if s8_07>=1 & s8_07<=3
replace techo_ch=2 if s8_07==5
label var techo_ch "Materiales de construcción del techo"


**************
***resid_ch***
**************

gen resid_ch =0    if s8_31==6
replace resid_ch=1 if s8_31==4 | s8_31==2
replace resid_ch=2 if s8_31==1 | s8_31==3
replace resid_ch=3 if s8_31==5
replace resid_ch=. if s8_31==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen  aguamejorada_ch = 1 if (s8_09 >= 1 &  s8_09 <=2) | (s8_09 >= 4 &  s8_09 <=5)
replace aguamejorada_ch = 0 if  s8_09 == 3 | (s8_09 >= 6 &  s8_09 <=8)
		
		
*********************
***banomejorado_ch***
*********************
gen   banomejorado_ch = 1 if (s8_12 == 1 & (s8_14 >= 1 & s8_14 <=3) & s8_13== 1)
replace banomejorado_ch = 0 if (s8_12 == 1 & (s8_14 >= 1 & s8_14 <=3) & s8_13 == 2) | s8_12 ==2 | s8_14 ==5 | (s8_12  == 1  & s8_14 ==4)
		

*************
***dorm_ch***
*************

gen dorm_ch= s8_21
recode dorm_ch (0=1)
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=s8_20
label var cuartos_ch "Habitaciones en el hogar"


***************
***cocina_ch***
***************

gen cocina_ch=(s8_17==1)
replace cocina_ch = . if  s8_17==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=(s8_24==1)
replace telef_ch = . if s8_24==.
label var telef_ch "El hogar tiene servicio telefónico fijo"


********
***NA***
********
*La información para estas variables está en el cuestionario, pero no en la base


gen refrig_ch=.
label var refrig_ch "El hogar posee refrigerador o heladera"

gen freez_ch=.
label var freez_ch "El hogar posee congelador"

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"

gen compu_ch=.
label var compu_ch "El hogar posee computador"


gen internet_ch = .   
label var internet_ch "El hogar posee conexión a Internet"



************
***cel_ch***
************

gen cel_ch= (s8_26==1)
replace cel_ch = .   if  s8_26== .
label var cel_ch "El hogar tiene servicio telefonico celular"


**************
***vivi1_ch***
**************

gen vivi1_ch=1 if s8_01==1
replace vivi1_ch=2 if s8_01==2
replace vivi1_ch=3 if s8_01==3 | s8_01==4 
replace vivi1_ch=. if s8_01==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


**************
***vivi2_ch***
**************

gen vivi2_ch=0
replace vivi2_ch=1 if s8_01==1 | s8_01==2
replace vivi2_ch=. if s8_01==.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************

gen viviprop_ch=0 	if s8_02==1
replace viviprop_ch=1 	if s8_02==2
replace viviprop_ch=3 	if s8_02==3 | s8_02==4 | s8_02==5 | s8_02==6
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia" 
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch


****************
***vivitit_ch***
****************

gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch= s8_03a if viviprop_ch==0
replace vivialq_ch=vivialq_ch*8.1 if  s8_03b=="B"
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch= s8_04a
replace vivialqimp_ch=vivialqimp_ch*8.1 if s8_04b=="B"
label var vivialqimp_ch "Alquiler mensual imputado"

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(s2_01==3) if s2_01!=. 	
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & inlist(s2_02a,1,2)) if s2_02a!=4 & migrante_ci!=.
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
	
	gen migrantiguo5_ci = 1 if inlist(s2_02a,1,2) & migrante_ci==1 
	replace migrantiguo5_ci = 0 if s2_02a == 3 & migrante_ci==1 
	replace migrantiguo5_ci = . if s2_02a == 4 | migrante_ci!=1 
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
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 */
rename s5_16cod codocupa
rename s5_17cod codindustria

compress


saveold "`base_out'", replace


log close



