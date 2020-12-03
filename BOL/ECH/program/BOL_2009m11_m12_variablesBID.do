
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

local PAIS BOL
local ENCUESTA ECH
local ANO "2009"
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
Última versión: Melany Gualavisi  melanyg@iadb.org
Fecha última modificación: 4/11/2014 (correccion de ingresos)

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
*YL: generacion "region_c" para los años 2009 y +. Para proyecto maps America.	
tostring folio, replace
gen region_c=real(substr(folio,1,1))

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
label value region_c region_c
label var region_c "division politica, estados"

***************
***factor_ch***
***************
gen factor_ch=.
replace factor_ch = factor
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
***************
*sort folio
*tostring folio,g(temp)
gen idh_ch = folio
label variable idh_ch "ID del hogar"
*drop temp

**************
****idp_ci****
**************
gen idp_ci= nroper
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
/*
gen byte zona_c=1
label variable zona_c "Zona del pais"
*/
*Modificado Mayra Sáenz Julio 2013. Se estaba asignando 1 a todas las observaciones gen byte zona_c=1, lo cual 
*es incorrecto porque la encuesta también abarca la zona rural.

gen byte zona_c=0 	if urb_rur==2
replace zona_c=1 	if urb_rur==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

label variable zona_c "Zona del pais"

************
****pais****
************
gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2009
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
replace relacion_ci=1 if  s1_08==1
replace relacion_ci=2 if  s1_08==2
replace relacion_ci=3 if  s1_08==3
replace relacion_ci=4 if  s1_08>=4 &  s1_08<=9
replace relacion_ci=5 if  s1_08==10 |  s1_08==12 
replace relacion_ci=6 if  s1_08==11

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
gen sexo_ci = s1_03
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci= s1_04
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************
gen civil_ci=.
replace civil_ci=1 		if s1_13==1
replace civil_ci=2 		if s1_13==2 | s1_13==3
replace civil_ci=3 		if s1_13==4 | s1_13==5
replace civil_ci=4 		if s1_13==6
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

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*
gen raza_ci=.
replace raza_ci= 1 if  (s1_14 >=1 & s1_14 <=6)
replace raza_ci= 1 if (s1_10==2 | s1_10==3 | s1_10== 4 |s1_10== 5) & raza_ci==.
replace raza_ci= 3 if (s1_14  ==7) 
replace raza_ci= 3 if (s1_10==1 | s1_10==6 | s1_10== 7 | s1_10== 8)& raza_ci==.
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"
*/

*Raza usando idioma
gen raza_idioma_ci = .
replace raza_idioma_ci= 1 if (s1_10==2 | s1_10==3 | s1_10== 4 | s1_10== 5) & raza_idioma_ci==.
replace raza_idioma_ci= 3 if (s1_10==1 | s1_10== 6) & raza_idioma_ci==.
bys idh_ch, sort: gen aux=raza_idioma_ci if s1_08==1
bys idh_ch, sort: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (s1_08 ==3 | s1_08==8))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

*Raza usando la definicion mas apropiada
gen raza_ci=.
replace raza_ci= 1 if  (s1_14 >=1 & s1_14<=6)
replace raza_ci= 3 if (s1_14 ==7) 
bys idh_ch: gen aux=raza_ci if s1_08==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & (s1_08 ==3|s1_08==8))  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años. Sin embargo, las variables construidas 
por el centro de estadística tienen en cuenta a la población con 10 años o más. Esto no es un problema dado que el 
programa para generar los indicadores de sociómetro restrige  todo a 15 o más años para que haya comparabilidad entre
países
*/
 
/************************************************************************************************************
* Líneas de pobreza oficiales
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci=z
/*
replace lp_ci= 656.6 if secmu=="CAPITAL (Sucre)"
replace lp_ci= 569.7 if secmu=="CAPITAL (La Paz)"
replace lp_ci= 664.6 if secmu=="PRIMERA SECCION (Cochabamba)"
replace lp_ci= 480.2 if secmu=="CAPITAL (Oruro)"
replace lp_ci= 513.9 if secmu=="CAPITAL (Potosi)"
replace lp_ci= 757.3 if secmu=="PRIMERA SECCIÓN (Tarija)"
replace lp_ci= 681.6 if secmu=="CAPITAL (Santa Cruz de la Sierra)"
replace lp_ci= 576.8 if secmu=="CAPITAL (Trinidad)"
replace lp_ci= 456.1 if secmu=="CUARTA SECCIION (El Alto)"
replace lp_ci= 699.3 if secmu=="CAPITAL (Cobija)"
replace lp_ci=  423.79 if zona_c==0
*/



label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci=zext
/*
replace lpe_ci= 331.6 if secmu=="CAPITAL (Sucre)"
replace lpe_ci= 316.8 if secmu=="CAPITAL (La Paz)"
replace lpe_ci= 333.7 if secmu=="PRIMERA SECCION (Cochabamba)"
replace lpe_ci= 267   if secmu=="CAPITAL (Oruro)"
replace lpe_ci= 285.7 if secmu=="CAPITAL (Potosi)"
replace lpe_ci= 382.4 if secmu=="PRIMERA SECCIÓN (Tarija)"
replace lpe_ci= 346.2 if secmu=="CAPITAL (Santa Cruz de la Sierra)"
replace lpe_ci= 293   if secmu=="CAPITAL (Trinidad)"
replace lpe_ci= 276.9 if secmu=="CUARTA SECCIION (El Alto)"
replace lpe_ci= 355.2 if secmu=="CAPITAL (Cobija)"
replace lpe_ci= 241.56 if zona_c==0
*/


label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 2009
gen salmm_ci= 	647.00

label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci= s5_58b==1	
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
*replace condocup_ci=1 if s5_01==1 | s5_02<=6  | s5_03==1
replace condocup_ci=1 if s5_01==1 | s5_02<=6 | (s5_03>=1 & s5_03<=7)
*replace condocup_ci=2 if (s5_01==2 | s5_02==7 | s5_03>1) & (s5_05==1) & (s5_04==1)
replace condocup_ci=2 if (s5_01==2 | s5_02==7 | s5_03>7) & (s5_05==1) & (s5_04==1)
*2015,10 MLO la encuesta pregunta a partir de 7 años (no 10)

recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
*Mayra Sáenz Octubre 2013 - Se cambia la s5_57 por la s5_07 que corresponde a la sección de condición de actividad.

gen cesante_ci=1 if s5_07==1 & condocup_ci==2
*2014,03 Modificacion MLO en siguiente linea
replace cesante_ci=0 if s5_07==2 & condocup_ci==2
*replace cesante_ci=0 if s5_07==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if s5_27>=1 & s5_27<=5
replace tamemp_ci=2 if s5_27>=6 & s5_27<=49
replace tamemp_ci=3 if s5_27>49 & s5_27!=.
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

gen aux_ps= s6_01eb if s6_01eb>1 & s6_01eb!=. 
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
gen desalent_ci=(emp_ci==0 & (s5_15==3 | s5_15==4))
replace desalent_ci=. if emp_ci==.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
/*Días a la semana s5_29a
*Horas diarias s5_29b 
*gen horassem =  s5_29b* s5_29a
*gen horaspri_ci= horassem*4.3

gen horaspri_ci=s5_29b* s5_29a
replace horaspri_ci=. if s5_29b==. | s5_29a==.
replace horaspri_ci=. if emp_ci~=1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"
*/

/*Correcion recomendad por LCM y generada por YL, Enero 2014
*dias a la semana =s5_29
*horas que trabaja en el dia=s5_29a
*minutos (se deben sumar a las horas) = s5_29b*/

gen hminutos= s5_29b/60
egen horas =rsum(hminutos s5_29a), missing
replace horas=. if hminutos==. & s5_29a==.

gen horaspri_ci=s5_29*horas
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************

gen horassec_ci= s5_46a*s5_46b1
replace horassec_ci=. if s5_46a==. | s5_46b1==.
replace horassec_ci=. if emp_ci~=1

egen horastot_ci= rsum(horaspri_ci horassec_ci), missing
replace horastot_ci = . if horaspri_ci == . & horassec_ci == .
replace horassec_ci=. if emp_ci~=1

***************
***subemp_ci***
***************
/*
gen subemp_ci=.
replace subemp_ci=1 if s5_52== 1 & horastot_ci <= 30
replace subemp_ci=0 if s5_52== 2 & emp_ci == 1
replace subemp_ci=. if s5_52==. | horastot_ci==.
label var subemp_ci "Personas en subempleo por horas"
*/
* Se considera subempleo visible: quiere trabajar mas horas y esta disponible. MGD 06/18/2014
gen subemp_ci=0
*replace subemp_ci=1 if s5_53==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (s5_53==1 & s5_52==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"


*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
*replace tiempoparc_ci=1 if s5_52==2 & horastot_ci<=30 & emp_ci == 1
*replace tiempoparc_ci=0 if s5_52==2 & emp_ci == 1 & horastot_ci>30

*Mod. MLO 2015, 10
replace tiempoparc_ci=(s5_53==2 & horaspri_ci<30 & emp_ci == 1)
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
replace categosec_ci=1 if s5_43>=4 & s5_43<=6
replace categosec_ci=2 if s5_43==3
replace categosec_ci=3 if s5_43==1 | s5_43==2 | s5_43==8
replace categosec_ci=4 if s5_43==7
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if s5_28==3 & categopri_ci==3
replace tipocontrato_ci=2 if s5_28==1 & categopri_ci==3
replace tipocontrato_ci=3 if ((s5_28==2 | s5_28==4) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s5_40==1
label var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
replace firmapeq_ci=1 if  s5_27>=1 & s5_27<=5 
replace firmapeq_ci=0 if  s5_27>=6 & s5_27!=.
label var firmapeq_ci "Trabajadores informales"
*/ 

*****************
***spublico_ci***
*****************
gen spublico_ci=.
replace spublico_ci=1 if s5_22==1
replace spublico_ci=0 if s5_22==2 | s5_22==3
replace spublico_ci=. if emp_ci~=1
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
*cobp:
*NA: No se puede estandarizar ya que no se distingue entre dos categorias:
*comerciantes y vendedores y trabajadores en servicios 

* Modificacion MGD 07/24/2014: clasificacion CIUO -88
g aux = substr(cod_ocp,1,3)
destring aux, replace
gen ocupa_ci=.
replace ocupa_ci=1 if ((aux>=210 & aux<=348) | (aux>=21 & aux<=34)) & emp_ci==1
replace ocupa_ci=2 if ((aux>=110 & aux<=131) |  aux==11) & emp_ci==1
replace ocupa_ci=3 if ((aux>=410 & aux<=422) |  aux==41 |  aux==42) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911) | aux==52 | aux==91) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=621) | (aux>=920 & aux<=921)) & emp_ci==1
replace ocupa_ci=7 if ((aux>=710 & aux<=834) | (aux>=930 & aux<=933) | aux==71 | aux==81 | aux==83)& emp_ci==1
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
act_prin:
       AGRICUL_GANADERIA_CAZA_SILVICULT 0
                                  PESCA 1
       EXPLOTACION_DE_MINAS_Y_CANTERIAS 2
                INDUSTRIA_MANUFACTURERA 3
          SUMINISTRO_DE_ELEC_GAS_Y_AGUA 4
                           CONSTRUCCION 5
COMERC_POR_MAY,MENOR,REP_DE_VEHI,MOTO,_ 6
                 HOTELES_Y_RESTAURANTES 7
    TRANSPORTE,ALMACENAMIENTO_Y_COMUNIC 8
              INTERMEDIACION_FINANCIERA 9
ACTIVIDADES_INMOBILIARIAS,EMPRESARIALES 10
  ADM_PUB,DEFENSA_Y_SEG_SOC_OBLIGATORIA 11
                              EDUCACION 12
                    SERV_SOC_Y_DE SALUD 13
SERV_COMUNITARIOS,SOCIALES_Y_PERSONALES 14
ACTIV_DE_HOG_PRIV_Y_ACTIV_NO DIF_DE_ HO 15
   SERV_DE_ORG_Y_ÓRG_EXTRATERRITORIALES 16
                                  Ns/Nr 99
*/
* Indicador corregido: la especificacion de rama en la variable caeb_ag inicia en 0 y no en 1. MGD 04/07/2014

gen rama_ci=.
replace rama_ci=1 if (caeb_op>=0 & caeb_op<=1) & emp_ci==1
replace rama_ci=2 if caeb_op==2 & emp_ci==1
replace rama_ci=3 if caeb_op==3 & emp_ci==1
replace rama_ci=4 if caeb_op==4 & emp_ci==1
replace rama_ci=5 if caeb_op==5 & emp_ci==1
replace rama_ci=6 if (caeb_op>=6 & caeb_op<=7) & emp_ci==1 
replace rama_ci=7 if caeb_op==8 & emp_ci==1
replace rama_ci=8 if (caeb_op>=9 & caeb_op<=10) & emp_ci==1
replace rama_ci=9 if (caeb_op>=11 & caeb_op<=16) & emp_ci==1
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
gen antiguedad_ci=.
replace antiguedad_ci=s5_19a/52 if s5_19b==2 & emp_ci==1
replace antiguedad_ci=s5_19a/12 if s5_19b==4 & emp_ci==1
replace antiguedad_ci=s5_19a	if s5_19b==8 & emp_ci==1
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (s5_14==3 & condocup_ci==3)
replace categoinac_ci = 2 if  ( s5_14==1 & condocup_ci==3)
replace categoinac_ci = 3 if  ( s5_14==2 & condocup_ci==3)
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
** Se corrigieron las correspondencias entre frecuencias y montos. MGD 04/10/2014

*************************
*********LABORAL*********
*************************
/*
s2_38f:
1  diario
2  semanal
3  quincenal
4  mensual
8  anual
*/

*******************
* salario liquido *
*******************
gen yliquido = .
replace yliquido= s5_31a*30		if s5_31b==1
replace yliquido= s5_31a*4.3	if s5_31b==2
replace yliquido= s5_31a*2		if s5_31b==3
replace yliquido= s5_31a		if s5_31b==4
replace yliquido= s5_31a/2		if s5_31b==5
replace yliquido= s5_31a/3		if s5_31b==6
replace yliquido= s5_31a/6		if s5_31b==7
replace yliquido= s5_31a/12		if s5_31b==8

**************
* comisiones *
**************
gen ycomisio = .
replace ycomisio= s5_33a1*30	if s5_33a2==1
replace ycomisio= s5_33a1*4.3	if s5_33a2==2
replace ycomisio= s5_33a1*2		if s5_33a2==3
replace ycomisio= s5_33a1		if s5_33a2==4
replace ycomisio= s5_33a1/2		if s5_33a2==5
replace ycomisio= s5_33a1/3		if s5_33a2==6
replace ycomisio= s5_33a1/6 	if s5_33a2==7
replace ycomisio= s5_33a1/12	if s5_33a2==8

****************
* horas extras *
****************
gen yhrsextr= .
replace yhrsextr= s5_33b1*30	if s5_33b2==1
replace yhrsextr= s5_33b1*4.3	if s5_33b2==2
replace yhrsextr= s5_33b1*2		if s5_33b2==3
replace yhrsextr= s5_33b1		if s5_33b2==4
replace yhrsextr= s5_33b1/2		if s5_33b2==5
replace yhrsextr= s5_33b1/3		if s5_33b2==6
replace yhrsextr= s5_33b1/6 	if s5_33b2==7
replace yhrsextr= s5_33b1/12	if s5_33b2==8

*********
* prima *
*********
*Esto tiene un periodo de referencia de los últimos 12 meses. Hay que revisarlo para los años anteriores 
gen yprima = .
replace yprima = s5_32a/12
replace yprima =. if  s5_32a==999999


*************
* aguinaldo *
*************
*Esto tiene un periodo de referencia de los últimos 12 meses. Hay que revisarlo para los años anteriores 
gen yaguina = .
replace yaguina = s5_32b/12
replace yaguina=. if   s5_32b==999999

*************
* alimentos *
*************
gen yalimen = .
replace yalimen= s5_36a3*30		if s5_36a2==1 & s5_36a1==1
replace yalimen= s5_36a3*4.3	if s5_36a2==2 & s5_36a1==1
replace yalimen= s5_36a3*2		if s5_36a2==3 & s5_36a1==1
replace yalimen= s5_36a3		if s5_36a2==4 & s5_36a1==1
replace yalimen= s5_36a3/2		if s5_36a2==5 & s5_36a1==1
replace yalimen= s5_36a3/3		if s5_36a2==6 & s5_36a1==1
replace yalimen= s5_36a3/6		if s5_36a2==7 & s5_36a1==1
replace yalimen= s5_36a3/12		if s5_36a2==8 & s5_36a1==1

**************
* transporte *
**************
gen ytranspo = .
replace ytranspo= s5_36b3*30	if s5_36b2==1 & s5_36b1==1
replace ytranspo= s5_36b3*4.3	if s5_36b2==2 & s5_36b1==1
replace ytranspo= s5_36b3*2		if s5_36b2==3 & s5_36b1==1
replace ytranspo= s5_36b3		if s5_36b2==4 & s5_36b1==1
replace ytranspo= s5_36b3/2		if s5_36b2==5 & s5_36b1==1
replace ytranspo= s5_36b3/3		if s5_36b2==6 & s5_36b1==1
replace ytranspo= s5_36b3/6	    if s5_36b2==7 & s5_36b1==1
replace ytranspo= s5_36b3/12	if s5_36b2==8 & s5_36b1==1

**************
* vestimenta *
**************
gen yvesti = .
replace yvesti= s5_36c3*30		if s5_36c2==1 & s5_36c1==1
replace yvesti= s5_36c3*4.3		if s5_36c2==2 & s5_36c1==1
replace yvesti= s5_36c3*2		if s5_36c2==3 & s5_36c1==1
replace yvesti= s5_36c3			if s5_36c2==4 & s5_36c1==1
replace yvesti= s5_36c3/2		if s5_36c2==5 & s5_36c1==1
replace yvesti= s5_36c3/3		if s5_36c2==6 & s5_36c1==1
replace yvesti= s5_36c3/6		if s5_36c2==7 & s5_36c1==1
replace yvesti= s5_36c3/12		if s5_36c2==8 & s5_36c1==1

************
* vivienda *
************
* No hay la cetegoria bimestral.
gen yvivien = .
replace yvivien= s5_36d3*30		if s5_36d2==1 & s5_36d1==1
replace yvivien= s5_36d3*4.3	if s5_36d2==2 & s5_36d1==1
replace yvivien= s5_36d3*2		if s5_36d2==3 & s5_36d1==1
replace yvivien= s5_36d3		if s5_36d2==4 & s5_36d1==1
replace yvivien= s5_36d3/3		if s5_36d2==6 & s5_36d1==1
replace yvivien= s5_36d3/6		if s5_36d2==7 & s5_36d1==1
replace yvivien= s5_36d3/12		if s5_36d2==8 & s5_36d1==1



*************
* otros *
*************
* No hay las categorias de bimestral y semestral(5 y 7) 
gen yotros = .
replace yotros= s5_36e3*30		if s5_36e2==1 & s5_36e1==1
replace yotros= s5_36e3*4.3		if s5_36e2==2 & s5_36e1==1
replace yotros= s5_36e3*2		if s5_36e2==3 & s5_36e1==1
replace yotros= s5_36e3			if s5_36e2==4 & s5_36e1==1
replace yotros= s5_36e3/3		if s5_36e2==6 & s5_36e1==1
replace yotros= s5_36e3/12		if s5_36e2==8 & s5_36e1==1

**********************************
* ingreso act. pr independientes *
**********************************
*Aquí se tiene en cuenta el monto de dinero que les queda a los independientes para el uso del hogar
gen yactpri = .
replace yactpri= s5_39a*30		if s5_39b==1
replace yactpri= s5_39a*4.3		if s5_39b==2
replace yactpri= s5_39a*2		if s5_39b==3
replace yactpri= s5_39a			if s5_39b==4
replace yactpri= s5_39a/2		if s5_39b==5
replace yactpri= s5_39a/3		if s5_39b==6
replace yactpri= s5_39a/6		if s5_39b==7
replace yactpri= s5_39a/12		if s5_39b==8

*********************
* salario liquido 2 *
*********************
* No hay las categorias de semestral ni anual (7 y 8).

gen yliquido2 = .
replace yliquido2= s5_48a*30	if s5_48b==1
replace yliquido2= s5_48a*4.3	if s5_48b==2
replace yliquido2= s5_48a*2		if s5_48b==3
replace yliquido2= s5_48a		if s5_48b==4
replace yliquido2= s5_48a/2		if s5_48b==5
replace yliquido2= s5_48a/3		if s5_48b==6

*****************
* Horas extra 2 *
*****************
*Para el empleo secundario se pregunta por los últimos 12 meses. Por eso se divide entre 12. Se debe 
*revisar esto para los años anteriores

gen yhrsextr2 = .
replace yhrsextr2=s5_49a2/12 if s5_49a1==1


***************************************
* alimentos, transporte y vestimenta2 *
***************************************
*Para el empleo secundario se pregunta por los últimos 12 meses. Por eso se divide entre 12. Se debe 
*revisar esto para los años anteriores

gen yalimen2 = .
replace yalimen2= s5_49b2/12	if s5_49b1==1


**************
* vivienda 2 *
**************

gen yvivien2= .
replace yvivien2= s5_49c2/12	if s5_49c1==1


*************************
******NO-LABORAL*********
*************************

*************
* intereses *
*************
gen yinteres = .
replace yinteres = s6_02a	

**************
* alquileres *
**************

gen yalqui = .
replace yalqui = s6_02b		


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
replace yotren = s6_02c		


************************
* alquileres agricolas *
************************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yalqagri = .
replace yalqagri =  s6_03a/12		


**************
* dividendos *
**************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen ydivi = .
replace ydivi =  s6_03b/12


*************************
* alquileres maquinaria *
*************************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yalqmaqui = .
replace yalqmaqui = s6_03c/12

 
******************
* indem. trabajo *
******************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yindtr = .
replace yindtr =  s6_04a/12


******************
* indem. seguros *
******************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yindseg = .
replace yindseg = s6_04b/12


***********
* bonosol *
***********
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen ybono = .
*2014,02 modificado por MLO
* como no esta la variable frecuencia (s6_01ef) en la base supongo que es mensual. este año ya se pagaba en forma mensual
replace ybono = s6_01eb
*replace ybono = s6_01ea/12


******************
* otros ingresos *
******************
*Esto se pregunta anual, por ello se divide entre doce.  Se deben revisar los programas de los años anteriores.
gen yotring = .
replace yotring = s6_04c/12



*******************
* asist. familiar *
*******************
* No hay la categoria de diario.

gen yasistfam = .
replace yasistfam= s6_05a1*4.3		if s6_05a2==2
replace yasistfam= s6_05a1*2		if s6_05a2==3
replace yasistfam= s6_05a1			if s6_05a2==4
replace yasistfam= s6_05a1/2		if s6_05a2==5
replace yasistfam= s6_05a1/3		if s6_05a2==6
replace yasistfam= s6_05a1/6		if s6_05a2==7
replace yasistfam= s6_05a1/12		if s6_05a2==8


*********************
* Trans. monetarias *
*********************
************************
* No hay la categoria de diario.

gen ytransmon = .
replace ytransmon= s6_05b1*4.3		if s6_05b2==2
replace ytransmon= s6_05b1*2		if s6_05b2==3
replace ytransmon= s6_05b1			if s6_05b2==4
replace ytransmon= s6_05b1/2		if s6_05b2==5
replace ytransmon= s6_05b1/3		if s6_05b2==6
replace ytransmon= s6_05b1/6		if s6_05b2==7
replace ytransmon= s6_05b1/12		if s6_05b2==8

***********
* remesas *
***********
*http://www.bcb.gob.bo/librerias/indicadores/otras/otras_imprimir2.php?qdd=3&qmm=11&qaa=2009
gen s6_112= .
replace s6_112 = s6_11 				if s6_11a==1
replace s6_112 = s6_11*10.25		if s6_11a==2 /*euro*/
replace s6_112 = s6_11*7.07			if s6_11a==3 /*dolar*/
replace s6_112 = s6_11*1.824		if s6_11a==4 /*peso argentino*/
replace s6_112 = s6_11*3.9566		if s6_11a==5 /*real*/
replace s6_112 = s6_11*.01308		if s6_11a==6 /*peso chileno*/

* No hay categoria diaria
gen yremesas = .
replace yremesas= s6_112*4.3	if s6_08==2
replace yremesas= s6_112*2		if s6_08==3
replace yremesas= s6_112		if s6_08==4
replace yremesas= s6_112/2		if s6_08==5
replace yremesas= s6_112/3		if s6_08==6
replace yremesas= s6_112/6		if s6_08==7
replace yremesas= s6_112/12		if s6_08==8

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

egen ylnmprid=rsum(yalimen ytranspo yvesti yvivien yotros), missing
replace ylnmprid=. if yalimen==. & ytranspo==. & yvesti==. & yvivien==. & yotros==.   
replace ylnmprid=0 if categopri_ci==4
replace ylnmprid=. if  ylnmprid>=3000000
*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingrpeso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

egen ylmsec_ci= rsum(yliquido2 yhrsextr2), missing
replace ylmsec_ci=. if emp_ci~=1 & yhrsextr2==. & yliquido2 ==.
replace ylmsec_ci=0 if categosec_ci==4
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


******************
****ylnmsec_ci****
******************

egen ylnmsec_ci=rsum(yalimen2  yvivien2), missing
replace ylnmsec_ci=. if yalimen2==.  & yvivien2==.  
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


*****************
***remesas_ci***
*****************

gen remesas_ci=yremesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 



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

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
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
gen rentaimp_ch= .
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
s4_03a:
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

gen byte aedu_ci=.

replace aedu_ci=0 if s4_03a==11 | s4_03a==12 | s4_03a==13

replace aedu_ci=1 if (s4_03a==14 | s4_03a==17) & s4_03b==1
replace aedu_ci=2 if (s4_03a==14 | s4_03a==17) & s4_03b==2
replace aedu_ci=3 if (s4_03a==14 | s4_03a==17) & s4_03b==3
replace aedu_ci=4 if (s4_03a==14 | s4_03a==17) & s4_03b==4
replace aedu_ci=5 if (s4_03a==14 | s4_03a==17) & s4_03b==5

replace aedu_ci=6 if (s4_03a==17 & s4_03b==6) | (s4_03a==15 & s4_03b==1)
replace aedu_ci=7 if (s4_03a==17 & s4_03b==7) | (s4_03a==15 & s4_03b==2)
replace aedu_ci=8 if (s4_03a==17 & s4_03b==8) | (s4_03a==15 & s4_03b==3)

replace aedu_ci=9 if (s4_03a==16 | s4_03a==18) & s4_03b==1
replace aedu_ci=10 if (s4_03a==16 | s4_03a==18) & s4_03b==2
replace aedu_ci=11 if (s4_03a==16 | s4_03a==18) & s4_03b==3
replace aedu_ci=12 if (s4_03a==16 | s4_03a==18) & s4_03b==4

replace aedu_ci=13 if (s4_03a>=24 & s4_03a<=30 & s4_03a~=27) & s4_03b==1
replace aedu_ci=14 if (s4_03a>=24 & s4_03a<=30 & s4_03a~=27) & s4_03b==2
replace aedu_ci=15 if (s4_03a>=24 & s4_03a<=30 & s4_03a~=27) & s4_03b==3
replace aedu_ci=16 if (s4_03a>=24 & s4_03a<=30 & s4_03a~=27) & s4_03b==4
replace aedu_ci=16 if (s4_03a==24 | s4_03a==28 | s4_03a==29) & s4_03b==5
replace aedu_ci=17 if (s4_03a==25 | s4_03a==26 | s4_03a==30) & s4_03b==5


replace aedu_ci=16 if (s4_03a==24 | s4_03a==28 | s4_03a==29) & s4_03b==8
replace aedu_ci=17 if (s4_03a==25 | s4_03a==26 | s4_03a==30) & s4_03b==8

replace aedu_ci=18 if s4_03a==27 & s4_03b==1
replace aedu_ci=19 if s4_03a==27 & s4_03b==2

label var aedu_ci "Anios de educacion aprobados" 
*/

/*
 0 A~NOS DE EDUCACIÓN
          11 ninguno
          12 curso de alfabetización
          13 educación pre-escolar
		  35 otros cursos

PRIMARIA 		  
          14 básico (1 a 5 años)
          15 intermedio (1 a 3 años)
          17 primaria (1 a 8 años)
		  
		  
SECUNDARIA		  
		  16 medio (1 a 4 años)
          18 secundaria (1 a 4 años)
		  

EDUCACION SUPERIOR		  
          26 normal
          27 universidad pública
          28 universidad privada
          29 postgrado diplomado
          30 postgrado maestría
          31 postgrado doctorado
          32 técnico de universidad
          33 técnico de instituto
          34 institutos de formación militar y policial
          
 
 *No se consideran por no ser educación formal sino "Alternativa" o "No formal"
          19 educación básica de adultos (eba)
          20 centro de educación media de adultos (cema)
          21 educación juvenil alternativa (eja)
          22 educación primaria de adultos (epa)
          23 educación secundaria de adultos (esa)
          24 educación técnica de adultos (eta)
          25 educación especial
 
*/

* Modificaciones Marcela Rubio Septiembre 2014

gen aedu_ci = .
* Primaria y Secundaria
replace aedu_ci = 0 if s4_03a==11 | s4_03a==12 | s4_03a==13
replace aedu_ci = 1 if (s4_03a==14 | s4_03a==17) & s4_03b==1
replace aedu_ci = 2 if (s4_03a==14 | s4_03a==17) & s4_03b==2
replace aedu_ci = 3 if (s4_03a==14 | s4_03a==17) & s4_03b==3
replace aedu_ci = 4 if (s4_03a==14 | s4_03a==17) & s4_03b==4
replace aedu_ci = 5 if (s4_03a==14 | s4_03a==17) & s4_03b==5
replace aedu_ci = 6 if (s4_03a==17 & s4_03b==6) | (s4_03a==15 & s4_03b==1) 
replace aedu_ci = 7 if (s4_03a==17 & s4_03b==7) | (s4_03a==15 & s4_03b==2) 
replace aedu_ci = 8 if (s4_03a==17 & s4_03b==8) | (s4_03a==15 & s4_03b==3) 
replace aedu_ci = 9 if (s4_03a==16 | s4_03a==18) & s4_03b==1
replace aedu_ci = 10 if (s4_03a==16 | s4_03a==18) & s4_03b==2
replace aedu_ci = 11 if (s4_03a==16 | s4_03a==18) & s4_03b==3
replace aedu_ci = 12 if (s4_03a==16 | s4_03a==18) & s4_03b==4
* Superior
replace aedu_ci = 13 if s4_03b==1 & (s4_03a==26 | s4_03a==27 | s4_03a==28 | s4_03a==32 | s4_03a==33 | s4_03a==34 ) 
replace aedu_ci = 14 if s4_03b==2 & (s4_03a==26 | s4_03a==27 | s4_03a==28 | s4_03a==32 | s4_03a==33 | s4_03a==34 ) 
replace aedu_ci = 15 if s4_03b==3 & (s4_03a==26 | s4_03a==27 | s4_03a==28 | s4_03a==32 | s4_03a==33 | s4_03a==34 ) 
replace aedu_ci = 16 if s4_03b==4 & (s4_03a==26 | s4_03a==27 | s4_03a==28 | s4_03a==32 | s4_03a==33 | s4_03a==34 ) 
replace aedu_ci = 17 if (s4_03b>=5 & s4_03b<=8 ) & (s4_03a==26 | s4_03a==27 | s4_03a==28 | s4_03a==32 | s4_03a==33 | s4_03a==34 ) 
*Postgrado
replace aedu_ci = 18 if s4_03b==1 & (s4_03a==29 | s4_03a==30 | s4_03a==31 ) 
replace aedu_ci = 19 if s4_03b==2 & (s4_03a==29 | s4_03a==30 | s4_03a==31 ) 
replace aedu_ci = 20 if s4_03b==3 & (s4_03a==29 | s4_03a==30 | s4_03a==31 )
replace aedu_ci = 21 if (s4_03b>=4 & s4_03b>=8) & (s4_03a==29 | s4_03a==30)
replace aedu_ci = 21 if (s4_03b==4  & s4_03a==31)
replace aedu_ci = 22 if (s4_03b==5  & s4_03a==31)
replace aedu_ci = 22 if (s4_03b==8  & s4_03a==31)


**************
***eduno_ci***
**************

gen byte eduno_ci=(s4_03a==11 | s4_03a==12 | s4_03a==13) 
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

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & s4_03b<8)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & s4_03b==8) | (aedu_ci>=17 & aedu_ci<.)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"

***************
***edupre_ci***
***************

gen byte edupre_ci=(s4_03a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
*Variable añadida por Iván Bornacelly - 01/12/2017

	g asispre_ci=.	
	replace asispre_ci=1 if s4_05==1 & s4_06a==13
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"
	
**************
***eduac_ci***
**************
* Cambia a terciaria académica a los insitutos militares o policiales. LCM dic-2013

gen byte eduac_ci=.
replace eduac_ci=1 if (s4_03a>=27 & s4_03a<=32 | s4_03a==34)
replace eduac_ci=0 if (s4_03a==26 | s4_03a>=33 )
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

*gen asiste_ci=(s4_1 2==1)
* LCM (introducido por YL):  Se cambia la forma de cálculo porque se deben considerar los rangos de edad lcm dic2013
*Modificación Mayra Sáenz Enero-2017: Se genera la dummy de acuerdo al documento metodológico.
gen asiste_ci= s4_05==1
/*
gen asiste_ci= 1 if s4_05==1
replace asiste_ci = 0 if s4_05==2*/
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

*gen pqnoasis_ci=s4_11

*Modificado Mayra Sáenz, Junio 2016
gen pqnoasis_ci=s4_13
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"vacación/receso" 2"falta de dinero" 3"por trabajo" 4"por enfermedad/accidente/discapacidad"
label def pqnoasis_ci 5"los establecimientos son distantes" 6"culminó sus estudios" 7"edad temprana/ edad avanzada", add
label def pqnoasis_ci 8"falta de interés" 9"labores de casa/ embarazo/cuidado de niños/as" 10"otra", add
label val pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci = 1 if s4_13==2
replace pqnoasis1_ci = 2 if s4_13==3
replace pqnoasis1_ci = 3 if s4_13==4 
replace pqnoasis1_ci = 4 if s4_13==8
replace pqnoasis1_ci = 5 if s4_13==9 
replace pqnoasis1_ci = 6 if s4_13==6 
replace pqnoasis1_ci = 7 if s4_13==7  
replace pqnoasis1_ci = 8 if s4_13==5
replace pqnoasis1_ci = 9 if s4_13==1  | s4_13==10

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

gen repiteult_ci=(s4_11 ==1)
replace repiteult_ci=. if s4_09==.
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


gen edupub_ci=(s4_07==2 | s4_07==3)
replace edupub_ci=. if s4_07==.
label var edupub_ci "Asiste a un centro de ensenanza público"

**************
***tecnica_ci*
**************
/*
gen tecnica_ci=.
replace tecnica_ci=1 if s4_03a==28 | s4_03a==29
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"
*/
*Modificación Mayra Sáenz - Octubre 2013
gen tecnica_ci=.
replace tecnica_ci=1 if s4_03a==32 | s4_03a==33 
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
*1  por cañeria dentro de la vivienda
*2  por cañería fuera de la vivienda, pero dentro del lote o ter
*3  por cañeria fuera del lote o terreno
*4  no se distribuye por cañeria

gen aguared_ch=(s8_10==1 | s8_10==2 | s8_10==3)
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
***aguared_ch***
****************


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

gen combust_ch= (s8_21==5 | s8_21== 7)
replace combust_ch = . if s8_21==.
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
*Hay que crear esta variable hacia atrás pues sí está disponible

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
*Revisar si esta variable se puede construir hacia atrás

gen resid_ch =0    if s8_17  ==6
replace resid_ch=1 if s8_17  ==4 | s8_17  ==2
replace resid_ch=2 if s8_17  ==1 | s8_17  ==3
replace resid_ch=3 if s8_17  ==5
replace resid_ch=. if s8_17  ==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if (s8_09 >= 1 &  s8_09 <=2) | (s8_09 >= 4 &  s8_09 <=5)
replace aguamejorada_ch = 0 if  s8_09 ==3 | (s8_09 >= 6 &  s8_09 <=8)
		
		
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if (s8_12 == 1 & (s8_14 >= 1 & s8_14 <=3) & s8_13== 1)
replace banomejorado_ch = 0 if (s8_12 == 1 & (s8_14 >= 1 & s8_14 <=3) & s8_13 == 2) | s8_12 ==2  | (s8_12  == 1  & s8_14 ==4)
	

*************
***dorm_ch***
*************

gen dorm_ch= s8_24 
recode dorm_ch (0=1)
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=s8_23 
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************

gen cocina_ch=(s8_20==1)
replace cocina_ch = . if  s8_20==.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=(s8_30==1)
replace telef_ch = . if s8_30==.
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

gen compu_ch=(s8_29==1)
replace compu_ch = .   if  s8_29 == .
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************
gen internet_ch=(s8_35==1)
replace internet_ch = .   if  s8_35 == .
label var internet_ch "El hogar posee conexión a Internet"



************
***cel_ch***
************

gen cel_ch= (s8_32==1)
replace cel_ch = .   if  s8_32== .
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


*************
***vivi2_ch***
*************

gen vivi2_ch=0
replace vivi2_ch=1 if s8_01==1 | s8_01==2
replace vivi2_ch=. if s8_01==.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************

*Se crea una variable parecida, pero con otro nombre

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

gen vivialq_ch= s8_03
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=s8_04
label var vivialqimp_ch "Alquiler mensual imputado"

*****************************
*** VARIABLES DE GDI *********
******************************
	
	/***************************
     * DISCAPACIDAD
    ***************************/
gen dis_ci==. 
lab def dis_ci 1 1 "Con Discapacidad" 0 "Sin Discapacidad"
lab val dis_ci dis_ci
label var dis_ci "Personas con discapacidad"


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
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) 
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 */
rename cod_ocp codocupa
rename cod_acp codindustria

compress


saveold "`base_out'", replace


log close


