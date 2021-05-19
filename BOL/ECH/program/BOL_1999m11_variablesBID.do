
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
* global ruta = "${surveysFolder}"

local PAIS BOL
local ENCUESTA ECH
local ANO "1999"
local ronda m11 


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
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 4 de Octubre de 2013

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

gen factor_ch=factorh
label variable factor_ch "Factor de expansion del hogar"


**************
****idh_ch****
**************
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=nrodeper
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=1 if zona==1
replace zona_c=0 if zona==2

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

gen anio_c=1999
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
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=8
replace relacion_ci=5 if parentco==10 | parentco==11
replace relacion_ci=6 if parentco==9

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

gen factor_ci=factorp
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=sexo

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estcivil==6
replace civil_ci=2 if estcivil==1 | estcivil==2
replace civil_ci=3 if estcivil==4 | estcivil==5
replace civil_ci=4 if estcivil==3

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
replace raza_ci= 1 if  (grupo ==1 | grupo ==2 | grupo ==4 | grupo ==5 | grupo ==6)
replace raza_ci= 1 if (idiapren==1 | idiapren==2 | idiapren== 4 | idiapren== 5) & raza_ci==.
replace raza_ci= 2 if  grupo ==7 
replace raza_ci= 3 if (grupo ==3 | grupo ==8) 
replace raza_ci= 3 if (idiapren==3 | idiapren==6 | idiapren== 7)& raza_ci==.
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
replace raza_idioma_ci= 1 if (idiapren==1 | idiapren==2 | idiapren== 4 | idiapren== 5) & raza_idioma_ci==.
replace raza_idioma_ci= 3 if (idiapren==3 | idiapren==6)& raza_idioma_ci==.
bys idh_ch, sort: gen aux=raza_idioma_ci if parentco==1
bys idh_ch, sort: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (parentco ==3|parentco==5))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

*Raza usando la definicion mas apropiada
gen raza_ci=.
replace raza_ci= 1 if (grupo ==1 | grupo ==2 | grupo ==4 | grupo ==5 | grupo ==6)
replace raza_ci= 2 if grupo ==7 
replace raza_ci= 3 if grupo ==8 
bys idh_ch, sort: gen aux=raza_ci if parentco==1
bys idh_ch, sort: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & (parentco ==3|parentco==5))  
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
label var id_ind_ci "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 
	************************************
	*** VARIABLES DEL MERCADO LABORAL***
	************************************
	
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
gen lp_ci =.
/*
replace lp_ci= 335.420044972232 if ciudad1==1 & urb_rur==1
replace lp_ci= 324.031424822892 if ciudad1==2 & urb_rur==1
replace lp_ci= 351.103517811812 if ciudad1==3 & urb_rur==1
replace lp_ci= 294.654238786948 if ciudad1==4 & urb_rur==1
replace lp_ci= 271.000161596932 if ciudad1==5 & urb_rur==1
replace lp_ci= 356.834573643814 if ciudad1==6 & urb_rur==1
replace lp_ci= 354.669687477575 if ciudad1==7 & urb_rur==1
replace lp_ci= 354.669687477575 if ciudad1==8 & urb_rur==1
replace lp_ci= 270.383301641924 if ciudad1==10 & urb_rur==1
replace lp_ci= 354.669687477575 if ciudad1==9 & urb_rur==1


replace lp_ci=    237.10     if  urb_rur==2

*/

/* Esta sección es para los residentes habituales del hogar mayores a 10 años*/ 


*BOL 1999
/*falta la variable ciudad1 en la base
Se ha reemplazado la variable ciudad1 por el de depto y zona_c Lourdes Montesdeoca Dic-2013
En La Paz se incluye a Cobija puesto que no hay forma de separarlos con las variables disponibles*/


replace lp_ci= 335.420044972232 if depto==1 & zona_c==1
replace lp_ci= 324.031424822892 if depto==2 & zona_c==1
replace lp_ci= 351.103517811812 if depto==3 & zona_c==1
replace lp_ci= 294.654238786948 if depto==4 & zona_c==1
replace lp_ci= 271.000161596932 if depto==5 & zona_c==1
replace lp_ci= 356.834573643814 if depto==6 & zona_c==1
replace lp_ci= 354.669687477575 if depto==7 & zona_c==1
replace lp_ci= 354.669687477575 if depto==8 & zona_c==1
replace lp_ci= 270.383301641924 if depto==9 & zona_c==1
*replace lp_ci= 354.669687477575 if  depto==10 & zona_c==1
                                

replace lp_ci=    237.10     if  zona_c==0

label var lp_ci "Linea de pobreza oficial del pais"

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

/*
replace lpe_ci= 169.387122710977 if ciudad1==1 & urb_rur==1
replace lpe_ci= 180.161472201528 if ciudad1==2 & urb_rur==1
replace lpe_ci= 177.307276494965 if ciudad1==3 & urb_rur==1
replace lpe_ci= 163.827756765543 if ciudad1==4 & urb_rur==1
replace lpe_ci= 150.676089847894 if ciudad1==5 & urb_rur==1
replace lpe_ci= 180.201459690126 if ciudad1==6 & urb_rur==1
replace lpe_ci= 180.172201238608 if ciudad1==7 & urb_rur==1
replace lpe_ci= 180.172201238608 if ciudad1==8 & urb_rur==1
replace lpe_ci= 164.122664096648 if ciudad1==10 & urb_rur==1
replace lpe_ci= 180.172201238608 if ciudad1==9 & urb_rur==1

replace lpe_ci=     134.74      if  urb_rur==2
*/
*BOL 1999

gen lpe_ci =.
/*falta la variable ciudad1 en la base
Se ha reemplazado la variable ciudad1 por el de depto y zona_c Lourdes Montesdeoca Dic-2013
En La Paz se incluye a Cobija puesto que no hay forma de separarlos con las variables disponibles*/


replace lpe_ci= 169.387122710977 if depto==1 & zona_c==1
replace lpe_ci= 180.161472201528 if depto==2 & zona_c==1
replace lpe_ci= 177.307276494965 if depto==3 & zona_c==1
replace lpe_ci= 163.827756765543 if depto==4 & zona_c==1
replace lpe_ci= 150.676089847894 if depto==5 & zona_c==1
replace lpe_ci= 180.201459690126 if depto==6 & zona_c==1
replace lpe_ci= 180.172201238608 if depto==7 & zona_c==1
replace lpe_ci= 180.172201238608 if depto==8 & zona_c==1
replace lpe_ci= 164.122664096648 if depto==9 & zona_c==1
*replace lpe_ci= 180.172201238608 if  depto==10 & zona_c==1
                                
replace lpe_ci=     134.74      if  zona_c==0

label var lpe_ci "Linea de indigencia oficial del pais"
*************
**salmm_ci***
*************

*BOL 1999
gen salmm_ci= 	330.00 
* revisado por Lourdes Montesdeoca dic/2013
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
gen afiliado_ci= 1 if   afilafp ==1	
recode afiliado_ci .=0 if condact>=1 & condact<=3
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.


replace tipopen_ci=1 if ingjub>0 &  ingjub~=.
replace tipopen_ci=2 if ingviu>0 & ingviu~=.
replace tipopen_ci=3 if ingben>0 & ingben~=.
replace tipopen_ci=4 if inginv>0 & inginv~=. 
replace tipopen_ci=12 if (ingjub>0 & ingviu>0) & (ingjub~=. & ingviu~=.)
replace tipopen_ci=13 if (ingjub>0 & ingben>0) & (ingjub~=. & ingben~=.)
replace tipopen_ci=23 if (ingviu>0 & ingben>0) & (ingviu~=. & ingben~=.)
replace tipopen_ci=123 if (ingjub>0 & ingviu>0 & ingben>0) & (ingjub~=. & ingviu~=. & ingben~=.)
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

gen instcot_ci=.

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact==2 | condact==3
replace condocup_ci=3 if (condact>3 & condact<=8) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/


* Homologacion toda la serie 05/22/2014 MGD


gen condocup_ci=.
*Mod. MLO 2015,10: se consideran otras causas excepcionales 
*replace condocup_ci=1 if trabajo==1 | (sondeo1>0 & sondeo1<=6)  | sondeo2==1
replace condocup_ci=1 if trabajo==1 | (sondeo1>0 & sondeo1<=6)  | (sondeo2>=1 & sondeo2<=7)
*replace condocup_ci=2 if (trabajo==2 | sondeo1==0 | (sondeo2>1 | sondeo2==0)) & (bustrab==1 | bus4sem==1) & (queriatr==1 | teniatpo==1)
replace condocup_ci=2 if  (trabajo==2 | sondeo1==0 | (sondeo2>7 | sondeo2==0)) & (bustrab==1 | bus4sem==1) & (queriatr==1 | teniatpo==1)

*2015,10 MLO la encuesta pregunta a partir de 7 años (no 10)
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if  cesante==1 & condocup_ci==2
* 2014, 03 Modificacion siguiente linea MLO
replace cesante_ci=0 if  cesante==2 & condocup_ci==2
*replace cesante_ci=0 if  cesante==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if tamestp==1
replace tamemp_ci=2 if tamestp>=2 & tamestp<=5
replace tamemp_ci=3 if tamestp>5 & tamestp<=7
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
*************
**pension_ci*
*************

egen aux_p=rsum(ingjub ingben inginv ingviu ingorf), missing
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
gen pensionsub_ci= .
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.

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

gen desalent_ci=(emp_ci==0 & bustrab==2 & (pqnobus==3 | pqnobus==4))
replace desalent_ci=. if emp_ci==.


*****************
***horaspri_ci***
*****************

gen horaspri_ci=hrstrap*diastrap
replace horaspri_ci=. if emp_ci~=1

*****************
***horastot_ci***
*****************

gen horassec=hrstras*diastras
replace horassec=. if emp_ci~=1

egen horastot_ci=rsum(horaspri_ci horassec), missing
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horassec

***************
***subemp_ci***
***************
/*
gen subemp_ci=(horastot_ci<=30 & mashrs==1)
replace subemp_ci=. if emp_ci~=1
*/
* Segun definicion del documento metodologico: horas de la actividad principal y si esta disponible a trabajar mas horas. MGD 06/18/2014
* Se podria considerar a las dos alternativas: desea trabajar y esta dispuesto a trabajar.
gen subemp_ci=0
*replace subemp_ci=1 if disponi==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (disponi==1 & mashrs==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
*Mod. MLO 2015, 10
*gen tiempoparc_ci=(horastot_ci<=30 & mashrs==2)
gen tiempoparc_ci=(horastot_ci<30 & mashrs==2)
replace tiempoparc_ci=. if emp_ci~=1


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categp>=4 & categp<=6
replace categopri_ci=2 if categp==3
replace categopri_ci=3 if categp==1 | categp==2 | categp==8
replace categopri_ci=4 if categp==7
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if categs>=4 & categs<=6
replace categosec_ci=2 if categs==3
replace categosec_ci=3 if categs==1 | categs==2 | categs==8
replace categosec_ci=4 if categs==7

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & trabsec==1

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if tamestp==1 /*1 a 4 personas*/
replace firmapeq_ci=0 if tamestp>=2 & tamestp<=7 /*5 o más personas*/
*/

*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=.
replace spublico_ci=1 if sectorp==2
replace spublico_ci=0 if sectorp==1
replace spublico_ci=. if emp_ci~=1


**************
***ocupa_ci***
**************

* Modificacion MGD 07/24/2014: clasificacion CIUO -88
gen aux= ocupp 
gen ocupa_ci=.
replace ocupa_ci=1 if (aux>=210 & aux<=348)  & emp_ci==1
replace ocupa_ci=2 if (aux>=110 & aux<=131)  & emp_ci==1
replace ocupa_ci=3 if (aux>=410 & aux<=422) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911)) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=621) | (aux>=920 & aux<=921)) & emp_ci==1
replace ocupa_ci=7 if ((aux>=710 & aux<=851) | (aux>=930 & aux<=933))& emp_ci==1
replace ocupa_ci=8 if (aux>=0 & aux<=11) & emp_ci==1

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

gen rama_ci=.
replace rama_ci=1 if (ramap>=111 & ramap<=500) & emp_ci==1
replace rama_ci=2 if (ramap>=1110 & ramap<=1429) & emp_ci==1
replace rama_ci=3 if (ramap>=1511 & ramap<=3699) & emp_ci==1
replace rama_ci=4 if (ramap>=4010 & ramap<=4100) & emp_ci==1
replace rama_ci=5 if (ramap>=4510 & ramap<=4540) & emp_ci==1
replace rama_ci=6 if (ramap>=5010 & ramap<=5521) & emp_ci==1 
replace rama_ci=7 if (ramap>=6010 & ramap<=6420) & emp_ci==1
replace rama_ci=8 if (ramap>=6511 & ramap<=7020) & emp_ci==1
replace rama_ci=9 if (ramap>=7130 & ramap<=9800) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=.
replace durades_ci=tpobusca/30  if perbus==1
replace durades_ci=tpobusca/4.3 if perbus==2
replace durades_ci=tpobusca/2   if perbus==3
replace durades_ci=tpobusca     if perbus==4
replace durades_ci=tpobusca*6   if perbus==5
replace durades_ci=tpobusca*12  if perbus==6
replace durades_ci=. if emp~=0




*******************
***antiguedad_ci***
*******************
/*En años*/

gen antiguedad_ci=.
replace antiguedad_ci=ctotpep/(4.3*12) if ctotpepp==2 & emp_ci==1
replace antiguedad_ci=ctotpep/12       if ctotpepp==4 & emp_ci==1
replace antiguedad_ci=ctotpep          if ctotpepp==6 & emp_ci==1

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (inactivo==1 & condocup_ci==3)
replace categoinac_ci = 2 if  (inactivo==2 & condocup_ci==3)
replace categoinac_ci = 3 if  (inactivo==3 & condocup_ci==3)
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
replace ypridb=saliqp*30  if frecsalp==1 
replace ypridb=saliqp*4.3 if frecsalp==2 
replace ypridb=saliqp*2   if frecsalp==3 
replace ypridb=saliqp     if frecsalp==4 
replace ypridb=saliqp/6   if frecsalp==5
replace ypridb=saliqp/12  if frecsalp==6

replace ypridb=0 if categopri_ci==4 

*Ingresos extras
local sub="hep prip"
foreach i of local sub {
gen ypriex`i'=.
replace ypriex`i'=ing`i'/12 
replace ypriex`i'=. if ing`i'==0
}

gen ypriexaguip=.
replace ypriexaguip=ypriexaguip/12 if recaguip==1
replace ypriexaguip=0 if recaguip==2


egen ypridbd=rsum(ypridb ypriexhep ypriexprip ypriexaguip), missing
replace ypridbd=. if ypridb==. & ypriexhep==. & ypriexprip==. & ypriexaguip==.

*Para los trabajadores independientes 

gen yprijbi=.
replace yprijbi=ingliqp*30 if frecliqp==1 
replace yprijbi=ingliqp*4.3 if frecliqp==2 
replace yprijbi=ingliqp*2 if frecliqp==3 
replace yprijbi=ingliqp if frecliqp==4 
replace yprijbi=ingliqp/6 if frecliqp==5
replace yprijbi=ingliqp/12 if frecliqp==6
replace yprijbi=. if ingliqp==99998 


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

local nnn="alip trap vesp vivp otrp"
foreach i of local nnn {

gen especie`i'=.
replace especie`i'=ing`i'*30  if frec`i'==1
replace especie`i'=ing`i'*4.3 if frec`i'==2
replace especie`i'=ing`i'*2   if frec`i'==3
replace especie`i'=ing`i'     if frec`i'==4
replace especie`i'=ing`i'/3   if frec`i'==5
replace especie`i'=ing`i'/6   if frec`i'==6
replace especie`i'=ing`i'/12  if frec`i'==7
replace especie`i'=0 if rec`i'==2
}

egen ylnmprid=rsum(especiealip especietrap especievesp especievivp especieotrp), missing
replace ylnmprid=. if especiealip==. & especietrap==. & especievesp==. & especievivp==. & especieotrp==. 
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
replace ysecb=saliqs*30 if frecsals==1 
replace ysecb=saliqs*4.3 if frecsals==2 
replace ysecb=saliqs*2 if frecsals==3 
replace ysecb=saliqs if frecsals==4 
replace ysecb=saliqs/6 if frecsals==5
replace ysecb=saliqs/12 if frecsals==6

replace ysecb=0 if categosec_ci==4 

*Ingresos extras

gen yxsa=inghes/12 
replace yxsa=. if inghes==0

gen yxsa1=ingpris/12 
replace yxsa1=. if ingpris==0

gen yxsa2=ingaguis/12 
replace yxsa2=. if ingaguis==0


egen ysecbd=rsum(ysecb yxsa yxsa1 yxsa2), missing
replace ysecbd=. if ysecb==. & yxsa==. & yxsa1==. & yxsa2==.


*Para los trabajadores independientes

gen ysecjbi=.
replace ysecjbi=ingliqs*30  if frecliqs==1 
replace ysecjbi=ingliqs*4.3 if frecliqs==2 
replace ysecjbi=ingliqs*2   if frecliqs==3 
replace ysecjbi=ingliqs     if frecliqs==4 
replace ysecjbi=ingliqs/6   if frecliqs==5
replace ysecjbi=ingliqs/12  if frecliqs==6

*Ingreso laboral monetario para todos

egen ylmsec_ci=rsum(ysecjbi ysecbd), missing
replace ylmsec_ci=. if ysecjbi==. & ysecbd==.
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

*Ingreso laboral no monetario de los dependientes

local nsn="alis tras vess vivs otrs"
foreach i of local nsn {

gen especie`i'=.
replace especie`i'=ing`i'*30  if frec`i'==1
replace especie`i'=ing`i'*4.3 if frec`i'==2
replace especie`i'=ing`i'*2   if frec`i'==3
replace especie`i'=ing`i'     if frec`i'==4
replace especie`i'=ing`i'/3   if frec`i'==5
replace especie`i'=ing`i'/6   if frec`i'==6
replace especie`i'=ing`i'/12  if frec`i'==7
replace especie`i'=0 if rec`i'==2
}

egen ylnmsecd=rsum(especiealis especietras especievess especievivs especieotrs), missing
replace ylnmsecd=. if especiealis==. & especietras==. & especievess==. & especievivs==. & especieotrs==. 
replace ylnmsecd=0 if categosec_ci==4


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
gen transl1=ingalpag/12
replace transl1=. if transl1==0

gen transl2=ingdiv/12
replace transl2=. if transl2==0

gen transl3=inginte/12
replace transl3=. if transl3==0

gen transl4=ingdiv/12
replace transl4=. if transl4==0

egen transltot=rsum(transl1 transl2  transl3 transl4), missing
replace transltot=. if transl1==. & transl2==. & transl3==. & transl4==.


*Rentas mensuales

local jjj="ingjub ingben inginv ingviu ingorf ingotro"
foreach i of local jjj {
replace `i'=. if `i'==0
}

egen rentastot=rsum(ingjub ingben inginv ingviu ingorf ingotro), missing
replace rentastot=. if ingjub==. & ingben==. & inginv==. & ingviu==. & ingorf==. & ingotro==. 


* Otros ingresos no laborales

gen frecaisf=frecasf

local xxx="aisf trai trae"

foreach i of local xxx {

gen otrosnl`i'=.
replace otrosnl`i'=ing`i'*30  if frec`i'==1
replace otrosnl`i'=ing`i'*4.3 if frec`i'==2
replace otrosnl`i'=ing`i'*2   if frec`i'==3
replace otrosnl`i'=ing`i'     if frec`i'==4
replace otrosnl`i'=ing`i'/6   if frec`i'==5
replace otrosnl`i'=ing`i'/12  if frec`i'==6
replace otrosnl`i'=. if ing`i'==0
}

egen yotrosnl=rsum(otrosnlaisf otrosnltrai otrosnltrae), missing
replace yotrosnl=. if otrosnlaisf==. &  otrosnltrai==. & otrosnltrae==. 


local www="ingheren ingindem ingextra"
foreach i of local www {
replace `i'=. if `i'==0
}

egen yotrosnl2=rsum(ingheren ingindem ingextra), missing
replace yotrosnl2=. if ingheren==. & ingindem==. & ingextra==. 
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



****************
***remesas_ci***
****************

gen remesas_ci=otrosnltrae

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

gen rentaimp_ch=alimp
replace rentaimp_ch=rentaimp_ch*5.8 if malimp==2
replace rentaimp_ch=. if alimp==0


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

replace aedu_ci=0 if nivel==11 | nivel==12 | nivel==13

replace aedu_ci=1 if (nivel==14 | nivel==16 | nivel==19) & curso==1
replace aedu_ci=2 if (nivel==14 | nivel==16 | nivel==19) & curso==2
replace aedu_ci=3 if (nivel==14 | nivel==16 | nivel==19) & curso==3
replace aedu_ci=4 if (nivel==14 | nivel==16 | nivel==19) & curso==4
replace aedu_ci=5 if (nivel==14 | nivel==16 | nivel==19) & curso==5

replace aedu_ci=6 if (nivel==14 & curso==6) | (nivel==17 & curso==1) | (nivel==19 & curso==6) 
replace aedu_ci=7 if (nivel==15 & curso==1) | (nivel==17 & curso==2) | (nivel==19 & curso==7) 
replace aedu_ci=8 if (nivel==15 & curso==2) | (nivel==17 & curso==3) | (nivel==19 & curso==8) 

replace aedu_ci=9 if  (nivel==15 & curso==3) | ((nivel==18 | nivel==20) & curso==1)
replace aedu_ci=10 if (nivel==15 & curso==4) | ((nivel==18 | nivel==20) & curso==2)
replace aedu_ci=11 if (nivel==15 & curso==5) | ((nivel==18 | nivel==20) & curso==3)
replace aedu_ci=12 if (nivel==15 & curso==6) | ((nivel==18 | nivel==20) & curso==4)

replace aedu_ci=12 if (nivel>=24 & nivel<=30 & nivel~=26) & curso==0
replace aedu_ci=13 if (nivel>=24 & nivel<=30 & nivel~=26) & curso==1 
replace aedu_ci=14 if (nivel>=24 & nivel<=30 & nivel~=26) & curso==2
replace aedu_ci=15 if (nivel>=24 & nivel<=30 & nivel~=26) & curso==3
replace aedu_ci=16 if (nivel>=24 & nivel<=30 & nivel~=26) & curso==4
replace aedu_ci=16 if (nivel==24 | nivel==27 | nivel==28) & curso==5
replace aedu_ci=17 if (nivel==25 | nivel==29) & curso==5

replace aedu_ci=16 if (nivel==24 | nivel==27 | nivel==28) & curso==8
replace aedu_ci=17 if (nivel==25 | nivel==29) & curso==8

replace aedu_ci=18 if nivel==26 & curso==1
replace aedu_ci=19 if nivel==26 & curso==2


**************
***eduno_ci***
**************

gen byte eduno_ci=(nivel==11 | nivel==12 | nivel==13) 
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

gen byte eduui_ci=0
replace eduui_ci=1 if (nivel==24 | nivel==27 | nivel==28 | nivel==30) & (curso>=1 & curso<=3)
replace eduui_ci=1 if (nivel==25 | nivel==29) & (curso>=1 & curso<=4)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (nivel==24 | nivel==27 | nivel==28 | nivel==30) & (curso>=4 & curso<=8)
replace eduuc_ci=1 if (nivel==25 | nivel==29) & (curso>=5 & curso<=8)
replace eduuc_ci=1 if nivel==26
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(nivel==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (nivel==25 | nivel==26 | nivel==27)
replace eduac_ci=0 if (nivel==24 | (nivel>=28 & nivel<=29))
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(inscrib==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=pqnoasi if asiste==2

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
	
gen pqnoasis1_ci = 1 if pqnoasi ==3
replace pqnoasis1_ci = 2 if pqnoasi ==4
replace pqnoasis1_ci = 3 if pqnoasi ==5 | pqnoasi ==6
replace pqnoasis1_ci = 4 if pqnoasi ==11
replace pqnoasis1_ci = 6 if pqnoasi ==8
replace pqnoasis1_ci = 7 if pqnoasi ==9 
replace pqnoasis1_ci = 8 if pqnoasi ==7  | pqnoasi ==10
replace pqnoasis1_ci = 9 if pqnoasi ==1  | pqnoasi ==2 | pqnoasi ==12

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

gen edupub_ci=(estabes==2)
replace edupub_ci=. if estabes==. | estabes==0

**************
***tecnica_ci*
**************
/*
gen tecnica_ci=.
replace tecnica_ci=1 if nivel==25 | nivel==26
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"
*/
gen tecnica_ci=.
replace tecnica_ci=1 if nivel==27 | nivel==28
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(agua==1)

gen aguadist_ch=1 if distagua==1
replace aguadist_ch=2 if distagua==2
replace aguadist_ch=3 if distagua==3

*Inclusión Mayra Sáenz Julio 2013
gen aguamala_ch=(agua==5 | agua==6)
replace aguamala_ch=. if agua==.
label var aguamala_ch "Agua unimproved según MDG" 

/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(elect==1)
replace luz_ch=. if elect==.


gen luzmide_ch=.
/*NA*/

gen combust_ch=.
/*NA*/

gen bano_ch=(sersani==1)

gen banoex_ch=(banoes==1)
replace banoex_ch=. if banoes==0

gen des1_ch=.
replace des1_ch=0 if desague==0
replace des1_ch=1 if desague==1 | desague==2
replace des1_ch=2 if desague==3
replace des1_ch=3 if desague==4

gen des2_ch=.
replace des2_ch=0 if desague==0
replace des2_ch=1 if desague==1 | desague==2
replace des2_ch=2 if desague==3

gen piso_ch=0 if pisos==1
replace piso_ch=1 if pisos>=2 & pisos<=7
replace piso_ch=2 if pisos==8

gen techo_ch=0 if techos==4
replace techo_ch=1 if techos>=1 & techos<=3
replace techo_ch=2 if techos==5

gen pared_ch=0 if paredes==2 | paredes==3 | paredes==6
replace pared_ch=1 if paredes==1 | paredes==4 | paredes==5
replace pared_ch=2 if paredes==7

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if (agua >= 1 & agua <=4)
replace aguamejorada_ch = 0 if (agua >= 5 & agua <=8)
		
*********************
***banomejorado_ch***
*********************
gen      banomejorado_ch = 1 if (sersani == 1 & (desague >= 1 & desague <=3) & banoes == 1)
replace banomejorado_ch = 0 if (sersani == 1 & (desague >= 1 & desague <=3) & banoes == 2) | sersani ==2 | (sersani == 1  & desague == 4)
	
gen dorm_ch=nrodorm

gen cuartos_ch=nrocuart

gen cocina_ch=(ccocina==1)

gen telef_ch=(sertelef==1)
replace telef_ch=. if sertelef==9

gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2
replace vivi1_ch=3 if tipoviv>=3 & tipoviv<=4


gen vivi2_ch=(tipoviv<=2)

gen viviprop_ch=0 if tenencia==1
replace viviprop_ch=1 if tenencia==2
replace viviprop_ch=2 if tenencia==3
replace viviprop_ch=3 if tenencia>=4 & tenencia<=8

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=alqviv if viviprop_ch==0
replace vivialq_ch=vivialq_ch*5.8 if malqviv==2
replace vivialq_ch=. if alqviv==0

gen vivialqimp_ch=alimp
replace vivialqimp_ch=vivialqimp_ch*5.8 if malimp==2
replace vivialqimp_ch=. if alimp==0


ren ocup ocup_old

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
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) 
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 */
rename ocupp codocupa
rename ramap codindustria


compress


saveold "`base_out'", replace


log close




