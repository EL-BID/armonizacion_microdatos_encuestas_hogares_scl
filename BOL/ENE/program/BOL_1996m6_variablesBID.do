
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
 
 *global ruta = "${surveysFolder}"

local PAIS BOL
local ENCUESTA ENE
local ANO "1996"
local ronda m6 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ENE
Round: m6
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

gen factor_ch=factorex
label variable factor_ch "Factor de expansion del hogar"


**************
****idh_ch****
**************
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=id_pers
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

#delimit;
gen byte zona_c=.;
replace zona =1 if (id_hog>=110001 & id_hog <= 110361)|
(id_hog>=130001 & id_hog <= 130105)|  
(id_hog>=140001 & id_hog <= 140015)|  
(id_hog>=210001 & id_hog <= 210780)|  
(id_hog>=220001 & id_hog <= 220420)|  
(id_hog>=220421 & id_hog <= 240013)|  
(id_hog>=310001 & id_hog <= 310540)|  
(id_hog>=310541 & id_hog <= 330090)|  
(id_hog>=410001 & id_hog <= 410239)|  
(id_hog>=420001 & id_hog <= 430060)|  
(id_hog>=430106 & id_hog <= 430120)|  
(id_hog>=510001 & id_hog <= 510240)|  
(id_hog>=520001 & id_hog <= 530060)|  
(id_hog>=610001 & id_hog <= 610181)|  
(id_hog>=620004 & id_hog <= 630015)|  
(id_hog>=710001 & id_hog <= 710843)|  
(id_hog>=720001 & id_hog <= 740014)|  
(id_hog>=810001 & id_hog <= 810244)|  
(id_hog>=820001 & id_hog <= 830043)|  
(id_hog>=910001 & id_hog <= 910120);
replace zona=0 if zona~=1;
#delimit cr

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

gen anio_c=1996
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=6
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=8
replace relacion_ci=5 if parentco==10 
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

gen factor_ci=factorex
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
replace civil_ci=1 if estcivil==4
replace civil_ci=2 if estcivil==1 
replace civil_ci=3 if estcivil==2
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
replace raza_ci= 1 if  idioma ==2 |  idioma ==4 |  idioma ==8 |  idioma ==16 
replace raza_ci= 3 if  idioma ==1 | idioma ==32 
by idh_ch: gen aux=raza_ci if relacion_ci==1
by idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
*/
gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if  (idioma >=2 & idioma <=17) | idioma ==35 |idioma ==64
replace raza_idioma_ci= 3 if  idioma ==1 | idioma ==32 |idioma==33
by idh_ch, sort: gen aux=raza_idioma_ci if parentco==1
by idh_ch, sort: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (parentco ==3 | parentco==5))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci "Indigena" 

gen id_afro_ci = 0
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

gen raza_ci=.

tab raza_idioma_ci 
tab raza_idioma_ci [iw=factorex]


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
/*falta la variable contexto en la base
replace lp_ci= 303.1 if contexto==11 |  contexto==13
replace lp_ci= 304.8  if contexto==21 |  contexto==23
replace lp_ci= 317.3  if contexto==31 |  contexto==33
replace lp_ci= 277.1 if contexto==41 |  contexto==43
replace lp_ci= 254.9 if contexto==51 |  contexto==53
replace lp_ci= 322.5  if contexto==61 |  contexto==63
replace lp_ci= 325.0 if contexto==71 |  contexto==73
replace lp_ci= 325.0 if contexto==81 |  contexto==83
replace lp_ci= 246.5  if contexto==22
replace lp_ci= 325.0 if contexto==91


replace lp_ci=    220.83     if contexto==14 |  contexto==24 |  contexto==34 |  contexto==44 |  contexto==54 |  contexto==64 |  contexto==74 |  contexto==94
*/
*BOL 1996
gen lp_ci =.

/*falta la variable contexto en la base
Se ha reemplazado la variable contexto por el de depto y zona_c Lourdes Montesdeoca Dic-2013
En La Paz se incluye a Cobija puesto que no hay forma de separarlos con las variables disponibles*/

replace lp_ci= 303.10 if  depto==1 & zona_c==1
replace lp_ci= 304.80  if depto==2 & zona_c==1
replace lp_ci= 317.30  if depto==3 & zona_c==1
replace lp_ci= 277.10 if  depto==4 & zona_c==1
replace lp_ci= 254.90 if  depto==5 & zona_c==1
replace lp_ci= 322.50  if depto==6 & zona_c==1
replace lp_ci= 325.00 if  depto==7 & zona_c==1
replace lp_ci= 325.00 if  depto==8 & zona_c==1
replace lp_ci= 246.50  if depto==9 & zona_c==1
*replace lp_ci= 325.00 if  depto==10 & zona_c==1

replace lp_ci=    220.83  if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

/* falta la variable contexto en la base

replace lpe_ci= 153.1 if contexto==11 |  contexto==13
replace lpe_ci= 169.5  if contexto==21 |  contexto==23
replace lpe_ci=  160.2  if contexto==31 |  contexto==33
replace lpe_ci= 154.1 if contexto==41 |  contexto==43
replace lpe_ci=  141.7 if contexto==51 |  contexto==53
replace lpe_ci=  162.9  if contexto==61 |  contexto==63
replace lpe_ci= 165.1 if contexto==71 |  contexto==73
replace lpe_ci= 165.1 if contexto==81 |  contexto==83
replace lpe_ci=  149.6 if contexto==22
replace lpe_ci= 165.1 if contexto==91


replace lpe_ci=    125.49     if contexto==14 |  contexto==24 |  contexto==34 |  contexto==44 |  contexto==54 |  contexto==64 |  contexto==74 |  contexto==94
*/

*BOL 1996
gen lpe_ci =.
/*falta la variable contexto en la base
Se ha reemplazado la variable contexto por el de depto y zona_c Lourdes Montesdeoca Dic-2013
En La Paz se incluye a Cobija puesto que no hay forma de separarlos con las variables disponibles*/

replace lpe_ci= 153.10 if   depto==1 & zona_c==1
replace lpe_ci= 169.50  if  depto==2 & zona_c==1
replace lpe_ci=  160.20  if depto==3 & zona_c==1
replace lpe_ci= 154.10 if   depto==4 & zona_c==1
replace lpe_ci=  141.70 if  depto==5 & zona_c==1
replace lpe_ci=  162.90  if depto==6 & zona_c==1
replace lpe_ci= 165.10 if   depto==7 & zona_c==1
replace lpe_ci= 165.10 if   depto==8 & zona_c==1
replace lpe_ci=  149.60 if  depto==9 & zona_c==1
*replace lpe_ci= 165.10 if   depto==10 & zona_c==1                   

replace lpe_ci=    125.49  if zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 1996
gen salmm_ci= 223.00
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
gen afiliado_ci= .
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.


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
replace condocup_ci=3 if (condact>3 & condact<=8) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Homologacion toda la serie 05/22/2014 MGD

gen condocup_ci=.
replace condocup_ci=1 if trabajo==1 | sondeo==1 
replace condocup_ci=2 if (trabajo==2 | sondeo==1) & (bustrab==1 | bus4sem==1)
recode condocup_ci .=3 if edad_ci>=7  /* mod 12/2015 MLO cambie edad a 7+*/
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if  cesante==1 & condocup_ci==2
replace cesante_ci=0 if  cesante==2 & condocup_ci==2

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if tamest>0 & tamest<=5
replace tamemp_ci=2 if tamest>5 & tamest<=49
replace tamemp_ci=3 if tamest>49 & tamest<9999 & tamest!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

egen aux_p=rsum(ingpenju ingpenbe), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
*recode ypen_ci .=0 
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

gen desalent_ci=(emp_ci==0 & bustrab==2 & (pqno==1 | pqno==2))
replace desalent_ci=. if emp_ci==.


*****************
***horaspri_ci***
*****************

gen horaspri_ci=hrstra*diastra
replace horaspri_ci=. if emp_ci~=1

*****************
***horastot_ci***
*****************

gen horassec=hrsxdia*diasxsem
replace horassec=. if emp_ci~=1

egen horastot_ci=rsum(horaspri_ci horassec), missing
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horassec

***************
***subemp_ci***
***************
/*
gen subemp_ci=(horastot_ci<=30 & deseamas==1)
replace subemp_ci=. if emp_ci~=1 | deseamas==0
*/

* Se considera subempleo visible: quiere trabajar mas horas y esta disponible. MGD 06/18/2014
gen subemp_ci=0
replace subemp_ci=1 if deseamas==1  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(horastot_ci<=30 & deseamas==2)
replace tiempoparc_ci=. if emp_ci~=1 | deseamas==0


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categp==4 
replace categopri_ci=2 if categp==3 | categp==6
replace categopri_ci=3 if categp==1 | categp==2 | categp==7
replace categopri_ci=4 if categp==5
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if categs==4 | categs==7
replace categosec_ci=2 if categs==3 | categs==6
replace categosec_ci=3 if categs==1 | categs==2 | categs==8
replace categosec_ci=4 if categs==5

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
* Alternativa considerando la variable tipo de trabajo MGD 6/11/2014
gen tipocontrato_ci=.
replace tipocontrato_ci =1 if tipotra==1 & categopri_ci==3
replace tipocontrato_ci =2 if tipotra==2 & categopri_ci==3
replace tipocontrato_ci =3 if tipocontrato_ci==. & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


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
gen firmapeq_ci=(tamest>=1 & tamest<=5) 
replace firmapeq_ci=. if emp~=1 | tamest==0 | tamest==9999 
*/

*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=(sectorp==1 | sectorp==2)
replace spublico_ci=. if emp_ci~=1 | sectorp==0


**************
***ocupa_ci***
**************
* Modificacion MGD 07/24/2014: clasificacion CIUO -88

gen aux =ocupp
gen ocupa_ci=.
replace ocupa_ci=1 if (aux>=210 & aux<=348)  & emp_ci==1
replace ocupa_ci=2 if (aux>=110 & aux<=131)  & emp_ci==1
replace ocupa_ci=3 if (aux>=410 & aux<=422) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911)) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=625) | (aux>=920 & aux<=921)) & emp_ci==1
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

* La muestra de la encuesta se incrementa en 4 millones de personas, asi como los empleados en 2 millones.  MGD 04/07/2014
* El mayor cambio, no obstante, se evidencia en la variable de rama de actividad en agricultura: pasa de 224 a 6000 aproximadamente. MGD 04/07/2014
* Se recodifico la variable segun la rev.  3
gen rama_ci=.
replace rama_ci=1 if (ramap>=11 & ramap<=50) & emp_ci==1
replace rama_ci=2 if (ramap>=111 & ramap<=142) & emp_ci==1
replace rama_ci=3 if (ramap>=151 & ramap<=372) & emp_ci==1
replace rama_ci=4 if (ramap>=401 & ramap<=410) & emp_ci==1
replace rama_ci=5 if (ramap>=451 & ramap<=455) & emp_ci==1
replace rama_ci=6 if (ramap>=501 & ramap<=553) & emp_ci==1 
replace rama_ci=7 if (ramap>=601 & ramap<=642) & emp_ci==1
replace rama_ci=8 if (ramap>=651 & ramap<=713) & emp_ci==1
replace rama_ci=9 if (ramap>=721 & ramap<=980) & emp_ci==1





****************
***durades_ci***
****************
/*En meses*/
/*Esta variable no es comparable con las siguientes*/

gen durades_ci=.

gen durades1_ci=tpobusca
replace durades1_ci=. if emp_ci~=0 | tpobusca==0


*******************
***antiguedad_ci***
*******************
/*En años*/

gen antiguedad_ci=.
*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (pqnotrab==9 & condocup_ci==3)
replace categoinac_ci = 2 if  (pqnotrab==8 & condocup_ci==3)
replace categoinac_ci = 3 if  (pqnotrab==7 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
*Modificación Mayra Sáenz - Septiembre 2014
*Las variables afiliado y cotizando son misssing.
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"


*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************
*Para los obreros y empleados 

gen ypridbd=.
replace ypridbd=sysprin*30  if typosys==1 
replace ypridbd=sysprin*4.3 if typosys==2 
replace ypridbd=sysprin*2   if typosys==3 
replace ypridbd=sysprin     if typosys==4 
replace ypridbd=sysprin/12  if typosys==5
replace ypridbd=0 if sysprin==99998
replace ypridbd=. if sysprin==0 | sysprin==99999


*Para los que no son obreros o empleados 

gen yprijbi=.
replace yprijbi=ganprin*30  if tipogan==1 
replace yprijbi=ganprin*4.3 if tipogan==2 
replace yprijbi=ganprin*2   if tipogan==3 
replace yprijbi=ganprin     if tipogan==4 
replace yprijbi=ganprin/12  if tipogan==5	
replace yprijbi=0 if ganprin==99998
replace yprijbi=0 if categopri_ci==4 
replace yprijbi=. if ganprin==99999 | ganprin==0


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

gen ylnmpri_ci=.


***************
***ylmsec_ci***
***************

*Ingreso laboral monetario de la actividad secundaria

gen ylmsec_ci=.
replace ylmsec_ci=ysecun*30  if tipoysec==1 
replace ylmsec_ci=ysecun*4.3 if tipoysec==2 
replace ylmsec_ci=ysecun*2   if tipoysec==3 
replace ylmsec_ci=ysecun     if tipoysec==4 
replace ylmsec_ci=ysecun/12  if tipoysec==5

replace ylmsec_ci=0 if categosec_ci==4 | ysecun==99998
replace ylmsec_ci=. if ysecun==99999
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ylnm_ci***
*************

gen ylnm_ci=.

*************
***ynlm_ci***
*************

*Ingresos no laborales monetarios

gen nesanopenju=mesanoa
gen nesanopenbe=mesanob
gen nesanoasisf=mesanoc
gen nesanootrho=mesanod
gen nesanoalqui=mesanoe
gen nesanointe=mesanof
gen nesanoagui=mesanog
gen nesanoputil=mesanoh
gen nesanobonop=mesanoi
gen nesanopress=mesanoj
gen nesanootro=mesanok

local jjj="penju penbe asisf otrho alqui inte agui putil bonop press otro"
foreach i of local jjj {
gen yy`i'=ing`i' if nesano`i'==1
replace yy`i'=ing`i'/12 if nesano`i'==2
replace yy`i'=0 if nesano`i'==0 | ing`i'==99998
replace yy`i'=. if ing`i'==-1 | ing`i'==99999
}

drop nesano*

egen ynlm_ci=rsum(yypenju yypenbe yyasisf yyotrho yyalqui yyinte yyagui yyputil yybonop yypress yyotro), missing
replace ynlm_ci=. if yypenju==. & yypenbe==. & yyasisf==. & yyotrho==. & yyalqui==. & yyinte==. & yyagui==. & yyputil==. & yybonop==. & yypress==. & yyotro==. 


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.


****************
***remesas_ci***
****************

gen remesas_ci=.

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

gen ylnm_ch=.


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

*******************
*** remesas_ch ***
*******************

gen remesas_ch=.


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

gen rentaimp_ch=.


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

replace aedu_ci=0 if nivel==0 | nivel==1 

replace aedu_ci=1 if nivel==2 & curso==1
replace aedu_ci=2 if nivel==2 & curso==2
replace aedu_ci=3 if nivel==2 & curso==3
replace aedu_ci=4 if nivel==2 & curso==4
replace aedu_ci=5 if nivel==2 & curso==5

replace aedu_ci=6 if nivel==3 & curso==1
replace aedu_ci=7 if nivel==3 & curso==2
replace aedu_ci=8 if nivel==3 & curso==3
replace aedu_ci=9 if (nivel==4 | nivel==7) & curso==1
replace aedu_ci=10 if (nivel==4 | nivel==7) & curso==2
replace aedu_ci=11 if (nivel==4 | nivel==7) & curso==3
replace aedu_ci=12 if (nivel==4 | nivel==7) & (curso==4 | curso==7 | curso==8)

replace aedu_ci=13 if (nivel==5 | nivel==6 | nivel==8) & curso==1 
replace aedu_ci=14 if (nivel==5 | nivel==6 | nivel==8) & curso==2 
replace aedu_ci=15 if (nivel==5 | nivel==6 | nivel==8) & curso==3 

replace aedu_ci=16 if (nivel==5 | nivel==8) & (curso==7 | curso==8)
replace aedu_ci=16 if (nivel==6) & curso==4

replace aedu_ci=17 if (nivel==6) & (curso==7 | curso==8)


**************
***eduno_ci***
**************

gen byte eduno_ci=(nivel==0 | nivel==1) 
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

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & curso<7)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & curso>=7 & curso<=8)
replace eduuc_ci=1 if aedu_ci>=17 & aedu<.
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(nivel==1)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (nivel==6)
replace eduac_ci=0 if (nivel==5 | nivel==8)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(asist==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=pqnoasi if asiste_ci==2

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************

gen pqnoasis1_ci =.

***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.


***************
***edupub_ci***
***************
/*Sobre los que se matricularon ese año*/

gen edupub_ci=(estabes==1)
replace edupub_ci=. if estabes==. | estabes==0

**************
***tecnica_ci*
**************

gen tecnica_ci=.
replace tecnica_ci=1 if nivel==8
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(agua==1 | agua==2 | agua==3)

gen aguadist_ch=1 if agua==1
replace aguadist_ch=2 if agua==2
replace aguadist_ch=3 if agua>=3 & agua<=7


*Inclusión Mayra Sáenz Julio 2013
gen aguamala_ch=(agua==6)
replace aguamala_ch=. if agua==.
label var aguamala_ch "Agua unimproved según MDG" 
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=.
gen luzmide_ch=.
/*NA*/

gen combust_ch=.
/*NA*/

gen bano_ch=(inodoro==1 | inodoro==2)

gen banoex_ch=(usoserv==1)
replace banoex_ch=. if usoserv==0

gen des1_ch=.

gen des2_ch=.
replace des2_ch=0 if desague==0
replace des2_ch=1 if desague==1 | desague==2
replace des2_ch=2 if desague==3

gen piso_ch=0 if pisos==5
replace piso_ch=1 if pisos>=1 & pisos<=4
replace piso_ch=2 if pisos==6

gen techo_ch=0 if techos==4
replace techo_ch=1 if techos>=1 & techos<=3
replace techo_ch=2 if techos==5

gen pared_ch=0 if paredes==1 | paredes==2 | paredes==6
replace pared_ch=1 if paredes==3 | paredes==4 | paredes==5
replace pared_ch=2 if paredes==7

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen  aguamejorada_ch = 1 if (agua >= 1 & agua <=3) | agua==5
replace aguamejorada_ch = 0 if  agua==4 | (agua >= 6 & agua <=7)
		
			
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if  (inodoro == 1 | inodoro == 2 ) & (desague >= 1 & desague <=3) & usoserv == 1
replace banomejorado_ch = 0 if ((inodoro == 1 | inodoro == 2 ) & (desague >= 1 & desague <=3) & usoserv == 2) | inodoro ==3 | ((inodoro == 1 | inodoro == 2 ) & desague == 4)
	
	

gen dorm_ch=nrodorm
recode dorm_ch (0=1)

gen cuartos_ch=nrocuart


gen cocina_ch=(cocina==1)

gen telef_ch=.
gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2
replace vivi1_ch=3 if tipoviv>=3 & tipoviv<=5


gen vivi2_ch=(tipoviv<=2)

gen viviprop_ch=0 if tenencia==2
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=3 if tenencia>=3 & tenencia<=7

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=.
gen vivialqimp_ch=.



ren ocup ocup_old

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
rename ocupp codocupa
rename ramap codindustria

compress


saveold "`base_out'", replace


log close


