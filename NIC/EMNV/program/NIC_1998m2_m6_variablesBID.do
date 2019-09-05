
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

local PAIS NIC
local ENCUESTA EMNV
local ANO "1998"
local ronda m2_m6

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: EMNV
Round: Febrero-Junio
Autores: Yessenia Loayza
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear


/***** revision August 03,2005

removed condition (& edad_ci<18) froom the following two lines:

by idh_ch: egen byte nhijos_ch=sum((relacion_ci==3) & edad_ci<18)
by idh_ch: egen byte notropari_ch=sum((relacion_ci==4) & edad_ci>=18)

***/

/** revision August 18,2006 (Victoria) 

Change in the variable aedu beacuse secundary education lasts 5 years in 
Nicaragua. 

Code for education dummies was changed beacuse there were mistakes in it. 
Old code can be seen in education section below. 
**/
* January 25 2007 (MFP)
*Change in firmapeq variable. It was created to reflect big firms instead of small firm


************
* Region_c *
************
gen region_c=  .
label var region_c "División política"


/*****************
Variables de Hogar
******************/
gen factor_ch=pesoviv
gen idh_ch=id_hogar
gen idp_ci=codper
gen zona_c=i05
replace zona_c=0 if i05==2
gen pais_c="NIC"
gen anio_c=1998
gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" | pais=="PAN" | pais=="DOM" | pais=="CRI" | pais=="BLZ" | pais=="GTM" | pais=="SLV" | pais=="HON" | pais=="NIC"
replace region_BID_c=2 if pais=="BAH" | pais=="BAR" | pais=="GUY" | pais=="JAM" | pais=="SUR" | pais=="TOT"
replace region_BID_c=3 if pais=="ECU" | pais=="COL" | pais=="PER" | pais=="VEN" | pais=="BOL" 
replace region_BID_c=4 if pais=="ARG" | pais=="BRA" | pais=="CHL" | pais=="PRY" | pais=="URU" 
replace region_BID_c=5 if pais=="HAI"
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
gen mes_c=etapa+3
gen relacion_ci=p202
replace relacion_ci=4 if p202>=5 & p202<=8
replace relacion_ci=5 if p202==9 | p202==11
replace relacion_ci=6 if p202==10

/*************************************
Variables de Infraestructura del Hogar
**************************************/
gen aguared_ch=(vb18==1 | vb18==2) /*Esta definido como acceso a agua de red!*/
gen aguadist_ch=0
replace aguadist_ch=1 if vb18==1
replace aguadist_ch=2 if vb18==2 & vb20b<=100
replace aguadist_ch=3 if vb18==2 & vb20b>100
gen aguamala_ch=(vb18==5 | vb18==6)
gen aguamide_ch=(vb21==1)
gen luz_ch=(vb38==1)
gen luzmide_ch=(vb39==1)
gen cocina_ch=(vb42==1)
gen combust_ch=(vb43==2 | vb43==3 | vb43==5)
gen bano_ch=(vb28!=5) /*Esta definido como si posee servicio higienico: inodoro o letrina*/
gen banoex_ch=(vb29==1)
replace banoex_ch=. if bano_ch==0

gen des1_ch=0
replace des1_ch=1 if vb28==2 | vb28==3 /*Red o pozo septico*/
replace des1_ch=2 if vb28==1 /*Pozo comun  - Letrina */
replace des1_ch=3 if vb28==4 /*Superficie*/

gen des2_ch=0
replace des2_ch=1 if vb28==2 | vb28==3 /*Red o pozo septico*/
replace des2_ch=2 if vb28==1 /*Pozo comun  - Letrina */
gen dessup=(vb28==4)

gen piso_ch=(va05==1 | va05==2 | va05==4)
replace piso_ch=2 if va05==3 | va05==6 /*Vale 0 si es piso de tierra, 1 si es permanente y 2 si es otros (o de ladrillo de barro)*/

gen pared_ch= (va04==2 |va04==4 |va04==6 |va04==7 |va04==8)
replace pared_ch=2 if va04==10 |va04==1 /*Otros*/

gen techo_ch=(va06==1 | va06==3)
replace techo_ch=2 if va06==6 /*Otros*/

gen resid_ch=0 /*Recoleccion*/
replace resid_ch=1 if vb35==2 | vb35==3 /*Quemada o enterrada*/
replace resid_ch=2 if vb35==5 /*Tirada*/
replace resid_ch =3 if vb35!=1 & vb35!=2 & vb35!=3 & vb35!=5 /*"Otros: Hecha abono o llevada al lugar"*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .

gen dorm_ch=vb11
gen cuartos_ch = va09

gen vivi1_ch = (va03==1) /*Casa*/
replace vivi1_ch=2 if va03==3 /*Departamento*/
replace vivi1_ch=3 if vivi1_ch==0

gen vivi2_ch = (va03==1|va03==3)

gen viviprop_ch = 0 /*Alquilada*/
replace viviprop_ch = 1 if vb13 ==1 | vb13==2 /*Propia*/
replace viviprop_ch = 2 if vb13 ==3 /*Pagando*/
replace viviprop_ch = 4 if vb13!=1 & vb13!=2 & vb13!=3 & vb13!=7

gen vivitit_ch = (vb13 ==1) /*Con titulo que certifique la propiedad de la vivienda*/

gen vivialq_ch= vb14
gen vivialqimp_ch= vb17
replace vivialqimp_ch=. if vb17==99999
gen telef_ch=(vb49!=4)
/*Los bienes durables (refrigerador, computadora y auto) ya fueron creados en un do anterior*/

****************
****freez_ch****
****************
gen freez_ch=.

****************
**internet_ch***
****************

gen internet_ch=.

****************
****cel_ch******
****************
gen cel_ch=.

/*********************
Variables Demograficas
*********************/
gen factor_ci=ponde
gen sexo_ci=p203
gen edad_ci=p204a
gen civil_ci=1 if p206==6 | p206==7
replace civil_ci=2 if p206==1 | p206==2
replace civil_ci=3 if p206==3 | p206==4
replace civil_ci=4 if p206==5 
gen jefe_ci=(p202==1)
sort idh_ch
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2) 
by idh_ch: egen byte nhijos_ch=sum(relacion_ci==3)
by idh_ch: egen byte notropari_ch=sum(relacion_ci==4)
by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)
by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)
gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0)  /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
sort idh_ch
by idh_ch:egen byte nmiembros_ch=sum((relacion_ci>0 & relacion_ci<5)|p202==9) if relacion_ci~=6 
by idh_ch:egen byte nmayor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|p202==9) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|p202==9) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum(((relacion_ci>0 & relacion_ci<5)|p202==9) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum(((relacion_ci>0 & relacion_ci<5)|p202==9) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum(((relacion_ci>0 & relacion_ci<5)|p202==9) & (edad_ci<1)) /*Hay que tener en cuenta que en 
este año, se pregunto si existen pensionistas en la casa, que tecnicamente son "otros no parientes", pero que en la practica
no deben ser incluídos en las variables de hogar (eg: _ch)*/

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"
/***************************
Variables de Demanda Laboral
****************************/


****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if ocupados==1
replace condocup_ci=2 if desocupa==1 | oactivos==1
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<6
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
* Alternativa 2 con variables originales tomando en cuenta la definicion de armonizacion MGD 06/05/2014
* La categoria 2 se reduce cuando se condiciona por busqueda.
gen condocup_ci=.
replace condocup_ci=1 if p5a01==1 | p5a02==1 | p5a03==1
replace condocup_ci=2 if condocup_ci!=1 & p5a04==1 
recode condocup_ci .=3 if edad_ci>=6
recode condocup_ci .=4 if edad_ci<6
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

************
***emp_ci***
************

gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1

****************
*afiliado_ci****
****************

gen afiliado_ci=.
/*
Se genera como missing porque la pregunta hace referencia sólo al seguro de salud y no es comparable con el resto de años.
gen afiliado_ci=(p3c43==1 | p3c43==4)
replace afiliado_ci=. if p3c43==.
*/
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (p5b30a==1 | p5d60a==1) & cotizando_ci==0 /*solo a ocupados*/
*Nota: no considero el trabajo de doce meses. Solo el principal y secundario (cuando hay)
****************
*cotizapri_ci***
****************
gen cotizapri_ci=.
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=.
label var cotizasec_ci "Cotizante a la Seguridad Social en actividad sec."

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*************
**pension_ci*
*************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*************
*   ypen_ci *
*************
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
*No encuentro la variable original

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if p5a08==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =4259
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =2246  
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

gen rama_ci=.
replace rama_ci=1 if (p5b13>=111 & p5b13<=500) & emp_ci==1
replace rama_ci=2 if (p5b13>=1010 & p5b13<=1429) & emp_ci==1
replace rama_ci=3 if (p5b13>=1510 & p5b13<=3720) & emp_ci==1
replace rama_ci=4 if (p5b13>=4010 & p5b13<=4100) & emp_ci==1
replace rama_ci=5 if (p5b13>=4510 & p5b13<=4550) & emp_ci==1
replace rama_ci=6 if (p5b13>=5010 & p5b13<=5520) & emp_ci==1
replace rama_ci=7 if (p5b13>=6010 & p5b13<=6420) & emp_ci==1
replace rama_ci=8 if (p5b13>=6510 & p5b13<=7020) & emp_ci==1
replace rama_ci=9 if (p5b13>=7111 & p5b13<=9800) & emp_ci==1


*************
**salmm_ci***
*************
generat salmm_ci=.
replace salmm_ci=300+500/2 if rama_ci==1
replace salmm_ci=600       if rama_ci==2
replace salmm_ci=500       if rama_ci==3
replace salmm_ci=600       if rama_ci==4  
replace salmm_ci=480       if rama_ci==5 
replace salmm_ci=550       if rama_ci==6 
replace salmm_ci=450       if rama_ci==7
replace salmm_ci=700       if rama_ci==8
replace salmm_ci=470+350/2 if rama_ci==9
replace salmm_ci=(300+500+600+500+600+480+550+450+700+470+350)/11 if salmm_ci==. 
label var salmm_ci "Salario minimo legal"


gen ocupa_ci=.
replace ocupa_ci=1 if real(substr(string(p5b12),1,1))==2 | real(substr(string(p5b12),1,1))==3
replace ocupa_ci=2 if real(substr(string(p5b12),1,1))==1
replace ocupa_ci=3 if real(substr(string(p5b12),1,1))==4
replace ocupa_ci=4 if real(substr(string(p5b12),1,2))==52
replace ocupa_ci=5 if real(substr(string(p5b12),1,2))==51
replace ocupa_ci=6 if real(substr(string(p5b12),1,1))==6
replace ocupa_ci=7 if real(substr(string(p5b12),1,1))==7 | real(substr(string(p5b12),1,1))==8
replace ocupa_ci=8 if real(substr(string(p5b12),1,1))==0
replace ocupa_ci=9 if real(substr(string(p5b12),1,1))==9
replace ocupa_ci=. if emp_ci==0

gen horaspri_ci=p5b17*p5b16
replace horaspri_ci=. if emp_ci==0 | p5b17>24 | p5b16>7

gen horastot_ci=p5c43
replace horastot_ci=. if emp_ci==0

gen salario=p5b19a*22 if p5b19b==1 
replace salario=p5b19a*4.3 if p5b19b==2
replace salario=p5b19a*2 if p5b19b==3 | p5b19b==4
replace salario=p5b19a if p5b19b==5
replace salario=p5b19a/3 if p5b19b==6
replace salario=p5b19a/6 if p5b19b==7
replace salario=p5b19a/12 if p5b19b==8
replace salario=. if p5b19b==9 | emp_ci==0 /*Hay algunos tipos que cobran con una frecuencia incierta(17), por lo que, ante
la duda los ponemos como missing.*/
gen agui=p5b22b/12 /*El aguinaldo esta preguntado para todo el año*/
egen ylmpri_ci=rsum(salario p5b21b agui)
replace ylmpri_ci=. if (salario==. & p5b21b==. & p5b22b==.) |emp_ci==0 /*INCLUYE LO MISMO QUE INCLUYE EL YLMPRI_CI DEL '93*/
drop salario agui

/*Para los ingresos laborales no monetarios de la actividad principal, el rubro mensual de uniformes
* debe ser calculado en base a la p5b25b, que hace referencia a cuántas veces al año recibe los uniformes.
*/
gen unif=p5b25b if p5b25c==12
replace unif=p5b25b/2 if p5b25c==6 
replace unif=p5b25b/3 if p5b25c==4
replace unif=p5b25b/4 if p5b25c==3
replace unif=p5b25b/6 if p5b25c==2
replace unif=p5b25b/12 if p5b25c==1
replace unif=. if emp_ci==0 | p5b25b==.



egen ylnmpri_ci=rsum(p5b23b p5b24b unif p5b26b)
replace ylnmpri_ci=. if (p5b23b==. & p5b24b==. & unif==. & p5b26b==.) | emp_ci==0

gen salariosec=p5c38a*22 if p5c38b==1 
replace salariosec=p5c38a*4.3 if p5c38b==2
replace salariosec=p5c38a*2 if p5c38b==3 | p5c38b==4
replace salariosec=p5c38a if p5c38b==5
replace salariosec=p5c38a/3 if p5c38b==6
replace salariosec=p5c38a/6 if p5c38b==7
replace salariosec=p5c38a/12 if p5c38b==8
replace salariosec=. if p5c38b==9 | emp_ci==0 /*Hay algunos tipos que cobran con una frecuencia incierta(17), por lo que, ante
la duda los ponemos como missing.*/
gen aguis=p5c41b/12 /*Esta definido por año*/
egen ylmsec_ci=rsum(salariosec p5c40b aguis)
replace ylmsec_ci=. if (salariosec==. & p5c40b==. & p5c41b==.) |emp_ci==0 
drop salariosec aguis

gen ylnmsec_ci=p5c42b
replace ylnmsec_ci=. if emp_ci==0

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.  

forvalues i=1(1)10{
gen gd22mo`i'=gd22`i'/12
} /*Existen categorías que estan anuales*/
egen ynlm_ch=rsum(gd12* gd22mo*)
replace ynlm_ch=. if  (gd1201==. & gd1202==. & gd1203==. & gd1204==. & gd1205==. & gd1206==. & gd1207==. & gd221==. & gd222==. & gd223==. & gd224==. & gd225==. & gd226==. & gd227==. & gd228==. & gd229==. & gd2210==.) 
drop gd22mo*
replace ynlm_ch=. if miembros_ci ==0
gen ylmotros_ci=.
gen ylnmotros_ci = .

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if p202<10
by idh_ch: egen ylm_ch=sum(ylm_ci)if p202<10
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if p202<10

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

gen ylmnr_ch=ylm_ch
replace ylmnr=. if nrylmpri_ch==1

by idh_ch: egen nper = sum(miembros_ci)
by idh_ch: gen ynlm_ci=ynlm_ch/nper if miembros_ci==1
gen ynlnm_ch=.

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen rentaimp_ch=vb17
replace rentaimp_ch=. if vb17==99999

gen autocons_ci=.
gen autocons_ch=.
gen remesas_ci=.
gen remesas_ch=.
gen ynlnm_ci=.

gen durades_ci=p5a07/4.3

gen antiguedad_ci=p5b14a if p5b14b==4
replace antiguedad_ci=p5b14a/12 if p5b14b==3
replace antiguedad_ci=p5b14a/48 if p5b14b==2
replace antiguedad_ci=p5b14a/365 if p5b14b==1
replace antiguedad_ci=. if emp_ci==0

/****************************
Variables del Mercado Laboral
****************************/
gen desalent_ci=(p5a05==12 | p5a05==13)
replace desalent_ci=. if emp_ci==1

*Modificacion MGD 06/20/2014 inclusion de numero de horas.
gen subemp_ci=(p5c45==1) & horaspri_ci <=30 & emp_ci==1

gen tiempoparc_ci=(p5c45==2)
replace tiempoparc_ci=. if emp_ci==0

gen categopri_ci=.
replace categopri_ci=1 if p5b20==4
replace categopri_ci=2 if p5b20==3
replace categopri_ci=3 if p5b20==1 | p5b20==2 | p5b20==5
replace categopri_ci=4 if p5b20==6
replace categopri_ci=. if emp_ci==0

gen categosec_ci=.
replace categosec_ci=1 if p5c39==4
replace categosec_ci=2 if p5c39==3
replace categosec_ci=3 if p5c39==1 | p5c39==2 | p5c39==5
replace categosec_ci=4 if p5c39==6
replace categosec_ci=. if emp_ci==0

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
replace  tipocontrato_ci=1 if (p5b27b==1 & p5b27a==1) & categopri_ci==3
replace  tipocontrato_ci=2 if (p5b27b==2 & p5b27a==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p5b27a==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

gen segsoc_ci=(p5b30a==1)
replace segsoc_ci=. if emp_ci==0

gen nempleos_ci=1 if p5c31==2
replace nempleos_ci=2 if p5c31==1
replace nempleos_ci=. if emp_ci==0
/*
gen firmapeq_ci=(p5b18<=2)
replace firmapeq_ci=. if emp_ci==0 |p5b18==.
*/
gen spublico_ci=.

*************
*tamemp_ci
*************
*Nicaragua Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50
/*
gen tamemp_ci=p5b18
label define tamemp_ci 1 "1 persona" 2 "2-5 personas" 3 "6-10 personas" ///
4 "11-30 personas" 5 "31-50 personas" 6 "51-100 personas" 7"101 y más personas" 
label var tamemp_ci "# empleados en la empresa de la actividad principal"

*/
gen tamemp_ci = 1 if p5b18>=1 & p5b18<=2
replace tamemp_ci = 2 if (p5b18>=3 & p5b18<=5)
replace tamemp_ci = 3 if (p5b18>=6 & p5b18<=7)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((p5a05==7) & condocup_ci==3)
replace categoinac_ci = 2 if  (p5a05==6 & condocup_ci==3)
replace categoinac_ci = 3 if  (p5a05==9 & condocup_ci==3)
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

/*******************
Variables Educativas
********************/
gen aedu_ci=.
replace aedu_ci=0 if p4b08a==0
replace aedu_ci=p4b08b if p4b08a==3
replace aedu_ci=6 if p4b08b>6 & p4b08a==3

replace aedu_ci=6+p4b08b if p4b08a==4
replace aedu_ci=11 if p4b08b>5 & p4b08a==4

replace aedu_ci=6+p4b08b if p4b08a==5
replace aedu_ci=9+p4b08b if p4b08a==6
replace aedu_ci=11 if p4b08a==6 & p4b08b>2
replace aedu_ci=11+p4b08b if p4b08a==7
replace aedu_ci=11+p4b08b if p4b08a==8

/*
OLD CODE:
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(p4b08a==3 & p4b08b <6)
gen edupc_ci=(p4b09==3 & p4b08b>=6)
gen edusi_ci=(p4b08a==4 & p4b08b <6) 
gen edusc_ci=(p4b09==4 & p4b08b>=6)
gen eduui_ci=(p4b08a==8 & p4b09!=8)
gen eduuc_ci=(p4b08a==8 & p4b09==8)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.
*/

gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(p4b08a==3 & p4b08b <6)
gen edupc_ci=(p4b09==3 & p4b08b>=6)

gen edusi_ci=(p4b08a==4 & p4b08b <5) | (p4b08a==5) | (p4b08a==6 & p4b08b <2)
gen edusc_ci=(p4b08a==4 & p4b08b>=5) | (p4b08a==6 & p4b08b>=2)


gen eduui_ci=(p4b08a==8 & p4b08b <5) | (p4b08a==7 & p4b08b<5)
gen eduuc_ci=(p4b08a==8 & p4b08b>=5) | (p4b08a==7 & p4b08b>=5)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.


gen edupre_ci=(p4a01==1)

gen eduac_ci=.

gen asiste_ci=0
replace asiste_ci=1 if p4a01!=4 & edad_ci<6
replace asiste_ci=1 if p4b10==1 & edad_ci>=6

gen edupub_ci=0
replace edupub_ci=1 if p4a03==1 & edad_ci<6
replace edupub_ci=1 if p4b23<=3 & edad_ci>=6


*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=p4b11
replace pqnoasis_ci = . if  p4b11 ==99

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p4b11==2
replace pqnoasis1_ci = 2 if p4b11==3
replace pqnoasis1_ci = 3 if p4b11==8
replace pqnoasis1_ci = 4 if p4b11==6
replace pqnoasis1_ci = 5 if p4b11==4
replace pqnoasis1_ci = 6 if p4b11==5 
replace pqnoasis1_ci = 7 if p4b11==1
replace pqnoasis1_ci = 7 if p4b11==7
replace pqnoasis1_ci = 9 if p4b11==9

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************

gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo ultimo grado o curso"


*************
***tecnica_ci**
*************
gen tecnica_ci=(p4b08a==7)
label var tecnica_ci "=1 formacion terciaria tecnica"


*********
*raza_ci*
*********
/*
p212:
           1 Espa¤ol 
           2 Miskito 
           3 Sumo/Sum
           4 Ingl‚s  
           5 Otro    
           9 NR 
*/
/*gen raza_ci=.
replace raza_ci= 1 if p212 ==2 | p212 ==3
replace raza_ci= 3 if p212 ==1 | p212 ==4 | p212 ==5 |raza_ci==.

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"
*/

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if p212 ==2 | p212 ==3
replace raza_idioma_ci= 3 if p212 ==1 | p212 ==4 | p212 ==5 |raza_idioma_ci==.

label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"  
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo"

gen raza_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1 
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_idioma_ci==2 
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 




/*
**********************
*** NICARAGUA 1998 ***
**********************


/*
Parentco
1. Jefe(a) 
2. Esposa(o)/Compañera(o)  
3. Hija(o)/hijastro(a)  
4. Padres/suegros  
5. Yerno/nuera 
6. Nieto(a)/bisnieto(a) 
7. Hermano(a)/Cuñado(a) 
8. Otros parientes del jefe(a)
9. Sin parentesco 
10. Empleado domestico(a)
11. Pensionista
*/

* Variables

 rename i01 departamento
 rename i02 municipio
 rename i03 areasuperv
 rename i04a segmento
 rename i05 area
 rename i08 hogar_num
 rename i11 npers
 rename codper id_pers
 rename p202 parentco
 rename p203 sexo
 rename p204a edad
 rename p204b edad_m
 rename p205a dianac
 rename p205b mesnac
 rename p205c anonac
 rename p206 estcivil
 rename p212 idiomat
 rename p3b09a sarampion
 rename p4b06 alfab
 rename p4b08a nivel
 rename p4b08b grado
 rename p4b12a matniv
 rename p4b12b matgrado
 rename p5b12 ocupp
 rename p5b13 ramap
 rename p5b18 tamestp
 rename p5b20 categp
 rename p5c32 ocups
 rename p5c33 ramas
 rename p5c39 categs
 rename p601a hvivos
 rename p601b numhijvivos
 rename p602 embulthij5
 rename p612 qaten
 rename va03 tipoviv
 rename va04 paredes
 rename va05 piso
 rename va09 cuartviv
 rename vb13 tenencia
 rename vb18 agua
 rename vb20a distkms
 rename vb20b distmts
 rename vb28 servsani
 rename vb29 usoserv
 rename vb42 prepalim
 rename vb43 combusti
 rename vb49 telefono

** AREA

 tab area [iw=peso]

** Gender classification of the population refering to the head of the household.

 sort i00 id_pers 
 
 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2
 
 
 egen sexo_d=max(sexo_d_), by(i00) 
 
 tab sexo   [iw=peso]
 tab sexo_d [iw=peso]

 tab sexo sexo_d if parentco==1

 sort i00 id_pers 
 
**Years of education
* 6 years or more of age

/*

SECCION 4. EDUCACION PARTE B: ESCOLARIDAD - PARA PERSONAS DE 6 AÑOS Y MAS -

6.¿..... Sabe:
 1. Leer y escribir
 2. Sólo sabe leer
 3. No sabe leer ni escribir

7. ¿Dónde aprendió..... a leer?

8. ¿Cuál es el nivel de estudio y el último grado o año que .... aprobó?

0. Ninguno 			=> PREG.10
1. Preescolar 
2. Educación de Adultos 
3. Primaria 
4. Secundaria 
5. Técnico Básico 
6. Técnico Medio 
7. Técnico Superior 
8. Universitario 
*/

 gen	 anoest=0  if nivel==0 | nivel==1 
 replace anoest=1  if (nivel==3 & grado==1) | (nivel==2 & grado==1) 
 replace anoest=2  if (nivel==3 & grado==2) | (nivel==2 & grado==2) 
 replace anoest=3  if (nivel==3 & grado==3) | (nivel==2 & grado==3) 
 replace anoest=4  if (nivel==3 & grado==4)
 replace anoest=5  if (nivel==3 & grado==5)
 replace anoest=6  if (nivel==3 & grado==6)
 replace anoest=7  if (nivel==4 & grado==1) | (nivel==5 & grado==1)
 replace anoest=8  if (nivel==4 & grado==2) | (nivel==5 & grado==2)
 replace anoest=9  if (nivel==4 & grado==3) | (nivel==5 & grado==3)
 replace anoest=10 if (nivel==4 & grado==4) | (nivel==6 & grado==1)
 replace anoest=11 if (nivel==4 & grado==5) | (nivel==6 & grado==2) | (nivel==6 & grado==3)
 replace anoest=12 if (nivel==8 & grado==1) | (nivel==7 & grado==1) 
 replace anoest=13 if (nivel==8 & grado==2) | (nivel==7 & grado==2) 
 replace anoest=14 if (nivel==8 & grado==3) | (nivel==7 & grado==3) 
 replace anoest=15 if (nivel==8 & grado==4) 
 replace anoest=16 if (nivel==8 & grado==5) 
 replace anoest=17 if (nivel==8 & grado==6) 

 replace anoest=99 if edad<7
 
 
** Economic Active Population 

 gen     peaa=1 if (p5a01==1 | p5a02==1 | p5a03==1) | (ocupp>=110 & ocupp<=9411)
 replace peaa=2 if (p5a04==1 |((p5a05>=1 & p5a05<=4) | p5a05==12 | p5a05==13))
 replace peaa=3 if peaa==. & edad>=6
 replace peaa=0 if edad<6

 gen	 tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 


************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*
MATRÍCULA ACTUAL

p4b10
10. ¿Se matriculó ..... en el presente año escolar en: Preescolar, Educación de
Adultos, Primaria, Secundaria, Universidad, PostGrado?
 1.Si => 12
 2.No => Mayores de 40 años pasar a 47
	 Menores de 40 pasar a 11
	 
11. ¿Por qué razón no asiste ..... en el presente año escolar?


matniv		matgrado
12. ¿En qué nivel educativo y grado o año se matriculó ...... en el presente año escolar?
 1. Preescolar  
 2. Educación de Adultos 
 3. Primaria  
 4. Secundaria  
 5. Técnico Básico  
 6. Técnico Medio  
 7. Técnico Superior
 8. Universitario
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen     NERP=0 if (edad>=7 & edad<=12) & (p4b10==1 | p4b10==2)
 replace NERP=1 if (edad>=7 & edad<=12) & matniv==3
	
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen     NERS=0 if (edad>=13 & edad<=17) & (p4b10==1 | p4b10==2)
 replace NERS=1 if (edad>=13 & edad<=17) & matniv==4 

** Upper secondary
* Educación Secundaria: Ciclo diversificado (4o Magisterio - 5o Bachillerato)

 gen     NERS2=0 if (edad>=16 & edad<=17) & (p4b10==1 | p4b10==2)
 replace NERS2=1 if (edad>=16 & edad<=17) & (matniv==4) & (matgrado>=4 & matgrado<=5)
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & Write

 gen     LIT2=0 if (edad>=15 & edad<=24) & (alfab>=1 & alfab<=3)
 replace LIT2=1 if (edad>=15 & edad<=24) & (alfab==1)
	
*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  matniv==3
 gen sec=1  if  matniv==4
 gen ter=1  if  matniv==8

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  


** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfab==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfab==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen     RATIOLIT2=0 if ((alfab==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfab==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* Without domestic Service

 gen     WENAS=0 if (edad>=15 & edad<=64) & (categp==1 | categp==2) & (ramap>=1320  & ramap<=9800) & peaa==1 & ocupp!=9131
 replace WENAS=1 if (edad>=15 & edad<=64) & (categp==1 | categp==2) & (ramap>=1320  & ramap<=9800) & peaa==1 & ocupp!=9131 & sexo==2 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	  WENASD=0 if (edad>=15 & edad<=64) & (categp==1 | categp==2) & (ramap>=1320  & ramap<=9800) & peaa==1
 replace  WENASD=1 if (edad>=15 & edad<=64) & (categp==1 | categp==2) & (ramap>=1320  & ramap<=9800) & peaa==1 & sexo==2

*Proportion of 1 to 4 Year Old Children Immunized Against Measles*

 gen	 MEASLES=0 if (edad>=1 & edad<=4) & (sarampion==1 | sarampion==2)
 replace MEASLES=1 if (edad>=1 & edad<=4) & (sarampion==1)
	
*Proportion of Births Attended by Skilled Health Personnel*
* 15 to 49 years of age

 gen	 SKILLED=0 if (qaten>=1  & qaten<=4)
 replace SKILLED=1 if (qaten>=1 & qaten<=3)

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

/*
38. ¿Com qué tipo de alumbrado cuenta principalmente este hogar?
 1. Energía eléctrica
 2. Planta generador eléctrico
 3. Gas, kerosene, candil
 4. Otro
 5. ninguno
*/

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (vb38>=1 & vb38<=5) /* Total population excluding missing information */
 replace ELEC=1 if (vb38==1 | vb38==2)
 	
** Target 10, Indicator: Proportion of the population using solidfuels (%)
/*
combusti (vb43)
43. ¿Qué combustible utilizan usualmente
para cocinar?
 1. Leña
 2. Gas butano o propano
 3. Gas o kerosene
 4. Carbón
 5. Electricidad
 6. Otro
*/

* Gender classification of the population refers to the head of the household.

 gen	 SFUELS=0 if (combusti>=1 & combusti<=6)  /* Total population excluding missing information */
 replace SFUELS=1 if (combusti==1 | combusti==4)
 	 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
18. De dónde obtiene principalmente agua este hogar
 1. Tubería dentro de la vivienda				==> 19
 2. Tubería fuera de la vivienda, pero dentro del terreno	==> 19
 3. Puesto público						==> 20
 4. Pozo público o privado					==> 20
 5. Río, manantial o quebrada					==> 20
 6. Camión, carreta o pipa					==> 20
 7. De otra vivienda						==> 20
 8. Otro							==> 20	

19. ¿El suministro de agua es de uso exclusivo del hogar?

20. ¿A qué distancia de su vivienda se encuentra la fuente donde obtiene el
agua y cuánto tiempo utiliza en llegar?

DISTANCIA /___/___/KMS. /___ /___/___/ MTS.
TIEMPO /___/___/HRS. /___/___/MINUT.
distkms vb20a 
distmts vb20b 

21. Paga este hogar por el agua que consumen:
22. ¿Cuánto pagó el mes pasado o la última vez por el agua que consumió?
23. Qué tratamiento aplican principalmente al agua para beber:
 1. Tal como la obtiene
 2. La hierven
 3. La cloran
 4. Compran agua purificada
 5. Otro

24. Desde 1993, ¿Ud. ha obtenido el agua del mismo lugar?

25. En 1993, de dónde obtenía principalmente el agua este hogar:

26. El agua del hogar era de uso exclusivo

27. En 1993, ¿Qué distancia recorría y qué tiempo le llevaba para obtener el agua?
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (agua>=1 & agua<=8)  /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4) | agua==7
 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
servsani (vb28)
28. ¿Con qué tipo de servicio higiénico cuenta este hogar?
 1. Excusado o letrina
INODORO
 2. Conectado a tubería de aguas negras
 3. Conectado a sumidero o pozo séptico
 4. Que descarga en el río o quebrada
 5. No tiene				==> 31

usoserv (vb29)
29. ¿El servicio higiénico es de uso de su hogar?
*/
 	
	*** The variables are not comparable with the 2001 survey, or with the 1993 survey.

** Dirt floors

* Piso 5=Tierra
* Solo para el hogar principal

 gen id_viv_=1 if hogar_num==1 & parentco==1
 gen id_viv=sum(id_viv_)

 egen piso_d=max(piso), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso_d>=1 & piso_d<=6)
 replace DIRT=1 if (piso_d==5)
	
** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if  (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
 replace UNMPLYMENT15=1 if  (edad>=15 & edad<=24) & (tasadeso==1 )

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
/*
49. Pagan en este hogar por concepto servicio telefónico:
 1. Si  
 2. Si, con el alquiler  
 3. No paga  
 4. No tiene teléfono  
*/

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (telefono>=1 & telefono<=4) /* Total population excluding missing information */
 replace TELCEL=1 if  (telefono>=1 & telefono<=3)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono>=1 & telefono<=4) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1 | telefono==3)

*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen	 CHILDREN=0 if  (edad>=12 & edad<=14) 
 replace CHILDREN=1 if  (edad>=12 & edad<=14) & (peaa==1)

** CCA 41 Number of Persons per Room*

/*
cuartviv
9. ¿Cuántos cuartos tiene en total esta
vivienda? (No incluya cocina, baños,
pasillos y garage)

vb10
10. ¿De cuántos cuartos dispone este hogar?
(No incluya cocina, baños, pasillos,
garajes y los cuartos dedicados a
trabajo o negocios)
*/

 gen persroom=npers/vb10 
 
 gen PERSROOM2=persroom if parentco==1

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
	
*Disconnected Youths 15 to 24 years olds

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (p5a05==7 | p5a05==11 |p5a05==12 | p5a05==13 | p5a05==15)

******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

*** Proportion of population below corresponding grade for age

 gen     rezago=0       if (anoest>=0 & anoest<99)  & edad==7 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==8
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==8
 
 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==9
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==10
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==11
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==11

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==12
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==12

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==13
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==14
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==15
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==16
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==17
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if  (edad>=8 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if  (edad>=8 & edad<=17) & (rezago==1)

* Primary completion rate [15 - 24 years of age]

 global variable PRIMCOMP

* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))

 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=17) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=17) & (anoest>=0 & anoest<99)



*/

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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



compress


saveold "`base_out'", replace


log close
