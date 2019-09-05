
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
local ANO "2001"
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

Change in the variable aedu because secundary education lasts 5 years in 
Nicaragua. 

Code for education dummies was changed beacuse there were mistakes in it. 
Old code can be seen in education section below. 
**/

/*****************
Variables de Hogar
******************/
gen factor_ch=peso2
gen idh_ch=id_hogar
gen idp_ci=s2p00
gen zona_c=i05
replace zona_c=0 if i05==2
gen pais_c="NIC"
gen anio_c=2001
gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" | pais=="PAN" | pais=="DOM" | pais=="CRI" | pais=="BLZ" | pais=="GTM" | pais=="SLV" | pais=="HON" | pais=="NIC"
replace region_BID_c=2 if pais=="BAH" | pais=="BAR" | pais=="GUY" | pais=="JAM" | pais=="SUR" | pais=="TOT"
replace region_BID_c=3 if pais=="ECU" | pais=="COL" | pais=="PER" | pais=="VEN" | pais=="BOL" 
replace region_BID_c=4 if pais=="ARG" | pais=="BRA" | pais=="CHL" | pais=="PRY" | pais=="URU" 
replace region_BID_c=5 if pais=="HAI"
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
gen mes_c=mesr1
gen relacion_ci=s2p2
replace relacion_ci=4 if s2p2>=5 & s2p2<=8
replace relacion_ci=5 if s2p2==9 | s2p2==11
replace relacion_ci=6 if s2p2==10

/*************************************
Variables de Infraestructura del Hogar
**************************************/
gen aguared_ch=(s1p20==1 | s1p20==2) /*Esta definido como acceso a agua de red!*/
gen aguadist_ch=0
replace aguadist_ch=1 if s1p20==1
replace aguadist_ch=2 if s1p20==2 & s1p23b<=100
replace aguadist_ch=3 if s1p20==2 & s1p23b>100
gen aguamala_ch=(s1p20==5 | s1p20==6)
gen aguamide_ch=(s1p26==1)
gen luz_ch=(s1p38==1)
gen luzmide_ch=(s1p39==1)
gen cocina_ch=(s1p42==1)
gen combust_ch=(s1p43==2 | s1p43==3 | s1p43==5)
gen bano_ch=(s1p29!=6) /*Esta definido como si posee servicio higienico: inodoro o letrina*/
gen banoex_ch=(s1p31==1)
replace banoex_ch=. if bano_ch==0
gen des1_ch=0
replace des1_ch=1 if s1p29==3 | s1p29==4 /*Red o pozo septico*/
replace des1_ch=2 if s1p29==1 | s1p29==2 /*Pozo comun  - Letrina */
replace des1_ch=3 if s1p29==5 /*Superficie*/

gen des2_ch=0
replace des2_ch=1 if s1p29==3 | s1p29==4 /*Red o pozo septico*/
replace des2_ch=2 if s1p29==1 | s1p29==2 /*Pozo comun  - Letrina */
gen dessup=(s1p29==5) /*Superficie*/

gen piso_ch=(s1p6==1 | s1p6==2 | s1p6==4)
replace piso_ch=2 if s1p6==3 | s1p6==6 /*Vale 0 si es piso de tierra, 1 si es permanente y 2 si es otros (o de ladrillo de barro)*/

gen pared_ch=(s1p5==1 | s1p5==2 |s1p5==4 |s1p5==6 |s1p5==7 |s1p5==8)
replace pared_ch=2 if s1p5==10 /*Otros*/

gen techo_ch=(s1p7==1 | s1p7==3)
replace techo_ch=2 if s1p7==6 /*Otros*/

gen resid_ch=0 /*Recoleccion*/
replace resid_ch=1 if s1p35==2 | s1p35==3 /*Quemada o enterrada*/
replace resid_ch=2 if s1p35==4 /*Tirada*/
replace resid_ch =3 if s1p35!=1 & s1p35!=2 & s1p35!=3 & s1p35!=4 /*"Otros: llevada al lugar"*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .

* Modificaciones Marcela Rubio Septiembre 2014

/*
gen dorm_ch=s1p13
gen cuartos_ch = s1p14
*/
gen dorm_ch=s1p14
gen cuartos_ch = s1p13


gen vivi1_ch=(s1p3==1) /*Casa*/
replace vivi1_ch=2 if s1p3==2/*Departamento*/
replace vivi1_ch=3 if vivi1_ch==0

gen vivi2_ch = (s1p3==1|s1p3==2)

gen viviprop_ch = 0 /*Alquilada*/
replace viviprop_ch = 1 if s1p16==1 | s1p16==2 /*Propia*/
replace viviprop_ch = 2 if s1p16==3 /*Pagando*/
replace viviprop_ch = 4 if s1p16!=1 & s1p16!=2 & s1p16!=3 & s1p16!=4

gen vivitit_ch = (s1p16==1) /*Con titulo que certifique la propiedad de la vivienda*/

gen vivialq_ch= s1p17
gen vivialqimp_ch= s1p19
gen telef_ch=(s1p51==1)
gen cel_ch=(s1p51==2)

**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"

/*Los bienes durables (refrigerador, computadora y auto) ya fueron creados en el do "Primer Arreglo"*/
****************
**internet_ch***
****************

gen internet_ch=.


/*********************
Variables Demograficas
*********************/
************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013	
gen region_c= i01

label define region_c  ///
          5  "Nueva Segovia" ///
          10 "Jinotega" ///
          20 "Madriz" ///
          25 "Estelí" ///
          30 "Chinandega" ///
          35 "León" ///
          40 "Matagalpa" ///
          50 "Boaco" ///
          55 "Managua" ///
          60 "Masaya" ///
          65 "Chontales" ///
          70 "Granada" ///
          75 "Carazo" ///
          80 "Rivas" ///
          85 "Río San Juan" ///
          91 "Raan" ///
          93 "Raas"
          
label value region_c region_c
label var region_c "División política, departamentos"
gen factor_ci=peso2
gen sexo_ci=s2p3
gen edad_ci=s2p4
gen civil_ci=1 if s2p6==6 | s2p6==7
replace civil_ci=2 if s2p6==1 | s2p6==2
replace civil_ci=3 if s2p6==3 | s2p6==4
replace civil_ci=4 if s2p6==5 
gen jefe_ci=(s2p2==1)
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
by idh_ch:egen byte nmiembros_ch=sum((relacion_ci>0 & relacion_ci<5)|s2p2==9) if relacion_ci~=6 
by idh_ch:egen byte nmayor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci<1)) /*Hay que tener en cuenta que en 
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
gen no_trab=(s5p1==2 & s5p2==2 & s5p3==2)

gen condocup_ci=.
replace condocup_ci=1 if s5p1==1 | s5p2==1 | s5p3==1 
replace condocup_ci=2 if no_trab==1 & (s5p5==1 | s5p5==2) /*no se preguunta si busco en las 4 ultimas semanas. Solo la semana anterior*/
replace condocup_ci=3 if s5p6>=6 & s5p6<=11
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci1
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
drop no_trab
*/

* Alternativa 2 con variables originales tomando en cuenta la definicion de armonizacion MGD 06/05/2014
gen condocup_ci=.
replace condocup_ci=1 if s5p1==1 | s5p2==1 | s5p3==1
replace condocup_ci=2 if condocup_ci!=1 & (s5p5==1 | s5p5==2)
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
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (s5p33a==1 | s5p68a==1) & cotizando_ci==0 /*solo a ocupados*/
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
*gen pension_ci=(pensionj>=1 & pensionj!=.)  

*Modificación Mayra Sáenz - Septiembre 2014
g pension_ci=(s5p6==7 | (pensionj>=1 & pensionj!=.))
label var pension_ci "1=Recibe pension contributiva"

*************
*   ypen_ci *
*************
gen ypen_ci=pensionj if  pension_ci==1
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


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
replace cesante_ci=1 if s5p9==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =linea_ge
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =linea_ex 
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

gen rama_ci=.
replace rama_ci=1 if (s5p14>=111 & s5p14<=500) & emp_ci==1
replace rama_ci=2 if (s5p14>=1010 & s5p14<=1429) & emp_ci==1
replace rama_ci=3 if (s5p14>=1510 & s5p14<=3720) & emp_ci==1
replace rama_ci=4 if (s5p14>=4010 & s5p14<=4100) & emp_ci==1
replace rama_ci=5 if (s5p14>=4510 & s5p14<=4550) & emp_ci==1
replace rama_ci=6 if (s5p14>=5010 & s5p14<=5520) & emp_ci==1
replace rama_ci=7 if (s5p14>=6010 & s5p14<=6420) & emp_ci==1
replace rama_ci=8 if (s5p14>=6511 & s5p14<=7020) & emp_ci==1
replace rama_ci=9 if (s5p14>=7111 & s5p14<=9800) & emp_ci==1


*************
**salmm_ci***
*************
generat salmm_ci=.
replace salmm_ci=550+785/2 if rama_ci==1
replace salmm_ci=950       if rama_ci==2
replace salmm_ci=670+895/2 if rama_ci==3
replace salmm_ci=1010      if rama_ci==4  
replace salmm_ci=1300      if rama_ci==5 
replace salmm_ci=1010      if rama_ci==6 | rama_ci==7
replace salmm_ci=1120      if rama_ci==8
replace salmm_ci=785+630/2 if rama_ci==9
replace salmm_ci=(550+785+950+670+895+1010+1010+1300+1010+1120+785+630)/12  if salmm_ci==. 
label var salmm_ci "Salario minimo legal"


gen ocupa_ci=.
replace ocupa_ci=1 if real(substr(string(s5p13),1,1))==2 | real(substr(string(s5p13),1,1))==3
replace ocupa_ci=2 if real(substr(string(s5p13),1,1))==1
replace ocupa_ci=3 if real(substr(string(s5p13),1,1))==4
replace ocupa_ci=4 if real(substr(string(s5p13),1,2))==52
replace ocupa_ci=5 if real(substr(string(s5p13),1,2))==51
replace ocupa_ci=6 if real(substr(string(s5p13),1,1))==6
replace ocupa_ci=7 if real(substr(string(s5p13),1,1))==7 | real(substr(string(s5p13),1,1))==8
replace ocupa_ci=8 if real(substr(string(s5p13),1,1))==0
replace ocupa_ci=9 if real(substr(string(s5p13),1,1))==9
replace ocupa_ci=. if emp_ci==0


gen horaspri_ci=s5p18
replace horaspri_ci=. if emp_ci==0 | s5p18>168

gen horastot_ci=s5p47
replace horastot_ci=. if emp_ci==0

gen salario=s5p20a*22 if s5p20b==1 
replace salario=s5p20a*4.3 if s5p20b==2
replace salario=s5p20a*2 if s5p20b==3 | s5p20b==4
replace salario=s5p20a if s5p20b==5
replace salario=s5p20a/3 if s5p20b==6
replace salario=s5p20a/6 if s5p20b==7
replace salario=s5p20a/12 if s5p20b==8
replace salario=. if emp_ci==0 
gen agui=s5p24b/12 /*El aguinaldo esta preguntado con frecuencia anual*/
egen ylmpri_ci=rsum(salario s5p23b agui)
replace ylmpri_ci=. if (salario==. & s5p23b ==. & s5p24b==.) |emp_ci==0 /*INCLUYE LO MISMO QUE INCLUYE EL YLMPRI_CI DEL '93*/
drop salario agui


gen unif=s5p27b/3 if s5p27c==4
replace unif=s5p27b/4 if s5p27c==3
replace unif=s5p27b/6 if s5p27c==2
replace unif=s5p27b/12 if s5p27c==1
replace unif=. if emp_ci==0 | s5p27b==.

* Modificaciones Marcela Rubio - Diciembre 2014: corrigiendo outliers

replace s5p28b=. if s5p28b>3000
replace s5p26b=. if s5p26b>5000

egen ylnmpri_ci=rsum(s5p25b s5p26b unif s5p28b )
replace ylnmpri_ci=. if (s5p25b==. & s5p26b==. & unif==. & s5p28b==.) | emp_ci==0

gen salariosec=s5p41a*22 if s5p41b==1 
replace salariosec=s5p41a*4.3 if s5p41b==2
replace salariosec=s5p41a*2 if s5p41b==3 | s5p41b==4
replace salariosec=s5p41a if s5p41b==5
replace salariosec=s5p41a/3 if s5p41b==6
replace salariosec=s5p41a/6 if s5p41b==7
replace salariosec=s5p41a/12 if s5p41b==8
replace salariosec=. if emp_ci==0 
gen aguis=s5p45b/12 /*El aguinaldo esta preguntado con frecuencia anual*/
egen ylmsec_ci=rsum(salariosec s5p44b agui)
replace ylmsec_ci=. if (salariosec==. & s5p44b ==. & s5p45b==.) |emp_ci==0 
drop salariosec

* Modificaciones Marcela Rubio - Diciembre 2014: corrigiendo outliers

/*
gen ylnmsec_ci=s5p46b 
replace ylnmsec_ci=. if emp_ci==0
*/

gen ylnmsec_ci=s5p46b if s5p46b<=4500
replace ylnmsec_ci=. if emp_ci==0

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1) /*ylmpri tiene valores o missing, no 0. COmo estamos interesados en las no respuestas, no fijamos en
quienes contestaron con .*/
replace nrylmpri_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.  




gen ylmotros_ci=.
gen ylnmotros_ci = .

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if s2p2<10
by idh_ch: egen ylm_ch=sum(ylm_ci)if s2p2<10
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if s2p2<10

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

gen ylmnr_ch=ylm_ch
replace ylmnr=. if nrylmpri_ch==1

foreach var of varlist intaho_ch intprest_ch indem_ch cesant_ch divid_ch loter_ch comp_ch donac_ch heren_ch otrosi_ch{
gen `var'mo=`var'/12
} /*Todas estas variables estan preguntadas con una frecuencia anual*/
gen remesasintc_c    =dinintc_ch*22      if frecdiin_ch==1
replace remesasintc_c=dinintc_ch*4.3   if frecdiin_ch==2
replace remesasintc_c=dinintc_ch*2   if frecdiin_ch==3
replace remesasintc_c=dinintc_ch     if frecdiin_ch==4
replace remesasintc_c=dinintc_ch/3   if frecdiin_ch==5
replace remesasintc_c=dinintc_ch/6   if frecdiin_ch==6
replace remesasintc_c=dinintc_ch/12  if frecdiin_ch==7

gen dinerointc_d=dinintd_ch*13.372 /*Tipo de cambio promedio para el 2001*/
gen remesasintc_d=dinerointc_d*22     if frecdiin_ch==1
replace remesasintc_d=dinerointc_d*4.3  if frecdiin_ch==2
replace remesasintc_d=dinerointc_d*2  if frecdiin_ch==3
replace remesasintc_d=dinerointc_d    if frecdiin_ch==4
replace remesasintc_d=dinerointc_d/3  if frecdiin_ch==5
replace remesasintc_d=dinerointc_d/6  if frecdiin_ch==6
replace remesasintc_d=dinerointc_d/12 if frecdiin_ch==7
gen remesasextc_c=dinextc_ch*22       if frecdiex_ch==1
replace remesasextc_c=dinextc_ch*4.3    if frecdiex_ch==2
replace remesasextc_c=dinextc_ch*2    if frecdiex_ch==3
replace remesasextc_c=dinextc_ch      if frecdiex_ch==4
replace remesasextc_c=dinextc_ch/3    if frecdiex_ch==5
replace remesasextc_c=dinextc_ch/6    if frecdiex_ch==6
replace remesasextc_c=dinextc_ch/12   if frecdiex_ch==7
gen dineroextc_d=dinextd_ch*13.372 /*Tipo de cambio promedio para el 2001*/
gen remesasextc_d=dineroextc_d*22 if frecdiex_ch==1
replace remesasextc_d=dineroextc_d*4.3 if frecdiex_ch==2
replace remesasextc_d=dineroextc_d*2 if frecdiex_ch==3
replace remesasextc_d=dineroextc_d if frecdiex_ch==4
replace remesasextc_d=dineroextc_d/3 if frecdiex_ch==5
replace remesasextc_d=dineroextc_d/6 if frecdiex_ch==6
replace remesasextc_d=dineroextc_d/12 if frecdiex_ch==7
egen ynlm_ch=rsum(alquilerc_ch alquilerv_ch becas_ch ayudas_ch pensiona_ch pensionj_ch pensiono_ch *mo *c_c *c_d)
/*Yo cree estas variables para que no tengas missing, asi que esta variable -ynlm- va a tener 0 en vez de .*/
drop alquilerc_ch alquilerv_ch becas_ch ayudas_ch pensiona_ch pensionj_ch pensiono_ch *mo

gen bienesintc_c=regintc_ch*22 if frecregint_ch==1
replace bienesintc_c=regintc_ch*4.3 if frecregint_ch==2
replace bienesintc_c=regintc_ch*2 if frecregint_ch==3
replace bienesintc_c=regintc_ch if frecregint_ch==4
replace bienesintc_c=regintc_ch/3 if frecregint_ch==5
replace bienesintc_c=regintc_ch/6 if frecregint_ch==6
replace bienesintc_c=regintc_ch/12 if frecregint_ch==7
gen regintc_d=regintd_ch*13.372 /*Tipo de cambio promedio para el 2001*/
gen bienesintc_d=regintc_d*22 if frecregint_ch==1
replace bienesintc_d=regintc_d*4.3 if frecregint_ch==2
replace bienesintc_d=regintc_d*2 if frecregint_ch==3
replace bienesintc_d=regintc_d if frecregint_ch==4
replace bienesintc_d=regintc_d/3 if frecregint_ch==5
replace bienesintc_d=regintc_d/6 if frecregint_ch==6
replace bienesintc_d=regintc_d/12 if frecregint_ch==7
gen bienesextc_c=regexc_ch*22 if frecregex_ch==1
replace bienesextc_c=regexc_ch*4.3 if frecregex_ch==2
replace bienesextc_c=regexc_ch*2 if frecregex_ch==3
replace bienesextc_c=regexc_ch if frecregex_ch==4
replace bienesextc_c=regexc_ch/3 if frecregex_ch==5
replace bienesextc_c=regexc_ch/6 if frecregex_ch==6
replace bienesextc_c=regexc_ch/12 if frecregex_ch==7
gen regexc_d=regexd_ch*13.372 /*Tipo de cambio promedio para el 2001*/
gen bienesextc_d=regexc_d*22 if frecregex_ch==1
replace bienesextc_d=regexc_d*4.3 if frecregex_ch==2
replace bienesextc_d=regexc_d*2 if frecregex_ch==3
replace bienesextc_d=regexc_d if frecregex_ch==4
replace bienesextc_d=regexc_d/3 if frecregex_ch==5
replace bienesextc_d=regexc_d/6 if frecregex_ch==6
replace bienesextc_d=regexc_d/12 if frecregex_ch==7

/*
gen ynlm_ci=. /*EL ingreso no laboral esta calculado a nivel hogar, no a nivel persona*/
*/
* Se genera como el promedio entre los miembros del hogar  para mantener metodología de 1998.
by idh_ch: egen nper = sum(miembros_ci) if miembros_ci==1
by idh_ch: gen ynlm_ci=ynlm_ch/nper if miembros_ci==1

* Modificacion Marcela Rubio - Octubre 2014: se generan como missing para consistencia con años anteriores
/*
egen ynlnm_ch=rsum(bienes*)
gen ynlnm_ci=ynlnm_ch/nmiembros_ch
*/
gen ynlnm_ch=.
gen ynlnm_ci=.

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen rentaimp_ch=s1p19

gen autocons_ci=.
gen autocons_ch=s8p27b
gen remesas_ci=.
egen remesas_ch=rsum(bienesext* remesasext*) /*Solo toma en cuenta las remesas del exterior*/
/*drop *c_c *c_d*/


gen durades_ci=s5p8/4.3

gen antiguedad_ci=s5p15a if s5p15b==4
replace antiguedad_ci=s5p15a /12 if s5p15b==3
replace antiguedad_ci=s5p15a /48 if s5p15b==2
replace antiguedad_ci=s5p15a /365 if s5p15b==1
replace antiguedad_ci=. if emp_ci==0



/****************************
Variables del Mercado Laboral
****************************/
gen desalent_ci=(s5p6==12 | s5p6==13)
replace desalent_ci=. if emp_ci==1

gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30 & s5p49==1)

gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & s5p49==2)
replace tiempoparc_ci=. if emp_ci==0

gen categopri_ci=.
replace categopri_ci=1 if s5p22==4
replace categopri_ci=2 if s5p22==3
replace categopri_ci=3 if s5p22==1 | s5p22==2 | s5p22==5
replace categopri_ci=4 if s5p22==6
replace categopri_ci=. if emp_ci==0

gen categosec_ci=.
replace categosec_ci=1 if s5p43==4
replace categosec_ci=2 if s5p43==3
replace categosec_ci=3 if s5p43==1 | s5p43==2 | s5p43==5
replace categosec_ci=4 if s5p43==6
replace categosec_ci=. if emp_ci==0

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

gen segsoc_ci=(s5p33a==1)
replace segsoc_ci=. if emp_ci==0

gen nempleos_ci=1 if s5p34==2
replace nempleos_ci=2 if s5p34==1
replace nempleos_ci=. if emp_ci==0
/*
gen firmapeq_ci=1 if s5p19<=3
replace firmapeq_ci=0 if s5p19>3 & s5p19<.
replace firmapeq_ci=. if emp_ci==0
*/
gen spublico_ci=(s5p21==1 | s5p21==2 | s5p21==5)

*************
*tamemp_ci
*************
*Nicaragua Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50
/*
gen tamemp_ci=s5p19 
replace tamemp_ci=. if s5p19==9
label define tamemp_ci 1 "1 persona" 2 "2-4 personas" 3 "5 personas" ///
4 "6-10 personas" 5 "11-30 personas" 6 "31-50 personas" 7"51-100 personas" 8"101 y más"
label var tamemp_ci "# empleados en la empresa de la actividad principal"

*/
gen tamemp_ci = 1 if s5p19>=1 & s5p19<=3
replace tamemp_ci = 2 if (s5p19>=4 & s5p19<=6)
replace tamemp_ci = 3 if (s5p19>=7 & s5p19<=8)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((s5p6==7) & condocup_ci==3)
replace categoinac_ci = 2 if  (s5p6==6 & condocup_ci==3)
replace categoinac_ci = 3 if  (s5p6==9 & condocup_ci==3)
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
replace aedu_ci=0 if s4p17a==0
replace aedu_ci=s4p17b if s4p17a==3
replace aedu_ci=6 if s4p17b>6 & s4p17a==3

replace aedu_ci=6+s4p17b if s4p17a==4
replace aedu_ci=11 if s4p17b>5 & s4p17a==4

replace aedu_ci=6+s4p17b if s4p17a==5
replace aedu_ci=9+s4p17b if s4p17a==6
replace aedu_ci=11 if s4p17a==6 & s4p17b >2

replace aedu_ci=11+s4p17b if s4p17a>=7 & s4p17a<=9
replace aedu_ci=16+s4p17b if s4p17a>=10 /*Considerando que la carrera universitaria toma 5 años en promedio*/

/* OLD CODE
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(s4p17a==3 & s4p17b<6)
gen edupc_ci=(s4p18==3 & s4p17b>=6)
gen edusi_ci=(s4p17a==4 & s4p17b<6) 
gen edusc_ci=(s4p18==4)
gen eduui_ci=(s4p17a==9 & s4p18!=8)
gen eduuc_ci=((s4p17a==9 & s4p18==9) | s4p17a==10 | s4p17a==11|s4p18==10 | s4p18==11)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.
*/

gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(s4p17a==3 & s4p17b<6)
gen edupc_ci=(s4p17a==3 & s4p17b>=6)

gen edusi_ci=(s4p17a==4 & s4p17b<5) | (s4p17a==5) | (s4p17a==6 & s4p17b<2)
gen edusc_ci=(s4p17a==4 & s4p17b>=5) | (s4p17a==6 & s4p17b>=2)


gen eduui_ci=(s4p17a==7 & s4p17b<5) | (s4p17a==8 & s4p17b<5) | (s4p17a==9 & s4p17b<5) 
gen eduuc_ci=(s4p17a==7 & s4p17b>=5) | (s4p17a==8 & s4p17b>=5) | (s4p17a==9 & s4p17b>=5) | (s4p17a>=10)

gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.

gen edupre_ci=(s4p2==3)

gen eduac_ci=.

gen asiste_ci=0
replace asiste_ci=1 if s4p2!=5 & s4p2!=6 & edad_ci<7
replace asiste_ci=1 if s4p19==1 & edad_ci>=7


gen edupub_ci=0
replace edupub_ci=1 if (s4p4==1 | s4p4==2) & edad_ci<6
replace edupub_ci=1 if s4p35<=3 & edad_ci>=6

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=s4p31
replace pqnoasis_ci = . if  s4p31 ==99

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if s4p31==11
replace pqnoasis1_ci = 2 if s4p31==4
replace pqnoasis1_ci = 3 if s4p31==2 | s4p31==10
replace pqnoasis1_ci = 4 if s4p31==12
replace pqnoasis1_ci = 5 if s4p31==3 | s4p31==9
replace pqnoasis1_ci = 8 if s4p31==5 | s4p31==7
replace pqnoasis1_ci = 9 if s4p31==1 | s4p31==6 | s4p31==8 | s4p31==13 | s4p31==14
replace pqnoasis1_ci = 7 if s4p31==7
replace pqnoasis1_ci = 9 if s4p31==9

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
*Mayra Sáenz - Septiembre 2013: La pregunta acerca de repite sólo hace referencia al último año.
*Por lo tanto, se utiliza esta variable para generar repiteul_ci
gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************
* es primera vez que se matriculo en este grado o año
gen repiteult_ci=(s4p29a==2)
label variable repiteult_ci "Esta repitendo ultimo grado o curso"


*************
***tecnica_ci**
*************
gen tecnica_ci=(s4p17a==8)
label var tecnica_ci "=1 formacion terciaria tecnica"	
*********
*raza_ci*
*********
/*
s2p10:
           1 mestizo del pacifico
           2 mestizo coste¤o
           3 blanco
           4 criollo
           5 creole/negro
           6 miskito
           7 mayagna(sumu)
           8 rama
           9 otro,cual
          10 no sabe
          99 ignorado
*/

/*
gen raza_ci=.
replace raza_ci= 1 if  s2p10 ==2| s2p10 ==6 | s2p10 ==7 | s2p10 ==8
replace raza_ci= 2 if s2p10 ==4 | s2p10 ==5
replace raza_ci= 3 if s2p10 ==1 | s2p10 ==3 | s2p10 ==9|raza_ci==.

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"*/


*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_ci=.
replace raza_ci= 1 if  s2p10 ==6 | s2p10 ==7 | s2p10 ==8
replace raza_ci= 2 if s2p10 ==4 | s2p10 ==5
replace raza_ci= 3 if s2p10 ==1 | s2p10 ==2 | s2p10 ==3 | s2p10 ==9|raza_ci==.

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"  
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"

gen raza_idioma_ci=.

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





/*

**********************
*** NICARAGUA 2001 ***
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
10. Trabajador domestico(a)
*/

* Variables

 rename i05 area
 rename s2p2 parentco
 rename s2p3 sexo
 rename s2p4 edad
 rename s4p17a ultcurso
 rename s4p17b ultgrado
 rename s4p21a  matricurso
 rename s5p14 rama
 rename s5p22 categ
 rename s5p13 ocup
 rename s1p43 combusti
 rename s1p20 agua
 rename s1p23a distagua1
 rename s1p23b distagua2
 rename s1p23c distagua3
 rename s1p31 servexc
 rename i09 numper
 rename s1p13 numcuar
 rename s1p3 tipoviv
 rename s1p16 tenenviv
 rename s1p5 paredes
 rename s1p6 piso
 rename s1p51 servtel
 rename s5p19 tamest
 rename s5p21 sector

** AREA

 tab area [iw=peso2]

** Gender classification of the population refering to the head of the household.

 sort i00* s2p00 
 
 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2
 
 
 egen sexo_d=max(sexo_d_), by(idh_ch) /* 20 households without h.head */
 
 tab sexo   [iw=peso2]
 tab sexo_d [iw=peso2]

 tab sexo sexo_d if parentco==1

 sort idh_ch s2p00 

**Years of education
* 6 years or more of age

SECCIÓN 4. EDUCACIÓN PARTE B. -ESCOLARIDAD - PARA PERSONAS DE 7 AÑOS Y MAS

ALFABETISMO

16 ..... Sabe:
 1. Leer y escribir
 2. Sólo sabe leer
 3. No sabe leer ni escribir
 
NIVEL EDUCATIVO

17. ¿Cuál es el nivel de estudio y el último grado o año que ..... aprobó?

ultcurso (s4p17a)	ultgrado(s4p17b)
 0. Ninguno ==>19
 1. Preescolar
 2. Educación de adultos
 3. Primaria
 4. Secundaria
 5. Técnico básico
 6. Técnico medio
 7. Formación docente
 8. Técnico superior
 9. Universitario
 10. Maestría
 11. Doctorado

18. ¿Cuál es el diploma, certificado o título más alto que obtuvo ..... ?

 0. Ninguno
 1. Preescolar
 2. Educación de adultos
 3. Primaria
 4. Secundaria
 5. Técnico básico
 6. Técnico medio
 7. Formación docente
 8. Técnico superior
 9. Universitario
 10. Postgrado
 11. Maestría
 12. Doctorado

MATRÍCULA ACTUAL

19. ¿Se matriculó ..... en el presente año escolar, en el sistema de educación formal?
 1.Si => 21
 2.No => Mayores de 40 años pasar a 47
 
20. ¿Por que razón no se matriculó .... en el presente año escolar?



 gen	 anoest=.
 replace anoest=0  if  ultcurso==0 | ultcurso==1
 replace anoest=1  if (ultcurso==3 & ultgrado==1) | (ultcurso==2 & ultgrado==1) 
 replace anoest=2  if (ultcurso==3 & ultgrado==2) | (ultcurso==2 & ultgrado==2)
 replace anoest=3  if (ultcurso==3 & ultgrado==3) | (ultcurso==2 & ultgrado==3)
 replace anoest=4  if (ultcurso==3 & ultgrado==4) 
 replace anoest=5  if (ultcurso==3 & ultgrado==5) 
 replace anoest=6  if (ultcurso==3 & ultgrado==6) 
 replace anoest=7  if (ultcurso==4 & ultgrado==1) | (ultcurso==5 & ultgrado==1)
 replace anoest=8  if (ultcurso==4 & ultgrado==2) | (ultcurso==5 & ultgrado==2) 
 replace anoest=9  if (ultcurso==4 & ultgrado==3) | (ultcurso==5 & ultgrado==3) 
 replace anoest=10 if (ultcurso==4 & ultgrado==4) | (ultcurso==6 & ultgrado==1)
 replace anoest=11 if (ultcurso==4 & (ultgrado==5 | ultgrado==6)) | (ultcurso==6 & (ultgrado==2 |  ultgrado==3) ) 
 replace anoest=12 if (ultcurso==9 & ultgrado==1) | (ultcurso==8 & ultgrado==1) | (ultcurso==7 & ultgrado==1)  
 replace anoest=13 if (ultcurso==9 & ultgrado==2) | (ultcurso==8 & ultgrado==2) | (ultcurso==7 & ultgrado==2) 
 replace anoest=14 if (ultcurso==9 & ultgrado==3) | (ultcurso==8 & ultgrado==3) | (ultcurso==7 & ultgrado==3)  
 replace anoest=15 if (ultcurso==9 & ultgrado==4) 				| (ultcurso==7 & ultgrado==4)
 replace anoest=16 if (ultcurso==9 & (ultgrado==5 | ultgrado==6)) 		| (ultcurso==7 & (ultgrado==5 | ultgrado==6))
 replace anoest=17 if (ultcurso==10) 
 replace anoest=18 if (ultcurso==11) 

 replace anoest=99 if edad<7
 
 
** Economic Active Population 

 gen	 peaa=1 if (s5p1==1 | s5p2==1 | s5p3==1)
 replace peaa=2 if (s5p5>=1 & s5p5<=2) | ((s5p6>=1 & s5p6<=4) | s5p6==12 | s5p6==13)
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

s4p19
19. ¿Se matriculó ..... en el presente año escolar, en el sistema de educación formal?
 1.Si => 21
 2.No => Mayores de 40 años pasar a 47

s4p20 
20. ¿Por que razón no se matriculó .... en el presente año escolar?
	MAYOR O IGUAL A 14 AÑOS => 47
	MENOR DE 14 AÑOS 	=> SECCIÓN 5

matricurso		s4p21b (grado)
21. ¿Cuál es el nivel educativo y grado o año en que se matriculó 
	..... en el presente año escolar?
 1. Preescolar 
 2. Educación de adultos 
 3. Primaria 
 4. Secundaria 
 5. Técnico básico 
 6. Técnico medio 
 7. Formación docente 
 8. Técnico superior 
 9. Universitario 
 10. Postgrado .
 11. Maestría 
 12. Doctorado 
 13. Educación especial 
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen     NERP=0 if (edad>=7 & edad<=12) & (s4p19==1 | s4p19==2)
 replace NERP=1 if (edad>=7 & edad<=12) & matricurso==3
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen     NERS=0 if (edad>=13 & edad<=17) & (s4p19==1 | s4p19==2)
 replace NERS=1 if (edad>=13 & edad<=17) & matricurso==4 

** Upper secondary
* Educación Secundaria: Ciclo diversificado (4o Magisterio - 5o Bachillerato)

 gen     NERS2=0 if (edad>=16 & edad<=17) & (s4p19==1 | s4p19==2)
 replace NERS2=1 if (edad>=16 & edad<=17) & (matricurso==4) & (s4p21b>=4 & s4p21b<=5)
		
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & Write

 gen     LIT2=0 if (edad>=15 & edad<=24) & (s4p16>=1 & s4p16<=3)
 replace LIT2=1 if (edad>=15 & edad<=24) & (s4p16==1)
	
*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  matricurso==3
 gen sec=1  if  matricurso==4
 gen ter=1  if  matricurso==9

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

 gen MA2=1 if ((s4p16==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((s4p16==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen     RATIOLIT2=0 if ((s4p16==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((s4p16==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* Without Domestic Service
/*
En la ocupacion trabajo como:
 1. Empleado / Obrero  
 2. Jornalero / Peon  
 3. Cuenta propia  
 4. Patron o Empresario 
 5. Miembro cooperativo de produccion 
 6. Trabajador sin pago o familiar no remun  
 7. Otro, cual

ocup
9131 Personal doméstico
*/

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ==1 | categ==2) & (rama>=1010 & rama<=9800) & peaa==1 & ocup!=9131
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==1 | categ==2) & (rama>=1010 & rama<=9800) & peaa==1 & ocup!=9131 & sexo==2 
	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categ==1 | categ==2) & (rama>=1320 & rama<=9800) & peaa==1 
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==1 | categ==2) & (rama>=1320 & rama<=9800) & peaa==1 & sexo==2 
	
*Proportion of Births Attended by Skilled Health Personnel*
* 12 a 54 in survey questionnaire

/*
s7p23
23. ¿Quién atendió su último parto?

 1. Ginecólogo(a)/médico 
 2. Comadrona/partera 
 3. Enfermera/auxiliar 
 4. Otro, cuál?_______
 9. NS/NR
*/

 gen	 SKILLED=0 if (s7p23>=1 & s7p23<=4) & (edad>=15 & edad<=49)
 replace SKILLED=1 if (s7p23>=1 & s7p23<=3) & (edad>=15 & edad<=49)

*Contraceptive Prevalence Rate*

/* 
s2p6
1. Unido
2. Casado

s7p23
1. Sí, natural
2. Sí, artificial

*/

 gen	 cpr=0 if (edad>=15 & edad<=49) & (s2p6==1 | s2p6==2)				 & sexo==2
 replace cpr=1 if (edad>=15 & edad<=49) & (s2p6==1 | s2p6==2) & (s7p28a==1 | s7p28a==2)  & sexo==2

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

/*
s1p38
38. ¿Com qué tipo de alumbrado cuenta principalmente este hogar?
 1. Energía eléctrica
 2. Planta generador eléctrico
 3. Gas, kerosene, candil
 4. Otro
 5. ninguno
*/

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (s1p38>=1 & s1p38<=5) /* Total population excluding missing information */
 replace ELEC=1 if (s1p38==1 | s1p38==2)

** Target 10, Indicator: Proportion of the population using solidfuels (%)

/*
combusti (s1p43)
43. ¿Qué combustible utilizan usualmente para cocinar?
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
agua (s1p20)
20. De dónde obtiene principalmente agua este hogar:
 1. Tubería dentro de la vivienda				==> 21
 2. Tubería fuera de la vivienda, pero dentro del terreno	==> 21
 3. Puesto público						==> 22
 4. Pozo público o privado					==> 22
 5. Río, manantial o quebrada					==> 22
 6. Camión, carreta o pipa					==> 22
 7. De otra vivienda/vecino/empresa				==> 22
 8. Otro							==> 22	

21. En promedio, ¿Cuántas horas al día o días por semana
cuentan con el suministro de agua?
 1. Suministro permanente					==> 26
 2. Suministro parcial
 		___ Horas por día				==> 25
 		___ Días por semana				==> 25
 			
22. ¿Quiénes son las personas encargadas de traer/acarrear
el agua a su vivienda?


23. ¿A qué distancia de su vivienda se encuentra la fuente
donde obtiene el agua y cuánto tiempo tarda en ir y venir?

DISTANCIA: ______________ Kms. ______________Mts _________ Varas

 Distagua a: Kilometros				s1p23a 
 Distagua b: Metros				s1p23b
 Distagua c: Varas (equivale a 0.8359mts)	s1p23c

Tiempo: ________ Hrs ____________Min

24. Cómo transporta principalmente el agua a su vivienda:
25. Cuando guardan el agua para beber o cocinar, lo hacen en
baldes, barriles, pilas, que se tapan:
26. Paga este hogar por el agua que consumen:
27. ¿Cuánto pagó el mes pasado o la última vez por el agua que consumió?
28. Qué tratamiento le aplican principalmente al agua para beber
 1. Tal como la obtiene
 2. La hierven
 3. La cloran
 4. Compran agua purificada
 5. Otro
 
*/

 gen distagua3m=distagua3*(0.8359) /* Varas */

 gen	 distsegura=1 if distagua1<=1
 replace distsegura=1 if distagua2<=1000
 replace distsegura=1 if distagua3m<=1000

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (agua>=1 & agua<=8)  /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4) | agua==7
 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
s1p29
1. Excusado o letrina sin tratar			==> 30
2. Excusado o letrina con tratamiento			==> 30
3. Inodoro, conectado a tubería de aguas negras		==> 31
4. Inodoro, conectado a sumidero o a pozo séptico	==> 31	
5. Inodoro, que descarga en río o quebrada		==> 31
6. No tiene						==> 31 

30. ¿El cuarto donde está la letrina, es utilizado para
otros fines?
 1. Sí, como depósito de granos
 2. Sí, como depósito de otros
 3. Sí, otro uso, cuál?
 4. No, sólo como letrina
 
servexc (s1p31)
31. ¿El servicio higiénico es de uso exclusivo del hogar?

32. El servicio higiénico está ubicado:
 1. Dentro de la vivienda
 2. Fuera de la vivienda

33. ¿A qué distancia de la vivienda está ubicado el servicio higiénico?
	Metros________	 Varas __________

34. ¿A qué distancia del servicio higiénico está ubicada la fuente de abastecimiento de agua?
	Metros________	 Varas __________
*/

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (s1p29>=1 & s1p29<=6) /* Total population excluding missing information */
 replace SANITATION=1 if (s1p29>=3 & s1p29<=4) | (s1p29==2 & s1p30==4)
 	
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*

tipoviv
3. Tipo de vivienda: (Por Observación)
 1. Casa o quinta 
 2. Apartamento o pieza 
 3. Cuarto en cuartería 
 4. Rancho o choza  
 5. Vivienda improvisada 
 6. Local usado como vivienda(negocio, bodega, etc) 


tenenviv
16. La vivienda que ocupa este hogar es:
 1. Propia con escritura 
 2. Propia sin escritura 
 3. Amortizándose/propia pagándose  
 4. Alquilada 
 5. Cedida o prestada 
 6. Recibida por servicios 
 7. Posando 
 8. Otra, cuál?___________________________

paredes
5. ¿Qué material predomina en las paredes exteriores de la vivienda?
 1. Ladrillo o bloque de barro   
 2. Bloque de cemento o concreto   
 3. Adobe o taquezal   
 4. Piedra cantera   
 5. Bambú, caña o palma   
 6. Madera   
 7. Madera y concreto (minifalda)   
 8. Lámina plycem o nicalit  
 9. Ripios o desechos  
 10. Otro,cuál? 

5.A ¿En qué estado se encuentra la pared?
 1. Bueno
 2. Malo 
 3. Regular
 
piso
6. ¿Qué material predomina en los pisos de la vivienda?
 1. Madera, tambo, etc  
 2. Embaldosado  
 3. Ladrillo de barro   
 4. Ladrillo de cemento mosaico, terrazo  
 5. Tierra  
 6. Otro, cuál?____________________________

6.A ¿En qué estado se encuentra el piso?
 1. Bueno
 2. Malo 
 3. Regular
 
13. ¿De cuántos cuartos dispone este hogar? (No incluya
cocina, baños, pasillos y garajes)
*/

 gen persroom=numper/numcuar 

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenenviv>=1 & tenenviv<=8) & (tipoviv>=1 & tipoviv<=6)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenenviv>=5 & tenenviv<=8) | (tipoviv>=4 & tipoviv<=6))

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if ((paredes>=1 & paredes<=10) & (piso>=1 & piso<=6))         /* Total population excluding missing information */
 replace secten_2=1 if ((paredes==5 | paredes==9 | paredes==10) | (piso>=5 & piso<=6))

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Piso 5=Tierra

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=6)
 replace DIRT=1 if (piso==5)
 	
** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if  (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
 replace UNMPLYMENT15=1 if  (edad>=15 & edad<=24) & (tasadeso==1 )

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
*Telephone Lines and Cellular Subscribers 
/*
51. Tiene servicio telefónico este hogar:
 1. Si, teléfono domiciliar 
 2. Si, teléfono celular  
 3. Ambos 
 4. No tiene  
*/

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (servtel>=1 & servtel<=4) /* Total population excluding missing information */
 replace TELCEL=1 if  (servtel>=1 & servtel<=3)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (servtel>=1 & servtel<=4) /* Total population excluding missing information */
 replace TEL=1 if (servtel==1 | servtel==3)

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (servtel>=1 & servtel<=4) /* Total population excluding missing information */
 replace CEL=1 if (servtel==2 | servtel==3)
	
** Target 18, Indicator: "Personal computers in use per 100 population"
/* 
SECCIÓN 9 PARTE E - EQUIPAMIENTO DEL HOGAR
21. Computadora
*/

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if (s9pe1>=1 & s9pe1<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (s9pe1==1)

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen	 CHILDREN=0 if  (edad>=12 & edad<=14) 
 replace CHILDREN=1 if  (edad>=12 & edad<=14) & (peaa==1)

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
	
** Disconnected Youths
/*
s5p6
6. ¿Cuál fue la razón principal por la que usted no buscó trabajo:
 1. Espera respuesta a solicitud de trabajo .
 2. Espera iniciar un nuevo trabajo 
 3. Espera cosecha o temporada de trabajo 
 4. Tiene un trabajo esporádico / ocasional 
 5. No tiene donde dejar a los niños 
 6. Está estudiando / menor de edad 
 7. Es pensionado / jubilado 
 8. Es rentista 
 9. Realiza los quehaceres del hogar 
 10. Incapacitado permanente para trabajar 
 11. Anciano 
 12. Se cansó de buscar 
 13. Piensa que no hay trabajo o que no le darán 
 14. Enfermedad/accidente 
 15. Está embarazada 
 16. Otro,cuál? 
*/

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (s5p6==7 | (s5p6>=11 & s5p6<=13) | s5p6==16)

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

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)

* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))

 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=17) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=17) & (anoest>=0 & anoest<99)
*/

*YL -> renombro esta variable para que no se confunda con el SOCOMETRO.
rename comp compxxx

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




