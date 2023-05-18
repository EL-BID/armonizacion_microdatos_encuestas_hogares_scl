
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
local ANO "1993"
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
Autores: 
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

June 20, 2006 (Analia)
replace techo_ch=(mattecho==7)
was replaced with
replace techo_ch=2 if mattecho==7
***/

/*** August 18, 2006 (Victoria)
Change in the code for years of education. 
For the option of Technical School now the prerequisite is 9 years of education instead of 6
in order to be coherent with Nicaragua 1998 y 2001,

old code: 
replace aedu_ci=6+gradoapr if nivelapr==3
replace aedu_ci=13 if gradoapr>7 & nivelapr==3

new code:
replace aedu_ci=9+gradoapr if nivelapr==3
replace aedu_ci=16 if gradoapr>7 & nivelapr==3

Also, code for education dummies was changed beacuse there were mistakes in it. 
Old code can be seen in education section below. 

Also changed duration of secundary education for 11 instead of 12
**/

* January 25 2007 (MFP)
*Change in firmapeq variable. It was created to reflect big firms instead of small firm

/***************/
/*NICARAGUA '93*/
/***************/


************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"
/******************
Variables del Hogar
*******************/
* 2014,02 MLO: el factor de ponderacion tiene problemas da una poblacion mas grande que LAC
gen factor_ch=1
*gen factor_ch=ponde
gen idh_ch=noformu
gen idp_ci=codident
gen zona_c=area
replace zona_c=0 if area==2
gen pais_c="NIC"
gen anio_c=1993
gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" | pais=="PAN" | pais=="DOM" | pais=="CRI" | pais=="BLZ" | pais=="GTM" | pais=="SLV" | pais=="HON" | pais=="NIC"
replace region_BID_c=2 if pais=="BAH" | pais=="BAR" | pais=="GUY" | pais=="JAM" | pais=="SUR" | pais=="TOT"
replace region_BID_c=3 if pais=="ECU" | pais=="COL" | pais=="PER" | pais=="VEN" | pais=="BOL" 
replace region_BID_c=4 if pais=="ARG" | pais=="BRA" | pais=="CHL" | pais=="PRY" | pais=="URU" 
replace region_BID_c=5 if pais=="HAI"
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
gen mes_c=mes1rint
gen relacion_ci=parentes
replace relacion_ci=4 if parentes>=5 & parentes<=8
replace relacion_ci=5 if parentes==10
replace relacion_ci=6 if parentes==9

/*************************************
Variables de Infraestructura del Hogar
**************************************/

gen aguared_ch=(abastagu==1 | abastagu==2) /*Esta definido como acceso a agua de red!*/
gen aguadist_ch_ch=0
replace aguadist_ch_ch=1 if abastagu==1
replace aguadist_ch_ch=2 if abastagu==2 & distagua<=100
replace aguadist_ch_ch=3 if abastagu==2 & distagua>100
gen aguamala_ch=(abastagu==4 | abastagu==6 | abastagu==7)
gen aguamide_ch=(mediagua==1)
gen luz_ch=(alumbrad==1)
gen luzmide_ch=(medidluz==1)
gen combust_ch=(combusti==1 | combusti==2 | combusti==5)
gen bano_ch=(servhigi!=3) /*Esta definido como si posee servicio higienico: inodoro o letrina*/
gen banoex_ch=(serhigex==1)
replace banoex_ch=. if bano_ch!=1

/*DESAGUE: Nicaragua no tiene opcion de desagüe a la superficie, por lo que solo creamos des2_ch */
gen des2_ch=0
replace des2_ch=1 if desague==1 | desague==2
replace des2_ch=2 if desague==3

gen piso_ch=(matpisos==2 | matpisos==3 | matpisos==5|matpisos==6)
replace piso_ch=2 if matpisos==4 | matpisos==7 /*Vale 0 si es piso de tierra, 1 si es permanente y 2 si es otros (o de ladrillo de barro)*/

gen pared_ch=(matpared==2 |matpared==4 |matpared==6 |matpared==7 |matpared==9)
replace pared_ch=2 if (matpared==1 | matpared==11)

gen techo_ch=(mattecho==1 | mattecho==3 | mattecho==6)
replace techo_ch=2 if mattecho==7

gen resid_ch=0 /*Recoleccion*/
replace resid_ch=1 if elimbasu==2 | elimbasu==3 /*Quemada o enterrada*/
replace resid_ch=2 if elimbasu==5 /*Tirada*/
replace resid_ch =3 if elimbasu==4 /*"Otros: Hecha abono"*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .

gen dorm_ch=nodormit
gen cuartos_ch = nocuarto

gen telef_ch = (telefono==1)
gen refrig_ch =(poseeref==1)
gen des1_ch=.
replace des1_ch=0 if servhigi==3
replace des1_ch=1 if desague==1 |desague==2
replace des1_ch=2 if desague==3 | desague==4
*replace des1_ch=3 


gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen cocina_ch =.
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch = tipvivie
replace vivi1_ch=3 if tipvivie!=1 & tipvivie!=2

gen vivi2_ch = (vivi1_ch==1 | vivi1_ch==2)

gen viviprop_ch = 0 /*Alquilada*/
replace viviprop_ch = 1 if sitlegvi ==1 | sitlegvi==2 /*Propia*/
replace viviprop_ch = 2 if sitlegvi ==5 /*Pagando*/
replace viviprop_ch = 3 if sitlegvi ==4/*Ocupada*/
replace viviprop_ch = 4 if sitlegvi!=1 & sitlegvi!=2 & sitlegvi!=4  & sitlegvi!=5 & sitlegvi!=6

gen vivitit_ch = (sitlegvi ==1) /*Con titulo que certifique la propiedad de la vivienda*/

gen vivialq_ch= pagaalqui if viviprop_ch==0
gen vivialqimp_ch=.


/*********************
Variables Demograficas
**********************/
* 2014, 02 MLO: el factor de ponderacion tiene problemas, da una poblacion exageradamente grande para lo que es el país
*gen factor_ci=.
* 2014, 09 Mayra Sáenz: Genero con uno temporalmente porque genera problemas en la construcción de ciertos indicadores.
gen factor_ci=1

*gen factor_ci=ponde
gen sexo_ci=sexo
gen edad_ci=anoscump
gen civil_ci=.
replace civil_ci=1 if estcivil==6
replace civil_ci=2 if estcivil==1 | estcivil==2
replace civil_ci=3 if estcivil==4 | estcivil==5
replace civil_ci=4 if estcivil==3
gen jefe_ci=(relacion_ci==1)
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
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5) if relacion_ci~=6
by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1))
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
/*gen condocup_ci=.
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 6"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
/*
gen condocup1_ci=.
replace condocup1_ci=1 if p5a01==1 | p5a02==1 | p5a03==1 
replace condocup1_ci=2 if p5a03==2 
replace condocup1_ci=3 if p5a05>=6 & p5a05<=11
replace condocup1_ci=4 if edad_ci<14
*/

* Modificación Marcela Rubio Septiembre 2014: se agrega sintaxis condocup habia generada como missing

/*
trault7d: trabajo en los ultimos 7 dias
1 si
2 no

caunobtr: por que no busco trabajo
1 Vacation, authorized absence
2 Sick, Maternity leave
3 Waiting response employer
4 Waiting new job
5 Waiting harvest
6 No work available
7 Student, minor
8 Retired
9 Rentista
10 Elderly, invalid
11 Housework
12 Did not want to
13 Sick
*/
* MGD 09/22/2014 recodificacion en categoria 3 condicionando a condocup_ci!=1 & condocup_ci!=2
gen condocup_ci = 1 if (trault7d==1 | (trault7d==2 & (caunobtr==1 | caunobtr==2 )))
replace condocup_ci = 2 if (condocup_ci!=1 & butrul7d==1)
replace condocup_ci = 3 if edad_ci>=6 & condocup_ci!=1 & condocup_ci!=2
replace condocup_ci = 4 if edad_ci<6
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 6"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"


****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=.
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
generat cesante_ci=.
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =. 
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
generat salmm_ci=.
label var salmm_ci "Salario minimo legal"


gen emp_ci=(trault7d==1 | (trault7d==2 & (caunobtr==1 | caunobtr==2))) 
/*trault7d toma 3 valores: 1- Trabajo la semana pasada, 2-No trabajo y 0-Menores de 6 años (90% de los 0)
y otros que no se que son... probablemente tipos que no hayan contestado.*/
gen byte emp=1 if trault7d==1 | caunobtr==1 | caunobtr==2

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1


gen ocupa_ci=.
replace ocupa_ci=1 if real(substr(string(docutp7d),1,1))==2 | real(substr(string(docutp7d),1,1))==3
replace ocupa_ci=2 if real(substr(string(docutp7d),1,1))==1
replace ocupa_ci=3 if real(substr(string(docutp7d),1,1))==4
replace ocupa_ci=4 if real(substr(string(docutp7d),1,2))==52
replace ocupa_ci=5 if real(substr(string(docutp7d),1,2))==51
replace ocupa_ci=6 if real(substr(string(docutp7d),1,1))==6
replace ocupa_ci=7 if real(substr(string(docutp7d),1,1))==7 | real(substr(string(docutp7d),1,1))==8
replace ocupa_ci=8 if real(substr(string(docutp7d),1,1))==0
replace ocupa_ci=9 if real(substr(string(docutp7d),1,1))==9
replace ocupa_ci=. if emp_ci==. | docutp7d>=61112

gen rama_ci=.
/*replace rama_ci=1 if (actetp7d>=10000 & actetp7d<=91006) & emp_ci==1
replace rama_ci=2 if (actetp7d>=100000 & actetp7d<=149900) & emp_ci==1
replace rama_ci=3 if (actetp7d>=150000 & actetp7d<=390909) & emp_ci==1
replace rama_ci=4 if (actetp7d>=400000 & actetp7d<=419900) & emp_ci==1
replace rama_ci=5 if (actetp7d>=420001 & actetp7d<=459900) & emp_ci==1
replace rama_ci=6 if (actetp7d>=500000 & actetp7d<=559900) & emp_ci==1
replace rama_ci=7 if (actetp7d>=600000 & actetp7d<=649900) & emp_ci==1
replace rama_ci=8 if (actetp7d>=650000 & actetp7d<=702999) & emp_ci==1
replace rama_ci=9 if (actetp7d>=711000 & actetp7d<=969901) & emp_ci==1*/

/*
* Codificacion rama de ocupacion en los ultimos 12 meses no 7 dias. MGD 01/29/2014
gen rama_ci1=.
replace rama_ci1=1 if (acetp12m>=10000 & acetp12m<=95301) & emp_ci==1
replace rama_ci1=2 if (acetp12m>=100000 & acetp12m<=149900) & emp_ci==1
replace rama_ci1=3 if (acetp12m>=150000 & acetp12m<=390909) & emp_ci==1
replace rama_ci1=4 if (acetp12m>=400000 & acetp12m<=410101) & emp_ci==1
replace rama_ci1=5 if (acetp12m>=420001 & acetp12m<=460001) & emp_ci==1
replace rama_ci1=6 if (acetp12m>=500000 & acetp12m<=559900) & emp_ci==1
replace rama_ci1=7 if (acetp12m>=600000 & acetp12m<=649900) & emp_ci==1
replace rama_ci1=8 if (acetp12m>=650000 & acetp12m<=702999) & emp_ci==1
replace rama_ci1=9 if (acetp12m>=711041  & acetp12m<=993001) & emp_ci==1
*/


gen horaspri_ci=horatp7d*ditrtp7d /*La pregunta se refiere a horas diarias*/
replace horaspri_ci=. if emp_ci==0 | horatp7d>24 | ditrtp7d>7

gen horastot_ci=horaspri_ci+hordts7d*ditrts7d /*La pregunta sobre horas en el trabajo secundario se refiere a horas diarias*/
replace horaspri_ci=. if emp_ci==0

gen ylmpri_ci=monttp7d*22 if utmotp7d==1 
replace ylmpri_ci=monttp7d*4 if utmotp7d==2
replace ylmpri_ci=monttp7d*2 if utmotp7d==3
replace ylmpri_ci=monttp7d if utmotp7d==4
replace ylmpri_ci=monttp7d/6 if utmotp7d==5
replace ylmpri_ci=monttp7d/12 if utmotp7d==6
gen aguinaldo=reagtp7d/12 /*El aguinaldo esta preguntado por año*/
replace ylmpri_ci=ylmpri_ci+aguinaldo /*Por lo tanto, ylmpri_ci incluye: incentivos por comision, antiguedad, calificacion o
produccion; horas extras, viaticos, zonaje y aguinaldo.*/
drop aguinaldo
replace ylmpri_ci=. if emp_ci==0 | utmotp7d==0

gen ylnmpri_ci=rcbatp7d+recttp7d+revitp7d+reuntp7d
replace ylnmpri_ci=. if emp_ci==0

gen ylmsec_ci=mtotts7d*22 if utmtts7d==1 
replace ylmsec_ci=mtotts7d*4 if utmtts7d==2
replace ylmsec_ci=mtotts7d*2 if utmtts7d==3
replace ylmsec_ci=mtotts7d if utmtts7d==4
replace ylmsec_ci=mtotts7d/6 if utmtts7d==5
replace ylmsec_ci=mtotts7d/12 if utmtts7d==6
replace ylmsec_ci=. if emp_ci==0 | utmtts7d==0

gen ylnmsec_ci=beadts7d
replace ylnmsec_ci=. if emp_ci==0

gen ylmotros_ci=paotts7d
replace ylmotros_ci=. if emp_ci==0
gen ylnmotros_ci=.

gen nrylmpri_ci=(ylmpri_ci==0 & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==. 
*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.  

local vars "pensr pensa insur char inter divid rent indem inher loter loan remex remin"
foreach var of local vars {
replace qr`var'=. if qr`var'>=77777
replace tr`var'=. if tr`var'>=77
gen `var'=(qr`var'*tr`var')/12
}

egen ynlm_ci=rsum(pensr pensa insur char inter divid rent indem inher loter loan remex remin)
replace ynlm_ci=. if pensr==. & pensa==. & insur==. & char==. & inter==. & divid==. & rent==. & indem==. & inher==. & loter==. & loan==. & remex==. & remin==. 


sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

gen ylmnr_ch=ylm_ch
replace ylmnr=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if relacion_ci!=6
gen ynlnm_ch=.

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)

gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.

egen remesas_ci=rsum(remex remin)
replace remesas_ci=. if remex==. & remin==.
sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6
drop pensr pensa insur char inter divid rent indem inher loter loan remex remin
gen ynlnm_ci=.

gen durades_ci=sembutra/4.3

gen antiguedad_ci=titrtp7d if utvttp7d==4
replace antiguedad_ci=titrtp7d/12 if utvttp7d==3
replace antiguedad_ci=titrtp7d/48 if utvttp7d==2
replace antiguedad_ci=titrtp7d/365 if utvttp7d==1
replace antiguedad_ci=. if emp_ci==0

/****************************
Variables del Mercado Laboral
****************************/
gen desemp1_ci=(trault7d==2 & butrul7d==1)
gen desemp2_ci=(desemp1_ci==1 | (trault7d==2 & butrul7d==2 & (caunobtr==3 | caunobtr==4 | caunobtr==5)))
gen desemp3_ci=(desemp2_ci==1 | (trault7d==2 & butrul7d==2 & sembutra>1))

gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)

gen desalent_ci=.

gen subemp_ci=. 
gen tiempoparc_ci=.

gen categopri_ci=1 if tioctp7d==7
replace categopri_ci=2 if tioctp7d==4 | tioctp7d==5
replace categopri_ci=3 if tioctp7d==1 | tioctp7d==2 | tioctp7d==3
replace categopri_ci=4 if tioctp7d==6
replace categopri_ci=. if emp_ci==0

gen categosec_ci=1 if trcots7d==7
replace categosec_ci=2 if trcots7d==4 | trcots7d==5
replace categosec_ci=3 if trcots7d==1 | trcots7d==2 | trcots7d==3
replace categosec_ci=4 if trcots7d==6
replace categosec_ci=. if emp_ci==0

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

gen segsoc_ci=(ainstp7d<=3)
replace segsoc_ci=. if emp_ci==0

gen nempleos_ci=1 if otrttp7d==2
replace nempleos_ci=2 if otrttp7d==1
replace nempleos_ci=. if emp_ci==0
/*
gen firmapeq_ci=(peretp7d<=2)
replace firmapeq_ci=. if emp_ci==0 | peretp7d==.
*/
gen spublico_ci=(traptp7d==2 | traptp7d==3)



*************
*tamemp_ci
*************
*Nicaragua Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50
/*
gen tamemp_ci=peretp7d
label define tamemp_ci 1 "1 persona" 2 "2-5 personas" 3 "6-10 personas" ///
4 "11-30 personas" 5 "31-50 personas" 6 "51-100 personas" 7"101 a 200" 8"Más de 200"
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
gen tamemp_ci = 1 if peretp7d>=1 & peretp7d<=2
replace tamemp_ci = 2 if (peretp7d>=3 & peretp7d<=5)
replace tamemp_ci = 3 if (peretp7d>=6 & peretp7d<=8)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((caunobtr==8) & condocup_ci==3)
replace categoinac_ci = 2 if  (caunobtr==7 & condocup_ci==3)
replace categoinac_ci = 3 if  (caunobtr==11 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************

* Modificación Marcela Rubio - Septiembre 2014

/*
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
*/

gen byte formal_ci=.

/*******************
Variables Educativas
********************/
gen aedu_ci=.
replace aedu_ci=0 if nivelapr==0
replace aedu_ci=gradoapr if nivelapr==1
replace aedu_ci=6 if gradoapr>6 & nivelapr==1

replace aedu_ci=6+gradoapr if nivelapr==2
replace aedu_ci=11 if gradoapr>5 & nivelapr==2

replace aedu_ci=9+gradoapr if nivelapr==3
replace aedu_ci=11 if gradoapr>2 & nivelapr==3
replace aedu_ci=11+gradoapr if nivelapr==4
replace aedu_ci=. if nivelapr==5 | nivelapr==6 | nivelapr==7

/* OLD CODE 
gen edupi_ci=(nivelapr==1 & gradoapr>0 & gradoapr<6)
gen edupc_ci=(nivelapr==1 & gradoapr==6)
gen edusi_ci=((nivelapr==2 | nivelapr==3) & ((diplobte!=2 & diplobte!=5) | diplobte==4))
gen edusc_ci=((nivelapr==2 | nivelapr==3) & (diplobte==2 | diplobte==5))
gen eduui_ci=(nivelapr==4 & gradoapr<5)
gen eduuc_ci=(nivelapr==4 & gradoapr>=5)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.
gen edupre_ci=.
gen eduac_ci=.
gen asiste_ci=(asiscedu==1 | asiscedu==2 | asiscedu==3)
gen repite_ci=.
gen edupub_ci=(tipocent==1 | asiscedu==1)
foreach var of varlist edu*{
replace `var'=. if aedu_ci==.
}
gen eduno_ci=(aedu==0)
*/

gen edupi_ci=(nivelapr==1 & gradoapr>0 & gradoapr<6)
gen edupc_ci=(nivelapr==1 & gradoapr==6)

gen edusi_ci=((nivelapr==2 & gradoapr<5) | (nivelapr==3 & gradoapr<2))
gen edusc_ci=((nivelapr==2 & gradoapr>=5) | (nivelapr==3 & gradoapr>=2))

gen eduui_ci=(nivelapr==4 & gradoapr<5)
gen eduuc_ci=(nivelapr==4 & gradoapr>=5)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.
gen edupre_ci=.
gen eduac_ci=.
gen asiste_ci=(asiscedu==1 | asiscedu==2 | asiscedu==3)
gen repite_ci=.
gen edupub_ci=(tipocent==1 | asiscedu==1)
foreach var of varlist edu*{
replace `var'=. if aedu_ci==.
}
gen eduno_ci=(aedu==0)

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=.

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo ultimo grado o curso"


*************
***tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"

*********
*raza_ci*
*********

gen raza_ci=.
gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .




/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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

clonevar codindustria=acetp12m
clonevar codocupa=docutp7d

compress


saveold "`base_out'", replace


log close


