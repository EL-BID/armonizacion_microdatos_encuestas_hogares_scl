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

local PAIS URY
local ENCUESTA ECH
local ANO "2000"
local ronda a 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Uruguay
Encuesta: ECH
Round: a
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 30 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/



/* programa modificado
en la variable aedu_ci se cambio un & por un | ya que las respuestas a la pregunta pe142 son
excluyentes 
codigo anterior:replace aedu_ci=12+pe142 if pe141==5 & pe141==6 
MFP:10Agosto-2005, gusrdado por ultima vez en  abril 8 de 2005 10am

**** revision August 21, 2006 (Victoria) 
codification of education dummy variables was changed because if not they
were not coherent with years of education.
old code can be seen below in the education section. 
****

**** revision October 27, 2006 (Victoria) 
codification of years of education was changed because there were no
observations with 0 years of education
old code can be seen below in the education section. 
****

*** revision August 2007 (Victoria) ***
With the unification Sociometro/Equis we decided to add two new varibales: howner and floor.
This variables were already created for Atlas

gen howner=(viviprop_ch==1 | viviprop==2);
replace howner=. if viviprop_ch==.;
gen floor=(piso_ch==1);
replace floor=. if piso_ch==.;

Also, the orginal data was replaced with the new Mecovi versions
*/

use `base_in', clear
/***************************************************************************************************************************
 							Harmonización 1999-2000
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=ident
gen idp_ci=persona
gen factor_ch=pesoan
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=2000
gen mes_c=0
forvalues j=1(1)12{
local h=`j'*4
local m=`h'-3
replace mes_c=`j' if ha2>=`m'& ha2<=`h'
}
gen relacion_ci=pe4
replace relacion_ci=4 if pe4==5
replace relacion_ci=5 if pe4==6
replace relacion_ci=6 if pe4==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

****************
* region_BID_c *
****************
gen region_BID_c=.
replace region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
* Region_c *
************
destring ha3, replace
gen region_c=ha3
label define region_c  1 "Montevideo" ///
           2 "Artigas" /// 
           3 "Canelones" /// 
           4 "Cerro Largo" /// 
           5 "Colonia" /// 
           6 "Durazno" /// 
           7 "Flores" /// 
           8 "Florida" /// 
           9 "Lavalleja" /// 
          10 "Maldonado" /// 
          11 "Paysandú" /// 
          12 "Río Negro" /// 
          13 "Rivera" /// 
          14 "Rocha" /// 
          15 "Salto" /// 
          16 "San José" /// 
          17 "Soriano" /// 
          18 "Tacuarembó" ///
          19 "Treinta y Tres" 
label value region_c region_c
label var region_c "División política"

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(hd5==1)
gen aguadist_ch=hd6
replace aguadist_ch=0 if hd6==4
gen aguamala_ch=(hd5==4|hd5==5) /*Cachimba=ojo de agua o arroyo*/	
gen aguamide_ch=.
gen luz_ch=(hd101==1 | hd101==2)
gen luzmide_ch=.
gen combust_ch=(hd102==1 | hd102==2 | hd102==3)
gen bano_ch=(hd7!=3)
gen banoex_ch=(hd8==1)
replace banoex_ch=. if bano_ch==0
gen des1_ch=.
gen des2_ch=hd9
replace des2_ch=0 if hd9==3
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.
gen dorm_ch=hd42
replace dorm_ch=. if hd42==9
gen cuartos_ch=hd41
replace cuartos_ch=. if hd41==99
gen cocina_ch=.
gen refrig_ch=(hd113==1)
gen freez_ch=(hd112==1)
gen auto_ch=(hd1110==1)
gen telef_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen viv1_ch=.
gen viv2_ch=(hc1==1)
gen viviprop_ch=0 if hd3==3
replace viviprop_ch=1 if hd3==1
replace viviprop_ch=2 if hd3==2
replace viviprop_ch=3 if hd3==4 | hd3==5
gen vivialq_ch=ph2
gen vivialqimp_ch=pg14


gen howner=(viviprop_ch==1 | viviprop==2)
replace howner=. if viviprop_ch==.
gen floor=(piso_ch==1)
replace floor=. if piso_ch==.


/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
gen factor_ci=pesoan
gen sexo_ci=pe2
gen edad_ci=pe3
replace edad_ci=. if pe3==99
gen civil_ci=1 if pe5==5
replace civil_ci=2 if pe5==1 | pe5==2
replace civil_ci=3 if pe5==3
replace civil_ci=4 if pe5==4
gen jefe_ci=(pe4==1)
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
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
sort idh_ch
by idh_ch: egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5) if relacion_ci~=6
by idh_ch: egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch: egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21))
by idh_ch: egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65))
by idh_ch: egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6))
by idh_ch: egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1))

	*****************
	***miembros_ci***
	*****************
	gen miembros_ci=(relacion_ci<5)
	label variable miembros_ci "Miembro del hogar"
	
**********
***raza***
**********
gen raza_ci= .
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

	
	
/******************************************************************************/
/*				VARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/
****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if pobpcoac>=11 & pobpcoac<=12
replace condocup_ci=2 if pobpcoac>=21 & pobpcoac<=23
replace condocup_ci=3 if pobpcoac>=30 & pobpcoac<=38
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen disse       =(pe6==4) 
gen bps         =(pe6==7) 
gen iamc        =(pe6==5) 

gen afiliado_ci=(disse==1 | bps==1 | iamc==1)
replace afiliado_ci=. if disse==. & bps==. & iamc==.
label var afiliado_ci "Afiliado a la Seguridad Social"
drop disse bps iamc
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*instpen_ci*****
****************
gen instpen_ci= .
label var instpen_ci "Institucion a la cual esta afiliado variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label define  instcot_ci 1"bps" 2"bps y afap" 3"policial" 4"militar" 5"profesional" 6 "notarial" 7"bancaria"
label var instcot_ci "institución a la cual cotiza por su trabajo"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. /*Se pregunta solamente a empleados del sector publico que es el 22% de los empleados (no es comparable con el resto de paises)*/
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
/*
*************
*tamemp_ci***
*************
gen tamemp_ci=pf081
label define tamemp_ci 1"menos de 10 personas" 2"más de 10 personas"
label value tamemp_ci tamemp_ci
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
*************
*ypen_ci*
*************
gen yjub=pg911 if pg911>0 & pg911!=.
gen ypen=pg912 if pg912>0 & pg912!=.
  
egen ypen_ci=rsum(yjub ypen)
replace ypen_ci=. if yjub==. & ypen==.
drop yjub ypen
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci=(ypen_ci>0 & ypen_ci!=.)
*gen pension_ci=( pf1311==1 | pf1312==1 | pf3111==1 | pf3112==1)
label var pension_ci "1=Recibe pension contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*************
* cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if pf32==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*********
*lp25_ci***
*********
gen lp25_ci=717.4257
label var lp25_ci "Linea de pobreza 2.5 dólares, año base 2005"

*********
*lp4_ci***
*********
gen lp4_ci=1147.881
label var lp4_ci "Linea de pobreza 4 dólares, año base 2005"

*************
**salmm_ci***
*************
gen salmm_ci= 1060
label var	salmm_ci "Salario minimo legal 2000"

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


*gen emp_ci=(pobpcoac==11|pobpcoac==12)

g x=substr(pf39,1,3)
replace x="21" if x=="x21"
replace x="21" if x=="X21"
destring x, replace
gen ocupa_ci=.
replace ocupa_ci=1 if (x>=211 & x<=348) & emp_ci==1
replace ocupa_ci=2 if (x>=111 & x<=145) & emp_ci==1
replace ocupa_ci=3 if (x>=411 & x<=433) & emp_ci==1
replace ocupa_ci=4 if ((x>=521 & x<=599) | x==911) & emp_ci==1
replace ocupa_ci=5 if ((x>=511 & x<=516) | (x>=912 & x<=917)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((x>=601 & x<=640) | x==921 | x==923) & emp_ci==1
replace ocupa_ci=7 if ((x>=711 & x<=890) | (x>=931 & x<=933)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (x==11 | x==12)& emp_ci==1
replace ocupa_ci=9 if ((x>=21 & x<=101) | (x>=943 & x<=953) | x==0) & emp_ci==1

destring pf40, replace
gen rama_ci=.
replace rama_ci=1 if pf40>0 & pf40<=5 & emp_ci==1
replace rama_ci=2 if pf40>=10 & pf40<=14 & emp_ci==1
replace rama_ci=3 if pf40>=15 & pf40<=36 & emp_ci==1
replace rama_ci=4 if (pf40==40 | pf40==41) & emp_ci==1
replace rama_ci=5 if pf40==45 & emp_ci==1
replace rama_ci=6 if pf40>=50 & pf40<=55 & emp_ci==1
replace rama_ci=7 if pf40>=60 & pf40<=64 & emp_ci==1
replace rama_ci=8 if pf40>=65 & pf40<=74 & emp_ci==1
replace rama_ci=9 if pf40>=75 & pf40<=99 & emp_ci==1
gen horaspri_ci=pf051
replace horaspri_ci=. if pf051==99 | emp_ci==0
gen horastot_ci=pf053
replace horastot_ci=. if pf053==99 | horaspri_ci==.


/******************************************YLMPRI_CI*************************************/
egen ylmpri_ci=rsum(pg11p pg12p pg13p pg14p pg15p pg16p) if pf41==1 & emp_ci==1
replace ylmpri_ci=. if pg11p==. & pg12p==. & pg13p==. & pg14p==. & pg15p==. & pg16p==. & pf41==1

egen ylmpri_ci2=rsum(pg21p pg22p pg23p pg24p pg25p pg26p)
replace ylmpri_ci2=. if pg21p==. & pg22p==. & pg23p==. & pg24p==. & pg25p==. & pg26p==. 
replace ylmpri_ci=ylmpri_ci2 if pf41==2 & emp_ci==1

gen pg81pmo=pg81p/12 if emp_ci==1/*pg81p esta definido en una base anual!*/
egen ylmpri_ci3=rsum(pg71p pg72p pg81pmo) 
replace ylmpri_ci3=. if pg71p==. & pg72p==. & pg81pmo==. 
replace ylmpri_ci=ylmpri_ci3 if pf41==3 & emp_ci==1

gen pg61pmo=pg61p/12 & emp_ci==1/*pg61p esta definido en una base anual!*/
egen ylmpri_ci4=rsum(pg51p pg61pmo) 
replace ylmpri_ci4=. if pg51p==. & pg61pmo==. 
replace ylmpri_ci=ylmpri_ci4 if pf41==4 & emp_ci==1

egen ylmpri_ci5=rsum(pg31p pg32p) 
replace ylmpri_ci5=. if pg31p==. & pg32p==. 
replace ylmpri_ci=ylmpri_ci5 if pf41==5 & emp_ci==1

egen ylmpri_ci6=rsum(pg41p pg42p) 
replace ylmpri_ci6=. if pg41p==. & pg42p==. 
replace ylmpri_ci=ylmpri_ci6 if pf41==6 & emp_ci==1

drop ylmpri_ci2 ylmpri_ci3 ylmpri_ci4 ylmpri_ci5 ylmpri_ci6


/******************************************YLNMPRI_CI**************************************/
gen pg82pmo=pg82p/12  & emp_ci==1/*pg82p esta definido en una base anual!*/
egen ylnmpri_ci=rsum(pg73p pg82pmo) if pf41==3 & emp_ci==1
replace ylnmpri_ci=. if pg73p==. & pg82pmo==. & pf41==3

replace ylnmpri_ci=pg17p if pf41==1 & emp_ci==1
replace ylnmpri_ci=pg27p if pf41==2 & emp_ci==1

gen pg62pmo=pg62p/12  & emp_ci==1/*pg62p esta definido en una base anual!*/
egen ylnmpri_ci4=rsum(pg52p pg62pmo) 
replace ylnmpri_ci4=. if pg52p==. & pg62pmo==.
replace ylnmpri_ci=ylnmpri_ci4 if pf41==4

replace ylnmpri_ci=pg33p if pf41==5 & emp_ci==1
replace ylnmpri_ci=pg43p if pf41==6 & emp_ci==1

drop ylnmpri_ci4
/********************************************************************************************/

gen ylmsec_ci=.
gen ylnmsec_ci=.
gen ylmotros_ci=.

gen pg61omo=pg61o/12  & emp_ci==1/*pg61o esta definido en una base anual!*/
gen pg81omo=pg81o/12  & emp_ci==1/*pg81o esta definido en una base anual!*/
egen ylmaux=rsum(pg11o pg12o pg13o pg14o pg15o pg16o pg21o pg22o pg23o pg24o pg25o pg26o pg31o pg32o pg41o pg42o pg51o pg61omo pg71o pg72o pg81omo) if emp_ci==1
replace ylmaux=. if pg11o==. & pg12o==. & pg13o==. & pg14o==. & pg15o==. & pg16o==. & pg21o==. & pg22o==. & pg23o==. & pg24o==. & pg25o==. & pg26o==. & pg31o==. & pg32o==. & pg41o==. & pg42o==. & pg51o==. & pg61omo==. & pg71o==. & pg72o==. & pg81omo==.

gen pg62omo=pg62o/12 if emp_ci==1/*pg62o esta definido en una base anual!*/
gen pg82omo=pg82o/12 if emp_ci==1/*pg82o esta definido en una base anual!*/
egen ylnmaux=rsum(pg17o pg27o pg33o pg43o pg52o pg62omo pg73o pg82omo) if emp_ci==1
replace ylnmaux=. if pg17o==. & pg27o==. & pg33o==. & pg43o==. & pg52o==. & pg62omo==. & pg73o==. & pg82omo==.

gen nrylmpri_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmaux)
replace ylm_ci=. if ylmpri_ci==. & ylmaux==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmaux)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmaux==.

drop ylnmaux ylmaux

gen pg131mo=pg131/12 /*pg131 esta definido en una base anual!*/
gen pg132mo=pg132/12 /*pg132 esta definido en una base anual!*/
egen ynlm_ci=rsum(pg911 pg912 pg921 pg922 pg101 pg102 pg111 pg112 pg121 pg122 pg131mo pg132mo)
replace ynlm_ci=. if pg911==. & pg912==. & pg921==. & pg922==. & pg101==. & pg102==. & pg111==. & pg112==. & pg121==. & pg122==. & pg131mo==. & pg132mo==.

gen ynlnm_ci=.
drop pg81pmo pg61pmo pg82pmo pg62pmo pg61omo pg81omo pg62omo pg82omo pg131mo pg132mo
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if relacion_ci!=6
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0
gen rentaimp_ch=vivialqimp_ch
gen autocons_ch=.
gen autocons_ci=.
egen remesas_ci=rsum(pg921 pg922 pg102 pg112 pg122 pg132)
replace remesas_ci=. if pg921==. & pg922==. & pg102==. & pg112==. & pg122==. & pg132==.
sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6

gen durades_ci=pf26/4.3 if pf26>0 /*if pobpcoac==21 | pobpcoac==23*/ /*De los que estan desempleados cuanto hace (meses) que buscan*/
replace durades_ci=. if pf26==99

gen antiguedad_ci=pf37
replace antiguedad_ci=. if pf37==99

/******************************************************************************************/
/*					VARIABLES DEL MERCADO LABORAL		          */
/******************************************************************************************/

gen desemp1_ci=(real(substr(string(pobpcoac),1,1))==2 & pf21==1)
gen desemp2_ci=(desemp1_ci==1 | pf21==2 & pf22==2 | pf22==3)
gen desemp3_ci=(desemp2_ci==1 | (pf23==1 | (pf26>=4 & pf26!=.)))
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
gen desalent_ci=(pobpcoac==37)
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & pf18==1)
replace subemp_ci=. if emp_ci==0
gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & pf18==2)
replace tiempoparc_ci=. if emp_ci==0
gen categopri_ci=1 if pf41==4
replace categopri_ci=2 if pf41==5 | pf41==6
replace categopri_ci=3 if pf41==1 | pf41==2 | pf41==3
replace categopri_ci=4 if pf41==7 | pf41==8
replace categopri_ci=. if emp_ci!=1
gen categosec_ci=.
gen contrato_ci=.
gen segsoc_ci=.
gen nempleos_ci=1 if pf07==1
replace nempleos_ci=2 if pf07>1
replace nempleos_ci=. if pf07==.
/*gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & pf081==1 & pf082>=1 & pf082<=5
replace firmapeq_ci=0 if emp_ci==1 & (pf081==1 & pf082>5)|pf081==2*/
gen spublico_ci=(emp_ci==1 & pf41==2)

*Genera la variable para empresas pequeñas
*drop tamemp_ci
gen tamemp_ci=1 if pf082==1 | pf082==2 | pf082==3 | pf082==4 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if pf082==6 | pf082==7 | pf082==8 | pf082==9 | pf082==5
*Empresas grandes
replace tamemp_ci=3 if pf081==2
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if pf3111==1 | pf3112==1
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if pf313==1
*Quehaceres del Hogar
replace categoinac_ci=3 if pf314==1
*Otra razon
replace categoinac_ci=4 if pf315==1 | pf316==1
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
tab categoinac_ci [iw=factor_ci]

/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/
/*
gen aedu_ci=0
replace aedu_ci=pe142 if pe141==1
replace aedu_ci=6 if pe142>=6 & pe141==1
replace aedu_ci=6+pe142 if pe141==2
*replace aedu_ci=9 if pe141==2 & aedu_ci>=9
replace aedu_ci=10 +pe142 if pe141==3
replace aedu_ci=6+pe142 if pe141==4
replace aedu_ci=12 if (pe141==3 | pe141==4) & aedu_ci>12


replace aedu_ci=12+pe142 if (pe141==5 | pe141==6)
replace aedu_ci=. if pe141==0 | pe142==0 | pe141>=7 |pe142==9
*Droppeamos a los militares. Ver programas 92-98 para mejor explicacion
*/

*recode pe142 (4=1) (5=2) (6=3) if pe141==2 | pe141==3 | pe141==4 ???

gen aedu_ci=.
replace aedu_ci=0        if pe141==8 | pe141==0
replace aedu_ci=0        if pe141==1 & pe142==9
replace aedu_ci=pe142    if pe141==1 & pe142<9
replace aedu_ci=pe142+6  if pe141==2 & pe142<9
replace aedu_ci=pe142+9  if pe141==3 & pe142<9
replace aedu_ci=pe142+9  if pe141==4 & pe142<9
replace aedu_ci=pe142+12 if pe141==5 | pe141==6 & pe142<9
replace aedu_ci=0        if aedu_ci==. & (edad>=5 & edad!=.)
replace aedu_ci=.        if pe141==7 | pe141==9 | pe141==. 


/*
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(pe141==1 & pe15==2)
gen edupc_ci=(pe141==1 & pe15==1)
gen edusi_ci=((pe141==3 | pe141==4) & pe15==2)|pe141==2
gen edusc_ci=((pe141==3 | pe141==4) & pe15==1)
gen eduui_ci=(pe141==5 | pe141==6) & pe15==2
gen eduuc_ci=(pe141==5 | pe141==6) & pe15==1
gen edus1i_ci=(pe141==2 & pe15==2)
gen edus1c_ci=(pe141==2 & pe15==1)
gen edus2i_ci=((pe141==3 | pe141==4) & pe15==2)
gen edus2c_ci=((pe141==3 | pe141==4) & pe15==1)
*/

**************
***eduno_ci***
**************

gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
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

gen byte edusi_ci=(aedu_ci>6 & aedu_ci<12)
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

gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
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

gen byte eduui_ci=(aedu_ci>12 & aedu_ci<16)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"



gen edupre_ci=(pe141==8)
gen eduac_ci=.
replace eduac_ci=1 if pe141==6 & (eduuc_ci==1 | eduui_ci==1)
replace eduac_ci=0 if (eduuc_ci==1 | eduui_ci==1) & pe141!=6
foreach var of varlist edu*{
replace `var'=. if aedu_ci==0 | aedu_ci==. | pe15==0 | pe15==.
}
gen asiste_ci=(pe11==1)
gen pqnoasist_ci=.
gen repite_ci=.
gen edupub_ci=(pe13==1)
label var  aedu_ci "Anios de Educacion"
****************
***tecnica_ci **
****************
gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"

/*
*********************************************************************************************
**Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
qui sum factor_ch	idh_ch	idp_c	zona_c	pais_c	anio_c	mes_c	relacion_ci	factor_ci	sexo_ci	edad_ci	civil_ci	///
jefe_ci	nconyuges_ch	nhijos_ch	notropari_ch	notronopari_ch	nempdom_ch	clasehog_ch	nmiembros_ch	///
miembros_ci	nmayor21_ch	nmenor21_ch	nmayor65_ch	nmenor6_ch	nmenor1_ch	ocupa_ci	rama_ci	horaspri_ci	///
horastot_ci	ylmpri_ci	ylnmpri_ci	ylmsec_ci	ylnmsec_ci	ylmotros_ci	ylnmotros_ci	nrylmpri_ci	tcylmpri_ci ///
ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci	nrylmpri_ch	tcylmpri_ch	ylm_ch	ylnm_ch	ylmnr_ch	ynlm_ch	ynlnm_ch	///
ylmhopri_ci	ylmho_ci	rentaimp_ch	autocons_ci	autocons_ch	remesas_ci	remesas_ch	durades_ci	antiguedad_ci ///
emp_ci	desemp_ci	pea_ci	 desalent_ci	subemp_ci	tiempoparc_ci ///
categopri_ci	categosec_ci	nempleos_ci	firmapeq_ci	spublico_ci	aedu_ci	eduno_ci ///
edupi_ci	edupc_ci	edusi_ci	edusc_ci	eduui_ci	eduuc_ci	edus1i_ci	edus1c_ci	edus2i_ci ///
edus2c_ci	edupre_ci	eduac_ci	asiste_ci	pqnoasis	repite_ci	repiteult_ci	edupub_ci	///
aguared_ch	aguadist_ch	aguamala_ch	aguamide_ch	luz_ch	luzmide_ch	combust_ch	bano_ch	banoex_ch	///
des1_ch	des2_ch	piso_ch	pared_ch	techo_ch	resid_ch	dorm_ch	cuartos_ch	cocina_ch	telef_ch ///
refrig_ch	freez_ch	auto_ch	compu_ch	internet_ch	cel_ch	vivi1_ch	vivi2_ch	viviprop_ch	///
vivitit_ch	vivialq_ch	vivialqimp_ch region_BID_c region_c raza_ci        lp25_ci	       lp4_ci	 ///
lp_ci	       lpe_ci	       cotizando_ci	         afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
tamemp_ci 	   pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	///
tamemp_ci categoinac_ci formal_ci

*/


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
compress


*do ruta\labelsBID.do, modify

saveold "`base_out'", replace


log close


