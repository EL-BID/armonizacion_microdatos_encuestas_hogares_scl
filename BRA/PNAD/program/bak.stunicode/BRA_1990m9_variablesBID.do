* (Versión Stata 13)
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

local PAIS BRA
local ENCUESTA PNAD
local ANO "1990"
local ronda m9 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Brasil
Encuesta: PNAD
Round: m9
Autores: 
Autores 2010: Yanira Oviedo
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/


use `base_in', clear

/********************************/
/*    VARIABLES DEL HOGAR	  */
/********************************/
keep if v201==2 /*YL: solo hogares permanentes*/

*****************
*** region_ci ***
*****************
*YL: generacion "region_c" proyecto maps America.	

gen region_c =.
replace region_c=11 if  v10==71
replace region_c=12 if  v10==72
replace region_c=13 if  v10==73
replace region_c=14 if  v10==74
replace region_c=15 if  v10==75
replace region_c=16 if  v10==76
replace region_c=21 if  v10==51
replace region_c=22 if  v10==52
replace region_c=23 if  v10==53
replace region_c=24 if  v10==54
replace region_c=25 if  v10==55
replace region_c=26 if  v10==56
replace region_c=27 if  v10==57
replace region_c=28 if  v10==58
replace region_c=29 if  v10>=59 & v10<=60
replace region_c=31 if  v10>=41 & v10<=42
replace region_c=32 if  v10==43
replace region_c=33 if  v10>=11 & v10<=14
replace region_c=35 if  v10>=20 & v10<=29
replace region_c=41 if  (v10>=30 & v10<=31) |  v10==37
replace region_c=42 if  v10==32
replace region_c=43 if  v10>=33 & v10<=35
replace region_c=50 if  v10==81
replace region_c=51 if  v10==82
replace region_c=52 if  v10==83
replace region_c=53 if  v10==61

label define region_c ///
11 "Rondônia" ///
12 "Acre" ///
13 "Amazonas" ///
14 "Roraima" ///
15 "Pará" ///
16 "Amapá" ///
17 "Tocantins" ///
21 "Maranhão" ///
22 "Piauí" ///
23 "Ceará" ///
24 "Rio Grande do Norte" ///
25 "Paraíba" ///
26 "Pernambuco" ///
27 "Alagoas" ///
28 "Sergipe" ///
29 "Bahia" ///
31 "Minas Gerais" ///
32 "Espírito Santo" ///
33 "Rio de Janeiro" ///
35 "São Paulo" ///
41 "Paraná" ///
42 "Santa Catarina" ///
43 "Rio Grande do Sul" ///
50 "Mato Grosso do Sul" ///
51 "Mato Grosso" ///
52 "Goiás" ///
53 "Distrito Federal"
label value region_c region_c

************************
*** region según BID ***
************************
gen region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
****idh_ch*****
***************
*YL: Se genera el id a nivel de familia no de unidade domiciliar
sort v10 v102 v103 p307
egen idh_ch=group(v10 v102 v103 p307)
label variable idh_ch "ID del hogar"

sort idh_ch p306
gen idp_ci=p306
gen factor_ch=v1091
gen zona_c=1 if v3==1 | v3==3
replace zona_c=0 if v3==5 | v3==7
gen str3 pais_c="BRA"
gen anio_c=1990
gen mes_c=9

gen relacion_ci=p306
replace relacion_ci=5 if p306==5 | p306==6 | p306==8
replace relacion_ci=6 if p306==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*p304
 2- branca 
 4- preta
 6- parda
 8- amarela
 9- sem declaração*/



gen raza_ci=.
replace raza_ci= 2 if  (p304 ==4 | p304 ==6)
replace raza_ci= 3 if (p304==2 | p304==8 | p304== 9) & raza_ci==.
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


/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(v206==1|v206==4)
gen aguadist_ch=1 if v206>=1 & v206<=3
replace aguadist_ch=2 if v206>=4 & v206<=6
replace aguadist_ch=0 if v206==9 
gen aguamala_ch=(v206==3 | v206==6) /*"Otra"*/	
gen aguamide_ch=.
gen luz_ch=(v210==1)
replace luz_ch=. if v210==9
gen luzmide_ch=.
gen combust_ch=.

* Modificacion Marcela Rubio Septiembre 2014: corrección en sintaxis
/*
gen bano_ch=(v208==1 |v208==3)
replace bano_ch=. if v208==9
*/

gen bano_ch=.
replace bano_ch = 0 if v208==5
replace bano_ch = 1 if (v208==1 |v208==3)

gen banoex_ch=(v208==1)
replace banoex_ch=. if bano_ch==0 | bano_ch==.

* Modificación Marcela Rubio Septiembre 2014: corrección en sintaxis
/*
gen des1_ch=1 if v207==0 | v207==2
replace des1_ch=2 if v207==4
replace des1_ch=3 if v207==6
replace des1_ch=0 if v207==0
replace des1_ch=. if v207==9
*/

gen des1_ch=.
replace des1_ch = 0 if bano_ch==0
replace des1_ch = 1 if v207==0 | v207==2
replace des1_ch = 2 if v207==4
replace des1_ch = 3 if v207==6

* Modificación Marcela Rubio Septiembre 2014: corrección en sintaxis

/*
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
*/

gen des2_ch=.
replace des2_ch = 0 if bano_ch==0
replace des2_ch = 1 if v207==0 | v207==2 | v207==4
replace des2_ch = 2 if v207==6

gen piso_ch=0
replace piso_ch=1 if v204!=6
replace piso_ch=2 if v204==8
replace piso_ch=. if v204==9
gen pared_ch=0
replace pared_ch=1 if v203==0 | v203==2 |v203==6
replace pared_ch=2 if v203==8
replace pared_ch=. if v203==9
gen techo_ch=. /*The questionaire of the 1990 survey is completely different from the ones that followed. The question on
roofing material since 1992 contains the option "palha" (straw), which has been identified as the only impermanent material
available. However, this option does not exist in 1990 and thus, given comparability across years, it has been decided to 
drop the variable altogether in 1990*/
gen resid_ch=0 if v209==0
replace resid_ch=1 if v209==2 |v209==4
replace resid_ch=2 if v209==6
replace resid_ch=3 if v209==8
replace resid_ch=. if v209==9

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen  aguamejorada_ch =.
				
*********************
***banomejorado_ch***
*********************
gen   banomejorado_ch =.


gen dorm_ch=v231
replace dorm_ch=. if v231==99 
gen cuartos_ch=v211
replace cuartos_ch=. if v211==99
gen cocina_ch=.
gen refrig_ch=(v216==1)
replace refrig_ch=. if v216==9
gen freez_ch=.
gen auto_ch=.
gen telef_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen viv1_ch=1 if v202==1
replace viv1_ch=2 if v202==3
replace viv1_ch=3 if v202==5 | v202==7
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
gen viviprop_ch=0 if v212==4
replace viviprop_ch=1 if v212==0
replace viviprop_ch=2 if v212==2
replace viviprop_ch=4 if v212==6 |v212==8
gen vivialq_ch=v213
replace vivialq_ch=. if v213>=888888888
gen vivialqimp_ch=.

/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
gen factor_ci=v1091
gen sexo_ci=1 if p303==1
replace sexo_ci=2 if p303==3
gen edad_ci=p805
replace edad_ci=. if edad_ci==999
gen civil_ci=.
gen jefe_ci=(p305==1)
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
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5) if miembros_ci==1
by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))

/******************************************************************************/
/*				VARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/
/*YL: Nota: en este año no dispongo del diccionario ni formulario. Asumo que las variables no cambian
pero tengo problemas con algunas variables. No puedo identificarlas bien debido a la falta de etiquetas
*/


****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (p501<=2)
replace condocup_ci=2 if (p501==3) /*tomaron alguna providencia en la semana de referencia*/
replace condocup_ci=3 if (p501>=4 & p501<=7)
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor 10 años"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

/*
Definiciones:
* População ocupada: Aquelas pessoas que, num determinado período de referência,
trabalharam ou tinham trabalho mas não trabalharam (por exemplo, pessoas em férias).

* População Desocupada: aquelas pessoas que não tinham trababalho, num determinado 
período de referência, mas estavam dispostas a trabalhar, e que, para isso, tomaram
alguma providência efetiva (consultando pessoas, jornais, etc.).

População Não Economicamente Ativa: pessoas não classificadas como ocupadas ou 
desocupadas

PET: >=10 años de edad
*/

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (p511==1 | p2401==1) & cotizando_ci==0 
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (p511==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (p2401==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

/* Nota: No estoy segura de esta porque no puedo entender bien el fomrulario.
gen cotizaotros_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizaotros_ci=1 if ( p3701==1) & cotizando_ci==0 
label var cotizaotros_ci "Cotizante a la Seguridad Social por otro trabajos o por aporte privado"
*/

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. /*solo se pregunta si tiene o no contrato*/
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (p5280==1 | p5281==2) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************
foreach var of varlist p578 p579 {
*2014, 01 revision MLO
replace `var'=. if `var'>=999999999
}

egen ypen_ci=rsum (p578 p579)
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
generat cesante_ci=. /*No puedo identificar esta variable, se debe tener el formulario*/
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

*************
**salmm_ci***
*************
gen salmm_ci=6056.31 /*cruzeiros*/
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=. /*No se puede identificar educación técnica superior*/
label var tecnica_ci "=1 formacion terciaria tecnica"	

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

****************
***formal_ci ***
****************
gen formal_ci=(cotizando_ci==1)

gen ocupa_ci=.
replace ocupa_ci=1 if p5030==1 & emp_ci==1
replace ocupa_ci=3 if p5030==2 & emp_ci==1
replace ocupa_ci=4 if p5030==5 & emp_ci==1
replace ocupa_ci=5 if p5030==7 & emp_ci==1
replace ocupa_ci=6 if p5030==3 & emp_ci==1
replace ocupa_ci=7 if (p5030==4 | p5030==6) & emp_ci==1 
replace ocupa_ci=9 if p5030==8 & emp_ci==1

****************
***  rama_ci ***
****************

*Ramos de atividade
  *1- agrícola
  *2- ind. de transformação
  *3- ind. da construção
  *4- out. Ativ. Industriais
  *5- comércio mercadorias
  *6- prestação de serviços
  *7- serv.aux.atividade ec.
  *8- transp. e comunicações
  *9- social
 *10- administração pública
 *11- outra atividade

 * No se puede distinguir que clasificacion CIIU utiliza.  Con la clasificacionq ue esta en el diccionario no se encuentra servicios de agua. MGD 04/29/2014
gen rama_ci=.
replace rama_ci=1 if p5040==1 & emp_ci==1
replace rama_ci=3 if (p5040==2 | p5040==4) & emp_ci==1
replace rama_ci=5 if p5040==3 & emp_ci==1
replace rama_ci=6 if p5040==5 & emp_ci==1
replace rama_ci=7 if p5040==8 & emp_ci==1
replace rama_ci=9 if (p5040==6 | p5040==7 | p5040==9 | p5040==10 | p5040==11) & emp_ci==1

label define rama_ci 1 "Agricultura, Caza, Civicultura y Pesca" 2 "Explotación de minas y Canteras" 3 "Industrias Manufactureras" 4 "Electricidad, Gas y Agua" 5 "Construcción" 6 "Comercio al por mayor y menor, Restaurantes y Hoteles" 7 "Transporte y Almacenamiento" 8 "Establecimientos Financieros, Seguros y Bienes Inmuebles" 9 "Servicios Sociales, Comunales y personales" 
label values rama_ci rama_ci


gen horaspri_ci=p508
replace horaspri_ci=. if p508==99 | emp_ci==0 
gen horastot_ci=p5100
replace horastot_ci=. if p5100>=99 | emp_ci==0
gen ylmpri_ci=p600
replace ylmpri_ci=. if p600>=999999999 | emp_ci==0
gen ylnmpri_ci=p538
replace ylnmpri_ci=. if p538>=999999999 | emp_ci==0
gen ylmsec_ci=.
gen ylnmsec_ci=.
gen nrylmpri_ci=0
replace nrylmpri_ci=1 if ylmpri_ci==. & emp_ci==1

replace p549=. if p549>=999999999 /*Para sacarnos de encima el problema de Top Coding*/
egen ylm_ci=rsum(ylmpri_ci p549)
replace ylm_ci=. if (ylmpri_ci==. & p549==.) | emp_ci==0

replace p550=. if p550>=999999999
egen ylnm_ci=rsum(ylnmpri_ci p550)
replace ylnm_ci=. if (ylnmpri_ci==. & p550==.) | emp_ci==0

foreach var of varlist p578 p579 p580 p581 p582 {
*2014, 01 revision MLO
replace `var'=. if `var'>=999999999
*replace `var'=. if `var'==999999999
}
egen ynlm_ci=rsum(p578 p579 p580 p581 p582)
replace ynlm_ci=. if  p578==. & p579==. & p580==. & p581==. & p582==. 
 
gen ynlnm_ci=.
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1
gen ylmnr_ch=ylm_ch
replace ylmnr=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1
gen ynlnm_ch=.
*2015, 03 modificacion MLO
*gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.
gen remesas_ci=.
sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if miembros_ci==1
gen durades_ci=p516 if emp_ci==0
replace durades_ci=. if p516==99 
/*gen antiguedad_ci=p3001 if emp_ci==1
replace antiguedad_ci=. if p3001==99*/

* Nota MGD 09/29/2014: se anade meses.
replace p3001=. if p3001==99 | p3001==-1
replace p3011=. if p3011==99 | p3011==-1 | p3011==88
gen aux2=p3011/12
egen antiguedad_ci=rsum(p3001 aux2) if emp_ci>=1
replace antiguedad_ci=. if p3001==. & p3011==. 
drop aux2

/******************************************************************************************/
/*					VARIABLES DEL MERCADO LABORAL			  */
/******************************************************************************************/
gen desalent_ci=.
gen subemp_ci=.
gen tiempoparc_ci=.
gen categopri_ci=1 if p505==7 | p505==8
replace categopri_ci=2 if p505==5 | p505==6
replace categopri_ci=3 if p505==1 | p505==2 | p505==3 | p505==4
replace categopri_ci=4 if p505==0
replace categopri_ci=. if emp_ci!=1
gen categosec_ci=1 if p2408==6
replace categosec_ci=2 if p2408==4
replace categosec_ci=3 if p2408==2
replace categosec_ci=4 if p2408==8
replace categosec_ci=. if emp_ci!=1 
gen nempleos_ci=1 if p2301==3
replace nempleos_ci=2 if p2301==1
replace nempleos_ci=. if p2301==. | emp_ci==0
/*
gen firmapeq_ci=0 if p2901==1
replace firmapeq_ci=1 if p2921==3 | p2941==5
replace firmapeq_ci=. if emp_ci==0

*cambio introducido en junio 13*
ren firmapeq_ci tamfirma_ci
gen firmapeq_ci=1 if tamfirma_ci==0
replace firmapeq_ci=0 if tamfirma_ci==1
replace firmapeq_ci=. if emp_ci==0
*/
gen spublico_ci=.


					****************************
					***	VARIABLES EDUCATIVAS ***
					****************************

*------------------------------------------------------------------------------------------------------------------
*YANIRA, Ag 2010: SE HACE UNA CORRECIÓN SOBRE LAS VARIABLES DE EDUCACIÓN. PUES LA VARIABLE DE INSUMO PARA CONSTRUIR 
*AÑOS DE EDUCACIÓN NO SE TUVO EN CUENTA UN CAMBIO EN LAS OPCIONES DE LAS VARIABLES INSUMO. LO CUAL GENERÓ UN ERROR
*------------------------------------------------------------------------------------------------------------------


**************
**asiste_ci***
**************

gen asiste_ci=(p314>=4 & p314<=15)
label var asiste_ci "Personas que actualmente asisten a un centro de enseñanza"


*************
***aedu_ci***
*************

gen aedu_ci=0
replace aedu_ci=. if edad_ci<5  
label var aedu_ci "Anios de educacion"


*PARA LOS QUE NO ASISTEN
*************************

*No aplica o no responde	
replace aedu_ci=. if p317==-1 & asiste_ci==0

	*Sistema antiguo
*Elementar (primário) - se asume que el máximo es 4 - Anteriormente se permitía 6 pero no 5
replace aedu_ci=min(p315,4) if p317==1 & p315>=1 & p315<=6 & asiste_ci==0
*Medio 1 ciclo (ginasial, etc) - se asume que el máximo es 8
replace aedu_ci=min(p315+4,8) if p317==2 & p315>=1 & p315<=5 & asiste_ci==0
replace aedu_ci=4  if p317==2 & p315==0 & asiste_ci==0
*Medio 2 ciclo (cientifico, clasico, etc, etc) - se asume que el máximo es 11, pero
*bajo la lógica anterior deberían se 12, ya que se permite hasta 4 años adicionales en este nivel
*Aunque solo es necesario tener 11 años de educación para completar la secundaria
replace aedu_ci=min(p315+8,12) if p317==3 & p315>=1 & p315<=4 & asiste_ci==0
replace aedu_ci=8  if p317==3 & p315==0 & asiste_ci==0

	*Sistema nuevo
*Primeiro grau - Bajo este sistema la primaria llega hasta el grado 8
replace aedu_ci=min(p315,8) if p317==4 & p315>=1 & p315<=8 & asiste_ci==0
replace aedu_ci=0  if p317==4 & p315==0 & asiste_ci==0
*Segundo grau - Secundaria son 4 años más
replace aedu_ci=min(p315+8,12) if p317==5 & p315>=1 & p315<=4 & asiste_ci==0
replace aedu_ci=8 if p317==5 & p315==0 & asiste_ci==0

*Superior
replace aedu_ci=min(p315+11,17) if p317==6 & p315>=1 & p315<=8 & asiste_ci==0
replace aedu_ci=11 if p317==6 & p315==0 & asiste_ci==0

*Maestria o doctorado  
*Para este ciclo no se pregunta el último año aprobado. Por lo tanto se supone que si terminó el ciclo 
*el individuo cuenta con 19 años de educación (2 años más de educación), si el individuo no terminó se le agrega 
*1 año más de eduación para quedar con 18 ya que si el último ciclo más alto alcanzado es postgrado, el individuo 
*por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if p317==7 & p315==0 & asiste_ci==0
replace aedu_ci=19 if p317==7 & p315==0 & asiste_ci==0


*PARA LOS QUE ASISTEN
**********************

*No aplica o no responde	
replace aedu_ci=. if p314==-1 & asiste_ci==1

*Pre-escolar, creche o alfabetización de adultos
replace aedu_ci=0 if (p314==7 | p314==8) & asiste_ci==1

*Regular de 1º grau/ Supletivo de 1º grau   (se asume que el máximo es 8) 
replace aedu_ci=min(p312-1,7) if (p314==4 | p314==9 | p314==11) & p312>=1 & p312<=8 & asiste_ci==1

*Regular de 2º grau/ Supletivo de 2º grau   (se asume que el máximo es 4, pero con 3 basta para completar el ciclo)
replace aedu_ci=min(p312+8-1,11) if (p314==5 | p314==10 | p314==12) & p312>=1 & p312<=4 & asiste_ci==1

*Pre-vestibular
replace aedu_ci=11  if p314==14 & asiste_ci==1

*Superior
replace aedu_ci=min(p312+11-1,17) if p314==6 & p312>=1 & p312<=8 & asiste_ci==1
replace aedu_ci=11 if p314==5 & p312==. & asiste_ci==1

*Maestria o doctorado  
*Si el último ciclo más alto alcanzado es postgrado, el individuo por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if p314==15  & asiste_ci==1

*Se deja sólo la información de las personas con 5 años o más
replace aedu_ci=. if edad_ci<5




**************
***eduno_ci***
**************
gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<8
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==8
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>8 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**************
***eduuc_ci***
**************
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
*La secundaria sólo dura 4 años. No puede divirse en ciclos
gen edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***************
***edus2i_ci***
***************
gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

***************
***edus2c_ci***
***************
gen edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo" 

***************
***edupre_ci***
***************
gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


foreach var of varlist edu* {
replace `var'=. if aedu_ci==.
}

******************
***pqnoasist_ci***
******************
gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci = .

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
label var  edupub_ci "Personas que asisten a centros de enseñanza públicos"
	


*******************
***tamemp_ci*******
*******************
gen tamemp_ci=1 if p2901==1  
replace tamemp_ci=2 if p2921==3
replace tamemp_ci=3 if p2941==5
label var  tamemp_ci "Tamaño de Empresa" 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

******************
***categoinac_ci**
******************
gen categoinac_ci=.
/*cambian las variables de la encuesta con respecto a años siguientes, por eso se deja como missing

gen categoinac_ci=1 if p501==6 & condocup_ci==3
replace categoinac_ci=2 if p501==4 & condocup_ci==3
replace categoinac_ci=3 if p501==5 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
*/
label var  categoinac_ci "Condición de Inactividad" 
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo


*variables que faltan generar
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen edus1c_ci=.
gen repiteult_ci=.
gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.
gen vivitit_ch=.
gen ylmotros_ci=.
gen ylnmotros_ci=.

**Cambio de moneda - Modificación Mayra Sáenz Septiembre 2014

sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch

local varing "ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch"
foreach e of local varing {
replace `e' = `e'/2750000 
}

sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch



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




compress


saveold "`base_out'", replace


log close
