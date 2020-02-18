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

local PAIS HTI
local ENCUESTA DHS
local ANO "2016_2017"
local ronda m11_m4

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Haití
Encuesta: DHS
Round: m11-m4, 2017
Autores:
Alvaro Altamirano Montoya
Última versión: Alvaro Altamirano - Email: alvaroalt@iadb.org, ajaltamiranomontoya@gmail.com
Fecha última modificación: 8 de Octubre de 2019

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
****************************************************************************/
set maxvar 10000
use `base_in', clear

***********
* Region_c *
************
rename v101 region_c

**************
* Región BID *
**************
gen region_BID_c=.
replace region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*************************
*  Factor de expansión  *
*************************
gen wgt_h = hv005/1000000
ren  wgt_h factor_ch
label variable factor_ch "Factor de expansion del hogar"

**************************
* Identificador del hogar*
**************************
gen idh_ch=hhid
label variable idh_ch "ID del hogar"

****************************
* Identificador de persona *
****************************
clonevar idp_ci=caseid
label variable idp_ci "ID de la persona en el hogar"
sort hhid caseid
***************************
* Zona urbana o zona rural*
***************************
gen zona_c=.
replace zona_c=1 if hv025==1
replace zona_c=0 if hv025==2
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

*************************
* País					*
*************************

gen pais_c="HTI"
label variable pais_c "Pais"

****************************
* Anio de la encuesta ******

gen anio_c=2017
label variable anio_c "Anio de la encuesta"

***********************
*  Mes de la encuesta *
***********************

gen mes_c=8
label variable mes_c "Mes de la encuesta"



*******************************
*Relación con el jefe de hogar*
*******************************
/*
lien de parenté:
           1  chef de ménage
           2  epoux(se)/conjoint(e)
           3  fils/fille
           4  petit-fils/petite-fille
           5  papa/maman
           6  beau-père/belle-mère
           7  beau-fils/belle-fille
           8  frère/soeur
           9  oncle/tante
          10  neveu/nièce
          11  cousin/cousine
          12  autre parent
          13  personnel de maison
          14  domestique = restavèk
          15  autre
*/
egen relationship = rowmax(v150 mv150)
gen relacion_ci=.
replace relacion_ci=1 if relationship==1
replace relacion_ci=2 if relationship==2
replace relacion_ci=3 if relationship==3
replace relacion_ci=4 if (relationship>=4 & relationship<=11) | relationship==13
replace relacion_ci=5 if relationship==13 | relationship==14

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 6 "Otros no parientes"
label value relacion_ci relacion_ci

****************************************
*Factor de expansión a nivel individual*
****************************************
gen wgt_individuals = v005/1000000
gen factor_ci=wgt_individuals
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
recode _merge (1 =2) (2=1)
gen sexo_ci=_merge  
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
egen age = rowmax(mv012 v012)
gen edad_ci=age 
label variable edad_ci "Edad del individuo"

**************
***civil_ci***
**************
egen civil = rowmax(v501 mv501)

gen civil_ci=1 if civil==0
replace civil_ci=2 if civil==1 | civil==2 
replace civil_ci=3 if civil==4 | civil==5
replace civil_ci=4 if civil==3
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

**************
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
gen miembros_ci=(relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"

		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
****************
****condocup_ci*
****************
egen trabaja=rowmax(v714 mv714)
egen ausente=rowmax(v714a mv714a)
egen paraquien_trabaja= rowmax(v719 mv719)
egen earnings_type=rowmax(v741 mv741)


gen condocup_ci=.
replace condocup_ci=1 if trabaja==1 | ausente==1
replace condocup_ci=2 if trabaja!=1 & ausente!=1
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label define condocup_ci 1 "ocupados" 2 "desocupados" 3 "inactivos" 4 "menor de edad (10)"
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
replace pea_ci=1 if emp_ci==1 | desemp_ci==1

****************
*afiliado_ci****
****************
egen seguro_salud=rowmax(v481 mv481)

gen seguro_salud_ci=seguro_salud
label var seguro_salud_ci "Posee seguro de salud"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*cotizapri_ci***
****************
*La pregunta es hecha solamente para la actividad principal
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


/*No hay información de valores de beneficio pensional*/
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
egen trabajo_12meses = rowmax(v731 mv731)
*A falta de consulta sobre si ha trabajado antes, usamos la pregunta  de si ha trabajó el anio pasado pero ahorita está desocupado
generate cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if trabajo_12meses==1 & condocup_ci==2
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

gen rama_ci =.

*************
**salmm_ci***
*************
*Alvaro AM: Fuente: Ver último párrafo de la página 140 del informe de DIAL: http://www.ihsi.ht/pdf/ecvmas/analyse/IHSI_DIAL_Rapport%20complet_11072014.pdf
generat salmm_ci=.
label var salmm_ci "Salario minimo legal"


*****************
***desalent_ci***
*****************
gen desalent_ci=.

******************************
*	subemp_ci
******************************
gen tothoras = .
gen subemp_ci= .

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.

*****************
***horaspri_ci***
*****************

gen horaspri_ci=.

*****************
***horastot_ci***
*****************
gen horastot_ci=.

******************
***categopri_ci***
******************
egen type_contract = rowmax(v719 mv719)
clonevar  tipo_contrato = type_contract

gen categopri_ci=.
replace categopri_ci=1 if tipo_contrato==3
replace categopri_ci=2 if tipo_contrato==1 | tipo_contrato==2
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1 "Cuenta propia"  
label define categopri_ci 2 "Empleado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************

gen categosec_ci=.

*****************
*tipocontrato_ci*
*****************
egen type_earnings = rowmax(v741 mv741)
clonevar  tipopago_ci = type_earnings

label var tipopago_ci "type of earnings from respondent's work"
label define tipopago_ci 0 "not paid" 1 "cash only" 2 "cash and in-kind"  3 "in-kind only"
label value tipopago_ci tipopago_ci

***************
***segsoc_ci***
***************
/*gen segsoc_ci=.
label variable segsoc_ci "Tiene Seguro Social"
*/
*****************
***nempleos_ci***
*****************
gen nempleos_ci=.

*****************
***tamfirma_ci***
*****************
*Estructura de tamano de firma adhoc, debido a las opciones de la pregunta en esta base.
gen tamfirma_ci=.

*****************
***spublico_ci***
*****************
*Nota: En el año 2009 no se pregunta acerca del tipo de establecimiento en que trabaja
gen spublico_ci=.
label var spublico_ci "Trabajador del sector público"

**************
***ocupa_ci***
**************
egen ocupa_aux = rowmax(v716 mv716)
clonevar ocupa_ci = ocupa_aux

****************
***durades_ci***
****************
gen durades_ci= . 
label variable durades_ci "Duracion del desempleo en meses- Intervalos"
label def durades_ci 1 "Menos de un mes" 2 "1 mes a menos de 2 meses" 3 "2 meses a menos de 3 meses"
label def durades_ci 4 "3 meses a menos de 6 meses" 5 "6 meses a menos de 1 año" 6"Más de 1 año", add
label val durades_ci durades1_ci 

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en el empleo (en años)"

*************
*tamemp_ci***
*************
gen tamemp_ci=. 
label var tamemp_ci "# empleados en la empresa de la actividad principal"

****************************
***VARIABLES DE EDUCACION***
****************************

*************
***aedu_ci*** 
*************
egen educ_aux = rowmax(v133 mv133)
clonevar aedu_ci=educ_aux

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
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"
***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"

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

/*
***************
***asiste_ci***
***************
gen asiste_ci=(hh_e16a==1)
replace asiste_ci=1 if i_h08a==1
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************
*Pregunta contiene restricción de edad (menores de 10 años)
recode hh_e16c (-9=.)
gen pqnoasis_ci=hh_e16c
label var pqnoasis_ci "Razones para no asistir"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

***************
***repite_ci***
***************
gen repite_ci=.

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"

***************
***edupub_ci***
***************

gen edupub_ci=hh_e16b==1
replace edupub_ci=1 if i_h09==1
label var edupub_ci "Asiste a un centro de enseñanza público"

*************
***tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

****************
***aguared_ch***
****************
gen aguared_ch=.
replace aguared_ch=(hh_c10a==1)
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
/* Existe la siguiente distribución de fuentes de agua, pero la misma no permite decir dónde se encuentra la fuente, suponer sería riesgoso me parece.
1. robinet privé - dinepa
2. fontaine publique
3. puit artésien / forage
4. puit protégé
5. source d'eau protégé
6. eau de pluie
7. kiosque (vendeur d'eau traitée)
8. eau traitée (camion, bouteille, sach
9. puit non protégé
10. source d'eau non protégée
11. eau non traitée (camion, bouteille
12. eau de surface (rivière, lac, mare
*/
gen aguadist_ch=.
/*
replace aguadist_ch=1 if hh_c10a==1
replace aguadist_ch=2 if hh_c10a==2 
replace aguadist_ch=3 if hh_c10a>=3 & hh_c10a<=10
replace aguadist_ch=. if hh_c10a==98 | hh_c10a==99
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1 "Dentro de la vivienda" 2 "Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3 "Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch
*/
*****************
***aguamala_ch***
*****************
gen aguamala_ch=(hh_c10a>6 & hh_c10a!=.)

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.

************
***luz_ch***
************

gen luz_ch=(hh_c09a<4 & hh_c09a!=.)
label var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************

gen luzmide_ch=(hh_c09a<3 & hh_c09a!=.)
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
gen combust_ch= (hh_d01==2 | hh_d01==3 | hh_d01==5 | hh_d01==6 & hh_d01!=.)
label var combust_ch "Principal combustible gas o electricidad" 

*************
***bano_ch***
*************

gen bano_ch=(hh_d02<6 & hh_d02!=.)
label var bano_ch "El hogar tiene servicio sanitario"

***************
***banoex_ch***
***************
gen banoex_ch=(hh_d02==1 | hh_d02==2 | hh_d02==4 & hh_d02!=.)
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if hh_c11==5 | hh_c11==6
replace des1_ch=1 if hh_c11==1 /*Red o pozo septico*/
replace des1_ch=2 if hh_c11==2 /*Letrina */
replace des1_ch=3 if hh_c11==4 | hh_c11==5 | hh_c11==6 /*Superficie*/
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0 "No tiene servicio sanitario" 1 "Conectado a red general o cámara séptica"
label def des1_ch 2 "Letrina o conectado a pozo ciego" 3 "Desemboca en río o calle", add
label val des1_ch des1_ch

*************
***des2_ch***
*************
gen des2_ch=.
replace des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2
replace des2_ch=2 if des1_ch==3
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0 "No tiene servicio sanitario" 1 "Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2 "Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************
gen piso_ch=.
replace piso_ch=0 if hh_c03==2
replace piso_ch=1 if hh_c03!=2 & hh_c03!=.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0 "Piso de tierra" 1 "Materiales permanentes"
label val piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch=.
replace pared_ch=0 if hh_c01==1 | hh_c01==2 | hh_c01==4 | hh_c01==5 | hh_c01==7
replace pared_ch=1 if hh_c01==3 | hh_c01==6
replace pared_ch=. if hh_c01==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0 "No permanentes" 1 "Permanentes"
label val pared_ch pared_ch

**************
***techo_ch***
**************
gen techo_ch=.
replace techo_ch=0 if hh_c02==1 | hh_c02==4 | hh_c02==6
replace techo_ch=1 if hh_c02==2 | hh_c02==3 | hh_c02==5
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0 "No permanentes" 1 "Permanentes"
label val techo_ch techo_ch

**************
***resid_ch***
**************
recode hh_c12 (-9=.)
gen resid_ch=.
replace resid_ch=0 if hh_c12==1 | hh_c12==2
replace resid_ch=1 if hh_c12==8 | hh_c12==9
replace resid_ch=2 if hh_c12==3 | hh_c12==4 | hh_c12==6 | hh_c12==7
replace resid_ch=3 if hh_c12==5
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0 "Recolección pública o privada" 1 "Quemados o enterrados"
label def resid_ch 2 "Tirados a un espacio abierto" 3 "Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .

*************
***dorm_ch***
*************
gen dorm_ch=hh_c04a
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
gen  cuartos_ch =hh_c04b+hh_c04a
label var cuartos_ch "Habitaciones en el hogar"

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
gen telef_ch=.
label var telef_ch "El hogar tiene servicio telefónico fijo"

gen cel_ch=.

***************
***refrig_ch***
***************
gen refrig_ch = .
label var refrig_ch "El hogar posee refrigerador o heladera"

**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"


*************
***auto_ch***
*************

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"

*************
***compu_ch***
**************
gen compu_ch=.
label var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************
gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"

**************
***vivi1_ch***
**************
gen vivi1_ch=.
replace vivi1_ch= 1 if hh_b16a<6
replace vivi1_ch= 2 if hh_b16a==6
replace vivi1_ch= 3 if  hh_b16a==8 | hh_b16a==9 | hh_b16a==10
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros"
label val vivi1_ch vivi1_ch

*************
***vivi2_ch***
*************
gen vivi2_ch =.
replace vivi2_ch = 1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch = 2 if vivi1_ch==3 
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
gen viviprop_ch =.
replace viviprop_ch = 0 if hh_c05==2 | hh_c05==3 
replace viviprop_ch = 1 if hh_c05==1
replace viviprop_ch = 2 if hh_c05==2
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0 "Alquilada" 1 "Propia y totalmente pagada" 2 "propia y en proceso de pago"
label def viviprop_ch 3 "Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch = .
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
* Alquiler en gourdes 
recode hh_c07a (-9 -8 =.)
g auxalqui = hh_c07a if  hh_c07b==1 
replace auxalqui = hh_c07a/5 if  hh_c07b==2
replace auxalqui = hh_c07a*0.02378 if  hh_c07b==3
rename auxalqui vivialq_ch
label var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"


*********
*raza_ci*
*********
*no consta la pregunta acerca del grupo étnico, pero sí de nacionalidad, que es la i_h03a (99.8% reportan ser haitianos)
gen raza_ci=.
gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

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
relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	///
 ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	 ///
 tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci , first


compress

global ruta = "\\Sdssrv03\surveys"

local PAIS HTI
local ENCUESTA DHS
local ANO "2016_2017"
local ronda m11_m4

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

saveold "`base_out'", replace


log close
