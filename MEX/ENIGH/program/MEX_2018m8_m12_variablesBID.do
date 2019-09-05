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

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2018"
local ronda m8_m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (tradicional)
Round: Septiembre-Diciembre
Autores:
Versión 2013: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última versión: Alvaro Altamirano- Email: alvaroalt@iadb.org
Fecha última modificación: Agosto de 2019

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


******************************************************************************
*	HOUSEHOLD VARIABLES
******************************************************************************

*****************
*** region_c ***
*****************
*Nota: generada solo para el 2010
gen region_c=real(substr(ubica_geo,1,2))
label define region_c ///
1 "Aguascalientes" ///
2 "Baja California" ///
3 "Baja California Sur" ///
4 "Campeche" ///
5 "Coahuila de Zaragoza" ///
6 "Colima" ///
7 "Chiapas" ///
8 "Chihuahua" ///
9 "Distrito Federal" ///
10 "Durango" ///
11 "Guanajuato" ///
12 "Guerrero" ///
13 "Hidalgo" ///
14 "Jalisco" ///
15 "México" ///
16 "Michoacán de Ocampo" ///
17 "Morelos" ///
18 "Nayarit" ///
19 "Nuevo León" ///
20 "Oaxaca" ///
21 "Puebla" ///
22 "Querétaro" ///
23 "Quintana Roo" ///
24 "San Luis Potosí" ///
25 "Sinaloa" ///
26 "Sonora" ///
27 "Tabasco" ///
28 "Tamaulipas" ///
29 "Tlaxcala" ///
30 "Veracruz de Ignacio de la Llave" ///
31 "Yucatán" ///
32 "Zacatecas" 
label value region_c region_c
label var region_c "division politico-administrativa, estados"


******************************
*	factor_ch
******************************
gen factor_ch=factor
label var factor_ch "Household Expansion Factor"

******************************
*	idh_ch
******************************
sort  folioviv foliohog 
egen idh_ch= group(folioviv foliohog)
label var idh_ch "ID del hogar"

******************************
*	idp_ci
******************************
destring numren, replace
gen idp_ci=numren
label var idp_ci "ID de la persona en el hogar"

******************************
*	zona_c
******************************
/*destring tam_loc, replace
gen zona_c= (tam_loc==1 | tam_loc==2)
label var zona_c "Area of the country"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c*/

/*
tam_loc:
1 Localidades con 100 000 y más habitantes
2 Localidades con 15 000 a 99 999 habitantes
3 Localidades con 2 500 a 14 999 habitantes
4 Localidades con menos de 2500 habitantes
*/

*Modificación Mayra Sáenz - Agosto 2015 Se reemplaza la clasificación de zona por la que consta en la sintaxis de CONEVAL

gen zona_c= 1      if tam_loc<="3"
replace zona_c = 0 if tam_loc=="4"
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural", add modify
label value zona_c zona_c

******************************
*	pais_c
******************************
gen str3 pais_c="MEX"
******************************
*	anio_c
******************************
gen anio_c=2018
label var anio_c "Year of the survey"

*****************
*** region según BID ***
*****************
gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

******************************
*	mes_c
******************************
gen mes_c= .
******************************
*	relacion_ci
******************************

gen relacion_ci=.
replace relacion_ci=1 if parentesco=="101" | parentesco=="102"
replace relacion_ci=2 if parentesco>="201" & parentesco<="204"
replace relacion_ci=3 if parentesco>="301" & parentesco<="305"
replace relacion_ci=4 if parentesco>="601" & parentesco<="623"
replace relacion_ci=5 if (parentesco>="501" & parentesco <="503") | (parentesco>="701" & parentesco<="715")
replace relacion_ci=6 if parentesco>="401" & parentesco<="461"
replace relacion_ci=. if parentesco=="999" | parentesco=="."
label var relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
label value relacion_ci relacion_ci

/*
Clasificar a los integrantes del hogar por clave de
parentesco (tabla Poblacion): Jefe: contar los registros con
parentesco igual a 101 o 102; Esposa: contar los registros
con parentesco de 201 a 204; Hijos: contar los registros
con parentesco de 301 a 304; Parientes: contar los
registros con parentesco de 601 a 623; NoParientes:
contar los registros con parentesco de 501 a 503.

*Códigos de parentesco
101 Jefe(a)
102 Persona sola
201 Esposo(a), compañero(a), cónyuge, pareja, marido, mujer, señor(a), consorte
202 Concubino(a)
203 Amasio(a)
204 Querido(a), amante
301 Hijo(a), hijo(a) consanguíneo, hijo(a) reconocido
302 Hijo(a) adoptivo(a)
303 Hijastro(a), entenado(a)
304 Hijo(a) de crianza
305 Hijo(a) recogido(a)
401 Trabajador(a) doméstico(a)
402 Recamarero(a)
403 Cocinero(a)
404 Lavandera(o)
405 Nana, niñera, nodriza
406 Mozo
407 Jardinero(a)
408 Velador, vigilante
409 Portero(a)
410 Chofer
411 Ama de llaves
412 Mayordomo
413 Dama de compañía, acompañante
421 Esposo(a) del(la) trabajador(a) doméstico(a)
431 Hijo(a) del(la) trabajador(a) doméstico(a)
441 Madre, padre del(la) trabajador(a) doméstico(a)
451 Nieto(a) del(la) trabajador(a) doméstico(a)
461 Otro pariente del(la) trabajador(a) doméstico(a)
501 No tiene parentesco
502 Tutor(a)
503 Tutelado(a), pupilo(a), alumno(a)
601 Madre, padre
602 Padrastro, madrastra
603 Hermano(a)
604 Medio(a) hermano(a)
605 Hermanastro(a)
606 Abuelo(a)
607 Bisabuelo(a)
608 Tatarabuelo(a)
609 Nieto(a)
610 Bisnieto(a)
611 Tataranieto(a)
612 Tío(a)
613 Sobrino(a)
614 Primo(a)
615 Suegro(a)
616 Consuegro(a)
617 Nuera, yerno
618 Cuñado(a)
619 Concuño(a)
620 Padrino, madrina
621 Ahijado(a)
622 Compadre, comadre
623 Familiar, otro parentesco
701 Huésped, abonado(a), pensionista
711 Esposo(a) del(la) huésped
712 Hijo(a) del(la) huésped
713 Madre o padre del(la) huésped
714 Nieto(a) pariente del(la) huésped
715 Otro(a) pariente del(la) huésped
999 Parentesco no especificado

*/



******************************************************************************
*	DEMOGRAPHIC VARIABLES
******************************************************************************

******************************
*	factor_ci
******************************
gen factor_ci=factor
label var factor_ci "Individual Expansion Factor"
******************************
*	sexo_ci
******************************
gen sexo_ci=real(sexo)
******************************
*	edad_ci
******************************
gen edad_ci=edad 
******************************
*	civil_ci
******************************

* MGR Nov, 2015: corrección en sintaxis

gen civil_ci=.
replace civil_ci=1 if edo_cony=="6"
replace civil_ci=2 if edo_cony=="1"|edo_cony=="2"
replace civil_ci=3 if edo_cony=="3"|edo_cony=="4"
replace civil_ci=4 if edo_cony=="5"
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci

/*
gen civil_ci=.
replace civil_ci=1 if edo_cony=="5"
replace civil_ci=2 if edo_cony=="1"|edo_cony=="6"
replace civil_ci=3 if edo_cony=="2"|edo_cony=="3"
replace civil_ci=4 if edo_cony=="4"
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci
*/
/*
1 vive con su pareja en unión libre?
2 está casado(a)?
3 está separado(a)?
4 está divorciado(a)?
5 es viudo(a)?
6 está soltero(a)?
*/

******************************
*	jefe_ci
******************************
gen jefe_ci=(relacion_ci==1)
label var jefe_ci "Jefe de hogar"

***************************************************************************
*	nconyuges_ch & nhijos_ch & notropari_ch & notronopari_ch & nempdom_ch
****************************************************************************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label var nconyuges_ch "Numero de conyuges"
label var nhijos_ch "Numero de hijos"
label var notropari_ch "Numero de otros familiares"
label var notronopari_ch "Numero de no familiares"
label var nempdom_ch "Numero de empleados domesticos"

******************************
*	clasehog_ch
******************************
gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
label var clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

***************************************************************************************
*	nmiembros_ch & nmayor21_ch & nmenor21_ch & nmayor65_ch & nmenor6_ch & nmenor1_ch  
***************************************************************************************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label var nmiembros_ch "Numero de familiares en el hogar"

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
gen miembros_ci=(relacion_ci<5)
label var miembros_ci "Miembro del hogar"

*option: gen miembros_ci=((paren>=100 & paren<=300) | (paren>=500 & paren<=700)) 


******************************************************************************
*	LABOR MARKET
******************************************************************************
****************
****condocup_ci*
****************
/* son las mismas solo las primeras estan en destring
trabajon=trabajo
verificn=verifica
*/
gen trabajon=real(trabajo_mp)
gen mot_ausen=real(motivo_aus)

generat condocup_ci=.
replace condocup_ci=1 if (trabajon==1) | (mot_ausen <=6)
replace condocup_ci=2 if act_pnea1=="1" | act_pnea2=="1" 
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad<12
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 12"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
/*Nota: En el esquema de la ENOE se considera a la población en edad de 
trabajar como aquella de catorce años en adelante, de acuerdo con la Ley 
Federal del Trabajo.
Fuente:http://www.inegi.org.mx/inegi/contenidos/espanol/prensa/comunicados/ocupbol.asp */


****************
*afiliado_ci****
****************
destring pres_* servmed* inscr_* inst_* atemed tam_emp1  contrato1, replace
generat afiliado_ci=0 if condocup_ci==1 | condocup_ci==2  
replace afiliado_ci=1 if (pres_81==8 | pres_82==8) | (inscr_1 == 1  & (servmed_3==3 | servmed_5==5 | servmed_6==6 | servmed_7==7))  /* inscrito en prestaciones de salud por trabajo*/
*replace afiliado_ci=1 if (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & atemed==1 & afiliado_ci==0 
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

* Formalidad sin restringir a PEA
destring pres_* servmed* inscr_* inst_* atemed tam_emp1  contrato1, replace
generat afiliado_ci1=0 if condocup_ci>=1 & condocup_ci<=3  
replace afiliado_ci1=1 if (pres_81==8 | pres_82==8) | (inscr_1 == 1  & (servmed_3==3 | servmed_5==5 | servmed_6==6 | servmed_7==7))  /* inscripto en prestaciones de salud por trabajo*/

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 
****************
*cotizando_ci***   
****************
gen cotizando_ci=. /*Revisar las variables inst_1 ó pres_91 */
label var cotizando_ci "Cotizante a la Seguridad Social"
*Nota: solo seguro social publico, con el cual tenga derecho a pensiones en el futuro.
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
*instpen_ci*****
****************
gen instpen_ci=. /*Revisar la variable inst_1 inst_2 inst_3 inst_4 */
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*************
**pension_ci*
*************
*generat pension_ci=(ing_1P032>0 & ing_1P032!=.) /* A todas las per mayores de cinco*/
*Modificación Mayra Sáenz - Agosto 2015: a partir de 2002 se puede diferenciar la pension nacional o del extranjero, se considera solo la nacional.
g pension_ci = (ypension>0 & ypension!=.)
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
*gen ypen_ci=ing_1P032 if pension_ci==1
*Modificación Mayra Sáenz - Agosto 2015
gen ypen_ci=ypension  if pension_ci==1
label var ypen_ci "Valor de la pension contributiva"

*****************
**  ypensub_ci  *
*****************
* Modificacion MLO 2014, 05
/*gen yp70mas=ing_1P044
gen yotroam=ing_1P045
gen yoportuni70=ing_1P042 if edad_ci>=70  /* solo se los dan a los que no entraron por SEDESOL*/
*/
* Oportunidades : Special cash transfers for every adult 70 years or older who is a member of a beneficiary family meanwhile its incorporated to the SEDESOL´s Program 70 and more.

*Modificación Mayra Sáenz - Agosto 2015 - Se modificó la base de datos original, por lo que se cambian los nombres de las variables.
*Alvaro AM - Agosto 2019: modifiqué el nombre a yp65más porque a partir de 2013 el programa redujo el requisito de edad de 70 a 68/65 (68 en general y 65 para población indígena).
gen yp65mas=P044
gen yotroam=P045
gen yoportuni70=P042 if edad_ci>=70  /* solo se los dan a los que no entraron por SEDESOL*/

egen ypensub_ci=rsum(yp65mas yotroam yoportuni70) 
replace ypensub_ci=. if yp65mas==. & yotroam==. & yoportuni70==.

*egen ypensub_ci=rsum(ing_1P044 ing_1P045 ing_1P042) 
*replace ypensub_ci=. if ing_1P044==. & ing_1P045==. & ing_1P042==.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
*Programas: Beneficio del programa 70 y más; Beneficio de otros programas para adultos mayores; y, Oportunidades

***************
*pensionsub_ci*
***************
gen pensionsub_ci=(ypensub_ci>0 & ypensub_ci!=.)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*************
* cesante_ci* 
*************
generat cesante_ci=. /*Discutir sobre las variables ing_1P022 o segsoc */
label var cesante_ci "Desocupado - definicion oficial del pais"


*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci=3001.17 if zona_c==1
replace lp_ci=1941.01 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci= 1516.62 if zona_c==1
replace lpe_ci= 1073.69 if zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*Daniela Zuluaga - Enero 2018
*http://www.sat.gob.mx/informacion_fiscal/tablas_indicadores/Paginas/salarios_minimos.aspx
/*Por resolución para la aplicación del salario mínimo Mexico habrá una sola área geográfica integrada por todos los municipios del país y demarcaciones 
territoriales (Delegaciones) de la Ciudad de México a partir del 1 de Octubre de 2015
*/

* Alvaro AM - SM 2018: https://www.gob.mx/cms/uploads/attachment/file/285013/TablaSalariosMinimos-01ene2018.pdf
gen salmm_ci=88.36*30 

label var salmm_ci "Salario minimo legal"

/*
******************************
*	emp_ci
******************************
*trabajo= trabajo durante el mes pasado

gen emp_ci=.
replace emp_ci=1 if trabajon==1 
replace emp_ci=0 if trabajon==2
replace emp_ci=. if trabajon==.
replace emp_ci=1 if (mot_ausen <=6)
label var emp_ci "1 Empleado"


******************************
*	desemp1_ci	& desemp2_ci & desemp3_ci 
******************************
gen desemp1_ci=(emp_ci==0 & act_buscot=="1")
replace desemp1_ci=. if emp_ci==.  
label var desemp1_ci "Personas sin trabajo que buscaron en el periodo de referencia"
 
gen desemp2_ci=.
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ult semana pero esperan respuesta de solicit"
 
gen desemp3_ci=.
label var desemp3_ci "des2 + no tienen trabajo pero buscaron antes de la semana pasada"


******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=.
gen pea3_ci=.
*/
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

******************************
*	desalent_ci
******************************
gen desalent_ci=.
/*NA: No se puede generar. Entrarian en 'no busco trabajo' por 'otra razon'*/
******************************
*	subemp_cim
******************************
gen subemp_ci=.
label var subemp_ci "Dispuestas a trabajar mas, pero trabajan 30hs o menos(semana)"
*NA 
******************************
*	horaspri_ci
******************************
gen horaspri_ci=htrab1 if emp_ci==1 & htrab1<148
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"
*NA


******************************
*	horastot_ci
******************************

egen horastot_ci= rsum(htrab1 htrab2)  if emp_ci==1 
replace horastot_ci = . if  horastot_ci>148
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"


******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci=. 
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"
*NA

******************************
*	categopri_ci
******************************
/*
gen categopri_ci=.
replace categopri_ci=1 if personal1=="1"
replace categopri_ci=2 if personal1=="2"
replace categopri_ci=3 if subor1=="1"
replace categopri_ci=4 if pago1== "2" 
replace categopri_ci=. if emp_ci!=1
label var categopri_ci "Categoria ocupacional trabajo principal"
label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4"Familiar no remunerado"
label value categopri_ci categopri_ci
*/
gen categopri_ci=.
replace categopri_ci=1 if personal1=="1" & condocup_ci==1
replace categopri_ci=2 if  categopri_ci!=1 & indep1=="1" & condocup_ci==1
replace categopri_ci=3 if subor1=="1" & condocup_ci==1
replace categopri_ci=4 if pago1== "2"  & condocup_ci==1
replace categopri_ci=. if emp_ci!=1
label var categopri_ci "Categoria ocupacional trabajo principal"
label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4"Familiar no remunerado"
label value categopri_ci categopri_ci


******************************
*	categosec_ci
******************************
gen categosec_ci=. 
replace categosec_ci=1 if personal2=="1"
replace categosec_ci=2 if indep2=="1"
replace categosec_ci=3 if subor2=="1"
replace categosec_ci=4 if pago2== "2" 
replace categosec_ci=. if emp_ci!=1
label var categosec_ci "Categoria ocupacional trabajo secundario"
label define categosec_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4"Familiar no remunerado"
label value categosec_ci categosec_ci


*****************
*tipocontrato_ci*
*****************
/*13. ¿En su trabajo cuenta con un contrato escrito?
1-si
2-no
3-no sabe
14. El contrato...
1-¿Es temporal o por obra determinada?..........
2-¿Es de base, planta o por tiempo indeterminado?...............................................
3-No sabe..........................................................
*/
/*
generat tipocontrato_ci=. /* Solo disponible para asalariados y trab independ*/
*replace tipocontrato_ci=1 if contrato1==2 
*replace tipocontrato_ci=2 if contrato1==1              
*replace tipocontrato_ci=. if contrato1==3
*2014, 05 Modificacion MLO
destring contrato1 tipocontr1, replace
replace tipocontrato_ci=1 if contrato1==1 & tipocontr1==2
replace tipocontrato_ci=2 if contrato1==1 & tipocontr1==1   
replace tipocontrato_ci=3 if contrato1==2        
replace tipocontrato_ci=. if contrato1==3
*/

* Corregido por categopri_ci MGD 06/17/2014
destring contrato1 tipocontr1, replace
g tipocontrato_ci=.
replace tipocontrato_ci=1 if (contrato1==1 & tipocontr1==2) & categopri_ci==3
replace tipocontrato_ci=2 if (contrato1==1 & tipocontr1==1) & categopri_ci==3
replace tipocontrato_ci=3 if (contrato1==2 | tipocontrato_ci==.) & categopri_ci==3      
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


******************************
*	segsoc_ci
******************************
destring segsoc , replace
gen segsoc_ci=0 if emp_ci == 1
replace segsoc_ci=1 if (segsoc ==1)
label var segsoc_ci "1=Cuenta con SS"

******************************
*	nempleos_ci
******************************
gen nempleos_ci=.
replace nempleos_ci=1 if num_trabaj=="1"
replace nempleos_ci=2 if num_trabaj=="2"
label var nempleos_ci "numero de empleos"
label define nempleos_ci 1 "un empleo" 2 "mas de en empleo"
label value nempleos_ci nempleos_ci
/*
******************************
*	firmapeq_ci
******************************
gen firmapeq_ci=0 if emp_ci==1
replace firmapeq_ci=1 if tam_emp1==1 | tam_emp1==2
label var firmapeq_ci "1=5 o menos trabajadores"
*/
******************************
*	spublico_ci
******************************
*2015, 10 Incorporacion MLO
destring clas_emp1, replace
gen spublico_ci=(clas_emp1==3 & condocup_ci==1)



******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************

tostring sinco1, replace
gen ocupa=real(substr(sinco1,1,2))
/*
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa==11 | ocupa==12) & emp_ci==1
replace ocupa_ci=2 if (ocupa==21) & emp_ci==1
replace ocupa_ci=3 if (ocupa==51 | ocupa==61 |ocupa==62) & emp_ci==1
replace ocupa_ci=4 if (ocupa==71 | ocupa==72) & emp_ci==1
replace ocupa_ci=5 if (ocupa==13 |ocupa==14 | ocupa==81 | ocupa==82) & emp_ci==1
replace ocupa_ci=6 if (ocupa==41) & emp_ci==1
replace ocupa_ci=7 if (ocupa>=52 & ocupa<=55) & emp_ci==1
replace ocupa_ci=8 if (ocupa==83) & emp_ci==1
replace ocupa_ci=9 if (ocupa==99) & emp_ci==1
*/

* Modificacion MGD 07/07/2014: correccion de la clasificacion de actividades segun el manual.
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=29) & emp_ci==1
replace ocupa_ci=2 if (ocupa>=9 & ocupa<=19) & emp_ci==1
replace ocupa_ci=3 if (ocupa>=31 & ocupa<=39) & emp_ci==1
replace ocupa_ci=4 if ((ocupa>=41 & ocupa<=49) | ocupa==95) & emp_ci==1
replace ocupa_ci=5 if ((ocupa>=51 & ocupa<=53) | ocupa==59 | ocupa==96) & emp_ci==1
replace ocupa_ci=6 if ((ocupa>=61 & ocupa<=69) | ocupa==91) & emp_ci==1
replace ocupa_ci=7 if ((ocupa>=71 & ocupa<=79) | (ocupa>=81 & ocupa<=89) | (ocupa>=92 & ocupa<=94) | ocupa==97) & emp_ci==1
replace ocupa_ci=8 if (ocupa==54) & emp_ci==1
replace ocupa_ci=9 if (ocupa==98 | ocupa==99) & emp_ci==1


******************************
*	rama_ci
******************************

tostring scian1, replace
gen ramat=real(substr(scian1,1,3))
gen rama_ci=1 if ramat>=111 & ramat<=115
replace rama_ci=2 if ramat>=211 & ramat<=213
replace rama_ci=3 if ramat>=311 & ramat<=339
replace rama_ci=4 if ramat>=221 & ramat<=222
replace rama_ci=5 if ramat>=236 & ramat<=238
replace rama_ci=6 if ramat>=400 & ramat<=469
replace rama_ci=7 if ramat>=481 & ramat<=493
replace rama_ci=9 if ramat>=511 & ramat<=932
replace rama_ci=8 if ramat>=520 & ramat<=530

/*Note: Actividad económica a la que se dedica la
empresa de acuerdo al Sistema de clasificación Industrial de América
del Norte. México, 2008 */


*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************


****************
***ylmpri_ci ***
****************
egen ylmpri_ci=rsum(ing_trab1 ing_negp1), missing



*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=.


*****************
*** ylnmpri_ci***
*****************

gen ylnmpri_ci=.


*****************************************************************
*Identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

***************
***ylmsec_ci***
***************
egen ylmsec_ci=rsum(ing_trab2 ing_negp2), missing


******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing

*************
***ylnm_ci***
*************

gen ylnm_ci=.

*************
*ylmotros_ci*
*************

gen ylmotros_ci= .

*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(ing_rent ing_tran otros), missing //CONEVAL no incluye otros

*************
***ynlnm_ci***
*************
*No se incluye el alquiler estimado
egen ynlnm = rsum(pago_esp reg_esp), missing

gen ynlnm_ci= ynlnm/nmiembros_ch


*****************
***remesas_ci***
*****************

gen remesas_ci=remesas


************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************


bys idh_ch: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.

*************
*** ylm_ch***
*************

bys idh_ch: egen ylm_ch=sum(ylm_ci) if miembros_ci==1


**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.


****************
*** ylmnr_ch ***
****************

bys idh_ch: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

bys idh_ch: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

**********************************
*** remesas_ch & remesasnm_ch ***
**********************************

bys idh_ch: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing


***************
*** ynlm_ch ***
***************

bys idh_ch: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

****************
*** ynlnm_ch ***
****************

bys idh_ch: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing

*******************
*** autocons_ci ***
*******************

gen autocons_ci= .


*******************
*** autocons_ch ***
*******************

bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing

*******************
*** rentaimp_ch ***
*******************
*Modificacion Mayra Sáenz - Agosto 2015- Antes estaba generada como missing.
gen rentaimp_ch= .

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

********************
***Transferencias***
********************

*-Monetarias

gen trac_pri = trat_pr

gen trac_pub = trat_pu

gen dona_pub = dona_pu
gen dona_pri = dona_pr

* TOTAL (las privadas incluyen transferencias del exterior)

egen trat_pri = rsum( trac_pri  dona_pr ), missing
egen trat_pub = rsum( trac_pub  dona_pu), missing

****************
*Rentas y otros*
****************
egen rtasot = rsum(ing_rent  otros), missing
label var rtasot "Rentas y otros"


******************************
*	durades_ci
******************************
gen durades_ci=.
*NA
******************************
*	antiguedad_ci
******************************
gen antiguedad_ci=.
*NA
*******************
***tamemp_ci***
*******************
  
*México Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if tam_emp1==1 | tam_emp1==2
replace tamemp_ci = 2 if (tam_emp1>=3 & tam_emp1<=7)
replace tamemp_ci = 3 if (tam_emp1>7 & tam_emp1<12)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp_o = 1 if (tam_emp1==1 | tam_emp1==2 | tam_emp1==3)
replace tamemp_o = 2 if (tam_emp1>=4 & tam_emp1<=7)
replace tamemp_o = 3 if (tam_emp1>7 & tam_emp1<12)

label define tamemp_o 1 "[1-9]" 2 "[10-49]" 3 "[50 y mas]"
label value tamemp_o tamemp_o
label var tamemp_o "Tamaño de empresa-OECD"


*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if ((act_pnea1=="2" | act_pnea2=="2") & condocup_ci==3) 
replace categoinac_ci = 2 if  ((act_pnea1=="4" | act_pnea2=="4") & condocup_ci==3) & categoinac_ci ==.
replace categoinac_ci = 3 if  ((act_pnea1=="3" | act_pnea2=="3") & condocup_ci==3) & categoinac_ci ==.
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3) & categoinac_ci ==.
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GUA" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

* Formalidad sin restringir a PEA
g formal_1= 0 if condocup_ci>=1 & condocup_ci<=3
replace formal_1=1 if cotizando_ci==1
replace formal_1=1 if afiliado_ci1==1 & (cotizando_ci!=1 | cotizando_ci!=0) & pais_c=="MEX" & anio_c>=2008


******************************************************************************
*	EDUCATION
******************************************************************************

******************************
*	aedu_ci
******************************
/*
Nivel
Valor Etiqueta
1 Preescolar
2 Primaria
3 Secundaria
4 Carrera técnica con secundaria terminada
5 Preparatoria o bachillerato
6 Carrera técnica con preparatoria terminada
7 Normal
8 Profesional
9 Maestría o doctorado
#33 grado: Grado escolar al que

Valor Etiqueta
1 Primer año
2 Segundo año
3 Tercer año
4 Cuarto año
5 Quinto año
6 Sexto año
*/


*Modificación Mayra Sáenz - Agosto 2015 - Inclusión de los cambios sugeridos por Ivan Bornacelly SCL/EDU.

destring nivel nivelaprob gradoaprob, replace

gen nivel_ed=nivelaprob
gen grado_ed= gradoaprob

gen aedu_ci=.
replace aedu_ci=0 if nivel_ed==0 |nivel_ed==1 
replace aedu_ci=grado_ed if nivel_ed==2
replace aedu_ci= grado_ed+6 if nivel_ed==3
replace aedu_ci= grado_ed+9 if nivel_ed==4
replace aedu_ci= grado_ed+12 if nivel_ed==5 | nivel_ed==6 |nivel_ed==7
replace aedu_ci= grado_ed+12 if nivel_ed==8
replace aedu_ci= grado_ed+17 if nivel_ed==9



/*
*asisten
gen aedu_ci=.
gen gradon=grado
destring nivel gradon nivelaprob gradoaprob, replace
replace aedu_ci=0 		    if nivel==1
replace aedu_ci=gradon 		if nivel==2
replace aedu_ci=gradon+6 	if nivel==3
replace aedu_ci=gradon+9 	if nivel==5 
replace aedu_ci=gradon+12 	if nivel==7
replace aedu_ci=gradon+17 	if nivel==9 
replace aedu_ci=gradon+12 	if nivel==8 
replace aedu_ci=gradon+12 	if nivel==4 |nivel==6
replace aedu_ci=aedu_ci-1 	if aedu_ci!=0

*no asisten

replace aedu_ci=0 if nivelaprob==0 | nivelaprob==1 
replace aedu_ci=gradoaprob 	if nivelaprob==2  
replace aedu_ci=gradoaprob+6 	if nivelaprob==3 
replace aedu_ci=gradoaprob+9 	if nivelaprob==4  
replace aedu_ci=gradoaprob+12 	if nivelaprob==5 
replace aedu_ci=gradoaprob+17 	if nivelaprob==8 | nivelaprob==9 
replace aedu_ci=gradoaprob+12 	if nivelaprob==7
replace aedu_ci=gradoaprob+12 	if nivelaprob==6
*/
******************************
*	eduno_ci
******************************
gen byte eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label var eduno_ci "Personas sin educacion"

******************************
*	edupi_ci
******************************
gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label var edupi_ci "Personas que no han completado Primaria"

******************************
*	edupc_ci
******************************
gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label var edupc_ci "Primaria Completa"

******************************
*	edusi_ci
******************************
gen byte edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edusi_ci=. if aedu_ci==.
label var edusi_ci "Secundaria Incompleta"

******************************
*	edusc_ci
******************************
gen byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label var edusc_ci "Secundaria Completa"

******************************
*	edus1i_ci
******************************
gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"

******************************
*	edus1c_ci
******************************
gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label var edus1c_ci "1er ciclo de Educacion Secundaria Completo"
******************************
*	edus2i_ci
******************************
gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
label var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"
******************************
*	edus2c_ci
******************************
gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media

******************************
*	eduui_ci
******************************
gen byte eduui_ci=(aedu_ci>12 & aedu_ci<17) 
replace eduui_ci=. if aedu_ci==.
label var eduui_ci "Universitaria o Terciaria Incompleta"

******************************
*	eduuc_ci
******************************
gen byte eduuc_ci=(aedu_ci>=17)
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "Universitaria o Terciaria Completa"

******************************
*	edupre_ci
******************************
gen edupre_ci=(nivel==1 | nivelaprob==1) 
replace edupre_ci=. if aedu_ci==.
label var edupre_ci "Educacion preescolar"
******************************
*	asispre_ci
******************************
*Variable agregada por Iván Bornacelly - 01/23/2017
	g asispre_ci=.
	replace asispre_ci=1 if asis_esc=="1" & nivel==1 & edad>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"	
******************************
*	eduac_ci
******************************
gen byte eduac_ci=.
label var eduac_ci "Educacion terciaria academica versus Educacion terciaria no academica"
*no se distingue entre superior universitario y no universitario (terciario)
******************************
*	asiste_ci
******************************
gen asiste_ci=(asis_esc=="1")
replace asiste_ci=. if aedu_ci==. | asis_esc !="1" & asis_esc !="2"
label var asiste_ci "Personas que actualmente asisten a la escuela"

******************************
*	pqnoasis_ci_ci
******************************
gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"
*NA

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

******************************
*	repite_ci
******************************
gen repite_ci=.
*NA

******************************
*	repiteult_ci
******************************
gen repiteult_ci=.
*NA

******************************
*	edupub_ci
******************************
gen edupub_ci=.
replace edupub_ci=1 if tipoesc=="1" 
replace edupub_ci=0 if tipoesc=="2" | tipoesc=="3"
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"


***************
***tecnica_ci**
***************
gen tecnica_ci=(nivel==6 | nivelaprob==6)
label var tecnica_ci "=1 formacion terciaria tecnica"	


******************************************************************************
*	INFRAESTRUCTURE VARIABLES 
******************************************************************************

******************************
*	aguared_ch
******************************
destring disp_agua , replace
gen aguared_ch=.
replace aguared_ch=1 if disp_agua ==1 | disp_agua ==2
replace aguared_ch=0 if disp_agua >=3 & disp_agua <=6
label var aguared_ch "Acceso a una fuente de agua por red"

******************************
*	aguadist_ch
******************************
gen aguadist_ch=.
replace aguadist_ch= 1 if disp_agua ==1
replace aguadist_ch= 2 if disp_agua ==2
replace aguadist_ch= 3 if disp_agua ==4 | disp_agua ==6
label var aguadist_ch "Ubicacion de la principal fuente de agua"

******************************
*	aguamala_ch
******************************
gen aguamala_ch=.
*NA
******************************
*	aguamide_ch
******************************
gen aguamide_ch=.
*NA
******************************
*	luz_ch
******************************
gen luz_ch=(disp_elect=="1")

******************************
*	luzmide_ch
******************************

gen luzmide_ch=(medidor_luz=="1")
*Se inserta en el 2010.


******************************
*	combust_ch
******************************
destring combustible, replace force
gen combust_ch=.
replace combust_ch=1 if combustible==3 | combustible==4 | combustible==5
replace combust_ch=0 if combustible==1 | combustible ==2 | combustible==6
label var combust_ch "Principal combustible usado es gas o electric"

******************************
*	bano_ch
******************************
destring excusado, replace 
gen bano_ch=.
replace bano_ch=1 if excusado==1
replace bano_ch=0 if excusado==2
label var bano_ch "Hogar con algun sc higienico (inodoro o letrina)"

******************************
*	banoex_ch
******************************
destring  uso_com, replace
gen banoex_ch=.
replace banoex_ch=0 if uso_compar==1
replace banoex_ch=1 if uso_compar==2
label var banoex_ch "Sc hig con uso exclusivo del hogar"

******************************
*	des1_ch
******************************
destring drenaje, replace
gen des1_ch=.
replace des1_ch=0 if drenaje ==5
replace des1_ch=1 if drenaje ==1 | drenaje ==2
replace des1_ch=2 if drenaje ==3
replace des1_ch=3 if drenaje ==4

******************************
*	des2_ch
******************************

gen des2_ch=. 
replace des2_ch=0 if des1_ch==0
replace des2_ch=1 if (des1_ch==1 | des1_ch==2)
replace des2_ch=2 if des1_ch==3 

******************************
*	piso_ch
******************************

replace mat_pisos="." if mat_pisos=="&"
destring mat_pisos, replace
gen piso_ch=.
replace piso_ch=0 if mat_piso==1
replace piso_ch=1 if mat_piso>=2 & mat_piso<=3

******************************
*	pared_ch
******************************
destring mat_pared, replace
gen pared_ch=.
replace pared_ch=0 if mat_pared ==1 | mat_pared ==2 | mat_pared ==4 | mat_pared ==5
replace pared_ch=1 if mat_pared ==3 | mat_pared >=6 & mat_pared <=8
label var pared_ch "Material Pared"

/*
1 Material de desecho.
2 Lamina de cartón.
3 Lamina metálica o de asbesto.
4 Carrizo bambú o palma.
5 Embarro o Bajareque.
6 Madera.
7 Adobe.
8 Tabique, ladrillo, block, piedra o concreto.
*/

******************************
*	techo_ch
******************************
destring mat_techos, replace
gen techo_ch=.
replace techo_ch=0 if mat_techos==1 | mat_techos==2 | mat_techos==6
replace techo_ch=1 if mat_techos==3 | mat_techos==4 | mat_techos==5 | (mat_techos>6 & mat_techos<=10)

/*
Material de desecho......................................
01
Lámina de cartón............................................
02
Lámina metálica ............................................
03
Lámina de asbesto.........................................
04
Lámina de fibrocemento ondulada (techo fijo)..
05
Palma o paja..................................................
06
Madera o tejamanil........................................
07
Terrado con viguería .....................................
08
Teja................................................................
09
Losa de concreto o viguetas con bovedilla....
10

1 Material de desecho.
2 Lamina de cartón.
3 Lamina metálica.
4 Lamina de asbesto.
5 Palma o paja.
6 Madera o tejamanil.
7 Terrado con viguería.
8 Teja.
9 Losa de concreto o viguetas con bovedilla.
*/

******************************
*	resid_ch
******************************

destring eli_ba, replace
gen resid_ch=.
replace resid_ch=0 if eli_ba==1 | eli_ba==2 | eli_ba==3
replace resid_ch=1 if eli_ba==4 | eli_ba==5
replace resid_ch=2 if eli_ba==6 | eli_ba==7
replace resid_ch=3 if eli_ba==8

/*
1 la recoge un camion o carrito de la basura
2 la tiran en el basurero publico
3 la tira en el contenedor o deposito
4 la queman?
5 la entierran?
6 la tiran en un terreno baldio o calle?
7 la tiran en la barraca o grieta?
8 la tiran al rio, lago o mar?
*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (disp_agua  >=1 &  disp_agua <=3) 
replace aguamejorada_ch = 0 if (disp_agua  >=4 &  disp_agua <=6)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (excusado   ==1  & (drenaje >=1 &  drenaje<=2) & uso_compar == 2)
replace banomejorado_ch = 0 if (excusado   ==1  & (drenaje >=1 &  drenaje<=2) & uso_compar == 1) | excusado   ==2 | (excusado  ==1  & (drenaje >=3 &  drenaje<=5))


******************************
*	dorm_ch
******************************

gen dorm_ch=cuart_dorm
label var dorm_ch "#Habitaciones exclusivamente para dormir" 

******************************
*	cuartos_ch
******************************
gen cuartos_ch=num_cuarto 
label var cuartos_ch "#Habitaciones en el hogar"
notes: cuartos_ch esta indicando cuartos, contando cocina pero no bano 

******************************
*	cocina_ch
******************************
destring cocina, replace
gen cocina_ch=.
replace cocina_ch=1 if cocina==1
replace cocina_ch=0 if cocina==2
label var cocina_ch "Si existe un cuarto separado y exclusivo para cocinar"

******************************
*	telef_ch
******************************
gen telef_ch=(telefono=="1")
label var telef_ch "Hogar con sc telefonico fijo"

******************************
*	refrig_ch
******************************
destring num_refri, replace
gen refrig_ch= .
replace refrig_ch= 0 if num_refri ==0
replace refrig_ch= 1 if num_refri>=1

******************************
*	freez_ch
******************************
gen freez_ch=.
*NA

******************************
*	auto_ch
******************************
destring num_auto num_van num_pickup, replace 
gen auto_ch=.
replace auto_ch = 0 if  num_auto==0 & num_van==0 & num_pickup==0
replace auto_ch = 1 if num_auto>=1 | num_van>=1 | num_pickup>=1
label var auto_ch "El hogar posee automovil prticular"

******************************
*	compu_ch
******************************

* Modificaciones Marcela Rubio Septiembre 2014
/*
gen compu_ch=.
replace compu_ch= 0 if num_compu==0
replace compu_ch= 0 if num_compu>=1
*/

gen compu_ch = (num_compu>0)

******************************
*	internet_ch
******************************
gen internet_ch=(conex_inte=="1")
******************************

*	cel_ch
******************************
gen cel_ch=(celular=="1") 

******************************
*	vivi1_ch
******************************
gen vivi1_ch=.
replace vivi1_ch=1 if tipo_viv =="1"
replace vivi1_ch=2 if tipo_viv =="2"
replace vivi1_ch=3 if tipo_viv >="3"
label var vivi1_ch "Tipo vivienda"
label define vivi1_ch 1"Casa" 2"Dpto" 3"Otr"

/*
01 Casa independiente
02 Departamento en edificio
03 Vivienda en vecindad
04 Vivienda en cuarto de azotea
05 Local no construido para habitación
-1 No especificado
*/

******************************
*	vivi2_ch
******************************
gen vivi2_ch=(tipo_viv =="2")

******************************
*	viviprop_ch
******************************
destring tenencia, replace
gen viviprop_ch=.
replace viviprop_ch=0 if tenencia==1
replace viviprop_ch=1 if tenencia==4   
replace viviprop_ch=2 if tenencia==3
replace viviprop_ch=3 if tenencia==2 | tenencia==5 | tenencia==6
label var viviprop_ch "Propiedad de la vivienda" 

/*
1 es rentada?
2 es prestada?
3 es propia pero la están pagando?
4 es propia?
5 esta intestada o en litigio?
6 Otra situación.
*/

******************************
*	vivitit_ch
******************************
destring escrituras, replace 
gen vivitit_ch=.
replace vivitit_ch=1 if escrituras==1 | escrituras==2
replace vivitit_ch=0 if escrituras==3
label var vivitit_ch "El hogar posee un titulo de propiedad"

******************************
*	vivialq_ch
******************************
gen vivialq_ch= renta
label var vivialq_ch "Alquiler mensual"
*Renta = Monto de la renta mensual de la vivienda

******************************
*	vivialqimp_ch
******************************
gen vivialqimp_ch=estim_pago
replace vivialqimp=0 if estim_pago<0
label var vivialqimp_ch "Alquiler mensual imputado"

*********
*raza_ci*
*********
/*gen raza_ci=.
replace raza_ci=1 if(hablaind=="1")
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"*/
destring parentesco, replace

gen raza_idioma_ci=.
replace raza_idioma_ci=1 if(hablaind=="1")
bys idh_ch: gen aux=raza_idioma_ci if parentesco==101
bys idh_ch: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (parentesco==301 | parentesco==301 | parentesco==609 | parentesco==610 | parentesco==611))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
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


*******************
***  seguro_ci  ***
*******************

g benefdes_ci=0 if desemp_ci==1
*replace benefdes_ci=1 if ing_1P036!=. & desemp_ci==1
*Modificado Mayra Sáenz - Agosto 2015
replace benefdes_ci=1 if P036!=. & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** yseguro_ci  ***
*******************
*g ybenefdes_ci=ing_1P036 if benefdes_ci==1
*Modificado Mayra Sáenz - Agosto 2015
g ybenefdes_ci=P036 if benefdes_ci==1
label var ybenefdes_ci "Monto de seguro de desempleo"


ren industria industria_orig
ren comercio comercio_orig
ren servicios servicios_orig




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

rename sinco1  codocupa
rename scian1 codindustria
destring codocupa codindustria, replace
compress

saveold "`base_out'", replace


log close



























