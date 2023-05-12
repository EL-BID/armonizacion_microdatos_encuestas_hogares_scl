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

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2010"
local ronda m8_m11

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

                    
capture log close
log using "`log_file'", replace 


/****************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH (tradicional)
Round: Agosto-Noviembre
Autores: Yessenia Loayza
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 19 de Agosto de 2013

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
*Y.L. Nota: generada solo para el 2010
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
bysort idh_ch:gen idp_ci=_n 
label var idp_ci "ID de la persona en el hogar"

******************************
*	zona_c
******************************

/*gen zona_c=(tam_loc==1 | tam_loc==2)
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

gen zona_c= 1      if tam_loc<=2
replace zona_c = 0 if (tam_loc>2 & tam_loc!=.)

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_ci

******************************
*	pais_c
******************************
gen str3 pais_c="MEX"
******************************
*	anio_c
******************************
gen anio_c=2010
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
gen mes_c=. 
label variable mes_c "Mes de la encuesta"

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

******************************************************************************
*	DEMOGRAPHIC VARIABLES
******************************************************************************

******************************
*	factor_ci
******************************
gen factor_ci=factor
label var factor_ci "Individual Expansion Factor"

	***************
	***upm_ci***
	***************
gen upm_ci=upm
	***************
	***estrato_ci***
	***************
gen estrato_ci=est_dis
						
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
gen civil_ci=.
replace civil_ci=1 if edocony=="6"
replace civil_ci=2 if edocony=="1"|edocony=="2"
replace civil_ci=3 if edocony=="3"|edocony=="4"
replace civil_ci=4 if edocony=="5"
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci
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
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 

	***************
	***afroind_ci***
	***************
gen afroind_ci=. 
replace afroind_ci=1 if etnia=="1" 
replace afroind_ci=2 if etnia=="0"
replace afroind_ci=3 if etnia=="2"
replace afroind_ci=. if etnia==" "
replace afroind_ci=9 if etnia==" " & edad_ci<3

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2010

	*************
	***dis_ci***
	**************
gen dis_ci= disc1
replace dis_ci="." if inlist(disc1,"&","7")
destring dis_ci, replace			
recode  dis_ci (8 = 0) (1/6=1)

	*************
	***dis_ch***
	**************
egen dis_ch = sum(dis_ci), by(idh_ch) 
replace dis_ch=1 if dis_ch>=1 & dis_ch!=. 

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
gen trabajon=real(trabajo)
gen verificn=real(verifica)
gen mot_ausen=real(motivo)
generat condocup_ci=.
replace condocup_ci=1 if (trabajon==1) | (verificn>=1 & verificn<=4) | (verificn==5 & mot_ausen <=6)
replace condocup_ci=2 if bustrab_1=="1" 
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
replace afiliado_ci=1 if (pres_81==8) | (inscr_1 == 1  & (servmed_3==3 | servmed_5==5 | servmed_6==6 | servmed_7==7))  /* inscripto en prestaciones de salud por trabajo*/
*replace afiliado_ci=1 if (inst_1==1 | inst_2==2 | inst_3==3 | inst_4==4) & atemed==1 & afiliado_ci==0 
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.
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

*Modificación Mayra Sáenz - Agosto 2015 - Se modificó la base de datos original, por lo que se cambian los nombres de las variables.
gen yp70mas=P044
gen yotroam=P045
gen yoportuni70=P042 if edad_ci>=70  /* solo se los dan a los que no entraron por SEDESOL*/

* Oportunidades : Special cash transfers for every adult 70 years or older who is a member of a beneficiary family meanwhile its incorporated to the SEDESOL´s Program 70 and more.

egen ypensub_ci=rsum(yp70mas yotroam yoportuni70) 
replace ypensub_ci=. if yp70mas==. & yotroam==. & yoportuni70==.

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
*lpe_ci***
*********
generat lpe_ci =1047.94 if zona_c==1
replace lpe_ci = 779.21 if zona_c==0
label var lpe_ci"linea de pobreza alimentaria oficial del pais"

*********
*lp2_ci***
*********
generat lp2_ci =1285.30 if zona_c==1
replace lp2_ci =921.26  if zona_c==0
label var lp2_ci "linea de pobreza de capacidades"

*********
*lp_ci***
*********
generat lp_ci =2102.59 if zona_c==1
replace lp_ci =1413.95 if zona_c==0
label var lp_ci "linea de pobreza de patrimonio oficial del pais"

*************
**salmm_ci***
*************

/*La encuesta fue levantada entre 21 agosto-28 noviembre 2010
*Al preguntarse por los ingresos de los seis meses anteriores 
se recolectó información correspondiente a los meses de febrero,
marzo,abril, mayo, junio y julio del 2010*/


quietly {
capture confirm variable ubica_geo
if !_rc {
         di as result "ubica_geo exists"
                    
	   
gen municipio= ubica_geo
gen zona_salmm=.	
replace zona_salmm=3	if municipio=="01001"
replace zona_salmm=3	if municipio=="01002"
replace zona_salmm=3	if municipio=="01003"
replace zona_salmm=3	if municipio=="01004"
replace zona_salmm=3	if municipio=="01005"
replace zona_salmm=3	if municipio=="01006"
replace zona_salmm=3	if municipio=="01007"
replace zona_salmm=3	if municipio=="01008"
replace zona_salmm=3	if municipio=="01009"
replace zona_salmm=3	if municipio=="01010"
replace zona_salmm=3	if municipio=="01011"
replace zona_salmm=1	if municipio=="02001"
replace zona_salmm=1	if municipio=="02002"
replace zona_salmm=1	if municipio=="02003"
replace zona_salmm=1	if municipio=="02004"
replace zona_salmm=1	if municipio=="02005"
replace zona_salmm=1	if municipio=="03001"
replace zona_salmm=1	if municipio=="03002"
replace zona_salmm=1	if municipio=="03003"
replace zona_salmm=1	if municipio=="03008"
replace zona_salmm=1	if municipio=="03009"
replace zona_salmm=3	if municipio=="04001"
replace zona_salmm=3	if municipio=="04002"
replace zona_salmm=3	if municipio=="04003"
replace zona_salmm=3	if municipio=="04004"
replace zona_salmm=3	if municipio=="04005"
replace zona_salmm=3	if municipio=="04006"
replace zona_salmm=3	if municipio=="04007"
replace zona_salmm=3	if municipio=="04008"
replace zona_salmm=3	if municipio=="04009"
replace zona_salmm=3	if municipio=="04010"
replace zona_salmm=3	if municipio=="04011"
replace zona_salmm=3	if municipio=="05001"
replace zona_salmm=3	if municipio=="05002"
replace zona_salmm=3	if municipio=="05003"
replace zona_salmm=3	if municipio=="05004"
replace zona_salmm=3	if municipio=="05005"
replace zona_salmm=3	if municipio=="05006"
replace zona_salmm=3	if municipio=="05007"
replace zona_salmm=3	if municipio=="05008"
replace zona_salmm=3	if municipio=="05009"
replace zona_salmm=3	if municipio=="05010"
replace zona_salmm=3	if municipio=="05011"
replace zona_salmm=3	if municipio=="05012"
replace zona_salmm=3	if municipio=="05013"
replace zona_salmm=3	if municipio=="05014"
replace zona_salmm=3	if municipio=="05015"
replace zona_salmm=3	if municipio=="05016"
replace zona_salmm=3	if municipio=="05017"
replace zona_salmm=3	if municipio=="05018"
replace zona_salmm=3	if municipio=="05019"
replace zona_salmm=3	if municipio=="05020"
replace zona_salmm=3	if municipio=="05021"
replace zona_salmm=3	if municipio=="05022"
replace zona_salmm=3	if municipio=="05023"
replace zona_salmm=3	if municipio=="05024"
replace zona_salmm=3	if municipio=="05025"
replace zona_salmm=3	if municipio=="05026"
replace zona_salmm=3	if municipio=="05027"
replace zona_salmm=3	if municipio=="05028"
replace zona_salmm=3	if municipio=="05029"
replace zona_salmm=3	if municipio=="05030"
replace zona_salmm=3	if municipio=="05031"
replace zona_salmm=3	if municipio=="05032"
replace zona_salmm=3	if municipio=="05033"
replace zona_salmm=3	if municipio=="05034"
replace zona_salmm=3	if municipio=="05035"
replace zona_salmm=3	if municipio=="05036"
replace zona_salmm=3	if municipio=="05037"
replace zona_salmm=3	if municipio=="05038"
replace zona_salmm=3	if municipio=="06001"
replace zona_salmm=3	if municipio=="06002"
replace zona_salmm=3	if municipio=="06003"
replace zona_salmm=3	if municipio=="06004"
replace zona_salmm=3	if municipio=="06005"
replace zona_salmm=3	if municipio=="06006"
replace zona_salmm=3	if municipio=="06007"
replace zona_salmm=3	if municipio=="06008"
replace zona_salmm=3	if municipio=="06009"
replace zona_salmm=3	if municipio=="06010"
replace zona_salmm=3	if municipio=="07001"
replace zona_salmm=3	if municipio=="07002"
replace zona_salmm=3	if municipio=="07003"
replace zona_salmm=3	if municipio=="07004"
replace zona_salmm=3	if municipio=="07005"
replace zona_salmm=3	if municipio=="07006"
replace zona_salmm=3	if municipio=="07007"
replace zona_salmm=3	if municipio=="07008"
replace zona_salmm=3	if municipio=="07009"
replace zona_salmm=3	if municipio=="07010"
replace zona_salmm=3	if municipio=="07011"
replace zona_salmm=3	if municipio=="07012"
replace zona_salmm=3	if municipio=="07013"
replace zona_salmm=3	if municipio=="07014"
replace zona_salmm=3	if municipio=="07015"
replace zona_salmm=3	if municipio=="07016"
replace zona_salmm=3	if municipio=="07017"
replace zona_salmm=3	if municipio=="07018"
replace zona_salmm=3	if municipio=="07019"
replace zona_salmm=3	if municipio=="07020"
replace zona_salmm=3	if municipio=="07021"
replace zona_salmm=3	if municipio=="07022"
replace zona_salmm=3	if municipio=="07023"
replace zona_salmm=3	if municipio=="07024"
replace zona_salmm=3	if municipio=="07025"
replace zona_salmm=3	if municipio=="07026"
replace zona_salmm=3	if municipio=="07027"
replace zona_salmm=3	if municipio=="07028"
replace zona_salmm=3	if municipio=="07029"
replace zona_salmm=3	if municipio=="07030"
replace zona_salmm=3	if municipio=="07031"
replace zona_salmm=3	if municipio=="07032"
replace zona_salmm=3	if municipio=="07033"
replace zona_salmm=3	if municipio=="07034"
replace zona_salmm=3	if municipio=="07035"
replace zona_salmm=3	if municipio=="07036"
replace zona_salmm=3	if municipio=="07037"
replace zona_salmm=3	if municipio=="07038"
replace zona_salmm=3	if municipio=="07039"
replace zona_salmm=3	if municipio=="07040"
replace zona_salmm=3	if municipio=="07041"
replace zona_salmm=3	if municipio=="07042"
replace zona_salmm=3	if municipio=="07043"
replace zona_salmm=3	if municipio=="07044"
replace zona_salmm=3	if municipio=="07045"
replace zona_salmm=3	if municipio=="07046"
replace zona_salmm=3	if municipio=="07047"
replace zona_salmm=3	if municipio=="07048"
replace zona_salmm=3	if municipio=="07049"
replace zona_salmm=3	if municipio=="07050"
replace zona_salmm=3	if municipio=="07051"
replace zona_salmm=3	if municipio=="07052"
replace zona_salmm=3	if municipio=="07053"
replace zona_salmm=3	if municipio=="07054"
replace zona_salmm=3	if municipio=="07055"
replace zona_salmm=3	if municipio=="07056"
replace zona_salmm=3	if municipio=="07057"
replace zona_salmm=3	if municipio=="07058"
replace zona_salmm=3	if municipio=="07059"
replace zona_salmm=3	if municipio=="07060"
replace zona_salmm=3	if municipio=="07061"
replace zona_salmm=3	if municipio=="07062"
replace zona_salmm=3	if municipio=="07063"
replace zona_salmm=3	if municipio=="07064"
replace zona_salmm=3	if municipio=="07065"
replace zona_salmm=3	if municipio=="07066"
replace zona_salmm=3	if municipio=="07067"
replace zona_salmm=3	if municipio=="07068"
replace zona_salmm=3	if municipio=="07069"
replace zona_salmm=3	if municipio=="07070"
replace zona_salmm=3	if municipio=="07071"
replace zona_salmm=3	if municipio=="07072"
replace zona_salmm=3	if municipio=="07073"
replace zona_salmm=3	if municipio=="07074"
replace zona_salmm=3	if municipio=="07075"
replace zona_salmm=3	if municipio=="07076"
replace zona_salmm=3	if municipio=="07077"
replace zona_salmm=3	if municipio=="07078"
replace zona_salmm=3	if municipio=="07079"
replace zona_salmm=3	if municipio=="07080"
replace zona_salmm=3	if municipio=="07081"
replace zona_salmm=3	if municipio=="07082"
replace zona_salmm=3	if municipio=="07083"
replace zona_salmm=3	if municipio=="07084"
replace zona_salmm=3	if municipio=="07085"
replace zona_salmm=3	if municipio=="07086"
replace zona_salmm=3	if municipio=="07087"
replace zona_salmm=3	if municipio=="07088"
replace zona_salmm=3	if municipio=="07089"
replace zona_salmm=3	if municipio=="07090"
replace zona_salmm=3	if municipio=="07091"
replace zona_salmm=3	if municipio=="07092"
replace zona_salmm=3	if municipio=="07093"
replace zona_salmm=3	if municipio=="07094"
replace zona_salmm=3	if municipio=="07096"
replace zona_salmm=3	if municipio=="07097"
replace zona_salmm=3	if municipio=="07098"
replace zona_salmm=3	if municipio=="07099"
replace zona_salmm=3	if municipio=="07100"
replace zona_salmm=3	if municipio=="07101"
replace zona_salmm=3	if municipio=="07102"
replace zona_salmm=3	if municipio=="07103"
replace zona_salmm=3	if municipio=="07104"
replace zona_salmm=3	if municipio=="07105"
replace zona_salmm=3	if municipio=="07106"
replace zona_salmm=3	if municipio=="07107"
replace zona_salmm=3	if municipio=="07108"
replace zona_salmm=3	if municipio=="07109"
replace zona_salmm=3	if municipio=="07110"
replace zona_salmm=3	if municipio=="07111"
replace zona_salmm=3	if municipio=="07112"
replace zona_salmm=3	if municipio=="07113"
replace zona_salmm=3	if municipio=="07114"
replace zona_salmm=3	if municipio=="07115"
replace zona_salmm=3	if municipio=="07116"
replace zona_salmm=3	if municipio=="07117"
replace zona_salmm=3	if municipio=="07118"
replace zona_salmm=3	if municipio=="07119"
replace zona_salmm=3	if municipio=="07120"
replace zona_salmm=3	if municipio=="07121"
replace zona_salmm=3	if municipio=="07122"
replace zona_salmm=3	if municipio=="07123"
replace zona_salmm=3	if municipio=="08001"
replace zona_salmm=3	if municipio=="08002"
replace zona_salmm=3	if municipio=="08003"
replace zona_salmm=3	if municipio=="08004"
replace zona_salmm=3	if municipio=="08005"
replace zona_salmm=3	if municipio=="08006"
replace zona_salmm=3	if municipio=="08007"
replace zona_salmm=3	if municipio=="08008"
replace zona_salmm=3	if municipio=="08009"
replace zona_salmm=3	if municipio=="08010"
replace zona_salmm=3	if municipio=="08011"
replace zona_salmm=3	if municipio=="08012"
replace zona_salmm=3	if municipio=="08013"
replace zona_salmm=3	if municipio=="08014"
replace zona_salmm=3	if municipio=="08015"
replace zona_salmm=3	if municipio=="08016"
replace zona_salmm=3	if municipio=="08017"
replace zona_salmm=3	if municipio=="08018"
replace zona_salmm=3	if municipio=="08019"
replace zona_salmm=3	if municipio=="08020"
replace zona_salmm=3	if municipio=="08021"
replace zona_salmm=3	if municipio=="08022"
replace zona_salmm=3	if municipio=="08023"
replace zona_salmm=3	if municipio=="08024"
replace zona_salmm=3	if municipio=="08025"
replace zona_salmm=3	if municipio=="08026"
replace zona_salmm=3	if municipio=="08027"
replace zona_salmm=1	if municipio=="08028"
replace zona_salmm=3	if municipio=="08029"
replace zona_salmm=3	if municipio=="08030"
replace zona_salmm=3	if municipio=="08031"
replace zona_salmm=3	if municipio=="08032"
replace zona_salmm=3	if municipio=="08033"
replace zona_salmm=3	if municipio=="08034"
replace zona_salmm=3	if municipio=="08035"
replace zona_salmm=3	if municipio=="08036"
replace zona_salmm=1	if municipio=="08037"
replace zona_salmm=3	if municipio=="08038"
replace zona_salmm=3	if municipio=="08039"
replace zona_salmm=3	if municipio=="08040"
replace zona_salmm=3	if municipio=="08041"
replace zona_salmm=3	if municipio=="08042"
replace zona_salmm=3	if municipio=="08043"
replace zona_salmm=3	if municipio=="08044"
replace zona_salmm=3	if municipio=="08045"
replace zona_salmm=3	if municipio=="08046"
replace zona_salmm=3	if municipio=="08047"
replace zona_salmm=3	if municipio=="08048"
replace zona_salmm=3	if municipio=="08049"
replace zona_salmm=3	if municipio=="08050"
replace zona_salmm=3	if municipio=="08051"
replace zona_salmm=3	if municipio=="08052"
replace zona_salmm=1	if municipio=="08053"
replace zona_salmm=3	if municipio=="08054"
replace zona_salmm=3	if municipio=="08055"
replace zona_salmm=3	if municipio=="08056"
replace zona_salmm=3	if municipio=="08057"
replace zona_salmm=3	if municipio=="08058"
replace zona_salmm=3	if municipio=="08059"
replace zona_salmm=3	if municipio=="08060"
replace zona_salmm=3	if municipio=="08061"
replace zona_salmm=3	if municipio=="08062"
replace zona_salmm=3	if municipio=="08063"
replace zona_salmm=3	if municipio=="08064"
replace zona_salmm=3	if municipio=="08065"
replace zona_salmm=3	if municipio=="08066"
replace zona_salmm=3	if municipio=="08067"
replace zona_salmm=1	if municipio=="09002"
replace zona_salmm=1	if municipio=="09003"
replace zona_salmm=1	if municipio=="09004"
replace zona_salmm=1	if municipio=="09005"
replace zona_salmm=1	if municipio=="09006"
replace zona_salmm=1	if municipio=="09007"
replace zona_salmm=1	if municipio=="09008"
replace zona_salmm=1	if municipio=="09009"
replace zona_salmm=1	if municipio=="09010"
replace zona_salmm=1	if municipio=="09011"
replace zona_salmm=1	if municipio=="09012"
replace zona_salmm=1	if municipio=="09013"
replace zona_salmm=1	if municipio=="09014"
replace zona_salmm=1	if municipio=="09015"
replace zona_salmm=1	if municipio=="09016"
replace zona_salmm=1	if municipio=="09017"
replace zona_salmm=3	if municipio=="10001"
replace zona_salmm=3	if municipio=="10002"
replace zona_salmm=3	if municipio=="10003"
replace zona_salmm=3	if municipio=="10004"
replace zona_salmm=3	if municipio=="10005"
replace zona_salmm=3	if municipio=="10006"
replace zona_salmm=3	if municipio=="10007"
replace zona_salmm=3	if municipio=="10008"
replace zona_salmm=3	if municipio=="10009"
replace zona_salmm=3	if municipio=="10010"
replace zona_salmm=3	if municipio=="10011"
replace zona_salmm=3	if municipio=="10012"
replace zona_salmm=3	if municipio=="10013"
replace zona_salmm=3	if municipio=="10014"
replace zona_salmm=3	if municipio=="10015"
replace zona_salmm=3	if municipio=="10016"
replace zona_salmm=3	if municipio=="10017"
replace zona_salmm=3	if municipio=="10018"
replace zona_salmm=3	if municipio=="10019"
replace zona_salmm=3	if municipio=="10020"
replace zona_salmm=3	if municipio=="10021"
replace zona_salmm=3	if municipio=="10022"
replace zona_salmm=3	if municipio=="10023"
replace zona_salmm=3	if municipio=="10024"
replace zona_salmm=3	if municipio=="10025"
replace zona_salmm=3	if municipio=="10026"
replace zona_salmm=3	if municipio=="10027"
replace zona_salmm=3	if municipio=="10028"
replace zona_salmm=3	if municipio=="10029"
replace zona_salmm=3	if municipio=="10030"
replace zona_salmm=3	if municipio=="10031"
replace zona_salmm=3	if municipio=="10032"
replace zona_salmm=3	if municipio=="10033"
replace zona_salmm=3	if municipio=="10034"
replace zona_salmm=3	if municipio=="10035"
replace zona_salmm=3	if municipio=="10036"
replace zona_salmm=3	if municipio=="10037"
replace zona_salmm=3	if municipio=="10038"
replace zona_salmm=3	if municipio=="10039"
replace zona_salmm=3	if municipio=="11001"
replace zona_salmm=3	if municipio=="11002"
replace zona_salmm=3	if municipio=="11003"
replace zona_salmm=3	if municipio=="11004"
replace zona_salmm=3	if municipio=="11005"
replace zona_salmm=3	if municipio=="11006"
replace zona_salmm=3	if municipio=="11007"
replace zona_salmm=3	if municipio=="11008"
replace zona_salmm=3	if municipio=="11009"
replace zona_salmm=3	if municipio=="11010"
replace zona_salmm=3	if municipio=="11011"
replace zona_salmm=3	if municipio=="11012"
replace zona_salmm=3	if municipio=="11013"
replace zona_salmm=3	if municipio=="11014"
replace zona_salmm=3	if municipio=="11015"
replace zona_salmm=3	if municipio=="11016"
replace zona_salmm=3	if municipio=="11017"
replace zona_salmm=3	if municipio=="11018"
replace zona_salmm=3	if municipio=="11019"
replace zona_salmm=3	if municipio=="11020"
replace zona_salmm=3	if municipio=="11021"
replace zona_salmm=3	if municipio=="11022"
replace zona_salmm=3	if municipio=="11023"
replace zona_salmm=3	if municipio=="11024"
replace zona_salmm=3	if municipio=="11025"
replace zona_salmm=3	if municipio=="11026"
replace zona_salmm=3	if municipio=="11027"
replace zona_salmm=3	if municipio=="11028"
replace zona_salmm=3	if municipio=="11029"
replace zona_salmm=3	if municipio=="11030"
replace zona_salmm=3	if municipio=="11031"
replace zona_salmm=3	if municipio=="11032"
replace zona_salmm=3	if municipio=="11033"
replace zona_salmm=3	if municipio=="11034"
replace zona_salmm=3	if municipio=="11035"
replace zona_salmm=3	if municipio=="11036"
replace zona_salmm=3	if municipio=="11037"
replace zona_salmm=3	if municipio=="11038"
replace zona_salmm=3	if municipio=="11039"
replace zona_salmm=3	if municipio=="11040"
replace zona_salmm=3	if municipio=="11041"
replace zona_salmm=3	if municipio=="11042"
replace zona_salmm=3	if municipio=="11043"
replace zona_salmm=3	if municipio=="11044"
replace zona_salmm=3	if municipio=="11045"
replace zona_salmm=3	if municipio=="11046"
replace zona_salmm=1	if municipio=="12001"
replace zona_salmm=3	if municipio=="12002"
replace zona_salmm=3	if municipio=="12003"
replace zona_salmm=3	if municipio=="12004"
replace zona_salmm=3	if municipio=="12005"
replace zona_salmm=3	if municipio=="12006"
replace zona_salmm=3	if municipio=="12007"
replace zona_salmm=3	if municipio=="12008"
replace zona_salmm=3	if municipio=="12009"
replace zona_salmm=3	if municipio=="12010"
replace zona_salmm=3	if municipio=="12011"
replace zona_salmm=3	if municipio=="12012"
replace zona_salmm=3	if municipio=="12013"
replace zona_salmm=3	if municipio=="12014"
replace zona_salmm=3	if municipio=="12015"
replace zona_salmm=3	if municipio=="12016"
replace zona_salmm=3	if municipio=="12017"
replace zona_salmm=3	if municipio=="12018"
replace zona_salmm=3	if municipio=="12019"
replace zona_salmm=3	if municipio=="12020"
replace zona_salmm=3	if municipio=="12021"
replace zona_salmm=3	if municipio=="12022"
replace zona_salmm=3	if municipio=="12023"
replace zona_salmm=3	if municipio=="12024"
replace zona_salmm=3	if municipio=="12025"
replace zona_salmm=3	if municipio=="12026"
replace zona_salmm=3	if municipio=="12027"
replace zona_salmm=3	if municipio=="12028"
replace zona_salmm=3	if municipio=="12029"
replace zona_salmm=3	if municipio=="12030"
replace zona_salmm=3	if municipio=="12031"
replace zona_salmm=3	if municipio=="12032"
replace zona_salmm=3	if municipio=="12033"
replace zona_salmm=3	if municipio=="12034"
replace zona_salmm=3	if municipio=="12035"
replace zona_salmm=3	if municipio=="12036"
replace zona_salmm=3	if municipio=="12037"
replace zona_salmm=3	if municipio=="12038"
replace zona_salmm=3	if municipio=="12039"
replace zona_salmm=3	if municipio=="12040"
replace zona_salmm=3	if municipio=="12041"
replace zona_salmm=3	if municipio=="12042"
replace zona_salmm=3	if municipio=="12043"
replace zona_salmm=3	if municipio=="12044"
replace zona_salmm=3	if municipio=="12045"
replace zona_salmm=3	if municipio=="12046"
replace zona_salmm=3	if municipio=="12047"
replace zona_salmm=3	if municipio=="12048"
replace zona_salmm=3	if municipio=="12049"
replace zona_salmm=3	if municipio=="12050"
replace zona_salmm=3	if municipio=="12051"
replace zona_salmm=3	if municipio=="12052"
replace zona_salmm=3	if municipio=="12053"
replace zona_salmm=3	if municipio=="12054"
replace zona_salmm=3	if municipio=="12055"
replace zona_salmm=3	if municipio=="12056"
replace zona_salmm=3	if municipio=="12057"
replace zona_salmm=3	if municipio=="12058"
replace zona_salmm=3	if municipio=="12059"
replace zona_salmm=3	if municipio=="12060"
replace zona_salmm=3	if municipio=="12061"
replace zona_salmm=3	if municipio=="12062"
replace zona_salmm=3	if municipio=="12063"
replace zona_salmm=3	if municipio=="12064"
replace zona_salmm=3	if municipio=="12065"
replace zona_salmm=3	if municipio=="12066"
replace zona_salmm=3	if municipio=="12067"
replace zona_salmm=3	if municipio=="12068"
replace zona_salmm=3	if municipio=="12069"
replace zona_salmm=3	if municipio=="12070"
replace zona_salmm=3	if municipio=="12071"
replace zona_salmm=3	if municipio=="12072"
replace zona_salmm=3	if municipio=="12073"
replace zona_salmm=3	if municipio=="12074"
replace zona_salmm=3	if municipio=="12075"
replace zona_salmm=3	if municipio=="12076"
replace zona_salmm=3	if municipio=="12077"
replace zona_salmm=3	if municipio=="12078"
replace zona_salmm=3	if municipio=="12079"
replace zona_salmm=3	if municipio=="12080"
replace zona_salmm=3	if municipio=="12081"
replace zona_salmm=3	if municipio=="13001"
replace zona_salmm=3	if municipio=="13002"
replace zona_salmm=3	if municipio=="13003"
replace zona_salmm=3	if municipio=="13004"
replace zona_salmm=3	if municipio=="13005"
replace zona_salmm=3	if municipio=="13006"
replace zona_salmm=3	if municipio=="13007"
replace zona_salmm=3	if municipio=="13008"
replace zona_salmm=3	if municipio=="13009"
replace zona_salmm=3	if municipio=="13010"
replace zona_salmm=3	if municipio=="13011"
replace zona_salmm=3	if municipio=="13012"
replace zona_salmm=3	if municipio=="13013"
replace zona_salmm=3	if municipio=="13014"
replace zona_salmm=3	if municipio=="13015"
replace zona_salmm=3	if municipio=="13016"
replace zona_salmm=3	if municipio=="13017"
replace zona_salmm=3	if municipio=="13018"
replace zona_salmm=3	if municipio=="13019"
replace zona_salmm=3	if municipio=="13020"
replace zona_salmm=3	if municipio=="13021"
replace zona_salmm=3	if municipio=="13022"
replace zona_salmm=3	if municipio=="13023"
replace zona_salmm=3	if municipio=="13024"
replace zona_salmm=3	if municipio=="13025"
replace zona_salmm=3	if municipio=="13026"
replace zona_salmm=3	if municipio=="13027"
replace zona_salmm=3	if municipio=="13028"
replace zona_salmm=3	if municipio=="13029"
replace zona_salmm=3	if municipio=="13030"
replace zona_salmm=3	if municipio=="13031"
replace zona_salmm=3	if municipio=="13032"
replace zona_salmm=3	if municipio=="13033"
replace zona_salmm=3	if municipio=="13034"
replace zona_salmm=3	if municipio=="13035"
replace zona_salmm=3	if municipio=="13036"
replace zona_salmm=3	if municipio=="13037"
replace zona_salmm=3	if municipio=="13038"
replace zona_salmm=3	if municipio=="13039"
replace zona_salmm=3	if municipio=="13040"
replace zona_salmm=3	if municipio=="13041"
replace zona_salmm=3	if municipio=="13042"
replace zona_salmm=3	if municipio=="13043"
replace zona_salmm=3	if municipio=="13044"
replace zona_salmm=3	if municipio=="13045"
replace zona_salmm=3	if municipio=="13046"
replace zona_salmm=3	if municipio=="13047"
replace zona_salmm=3	if municipio=="13048"
replace zona_salmm=3	if municipio=="13049"
replace zona_salmm=3	if municipio=="13050"
replace zona_salmm=3	if municipio=="13051"
replace zona_salmm=3	if municipio=="13052"
replace zona_salmm=3	if municipio=="13053"
replace zona_salmm=3	if municipio=="13054"
replace zona_salmm=3	if municipio=="13055"
replace zona_salmm=3	if municipio=="13056"
replace zona_salmm=3	if municipio=="13057"
replace zona_salmm=3	if municipio=="13058"
replace zona_salmm=3	if municipio=="13059"
replace zona_salmm=3	if municipio=="13060"
replace zona_salmm=3	if municipio=="13061"
replace zona_salmm=3	if municipio=="13062"
replace zona_salmm=3	if municipio=="13063"
replace zona_salmm=3	if municipio=="13064"
replace zona_salmm=3	if municipio=="13065"
replace zona_salmm=3	if municipio=="13066"
replace zona_salmm=3	if municipio=="13067"
replace zona_salmm=3	if municipio=="13068"
replace zona_salmm=3	if municipio=="13069"
replace zona_salmm=3	if municipio=="13070"
replace zona_salmm=3	if municipio=="13071"
replace zona_salmm=3	if municipio=="13072"
replace zona_salmm=3	if municipio=="13073"
replace zona_salmm=3	if municipio=="13074"
replace zona_salmm=3	if municipio=="13075"
replace zona_salmm=3	if municipio=="13076"
replace zona_salmm=3	if municipio=="13077"
replace zona_salmm=3	if municipio=="13078"
replace zona_salmm=3	if municipio=="13079"
replace zona_salmm=3	if municipio=="13080"
replace zona_salmm=3	if municipio=="13081"
replace zona_salmm=3	if municipio=="13082"
replace zona_salmm=3	if municipio=="13083"
replace zona_salmm=3	if municipio=="13084"
replace zona_salmm=3	if municipio=="14001"
replace zona_salmm=3	if municipio=="14002"
replace zona_salmm=3	if municipio=="14003"
replace zona_salmm=3	if municipio=="14004"
replace zona_salmm=3	if municipio=="14005"
replace zona_salmm=3	if municipio=="14006"
replace zona_salmm=3	if municipio=="14007"
replace zona_salmm=3	if municipio=="14008"
replace zona_salmm=3	if municipio=="14009"
replace zona_salmm=3	if municipio=="14010"
replace zona_salmm=3	if municipio=="14011"
replace zona_salmm=3	if municipio=="14012"
replace zona_salmm=3	if municipio=="14013"
replace zona_salmm=3	if municipio=="14014"
replace zona_salmm=3	if municipio=="14015"
replace zona_salmm=3	if municipio=="14016"
replace zona_salmm=3	if municipio=="14017"
replace zona_salmm=3	if municipio=="14018"
replace zona_salmm=3	if municipio=="14019"
replace zona_salmm=3	if municipio=="14020"
replace zona_salmm=3	if municipio=="14021"
replace zona_salmm=3	if municipio=="14022"
replace zona_salmm=3	if municipio=="14023"
replace zona_salmm=3	if municipio=="14024"
replace zona_salmm=3	if municipio=="14025"
replace zona_salmm=3	if municipio=="14026"
replace zona_salmm=3	if municipio=="14027"
replace zona_salmm=3	if municipio=="14028"
replace zona_salmm=3	if municipio=="14029"
replace zona_salmm=3	if municipio=="14030"
replace zona_salmm=3	if municipio=="14031"
replace zona_salmm=3	if municipio=="14032"
replace zona_salmm=3	if municipio=="14033"
replace zona_salmm=3	if municipio=="14034"
replace zona_salmm=3	if municipio=="14035"
replace zona_salmm=3	if municipio=="14036"
replace zona_salmm=3	if municipio=="14037"
replace zona_salmm=3	if municipio=="14038"
replace zona_salmm=2	if municipio=="14039"
replace zona_salmm=3	if municipio=="14040"
replace zona_salmm=3	if municipio=="14041"
replace zona_salmm=3	if municipio=="14042"
replace zona_salmm=3	if municipio=="14043"
replace zona_salmm=3	if municipio=="14044"
replace zona_salmm=3	if municipio=="14045"
replace zona_salmm=3	if municipio=="14046"
replace zona_salmm=3	if municipio=="14047"
replace zona_salmm=3	if municipio=="14048"
replace zona_salmm=3	if municipio=="14049"
replace zona_salmm=3	if municipio=="14050"
replace zona_salmm=3	if municipio=="14051"
replace zona_salmm=3	if municipio=="14052"
replace zona_salmm=3	if municipio=="14053"
replace zona_salmm=3	if municipio=="14054"
replace zona_salmm=3	if municipio=="14055"
replace zona_salmm=3	if municipio=="14056"
replace zona_salmm=3	if municipio=="14057"
replace zona_salmm=3	if municipio=="14058"
replace zona_salmm=3	if municipio=="14059"
replace zona_salmm=3	if municipio=="14060"
replace zona_salmm=3	if municipio=="14061"
replace zona_salmm=3	if municipio=="14062"
replace zona_salmm=3	if municipio=="14063"
replace zona_salmm=3	if municipio=="14064"
replace zona_salmm=3	if municipio=="14065"
replace zona_salmm=3	if municipio=="14066"
replace zona_salmm=3	if municipio=="14067"
replace zona_salmm=3	if municipio=="14068"
replace zona_salmm=3	if municipio=="14069"
replace zona_salmm=2	if municipio=="14070"
replace zona_salmm=3	if municipio=="14071"
replace zona_salmm=3	if municipio=="14072"
replace zona_salmm=3	if municipio=="14073"
replace zona_salmm=3	if municipio=="14074"
replace zona_salmm=3	if municipio=="14075"
replace zona_salmm=3	if municipio=="14076"
replace zona_salmm=3	if municipio=="14077"
replace zona_salmm=3	if municipio=="14078"
replace zona_salmm=3	if municipio=="14079"
replace zona_salmm=3	if municipio=="14080"
replace zona_salmm=3	if municipio=="14081"
replace zona_salmm=3	if municipio=="14082"
replace zona_salmm=3	if municipio=="14083"
replace zona_salmm=3	if municipio=="14084"
replace zona_salmm=3	if municipio=="14085"
replace zona_salmm=3	if municipio=="14086"
replace zona_salmm=3	if municipio=="14087"
replace zona_salmm=3	if municipio=="14088"
replace zona_salmm=3	if municipio=="14089"
replace zona_salmm=3	if municipio=="14090"
replace zona_salmm=3	if municipio=="14091"
replace zona_salmm=3	if municipio=="14092"
replace zona_salmm=3	if municipio=="14093"
replace zona_salmm=3	if municipio=="14094"
replace zona_salmm=3	if municipio=="14095"
replace zona_salmm=3	if municipio=="14096"
replace zona_salmm=2	if municipio=="14097"
replace zona_salmm=2	if municipio=="14098"
replace zona_salmm=3	if municipio=="14099"
replace zona_salmm=3	if municipio=="14100"
replace zona_salmm=2	if municipio=="14101"
replace zona_salmm=3	if municipio=="14102"
replace zona_salmm=3	if municipio=="14103"
replace zona_salmm=3	if municipio=="14104"
replace zona_salmm=3	if municipio=="14105"
replace zona_salmm=3	if municipio=="14106"
replace zona_salmm=3	if municipio=="14107"
replace zona_salmm=3	if municipio=="14108"
replace zona_salmm=3	if municipio=="14109"
replace zona_salmm=3	if municipio=="14110"
replace zona_salmm=3	if municipio=="14111"
replace zona_salmm=3	if municipio=="14112"
replace zona_salmm=3	if municipio=="14113"
replace zona_salmm=3	if municipio=="14114"
replace zona_salmm=3	if municipio=="14115"
replace zona_salmm=3	if municipio=="14116"
replace zona_salmm=3	if municipio=="14117"
replace zona_salmm=3	if municipio=="14118"
replace zona_salmm=3	if municipio=="14119"
replace zona_salmm=2	if municipio=="14120"
replace zona_salmm=3	if municipio=="14121"
replace zona_salmm=3	if municipio=="14122"
replace zona_salmm=3	if municipio=="14123"
replace zona_salmm=3	if municipio=="14124"
replace zona_salmm=3	if municipio=="14125"
replace zona_salmm=3	if municipio=="15001"
replace zona_salmm=3	if municipio=="15002"
replace zona_salmm=3	if municipio=="15003"
replace zona_salmm=3	if municipio=="15004"
replace zona_salmm=3	if municipio=="15005"
replace zona_salmm=3	if municipio=="15006"
replace zona_salmm=3	if municipio=="15007"
replace zona_salmm=3	if municipio=="15008"
replace zona_salmm=3	if municipio=="15009"
replace zona_salmm=3	if municipio=="15010"
replace zona_salmm=3	if municipio=="15011"
replace zona_salmm=3	if municipio=="15012"
replace zona_salmm=1	if municipio=="15013"
replace zona_salmm=3	if municipio=="15014"
replace zona_salmm=3	if municipio=="15015"
replace zona_salmm=3	if municipio=="15016"
replace zona_salmm=3	if municipio=="15017"
replace zona_salmm=3	if municipio=="15018"
replace zona_salmm=3	if municipio=="15019"
replace zona_salmm=1	if municipio=="15020"
replace zona_salmm=3	if municipio=="15021"
replace zona_salmm=3	if municipio=="15022"
replace zona_salmm=3	if municipio=="15023"
replace zona_salmm=1	if municipio=="15024"
replace zona_salmm=3	if municipio=="15025"
replace zona_salmm=3	if municipio=="15026"
replace zona_salmm=3	if municipio=="15027"
replace zona_salmm=3	if municipio=="15028"
replace zona_salmm=3	if municipio=="15029"
replace zona_salmm=3	if municipio=="15030"
replace zona_salmm=3	if municipio=="15031"
replace zona_salmm=3	if municipio=="15032"
replace zona_salmm=1	if municipio=="15033"
replace zona_salmm=3	if municipio=="15034"
replace zona_salmm=3	if municipio=="15035"
replace zona_salmm=3	if municipio=="15036"
replace zona_salmm=3	if municipio=="15037"
replace zona_salmm=3	if municipio=="15038"
replace zona_salmm=3	if municipio=="15039"
replace zona_salmm=3	if municipio=="15040"
replace zona_salmm=3	if municipio=="15041"
replace zona_salmm=3	if municipio=="15042"
replace zona_salmm=3	if municipio=="15043"
replace zona_salmm=3	if municipio=="15044"
replace zona_salmm=3	if municipio=="15045"
replace zona_salmm=3	if municipio=="15046"
replace zona_salmm=3	if municipio=="15047"
replace zona_salmm=3	if municipio=="15048"
replace zona_salmm=3	if municipio=="15049"
replace zona_salmm=3	if municipio=="15050"
replace zona_salmm=3	if municipio=="15051"
replace zona_salmm=3	if municipio=="15052"
replace zona_salmm=3	if municipio=="15053"
replace zona_salmm=3	if municipio=="15054"
replace zona_salmm=3	if municipio=="15055"
replace zona_salmm=3	if municipio=="15056"
replace zona_salmm=1	if municipio=="15057"
replace zona_salmm=3	if municipio=="15058"
replace zona_salmm=3	if municipio=="15059"
replace zona_salmm=3	if municipio=="15060"
replace zona_salmm=3	if municipio=="15061"
replace zona_salmm=3	if municipio=="15062"
replace zona_salmm=3	if municipio=="15063"
replace zona_salmm=3	if municipio=="15064"
replace zona_salmm=3	if municipio=="15065"
replace zona_salmm=3	if municipio=="15066"
replace zona_salmm=3	if municipio=="15067"
replace zona_salmm=3	if municipio=="15068"
replace zona_salmm=3	if municipio=="15069"
replace zona_salmm=3	if municipio=="15070"
replace zona_salmm=3	if municipio=="15071"
replace zona_salmm=3	if municipio=="15072"
replace zona_salmm=3	if municipio=="15073"
replace zona_salmm=3	if municipio=="15074"
replace zona_salmm=3	if municipio=="15075"
replace zona_salmm=3	if municipio=="15076"
replace zona_salmm=3	if municipio=="15077"
replace zona_salmm=3	if municipio=="15078"
replace zona_salmm=3	if municipio=="15079"
replace zona_salmm=3	if municipio=="15080"
replace zona_salmm=3	if municipio=="15081"
replace zona_salmm=3	if municipio=="15082"
replace zona_salmm=3	if municipio=="15083"
replace zona_salmm=3	if municipio=="15084"
replace zona_salmm=3	if municipio=="15085"
replace zona_salmm=3	if municipio=="15086"
replace zona_salmm=3	if municipio=="15087"
replace zona_salmm=3	if municipio=="15088"
replace zona_salmm=3	if municipio=="15089"
replace zona_salmm=3	if municipio=="15090"
replace zona_salmm=3	if municipio=="15091"
replace zona_salmm=3	if municipio=="15092"
replace zona_salmm=3	if municipio=="15093"
replace zona_salmm=3	if municipio=="15094"
replace zona_salmm=3	if municipio=="15095"
replace zona_salmm=3	if municipio=="15096"
replace zona_salmm=3	if municipio=="15097"
replace zona_salmm=3	if municipio=="15098"
replace zona_salmm=3	if municipio=="15099"
replace zona_salmm=3	if municipio=="15100"
replace zona_salmm=3	if municipio=="15101"
replace zona_salmm=3	if municipio=="15102"
replace zona_salmm=3	if municipio=="15103"
replace zona_salmm=1	if municipio=="15104"
replace zona_salmm=3	if municipio=="15105"
replace zona_salmm=3	if municipio=="15106"
replace zona_salmm=3	if municipio=="15107"
replace zona_salmm=3	if municipio=="15108"
replace zona_salmm=1	if municipio=="15109"
replace zona_salmm=3	if municipio=="15110"
replace zona_salmm=3	if municipio=="15111"
replace zona_salmm=3	if municipio=="15112"
replace zona_salmm=3	if municipio=="15113"
replace zona_salmm=3	if municipio=="15114"
replace zona_salmm=3	if municipio=="15115"
replace zona_salmm=3	if municipio=="15116"
replace zona_salmm=3	if municipio=="15117"
replace zona_salmm=3	if municipio=="15118"
replace zona_salmm=3	if municipio=="15119"
replace zona_salmm=3	if municipio=="15120"
replace zona_salmm=1	if municipio=="15121"
replace zona_salmm=3	if municipio=="15122"
replace zona_salmm=3	if municipio=="15123"
replace zona_salmm=3	if municipio=="15124"
replace zona_salmm=3	if municipio=="15125"
replace zona_salmm=3	if municipio=="16001"
replace zona_salmm=3	if municipio=="16002"
replace zona_salmm=3	if municipio=="16003"
replace zona_salmm=3	if municipio=="16004"
replace zona_salmm=3	if municipio=="16005"
replace zona_salmm=3	if municipio=="16006"
replace zona_salmm=3	if municipio=="16007"
replace zona_salmm=3	if municipio=="16008"
replace zona_salmm=3	if municipio=="16009"
replace zona_salmm=3	if municipio=="16010"
replace zona_salmm=3	if municipio=="16011"
replace zona_salmm=3	if municipio=="16012"
replace zona_salmm=3	if municipio=="16013"
replace zona_salmm=3	if municipio=="16014"
replace zona_salmm=3	if municipio=="16015"
replace zona_salmm=3	if municipio=="16016"
replace zona_salmm=3	if municipio=="16017"
replace zona_salmm=3	if municipio=="16018"
replace zona_salmm=3	if municipio=="16019"
replace zona_salmm=3	if municipio=="16020"
replace zona_salmm=3	if municipio=="16021"
replace zona_salmm=3	if municipio=="16022"
replace zona_salmm=3	if municipio=="16023"
replace zona_salmm=3	if municipio=="16024"
replace zona_salmm=3	if municipio=="16025"
replace zona_salmm=3	if municipio=="16026"
replace zona_salmm=3	if municipio=="16027"
replace zona_salmm=3	if municipio=="16028"
replace zona_salmm=3	if municipio=="16029"
replace zona_salmm=3	if municipio=="16030"
replace zona_salmm=3	if municipio=="16031"
replace zona_salmm=3	if municipio=="16032"
replace zona_salmm=3	if municipio=="16033"
replace zona_salmm=3	if municipio=="16034"
replace zona_salmm=3	if municipio=="16035"
replace zona_salmm=3	if municipio=="16036"
replace zona_salmm=3	if municipio=="16037"
replace zona_salmm=3	if municipio=="16038"
replace zona_salmm=3	if municipio=="16039"
replace zona_salmm=3	if municipio=="16040"
replace zona_salmm=3	if municipio=="16041"
replace zona_salmm=3	if municipio=="16042"
replace zona_salmm=3	if municipio=="16043"
replace zona_salmm=3	if municipio=="16044"
replace zona_salmm=3	if municipio=="16045"
replace zona_salmm=3	if municipio=="16046"
replace zona_salmm=3	if municipio=="16047"
replace zona_salmm=3	if municipio=="16048"
replace zona_salmm=3	if municipio=="16049"
replace zona_salmm=3	if municipio=="16050"
replace zona_salmm=3	if municipio=="16051"
replace zona_salmm=3	if municipio=="16052"
replace zona_salmm=3	if municipio=="16053"
replace zona_salmm=3	if municipio=="16054"
replace zona_salmm=3	if municipio=="16055"
replace zona_salmm=3	if municipio=="16056"
replace zona_salmm=3	if municipio=="16057"
replace zona_salmm=3	if municipio=="16058"
replace zona_salmm=3	if municipio=="16059"
replace zona_salmm=3	if municipio=="16060"
replace zona_salmm=3	if municipio=="16061"
replace zona_salmm=3	if municipio=="16062"
replace zona_salmm=3	if municipio=="16063"
replace zona_salmm=3	if municipio=="16064"
replace zona_salmm=3	if municipio=="16065"
replace zona_salmm=3	if municipio=="16066"
replace zona_salmm=3	if municipio=="16067"
replace zona_salmm=3	if municipio=="16068"
replace zona_salmm=3	if municipio=="16069"
replace zona_salmm=3	if municipio=="16070"
replace zona_salmm=3	if municipio=="16071"
replace zona_salmm=3	if municipio=="16072"
replace zona_salmm=3	if municipio=="16073"
replace zona_salmm=3	if municipio=="16074"
replace zona_salmm=3	if municipio=="16075"
replace zona_salmm=3	if municipio=="16076"
replace zona_salmm=3	if municipio=="16077"
replace zona_salmm=3	if municipio=="16078"
replace zona_salmm=3	if municipio=="16079"
replace zona_salmm=3	if municipio=="16080"
replace zona_salmm=3	if municipio=="16081"
replace zona_salmm=3	if municipio=="16082"
replace zona_salmm=3	if municipio=="16083"
replace zona_salmm=3	if municipio=="16084"
replace zona_salmm=3	if municipio=="16085"
replace zona_salmm=3	if municipio=="16086"
replace zona_salmm=3	if municipio=="16087"
replace zona_salmm=3	if municipio=="16088"
replace zona_salmm=3	if municipio=="16089"
replace zona_salmm=3	if municipio=="16090"
replace zona_salmm=3	if municipio=="16091"
replace zona_salmm=3	if municipio=="16092"
replace zona_salmm=3	if municipio=="16093"
replace zona_salmm=3	if municipio=="16094"
replace zona_salmm=3	if municipio=="16095"
replace zona_salmm=3	if municipio=="16096"
replace zona_salmm=3	if municipio=="16097"
replace zona_salmm=3	if municipio=="16098"
replace zona_salmm=3	if municipio=="16099"
replace zona_salmm=3	if municipio=="16100"
replace zona_salmm=3	if municipio=="16101"
replace zona_salmm=3	if municipio=="16102"
replace zona_salmm=3	if municipio=="16103"
replace zona_salmm=3	if municipio=="16104"
replace zona_salmm=3	if municipio=="16105"
replace zona_salmm=3	if municipio=="16106"
replace zona_salmm=3	if municipio=="16107"
replace zona_salmm=3	if municipio=="16108"
replace zona_salmm=3	if municipio=="16109"
replace zona_salmm=3	if municipio=="16110"
replace zona_salmm=3	if municipio=="16111"
replace zona_salmm=3	if municipio=="16112"
replace zona_salmm=3	if municipio=="16113"
replace zona_salmm=3	if municipio=="17001"
replace zona_salmm=3	if municipio=="17002"
replace zona_salmm=3	if municipio=="17003"
replace zona_salmm=3	if municipio=="17004"
replace zona_salmm=3	if municipio=="17005"
replace zona_salmm=3	if municipio=="17006"
replace zona_salmm=3	if municipio=="17007"
replace zona_salmm=3	if municipio=="17008"
replace zona_salmm=3	if municipio=="17009"
replace zona_salmm=3	if municipio=="17010"
replace zona_salmm=3	if municipio=="17011"
replace zona_salmm=3	if municipio=="17012"
replace zona_salmm=3	if municipio=="17013"
replace zona_salmm=3	if municipio=="17014"
replace zona_salmm=3	if municipio=="17015"
replace zona_salmm=3	if municipio=="17016"
replace zona_salmm=3	if municipio=="17017"
replace zona_salmm=3	if municipio=="17018"
replace zona_salmm=3	if municipio=="17019"
replace zona_salmm=3	if municipio=="17020"
replace zona_salmm=3	if municipio=="17021"
replace zona_salmm=3	if municipio=="17022"
replace zona_salmm=3	if municipio=="17023"
replace zona_salmm=3	if municipio=="17024"
replace zona_salmm=3	if municipio=="17025"
replace zona_salmm=3	if municipio=="17026"
replace zona_salmm=3	if municipio=="17027"
replace zona_salmm=3	if municipio=="17028"
replace zona_salmm=3	if municipio=="17029"
replace zona_salmm=3	if municipio=="17030"
replace zona_salmm=3	if municipio=="17031"
replace zona_salmm=3	if municipio=="17032"
replace zona_salmm=3	if municipio=="17033"
replace zona_salmm=3	if municipio=="18001"
replace zona_salmm=3	if municipio=="18002"
replace zona_salmm=3	if municipio=="18003"
replace zona_salmm=3	if municipio=="18004"
replace zona_salmm=3	if municipio=="18005"
replace zona_salmm=3	if municipio=="18006"
replace zona_salmm=3	if municipio=="18007"
replace zona_salmm=3	if municipio=="18008"
replace zona_salmm=3	if municipio=="18009"
replace zona_salmm=3	if municipio=="18010"
replace zona_salmm=3	if municipio=="18011"
replace zona_salmm=3	if municipio=="18012"
replace zona_salmm=3	if municipio=="18013"
replace zona_salmm=3	if municipio=="18014"
replace zona_salmm=3	if municipio=="18015"
replace zona_salmm=3	if municipio=="18016"
replace zona_salmm=3	if municipio=="18017"
replace zona_salmm=3	if municipio=="18018"
replace zona_salmm=3	if municipio=="18019"
replace zona_salmm=3	if municipio=="18020"
replace zona_salmm=3	if municipio=="19001"
replace zona_salmm=3	if municipio=="19002"
replace zona_salmm=3	if municipio=="19003"
replace zona_salmm=3	if municipio=="19004"
replace zona_salmm=3	if municipio=="19005"
replace zona_salmm=2	if municipio=="19006"
replace zona_salmm=3	if municipio=="19007"
replace zona_salmm=3	if municipio=="19008"
replace zona_salmm=3	if municipio=="19009"
replace zona_salmm=3	if municipio=="19010"
replace zona_salmm=3	if municipio=="19011"
replace zona_salmm=3	if municipio=="19012"
replace zona_salmm=3	if municipio=="19013"
replace zona_salmm=3	if municipio=="19014"
replace zona_salmm=3	if municipio=="19015"
replace zona_salmm=3	if municipio=="19016"
replace zona_salmm=3	if municipio=="19017"
replace zona_salmm=3	if municipio=="19018"
replace zona_salmm=2	if municipio=="19019"
replace zona_salmm=3	if municipio=="19020"
replace zona_salmm=2	if municipio=="19021"
replace zona_salmm=3	if municipio=="19022"
replace zona_salmm=3	if municipio=="19023"
replace zona_salmm=3	if municipio=="19024"
replace zona_salmm=3	if municipio=="19025"
replace zona_salmm=2	if municipio=="19026"
replace zona_salmm=3	if municipio=="19027"
replace zona_salmm=3	if municipio=="19028"
replace zona_salmm=3	if municipio=="19029"
replace zona_salmm=3	if municipio=="19030"
replace zona_salmm=3	if municipio=="19031"
replace zona_salmm=3	if municipio=="19032"
replace zona_salmm=3	if municipio=="19033"
replace zona_salmm=3	if municipio=="19034"
replace zona_salmm=3	if municipio=="19035"
replace zona_salmm=3	if municipio=="19036"
replace zona_salmm=3	if municipio=="19037"
replace zona_salmm=3	if municipio=="19038"
replace zona_salmm=2	if municipio=="19039"
replace zona_salmm=3	if municipio=="19040"
replace zona_salmm=3	if municipio=="19041"
replace zona_salmm=3	if municipio=="19042"
replace zona_salmm=3	if municipio=="19043"
replace zona_salmm=3	if municipio=="19044"
replace zona_salmm=3	if municipio=="19045"
replace zona_salmm=2	if municipio=="19046"
replace zona_salmm=3	if municipio=="19047"
replace zona_salmm=2	if municipio=="19048"
replace zona_salmm=3	if municipio=="19049"
replace zona_salmm=3	if municipio=="19050"
replace zona_salmm=3	if municipio=="19051"
replace zona_salmm=3	if municipio=="20001"
replace zona_salmm=3	if municipio=="20002"
replace zona_salmm=3	if municipio=="20003"
replace zona_salmm=3	if municipio=="20004"
replace zona_salmm=3	if municipio=="20005"
replace zona_salmm=3	if municipio=="20006"
replace zona_salmm=3	if municipio=="20007"
replace zona_salmm=3	if municipio=="20008"
replace zona_salmm=3	if municipio=="20009"
replace zona_salmm=3	if municipio=="20010"
replace zona_salmm=3	if municipio=="20011"
replace zona_salmm=3	if municipio=="20012"
replace zona_salmm=3	if municipio=="20013"
replace zona_salmm=3	if municipio=="20014"
replace zona_salmm=3	if municipio=="20015"
replace zona_salmm=3	if municipio=="20016"
replace zona_salmm=3	if municipio=="20017"
replace zona_salmm=3	if municipio=="20018"
replace zona_salmm=3	if municipio=="20019"
replace zona_salmm=3	if municipio=="20020"
replace zona_salmm=3	if municipio=="20021"
replace zona_salmm=3	if municipio=="20022"
replace zona_salmm=3	if municipio=="20023"
replace zona_salmm=3	if municipio=="20024"
replace zona_salmm=3	if municipio=="20025"
replace zona_salmm=3	if municipio=="20026"
replace zona_salmm=3	if municipio=="20027"
replace zona_salmm=3	if municipio=="20028"
replace zona_salmm=3	if municipio=="20029"
replace zona_salmm=3	if municipio=="20030"
replace zona_salmm=3	if municipio=="20031"
replace zona_salmm=3	if municipio=="20032"
replace zona_salmm=3	if municipio=="20033"
replace zona_salmm=3	if municipio=="20034"
replace zona_salmm=3	if municipio=="20035"
replace zona_salmm=3	if municipio=="20036"
replace zona_salmm=3	if municipio=="20037"
replace zona_salmm=3	if municipio=="20038"
replace zona_salmm=3	if municipio=="20039"
replace zona_salmm=3	if municipio=="20040"
replace zona_salmm=3	if municipio=="20041"
replace zona_salmm=3	if municipio=="20042"
replace zona_salmm=3	if municipio=="20043"
replace zona_salmm=3	if municipio=="20044"
replace zona_salmm=3	if municipio=="20045"
replace zona_salmm=3	if municipio=="20046"
replace zona_salmm=3	if municipio=="20047"
replace zona_salmm=3	if municipio=="20048"
replace zona_salmm=3	if municipio=="20049"
replace zona_salmm=3	if municipio=="20050"
replace zona_salmm=3	if municipio=="20051"
replace zona_salmm=3	if municipio=="20052"
replace zona_salmm=3	if municipio=="20053"
replace zona_salmm=3	if municipio=="20054"
replace zona_salmm=3	if municipio=="20055"
replace zona_salmm=3	if municipio=="20056"
replace zona_salmm=3	if municipio=="20057"
replace zona_salmm=3	if municipio=="20058"
replace zona_salmm=3	if municipio=="20059"
replace zona_salmm=3	if municipio=="20060"
replace zona_salmm=3	if municipio=="20061"
replace zona_salmm=3	if municipio=="20062"
replace zona_salmm=3	if municipio=="20063"
replace zona_salmm=3	if municipio=="20064"
replace zona_salmm=3	if municipio=="20065"
replace zona_salmm=3	if municipio=="20066"
replace zona_salmm=3	if municipio=="20067"
replace zona_salmm=3	if municipio=="20068"
replace zona_salmm=3	if municipio=="20069"
replace zona_salmm=3	if municipio=="20070"
replace zona_salmm=3	if municipio=="20071"
replace zona_salmm=3	if municipio=="20072"
replace zona_salmm=3	if municipio=="20073"
replace zona_salmm=3	if municipio=="20074"
replace zona_salmm=3	if municipio=="20075"
replace zona_salmm=3	if municipio=="20076"
replace zona_salmm=3	if municipio=="20077"
replace zona_salmm=3	if municipio=="20078"
replace zona_salmm=3	if municipio=="20079"
replace zona_salmm=3	if municipio=="20080"
replace zona_salmm=3	if municipio=="20081"
replace zona_salmm=3	if municipio=="20082"
replace zona_salmm=3	if municipio=="20083"
replace zona_salmm=3	if municipio=="20084"
replace zona_salmm=3	if municipio=="20085"
replace zona_salmm=3	if municipio=="20086"
replace zona_salmm=3	if municipio=="20087"
replace zona_salmm=3	if municipio=="20088"
replace zona_salmm=3	if municipio=="20089"
replace zona_salmm=3	if municipio=="20090"
replace zona_salmm=3	if municipio=="20091"
replace zona_salmm=3	if municipio=="20092"
replace zona_salmm=3	if municipio=="20093"
replace zona_salmm=3	if municipio=="20094"
replace zona_salmm=3	if municipio=="20095"
replace zona_salmm=3	if municipio=="20096"
replace zona_salmm=3	if municipio=="20097"
replace zona_salmm=3	if municipio=="20098"
replace zona_salmm=3	if municipio=="20099"
replace zona_salmm=3	if municipio=="20100"
replace zona_salmm=3	if municipio=="20101"
replace zona_salmm=3	if municipio=="20102"
replace zona_salmm=3	if municipio=="20103"
replace zona_salmm=3	if municipio=="20104"
replace zona_salmm=3	if municipio=="20105"
replace zona_salmm=3	if municipio=="20106"
replace zona_salmm=3	if municipio=="20107"
replace zona_salmm=3	if municipio=="20108"
replace zona_salmm=3	if municipio=="20109"
replace zona_salmm=3	if municipio=="20110"
replace zona_salmm=3	if municipio=="20111"
replace zona_salmm=3	if municipio=="20112"
replace zona_salmm=3	if municipio=="20113"
replace zona_salmm=3	if municipio=="20114"
replace zona_salmm=3	if municipio=="20115"
replace zona_salmm=3	if municipio=="20116"
replace zona_salmm=3	if municipio=="20117"
replace zona_salmm=3	if municipio=="20118"
replace zona_salmm=3	if municipio=="20119"
replace zona_salmm=3	if municipio=="20120"
replace zona_salmm=3	if municipio=="20121"
replace zona_salmm=3	if municipio=="20122"
replace zona_salmm=3	if municipio=="20123"
replace zona_salmm=3	if municipio=="20124"
replace zona_salmm=3	if municipio=="20125"
replace zona_salmm=3	if municipio=="20126"
replace zona_salmm=3	if municipio=="20127"
replace zona_salmm=3	if municipio=="20128"
replace zona_salmm=3	if municipio=="20129"
replace zona_salmm=3	if municipio=="20130"
replace zona_salmm=3	if municipio=="20131"
replace zona_salmm=3	if municipio=="20132"
replace zona_salmm=3	if municipio=="20133"
replace zona_salmm=3	if municipio=="20134"
replace zona_salmm=3	if municipio=="20135"
replace zona_salmm=3	if municipio=="20136"
replace zona_salmm=3	if municipio=="20137"
replace zona_salmm=3	if municipio=="20138"
replace zona_salmm=3	if municipio=="20139"
replace zona_salmm=3	if municipio=="20140"
replace zona_salmm=3	if municipio=="20141"
replace zona_salmm=3	if municipio=="20142"
replace zona_salmm=3	if municipio=="20143"
replace zona_salmm=3	if municipio=="20144"
replace zona_salmm=3	if municipio=="20145"
replace zona_salmm=3	if municipio=="20146"
replace zona_salmm=3	if municipio=="20147"
replace zona_salmm=3	if municipio=="20148"
replace zona_salmm=3	if municipio=="20149"
replace zona_salmm=3	if municipio=="20150"
replace zona_salmm=3	if municipio=="20151"
replace zona_salmm=3	if municipio=="20152"
replace zona_salmm=3	if municipio=="20153"
replace zona_salmm=3	if municipio=="20154"
replace zona_salmm=3	if municipio=="20155"
replace zona_salmm=3	if municipio=="20156"
replace zona_salmm=3	if municipio=="20157"
replace zona_salmm=3	if municipio=="20158"
replace zona_salmm=3	if municipio=="20159"
replace zona_salmm=3	if municipio=="20160"
replace zona_salmm=3	if municipio=="20161"
replace zona_salmm=3	if municipio=="20162"
replace zona_salmm=3	if municipio=="20163"
replace zona_salmm=3	if municipio=="20164"
replace zona_salmm=3	if municipio=="20165"
replace zona_salmm=3	if municipio=="20166"
replace zona_salmm=3	if municipio=="20167"
replace zona_salmm=3	if municipio=="20168"
replace zona_salmm=3	if municipio=="20169"
replace zona_salmm=3	if municipio=="20170"
replace zona_salmm=3	if municipio=="20171"
replace zona_salmm=3	if municipio=="20172"
replace zona_salmm=3	if municipio=="20173"
replace zona_salmm=3	if municipio=="20174"
replace zona_salmm=3	if municipio=="20175"
replace zona_salmm=3	if municipio=="20176"
replace zona_salmm=3	if municipio=="20177"
replace zona_salmm=3	if municipio=="20178"
replace zona_salmm=3	if municipio=="20179"
replace zona_salmm=3	if municipio=="20180"
replace zona_salmm=3	if municipio=="20181"
replace zona_salmm=3	if municipio=="20182"
replace zona_salmm=3	if municipio=="20183"
replace zona_salmm=3	if municipio=="20184"
replace zona_salmm=3	if municipio=="20185"
replace zona_salmm=3	if municipio=="20186"
replace zona_salmm=3	if municipio=="20187"
replace zona_salmm=3	if municipio=="20188"
replace zona_salmm=3	if municipio=="20189"
replace zona_salmm=3	if municipio=="20190"
replace zona_salmm=3	if municipio=="20191"
replace zona_salmm=3	if municipio=="20192"
replace zona_salmm=3	if municipio=="20193"
replace zona_salmm=3	if municipio=="20194"
replace zona_salmm=3	if municipio=="20195"
replace zona_salmm=3	if municipio=="20196"
replace zona_salmm=3	if municipio=="20197"
replace zona_salmm=3	if municipio=="20198"
replace zona_salmm=3	if municipio=="20199"
replace zona_salmm=3	if municipio=="20200"
replace zona_salmm=3	if municipio=="20201"
replace zona_salmm=3	if municipio=="20202"
replace zona_salmm=3	if municipio=="20203"
replace zona_salmm=3	if municipio=="20204"
replace zona_salmm=3	if municipio=="20205"
replace zona_salmm=3	if municipio=="20206"
replace zona_salmm=3	if municipio=="20207"
replace zona_salmm=3	if municipio=="20208"
replace zona_salmm=3	if municipio=="20209"
replace zona_salmm=3	if municipio=="20210"
replace zona_salmm=3	if municipio=="20211"
replace zona_salmm=3	if municipio=="20212"
replace zona_salmm=3	if municipio=="20213"
replace zona_salmm=3	if municipio=="20214"
replace zona_salmm=3	if municipio=="20215"
replace zona_salmm=3	if municipio=="20216"
replace zona_salmm=3	if municipio=="20217"
replace zona_salmm=3	if municipio=="20218"
replace zona_salmm=3	if municipio=="20219"
replace zona_salmm=3	if municipio=="20220"
replace zona_salmm=3	if municipio=="20221"
replace zona_salmm=3	if municipio=="20222"
replace zona_salmm=3	if municipio=="20223"
replace zona_salmm=3	if municipio=="20224"
replace zona_salmm=3	if municipio=="20225"
replace zona_salmm=3	if municipio=="20226"
replace zona_salmm=3	if municipio=="20227"
replace zona_salmm=3	if municipio=="20228"
replace zona_salmm=3	if municipio=="20229"
replace zona_salmm=3	if municipio=="20230"
replace zona_salmm=3	if municipio=="20231"
replace zona_salmm=3	if municipio=="20232"
replace zona_salmm=3	if municipio=="20233"
replace zona_salmm=3	if municipio=="20234"
replace zona_salmm=3	if municipio=="20235"
replace zona_salmm=3	if municipio=="20236"
replace zona_salmm=3	if municipio=="20237"
replace zona_salmm=3	if municipio=="20238"
replace zona_salmm=3	if municipio=="20239"
replace zona_salmm=3	if municipio=="20240"
replace zona_salmm=3	if municipio=="20241"
replace zona_salmm=3	if municipio=="20242"
replace zona_salmm=3	if municipio=="20243"
replace zona_salmm=3	if municipio=="20244"
replace zona_salmm=3	if municipio=="20245"
replace zona_salmm=3	if municipio=="20246"
replace zona_salmm=3	if municipio=="20247"
replace zona_salmm=3	if municipio=="20248"
replace zona_salmm=3	if municipio=="20249"
replace zona_salmm=3	if municipio=="20250"
replace zona_salmm=3	if municipio=="20251"
replace zona_salmm=3	if municipio=="20252"
replace zona_salmm=3	if municipio=="20253"
replace zona_salmm=3	if municipio=="20254"
replace zona_salmm=3	if municipio=="20255"
replace zona_salmm=3	if municipio=="20256"
replace zona_salmm=3	if municipio=="20257"
replace zona_salmm=3	if municipio=="20258"
replace zona_salmm=3	if municipio=="20259"
replace zona_salmm=3	if municipio=="20260"
replace zona_salmm=3	if municipio=="20261"
replace zona_salmm=3	if municipio=="20262"
replace zona_salmm=3	if municipio=="20263"
replace zona_salmm=3	if municipio=="20264"
replace zona_salmm=3	if municipio=="20265"
replace zona_salmm=3	if municipio=="20266"
replace zona_salmm=3	if municipio=="20267"
replace zona_salmm=3	if municipio=="20268"
replace zona_salmm=3	if municipio=="20269"
replace zona_salmm=3	if municipio=="20270"
replace zona_salmm=3	if municipio=="20271"
replace zona_salmm=3	if municipio=="20272"
replace zona_salmm=3	if municipio=="20273"
replace zona_salmm=3	if municipio=="20274"
replace zona_salmm=3	if municipio=="20275"
replace zona_salmm=3	if municipio=="20276"
replace zona_salmm=3	if municipio=="20277"
replace zona_salmm=3	if municipio=="20278"
replace zona_salmm=3	if municipio=="20279"
replace zona_salmm=3	if municipio=="20280"
replace zona_salmm=3	if municipio=="20281"
replace zona_salmm=3	if municipio=="20282"
replace zona_salmm=3	if municipio=="20283"
replace zona_salmm=3	if municipio=="20284"
replace zona_salmm=3	if municipio=="20285"
replace zona_salmm=3	if municipio=="20286"
replace zona_salmm=3	if municipio=="20287"
replace zona_salmm=3	if municipio=="20288"
replace zona_salmm=3	if municipio=="20289"
replace zona_salmm=3	if municipio=="20290"
replace zona_salmm=3	if municipio=="20291"
replace zona_salmm=3	if municipio=="20292"
replace zona_salmm=3	if municipio=="20293"
replace zona_salmm=3	if municipio=="20294"
replace zona_salmm=3	if municipio=="20295"
replace zona_salmm=3	if municipio=="20296"
replace zona_salmm=3	if municipio=="20297"
replace zona_salmm=3	if municipio=="20298"
replace zona_salmm=3	if municipio=="20299"
replace zona_salmm=3	if municipio=="20300"
replace zona_salmm=3	if municipio=="20301"
replace zona_salmm=3	if municipio=="20302"
replace zona_salmm=3	if municipio=="20303"
replace zona_salmm=3	if municipio=="20304"
replace zona_salmm=3	if municipio=="20305"
replace zona_salmm=3	if municipio=="20306"
replace zona_salmm=3	if municipio=="20307"
replace zona_salmm=3	if municipio=="20308"
replace zona_salmm=3	if municipio=="20309"
replace zona_salmm=3	if municipio=="20310"
replace zona_salmm=3	if municipio=="20311"
replace zona_salmm=3	if municipio=="20312"
replace zona_salmm=3	if municipio=="20313"
replace zona_salmm=3	if municipio=="20314"
replace zona_salmm=3	if municipio=="20315"
replace zona_salmm=3	if municipio=="20316"
replace zona_salmm=3	if municipio=="20317"
replace zona_salmm=3	if municipio=="20318"
replace zona_salmm=3	if municipio=="20319"
replace zona_salmm=3	if municipio=="20320"
replace zona_salmm=3	if municipio=="20321"
replace zona_salmm=3	if municipio=="20322"
replace zona_salmm=3	if municipio=="20323"
replace zona_salmm=3	if municipio=="20324"
replace zona_salmm=3	if municipio=="20325"
replace zona_salmm=3	if municipio=="20326"
replace zona_salmm=3	if municipio=="20327"
replace zona_salmm=3	if municipio=="20328"
replace zona_salmm=3	if municipio=="20329"
replace zona_salmm=3	if municipio=="20330"
replace zona_salmm=3	if municipio=="20331"
replace zona_salmm=3	if municipio=="20332"
replace zona_salmm=3	if municipio=="20333"
replace zona_salmm=3	if municipio=="20334"
replace zona_salmm=3	if municipio=="20335"
replace zona_salmm=3	if municipio=="20336"
replace zona_salmm=3	if municipio=="20337"
replace zona_salmm=3	if municipio=="20338"
replace zona_salmm=3	if municipio=="20339"
replace zona_salmm=3	if municipio=="20340"
replace zona_salmm=3	if municipio=="20341"
replace zona_salmm=3	if municipio=="20342"
replace zona_salmm=3	if municipio=="20343"
replace zona_salmm=3	if municipio=="20344"
replace zona_salmm=3	if municipio=="20345"
replace zona_salmm=3	if municipio=="20346"
replace zona_salmm=3	if municipio=="20347"
replace zona_salmm=3	if municipio=="20348"
replace zona_salmm=3	if municipio=="20349"
replace zona_salmm=3	if municipio=="20350"
replace zona_salmm=3	if municipio=="20351"
replace zona_salmm=3	if municipio=="20352"
replace zona_salmm=3	if municipio=="20353"
replace zona_salmm=3	if municipio=="20354"
replace zona_salmm=3	if municipio=="20355"
replace zona_salmm=3	if municipio=="20356"
replace zona_salmm=3	if municipio=="20357"
replace zona_salmm=3	if municipio=="20358"
replace zona_salmm=3	if municipio=="20359"
replace zona_salmm=3	if municipio=="20360"
replace zona_salmm=3	if municipio=="20361"
replace zona_salmm=3	if municipio=="20362"
replace zona_salmm=3	if municipio=="20363"
replace zona_salmm=3	if municipio=="20364"
replace zona_salmm=3	if municipio=="20365"
replace zona_salmm=3	if municipio=="20366"
replace zona_salmm=3	if municipio=="20367"
replace zona_salmm=3	if municipio=="20368"
replace zona_salmm=3	if municipio=="20369"
replace zona_salmm=3	if municipio=="20370"
replace zona_salmm=3	if municipio=="20371"
replace zona_salmm=3	if municipio=="20372"
replace zona_salmm=3	if municipio=="20373"
replace zona_salmm=3	if municipio=="20374"
replace zona_salmm=3	if municipio=="20375"
replace zona_salmm=3	if municipio=="20376"
replace zona_salmm=3	if municipio=="20377"
replace zona_salmm=3	if municipio=="20378"
replace zona_salmm=3	if municipio=="20379"
replace zona_salmm=3	if municipio=="20380"
replace zona_salmm=3	if municipio=="20381"
replace zona_salmm=3	if municipio=="20382"
replace zona_salmm=3	if municipio=="20383"
replace zona_salmm=3	if municipio=="20384"
replace zona_salmm=3	if municipio=="20385"
replace zona_salmm=3	if municipio=="20386"
replace zona_salmm=3	if municipio=="20387"
replace zona_salmm=3	if municipio=="20388"
replace zona_salmm=3	if municipio=="20389"
replace zona_salmm=3	if municipio=="20390"
replace zona_salmm=3	if municipio=="20391"
replace zona_salmm=3	if municipio=="20392"
replace zona_salmm=3	if municipio=="20393"
replace zona_salmm=3	if municipio=="20394"
replace zona_salmm=3	if municipio=="20395"
replace zona_salmm=3	if municipio=="20396"
replace zona_salmm=3	if municipio=="20397"
replace zona_salmm=3	if municipio=="20398"
replace zona_salmm=3	if municipio=="20399"
replace zona_salmm=3	if municipio=="20400"
replace zona_salmm=3	if municipio=="20401"
replace zona_salmm=3	if municipio=="20402"
replace zona_salmm=3	if municipio=="20403"
replace zona_salmm=3	if municipio=="20404"
replace zona_salmm=3	if municipio=="20405"
replace zona_salmm=3	if municipio=="20406"
replace zona_salmm=3	if municipio=="20407"
replace zona_salmm=3	if municipio=="20408"
replace zona_salmm=3	if municipio=="20409"
replace zona_salmm=3	if municipio=="20410"
replace zona_salmm=3	if municipio=="20411"
replace zona_salmm=3	if municipio=="20412"
replace zona_salmm=3	if municipio=="20413"
replace zona_salmm=3	if municipio=="20414"
replace zona_salmm=3	if municipio=="20415"
replace zona_salmm=3	if municipio=="20416"
replace zona_salmm=3	if municipio=="20417"
replace zona_salmm=3	if municipio=="20418"
replace zona_salmm=3	if municipio=="20419"
replace zona_salmm=3	if municipio=="20420"
replace zona_salmm=3	if municipio=="20421"
replace zona_salmm=3	if municipio=="20422"
replace zona_salmm=3	if municipio=="20423"
replace zona_salmm=3	if municipio=="20424"
replace zona_salmm=3	if municipio=="20425"
replace zona_salmm=3	if municipio=="20426"
replace zona_salmm=3	if municipio=="20427"
replace zona_salmm=3	if municipio=="20428"
replace zona_salmm=3	if municipio=="20429"
replace zona_salmm=3	if municipio=="20430"
replace zona_salmm=3	if municipio=="20431"
replace zona_salmm=3	if municipio=="20432"
replace zona_salmm=3	if municipio=="20433"
replace zona_salmm=3	if municipio=="20434"
replace zona_salmm=3	if municipio=="20435"
replace zona_salmm=3	if municipio=="20436"
replace zona_salmm=3	if municipio=="20437"
replace zona_salmm=3	if municipio=="20438"
replace zona_salmm=3	if municipio=="20439"
replace zona_salmm=3	if municipio=="20440"
replace zona_salmm=3	if municipio=="20441"
replace zona_salmm=3	if municipio=="20442"
replace zona_salmm=3	if municipio=="20443"
replace zona_salmm=3	if municipio=="20444"
replace zona_salmm=3	if municipio=="20445"
replace zona_salmm=3	if municipio=="20446"
replace zona_salmm=3	if municipio=="20447"
replace zona_salmm=3	if municipio=="20448"
replace zona_salmm=3	if municipio=="20449"
replace zona_salmm=3	if municipio=="20450"
replace zona_salmm=3	if municipio=="20451"
replace zona_salmm=3	if municipio=="20452"
replace zona_salmm=3	if municipio=="20453"
replace zona_salmm=3	if municipio=="20454"
replace zona_salmm=3	if municipio=="20455"
replace zona_salmm=3	if municipio=="20456"
replace zona_salmm=3	if municipio=="20457"
replace zona_salmm=3	if municipio=="20458"
replace zona_salmm=3	if municipio=="20459"
replace zona_salmm=3	if municipio=="20460"
replace zona_salmm=3	if municipio=="20461"
replace zona_salmm=3	if municipio=="20462"
replace zona_salmm=3	if municipio=="20463"
replace zona_salmm=3	if municipio=="20464"
replace zona_salmm=3	if municipio=="20465"
replace zona_salmm=3	if municipio=="20466"
replace zona_salmm=3	if municipio=="20467"
replace zona_salmm=3	if municipio=="20468"
replace zona_salmm=3	if municipio=="20469"
replace zona_salmm=3	if municipio=="20470"
replace zona_salmm=3	if municipio=="20471"
replace zona_salmm=3	if municipio=="20472"
replace zona_salmm=3	if municipio=="20473"
replace zona_salmm=3	if municipio=="20474"
replace zona_salmm=3	if municipio=="20475"
replace zona_salmm=3	if municipio=="20476"
replace zona_salmm=3	if municipio=="20477"
replace zona_salmm=3	if municipio=="20478"
replace zona_salmm=3	if municipio=="20479"
replace zona_salmm=3	if municipio=="20480"
replace zona_salmm=3	if municipio=="20481"
replace zona_salmm=3	if municipio=="20482"
replace zona_salmm=3	if municipio=="20483"
replace zona_salmm=3	if municipio=="20484"
replace zona_salmm=3	if municipio=="20485"
replace zona_salmm=3	if municipio=="20486"
replace zona_salmm=3	if municipio=="20487"
replace zona_salmm=3	if municipio=="20488"
replace zona_salmm=3	if municipio=="20489"
replace zona_salmm=3	if municipio=="20490"
replace zona_salmm=3	if municipio=="20491"
replace zona_salmm=3	if municipio=="20492"
replace zona_salmm=3	if municipio=="20493"
replace zona_salmm=3	if municipio=="20494"
replace zona_salmm=3	if municipio=="20495"
replace zona_salmm=3	if municipio=="20496"
replace zona_salmm=3	if municipio=="20497"
replace zona_salmm=3	if municipio=="20498"
replace zona_salmm=3	if municipio=="20499"
replace zona_salmm=3	if municipio=="20500"
replace zona_salmm=3	if municipio=="20501"
replace zona_salmm=3	if municipio=="20502"
replace zona_salmm=3	if municipio=="20503"
replace zona_salmm=3	if municipio=="20504"
replace zona_salmm=3	if municipio=="20505"
replace zona_salmm=3	if municipio=="20506"
replace zona_salmm=3	if municipio=="20507"
replace zona_salmm=3	if municipio=="20508"
replace zona_salmm=3	if municipio=="20509"
replace zona_salmm=3	if municipio=="20510"
replace zona_salmm=3	if municipio=="20511"
replace zona_salmm=3	if municipio=="20512"
replace zona_salmm=3	if municipio=="20513"
replace zona_salmm=3	if municipio=="20514"
replace zona_salmm=3	if municipio=="20515"
replace zona_salmm=3	if municipio=="20516"
replace zona_salmm=3	if municipio=="20517"
replace zona_salmm=3	if municipio=="20518"
replace zona_salmm=3	if municipio=="20519"
replace zona_salmm=3	if municipio=="20520"
replace zona_salmm=3	if municipio=="20521"
replace zona_salmm=3	if municipio=="20522"
replace zona_salmm=3	if municipio=="20523"
replace zona_salmm=3	if municipio=="20524"
replace zona_salmm=3	if municipio=="20525"
replace zona_salmm=3	if municipio=="20526"
replace zona_salmm=3	if municipio=="20527"
replace zona_salmm=3	if municipio=="20528"
replace zona_salmm=3	if municipio=="20529"
replace zona_salmm=3	if municipio=="20530"
replace zona_salmm=3	if municipio=="20531"
replace zona_salmm=3	if municipio=="20532"
replace zona_salmm=3	if municipio=="20533"
replace zona_salmm=3	if municipio=="20534"
replace zona_salmm=3	if municipio=="20535"
replace zona_salmm=3	if municipio=="20536"
replace zona_salmm=3	if municipio=="20537"
replace zona_salmm=3	if municipio=="20538"
replace zona_salmm=3	if municipio=="20539"
replace zona_salmm=3	if municipio=="20540"
replace zona_salmm=3	if municipio=="20541"
replace zona_salmm=3	if municipio=="20542"
replace zona_salmm=3	if municipio=="20543"
replace zona_salmm=3	if municipio=="20544"
replace zona_salmm=3	if municipio=="20545"
replace zona_salmm=3	if municipio=="20546"
replace zona_salmm=3	if municipio=="20547"
replace zona_salmm=3	if municipio=="20548"
replace zona_salmm=3	if municipio=="20549"
replace zona_salmm=3	if municipio=="20550"
replace zona_salmm=3	if municipio=="20551"
replace zona_salmm=3	if municipio=="20552"
replace zona_salmm=3	if municipio=="20553"
replace zona_salmm=3	if municipio=="20554"
replace zona_salmm=3	if municipio=="20555"
replace zona_salmm=3	if municipio=="20556"
replace zona_salmm=3	if municipio=="20557"
replace zona_salmm=3	if municipio=="20558"
replace zona_salmm=3	if municipio=="20559"
replace zona_salmm=3	if municipio=="20560"
replace zona_salmm=3	if municipio=="20561"
replace zona_salmm=3	if municipio=="20562"
replace zona_salmm=3	if municipio=="20563"
replace zona_salmm=3	if municipio=="20564"
replace zona_salmm=3	if municipio=="20565"
replace zona_salmm=3	if municipio=="20566"
replace zona_salmm=3	if municipio=="20567"
replace zona_salmm=3	if municipio=="20568"
replace zona_salmm=3	if municipio=="20569"
replace zona_salmm=3	if municipio=="20570"
replace zona_salmm=3	if municipio=="21001"
replace zona_salmm=3	if municipio=="21002"
replace zona_salmm=3	if municipio=="21003"
replace zona_salmm=3	if municipio=="21004"
replace zona_salmm=3	if municipio=="21005"
replace zona_salmm=3	if municipio=="21006"
replace zona_salmm=3	if municipio=="21007"
replace zona_salmm=3	if municipio=="21008"
replace zona_salmm=3	if municipio=="21009"
replace zona_salmm=3	if municipio=="21010"
replace zona_salmm=3	if municipio=="21011"
replace zona_salmm=3	if municipio=="21012"
replace zona_salmm=3	if municipio=="21013"
replace zona_salmm=3	if municipio=="21014"
replace zona_salmm=3	if municipio=="21015"
replace zona_salmm=3	if municipio=="21016"
replace zona_salmm=3	if municipio=="21017"
replace zona_salmm=3	if municipio=="21018"
replace zona_salmm=3	if municipio=="21019"
replace zona_salmm=3	if municipio=="21020"
replace zona_salmm=3	if municipio=="21021"
replace zona_salmm=3	if municipio=="21022"
replace zona_salmm=3	if municipio=="21023"
replace zona_salmm=3	if municipio=="21024"
replace zona_salmm=3	if municipio=="21025"
replace zona_salmm=3	if municipio=="21026"
replace zona_salmm=3	if municipio=="21027"
replace zona_salmm=3	if municipio=="21028"
replace zona_salmm=3	if municipio=="21029"
replace zona_salmm=3	if municipio=="21030"
replace zona_salmm=3	if municipio=="21031"
replace zona_salmm=3	if municipio=="21032"
replace zona_salmm=3	if municipio=="21033"
replace zona_salmm=3	if municipio=="21034"
replace zona_salmm=3	if municipio=="21035"
replace zona_salmm=3	if municipio=="21036"
replace zona_salmm=3	if municipio=="21037"
replace zona_salmm=3	if municipio=="21038"
replace zona_salmm=3	if municipio=="21039"
replace zona_salmm=3	if municipio=="21040"
replace zona_salmm=3	if municipio=="21041"
replace zona_salmm=3	if municipio=="21042"
replace zona_salmm=3	if municipio=="21043"
replace zona_salmm=3	if municipio=="21044"
replace zona_salmm=3	if municipio=="21045"
replace zona_salmm=3	if municipio=="21046"
replace zona_salmm=3	if municipio=="21047"
replace zona_salmm=3	if municipio=="21048"
replace zona_salmm=3	if municipio=="21049"
replace zona_salmm=3	if municipio=="21050"
replace zona_salmm=3	if municipio=="21051"
replace zona_salmm=3	if municipio=="21052"
replace zona_salmm=3	if municipio=="21053"
replace zona_salmm=3	if municipio=="21054"
replace zona_salmm=3	if municipio=="21055"
replace zona_salmm=3	if municipio=="21056"
replace zona_salmm=3	if municipio=="21057"
replace zona_salmm=3	if municipio=="21058"
replace zona_salmm=3	if municipio=="21059"
replace zona_salmm=3	if municipio=="21060"
replace zona_salmm=3	if municipio=="21061"
replace zona_salmm=3	if municipio=="21062"
replace zona_salmm=3	if municipio=="21063"
replace zona_salmm=3	if municipio=="21064"
replace zona_salmm=3	if municipio=="21065"
replace zona_salmm=3	if municipio=="21066"
replace zona_salmm=3	if municipio=="21067"
replace zona_salmm=3	if municipio=="21068"
replace zona_salmm=3	if municipio=="21069"
replace zona_salmm=3	if municipio=="21070"
replace zona_salmm=3	if municipio=="21071"
replace zona_salmm=3	if municipio=="21072"
replace zona_salmm=3	if municipio=="21073"
replace zona_salmm=3	if municipio=="21074"
replace zona_salmm=3	if municipio=="21075"
replace zona_salmm=3	if municipio=="21076"
replace zona_salmm=3	if municipio=="21077"
replace zona_salmm=3	if municipio=="21078"
replace zona_salmm=3	if municipio=="21079"
replace zona_salmm=3	if municipio=="21080"
replace zona_salmm=3	if municipio=="21081"
replace zona_salmm=3	if municipio=="21082"
replace zona_salmm=3	if municipio=="21083"
replace zona_salmm=3	if municipio=="21084"
replace zona_salmm=3	if municipio=="21085"
replace zona_salmm=3	if municipio=="21086"
replace zona_salmm=3	if municipio=="21087"
replace zona_salmm=3	if municipio=="21088"
replace zona_salmm=3	if municipio=="21089"
replace zona_salmm=3	if municipio=="21090"
replace zona_salmm=3	if municipio=="21091"
replace zona_salmm=3	if municipio=="21092"
replace zona_salmm=3	if municipio=="21093"
replace zona_salmm=3	if municipio=="21094"
replace zona_salmm=3	if municipio=="21095"
replace zona_salmm=3	if municipio=="21096"
replace zona_salmm=3	if municipio=="21097"
replace zona_salmm=3	if municipio=="21098"
replace zona_salmm=3	if municipio=="21099"
replace zona_salmm=3	if municipio=="21100"
replace zona_salmm=3	if municipio=="21101"
replace zona_salmm=3	if municipio=="21102"
replace zona_salmm=3	if municipio=="21103"
replace zona_salmm=3	if municipio=="21104"
replace zona_salmm=3	if municipio=="21105"
replace zona_salmm=3	if municipio=="21106"
replace zona_salmm=3	if municipio=="21107"
replace zona_salmm=3	if municipio=="21108"
replace zona_salmm=3	if municipio=="21109"
replace zona_salmm=3	if municipio=="21110"
replace zona_salmm=3	if municipio=="21111"
replace zona_salmm=3	if municipio=="21112"
replace zona_salmm=3	if municipio=="21113"
replace zona_salmm=3	if municipio=="21114"
replace zona_salmm=3	if municipio=="21115"
replace zona_salmm=3	if municipio=="21116"
replace zona_salmm=3	if municipio=="21117"
replace zona_salmm=3	if municipio=="21118"
replace zona_salmm=3	if municipio=="21119"
replace zona_salmm=3	if municipio=="21120"
replace zona_salmm=3	if municipio=="21121"
replace zona_salmm=3	if municipio=="21122"
replace zona_salmm=3	if municipio=="21123"
replace zona_salmm=3	if municipio=="21124"
replace zona_salmm=3	if municipio=="21125"
replace zona_salmm=3	if municipio=="21126"
replace zona_salmm=3	if municipio=="21127"
replace zona_salmm=3	if municipio=="21128"
replace zona_salmm=3	if municipio=="21129"
replace zona_salmm=3	if municipio=="21130"
replace zona_salmm=3	if municipio=="21131"
replace zona_salmm=3	if municipio=="21132"
replace zona_salmm=3	if municipio=="21133"
replace zona_salmm=3	if municipio=="21134"
replace zona_salmm=3	if municipio=="21135"
replace zona_salmm=3	if municipio=="21136"
replace zona_salmm=3	if municipio=="21137"
replace zona_salmm=3	if municipio=="21138"
replace zona_salmm=3	if municipio=="21139"
replace zona_salmm=3	if municipio=="21140"
replace zona_salmm=3	if municipio=="21141"
replace zona_salmm=3	if municipio=="21142"
replace zona_salmm=3	if municipio=="21143"
replace zona_salmm=3	if municipio=="21144"
replace zona_salmm=3	if municipio=="21145"
replace zona_salmm=3	if municipio=="21146"
replace zona_salmm=3	if municipio=="21147"
replace zona_salmm=3	if municipio=="21148"
replace zona_salmm=3	if municipio=="21149"
replace zona_salmm=3	if municipio=="21150"
replace zona_salmm=3	if municipio=="21151"
replace zona_salmm=3	if municipio=="21152"
replace zona_salmm=3	if municipio=="21153"
replace zona_salmm=3	if municipio=="21154"
replace zona_salmm=3	if municipio=="21155"
replace zona_salmm=3	if municipio=="21156"
replace zona_salmm=3	if municipio=="21157"
replace zona_salmm=3	if municipio=="21158"
replace zona_salmm=3	if municipio=="21159"
replace zona_salmm=3	if municipio=="21160"
replace zona_salmm=3	if municipio=="21161"
replace zona_salmm=3	if municipio=="21162"
replace zona_salmm=3	if municipio=="21163"
replace zona_salmm=3	if municipio=="21164"
replace zona_salmm=3	if municipio=="21165"
replace zona_salmm=3	if municipio=="21166"
replace zona_salmm=3	if municipio=="21167"
replace zona_salmm=3	if municipio=="21168"
replace zona_salmm=3	if municipio=="21169"
replace zona_salmm=3	if municipio=="21170"
replace zona_salmm=3	if municipio=="21171"
replace zona_salmm=3	if municipio=="21172"
replace zona_salmm=3	if municipio=="21173"
replace zona_salmm=3	if municipio=="21174"
replace zona_salmm=3	if municipio=="21175"
replace zona_salmm=3	if municipio=="21176"
replace zona_salmm=3	if municipio=="21177"
replace zona_salmm=3	if municipio=="21178"
replace zona_salmm=3	if municipio=="21179"
replace zona_salmm=3	if municipio=="21180"
replace zona_salmm=3	if municipio=="21181"
replace zona_salmm=3	if municipio=="21182"
replace zona_salmm=3	if municipio=="21183"
replace zona_salmm=3	if municipio=="21184"
replace zona_salmm=3	if municipio=="21185"
replace zona_salmm=3	if municipio=="21186"
replace zona_salmm=3	if municipio=="21187"
replace zona_salmm=3	if municipio=="21188"
replace zona_salmm=3	if municipio=="21189"
replace zona_salmm=3	if municipio=="21190"
replace zona_salmm=3	if municipio=="21191"
replace zona_salmm=3	if municipio=="21192"
replace zona_salmm=3	if municipio=="21193"
replace zona_salmm=3	if municipio=="21194"
replace zona_salmm=3	if municipio=="21195"
replace zona_salmm=3	if municipio=="21196"
replace zona_salmm=3	if municipio=="21197"
replace zona_salmm=3	if municipio=="21198"
replace zona_salmm=3	if municipio=="21199"
replace zona_salmm=3	if municipio=="21200"
replace zona_salmm=3	if municipio=="21201"
replace zona_salmm=3	if municipio=="21202"
replace zona_salmm=3	if municipio=="21203"
replace zona_salmm=3	if municipio=="21204"
replace zona_salmm=3	if municipio=="21205"
replace zona_salmm=3	if municipio=="21206"
replace zona_salmm=3	if municipio=="21207"
replace zona_salmm=3	if municipio=="21208"
replace zona_salmm=3	if municipio=="21209"
replace zona_salmm=3	if municipio=="21210"
replace zona_salmm=3	if municipio=="21211"
replace zona_salmm=3	if municipio=="21212"
replace zona_salmm=3	if municipio=="21213"
replace zona_salmm=3	if municipio=="21214"
replace zona_salmm=3	if municipio=="21215"
replace zona_salmm=3	if municipio=="21216"
replace zona_salmm=3	if municipio=="21217"
replace zona_salmm=3	if municipio=="22001"
replace zona_salmm=3	if municipio=="22002"
replace zona_salmm=3	if municipio=="22003"
replace zona_salmm=3	if municipio=="22004"
replace zona_salmm=3	if municipio=="22005"
replace zona_salmm=3	if municipio=="22006"
replace zona_salmm=3	if municipio=="22007"
replace zona_salmm=3	if municipio=="22008"
replace zona_salmm=3	if municipio=="22009"
replace zona_salmm=3	if municipio=="22010"
replace zona_salmm=3	if municipio=="22011"
replace zona_salmm=3	if municipio=="22012"
replace zona_salmm=3	if municipio=="22013"
replace zona_salmm=3	if municipio=="22014"
replace zona_salmm=3	if municipio=="22015"
replace zona_salmm=3	if municipio=="22016"
replace zona_salmm=3	if municipio=="22017"
replace zona_salmm=3	if municipio=="22018"
replace zona_salmm=3	if municipio=="23001"
replace zona_salmm=3	if municipio=="23002"
replace zona_salmm=3	if municipio=="23003"
replace zona_salmm=3	if municipio=="23004"
replace zona_salmm=3	if municipio=="23005"
replace zona_salmm=3	if municipio=="23006"
replace zona_salmm=3	if municipio=="23007"
replace zona_salmm=3	if municipio=="23008"
replace zona_salmm=3	if municipio=="23009"
replace zona_salmm=3	if municipio=="23010"
replace zona_salmm=3	if municipio=="24001"
replace zona_salmm=3	if municipio=="24002"
replace zona_salmm=3	if municipio=="24003"
replace zona_salmm=3	if municipio=="24004"
replace zona_salmm=3	if municipio=="24005"
replace zona_salmm=3	if municipio=="24006"
replace zona_salmm=3	if municipio=="24007"
replace zona_salmm=3	if municipio=="24008"
replace zona_salmm=3	if municipio=="24009"
replace zona_salmm=3	if municipio=="24010"
replace zona_salmm=3	if municipio=="24011"
replace zona_salmm=3	if municipio=="24012"
replace zona_salmm=3	if municipio=="24013"
replace zona_salmm=3	if municipio=="24014"
replace zona_salmm=3	if municipio=="24015"
replace zona_salmm=3	if municipio=="24016"
replace zona_salmm=3	if municipio=="24017"
replace zona_salmm=3	if municipio=="24018"
replace zona_salmm=3	if municipio=="24019"
replace zona_salmm=3	if municipio=="24020"
replace zona_salmm=3	if municipio=="24021"
replace zona_salmm=3	if municipio=="24022"
replace zona_salmm=3	if municipio=="24023"
replace zona_salmm=3	if municipio=="24024"
replace zona_salmm=3	if municipio=="24025"
replace zona_salmm=3	if municipio=="24026"
replace zona_salmm=3	if municipio=="24027"
replace zona_salmm=3	if municipio=="24028"
replace zona_salmm=3	if municipio=="24029"
replace zona_salmm=3	if municipio=="24030"
replace zona_salmm=3	if municipio=="24031"
replace zona_salmm=3	if municipio=="24032"
replace zona_salmm=3	if municipio=="24033"
replace zona_salmm=3	if municipio=="24034"
replace zona_salmm=3	if municipio=="24035"
replace zona_salmm=3	if municipio=="24036"
replace zona_salmm=3	if municipio=="24037"
replace zona_salmm=3	if municipio=="24038"
replace zona_salmm=3	if municipio=="24039"
replace zona_salmm=3	if municipio=="24040"
replace zona_salmm=3	if municipio=="24041"
replace zona_salmm=3	if municipio=="24042"
replace zona_salmm=3	if municipio=="24043"
replace zona_salmm=3	if municipio=="24044"
replace zona_salmm=3	if municipio=="24045"
replace zona_salmm=3	if municipio=="24046"
replace zona_salmm=3	if municipio=="24047"
replace zona_salmm=3	if municipio=="24048"
replace zona_salmm=3	if municipio=="24049"
replace zona_salmm=3	if municipio=="24050"
replace zona_salmm=3	if municipio=="24051"
replace zona_salmm=3	if municipio=="24052"
replace zona_salmm=3	if municipio=="24053"
replace zona_salmm=3	if municipio=="24054"
replace zona_salmm=3	if municipio=="24055"
replace zona_salmm=3	if municipio=="24056"
replace zona_salmm=3	if municipio=="24057"
replace zona_salmm=3	if municipio=="24058"
replace zona_salmm=3	if municipio=="25001"
replace zona_salmm=3	if municipio=="25002"
replace zona_salmm=3	if municipio=="25003"
replace zona_salmm=3	if municipio=="25004"
replace zona_salmm=3	if municipio=="25005"
replace zona_salmm=3	if municipio=="25006"
replace zona_salmm=3	if municipio=="25007"
replace zona_salmm=3	if municipio=="25008"
replace zona_salmm=3	if municipio=="25009"
replace zona_salmm=3	if municipio=="25010"
replace zona_salmm=3	if municipio=="25011"
replace zona_salmm=3	if municipio=="25012"
replace zona_salmm=3	if municipio=="25013"
replace zona_salmm=3	if municipio=="25014"
replace zona_salmm=3	if municipio=="25015"
replace zona_salmm=3	if municipio=="25016"
replace zona_salmm=3	if municipio=="25017"
replace zona_salmm=3	if municipio=="25018"
replace zona_salmm=3	if municipio=="26001"
replace zona_salmm=1	if municipio=="26002"
replace zona_salmm=3	if municipio=="26003"
replace zona_salmm=2	if municipio=="26004"
replace zona_salmm=3	if municipio=="26005"
replace zona_salmm=3	if municipio=="26006"
replace zona_salmm=2	if municipio=="26007"
replace zona_salmm=3	if municipio=="26008"
replace zona_salmm=3	if municipio=="26009"
replace zona_salmm=3	if municipio=="26010"
replace zona_salmm=3	if municipio=="26011"
replace zona_salmm=2	if municipio=="26012"
replace zona_salmm=3	if municipio=="26013"
replace zona_salmm=3	if municipio=="26014"
replace zona_salmm=3	if municipio=="26015"
replace zona_salmm=2	if municipio=="26016"
replace zona_salmm=2	if municipio=="26017"
replace zona_salmm=2	if municipio=="26018"
replace zona_salmm=1	if municipio=="26019"
replace zona_salmm=2	if municipio=="26020"
replace zona_salmm=2	if municipio=="26021"
replace zona_salmm=2	if municipio=="26022"
replace zona_salmm=3	if municipio=="26023"
replace zona_salmm=3	if municipio=="26024"
replace zona_salmm=2	if municipio=="26025"
replace zona_salmm=2	if municipio=="26026"
replace zona_salmm=3	if municipio=="26027"
replace zona_salmm=3	if municipio=="26028"
replace zona_salmm=2	if municipio=="26029"
replace zona_salmm=2	if municipio=="26030"
replace zona_salmm=3	if municipio=="26031"
replace zona_salmm=3	if municipio=="26032"
replace zona_salmm=2	if municipio=="26033"
replace zona_salmm=3	if municipio=="26034"
replace zona_salmm=2	if municipio=="26035"
replace zona_salmm=2	if municipio=="26036"
replace zona_salmm=3	if municipio=="26037"
replace zona_salmm=3	if municipio=="26038"
replace zona_salmm=1	if municipio=="26039"
replace zona_salmm=3	if municipio=="26040"
replace zona_salmm=3	if municipio=="26041"
replace zona_salmm=2	if municipio=="26042"
replace zona_salmm=1	if municipio=="26043"
replace zona_salmm=3	if municipio=="26044"
replace zona_salmm=2	if municipio=="26045"
replace zona_salmm=2	if municipio=="26046"
replace zona_salmm=2	if municipio=="26047"
replace zona_salmm=1	if municipio=="26048"
replace zona_salmm=3	if municipio=="26049"
replace zona_salmm=3	if municipio=="26050"
replace zona_salmm=3	if municipio=="26051"
replace zona_salmm=3	if municipio=="26052"
replace zona_salmm=3	if municipio=="26053"
replace zona_salmm=3	if municipio=="26054"
replace zona_salmm=1	if municipio=="26055"
replace zona_salmm=2	if municipio=="26056"
replace zona_salmm=3	if municipio=="26057"
replace zona_salmm=2	if municipio=="26058"
replace zona_salmm=1	if municipio=="26059"
replace zona_salmm=2	if municipio=="26060"
replace zona_salmm=3	if municipio=="26061"
replace zona_salmm=2	if municipio=="26062"
replace zona_salmm=3	if municipio=="26063"
replace zona_salmm=2	if municipio=="26064"
replace zona_salmm=2	if municipio=="26065"
replace zona_salmm=3	if municipio=="26066"
replace zona_salmm=3	if municipio=="26067"
replace zona_salmm=3	if municipio=="26068"
replace zona_salmm=3	if municipio=="26069"
replace zona_salmm=1	if municipio=="26070"
replace zona_salmm=2	if municipio=="26071"
replace zona_salmm=2	if municipio=="26072"
replace zona_salmm=3	if municipio=="27001"
replace zona_salmm=3	if municipio=="27002"
replace zona_salmm=3	if municipio=="27003"
replace zona_salmm=3	if municipio=="27004"
replace zona_salmm=3	if municipio=="27005"
replace zona_salmm=3	if municipio=="27006"
replace zona_salmm=3	if municipio=="27007"
replace zona_salmm=3	if municipio=="27008"
replace zona_salmm=3	if municipio=="27009"
replace zona_salmm=3	if municipio=="27010"
replace zona_salmm=3	if municipio=="27011"
replace zona_salmm=3	if municipio=="27012"
replace zona_salmm=3	if municipio=="27013"
replace zona_salmm=3	if municipio=="27014"
replace zona_salmm=3	if municipio=="27015"
replace zona_salmm=3	if municipio=="27016"
replace zona_salmm=3	if municipio=="27017"
replace zona_salmm=3	if municipio=="28001"
replace zona_salmm=2	if municipio=="28002"
replace zona_salmm=2	if municipio=="28003"
replace zona_salmm=2	if municipio=="28004"
replace zona_salmm=3	if municipio=="28005"
replace zona_salmm=3	if municipio=="28006"
replace zona_salmm=1	if municipio=="28007"
replace zona_salmm=3	if municipio=="28008"
replace zona_salmm=2	if municipio=="28009"
replace zona_salmm=3	if municipio=="28010"
replace zona_salmm=2	if municipio=="28011"
replace zona_salmm=2	if municipio=="28012"
replace zona_salmm=3	if municipio=="28013"
replace zona_salmm=1	if municipio=="28014"
replace zona_salmm=1	if municipio=="28015"
replace zona_salmm=3	if municipio=="28016"
replace zona_salmm=3	if municipio=="28017"
replace zona_salmm=3	if municipio=="28018"
replace zona_salmm=3	if municipio=="28019"
replace zona_salmm=3	if municipio=="28020"
replace zona_salmm=2	if municipio=="28021"
replace zona_salmm=1	if municipio=="28022"
replace zona_salmm=3	if municipio=="28023"
replace zona_salmm=1	if municipio=="28024"
replace zona_salmm=1	if municipio=="28025"
replace zona_salmm=3	if municipio=="28026"
replace zona_salmm=1	if municipio=="28027"
replace zona_salmm=2	if municipio=="28028"
replace zona_salmm=2	if municipio=="28029"
replace zona_salmm=3	if municipio=="28030"
replace zona_salmm=3	if municipio=="28031"
replace zona_salmm=1	if municipio=="28032"
replace zona_salmm=1	if municipio=="28033"
replace zona_salmm=3	if municipio=="28034"
replace zona_salmm=1	if municipio=="28035"
replace zona_salmm=3	if municipio=="28036"
replace zona_salmm=3	if municipio=="28037"
replace zona_salmm=2	if municipio=="28038"
replace zona_salmm=3	if municipio=="28039"
replace zona_salmm=1	if municipio=="28040"
replace zona_salmm=3	if municipio=="28041"
replace zona_salmm=3	if municipio=="28042"
replace zona_salmm=2	if municipio=="28043"
replace zona_salmm=3	if municipio=="29001"
replace zona_salmm=3	if municipio=="29002"
replace zona_salmm=3	if municipio=="29003"
replace zona_salmm=3	if municipio=="29004"
replace zona_salmm=3	if municipio=="29005"
replace zona_salmm=3	if municipio=="29006"
replace zona_salmm=3	if municipio=="29007"
replace zona_salmm=3	if municipio=="29008"
replace zona_salmm=3	if municipio=="29009"
replace zona_salmm=3	if municipio=="29010"
replace zona_salmm=3	if municipio=="29011"
replace zona_salmm=3	if municipio=="29012"
replace zona_salmm=3	if municipio=="29013"
replace zona_salmm=3	if municipio=="29014"
replace zona_salmm=3	if municipio=="29015"
replace zona_salmm=3	if municipio=="29016"
replace zona_salmm=3	if municipio=="29017"
replace zona_salmm=3	if municipio=="29018"
replace zona_salmm=3	if municipio=="29019"
replace zona_salmm=3	if municipio=="29020"
replace zona_salmm=3	if municipio=="29021"
replace zona_salmm=3	if municipio=="29022"
replace zona_salmm=3	if municipio=="29023"
replace zona_salmm=3	if municipio=="29024"
replace zona_salmm=3	if municipio=="29025"
replace zona_salmm=3	if municipio=="29026"
replace zona_salmm=3	if municipio=="29027"
replace zona_salmm=3	if municipio=="29028"
replace zona_salmm=3	if municipio=="29029"
replace zona_salmm=3	if municipio=="29030"
replace zona_salmm=3	if municipio=="29031"
replace zona_salmm=3	if municipio=="29032"
replace zona_salmm=3	if municipio=="29033"
replace zona_salmm=3	if municipio=="29034"
replace zona_salmm=3	if municipio=="29035"
replace zona_salmm=3	if municipio=="29036"
replace zona_salmm=3	if municipio=="29037"
replace zona_salmm=3	if municipio=="29038"
replace zona_salmm=3	if municipio=="29039"
replace zona_salmm=3	if municipio=="29040"
replace zona_salmm=3	if municipio=="29041"
replace zona_salmm=3	if municipio=="29042"
replace zona_salmm=3	if municipio=="29043"
replace zona_salmm=3	if municipio=="29044"
replace zona_salmm=3	if municipio=="29045"
replace zona_salmm=3	if municipio=="29046"
replace zona_salmm=3	if municipio=="29047"
replace zona_salmm=3	if municipio=="29048"
replace zona_salmm=3	if municipio=="29049"
replace zona_salmm=3	if municipio=="29050"
replace zona_salmm=3	if municipio=="29051"
replace zona_salmm=3	if municipio=="29052"
replace zona_salmm=3	if municipio=="29053"
replace zona_salmm=3	if municipio=="29054"
replace zona_salmm=3	if municipio=="29055"
replace zona_salmm=3	if municipio=="29056"
replace zona_salmm=3	if municipio=="29057"
replace zona_salmm=3	if municipio=="29058"
replace zona_salmm=3	if municipio=="29059"
replace zona_salmm=3	if municipio=="29060"
replace zona_salmm=3	if municipio=="30001"
replace zona_salmm=3	if municipio=="30002"
replace zona_salmm=3	if municipio=="30003"
replace zona_salmm=3	if municipio=="30004"
replace zona_salmm=3	if municipio=="30005"
replace zona_salmm=3	if municipio=="30006"
replace zona_salmm=3	if municipio=="30007"
replace zona_salmm=3	if municipio=="30008"
replace zona_salmm=3	if municipio=="30009"
replace zona_salmm=3	if municipio=="30010"
replace zona_salmm=3	if municipio=="30011"
replace zona_salmm=3	if municipio=="30012"
replace zona_salmm=3	if municipio=="30013"
replace zona_salmm=3	if municipio=="30014"
replace zona_salmm=3	if municipio=="30015"
replace zona_salmm=3	if municipio=="30016"
replace zona_salmm=3	if municipio=="30017"
replace zona_salmm=3	if municipio=="30018"
replace zona_salmm=3	if municipio=="30019"
replace zona_salmm=3	if municipio=="30020"
replace zona_salmm=3	if municipio=="30021"
replace zona_salmm=3	if municipio=="30022"
replace zona_salmm=3	if municipio=="30023"
replace zona_salmm=3	if municipio=="30024"
replace zona_salmm=3	if municipio=="30025"
replace zona_salmm=3	if municipio=="30026"
replace zona_salmm=3	if municipio=="30027"
replace zona_salmm=3	if municipio=="30028"
replace zona_salmm=3	if municipio=="30029"
replace zona_salmm=3	if municipio=="30030"
replace zona_salmm=3	if municipio=="30031"
replace zona_salmm=3	if municipio=="30032"
replace zona_salmm=3	if municipio=="30033"
replace zona_salmm=3	if municipio=="30034"
replace zona_salmm=3	if municipio=="30035"
replace zona_salmm=3	if municipio=="30036"
replace zona_salmm=3	if municipio=="30037"
replace zona_salmm=3	if municipio=="30038"
replace zona_salmm=1	if municipio=="30039"
replace zona_salmm=2	if municipio=="30040"
replace zona_salmm=3	if municipio=="30041"
replace zona_salmm=3	if municipio=="30042"
replace zona_salmm=3	if municipio=="30043"
replace zona_salmm=3	if municipio=="30044"
replace zona_salmm=3	if municipio=="30045"
replace zona_salmm=3	if municipio=="30046"
replace zona_salmm=3	if municipio=="30047"
replace zona_salmm=1	if municipio=="30048"
replace zona_salmm=3	if municipio=="30049"
replace zona_salmm=3	if municipio=="30050"
replace zona_salmm=3	if municipio=="30051"
replace zona_salmm=3	if municipio=="30052"
replace zona_salmm=3	if municipio=="30053"
replace zona_salmm=3	if municipio=="30054"
replace zona_salmm=3	if municipio=="30055"
replace zona_salmm=3	if municipio=="30056"
replace zona_salmm=3	if municipio=="30057"
replace zona_salmm=3	if municipio=="30058"
replace zona_salmm=3	if municipio=="30059"
replace zona_salmm=3	if municipio=="30060"
replace zona_salmm=1	if municipio=="30061"
replace zona_salmm=3	if municipio=="30062"
replace zona_salmm=3	if municipio=="30063"
replace zona_salmm=3	if municipio=="30064"
replace zona_salmm=3	if municipio=="30065"
replace zona_salmm=3	if municipio=="30066"
replace zona_salmm=3	if municipio=="30067"
replace zona_salmm=3	if municipio=="30068"
replace zona_salmm=3	if municipio=="30069"
replace zona_salmm=3	if municipio=="30070"
replace zona_salmm=3	if municipio=="30071"
replace zona_salmm=3	if municipio=="30072"
replace zona_salmm=3	if municipio=="30073"
replace zona_salmm=3	if municipio=="30074"
replace zona_salmm=3	if municipio=="30075"
replace zona_salmm=3	if municipio=="30076"
replace zona_salmm=3	if municipio=="30077"
replace zona_salmm=3	if municipio=="30078"
replace zona_salmm=3	if municipio=="30079"
replace zona_salmm=3	if municipio=="30080"
replace zona_salmm=3	if municipio=="30081"
replace zona_salmm=1	if municipio=="30082"
replace zona_salmm=3	if municipio=="30083"
replace zona_salmm=3	if municipio=="30084"
replace zona_salmm=3	if municipio=="30085"
replace zona_salmm=3	if municipio=="30086"
replace zona_salmm=3	if municipio=="30087"
replace zona_salmm=3	if municipio=="30088"
replace zona_salmm=3	if municipio=="30089"
replace zona_salmm=3	if municipio=="30090"
replace zona_salmm=3	if municipio=="30091"
replace zona_salmm=3	if municipio=="30092"
replace zona_salmm=3	if municipio=="30093"
replace zona_salmm=3	if municipio=="30094"
replace zona_salmm=3	if municipio=="30095"
replace zona_salmm=3	if municipio=="30096"
replace zona_salmm=3	if municipio=="30097"
replace zona_salmm=3	if municipio=="30098"
replace zona_salmm=3	if municipio=="30099"
replace zona_salmm=3	if municipio=="30100"
replace zona_salmm=3	if municipio=="30101"
replace zona_salmm=3	if municipio=="30102"
replace zona_salmm=3	if municipio=="30103"
replace zona_salmm=3	if municipio=="30104"
replace zona_salmm=3	if municipio=="30105"
replace zona_salmm=3	if municipio=="30106"
replace zona_salmm=3	if municipio=="30107"
replace zona_salmm=1	if municipio=="30108"
replace zona_salmm=3	if municipio=="30109"
replace zona_salmm=3	if municipio=="30110"
replace zona_salmm=1	if municipio=="30111"
replace zona_salmm=3	if municipio=="30112"
replace zona_salmm=3	if municipio=="30113"
replace zona_salmm=3	if municipio=="30114"
replace zona_salmm=3	if municipio=="30115"
replace zona_salmm=3	if municipio=="30116"
replace zona_salmm=3	if municipio=="30117"
replace zona_salmm=3	if municipio=="30118"
replace zona_salmm=3	if municipio=="30119"
replace zona_salmm=3	if municipio=="30120"
replace zona_salmm=3	if municipio=="30121"
replace zona_salmm=3	if municipio=="30122"
replace zona_salmm=3	if municipio=="30123"
replace zona_salmm=3	if municipio=="30124"
replace zona_salmm=3	if municipio=="30125"
replace zona_salmm=3	if municipio=="30126"
replace zona_salmm=3	if municipio=="30127"
replace zona_salmm=3	if municipio=="30128"
replace zona_salmm=3	if municipio=="30129"
replace zona_salmm=3	if municipio=="30130"
replace zona_salmm=2	if municipio=="30131"
replace zona_salmm=3	if municipio=="30132"
replace zona_salmm=3	if municipio=="30133"
replace zona_salmm=3	if municipio=="30134"
replace zona_salmm=3	if municipio=="30135"
replace zona_salmm=3	if municipio=="30136"
replace zona_salmm=3	if municipio=="30137"
replace zona_salmm=3	if municipio=="30138"
replace zona_salmm=3	if municipio=="30139"
replace zona_salmm=3	if municipio=="30140"
replace zona_salmm=3	if municipio=="30141"
replace zona_salmm=3	if municipio=="30142"
replace zona_salmm=3	if municipio=="30143"
replace zona_salmm=3	if municipio=="30144"
replace zona_salmm=3	if municipio=="30145"
replace zona_salmm=3	if municipio=="30146"
replace zona_salmm=3	if municipio=="30147"
replace zona_salmm=3	if municipio=="30148"
replace zona_salmm=3	if municipio=="30149"
replace zona_salmm=3	if municipio=="30150"
replace zona_salmm=3	if municipio=="30151"
replace zona_salmm=3	if municipio=="30152"
replace zona_salmm=3	if municipio=="30153"
replace zona_salmm=3	if municipio=="30154"
replace zona_salmm=3	if municipio=="30155"
replace zona_salmm=3	if municipio=="30156"
replace zona_salmm=3	if municipio=="30157"
replace zona_salmm=3	if municipio=="30158"
replace zona_salmm=3	if municipio=="30159"
replace zona_salmm=3	if municipio=="30160"
replace zona_salmm=3	if municipio=="30161"
replace zona_salmm=3	if municipio=="30162"
replace zona_salmm=3	if municipio=="30163"
replace zona_salmm=3	if municipio=="30164"
replace zona_salmm=3	if municipio=="30165"
replace zona_salmm=3	if municipio=="30166"
replace zona_salmm=3	if municipio=="30167"
replace zona_salmm=3	if municipio=="30168"
replace zona_salmm=3	if municipio=="30169"
replace zona_salmm=3	if municipio=="30170"
replace zona_salmm=3	if municipio=="30171"
replace zona_salmm=3	if municipio=="30172"
replace zona_salmm=3	if municipio=="30173"
replace zona_salmm=3	if municipio=="30174"
replace zona_salmm=3	if municipio=="30175"
replace zona_salmm=3	if municipio=="30176"
replace zona_salmm=3	if municipio=="30177"
replace zona_salmm=3	if municipio=="30178"
replace zona_salmm=3	if municipio=="30179"
replace zona_salmm=3	if municipio=="30180"
replace zona_salmm=3	if municipio=="30181"
replace zona_salmm=3	if municipio=="30182"
replace zona_salmm=3	if municipio=="30183"
replace zona_salmm=3	if municipio=="30184"
replace zona_salmm=3	if municipio=="30185"
replace zona_salmm=3	if municipio=="30186"
replace zona_salmm=3	if municipio=="30187"
replace zona_salmm=3	if municipio=="30188"
replace zona_salmm=2	if municipio=="30189"
replace zona_salmm=3	if municipio=="30190"
replace zona_salmm=3	if municipio=="30191"
replace zona_salmm=3	if municipio=="30192"
replace zona_salmm=3	if municipio=="30193"
replace zona_salmm=3	if municipio=="30194"
replace zona_salmm=3	if municipio=="30195"
replace zona_salmm=3	if municipio=="30196"
replace zona_salmm=3	if municipio=="30197"
replace zona_salmm=3	if municipio=="30198"
replace zona_salmm=3	if municipio=="30199"
replace zona_salmm=3	if municipio=="30200"
replace zona_salmm=3	if municipio=="30201"
replace zona_salmm=3	if municipio=="30202"
replace zona_salmm=3	if municipio=="30203"
replace zona_salmm=1	if municipio=="30204"
replace zona_salmm=3	if municipio=="30205"
replace zona_salmm=1	if municipio=="30206"
replace zona_salmm=3	if municipio=="30207"
replace zona_salmm=3	if municipio=="30208"
replace zona_salmm=3	if municipio=="30209"
replace zona_salmm=3	if municipio=="30210"
replace zona_salmm=3	if municipio=="30211"
replace zona_salmm=3	if municipio=="30212"
replace zona_salmm=3	if municipio=="31001"
replace zona_salmm=3	if municipio=="31002"
replace zona_salmm=3	if municipio=="31003"
replace zona_salmm=3	if municipio=="31004"
replace zona_salmm=3	if municipio=="31005"
replace zona_salmm=3	if municipio=="31006"
replace zona_salmm=3	if municipio=="31007"
replace zona_salmm=3	if municipio=="31008"
replace zona_salmm=3	if municipio=="31009"
replace zona_salmm=3	if municipio=="31010"
replace zona_salmm=3	if municipio=="31011"
replace zona_salmm=3	if municipio=="31012"
replace zona_salmm=3	if municipio=="31013"
replace zona_salmm=3	if municipio=="31014"
replace zona_salmm=3	if municipio=="31015"
replace zona_salmm=3	if municipio=="31016"
replace zona_salmm=3	if municipio=="31017"
replace zona_salmm=3	if municipio=="31018"
replace zona_salmm=3	if municipio=="31019"
replace zona_salmm=3	if municipio=="31020"
replace zona_salmm=3	if municipio=="31021"
replace zona_salmm=3	if municipio=="31022"
replace zona_salmm=3	if municipio=="31023"
replace zona_salmm=3	if municipio=="31024"
replace zona_salmm=3	if municipio=="31025"
replace zona_salmm=3	if municipio=="31026"
replace zona_salmm=3	if municipio=="31027"
replace zona_salmm=3	if municipio=="31028"
replace zona_salmm=3	if municipio=="31029"
replace zona_salmm=3	if municipio=="31030"
replace zona_salmm=3	if municipio=="31031"
replace zona_salmm=3	if municipio=="31032"
replace zona_salmm=3	if municipio=="31033"
replace zona_salmm=3	if municipio=="31034"
replace zona_salmm=3	if municipio=="31035"
replace zona_salmm=3	if municipio=="31036"
replace zona_salmm=3	if municipio=="31037"
replace zona_salmm=3	if municipio=="31038"
replace zona_salmm=3	if municipio=="31039"
replace zona_salmm=3	if municipio=="31040"
replace zona_salmm=3	if municipio=="31041"
replace zona_salmm=3	if municipio=="31042"
replace zona_salmm=3	if municipio=="31043"
replace zona_salmm=3	if municipio=="31044"
replace zona_salmm=3	if municipio=="31045"
replace zona_salmm=3	if municipio=="31046"
replace zona_salmm=3	if municipio=="31047"
replace zona_salmm=3	if municipio=="31048"
replace zona_salmm=3	if municipio=="31049"
replace zona_salmm=3	if municipio=="31050"
replace zona_salmm=3	if municipio=="31051"
replace zona_salmm=3	if municipio=="31052"
replace zona_salmm=3	if municipio=="31053"
replace zona_salmm=3	if municipio=="31054"
replace zona_salmm=3	if municipio=="31055"
replace zona_salmm=3	if municipio=="31056"
replace zona_salmm=3	if municipio=="31057"
replace zona_salmm=3	if municipio=="31058"
replace zona_salmm=3	if municipio=="31059"
replace zona_salmm=3	if municipio=="31060"
replace zona_salmm=3	if municipio=="31061"
replace zona_salmm=3	if municipio=="31062"
replace zona_salmm=3	if municipio=="31063"
replace zona_salmm=3	if municipio=="31064"
replace zona_salmm=3	if municipio=="31065"
replace zona_salmm=3	if municipio=="31066"
replace zona_salmm=3	if municipio=="31067"
replace zona_salmm=3	if municipio=="31068"
replace zona_salmm=3	if municipio=="31069"
replace zona_salmm=3	if municipio=="31070"
replace zona_salmm=3	if municipio=="31071"
replace zona_salmm=3	if municipio=="31072"
replace zona_salmm=3	if municipio=="31073"
replace zona_salmm=3	if municipio=="31074"
replace zona_salmm=3	if municipio=="31075"
replace zona_salmm=3	if municipio=="31076"
replace zona_salmm=3	if municipio=="31077"
replace zona_salmm=3	if municipio=="31078"
replace zona_salmm=3	if municipio=="31079"
replace zona_salmm=3	if municipio=="31080"
replace zona_salmm=3	if municipio=="31081"
replace zona_salmm=3	if municipio=="31082"
replace zona_salmm=3	if municipio=="31083"
replace zona_salmm=3	if municipio=="31084"
replace zona_salmm=3	if municipio=="31085"
replace zona_salmm=3	if municipio=="31086"
replace zona_salmm=3	if municipio=="31087"
replace zona_salmm=3	if municipio=="31088"
replace zona_salmm=3	if municipio=="31089"
replace zona_salmm=3	if municipio=="31090"
replace zona_salmm=3	if municipio=="31091"
replace zona_salmm=3	if municipio=="31092"
replace zona_salmm=3	if municipio=="31093"
replace zona_salmm=3	if municipio=="31094"
replace zona_salmm=3	if municipio=="31095"
replace zona_salmm=3	if municipio=="31096"
replace zona_salmm=3	if municipio=="31097"
replace zona_salmm=3	if municipio=="31098"
replace zona_salmm=3	if municipio=="31099"
replace zona_salmm=3	if municipio=="31100"
replace zona_salmm=3	if municipio=="31101"
replace zona_salmm=3	if municipio=="31102"
replace zona_salmm=3	if municipio=="31103"
replace zona_salmm=3	if municipio=="31104"
replace zona_salmm=3	if municipio=="31105"
replace zona_salmm=3	if municipio=="31106"
replace zona_salmm=3	if municipio=="32001"
replace zona_salmm=3	if municipio=="32002"
replace zona_salmm=3	if municipio=="32003"
replace zona_salmm=3	if municipio=="32004"
replace zona_salmm=3	if municipio=="32005"
replace zona_salmm=3	if municipio=="32006"
replace zona_salmm=3	if municipio=="32007"
replace zona_salmm=3	if municipio=="32008"
replace zona_salmm=3	if municipio=="32009"
replace zona_salmm=3	if municipio=="32010"
replace zona_salmm=3	if municipio=="32011"
replace zona_salmm=3	if municipio=="32012"
replace zona_salmm=3	if municipio=="32013"
replace zona_salmm=3	if municipio=="32014"
replace zona_salmm=3	if municipio=="32015"
replace zona_salmm=3	if municipio=="32016"
replace zona_salmm=3	if municipio=="32017"
replace zona_salmm=3	if municipio=="32018"
replace zona_salmm=3	if municipio=="32019"
replace zona_salmm=3	if municipio=="32020"
replace zona_salmm=3	if municipio=="32021"
replace zona_salmm=3	if municipio=="32022"
replace zona_salmm=3	if municipio=="32023"
replace zona_salmm=3	if municipio=="32024"
replace zona_salmm=3	if municipio=="32025"
replace zona_salmm=3	if municipio=="32026"
replace zona_salmm=3	if municipio=="32027"
replace zona_salmm=3	if municipio=="32028"
replace zona_salmm=3	if municipio=="32029"
replace zona_salmm=3	if municipio=="32030"
replace zona_salmm=3	if municipio=="32031"
replace zona_salmm=3	if municipio=="32032"
replace zona_salmm=3	if municipio=="32033"
replace zona_salmm=3	if municipio=="32034"
replace zona_salmm=3	if municipio=="32035"
replace zona_salmm=3	if municipio=="32036"
replace zona_salmm=3	if municipio=="32037"
replace zona_salmm=3	if municipio=="32038"
replace zona_salmm=3	if municipio=="32039"
replace zona_salmm=3	if municipio=="32040"
replace zona_salmm=3	if municipio=="32041"
replace zona_salmm=3	if municipio=="32042"
replace zona_salmm=3	if municipio=="32043"
replace zona_salmm=3	if municipio=="32044"
replace zona_salmm=3	if municipio=="32045"
replace zona_salmm=3	if municipio=="32046"
replace zona_salmm=3	if municipio=="32047"
replace zona_salmm=3	if municipio=="32048"
replace zona_salmm=3	if municipio=="32049"
replace zona_salmm=3	if municipio=="32050"
replace zona_salmm=3	if municipio=="32051"
replace zona_salmm=3	if municipio=="32052"
replace zona_salmm=3	if municipio=="32053"
replace zona_salmm=3	if municipio=="32054"
replace zona_salmm=3	if municipio=="32055"
replace zona_salmm=3	if municipio=="32056"
replace zona_salmm=3	if municipio=="32057"
replace zona_salmm=3	if municipio=="32058"

label define zona_salmm 1"A" 2"B" 3"C"
label value zona_salmm zona_salmm
label var zona_salmm "estructura zonal para asignación del SML"
        }
  
  
else {
      di as result "ubica_geo does not exist"
            
gen zona_salmm=.	
replace zona_salmm=3	if entidad=="01"
replace zona_salmm=1	if entidad=="02"
replace zona_salmm=1	if entidad=="03"
replace zona_salmm=3	if entidad=="04"
replace zona_salmm=3	if entidad=="05"
replace zona_salmm=3	if entidad=="06"
replace zona_salmm=3	if entidad=="07"
replace zona_salmm=3	if entidad=="08"
replace zona_salmm=1	if entidad=="08"
replace zona_salmm=1	if entidad=="09"
replace zona_salmm=3	if entidad=="10"
replace zona_salmm=3	if entidad=="11"
replace zona_salmm=1	if entidad=="12"
replace zona_salmm=3	if entidad=="12"
replace zona_salmm=3	if entidad=="13"
replace zona_salmm=3	if entidad=="14"
replace zona_salmm=2	if entidad=="14"
replace zona_salmm=3	if entidad=="15"
replace zona_salmm=1	if entidad=="15"
replace zona_salmm=3	if entidad=="16"
replace zona_salmm=3	if entidad=="17"
replace zona_salmm=3	if entidad=="18"
replace zona_salmm=3	if entidad=="19"
replace zona_salmm=2	if entidad=="19"
replace zona_salmm=3	if entidad=="20"
replace zona_salmm=3	if entidad=="21"
replace zona_salmm=3	if entidad=="22"
replace zona_salmm=3	if entidad=="23"
replace zona_salmm=3	if entidad=="24"
replace zona_salmm=3	if entidad=="25"
replace zona_salmm=3	if entidad=="26"
replace zona_salmm=1	if entidad=="26"
replace zona_salmm=2	if entidad=="26"
replace zona_salmm=3	if entidad=="27"
replace zona_salmm=3	if entidad=="28"
replace zona_salmm=2	if entidad=="28"
replace zona_salmm=1	if entidad=="28"
replace zona_salmm=3	if entidad=="29"
replace zona_salmm=3	if entidad=="30"
replace zona_salmm=1	if entidad=="30"
replace zona_salmm=2	if entidad=="30"
replace zona_salmm=3	if entidad=="31"
replace zona_salmm=3	if entidad=="32"
label define zona_salmm 1"A" 2"B" 3"C"
label value zona_salmm zona_salmm
label var zona_salmm "estructura zonal para asignación del SML"
   }
   }

* MLO esta en valores diarios, no hace falta multiplicar por 8
generat salmm_ci=57.46*30 if zona_salmm==1
replace salmm_ci=55.84*30 if zona_salmm==2
replace salmm_ci=54.47*30 if zona_salmm==3
label var salmm_ci "Salario minimo legal"
/*
******************************
*	emp_ci
******************************
*trabajo= trabajo durante el mes pasado
gen trabajon=real(trabajo)
gen verificn=real(verifica)
gen mot_ausen=real(motivo)

gen emp_ci=.
replace emp_ci=1 if trabajon==1 
replace emp_ci=0 if trabajon==2
replace emp_ci=. if trabajon==.
replace emp_ci=1 if ((verificn>=1 & verificn<=4) | (verificn==5 & mot_ausen <=6))
label var emp_ci "1 Empleado"


******************************
*	desemp1_ci	& desemp2_ci & desemp3_ci 
******************************
gen desemp1_ci=(emp_ci==0 & bustrab_1=="1")
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
*	subemp_ci
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
replace categopri_ci=2 if personal1=="2"sub

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
replace categosec_ci=2 if personal2=="2"
replace categosec_ci=3 if subor2=="1"
replace categosec_ci=4 if pago2== "2" 
replace categosec_ci=. if emp_ci!=1
label var categosec_ci "Categoria ocupacional trabajo secundario"
label define categosec_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4"Familiar no remunerado"
label value categosec_ci categosec_ci

*****************
*tipocontrato_ci*
*****************
/*
generat tipocontrato_ci=. /* Solo disponible para asalariados y trab independ*/
replace tipocontrato_ci=1 if contrato1==2 
replace tipocontrato_ci=2 if contrato1==1              
replace tipocontrato_ci=. if contrato1==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

* Corregido por categopri_ci e incorporando todas las categorias MGD 06/17/2014
destring contrato1 tipocontr1, replace
g tipocontrato_ci=.
replace tipocontrato_ci=1 if (contrato1==1 & tipocontr1==2) & categopri_ci==3
replace tipocontrato_ci=2 if (contrato1==1 & tipocontr1==1) & categopri_ci==3
replace tipocontrato_ci=3 if (contrato1==2 | tipocontrato_ci==.) & categopri_ci==3      
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

******************************
*	nempleos_ci
******************************
gen nempleos_ci=.
replace nempleos_ci=1 if trabajos=="1"
replace nempleos_ci=2 if trabajos=="2"
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

tostring cuo1, replace
gen ocupa=real(substr(cuo1,1,2))
/*
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa==11 | ocupa==12) & emp_ci==1
replace ocupa_ci=2 if (ocupa==21) & emp_ci==1
replace ocupa_ci=3 if (ocupa==51 | ocupa==61 |ocupa==62) & emp_ci==1
replace ocupa_ci=4 if (ocupa==71 | ocupa==72) & emp_ci==1
replace ocupa_ci=5 if (ocupa==13 |ocupa==14 | ocupa==81|ocupa==82) & emp_ci==1
replace ocupa_ci=6 if (ocupa==41) & emp_ci==1
replace ocupa_ci=7 if (ocupa>=52 & ocupa<=55) & emp_ci==1
replace ocupa_ci=8 if (ocupa==83) & emp_ci==1
replace ocupa_ci=9 if (ocupa==99) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1
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
*Modificación Mayra Sáenz - Agosto 2015: Se reemplazan los ingresos por los generados con base en CONEVAL

/*

******************************
*	ylmpri_ci 
******************************

replace ing_1P008 = ing_1P008/12
replace ing_1P009 = ing_1P009/12

*Trabajo principal subordinado
egen ylmpri_ci_sub_=rsum(ing_1P001-ing_1P009) if emp_ci==1 
replace ylmpri_ci_sub=. if (ylmpri_ci_sub_<=0  | ylmpri_ci_sub_>=999999999)


*Trabajo principal independientes
egen ylmpri_ci_indep_= rsum(ing_1P010) if emp_ci==1 
replace ylmpri_ci_indep= . if (ing_1P010<=0 | ing_1P010>=999999999)


*Trabajadores en cooperativas, sociedades
egen ylmpri_ci_asoc_= rsum(ing_1P011)	if emp_ci==1
replace ylmpri_ci_asoc=. if ing_1P011<=0 | ing_1P011>=999999999

egen ylmpri_ci= rsum(ylmpri_ci_sub ylmpri_ci_indep  ylmpri_ci_asoc) if ylmpri_ci_sub!=. | ylmpri_ci_indep!=. | ylmpri_ci_asoc!=.

*br folioviv foliohog numren emp_ci indep1 subor1 ylmpri_ci_sub ylmpri_ci_indep ylmpri_ci_asoc  ylmpri_ci ing_1P001-ing_1P013

***************************************************
*Ingreso laboral no monetario actividad secundaria*
***************************************************

gen ylnmsec_ci=.

******************************
*	ylmsec_ci
******************************

replace ing_1P015 = ing_1P015/12
replace ing_1P016 = ing_1P016/12

*Trabaja secundario para subordinados
egen ylmsec_ci_sub_=rsum(ing_1P014-ing_1P016) if emp_ci==1 
replace ylmsec_ci_sub=. if (ylmsec_ci_sub_<=0  | ylmsec_ci_sub_>=999999999)

*Trabajo secundario independientes
egen ylmsec_ci_indep_= rsum(ing_1P017) if emp_ci==1 
replace ylmsec_ci_indep= . if (ing_1P017<=0 | ing_1P017>=999999999)

*Trabajadores en cooperativas, sociedades
egen ylmsec_ci_asoc_= rsum(ing_1P018)	if emp_ci==1
replace ylmsec_ci_asoc=. if ing_1P018<=0 | ing_1P018>=999999999

egen ylmsec_ci= rsum(ylmsec_ci_sub ylmsec_ci_indep ylmsec_ci_asoc) if ylmsec_ci_sub!=. | ylmsec_ci_indep!=. | ylmsec_ci_asoc!=.

*************
*ylmotros_ci*
*************

egen ylmotros_ci= rsum(ing_1P021)

*********
*ynlm_ci*
*********

egen ynlm_ci=rsum(ing_1P023-ing_1P066 ing_1P012 ing_1P019)   


*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

******************************
*	nrylmpri_ci 
******************************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1
label var nrylmpri_ci "Identificador de No respuesta del ingreso de la actividad principal"


*br folioviv foliohog numren emp_ci indep1 subor1 personal1 ylmpri_ci_sub ylmpri_ci_indep ylmpri_ci_asoc  ylmpri_ci   ing_1P001-ing_1P013 if emp_ci==1 & nrylmpri_ci==1

*****************
*** ylnmpri_ci***
*****************

gen ylnmpri_ci=.

*****************************************************************
*Identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

******************************
*	ylm_ci 
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci) if emp_ci==1
label var ylm_ci "Ingreso laboral monetario total"

******************************
*	ylnm_ci
******************************

gen ylnm_ci=.

******************************
*	ynlnm_ci
******************************
gen ynlnm_ci=.


************************
*** HOUSEHOLD INCOME ***
************************

******************************
*	nrylmpri_ch
******************************

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
label var nrylmpri_ch "Identificador de los hogares en donde alguno de los miembros NS/NR el ingreso de la actividad principal"
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.

******************************
*	ylm_ch  
******************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar-ignora No Respuesta"

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.

******************************
*	ylmnr_ch 
******************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
label var ylmnr_ch "Ingreso laboral monetario del hogar - considera la no respuesta"
replace ylmnr_ch=. if nrylmpri_ch==1

******************************
*	ylnm_ch 
******************************

gen ylnm_ch=.
label var ylnm_ch "Ingreso laboral no monetario del hogar" 

******************************
*	remesas_ci & remesas_ch 
******************************
/*
gen remesas_ci=ing_1P041
label var remesas_ci "Remesas reportadas por el individuo"

egen remesas_ch=rsum(remesas_ci)if miembros_ci==1
label var remesas_ch "Remesas del hogar"
*/
*Modificación Mayra Sáenz - Septiembre 2014
gen remesas_ci=ing_1P041 if ing_1P041>=0 & ing_1P041~=.
egen double remesas_ch = total(remesas_ci), by(idh_ch)
replace remesas_ch=. if remesas_ch==0

******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlmh=sum(ynlm_ci) if miembros_ci==1
egen ynlm_ch=rsum(ynlmh )
label var ynlm_ch "Ingreso no laboral monetario del Hogar"

******************************
*	ynlnm_ch 
******************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar"

******************************
*	ylmhopri_ci 
******************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
*NA 

******************************
*	ylmho_ci 
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario monetario de todas las actividades"

******************************
*	autocons_ci & autocons_ch
******************************
gen autocons_ci=.
gen autocons_ch=.

******************************
*	rentaimp_ch 
******************************
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

*/

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
egen ynlnm = rsum(autocons pago_esp reg_esp redan reg_espn), missing

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

gen autocons_ci= (autocons/nmiembros_ch)


*******************
*** autocons_ch ***
*******************

bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing

*******************
*** rentaimp_ch ***
*******************
*Modificacion Mayra Sáenz - Agosto 2015- Antes estaba generada como missing.
gen rentaimp_ch= est_alq

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

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (bustrab_3=="3" & condocup_ci==3)
replace categoinac_ci = 2 if  (bustrab_5=="5" & condocup_ci==3)
replace categoinac_ci = 3 if  (bustrab_4=="4" & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
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




******************************************************************************
*	EDUCATION
******************************************************************************
destring nivel nivelaprob gradoaprob antec_esc, replace

gen nivel_ed=nivelaprob
gen grado_ed= gradoaprob

gen aedu_ci=.
replace aedu_ci=0 if nivel_ed==0 |nivel_ed==1 
replace aedu_ci=grado_ed if nivel_ed==2
replace aedu_ci= grado_ed+6 if nivel_ed==3
replace aedu_ci= grado_ed+6 if nivel_ed==6 & antec_esc==1
replace aedu_ci= grado_ed+9 if nivel_ed==4
replace aedu_ci= grado_ed+9 if nivel_ed==6 & antec_esc==2
replace aedu_ci= grado_ed+12 if nivel_ed==5 |nivel_ed==7
replace aedu_ci= grado_ed+12 if nivel_ed==6 & antec_esc==3
replace aedu_ci= grado_ed+12+5 if nivel_ed==8
replace aedu_ci= grado_ed+12+5+2 if nivel_ed==9


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
gen byte eduui_ci=(aedu_ci>12 & aedu_ci<16) & (nivelaprob==7 | nivelaprob==5)
replace eduui_ci=1 if (aedu_ci>12 & aedu_ci<15 & nivelaprob==6 & (antec_esc==3 | antec_esc == 2)) 
replace eduui_ci=. if aedu_ci==.
label var eduui_ci "Universitaria o Terciaria Incompleta"

******************************
*	eduuc_ci
******************************
gen byte eduuc_ci=(aedu_ci>=16) & (nivelaprob==7 | nivelaprob==5)
replace eduui_ci=1 if (aedu_ci>=15  & nivelaprob==6 & (antec_esc==3 | antec_esc == 2 )) 
replace eduuc_ci=1 if nivelaprob==8 | nivelaprob==9
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "Universitaria o Terciaria Completa"

******************************
*	edupre_ci
******************************
gen edupre_ci=.
label var edupre_ci "Educacion preescolar"
******************************
*	asispre_ci
******************************
g asispre_ci=(asis_esc=="1" & nivel==1)
la var asispre_ci "Asiste a educacion prescolar"	

******************************
*	eduac_ci
******************************
gen byte eduac_ci=.
replace eduac_ci=0 if nivelaprob==6 & antec_esc==3 | nivelaprob==5
replace eduac_ci=1 if nivelaprob>=7 & nivelaprob<=9
label var eduac_ci "Superior universitario vs. no universitario"

******************************
*	asiste_ci
******************************
gen asiste_ci=(asis_esc=="1")
replace asiste_ci=. if asis_esc !="1" & asis_esc !="2"
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
replace edupub_ci=1 if tipoesc=="1" & asis_esc=="1"
replace edupub_ci=0 if (tipoesc=="2" | tipoesc=="3") & asis_esc=="1"
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"

drop nivel_ed grado_ed




******************************************************************************
*	INFRAESTRUCTURE VARIABLES 
******************************************************************************

************
*aguared_ch*
************
destring dis_agua , replace
gen aguared_ch=.
replace aguared_ch=1 if dis_agua ==1 | dis_agua ==2
replace aguared_ch=0 if dis_agua >=3 & dis_agua <=6
label var aguared_ch "Acceso a una fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0

*****************
*aguafuente_ch*
*****************

gen aguafuente_ch = 0
replace aguafuente_ch = 1 if (dis_agua==1 | dis_agua==2)
replace aguafuente_ch = 2 if dis_agua==3
replace aguafuente_ch = 5 if dis_agua==4
replace aguafuente_ch = 9 if dis_agua==5
replace aguafuente_ch = 6 if dis_agua==6
replace aguafuente_ch = 10 if dis_agua==7 

*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch= 1 if dis_agua==1
replace aguadist_ch= 2 if dis_agua==2
replace aguadist_ch= 3 if dis_agua ==3
label var aguadist_ch "Ubicacion de la principal fuente de agua"


**************
*aguadisp1_ch*
**************
destring dot_agua, replace
gen aguadisp1_ch =9

**************
*aguadisp2_ch*
**************
destring dot_agua, replace
gen aguadisp2_ch = .
replace aguadisp2_ch = 1 if dot_agua>1
replace aguadisp2_ch = 3 if dot_agua==1

*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10

*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7 

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

*****************
*bano_ch         *  Altered
*****************
destring excus, replace
destring drenaje, replace
destring adm_ag, replace
gen bano_ch=.
replace bano_ch=0 if excus==2
replace bano_ch=1 if drenaje==1 & excus==1 
replace bano_ch=2 if drenaje==2 & excus==1 
replace bano_ch=4 if (drenaje==4 | drenaje==3) & excus==1
replace bano_ch=6 if drenaje==5 & excus==1 & adm_ag== 3

***************
***banoex_ch***
***************
destring uso_com, replace
gen banoex_ch=.
replace banoex_ch=1 if uso_com==2
replace banoex_ch=0 if uso_com==1
label var banoex_ch "Servicio higiénico de uso exclusivo del hogar"

*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch = 0 if excus == 1
replace sinbano_ch = 1 if excus == 2 & drenaje <=4
replace sinbano_ch = 3 if excus == 2 & drenaje ==5
*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch =9

******************************
*	luz_ch
******************************
gen luz_ch=(elect=="1")

******************************
*	luzmide_ch
******************************

gen luzmide_ch=(med_luz=="1")
*Se inserta en el 2010.


******************************
*	combust_ch
******************************
destring combus, replace
gen combust_ch=.
replace combust_ch=1 if combus==3 | combus==4 | combus==5
replace combust_ch=0 if combus==1 | combus ==2 | combus==6
label var combust_ch "Principal combustible usado es gas o electric"


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

destring pisos, replace
gen piso_ch=.
replace piso_ch=0 if pisos==1
replace piso_ch=1 if pisos>=2 & pisos<=3

******************************
*	pared_ch
******************************
destring pared, replace
gen pared_ch=.
replace pared_ch=0 if pared==1 | pared==2 | pared==4 | pared==5
replace pared_ch=1 if pared==3 | pared>=6 & pared<=8
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
destring techos, replace
gen techo_ch=.
replace techo_ch=0 if techos==1 | techos==2 | techos==5 
replace techo_ch=1 if techos==3 | techos==3 | (techos>=6 & techos<=9)

/*
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


******************************
*	dorm_ch
******************************

gen dorm_ch=dormi
label var dorm_ch "#Habitaciones exclusivamente para dormir" 

******************************
*	cuartos_ch
******************************
gen cuartos_ch=cuart
label var cuartos_ch "#Habitaciones en el hogar"
notes: cuartos_ch esta indicando cuartos, contando cocina pero no bano 

******************************
*	cocina_ch
******************************
destring cua_coc, replace
gen cocina_ch=.
replace cocina_ch=1 if cua_coc==1
replace cocina_ch=0 if cua_coc==2
label var cocina_ch "Si existe un cuarto separado y exclusivo para cocinar"

******************************
*	telef_ch
******************************
gen telef_ch=(serv_1=="1")
label var telef_ch "Hogar con sc telefonico fijo"

******************************
*	refrig_ch
******************************
destring eqh10_n, replace
gen refrig_ch= .
replace refrig_ch= 0 if eqh10_n ==0
replace refrig_ch= 1 if eqh10_n>=1

******************************
*	freez_ch
******************************
gen freez_ch=.
*NA

******************************
*	auto_ch
******************************
destring vehi1_n vehi2_n vehi3_n, replace 
gen auto_ch=.
replace auto_ch = 0 if  vehi1_n==0 & vehi2_n==0 & vehi3_n==0
replace auto_ch = 1 if vehi1_n>=1 | vehi2_n>=1 | vehi3_n>=1
label var auto_ch "El hogar posee automovil prticular"

******************************
*	compu_ch
******************************

* Modificaciones Marcela Rubio Septiembre 2014
/*
gen compu_ch=.
replace compu_ch= 0 if eqh17_n==0
replace compu_ch= 0 if eqh17_n>=1
*/
gen compu_ch = (eqh17_n>0)

******************************
*	internet_ch
******************************
gen internet_ch=(serv_4=="1")
******************************

*	cel_ch
******************************
gen cel_ch=(serv_2=="1") 

******************************
*	vivi1_ch
******************************
gen vivi1_ch=.
replace vivi1_ch=1 if claviv=="1"
replace vivi1_ch=2 if claviv=="2"
replace vivi1_ch=3 if claviv>="3"
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
gen vivi2_ch=(vivi1_ch==2)

******************************
*	viviprop_ch
******************************
destring tenen, replace
gen viviprop_ch=.
replace viviprop_ch=0 if tenen==1
replace viviprop_ch=1 if tenen==4   
replace viviprop_ch=2 if tenen==3
replace viviprop_ch=3 if tenen==2 | tenen==5 | tenen==6
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
destring escri, replace 
gen vivitit_ch=.
replace vivitit_ch=1 if escri==1 | escri==2
replace vivitit_ch=0 if escri==3
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
gen vivialqimp_ch=estim
replace vivialqimp=0 if estim<0
label var vivialqimp_ch "Alquiler mensual imputado"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first


rename cuo1  codocupa
rename scian1 codindustria
destring codocupa codindustria, replace
compress


saveold "`base_out'", replace


log close


















































