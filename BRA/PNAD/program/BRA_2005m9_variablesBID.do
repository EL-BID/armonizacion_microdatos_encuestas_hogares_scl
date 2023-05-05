* (versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOvI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS BRA
local ENCUESTA PNAD
local ANO "2005"
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
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
versión 2010: Yanira Oviedo
 Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
 Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com - Octubre de 2017
Última modificación: Cesar Lins - Marzo 2021

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

*Nota: Bases de datos con nuevos pesos (descargadas el 30 septiembre 2013)
use `base_in', clear

*****************
*** region_ci ***
*****************
*YL: generacion "region_c" proyecto maps America.	
destring uf, replace	
gen region_c = uf
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

/********************************/
/*    VARIABLES DE IDENFIFACIÓN	*/
/********************************/

***************
****idh_ch*****
***************
sort uf v0102 v0103 v0403
egen idh_ch=group(uf v0102 v0103 v0403)
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************
gen idp_ci=v0301

***************
***factor_ci***
***************
gen factor_ci=v4729

***************
***factor_ch***
***************
gen factor_ch=v4611

**********
***zona***
**********
gen zona_c=1 if v4728>=1 & v4728<=3
replace zona_c=0 if v4728>=4 & v4728<=8
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************
gen str3 pais_c="BRA"

**********
***anio***
**********
gen anio_c=2005
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=9

************
***upm_ci***
************
gen upm_ci=upa

***************
***estrato_ci**
***************
gen estrato_ci=v4602


/********************************/
/*    vARIABLES DEL HOGAR	*/
/********************************/

/************************************************************************/
/*			vARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	

 *****************
 ***aguared_ch****
 *****************  
gen aguared_ch=(v0212==2 | v0213==1)
label var aguared_ch "Acceso a fuente de agua por red"

 *****************
 ***aguadist_ch****
 *****************  
gen aguadist_ch=1 if v0211==1 |v0213==1
replace aguadist_ch=2 if v0214==2
replace aguadist_ch=3 if v0214==4
replace aguadist_ch=. if v0214==9 
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la casa" 2"Afuera de la casa pero dentro del terreno" 3"Afuera de la casa y del terreno" 
label val aguadist_ch aguadist_ch  

 *****************
 ***aguamala_ch***
 ***************** 
gen aguamala_ch=(v0212==6) /*"Otra procedencia"*/	
replace aguamala_ch=. if v0212 == 9
label var aguamala_ch "Agua unimproved según MDG"

 *****************
 ***aguamide_ch***
 ***************** 
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

 ************
 ***luz_ch***
 ************ 
gen luz_ch=(v0219==1)
replace luz_ch=. if v0219==9
label var luz_ch  "La principal fuente de iluminación es electricidad"

 ****************
 ***luzmide_ch***
 **************** 
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

 ****************
 ***combust_ch***
 ****************
gen combust_ch=(v0223==1|v0223==2|v0223==5)
replace combust_ch=. if v0223==9
label var combust_ch "Principal combustible gas o electricidad" 

 *************
 ***bano_ch***
 *************
gen bano_ch=(v0215==1)
replace bano_ch=. if v0215==9
label var bano_ch "El hogar tiene servicio sanitario"

 ***************
 ***banoex_ch***
 ***************
gen banoex_ch=(v0216==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.|v0216==9
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

 *************
 ***des1_ch***
 *************
gen des1_ch=1 if v0217>=1 & v0217<=3
replace des1_ch=2 if v0217==4
replace des1_ch=3 if v0217>=5
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if v0217==9
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

 *************
 ***des2_ch***
 *************
*El indicador debería ser una reclasificación de des1_ch, por ello se cambia aquí: 
gen des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2 
replace des2_ch=2 if des1_ch==3
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

 *************
 ***piso_ch***
 *************
gen piso_ch=.
label var piso_ch "Materiales de construcción del piso" 

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
gen aguamejorada_ch = . 
replace aguamejorada_ch = 1 if v0212 == 2 | v0212 ==4
replace aguamejorada_ch = 0 if v0212 == 6
				
 *********************
 ***banomejorado_ch***
 *********************
gen banomejorado_ch = .
replace banomejorado_ch = 1 if (v0215 == 1 & (v0217 >= 1 & v0217 <=3) & v0216 == 2 )
replace banomejorado_ch = 0 if (v0215 == 1 & (v0217 >= 1 & v0217 <=3) & v0216 == 4) | v0215 == 3 | (v0215 == 1 & (v0217 >= 4 & v0217<=7))


 **************
 ***pared_ch***
 **************
* Se cambia la construcción de la variable incluyendo: tapia sin revestir y de paja 
/*
gen pared_ch=0
replace pared_ch=1 if v0203==1 | v0203==2 |v0203==4
replace pared_ch=2 if v0203==6 | v0203==3 |v0203==5
replace pared_ch=. if v0203==9
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales:otros"
label val pared_ch pared_ch
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 5 (paja) como material impermanente
gen pared_ch=0 if v0203==5 
replace pared_ch=1 if v0203==1 | v0203==2 |v0203==4
replace pared_ch=2 if v0203==6 | v0203==3 
replace pared_ch=. if v0203==9
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales: natural, otros"
label val pared_ch pared_ch

 **************
 ***techo_ch***
 **************
/*
*No se incluían los techos de paja
gen techo_ch=0
replace techo_ch=1 if v0204<=5
replace techo_ch=2 if v0204==7 |v0204==6
replace techo_ch=. if v0204==9
label var techo_ch "Materiales de construcción del techo"
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 6 (paja) como material impermanente
gen techo_ch=0 if v0204==6
replace techo_ch=1 if v0204<=5
replace techo_ch=2 if v0204==7
replace techo_ch=. if v0204==9
label var techo_ch "Materiales de construcción del techo"

 **************
 ***resid_ch***
 **************
gen resid_ch=0 if v0218==1 | v0218==2
replace resid_ch=1 if v0218==3
replace resid_ch=2 if v0218==4 | v0218==5
replace resid_ch=3 if v0218==6 
replace resid_ch=. if v0218==9 
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

 **************
 ***dorm_ch***
 **************
gen dorm_ch=v0206
replace dorm_ch=. if v0206==99 |v0206==-1
label var dorm_ch "Habitaciones para dormir"

 ****************
 ***cuartos_ch***
 ****************
gen cuartos_ch=v0205
replace cuartos_ch=. if v0205==99 | v0205==-1
label var cuartos_ch "Habitaciones en el hogar"

 ***************
 ***cocina_ch***
 ***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

 **************
 ***telef_ch***
 **************
gen telef_ch=(v2020==2)
replace telef_ch=. if v2020==9
label var telef_ch "El hogar tiene servicio telefónico fijo"

 ***************
 ***refrig_ch***
 *************** 
gen refrig_ch=(v0228==2 |v0228==4)
replace refrig_ch=. if v0228==9
label var refrig_ch "El hogar posee refrigerador o heladera"

 ***************
 ***freez_ch***
 *************** 
gen freez_ch=(v0229==1)
replace freez_ch=. if v0229==9
label var freez_ch "El hogar posee congelador"

 *************
 ***auto_ch***
 *************
gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"

 **************
 ***compu_ch***
 **************
capture gen compu_ch=(v0231==1)
replace compu_ch = . if v0231 == . | v0231 == 9 
label var compu_ch "El hogar posee computador"

 ***************
 ***internet_ch*
 *************** 
capture gen internet_ch=(v0232==2)
replace internet_ch = . if v0232 == . | v0232 == 9
label var internet_ch "El hogar posee conexión a Internet"

 *************
 ***cel_ch***
 ************* 
gen cel_ch=(v0220==2)
replace cel_ch = . if v0220 == 9 | v0220 == .
label var cel_ch "El hogar tiene servicio telefonico celular"

 ***************
 ***vivi1_ch***
 *************** 
gen viv1_ch=1 if v0202==2
replace viv1_ch=2 if v0202==4
replace viv1_ch=3 if v0202==6
replace viv1_ch = . if v0202 == 9 
label var viv1_ch "Tipo de vivienda en la que reside el hogar"
label def viv1_ch 1"Casa" 2"Departamento" 3"Otros"
label val viv1_ch viv1_ch

 ***************
 ***vivi2_ch***
 ***************
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
label var viv2_ch "La vivienda es casa o departamento"

 *****************
 ***viviprop_ch***
 *****************
gen viviprop_ch=0 if v0207==3
replace viviprop_ch=1 if v0207==1
replace viviprop_ch=2 if v0207==2
replace viviprop_ch=3 if v0207>=4
replace viviprop_ch=. if v0207==9 | v0207==.
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

 *****************
 ***vivitit_ch***
 *****************
gen vivitit_ch = .
label var vivitit_ch "El hogar posee un título de propiedad"

 *****************
 ***vivialq_ch***
 *****************
gen vivialq_ch=v0208
replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0
label var vivialq_ch "Alquiler mensual"

 *****************
 ***vivialqimp_ch*
 *****************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"


/************************************************************************/
/*				vARIABLES DEMOGRAFICAS			*/
/************************************************************************/

 ***************
 ***relacion_ci*
 ***************
gen relacion_ci=v0402
replace relacion_ci=5 if v0402==5|v0402==6|v0402==8
replace relacion_ci=6 if v0402==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

 *************
 ***sexo_ci***
 *************
gen sexo_ci=1 if v0302==2
replace sexo_ci=2 if v0302==4
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

 *************
 ***edad_ci***
 *************
gen edad_ci=v8005
replace edad_ci=. if edad_ci==999
label variable edad_ci "Edad del individuo"

 **************
 ***civil_ci***
 **************
gen civil_ci=.
capture replace civil_ci=1 if v1001==3 & v1003==3 /*EN ALGUNOS AÑOS NO ESTA EL MODULO DE NUPCIALIDAD!*/
capture replace civil_ci=2 if v1001==1
capture replace civil_ci=3 if v1004==2
capture replace civil_ci=4 if v1004==4
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

 ***************
 ***jefe_ci***
 ***************
gen jefe_ci=(v0402==1)
label variable jefe_ci "Jefe de hogar"
 
sort idh_ch

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
replace  clasehog_ch=1   if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0   /*Unipersonal*/
replace  clasehog_ch=2   if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0                      /*Nuclear (child with or without spouse but without other relatives)*/
replace  clasehog_ch=2   if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0    /*Nuclear (spouse with or without children but without other relatives)*/
replace  clasehog_ch=3   if notropari_ch>0 & notronopari_ch==0                                     /*Ampliado*/
replace  clasehog_ch=4   if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0)) /*Compuesto (some relatives plus non relative)*/
replace  clasehog_ch=5   if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0    /*Corresidente*/
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

******************
***nmiembros_ch***
******************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4) if miembros_ci==1
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



*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	
												
	****************
	***afroind_ci***
	****************
**Pregunta: COR OU RACA? (v0404) (BRANCA 2, PRETA 4, AMARELA 6, PARDA 8, INDIGENA 0, IGNORADA 9) 

gen afroind_ci=. 
replace afroind_ci=1  if v0404==0
replace afroind_ci=2 if v0404 == 4 | v0404 == 8 
replace afroind_ci=3 if v0404 == 2 | v0404 == 6 
replace afroind_ci=. if v0404==9


	****************
	***afroind_ch***
	****************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe 

	********************
	***afroind_ano_c***
	********************
gen afroind_ano_c = 1992


	************
	***dis_ci***
	************
gen dis_ci=. 

	************
	***dis_ch***
	************
gen dis_ch=. 

/******************************************************************************/
/*				VARIABLES DEL MERCADO LABORAL		      */
/******************************************************************************/


****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (v9001==1 | v9002==2 | v9003==1 | v9004==2)
replace condocup_ci=2 if  v9004==4 & (v9115==1 & (v9119>=1 & v9119<=8)) /*tomaron alguna providencia en la semana de referencia*/
replace condocup_ci=3 if  condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
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
replace cotizando_ci=1 if (v9059==1 | v9099==1 | v9103==1 | v9120==2) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (v9059==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (v9099==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

gen cotizaotros_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizaotros_ci=1 if (v9103==1 | v9120==2) & cotizando_ci==0 
label var cotizaotros_ci "Cotizante a la Seguridad Social por otro trabajos o por aporte privado"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


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
*sum v1252 v1255 v1258 v1261
* 2014, 01 revision MLO
foreach var of varlist v1252 v1255 v1258 v1261{ 
replace `var'=. if `var'>=999999 | `var'==-1
}
gen pension_ci=0 
replace pension_ci=1 if (v1252>0 & v1252!=.) | (v1255>0 & v1255!=.) | (v1258>0 & v1258!=.) | (v1261>0 & v1261!=.) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
***************
*pensionsub_ci*
***************
/*DZ Octubre 2017- Creacion de la variable  pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 300 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(v1273==300)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

* Nota: ypen_ci e ypensub_ci se encuentran con las variables de ingreso.

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if (v9067==1 | v9106==2) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*****************
*region /area ***
*****************
gen region=.	
replace region=1	if uf>=11 & uf<=17
replace region=2	if uf>=21 & uf<=29
replace region=3	if uf>=31 & uf<=35
replace region=4	if uf>=41 & uf<=43
replace region=5	if uf>=50 & uf<=53
label define region 1"norte" 2"Nordeste" 3"Sudeste/leste" 4"sul" 5"Centro_Oeste"
label value region region
label var region "distribución regional del país"

gen area=.
replace area=1 if zona_c==1
replace area=2 if zona_c==0
replace area=3 if v4727==1
label define area 1"urbana" 2"rural" 3"metropolitana" 
label value area area
label var area "area del pais"

*********
*lp_ci***
*********
gen lp_ci=.				
replace lp_ci=163.50822260757	if region==4	& area==1		/*sur-urbana*/
replace lp_ci=148.84886468114	if region==4	& area==2		/*sur-rural */
replace lp_ci=166.891151304824	if region==2	& area==1		/*noreste-urbana*/
replace lp_ci=148.84886468114	if region==2	& area==2		/*noreste-rural*/
replace lp_ci=130.806578071756	if region==3	& area==1		/*sudeste-urbano*/
replace lp_ci=111.636648525155	if region==3	& area==2		/*sudeste-rural*/
replace lp_ci=171.401722996494	if region==1	& area==1		/*norte-urbano*/
replace lp_ci=149.976507675556	if region==1	& area==2		/*norte-rural */
replace lp_ci=138.70007847498	if region==5	& area==1		/*centro oeste-urbano*/
replace lp_ci=121.785434759914	if region==5	& area==2		/*centro oeste-rural */
replace lp_ci=186.061080922923	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lp_ci=157.87000806448	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lp_ci=142.083007215133	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lp_ci=187.188723774343	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lp_ci=165.763508453405	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lp_ci=135.317149734827	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lp_ci=161.252936761735	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lp_ci=207.486296243862	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=171.401722996494	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lp_ci=147.721221829721	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci=193.954581311848	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=182.678152082673	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=145.465935983886	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=165.763508453405	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci=.				
replace lpe_ci=81.7541113037848	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=74.4244323405701	if region==4	& area==2		/*sur-rural */
replace lpe_ci=83.445575652412	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=74.4244323405701	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=65.403289035878	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=55.8183242625774	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=85.700861498247	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=74.9882538377779	if region==1	& area==2		/*norte-rural */
replace lpe_ci=69.3500392374901	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=60.8927173799571	if region==5	& area==2		/*centro oeste-rural */
replace lpe_ci=93.0305404614617	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lpe_ci=78.9350040322401	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lpe_ci=71.0415036075667	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lpe_ci=93.5943618871714	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lpe_ci=82.8817542267023	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lpe_ci=67.6585748674134	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lpe_ci=80.6264683808673	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lpe_ci=103.743148121931	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=85.700861498247	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci=73.8606109148604	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci=96.9772906559239	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=91.3390760413364	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=72.7329679919429	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=82.8817542267023	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lpe_ci "Linea de indigencia oficial del pais"

drop area 


*************
**salmm_ci***
*************
gen salmm_ci=300
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
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)
label var pea_ci "Población Económicamente Activa"

****************
***formal_ci ***
****************
gen formal_ci=(cotizando_ci==1)
label var formal_ci "1=afiliado o cotizante / PEA"

*****************
***desalent_ci***
*****************
gen desalent_ci=.
label var desalent_ci "Trabajadores desalentados"

**************
***ocupa_ci***
**************
gen ocupa_ci=.
replace ocupa_ci=1 if v4810==2 | v4810==3 & emp_ci==1
replace ocupa_ci=2 if v4810==1 & emp_ci==1
replace ocupa_ci=3 if v4810==4 & emp_ci==1
replace ocupa_ci=4 if v4810==6 & emp_ci==1
replace ocupa_ci=5 if v4810==5 & emp_ci==1
replace ocupa_ci=6 if v4810==7 & emp_ci==1
replace ocupa_ci=7 if v4810==8 & emp_ci==1
replace ocupa_ci=8 if v4810==9 & emp_ci==1 
replace ocupa_ci=9 if v4810==10 & emp_ci==1
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if v9907>1101 & v9907<5002
replace rama_ci=2 if v9907>=10000 & v9907<=14004 
replace rama_ci=3 if v9907>=15010 & v9907<=37000 
replace rama_ci=4 if v9907>=40010 & v9907<=41000 
replace rama_ci=5 if v9907>=45005 & v9907<=45999 
replace rama_ci=6 if v9907>=50010 & v9907<=55030
replace rama_ci=7 if v9907>=60010 & v9907<=64020
replace rama_ci=8 if v9907>=65000 & v9907<=70002
replace rama_ci=9 if v9907>=71010 & v9907<=99000
replace rama_ci=. if emp_ci==0
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

*****************
***horaspri_ci***
*****************

gen horaspri_ci=v9058
replace horaspri_ci=. if horaspri_ci==99 |horaspri_ci==-1 | v4714!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
gen horasprik_ci=horaspri_ci
capture replace horasprik_ci=v0713 if edad_ci>=5 & edad_ci<=9
replace horasprik_ci=. if edad_ci>=5 & edad_ci<=9 & (horasprik_ci==99 | horasprik_ci==-1| emp_ci==0)

*2014,01 revision MLO
replace horaspri_ci=. if horaspri_ci<0 | horaspri_ci>150
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
*gen horastot_ci=.
*2014, 01 incorporacio MLO
replace v9058 = . if v9058 == -1 | v9058 == 99
replace v9101 = . if v9101 == -1 | v9101 == 99
replace v9105 = . if v9105 == -1 | v9105 == 99

egen horastot_ci = rsum(v9058 v9101 v9105) 
replace horastot_ci = . if  (horaspri_ci==. & v9101==. & v9105==.) | v4714!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
replace horastot_ci = . if horastot_ci < 0
replace horastot_ci = . if horastot_ci > 150
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	
replace v1091=. if v1091==99 | v1091==-1
replace v1092=. if v1092==99 | v1092==-1


*Yanira Oviedo, Junio 2010: Se estaba multiplicando por 12, pero al ser un valor anual, debería dividirse 
/*gen aux1=v1091/12
egen durades_ci=rsum(aux1 v1092) if  v4714!=1 & edad_ci>=10
replace durades_ci=. if (v1091==. & v1092==.) */

****************
***durades_ci***
****************
gen durades_ci=.
label variable durades_ci "Duracion del desempleo en meses"
*MLO 03,2014


*******************
***antiguedad_ci***
*******************
replace v9611=. if v9611==99 | v9611==-1
replace v9612=. if v9612==99 | v9612==-1
gen aux2=v9612/12
egen antiguedad_ci=rsum(v9611 aux2) if emp_ci==1
replace antiguedad_ci=. if v9611==. & v9612==. 
drop aux*
label var antiguedad_ci "Antiguedad en la actividad actual en anios"


***************
***subemp_ci***
***************
gen subemp_ci=.
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
gen categopri_ci=1 if v9029==4 | (v9008>=8 & v9008<=10)
replace categopri_ci=1 if v0708==4 | v0711==4
replace categopri_ci=2 if v0708==3 | v9029==3 |v0711==3 |(v9008>=5 & v9008<=7)
replace categopri_ci=3 if v0708==1 | v0708==2 |v9029==1 |v9029==2 |v0711==1 |v0711==2 | (v9008>=1 & v9008<=4)
replace categopri_ci=4 if (v0708>=5 & v0708<=8) |(v9029>=5 & v9029<=8) |(v0711>=5 & v0711<=8) | (v9008>=11 & v9008<=13)
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional en la ocupación principal'"

******************
***categosec_ci***
******************
gen categosec_ci=1 if v9092==4
replace categosec_ci=2 if v9092==3
replace categosec_ci=3 if v9092==1
replace categosec_ci=4 if v9092==5 |v9092==6
replace categosec_ci=. if emp_ci!=1 
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categosec_ci 3"Empleado" 4" No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional en la ocupación secundaria"

*****************
***nempleos_ci***
*****************
gen nempleos_ci=1 if v9005==1
replace nempleos_ci=2 if v9005>1 & v9005!=.
label var nempleos_ci "Número de empleos" 
label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
label value nempleos_ci nempleos_ci

*****************
***spublico_ci***
*****************
gen spublico_ci=(v9032==4)
replace spublico_ci=. if v9032==9
label var spublico_ci "Personas que trabajan en el sector público"

*******************
***tamemp_ci*******
*******************
gen tamemp_ci=1 if v9019==1 | v9019==3 | v9019==5 |v9017==1 | v9017==3 | v9017==5 | v9040==2 | v9040==4 | v9048==2 | v9048==4 | v9048==6 
replace tamemp_ci=2 if v9019==7 | v9017==7 | v9040==6 | v9048==8
replace tamemp_ci=3 if v9019==8 | v9017==8 | v9040==8 | v9048==0

* rev MLO, 2015, 03
* se incorporan cuenta propia y trabajadores agricolas
recode tamemp_ci . =1 if v9049==3
replace tamemp_ci=1 if v9014==2 |  v9014==4 |  v9014==6
replace tamemp_ci=1 if v9049==3 | v9050==6 | v9050==4 | v9050==2 | v9052==2 | v9052==4 | v9052==6
replace tamemp_ci=2 if v9014==8 | v9052==8
replace tamemp_ci=3 if v9014==0 | v9050==8 | v9052==0 
label var  tamemp_ci "Tamaño de Empresa" 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

******************
***categoinac_ci**
******************
gen categoinac_ci=1 if (v9122==2 | v9123==1) & condocup_ci==3
replace categoinac_ci=2 if v0602==2 & condocup_ci==3
replace categoinac_ci=3 if v9121==1 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo


******************
***tipopen_ci**
******************
gen tipopen_ci = . 
label var  tipopen_ci "Tipo de pension"


************************
******* Ingreso ********
************************

/*EN TODOS LOS AÑOS, EXCEPTO EN EL '96, '97 Y '01 SE DESCRIBEN LAS CONDICIONES LABORALES DE LOS NIÑOS DE ENTRE 5 Y 9 AÑOS.
ESTO QUIERE DECIR QUE HAY QUE TENER EN CUENTA QUE EN ESOS AÑOS LOS INGRESOS DE ESTOS NIÑOS vAN A vALER 0, CUANDO EN OTROS 
AÑOS TIENEN UN vALOR POSITIvO. PARA MANTENER LA COMPARABILIDAD ACROSS TIME, SOLO SE DEBEN AGARRAR LOS INGRESOS DE LOS 
MAYORES DE 10 AÑOS, A MENOS QUE SE vAYAN A EXCLUIR LOS AÑOS 1996 Y 1997! ==>> CREAMOS DOS vARIABLES DE INGRESO, UNA QUE LOS 
TIENE EN CUENTA (ej: ylmprik_ci) Y OTRA QUE NO (ylmpri_ci original)*/ 


/*TODAS LAS vARIABLES "SECUNDARIAS": ylmsec_ci, ylnmsec_ci, ylmotros_ci, ylnmotros_ci Y durades_ci ESTAN CREADAS SÓLO PARA 
LOS MAYORES DE 10 AÑOS. POR LO TANTO LAS vARIABLES AGREGADAS CON SUFIJO k EN REALIDAD SÓLO SE REFIEREN A LA ACTIvIDAD 
PRINCIPAL DE LOS NIÑOS*/

 ***************
 ***ylmpri_ci***
 ***************
gen ylmpri_ci=v9532 
replace ylmpri_ci=. if v9532==-1 | v9532>=999999 | v4714!=1 

gen ylmprik_ci=v9532
replace ylmprik_ci=. if v9532==-1 | v9532>=999999 | emp_ci==0 
capture replace ylmprik_ci=v7122 if edad_ci>=5 & edad_ci<=9
capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (v7122==-1 | v7122>=999999 |emp_ci==0)
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


 ***************
 ***ylnmpri_ci**
 ***************
gen ylnmpri_ci=v9535 if edad_ci>=10
replace ylnmpri_ci=. if v9535==-1 | v9535>=999999 | v4714!=1

gen ylnmprik_ci=v9535
replace ylnmprik_ci=. if v9535==-1 | v9535>=999999 | emp_ci==0
capture replace ylnmprik_ci=v7125 if edad_ci>=5 & edad_ci<=9
capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (v7125==-1 | v7125>=999999 | emp_ci==0)
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"  

****************
***ylmsec_ci *** 
****************
gen ylmsec_ci=v9982 if edad_ci>=10
replace ylmsec_ci=. if v9982==-1 | v9982>=999999 | v4714!=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
* ylnmsec_ci   * 
**************** 
gen ylnmsec_ci=v9985 if edad_ci>=10
replace ylnmsec_ci=. if v9985==-1 | v9985>=999999 | v4714!=1
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

****************
* ylmotros_ci * 
****************
gen ylmotros_ci=v1022 if edad_ci>=10
replace ylmotros_ci=. if v1022==-1 | v1022>=999999 | v4714!=1
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

****************
* ylnmotros_ci * 
**************** 
gen ylnmotros_ci=v1025 if edad_ci>=10
replace ylnmotros_ci=. if v1025==-1 | v1025>=999999 | v4714!=1
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

*****************
** nrylmpri_ci ** 
*****************
gen nrylmpri_ci=(ylmpri_ci==. & v4714==1)
replace nrylmpri_ci=. if v4714==2

gen nrylmprik_ci=(ylmprik_ci==. & emp_ci==1)
replace nrylmprik_ci=. if emp_ci==0

**************
*** ylm_ci *** 
**************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.

egen ylmk_ci=rsum(ylmprik_ci ylmsec_ci ylmotros_ci)
replace ylmk_ci=. if ylmprik_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

***************
*** ylnm_ci *** 
*************** 
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==. & ylnmotros_ci==.

egen ylnmk_ci=rsum(ylnmprik_ci ylnmsec_ci ylnmotros_ci)
replace ylnmk_ci=. if ylnmprik_ci==. & ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

**************
** ynlm_ci ** 
**************
foreach var of varlist v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273{ 
replace `var'=. if `var'>=999999 | `var'==-1
}

egen ynlm_ci=rsum(v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273) if edad_ci>=10
replace ynlm_ci=. if (v1252==. &  v1255==. &  v1258==. &  v1261==. &  v1264==. &  v1267==. & v1270==. & v1273==.) | ynlm_ci<0
label var ynlm_ci "Ingreso no laboral monetario"  

**************
** ynlnm_ci ** 
**************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 

****************
* nrylmpri_ch  * 
**************** 
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen nrylmprik_ch=max(nrylmprik_ci) if miembros_ci==1

**************
*** ylm_ch *** 
**************
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1
by idh_ch: egen ylmk_ch=sum(ylmk_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch *** 
***************
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1
by idh_ch: egen ylnmk_ch=sum(ylnmk_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

****************
*** ylmnr_ch *** 
****************
gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1
gen ylmnrk_ch=ylmk_ch
replace ylmnrk_ch=. if nrylmprik_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"


****************
*** ynlm_ch *** 
****************
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del hogar"

****************
*** ynlnm_ch *** 
****************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

****************
* ylmhopri_ci  * 
****************
*2015, 03 modificacion MLO
*gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
gen ylmhoprik_ci=ylmprik_ci/(horasprik_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
replace ylmhoprik_ci=. if ylmhoprik_ci<=0
label var ylmhopri_ci "Salario monetario de la actividad principal" 

*************
* ylmho_ci  * 
*************
gen ylmho_ci = .
label var ylmho_ci "Salario monetario de todas las actividades" 

********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"


****************
* remesas_ci  * 
****************
gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

sort idh_ch

****************
* remesas_ch  * 
****************
gen remesas_ch=.
label var remesas_ch "Remesas mensuales del hogar" 


*************
*ypen_ci*
*************
*sum v1252 v1255 v1258 v1261
egen ypen_ci=rsum (v1252 v1255 v1258 v1261)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "valor de la pension contributiva"

*****************
**  ypensub_ci  *
*****************
/*DZ Octubre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 300 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=v1273 if v1273==300
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*****************
**  tcylmpri_ci *
*****************
gen tcylmpri_ci=.
lab var tcylmpri_ci "Id de top-code del ingreso de la actividad principal"

*****************
**  tcylmpri_ch *
*****************
gen tcylmpri_ch=.
lab var tcylmpri_ch "Id de top-code del ingreso de la actividad principal"


					****************************
					***	VARIABLES EDUCATIVAS ***
					****************************
					
/* 
Notas construcción aedu_ci: 

En todos los casos Alfabetização de jóvens e adultos, Creche, 
Classe de alfabetização - CA, Maternal, jardim de infáncia, etc... imputan 0 
años de educación.

En todos los casos, para aquellos que asisten se le resta 1 al grado declarado 
para el computo de aedu_ci ya que el asistente no completó dicho año. 

Hay dos sistemas:  Esino fundamental ou 1er  grau, abarca 8 años de 
duración y siguen 4 de Esino fundamental ou 2do grau. El otro, se computan 4 años 
obligatorios de Elementar, 4 años de Medio 1er ciclo y se completa con 4 años 
de Medio 2do ciclo (científico, classico, etc..)

Para los que declaran cursos no seriados, en el caso de los asistentes, al 
no contar con información sobre los años de escolaridad aprobados (ya que 
declaran nivel pero no grado) se imputa por metodología el número máximo de años
del nivel anterior. Para los no asistentes, el procedimiento es análogo salvo 
para aquellos en los que pueda discriminarse la finalización del curso en cuyo 
caso se asignan los años que correspondan.

La Educación Pre-Vestibular hace referencia a cursos de nivelación cortos 
(menores a un año) que son requisito de admisión para las universidades 
o servicio público. En esos casos, se imputan los 12 años de educación 
por secundario completo.

En la encuesta, para el nivel superior (Maestría o Doctorado) no se pregunta el
grado al que asisten, por lo que se imputan los años requeridos para 
acceder a ese nivel. En caso de haber finalizado dicha instancia, se imputan 
como aprobados el promedio de duración entre maestría y doctorado al no poder 
discriminar que individuo pertenece a cada nivel.
*/

* Valores sem declaração van a perdidos:
replace v6002 = . if v6002 == 9
replace v0605 = . if v0605 == 9
replace v0606 = . if v0606 == 9
replace v0610 = . if v0610 == 9
replace v0611 = . if v0611 == 9 

**************
**asiste_ci***
**************
gen asiste_ci = (v0602 == 2)
label var asiste_ci "Personas que actualmente asisten a un centro de enseñanza"

***************
***edupub_ci***
***************
gen edupub_ci = (v6002 == 2)
replace edupub_ci=. if v6002 == .
label var  edupub_ci "Personas que asisten a centros de enseñanza públicos"


*************
***aedu_ci***
*************
gen nivel_asiste = v0603
gen grado_asiste = v0605
gen nivel_no_asiste = v0607
gen grado_no_asiste = v0610
gen finalizo = v0611

gen aedu_ci =. 
label var aedu_ci "Anios de educacion"

* PARA LOS QUE ASISTEN:
***********************
* Alfabetização de adultos, Creche, Pré-escolar
replace aedu_ci = 0 if inlist(nivel_asiste, 6, 7, 8) 

* Esino fundamental 1 y 2 
replace aedu_ci = grado_asiste - 1 if inlist(nivel_asiste, 1, 3) // Ensino fundamental ou 1º grau
replace aedu_ci = grado_asiste + 8 - 1  if inlist(nivel_asiste, 2, 4) // Ensino fundamental ou 2º grau

* Superior
replace aedu_ci = grado_asiste + 12 - 1 if nivel_asiste == 5 // Universitario

* Imputación para los que declaran nivel pero no grado (incluye no seriados)
replace aedu_ci = 0 if (inlist(nivel_asiste, 1, 3) & grado_asiste == .) // Ensino fundamental ou 1º grau
replace aedu_ci = 8 if (inlist(nivel_asiste, 2, 4) & grado_asiste == .) // Ensino fundamental ou 2º grau
replace aedu_ci = 12 if nivel_asiste == 9 // Pre-Vestibular 
replace aedu_ci = 12 if (nivel_asiste == 5 & grado_asiste == .) // Universitario
replace aedu_ci = 12 + 4 if nivel_asiste == 10 // Maestría o Doctorado


* PARA LOS QUE NO ASISTEN:
**************************

* Alfabetização de adultos, Creche, Pré-escolar
replace aedu_ci = 0 if inlist(nivel_no_asiste, 8, 9, 10)

* Esino fundamental 

* Sistema antiguo 
replace aedu_ci = grado_no_asiste if nivel_no_asiste == 1 // Elementar (primario) - 4 anios
replace aedu_ci = grado_no_asiste + 4 if nivel_no_asiste == 2 // Medio 1er ciclo (ginasal) - 4 anios
replace aedu_ci = grado_no_asiste + 8 if nivel_no_asiste == 3 // Medio 2do ciclo (científico, clasico etc.)

* Sistema actual
replace aedu_ci = grado_no_asiste if nivel_no_asiste == 4 // Ensino fundamental ou 1º grau
replace aedu_ci = grado_no_asiste + 8 if nivel_no_asiste == 5 // Ensino fundamental ou 2º grau

* Superior
replace aedu_ci = grado_no_asiste + 12 if nivel_no_asiste == 6 // Universitario

* Imputación para los que declaran nivel pero no grado

* No finalizado
replace aedu_ci = 0 if (inlist(nivel_no_asiste, 1, 4)  & grado_no_asiste == . & inlist(finalizo, 3, .)) // Elementar, Esino fundamental ou 1º grau
replace aedu_ci = 4 if (nivel_no_asiste == 2 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Medio 1 
replace aedu_ci = 8 if (nivel_no_asiste == 3 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Medio 2 
replace aedu_ci = 8 if (nivel_no_asiste == 5 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Ensino fundamental ou 2º grau
replace aedu_ci = 12 if (nivel_no_asiste == 6 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Universitario
replace aedu_ci = 12 + 4 if (nivel_no_asiste == 7 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Maestría o Doctorado

* Finalizado 
replace aedu_ci = 4 if (nivel_no_asiste == 1  & grado_no_asiste == . & finalizo == 1) // Elementar
replace aedu_ci = 8 if (nivel_no_asiste == 2 & grado_no_asiste == . & finalizo == 1) // Medio 1 
replace aedu_ci = 8 if (nivel_no_asiste == 4 & grado_no_asiste == . & finalizo == 1) // Esino fundamental ou 1º grau
replace aedu_ci = 12 if (nivel_no_asiste == 3 & grado_no_asiste == . & finalizo == 1) // Medio 2 
replace aedu_ci = 12 if (nivel_no_asiste == 5 & grado_no_asiste == . & finalizo == 1) // Ensino fundamental ou 2º grau
replace aedu_ci = 12 + 4 if (nivel_no_asiste == 6 & grado_no_asiste == . & finalizo == 1) // Universitario
replace aedu_ci = 12 + 4 + 2 if (nivel_no_asiste == 7 & grado_no_asiste == . & finalizo == 1) // Maestría o Doctorado

**************
***eduno_ci***
**************
gen byte eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == . 
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci = (aedu_ci > 0 & aedu_ci < 5)
replace edupi_ci = . if aedu_ci == .
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci = (aedu_ci == 5)
replace edupc_ci = . if aedu_ci == .
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci = (aedu_ci > 5 & aedu_ci < 12) 
replace edusi_ci = . if aedu_ci == .
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen byte edusc_ci = (aedu_ci == 12) 
replace edusc_ci = . if aedu_ci == .
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
* Entre 13 y 14 anios o 15 que no declaran nivel finalizado.
gen byte eduui_ci = (aedu_ci >= 13 & aedu_ci <= 14) | (aedu_ci == 15 & finalizo != 1) 
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**************
***eduuc_ci***
**************
/* Aquellos con 15 anios que completaron nivel 
o cualqueira con mas de 15 anios de educ.*/
gen byte eduuc_ci = (aedu_ci == 15 & finalizo == 1 | aedu_ci > 15) 
replace eduuc_ci = . if aedu_ci == .
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
gen edus1i_ci = (aedu_ci > 5 & aedu_ci < 9)
replace edus1i_ci = . if aedu_ci == .
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci = (aedu_ci == 9)
replace edus1c_ci = . if aedu_ci == .
label variable edus1c_ci "1er ciclo de la secundaria completo" 

***************
***edus2i_ci***
***************
gen byte edus2i_ci = (aedu_ci > 9 & aedu_ci < 12)
replace edus2i_ci = . if aedu_ci == .
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

***************
***edus2c_ci***
***************
gen edus2c_ci = (aedu_ci == 12)
replace edus2c_ci = . if aedu_ci == .
label variable edus2c_ci "2do ciclo de la secundaria completo" 

***************
***edupre_ci***
***************
* No se declara la finalización en ese nivel.
gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci**
***************
g asispre_ci = (v0603 == 8) 
la var asispre_ci "Asiste a educacion prescolar"	
	
**************
***eduac_ci***
**************
* No puede discriminarse educación superior no unviersitaria.
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


******************
***pqnoasis_ci***
******************
gen pqnoasis_ci = .
label var pqnoasis_ci "Razones para no asistir a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
gen pqnoasis1_ci = .

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************
***repiteult_ci***
******************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el último año o grado"

drop nivel_asiste grado_asiste grado_no_asiste nivel_no_asiste finalizo

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(v5030==98) 
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & (v0507==1 | (v5080!=. & v5080!=98) | (migrante_ci==1 & v5065==6) | (migrante_ci==1 & v5063==4))) if migrante_ci!=. & !inrange(edad_ci,0,4)	
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=(migrante_ci==1 & (v0507==1 | (v5080!=. & v5080!=98) | (migrante_ci==1 & v5065==6) | (migrante_ci==1 & v5063==4))) if migrante_ci!=. & !inrange(edad_ci,0,4)	
	replace migrantiguo5_ci = 0 if migantiguo5_ci != 1 & migrante_ci==1
	replace migrantiguo5_ci = . if migrante_ci==0
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"


*******************
*** Brazil 2005 ***
*******************

** variables
clonevar	factor=v4729
clonevar	nrocont=v0102
clonevar	nroserie=v0103
clonevar	persa=v4724
clonevar	persb=v4725
clonevar	sexo=v0302
clonevar	piel=v0404
clonevar	edad=v8005
clonevar	alfabet=v0601
clonevar	asiste=v0602
clonevar	cursoasi=v0603
clonevar	serieasi=v0605
clonevar	ultcurso=v0607
clonevar	ultserie=v0610
clonevar	terult=v0611
clonevar	situacen=v4105
clonevar	tiposector=v4106
clonevar	areacen=v4107
clonevar	estrato=v4602
clonevar	nromuni=v4604
clonevar	cond10ym=v4704
clonevar	cat10ym=v4706
clonevar	ramar=v4809
clonevar	yth=v4726
clonevar	espdom=v0201
clonevar	matpared=v0203
clonevar	matecho=v0204
clonevar	nrocuart=v0205
clonevar	nrodorm=v0206
clonevar	tenenviv=v0207
clonevar	tenenter=v0210
clonevar	aguacan=v0211
clonevar	abasagua=v0212
clonevar	aguared=v0213
clonevar	aguapozo=v0214
clonevar	sanita=v0215
clonevar	usosani=v0216
clonevar	sissan=v0217
clonevar	celular=v0220
clonevar	telefono=v2020
clonevar	combcoci=v0223
clonevar	computad=v0231
clonevar	internet=v0232
clonevar	sectorem=v9032
clonevar	tamest1=v9040
clonevar	tamest2=v9048
clonevar	catsec=v9092
clonevar	sectosec=v9093
clonevar	qqhh=v9121
clonevar	jubila=v9122
clonevar	pensio=v9123
clonevar	ocusec=v9990
*clonevar	ocup=v9906
clonevar	ramsec=v9991
clonevar	totpers=v0105
 gen condocup=v4705 if edad>=10 

 recode sexo (2=1) (4=2)

** AREA

 generate area=.
 replace area=2 if v4105>=4 & v4105<=8
 replace area=1 if v4105>=1 & v4105<=3
 label values area area
 label define area 1 Urban 2 Rural

 tab area [w=factor]

************************************************************************************

* For this year(2004) the rural sample of the states of Rondônia, Acre, Amazonas
* Roraima, Pará and Amapá is excluded in order to maintain the comparability of the 
* results with previous years.

 count
 drop if area==2 & (uf==11 | uf==12 | uf==13 | uf==14 | uf==15 | uf==16)
 count

************************************************************************************

 tab area [w=factor]

** Gender classification of the population refering to the head of the household.

 sort nrocont nroserie v0301

* Household ID

 gen x=1 if v0402==1 	/* Condição na família */
 gen id_hogar=sum(x)
 drop x

* Dwelling ID

 gen x=1 if v0401==1	/* Condição na unidade domiciliar */
 gen id_viv=sum(x)
 drop x

 gen	 sexo_d_=1 if v0402==1 & sexo==1
 replace sexo_d_=2 if v0402==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 tab sexo [w=factor]
 tab sexo_d [w=factor]

 tab sexo sexo_d if v0402==1

** Years of education. 

 gen anoest=.
 replace anoest=0  if v4703==1 
  *replace anoest=0 if (ultcurso==1 & terult==3) | (ultcurso==4 & terult==3) | ultcurso==9 | ultcurso==10 | (cursoasi==1 & serieasi==1) | (cursoasi==3 & serieasi==1) | cursoasi==6 | cursoasi==7 | cursoasi==8 | (asiste==4 & ultcurso==.)| (ultcurso==8 & terult==3)
 replace anoest=1  if (ultcurso==1 & ultserie==1) | (ultcurso==4 & ultserie==1) | (cursoasi==1 & serieasi==2) | (cursoasi==3 & serieasi==2)
 replace anoest=2  if (ultcurso==1 & ultserie==2) | (ultcurso==4 & ultserie==2) | (cursoasi==1 & serieasi==3) | (cursoasi==3 & serieasi==3)
 replace anoest=3  if (ultcurso==1 & ultserie==3) | (ultcurso==4 & ultserie==3) | (cursoasi==1 & serieasi==4) | (cursoasi==3 & serieasi==4)
 replace anoest=4  if v4703==5
  *replace anoest=4 if (ultcurso==1 & ultserie==4) | (ultcurso==4 & ultserie==4) | (ultcurso==2 & ultserie==0 & terult~=1) | (cursoasi==1 & serieasi==5) | (cursoasi==3 & serieasi==5)
 replace anoest=5  if (ultcurso==1 & ultserie==5) | (ultcurso==4 & ultserie==5) | (ultcurso==2 & ultserie==1) | (cursoasi==1 & serieasi==6) | (cursoasi==3 & serieasi==6)
 replace anoest=6  if (ultcurso==1 & ultserie==6) | (ultcurso==4 & ultserie==6) | (ultcurso==2 & ultserie==2) | (cursoasi==1 & serieasi==7) | (cursoasi==3 & serieasi==7)
 replace anoest=7  if (ultcurso==2 & ultserie==3) | (ultcurso==4 & ultserie==7) | (cursoasi==1 & serieasi==8) | (cursoasi==3 & serieasi==8)
 replace anoest=8  if v4703==9
  *replace anoest=8 if (ultcurso==2 & ultserie==4) | (ultcurso==4 & ultserie==8) | (ultcurso==3 & ultserie==0 & terult~=1) | (ultcurso==5 & ultserie==0 & terult~=1) | ((cursoasi==2 | cursoasi==4) & serieasi==1) | (cursoasi==4 & (serieasi==0 | serieasi==1))
 replace anoest=9  if (ultcurso==2 & ultserie==5) | (ultcurso==3 & ultserie==1) | (ultcurso==5 & ultserie==1) | ((cursoasi==2 | cursoasi==4) & serieasi==2) | (cursoasi==4 & serieasi==2)
 replace anoest=10 if (ultcurso==3 & ultserie==2) | (ultcurso==5 & ultserie==2) | ((cursoasi==2 | cursoasi==4) & serieasi==3) | (cursoasi==4 & serieasi==3)
 replace anoest=11 if v4703==12
 replace anoest=12 if v4703==13
  *replace anoest=11 if (ultcurso==3 & ultserie==3) | (ultcurso==6 & ultserie==0 & terult~=1) | (ultcurso==5 & ultserie==3) | ((cursoasi==2 | cursoasi==4) & serieasi==4) | (cursoasi==5 & (serieasi==0 | serieasi==1)) | cursoasi==9
  *replace anoest=12 if (ultcurso==3 & ultserie==4) | (ultcurso==6 & ultserie==1) | (cursoasi==5 & serieasi==2) 
 replace anoest=13 if (ultcurso==6 & ultserie==2) | (cursoasi==5 & serieasi==3)
 replace anoest=14 if (ultcurso==6 & ultserie==3) | (cursoasi==5 & serieasi==4)
 replace anoest=15 if (ultcurso==6 & ultserie==4) | (cursoasi==5 & serieasi==5)
 replace anoest=16 if (ultcurso==6 & ultserie==5) | (ultcurso==7 & terult==3) | (cursoasi==5 & serieasi==6) | cursoasi==10
 replace anoest=17 if (ultcurso==6 & ultserie==6) | (ultcurso==7 & terult==1)
 replace anoest=99 if v4703==17
  *replace anoest=99 if ultcurso==0 | ((cursoasi==2 | cursoasi==4) & (serieasi==9 | serieasi==0)) | (cursoasi==3 & (serieasi==9 | serieasi==0)) | (cursoasi==1 & (serieasi==9 | serieasi==0))

 tab anoest
 tab v4703
 tab v4703 anoest,missing

 tab anoest [w=factor]
 tab v4703 [w=factor]


 gen	 condact=1 if condocup==1 & cond10ym==1
 replace condact=2 if condocup==2 & cond10ym==1
 replace condact=3 if cond10ym==2
 replace condact=4 if cond10ym==3

 gen	 peaa=0
 replace peaa=1 if condact==1
 replace peaa=2 if condact==2
 replace peaa=3 if condact==3

 gen	 tasadeso=0 if peaa==1
 replace tasadeso=1 if peaa==2
 
 gen	 categ=1 if cat10ym>=1 & cat10ym<=5
 replace categ=2 if cat10ym>=6 & cat10ym<=8
 replace categ=3 if cat10ym==9
 replace categ=4 if cat10ym==10
 replace categ=5 if cat10ym==11 | cat10ym==12
 replace categ=6 if cat10ym==13


************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

 gen     NERP=0 if (edad>=7 & edad<=10) & (asiste==2 | asiste==4)
 replace NERP=1 if (edad>=7 & edad<=10) & (asiste==2) & ((cursoasi==1|cursoasi==3) & (serieasi>=1 & serieasi<=4))


** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen     NERS=0 if (edad>=11 & edad<=17) & (asiste==2 | asiste==4)
 replace NERS=1 if (edad>=11 & edad<=17) & (asiste==2) & ( ((cursoasi==1|cursoasi==3) & (serieasi>=5 & serieasi<=8)) | ((cursoasi==2 | cursoasi==4) & (serieasi>=1 & serieasi<=3)) )

* Upper secondary
* Ensino medio ou 2º grau

 gen     NERS2=0 if (edad>=15 & edad<=17) & (asiste==2 | asiste==4) 
 replace NERS2=1 if (edad>=15 & edad<=17) & (asiste==2) & ((cursoasi==2 | cursoasi==4) & (serieasi>=1 & serieasi<=3))

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     LIT=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace LIT=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen     LIT2=0 if  (edad>=15 & edad<=24) & (alfabet==1 | alfabet==3)
 replace LIT2=1 if  (edad>=15 & edad<=24) & (alfabet==1) 

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if asiste==2 & (cursoasi==1 | cursoasi==3) & (serieasi>=1 & serieasi<=4)
 gen sec=1 if  asiste==2 & (((cursoasi==1|cursoasi==3) & (serieasi>=5 & serieasi<=8)) | ((cursoasi==2 | cursoasi==4) & (serieasi>=1 & serieasi<=3)))
 gen ter=1 if  asiste==2 & ((cursoasi==5 & serieasi<=5) | ((cursoasi==2 | cursoasi==4) & (serieasi==4)))

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
 
 gen RATIOALL=0 if     (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    
 
 
** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen RATIOLIT2=0     if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen RATIOLIT=0 if     ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 


** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* Without Domestic Service

 gen     WENAS=0 if (edad>=15 & edad<=64) & (categ==1) & (ramar>=2 & ramar<=13)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==1) & (ramar>=2 & ramar<=13) & sexo==2

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
*Gender-With domestic servants*

 gen     WENASD=0 if (edad>=15 & edad<=64) & (categ==1 | categ==2) & (ramar>=2 & ramar<=13)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==1 | categ==2) & (ramar>=2 & ramar<=13) & sexo==2

*** GOAL 7 ENSURE ENvIROMENTAL SUSTAINABILITY

* Access to Electricity ** Additional Indicator

* Gender classification of the population refers to the head of the household.

 gen     ELEC=0 if espdom==1 & (v0219>=1 & v0219<=5)  /* Total population excluding missing information */
 replace ELEC=1 if espdom==1 & (v0219==1)


 gen     SFUELS=0 if espdom==1 & (combcoci>=1 & combcoci<=6)   /* Total population excluding missing information */
 replace SFUELS=1 if espdom==1 & (combcoci==3 | combcoci==4)

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

* Gender classification of the population refers to the head of the household.

 gen     WATER=0 if espdom==1 & (aguacan==1 | aguacan==3 | aguacan==9)   /* Total population excluding missing information */
 replace WATER=1 if espdom==1 & ((aguacan==1 & (abasagua==2 | abasagua==4)) | (aguacan==3 & (aguared==1 |aguapozo==2)))


** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural*
* Gender classification of the population refers to the head of the household.

 gen     SANITATION=0 if espdom==1 & (sanita==1 | sanita==3 | sanita==9)    /* Total population excluding missing information */
 replace SANITATION=1 if espdom==1 & (sanita==1 & (sissan>=1 & sissan<=3))

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
* PERSONS PER ROOM

 gen a=(totpers/nrocuart) if ((totpers>0 & totpers<99) | (nrocuart>0 & nrocuart<99)) & v0401==1

 egen persroom=max(a), by(id_viv) /* Per dwelling */

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if (tenenviv>=1 & tenenviv<=6)   /* Total population excluding missing information */
 replace secten_1=1 if (tenenviv>=4 & tenenviv<=6)

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if (matpared>=1 & matpared<=6)   /* Total population excluding missing information */ 
 replace secten_2=1 if (matpared>=3 & matpared<=6)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  espdom==1 & (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1)   /* Total population excluding missing information */
 replace SECTEN=0 if  espdom==1 & (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator

* NA
compress

** GOAL 8. DEvELOP A GLOBAL PARTNERSHIP FOR DEvELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen     UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (tasadeso==1) 

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if espdom==1 & (telefono==2 | telefono==4) & (celular==2 | celular==4)   /* Total population excluding missing information */
 replace TELCEL=1 if espdom==1 & (telefono==2 | celular==2)
	
** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if espdom==1 & (telefono==2 | telefono==4)   /* Total population excluding missing information */
 replace TEL=1 if espdom==1 & (telefono==2)


** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if espdom==1 & (celular==2 | celular==4)   /* Total population excluding missing information */
 replace CEL=1 if espdom==1 & (celular==2)

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if espdom==1 & (computad==1 | computad==3)   /* Total population excluding missing information */
 replace COMPUTER=1 if espdom==1 & (computad==1)

* Target 18, Indicator: "Internet users per 100 population"

* Gender classification of the population refers to the head of the household.

 gen     INTUSERS=0 if espdom==1 & (computad==1 | computad==3)   /* Total population excluding missing information */
 replace INTUSERS=1 if espdom==1 & (internet==2)

*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

** CCA 19. Proportion of children under 15 who are working

 gen     CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & peaa==1

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if espdom==1 & v0401==1  /* Per dwelling */

 gen popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if espdom==1 & persroom<.   /* Total population excluding missing information */
 replace PLT2=1 if espdom==1 & (popinlessthan2==1)

** Disconnected Youths

 gen     DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & peaa==3 & qqhh==3 & asiste==4

*** Proportion of population below corresponding grade for age

 gen rezago=0		if (anoest>=0 & anoest<99)  & edad==7 /* This year of age is not included in the calculations */
	 
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
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=4  & anoest<99)

* Average years of education of the population 15+

 gen     AEDUC_15=anoest if ((edad>=15 & edad<.) & (anoest>=0 & anoest<99))
 global variable AEDUC_15

 gen     AEDUC_15_24=anoest if ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if ((edad>=25 & edad<.) & (anoest>=0 & anoest<99))

* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=17) & (anoest>=0 & anoest<99)

* Grade for age primary [ISCED 1]

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=10) & (anoest>=0 & anoest<99)

* Grade for age Secondary [ISCED 2 & 3]

 gen GFAS=(anoest/(edad-7)) if (edad>=11 & edad<=17) & (anoest>=0 & anoest<99)



/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci migrantiguo5_ci miglac_ci , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename v9906 codocupa
rename v9907 codindustria

compress

saveold "`base_out'", version(12) replace


log close



