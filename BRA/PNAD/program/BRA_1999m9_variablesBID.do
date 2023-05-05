* (Versión Stata 13)
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

local PAIS BRA
local ENCUESTA PNAD
local ANO "1999"
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
Versión 2010: Yanira Oviedo
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Última modificación: Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Octubre de 2017

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

*solo hogares permanentes
keep if espdom==1

*****************
*** region_ci ***
*****************
*YL: generacion "region_c" proyecto maps America.	
gen region_c = unifede
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
egen idh_ch=group(unifede nrocont nroserie nrofam)
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************
sort idh_ch
by idh_ch: gen idp_ci=_n

***************
***factor_ci***
***************
gen factor_ci =  pesoper 

***************
***factor_ch***
***************
gen factor_ch=pesodom

**********
***zona***
**********
gen zona_c=1 if situacen>=1 & situacen<=3
replace zona_c=0 if situacen>=4 & situacen<=8
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
gen anio_c=1998
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=9

************
***upm_ci***
************
gen upm_ci = upa

***************
***estrato_ci**
***************
gen estrato_ci = estrato


/********************************/
/*    vARIABLES DEL HOGAR	*/
/********************************/

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	

 *****************
 ***aguared_ch****
 *****************  
gen aguared_ch=(abasagua==2 | aguared==1)
label var aguared_ch "Acceso a fuente de agua por red"

 *****************
 ***aguadist_ch***
 *****************  
gen aguadist_ch=1 if aguacan==1 |aguared==1
replace aguadist_ch=2 if aguapozo==2
replace aguadist_ch=3 if aguapozo==4
replace aguadist_ch=0 if aguapozo==9 
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la casa" 2"Afuera de la casa pero dentro del terreno" 3"Afuera de la casa y del terreno" 
label val aguadist_ch aguadist_ch  

 *****************
 ***aguamala_ch***
 *****************
gen aguamala_ch=(abasagua==6) /*"Otra"*/	
label var aguamala_ch "Agua unimproved según MDG"

 *****************
 ***aguamide_ch***
 ***************** 
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

 ************
 ***luz_ch***
 ************ 
gen luz_ch=(ilumina==1)
replace luz_ch=. if ilumina==9
label var luz_ch  "La principal fuente de iluminación es electricidad"

 ****************
 ***luzmide_ch***
 **************** 
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

 ****************
 ***combust_ch***
 ****************
gen combust_ch=(combcoci==1|combcoci==2|combcoci==5)
replace combust_ch=. if combcoci==9
label var combust_ch "Principal combustible gas o electricidad" 

 *************
 ***bano_ch***
 *************
gen bano_ch=(sanita==1)
replace bano_ch=. if sanita==9
label var bano_ch "El hogar tiene servicio sanitario"


 ***************
 ***banoex_ch***
 ***************
gen banoex_ch=(usosani==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.| usosani==9
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************
gen des1_ch=1 if sissan>=1 & sissan<=3
replace des1_ch=2 if sissan==4
replace des1_ch=3 if sissan>=5
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if sissan==9
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

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen   aguamejorada_ch = 1 if abasagua ==2 | abasagua == 4
replace aguamejorada_ch = 0 if abasagua ==6
				
*********************
***banomejorado_ch***
*********************
gen  banomejorado_ch = 1 if (sanita == 1 & (sissan >= 1 & sissan <=3) & usosani == 2 )
replace banomejorado_ch = 0 if (sanita == 1 & (sissan >= 1 & sissan <=3) & usosani == 4) | sanita == 3 | (sanita== 1 & (sissan >= 4 & sissan <=7))
	
**************
***pared_ch***
**************
* Se cambia la construcción de la variable incluyendo: tapia sin revestir y de paja 
/*
gen pared_ch=0
replace pared_ch=1 if matpared==1 | matpared==2 |matpared==4
replace pared_ch=2 if matpared==6 | matpared==3 |matpared==5
replace pared_ch=. if matpared==9
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 5 (paja) como material impermanente
gen pared_ch=0 if matpared==5 
replace pared_ch=1 if matpared==1 | matpared==2 | matpared==4
replace pared_ch=2 if matpared==6 | matpared==3 
replace pared_ch=. if matpared==9
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales:otros"
label val pared_ch pared_ch

**************
***techo_ch***
**************
/*
*No se incluían los techos de paja
gen techo_ch=0
replace techo_ch=1 if matecho<=5
replace techo_ch=2 if matecho==7 |matecho==6
replace techo_ch=. if matecho==9
label var techo_ch "Materiales de construcción del techo"
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 6 (paja) como material impermanente
gen techo_ch=0 if matecho==6
replace techo_ch=1 if matecho<=5
replace techo_ch=2 if matecho==7
replace techo_ch=. if matecho==9
label var techo_ch "Materiales de construcción del techo"


**************
***resid_ch***
**************
gen resid_ch=0 if basura==1 | basura==2
replace resid_ch=1 if basura==3
replace resid_ch=2 if basura==4 | basura==5
replace resid_ch=3 if basura==6
replace resid_ch=. if basura==9
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


*************
***dorm_ch***
*************
gen dorm_ch=nrodorm
replace dorm_ch=. if nrodorm==99 |nrodorm==-1
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
gen cuartos_ch=nrocuart
replace cuartos_ch=. if nrocuart==99 | nrocuart==-1
label var cuartos_ch "Habitaciones en el hogar"


***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
gen telef_ch=(telefono==2)
replace telef_ch=. if telefono==9
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************
gen refrig_ch=(gelade==2 |gelade==4)
replace refrig_ch=. if gelade==9
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************
gen freez_ch=(frezer==1)
replace freez_ch=. if frezer==9
label var freez_ch "El hogar posee congelador"

*************
***auto_ch***
*************
gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"

***************
***compu_ch***
*************** 
capture gen compu_ch=.
label var compu_ch "El hogar posee computador"

 ***************
 ***internet_ch*
 *************** 
capture gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"

 ************
 ***cel_ch***
 ************
gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefonico celular"

 **************
 ***vivi1_ch***
 **************
gen viv1_ch=1 if tipodom==2
replace viv1_ch=2 if tipodom==4
replace viv1_ch=3 if tipodom==6
label var viv1_ch "Tipo de vivienda en la que reside el hogar"
label def viv1_ch 1"Casa" 2"Departamento" 3"Otros"
label val viv1_ch viv1_ch

 **************
 ***vivi2_ch***
 **************
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
label var viv2_ch "La vivienda es casa o departamento"

 *****************
 ***viviprop_ch***
 *****************
gen viviprop_ch=0 if tenenviv==3
replace viviprop_ch=1 if tenenviv==1
replace viviprop_ch=2 if tenenviv==2
replace viviprop_ch=3 if tenenviv>=4
replace viviprop_ch=. if tenenviv==9
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
gen vivialq_ch=montarri
replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0
label var vivialq_ch "Alquiler mensual"

 *****************
 ***vivialqimp_ch*
 *****************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"

/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/

 ***************
 ***relacion_ci*
 ***************
gen relacion_ci=parenfam
replace relacion_ci=5 if parenfam==5|parenfam==6|parenfam==8
replace relacion_ci=6 if parenfam==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

 *************
 ***sexo_ci***
 *************
gen sexo_ci=1 if sexo==2
replace sexo_ci=2 if sexo==4
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

 *************
 ***edad_ci***
 *************
gen edad_ci=edad
replace edad_ci=. if edad_ci==999
label variable edad_ci "Edad del individuo"

**************
***civil_ci***
**************
gen civil_ci=.
capture replace civil_ci=1 if vivecony==3 & viviocon==3 /*EN ALGUNOS AÑOS NO ESTA EL MODULO DE NUPCIALIDAD!*/
capture replace civil_ci=2 if vivecony==1
capture replace civil_ci=3 if viudo==2
capture replace civil_ci=4 if viudo==4
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

*************
***jefe_ci***
*************
gen jefe_ci=(parenfam==1)
label variable jefe_ci "Jefe de hogar"

sort idh_ch

******************
***nconyuges_ch***
******************
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2) 
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************
by idh_ch: egen byte nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************
by idh_ch: egen byte notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************
by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"

****************
***nempdom_ch***
****************
by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

*****************
***clasehog_ch***
*****************
gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

sort idh_ch

******************
***nmiembros_ch***
******************
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5) if miembros_ci==1
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************
by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************
by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************
by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65))
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************
by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************
by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

/*************************************************/
/*				VARIABLES DE DIVERSIDAD			*/
/************************************************/

****************
***afroind_ci***
****************
	
/*PIEL COLOR DE LA PIEL (Pregunta 4: El color o raza de .... es:)
0: Indígena
2: Blanca
4: Negra
6: Amarilla
8: Parda
9: No responde*/

gen afroind_ci = . 
replace afroind_ci = 1 if piel == 0 // Indígena
replace afroind_ci = 2 if piel == 4 | piel == 8 // Negra, Parda
replace afroind_ci = 3 if piel == 2 | piel == 6 // Blanca, Amarilla

****************
***afroind_ch***
****************
gen afroind_jefe = afroind_ci if relacion_ci == 1
egen afroind_ch = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe 

*******************
***afroind_ano_c***
*******************
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
replace condocup_ci=1 if (trabajo1==1 | trabajo2==2 | trabajo3==1 | trabajo4==2)
replace condocup_ci=2 if  trabajo4==4 & (bustrab1==1 & metodbus>=1 & metodbus<=8) /*tomaron alguna providencia en la semana de referencia*/
replace condocup_ci=3 if  condocup_ci!=1 & condocup_ci!=2
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
replace cotizando_ci=1 if (instipre==1 | iprevsec==1 | iprevotr==1 | conenpri==2) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (instipre==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (iprevsec==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

gen cotizaotros_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizaotros_ci=1 if (iprevotr==1 | conenpri==2) & cotizando_ci==0 
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
*2014, 01 revision MLO
foreach var of varlist yaposen ypen yotapose yotpen { 
replace `var'=. if `var'>=999999 | `var'==-1
}
gen pension_ci=0 
replace pension_ci=1 if (yaposen>0 & yaposen!=.) | (ypen>0 & ypen!=.) | (yotapose>0 & yotapose!=.) | (yotpen>0 & yotpen!=.) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
***************
*pensionsub_ci*
***************
/*DZ Octubre 2017- Creacion de la variable  pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 136 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(yotr==136)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

* Nota: ypen_ci e ypensub_ci se encuentran con las variables de ingreso.

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if (tuvotra1==1 | alguntr1==2) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*****************
*region /area ***
*****************
drop region 
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
replace area=3 if areacen==1
label define area 1"urbana" 2"rural" 3"metropolitana" 
label value area area
label var area "area del pais"

*********
*lp_ci***
*********
gen lp_ci=.				
replace lp_ci=99.5815109198707	if region==4	& area==1		/*sur-urbana*/
replace lp_ci=90.6535133663003	if region==4	& area==2		/*sur-rural */
replace lp_ci=101.641818014122	if region==2	& area==1		/*noreste-urbana*/
replace lp_ci=90.6535133663003	if region==2	& area==2		/*noreste-rural*/
replace lp_ci=79.6652087271876	if region==3	& area==1		/*sudeste-urbano*/
replace lp_ci=67.9901350334341	if region==3	& area==2		/*sudeste-rural*/
replace lp_ci=104.38889419785	if region==1	& area==1		/*norte-urbano*/
replace lp_ci=91.3402824557767	if region==1	& area==2		/*norte-rural */
replace lp_ci=84.4725920138754	if region==5	& area==1		/*centro oeste-urbano*/
replace lp_ci=74.1710564032768	if region==5	& area==2		/*centro oeste-rural */
replace lp_ci=113.31689175142	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lp_ci=96.1476657337556	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lp_ci=86.5328991342533	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lp_ci=114.003660753807	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lp_ci=100.955049011734	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lp_ci=82.4122848934975	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lp_ci=98.2079728280068	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lp_ci=126.365503493493	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=104.38889419785	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lp_ci=89.9667443639129	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci=118.124275029399	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=111.25658457008	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=88.5932062720491	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=100.955049011734	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci=.				
replace lpe_ci=49.7907554599353	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=45.3267566831502	if region==4	& area==2		/*sur-rural */
replace lpe_ci=50.8209090070609	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=45.3267566831502	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=39.8326043635938	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=33.9950675167171	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=52.1944470989248	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=45.6701412278884	if region==1	& area==2		/*norte-rural */
replace lpe_ci=42.2362960069377	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=37.0855282016384	if region==5	& area==2		/*centro oeste-rural */
replace lpe_ci=56.65844587571	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lpe_ci=48.0738328668778	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lpe_ci=43.2664495671267	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lpe_ci=57.0018303769036	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lpe_ci=50.4775245058672	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lpe_ci=41.2061424467487	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lpe_ci=49.1039864140034	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lpe_ci=63.1827517467463	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=52.1944470989248	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci=44.9833721819565	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci=59.0621375146994	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=55.6282922850398	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=44.2966031360245	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=50.4775245058672	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
gen salmm_ci=136
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
replace ocupa_ci=1 if ocupr==1 & emp_ci==1
replace ocupa_ci=3 if ocupr==2 & emp_ci==1
replace ocupa_ci=4 if ocupr==5 & emp_ci==1
replace ocupa_ci=5 if ocupr==7 & emp_ci==1
replace ocupa_ci=6 if ocupr==3 & emp_ci==1
replace ocupa_ci=7 if (ocupr==4 | ocupr==6) & emp_ci==1 
replace ocupa_ci=9 if ocupr==8 & emp_ci==1
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
replace rama_ci=1 if ramprin>0 & ramprin<50
replace rama_ci=2 if ramprin>=50 & ramprin<=59 
replace rama_ci=3 if ramprin>=100 & ramprin<=300 
replace rama_ci=4 if ramprin>=351 & ramprin<=353 
replace rama_ci=5 if ramprin==340 
replace rama_ci=6 if (ramprin>=410 & ramprin<=419) | (ramprin>=420 & ramprin<=424)|(ramprin>=511 & ramprin<=512)
replace rama_ci=7 if (ramprin>=471 & ramprin<=477) | (ramprin>=481 & ramprin<=482)|ramprin==583
replace rama_ci=8 if (ramprin>=451 & ramprin<=453) | (ramprin>=461 & ramprin<=464)
replace rama_ci=9 if (ramprin>=610 & ramprin<=619) | (ramprin>=621 & ramprin<=624)|(ramprin>=631 & ramprin<=632) | (ramprin>=711 & ramprin<=717) | (ramprin>=721 & ramprin<=727) | (ramprin==801)| (ramprin>=521 & ramprin<=582) | (ramprin>=584 & ramprin<=610) | ramprin==354
replace rama_ci=. if emp_ci==0
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


*****************
***horaspri_ci***
*****************

gen horaspri_ci=hrsnor
replace horaspri_ci=. if horaspri_ci==99 |horaspri_ci==-1 | condocup!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
gen horasprik_ci=horaspri_ci
capture replace horasprik_ci=hrnor59 if edad_ci>=5 & edad_ci<=9
replace horasprik_ci=. if edad_ci>=5 & edad_ci<=9 & (horasprik_ci==99 | horasprik_ci==-1| emp_ci==0)

*2014,01 revision MLO
replace horaspri_ci=. if horaspri_ci<0 | horaspri_ci>150
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************
*gen horastot_ci=.
*2014, 01 incorporacio MLO
replace hrsnor = . if hrsnor == -1 | hrsnor == 99
replace hrsec = . if hrsec == -1 | hrsec == 99
replace hrotr = . if hrotr == -1 | hrotr == 99


egen horastot_ci = rsum(hrsnor hrsec hrotr) 
replace horastot_ci = . if  (horaspri_ci==. & hrsec==. & hrotr==.) | condocup!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/

replace horastot_ci = . if horastot_ci < 0
replace horastot_ci = . if horastot_ci > 150
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	
replace anosal=. if anosal==99 | anosal==-1
replace mesesal=. if mesesal==99 | mesesal==-1


*Yanira Oviedo, Junio 2010: Se estaba multiplicando por 12, pero al ser un valor anual, debería dividirse 
/*
gen aux1=anosal/12
egen durades_ci=rsum(aux1 mesesal) if  condocup!=1 & edad_ci>=10
replace durades_ci=. if (anosal==. & mesesal==.) */
*MLO 03,2014

****************
***durades_ci***
****************
gen durades_ci=.
label variable durades_ci "Duracion del desempleo en meses"

*******************
***antiguedad_ci***
*******************
replace anost=. if anost==99 | anost==-1
replace mest=. if mest==99 | mest==-1
gen aux2=mest/12
egen antiguedad_ci=rsum(anost aux2) if emp_ci==1
replace antiguedad_ci=. if anost==. & mest==. 
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
gen categopri_ci=1 if catprina==4 | (catpriag>=8 & catpriag<=10)
replace categopri_ci=1 if cat59ano==4 | cat59sem==4
replace categopri_ci=2 if cat59ano==3 | catprina==3 |cat59sem==3 |(catpriag>=5 & catpriag<=7)
replace categopri_ci=3 if cat59ano==1 | cat59ano==2 |catprina==1 |catprina==2 |cat59sem==1 |cat59sem==2 | (catpriag>=1 & catpriag<=4)
replace categopri_ci=4 if (cat59ano>=5 & cat59ano<=8) |(catprina>=5 & catprina<=8) |(cat59sem>=5 & cat59sem<=8) | (catpriag>=11 & catpriag<=13)
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional en la ocupación principal'"

******************
***categosec_ci***
******************
gen categosec_ci=1 if catsec==4
replace categosec_ci=2 if catsec==3
replace categosec_ci=3 if catsec==1
replace categosec_ci=4 if catsec==5 |catsec==6
replace categosec_ci=. if emp_ci!=1 
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categosec_ci 3"Empleado" 4" No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional en la ocupación secundaria"

*****************
***nempleos_ci***
*****************
gen nempleos_ci=1 if nrotrab==1
replace nempleos_ci=2 if nrotrab>1
label var nempleos_ci "Número de empleos" 
label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
label value nempleos_ci nempleos_ci

*****************
***spublico_ci***
*****************
gen spublico_ci=(sectorem==4)
replace spublico_ci=. if sectorem==9
label var spublico_ci "Personas que trabajan en el sector público"


*******************
***tamemp_ci*******
*******************
gen tamemp_ci=1 if nroetemp==1 | nroetemp==3 | nroetemp==5 | nroeper==1 | nroeper==3 | nroeper==5 | tamest1==2 | tamest1==4 | tamest2==2 | tamest2==4 | tamest2==6 
replace tamemp_ci=2 if nroetemp==7 | nroeper==7 | tamest1==6 | tamest2==8
replace tamemp_ci=3 if nroetemp==8 | nroeper==8 | tamest1==8 | tamest2==0

label var  tamemp_ci "Tamaño de Empresa" 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

******************
***categoinac_ci**
******************
gen categoinac_ci=1 if (jubila==2 | renpen==1) & condocup_ci==3
replace categoinac_ci=2 if asist==2 & condocup_ci==3
replace categoinac_ci=3 if qqhh==1 & condocup_ci==3
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

/*TODAS LAS vARIABLES "SECUNDARIAS": ylmsec_ci, ylnmsec_ci, ylmotros_ci, ylnmotros_ci Y durades_ci ESTAN CREADAS SÓLO PARA 
LOS MAYORES DE 10 AÑOS. POR LO TANTO LAS vARIABLES AGREGADAS CON SUFIJO k EN REALIDAD SÓLO SE REFIEREN A LA ACTIvIDAD 
PRINCIPAL DE LOS NIÑOS*/

***************
***ylmpri_ci***
***************
gen ylmpri_ci=yocpef 
replace ylmpri_ci=. if yocpef==-1 | yocpef>=999999 | condocup!=1 
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

gen ylmprik_ci=yocpef
replace ylmprik_ci=. if yocpef==-1 | yocpef>=999999 | emp_ci==0 
capture replace ylmprik_ci=yef59 if edad_ci>=5 & edad_ci<=9
capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (yef59==-1 | yef59>=999999 |emp_ci==0)

****************
***ylnmpri_ci***
****************
gen ylnmpri_ci=yocpes if edad_ci>=10
replace ylnmpri_ci=. if yocpes==-1 | yocpes>=999999 | condocup!=1

gen ylnmprik_ci=yocpes
replace ylnmprik_ci=. if yocpes==-1 | yocpes>=999999 | emp_ci==0
capture replace ylnmprik_ci=yes59 if edad_ci>=5 & edad_ci<=9
capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (yes59==-1 | yes59>=999999 | emp_ci==0)

***************
***ylmsec_ci***  
***************
gen ylmsec_ci=yocsef if edad_ci>=10
replace ylmsec_ci=. if yocsef==-1 | yocsef>=999999 | condocup!=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************
gen ylnmsec_ci=yocses if edad_ci>=10
replace ylnmsec_ci=. if yocses==-1 | yocses>=999999 | condocup!=1
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=yotrosef if edad_ci>=10
replace ylmotros_ci=. if yotrosef==-1 | yotrosef>=999999 | condocup!=1
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

******************
***ylnmotros_ci***
******************
gen ylnmotros_ci=yotroses if edad_ci>=10
replace ylnmotros_ci=. if yotroses==-1 | yotroses>=999999 | condocup!=1
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=(ylmpri_ci==. & condocup==1)
replace nrylmpri_ci=. if condocup==2
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

gen nrylmprik_ci=(ylmprik_ci==. & emp_ci==1)
replace nrylmprik_ci=. if emp_ci==0

************
***ylm_ci***
************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

egen ylmk_ci=rsum(ylmprik_ci ylmsec_ci ylmotros_ci)
replace ylmk_ci=. if ylmprik_ci==. & ylmsec_ci==. & ylmotros_ci==.

*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

egen ylnmk_ci=rsum(ylnmprik_ci ylnmsec_ci ylnmotros_ci)
replace ylnmk_ci=. if ylnmprik_ci==. & ylnmsec_ci==. & ylnmotros_ci==.

*************
***ynlm_ci***
*************
foreach var of varlist yaposen ypen yotapose yotpen yabon yalug ydon yotr{ 
replace `var'=. if `var'>=999999 | `var'==-1
}
egen ynlm_ci=rsum(yaposen ypen yotapose yotpen yabon yalug ydon yotr) if edad_ci>=10
replace ynlm_ci=. if (yaposen==. &  ypen==. &  yotapose==. &  yotpen==. &  yabon==. &  yalug==. & ydon==. & yotr==.) | ynlm_ci<0
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ylnm_ci***
**************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 

sort idh_ch 
*****************
***nrylmpri_ch***
*****************
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen nrylmprik_ch=max(nrylmprik_ci) if miembros_ci==1

**************
*** ylm_ch ***
**************
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"

by idh_ch: egen ylmk_ch=sum(ylmk_ci)if miembros_ci==1

***************
*** ylnm_ch ***
***************
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

by idh_ch: egen ylnmk_ch=sum(ylnmk_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************
gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1
gen ylmnrk_ch=ylmk_ch
replace ylmnrk_ch=. if nrylmprik_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del hogar"

**************
***ynlnm_ch***
**************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*****************
***ylmhopri_ci ***
*****************
*2015, 03 modificacion MLO
*gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
gen ylmhoprik_ci=ylmprik_ci/(horasprik_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
replace ylmhoprik_ci=. if ylmhoprik_ci<=0

***************
***ylmho_ci ***
***************
gen ylmho_ci=.
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
***remesas_ci***
****************
gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
***remesas_ch***
****************
gen remesas_ch=.
label var remesas_ch "Remesas mensuales del hogar" 

*************
*ypen_ci*
*************
egen ypen_ci=rsum (yaposen ypen yotapose yotpen)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "Valor de la pension contributiva"

*****************
**  ypensub_ci  *
*****************
/*DZ Octubre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 136 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=yotr if yotr==136
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

// Falta generar:

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
duración y siguen 4 de Esino fundamental ou 2do grau (en este anio aparecen como
Regular ou 1er grau y Regular ou 2do grau respectivamente). El otro, se computan 
4 años obligatorios de Elementar, 4 años de Medio 1er ciclo y se completa con 4 años 
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

Para mantener consistencia con los scipts de los anios 2002 a 2020 se construye
asispre_ci condicionando por edad el nivel educativo cursoasi == 7 (Pre-escolar 
oou creche) para identificar los asistentes a pre-escolar, se toman individuos
de 4 a 5 anios. 
*/

* Valores ignorados o no aplicables van a perdidos:

replace asist = . if asist == 0 | asist == 9
replace cursoasi = . if cursoasi == -1 | cursoasi == 0
replace serieasi = . if serieasi == 0 | serieasi == 9 
replace asisant = . if asisant == 0 | asisant == 9
replace ultcurso = . if ultcurso == -1 | ultcurso == 0
replace ultserie = . if ultserie == 0 | ultserie == 9
replace terult = . if terult == 0 | terult == 9


**************
**asiste_ci***
**************
gen asiste_ci = (asist == 2)
label var asiste_ci "Personas que actualmente asisten a un centro de enseñanza"

***************
***edupub_ci***
***************
// No se pregunta por el tipo de centro educativo
gen edupub_ci = .
label var  edupub_ci "Personas que asisten a centros de enseñanza públicos"


*************
***aedu_ci***
*************

gen aedu_ci = . 

* PARA LOS QUE ASISTEN:
***********************
* Alfabetização de adultos, Creche, Pré-escolar
replace aedu_ci = 0 if inlist(cursoasi, 6, 7) 

* Regular de grado 1 y 2
replace aedu_ci = serieasi - 1 if inlist(cursoasi, 1, 3) // Regular ou 1º grau
replace aedu_ci = serieasi + 8 - 1 if inlist(cursoasi, 2, 4) // Regular ou 2º grau

* Superior 
replace aedu_ci = serieasi + 12 - 1 if cursoasi == 5 // Superior 

* Imputación para los que declaran nivel pero no grado (incluye no seriados)
replace aedu_ci = 0 if (inlist(cursoasi, 1, 3) & serieasi == .) // Regular ou 1º grau
replace aedu_ci = 8 if (inlist(cursoasi, 2, 4) & serieasi == .) // Regular ou 2º grau
replace aedu_ci = 12 if cursoasi == 8 // Pre-Vestibular
replace aedu_ci = 12 if (cursoasi == 5 & serieasi == .) // Superior
replace aedu_ci = 12 + 4 if cursoasi == 9 // Maestría o Doctorado

* PARA LOS QUE NO ASISTEN:
**************************

* Alfabetização de adultos, Creche, Pré-escolar
replace aedu_ci = 0 if inlist(ultcurso, 8, 9) 

* Regular 

* Sistema antiguo 
replace aedu_ci = ultserie if ultcurso == 1 // Elementar (primario) - 4 anios
replace aedu_ci = ultserie + 4 if ultcurso == 2 // Medio 1er ciclo (ginasal) - 4 anios
replace aedu_ci = ultserie + 8 if ultcurso == 3 // Medio 2do ciclo (científico, clasico etc.)

* Sistema actual 
replace aedu_ci = ultserie if ultcurso == 4 // 1 grado
replace aedu_ci = ultserie + 8 if ultcurso == 5 // 2 grado

* Superior 
replace aedu_ci = ultserie + 12 if ultcurso == 6 // Superior

* Imputación para los que declaran nivel pero no grado

* No finalizado
replace aedu_ci = 0 if (inlist(ultcurso, 1, 4) & ultserie == . & inlist(terult, 3, .)) // Elementar, Regular ou 1º grau
replace aedu_ci = 4 if (ultcurso == 2 & ultserie == . & inlist(terult, 3, .)) // Medio 1 
replace aedu_ci = 8 if (ultcurso == 3 & ultserie == . & inlist(terult, 3, .)) // Medio 2 
replace aedu_ci = 8 if (ultcurso == 5 & ultserie == . & inlist(terult, 3, .)) // Regular ou 2º grau
replace aedu_ci = 12 if (ultcurso == 6 & ultserie == . & inlist(terult, 3, .)) // Superior
replace aedu_ci = 12 + 4 if (ultcurso == 7 & ultserie == . & inlist(terult, 3, .)) // Maestría o Doctorado

* Finalizado 
replace aedu_ci = 4 if (ultcurso == 1  & ultserie == . & terult == 1) // Elementar
replace aedu_ci = 8 if (ultcurso == 2 & ultserie == . & terult == 1) // Medio 1 
replace aedu_ci = 8 if (ultcurso == 4 & ultserie == . & terult == 1) // Regular ou 1º grau
replace aedu_ci = 12 if (ultcurso == 3 & ultserie == . & terult == 1) // Medio 2 
replace aedu_ci = 12 if (ultcurso == 5 & ultserie == . & terult == 1) // Regular ou 2º grau
replace aedu_ci = 12 + 4 if (ultcurso == 6 & ultserie == . & terult == 1) // Universitario
replace aedu_ci = 12 + 4 + 2 if (ultcurso == 7 & ultserie == . & terult == 1) // Maestría o Doctorado

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
gen byte eduui_ci = (aedu_ci >= 13 & aedu_ci <= 14) | (aedu_ci == 15 & terult != 1) 
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**************
***eduuc_ci***
**************
/* Aquellos con 15 anios que completaron nivel 
o cualqueira con mas de 15 anios de educ.*/
gen byte eduuc_ci = (aedu_ci == 15 & terult == 1 | aedu_ci > 15) 
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
g asispre_ci = (cursoasi == 7 & inlist(edad, 4, 5))
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

******************************
*** VARIABLES DE MIGRACION ***
******************************

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(dondenac==98) 
	replace migrante_ci = . if dondenac == 99
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & (vivsep94==1 | (ufed94!=. & ufed94!=98) | (migrante_ci==1 & tpoufede==6) | (migrante_ci==1 & tpoufedc==4))) if migrante_ci!=. & !inrange(edad_ci,0,4)	
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=(migrante_ci==1 & (vivsep94==1 | (ufed94!=. & ufed94!=98) | (migrante_ci==1 & tpoufede==6) | (migrante_ci==1 & tpoufedc==4))) if migrante_ci!=. & !inrange(edad_ci,0,4)
	replace migrantiguo5_ci = 0 if migantiguo5_ci != 1 & migrante_ci==1
	replace migrantiguo5_ci = . if migrante_ci==0
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"


***********************************************************
***********************************************************



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
rename ocuprin codocupa
rename ramprin codindustria

compress

saveold "`base_out'", version(12) replace


log close





