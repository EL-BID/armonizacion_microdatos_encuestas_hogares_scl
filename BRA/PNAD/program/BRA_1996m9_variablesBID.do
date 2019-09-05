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
local ANO "1996"
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
/*    VARIABLES DEL HOGAR	*/
/********************************/

egen idh_ch=group(unifede nrocont nroserie nrofam)
sort idh_ch
by idh_ch: gen idp_ci=_n
gen factor_ch=pesodom
gen zona_c=1 if (situacen>=1 & situacen<=3) | situacen==9
replace zona_c=0 if (situacen>=4 & situacen<=8) | situacen==10
gen str3 pais_c="BRA"
gen anio_c=1996
gen mes_c=9
gen relacion_ci=parenfam
replace relacion_ci=5 if parenfam==5|parenfam==6|parenfam==8
replace relacion_ci=6 if parenfam==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(abasagua==2 | aguared==1)
gen aguadist_ch=1 if aguacan==1 |aguared==1
replace aguadist_ch=2 if aguapozo==2
replace aguadist_ch=3 if aguapozo==4
replace aguadist_ch=0 if aguapozo==9 
gen aguamala_ch=(abasagua==6) /*"Otra"*/	
gen aguamide_ch=.
gen luz_ch=(ilumina==1)
replace luz_ch=. if ilumina==9
gen luzmide_ch=.
gen combust_ch=(combcoci==1|combcoci==2|combcoci==5)
replace combust_ch=. if combcoci==9
gen bano_ch=(sanita==1)
replace bano_ch=. if sanita==9
gen banoex_ch=(usosani==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.|usosani==9
gen des1_ch=1 if sissan>=1 & sissan<=3
replace des1_ch=2 if sissan==4
replace des1_ch=3 if sissan>=5
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if sissan==9

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

gen piso_ch=.

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
replace techo_ch=2 if matecho==7 | matecho==6
replace techo_ch=. if matecho==9
label var techo_ch "Materiales de construcción del techo"
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 6 (paja) como material impermanente
gen techo_ch=0 if matecho==6
replace techo_ch=1 if matecho<=5
replace techo_ch=2 if matecho==7
replace techo_ch=. if matecho==9
label var techo_ch "Materiales de construcción del techo"

gen resid_ch=0 if basura==1 | basura==2
replace resid_ch=1 if basura==3
replace resid_ch=2 if basura==4 | basura==5
replace resid_ch=3 if basura==6
replace resid_ch=. if basura==9

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
	
gen dorm_ch=nrodorm
replace dorm_ch=. if nrodorm==99 |nrodorm==-1
gen cuartos_ch=nrocuart
replace cuartos_ch=. if nrocuart==99 | nrocuart==-1
gen cocina_ch=.
gen refrig_ch=(gelade==2 |gelade==4)
replace refrig_ch=. if gelade==9
gen freez_ch=(frezer==1)
replace freez_ch=. if frezer==9
gen auto_ch=.
gen telef_ch=(telefono==2)
replace telef_ch=. if telefono==9
capture gen compu_ch=(V0231==1)
capture gen internet_ch=(V0232==2)
gen cel_ch=.
gen viv1_ch=1 if tipodom==2
replace viv1_ch=2 if tipodom==4
replace viv1_ch=3 if tipodom==6
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
gen viviprop_ch=0 if tenenviv==3
replace viviprop_ch=1 if tenenviv==1
replace viviprop_ch=2 if tenenviv==2
replace viviprop_ch=4 if tenenviv>=4
replace viviprop_ch=. if tenenviv==9
gen vivialq_ch=montarri
replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0
gen vivialqimp_ch=.

/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*PIEL COLOR DE LA PIEL (Pregunta 4: El color o raza de .... es:)
0: Indígena
2: Blanca
4: Negra
6: Amarilla
8: Parda
9: No responde*/

gen raza_ci=.
replace raza_ci= 1 if  (piel ==0)
replace raza_ci= 2 if  (piel ==4 | piel ==8)
replace raza_ci= 3 if (piel==2 | piel==6 | piel== 9) & raza_ci==.
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

gen factor_ci=pesodom /*AUN CUANDO HAY UN FACTOR DE PERSONAS ES IDENTICO AL DE HOGARES, EXCEPTO PARA EL '93 EN DONDE SE REGISTRAN VALORES NEGATIVOS! PARA HOMOGENEIZAR,A TODOS LES PONEMOS EL FACTOR DE EXPANSION DEL HOGAR*/
gen sexo_ci=1 if sexo==2
replace sexo_ci=2 if sexo==4
gen edad_ci=edad
replace edad_ci=. if edad_ci==999
gen civil_ci=.
capture replace civil_ci=1 if vivecony==3 & viviocon==3 /*EN ALGUNOS AÑOS NO ESTA EL MODULO DE NUPCIALIDAD!*/
capture replace civil_ci=2 if vivecony==1
capture replace civil_ci=3 if viudo==2
capture replace civil_ci=4 if viudo==4
gen jefe_ci=(parenfam==1)
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
foreach var of varlist yaposen ypen yotapose yotpen{ 
replace `var'=. if `var'>=999999 | `var'==-1
}


gen pension_ci=0 
replace pension_ci=1 if (yaposen>0 & yaposen!=.) | (ypen>0 & ypen!=.) | (yotapose>0 & yotapose!=.) | (yotpen>0 & yotpen!=.) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************
sum yaposen ypen yotapose yotpen
egen ypen_ci=rsum (yaposen ypen yotapose yotpen)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
/*DZ Octubre 2017- Creacion de la variable  pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 112 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(yotr==112)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*****************
**  ypensub_ci  *
*****************
/*DZ Octubre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 112 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=yotr if yotr==112
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if (tuvotra1==1 | alguntr1==2) & condocup_ci==2
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
replace area=3 if areacen==1
label define area 1"urbana" 2"rural" 3"metropolitana" 
label value area area
label var area "area del pais"


*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci=87.0005956274323	if region==4	& area==1		/*sur-urbana*/
replace lp_ci=79.2005422064123	if region==4	& area==2		/*sur-rural */
replace lp_ci=88.800607926096	if region==2	& area==1		/*noreste-urbana*/
replace lp_ci=79.2005422064123	if region==2	& area==2		/*noreste-rural*/
replace lp_ci=69.6004764943372	if region==3	& area==1		/*sudeste-urbano*/
replace lp_ci=59.4004066624179	if region==3	& area==2		/*sudeste-rural*/
replace lp_ci=91.2006243750385	if region==1	& area==1		/*norte-urbano*/
replace lp_ci=79.8005463566911	if region==1	& area==2		/*norte-rural */
replace lp_ci=73.8005052495521	if region==5	& area==1		/*centro oeste-urbano*/
replace lp_ci=64.8004436344953	if region==5	& area==2		/*centro oeste-rural */
replace lp_ci=99.0006777960586	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lp_ci=84.0005751042973	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lp_ci=75.6005175710417	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lp_ci=99.600681870251	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lp_ci=88.2006038519036	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lp_ci=72.0004929280625	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lp_ci=85.800587402961	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lp_ci=110.400755814406	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=91.2006243750385	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lp_ci=78.6005381322199	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci=103.200706543665	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=97.2006654213085	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=77.4005299077486	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=88.2006038519036	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =.
replace lpe_ci=43.5002978137162	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=39.6002711032061	if region==4	& area==2		/*sur-rural */
replace lpe_ci=44.400303963048	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=39.6002711032061	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=34.8002382471686	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=29.7002033312089	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=45.6003121875193	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=39.9002731783456	if region==1	& area==2		/*norte-rural */
replace lpe_ci=36.900252624776	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=32.4002218172477	if region==5	& area==2		/*centro oeste-rural */
replace lpe_ci=49.5003388980293	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lpe_ci=42.0002875521487	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lpe_ci=37.8002587855208	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lpe_ci=49.8003409351255	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lpe_ci=44.1003019259518	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lpe_ci=36.0002464640312	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lpe_ci=42.9002937014805	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lpe_ci=55.200377907203	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=45.6003121875193	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci=39.3002690661099	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci=51.6003532718324	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=48.6003327106542	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=38.7002649538743	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=44.1003019259518	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lpe_ci "Linea de indigencia oficial del pais"


*************
**salmm_ci***
*************
gen salmm_ci=112
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

/*************************************************************************************************************************/
gen ocupa_ci=.
replace ocupa_ci=1 if ocupr==1 & emp_ci==1
replace ocupa_ci=3 if ocupr==2 & emp_ci==1
replace ocupa_ci=4 if ocupr==5 & emp_ci==1
replace ocupa_ci=5 if ocupr==7 & emp_ci==1
replace ocupa_ci=6 if ocupr==3 & emp_ci==1
replace ocupa_ci=7 if (ocupr==4 | ocupr==6) & emp_ci==1 
replace ocupa_ci=9 if ocupr==8 & emp_ci==1

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
label define rama_ci 1 "Agricultura, Caza, Civicultura y Pesca" 2 "Explotación de minas y Canteras" 3 "Industrias Manufactureras" 4 "Electricidad, Gas y Agua" 5 "Construcción" 6 "Comercio al por mayor y menor, Restaurantes y Hoteles" 7 "Transporte y Almacenamiento" 8 "Establecimientos Financieros, Seguros y Bienes Inmuebles" 9 "Servicios Sociales, Comunales y personales" 
label values rama_ci rama_ci

gen horaspri_ci=hrsnor
replace horaspri_ci=. if horaspri_ci==99 |horaspri_ci==-1 | condocup!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
gen horasprik_ci=horaspri_ci
capture replace horasprik_ci=hrnor59 if edad_ci>=5 & edad_ci<=9
replace horasprik_ci=. if edad_ci>=5 & edad_ci<=9 & (horasprik_ci==99 | horasprik_ci==-1| emp_ci==0)

*2014,01 revision MLO
replace horaspri_ci=. if horaspri_ci<0 | horaspri_ci>150

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

gen ylmpri_ci=yocpef 
replace ylmpri_ci=. if yocpef==-1 | yocpef>=999999 | condocup!=1 

gen ylmprik_ci=yocpef
replace ylmprik_ci=. if yocpef==-1 | yocpef>=999999 | emp_ci==0 
capture replace ylmprik_ci=yef59 if edad_ci>=5 & edad_ci<=9
capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (yef59==-1 | yef59>=999999 |emp_ci==0)

gen ylnmpri_ci=yocpes if edad_ci>=10
replace ylnmpri_ci=. if yocpes==-1 | yocpes>=999999 | condocup!=1

gen ylnmprik_ci=yocpes
replace ylnmprik_ci=. if yocpes==-1 | yocpes>=999999 | emp_ci==0
capture replace ylnmprik_ci=yes59 if edad_ci>=5 & edad_ci<=9
capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (yes59==-1 | yes59>=999999 | emp_ci==0)

/*TODAS LAS VARIABLES "SECUNDARIAS": ylmsec_ci, ylnmsec_ci, ylmotros_ci, ylnmotros_ci Y durades_ci ESTAN CREADAS SÓLO PARA 
LOS MAYORES DE 10 AÑOS. POR LO TANTO LAS VARIABLES AGREGADAS CON SunifedeIJO k EN REALIDAD SÓLO SE REFIEREN A LA ACTIVIDAD 
PRINCIPAL DE LOS NIÑOS*/

gen ylmsec_ci=yocsef if edad_ci>=10
replace ylmsec_ci=. if yocsef==-1 | yocsef>=999999 | condocup!=1

gen ylnmsec_ci=yocses if edad_ci>=10
replace ylnmsec_ci=. if yocses==-1 | yocses>=999999 | condocup!=1

gen ylmotros_ci=yotrosef if edad_ci>=10
replace ylmotros_ci=. if yotrosef==-1 | yotrosef>=999999 | condocup!=1

gen ylnmotros_ci=yotroses if edad_ci>=10
replace ylnmotros_ci=. if yotroses==-1 | yotroses>=999999 | condocup!=1

gen nrylmpri_ci=(ylmpri_ci==. & condocup==1)
replace nrylmpri_ci=. if condocup==2

gen nrylmprik_ci=(ylmprik_ci==. & emp_ci==1)
replace nrylmprik_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.

egen ylmk_ci=rsum(ylmprik_ci ylmsec_ci ylmotros_ci)
replace ylmk_ci=. if ylmprik_ci==. & ylmsec_ci==. & ylmotros_ci==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==. & ylnmotros_ci==.

egen ylnmk_ci=rsum(ylnmprik_ci ylnmsec_ci ylnmotros_ci)
replace ylnmk_ci=. if ylnmprik_ci==. & ylnmsec_ci==. & ylnmotros_ci==.


foreach var of varlist yaposen ypen yotapose yotpen yabon yalug ydon yotr{ 
replace `var'=. if `var'>=999999 | `var'==-1
}
egen ynlm_ci=rsum(yaposen ypen yotapose yotpen yabon yalug ydon yotr) if edad_ci>=10
replace ynlm_ci=. if (yaposen==. &  ypen==. &  yotapose==. &  yotpen==. &  yabon==. &  yalug==. & ydon==. & yotr==.) | ynlm_ci<0

gen ynlnm_ci=.
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen nrylmprik_ch=max(nrylmprik_ci) if miembros_ci==1

by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1
by idh_ch: egen ylmk_ch=sum(ylmk_ci) if miembros_ci==1
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1
by idh_ch: egen ylnmk_ch=sum(ylnmk_ci) if miembros_ci==1

gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1
gen ylmnrk_ch=ylmk_ch
replace ylmnrk_ch=. if nrylmprik_ch==1

by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1
gen ynlnm_ch=.
*2015, 03 modificacion MLO
*gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
gen ylmhoprik_ci=ylmprik_ci/(horasprik_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
replace ylmhoprik_ci=. if ylmhoprik_ci<=0

gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.
gen remesas_ci=.
sort idh_ch
gen remesas_ch=.

replace anosal=. if anosal==99 | anosal==-1
replace mesesal=. if mesesal==99 | mesesal==-1

*Yanira Oviedo, Junio 2010: Se estaba multiplicando por 12, pero al ser un valor anual, debería dividirse 
/*gen aux1=anosal/12
egen durades_ci=rsum(aux1 mesesal) if  condocup!=1 & edad_ci>=10
replace durades_ci=. if (anosal==. & mesesal==.) 
*/
*MLO 03,2014
gen durades_ci=.

replace anost=. if anost==99 | anost==-1
replace mest=. if mest==99 | mest==-1
gen aux2=mest/12
egen antiguedad_ci=rsum(anost aux2) if emp_ci==1
replace antiguedad_ci=. if anost==. & mest==. 

drop aux*

drop *k_ci


/******************************************************************************************/
/*					VARIABLES DEL MERCADO LABORAL			  			*/
/******************************************************************************************/


gen desalent_ci=.
gen subemp_ci=.
gen tiempoparc_ci=.

gen categopri_ci=1 if catprina==4 | (catpriag>=8 & catpriag<=10)
replace categopri_ci=2 if catprina==3 |(catpriag>=5 & catpriag<=7)
replace categopri_ci=3 if catprina==1 |catprina==2 | (catpriag>=1 & catpriag<=4)
replace categopri_ci=4 if (catprina>=5 & catprina<=8) | (catpriag>=11 & catpriag<=13)
replace categopri_ci=. if emp_ci!=1

gen categosec_ci=1 if catsec==4
replace categosec_ci=2 if catsec==3
replace categosec_ci=3 if catsec==1
replace categosec_ci=4 if catsec==5 |catsec==6
replace categosec_ci=. if emp_ci!=1 
gen nempleos_ci=1 if nrotrab==1
replace nempleos_ci=2 if nrotrab>1
/*
gen firmapeq=1 if catpriag==1 & tamest1<=4 /*catpriag=Empleado permanente en el Agro*/
replace firmapeq=0 if catpriag==1 & (tamest1==6 | tamest1==8) /*catpriag=Empleado permanente en el Agro*/
replace firmapeq=1 if (catpriag>=2 & catpriag<=4) & ((tuvoayu==1 & nroayu<=6) | tuvoayu==3) /*catpriag= Algun tipo de empleado en el Agro*/
replace firmapeq=0 if (catpriag>=2 & catpriag<=4) & (tuvoayu==1 & (nroayu==8 | nroayu==0)) /*catpriag= Algun tipo de empleado en el Agro*/
replace firmapeq=1 if catpriag==5 & ((socio==1 & nrosocio<=6) | socio==3) /*catpriag=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=0 if catpriag==5 & socio==1 & nrosocio==8 /*catpriag=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=1 if catpriag==6 | catpriag==7 /*Cuenta Propia en Agro o en otra actividad*/
replace firmapeq=0 if (catpriag==8 | catprina==4) & ((tamest2==0 | tamest2==8) | ((tamest2==2 | tamest2==4) & socio==1 & nrosocio>=6)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (catpriag==8 | catprina==4) & ((tamest2<=6 & socio==3) | ((tamest2==2 | tamest2==4) & socio==1 & nrosocio<=4)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (catpriag==9 | catpriag==10) & ((empltemp==2 & nroetemp<=5 & emplper==4) | (empltemp==2 & nroetemp<=3 & emplper==2 & nroeper<=3) | (empltemp==4 & emplper==2 & nroeper<=5) | (empltemp==4 & emplper==4)) /*Empleador en Agro u otras actividades*/
replace firmapeq=0 if (catpriag==9 | catpriag==10) & ((empltemp==2 & (nroetemp==7 | nroetemp==8)) | (empltemp==4 & emplper==2 & nroeper>=5)) /*Empleador en Agro u otras actividades*/
replace firmapeq=1 if (catpriag>=11 & catpriag<=13) | (catprina>=5 & catprina<=7) /*Trabajador No remunerado*/
replace firmapeq=1 if catprina==1 & (sectorem==2 & tamest1<=4)  /*Empleado NO Agricola*/
replace firmapeq=0 if catprina==1 & (sectorem==2 & (tamest1==6 | tamest1==8)) /*Empleado NO Agricola*/
/*Los empleados NO Agricolas que trabajan en el sector PUBLICO o que son empleados domesticos no tienen tamaño de firma!*/
replace firmapeq=1 if catprina==3 & (socio==3 | (socio==1 | nrosocio<=6))/*Cuenta Propia NO Agricola*/
replace firmapeq=0 if catprina==3 & (socio==1 | (nrosocio==8 | nrosocio==0))/*Cuenta Propia NO Agricola*/
/*Que pasa con los trabajadores no remunerados? Se incluyen en tamaño de firma?*/

ren firmapeq firmapeq_ci
*cambio introducido el 06/13/05*
*/

gen spublico_ci=(sectorem==4)
replace spublico_ci=. if sectorem==9

/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/
gen asiste_ci=(asist==2)
*Codigo original de Suzanne!*
gen aedu_ci=99

preserve
keep if asiste_ci==0
/*not aplicable or no response*/	
	replace aedu_ci=. if ultcurso==-1 | ultcurso==8
** pre-escolar
	replace aedu_ci=0 if ultcurso==9 & anio_c<2001
	replace aedu_ci=0 if (ultcurso==9 | ultcurso==10) & anio_c>=2001
** elementar(primario)  ** admissao=4
	replace aedu_ci=min(ultserie,4) if ultcurso==1 & ultserie>=1 & ultserie<=5
** elementar(primario)  -- problem  people said 6 years of primario in 1995
** there were no people who answered v317==1 & v315==6 in 1990
** here we decided to give people who answered ultcurso==1 & ultserie==6 aedu_ci==6
** this is questionable but avoids missings
         replace aedu_ci=6 if ultcurso==1 & ultserie==6   
** medio 1 ciclo (ginasial, etc)
	replace aedu_ci=min(ultserie+4,8) if ultcurso==2 & ultserie>=1 & ultserie<=5
** medio 2 ciclo (cientifico, clasico, etc, etc)
	replace aedu_ci=min(ultserie+8,11) if ultcurso==3 & ultserie>=1 & ultserie<=4
** primeiro grau
	replace aedu_ci=ultserie if ultcurso==4 & ultserie>=1 & ultserie<=8
** segundo grau
	replace aedu_ci=min(ultserie+8,11) if ultcurso==5 & ultserie>=1 & ultserie<=4
** superior
	replace aedu_ci=min(ultserie+11,17) if ultcurso==6 & ultserie>=1 & ultserie<=8
** maestria o doctorado  (set to 17 as in algorithm  -- unknown years)
*** I don't understand why everyone who reports ultcurso=7 has a 0 for ultserie?????
replace aedu_ci=17 if ultcurso==7 & ultserie==0
replace aedu_ci=min(20,(15+serieasi)) if ultcurso==7 & ultserie>0 & ultserie<9 /**** this is the number of observations in the out of school data set*/
replace aedu_ci=0  if ultcurso==1 & ultserie==0 & terult!=1
replace aedu_ci=4  if ultcurso==2 & ultserie==0 & terult!=1
replace aedu_ci=8  if ultcurso==3 & ultserie==0 & terult!=1
replace aedu_ci=0  if ultcurso==4 & ultserie==0 & terult!=1
replace aedu_ci=8 if ultcurso==5 & ultserie==0 & terult!=1
replace aedu_ci=11 if ultcurso==6 & ultserie==0 & terult!=1
*** careful:  all who answer ultcurso==7 (master/doctorado) report 0 for ultserie and 0 for termino so must use terult to classify
replace aedu_ci=16 if ultcurso==7 & ultserie==0 & terult!=1

*** what if series==0 but terult==1  (we give them the usual diploma years)
*** should we check the variable eraseria  -- este curso que asistio era seriado?

replace aedu_ci=4 if ultcurso==1 & ultserie==0 & terult==1
replace aedu_ci=8 if ultcurso==2 & ultserie==0 & terult==1
replace aedu_ci=11 if ultcurso==3 & ultserie==0 & terult==1 
replace aedu_ci=8   if ultcurso==4 & ultserie==0 & terult==1
replace aedu_ci=11 if ultcurso==5 & ultserie==0 & terult==1 
replace aedu_ci=15 if ultcurso==6 & ultserie==0 & terult==1 
replace aedu_ci=18 if ultcurso==7 & ultserie==0 & terult==1
save "$ruta\survey\BRA\PNAD\1996\m9\data_orig\Noasiste.dta", replace

restore
keep if asiste_ci==1
*** for those  currently enrolled in school  (if answer both use not enrolled)
*** drop if answered yes to in school and was in school
	replace aedu_ci=. if cursoasi==-1 | cursoasi==6
** pre-escolar
	replace aedu_ci=0 if cursoasi==7 & anio_c<2001
	replace aedu_ci=0 if (cursoasi==7 | cursoasi==8) & anio_c>=2001
** pre-vestibular
	replace aedu_ci=11 if cursoasi==8 & anio_c<2001
	replace aedu_ci=11 if cursoasi==9 & anio_c>=2001
** regular primeiro grau  (** note this creates a zero if in 1st year of 1 grau)
	replace aedu_ci=(serieasi-1) if cursoasi==1 & serieasi>=1 & serieasi<=8
** regular segundo grau
	replace aedu_ci=min(serieasi+7,10) if cursoasi==2 & serieasi>=1 & serieasi<=4
** supletivo de primeiro grau
	replace aedu_ci=(serieasi-1) if cursoasi==3 & serieasi>=1 & serieasi<=8
** supletivo de segundo grau
	replace aedu_ci=min(serieasi+7,10) if cursoasi==4 & serieasi>=1 & serieasi<=4
** superior
	replace aedu_ci=min(serieasi+10,16) if cursoasi==5 & serieasi>=1 & serieasi<=8
** maestria o doctorado  (??? why set to 17 -- see algorithm)
	replace aedu_ci=17 if cursoasi==9 /** check if we missed some diploma year  (valid grau but 0 for serie)*/

replace aedu_ci=0  if cursoasi==1 & serieasi==0 & termino==0
replace aedu_ci=8  if cursoasi==2 & serieasi==0 & termino==0
replace aedu_ci=0  if cursoasi==3 & serieasi==0 & termino==0
replace aedu_ci=8  if cursoasi==4 & serieasi==0 & termino==0
replace aedu_ci=11 if cursoasi==5 & serieasi==0 & termino==0
replace aedu_ci=11 if cursoasi==8 & serieasi==0 & termino==0
replace aedu_ci=15 if cursoasi==9 & serieasi==0 & termino==0
replace aedu_ci=(15+serieasi) if cursoasi==9 & serieasi>0 & serieasi<6
*** top code at 20 years
replace aedu_ci=20 if cursoasi==9 & serieasi>0 & serieasi<9
save "$ruta\survey\BRA\PNAD\1996\m9\data_orig\Asiste.dta", replace
append using "$ruta\survey\BRA\PNAD\1996\m9\data_orig\Noasiste.dta"
*/
** zero if never attended school and invalid anwsers for all other school questions
replace aedu_ci=0 if (asisant==4) & (asist==4) & (ultcurso<1 | ultcurso>7) & (ultserie==0 | ultserie==9)

** zero if 
replace aedu_ci=0 if (asist==4) & (asisant==4) & (cursoasi<1 | cursoasi==6 | cursoasi==7) & (serieasi==0 | serieasi==9)

** zero if never attended and it is not even attending now
replace aedu_ci=0 if ultcurso==-1 & cursoasi==-1

replace aedu_ci=. if asisant==9

tab aedu_ci if ultcurso==7 & ultserie==0

** missing combinations
replace aedu_ci=. if asist==9 & asisant==9
replace aedu_ci=. if asist==0 & asisant==0
replace aedu_ci=. if asist==0 & asisant==9
replace aedu_ci=. if asist==9 & asisant==0

** children younger than 5 should be missing
replace aedu_ci=. if edad_ci<5

replace aedu_ci=. if aedu_ci==99
replace aedu_ci=. if edad_ci<5


/*
PROGRAMACION VIEJA:
gen aedu_ci=.
replace aedu_ci=serieasi if cursoasi==1 
replace aedu_ci=ultserie if ultcurso==1 | ultcurso==4
replace aedu_ci=9 if (cursoasi==2 & serieasi==1)| ultcurso==2 | (ultcurso==5 & ultserie==1)
replace aedu_ci=10 if cursoasi==2 & serieasi==2 | ultcurso==3 | (ultcurso==5 & ultserie==2)
replace aedu_ci=10+serieasi if cursoasi==5 
replace aedu_ci=10+ultserie if ultcurso==6
replace aedu_ci=14+ultserie if ultcurso==7

if `i'<2001{
replace aedu_ci=14+serieasi if cursoasi==9 
replace aedu_ci=0 if (cursoasi==7 | cursoasi==8) | ultcurso==9
}
if `i'>=2001{
replace aedu_ci=14+serieasi if cursoasi==10 
replace aedu_ci=0 if (cursoasi>=7 & cursoasi<=9) | (ultcurso==9 | ultcurso==10)
}
*/

/*
PROGRAMACION VIEJA: 
gen eduno_ci=(asist==4 & asisant==4)
gen edupi_ci=((cursoasi==1 & serieasi<8) | ((ultcurso==1 | ultcurso==4) & ultserie<8)) 
gen edupc_ci=((cursoasi==1 & serieasi==8) | ((ultcurso==1 | ultcurso==4) & ultserie==8))
gen edusi_ci=((cursoasi==2 & serieasi==1) | ultcurso==2 |(ultcurso==5 & ultserie==1))
gen edusc_ci=((cursoasi==2 & serieasi==2) | ultcurso==3 |(ultcurso==5 & ultserie==2))
gen eduui_ci=((cursoasi==5 & serieasi<=4) | (ultcurso==6 & (termino==3 | terult==3)))
gen eduuc_ci=((cursoasi==5 & serieasi>4) | (ultcurso==6 & terult==1))
gen edus1i_ci=((cursoasi==2 & serieasi==1) | (ultcurso==2 & terult==3))
gen edus1c_ci=((cursoasi==2 & serieasi==2) | (ultcurso==2 & terult==1) | (ultcurso==5 & ultserie==1))
gen edus2i_ci=((cursoasi==2 & serieasi==2) | (ultcurso==3 & terult==3))
gen edus2c_ci=((cursoasi==5) | (ultcurso==3 & terult==1) | (ultcurso==5 & ultserie==2))
gen edupre_ci=(cursoasi==7 | ultcurso==9)
gen eduac_ci=.

label var  eduno_ci "Sin Educacion"
label var  edupi_ci "Primaria Incompleta"		
label var  edupc_ci "Primaria Completa"		
label var  edusi_ci "Secundaria Incompleta"		
label var  edusc_ci "Secundaria Completa"
label var  eduui_ci "Universitaria o Terciaria Incompleta"		
label var  eduuc_ci  "Universitaria o Terciaria Completa"
*/


gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

* Modificaciones Marcela Rubio Septiembre 2014
/*
gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<4
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"
*/

gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<8
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

/*
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==4
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"
*/
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==8
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

/*
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>4 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"
*/
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>8 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

** SE DEFINE PRIMER CICLO DE SECUNDARIA LOS PRIMEROS 4 DE 7 ANIOS **

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>4 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 


gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==8
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo" 

***

** SE DEFINE SEGUNDO CICLO DE SECUNDARIA LOS ULTIMOS 3 DE 7 ANIOS **

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>8 & aedu_ci<11
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==11
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo" 

***
/*
gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<15
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"
*/

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

/*
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=15
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"
*/

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


foreach var of varlist edu* {
replace `var'=. if aedu_ci==.
}

gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci = .

gen repite_ci=.
gen edupub_ci=.
label var  aedu_ci "Anios de Educacion"



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


*variables que faltan generar
gen tcylmpri_ci=.
gen tcylmpri_ch=.

gen repiteult_ci=.
gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.
gen ylmho_ci=. 
gen vivitit_ch=.
gen compu_ch=.
gen internet_ch=.

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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename ocuprin codocupa
rename ramprin codindustria

compress

saveold "`base_out'", version(12) replace


log close
