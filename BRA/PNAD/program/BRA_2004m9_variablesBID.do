
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
local ANO "2004"
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
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Última modificación: Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Octubre de 2017

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
/*    vARIABLES DEL HOGAR	*/
/********************************/

***************
****idh_ch*****
***************
sort uf v0102 v0103 v0403
egen idh_ch=group(uf v0102 v0103 v0403)
label variable idh_ch "ID del hogar"

gen idp_ci=v0301
gen factor_ch=v4611
gen zona_c=1 if v4728>=1 & v4728<=3
replace zona_c=0 if v4728>=4 & v4728<=8
gen str3 pais_c="BRA"
gen anio_c=2004
gen mes_c=9
gen relacion_ci=v0402
replace relacion_ci=5 if v0402==5|v0402==6|v0402==8
replace relacion_ci=6 if v0402==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

/************************************************************************/
/*			vARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(v0212==2 | v0213==1)
gen aguadist_ch=1 if v0211==1 |v0213==1
replace aguadist_ch=2 if v0214==2
replace aguadist_ch=3 if v0214==4
replace aguadist_ch=0 if v0214==9 
gen aguamala_ch=(v0212==6) /*"Otra"*/	
gen aguamide_ch=.
gen luz_ch=(v0219==1)
replace luz_ch=. if v0219==9
gen luzmide_ch=.
gen combust_ch=(v0223==1|v0223==2|v0223==5)
replace combust_ch=. if v0223==9
gen bano_ch=(v0215==1)
replace bano_ch=. if v0215==9
gen banoex_ch=(v0216==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.|v0216==9
gen des1_ch=1 if v0217>=1 & v0217<=3
replace des1_ch=2 if v0217==4
replace des1_ch=3 if v0217>=5
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if v0217==9

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
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales:otros"
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

gen resid_ch=0 if v0218==1 | v0218==2
replace resid_ch=1 if v0218==3
replace resid_ch=2 if v0218==4 | v0218==5
replace resid_ch=3 if v0218==6
replace resid_ch=. if v0218==9

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if v0212 == 2 | v0212 ==4
replace aguamejorada_ch = 0 if v0212 == 6
				
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if (v0215 == 1 & (v0217 >= 1 & v0217 <=3) & v0216 == 2 )
replace banomejorado_ch = 0 if (v0215 == 1 & (v0217 >= 1 & v0217 <=3) & v0216 == 4) | v0215 == 3 | (v0215 == 1 & (v0217 >= 4 & v0217<=7))

gen dorm_ch=v0206
replace dorm_ch=. if v0206==99 |v0206==-1
gen cuartos_ch=v0205
replace cuartos_ch=. if v0205==99 | v0205==-1
gen cocina_ch=.
gen refrig_ch=(v0228==2 |v0228==4)
replace refrig_ch=. if v0228==9
gen freez_ch=(v0229==1)
replace freez_ch=. if v0229==9
gen auto_ch=.
gen telef_ch=(v2020==2)
replace telef_ch=. if v2020==9
capture gen compu_ch=(v0231==1)
capture gen internet_ch=(v0232==2)
gen cel_ch=(v0220==2)
gen viv1_ch=1 if v0202==2
replace viv1_ch=2 if v0202==4
replace viv1_ch=3 if v0202==6
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
gen viviprop_ch=0 if v0207==3
replace viviprop_ch=1 if v0207==1
replace viviprop_ch=2 if v0207==2
replace viviprop_ch=4 if v0207>=4
replace viviprop_ch=. if v0207==9
gen vivialq_ch=v0208
replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0
gen vivialqimp_ch=.



/************************************************************************/
/*				vARIABLES DEMOGRAFICAS			*/
/************************************************************************/
****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"




gen factor_ci=v4611 /*AUN CUANDO HAY UN FACTOR DE PERSONAS ES IDENTICO AL DE HOGARES, EXCEPTO PARA EL '93 EN DONDE SE REGISTRAN vALORES NEGATIvOS! PARA HOMOGENEIZAR,A TODOS LES PONEMOS EL FACTOR DE EXPANSION DEL HOGAR*/
gen sexo_ci=1 if v0302==2
replace sexo_ci=2 if v0302==4
gen edad_ci=v8005
replace edad_ci=. if edad_ci==999
gen civil_ci=.
capture replace civil_ci=1 if v1001==3 & v1003==3 /*EN ALGUNOS AÑOS NO ESTA EL MODULO DE NUPCIALIDAD!*/
capture replace civil_ci=2 if v1001==1
capture replace civil_ci=3 if v1004==2
capture replace civil_ci=4 if v1004==4
gen jefe_ci=(v0402==1)
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


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	
												
	***************
	***afroind_ci***
	***************
**Pregunta: COR OU RACA? (v0404) (BRANCA 2, PRETA 4, AMARELA 6, PARDA 8, INDIGENA 0, IGNORADA 9) 

gen afroind_ci=. 
replace afroind_ci=1  if v0404==0
replace afroind_ci=2 if v0404 == 4 | v0404 == 8 
replace afroind_ci=3 if v0404 == 2 | v0404 == 6 
replace afroind_ci=. if v0404==9


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=1990


	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 

/******************************************************************************/
/*				vARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/

/************************************************************************************************************************/

****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (v9001==1 | v9002==2 | v9003==1 | v9004==2)
replace condocup_ci=2 if  v9004==4 & (v9115==1 & (v9119>=1 & v9119<=8)) /*tomaron alguna providencia en la semana de referencia*/
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
 
*************
*ypen_ci*
*************
*sum v1252 v1255 v1258 v1261
egen ypen_ci=rsum (v1252 v1255 v1258 v1261)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "valor de la pension contributiva"

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
segun la fuente, el monto bpc para adultos mayores fue de 260 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(v1273==260)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*****************
**  ypensub_ci  *
*****************
/*DZ Octubre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 260 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=v1273 if v1273==260
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

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

cap drop area
gen area=.
replace area=1 if zona_c==1
replace area=2 if zona_c==0
replace area=3 if v4727==1

label var area "area del pais"

*********
*lp_ci***
*********
gen lp_ci=.				
replace lp_ci=155.413626616596	if region==4	& area==1		/*sur-urbana*/
replace lp_ci=141.479991091213	if region==4	& area==2		/*sur-rural */
replace lp_ci=158.629080916332	if region==2	& area==1		/*noreste-urbana*/
replace lp_ci=141.479991091213	if region==2	& area==2		/*noreste-rural*/
replace lp_ci=124.330901279685	if region==3	& area==1		/*sudeste-urbano*/
replace lp_ci=106.109993332001	if region==3	& area==2		/*sudeste-rural*/
replace lp_ci=162.916353406591	if region==1	& area==1		/*norte-urbano*/
replace lp_ci=142.551809281736	if region==1	& area==2		/*norte-rural */
replace lp_ci=131.833628083272	if region==5	& area==1		/*centro oeste-urbano*/
replace lp_ci=115.756356367126	if region==5	& area==2		/*centro oeste-rural */
replace lp_ci=176.849988931974	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lp_ci=150.054536071731	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lp_ci=135.049082423783	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lp_ci=177.921806986581	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lp_ci=157.557262861726	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lp_ci=128.618173742761	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lp_ci=153.269990371467	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lp_ci=197.214533056829	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=162.916353406591	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lp_ci=140.408173036606	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci=184.352715721969	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=173.634534496322	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=138.264536791477	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=157.557262861726	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci=.				
replace lpe_ci=77.7068133082981	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=70.7399955456063	if region==4	& area==2		/*sur-rural */
replace lpe_ci=79.314540458166	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=70.7399955456063	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=62.1654506398426	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=53.0549966660006	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=81.4581767032955	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=71.275904640868	if region==1	& area==2		/*norte-rural */
replace lpe_ci=65.9168140416359	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=57.8781781835628	if region==5	& area==2		/*centro oeste-rural */
replace lpe_ci=88.4249944659872	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lpe_ci=75.0272680358654	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lpe_ci=67.5245412118913	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lpe_ci=88.9609034932903	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lpe_ci=78.7786314308628	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lpe_ci=64.3090868713804	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lpe_ci=76.6349951857333	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lpe_ci=98.6072665284147	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=81.4581767032955	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci=70.2040865183032	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci=92.1763578609846	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=86.8172672481608	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=69.1322683957385	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=78.7786314308628	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lpe_ci "Linea de indigencia oficial del pais"
drop area


*************
**salmm_ci***
*************
gen salmm_ci=260
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
replace ocupa_ci=1 if v4810==2 | v4810==3 & emp_ci==1
replace ocupa_ci=2 if v4810==1 & emp_ci==1
replace ocupa_ci=3 if v4810==4 & emp_ci==1
replace ocupa_ci=4 if v4810==6 & emp_ci==1
replace ocupa_ci=5 if v4810==5 & emp_ci==1
replace ocupa_ci=6 if v4810==7 & emp_ci==1
replace ocupa_ci=7 if v4810==8 & emp_ci==1
replace ocupa_ci=8 if v4810==9 & emp_ci==1 
replace ocupa_ci=9 if v4810==10 & emp_ci==1

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
label define rama_ci 1 "Agricultura, Caza, Civicultura y Pesca" 2 "Explotación de minas y Canteras" 3 "Industrias Manufactureras" 4 "Electricidad, Gas y Agua" 5 "Construcción" 6 "Comercio al por mayor y menor, Restaurantes y Hoteles" 7 "Transporte y Almacenamiento" 8 "Establecimientos Financieros, Seguros y Bienes Inmuebles" 9 "Servicios Sociales, Comunales y personales" 
label values rama_ci rama_ci


/*EN TODOS LOS AÑOS, EXCEPTO EN EL '96, '97 Y '01 SE DESCRIBEN LAS CONDICIONES LABORALES DE LOS NIÑOS DE ENTRE 5 Y 9 AÑOS.
ESTO QUIERE DECIR QUE HAY QUE TENER EN CUENTA QUE EN ESOS AÑOS LOS INGRESOS DE ESTOS NIÑOS vAN A vALER 0, CUANDO EN OTROS 
AÑOS TIENEN UN vALOR POSITIvO. PARA MANTENER LA COMPARABILIDAD ACROSS TIME, SOLO SE DEBEN AGARRAR LOS INGRESOS DE LOS 
MAYORES DE 10 AÑOS, A MENOS QUE SE vAYAN A EXCLUIR LOS AÑOS 1996 Y 1997! ==>> CREAMOS DOS vARIABLES DE INGRESO, UNA QUE LOS 
TIENE EN CUENTA (ej: ylmprik_ci) Y OTRA QUE NO (ylmpri_ci original)*/ 

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


*****************
**ylmpri_ci******
*****************

gen ylmpri_ci=v9532 
replace ylmpri_ci=. if v9532==-1 | v9532>=999999 | v4714!=1 

gen ylmprik_ci=v9532
replace ylmprik_ci=. if v9532==-1 | v9532>=999999 | emp_ci==0 
capture replace ylmprik_ci=v7122 if edad_ci>=5 & edad_ci<=9
capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (v7122==-1 | v7122>=999999 |emp_ci==0)

gen ylnmpri_ci=v9535 if edad_ci>=10
replace ylnmpri_ci=. if v9535==-1 | v9535>=999999 | v4714!=1

gen ylnmprik_ci=v9535
replace ylnmprik_ci=. if v9535==-1 | v9535>=999999 | emp_ci==0
capture replace ylnmprik_ci=v7125 if edad_ci>=5 & edad_ci<=9
capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (v7125==-1 | v7125>=999999 | emp_ci==0)

/*TODAS LAS vARIABLES "SECUNDARIAS": ylmsec_ci, ylnmsec_ci, ylmotros_ci, ylnmotros_ci Y durades_ci ESTAN CREADAS SÓLO PARA 
LOS MAYORES DE 10 AÑOS. POR LO TANTO LAS vARIABLES AGREGADAS CON SUFIJO k EN REALIDAD SÓLO SE REFIEREN A LA ACTIvIDAD 
PRINCIPAL DE LOS NIÑOS*/

gen ylmsec_ci=v9982 if edad_ci>=10
replace ylmsec_ci=. if v9982==-1 | v9982>=999999 | v4714!=1

gen ylnmsec_ci=v9985 if edad_ci>=10
replace ylnmsec_ci=. if v9985==-1 | v9985>=999999 | v4714!=1

gen ylmotros_ci=v1022 if edad_ci>=10
replace ylmotros_ci=. if v1022==-1 | v1022>=999999 | v4714!=1

gen ylnmotros_ci=v1025 if edad_ci>=10
replace ylnmotros_ci=. if v1025==-1 | v1025>=999999 | v4714!=1

gen nrylmpri_ci=(ylmpri_ci==. & v4714==1)
replace nrylmpri_ci=. if v4714==2

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


foreach var of varlist v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273{ 
replace `var'=. if `var'>=999999 | `var'==-1
}

egen ynlm_ci=rsum(v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273) if edad_ci>=10
replace ynlm_ci=. if (v1252==. &  v1255==. &  v1258==. &  v1261==. &  v1264==. &  v1267==. & v1270==. & v1273==.) | ynlm_ci<0

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

replace v1091=. if v1091==99 | v1091==-1
replace v1092=. if v1092==99 | v1092==-1


*Yanira Oviedo, Junio 2010: Se estaba multiplicando por 12, pero al ser un valor anual, debería dividirse 
/*gen aux1=v1091/12
egen durades_ci=rsum(aux1 v1092) if  v4714!=1 & edad_ci>=10
replace durades_ci=. if (v1091==. & v1092==.) */
*MLO 03,2014
gen durades_ci=.


replace v9611=. if v9611==99 | v9611==-1
replace v9612=. if v9612==99 | v9612==-1
gen aux2=v9612/12
egen antiguedad_ci=rsum(v9611 aux2) if emp_ci==1
replace antiguedad_ci=. if v9611==. & v9612==. 

drop aux*

/******************************************************************************************/
/*					vARIABLES DEL MERCADO LABORAL			  			*/
/******************************************************************************************/
gen desalent_ci=.
gen subemp_ci=.
gen tiempoparc_ci=.

gen categopri_ci=1 if v9029==4 | (v9008>=8 & v9008<=10)
replace categopri_ci=1 if v0708==4 | v0711==4
replace categopri_ci=2 if v0708==3 | v9029==3 |v0711==3 |(v9008>=5 & v9008<=7)
replace categopri_ci=3 if v0708==1 | v0708==2 |v9029==1 |v9029==2 |v0711==1 |v0711==2 | (v9008>=1 & v9008<=4)
replace categopri_ci=4 if (v0708>=5 & v0708<=8) |(v9029>=5 & v9029<=8) |(v0711>=5 & v0711<=8) | (v9008>=11 & v9008<=13)
replace categopri_ci=. if emp_ci!=1

gen categosec_ci=1 if v9092==4
replace categosec_ci=2 if v9092==3
replace categosec_ci=3 if v9092==1
replace categosec_ci=4 if v9092==5 |v9092==6
replace categosec_ci=. if emp_ci!=1 
gen nempleos_ci=1 if v9005==1
replace nempleos_ci=2 if v9005>1 & v9005!=.
/*
gen firmapeq=1 if v9008==1 & v9040<=4 /*v9008=Empleado permanente en el Agro*/
replace firmapeq=0 if v9008==1 & (v9040==6 | v9040==8) /*v9008=Empleado permanente en el Agro*/
replace firmapeq=1 if (v9008>=2 & v9008<=4) & ((v9013==1 & v9014<=6) | v9013==3) /*v9008= Algun tipo de empleado en el Agro*/
replace firmapeq=0 if (v9008>=2 & v9008<=4) & (v9013==1 & (v9014==8 | v9014==0)) /*v9008= Algun tipo de empleado en el Agro*/
replace firmapeq=1 if v9008==5 & ((v9049==1 & v9050<=6) | v9049==3) /*v9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=0 if v9008==5 & v9049==1 & v9050==8 /*v9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=1 if v9008==6 | v9008==7 /*Cuenta Propia en Agro o en otra actividad*/
replace firmapeq=0 if (v9008==8 | v9029==4) & ((v9048==0 | v9048==8) | ((v9048==2 | v9048==4) & v9049==1 & v9050>=6)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (v9008==8 | v9029==4) & ((v9048<=6 & v9049==3) | ((v9048==2 | v9048==4) & v9049==1 & v9050<=4)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (v9008==9 | v9008==10) & ((v9016==2 & v9017<=5 & v9018==4) | (v9016==2 & v9017<=3 & v9018==2 & v9019<=3) | (v9016==4 & v9018==2 & v9019<=5) | (v9016==4 & v9018==4)) /*Empleador en Agro u otras actividades*/
replace firmapeq=0 if (v9008==9 | v9008==10) & ((v9016==2 & (v9017==7 | v9017==8)) | (v9016==4 & v9018==2 & v9019>=5)) /*Empleador en Agro u otras actividades*/
replace firmapeq=1 if (v9008>=11 & v9008<=13) | (v9029>=5 & v9029<=7) /*Trabajador No remunerado*/
replace firmapeq=1 if v9029==1 & (v9032==2 & v9040<=4)  /*Empleado NO Agricola*/
replace firmapeq=0 if v9029==1 & (v9032==2 & (v9040==6 | v9040==8)) /*Empleado NO Agricola*/
/*Los empleados NO Agricolas que trabajan en el sector PUBLICO o que son empleados domesticos no tienen tamaño de firma!*/
replace firmapeq=1 if v9029==3 & (v9049==3 | (v9049==1 | v9050<=6))/*Cuenta Propia NO Agricola*/
replace firmapeq=0 if v9029==3 & (v9049==1 | (v9050==8 | v9050==0))/*Cuenta Propia NO Agricola*/
/*Que pasa con los trabajadores no remunerados? Se incluyen en tamaño de firma?*/

ren firmapeq firmapeq_ci
*cambio introducido el 06/13/05*
*/

gen spublico_ci=(v9032==4)
replace spublico_ci=. if v9032==9






					****************************
					***	vARIABLES EDUCATIvAS ***
					****************************

*------------------------------------------------------------------------------------------------------------------
*YANIRA, Ag 2010: SE HACE UNA CORRECIÓN SOBRE LAS vARIABLES DE EDUCACIÓN. PUES LA vARIABLE DE INSUMO PARA CONSTRUIR 
*AÑOS DE EDUCACIÓN NO SE TUvO EN CUENTA UN CAMBIO EN LAS OPCIONES DE LAS vARIABLES INSUMO. LO CUAL GENERÓ UN ERROR
*------------------------------------------------------------------------------------------------------------------


**************
**asiste_ci***
**************

gen asiste_ci=(v0602==2)
label var asiste_ci "Personas que actualmente asisten a un centro de enseñanza"


*************
***aedu_ci***
*************

/**Modificado Mayra Sáenz 12/10/2014
*gen aedu_ci=.
* Si se genera con . se generan alrededor de 10% de hogares con jefe de hogar con missing en educación.
gen aedu_ci=0
label var aedu_ci "Anios de educacion"


*PARA LOS QUE NO ASISTEN
*************************

*Pre-escolar, creche o alfabetización de adultos
replace aedu_ci=0 if (v0607==8| v0607==9 | v0607==10) & asiste_ci==0

	*Sistema antiguo
*Elementar (primário) - se asume que el máximo es 4 - Anteriormente se permitía 6 pero no 5
replace aedu_ci=0  if v0607==1 & v0610==. & v0611!=1 & asiste_ci==0
replace aedu_ci=min(v0610,4) if v0607==1 & v0610>=1 & v0610<=6 & asiste_ci==0
*Medio 1 ciclo (ginasial, etc) - se asume que el máximo es 8
replace aedu_ci=min(v0610+4,8) if v0607==2 & v0610>=1 & v0610<=5 & asiste_ci==0
replace aedu_ci=4  if v0607==2 & v0610==. & v0611!=1 & asiste_ci==0
*Medio 2 ciclo (cientifico, clasico, etc, etc) - se asume que el máximo es 11, pero
*bajo la lógica anterior deberían se 12, ya que se permite hasta 4 años adicionales en este nivel
*Aunque solo es necesario tener 11 años de educación para completar la secundaria
replace aedu_ci=min(v0610+8,12) if v0607==3 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8  if v0607==3 & v0610==. & v0611!=1 & asiste_ci==0

	*Sistema nuevo
*Primeiro grau - Bajo este sistema la primaria llega hasta el grado 8
replace aedu_ci=min(v0610,8) if v0607==4 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=0  if v0607==4 & v0610==. & v0611!=1 & asiste_ci==0
*Segundo grau - Secundaria son 4 años más
replace aedu_ci=min(v0610+8,12) if v0607==5 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8 if v0607==5 & v0610==. & v0611!=1 & asiste_ci==0

*Superior
replace aedu_ci=min(v0610+11,17) if v0607==6 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=11 if v0607==6 & v0610==. & v0611!=1 & asiste_ci==0

*Maestria o doctorado  
*Para este ciclo no se pregunta el último año aprobado. Por lo tanto se supone que si terminó el ciclo 
*el individuo cuenta con 19 años de educación (2 años más de educación), si el individuo no terminó se le agrega 
*1 año más de eduación para quedar con 18 ya que si el último ciclo más alto alcanzado es postgrado, el individuo 
*por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if v0607==7 & v0611==3 & asiste_ci==0
replace aedu_ci=19 if v0607==7 & v0611==1 & asiste_ci==0


*PARA LOS QUE ASISTEN
**********************

*Pre-escolar, creche o alfabetización de adultos
replace aedu_ci=0 if (v0603==6| v0603==7 | v0603==8) & asiste_ci==1

*Regular de 1º grau/ Supletivo de 1º grau   (se asume que el máximo es 8) 
replace aedu_ci=0  if (v0603==1 | v0603==3) & v0605==. & asiste_ci==1
replace aedu_ci=min(v0605-1,7) if (v0603==1 | v0603==3) & v0605>=1 & v0605<=8 & asiste_ci==1
*Regular de 2º grau/ Supletivo de 2º grau   (se asume que el máximo es 4, pero con 3 basta para completar el ciclo)
replace aedu_ci=min(v0605+8-1,11) if (v0603==2 | v0603==4) & v0605>=1 & v0605<=4 & asiste_ci==1
replace aedu_ci=8  if (v0603==2 | v0603==4) & v0605==. & asiste_ci==1

*Pre-vestibular
replace aedu_ci=11  if v0603==9 & asiste_ci==1

*Superior
replace aedu_ci=min(v0605+11-1,17) if v0603==5 & v0605>=1 & v0605<=8 & asiste_ci==1
replace aedu_ci=11 if v0603==5 & v0605==. & asiste_ci==1

*Maestria o doctorado  
*Si el último ciclo más alto alcanzado es postgrado, el individuo por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if v0603==10  & asiste_ci==1

*Se deja sólo la información de las personas con 5 años o más
replace aedu_ci=. if edad_ci<5

*/

*/

*Modificación Mayra Sáenz - Agosto 2015: Se incluyen las variables con los cambios sugeridos por 
*Iván Bornacelly de SCL/EDU : Consideramos que esto no es una argumento fuerte para asignarle 0 años de educación a 64,548 observaciones  Aproximadamente 15% de la muestra. Además que no tienen información en ninguna de las otras variables de educación.
*Todo tipo de educación fuera de las estructura tradicional de educación, tal como la educación especial o educación adulta se le asigna el valor de missing y no el valor de cero. 
*De acuerdo con la ley No 4.024 de 20 de marzo de Diciembre de 1961 - http://www.planalto.gov.br/ccivil_03/LEIS/L4024.htm - Complementada por la ley 5.692 de 11 de agosto de 1971 - http://www.planalto.gov.br/ccivil_03/LEIS/L5692.htm#art87
*Primaria es obligatoria por los menos 4 años, pero podría también durar entre 5 o 6  años con complemento. 

/*
*************
***aedu_ci***
*************
gen aedu_ci=.
label var aedu_ci "Anios de educacion"

*PARA LOS QUE NO ASISTEN
*************************
*Creche o alfabetización para adultos
replace aedu_ci=. if (v0607==8 | v0607==9)
*Pre-escolar
replace aedu_ci=0 if (v0607==10) & asiste_ci==0

*Sistema antiguo  Basado en la Ley 1961 con ajustes de 1971
*Elementar (primário)  Son obligatorios 4 años. Pueden llegar a ser 6. 
replace aedu_ci=0  if v0607==1 & v0610==. & v0611!=1 & asiste_ci==0
replace aedu_ci=v0610 if v0607==1 & v0610>=1 & v0610<=6 & asiste_ci==0
*Medio 1 ciclo (ginasial, etc) - se asume que quienes llegan a este nivel por lo menos hicieron cuatro años del anterior.
replace aedu_ci=v0610+4 if v0607==2 & v0610>=1 & v0610<=5 & asiste_ci==0
replace aedu_ci=4  if v0607==2 & v0610==. & v0611!=1 & asiste_ci==0
*Medio 2 ciclo (cientifico, clasico, etc, etc)  En este nivel son obligatorios 4 años también. No es importante el nivel máximo que se indique.  
replace aedu_ci=v0610+8 if v0607==3 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8  if v0607==3 & v0610==. & v0611!=1 & asiste_ci==0

	*Sistema nuevo
*Primeiro grau - Bajo este sistema la primaria comprende 8 años obligatorios
replace aedu_ci=v0610 if v0607==4 & v0610>=1 & v0610<=9 & asiste_ci==0
replace aedu_ci=0  if v0607==4 & v0610==. & v0611!=1 & asiste_ci==0
*Segundo grau - Secundaria son 4 años más
replace aedu_ci=v0610+8 if v0607==5 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8 if v0607==5 & v0610==. & v0611!=1 & asiste_ci==0

*Superior
replace aedu_ci=v0610+12 if v0607==6 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=12 if v0607==6 & v0610==. & v0611!=1 & asiste_ci==0

*Maestria o doctorado  
/*Para este ciclo no se pregunta el último año aprobado. Por lo tanto se supone que si terminó el ciclo 
el individuo cuenta con 19 años de educación (2 años más de educación), si el individuo no terminó se le agrega 1 año más de educación para quedar con 18 ya que si el último ciclo más alto alcanzado es postgrado, el individuo 
por lo menos tuvo que cursar 1 año en ese nivel
No se puede identificar si la persona tiene maestría o doctorado por separado. Se asume que el nivel educativo más alto posible logrado es Maestria que dura en promedio dos años.*/
replace aedu_ci=18 if v0607==7 & v0611==3 & asiste_ci==0
replace aedu_ci=19 if v0607==7 & v0611==1 & asiste_ci==0

*Creche o alfabetización para adultos
replace aedu_ci=. if (v0603==6 | v0603==7)

*Pre-escolar
replace aedu_ci=0 if (v0603==8) & asiste_ci==1

*Regular de 1º grau/ Supletivo de 1º grau  replace aedu_ci=0  if (v0603==1 | v0603==3) & v0605==. & asiste_ci==1
replace aedu_ci=v0605-1 if (v0603==1 | v0603==3) & v0605>=1 & v0605<=8 & asiste_ci==1
*Regular de 2º grau/ Supletivo de 2º grau   (se asume que el máximo es 4, pero con 3 basta para completar el ciclo)
*Dónde encuentran que con 3 años es suficientes para completar el ciclo
replace aedu_ci=v0605+8-1 if (v0603==2 | v0603==4) & v0605>=1 & v0605<=4 & asiste_ci==1
replace aedu_ci=8  if (v0603==2 | v0603==4) & v0605==. & asiste_ci==1

*Pre-vestibular
replace aedu_ci=12  if v0603==9 & asiste_ci==1

*Superior
replace aedu_ci=v0605+11 if v0603==5 & v0605>=1 & v0605<=8 & asiste_ci==1
replace aedu_ci=12 if v0603==5 & v0605==. & asiste_ci==1

*Maestria o doctorado  
*Si el último ciclo más alto alcanzado es postgrado, el individuo por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if v0603==10  & asiste_ci==1

*Se deja sólo la información de las personas con 5 años o más
replace aedu_ci=. if edad_ci<5
*/

*Modificado por Iván Bornacelly - 03/07/2017

gen nivel_asist=v0603
gen grado_asist=v0605
gen nivel_no_asist=v0607
gen grado_no_asist=v0610
gen finalizo=v0611
gen Ensino_8_9=.
gen finalizo_1=v0609
gen seria_asist=v0604
gen seria_no_asist=v0608
gen aedu_ci=.
label var aedu_ci "Anios de educacion"

*PARA LOS QUE ASISTEN:*
**********************
*Creche & Pre-escolar
replace aedu_ci=0 if nivel_asist==7 | nivel_asist==8 // Estudiantes de Prescolar y jardín no se les asigna años de educación.
*se incluyen en 0 los que asisten a alfabetización para adultos cambio hehco por Angela Lopez
replace aedu_ci=0 if nivel_asist==6

*Primaria / Básica - Nuevo sistema (Regular de ensino Fundamental grado 1)
*Se le resta 1 por que está asistiendo al grado que reporta, por lo tanto no se debe considerar dentro de los años de educación aprobados.
replace aedu_ci=grado_asist-1 if nivel_asist==1  


*Secundaria / Ensino Fundamental 2do Ciclo - Sistema Nuevo (Regular de ensino Fundamental grado 2)
replace aedu_ci=grado_asist+8-1 if nivel_asist==2

*Primaria / Básica - Supletivo
*Seriado
replace aedu_ci=grado_asist-1 if nivel_asist==3 & seria_asist==2 


*Secundaria  Ensino Fundamental 2do Ciclo - Supletivo
*Seriado
replace aedu_ci=grado_asist+8-1 if nivel_asist==4 & seria_asist==2


*Superior (Preuniversitario y Superior)
replace aedu_ci=12 if nivel_asist==9 // Preuniversitario
replace aedu_ci=grado_asist+12-1 if nivel_asist==5 // Universitario - No incluye Postgrados

*Maestrias/Doctorado
replace aedu_ci=12+5 if nivel_asist==10

*Quitando a quienes no se cuentan:
*replace aedu_ci=. if nivel_asist==6 // Educación para adultos. 

*Reemplazando por missing los que tienen como respuesta: Indenterminado (9)
replace aedu_ci=. if v0605==9

*PARA LOS QUE NO ASISTEN:*
**************************
*Creche & Pre-escolar
replace aedu_ci=0 if nivel_no_asist==9 | nivel_no_asist==10 // Estudiantes de Prescolar y jardín no se les asigna años de educación.
replace aedu_ci=0 if v0606 == 4

*Primaria / Elemental
*Termino 1er Año
replace aedu_ci=grado_no_asist if nivel_no_asist==1 & finalizo_1==1
*No terminó 1er año
replace aedu_ci=0 if nivel_no_asist==1 & finalizo_1==3

*Medio 1 // Se asume que son 4 años obligatorios. Pueden llegar a ser 6.
*Seriado -> Termino 1er Año 
replace aedu_ci=grado_no_asist+4 if nivel_no_asist==2 & seria_no_asist==2 & finalizo_1==1 
*Seriado -> No terminó 1er Año
replace aedu_ci=4 if nivel_no_asist==2 & seria_no_asist==2 & finalizo_1==3 
*No Seriado -> Si concluyó
replace aedu_ci=8 if nivel_no_asist==2 & seria_no_asist==4 & finalizo==1 
*No Seriado -> No concluyó
replace aedu_ci=4 if nivel_no_asist==2 & seria_no_asist==4 & finalizo==3 

*Medio 2 // Se asume que son 4 años obligatorios (Adicionales a los anteriores). Pueden llegar a ser 4
*Seriado -> Terminó 1er Año
replace aedu_ci=grado_no_asist+4+4 if nivel_no_asist==3 & seria_no_asist==2 & finalizo_1==1 
*Seriado -> No terminó 1er Año
replace aedu_ci=8 if nivel_no_asist==3 & seria_no_asist==2 & finalizo_1==3
*No Seriado -> Si concluyó
replace aedu_ci=12 if nivel_no_asist==3 & seria_no_asist==4 & finalizo==1
*No Seriado -> No concluyó
replace aedu_ci=8 if nivel_no_asist==3 & seria_no_asist==4 & finalizo==1

*Ensino Fundamental
*Eliminando indeterminados (9)
replace grado_no_asist=. if grado_no_asist==9 & nivel_no_asist==4
*Seriado -> Terminó 1er año
replace aedu_ci=grado_no_asist if nivel_no_asist==4 & seria_no_asist==2 & finalizo_1==1
*Seriado -> No terminó 1er Año
replace aedu_ci=0 if nivel_no_asist==4 & seria_no_asist==2 & finalizo_1==3
*No Seriado -> Si concluyó
replace aedu_ci=8 if nivel_no_asist==4 & seria_no_asist==4 & finalizo==1
*No Seriado -> No concluyó
replace aedu_ci=0 if nivel_no_asist==4 & seria_no_asist==4 & finalizo==3

*Ensino Medio // Se suman 8 años de Ensino Fundamental
*Eliminando indeterminados (9)
replace grado_no_asist=. if grado_no_asist==9 & nivel_no_asist==5
*Seriado -> Terminó 1er año
replace aedu_ci=grado_no_asist+8 if nivel_no_asist==5 & seria_no_asist==2 & finalizo_1==1
*Seriado -> No terminó 1er Año
replace aedu_ci=8 if nivel_no_asist==5 & seria_no_asist==2 & finalizo_1==3
*No Seriado -> Si concluyó
replace aedu_ci=12 if nivel_no_asist==5 & seria_no_asist==4 & finalizo==1
*No Seriado -> No concluyó
replace aedu_ci=8 if nivel_no_asist==5 & seria_no_asist==4 & finalizo==3

*Superior
*Termino 1er Año
replace aedu_ci=grado_no_asist+12 if nivel_no_asist==6 & finalizo_1==1
*No Termino 1er Año
replace aedu_ci=12 if nivel_no_asist==6 & finalizo_1==3

*Maestrado ou dooutorado
*Concluyó
replace aedu_ci=17+2 if nivel_no_asist==7 & finalizo==1
*No Concluyó
replace aedu_ci=17+1 if nivel_no_asist==7 & finalizo==3

*Quitando a quienes no se cuentan:
*replace aedu_ci=. if nivel_no_asist==8 // Educación para adultos. 

*Reemplazando por missing los que tienen como respuesta: Indenterminado (9)
replace aedu_ci=0 if v0610==9


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
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<11
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
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<16
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**************
***eduuc_ci***
**************
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=16
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

***************
***asispre_ci***
***************
*Creación de la variable asistencia a preescolar por Iván Bornacelly - 01/12/17
	g asispre_ci=.
	replace asispre_ci=1 if (v0603==7 | v0603==8) & v8005>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"

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
gen edupub_ci=(v6002==2)
label var  edupub_ci "Personas que asisten a centros de enseñanza públicos"







*******************
*** Brazil 2004 ***
*******************

** variables

clonevar	nrocont=v0102
clonevar	nroserie=v0103
clonevar	persa=v4724
clonevar	persb=v4725
clonevar	sexo=v0302
clonevar	piel=v0404
clonevar	edad=v8005
clonevar	factor=v4729
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
clonevar	ocup=v9906
clonevar	ramsec=v9991
clonevar	totpers=v0105
 gen condocup=v4705 if edad>=10 

 recode sexo (2=1) (4=2)

** AREA

 generate area=.
 replace area=2 if situacen>=4 & situacen<=8
 replace area=1 if situacen>=1 & situacen<=3
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


*variables que faltan generar
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen edus1c_ci=.
gen repiteult_ci=.
gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.
gen ylmho_ci=. 
gen vivitit_ch=.

ren ocup ocup_old

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename v9906 codocupa
rename v9907 codindustria

compress

                     
saveold "`base_out'", version(12) replace


log close



