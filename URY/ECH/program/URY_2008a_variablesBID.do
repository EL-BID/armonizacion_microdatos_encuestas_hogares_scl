

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
local ANO "2008"
local ronda a 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Uruguay
Encuesta: ECH
Round: a
Autores: 
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
 Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
 Daniela Zuluaga (DZ) E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com - Octubre de 2017
Versión 2021: Cesar Lins (SCL/GDI) - Marzo 2021


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear



/***************************************************************************************************************************
 							armonización 2008
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=correlat
gen idp_ci=nper
gen factor_ch=pesoan
*A partir de 2006 hay zona rural.

gen zona_c=.
replace zona_c=1 if region == 1 | region == 2
replace zona_c=0 if region == 3
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


gen str3 pais_c="URY"
gen anio_c=2008

destring dpto, replace
gen region_c = dpto
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
gen mes_c=mes

/*
e32
1	Jefe
2	Esposo o compañero
3	Hijo de ambos
4	Hijo sólo del jefe
5	Hijo sólo del cónyuge
6	Yerno o nuera
7	Padre
8	Suegro
9	Hermano
10	Cuñado
11	Nieto
12	Otro pariente
13	Otro no pariente
14	Servicio doméstico o familiar del mismo
*/

gen relacion_ci=e32
replace relacion_ci=3 if e32==4 | e32==5
replace relacion_ci=4 if e32>=6 & e32<=14
replace relacion_ci=5 if e32==13
replace relacion_ci=6 if e32==14
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


/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	



*******************
*aguared_ch.    *
*******************

gen aguared_ch=(d11==1)
replace aguared_ch =. if d11==.


*****************
*aguafconsumo_ch*
*****************

gen aguafconsumo_ch =.
replace aguafconsumo_ch = 1 if d11==1 & d14_1==1
replace aguafconsumo_ch = 2 if (d11==1 & d14_1>1) | d11==2
replace aguafconsumo_ch = 4 if d11==4
replace aguafconsumo_ch = 6 if d11==5
replace aguafconsumo_ch = 8 if d11==6
replace aguafconsumo_ch = 9 if d11==3 
replace aguafconsumo_ch = 10 if d11==7



*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.
replace aguafuente_ch = 1 if d11==1 & d14_1==1
replace aguafuente_ch = 2 if (d11==1 & d14_1>1) | d11==2
replace aguafuente_ch = 4 if d11==4
replace aguafuente_ch = 6 if d11==5
replace aguafuente_ch = 8 if d11==6
replace aguafuente_ch = 9 if d11==3 
replace aguafuente_ch = 10 if d11==7



*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch= 1 if d14_1==1
replace aguadist_ch= 2 if d14_1==2
replace aguadist_ch= 3 if d14_1==3


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch = 9
*label var aguadisp1 "= 9 la encuesta no pregunta si el servicio de agua es constante"


**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9
*label var aguadisp2_ch "= 9 la encuesta no pregunta si el servicio de agua es constante"


*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10
*label var aguamala_ch "= 1 si la fuente de agua no es mejorada"

*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7


*****************
*aguamide_ch*  Altered
*****************

*96. El hogar usa un medidor para pagar por su consumo de agua
gen aguamide_ch=.


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=0
replace bano_ch=1 if d18_1==1 
replace bano_ch=2 if d18_1==2 
replace bano_ch=6 if d18_1==4
replace bano_ch=4 if d18_1==3 



*****************
*banoex_ch         *  Altered
*****************
*101. El servicio higiénico es de uso exclusivo del hogar
gen banoex_ch=1 if d17==1
replace banoex_ch=0 if d17==2


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

************
*sinbano_ch*
************
gen sinbano_ch =3
replace sinbano_ch = 0 if d16>0
*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"

*La principal fuente de iluminación es electricidad
gen luz_ch=.
replace luz_ch=1 if d19_1==1 | d19_1==2
replace luz_ch=0 if d19_1==3 | d19_1==4 | d19_1==5



*Medidor de luz
gen luzmide_ch=.

*El combustible principal usado en el hogar es gas o electricidad

/*
d21	1	Energía eléctrica (U.T.E)
	2	Energía eléctrica (grupo electrógeno)
	3	Gas por cañería
	4	Supergás
	5	Queroseno
	6	Leña
	7	Ninguna

*/

gen combust_ch=(d21>=1 & d21<6)
replace combust_ch =. if d21==.



gen des1_ch=.
replace des1_ch=0 if d15==3
replace des1_ch=1 if d18_1==1 | (d18_1==2 & d15==1)
replace des1_ch=2 if (d18_1==2 & d15==2)
replace des1_ch=3 if d18_1==3 



*Tipo de desagüe sin incluir la definición de unimproved de los MDG
gen des2_ch=.
replace des2_ch= 0 if des1_ch==0
replace des2_ch= 1 if des1_ch==1 | des1_ch==2 | des1_ch==3
replace des2_ch= 2 if d18_1 ==4

*Materiales de construcción del piso
gen piso_ch=.
replace piso_ch=0 if c4==5
replace piso_ch=1 if c4==1 | c4==2 | c4==3 
replace piso_ch= 2 if c4 ==4
*Materiales de construcción de las paredes


gen pared_ch=.

replace pared_ch=0 if c2==6
replace pared_ch=1 if c2<6

*Materiales de construcción del techo

gen techo_ch=.
replace techo_ch=0 if c3 == 6
replace techo_ch=1 if c3 >= 1 & c3 <= 5

*Método de eliminación de los residuos
gen resid_ch=.


/*
d10	N°	Número de habitaciones para dormir
*/

gen dorm_ch=d10

/*
d9	N°	Número de habitaciones residenciales
*/

gen cuartos_ch=d9


*Cuarto de cocina


/*
d20	1	SI, privado de este hogar
	2	SI, compartido con otros hogares
	3	NO hay
*/

gen cocina_ch=.
replace cocina_ch = 1 if d20==1 | d20==2 
replace cocina_ch = 0 if d20==3

/*
d21_3		1 = Si / 2 = No		Refrigerador (con o sin frezer)
d21_4		1 = Si / 2 = No		Freezer (solo)
d21_5_1		1 = Si / 2 = No		TV color
d21_6		1 = Si / 2 = No		Radio
d21_14_1 	1 = Si / 2 = No		Microcomputador (incluye laptop)
d21_15		1 = Si / 2 = No		Conexión a internet
d21_16_1 	1 = Si / 2 = No		Teléfono
d21_17_1 	1 = Si / 2 = No		Celular
d21_18_1 	1 = Si / 2 = No		Automóvil o camioneta
*/

gen refrig_ch=.
replace refrig_ch= 1 if d22_3 ==1
replace refrig_ch= 0 if d22_3 ==2

gen freez_ch=.
replace freez_ch= 1 if d22_4 ==1
replace freez_ch= 0 if d22_4 ==2

gen auto_ch=.
replace auto_ch= 1 if d22_18_1 ==1
replace auto_ch= 0 if d22_18_1 ==2

gen telef_ch=.
replace telef_ch= 1 if d22_16_1 ==1
replace telef_ch= 0 if d22_16_1 ==2

gen compu_ch=.
replace compu_ch= 1 if d22_14_1 ==1
replace compu_ch= 0 if d22_14_1 ==2

gen internet_ch=.
replace internet_ch= 1 if d22_15_1 ==1 | d22_15_2 ==1
replace internet_ch= 0 if d22_15_1 ==2

gen cel_ch=.
replace cel_ch= 1 if d22_17_1 ==1
replace cel_ch= 0 if d22_17_1 ==2

/*
TENENCIA DE LA VIVIENDA		d8_1	1	Propietario de la vivienda y el terreno y la está pagando
					2	Propietario de la vivienda y el terreno y ya la pagó
					3	Propietario solamente de la vivienda y la está pagando
					4	Propietario solamente de la vivienda y ya la pagó
					5	Inquilino o arrendatario de la vivienda
					6	Ocupante con relación de dependencia
					7	Ocupante gratuito
					8	Ocupante sin permiso del propietario
				d8_2	$	Monto de la cuota de compra
				d8_3	$	Monto de la cuota de alquiler
*/

*Tipo de vivienda en la que reside el hogar

gen vivi1_ch=1 if c1==1
replace vivi1_ch=2 if c1==3 | c1==4
replace vivi1_ch=3 if c1==2 | c1==5


*La vivienda en la que reside es una casa o un departamento

gen vivi2_ch=1 if c1!=5
replace vivi2=0 if vivi2==.

*Propiedad de la vivienda

gen viviprop_ch=0 if d8_1==5
replace viviprop_ch=2 if d8_1==1 | d8_1==3
replace viviprop_ch=1 if d8_1==2 | d8_1==4
replace viviprop_ch=3 if d8_1==6 | d8_1==7 | d8_1==8 

*Alquiler mensual
gen vivialq_ch=d8_3 if viviprop_ch==0

*Alquiler mensual imputado
gen vivialqimp_ch=d8_3 if viviprop_ch~=0
gen vivitit_ch=.


/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/



gen factor_ci=pesoano

/*
e27	1	Hombre
	2	Mujer
*/

gen sexo_ci=e27

/*
e28	Años	Años cumplidos
*/

gen edad_ci=e28

/*
ESTADO CIVIL ACTUAL	e40	1	Divorciado/a
				2	Casado/a (incluye separado y aún no se divorció)
				3	Viudo/a
				4	Soltero/a
				5	Separado/a de unión libre


gen civil_ci=1 		if e40==4
replace civil_ci=2 	if e40==2
replace civil_ci=3 	if e40==1 | e40==5
replace civil_ci=4 	if e40==3
*/

*Modificado por SCGR - Abril 2017
*Unión formal o informal*
gen civil_ci=2 		if e37==1
replace civil_ci=1  if e40==4 & e37==2
replace civil_ci=3  if (e40==1 | e40==2 | e40==5) & e37==2
replace civil_ci=4 	if (e40==3) & e37==2

label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci


*************
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

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	
													
    ***************
	***afroind_ci***
	***************
**Pregunta: ¿Cree tener ascendencia...? ¿Cuál considera principal de las declaradas?:(e31_6) (1 - Afro o Negra; 2 - Asiatica o Amarilla; 3 - Blanca; 4 - Indigena; 5 - Otra) 
**En Uruguay puedes reportar más de una identidad pero la pregunta e31_6 pregunta cuál es la identidad principal. 
**Nota: Hay 7000 mv en la variable e31_6, con este código se le asigna el valor de las variables individuales si solo seleccionó una raza (e31_1, e31_2...)
******* Si seleccionó más de una raza se le asigna la raza con mayor número de personas. 

gen afroind_ci=. 
replace afroind_ci=1 if e31_6 == 4
replace afroind_ci=1 if e31_6 == 0 & e31_4==1 
replace afroind_ci=2 if e31_6 == 1 
replace afroind_ci=2 if e31_6 == 0 & e31_1==1 
replace afroind_ci=3 if afroind_ci==.

**replace afroind_ci=. if e31_6 ==. | e31_6 == 0


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2008


	*******************
	***dis_ci***
	*******************
gen dis_ci=. 


	*******************
	***dis_ch***
	*******************
gen dis_ch=. 

/******************************/
/*VARIABLES DE DEMANDA LABORAL*/
/******************************/
****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if pobpcoac==2
replace condocup_ci=2 if pobpcoac>=3 & pobpcoac<=5
replace condocup_ci=3 if pobpcoac>=6 & pobpcoac<=11
replace condocup_ci=4 if edad_ci<14
*/
*/
gen condocup_ci=.
*replace condocup_ci=1 if f68==1  | f69==1 | f70==1
replace condocup_ci=1 if f68==1  | f69==1 | (f70==1  & f71!=.) /* MLO, 2015,11 : se incluye condicion que no trabajo por razones extraordinarias*/
replace condocup_ci=2 if f109==1 & f110==1 & (f113>=1 & f113<=6) /* SGR, Modificado Mayo 2017 */
replace condocup_ci=2 if f109==1 & f110==2 & ((f111==2 | f111==3) | ((f111==1 | f111==4 | f111==5 | f111==6) & f112==1 & (f113>=1 & f113<=6))) /* SGR, Modificado Mayo 2017 */
*replace condocup_ci=2 if f109==1 & [(f113>=1 & f113<=6) | ((f111>=1 & f111<=3) & (f113>=1 & f113<=6))]
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14

label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************

gen policial=(e43_3==1) 
gen militar =(e43_4==1) 
gen bps     =(e43_6==1) 
gen iamc    =(e43_7==1 & e45_1==3) 

gen afiliado_ci=(policial==1 | militar==1 | bps==1 | iamc==1)
replace afiliado_ci=. if policial==. & militar==. & bps==. & iamc==.
label var afiliado_ci "Afiliado a la Seguridad Social"
drop policial militar bps iamc
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.
****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*cotizando_ci***
****************
gen cotizando_ci=0 if condocup_ci==1 | condocup_ci==2
replace cotizando_ci=1 if (f85==1 | f99==1) & cotizando_ci==0
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (f85==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (f99==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

****************
*instpen_ci*****
****************
gen instpen_ci= .
label var instpen_ci "Institucion a la cual esta afiliado variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=f86
replace instcot_ci=. if instcot_ci==0
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
gen tamemp_ci=f80
replace tamemp_ci=. if f80==0
label define tamemp_ci 1"una persona" 2"2-4 personas" 3"5-9 personas" 4 "10-49 personas" 5"50 o más" 6"10-19 personas" 7"20-49 personas"
label value tamemp_ci tamemp_ci
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
*************
*ypen_ci*
*************
egen yjub=rsum(g153_1 g153_2 g153_3 g153_4 g153_5 g153_6 g153_7 g153_8 g153_9 g153_10) 
egen ypen=rsum(g153_12 g153_13 g153_14 g153_15 g153_16 g153_17 g153_18 g153_19 g153_20 g153_21) 

egen ypen_ci =rsum(yjub ypen)
replace ypen_ci=. if yjub==. & ypen==.
drop yjub ypen
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci=(ypen_ci>0 & ypen_ci!=.)
label var pension_ci "1=Recibe pension contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************
/*DZ Octubre 2017- Se crea variable pension subsidiada* Dado que la pregunta es excluyente y el programa de pensión subsidiada en Uruguay es para Adultos mayores y/o discapacitados
se pone la condicion de mayor de 70 años (edad para recivir el beneficio) en las personas que afirmaron tener pension por invalidez*/
gen pensionsub_ci= ((f128==1) | (f128==3 & edad_ci>69))
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*************
* cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if f119==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci = 4320.37133602876 if mes==1 & region == 1
replace lp_ci = 4374.37400651563 if mes==2 & region == 1
replace lp_ci = 4399.01843922295 if mes==3 & region == 1
replace lp_ci = 4416.45751257989 if mes==4 & region == 1
replace lp_ci = 4456.72949101979 if mes==5 & region == 1
replace lp_ci = 4525.91434153287 if mes==6 & region == 1
replace lp_ci = 4559.91443206779 if mes==7 & region == 1
replace lp_ci = 4589.51728966148 if mes==8 & region == 1
replace lp_ci = 4608.04101046886 if mes==9 & region == 1
replace lp_ci = 4642.85106513919 if mes==10 & region == 1
replace lp_ci = 4666.46975669258 if mes==11 & region == 1
replace lp_ci = 4698.41672353446 if mes==12 & region == 1

replace lp_ci = 2554.87666889725 if mes==1 & region == 2
replace lp_ci = 2587.04421469432 if mes==2 & region == 2
replace lp_ci = 2601.19548475876 if mes==3 & region == 2
replace lp_ci = 2612.44878560058 if mes==4 & region == 2
replace lp_ci = 2636.57138323119 if mes==5 & region == 2
replace lp_ci = 2678.80898630187 if mes==6 & region == 2
replace lp_ci = 2697.50145136943 if mes==7 & region == 2
replace lp_ci = 2714.20291607525 if mes==8 & region == 2
replace lp_ci = 2724.05068674502 if mes==9 & region == 2
replace lp_ci = 2746.93732963155 if mes==10 & region == 2
replace lp_ci = 2763.58421797523 if mes==11 & region == 2
replace lp_ci = 2784.38530874914 if mes==12 & region == 2

replace lp_ci = 1447.91888211255 if mes==1 & region == 3 
replace lp_ci = 1457.42532815149 if mes==2 & region == 3 
replace lp_ci = 1465.689685296   if mes==3 & region == 3 
replace lp_ci = 1470.95293856096 if mes==4 & region == 3 
replace lp_ci = 1483.1003754496  if mes==5 & region == 3 
replace lp_ci = 1500.86265215981 if mes==6 & region == 3 
replace lp_ci = 1511.05969897927 if mes==7 & region == 3
replace lp_ci = 1517.44514662998 if mes==8 & region == 3 
replace lp_ci = 1522.91131466729 if mes==9 & region == 3 
replace lp_ci = 1537.9495030638  if mes==10 & region == 3 
replace lp_ci = 1548.44166516341 if mes==11 & region == 3 
replace lp_ci = 1558.29806214626 if mes==12 & region == 3

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 1405.73784522145 if mes==1 & region == 1
replace lpe_ci = 1425.83833404991 if mes==2 & region == 1
replace lpe_ci = 1461.77802511878 if mes==3 & region == 1
replace lpe_ci = 1461.32417124732 if mes==4 & region == 1
replace lpe_ci = 1475.20804347201 if mes==5 & region == 1
replace lpe_ci = 1507.02766689862 if mes==6 & region == 1
replace lpe_ci = 1501.93924584833 if mes==7 & region == 1
replace lpe_ci = 1534.31573100443 if mes==8 & region == 1
replace lpe_ci = 1540.50254304103 if mes==9 & region == 1
replace lpe_ci = 1519.50171408178 if mes==10 & region == 1
replace lpe_ci = 1506.99478402721 if mes==11 & region == 1
replace lpe_ci = 1533.30768376254 if mes==12 & region == 1

replace lpe_ci = 1324.31904066793 if mes==1 & region == 2
replace lpe_ci = 1342.41109737042 if mes==2 & region == 2
replace lpe_ci = 1376.61080804744 if mes==3 & region == 2
replace lpe_ci = 1374.02302443605 if mes==4 & region == 2
replace lpe_ci = 1387.2302343497  if mes==5 & region == 2
replace lpe_ci = 1418.61647962718 if mes==6 & region == 2
replace lpe_ci = 1412.82138419475 if mes==7 & region == 2
replace lpe_ci = 1443.92232529874 if mes==8 & region == 2
replace lpe_ci = 1449.6188914585  if mes==9 & region == 2
replace lpe_ci = 1428.33938879174 if mes==10 & region == 2
replace lpe_ci = 1416.14327399986 if mes==11 & region == 2
replace lpe_ci = 1442.35135851256 if mes==12 & region == 2

replace lpe_ci = 1191.36257086216 if mes==1 & region == 3 
replace lpe_ci = 1206.54719434553 if mes==2 & region == 3
replace lpe_ci = 1238.23135999193 if mes==3 & region == 3 
replace lpe_ci = 1233.40564200481 if mes==4 & region == 3 
replace lpe_ci = 1245.25033785156 if mes==5 & region == 3
replace lpe_ci = 1276.19774641604 if mes==6 & region == 3 
replace lpe_ci = 1267.32515873458 if mes==7 & region == 3 
replace lpe_ci = 1293.48550820996 if mes==8 & region == 3 
replace lpe_ci = 1297.93509670696 if mes==9 & region == 3
replace lpe_ci = 1279.1391562465  if mes==10 & region == 3
replace lpe_ci = 1271.199835601   if mes==11 & region == 3 
replace lpe_ci = 1297.09595218812 if mes==12 & region == 3 

label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci = .
replace salmm_ci = 3416   if mes_c==1 | mes_c==2  | mes_c==3  | mes_c==4 | mes_c==5 | mes_c==6 
replace salmm_ci = 4150   if mes_c==7 | mes_c==8  | mes_c==9 | mes_c==10 | mes_c==11 | mes_c==12
label var salmm_ci	"Salario minimo legal 2008"
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


/*
CONDICION DE ACTIVIDAD ECONOMICA	pobpcoac	1	Menor de 14 años
							2	Ocupados
							3	Desocupados por primera vez
							4	Desocupados propiamente dichos
							5	Desocupados en seguro de paro
							6	Inactivo, realiza quehaceres del hogar
							7	Inactivo, estudiante
							8	Inactivo, rentista
							9	Inactivo, pensionista
							10	Inactivo, jubilado
							11	Inactivo, otro
*/

*gen emp_ci=(pobpcoac==2)

/*
TAREAS QUE PROPORCIONAN MAYORES INGRESOS	f67_1		Descripción
						f67-2	Cód.	Código
*/

***************************** REVISAR EL NUMERO DE DIGITOS


replace f73_2="." if f73_2=="X211"

destring f73_2, replace

* Modificacion MGD 07/15/2014: correccion del grupo 9.
gen ocupa_ci=.
replace ocupa_ci=1 if (f73_2>=2110 & f73_2<=3480) & emp_ci==1
replace ocupa_ci=2 if (f73_2>=1110 & f73_2<=1310) & emp_ci==1
replace ocupa_ci=3 if (f73_2>=4110 & f73_2<=4223) & emp_ci==1
replace ocupa_ci=4 if ((f73_2>=5210 & f73_2<=5230) | (f73_2>=9110 & f73_2<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((f73_2>=5111 & f73_2<=5169) | (f73_2>=9120 & f73_2<=9172)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f73_2>=6110 & f73_2<=6210) | (f73_2>=9211 & f73_2<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((f73_2>=7110 & f73_2<=8340) | (f73_2>=9311 & f73_2<=9333)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f73_2>=110 & f73_2<=129) & emp_ci==1
replace ocupa_ci=9 if f73_2>=9999 & emp_ci==1

****************
*** rama_ci  ***
****************
/*
A QUE SE DEDICA EL ESTABLECIMIENTO DONDE REALIZA LAS TAREAS	f68_1		Descripción
								f74_2	Cód.	Código
*/

replace f74_2="." if f74_2=="." | f74_2=="+512" | f74_2=="<" | f74_2=="X211" | f74_2=="15 4"
destring f74_2, replace

gen rama_ci=.
replace rama_ci=1 if (f74_2>=100 & f74_2<=500) & emp_ci==1
replace rama_ci=2 if (f74_2>=1000 & f74_2<=1429) & emp_ci==1
replace rama_ci=3 if (f74_2>=1500 & f74_2<=3720) & emp_ci==1
replace rama_ci=4 if (f74_2>=4000 & f74_2<=4100) & emp_ci==1
replace rama_ci=5 if (f74_2>=4500 & f74_2<=4550) & emp_ci==1
replace rama_ci=6 if (f74_2>=5010 & f74_2<=5520) & emp_ci==1
replace rama_ci=7 if (f74_2>=6000 & f74_2<=6420) & emp_ci==1
replace rama_ci=8 if (f74_2>=6500 & f74_2<=7499) & emp_ci==1
replace rama_ci=9 if (f74_2>=7500 & f74_2<=9900) & emp_ci==1

*********************************************************************

/*
HORAS TRABAJADAS POR SEMANA	f88	Nº	Número de horas trabajadas por semana

CUANTAS HORAS TRABAJA EN OTRAS OCUPACIONES	f101	Nº	Número de horas que trabaja
*/

*********************************** REVISAR LAS VARIABLES DE INGRESO****************************
gen horaspri_ci=f88_1
replace horaspri_ci=. if  emp_ci==0 | horaspri_ci==99

*Horas totales trabajadas en todas las actividades
egen horastot_ci=rsum(horaspri_ci f101)
replace horastot_ci=. if f101==. & horaspri_ci==.

gen durades_ci=f116/4.3 if f116>0

gen antigenanio=(f89_1/12)
egen antiguedad_ci=rowtotal(antigenanio  f89_2)
*Mayra Sáenz-NO se encuentra la variable categ_ci  se cambia por condocup_ci.
recode antiguedad_ci 0=. if condocup_ci !=1

/*
gen desemp1_ci=((pobpcoac==3|pobpcoac==4|pobpcoac==5) & f110==1)

gen desemp2_ci=(desemp1_ci==1 | f110==2 & f111==2 | f111==3)

gen desemp3_ci=(desemp2_ci==1 | (f112==1 | f116>=4))

gen pea1_ci=(emp_ci==1 | desemp1_ci==1)

gen pea2_ci=(emp_ci==1 | desemp2_ci==1)

gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
gen desalent_ci=.

**************
* Subempleo 
**************
/*
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & (f105==1))
replace subemp_ci=. if emp_ci==0
*/
* Modificacion MGD 06/23/2014: horas de la actividad principaln y considerando disponibilidad (subempleo visible).
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30) & (f105==1 & f106==1)

* Mod. 2015/11 MLO
*gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & f105==2)
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30 & f105==2)
replace tiempoparc_ci=. if emp_ci==0

/*
CATEGORÍA DE LA OCUPACIÓN	f75	1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
					8	Programa público de empleo
*/

gen categopri_ci=1 	if f75==4
replace categopri_ci=2 	if f75==5 | f75==6 | f75==3
replace categopri_ci=3 	if f75==1 | f75==2 
replace categopri_ci=4 	if f75==7 
replace categopri_ci=. 	if emp_ci!=1

/*
CATEGORÍA DE LA OCUPACIÓN	f95	1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
*/


gen categosec_ci=1 if f95==4
replace categosec_ci=2 if f95==5 | f95==6 | f95==3
replace categosec_ci=3 if f95==1 | f95==2 
replace categosec_ci=4 if f95==7 

gen contrato_ci=.

gen segsoc_ci=.

/*
TRABAJOS QUE TIENE	f72	Nº	Número de trabajos que tiene
*/

gen nempleos_ci=1 if f72==1
replace nempleos_ci=2 if f72>1

/*
TAMAÑO  DE LA EMPRESA 	f80	1	Una persona
				2	2 a 4 personas
				3	5 a 9 personas
				4	10 a 49  personas
				5	50 o más personas
*/
/*

gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & f80==1 | f80==2
replace firmapeq_ci=0 if emp_ci==1 & f80>2*/

/*
CATEGORÍA DE LA OCUPACIÓN	f75/f85		2	Asalariado público
*/

gen spublico_ci=(emp_ci==1 & f75==2)
replace spublico =. if emp_ci==.

*Genera la variable para empresas pequeñas
*drop tamemp_ci
gen tamemp_ci=1 if f80==1 | f80==2 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if f80==4 | f80==3
*Empresas grandes
replace tamemp_ci=3 if f80==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if f127_1==1 | f127_2==1
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if f127_4==1
*Quehaceres del Hogar
replace categoinac_ci=3 if f127_5==1
*Otra razon
replace categoinac_ci=4 if f127_3==1 
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
tab categoinac_ci [iw=factor_ci]
*******************
***formal***
*******************
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

*****************************************************************************************************************
******************************************INGRESOS***************************************************************
*****************************************************************************************************************
*************
* ylmpri_ci *
*************
/*
ylmpri_ci			
SUELDO O JORNALES LÍQUIDOS	g129_1	$	Valor recibido el mes pasado
COMISIONES, INCENTIVOS, HORAS EXTRAS, HABILITACIONES	g129_2	$	Valor recibido el mes pasado
VIÁTICOS NO SUJETOS A RENDICIÓN	g129_3	$	Valor recibido el mes pasado
PROPINAS	g129_4	$	Valor recibido el mes pasado
AGUINALDO	g129_5	$	Valor recibido el mes pasado
SALARIO VACACIONAL	g129_6	$	Valor recibido el mes pasado
PAGOS ATRASADOS	g129_7	$	Valor recibido el mes pasado
DERECHO A CULTIVO PARA PROPIO CONSUMO	g137_3	$	Monto percibido por la venta de esos productos
RETIRO REALIZADO PARA GASTOS DEL HOGAR	g147	$	
DISTRIBUCIÓN DE UTILIDADES	g148	$	anual
RECIBIÓ POR MEDIANERÍA O PARCERÍA	g150	$	anual
RECIBIÓ POR PASTOREO	g151	$	anual
RECIBIÓ POR GANADO A CAPITALIZACIÓN	g152	$	anual
	

*/
foreach i in g148 g150 g151 g152 {
gen `i'm = `i'/12
}


egen ylmpri_ci=rsum(g129_1	g129_2 g129_3 g129_4 g129_5 g129_6 g129_7 g137_3 g147 g148m g150m g151m g152m) if emp_ci==1, missing

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

	****************
	***ylnmpri_ci***
	****************
	/*
	
	BOLETOS DE TRANSPORTE	g129_8	$	Valor recibido el mes pasado
	RECIBIÓ ALIMENTOS O BEBIDAS	g130_4	$	Otros - Monto estimado
	RECIBIÓ TICKETS DE ALIMENTACIÓN	g131_2	$	Valor recibido el mes pasado
	RECIBIÓ VIVIENDA O ALOJAMIENTO	g132_3	$	Monto que habría tenido que pagar por ese alojamiento
	RECIBIÓ OTRO TIPO DE RETRIBUCIÓN EN ESPECIE	g134_2	$	Monto que habría tenido que pagar por esos bienes
	RECIBIÓ ALGÚN OTRO TIPO DE COMPLEMENTO PAGADO POR EL EMPLEADOR	g135_2	$	Monto estimado
Autocons	DERECHO A CULTIVO PARA PROPIO CONSUMO	g137_2	$	Monto que habría tenido que pagar por esos alimentos
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador no agropecuario)	g149_2	$	Monto que habría tenido que pagar por esos bienes
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_3	$	Valor de lo consumido en carnes o chacinados
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_4	$	Valor de lo consumido en lácteos
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_5	$	Valor de lo consumido en huevos y aves
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_6	$	Valor de lo consumido en productos de la huerta
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_7	$	Valor consumido en otros alimentos

*/
gen desay=(g130_2*mto_desay)
gen almue= (g130_3*mto_almue)
gen cuota= (g133_2*mto_cuota) if g133_1==1
gen vacas = (g136_2*mto_vacas)
gen oveja = (g136_3*mto_oveja)
gen caballo = (g136_4*mto_caball)

*No costa la variable disse en el formulario.

egen ylnmpri_ci= rsum( desay almue cuota vacas oveja caballo g129_8 g130_4 g131_2 g132_3 g134_2 g135_2 g137_2 g149_2 g149_3 g149_4 g149_5 g149_6 g149_7) if emp_ci==1, missing
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

	***************
	***ylmsec_ci***
	***************
/*

SUELDO O JORNALES LÍQUIDOS	g138_1	$	Valor recibido el mes pasado
COMISIONES, INCENTIVOS, HORAS EXTRAS, HABILITACIONES	g138_2	$	Valor recibido el mes pasado
VIÁTICOS NO SUJETOS A RENDICIÓN	g138_3	$	Valor recibido el mes pasado
PROPINAS	g138_4	$	Valor recibido el mes pasado
AGUINALDO	g138_5	$	Valor recibido el mes pasado
SALARIO VACACIONAL	g138_6	$	Valor recibido el mes pasado
PAGOS ATRASADOS	g138_7	$	Valor recibido el mes pasado
DERECHO A CULTIVO PARA PROPIO CONSUMO	g146_3	$	Monto percibido por la venta de esos productos


*/

	egen ylmsec_ci=rsum(g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7 g146_3) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
	****************
	***ylnmsec_ci***
	****************
	/*

BOLETOS DE TRANSPORTE	g138_8	$	Valor recibido el mes pasado
RECIBIÓ ALIMENTOS O BEBIDAS	g139_4	$	Otros - Monto estimado
RECIBIÓ TICKETS DE ALIMENTACIÓN	g140_2	$	Valor recibido el mes pasado
RECIBIÓ VIVIENDA O ALOJAMIENTO	g141_3	$	Monto que habría tenido que pagar por ese alojamiento
RECIBIÓ OTRO TIPO DE RETRIBUCIÓN EN ESPECIE	g143_2	$	Monto que habría tenido que pagar por esos bienes
RECIBIÓ ALGÚN OTRO TIPO DE COMPLEMENTO PAGADO POR EL EMPLEADOR	g144_2	$	Monto estimado
Autoconsumo DERECHO A CULTIVO PARA PROPIO CONSUMO	g146_2	$	Monto que habría tenido que pagar por esos alimentos

        
*/

/*
RECIBIÓ ALIMENTOS O BEBIDAS	g139_1	1 = Sí / 2 = No	
	g139_2	Nº	Número de desayunos / meriendas
	g139_3	Nº	Número de almuerzos / cenas
*/

gen desaysec=(g139_2*mto_desay)
gen almuesec= (g139_3*mto_almue)
*gen cuota = En este año no se pregunta acerca de la cuota mutual.

/*
g145_2	Nº	Vacunos
g145_3	Nº	Ovinos
g145_4	Nº	Equinos
*/

gen vacassec = (g145_2*mto_vacas)
gen ovejasec = (g145_3*mto_oveja)
gen caballosec = (g145_4*mto_caball)


egen ylnmsec_ci=rsum(desaysec almuesec vacassec ovejasec caballosec g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2) if emp_ci==1, missing
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

	*****************
	***ylmotros_ci***
	*****************

	egen ylmotros_ci= rsum(g129_1 g129_2 g129_3 g129_4 g129_5 g129_6 g129_7 g137_3 g147 g148m g150m g151m g152m g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7 g146_3) if emp_ci==0, missing
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral no monetario otros trabajos para todos los años
	
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci= rsum( desay almue cuota vacas oveja caballo g129_8 g130_4 g131_2 g132_3 g134_2 g135_2 g137_2 g149_2 g149_3 g149_4 g149_5 g149_6 g149_7 desaysec almuesec vacassec ovejasec caballosec g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2) if emp_ci==0, missing
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral monetario otros como parte del ingreso laboral monetario total ya que no había sido incluido
	
	************
	***ylm_ci***
	************
	egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
	replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==. & ylmotros_ci==.
	label var ylm_ci "Ingreso laboral monetario total"  

	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral monetario otros como parte del ingreso laboral monetario total ya que no había sido incluido
	
	*************
	***ylnm_ci***
	*************
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  

	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral no monetario otros como parte del ingreso laboral no monetario total ya que no había sido incluido
	
	*************
	***ynlm_ci***
	*************
	/*
Recibió el mes pasado por JUBILACIONES:			
BPS - CAJA INDUSTRIA Y COMERCIO	g153_1	$	
BPS - CAJA CIVIL Y ESCOLAR	g153_2	$	
BPS - RURAL Y SERVICIO DOMÉSTICO	g153_3	$	
UNIÓN POSTAL	g153_4	$	
POLICIAL	g153_5	$	
MILITAR	g153_6	$	
PROFESIONAL	g153_7	$	
NOTARIAL	g153_8	$	
BANCARIA	g153_9	$	
OTRA 	g153_10	$	
OTRO PAÍS	g153_11	$	
Recibió el mes pasado por PENSIONES:			
BPS - CAJA INDUSTRIA Y COMERCIO	g153_12	$	
BPS - CAJA CIVIL Y ESCOLAR	g153_13	$	
BPS - RURAL Y SERVICIO DOMÉSTICO	g153_14	$	
UNIÓN POSTAL	g153_15	$	
POLICIAL	g153_16	$	
MILITAR	g153_17	$	
PROFESIONAL	g153_18	$	
NOTARIAL	g153_19	$	
BANCARIA	g153_20	$	
OTRA 	g153_21	$	
OTRO PAÍS	g153_22	$	
SEGURO DE DESEMPLEO	g153_23	$	
COMPENSACIONES POR ACCIDENTE, MATERNIDAD O ENFERMEDAD	g153_24	$	
BECAS, SUBSIDIOS, DONACIONES	g153_25	$	Del país
BECAS, SUBSIDIOS, DONACIONES	g153_26	$	Del extranjero
RECIBE PENSIÓN ALIMENTICIA O ALGUNA CONTRIBUCIÓN POR DIVORCIO O SEPARACIÓN	g156_2	$	Del país
	g156_3	$	Del extranjero
OTROS INGRESOS CORRIENTES	g157_2	$	

RECIBE DINERO DE ALGÚN FAMILIAR U OTRO HOGAR EN EL PAÍS	h158_2	$


* Variables anuales	a nivel de hogar		
FUERON ALQUILADAS 	h162_2	$	Alquileres del país a
FUERON ALQUILADAS 	h162_3	$	Alquileres del extranjero
RECIBIÓ POR ARRENDAMIENTO	h165_1	$	Arrendamientos del país
RECIBIÓ POR ARRENDAMIENTO	h165_2	$	Arrendamientos del extranjero
RECIBIÓ POR MEDIANERÍA	h166	$	
RECIBIÓ POR PASTOREO	h167	$	
RECIBIÓ POR GANADO A CAPITALIZACIÓN	h168	$	
RECIBIÓ POR INTERESES	h170_1	$	Intereses del país 
RECIBIÓ POR INTERESES	h170_2	$	Intereses del extranjero
RECIBIÓ POR UTILIDADES Y DIVIDENDOS DE ALGÚN NEGOCIO EN EL QUE NO TRABAJA	h172_1	$	Utilidades y dividendos del país
	h172_2	$	Utilidades y dividendos del extranjero
RECIBIÓ INDEMNIZACIÓN POR DESPIDO	h173_2	$	
 remesas -RECIBIÓ ALGUNA COLABORACIÓN ECONÓMICA DE ALGÚN FAMILIAR EN EL EXTERIOR	h174_2	$	
RECIBIÓ ALGÚN INGRESO EXTRAORDINARIO	h175_2	$	
           
*/

foreach i in h162_2 h162_3 h165_1 h165_2 h166 h167 h168 h170_1 h170_2 h172_1 h172_2 h173_2 h174_2 h175_2{
gen `i'm=`i'/12 /*Estos estan definidos en base anual!*/
}

bys idh_ch: egen numper = sum(miembros_ci)
bys idh_ch: egen npermax = max(numper)
drop numper
* Los ingresos a nivel de hogar se dividen para los miembros del hogar y se obtiene un per capita.
egen inghog1 = rsum(h158_2 h162_2m h162_3m h165_1m h165_2m h166m h167m h168m h170_1m h170_2m h172_1m h172_2m h173_2m h174_2m h175_2m), missing
gen inghog= inghog1/npermax

*Transferencias de programas sociales

/*
CUÁLES/CUÁNTAS CANASTAS RECIBE MENSUALMENTE	e61_1_1	1 = Sí / 2 = No	Común (INDA)
	e61_1_2	Nº	Número de veces que recibe la canasta al mes
	e61_2_1	1 = Sí / 2 = No	Bajo peso (riesgo nutricional)
	e61_2_2	Nº	Número de veces que recibe la canasta al mes
	e61_3_1	1 = Sí / 2 = No	Plomo
	e61_3_2	Nº	Número de veces que recibe la canasta al mes
	e61_4_1	1 = Sí / 2 = No	Pensionistas
	e61_4_2	Nº	Número de veces que recibe la canasta al mes
	e61_5_1	1 = Sí / 2 = No	Diabéticos
	e61_5_2	Nº	Número de veces que recibe la canasta al mes
	e61_6_1	1 = Sí / 2 = No	Renales
	e61_6_2	Nº	Número de veces que recibe la canasta al mes
	e61_7_1	1 = Sí / 2 = No	Renal-diabético
	e61_7_2	Nº	Número de veces que recibe la canasta al mes
	e61_8_1	1 = Sí / 2 = No	Celíacos
	e61_8_2	Nº	Número de veces que recibe la canasta al mes
	e61_9_1	1 = Sí / 2 = No	Tuberculosis
	e61_9_2	Nº	Número de veces que recibe la canasta al mes
	e61_10_1	1 = Sí / 2 = No	Oncológicos
	e61_10_2	Nº	Número de veces que recibe la canasta al mes
	e61_11_1	1 = Sí / 2 = No	Sida (VIH+)
	e61_11_2	Nº	Número de veces que recibe la canasta al mes
	e61_12_1	1 = Sí / 2 = No	Escolar contexto crítico
	e61_12_2	Nº	Número de veces que recibe la canasta al mes
	e61_13_1	Cód.	Otra
	e61_13_2	Nº	Número de veces que recibe la canasta al mes
	e61_13_3		Descripción

*/

gen canasta = (e61_1_2 * indacomun) + (e61_2_2	 * indabajo) + (e61_3_2 * indaplomo) + (e61_4_2 * indapensi) + ///
(e61_5_2 * indadiabet) + (e61_6_2 * indarenal) + (e61_7_2 * indarendia) + (e61_8_2 * indaceliac) + (e61_9_2 * indatuberc) + ///
(e61_10_2 * indaoncolo) + (e61_11_2 * indasida) + (e61_13_2 * otrcanast)

*CONCURRENCIA A COMEDOR O MERENDERO GRATUITO 

egen transf_desay= rsum(e59_2_1_1 e59_2_1_3 e59_2_2_1 e59_2_2_3 e59_2_3_1 e59_2_3_3 e59_2_4_1 e59_2_4_3 e59_2_5_1 e59_2_5_3 e59_2_6_1 e59_2_6_3), missing
gen tdesay = (transf_desay*4.3)*mto_desay

egen transf_almue= rsum(e59_2_1_2 e59_2_1_4 e59_2_2_2 e59_2_2_4 e59_2_3_2 e59_2_3_4 e59_2_4_2 e59_2_4_4 e59_2_5_2 e59_2_5_4 e59_2_6_2 e59_2_6_4), missing
gen talmue = transf_almue*4.3*mto_almue

/*
e59_2_7	7	Sólo almuerzo
	8	Sólo desayuno o merienda
	9	Desayuno y almuerzo
	10	Almuerzo y Merienda
	11	Desayuno, almuerzo y merienda
RECIBE ALGÚN TIPO DE ALIMENTACIÓN DE ALGÚN PROGRAMA PÚBLICO (SALVO CANASTAS)	

e60-1	1 = Si / 2 = No	
e60_2	Nº	Número de veces que recibe por semana
e60_3	Nº	Número de veces que recibe por mes
*/
gen tsolalmue = (5*4.3*mto_almue) if e59_2_7==7
gen tsoldesay = (5*4.3*mto_desay) if e59_2_7==8
gen tsoldesalm = (5*4.3*(mto_desay + mto_almue)) if e59_2_7==9 | e59_2_7==10
gen tsoldealme = (5*4.3*(mto_desay+ mto_desay + mto_almue)) if e59_2_7==11
gen salvcana = (e60_3*mto_almue)
replace salvcana= (e60_2*4.3*mto_almue) if e60_3==0

*HOGAR CONSTITUIDO	mto_hogcon	$	Valor del hogar constituido
*COBRA HOGAR CONSTITUIDO	g154_1	1 = Sí / 2 = No	
*g154_2	1 = Sí / 2 = No	Declarado en el sueldo

gen hogcosnt = mto_hogcon if g154_1==1 & g154_2==2

* Total transferencias
egen transf= rsum(canasta tdesay talmue tsolalmue tsoldesay tsoldesalm tsoldealme salvcana hogcosnt), missing

*Asignaciones familiares no declaradas en el sueldo
/*COBRA ASIGNACIONES FAMILIARES	g155_1	1 = Sí / 2 = No	
	g155_2	Nº	Cuántas
	g155_3	1 = Si / 2 = No	Declaradas en el sueldo
	g155_4	$	Monto que cobró el mes pasado
	*/

gen asigfam = g155_4 if g155_3 ==2

egen ynlm_ci=rsum(inghog transf asigfam g153_1 g153_2 g153_3 g153_4 g153_5 g153_6 g153_7 g153_8 g153_9 g153_10 g153_11 g153_12 g153_13 g153_14 g153_15 g153_16 g153_17 g153_18 g153_19 g153_20 g153_21 g153_22 g153_23 g153_24 g153_25 g153_26 g156_2 g156_3 g157_2), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	
	*RECIBE AYUDA EN ESPECIE DE ALGÚN FAMILIAR U OTRO HOGAR EN EL PAÍS	h159_2	$
	
	gen ynlnm_ci= (h159_2/npermax)
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci= (h174_2m/npermax)
	label var remesas_ci "Remesas mensuales reportadas por el individuo" 



		************************
		***INGRESOS DEL HOGAR***
		************************

	*****************
	***nrylmpri_ch***
	*****************
	by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
	replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
	replace nrylmpri_ch=. if nrylmpri_ch==.
	label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

	************
	***ylm_ch***
	************
	by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
	label var ylm_ch "Ingreso laboral monetario del hogar"

	*************
	***ylnm_ch***
	*************
	by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
	label var ylnm_ch "Ingreso laboral no monetario del hogar"

	**************
	***ylmnr_ch***
	**************
	by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
	replace ylmnr_ch=. if nrylmpri_ch==1
	label var ylmnr_ch "Ingreso laboral monetario del hogar"

	*************
	***ynlm_ch***
	*************
	by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
	label var ynlm_ch "Ingreso no laboral monetario del hogar"

	**************
	***ynlnm_ch***
	**************
	gen ynlnm_ch=.
	label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

	*****************
	***ylmhopri_ci***
	*****************
	gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri_ci)
	replace ylmhopri_ci=. if ylmhopri_ci<=0
	label var ylmhopri_ci "Salario monetario de la actividad principal" 

	**************
	***ylmho_ci***
	**************
	gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
	label var ylmho_ci "Salario monetario de todas las actividades" 

	*****************
	***rentaimp_ch***
	*****************
	gen rentaimp_ch=vivialqimp_ch
	label var rentaimp_ch "Rentas imputadas del hogar"

	*****************
	***autocons_ci***
	*****************
	egen autocons_ci= rsum(g137_2 g149_2 g149_3 g149_4 g149_5 g149_6 g149_7 g146_2), missing
	label var autocons_ci "Autoconsumo reportado por el individuo"
	
	*****************
	***autocons_ch***
	*****************	
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
la var autocons_ch "Autoconsumo del Hogar"

	****************
	***remesas_ch***
	****************
	by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
	label var remesas_ch "Remesas mensuales del hogar"	

/*
*Ingreso Laboral Monetario actividad principal
gen g148_ = g148/12
gen g150_ = g150/12
gen g151_ = g151/12
gen g152_ = g151/12

egen ylmpri_ci=rsum(g129_1 g129_2 g129_3 g129_4 g129_5 g129_6 g129_7    g148_ g150_ g151_ g152_) 
replace ylmpri_ci=. if g129_1==. & g129_2==. & g129_3==. & g129_4==. & g129_5==. & g129_6==. & g129_7==. &  g148_==. & g150_==. & g151_==. & g152_==. 

*Ingreso laboral no monetario actividad principal
egen ylnmpri_ci=rsum(g130_4 g131_2 g132_3 g134_2 g135_2 g137_2 g129_8) 

*Ingreso laboral actividad secundaria
egen ylmsec_ci=rsum(g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7)

*Ingreso laboral no monetario actividad secundaria
egen ylnmsec_ci=rsum(g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2)

*Ingreso laboral no monetario otros trabajos
gen ylmotros_ci=.

*Identificador de respuesta no responda del ingreso de la actividad principal
gen nrylmpri_ci=.

*Identificador top code del ingreso de la actividad principal
gen tcylmpri=.

*Ingreso laboral monetario total
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) 
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*Ingreso laboral no monetario total
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) 
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

*Ingreso no laboral monetario total
egen ynlm_ci = rsum(g153_1 g153_2 g153_3 g153_4 g153_5 g153_6 g153_7 g153_8 g153_9 g153_10 g153_11 g153_12 g153_13 g153_14 g153_15 g153_16 g153_17 g153_18 g153_19 g153_20 g153_21 g153_22 g153_23 g153_24 g153_25 g153_26 g155_4 g156_2 g156_3)

*Ingreso  no laboral no monetario
gen ynlnm_ci=.

*Identificador de no respuesta por hogar
gen nrylmpri_ch=.

*Identificador en donde alguno de los miembros reporta como top-code el ingreso de la actividad principal
gen tcylmpri_ch=.



sort idh_ch
*Ingreso laboral monetario del hogar
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
*Ingreso laboral no monetario del hogar
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
*Ingreso laboral monetario del hogar 2
gen ylmnr_ch=.


*Ingreso no laboral monetario del hogar
by idh_ch: egen ynlm_ch=sum(ynlm_ci) if relacion_ci!=6

*Ingreso no laboral no monetario del hogar
gen ynlnm_ch=.

*Salario monetario de la actividad principal
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)

replace ylmhopri_ci=. if ylmhopri_ci<=0

*Salario monetario de todas las actividades
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0


gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.

/*
egen remesas_ci=rsum(g5_2 g5_3 g5_4 g5_5 g5_6 g5_7 g5_8 g5_9 g5_10 g5_11)
replace remesas_ci=. if g5_2==. & g5_3==. & g5_4==. & g5_5==. & g5_6==. & g5_7==. & g5_8==. & g5_9==. & g5_10==. & g5_11==.

sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6
*/

gen remesas_ci=h174_2
gen remesas_ch=h174_2


*/

/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/

**************
***AEDU_CI****
**************

/*Para la suma de años educativos se generan variables temporales de maximo 
para niveles que son equivalentes, se imputa el maximo correspondiente 
a cada nivel de manera de no sobreestimar los años de educacion aprobados*/

egen cb_añostc = rowmax(e52_4 e52_7_1) if e52_7_2 == 3 /*computa el maximo de CB o tecnico con requisito primaria*/
egen bach_años = rowmax(e52_5 e52_6) /*computa el maximo de bachillerato o bachillerato tecnologico*/
egen bach_añostc = rowmax(e52_5 e52_6 e52_7_1) if e52_7_2 == 2 /*computa el maximo de bachillerato, bachillerato tecnologico o tecnica con requisito CB*/
egen sup_años = rowmax(e52_8 e52_9 e52_10) /*computa el maximo de superior: magisterio, universitario o terciario no universitario */
egen sup_añostc = rowmax(e52_8 e52_9 e52_10 e52_7_1) if e52_7_2 == 1 /*computa el maximo de superior: magisterio, universitario, terciario no universitario o tecnico con requisito bachillerato completo */

/* Se generan años aprobados para los niveles remplazando por missing 
el codigo de perdido (9) */
gen años_cb = e52_4 
replace años_cb = cb_añostc if e52_7_2 == 3
replace años_cb =. if años_cb == 9

gen años_bc = bach_años
replace años_bc = bach_añostc if e52_7_2 == 2
replace años_bc =. if años_bc == 9

gen años_sup = sup_años
replace años_sup = sup_añostc if e52_7_2 == 1
replace años_sup =. if años_sup == 9

gen años_prim = e52_2
replace años_prim =. if años_prim == 9

gen años_post = e52_11
replace años_post =. if años_post == 9

** Se genera aedu_ci 

gen aedu_ci = 0
qui foreach v of var años_prim años_cb años_bc años_sup años_post {

	replace aedu_ci = aedu_ci + `v' if !missing(`v')

}
replace aedu_ci =. if (años_prim==. & años_cb==. & años_bc==. & años_sup==. & años_post)
replace aedu_ci =. if e52_2 == 9
replace aedu_ci = floor(aedu_ci)

** eliminamos las variables temporales 
drop años_prim años_post cb_añostc bach_años bach_añostc sup_años sup_añostc años_cb años_bc años_sup

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

gen byte eduui_ci=(aedu_ci>12 & e52_8<4) | (aedu_ci>12 & e52_10<3) | (aedu_ci>12 & e52_9<4) // magisterio, profesorado, tecnica, universitaria
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta" 


***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>12 & e52_8>=4 & e52_8!=9) | (aedu_ci>12 & e52_10>=3 & e52_10!=9) | (aedu_ci>12 & e52_9>=4 & e52_9!=9) // magisterio, tecnica, universitaria
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

/* 
Para los casos en los cuales el respondiente imputa un nivel finalizado pero 
otro incompleto y por ende se pisan eduuc con eduui se le da prioridad al 
nivel completo.
*/
replace eduui_ci = 0 if eduuc_ci == 1


***************
***edupre_ci***
***************

gen edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
gen asispre_ci=.

***************
***eduac_ci***
***************

gen eduac_ci=.
replace eduac_ci = 0 if aedu_ci>12 & e52_8>0 & e52_8!=9 // magisterio o profesorado
replace eduac_ci = 0 if aedu_ci>12 & e52_10>0 & e52_10!=9 // terciario no univ
replace eduac_ci = 1 if aedu_ci>12 & e52_9>0 & e52_9!=9 // universidad o similar


***************
***asiste_ci***
***************

gen byte asiste_ci = (e50==1)

***************
**pqnoasis_ci**
***************

gen pqnoasis_ci=.
	
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci =.


**************
**repite_ci***
**************

gen repite_ci=.

**************
*repiteult_ci*
**************

gen repiteult_ci=.

**************
**edupub_ci***
**************

gen edupub_ci = 0 if (asiste_ci == 1 & e54 == 2)
replace edupub_ci = 1 if (asiste_ci == 1 & e54 == 1)

******************************
*** VARIABLES DE MIGRACION ***
******************************

	*******************
	*** migrante_ci ***
	*******************
	gen migrante_ci=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"

	**********************
	*** migrantiguo5_ci **
	**********************
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

rename f74_2 codindustria 
rename f73_2 codocupa 
compress

saveold "`base_out'",  replace


log close
