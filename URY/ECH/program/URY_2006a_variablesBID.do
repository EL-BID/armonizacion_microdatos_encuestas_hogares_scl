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

local PAIS URY
local ENCUESTA ECH
local ANO "2006"
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
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Octubre de 2017


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear
/*******************
**********************/

*local base2006 "Y:\Uruguay\2006\Data\ury06.dta"


use `base_in', clear



/***************************************************************************************************************************
 							armonización 2006
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=correlat
gen idp_ci=nper
gen factor_ch=pesoan

*A partir de este año hay zona rural.

destring region, replace
gen zona_c=.
replace zona_c=1 if region >= 11 & region <= 29 
replace zona_c=0 if region >= 36 & region <= 39

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

gen str3 pais_c="URY"
gen anio_c=2006

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

gen mes_c=real(mes)

/*
e31
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

gen relacion_ci=e31
replace relacion_ci=3 if e31==4 | e31==5
replace relacion_ci=4 if e31>=6 & e31<=14
replace relacion_ci=5 if e31==13
replace relacion_ci=6 if e31==14
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

/*
d10	1	Red general
	2	Pozo surgente
	3	Aljibe
	4	Arroyo, rio
	5	Otro
*/

gen aguared_ch=(d10==1)
replace aguared_ch =. if d10==.

gen aguadist_ch=d13
replace aguadist_ch=0 if d13==3
* Nota Marcela G. Rubio
* Variable fué generado como missing. Pregunta cambió de la d6 a la d13 de cuestionario 2005 a 2006.

gen aguamala_ch=(d10==3|d10==3) 
replace aguamala_ch =. if d10==.

gen aguamide_ch=.

gen luz_ch=.

gen luzmide_ch=.

/*
d20	1	Energía eléctrica (U.T.E)
	2	Energía eléctrica (grupo electrógeno)
	3	Gas por cañería
	4	Supergás
	5	Queroseno
	6	Leña
	7	Ninguna
*/

gen combust_ch=(d20>=1 & d20<=6)
replace combust_ch =. if d10==.


/*
d14	1	Si. Con cisterna
	2	Si. Sin cisterna
	3	No

*/
gen bano_ch=(d14!=3)
replace bano_ch = . if d14==.

/*
d16	1	De uso exclusivo del hogar
	2	Compartido con otro hogar
*/


gen banoex_ch=.
replace banoex_ch=1 if d16==1
replace banoex_ch=0 if d16==2

gen des1_ch=.
replace des1_ch=0 if d15==3
replace des1_ch=1 if d17==1 | (d17==2 & d14==1)
replace des1_ch=2 if (d17==2 & d14==2)
replace des1_ch=3 if d17==3 

gen des2_ch=.
replace des2_ch= 0 if des1_ch==0
replace des2_ch= 1 if des1_ch==1 | des1_ch==2 | des1_ch==3
replace des2_ch= 2 if d17 ==4

/*
C4	1	Cerámica, parquet, moquete, linóleo
	2	Baldosas calcáreas
	3	Alisado de hormigón
	4	Solo contrapiso sin piso
	5	Tierra sin piso ni contrapiso
*/

gen piso_ch=.
replace piso_ch= 0 if c4 ==5
replace piso_ch= 1 if c4 ==1 |c4 ==2 | c4 ==3
replace piso_ch= 2 if c4 ==4


/*
MATERIAL PREDOMINANTE DE PAREDES EXTERNAS	C2	1	Ladrillos, ticholos o bloques terminados
		2	Ladrillos, ticholos o bloques sin terminar
		3	Materiales livianos con revestimiento
		4	Materiales livianos sin revestimiento
		5	Adobe
		6	Materiales de desecho
*/


gen pared_ch=.
replace pared_ch=0 if c2 == 6
replace pared_ch=1 if c2 >= 1 & c2 <= 5


/*
MATERIAL PREDOMINANTE DEL TECHO	C3	1	Planchada de hormigón con protección (tejas u otros)
		2	Planchada de hormigón sin protección
		3	Liviano con cielorraso
		4	Liviano sin cielorraso
		5	Quincha
		6	Materiales de desecho
*/

gen techo_ch=.
replace techo_ch=0 if c3 == 6
replace techo_ch=1 if c3 >= 1 & c3 <= 5

gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (d10 >=1 & d10 <=2)
replace aguamejorada_ch = 0 if (d10 >=3 & d10 <=5)

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (d14 ==1 | d14 ==2) & d16 ==1 & (d17 ==1 | d17 ==2)
replace banomejorado_ch = 0 if ((d14 ==1 | d14 ==2) & d16 ==2) | ((d14 ==1 | d14 ==2) & d17 ==2) | d14 ==3 | ((d14 ==1 | d14 ==2) & d16 ==1 & (d17 ==3 | d17 ==4))


/*
d9	N°	Número de habitaciones para dormir
*/

gen dorm_ch=d9
replace dorm_ch=. if d9==9
sum dorm*
/*
d8	N°	Número de habitaciones residenciales
*/

gen cuartos_ch=d8
replace cuartos_ch=. if d8==99
sum cuartos*

/*
d19	1	SI, privado de este hogar
	2	SI, compartido con otros hogares
	3	NO hay
*/

gen cocina_ch=.
replace cocina_ch = 1 if d19==1 | d19==2 
replace cocina_ch = 1 if d19==0

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
replace refrig_ch= 1 if d21_3 ==1
replace refrig_ch= 0 if d21_3 ==2

gen freez_ch=.
replace freez_ch= 1 if d21_4 ==1
replace freez_ch= 0 if d21_4 ==2

gen auto_ch=.
replace auto_ch= 1 if d21_18_1 ==1
replace auto_ch= 0 if d21_18_1 ==2

gen telef_ch=.
replace telef_ch= 1 if d21_16_1 ==1
replace telef_ch= 0 if d21_16_1 ==2

gen compu_ch=.
replace compu_ch= 1 if d21_14_1 ==1
replace compu_ch= 0 if d21_14_1 ==2

gen internet_ch=.
replace internet_ch= 1 if d21_15 ==1
replace internet_ch= 0 if d21_15 ==2

gen cel_ch=.
replace cel_ch= 1 if d21_17_1 ==1
replace cel_ch= 0 if d21_17_1 ==2

/*
TENENCIA DE LA VIVIENDA		d7_1	1	Propietario de la vivienda y el terreno y la está pagando
					2	Propietario de la vivienda y el terreno y ya la pagó
					3	Propietario solamente de la vivienda y la está pagando
					4	Propietario solamente de la vivienda y ya la pagó
					5	Inquilino o arrendatario de la vivienda
					6	Ocupante con relación de dependencia
					7	Ocupante gratuito
					8	Ocupante sin permiso del propietario
				d7_2	$	Monto de la cuota de compra
				d7_3	$	Monto de la cuota de alquiler
*/



/*
C1	1	Casa
	2	Apartamento o casa en complejo habitacional
	3	Apartamento en edificio de altura
	4	Apartamento en edificio de una planta
	5	Local no destinado a vivienda
*/

gen vivi1_ch=.
replace vivi1_ch=1 if c1==1
replace vivi1_ch=2 if c1>=2 & c1<=4
replace vivi1_ch=3 if c1==5

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if  vivi2_ch~=1

gen viviprop_ch=0 if d7_1==5
replace viviprop_ch=2 if d7_1==1 | d7_1==3
replace viviprop_ch=1 if d7_1==2 | d7_1==4
replace viviprop_ch=3 if d7_1==6 | d7_1==7 | d7_1==8 
gen vivialq_ch=d7_3 if viviprop_ch==0
gen vivialqimp_ch=d7_3 if viviprop_ch~=0
gen vivitit_ch=.

/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/


gen factor_ci=pesoan

/*
e26	1	Hombre
	2	Mujer
*/

gen sexo_ci=e26

/*
e27	Años	Años cumplidos
*/

gen edad_ci=e27
replace edad_ci=. if e27==99

/*
ESTADO CIVIL	e37	1	Divorciado
			2	Casado
			3	Viudo
			4	Soltero


gen civil_ci=1 		if e37==4
replace civil_ci=2 	if e37==2
replace civil_ci=3 	if e37==1
replace civil_ci=4 	if e37==3
*/


*Modificado por SCGR - Abril 2017
*Unión formal o informal*
gen civil_ci=2 		if e34==1
replace civil_ci=1  if e37==4 & e34==2
replace civil_ci=3  if (e37==1 | e37==2) & e34==2
replace civil_ci=4 	if (e37==3) & e34==2

label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci



/*
RELACION DE PARENTESCO	e31	1	Jefe
*/

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
**********
***raza***
**********

/*
ASCENDENCIA	e30_1	1 = Si / 2 = No	Afro o negra
	e30_2	1 = Si / 2 = No	Amarilla
	e30_3	1 = Si / 2 = No	Blanca
	e30_4	1 = Si / 2 = No	Indígena
	e30_5_1		Otro (descripción)
	e30_5_2	1 = Si / 2 = No	
	e30_6	1	No sabe
*/


gen raza_ci= .
replace raza_ci=1 if e30_4==1
replace raza_ci=2 if e30_1==1
replace raza_ci=3 if raza_ci== .

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña
gen raza_idioma_ci = .

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_ci==1 
label define id_ind_ci 1 "Ind?na" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2 
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 



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

*Nota: en el 2006 y 2007 la variable original "pobpcoac"p resenta problemas
gen condocup_ci=.
*replace condocup_ci=1 if f62==1  | f63==1 | f64==1
replace condocup_ci=1 if f62==1  | f63==1 | (f64==1  & f65!=.) /* MLO, 2015,11 : se incluye condicion que no trabajo por razones extraordinarias*/
replace condocup_ci=2 if f101==1 & f102==1 & (f105>=1&f105<=5) /* SGR, Modificado Mayo 2017 */
replace condocup_ci=2 if f101==1 & f102==2 & ((f103>=2&f103<=3) | ((f103==1 | f103==4 | f103==5) & f104==1 & (f105>=1&f105<=5))) /* SGR, Modificado Mayo 2017 */
*replace condocup_ci=2 if f101==1 & [(f105>=1 & f105<=5) | ((f103>=1 & f103<=3) & (f105>=1 & f105<=5))]
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen policial=(e42_3==1) 
gen militar =(e42_4==1) 
gen bps     =(e42_6==1) 
gen iamc    =(e42_7==1 & e44_1==3) 

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
replace cotizando_ci=1 if (f78==1 | f91==1) & cotizando_ci==0
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (f78==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (f91==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

****************
*instpen_ci*****
****************
gen instpen_ci= .
label var instpen_ci "Institucion a la cual esta afiliado variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=f79
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
gen tamemp_ci=f75
replace tamemp_ci=. if f80==0
label define tamemp_ci 1"una persona" 2"2-4 personas" 3"5-9 personas" 4 "10-49 personas" 5"50 o más" 6"10-19 personas" 7"20-49 personas"
label value tamemp_ci tamemp_ci
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
*************
*ypen_ci*
*************
egen yjub=rsum(g145_1 g145_2 g145_3 g145_4 g145_5 g145_6 g145_7 g145_8 g145_9 g145_10) 
egen ypen=rsum(g145_12 g145_13 g145_14 g145_15 g145_16 g145_17 g145_18 g145_19 g145_20 g145_21) 
egen ypen_ci =rsum(yjub ypen)
replace ypen_ci=. if yjub==. & ypen==.
drop yjub ypen
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci=(f119_1==1 |  f119_2==1)
*replace pension_ci=. if pobpcoac==.
label var pension_ci "1=Recibe pension contributiva"


***************
*pensionsub_ci*
***************
/*DZ Octubre 2017- Se crea variable pension subsidiada* Dado que la pregunta es excluyente y el programa de pensión subsidiada en Uruguay es para Adultos mayores y/o discapacitados
se pone la condicion de mayor de 70 años (edad para recivir el beneficio) en las personas que afirmaron tener pension por invalidez*/
gen pensionsub_ci= ((f120==1) | (f120==3 & edad_ci>69))
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"



*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
* cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if f111==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********

* generar variable auxiliar de region para desagregar por Montevideo, interior urbano y rural
gen aux_reg = .
replace aux_reg = 1 if dpto==1
replace aux_reg = 2 if zona_c==1 & aux_reg!=1
replace aux_reg = 3 if zona_c==0 & aux_reg!=1

gen lp_ci =.
replace lp_ci = 3885.89266726056 if mes=="01" & aux_reg==1
replace lp_ci = 3920.26260649145 if mes=="02" & aux_reg==1
replace lp_ci = 3930.25963245643 if mes=="03" & aux_reg==1
replace lp_ci = 3943.9651024585  if mes=="04" & aux_reg==1
replace lp_ci = 3980             if mes=="05" & aux_reg==1
replace lp_ci = 4004.21152259131 if mes=="06" & aux_reg==1
replace lp_ci = 4030.20673773105 if mes=="07" & aux_reg==1
replace lp_ci = 4058.94363820465 if mes=="08" & aux_reg==1
replace lp_ci = 4071.29068151639 if mes=="09" & aux_reg==1
replace lp_ci = 4058.14272474144 if mes=="10" & aux_reg==1
replace lp_ci = 4044.20649686527 if mes=="11" & aux_reg==1
replace lp_ci = 4065.30855207506 if mes=="12" & aux_reg==1

replace lp_ci = 2297.36028225615 if mes=="01" & aux_reg==2
replace lp_ci = 2317.03248299179 if mes=="02" & aux_reg==2
replace lp_ci = 2322.44777422533 if mes=="03" & aux_reg==2
replace lp_ci = 2330.64357200543 if mes=="04" & aux_reg==2
replace lp_ci = 2352             if mes=="05" & aux_reg==2
replace lp_ci = 2366.93065885239 if mes=="06" & aux_reg==2
replace lp_ci = 2379.30734020768 if mes=="07" & aux_reg==2
replace lp_ci = 2396.26538728401 if mes=="08" & aux_reg==2
replace lp_ci = 2403.76157232535 if mes=="09" & aux_reg==2
replace lp_ci = 2397.10072935749 if mes=="10" & aux_reg==2
replace lp_ci = 2390.34966581086 if mes=="11" & aux_reg==2
replace lp_ci = 2403.61725702207 if mes=="12" & aux_reg==2

replace lp_ci = 1330.52245925094 if mes=="01" & aux_reg==3
replace lp_ci = 1336.75529264223 if mes=="02" & aux_reg==3
replace lp_ci = 1339.97770782617 if mes=="03" & aux_reg==3
replace lp_ci = 1344.55651401049 if mes=="04" & aux_reg==3
replace lp_ci = 1357             if mes=="05" & aux_reg==3
replace lp_ci = 1362.44828221944 if mes=="06" & aux_reg==3
replace lp_ci = 1369.57051308521 if mes=="07" & aux_reg==3
replace lp_ci = 1379.12601525181 if mes=="08" & aux_reg==3
replace lp_ci = 1382.7899621928  if mes=="09" & aux_reg==3
replace lp_ci = 1376.90599748386 if mes=="10" & aux_reg==3
replace lp_ci = 1369.87353618243 if mes=="11" & aux_reg==3
replace lp_ci = 1377.75449952627 if mes=="12" & aux_reg==3

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 1083.56623085892 if mes=="01" & aux_reg==1
replace lpe_ci = 1096.75693488624 if mes=="02" & aux_reg==1
replace lpe_ci = 1101.64568990241 if mes=="03" & aux_reg==1
replace lpe_ci = 1112.30054169458 if mes=="04" & aux_reg==1
replace lpe_ci = 1111.25279920299 if mes=="05" & aux_reg==1
replace lpe_ci = 1115.16301069295 if mes=="06" & aux_reg==1
replace lpe_ci = 1130.17410175135 if mes=="07" & aux_reg==1
replace lpe_ci = 1138.88903695445 if mes=="08" & aux_reg==1
replace lpe_ci = 1151.17829958956 if mes=="09" & aux_reg==1
replace lpe_ci = 1145.71032277148 if mes=="10" & aux_reg==1
replace lpe_ci = 1158.36523257554 if mes=="11" & aux_reg==1
replace lpe_ci = 1155.39096089977 if mes=="12" & aux_reg==1

replace lpe_ci = 1019.76735152693 if mes=="01" & aux_reg==2
replace lpe_ci = 1032.29012128794 if mes=="02" & aux_reg==2
replace lpe_ci = 1036.48145612472 if mes=="03" & aux_reg==2
replace lpe_ci = 1046.35095036473 if mes=="04" & aux_reg==2
replace lpe_ci = 1044.77239619028 if mes=="05" & aux_reg==2
replace lpe_ci = 1047.93837629279 if mes=="06" & aux_reg==2
replace lpe_ci = 1061.89775181555 if mes=="07" & aux_reg==2
replace lpe_ci = 1070.46248915666 if mes=="08" & aux_reg==2
replace lpe_ci = 1082.46095408121 if mes=="09" & aux_reg==2
replace lpe_ci = 1077.13439437235 if mes=="10" & aux_reg==2
replace lpe_ci = 1089.31785598964 if mes=="11" & aux_reg==2
replace lpe_ci = 1086.06322516037 if mes=="12" & aux_reg==2

replace lpe_ci = 916.344468989304 if mes=="01" & aux_reg==3
replace lpe_ci = 928.480951147987 if mes=="02" & aux_reg==3
replace lpe_ci = 932.552425661711 if mes=="03" & aux_reg==3
replace lpe_ci = 941.625488524996 if mes=="04" & aux_reg==3
replace lpe_ci = 939.005767366695 if mes=="05" & aux_reg==3
replace lpe_ci = 942.018377771219 if mes=="06" & aux_reg==3
replace lpe_ci = 953.140584104363 if mes=="07" & aux_reg==3
replace lpe_ci = 962.274194721866 if mes=="08" & aux_reg==3
replace lpe_ci = 972.925938596026 if mes=="09" & aux_reg==3
replace lpe_ci = 966.393670034881 if mes=="10" & aux_reg==3
replace lpe_ci = 976.566543794828 if mes=="11" & aux_reg==3
replace lpe_ci = 973.095785827999 if mes=="12" & aux_reg==3

label var lpe_ci "Linea de indigencia oficial del pais"

drop aux_reg


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci = .
replace salmm_ci = 2617.5   if mes_c==1 | mes_c==2  | mes_c==3  | mes_c==4 | mes_c==5 | mes_c==6 
replace salmm_ci = 3000     if mes_c==7 | mes_c==8  | mes_c==9 | mes_c==10 | mes_c==11 | mes_c==12
label var	salmm_ci	"Salario minimo legal 2006"
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


replace f67_2="." if f67_2=="X211"

destring f67_2, replace

* Modificacion MGD 07/15/2014: correccion del grupo 9.
gen ocupa_ci=.
replace ocupa_ci=1 if (f67_2>=2110 & f67_2<=3480) & emp_ci==1
replace ocupa_ci=2 if (f67_2>=1110 & f67_2<=1310) & emp_ci==1
replace ocupa_ci=3 if (f67_2>=4110 & f67_2<=4223) & emp_ci==1
replace ocupa_ci=4 if ((f67_2>=5200 & f67_2<=5230) | (f67_2>=9110 & f67_2<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((f67_2>=5111 & f67_2<=5169) | (f67_2>=9120 & f67_2<=9172)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f67_2>=6100 & f67_2<=6210) | (f67_2>=9211 & f67_2<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((f67_2>=7110 & f67_2<=8340) | (f67_2>=9310 & f67_2<=9333)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f67_2>=100 & f67_2<=129) & emp_ci==1
replace ocupa_ci=9 if f67_2==9999 & emp_ci==1


****************
*** rama_ci  ***
****************
/*
A QUE SE DEDICA EL ESTABLECIMIENTO DONDE REALIZA LAS TAREAS	f68_1		Descripción
								f68_2	Cód.	Código
*/

replace f68_2="." if f68_2=="." | f68_2=="+512" | f68_2=="<" | f68_2=="X211" | f68_2=="15 4"
destring f68_2, replace

gen rama_ci=.
replace rama_ci=1 if (f68_2>=100 & f68_2<=500) & emp_ci==1
replace rama_ci=2 if (f68_2>=1000 & f68_2<=1429) & emp_ci==1
replace rama_ci=3 if (f68_2>=1500 & f68_2<=3720) & emp_ci==1
replace rama_ci=4 if (f68_2>=4000 & f68_2<=4100) & emp_ci==1
replace rama_ci=5 if (f68_2>=4500 & f68_2<=4550) & emp_ci==1
replace rama_ci=6 if (f68_2>=5010 & f68_2<=5520) & emp_ci==1
replace rama_ci=7 if (f68_2>=6000 & f68_2<=6420) & emp_ci==1
replace rama_ci=8 if (f68_2>=6500 & f68_2<=7499) & emp_ci==1
replace rama_ci=9 if (f68_2>=7500 & f68_2<=9900) & emp_ci==1

*********************************************************************

/*
HORAS TRABAJADAS POR SEMANA	f81	Nº	Número de horas trabajadas por semana

CUANTAS HORAS TRABAJA EN OTRAS OCUPACIONES	f93	Nº	Número de horas que trabaja
*/


gen horaspri_ci=f81
replace horaspri_ci=. if f81==99 | emp_ci==0
replace f93=. if f93==99

egen horastot_ci=rsum(horaspri_ci f93)
replace horastot_ci=. if f93==. & horaspri_ci==.

gen durades_ci=f108/4.3  if f108>0
replace durades_ci=. if f108==99

gen antigenanio=(f82_1/12)
egen antiguedad_ci=rowtotal(antigenanio  f82_2)
*Mayra Sáenz-NO se encuentra la variable categ_ci  se cambia por condocup_ci.
recode antiguedad_ci 0=. if condocup_ci !=1


/*
gen desemp1_ci=((pobpcoac==3|pobpcoac==4|pobpcoac==5) & f102==1)

gen desemp2_ci=(desemp1_ci==1 | f102==2 & f103==2 | f103==3)

gen desemp3_ci=(desemp2_ci==1 | (f104==1 | f108>=4))

gen pea1_ci=(emp_ci==1 | desemp1_ci==1)

gen pea2_ci=(emp_ci==1 | desemp2_ci==1)

gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
gen desalent_ci=.

/* Variable mal generada
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & (f97>=1 & f97<=3))
replace subemp_ci=. if emp_ci==0
*/

* Modificacion MGD 06/23/2014: horas de la actividad principaln y considerando disponibilidad (subempleo visible).
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30) & (f97==1 & f98==1)
* Mod. 2015/11 MLO
*gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & f97==2)
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30 & f97==2)
replace tiempoparc_ci=. if emp_ci==0

/*
CATEGORÍA DE LA OCUPACIÓN	f69	1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
					8	Programa público de empleo
*/

gen categopri_ci=1 	if f69==4
replace categopri_ci=2 	if f69==5 | f69==6 | f69==3
replace categopri_ci=3 	if f69==1 | f69==2 
replace categopri_ci=4 	if f69==7 
replace categopri_ci=. 	if emp_ci!=1

/*
CATEGORÍA DE LA OCUPACIÓN	f87	1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
*/


gen categosec_ci=1 if f87==4
replace categosec_ci=2 if f87==5 | f87==6 | f87==3
replace categosec_ci=3 if f87==1 | f87==2 
replace categosec_ci=4 if f87==7 

gen contrato_ci=.

gen segsoc_ci=.

/*
TRABAJOS QUE TIENE	f66	Nº	Número de trabajos que tiene
*/

gen nempleos_ci=1 if f66==1
replace nempleos_ci=2 if f66>1

/*
TAMAÑO  DE LA EMPRESA 	f75	1	Una persona
				2	2 a 4 personas
				3	5 a 9 personas
				4	10 a 49  personas
				5	50 o más personas
*/


/*gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & f75==1 | f75==2
replace firmapeq_ci=0 if emp_ci==1 & f75>2*/

/*
CATEGORÍA DE LA OCUPACIÓN	f69/f87		2	Asalariado público
*/

gen spublico_ci=(emp_ci==1 & f69==2)
replace spublico =. if emp_ci==.

*Genera la variable para empresas pequeñas
*drop tamemp_ci
gen tamemp_ci=1 if f75==1 | f75==2 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if f75==4 | f75==3
*Empresas grandes
replace tamemp_ci=3 if f75==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if f119_1==1 | f119_2==1
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if f119_4==1
*Quehaceres del Hogar
replace categoinac_ci=3 if f119_5==1
*Otra razon
replace categoinac_ci=4 if f119_3==1 
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
SUELDO O JORNALES LÍQUIDOS	g121_1	
COMISIONES, INCENTIVOS, HORAS EXTRAS	g121_2	
VIÁTICOS NO SUJETOS A RENDICIÓN	g121_3	
PROPINAS	g121_4	
AGUINALDO	g121_5	
SALARIO VACACIONAL	g121_6	
PAGOS ATRASADOS	g121_7	
RETIRO REALIZADO PARA GASTOS DEL HOGAR	g139	
Cuenta propia		
DISTRIBUCIÓN DE UTILIDADES anual	g140	
RECIBIO POR MEDIANERIA O PARCERIA anual	g142	
RECIBIO POR PASTOREO anual	g143	
RECIBIO POR GANADO A CAPITALIZACIÓN anual	g144	

*/
foreach i in g140 g142 g143 g144 {
gen `i'm = `i'/12
}


egen ylmpri_ci=rsum(g121_1 g121_2 g121_3 g121_4 g121_5 g121_6 g121_7 g139 g140m g142m g143m g144m) if emp_ci==1, missing

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
	
	BOLETOS DE TRANSPORTE	g121_8	
	RECIBIO ALIMENTOS O BEBIDAS	g122_4	Monto estimado
	RECIBIO TICKETS DE ALIMENTACION	g123_2	Valor recibido el mes pasado
	RECIBIO VIVIENDA O ALOJAMIENTO	g124_3	Monto que habria tenido que pagar por ese alojamiento
	RECIBIO OTRO TIPO DE RETRIBUCIÓN EN ESPECIE	g126_2	Monto que habria tenido que pagar por esos bienes
	RECIBIO ALGUN OTRO TIPO DE COMPONENTE PAGADO POR EL EMPLEADOR	g127_2	Monto estimado
Autoconsumo	DERECHO A CULTIVO PARA PROPIO CONSUMO	g129_2	Monto que habria tenido que pagar por esos alimentos
Autoconsumo	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador no agropecuario)	g141_2	Monto que habria tenido que pagar por esos bienes
Autoconsumo	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g141_3	Valor de lo consumido en carnes o chacinados
Autoconsumo	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g141_4	Valor de lo consumido en lácteos
Autoconsumo	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g141_5	Valor de lo consumido en huevos y aves
Autoconsumo	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g141_6	Valor de lo consumido en productos de la huerta
Autoconsumo	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g141_7	Valor consumido en otros alimentos

*/



gen desay=(g122_2*mto_desay)
gen almue= (g122_3*mto_almue)
gen cuota= (g125_2*mto_cuota) if g125_1 == 1
gen vacas = (g128_2*mto_vacas)
gen oveja = (g128_3*mto_oveja)
gen caballo = (g128_4*mto_caball)

/* Nota Marcela G. Rubio - a partir de 2006 se incluyen variables desay, almue, cuota, vacas, oveja y caballo como parte del ingreso laboral no 
montario principal*/

*No costa la variable disse en el formulario.

egen ylnmpri_ci= rsum(g121_8 desay almue g122_4 g123_2 g124_3 cuota g126_2 g127_2 vacas oveja caballo g129_2 g141_2 g141_3 g141_4 g141_5 g141_6 g141_7) if emp_ci==1, missing
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

	***************
	***ylmsec_ci***
	***************
/*

SUELDO O JORNALES LÍQUIDOS	g130_1
COMISIONES, INCENTIVOS, HORAS EXTRAS	g130_2
VIÁTICOS NO SUJETOS A RENDICIÓN	g130_3
PROPINAS	g130_4
AGUINALDO	g130_5
SALARIO VACACIONAL	g130_6
PAGOS ATRASADOS	g130_7

*/

	egen ylmsec_ci=rsum(g130_1 g130_2 g130_3 g130_4 g130_5 g130_6 g130_7) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
	****************
	***ylnmsec_ci***
	****************
	/*
	BOLETOS DE TRANSPORTE	g130_8	
	RECIBIO ALIMENTOS O BEBIDAS	g131_4	Monto estimado
	RECIBIO TICKETS DE ALIMENTACION	g132_2	Valor recibido el mes pasado
	RECIBIO VIVIENDA O ALOJAMIENTO	g133_3	Monto que habria tenido que pagar por ese alojamiento
	RECIBIO OTRO TIPO DE RETRIBUCIÓN EN ESPECIE	g135_2	Monto que habria tenido que pagar por esos bienes
	RECIBIO ALGUN OTRO TIPO DE COMPONENTE PAGADO POR EL EMPLEADOR	g136_2	Monto estimado
Autoconsumo	DERECHO A CULTIVO PARA PROPIO CONSUMO	g138_2	Monto que habria tenido que pagar por esos alimentos
        
*/

/*
RECIBIÓ ALIMENTOS O BEBIDAS	g139_1	1 = Sí / 2 = No	
	g131_2	Nº	Número de desayunos / meriendas
	g131_3	Nº	Número de almuerzos / cenas
*/

gen desaysec=(g131_2*mto_desay)
gen almuesec= (g131_3*mto_almue)
*gen cuota = En este año no se pregunta acerca de la cuota mutual.

/*
g137_2	Nº	Vacunos
g137_3	Nº	Ovinos
g137_4	Nº	Equinos
*/

gen vacassec = (g137_2*mto_vacas)
gen ovejasec = (g137_3*mto_oveja)
gen caballosec = (g137_4*mto_caball)


egen ylnmsec_ci=rsum(desaysec almuesec vacassec ovejasec caballosec g130_8	g131_4 g132_2 g133_3 g135_2 g136_2 g138_2) if emp_ci==1, missing
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

/* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
 Estimo variables desaysec, almuesec, vacassec, ovejasec, caballosec que no habían sido generadas y las incluyo como parte del ingreso laboral
no monetario secundario */
	
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

	egen ylmotros_ci= rsum(g121_1 g121_2 g121_3 g121_4 g121_5 g121_6 g121_7 g139 g140m g142m g143m g144m g130_1 g130_2 g130_3 g130_4 g130_5 g130_6 g130_7) if emp_ci==0, missing

	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral monetario otros trabajos para todos los años
	
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci=rsum(g121_8 desay almue g122_4 g123_2 g124_3 cuota g126_2 g127_2 vacas oveja caballo g129_2 g141_2 g141_3 g141_4 g141_5 g141_6 g141_7 desaysec almuesec vacassec ovejasec caballosec g130_8	g131_4 g132_2 g133_3 g135_2 g136_2 g138_2) if emp_ci==0, missing
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral no monetario otros trabajos para todos los años
	
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
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci ylnmotros_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  

	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral no monetario otros como parte del ingreso laboral no monetario total ya que no había sido incluido
	
	*************
	***ynlm_ci***
	*************
	/*
jubilación	BPS - CAJA INDUSTRIA Y COMERCIO 	g145_1	
jubilación	BPS - CAJA CIVIL Y ESCOLAR	g145_2	
jubilación	BPS - RURAL Y SERVICIO DOMÉSTICO	g145_3	
jubilación	UNIÓN POSTAL	g145_4	
jubilación	POLICIAL	g145_5	
jubilación	MILITAR	g145_6	
jubilación	PROFESIONAL	g145_7	
jubilación	NOTARIAL	g145_8	
jubilación	BANCARIA	g145_9	
jubilación	OTRA 	g145_10	
jubilación	OTRO PAÍS	g145_11	
pensión	BPS - CAJA INDUSTRIA Y COMERCIO	g145_12	
pensión	BPS - CAJA CIVIL Y ESCOLAR	g145_13	
pensión	BPS - RURAL Y SERVICIO DOMÉSTICO	g145_14	
pensión	UNIÓN POSTAL	g145_15	
pensión	POLICIAL	g145_16	
pensión	MILITAR	g145_17	
pensión	PROFESIONAL	g145_18	
pensión	NOTARIAL	g145_19	
pensión	BANCARIA	g145_20	
pensión	OTRA 	g145_21	
pensión	OTRO PAÍS	g145_22	
	SEGURO DE DESEMPLEO	g145_23	
	COMPENSACIONES POR ACCIDENTE, MATERNIDAD O ENFERMEDAD	g145_24	
	BECAS, SUBSIDIOS, DONACIONES	g145_25	Del país
	BECAS, SUBSIDIOS, DONACIONES	g145_26	Del extranjero
	COBRA ASIGNACIONES FAMILIARES	g147_4	Monto que cobró la ultima vez
	RECIBE PENSIÓN ALIMENTICIA O ALGUNA CONTRIBUCIÓN POR DIVORCIO O SEPARACIÓN	g148_2	Del país
	RECIBE PENSIÓN ALIMENTICIA O ALGUNA CONTRIBUCIÓN POR DIVORCIO O SEPARACIÓN	g148_3	Del extranjero
hogar	RECIBE DINERO DE ALGUN FAMILIAR U OTRO HOGAR EN EL PAIS	h149_2	
hogar	FUERON ALQUILADAS 	h53_2	Alquileres del país anual
hogar	FUERON ALQUILADAS 	h153_3	Alquileres del extranjero anual
hogar	RECIBIO POR ARRENDAMIENTO	h156_1	Arrendamientos del país anual
hogar	RECIBIO POR ARRENDAMIENTO	h156_2	Arrendamientos del extranjero anual
hogar	RECIBIO POR MEDIANERIA	h157	Anual
hogar	RECIBIO POR PASTOREO	h158	Anual
hogar	RECIBIO POR GANADO A CAPITALIZACION	h159	Anual
hogar	RECIBIO POR INTERESES	h161_1	Intereses del país anual
hogar	RECIBIO POR INTERESES	h161_2	Intereses del extranjero anual
hogar	RECIBIO POR UTILIDADES Y DIVIDENDOS DE ALGUN NEGOCIO	h163_1	Utilidades y dividendos del país anual
hogar	RECIBIO POR UTILIDADES Y DIVIDENDOS DE ALGUN NEGOCIO	h163_2	Utilidades y dividendos del extranjero anual
hogar	RECIBIO INDEMNIZACION POR DESPIDO	h164_2	Anual
hogar (remesas)	RECIBIO ALGUNA COLABORACIÓN ECONÓMICA DE ALGUN FAMILIAR EN EL EXTERIOR	h165_2	Anual
hogar	RECIBIO ALGUN INGRESO EXTRAORDINARIO	h166_2	Anual

              
*/


foreach i in h153_2 h153_3 h156_1 h156_2 h157 h158 h159 h161_1 h161_2 h163_1 h163_2 h164_2 h165_2 h166_2 {
gen `i'm=`i'/12 /*Estos estan definidos en base anual!*/
}

bys idh_ch: egen numper = sum(miembros_ci)
bys idh_ch: egen npermax = max(numper)
drop numper
* Los ingresos a nivel de hogar se dividen para los miembros del hogar y se obtiene un per capita.
egen inghog1 = rsum(h149_2 h153_2m h153_3m h156_1m h156_2m h157m h158m h159m h161_1m h161_2m h163_1m h163_2m h164_2m h165_2m h166_2m), missing
gen inghog= inghog1/npermax

*Transferencias de programas sociales
gen canasta = (e60_1_2 * indacomun) + (e60_2_2 * indabajo) + (e60_3_2 * indaplomo) + (e60_4_2 * indapensi) + ///
(e60_5_2 * indadiabet) + (e60_6_2 * indarenal) + (e60_7_2 * indarendia) + (e60_8_2 * indaceliac) + (e60_9_2 * indatuberc) + ///
(e60_10_2 * indaoncolo) + (e60_11_2 * indasida) + (e60_12_2 * otrcanast)

*CONCURRENCIA A COMEDOR O MERENDERO GRATUITO 

egen transf_desay= rsum(e58_2_1_1 e58_2_1_3 e58_2_2_1 e58_2_2_3 e58_2_3_1 e58_2_3_3 e58_2_4_1 e58_2_4_3 e58_2_5_1 e58_2_5_3 e58_2_6_1 e58_2_6_3), missing
gen tdesay = (transf_desay*4.3)*mto_desay

egen transf_almue= rsum(e58_2_1_2 e58_2_1_4 e58_2_2_2 e58_2_2_4 e58_2_3_2 e58_2_3_4 e58_2_4_2 e58_2_4_4 e58_2_5_2 e58_2_5_4 e58_2_6_2 e58_2_6_4), missing
gen talmue = transf_almue*4.3*mto_almue

/*
e58_2_7	7	Sólo almuerzo
	8	Sólo desayuno o merienda
	9	Desayuno y almuerzo
	10	Almuerzo y Merienda
	11	Desayuno, almuerzo y merienda
RECIBE ALGÚN TIPO DE ALIMENTACIÓN DE ALGÚN PROGRAMA PÚBLICO (SALVO CANASTAS)	

e59-1	1 = Si / 2 = No	
e59_2	Nº	Número de veces que recibe por semana
e59_3	Nº	Número de veces que recibe por mes
*/
gen tsolalmue = (5*4.3*mto_almue) if e58_2_7==7
gen tsoldesay = (5*4.3*mto_desay) if e58_2_7==8
gen tsoldesalm = (5*4.3*(mto_desay + mto_almue)) if e58_2_7==9 | e58_2_7==10
gen tsoldealme = (5*4.3*(mto_desay+ mto_desay + mto_almue)) if e58_2_7==11
gen salvcana = (e59_3*mto_almue)
replace salvcana= (e59_2*4.3*mto_almue) if e59_3==0
*HOGAR CONSTITUIDO	mto_hogcon	$	Valor del hogar constituido
gen hogcosnt = mto_hogcon if g146_1 ==	1 & g146_2==2


egen transf= rsum(canasta tdesay talmue tsolalmue tsoldesay tsoldesalm tsoldealme salvcana hogcosnt), missing

*Asignaciones familiares no declaradas en el sueldo
gen asigfam = g147_4 if g147_3 ==2

egen ynlm_ci=rsum(inghog transf g145_1 g145_2 g145_3 g145_4 g145_5 g145_6 g145_7 g145_8 g145_9 g145_10 g145_11 g145_12 g145_13 g145_14 g145_15 g145_16 g145_17 g145_18 g145_19 g145_20 g145_21 g145_22 g145_23 g145_24 g145_25 g145_26 asigfam g148_2 g148_3), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	
	*hogar	RECIBE AYUDA EN ESPECIE DE ALGUN FAMILIAR U OTRO HOGAR EN EL PAIS	h150_2
	
	gen ynlnm_ci= (h150_2/npermax)
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci= (h165_2m/npermax)
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
	
	egen autocons_ci= rsum(g138_2 g129_2 g141_2 g141_3 g141_4 g141_5 g141_6 g141_7), missing
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
gen g140_ = g140/12
gen g142_ = g142/12
gen g143_ = g143/12
gen g144_ = g144/12


egen ylmpri_ci=rsum(g121_1 g121_2 g121_3 g121_4 g121_5 g121_6 g121_7 g140_) 
replace ylmpri_ci=. if (g121_1==. & g121_2==. & g121_3==. & g121_4==. & g121_5==. & g121_6==. & g121_7==. & g140==.)

egen ylnmpri_ci=rsum(g121_8 g122_4 g123_2 g124_3 g125_2 g126_2 g127_2) if emp_ci==1

egen ylmsec_ci=rsum(g130_1 g130_2 g130_3 g130_4 g130_5 g130_6 g130_7)

egen ylnmsec_ci=rsum(g130_8 g131_4 g132_2 g133_3 g135_2 g136_2)
replace ylnmsec_ci = . if (g130_8==. & g131_4==. & g132_2==. & g133_3==. & g135_2==. & g136_2==.)

gen ylmotros_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) if emp_ci==1
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) if emp_ci==1
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


gen h153_2_ = h153_2/12
gen h153_3_ = h153_3/12
gen h156_1_ = h156_1/12
gen h156_2_ = h156_2/12
gen h157_   = h157/12
gen h158_   = h158/12
gen h159_   = h159/12
gen h161_1_ = h161_1/12
gen h161_2_ = h161_2/12
gen h163_1_ = h163_1/12
gen h163_2_ = h163_2/12
gen h164_2_ = h164_2/12
gen h165_2_ = h165_2/12
gen h166_2_ = h166_2/12

egen ynlmhog = rsum(h149_2 h153_2_ h153_3_ h156_1_ h156_2_ h157_ h158_ h159_ h161_1_ h161_2_ h163_1_ h163_2_ h164_2_ h165_2_ h166_2_)
gen ynlmind = ynlmhog/nmiembros_ch
egen ynlm_ci = rsum(g145_1 g145_2 g145_3 g145_4 g145_5 g145_6 g145_7 g145_8 g145_9 g145_10 g145_11 g145_12 g145_13 g145_14 g145_15 g145_16 g145_17 g145_18 g145_19 g145_20 g145_21 g145_22 g145_23 g145_24 g145_25 g145_26 g147_4 g148_2 g148_3 ynlmind)
gen ynlnm_ci=h150_2/nmiembros_ch



sort idh_ch
* by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
* gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci) if relacion_ci!=6
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)

replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0
gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.


gen remesas_ci=h165_2

/*
egen remesas_ci=rsum(g5_2 g5_3 g5_4 g5_5 g5_6 g5_7 g5_8 g5_9 g5_10 g5_11)
replace remesas_ci=. if g5_2==. & g5_3==. & g5_4==. & g5_5==. & g5_6==. & g5_7==. & g5_8==. & g5_9==. & g5_10==. & g5_11==.

sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6
*/

*/

/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/


/*
NIVEL Y AÑO MAS ALTO ALCANZADO	
			e52_1_1	Año	Años cursados en Primaria
			e52_1_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52_2_1	Año	Años cursados en Secundaria
			e52_2_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52_3_1	Año	Años cursados en Enseñanza Técnica
			e52_3_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52_3_3	1	Exigencia de enseñanza secundaria completa para realizar curso de UTU
				2	Exigencia de enseñanza secundaria primer ciclo para realizar curso de UTU
				3	Exigencia de enseñanza primaria completa para realizar curso de UTU
				4	Ninguna exigencia
			e52-4_1	Año	Magisterio o Profesorado
			e52-4_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52_5-1	Año	Universidad o similar
			e52_5_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52_6_1	Año	Terciario no universitario
			e52_6_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52-7-1	Año	Posgrado (maestría o doctorado)
			e52_7_2	1 = Si / 2 = No	Finalizo o no el nivel
*/


tab e52_1_1	
tab e52_2_1	
tab e52_3_1	
tab e52_4_1	
tab e52_5_1
tab e52_6_1
tab e52_7_1	



/*
De acuerdo al manual del entrevistador 2006
la formación profesional básica no es equivalente al
ciclo basico. Por lo tanto no los años en este no se consideran.
En caso de que alguien la haya tomado se consideran que tiene primaria
ya que para tomarla es requisito haber finalizado la pimaria. 
*/

/*  Criterios para la elaboración de años de educación aprobados:
       > No se toma en cuenta los años de preescolar
	   > Los años de educacion primaria especial también son 6 años, como la primaria comun
	   > Solo para años 2006 y 2007 se reconoce la educación técnica. Pero se asume que todos tiene ciclo basico 
	     (o sea 9 años de educacion). Esto para no dejar con cero mising a estos individuos. En los otros años no es necesario hacerlo
		 por que las preguntas son tomadas diferentes.
	   
*/

** Aug, 2015: Se efectuan cambios en sintaxis de variable aedu_ci en base a revisión por Iván Bornacelly SCL/EDU **
** Ajustado Jul, 2017 por Iván Bornacelly SLC/EDU

*Replicando lo que está en la progrmación de años posteriores.
*NOTA: No hay categoria ENSEÑANZA TÉCNICA para la variable de asistencia. Esta categoría si existe para el caso de lso

*Para quienes están asistiendo.[e_52] - Si grupo de variables tiene 1 quiere decir que la persona asiste al nivel señalado. De lo contrario no asiste a ese nivel. 
gen a_post=1 if e50_12>=1
replace a_post=0 if e50_12==0

gen a_terc=1 if e50_11>=1
replace a_terc=0 if e50_11==0

gen a_univ=1 if e50_10>=1
replace a_univ=0 if e50_10==0

gen a_mag=1 if e50_9>=1
replace a_mag=0 if e50_9==0

gen a_bachtec=1 if e50_8>=1
replace a_bachtec=0 if e50_8==0

gen a_bachsec=1 if e50_6>=1
replace a_bachsec=0 if e50_6==0

gen a_profbas=1 if e50_7>=1 // Esta podría ser equivalente a Enseñanza Técnica
replace a_profbas=0 if e50_7==0

gen a_cbutu=1 if e50_5>=1
replace a_cbutu=0 if e50_5==0

gen a_cbliceo=1 if e50_4>=1
replace a_cbliceo=0 if e50_4==0

gen a_priesp=1 if e50_2>=0
replace a_priesp=0 if e50_2==0

gen a_pricom=1 if e50_3>=1
replace a_pricom=0 if e50_3==0

gen a_preesc=1 if e50_1>=1
replace a_preesc=0 if e50_1==0


gen aedu_ci=.
replace aedu_ci= 0            if a_preesc==1
replace aedu_ci= e50_2        if a_priesp==1
replace aedu_ci= e50_3        if a_pricom==1
replace aedu_ci= e50_4 + 6    if a_cbliceo==1
replace aedu_ci= e50_5 + 6    if a_cbutu==1
replace aedu_ci= e50_6 + 6    if a_bachsec==1 // Se le adiciona 6 porque la variable toma valores 4-6
replace aedu_ci= e50_8 + 6    if a_bachtec==1 // Se le adiciona 6 porque la variable toma valores 4-6
replace aedu_ci= e50_7 + 12   if a_profbas==1 // Esto podría ser equivalente a Enseñanza Técnica.
replace aedu_ci= e50_9 + 12   if a_mag==1
replace aedu_ci= e50_10 + 12  if a_univ==1
replace aedu_ci= e50_11 + 12  if a_terc==1
replace aedu_ci= e50_12 + 17  if a_post==1
*replace aedu_ci=0             if e50==2 & (edad>=5 & edad!=.)  <<------- Revisar si esto iría acá // No CREO

replace aedu_ci=aedu_ci-1 if aedu_ci>=1 & aedu_ci!=.

*Para quienes no asisten esta variable se completa con la pregunta: ¿cuál es el nivel y año más alto aprobado? [e_54]
*NOTA: No existe la categoria PRESCOLAR.
gen post=1 if e52_7_1>=1
replace post=0 if e52_7_1==0

gen terc=1 if e52_6_1>=1
replace terc=0 if e52_6_1==0

gen univ=1 if e52_5_1>=1
replace univ=0 if e52_5_1==0

gen mag=1 if e52_4_1>=1
replace mag=0 if e52_4_1==0

gen enstec=1 if e52_3_1>=1
replace enstec=0 if e52_3_1==0

gen bachsec=1 if e52_2_1>=1
replace bachsec=0 if e52_2_1==0

gen eprim=1 if e52_1_1>=1
replace eprim=0 if e52_1_1==0

replace aedu_ci=e52_1_1 if eprim==1 & e51==1
replace aedu_ci=e52_2_1+6 if bachsec==1 & e51==1
replace aedu_ci=e52_3_1+12 if enstec==1 & e51==1
replace aedu_ci=e52_4_1+12 if mag==1 & e51==1
replace aedu_ci=e52_5_1+12 if univ==1 & e51==1
replace aedu_ci=e52_6_1+12 if terc==1 & e51==1
replace aedu_ci=e52_7_1+17 if post==1 & e51==1

replace aedu_ci=0             if e51==2 & (edad>=5 & edad!=.)
replace aedu_ci=0 			  if e51!=2 & e51!=. & aedu_ci==.

*Eliminando outliers / Registrando variable aedu_ci como missing ---->> Deben ser por error de registro de la información
replace aedu_ci=. if aedu_ci>=25  


/*
recode e50_6 (4=1) (5=2) (6=3)
recode e50_8 (4=1) (5=2) (6=3)

/*

gen aprees = (e50_1>0)
gen aprie  = (e50_2>0)
gen apric  = (e50_3>0)
gen alic   = (e50_4>0)
gen acicb  = (e50_5>0)
gen abachs = (e50_6>0)
gen fpbas  = (e50_7>0)
gen abacht = (e50_8>0)
gen amag   = (e50_9>0)
gen auniv  = (e50_10>0)
gen aterc  = (e50_11>0)
gen aposg  = (e50_12>0)


gen dpri = 1 if (e52_1_1>0)
gen dsec = 1 if (e52_2_1>0)
gen dtec = 1 if (e52_3_1>0)
gen dmag = 1 if (e52_4_1>0)
gen duni = 1 if (e52_5_1>0)
gen dter = 1 if (e52_6_1>0)
gen dmae = 1 if (e52_7_1>0)


*/

gen asiste=.
replace asiste=1 if e48==1 // Asistiendo
replace asiste=0 if e48==2 // No asistiendo

gen ha_asistido=.
replace ha_asistido=1 if e51==1 // Ha asistido
replace ha_asistido=0 if e51==2 // No ha asistido


*Para los que asisten


/*
gen aedu_ci = .
replace aedu_ci = 0         if aprees==1 
replace aedu_ci = e50_2     if aprie==1  & e50_2<=6
replace aedu_ci = e50_3     if apric==1  & e50_3<=6
replace aedu_ci = e50_4+6   if alic==1   & e50_4<=3
replace aedu_ci = e50_5+6   if acicb==1  & e50_5<=3
replace aedu_ci = e50_6+9   if abachs==1 & e50_6<=3
replace aedu_ci = 7         if e50_7>=1 & e50_7!=.
replace aedu_ci = e50_8+9   if abacht==1 & e50_8<=3
replace aedu_ci = e50_9+12  if amag==1   & e50_9<9
replace aedu_ci = e50_10+12 if auniv==1  & e50_10<9
replace aedu_ci = e50_11+12 if aterc==1  & e50_11<9
replace aedu_ci = e50_12+17 if aposg==1  & e50_12<9
replace aedu_ci = aedu_ci-1 if aedu_ci>0 & aedu_ci!=.
*/

gen aprees = (e50_1>0)
gen aprie  = (e50_2>0) 
gen apric  = (e50_3>0) // Primaria tradicional
gen alic   = (e50_4>0) // Secundaria baja
gen acicb  = (e50_5>0) // Secundaria baja
gen abachs = (e50_6>0) // Secundaria alta
gen fpbas  = (e50_7>0) // Secundaria alta
gen abacht = (e50_8>0)
gen amag   = (e50_9>0)
gen auniv  = (e50_10>0)
gen aterc  = (e50_11>0)
gen aposg  = (e50_12>0)


gen aedu_ci = .
replace aedu_ci = 0         if aprees==1 
replace aedu_ci = .         if aprie==1  // No se incluye. Equivale a educación especial.
replace aedu_ci = e50_3     if apric==1  & e50_3<=6
replace aedu_ci = e50_4+6   if alic==1   & e50_4<=3
replace aedu_ci = e50_5+6   if acicb==1  & e50_5<=3
replace aedu_ci = e50_6+9   if abachs==1 & e50_6<=3
replace aedu_ci = .         if fpbas==1 // No se incluye. Corresponde a educación para adultos
replace aedu_ci = e50_8+9   if abacht==1 & e50_8<=3
replace aedu_ci = e50_9+12  if amag==1   & e50_9<9
replace aedu_ci = e50_10+12 if auniv==1  & e50_10<9
replace aedu_ci = e50_11+12 if aterc==1  & e50_11<9
replace aedu_ci = e50_12+17 if aposg==1  & e50_12<9

replace aedu_ci = aedu_ci-1 if aedu_ci>0 & aedu_ci!=.


*Para los que asistieron

gen dpri = 1 if (e52_1_1>0) // Primaria
gen dsec = 1 if (e52_2_1>0) // Secundaria
gen dtec = 1 if (e52_3_1>0) // Enseñanza técnica – Educación para adultos
gen dmag = 1 if (e52_4_1>0) // Profesorado - Magisterio
gen duni = 1 if (e52_5_1>0) // Universidad
gen dter = 1 if (e52_6_1>0) // Terciario no Universitario
gen dmae = 1 if (e52_7_1>0) // Postgrado

/*
replace aedu_ci = e52_1_1 		    if dpri==1 & e52_1_1 <=6
replace aedu_ci = e52_2_1 + 6		if dsec==1 & e52_2_1 <=6
replace aedu_ci = e52_3_1 + 9		if dtec==1 & e52_3_1 <=6
replace aedu_ci = e52_4_1 + 12		if dmag==1 & e52_4_1 <9
replace aedu_ci = e52_5_1 + 12		if duni==1 & e52_5_1 <9
replace aedu_ci = e52_6_1 + 12		if dter==1 & e52_6_1 <9
replace aedu_ci = e52_7_1 + 17		if dmae==1 & e52_7_1 <9
replace aedu_ci = 0                 if e51==2 & (edad>=5 & edad!=.)
*br  e50* e52_1_1  e52_2_1  e52_3_1  e52_4_1  e52_5_1  e52_6_1  e52_7_1 aedu_ci
*/

replace aedu_ci = e52_1_1 		    if dpri==1 & e52_1_1 <=6
replace aedu_ci = e52_2_1 + 6		if dsec==1 & e52_2_1 <=6
replace aedu_ci = e52_4_1 + 12		if dmag==1 & e52_4_1 <9
replace aedu_ci = e52_5_1 + 12		if duni==1 & e52_5_1 <9
replace aedu_ci = e52_6_1 + 12		if dter==1 & e52_6_1 <9
replace aedu_ci = e52_7_1 + 17		if dmae==1 & e52_7_1 <9
replace aedu_ci=. 		if dtec==1
replace aedu_ci = 0                 if e51==2 & (edad>=5 & edad!=.)  // Para quienes nunca asistieron y no están asistiendo.
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

gen byte edusi_ci=(aedu_ci>6 & aedu_ci<11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************


gen byte edusc_ci=(aedu_ci==11)
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

***************
***edupre_ci***
***************

gen edupre_ci=a_preesc

****************
***asispre_ci***
****************
* Agregada por Iván Bornacelly - 01/23/2017
	g asispre_ci=.
	replace asispre_ci=1 if e48==1 & e50_1!=0 & e27>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"	

/*
e50_10	Año	Universidad o similar
e50_11	Año	Terciario no universitario
*/

gen eduac_ci=.
replace eduac_ci = 1 if e50_10>0
replace eduac_ci = 0 if e50_11>0
replace eduac_ci =. if e50_10>=10 | e50_11>=10

/*
ASISTENCIA ACTUAL A ESTABLECIMIENTO DE ENSEÑANZA	e48	1 = Si / 2 = No	
*/

gen asiste_ci=.
replace asiste_ci = 1 if (e48==1)
replace asiste_ci = 0 if (e48==2)


gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci =.

gen repite_ci=.
gen repiteult_ci=.
/*
ESTABLECIMIENTO PÚBLICO O PRIVADO	e49	1	Público
						2	Privado
*/

gen edupub_ci=.
replace edupub_ci = 1 if (e49==1)
replace edupub_ci = 0 if (e49==2)

label var  aedu_ci "Anios de Educacion"

****************
***tecnica_ci **
****************
gen tecnica_ci=(e50_11>=1 & e50_11<=9)
label var tecnica_ci "=1 formacion terciaria tecnica"	

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



rename f68_2 codindustria 
rename f67_2 codocupa 
compress



saveold "`base_out'", replace


log close
