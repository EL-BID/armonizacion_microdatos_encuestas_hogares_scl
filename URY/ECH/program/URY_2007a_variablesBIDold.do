

* (Versi�n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 global ruta = "\\Sdssrv03\surveys"

local PAIS URY
local ENCUESTA ECH
local ANO "2007"
local ronda a 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Uruguay
Encuesta: ECH
Round: a
Autores: 
Generaci�n nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
�ltima versi�n: Mayra S�enz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha �ltima modificaci�n: 30 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

/***************************************************************************************************************************
 							armonizaci�n 2007
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=correlat
gen idp_ci=nper
gen factor_ch=pesoan

*A partir de 2006 hay zona rural.

destring region, replace
gen zona_c=.
replace zona_c=1 if region == 1 | region == 2
replace zona_c=0 if region == 3
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


gen str3 pais_c="URY"
gen anio_c=2007

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
          11 "Paysand�" /// 
          12 "R�o Negro" /// 
          13 "Rivera" /// 
          14 "Rocha" /// 
          15 "Salto" /// 
          16 "San Jos�" /// 
          17 "Soriano" /// 
          18 "Tacuaremb�" ///
          19 "Treinta y Tres" 
label value region_c region_c
label var region_c "Divisi�n pol�tica"
gen mes_c=real(mes)

/*
e32
1	Jefe
2	Esposo o compa�ero
3	Hijo de ambos
4	Hijo s�lo del jefe
5	Hijo s�lo del c�nyuge
6	Yerno o nuera
7	Padre
8	Suegro
9	Hermano
10	Cu�ado
11	Nieto
12	Otro pariente
13	Otro no pariente
14	Servicio dom�stico o familiar del mismo
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
label define region_BID_c 1 "Centroam�rica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	

/*
d11	
1	Red general
2	Canilla p�blica
3	Pozo surgente no protegido
4	Pozo surgente protegido
5	Aljibe
6	Arroyo, r�o
7	Otro
*/

gen aguared_ch=(d11==1)
replace aguared_ch =. if d11==.

gen aguadist_ch=.
replace aguadist_ch=1 if d14_1==1
replace aguadist_ch=2 if d14_2==1 & d14_1==2
replace aguadist_ch=3 if d14_2==2 & d14_1==2
* Nota Marcela G. Rubio - Abril 2014
* Variable habia sido generada como missing. Preguntas de cuestionario cambiaron de 2006 a 2007 por lo que genero variable. 

gen aguamala_ch=(d11==5|d11==6) 
replace aguamala_ch =. if d11==.

gen aguamide_ch=.

gen luz_ch=.

gen luzmide_ch=.

/*
d21	1	Energ�a el�ctrica (U.T.E)
	2	Energ�a el�ctrica (grupo electr�geno)
	3	Gas por ca�er�a
	4	Superg�s
	5	Queroseno
	6	Le�a
	7	Ninguna

*/

gen combust_ch=(d21>=1 & d21<=6)
replace combust_ch =. if d10==.


/*
d15	1	S�. Con cisterna
	2	S�. Sin cisterna
	3	No
*/

gen bano_ch=(d15!=3)
replace bano_ch = . if d15==.

/*
d17	1	De uso exclusivo del hogar
	2	Compartido con otro hogar
*/


gen banoex_ch=.
replace banoex_ch=1 if d17==1
replace banoex_ch=0 if d17==2


/*
d18.1	1	Red general
	2	Fosa s�ptica, pozo negro
	3	Entubado hacia el arroyo
	4	Otro (superficie, etc.)
	
	d15	1	S�. Con cisterna
	2	S�. Sin cisterna
	3	No

*/

gen des1_ch=.
replace des1_ch=0 if d15==3
replace des1_ch=1 if d18_1==1 | (d18_1==2 & d15==1)
replace des1_ch=2 if (d18_1==2 & d15==2)
replace des1_ch=3 if d18_1==3 

gen des2_ch=.
replace des2_ch= 0 if des1_ch==0
replace des2_ch= 1 if des1_ch==1 | des1_ch==2 | des1_ch==3
replace des2_ch= 2 if d18_1 ==4


/*
C4	1	Cer�mica, parquet, moquete, lin�leo
	2	Baldosas calc�reas
	3	Alisado de hormig�n
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
MATERIAL PREDOMINANTE DEL TECHO	C3	1	Planchada de hormig�n con protecci�n (tejas u otros)
		2	Planchada de hormig�n sin protecci�n
		3	Liviano con cielorraso
		4	Liviano sin cielorraso
		5	Quincha
		6	Materiales de desecho
*/

gen techo_ch=.
replace techo_ch=0 if c3 == 6
replace techo_ch=1 if c3 >= 1 & c3 <= 5

gen resid_ch=.

/*
d10	N�	N�mero de habitaciones para dormir
*/

gen dorm_ch=d10
replace dorm_ch=. if d10==9
sum dorm*
/*
d9	N�	N�mero de habitaciones residenciales
*/

gen cuartos_ch=d9
replace cuartos_ch=. if d9==99
sum cuartos*

/*
d20	1	SI, privado de este hogar
	2	SI, compartido con otros hogares
	3	NO hay
*/

gen cocina_ch=.
replace cocina_ch = 1 if d20==1 | d20==2 
replace cocina_ch = 1 if d20==0

/*
d22_3		1 = Si / 2 = No		Refrigerador (con o sin frezer)
d22_4		1 = Si / 2 = No		Freezer (solo)
d22_5_1		1 = Si / 2 = No		TV color
d22_6		1 = Si / 2 = No		Radio
d22_14_1 	1 = Si / 2 = No		Microcomputador (incluye laptop)
d22_15		1 = Si / 2 = No		Conexi�n a internet
d22_16_1 	1 = Si / 2 = No		Tel�fono
d22_17_1 	1 = Si / 2 = No		Celular
d22_18_1 	1 = Si / 2 = No		Autom�vil o camioneta
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
TENENCIA DE LA VIVIENDA		d8_1	1	Propietario de la vivienda y el terreno y la est� pagando
					2	Propietario de la vivienda y el terreno y ya la pag�
					3	Propietario solamente de la vivienda y la est� pagando
					4	Propietario solamente de la vivienda y ya la pag�
					5	Inquilino o arrendatario de la vivienda
					6	Ocupante con relaci�n de dependencia
					7	Ocupante gratuito
					8	Ocupante sin permiso del propietario
				d8_2	$	Monto de la cuota de compra
				d8_3	$	Monto de la cuota de alquiler
*/

gen vivi1_ch=.
replace vivi1_ch=1 if c1==1
replace vivi1_ch=2 if c1>=2 & c1<=4
replace vivi1_ch=3 if c1==5

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if  vivi2_ch~=1

* Nota Marcela G. Rubio - Abril 2014
* Varivables vivi1_ch y vivi2_ch hab�an sido estimadas como missing values

gen viviprop_ch=0 if d8_1==5
replace viviprop_ch=2 if d8_1==1 | d8_1==3
replace viviprop_ch=1 if d8_1==2 | d8_1==4
replace viviprop_ch=3 if d8_1==6 | d8_1==7 | d8_1==8 
gen vivialq_ch=d8_3 if viviprop_ch==0
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
e28	A�os	A�os cumplidos
*/

gen edad_ci=e28
replace edad_ci=. if e28==99

/*
ESTADO CIVIL ACTUAL	e40	1	Divorciado/a
				2	Casado/a (incluye separado y a�n no se divorci�)
				3	Viudo/a
				4	Soltero/a
				5	Separado/a de uni�n libre


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

**********
***raza***
**********
/*
ASCENDENCIA	e31_1	1 = S� / 2 = No	Afro o Negra
	e31_2	1 = S� / 2 = No	Amarilla
	e31_3	1 = S� / 2 = No	Blanca
	e31_4	1 = S� / 2 = No	Ind�gena
	e31_5_1	1 = Si / 2 = No	Otro
	e31_5_2	Otra	Descripci�n

*/


gen raza_ci= .
replace raza_ci=1 if e31_4==1
replace raza_ci=2 if e31_1==1
replace raza_ci=3 if raza_ci== .

label define raza_ci 1 "Ind�gena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 



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

*Nota: en el 2006 y 2007 la variable original "pobpcoac"presenta problemas
gen condocup_ci=.
replace condocup_ci=1 if f68==1  | f69==1 | f70==1
replace condocup_ci=2 if f109==1 & [(f113>=1 & f113<=6) | ((f111>=1 & f111<=3) & (f113>=1 & f113<=6))]
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
*Nota: solo seguro social publico, con el cual tenga derecho a pensiones en el futuro.

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
label var instcot_ci "instituci�n a la cual cotiza por su trabajo"

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
label define tamemp_ci 1"una persona" 2"2-4 personas" 3"5-9 personas" 4 "10-49 personas" 5"50 o m�s" 6"10-19 personas" 7"20-49 personas"
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
gen pension_ci=(f127_1==1 |  f127_2==1)
*replace pension_ci=. if pobpcoac==.
label var pension_ci "1=Recibe pension contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
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
replace lp_ci = 4145.90999082826 if mes=="01" & region == 1
replace lp_ci = 4163.03643097332 if mes=="02" & region == 1
replace lp_ci = 4192.24170953621 if mes=="03" & region == 1
replace lp_ci = 4211.71763996794 if mes=="04" & region == 1
replace lp_ci = 4235.49977280042 if mes=="05" & region == 1
replace lp_ci = 4232.45714593424 if mes=="06" & region == 1
replace lp_ci = 4273.00131210583 if mes=="07" & region == 1
replace lp_ci = 4323.58605694727 if mes=="08" & region == 1
replace lp_ci = 4317.38935801313 if mes=="09" & region == 1
replace lp_ci = 4263.48149504138 if mes=="10" & region == 1
replace lp_ci = 4260.48155061765 if mes=="11" & region == 1
replace lp_ci = 4285.65433052209 if mes=="12" & region == 1

replace lp_ci = 2450.74244749148 if mes=="01" & region == 2
replace lp_ci = 2459.51600660969 if mes=="02" & region == 2
replace lp_ci = 2476.28466981549 if mes=="03" & region == 2
replace lp_ci = 2488.43652050621 if mes=="04" & region == 2
replace lp_ci = 2503.35633474486 if mes=="05" & region == 2
replace lp_ci = 2501.42939315946 if mes=="06" & region == 2
replace lp_ci = 2523.66280679305 if mes=="07" & region == 2
replace lp_ci = 2551.49518333217 if mes=="08" & region == 2
replace lp_ci = 2549.15092306182 if mes=="09" & region == 2
replace lp_ci = 2522.15187074558 if mes=="10" & region == 2
replace lp_ci = 2521.43919920295 if mes=="11" & region == 2
replace lp_ci = 2536.31266887224 if mes=="12" & region == 2

replace lp_ci = 1399.34414729227 if mes=="01" & region == 3 
replace lp_ci = 1401.52664144384 if mes=="02" & region == 3 
replace lp_ci = 1411.41672337311 if mes=="03" & region == 3 
replace lp_ci = 1419.82686126429 if mes=="04" & region == 3
replace lp_ci = 1427.3891830804  if mes=="05" & region == 3 
replace lp_ci = 1427.39115120009 if mes=="06" & region == 3
replace lp_ci = 1441.17065609925 if mes=="07" & region == 3 
replace lp_ci = 1453.63727943871 if mes=="08" & region == 3
replace lp_ci = 1452.3803117331  if mes=="09" & region == 3 
replace lp_ci = 1433.68321599485 if mes=="10" & region == 3 
replace lp_ci = 1431.05465611606 if mes=="11" & region == 3 
replace lp_ci = 1439.33776553932 if mes=="12" & region == 3 

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 1177.62464049224 if mes=="01" & region == 1
replace lpe_ci = 1196.49665716157 if mes=="02" & region == 1
replace lpe_ci = 1218.66948017847 if mes=="03" & region == 1
replace lpe_ci = 1268.43646659344 if mes=="04" & region == 1
replace lpe_ci = 1294.34077489413 if mes=="05" & region == 1
replace lpe_ci = 1301.36591183986 if mes=="06" & region == 1
replace lpe_ci = 1310.76835764415 if mes=="07" & region == 1
replace lpe_ci = 1362.41295387482 if mes=="08" & region == 1
replace lpe_ci = 1380.49275981773 if mes=="09" & region == 1
replace lpe_ci = 1414.33024454471 if mes=="10" & region == 1
replace lpe_ci = 1402.595392451   if mes=="11" & region == 1
replace lpe_ci = 1393.59241878458 if mes=="12" & region == 1

replace lpe_ci = 1107.77157042198 if mes=="01" & region == 2
replace lpe_ci = 1126.08933662626 if mes=="02" & region == 2
replace lpe_ci = 1147.69370725496 if mes=="03" & region == 2
replace lpe_ci = 1196.7495382921  if mes=="04" & region == 2
replace lpe_ci = 1220.6867948887  if mes=="05" & region == 2
replace lpe_ci = 1226.79366795683 if mes=="06" & region == 2
replace lpe_ci = 1236.34667279505 if mes=="07" & region == 2
replace lpe_ci = 1286.03174242842 if mes=="08" & region == 2
replace lpe_ci = 1302.69039275932 if mes=="09" & region == 2
replace lpe_ci = 1335.00415920722 if mes=="10" & region == 2
replace lpe_ci = 1322.67740811099 if mes=="11" & region == 2
replace lpe_ci = 1313.25857924459 if mes=="12" & region == 2

replace lpe_ci = 992.262591809932 if mes=="01" & region == 3
replace lpe_ci = 1010.41478996558 if mes=="02" & region == 3
replace lpe_ci = 1031.29498308072 if mes=="03" & region == 3 
replace lpe_ci = 1078.96808584147 if mes=="04" & region == 3
replace lpe_ci = 1101.65538784681 if mes=="05" & region == 3 
replace lpe_ci = 1107.07342281971 if mes=="06" & region == 3 
replace lpe_ci = 1115.37267528983 if mes=="07" & region == 3 
replace lpe_ci = 1161.18495846815 if mes=="08" & region == 3
replace lpe_ci = 1175.37843935913 if mes=="09" & region == 3 
replace lpe_ci = 1203.96171879381 if mes=="10" & region == 3 
replace lpe_ci = 1191.60294660149 if mes=="11" & region == 3 
replace lpe_ci = 1181.89400293983 if mes=="12" & region == 3 

label var lpe_ci "Linea de indigencia oficial del pais"

* Nota Marcela G. Rubio - Abril 2014
* genero variables de l�nea de pobreza total y extrema que no hab�an sido generadas


*********
*lp25_ci***
*********
gen lp25_ci=1339.234
label var lp25_ci "Linea de pobreza 2.5 d�lares, a�o base 2005"

*********
*lp4_ci***
*********
gen lp4_ci=2142.774
label var lp4_ci "Linea de pobreza 4 d�lares, a�o base 2005"

*************
**salmm_ci***
*************
gen salmm_ci = .
replace salmm_ci = 3075   if mes_c==1 | mes_c==2  | mes_c==3  | mes_c==4 | mes_c==5 | mes_c==6 
replace salmm_ci = 3244   if mes_c==7 | mes_c==8  | mes_c==9 | mes_c==10 | mes_c==11 | mes_c==12
label var	salmm_ci	"Salario minimo legal 2007"
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
CONDICION DE ACTIVIDAD ECONOMICA	pobpcoac	1	Menor de 14 a�os
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
TAREAS QUE PROPORCIONAN MAYORES INGRESOS	f67_1		Descripci�n
						f67-2	C�d.	C�digo
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
replace ocupa_ci=5 if ((f73_2>=5111 & f73_2<=5169) | (f73_2>=9120 & f73_2<=9172)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregaci�n a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f73_2>=6110 & f73_2<=6210) | (f73_2>=9211 & f73_2<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((f73_2>=7110 & f73_2<=8340) | (f73_2>=9311 & f73_2<=9333)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f73_2>=110 & f73_2<=129) & emp_ci==1
replace ocupa_ci=9 if f73_2>=9999 & emp_ci==1

****************
*** rama_ci  ***
****************
/*
A QUE SE DEDICA EL ESTABLECIMIENTO DONDE REALIZA LAS TAREAS	f68_1		Descripci�n
								f74_2	C�d.	C�digo
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
HORAS TRABAJADAS POR SEMANA	f88	N�	N�mero de horas trabajadas por semana

CUANTAS HORAS TRABAJA EN OTRAS OCUPACIONES	f101	N�	N�mero de horas que trabaja
*/

*********************************** REVISAR LAS VARIABLES DE INGRESO****************************
gen horaspri_ci=f88
replace horaspri_ci=. if f88==99 | emp_ci==0
replace f101=. if f101==99

egen horastot_ci=rsum(horaspri_ci f101)
replace horastot_ci=. if f101==. & horaspri_ci==.
gen durades_ci=f116/4.3 if f116>0
replace durades_ci=. if f116==99

gen antigenanio=(f89_1/12)
egen antiguedad_ci=rowtotal(antigenanio  f89_2)
*Mayra S�enz-NO se encuentra la variable categ_ci  se cambia por condocup_ci.
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

/* Variable mal generada
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & (f105>=1 & f105<=3))
replace subemp_ci=. if emp_ci==0
*/

* Modificacion MGD 06/23/2014: horas de la actividad principal y considerando disponibilidad (subempleo visible).
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30) & (f105==1 & f106==1)

gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & f105==2)
replace tiempoparc_ci=. if emp_ci==0

/*
CATEGOR�A DE LA OCUPACI�N	f75	1	Asalariado privado
					2	Asalariado p�blico
					3	Miembro de cooperativa de producci�n
					4	Patr�n
					5	Cuenta propia sin local o inversi�n
					6	Cuenta propia con local o inversi�n
					7	Miembro del hogar no remunerado
					8	Programa p�blico de empleo
*/

gen categopri_ci=1 	if f75==4
replace categopri_ci=2 	if f75==5 | f75==6 | f75==3
replace categopri_ci=3 	if f75==1 | f75==2 
replace categopri_ci=4 	if f75==7 
replace categopri_ci=. 	if emp_ci!=1

/*
CATEGOR�A DE LA OCUPACI�N	f95	1	Asalariado privado
					2	Asalariado p�blico
					3	Miembro de cooperativa de producci�n
					4	Patr�n
					5	Cuenta propia sin local o inversi�n
					6	Cuenta propia con local o inversi�n
					7	Miembro del hogar no remunerado
*/


gen categosec_ci=1 if f95==4
replace categosec_ci=2 if f95==5 | f95==6 | f95==3
replace categosec_ci=3 if f95==1 | f95==2 
replace categosec_ci=4 if f95==7 

gen contrato_ci=.

gen segsoc_ci=.

/*
TRABAJOS QUE TIENE	f72	N�	N�mero de trabajos que tiene
*/

gen nempleos_ci=1 if f72==1
replace nempleos_ci=2 if f72>1

/*
TAMA�O  DE LA EMPRESA 	f80	1	Una persona
				2	2 a 4 personas
				3	5 a 9 personas
				4	10 a 49  personas
				5	50 o m�s personas
*/

/*
gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & f80==1 | f80==2
replace firmapeq_ci=0 if emp_ci==1 & f80>2*/

/*
CATEGOR�A DE LA OCUPACI�N	f75/f85		2	Asalariado p�blico
*/

gen spublico_ci=(emp_ci==1 & f75==2)
replace spublico =. if emp_ci==.

*Genera la variable para empresas peque�as
*drop tamemp_ci
gen tamemp_ci=1 if f80==1 | f80==2 
label var  tamemp_ci "Tama�o de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if f80==4 | f80==3
*Empresas grandes
replace tamemp_ci=3 if f80==5
label define tama�o 1"Peque�a" 2"Mediana" 3"Grande"
label values tamemp_ci tama�o
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if f127_1==1 | f127_2==1
label var  categoinac_ci "Condici�n de Inactividad" 
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

************************************************************************************************************************
******************************************INGRESOS**********************************************************************
************************************************************************************************************************

*************
* ylmpri_ci *
*************
/*
ylmpri_ci			
SUELDO O JORNALES L�QUIDOS	g129_1	$	Valor recibido el mes pasado
COMISIONES, INCENTIVOS, HORAS EXTRAS, HABILITACIONES	g129_2	$	Valor recibido el mes pasado
VI�TICOS NO SUJETOS A RENDICI�N	g129_3	$	Valor recibido el mes pasado
PROPINAS	g129_4	$	Valor recibido el mes pasado
AGUINALDO	g129_5	$	Valor recibido el mes pasado
SALARIO VACACIONAL	g129_6	$	Valor recibido el mes pasado
PAGOS ATRASADOS	g129_7	$	Valor recibido el mes pasado
DERECHO A CULTIVO PARA PROPIO CONSUMO	g137_3	$	Monto percibido por la venta de esos productos
RETIRO REALIZADO PARA GASTOS DEL HOGAR	g147	$	
DISTRIBUCI�N DE UTILIDADES	g148	$	anual
RECIBI� POR MEDIANER�A O PARCER�A	g150	$	anual
RECIBI� POR PASTOREO	g151	$	anual
RECIBI� POR GANADO A CAPITALIZACI�N	g152	$	anual
	

*/
foreach i in g148 g150 g151 g152 {
gen `i'm = `i'/12
}


egen ylmpri_ci=rsum(g129_1	g129_2 g129_3 g129_4 g129_5 g129_6 g129_7 g137_3 g147 g148m g150m g151m g152m) if emp_ci==1, missing

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso s�lo para las personas ocupadas emp_ci==1


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
	RECIBI� ALIMENTOS O BEBIDAS	g130_4	$	Otros - Monto estimado
	RECIBI� TICKETS DE ALIMENTACI�N	g131_2	$	Valor recibido el mes pasado
	RECIBI� VIVIENDA O ALOJAMIENTO	g132_3	$	Monto que habr�a tenido que pagar por ese alojamiento
	RECIBI� OTRO TIPO DE RETRIBUCI�N EN ESPECIE	g134_2	$	Monto que habr�a tenido que pagar por esos bienes
	RECIBI� ALG�N OTRO TIPO DE COMPLEMENTO PAGADO POR EL EMPLEADOR	g135_2	$	Monto estimado
Autocons	DERECHO A CULTIVO PARA PROPIO CONSUMO	g137_2	$	Monto que habr�a tenido que pagar por esos alimentos
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador no agropecuario)	g149_2	$	Monto que habr�a tenido que pagar por esos bienes
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_3	$	Valor de lo consumido en carnes o chacinados
Autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g149_4	$	Valor de lo consumido en l�cteos
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
* Se filtra ingreso s�lo para las personas ocupadas emp_ci==1


	***************
	***ylmsec_ci***
	***************
/*

SUELDO O JORNALES L�QUIDOS	g138_1	$	Valor recibido el mes pasado
COMISIONES, INCENTIVOS, HORAS EXTRAS, HABILITACIONES	g138_2	$	Valor recibido el mes pasado
VI�TICOS NO SUJETOS A RENDICI�N	g138_3	$	Valor recibido el mes pasado
PROPINAS	g138_4	$	Valor recibido el mes pasado
AGUINALDO	g138_5	$	Valor recibido el mes pasado
SALARIO VACACIONAL	g138_6	$	Valor recibido el mes pasado
PAGOS ATRASADOS	g138_7	$	Valor recibido el mes pasado
DERECHO A CULTIVO PARA PROPIO CONSUMO	g146_3	$	Monto percibido por la venta de esos productos


*/

	egen ylmsec_ci=rsum(g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7 g146_3) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 
	
	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso s�lo para las personas ocupadas emp_ci==1


	****************
	***ylnmsec_ci***
	****************
	/*

BOLETOS DE TRANSPORTE	g138_8	$	Valor recibido el mes pasado
RECIBI� ALIMENTOS O BEBIDAS	g139_4	$	Otros - Monto estimado
RECIBI� TICKETS DE ALIMENTACI�N	g140_2	$	Valor recibido el mes pasado
RECIBI� VIVIENDA O ALOJAMIENTO	g141_3	$	Monto que habr�a tenido que pagar por ese alojamiento
RECIBI� OTRO TIPO DE RETRIBUCI�N EN ESPECIE	g143_2	$	Monto que habr�a tenido que pagar por esos bienes
RECIBI� ALG�N OTRO TIPO DE COMPLEMENTO PAGADO POR EL EMPLEADOR	g144_2	$	Monto estimado
Autoconsumo DERECHO A CULTIVO PARA PROPIO CONSUMO	g146_2	$	Monto que habr�a tenido que pagar por esos alimentos
       
*/


/*
RECIBI� ALIMENTOS O BEBIDAS	g139_1	1 = S� / 2 = No	
	g139_2	N�	N�mero de desayunos / meriendas
	g139_3	N�	N�mero de almuerzos / cenas
*/

gen desaysec=(g139_2*mto_desay)
gen almuesec= (g139_3*mto_almue)
*gen cuota = En este a�o no se pregunta acerca de la cuota mutual.

/*
g145_2	N�	Vacunos
g145_3	N�	Ovinos
g145_4	N�	Equinos
*/

gen vacassec = (g145_2*mto_vacas)
gen ovejasec = (g145_3*mto_oveja)
gen caballosec = (g145_4*mto_caball)


egen ylnmsec_ci=rsum(desaysec almuesec vacassec ovejasec caballosec g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2) if emp_ci==1, missing
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso s�lo para las personas ocupadas emp_ci==1
	
**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde alg�n miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

	*****************
	***ylmotros_ci***
	*****************

	egen ylmotros_ci= rsum(g129_1	g129_2 g129_3 g129_4 g129_5 g129_6 g129_7 g137_3 g147 g148m g150m g151m g152m g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7 g146_3) if emp_ci==0, missing
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral monetario otros trabajos para todos los a�os
	
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci= rsum( desay almue cuota vacas oveja caballo g129_8 g130_4 g131_2 g132_3 g134_2 g135_2 g137_2 g149_2 g149_3 g149_4 g149_5 g149_6 g149_7 desaysec almuesec vacassec ovejasec caballosec g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2) if emp_ci==0, missing
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral no monetario otros trabajos para todos los a�os
	
	************
	***ylm_ci***
	************
	egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
	replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==. & ylmotros_ci==.
	label var ylm_ci "Ingreso laboral monetario total"  

	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral monetario otros como parte del ingreso laboral monetario total ya que no hab�a sido incluido
	
	*************
	***ylnm_ci***
	*************
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  
	
	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral no monetario otros como parte del ingreso laboral no monetario total ya que no hab�a sido incluido

	*************
	***ynlm_ci***
	*************
	/*
Recibi� el mes pasado por JUBILACIONES:			
BPS - CAJA INDUSTRIA Y COMERCIO	g153_1	$	
BPS - CAJA CIVIL Y ESCOLAR	g153_2	$	
BPS - RURAL Y SERVICIO DOM�STICO	g153_3	$	
UNI�N POSTAL	g153_4	$	
POLICIAL	g153_5	$	
MILITAR	g153_6	$	
PROFESIONAL	g153_7	$	
NOTARIAL	g153_8	$	
BANCARIA	g153_9	$	
OTRA 	g153_10	$	
OTRO PA�S	g153_11	$	
Recibi� el mes pasado por PENSIONES:			
BPS - CAJA INDUSTRIA Y COMERCIO	g153_12	$	
BPS - CAJA CIVIL Y ESCOLAR	g153_13	$	
BPS - RURAL Y SERVICIO DOM�STICO	g153_14	$	
UNI�N POSTAL	g153_15	$	
POLICIAL	g153_16	$	
MILITAR	g153_17	$	
PROFESIONAL	g153_18	$	
NOTARIAL	g153_19	$	
BANCARIA	g153_20	$	
OTRA 	g153_21	$	
OTRO PA�S	g153_22	$	
SEGURO DE DESEMPLEO	g153_23	$	
COMPENSACIONES POR ACCIDENTE, MATERNIDAD O ENFERMEDAD	g153_24	$	
BECAS, SUBSIDIOS, DONACIONES	g153_25	$	Del pa�s
BECAS, SUBSIDIOS, DONACIONES	g153_26	$	Del extranjero
RECIBE PENSI�N ALIMENTICIA O ALGUNA CONTRIBUCI�N POR DIVORCIO O SEPARACI�N	g156_2	$	Del pa�s
	g156_3	$	Del extranjero
OTROS INGRESOS CORRIENTES	g157_2	$	

RECIBE DINERO DE ALG�N FAMILIAR U OTRO HOGAR EN EL PA�S	h158_2	$


* Variables anuales	a nivel de hogar		
FUERON ALQUILADAS 	h162_2	$	Alquileres del pa�s a
FUERON ALQUILADAS 	h162_3	$	Alquileres del extranjero
RECIBI� POR ARRENDAMIENTO	h165_1	$	Arrendamientos del pa�s
RECIBI� POR ARRENDAMIENTO	h165_2	$	Arrendamientos del extranjero
RECIBI� POR MEDIANER�A	h166	$	
RECIBI� POR PASTOREO	h167	$	
RECIBI� POR GANADO A CAPITALIZACI�N	h168	$	
RECIBI� POR INTERESES	h170_1	$	Intereses del pa�s 
RECIBI� POR INTERESES	h170_2	$	Intereses del extranjero
RECIBI� POR UTILIDADES Y DIVIDENDOS DE ALG�N NEGOCIO EN EL QUE NO TRABAJA	h172_1	$	Utilidades y dividendos del pa�s
	h172_2	$	Utilidades y dividendos del extranjero
RECIBI� INDEMNIZACI�N POR DESPIDO	h173_2	$	
 remesas -RECIBI� ALGUNA COLABORACI�N ECON�MICA DE ALG�N FAMILIAR EN EL EXTERIOR	h174_2	$	
RECIBI� ALG�N INGRESO EXTRAORDINARIO	h175_2	$	
           
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
CU�LES/CU�NTAS CANASTAS RECIBE MENSUALMENTE	e62_1_1	1 = S� / 2 = No	Com�n (INDA)
	e62_1_2	N�	N�mero de veces que recibe la canasta al mes
	e62_2_1	1 = S� / 2 = No	Bajo peso (riesgo nutricional)
	e62_2_2	N�	N�mero de veces que recibe la canasta al mes
	e62_3_1	1 = S� / 2 = No	Plomo
	e62_3_2	N�	N�mero de veces que recibe la canasta al mes
	e62_4_1	1 = S� / 2 = No	Pensionistas
	e62_4_2	N�	N�mero de veces que recibe la canasta al mes
	e62_5_1	1 = S� / 2 = No	Diab�ticos
	e62_5_2	N�	N�mero de veces que recibe la canasta al mes
	e62_6_1	1 = S� / 2 = No	Renales
	e62_6_2	N�	N�mero de veces que recibe la canasta al mes
	e62_7_1	1 = S� / 2 = No	Renal-diab�tico
	e62_7_2	N�	N�mero de veces que recibe la canasta al mes
	e62_8_1	1 = S� / 2 = No	Cel�acos
	e62_8_2	N�	N�mero de veces que recibe la canasta al mes
	e62_9_1	1 = S� / 2 = No	Tuberculosis
	e62_9_2	N�	N�mero de veces que recibe la canasta al mes
	e62_10_1	1 = S� / 2 = No	Oncol�gicos
	e62_10_2	N�	N�mero de veces que recibe la canasta al mes
	e62_11_1	1 = S� / 2 = No	Sida (VIH+)
	e62_11_2	N�	N�mero de veces que recibe la canasta al mes
	e62_12_1	1 = S� / 2 = No	Escolar contexto cr�tico
	e62_12_2	N�	N�mero de veces que recibe la canasta al mes
	e62_13_1	C�d.	Otra
	e62_13_2	N�	N�mero de veces que recibe la canasta al mes
	e62_13_3		Descripci�n

*/

gen canasta = (e62_1_2 * indacomun) + (e62_2_2	 * indabajo) + (e62_3_2 * indaplomo) + (e62_4_2 * indapensi) + ///
(e62_5_2 * indadiabet) + (e62_6_2 * indarenal) + (e62_7_2 * indarendia) + (e62_8_2 * indaceliac) + (e62_9_2 * indatuberc) + ///
(e62_10_2 * indaoncolo) + (e62_11_2 * indasida) + (e62_13_2 * otrcanast)

*CONCURRENCIA A COMEDOR O MERENDERO GRATUITO 

egen transf_desay= rsum(e60_2_1_1 e60_2_1_3 e60_2_2_1 e60_2_2_3 e60_2_3_1 e60_2_3_3 e60_2_4_1 e60_2_4_3 e60_2_5_1 e60_2_5_3 e60_2_6_1 e60_2_6_3), missing
gen tdesay = (transf_desay*4.3)*mto_desay

egen transf_almue= rsum(e60_2_1_2 e60_2_1_4 e60_2_2_2 e60_2_2_4 e60_2_3_2 e60_2_3_4 e60_2_4_2 e60_2_4_4 e60_2_5_2 e60_2_5_4 e60_2_6_2 e60_2_6_4), missing
gen talmue = transf_almue*4.3*mto_almue

/*
e60_2_7	7	S�lo almuerzo
	8	S�lo desayuno o merienda
	9	Desayuno y almuerzo
	10	Almuerzo y Merienda
	11	Desayuno, almuerzo y merienda
RECIBE ALG�N TIPO DE ALIMENTACI�N DE ALG�N PROGRAMA P�BLICO (SALVO CANASTAS)	

e61-1	1 = Si / 2 = No	
e61_2	N�	N�mero de veces que recibe por semana
e61_3	N�	N�mero de veces que recibe por mes
*/
gen tsolalmue = (5*4.3*mto_almue) if e60_2_7==7
gen tsoldesay = (5*4.3*mto_desay) if e60_2_7==8
gen tsoldesalm = (5*4.3*(mto_desay + mto_almue)) if e60_2_7==9 | e60_2_7==10
gen tsoldealme = (5*4.3*(mto_desay+ mto_desay + mto_almue)) if e60_2_7==11
gen salvcana = (e61_3*mto_almue)
replace salvcana= (e61_2*4.3*mto_almue) if e61_3==0

*HOGAR CONSTITUIDO	mto_hogcon	$	Valor del hogar constituido
*COBRA HOGAR CONSTITUIDO	g154_1	1 = S� / 2 = No	
*g154_2	1 = S� / 2 = No	Declarado en el sueldo

gen hogcosnt = mto_hogcon if g154_1==1 & g154_2==2

* Total transferencias
egen transf= rsum(canasta tdesay talmue tsolalmue tsoldesay tsoldesalm tsoldealme salvcana hogcosnt), missing

*Asignaciones familiares no declaradas en el sueldo
/*COBRA ASIGNACIONES FAMILIARES	g155_1	1 = S� / 2 = No	
	g155_2	N�	Cu�ntas
	g155_3	1 = Si / 2 = No	Declaradas en el sueldo
	g155_4	$	Monto que cobr� el mes pasado
	*/

gen asigfam = g155_4 if g155_3 ==2

egen ynlm_ci=rsum(inghog transf asigfam g153_1 g153_2 g153_3 g153_4 g153_5 g153_6 g153_7 g153_8 g153_9 g153_10 g153_11 g153_12 g153_13 g153_14 g153_15 g153_16 g153_17 g153_18 g153_19 g153_20 g153_21 g153_22 g153_23 g153_24 g153_25 g153_26 g156_2 g156_3 g157_2), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	
	*RECIBE AYUDA EN ESPECIE DE ALG�N FAMILIAR U OTRO HOGAR EN EL PA�S	h159_2	$
	
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
	label var nrylmpri_ch "Hogares con alg�n miembro que no respondi� por ingresos"

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
gen g148_ = g148/12
gen g150_ = g150/12
gen g151_ = g151/12
gen g152_ = g151/12

egen ylmpri_ci=rsum(g129_1 g129_2 g129_3 g129_4 g129_5 g129_6 g129_7    g148_ g150_ g151_ g152_) 
replace ylmpri_ci=. if g129_1==. & g129_2==. & g129_3==. & g129_4==. & g129_5==. & g129_6==. & g129_7==. &  g148_==. & g150_==. & g151_==. & g152_==. 

egen ylnmpri_ci=rsum(g129_8 g130_4 g131_2 g132_3 g134_2 g135_2 g137_2) 

egen ylmsec_ci=rsum(g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7)

egen ylnmsec_ci=rsum(g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2)

gen ylmotros_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) 
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) 
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

egen ynlm_ci = rsum(g153_1 g153_2 g153_3 g153_4 g153_5 g153_6 g153_7 g153_8 g153_9 g153_10 g153_11 g153_12 g153_13 g153_14 g153_15 g153_16 g153_17 g153_18 g153_19 g153_20 g153_21 g153_22 g153_23 g153_24 g153_25 g153_26 g155_4 g156_2 g156_3)

gen ynlnm_ci=.

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

gen remesas_ci=h174_2

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
NIVEL Y A�O MAS ALTO ALCANZADO	
			e54_1_1	A�o	A�os cursados en Primaria
			e54_1_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_2_1	A�o	A�os cursados en Secundaria
			e54_2_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_3_1	A�o	A�os cursados en Ense�anza T�cnica
			e54_3_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_3_3	1	Exigencia de ense�anza secundaria completa para realizar curso de UTU
				2	Exigencia de ense�anza secundaria primer ciclo para realizar curso de UTU
				3	Exigencia de ense�anza primaria completa para realizar curso de UTU
				4	Ninguna exigencia
			e52-4_1	A�o	Magisterio o Profesorado
			e52-4_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_5-1	A�o	Universidad o similar
			e54_5_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_6_1	A�o	Terciario no universitario
			e54_6_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52-7-1	A�o	Posgrado (maestr�a o doctorado)
			e54_7_2	1 = Si / 2 = No	Finalizo o no el nivel
*/

gen dpri = 1 if (e54_1_1>0)
gen dsec = 1 if (e54_2_1>0)
gen dtec = 1 if (e54_3_1>0)
gen dmag = 1 if (e54_4_1>0)
gen duni = 1 if (e54_5_1>0)
gen dter = 1 if (e54_6_1>0)
gen dmae = 1 if (e54_7_1>0)

gen aprees = (e52_1>0)
gen aprie  = (e52_2>0)
gen apric  = (e52_3>0)
gen alic   = (e52_4>0)
gen acicb  = (e52_5>0)
gen abachs = (e52_6>0)
gen abacht = (e52_8>0)
gen amag   = (e52_9>0)
gen auniv  = (e52_10>0)
gen aterc  = (e52_11>0)
gen aposg  = (e52_12>0)

/*
De acuerdo al manual del entrevistador 
la formaci�n profesional b�sica no es equivalente al
ciclo basico. Por lo tanto no los a�os en este no se consideran.
En caso de que alguien la haya tomado se consideran que tiene primaria
ya que para tomarla es requisito haber finalizado la pimaria. 
*/

/*  Criterios para la elaboraci�n de a�os de educaci�n aprobados:
       > No se toma en cuenta los a�os de preescolar
	   > Los a�os de educacion primaria especial tambi�n son 6 a�os, como la primaria comun
	   > Solo para a�os 2006 y 2007 se reconoce la educaci�n t�cnica. Pero se asume que todos tiene ciclo basico 
	     (o sea 9 a�os de educacion). Esto para no dejar con cero mising a estos individuos. En los otros a�os no es necesario hacerlo
		 por que las preguntas son tomadas diferentes.
*/

recode e52_6 (4=1) (5=2) (6=3)
recode e52_8 (4=1) (5=2) (6=3)

*Para los que asisten
gen aedu_ci = .
replace aedu_ci = 0         if aprees==1 
replace aedu_ci = e52_2     if aprie==1  & e52_2<=6
replace aedu_ci = e52_3     if apric==1  & e52_3<=6
replace aedu_ci = e52_4+6   if alic==1   & e52_4<=3
replace aedu_ci = e52_5+6   if acicb==1  & e52_5<=3
replace aedu_ci = e52_6+9   if abachs==1 & e52_6<=3
replace aedu_ci = 7         if e52_7>=1 & e52_7!=.
replace aedu_ci = e52_8+9   if abacht==1 & e52_8<=3
replace aedu_ci = e52_9+12  if amag==1   & e52_9<9
replace aedu_ci = e52_10+12 if auniv==1  & e52_10<9
replace aedu_ci = e52_11+12 if aterc==1  & e52_11<9
replace aedu_ci = e52_12+17 if aposg==1  & e52_12<9
replace aedu_ci = aedu_ci-1 if aedu_ci>0 & aedu_ci!=.

*Para los que asistieron
replace aedu_ci = e54_1_1 		    if dpri==1 & e54_1_1 <=6
replace aedu_ci = e54_2_1 + 6		if dsec==1 & e54_2_1 <=6
replace aedu_ci = e54_3_1 + 9		if dtec==1 & e54_3_1 <=6
replace aedu_ci = e54_4_1 + 12		if dmag==1 & e54_4_1 <9
replace aedu_ci = e54_5_1 + 12		if duni==1 & e54_5_1 <9
replace aedu_ci = e54_6_1 + 12		if dter==1 & e54_6_1 <9
replace aedu_ci = e54_7_1 + 17		if dmae==1 & e54_7_1 <9
replace aedu_ci=0                   if e53==2 & (edad>=5 & edad!=.)



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

gen edupre_ci=aprees



/*
e52_10	A�o	Universidad o similar
e52_11	A�o	Terciario no universitario
*/

gen eduac_ci=.
replace eduac_ci = 1 if e52_10>0
replace eduac_ci = 0 if e52_11>0
replace eduac_ci =. if e52_10>=10 | e52_11>=10

/*
ASISTENCIA ACTUAL A ESTABLECIMIENTO DE ENSE�ANZA	e50	1 = Si / 2 = No	
*/

gen asiste_ci=.
replace asiste_ci = 1 if (e50==1)
replace asiste_ci = 0 if (e50==2)


gen pqnoasis_ci=.

gen repite_ci=.
gen repiteult_ci=.

/*
ESTABLECIMIENTO P�BLICO O PRIVADO	e51	1	P�blico
						2	Privado
*/

gen edupub_ci=.
replace edupub_ci = 1 if (e51==1)
replace edupub_ci = 0 if (e51==2)

label var  aedu_ci "Anios de Educacion"
****************
***tecnica_ci **
****************
gen tecnica_ci=(e52_11>=1 & e52_11<=9)
label var tecnica_ci "=1 formacion terciaria tecnica"

/*_____________________________________________________________________________________________________*/
* Verificaci�n de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* Tambi�n se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch	notropari_ch notronopari_ch	nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci lp25_ci lp4_ci	lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

*firmapeq_ci




* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
compress

do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"

saveold "`base_out'", replace


log close




	
