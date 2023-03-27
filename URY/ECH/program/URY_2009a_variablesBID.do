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
local ANO "2009"
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
 							armonización 2009
****************************************************************************************************************************/
***********
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013
destring dpto, replace
gen region_c=  dpto

label define region_c  ///
          1	"Montevideo" ///
2	"Artigas" ///
3	"Canelones" ///
4	"Cerro Largo" ///
5	"Colonia" ///
6	"Durazno" ///
7	"Flores" ///
8	"Florida" ///
9	"Lavalleja" ///
10	"Maldonado" ///
11	"Paysandú" ///
12	"Río Negro" ///
13	"Rivera" ///
14	"Rocha" ///
15	"Salto" ///
16	"San José" ///
17	"Soriano" ///
18	"Tacuarembó" ///
19	"Treinta y tres"

		    
label value region_c region_c
label var region_c "División política, departamento"

****************
* region_BID_c *
****************
gen region_BID_c=.
replace region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/

*1. Factor de expansión del hogar: 
gen factor_ch=pesoan

*2. Identificador del hogar
gen idh_ch=numero

*3. Identificador de persona
gen idp_ci=nper

*A partir de 2006 hay zona rural.

destring region, replace
gen zona_c=.
replace zona_c=1 if region == 1 | region == 2
replace zona_c=0 if region == 3
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

*5. País
gen str3 pais_c="URY"

*6. Anio de la encuesta
gen anio_c=2009

*7. Mes de la encuesta
gen mes_c=real(mes)

*8. Relación o parentesco con el jefe de hogar
/*
e30
1	Jefe
2	Esposo o compañero
3	Hijo de ambos
4	Hijo sólo del jefe
5	Hijo sólo del cónyuge
6	Yerno o nuera
7	Padre o madre
8	Suegro/a
9	Hermano/a
10	Cuñado/a
11	Nieto/a
12	Otro pariente
13	Otro no pariente
14	Servicio doméstico o familiar del mismo
*/

gen relacion_ci=e30
replace relacion_ci=3 if e30==4 | e30==5 | e30==3
replace relacion_ci=4 if e30>=6 & e30<=12
replace relacion_ci=5 if e30==13
replace relacion_ci=6 if e30==14
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

*9. Factor de expansión a nivel individual

gen factor_ci=pesoano

*10. Sexo

/*
e26	1	Hombre
	2	Mujer
*/

gen sexo_ci=e26


*11. Edad

/*
e27	Años	Años cumplidos
*/

gen edad_ci=e27
label var edad_ci "Edad del Individuo"




*12. Estado civil 

/*
ESTADO CIVIL ACTUAL	e36	
				1	Separado/a de unión libre
				2	Divorciado/a
				3	Casado/a (incluye separado y aún no se divorció)
				4	Viudo/a
				5	Soltero/a



gen civil_ci=1 		if e36==5
replace civil_ci=2 	if e36==3
replace civil_ci=3 	if e36==1 | e36==2
replace civil_ci=4 	if e36==4

label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci
*/

*Modificado por SCGR - Abril 2017
*Unión formal o informal*
gen civil_ci=2 		if e33==1
replace civil_ci=1  if e36==5 & e33==2
replace civil_ci=3  if (e36==1 | e36==2 | e36==3) & e33==2
replace civil_ci=4 	if (e36==4) & e33==2

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
	*** afroind_ci ***
	***************
**Pregunta: ¿Cree tener ascendencia...? ¿Cuál considera principal de las declaradas?:(e29_6) (1 - Afro o Negra; 2 - Asiatica o Amarilla; 3 - Blanca; 4 - Indigena; 5 - Otra) 
**En Uruguay puedes reportar más de una identidad pero la pregunta e29_6 pregunta cuál es la identidad principal. 
gen afroind_ci=. 
replace afroind_ci=1 if e29_6 == 4
replace afroind_ci=2 if e29_6 == 1 
replace afroind_ci=3 if e29_6 == 2 | e29_6 == 3 | e29_6 == 5
replace afroind_ci=. if e29_6 ==.

	***************
	*** afroind_ch ***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe

	*******************
	*** afroind_ano_c ***
	*******************
gen afroind_ano_c=2008


	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 


	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 



****************************************************************************************
* VARIABLES DE MERCADO LABORAL
****************************************************************************************
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
gen condocup_ci=.
*replace condocup_ci=1 if f66==1  | f67==1 | f68==1
replace condocup_ci=1 if f66==1  | f67==1 | (f68==1  & f69!=.) /* MLO, 2015,11 : se incluye condicion que no trabajo por razones extraordinarias*/
replace condocup_ci=2 if f106==1 & f107==1 & (f110>=1 & f110<=6) /* SGR, Modificado Mayo 2017 */
replace condocup_ci=2 if f106==1 & f107==2 & ((f108==2 | f108==3) | ((f108==1 | f108==4 | f108==5 | f108==6) & f109==1 & (f110>=1 & f110<=6))) /* SGR, Modificado Mayo 2017 */
*replace condocup_ci=2 if f106==1 & [(f110>=1 & f110<=6) | ((f108>=1 & f108<=3) & (f110>=1 & f110<=6))]
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen msp =.
replace msp =1 if (e45_1_1==1) &  e45_1==1
replace msp =0 if (e45_1_1==2) |  e45_1==2

gen iamc=.
replace iamc=1 if (e45_2_1==1) &  e45_2==1
replace iamc=0 if (e45_2_1>=2 & e45_2_1<=3) |  e45_2==2

gen spm =.
replace spm =1 if (e45_3_1==1) & e45_3==1
replace spm =0 if (e45_3_1>=2 & e45_3_1<=3) | e45_3==2

gen hpm =.
replace hpm=1 if e45_4==1
replace hpm=0 if e45_4==2

gen bps=.
replace bps=1 if e45_5==1
replace bps=0 if e45_5==2

gen afiliado_ci=(msp==1 | iamc==1 | spm==1 | hpm==1 | bps==1)
replace afiliado_ci=. if msp==. & iamc==. & spm==. & hpm==. & bps==.
label var afiliado_ci "Afiliado a la Seguridad Social"
drop msp iamc spm hpm bps
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
replace cotizando_ci=1 if (f82==1 | f96==1) & cotizando_ci==0
label var cotizando_ci "Cotizante a la Seguridad Social"
*Nota: solo seguro social publico, con el cual tenga derecho a pensiones en el futuro.

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (f82==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (f96==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

****************
*instpen_ci*****
****************
gen instpen_ci= .
label var instpen_ci "Institucion a la cual esta afiliado variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=f83
replace instcot_ci=. if instcot_ci==0
label define  instcot_ci 1"bps" 2"bps y afap" 3"policial" 4"militar" 5"profesional" 6 "notarial" 7"bancaria"
label var instcot_ci "institución a la cual cotiza por su trabajo"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
/*
*************
*tamemp_ci***
*************
gen tamemp_ci=f77
replace tamemp_ci=. if f77==0
label define tamemp_ci 1"una persona" 2"2-4 personas" 3"5-9 personas" 4 "10-49 personas" 5"50 o más" 6"10-19 personas" 7"20-49 personas"
label value tamemp_ci tamemp_ci
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
*************
*ypen_ci*
*************
egen yjub=rsum(g148_1_1 g148_1_2 g148_1_3 g148_1_4 g148_1_5 g148_1_6 g148_1_7 g148_1_8 g148_1_9 g148_1_10) 
egen ypen=rsum(g148_2_1 g148_2_2 g148_2_3 g148_2_4 g148_2_5 g148_2_6 g148_2_7 g148_2_8 g148_2_9 g148_2_10) 

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
gen pensionsub_ci= ((f125==1) | (f125==3 & edad_ci>69))
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*************
* cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if f116==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"


*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci = 4693.33688342708 if mes=="01" & region == 1
replace lp_ci = 4699.08990569986 if mes=="02" & region == 1
replace lp_ci = 4735.75183500042 if mes=="03" & region == 1
replace lp_ci = 4758.88947774993 if mes=="04" & region == 1
replace lp_ci = 4796.54825798753 if mes=="05" & region == 1
replace lp_ci = 4884.06018968127 if mes=="06" & region == 1
replace lp_ci = 4939.41088246826 if mes=="07" & region == 1
replace lp_ci = 5026.53207283263 if mes=="08" & region == 1
replace lp_ci = 5043.37328327272 if mes=="09" & region == 1
replace lp_ci = 5041.96661081531 if mes=="10" & region == 1
replace lp_ci = 5050.32415900794 if mes=="11" & region == 1
replace lp_ci = 5075.31255137493 if mes=="12" & region == 1

replace lp_ci = 2781.26103688604 if mes=="01" & region == 2
replace lp_ci = 2784.13244506652 if mes=="02" & region == 2
replace lp_ci = 2805.26200348267 if mes=="03" & region == 2
replace lp_ci = 2820.17059436336 if mes=="04" & region == 2
replace lp_ci = 2842.57047308718 if mes=="05" & region == 2
replace lp_ci = 2893.79968741422 if mes=="06" & region == 2
replace lp_ci = 2923.01391132248 if mes=="07" & region == 2
replace lp_ci = 2973.98474033837 if mes=="08" & region == 2
replace lp_ci = 2983.84532241034 if mes=="09" & region == 2
replace lp_ci = 2984.47290743367 if mes=="10" & region == 2
replace lp_ci = 2989.86821649803 if mes=="11" & region == 2
replace lp_ci = 3004.16212550617 if mes=="12" & region == 2

replace lp_ci = 1583.34490324798 if mes=="01" & region == 3 
replace lp_ci = 1587.58284120537 if mes=="02" & region == 3 
replace lp_ci = 1598.54428367267 if mes=="03" & region == 3 
replace lp_ci = 1615.26298625615 if mes=="04" & region == 3 
replace lp_ci = 1622.17011868331 if mes=="05" & region == 3 
replace lp_ci = 1625.58929529017 if mes=="06" & region == 3 
replace lp_ci = 1635.61368056297 if mes=="07" & region == 3 
replace lp_ci = 1644.93838068831 if mes=="08" & region == 3 
replace lp_ci = 1650.93157152019 if mes=="09" & region == 3 
replace lp_ci = 1656.62083749193 if mes=="10" & region == 3 
replace lp_ci = 1659.51203791058 if mes=="11" & region == 3 
replace lp_ci = 1662.42879642077 if mes=="12" & region == 3 

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 1582.76956220788 if mes=="01" & region == 1
replace lpe_ci = 1563.35283995409 if mes=="02" & region == 1
replace lpe_ci = 1568.54979479167 if mes=="03" & region == 1
replace lpe_ci = 1544.5731615993  if mes=="04" & region == 1
replace lpe_ci = 1542.78375643215 if mes=="05" & region == 1
replace lpe_ci = 1549.47263176182 if mes=="06" & region == 1
replace lpe_ci = 1560.25006937774 if mes=="07" & region == 1
replace lpe_ci = 1582.20139149003 if mes=="08" & region == 1
replace lpe_ci = 1579.18624365576 if mes=="09" & region == 1
replace lpe_ci = 1581.07162573332 if mes=="10" & region == 1
replace lpe_ci = 1576.75335087815 if mes=="11" & region == 1
replace lpe_ci = 1590.1127610752  if mes=="12" & region == 1

replace lpe_ci = 1491.63047183513 if mes=="01" & region == 2
replace lpe_ci = 1473.1195327663  if mes=="02" & region == 2
replace lpe_ci = 1477.53143368142 if mes=="03" & region == 2
replace lpe_ci = 1453.03964287713 if mes=="04" & region == 2
replace lpe_ci = 1450.87382088576 if mes=="05" & region == 2
replace lpe_ci = 1456.84648823547 if mes=="06" & region == 2
replace lpe_ci = 1466.50077768253 if mes=="07" & region == 2
replace lpe_ci = 1487.87290423561 if mes=="08" & region == 2
replace lpe_ci = 1484.6696906361  if mes=="09" & region == 2
replace lpe_ci = 1486.68072126091 if mes=="10" & region == 2
replace lpe_ci = 1482.53777240889 if mes=="11" & region == 2
replace lpe_ci = 1495.15984689311 if mes=="12" & region == 2

replace lpe_ci = 1181.45286778859 if mes=="01" & region == 3 
replace lpe_ci = 1204.97934297307 if mes=="02" & region == 3 
replace lpe_ci = 1202.71329586572 if mes=="03" & region == 3 
replace lpe_ci = 1218.41402459248 if mes=="04" & region == 3 
replace lpe_ci = 1212.73577686034 if mes=="05" & region == 3 
replace lpe_ci = 1213.57047949143 if mes=="06" & region == 3 
replace lpe_ci = 1210.47994183096 if mes=="07" & region == 3 
replace lpe_ci = 1239.99537848058 if mes=="08" & region == 3
replace lpe_ci = 1274.5258405168  if mes=="09" & region == 3 
replace lpe_ci = 1276.57355751258 if mes=="10" & region == 3 
replace lpe_ci = 1297.78353820338 if mes=="11" & region == 3
replace lpe_ci = 1285.81394707842 if mes=="12" & region == 3 

label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci= 4441
label var	salmm_ci "Salario minimo legal 2009"
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

*gen emp_ci=(pobpcoac==2)

*25. Ocupación laboral actividad principal *** Se necesita crear primero la variable emp_ci

replace f71_2="." if f71_2=="X211"

destring f71_2, replace

* Modificacion MGD 07/15/2014: correccion del grupo 9.
gen ocupa_ci=.
replace ocupa_ci=1 if (f71_2>=2110 & f71_2<=3480) & emp_ci==1
replace ocupa_ci=2 if (f71_2>=1110 & f71_2<=1310) & emp_ci==1
replace ocupa_ci=3 if (f71_2>=4110 & f71_2<=4223) & emp_ci==1
replace ocupa_ci=4 if ((f71_2>=5210 & f71_2<=5230) | (f71_2>=9110 & f71_2<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((f71_2>=5111 & f71_2<=5169) | (f71_2>=9120 & f71_2<=9172)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f71_2>=6110 & f71_2<=6210) | (f71_2>=9211 & f71_2<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((f71_2>=7110 & f71_2<=8340) | (f71_2>=9311 & f71_2<=9333)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f71_2>=110 & f71_2<=129) & emp_ci==1
replace ocupa_ci=9 if f71_2>=9999 & emp_ci==1

*26. Rama laboral de la actividad principal   **** Se necesita crear primero la variable emp_ci


****************
*** rama_ci  ***
****************
/*
A QUE SE DEDICA EL ESTABLECIMIENTO DONDE REALIZA LAS TAREAS	f68_1		Descripción
								f72_2	Cód.	Código
*/

replace f72_2="." if f72_2=="." | f72_2=="+512" | f72_2=="<" | f72_2=="X211" | f72_2=="15 4"
destring f72_2, replace

gen rama_ci=.
replace rama_ci=1 if (f72_2>=100 & f72_2<=500) & emp_ci==1
replace rama_ci=2 if (f72_2>=1000 & f72_2<=1429) & emp_ci==1
replace rama_ci=3 if (f72_2>=1500 & f72_2<=3720) & emp_ci==1
replace rama_ci=4 if (f72_2>=4000 & f72_2<=4100) & emp_ci==1
replace rama_ci=5 if (f72_2>=4500 & f72_2<=4550) & emp_ci==1
replace rama_ci=6 if (f72_2>=5010 & f72_2<=5520) & emp_ci==1
replace rama_ci=7 if (f72_2>=6000 & f72_2<=6420) & emp_ci==1
replace rama_ci=8 if (f72_2>=6500 & f72_2<=7499) & emp_ci==1
replace rama_ci=9 if (f72_2>=7500 & f72_2<=9900) & emp_ci==1


*27. Horas totales trabajadas en la actividad principal

gen horaspri_ci=f85
replace horaspri_ci=. if f85==99 | emp_ci==0

*28. Horas totales trabajadas en todas las actividades


gen horastot_ci=f85+f98

*55a. Duración del desempleo

gen durades_ci=f113/4.3 if f113>0
replace durades_ci=. if f116==99

*55. Antigüedad en la actividad actual
gen antigenanio=(f88_1/12)
egen antiguedad_ci=rowtotal(antigenanio  f88_2)
*Mayra Sáenz-NO se encuentra la variable categ_ci  se cambia por condocup_ci.
recode antiguedad_ci 0=. if condocup_ci !=1



/*
*56. Empleado

*cap gen emp_ci=(pobpcoac==2)

*57. Personas que no tienen trabajo y han buscado en el periodo de referencia de la encuesta

gen desemp1_ci=((pobpcoac==3|pobpcoac==4|pobpcoac==5) & f107==1)

*58. Personas que no trabajaron ni buscaron trabajo en la última semanana pero esperan respuesta de una solicitud de empleo

gen desemp2_ci=(desemp1_ci==1 | f107==2 & f108==2 | f108==3)

*59. Personas que no tienen trabajo pero han buscado trabajo en periodos anteriores a la semana pasada

gen desemp3_ci=(desemp2_ci==1 | (f109==1 | f113>=4))


*60. PEA 1

gen pea1_ci=(emp_ci==1 | desemp1_ci==1)

*61. PEA 2

gen pea2_ci=(emp_ci==1 | desemp2_ci==1)

*62. PEA 3

gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
*63. Trabajadores desalentados: Personas que creen que por alguna razón no conseguirán empleo

gen desalent_ci=.

*64. Trabajadores sub-empleados: personas dispuestas a trabajar más pero trabajan 30 horas a la semana o menos
/*
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & (f102==1 ))
replace subemp_ci=. if emp_ci==0
*/

* Modificacion MGD 06/23/2014: horas de la actividad principaln y considerando disponibilidad (subempleo visible).
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30) & (f102==1 & f103==1)


*65. Trabajadores a medio tiempo: personas que trabajan menos de 30 horas a la semana y no quieren trabajar más
* Mod. 2015/11 MLO
*gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & f102==2)
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30 & f102==2)
replace tiempoparc_ci=. if emp_ci==0

*66. Categoría ocupacional en la actividad principal

/*
CATEGORÍA DE LA OCUPACIÓN	f73
					1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
					8	Programa público de empleo
*/

gen categopri_ci=1 	if f73==4
replace categopri_ci=2 	if f73==5 | f73==6 | f73==3
replace categopri_ci=3 	if f73==1 | f73==2 
replace categopri_ci=4 	if f73==7 
replace categopri_ci=. 	if emp_ci!=1

*67. Categoría ocupacional en la actividad secundaria

/*
CATEGORÍA DE LA OCUPACIÓN	f92	
					1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
*/


gen categosec_ci=1 if f92==4
replace categosec_ci=2 if f92==5 | f92==6 | f92==3
replace categosec_ci=3 if f92==1 | f92==2 
replace categosec_ci=4 if f92==7 

*68. Personas empleadas que han firmado un contrato de trabajo


gen contrato_ci=.


*69. Personas que cuentan con seguro social

gen segsoc_ci=.


*70. Número de empleos

gen nempleos_ci=1 if f70==1
replace nempleos_ci=2 if f70>1 & f70!=.

*71. Trabajadores formales

/*
TAMAÑO  DE LA EMPRESA 	f77
				1	Una persona
				2	2 a 4 personas
				3	5 a 9 personas
				4	10 a 49  personas
				5	50 o más personas
*/

/*
gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & f77==1 | f77==2
replace firmapeq_ci=0 if emp_ci==1 & f77>2
*/



*72. Personas que trabajan en el sector público

gen spublico_ci=(emp_ci==1 & f73==2)
replace spublico =. if emp_ci==.

*Genera la variable para empresas pequeñas
gen tamemp_ci=1 if f77==1 | f77==2 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if f77==3 | f77==4
*Empresas grandes
replace tamemp_ci=3 if f77==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if f124_1==1 | f124_2==1
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if f124_4==1
*Quehaceres del Hogar
replace categoinac_ci=3 if f124_5==1
*Otra razon
replace categoinac_ci=4 if f124_3==1 
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
SUELDO O JORNALES LÍQUIDOS	g126_1	$	Monto percibido el mes pasado
COMISIONES, INCENTIVOS, HORAS EXTRAS, HABILITACIONES	g126_2	$	Monto percibido el mes pasado
VIÁTICOS NO SUJETOS A RENDICIÓN	g126_3	$	Monto percibido el mes pasado
PROPINAS	g126_4	$	Monto percibido el mes pasado
AGUINALDO	g126_5	$	Monto percibido el mes pasado
SALARIO VACACIONAL	g126_6	$	Monto percibido el mes pasado
PAGOS ATRASADOS	g126_7	$	Monto percibido el mes pasado
DERECHO A CULTIVO PARA PROPIO CONSUMO	g133_2	$	Monto percibido por la venta de esos productos
RETIRO REALIZADO PARA GASTOS DEL HOGAR	g142	$	
DISTRIBUCIÓN DE UTILIDADES	g143	$	anual
RECIBIÓ POR MEDIANERÍA O PARCERÍA	g145	$	Monto percibido en los últimos 12 meses
RECIBIÓ POR PASTOREO	g146	$	Monto percibido en los últimos 12 meses
RECIBIÓ POR GANADO A CAPITALIZACIÓN	g147	$	Monto percibido en los últimos 12 meses

	

*/
foreach i in g143 g145 g146 g147{
gen `i'm = `i'/12
}


egen ylmpri_ci=rsum(g126_1 g126_2 g126_3 g126_4 g126_5 g126_6 g126_7 g133_2 g142 g143m g145m g146m g147m) if emp_ci==1, missing

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
	BOLETOS DE TRANSPORTE	g126_8	$	Monto percibido el mes pasado
	RECIBIÓ ALIMENTOS O BEBIDAS	g127_1	Nº	Número de desayunos / meriendas
	RECIBIÓ ALIMENTOS O BEBIDAS	g127_2	Nº	Número de almuerzos / cenas
	RECIBIÓ ALIMENTOS O BEBIDAS	g127_3	$	Otros - Monto estimado
	RECIBIÓ TICKETS DE ALIMENTACIÓN	g128_1	$	Monto recibido el mes pasado
	RECIBIÓ VIVIENDA O ALOJAMIENTO	g129_2	$	Monto que habría tenido que pagar por ese alojamiento
	RECIBIÓ OTRO TIPO DE RETRIBUCIÓN EN ESPECIE	g130_1	$	Monto que habría tenido que pagar por esos bienes
	RECIBIÓ ALGÚN OTRO TIPO DE COMPLEMENTO PAGADO POR EL EMPLEADOR	g131_1	$	Monto estimado
autocons	DERECHO A CULTIVO PARA PROPIO CONSUMO	g133_1	$	Monto que habría tenido que pagar por esos alimentos
autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador no agropecuario)	g144_1	$	Monto que habría tenido que pagar por esos bienes
autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g144_2_1	$	Valor de lo consumido en carnes o chacinados
autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g144_2_2	$	Valor de lo consumido en lácteos
autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g144_2_3	$	Valor de lo consumido en huevos y aves
autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g144_2_4	$	Valor de lo consumido en productos de la huerta
autocons	RETIRO DE PRODUCTOS PARA CONSUMO PROPIO (trabajador agropecuario)	g144_2_5	$	Valor consumido en otros alimentos

*/
gen desay=(g127_1*mto_desay)
gen almue= (g127_2*mto_almue)
*gen cuota = En este año no se pregunta acerca de la cuota mutual.
/*
DERECHO A PASTOREO	g132_1	Nº	Vacunos
DERECHO A PASTOREO	g132_2	Nº	Ovinos
DERECHO A PASTOREO	g132_3	Nº	Equinos
*/

gen vacas = (g132_1*mto_vacas)
gen oveja = (g132_2*mto_oveja)
gen caballo = (g132_3*mto_caball)

*No costa la variable disse en el formulario.

egen ylnmpri_ci= rsum( desay almue vacas oveja caballo g126_8 g127_3 g128_1 g129_2 g130_1 g131_1 g133_1 g144_1 g144_2_1 g144_2_2 g144_2_3 g144_2_4 g144_2_5) if emp_ci==1, missing
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1


	***************
	***ylmsec_ci***
	***************
/*
SUELDO O JORNALES LÍQUIDOS	g134_1	$	Monto percibido el mes pasado
COMISIONES, INCENTIVOS, HORAS EXTRAS, HABILITACIONES	g134_2	$	Monto percibido el mes pasado
VIÁTICOS NO SUJETOS A RENDICIÓN	g134_3	$	Monto percibido el mes pasado
PROPINAS	g134_4	$	Monto percibido el mes pasado
AGUINALDO	g134_5	$	Monto percibido el mes pasado
SALARIO VACACIONAL	g134_6	$	Monto percibido el mes pasado
PAGOS ATRASADOS	g134_7	$	Monto percibido el mes pasado
DERECHO A CULTIVO PARA PROPIO CONSUMO	g141_2	$	Monto percibido por la venta de esos productos

*/


	egen ylmsec_ci=rsum(g134_1 g134_2 g134_3 g134_4 g134_5 g134_6 g134_7 g141_2) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
	****************
	***ylnmsec_ci***
	****************
	/*

	BOLETOS DE TRANSPORTE	g134_8	$	Monto percibido el mes pasado
	RECIBIÓ ALIMENTOS O BEBIDAS	g135_3	$	Otros - Monto estimado
	RECIBIÓ TICKETS DE ALIMENTACIÓN	g136_1	$	Valor recibido el mes pasado
	RECIBIÓ VIVIENDA O ALOJAMIENTO	g137_2	$	Monto que habría tenido que pagar por ese alojamiento
	RECIBIÓ OTRO TIPO DE RETRIBUCIÓN EN ESPECIE	g138_1	$	Monto que habría tenido que pagar por esos bienes
	RECIBIÓ ALGÚN OTRO TIPO DE COMPLEMENTO PAGADO POR EL EMPLEADOR	g139_1	$	Monto estimado
autocons	DERECHO A CULTIVO PARA PROPIO CONSUMO	g141_1	$	Monto que habría tenido que pagar por esos alimentos

        
*/


/*RECIBIÓ ALIMENTOS O BEBIDAS	g135_1	Nº	Número de desayunos / meriendas
RECIBIÓ ALIMENTOS O BEBIDAS	g135_2	Nº	Número de almuerzos / cenas
DERECHO A PASTOREO	g140_1	Nº	Vacunos
DERECHO A PASTOREO	g140_2	Nº	Ovinos
DERECHO A PASTOREO	g140_3	Nº	Equinos
*/

gen desaysec=(g135_1*mto_desay)
gen almuesec= (g135_2*mto_almue)
*gen cuota = En este año no se pregunta acerca de la cuota mutual.

gen vacassec = (g140_1*mto_vacas)
gen ovejasec = (g140_2*mto_oveja)
gen caballosec = (g140_3*mto_caball)


egen ylnmsec_ci=rsum(desaysec almuesec vacassec ovejasec caballosec g134_8 g135_3 g136_1 g137_2 g138_1 g139_1 g141_1) if emp_ci==1, missing
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

	egen ylmotros_ci= rsum(g126_1 g126_2 g126_3 g126_4 g126_5 g126_6 g126_7 g133_2 g142 g143m g145m g146m g147m g134_1 g134_2 g134_3 g134_4 g134_5 g134_6 g134_7 g141_2) if emp_ci==0, missing
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral monetario otros trabajos para todos los años
	
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci= rsum( desay almue vacas oveja caballo g126_8 g127_3 g128_1 g129_2 g130_1 g131_1 g133_1 g144_1 g144_2_1 g144_2_2 g144_2_3 g144_2_4 g144_2_5 desaysec almuesec vacassec ovejasec caballosec g134_8 g135_3 g136_1 g137_2 g138_1 g139_1 g141_1) if emp_ci==0, missing
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
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  
	
	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral no monetario otros como parte del ingreso laboral no monetario total ya que no había sido incluido

	*************
	***ynlm_ci***
	*************
	/*
BPS - CAJA INDUSTRIA Y COMERCIO	g148_1_1	$	Monto percibido el mes pasado
BPS - CAJA CIVIL Y ESCOLAR	g148_1_2	$	Monto percibido el mes pasado
BPS - RURAL Y SERVICIO DOMÉSTICO	g148_1_3	$	Monto percibido el mes pasado
UNIÓN POSTAL	g148_1_4	$	Monto percibido el mes pasado
POLICIAL	g148_1_5	$	Monto percibido el mes pasado
MILITAR	g148_1_6	$	Monto percibido el mes pasado
PROFESIONAL	g148_1_7	$	Monto percibido el mes pasado
NOTARIAL	g148_1_8	$	Monto percibido el mes pasado
BANCARIA	g148_1_9	$	Monto percibido el mes pasado
OTRA 	g148_1_10	$	Monto percibido el mes pasado
OTRO PAÍS	g148_1_11	$	Monto percibido el mes pasado
BPS - CAJA INDUSTRIA Y COMERCIO	g148_2_1	$	Monto percibido el mes pasado
BPS - CAJA CIVIL Y ESCOLAR	g148_2_2	$	Monto percibido el mes pasado
BPS - RURAL Y SERVICIO DOMÉSTICO	g148_2_3	$	Monto percibido el mes pasado
UNIÓN POSTAL	g148_2_4	$	Monto percibido el mes pasado
POLICIAL	g148_2_5	$	Monto percibido el mes pasado
MILITAR	g148_2_6	$	Monto percibido el mes pasado
PROFESIONAL	g148_2_7	$	Monto percibido el mes pasado
NOTARIAL	g148_2_8	$	Monto percibido el mes pasado
BANCARIA	g148_2_9	$	Monto percibido el mes pasado
OTRA 	g148_2_10	$	Monto percibido el mes pasado
OTRO PAÍS	g148_2_11	$	Monto percibido el mes pasado
SEGURO DE DESEMPLEO	g148_3	$	Monto percibido el mes pasado
COMPENSACIONES POR ACCIDENTE, MATERNIDAD O ENFERMEDAD	g148_4	$	Monto percibido el mes pasado
BECAS, SUBSIDIOS, DONACIONES	g148_5_1	$	Del país
	g148_5_2	$	Del extranjero
RECIBE PENSIÓN ALIMENTICIA O ALGUNA CONTRIBUCIÓN POR DIVORCIO O SEPARACIÓN	g153_1	$	Del país
RECIBE PENSIÓN ALIMENTICIA O ALGUNA CONTRIBUCIÓN POR DIVORCIO O SEPARACIÓN	g153_2	$	Del extranjero
OTRO INGRESO CORRIENTE ADEMÁS DE LOS DECLARADOS	g154_1	$	Monto que cobró el mes pasado



RECIBE DINERO DE ALGÚN FAMILIAR U OTRO HOGAR EN EL PAÍS	h155_1	$
TARJETA ALIMENTARIA DE INDA/MIDES	h157_1	$


* Variables anuales	a nivel de hogar		
	FUERON ALQUILADAS 	h160_1	$	Alquileres del país
	FUERON ALQUILADAS 	h160_2	$	Alquileres del extranjero
	RECIBIÓ POR ARRENDAMIENTO	h163_1	$	Arrendamientos del país
	RECIBIÓ POR ARRENDAMIENTO	h163_2	$	Arrendamientos del extranjero
	RECIBIÓ POR MEDIANERÍA	h164	$	
	RECIBIÓ POR PASTOREO	h165	$	
	RECIBIÓ POR GANADO A CAPITALIZACIÓN	h166	$	
	RECIBIÓ POR INTERESES	h168_1	$	Intereses del país 
	RECIBIÓ POR INTERESES	h168_2	$	Intereses del extranjero
	RECIBIÓ POR UTILIDADES Y DIVIDENDOS DE ALGÚN NEGOCIO EN EL QUE NO TRABAJA	h170_1	$	Utilidades y dividendos del país
	RECIBIÓ POR UTILIDADES Y DIVIDENDOS DE ALGÚN NEGOCIO EN EL QUE NO TRABAJA	h170_2	$	Utilidades y dividendos del extranjero
	RECIBIÓ POR UTILIDADES Y DIVIDENDOS DE ALGÚN NEGOCIO EN EL QUE NO TRABAJA	h171_1	$	
remesas	RECIBIÓ ALGUNA COLABORACIÓN ECONÓMICA DE ALGÚN FAMILIAR EN EL EXTERIOR	h172_1	$	
	RECIBIÓ ALGÚN INGRESO EXTRAORDINARIO	h173_1	$	

           
*/


foreach i in h160_1 h160_2 h163_1 h163_2 h164 h165 h166 h168_1 h168_2 h170_1 h170_2 h171_1 h172_1 h173_1{
gen `i'm=`i'/12 /*Estos estan definidos en base anual!*/
}

bys idh_ch: egen numper = sum(miembros_ci)
bys idh_ch: egen npermax = max(numper)
drop numper
* Los ingresos a nivel de hogar se dividen para los miembros del hogar y se obtiene un per capita.
egen inghog1 = rsum(h155_1 h157_1 h160_1m h160_2m h163_1m h163_2m h164m h165m h166m h168_1m h168_2m h170_1m h170_2m h171_1m h172_1m h173_1m), missing
gen inghog= inghog1/npermax

*Transferencias de programas sociales

/*
	CUÁLES/CUÁNTAS CANASTAS RECIBE MENSUALMENTE	e59_1	1 = Sí / 2 = No	Común (INDA)
	e59_1_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_2	1 = Sí / 2 = No	Bajo peso (riesgo nutricional)
	e59_2_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_3	1 = Sí / 2 = No	Plomo
	e59_3_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_4	1 = Sí / 2 = No	Pensionistas
	e59_4_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_5	1 = Sí / 2 = No	Diabéticos
	e59_5_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_6	1 = Sí / 2 = No	Renales
	e59_6_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_7	1 = Sí / 2 = No	Renal-diabético
	e59_7_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_8	1 = Sí / 2 = No	Celíacos
	e59_8_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_9	1 = Sí / 2 = No	Tuberculosis
	e59_9_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_10	1 = Sí / 2 = No	Oncológicos
	e59_10_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_11	1 = Sí / 2 = No	Sida (VIH+)
	e59_11_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_12	1 = Sí / 2 = No	Escolar contexto crítico
	e59_12_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_13	1 = Sí / 2 = No	Otro
	e59_13_1	Nº	Cantidad de veces que recibe la canasta al mes
	e59_13_2		Descripción


*/

gen canasta1 = (e59_1_1 * indacomun) + (e59_2_1	 * indabajo) + (e59_3_1 * indaplomo) + (e59_4_1 * indapensi) + ///
(e59_5_1 * indadiabet) + (e59_6_1 * indarenal) + (e59_7_1 * indarendia) + (e59_8_1 * indaceliac) + (e59_9_1 * indatuberc) + ///
(e59_10_1 * indaoncolo) + (e59_11_1 * indasida) 
*CONCURRENCIA A COMEDOR O MERENDERO GRATUITO 

gen tdesay = (e57_3_1*4.3)*mto_desay

gen talmue = (e57_3_2*4.3)*mto_almue

/*
RECIBE ALGÚN TIPO DE ALIMENTACIÓN DE ALGÚN PROGRAMA PÚBLICO (SALVO CANASTAS)	

e60-1	1 = Si / 2 = No	
e60_2	Nº	Número de veces que recibe por semana
e60_3	Nº	Número de veces que recibe por mes
*/

*Merienda o cena
gen tsolalmue = (e57_3_3*4.3*mto_desay) 
gen tsoldesay = (e57_3_4*4.3*mto_desay) 

gen salvcana = (e58_1*4.3*mto_almue) 

*HOGAR CONSTITUIDO	mto_hogcon	$	Valor del hogar constituido
*COBRA HOGAR CONSTITUIDO	g149	1 = Sí / 2 = No	
*	g149_1	1 = Sí / 2 = No	Declarado en el sueldo


gen hogcosnt = mto_hogcon if g149==1 & g149_1==2

* Total transferencias
egen transf= rsum(canasta1 tdesay talmue tsolalmue tsoldesay salvcana hogcosnt), missing


egen ynlm_ci=rsum(inghog transf g148_1_1 g148_1_2 g148_1_3 g148_1_4 g148_1_5 g148_1_6 g148_1_7 g148_1_8 g148_1_9 g148_1_10 g148_1_11 g148_2_1 g148_2_2 g148_2_3 g148_2_4 g148_2_5 g148_2_6 g148_2_7 g148_2_8 g148_2_9 g148_2_10 g148_2_11 g148_3 g148_4 g148_5_1 g148_5_2 g153_1 g153_2 g154_1), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	*RECIBE AYUDA EN ESPECIE DE ALGÚN FAMILIAR U OTRO HOGAR EN EL PAÍS	h156_1
	
	gen ynlnm_ci= (h156_1/npermax)
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci= (h172_1m/npermax)
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
	***autocons_ci***
	*****************
	egen autocons_ci= rsum(g141_1 g133_1 g144_1 g144_2_1 g144_2_2 g144_2_3 g144_2_4 g144_2_5), missing
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
*29. Ingreso laboral monetario actividad principal
gen g143_ = g143/12
gen g145_ = g145/12
gen g146_ = g146/12
gen g147_ = g147/12
egen ylmpri_ci=rsum(g126_1 g126_2  g126_3  g126_4  g126_5  g126_6  g126_7    g143_ g145_ g146_ g147_) 
replace ylmpri_ci=. if (g126_1==. & g126_2==. & g126_3==. &  g126_4==. &  g126_5==. &  g126_6==. &  g126_7==. &  g143_==. & g145_==. & g146_==. & g147_==.)

*30. Ingreso laboral no monetario actividad principal

egen ylnmpri_ci=rsum(g126_8 g127_3  g128_1 g129_2 g130_1 g131_1 g133_1 ) if emp_ci==1


*31. Ingreso Laboral Monetario actividad secundaria

egen ylmsec_ci=rsum(g134_1 g134_2  g134_3  g134_4  g134_5  g134_6  g134_7  ) if emp_ci==1

*32. Ingreso laboral no monetario actividad secundaria

egen ylnmsec_ci=rsum(g134_8 g135_3 g136_1 g137_2 g138_1 g139_1 g141_1) if emp_ci==1

*33. Ingreso laboral monetario otros trabajos

gen ylmotros_ci=.

*34. Ingreso laboral no monetario otros trabajos

gen ylnmotros_ci=.

*35. Identificador de No respuesta (NR) del ingreso de la actividad principal

gen nrylmpri=.

*36. Identificador del top-code del ingreso de la actividad principal

gen tcylmpri_ci=.



*gen ynlm_ci


*37. Ingreso laboral monetario total

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) 
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*38. Ingreso laboral no monetario total

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) 
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*39. Ingreso no laboral monetario (otras fuentes)

egen ynlm_ci = rsum(g148_1_1 g148_1_2  g148_1_3 g148_1_4  g148_1_5  g148_1_6  g148_1_7  g148_1_8  g148_1_9  g148_1_10  g148_1_11 g148_2_1 g148_2_2 g148_2_3 g148_2_4 g148_2_5 g148_2_6 g148_2_7 g148_2_8 g148_2_9 g148_2_10 g148_2_11  g148_3 g148_4 g148_5_1 g148_5_2 g153_1 g153_2 g154_1 )

*40. Ingreso no laboral no monetario

gen ynlnm_ci=.

*41. Identificador de los hogares en donde alguno de los miembros no sabe/No responde el ingreso de
*la actividad principal.

gen nrylmpri_ch=.

*42. Identificador de los hogares en donde alguno de los miembros reporta
*como top code el ingreso de la actividad principal

gen tcylmpri_ch=.


*43. Ingreso laboral monetario del hogar	
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6

*44. Ingreso laboral no monetario del hogar

by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6


*45. Ingreso no laboral monetario del hogar


*46. Ingreso no laboral monetario del hogar
* gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci) if relacion_ci!=6


*47. Salario monetario de la actividad principal en horas

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0

*48. Salario monetario de todas las actividades

gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0

*50. Rentas imputadas del hogar

gen rentaimp_ch=.

*51. Autoconsumo reportado por el individuo

gen autocons_ci=.

*52. Autoconsumo del hogar
gen autocons_ch=.

*53. Remesas reportadas por el individuo

gen remesas_ci=h172_1

*54. Remesas del hogar

gen remesas_ch=h172_1
*/
*******************************************************************************************************
********************************VARIABLES EDUCATIVAS***************************************************
*******************************************************************************************************

**************
***AEDU_CI****
**************

/*Para la suma de años educativos se generan variables temporales de maximo 
para niveles que son equivalentes, se imputa el maximo correspondiente 
a cada nivel de manera de no sobreestimar los años de educacion aprobados*/


egen cb_añostc = rowmax(e51_4 e51_7) if e51_7_1 == 3 /*computa el maximo de CB o tecnico con requisito primaria*/
egen bach_años = rowmax(e51_5 e51_6) /*computa el maximo de bachillerato o bachillerato tecnologico*/
egen bach_añostc = rowmax(e51_5 e51_6 e51_7) if e51_7_1 == 2 /*computa el maximo de bachillerato, bachillerato tecnologico  o tecnica con requisito CB*/
egen sup_años = rowmax(e51_8 e51_9 e51_10) /*computa el maximo de superior: magisterio, universitario  o terciario no universitario */
egen sup_añostc = rowmax(e51_8 e51_9 e51_10 e51_7) if e51_7_1 == 1 /*computa el maximo de superior: magisterio, universitario, terciario no universitario o tecnico con requisito bachillerato completo */

/* Se generan años aprobados para los niveles remplazando por missing 
el codigo de perdido (9) */
gen años_cb = e51_4 
replace años_cb = cb_añostc if e51_7_1 == 3
replace años_cb =. if años_cb == 9

gen años_bc = bach_años
replace años_bc = bach_añostc if e51_7_1 == 2
replace años_bc =. if años_bc == 9

gen años_sup = sup_años
replace años_sup = sup_añostc if e51_7_1 == 1
replace años_sup =. if años_sup == 9

gen años_prim = e51_2
replace años_prim =. if años_prim == 9

gen años_post = e51_11
replace años_post =. if años_post == 9

** Se genera aedu_ci 

gen aedu_ci = 0
qui foreach v of var años_prim años_cb años_bc años_sup años_post {

	replace aedu_ci = aedu_ci + `v' if !missing(`v')

}
replace aedu_ci =. if (años_prim==. & años_cb==. & años_bc==. & años_sup==. & años_post)
replace aedu_ci =. if e51_2 == 9
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

gen byte eduui_ci=(aedu_ci>12 & e51_8<4) | (aedu_ci>12 & e51_10<3) | (aedu_ci>12 & e51_9<4) // magisterio, profesorado, tecnica, universitaria
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta" 


***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>12 & e51_8>=4 & e51_8!=9) | (aedu_ci>12 & e51_10>=3 & e51_10!=9) | (aedu_ci>12 & e51_9>=4 & e51_9!=9) // magisterio, tecnica, universitaria
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
***eduac_ci****
***************

gen eduac_ci=.
replace eduac_ci = 0 if aedu_ci>12 & e51_8>0 & e51_8!=9 // magisterio o profesorado
replace eduac_ci = 0 if aedu_ci>12 & e51_10>0 & e51_10!=9 // terciario no univ
replace eduac_ci = 1 if aedu_ci>12 & e51_9>0 & e51_9!=9 // universidad o similar

*88. Personas que actualmente asisten a centros de ensenanza

***************
***asiste_ci***
***************

gen byte asiste_ci = (e49==1)


*89. Razones para no asistir a la escuela

***************
**pqnoasis_ci**
***************

gen pqnoasis_ci=.
	
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci =.

*90. Personas que han repetido al menos un ano o grado

**************
**repite_ci***
**************

gen repite_ci=.

*91. Personas que han repetido el último grado

**************
*repiteult_ci*
**************

gen repiteult_ci=.

*92. Personas que asisten a centros de ensenanza públicos

**************
**edupub_ci***
**************

gen edupub_ci =.

************************************************************

*93. Acceso a una fuente de agua por red

/*
d11
1 Red general
2 Pozo surgente no protegido
3 Pozo surgente protegido
4 Aljibe
5 Arroyo, río
6 Otro
*/

gen aguared_ch=(d11==1)
replace aguared_ch =. if d11==.


*94. Ubicación principal de la fuente de agua

gen aguadist_ch=d12
replace aguadist_ch=. if d12==4

*95. La principal fuente de agua es unimproved según los mdg

gen aguamala_ch=(d11==4|d11==5) 
replace aguamala_ch =. if d11==.

*96. El hogar usa un medidor para pagar por su consumo de agua


gen aguamide_ch=.

*97. La principal fuente de iluminación es electricidad

gen luz_ch=(d18==1)


*98. El hogar usa un medidor para pagar la electricidad

gen luzmide_ch=.

*99. El combustible principal usado en el hogar es gas o electricidad

gen combust_ch=1 if d20==1 | d20==2 | d20==3 | d20==4
replace combust_ch=0 if combust_ch==.

*100. El hogar tiene algún tipo de servicio higíenico

gen bano_ch=1 if d13<3
replace bano_ch=0 if d13==3

*101. El servicio higiénico es de uso exclusivo del hogar

gen banoex_ch=1 if d15==1
replace banoex_ch=0 if d15==2

*102. Tipo de desagüe incluyendo la definición de unimproved del MDG


gen des1_ch=.
replace des1_ch=0 if d13==3
replace des1_ch=1 if d16==1 | (d16==2 & d13==1)
replace des1_ch=2 if (d16==2 & d13==2)
replace des1_ch=3 if d16==3 


*Tipo de desagüe sin incluir la definición de unimproved de los MDG
gen des2_ch=.
replace des2_ch= 0 if des1_ch==0
replace des2_ch= 1 if des1_ch==1 | des1_ch==2 | des1_ch==3
replace des2_ch= 2 if d16 ==4


*104. Materiales de construcción del piso

* Modificaciones Marcela Rubio Septiembre 2014
/*
gen piso_ch=.

replace piso_ch=0 if c4==5
replace piso_ch=1 if c4==1 | c4==2 | c4==3 | 4
*/

gen piso_ch = .
replace piso_ch = 0 if c4==5
replace piso_ch = 1 if c4>=1 & c4<=3
replace piso_ch = 2 if c4==4

*105. Materiales de construcción de las paredes

gen pared_ch=.

replace pared_ch=0 if c2==6
replace pared_ch=1 if c2<6

*106. Materiales de construcción del techo

gen techo_ch=.

replace techo_ch=0 if c3==6 | c3==5
replace techo_ch=1 if c3<5

*107. Método de eliminación de residuos

gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if d11 ==1 | d11 ==3 
replace aguamejorada_ch = 0 if d11 ==2 | (d11 >=4 & d11 <=6)

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (d13 ==1 | d13 ==2) & d15 ==1 & (d16 ==1 | d16 ==2)
replace banomejorado_ch = 0 if  ((d13 ==1 | d13 ==2) & d15 ==2) | d13 ==3 | ((d13 ==1 | d13 ==2) & d15 ==1 & (d16 ==3 | d16 ==4))
		
*108. Cantidad de habitaciones que se destinan exclusivamente para dormir
gen dorm_ch=d10

*109. Cantidad de habitaciones en el hogar

gen cuartos_ch=d9

*110. Si existe un cuarto separado para cocinar

gen cocina_ch=.
replace cocina_ch=1 if d19==1 | d19==2
replace cocina_ch=0 if d19==3

*111. El hogar tiene servicio telefónico fijo

gen telef_ch=d21_17
replace telef_ch=0 if telef_ch==2

*112. El hogar posee heladera o refrigerador

gen refrig_ch=d21_3
replace refrig_ch=0 if refrig_ch==2


*113. El hogar posee freezer o congelador

gen freez_ch=.

*114. El hogar posee automóvil particular

gen auto_ch=d21_18
replace auto_ch=0 if auto_ch==2

*115. El hogar posee computadora

gen compu_ch=d21_15
replace compu_ch=0 if compu_ch==2

*116. El hogar posee conexión a internet

gen internet_ch=d21_16
replace internet_ch=0 if internet_ch==2

*117. El hogar tiene servicio telefónico celular

replace e60=0 if e60==2
bys idh_ch: egen byte cel=sum(e60)
gen cel_ch=1 if cel>=1 & cel!=.
replace cel_ch=0 if cel==0

*118. Tipo de vivienda en la que reside el hogar

gen vivi1_ch=1 if c1==1
replace vivi1_ch=2 if c1==3 | c1==4
replace vivi1_ch=3 if c1==2 | c1==5

*119. La vivienda en la que reside el hogar es una casa o un departamento

gen vivi2_ch=1 if c1!=5
replace vivi2=0 if vivi2==.
*120. Propiedad de la vivienda

gen viviprop_ch=.
replace viviprop_ch=0 if  d8_1==5
replace viviprop_ch=1 if d8_1==2 | d8_1==4
replace viviprop_ch=2 if d8_1==1 | d8_1==3
replace viviprop_ch=3 if d8_1>=6 & d8_1<=9


*121. El hogar posee un título de propiedad

gen vivitit_ch=.

*122. Alquiler mensual

gen vivialq_ch=d8_3 if viviprop_ch==0

*Alquiler mensual imputado
gen vivialqimp_ch=d8_3 if viviprop_ch~=0


*****************
***rentaimp_ch***
*****************
gen rentaimp_ch=vivialqimp_ch
label var rentaimp_ch "Rentas imputadas del hogar"
	
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
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



rename f72_2 codindustria 
rename f71_2 codocupa 
compress


saveold "`base_out'",  replace


log close
