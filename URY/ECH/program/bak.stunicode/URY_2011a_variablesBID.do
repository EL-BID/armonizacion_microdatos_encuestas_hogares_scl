

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
local ANO "2011"
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
Autores: Yessenia Loayza
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

/***************************************************************************************************************************
 							armonización 2010
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/

destring dpto, replace
gen region_c = dpto
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

*1. Factor de expansión del hogar: 
gen factor_ch=pesoan

*2. Identificador del hogar
gen idh_ch=numero

*3. Identificador de persona
gen idp_ci=nper


*4. Zona urbana versus rural
/*Mayra Sáenz-Noviembre 2013: 
A partir de 2006 se incluye una muestra rural, sin embargo en 2006-2009 sólo se desagregan 3 categorías:
 (1) montevideo, (2) Interior con más de 5000 habitantes, y (3) interior con menos de 5000 habitantes y rural. 
 En cambio, en 2010 se desagregan 4 categorías: (1) montevideo, (2) Interior con más de 5000 habitantes, 
 (3) interior con menos de 5000 habitantes y (4) rural. 
 Por lo tanto, para hacer comparables los datos se genera la variable zona considerando a las zonas de
 menos de 5000 habitantes como rural. Es decir, zona rural=interior con menos de 5000 habitantes y rural.*/
rename región region
gen zona_c=.
replace zona_c=1 if region == 1 | region == 2  
replace zona_c=0 if region == 3 | region == 4

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


*5. País
gen str3 pais_c="URY"

*6. Anio de la encuesta
	

	gen anio_c=2011
	label variable anio_c "Anio de la encuesta" 

*7. Mes de la encuesta
gen mes_c=mes

*8. Relación o parentesco con el jefe de hogar
/*

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

gen relacion_ci=.
replace relacion_ci=1 if e30==1
replace relacion_ci=2 if e30==2
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
*UniÃ³n formal o informal*
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

**********
***raza***
**********
gen raza_ci= .
replace raza_ci=1 if e29_6==4
replace raza_ci=2 if e29_6==1
replace raza_ci=3 if raza_ci== .

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña
gen raza_idioma_ci=.

*NOTE: HERE WE DOUBLE COUNT ACROSS POPULATIONS. There could be a case in which
*someone said that he or she is indigenous and afro-d. We are counting that person
*both in the indigenous and afro dummies to measure statistics WITHIN each population
*However, if we want to sum every population up to 100%, we should use raza_ci instead

*This will apply for the following years as well

gen id_ind_ci = 0
replace id_ind_ci=1 if e29_4==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if e29_1==1
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 


*****************************************************************************************
*  VARIABLES DE MERCADO LABORAL
*****************************************************************************************
	

****************
****condocup_ci*
****************
gen condocup_ci=.
*replace condocup_ci=1 if f66==1  | f67==1 | f68==1
replace condocup_ci=1 if f66==1  | f67==1 | (f68==1 & f69!=.) /* MLO, 2015,11 : se incluye condicion que no trabajo por razones extraordinarias*/
replace condocup_ci=2 if f106==1 & f107==1 & (f110>=1 & f110<=6) /* SGR, Modificado Mayo 2017 */
replace condocup_ci=2 if f106==1 & f107==2 & ((f108==2 | f108==3) | ((f108==1 | f108==4 | f108==5 | f108==6) & f109==1 & (f110>=1 & f110<=6))) /* SGR, Modificado Mayo 2017 */
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14

label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"


/*
gen condocup_ci=.
replace condocup_ci=1 if pobpcoac==2
replace condocup_ci=2 if pobpcoac>=3 & pobpcoac<=5
replace condocup_ci=3 if pobpcoac>=6 & pobpcoac<=11
replace condocup_ci=4 if edad_ci<14

label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
*56. Empleado

cap gen emp_ci=(pobpcoac==2)

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

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci = 5442.70347792301 if mes==1 & region == 1
replace lp_ci = 5504.57895766028 if mes==2 & region == 1
replace lp_ci = 5578.05477015902 if mes==3 & region == 1
replace lp_ci = 5608.27693061681 if mes==4 & region == 1
replace lp_ci = 5639.44067596314 if mes==5 & region == 1
replace lp_ci = 5669.00801120842 if mes==6 & region == 1
replace lp_ci = 5708.02203214367 if mes==7 & region == 1
replace lp_ci = 5749.22651263188 if mes==8 & region == 1
replace lp_ci = 5784.4744566796 if mes==9 & region == 1
replace lp_ci = 5822.80314590048 if mes==10 & region == 1
replace lp_ci = 5846.68364925623 if mes==11 & region == 1
replace lp_ci = 5882.59710179738 if mes==12 & region == 1

replace lp_ci = 3220.90207545304 if mes==1 & region == 2
replace lp_ci = 3262.51893432455 if mes==2 & region == 2
replace lp_ci = 3309.42101396761 if mes==3 & region == 2
replace lp_ci = 3324.57183581768 if mes==4 & region == 2
replace lp_ci = 3333.06970053552 if mes==5 & region == 2
replace lp_ci = 3344.40544063129 if mes==6 & region == 2
replace lp_ci = 3370.50134568314 if mes==7 & region == 2
replace lp_ci = 3388.41690776509 if mes==8 & region == 2
replace lp_ci = 3399.90154856245 if mes==9 & region == 2
replace lp_ci = 3416.95612713416 if mes==10 & region == 2
replace lp_ci = 3437.91020595694 if mes==11 & region == 2
replace lp_ci = 3458.60903959948 if mes==12 & region == 2

replace lp_ci = 1774.9424801779 if mes==1 & (region == 3 | region == 4)
replace lp_ci = 1793.30356581568 if mes==2 & (region == 3 | region == 4)
replace lp_ci = 1817.26878055718 if mes==3 & (region == 3 | region == 4)
replace lp_ci = 1825.0152200868 if mes==4 & (region == 3 | region == 4)
replace lp_ci = 1829.51682826245 if mes==5 & (region == 3 | region == 4)
replace lp_ci = 1834.6595653891 if mes==6 & (region == 3 | region == 4)
replace lp_ci = 1848.63088523364 if mes==7 & (region == 3 | region == 4)
replace lp_ci = 1859.00943162552 if mes==8 & (region == 3 | region == 4)
replace lp_ci = 1867.08341237174 if mes==9 & (region == 3 | region == 4)
replace lp_ci = 1879.1241411019 if mes==10 & (region == 3 | region == 4)
replace lp_ci = 1890.3476102816 if mes==11 & (region == 3 | region == 4)
replace lp_ci = 1903.55067020806 if mes==12 & (region == 3 | region == 4)

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 1770.25758299422 if mes==1 & region == 1
replace lpe_ci = 1792.37894944386 if mes==2 & region == 1
replace lpe_ci = 1824.234746527 if mes==3 & region == 1
replace lpe_ci = 1823.77551473641 if mes==4 & region == 1
replace lpe_ci = 1829.48091227301 if mes==5 & region == 1
replace lpe_ci = 1837.53687853917 if mes==6 & region == 1
replace lpe_ci = 1861.55766796429 if mes==7 & region == 1
replace lpe_ci = 1869.18274718614 if mes==8 & region == 1
replace lpe_ci = 1874.62158378084 if mes==9 & region == 1
replace lpe_ci = 1894.08844650229 if mes==10 & region == 1
replace lpe_ci = 1899.24568114567 if mes==11 & region == 1
replace lpe_ci = 1900.89745360066 if mes==12 & region == 1

replace lpe_ci = 1661.23329283699 if mes==1 & region == 2
replace lpe_ci = 1679.16790195228 if mes==2 & region == 2
replace lpe_ci = 1710.18599100286 if mes==3 & region == 2
replace lpe_ci = 1708.4379183755 if mes==4 & region == 2
replace lpe_ci = 1715.02860184896 if mes==5 & region == 2
replace lpe_ci = 1716.31396122907 if mes==6 & region == 2
replace lpe_ci = 1732.54528304271 if mes==7 & region == 2
replace lpe_ci = 1737.44486959855 if mes==8 & region == 2
replace lpe_ci = 1748.497391047 if mes==9 & region == 2
replace lpe_ci = 1762.59640376674 if mes==10 & region == 2
replace lpe_ci = 1765.96436317535 if mes==11 & region == 2
replace lpe_ci = 1766.88740046971 if mes==12 & region == 2

replace lpe_ci = 1492.82465044133 if mes==1 & (region == 3 | region == 4)
replace lpe_ci = 1510.55837246995 if mes==2 & (region == 3 | region == 4)
replace lpe_ci = 1539.00623343289 if mes==3 & (region == 3 | region == 4)
replace lpe_ci = 1537.53637090932 if mes==4 & (region == 3 | region == 4)
replace lpe_ci = 1543.77977652497 if mes==5 & (region == 3 | region == 4)
replace lpe_ci = 1545.00740489349 if mes==6 & (region == 3 | region == 4)
replace lpe_ci = 1559.80130422871 if mes==7 & (region == 3 | region == 4)
replace lpe_ci = 1563.71352866159 if mes==8 & (region == 3 | region == 4)
replace lpe_ci = 1573.91788255028 if mes==9 & (region == 3 | region == 4)
replace lpe_ci = 1587.09276624274 if mes==10 & (region == 3 | region == 4)
replace lpe_ci = 1588.97390291722 if mes==11 & (region == 3 | region == 4)
replace lpe_ci = 1588.78697056547 if mes==12 & (region == 3 | region == 4)

label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci= 6000
label var	salmm_ci	"Salario minimo legal 2010"

****************
*afiliado_ci****
****************

gen msp =.
replace msp =1 if (e45_1_1==1 | e45_1_1==4) &  e45_1==1
replace msp =0 if (e45_1_1==2 | e45_1_1==3) |  e45_1==2

gen iamc=.
replace iamc=1 if (e45_2_1==1 | e45_2_1==6) &  e45_2==1
replace iamc=0 if (e45_2_1>=2 & e45_2_1<=5) |  e45_2==2

gen spm =.
replace spm =1 if (e45_3_1==1 | e45_3_1==6) & e45_3==1
replace spm =0 if (e45_3_1>=2 & e45_3_1<=5) | e45_3==2

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
*MRU - Abril 2014: pego esto porque no estaba copiado correctamente del do-file de la unidad x

****************
*cotizando_ci***
****************
gen cotizando_ci=0 if condocup_ci==1 | condocup_ci==2
replace cotizando_ci=1 if (f82==1 | f96==1) & cotizando_ci==0
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (f82==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (f96==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"
*MRU - Abril 2014: pego esto porque no estaba copiado correctamente del do-file de la unidad X

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

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

gen tipocontrato_ci=. /* No preguntan*/
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci



*************
*cesante_ci* 
*************
/*
gen cesante_ci=1 if f116==1 & categ_ci==2
replace cesante_ci=0 if f116==2 & categ_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	
*/
*Modificado Mayra Sáenz 
gen cesante_ci=1 if f116==1 & condocup_ci==2
replace cesante_ci=0 if f116==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************

egen aux1=rowtotal(g148_1_1 g148_1_2 g148_1_3 g148_1_4 g148_1_5 g148_1_6 g148_1_7 g148_1_8 g148_1_9 g148_1_10), mis /*Se excluyen pensiones recibidas del exterior*/
egen aux2=rowtotal(g148_2_1 g148_2_2 g148_2_3 g148_2_4 g148_2_5 g148_2_6 g148_2_7 g148_2_8 g148_2_9 g148_2_10), mis /*Se excluyen pensiones recibidas del exterior*/

/*gen pension_ci=1 if (edad_ci>=65 & aux1>0 & aux1<.) | (edad_ci>=65 & aux2>0 & aux2<.) 
recode pension_ci .=0 if edad_ci>=65
label var pension_ci "1=Recibe pension contributiva"*/

/*
* Cambio MGD 07/2015:generar sin restriccion de edad
gen pension_ci=1 if (aux1>0 & aux1!=.) | (aux2>0 & aux2!=.) 
*recode pension_ci .=0 if edad_ci>=65
label var pension_ci "1=Recibe pension contributiva"
*/

* MGR, Aug 2015: correción en sintáxis, se generaba como el 100%
egen ypension = rowtotal(aux1 aux2), missing

gen pension_ci= (ypension>0 & ypension!=.)
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
* Cambio MGD 07/2015:mal generada, hay que sumar
recode aux1 aux2 (0=.)
egen 	ypen_ci=rsum(aux1 aux2),m
replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

/*DZ Octubre 2017- Se crea variable pension subsidiada* Dado que la pregunta es excluyente y el programa de pensión subsidiada en Uruguay es para Adultos mayores y/o discapacitados
se pone la condicion de mayor de 70 años (edad para recivir el beneficio) en las personas que afirmaron tener pension por invalidez*/
gen pensionsub_ci= ((f125==1) | (f125==3 & edad_ci>69))
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
*replace ypensub_ci=. if edad_ci<65 | pensionsub_ci==0
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=.
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
/*
* URU 2011
gen salmm_ci= 	20
label var salmm_ci "Salario minimo legal"*/
************
***emp_ci***
************

cap gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1



*63. Trabajadores desalentados: Personas que creen que por alguna razón no conseguirán empleo

gen desalent_ci=.

*27. Horas totales trabajadas en la actividad principal

gen horaspri_ci=f85
replace horaspri_ci=. if f85==99 | emp_ci==0

*28. Horas totales trabajadas en todas las actividades

gen horastot_ci=f85+f98

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
replace categopri_ci=3 	if f73==1 | f73==2 | f73==8
replace categopri_ci=4 	if f73==7 
replace categopri_ci=. 	if emp_ci!=1
*Modificación MLO
replace categopri_ci=0 if f73==8 & condocup_ci==1

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
replace segsoc_ci=1 if f82==1
replace segsoc_ci=0 if f82==2


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

* Modificacion MGD 07/15/2014: mal generada la variable, se dejaban de  lado categorias.
*Genera la variable para empresas pequeñas
gen tamemp_ci=1 if f77==1 | f77==2 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if f77==3 | f77==6 | f77==7
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

*25. Ocupación laboral actividad principal *** Se necesita crear primero la variable emp_ci

replace f71_2="." if f71_2=="X211"

destring f71_2, replace

* Modificacion MGD 07/15/2014: correccion del grupo 9.
gen ocupa_ci=.
replace ocupa_ci=1 if (f71_2>=2110 & f71_2<=3480) & emp_ci==1
replace ocupa_ci=2 if (f71_2>=1110 & f71_2<=1310) & emp_ci==1
replace ocupa_ci=3 if (f71_2>=4110 & f71_2<=4223) & emp_ci==1
replace ocupa_ci=4 if ((f71_2>=5210 & f71_2<=5230) | (f71_2>=9110 & f71_2<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((f71_2>=5111 & f71_2<=5169) | (f71_2>=9120 & f71_2<=9171)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f71_2>=6110 & f71_2<=6210) | (f71_2>=9211 & f71_2<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((f71_2>=7110 & f71_2<=8340) | (f71_2>=9311 & f71_2<=9333)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f71_2>=110 & f71_2<=129) & emp_ci==1
replace ocupa_ci=9 if f71_2>=9999 & emp_ci==1

****************
*** rama_ci  ***
**************** 

replace f72_2="." if f72_2=="." | f72_2=="+512" | f72_2=="<" | f72_2=="X211" | f72_2=="15 4"
destring f72_2, replace force

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


*COmentario Mayra Sáenz 2013- No se encuentra la variable categ_ci se reemplaza por condocup_ci.
*Modificación MLO
drop rama_ci
recode f72_2 (000/500=1 "Agricultura, caza, silvicultura y pesca") (1000/1430=2 "Explotación de minas y canteras") (1500/3700=3 "Industrias manufactureras") /*
*/ (4000/4100=4 "Electricidad, gas y agua") (4500/4550=5 "Construcción") (5000/5520=6 "Comercio, restaurantes y hoteles") /*
*/ (6000/6420=7 "Transporte y almacenamiento") (6500/7499=8 "Establecimientos financieros, seguros e inmuebles") /*
*/ (7500/9900=9 "Servicios sociales y comunales"),gen(rama_ci)
replace rama_ci=. if condocup_ci !=1


*55a. Duración del desempleo

gen durades_ci=f113/4.3 if f113>0
replace durades_ci=. if f116==99

*****************
**antiguedad_ci**
*****************
*Modificación MLO
gen antigenanio=(f88_1/12)
egen antiguedad_ci=rowtotal(antigenanio  f88_2)
*Mayra Sáenz-NO se encuentra la variable categ_ci  se cambia por condocup_ci.
recode antiguedad_ci 0=. if condocup_ci !=1

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

**************************************************************************************************************************
***********************************************INGRESOS*******************************************************************
**************************************************************************************************************************	
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
	* estimo variable ingreso laboral no monetario otros trabajos para todos los años

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

gen canasta1 = (e59_2_1	 * indabajo) + (e59_3_1 * indaplomo) + (e59_4_1 * indapensi) + ///
(e59_5_1 * indadiabet) + (e59_6_1 * indarenal) + (e59_7_1 * indarendia) + (e59_8_1 * indaceliac) + (e59_9_1 * indatuberc) + ///
(e59_10_1 * indaoncolo) + (e59_11_1 * indasida) /*+ (e59_13_1 * otrcanast)*/

*CONCURRENCIA A COMEDOR O MERENDERO GRATUITO e57_4_1	N°	Desayunos y/o meriendas
*e57_4_2	N°	Almuerzos y/o cenas


gen tdesay = (e57_4_1*4.3)*mto_desay

gen talmue = (e57_4_2*4.3)*mto_almue

/*
RECIBE ALGÚN TIPO DE ALIMENTACIÓN DE ALGÚN PROGRAMA PÚBLICO (SALVO CANASTAS)	

RECIBE ALGÚN TIPO DE ALIMENTACIÓN DE ALGÚN PROGRAMA PÚBLICO (EXCLUIDAS CANASTAS)	e58	1 = Sí / 2 = No	
	e58_1	N°	Cantidad de veces que recibe por semana

*/

gen salvcana = (e58_1*4.3*mto_almue) 

*HOGAR CONSTITUIDO	mto_hogcon	$	Valor del hogar constituido
*COBRA HOGAR CONSTITUIDO	g149	1 = Sí / 2 = No	
*	g149_1	1 = Sí / 2 = No	Declarado en el sueldo


gen hogcosnt = mto_hogcon if g149==1 & g149_1==2

* Total transferencias
egen transf= rsum(canasta1 tdesay talmue salvcana hogcosnt), missing


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
	***rentaimp_ch***
	*****************
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

*123. Alquiler mensual imputado

gen vivialqimp_ch=d8_3 if viviprop_ch~=0


	gen rentaimp_ch=vivialqimp_ch

	label var rentaimp_ch "Rentas imputadas del hogar"

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

egen ylmpri_ci=rsum(g126_1 g126_2  g126_3  g126_4  g126_5  g126_6  g126_7  g143_ g145_ g146_ g147_) if emp_ci==1 
replace ylmpri_ci=. if (g126_1==. & g126_2==. & g126_3==. &  g126_4==. &  g126_5==. &  g126_6==. &  g126_7==. &  g143_==. & g145_==. & g146_==. & g147_==.)

*30. Ingreso laboral no monetario actividad principal

egen ylnmpri_ci=rsum(g126_8 g127_3  g128_1 g129_2 g130_1 g131_1 g133_1 ) if emp_ci==1


*31. Ingreso Laboral Monetario actividad secundaria

egen ylmsec_ci=rsum(g134_1 g134_2  g134_3  g134_4  g134_5  g134_6  g134_7) if emp_ci==1

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

gen remesas_ci=.

*54. Remesas del hogar

gen remesas_ch=h172_1
*/

****************************************************************************************************************************
*********************************VARIABLES EDUCATIVAS***********************************************************************
****************************************************************************************************************************

*74. Anios de educación


gen post=1 if e51_11>=1 & e51_11!=9
replace post=0 if e51_11==0

gen terc=1 if e51_10>=1 & e51_10!=9
replace terc=0 if e51_10==0

gen univ=1 if e51_9>=1 & e51_9!=9
replace univ=0 if e51_9==0

gen mag=1 if e51_8>=1 & e51_8!=9
replace mag=0 if e51_8==0

gen enst=1 if e51_7>=1 & e51_7!=9
replace enst=0 if e51_7==0

gen bachtec=1 if e51_6>=1 & e51_6!=9
replace bachtec=0 if e51_6==0

gen bachsec=1 if e51_5>=1 & e51_5!=9
replace bachsec=0 if e51_5==0

gen cbliceo=1 if e51_4>=1 & e51_4!=9
replace cbliceo=0 if e51_4==0

gen priesp=1 if e51_3>=1 & e51_3!=9
replace priesp=0 if e51_3==0

gen pricom=1 if e51_2>=1 & e51_2!=9
replace pricom=0 if e51_2==0

gen preesc=1 if e193==1 | e193==2
replace preesc=0 if  e193==3

/*  Criterios para la elaboración de años de educación aprobados:
       > No se toma en cuenta los años de preescolar
	   > Los años de educacion primaria especial también son 6 años, como la primaria comun
*/
/*
*Ajustando “9” – “No saben/No responden” 

gen e51_2n=e51_2 // Primaria Común
replace e51_2n=0 if e51_2==9

gen e51_4n=e51_4 // Ciclo básico Liceo o  UTU
replace e51_4n=0 if e51_4==9

gen  e51_5n=e51_5 // Bachillerato Secundario
replace e51_5n=0 if e51_5==9

gen e51_6n=e51_6 // Bachiellrato Tecnológico UTU
replace e51_6n=0 if e51_6==9

gen e51_8n=e51_8 //Magisterio
replace e51_8n=0 if e51_8==9

gen e51_9n=e51_9 // Universidad
replace e51_9n=0 if e51_9==9

gen e51_10n=e51_10 // Terciario No Universitario
replace e51_10n=0 if e51_10==9

gen e51_11n=e51_11 // Postgrados
replace e51_11n=0 if e51_11==9

gen aedu_ci=.
replace aedu_ci=0 if preesc==1
replace aedu_ci= e51_2n  + e51_4n + e51_5n+  e51_8n + e51_9n + e51_10n + e51_11n if e51_5n>=e51_6n
replace aedu_ci= e51_2n  + e51_4n + e51_6n + e51_8n + e51_9n + e51_10n + e51_11n if e51_6n> e51_5n 

replace aedu_ci=.  if e51_3>=1 & e51_3<=9 // Educación Especial
replace aedu_ci=.  if e51_7_1>=1 & e51_7_1<=9 // Educación para Adultos
replace aedu_ci=0 if e51_2n==0 & e51_4n==0 & e51_5n==0 & e51_6n==0 & e51_8n==0 & e51_9n==0 & e51_10n==0 & e51_11n==0
*/

** Aug, 2015: Se efectuan cambios en sintaxis de variable aedu_ci en base a revisión por Iván Bornacelly SCL/EDU **
** Ajustado Jul, 2017 por Iván Bornacelly SCL/EDU

gen aedu_ci=.
replace aedu_ci= 0            if preesc==1 
replace aedu_ci= 0           if (e51_2==9  | e51_3==9)
replace aedu_ci= e51_3        if priesp==1  & e51_3<9
replace aedu_ci= e51_2        if pricom==1  & e51_2<9 
replace aedu_ci= e51_4 + 6    if cbliceo==1 & e51_4<9
replace aedu_ci= e51_5 + 9    if bachsec==1 & e51_5<9
replace aedu_ci= e51_6 + 9    if bachtec==1 & (e51_6>e51_5) & (e51_6<9 )
replace aedu_ci= e51_7 + 12   if enst==1 & (e51_7_1==1 | aedu_ci>=12 & aedu_ci!=.) & e51_7<9 // Incluyendo educación técnica - No es educación exclusiva para adultos
replace aedu_ci= e51_8 + 12   if mag==1  & e51_8<9
replace aedu_ci= e51_9 + 12   if univ==1 & e51_9<9
replace aedu_ci= e51_10 + 12  if terc==1 & (e51_10>e51_9) & e51_10<9
replace aedu_ci= e51_11 + 17  if post==1 & e51_11<9 
replace aedu_ci=0             if e49==2 & (edad>=5 & edad!=.)
replace aedu_ci=0             if e49==1 & (edad>=5 & edad!=.) & aedu_ci==. // Población que declara estar asistiendo o haber asistido, pero no reporta ningún nivel o años de educación aprobado


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

gen edupre_ci=(e193==1)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"

****************
***asispre_ci***
****************
	g asispre_ci=.
	replace asispre_ci=1 if e193==1 & e27>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"	

***************
***eduac_ci****
***************

gen eduac_ci=.
replace eduac_ci = 1 if e51_9>0
replace eduac_ci = 0 if e51_10>0
replace eduac_ci =. if e51_9>=10 | e51_10>=10

*88. Personas que actualmente asisten a centros de ensenanza

gen asiste_ci=.

*cambio MLO 01,2014
*replace asiste_ci=1 if e197 == 1 | e201 == 1 |  e212 == 1 | e215 == 1 |  e218 == 1 |   e221 == 1 | e224 == 1
*cambio MGR 06,2015 (corrección señalada por Iván Bornacelly SCL/EDU)
replace asiste_ci=1 if e193==1 | e197 == 1 | e201 == 1 |  e212 == 1 | e215 == 1 |  e218 == 1 |   e221 == 1 | e224 == 1
recode asiste_ci .=0
/* Y.L.. Note: la pregunta en el cuestionario cambia a "asiste o asistió",no se puede 
continuar con la metodología del anio anterior para el calculo de esta variable
replace asiste_ci = 1 if (e49==1)
replace asiste_ci = 0 if (e49==2)
*/

*89. Razones para no asistir a la escuela

gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci =.

*90. Personas que han repetido al menos un ano o grado

gen repite_ci=.

*91. Personas que han repetido el último grado

gen repiteult_ci=.

*92. Personas que asisten a centros de ensenanza públicos

gen edupub_ci=.
*Primaria
replace edupub_ci=1 if e198 ==1 & e197 == 1
replace edupub_ci=0 if e198 ==2 & e197 == 1
*Educación media 
replace edupub_ci=1 if e210_1 ==1 & e201 == 1
replace edupub_ci=0 if e210_1 ==2 & e201 == 1
*Tecnica
replace edupub_ci=1 if e213 ==1 & e212 == 1
replace edupub_ci=0 if e213 ==2 & e212 == 1
*Normal (Magisterio o Profesorado)
replace edupub_ci=1 if e216 ==1 & e215 == 1
replace edupub_ci=0 if e216 ==2 & e215 == 1
*Universitaria
replace edupub_ci=1 if e219 ==1 & e218 == 1
replace edupub_ci=0 if e219 ==2 & e218 == 1
*No universitaria
replace edupub_ci=1 if e222 ==1 & e221 == 1
replace edupub_ci=0 if e222 ==2 & e221 == 1
*Postgrado
replace edupub_ci=1 if e225 ==1 & e224 == 1
replace edupub_ci=0 if e225 ==2 & e224 == 1

* Nota Marcela G. Rubio - Abril 2013
/* Variable había sido generada como missing debido a que estructura de preguntas de educación cambiaron a partir de 2011 por lo que he estimado variable ///
 de una manera diferente a los años previos */

*************
**tecnica_ci*
*************

gen tecnica_ci=.
replace tecnica_ci=1 if e51_10>=1 & e51_10<=9
replace tecnica_ci=0 if e51_9>=1 & e51_9<=9
label var tecnica_ci "1=formacion terciaria tecnica"
*Se crea esta variable para ver si se trabaja con esta a partir del anio 2011
gen tipoest=.
replace tipoest=1 if e198<2 & e210_1<2 & e210_2<2 & e210_3<2 & e213<2 & e216<2 & e219<2 & e222<2 & e225<2  & e49==1
replace tipoest=2 if (e198>1 | e198==0) & (e210_1>1 | e210_1==0) & (e210_2>1 | e210_2==0) & (e210_3>1 | e210_3==0) & (e213>1 | e213==0)  & (e216>1 | e216==0) & (e219>1 | e219==0) & (e222>1 | e222==0) & (e225>1 | e225==0)  & e49==1
replace tipoest=3 if (e198>=0 | e210_1>=0 | e210_2>=0 | e210_3>=0 | e213>=0 | e216>=0 | e219>=0 | e222>=0 | e225>=0)  & (e49==1) & (tipoest==.)
label define tipoest 1 "publica" 2"privada" 3 "mixta"
label value tipoest tipoest


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

gen piso_ch=.
replace piso_ch=0 if c4==5
replace piso_ch=1 if c4==1 | c4==2 | c4==3
replace piso_ch=2 if c4==4

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



/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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




compress


saveold "`base_out'",   replace


log close
