

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
local ANO "2003"
local ronda a 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
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
Fecha última modificación: 30 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/



/***** revision August 03,2005

removed condition (& edad_ci<18) froom the following two lines:

by idh_ch: egen byte nhijos_ch=sum((relacion_ci==3) & edad_ci<18)
by idh_ch: egen byte notropari_ch=sum((relacion_ci==4) & edad_ci>=18)

****** revision August 10,2005

1. remove decimals from variable aedu_ci by including the following condition:
gen aedu_ci=floor(aedu)

2. add the following comment:
*esta variable de educacion no tiene missings*

previous code :
gen aedu_ci=e11_2+e11_3+e11_4+e11_5+e11_6

******

****revision August 21, 2006

change code for education dummy variables because they were not coherent
with the years of education.
old code can be seen below in the education section.
*****

*** revision August 2007 (Victoria) ***
With the unification Sociometro/Equis we decided to add two new varibales: howner and floor.
This variables were already created for Atlas

gen howner=(viviprop_ch==1 | viviprop==2);
replace howner=. if viviprop_ch==.;
gen floor=(piso_ch==1);
replace floor=. if piso_ch==.;

Also, the orginal data was replaced with the new Mecovi versions

*****

***/


use `base_in', clear

/***************************************************************************************************************************
 							Harmonización 01-03
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=correlat
gen idp_ci=nper
gen factor_ch=pesoan
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=2003
gen mes_c=mes
gen relacion_ci=e3
replace relacion_ci=3 if e3==4 | e3==5
replace relacion_ci=4 if e3>=6 & e3<=9
replace relacion_ci=5 if e3==11
replace relacion_ci=6 if e3==10
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

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

destring dpto, replace
gen region_c=dpto
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

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(d5==1)
gen aguadist_ch=d6
replace aguadist_ch=0 if d6==3
gen aguamala_ch=(d5==3|d5==3) /*Cachimba=ojo de agua o arroyo*/	
gen aguamide_ch=.
gen luz_ch=.
gen luzmide_ch=.
gen combust_ch=(d9==1 | d9==2 | d9==3)
gen bano_ch=(d7!=3)
gen banoex_ch=.
gen des1_ch=.
replace des1_ch=0 if d7==3
replace des1_ch=1 if d8==1
replace des1_ch=2 if d8==2
replace des1_ch=3 if d8==3 
gen des2_ch=d8
replace des2_ch=0 if d8==3
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if d5 ==1 | d5 ==2
replace aguamejorada_ch = 0 if d5 ==3

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (d7 ==1 | d7 ==2) & (d8 ==1 | d8 ==2) // NO se puede identificar si el banho es privado o compartido
replace banomejorado_ch = 0 if  d7 ==3 | ((d7 ==1 | d7 ==2) & d8 ==3)

gen dorm_ch=d4
replace dorm_ch=. if d4==9
gen cuartos_ch=d3
replace cuartos_ch=. if d3==99
gen cocina_ch=.
gen refrig_ch=(d10_3==1)
gen freez_ch=.
gen auto_ch=(d10_12==1)
gen telef_ch=(d10_13==1)
gen compu_ch=(d10_10==1)
gen internet_ch=(d10_11==1)
gen cel_ch=.
gen vivi1_ch=.
gen vivi2_ch=(c1==1)
gen viviprop_ch=0 if d2==3
replace viviprop_ch=1 if d2==1
replace viviprop_ch=2 if d2==2
replace viviprop_ch=3 if d2==4 | d2==5
gen vivialq_ch=d13 if viviprop_ch==0
gen vivialqimp_ch=d11
gen vivitit_ch=.



/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
gen factor_ci=pesoan
gen sexo_ci=e1
gen edad_ci=e2
replace edad_ci=. if e2==99
gen civil_ci=1 if e4==6
replace civil_ci=2 if e4==1 | e4==2
replace civil_ci=3 if e4==3 | e4==4
replace civil_ci=4 if e4==5
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
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

	
/******************************/
/*VARIABLES DE DEMANDA LABORAL*/
/******************************/
****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if pobpcoac==2
replace condocup_ci=2 if pobpcoac>=3 & pobpcoac<=5
replace condocup_ci=3 if pobpcoac>=6 & pobpcoac<=11
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen milit_polic =(e8_2==1) 
gen bps         =(e6==1) 
gen iamc        =(e5==1) 

gen afiliado_ci=(milit_polic==1 | bps==1 | iamc==1)
replace afiliado_ci=. if milit_polic==. & bps==. & iamc==.
label var afiliado_ci "Afiliado a la Seguridad Social"
drop milit_polic bps iamc
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
replace cotizando_ci=1 if (f10_2==1 | f16_2==1) & cotizando_ci==0
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (f10_2==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (f16_2==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

****************
*instpen_ci*****
****************
gen instpen_ci= .
label var instpen_ci "Institucion a la cual esta afiliado variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
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
gen tamemp_ci=f8
replace tamemp_ci=. if f8==0
label define tamemp_ci 1"una persona" 2"2-4 personas" 3"5-9 personas" 4 "10-49 personas" 5"50 o más" 6"10-19 personas" 7"20-49 personas"
label value tamemp_ci tamemp_ci
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
*************
*ypen_ci*
*************
gen yjub=g3_1 
gen ypen=g3_2

egen ypen_ci=rsum(yjub ypen)
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
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*************
* cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if f32==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********

* generar variable auxiliar de region para desagregar por Montevideo, interior urbano y rural
gen aux_reg = .
replace aux_reg = 1 if dpto==1
replace aux_reg = 2 if dpto!=1

gen lp_ci =.
replace lp_ci = 3150.39670281125 if mes==1 & aux_reg==1
replace lp_ci = 3207.02600434292 if mes==2 & aux_reg==1
replace lp_ci = 3237.76241799708 if mes==3 & aux_reg==1
replace lp_ci = 3267.28196187246 if mes==4 & aux_reg==1
replace lp_ci = 3285.70620778351 if mes==5 & aux_reg==1
replace lp_ci = 3287.82009667134 if mes==6 & aux_reg==1
replace lp_ci = 3311.46074850701 if mes==7 & aux_reg==1
replace lp_ci = 3331.51348069318 if mes==8 & aux_reg==1
replace lp_ci = 3358.96986660183 if mes==9 & aux_reg==1
replace lp_ci = 3358.79668089437 if mes==10 & aux_reg==1
replace lp_ci = 3368.88506494797 if mes==11 & aux_reg==1
replace lp_ci = 3386.85893214916 if mes==12 & aux_reg==1

replace lp_ci = 1860.08262784765 if mes==1 & aux_reg==2
replace lp_ci = 1892.40706402164 if mes==2 & aux_reg==2
replace lp_ci = 1911.42073415419 if mes==3 & aux_reg==2
replace lp_ci = 1929.50645995952 if mes==4 & aux_reg==2
replace lp_ci = 1940.69633445149 if mes==5 & aux_reg==2
replace lp_ci = 1942.52490028156 if mes==6 & aux_reg==2
replace lp_ci = 1955.25476909722 if mes==7 & aux_reg==2
replace lp_ci = 1966.51409215701 if mes==8 & aux_reg==2
replace lp_ci = 1983.17683764536 if mes==9 & aux_reg==2
replace lp_ci = 1984.79323620038 if mes==10 & aux_reg==2
replace lp_ci = 1991.0343409661  if mes==11 & aux_reg==2
replace lp_ci = 2002.50409528177 if mes==12 & aux_reg==2

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 857.85138113493 if mes==1 & aux_reg==1
replace lpe_ci = 865.836999308277 if mes==2 & aux_reg==1
replace lpe_ci = 888.717044296915 if mes==3 & aux_reg==1
replace lpe_ci = 895.083192126719 if mes==4 & aux_reg==1
replace lpe_ci = 893.945677707914 if mes==5 & aux_reg==1
replace lpe_ci = 909.652991548556 if mes==6 & aux_reg==1
replace lpe_ci = 906.720956794078 if mes==7 & aux_reg==1
replace lpe_ci = 933.785975384313 if mes==8 & aux_reg==1
replace lpe_ci = 942.238933093702 if mes==9 & aux_reg==1
replace lpe_ci = 958.009421234325 if mes==10 & aux_reg==1
replace lpe_ci = 947.966260768615 if mes==11 & aux_reg==1
replace lpe_ci = 953.149674781083 if mes==12 & aux_reg==1

replace lpe_ci = 802.80208838934 if mes==1 & aux_reg==2
replace lpe_ci = 810.162433230963 if mes==2 & aux_reg==2
replace lpe_ci = 832.061128987865 if mes==3 & aux_reg==2
replace lpe_ci = 837.912811114975 if mes==4 & aux_reg==2
replace lpe_ci = 836.471847507827 if mes==5 & aux_reg==2
replace lpe_ci = 851.514541685085 if mes==6 & aux_reg==2
replace lpe_ci = 848.632804624272 if mes==7 & aux_reg==2
replace lpe_ci = 875.200268147348 if mes==8 & aux_reg==2
replace lpe_ci = 883.623788990882 if mes==9 & aux_reg==2
replace lpe_ci = 899.227434296578 if mes==10 & aux_reg==2
replace lpe_ci = 889.225566135961 if mes==11 & aux_reg==2
replace lpe_ci = 894.198932740414 if mes==12 & aux_reg==2

label var lpe_ci "Linea de indigencia oficial del pais"

drop aux_reg


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci = .
replace salmm_ci = 1145   if mes_c==1 | mes_c==2  | mes_c==3  | mes_c==4
replace salmm_ci = 1170   if mes_c==5 | mes_c==6  | mes_c==7  | mes_c==8
replace salmm_ci = 1194   if mes_c==9 | mes_c==10 | mes_c==11 | mes_c==12
label var	salmm_ci	"Salario minimo legal 2003"

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

* Modificacion MGD 07/15/2014: correccion del grupo 9.
gen ocupa_ci=.
replace ocupa_ci=1 if (f5_2>=211 & f5_2<=348) & emp_ci==1
replace ocupa_ci=2 if (f5_2>=111 & f5_2<=131) & emp_ci==1
replace ocupa_ci=3 if (f5_2>=411 & f5_2<=422) & emp_ci==1
replace ocupa_ci=4 if ((f5_2>=521 & f5_2<=523) | f5_2==911) & emp_ci==1
replace ocupa_ci=5 if ((f5_2>=511 & f5_2<=516) | (f5_2>=912 & f5_2<=917)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f5_2>=611 & f5_2<=616) | f5_2==921) & emp_ci==1
replace ocupa_ci=7 if ((f5_2>=711 & f5_2<=834) | (f5_2>=931 & f5_2<=933)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f5_2==11 | f5_2==12)& emp_ci==1
replace ocupa_ci=9 if f5_2>=9999 & emp_ci==1

****************
*** rama_ci  ***
****************
gen rama_ci=.
replace rama_ci=1 if (f6_2>0 & f6_2<=9) & emp_ci==1
replace rama_ci=2 if (f6_2>=10 & f6_2<=14) & emp_ci==1
replace rama_ci=3 if (f6_2>=15 & f6_2<=37) & emp_ci==1
replace rama_ci=4 if (f6_2==40 | f6_2==41) & emp_ci==1
replace rama_ci=5 if f6_2==45 & emp_ci==1
replace rama_ci=6 if (f6_2>=50 & f6_2<=55) & emp_ci==1
replace rama_ci=7 if (f6_2>=60 & f6_2<=64) & emp_ci==1
replace rama_ci=8 if (f6_2>=65 & f6_2<=74) & emp_ci==1
replace rama_ci=9 if (f6_2>=75 & f6_2<=99) & emp_ci==1


gen horaspri_ci=f17_1
replace horaspri_ci=. if f17_1==99 | emp_ci==0
replace f17_2=. if f17_2==99
egen horastot_ci=rsum(horaspri_ci f17_2)
replace horastot_ci=. if f17_2==. & horaspri_ci==.
gen durades_ci=f29/4.3 if f29>0 & f29!=.
replace durades_ci=. if f29==99
gen antiguedad_ci=.

/*
gen desemp1_ci=((pobpcoac==3|pobpcoac==4|pobpcoac==5) & f23==1)
gen desemp2_ci=(desemp1_ci==1 | f23==2 & f24==2 | f24==3)
gen desemp3_ci=(desemp2_ci==1 | (f25==1 | f29>=4))
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
gen desalent_ci=.
/*
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & (f18>=1 &f18<=3))
replace subemp_ci=. if emp_ci==0
*/
* Modificacion MGD 06/23/2014: horas de la actividad principaln y considerando disponibilidad (subempleo visible).
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30) & ((f18>=1 &f18<=3) & f19==1)
* Mod. 2015/11 MLO
*gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & f18==4)
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30 & f18==4)
replace tiempoparc_ci=. if emp_ci==0
gen categopri_ci=1 if f7==4
replace categopri_ci=2 if f7==5 | f7==6 | f7==3
replace categopri_ci=3 if f7==1 | f7==2 
replace categopri_ci=4 if f7==7 
replace categopri_ci=. if emp_ci!=1
gen categosec_ci=1 if f13==4
replace categosec_ci=2 if f13==5 | f13==6 | f13==3
replace categosec_ci=3 if f13==1 | f13==2 
replace categosec_ci=4 if f13==7 
gen contrato_ci=.
gen segsoc_ci=.
gen nempleos_ci=1 if f4==1
replace nempleos_ci=2 if f4>1
/*gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & f8==1|f8==2
replace firmapeq_ci=0 if emp_ci==1 & f8>2*/
gen spublico_ci=(emp_ci==1 & f7==2)

*Genera la variable para empresas pequeñas
*drop tamemp_ci
gen tamemp_ci=1 if f8==1 | f8==2 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if f8==4 | f8==3
*Empresas grandes
replace tamemp_ci=3 if f8==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if f40_1==1 | f40_2==1
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if f40_4==1
*Quehaceres del Hogar
replace categoinac_ci=3 if f40_5==1
*Otra razon
replace categoinac_ci=4 if f40_3==1 
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


****************************************************************************************************************
*********************************************** INGRESOS *******************************************************
****************************************************************************************************************
*************
* ylmpri_ci *
*************
/*
SUELDO O JORNALES LÍQUIDOS 	g1_1_1
COMISIONES, INCENTIVOS, HORAS EXTRAS 	g1_1_2
VIÁTICOS NO SUJETOS A RENDICIÓN 	g1_1_3
PROPINAS 	g1_1_4
AGUINALDO 	g1_1_5
SALARIO VACACIONAL 	g1_1_6
*CUOTAS MUTUALES 	g1_1_9           

**Cuenta propia
RETIRO DE DINERO EL MES PASADO 	g2_1
	
DISTRIBUCIÓN DE UTILIDADES EN ULT. 12 MESES 	g2_3



*/

gen g2_3m = g2_3/12

egen ylmpri_ci=rsum(g1_1_1	g1_1_2	g1_1_3	g1_1_4	g1_1_5	g1_1_6 g2_1 g2_3m) if emp_ci==1, missing

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
RETRIBUCIONES EN ESPECIE 	g1_1_7
COMPLEMENTO PAGADO POR EMPLEADOR 	g1_1_8
MONTO MENSUAL DE RETIRO DE PRODUCTOS 	g2_2	(Autoconsumo)
*/
	egen ylnmpri_ci= rsum(g1_1_7 g1_1_8 g2_2) if emp_ci==1, missing
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

	***************
	***ylmsec_ci***
	***************
/*
SUELDO O JORNALES LÍQUIDOS 	g1_2_1	
COMISIONES, INCENTIVOS, HORAS EXTRAS 	g1_2_2	
VIÁTICOS NO SUJETOS A RENDICIÓN 	g1_2_3	
PROPINAS 	g1_2_4	
AGUINALDO 	g1_2_5	
SALARIO VACACIONAL 	g1_2_6	
*CUOTAS MUTUALES 	g1_2_9	
*/

	egen ylmsec_ci=rsum(g1_2_1	g1_2_2	g1_2_3	g1_2_4	g1_2_5	g1_2_6) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
	****************
	***ylnmsec_ci***
	****************
	/*
RETRIBUCIONES EN ESPECIE 	g1_2_7
COMPLEMENTO PAGADO POR EMPLEADOR 	g1_2_8
        
*/


egen ylnmsec_ci=rsum(g1_2_7 g1_2_8) if emp_ci==1, missing
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

	egen ylmotros_ci= rsum(g1_1_1	g1_1_2	g1_1_3	g1_1_4	g1_1_5	g1_1_6 g2_1 g2_3m g1_2_1	g1_2_2	g1_2_3	g1_2_4	g1_2_5	g1_2_6) if emp_ci==0, missing

	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral monetario otros trabajos para todos los años
	
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci= rsum(g1_1_7 g1_1_8 g2_2 g1_2_7 g1_2_8) if emp_ci==0, missing
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
G3. INGRESOS POR TRANSFERENCIAS	
JUBILACIONES 	g3_1
PENSIONES 	g3_2
SEGURO DE DESEMPLEO 	g3_3
COMPENSACIONES POR ACCIDENTE 	g3_4
BECAS, SUBSIDIOS O DONACIONES 	g3_5
CONTRIBUCIONES POR DIVORCIO 	g3_6
AYUDAS FAMILIARES U OTROS HOGARES 	g3_7
HOGAR CONSTITUIDO 	g3_8
G4. OTROS INGRESOS	
ÚLTIMO MES	
POR ALQUILER 	g4_1_1
ALGÚN OTRO INGRESO CORRIENTE 	g4_1_2
ÚLTIMOS DOCE MESES 2/	
POR ARRENDAMIENTO DE TERRENOS 	g4_2_1
INTERESES DE CTAS. BANC. 	g4_2_2
UTILIDADES, DIVIDENDOS 	g4_2_3
INDEMNIZACIÓN POR DESPIDO 	g4_2_4
OTRO INGRESO CORRIENTE 	g4_2_5
INGRESO EXTRAORDINARIOS (JUEGOS DE AZAR, ETC) 	g4_2_6
G5. INGRESOS DEL EXTERIOR	
JUBILACIONES 	g5_2
PENSIONES 	g5_3
BECAS, SUBSIDIOS O DONACIONES 	g5_4
CONTRIBUCIONES POR DIVORCIO 	g5_5
AYUDAS FAMILIARES U OTROS HOGARES 	g5_6
ARRENDAMIENTOS DE TIERRAS O TERRENOS 	g5_7
ALQUILER DE CASAS 	g5_8
INTERESES PROVENIENTES DE CUENTAS BANC. 	g5_9
INTERESES PROVENIENTES DE PRÉSTAMOS 	g5_10
UTILIDADES O DIVIDENDOS DE NEGOCIOS 	g5_11
              
*/

forvalues j=1(1)6{
gen g4_2_`j'm=g4_2_`j'/12 /*Estos estan definidos en base anual!*/
}

egen ynlm_ci=rsum(g3_1 g3_2 g3_3 g3_4 g3_5 g3_6 g3_7 g3_8 g4_1_1 g4_1_2 g4_2_1m g4_2_2m g4_2_3m g4_2_4m g4_2_5m g4_2_6m g5_2 g5_3 g5_4 g5_5 g5_6 g5_7 g5_8 g5_9 g5_10 g5_11), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci= g5_6
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
	gen autocons_ci= g2_2
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
gen g2_3mo=g2_3/12 /*Esta en base anual*/
egen ylmpri_ci=rsum(g1_1_1 g1_1_2 g1_1_3 g1_1_4 g1_1_5 g1_1_6) if (f7==1 | f7==2|f7==7) & emp_ci==1
replace ylmpri_ci=. if g1_1_1==. & g1_1_2==. & g1_1_3==. & g1_1_4==. & g1_1_5==0 & g1_1_6==. & (f7==1 | f7==2|f7==7)
*egen ylmpri_ci2=rsum(g2_1 g2_3mo) 
*replace ylmpri_ci2=. if g2_1==. & g2_3mo==.
replace ylmpri_ci=g2_3mo if (f7==3|f7==4|f7==5|f7==6) & emp_ci==1 
*drop ylmpri_ci2

egen ylmpri2_ci=rsum(ylmpri_ci g1_1_8 g1_1_9) if (f7==1 | f7==2|f7==7) & emp_ci==1
replace ylmpri2_ci=. if ylmpri_ci==. & g1_1_8==. & g1_1_9==. & (f7==1 | f7==2|f7==7)


egen ylnmpri_ci=rsum(g1_1_7 g2_2) if emp_ci==1
replace ylnmpri_ci=. if g1_1_7==. & g2_2==. 

gen ylmsec_ci=.
gen ylnmsec_ci=.
gen ylmotros_ci=.

egen ylmaux=rsum(g1_2_1 g1_2_2 g1_2_3 g1_2_4 g1_2_5 g1_2_6 g1_2_8 g1_2_9) if emp_ci==1
replace ylmaux=. if g1_2_1==. & g1_2_2==. & g1_2_3==. & g1_2_4==. & g1_2_5==. & g1_2_6==. & g1_2_8==. & g1_2_9==.

gen ylnmaux=g1_2_7
gen nrylmpri_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmaux) if emp_ci==1
replace ylm_ci=. if ylmpri_ci==. & ylmaux==.

egen ylm2_ci=rsum(ylmpri2_ci ylmaux) if emp_ci==1
replace ylm2_ci=. if ylmpri2_ci==. & ylmaux==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmaux) if emp_ci==1
replace ylnm_ci=. if ylnmpri_ci==. & ylnmaux==.

drop ylnmaux ylmaux

forvalues j=1(1)6{
gen g4_2_`j'mo=g4_2_`j'/12 /*Estos estan definidos en base anual!*/
}

*** Modificacion a ynlm_ci realizada el 08/30/05
*** se estaban incluyendo g3_9_1 u g3_9_2 que son variables categoricas, y ademas faltaba incluir ingresos del exterior (g5_2 a g5_11)
egen ynlm_ci=rsum(g3_1 g3_2 g3_3 g3_4 g3_5 g3_6 g3_7 g3_8 g4_1_1 g4_1_2 g4_2_1mo g4_2_2mo g4_2_3mo g4_2_4mo g4_2_5mo g4_2_6mo g5_2 g5_3 g5_4 g5_5 g5_6 g5_7 g5_8 g5_9 g5_10 g5_11)
replace ynlm_ci=. if g3_1==. & g3_2==. & g3_3==. & g3_4==. & g3_5==. & g3_6==. & g3_7==. & g3_8==. & g4_1_1==. & g4_1_2==. & g4_2_1mo==. & g4_2_2mo==. & g4_2_3mo==. & g4_2_4mo==. & g4_2_5mo==. & g4_2_6mo==. & g5_2==. & g5_3==. & g5_4==. & g5_5==. & g5_6==. & g5_7==. & g5_8==. & g5_9==. & g5_10==. & g5_11==. 
****

gen ynlnm_ci=.

sort idh_ch
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if relacion_ci!=6
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri2_ci=ylmpri2_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0
gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.
egen remesas_ci=rsum(g5_2 g5_3 g5_4 g5_5 g5_6 g5_7 g5_8 g5_9 g5_10 g5_11)
replace remesas_ci=. if g5_2==. & g5_3==. & g5_4==. & g5_5==. & g5_6==. & g5_7==. & g5_8==. & g5_9==. & g5_10==. & g5_11==.

sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6
*/
/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/


replace e11_2 = 0 if e11_2==0.5
replace e11_3 = 0 if e11_3==0.5
replace e11_4 = 0 if e11_4==0.5
replace e11_5 = 0 if e11_5==0.5
replace e11_6 = 0 if e11_6==0.5

/*
gen utusec=e11_3+e11_4
generate aedu_ci = e11_2+ min(utusec,6)+ e11_5+ e11_6 
replace aedu_ci =12+ e11_4 + e11_5 + e11_6 if (e11_2==6 & e11_3==6)
*/

gen prim=(e11_2>0 & e11_2<=6)
gen secu=(e11_3>0 & e11_3<=6)
gen tecn=(e11_4>0 & e11_4<=6)
gen mag =(e11_5>0 & e11_5<9)
gen univ=(e11_6>0 & e11_6<9)

gen aedu_ci=.
replace aedu_ci= e11_2        if prim==1 & e11_2<=6 
replace aedu_ci= e11_3 + 6    if secu==1 & e11_3<=6
*replace aedu_ci= e11_4 + 12   if tecn==1 & e11_4_1==1 & e11_4<=6
replace aedu_ci= e11_5 + 12   if mag==1  & e11_5<9
replace aedu_ci= e11_6 + 12   if univ==1 & (e11_6>e11_5) & (e11_6<9) /* se toma los años de mag o univ no la suma de los dos */
replace aedu_ci=0             if e10==2 & (edad>=5 & edad!=.)
drop prim secu tecn mag univ

/*
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(e11_2>0 & e11_2<6 )
gen edupc_ci=(e11_2==6 & e11_3==0 & e11_4==0 & e11_5==0 & e11_6==0)

gen edusi_ci=(((e11_3>0 & e11_3<6) | (utusec<6))  & e11_5==0 & e11_6==0)

gen edusc_ci=(((e11_3==6)| (utusec==6)) & e11_5==0 & e11_6==0)

gen eduui_ci=((e11_5>0 & e11_5<5)  | (e11_6>0 & e11_6<5) | (utusec>6 & utusec<11))

gen eduuc_ci=((e11_5>=5 | e11_6>=5) | (utusec>=11))
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

gen edupre_ci=(e11_1>0)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"




gen eduac_ci=.
gen asiste_ci=(e9==1)
gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci =.

gen repite_ci=.
gen repiteult_ci=.
gen edupub_ci=(e14==1)
label var  aedu_ci "Anios de Educacion"

****************
***tecnica_ci **
****************
gen tecnica_ci=.
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


rename f6_2 codindustria 
rename f5_2 codocupa 
compress



saveold "`base_out'", replace


log close

	
