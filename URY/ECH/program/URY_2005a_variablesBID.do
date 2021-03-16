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
local ANO "2005"
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
Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com - 30 de Octubre de 2013
Versión 2021: Cesar Lins (SCL/GDI) - Marzo 2021


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


/*

*** revision August 2007 (Victoria) ***
With the unification Sociometro/Equis we decided to add two new varibales: howner and floor.
This variables were already created for Atlas

gen howner=(viviprop_ch==1 | viviprop==2);
replace howner=. if viviprop_ch==.;
gen floor=(piso_ch==1);
replace floor=. if piso_ch==.;

Also, the orginal data was replaced with the new Mecovi versions
****/


/*(Melisa,mmorales, May 2009: 'asiste_ci' should be changed
gen asiste_ci=(e9==1)
replace asiste_ci=. if e9==0
0: indicates missing values*/

**************************************************************



use `base_in', clear


/***************************************************************************************************************************
 							Harmonización 04-05
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=correlat
gen idp_ci=nper
gen factor_ch=pesoan
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=2005

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
* Nota Marcela G. Rubio - Abril 2014
* variable vivialqimp_ch había sido generado como missing pero pregunta d11 todavia existe en el cuestionario de este año
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
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		

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
gen milit_polic =(e5_2==1) 
gen bps         =(e5_4==1) 
gen iamc        =(e5_5==1 & e6_1_1==3) 

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
replace lp_ci = 3642.17964229761 if mes==1 & aux_reg==1
replace lp_ci = 3645.51166011291 if mes==2 & aux_reg==1
replace lp_ci = 3659.81498296885 if mes==3 & aux_reg==1
replace lp_ci = 3717.90487409232 if mes==4 & aux_reg==1
replace lp_ci = 3719.42052145417 if mes==5 & aux_reg==1
replace lp_ci = 3731.83217233522 if mes==6 & aux_reg==1
replace lp_ci = 3770.37875457006 if mes==7 & aux_reg==1
replace lp_ci = 3784.83694557747 if mes==8 & aux_reg==1
replace lp_ci = 3811.97259259164 if mes==9 & aux_reg==1
replace lp_ci = 3837.87186768167 if mes==10 & aux_reg==1
replace lp_ci = 3837.51446529372 if mes==11 & aux_reg==1
replace lp_ci = 3842.27047052844 if mes==12 & aux_reg==1

replace lp_ci = 2154.29532126526 if mes==1 & aux_reg==2
replace lp_ci = 2154.29905585989 if mes==2 & aux_reg==2
replace lp_ci = 2162.87427304197 if mes==3 & aux_reg==2
replace lp_ci = 2198.23155484458 if mes==4 & aux_reg==2
replace lp_ci = 2199.68481334649 if mes==5 & aux_reg==2
replace lp_ci = 2206.70203040277 if mes==6 & aux_reg==2
replace lp_ci = 2230.17912244474 if mes==7 & aux_reg==2
replace lp_ci = 2237.62333121985 if mes==8 & aux_reg==2
replace lp_ci = 2253.71661917829 if mes==9 & aux_reg==2
replace lp_ci = 2269.0197276348 if mes==10 & aux_reg==2
replace lp_ci = 2269.26998645154 if mes==11 & aux_reg==2
replace lp_ci = 2272.49423067224 if mes==12 & aux_reg==2

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 1041.29154195195 if mes==1 & aux_reg==1
replace lpe_ci = 1041.67526722743 if mes==2 & aux_reg==1
replace lpe_ci = 1046.35444874899 if mes==3 & aux_reg==1
replace lpe_ci = 1055.31108802069 if mes==4 & aux_reg==1
replace lpe_ci = 1055.952067744 if mes==5 & aux_reg==1
replace lpe_ci = 1059.59109185646 if mes==6 & aux_reg==1
replace lpe_ci = 1071.22041871355 if mes==7 & aux_reg==1
replace lpe_ci = 1069.02654592148 if mes==8 & aux_reg==1
replace lpe_ci = 1081.78359168604 if mes==9 & aux_reg==1
replace lpe_ci = 1069.42419572874 if mes==10 & aux_reg==1
replace lpe_ci = 1060.41826365624 if mes==11 & aux_reg==1
replace lpe_ci = 1065.00835643902 if mes==12 & aux_reg==1

replace lpe_ci = 978.86018806999 if mes==1 & aux_reg==2
replace lpe_ci = 979.331073172796 if mes==2 & aux_reg==2
replace lpe_ci = 983.962309969901 if mes==3 & aux_reg==2
replace lpe_ci = 992.919483835342 if mes==4 & aux_reg==2
replace lpe_ci = 993.436650926241 if mes==5 & aux_reg==2
replace lpe_ci = 996.988317194609 if mes==6 & aux_reg==2
replace lpe_ci = 1008.37213222479 if mes==7 & aux_reg==2
replace lpe_ci = 1005.89963729258 if mes==8 & aux_reg==2
replace lpe_ci = 1018.39160285786 if mes==9 & aux_reg==2
replace lpe_ci = 1005.98050895947 if mes==10 & aux_reg==2
replace lpe_ci = 996.867137515337 if mes==11 & aux_reg==2
replace lpe_ci = 1001.50119484137 if mes==12 & aux_reg==2

label var lpe_ci "Linea de indigencia oficial del pais"

drop aux_reg

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci = .
replace salmm_ci = 2050   if mes_c==1 | mes_c==2  | mes_c==3  | mes_c==4 | mes_c==5 | mes_c==6 
replace salmm_ci = 2500   if mes_c==7 | mes_c==8  | mes_c==9 | mes_c==10 | mes_c==11 | mes_c==12
label var	salmm_ci	"Salario minimo legal 2005"
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
replace ocupa_ci=1 if ((f5_2>=2110 & f5_2<=3480) | (f5_2>=211 & f5_2<=348)) & emp_ci==1
replace ocupa_ci=2 if ((f5_2>=1110 & f5_2<=1310) | (f5_2>=111 & f5_2<=131)) & emp_ci==1
replace ocupa_ci=3 if ((f5_2>=4110 & f5_2<=4223) | (f5_2>=411 & f5_2<=422)) & emp_ci==1
replace ocupa_ci=4 if ((f5_2>=5210 & f5_2<=5230) | (f5_2>=521 & f5_2<=523) | (f5_2>=9110 & f5_2<=9113) | f5_2==911) & emp_ci==1
replace ocupa_ci=5 if ((f5_2>=5111 & f5_2<=5169) | (f5_2>=511 & f5_2<=516) | (f5_2>=9120 & f5_2<=9172) | (f5_2>=912 & f5_2<=917)) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if ((f5_2>=6110 & f5_2<=6210) | (f5_2>=611 & f5_2<=616) | (f5_2>=9211 & f5_2<=9213) | f5_2==921) & emp_ci==1
replace ocupa_ci=7 if ((f5_2>=7110 & f5_2<=8340) | (f5_2>=711 & f5_2<=834) | (f5_2>=9311 & f5_2<=9333) | (f5_2>=931 & f5_2<=933)) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f5_2==11 | f5_2==12)& emp_ci==1
replace ocupa_ci=9 if f5_2>=9999 & emp_ci==1

****************
*** rama_ci  ***
****************
gen rama_ci=.
replace rama_ci=1 if ((f6_2>=100 & f6_2<=500) | (f6_2>=1 & f6_2<=5)) & emp_ci==1
replace rama_ci=2 if ((f6_2>=1000 & f6_2<=1429) | (f6_2>=10 & f6_2<=14)) & emp_ci==1
replace rama_ci=3 if ((f6_2>=1500 & f6_2<=3720) | (f6_2>=15 & f6_2<=37)) & emp_ci==1
replace rama_ci=4 if ((f6_2>=4000 & f6_2<=4100) | (f6_2>=40 & f6_2<=41)) & emp_ci==1
replace rama_ci=5 if ((f6_2>=4500 & f6_2<=4550) | (f6_2==45))& emp_ci==1
replace rama_ci=6 if ((f6_2>=5010 & f6_2<=5520) | (f6_2>=50 & f6_2<=55)) & emp_ci==1
replace rama_ci=7 if ((f6_2>=6000 & f6_2<=6420) | (f6_2>=60 & f6_2<=64)) & emp_ci==1
replace rama_ci=8 if ((f6_2>=6500 & f6_2<=7499) | (f6_2>=65 & f6_2<=74)) & emp_ci==1
replace rama_ci=9 if ((f6_2>=7500 & f6_2<=9900) | (f6_2>=75 & f6_2<=99))& emp_ci==1

gen horaspri_ci=f17_1
replace horaspri_ci=. if f17_1==99 | emp_ci==0
replace f17_2=. if f17_2==99
egen horastot_ci=rsum(horaspri_ci f17_2)
replace horastot_ci=. if f17_2==. & horaspri_ci==.
gen durades_ci=f29/4.3 if f29>0
replace durades_ci=. if f29==99


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
****************************************INGRESOS**************************************************************** 
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

	egen ylmotros_ci= rsum(g1_1_1 g1_1_2 g1_1_3	g1_1_4	g1_1_5	g1_1_6 g2_1 g2_3m g1_2_1	g1_2_2	g1_2_3	g1_2_4	g1_2_5	g1_2_6) if emp_ci==0, missing
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

** Aug, 2015: Se efectuan cambios en sintaxis de variable aedu_ci en base a revisión por Iván Bornacelly SCL/EDU **
** Ajustado Jul,2017 por Iván Bornacelly SCL/EDU

replace e11_1=0 if e11_1==0.5
replace e11_2=0 if e11_2==0.5
replace e11_3=0 if e11_3==0.5
replace e11_4=0 if e11_4==0.5
replace e11_5=0 if e11_5==0.5
replace e11_6=0 if e11_6==0.5

gen aedu_ci=.
replace aedu_ci=e11_2 + e11_3
replace aedu_ci=12 + e11_4 if e11_4>e11_5 & e11_4>e11_6 
replace aedu_ci=12 + e11_4 if e11_4==e11_5 & e11_4!=0
replace aedu_ci=12 + e11_4 if e11_4==e11_6 & e11_4!=0
replace aedu_ci=12 + e11_5 if e11_5>e11_4 & e11_5>e11_6
replace aedu_ci=12 + e11_5 if e11_5==e11_6 & e11_5!=0
replace aedu_ci=12 + e11_6 if e11_6>=e11_4 & e11_6>e11_5
replace aedu_ci=. if edad<=4

/*
replace e11_2 = 0 if e11_2==0.5
replace e11_3 = 0 if e11_3==0.5
replace e11_4 = 0 if e11_4==0.5
replace e11_5 = 0 if e11_5==0.5
replace e11_6 = 0 if e11_6==0.5

/* Antigua
gen utusec=e11_3+e11_4
generate aedu_ci = e11_2+ min(utusec,6)+ e11_5+ e11_6 
replace aedu_ci =12+ e11_4 + e11_5 + e11_6 if (e11_2==6 & e11_3==6)
*/


** Aug, 2015: Se efectuan cambios en sintaxis de variable aedu_ci en base a revisión por Iván Bornacelly SCL/EDU **


gen prim=(e11_2>0 & e11_2<=6)
gen secu=(e11_3>0 & e11_3<=6)
gen tecn=(e11_4>0 & e11_4<=6)
gen mag =(e11_5>0 & e11_5<9)
gen univ=(e11_6>0 & e11_6<9)

/*
gen aedu_ci=.
replace aedu_ci= e11_2        if prim==1 & e11_2<=6 
replace aedu_ci= e11_3 + 6    if secu==1 & e11_3<=6
*replace aedu_ci= e11_4 + 12   if tecn==1 & e11_4_1==1 & e11_4<=6
replace aedu_ci= e11_5 + 12   if mag==1  & e11_5<9
replace aedu_ci= e11_6 + 12   if univ==1 & (e11_6>e11_5) & (e11_6<9) /* se toma los años de mag o univ no la suma de los dos */
replace aedu_ci=0             if e10==2 & (edad>=5 & edad!=.)
drop prim secu tecn mag univ
*/


gen aedu_ci=.
replace aedu_ci=e11_2+e11_3+e11_5+e11_6
replace aedu_ci=0 if e11_2==0 & e11_3==0 & e11_5==0 & e11_6==0
replace aedu_ci=. if e11_4>=1 & e11_4<=6


/*
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(e11_2>0 & e11_2<6 )
gen edupc_ci=(e11_2==6 & e11_3==0 & e11_4==0 & e11_5==0 & e11_6==0)

gen edusi_ci=(((e11_3>0 & e11_3<6) | (utusec<6))  & e11_5==0 & e11_6==0)

gen edusc_ci=(((e11_3==6)| (utusec==6)) & e11_5==0 & e11_6==0)

gen eduui_ci=((e11_5>0 & e11_5<5)  | (e11_6>0 & e11_6<5) | (utusec>6 & utusec<11))

gen eduuc_ci=((e11_5>=5 | e11_6>=5) | (utusec>=11))
*/
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

gen edupre_ci=(e11_1>0)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"

g antiguedad_ci=.


gen eduac_ci=.
gen asiste_ci=(e9==1)

****************
***asispre_ci***
****************
* Agregada por Iván Bornacelly - 01/23/2017
	g asispre_ci=.	

/*(Melisa,mmorales, May 2009: 'asiste_ci' should be changed
gen asiste_ci=(e9==1)
replace asiste_ci=. if e9==0
0: indicates missing values
*/

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
gen tecnica_ci=((e11_4>=1 & e11_4<=9) & e11_4_1==1)
label var tecnica_ci "=1 formacion terciaria tecnica"

*******************
*** SALUD  ***
*******************

*******************
*** cobsalud_ci ***
*******************

gen cobsalud_ci=1 if e5_1==1 | e5_2==1 | e5_3==1 | e5_4==1 | e5_5==1 | e5_6_1==1 
recode cobsalud_ci (.=0)

label var cobsalud_ci "Tiene cobertura de salud"
label define cobsalud_ci 0 "No" 1 "Si" 
label value cobsalud_ci cobsalud_ci

************************
*** tipocobsalud_ci  ***
************************
gen tipocobsalud_ci=1 if e5_1==1
replace tipocobsalud_ci=2 if e5_2==1
replace tipocobsalud_ci=3 if e5_3==1
replace tipocobsalud_ci=4 if e5_4==1
replace tipocobsalud_ci=5 if e5_5==1
replace tipocobsalud_ci=6 if e5_6_1==1

recode tipocobsalud_ci (.=0)

label var tipocobsalud_ci "Tipo cobertura de salud"
lab def tipocobsalud_ci 1 "MSP" 2"pol/mil" 3"Municipal" 4 "BPS" 5"IAMC" 6"otro" 0"Sin Cobertura"
lab val tipocobsalud_ci tipocobsalud_ci

*********************
*** distancia_ci  ***
*********************
gen distancia_ci=.

label var distancia_ci "Dificultad de acceso a salud por distancia"
lab def distancia_ci 0 "No" 1 "Si"
lab val distancia_ci distancia_ci

*****************
*** costo_ci  ***
*****************
gen costo_ci=.
label var costo_ci "Dificultad de acceso a salud por costo"
lab def costo_ci 0 "No" 1 "Si"
lab val costo_ci costo_ci

********************
*** atencion_ci  ***
********************
gen atencion_ci=.

label var atencion_ci "Dificultad de acceso a salud por problemas de atencion"
lab def atencion_ci 0 "No" 1 "Si"
lab val atencion_ci atencion_ci


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

saveold "`base_out'",   replace


log close



********************
*** URUGUAY 2005 ***
********************
/*


e3 - Parentco 
 1. Jefe
 2. Esposo o compañero
 3. Hijo de ambos
 4. Hijo sólo del jefe
 5. Hijo sólo del cónyuge
 6. Yerno o nuera
 7. Nieto
 8. Padres o Suegros
 9. Otro pariente
 10. Servicio doméstico o familiar de éste
 11. No pariente


 rename e1 sexo
 rename e2 edad
 rename e3 parentco
 destring sexo, replace
 
 rename pesoan factor

 gen	 incl=1 if (parentco>=1 & parentco<=11)
 replace incl=0 if (parentco==10)

** AREA

* Only Urban Areas

* Gender classification of the population refering to the head of the household.

 sort correlativ  nper 

 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(correlativ)


 sort correlativ  nper 

** Years of education. 
/*
E9
9. ¿Asiste actualmente a un establecimiento de enseñanza
preescolar, primaria, secundaria, superior o técnica?
 1. Si (marque y pase a preg. 11)
 2. No 

E10
10 Aunque no asiste actualmente, ¿asistió alguna vez a
un establecimiento de enseñanza preescolar, primaria,
secundaria, superior o técnica?
 1. Si 
 2. No (marque y pase a Capítulo F) 

E11
11 Indique la cantidad de años aprobados en cada uno
de los siguientes niveles de educación:

 1. Preescolar o jardín					E11_1	Años aprobados en preescolar o jardín 	
 2. Primaria 						E11_2	Años  aprobados en primaria		
 3. Secundaria 						E11_3	Años  aprobados en secundaria		
 4. Enseñanza Técnica (UTU, similar) 		(A)	E11_4	Años  aprobados en enseñanza técnica	
	Para ese curso se exigía:		(A)	E11_4_1						
	1. Enseñanza Secundaria completa 	(A)		1	Enseñanza Secundaria Completa
	2. Enseñanza Secundaria 1° Ciclo 	(A)		2	Enseñanza Secundaria 1er. Ciclo
	3. Enseñanza Primaria completa 		(A)		3	Enseñanza Primaria Completa
	4. Ninguna 				(A)		4	Ninguna
 5. Magisterio o Profesorado			(A)	E11_5	Años  aprobados en magisterio o profesorado 
 6. Universidad o similar 			(A)	E11_6	Años  aprobados en Universidad o similar
							 (El valor 0,5 en la variable indica asistencia al primer 
							  año del nivel sin haber aprobado ningún año.)

12 ¿Cuál es el curso o carrera? (sólo para quienes respondieron en la parte A):

E13
13 ¿Finalizó el nivel más alto indicado? 
 1. Si
 2. No 
*/

***************************************************************************************************
* YEARS OF EDUCATION DATA FOR THE SERIES 2001-2005 IS NOT STRICTLY COMPARABLE WITH PREVIOUS YEARS *
* DUE TO DIFFERENCES IN THE QUESTIONNAIRES (UTU)						  *
***************************************************************************************************

 tab e11_2 e11_3
 tab e11_3 if e11_2==6

 gen e11_2_o= e11_2
 gen e11_3_o= e11_3
 gen e11_4_o= e11_4
 gen e11_5_o= e11_5
 gen e11_6_o= e11_6

 gen correc_a=1 if (e11_2>=0.5 & e11_2<=6) & ((e11_3>=0.5 & e11_3<=6) | (e11_4>=0.5 & e11_4<=6) | (e11_5>=0.5 & e11_5<=5) | (e11_6>=0.5 & e11_6<=8))
 replace e11_2=. if correc_a==1

 gen correc_b=1 if (e11_3>=0.5 & e11_3<=6) & ((e11_4>=0.5 & e11_4<=6) | (e11_5>=0.5 & e11_5<=5) | (e11_6>=0.5 & e11_6<=8))
 replace e11_3=. if correc_b==1

 gen correc_c=1 if (e11_4>=0.5 & e11_4<=6) & ((e11_5>=0.5 & e11_5<=5) | (e11_6>=0.5 & e11_6<=8))
 replace e11_4=. if correc_c==1

 gen correc_d=1 if (e11_5>=0.5 & e11_5<=5) & ((e11_6>=0.5 & e11_6<=8))
 replace e11_5=. if correc_d==1

 gen	 anoest=.
 replace anoest=0  if e10==2 | e11_2==.5 | (e11_1>=0.5 & e11_1<=4) 
 replace anoest=1  if e11_2==1
 replace anoest=2  if e11_2==2
 replace anoest=3  if e11_2==3
 replace anoest=4  if e11_2==4
 replace anoest=5  if e11_2==5
 replace anoest=6  if e11_2==6 | e11_3==0.5 
 replace anoest=7  if e11_3==1 
 replace anoest=8  if e11_3==2 
 replace anoest=9  if e11_3==3 | e11_4==0.5 /* UTU */
 replace anoest=10 if e11_3==4 | e11_4==1  
 replace anoest=11 if e11_3==5 | e11_4==2
 replace anoest=12 if e11_3==6 | e11_4==3 | e11_5==0.5 | e11_6==0.5  
 replace anoest=13 if e11_6==1 | e11_4==4 | e11_5==1 
 replace anoest=14 if e11_6==2 | e11_4==5 | e11_5==2 
 replace anoest=15 if e11_6==3 | e11_4==6 | e11_5==3 
 replace anoest=16 if e11_6==4 | e11_5==4 
 replace anoest=17 if e11_6==5 | e11_5==5
 replace anoest=18 if e11_6==6 | e11_6==8
 replace anoest=19 if e11_6==7 
 replace anoest=99 if e11_6==9 | e11_2==9 | e11_3==9 | e11_4==9 | e11_5==9

 tab anoest [w=factor]


** Economic Active Population 
/*
POBPCOAC
VARIABLE RESUMEN DE CONDICION DE ACTIVIDAD ECONÓMICA		
 1.  Menor de 14 años
 2.  Ocupados
 3.  Desocupados B.T.1.V
 4.  Desocupados propiamente dichos
 5.  Desocupados en seguro de paro
 6.  Inactivo, realiza los quehaceres del hogar
 7.  Inactivo, estudiante
 8.  Inactivo, rentista
 9.  Inactivo, pensionista
 10. Inactivo, jubilado
 11. Inactivo, otro
*/

 gen	 peaa=0 if (pobpcoac==1)
 replace peaa=1 if (pobpcoac==2)
 replace peaa=2 if (pobpcoac>=3 & pobpcoac<=5)
 replace peaa=3 if (pobpcoac>=6 & pobpcoac<=11)

 gen	 tasadeso=0 if peaa==1
 replace tasadeso=1 if peaa==2



************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*
E9
9. ¿Asiste actualmente a un establecimiento de enseñanza
preescolar, primaria, secundaria, superior o técnica?
 1. Si (marque y pase a preg. 11)
 2. No 

E11
11 Indique la cantidad de años aprobados en cada uno
de los siguientes niveles de educación:

 1. Preescolar o jardín					E11_1	Años aprobados en preescolar o jardín 	
 2. Primaria 						E11_2	Años  aprobados en primaria		
 3. Secundaria 						E11_3	Años  aprobados en secundaria		
 4. Enseñanza Técnica (UTU, similar) 		(A)	E11_4	Años  aprobados en enseñanza técnica	
	Para ese curso se exigía:		(A)	E11_4_1						
	1. Enseñanza Secundaria completa 	(A)		1 Enseñanza Secundaria Completa
	2. Enseñanza Secundaria 1° Ciclo 	(A)		2 Enseñanza Secundaria 1er. Ciclo
	3. Enseñanza Primaria completa 		(A)		3 Enseñanza Primaria Completa
	4. Ninguna 				(A)		4 Ninguna
 5. Magisterio o Profesorado			(A)	E11_5	Años  aprobados en magisterio o profesorado 
 6. Universidad o similar 			(A)	E11_6	Años  aprobados en Universidad o similar
							 (El valor 0,5 en la variable indica asistencia al primer 
							  año del nivel sin haber aprobado ningún año.)

E13
13 ¿Finalizó el nivel más alto indicado? 
 1. Si
 2. No 
 
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if (edad>=6 & edad<=11) & (e9==1 | e9==2)
 replace NERP=1 if (edad>=6 & edad<=11) & (e11_2>=0.5 & e11_2<=5) & e9==1

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=12 & edad<=17) & (e9==1 | e9==2)
 replace NERS=1 if (edad>=12 & edad<=17) & (e11_3>=0.5 & e11_3<=5) & e9==1
	
* Upper secondary
* Bachillerato Diversificado

 gen	 NERS2=0 if (edad>=15 & edad<=17) & (e9==1 | e9==2)
 replace NERS2=1 if (edad>=15 & edad<=17) & (e11_3>=3 & e11_3<=5) & e9==1
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen	 ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  e9==1 & (e11_2>=0.5 & e11_2<=5)
 gen sec=1  if  e9==1 & (e11_3>=0.5 & e11_3<=5)
 gen ter=1  if  e9==1 & ((e11_6>=0.5 & e11_6<=4) | (e11_5>=0.5 & e13==2))

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
 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.
 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
/*
F7. En el trabajo que le proporciona los mayores	F6_2 ¿Qué produce o a qué 
ingresos, es: ...					 se dedica principalmente el 
 1. Empleado u obrero privado				 establecimiento u oficina donde 
 2. Empleado u obrero público				 desarrolla el trabajo de   
 3. Miembro de cooperativa de producción		 mayores ingresos?
 4. Patrón con personal a su cargo			
 5. Trabajador por cuenta propia sin local		F5_2 Ocupación
 6. Trabajador por cuenta propia con local		913. Personal doméstico y afines, 
 7. Miembro del hogar no remunerado			 limpiadores, lavanderos y planchadores
*/

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (f7>=1 & f7<=2) & (f6_2>=10 & f6_2<=99) & (f5_2~=913) & (tasadeso==0)
 replace WENAS=1 if (edad>=15 & edad<=64) & (f7>=1 & f7<=2) & (f6_2>=10 & f6_2<=99) & (f5_2~=913) & (tasadeso==0)  & sexo==2

	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENAS2=0 if (edad>=15 & edad<=64) & (f7>=1 & f7<=2) & (f6_2>=10 & f6_2<=99) & tasadeso==0
 replace WENAS2=1 if (edad>=15 & edad<=64) & (f7>=1 & f7<=2) & (f6_2>=10 & f6_2<=99) & tasadeso==0 & sexo==2
	
*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator

* NA

** Target 9, Indicator: Proportion of the population using solidfuels (%)
 
/*
9 ¿Cuál es la principal fuente de energía utilizada por este hogar para cocinar?
 1. Electricidad
 2. Gas por cañería
 3. Supergas
 4. Otra fuente (queroseno, leña)
 5. Ninguna
*/

* NA

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
D5
5. ¿Cuál es el origen del agua utilizada por este hogar para beber y cocinar?
 1. Red general 
 2. Pozo surgente (perforado y entubado) 
 3. Otro (aljibe, arroyo, río, cachimba) 

D6
6. ¿Cómo le llega esa agua a la vivienda que ocupa este hogar?
 1. Por cañería dentro de la vivienda 
 2. Por cañería fuera de la vivienda 
 3. Por otros medios 

** D6 2000 Survey => 
 *  1. Cañería dentro de la vivienda 					      (94.71 Pop %)
 *  2  Caneria fuera viv" ==> 2a. Cañería fuera de la viv a menos de 100mts    (4.09 Pop %) 
 * 			      2b. Cañería fuera de la vivienda a más de 100mts (0.57 Pop %) 
 *  3. Otros medios 							       (0.63 Pop %)
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if ((d5>=1 & d5<=3) & (d6>=1 & d6<=3)) /* Total population excluding missing information */
 replace WATER=1 if ((d5>=1 & d5<=2) & (d6>=1 & d6<=2)) 

** Target 10, Proportion of Population with Access to Improved Sanitation, Urban and Rural
/*
D7
7. ¿Esta vivienda tiene servicio sanitario?
 1. Si con cisterna 
 2. Si sin cisterna 
 3. No 			==> 9

D8
8 La evacuación del servicio sanitario ¿se realiza a:
 0. (3 in D7)
 1. Red general 
 2. Fosa séptica, pozo negro
 3. Otro (superficie, etc.) 
*/

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (d7>=1 & d7<=3) /* Total population excluding missing information */
 replace SANITATION=1 if (d7==1 & d8<=2)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
/*
C1
1. Tipo de vivienda:
 1. Casa o apartamento 
 2. Cuarto en casa o apartamento
 3. Otro

D2
2. Con respecto a esta vivienda, ¿este hogar es
 1. Propietario y ya la pagó? 
 2. Propietario y la está pagando?
 3. Arrendatario? 
 4. Ocupante sin pagar con permiso? 
 5. Ocupante sin pagar sin permiso? 

D3
3. ¿Cuál es el número total de habitaciones
que utiliza este hogar, sin considerar
baños ni cocinas?
*/

 gen persroom=(d16/d3) /* D16 Total de personas en el hogar*/

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0     if (c1>=1 & c1<=3) & (d2>=1 & d2<=5) /* Total population excluding missing information */
 replace secten_1=1 	if (c1==3)         | (d2==4 | d2==5) 

* 2. Low quality of the floor or walls materials.

* NA

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.


 gen	 SECTEN=1 if (secten_1>=0 & secten_1<=1) /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Additional indicator
* NA

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) /* Total population excluding missing information */
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (tasadeso==1)  
	
** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
/*
10 Este hogar ¿cuenta con:		SI/NO
 10. Microcomputadora			d10_10
 11. Conexión a Internet 		d10_11
 13. Teléfono 				d10_13
*/

* Gender classification of the population refers to the head of the household.

 rename d10_13 tel /* It is not clear if this category includes cellular phones */ 

 gen	 TELCEL=0 if (tel==1| tel==2) /* Total population excluding missing information */
 replace TELCEL=1 if (tel==1)

 gen	 TEL=0 if (tel==1| tel==2) /* Total population excluding missing information */
 replace TEL=1 if (tel==1)
 	
** Target 18, Indicator: "Personal computers in use per 100 population"

 rename d10_10 computer

* Gender classification of the population refers to the head of the household.

 gen	 COMPUTER=0 if (computer==1 | computer==2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computer==1)

** Target 18, Indicator: "Internet users per 100 population"

 rename d10_11 internet

* Gender classification of the population refers to the head of the household.

 gen 	 INTUSERS=0 if (internet==1 | internet==2)  /* Total population excluding missing information */
 replace INTUSERS=1 if (internet==1)

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

** Disconnected Youths
/*
F24
24.¿Por cuál de las siguientes causas no buscó trabajo,
ni trató de establecer su propio negocio la semana
pasada?
 1. Está incapacitado físicamente
 2. Volverá a trabajar en 30 días
 3. Espera resultado de gestiones
 4. Buscó antes y dejó de buscar
 5. Ninguna razón especial

POBPCOAC
VARIABLE RESUMEN DE CONDICION DE ACTIVIDAD ECONÓMICA		
 1.  Menor de 14 años
 2.  Ocupados
 3.  Desocupados B.T.1.V
 4.  Desocupados propiamente dichos
 5.  Desocupados en seguro de paro
 6.  Inactivo, realiza los quehaceres del hogar
 7.  Inactivo, estudiante
 8.  Inactivo, rentista
 9.  Inactivo, pensionista
 10. Inactivo, jubilado
 11. Inactivo, otro
*/

 gen	 var1=0  if (edad>=15 & edad<=24) 
 replace var1=1  if (edad>=15 & edad<=24) & (f24>=1 & f24<=3)

 gen DISCONN=0 if     (edad>=15 & edad<=24) & (pobpcoac>=1 & pobpcoac<=11)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (pobpcoac==9 | pobpcoac==10 | (pobpcoac==11 & var1==0))

*** Rezago escolar

 gen	 rezago=0	if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7

 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15
 
 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16
 
 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==17
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen 	 REZ=0 if  (edad>=7 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if  (edad>=7 & edad<=17) & (rezago==1)

* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
	
* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))
 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))
 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=17) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=17) & (anoest>=0 & anoest<99)


*/

	
