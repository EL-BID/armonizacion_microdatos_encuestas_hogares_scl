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

local PAIS SLV
local ENCUESTA EHPM
local ANO "1997"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: El Salvador
Encuesta: EHPM
Round: a
Autores: Marcela Rubio (mrubio@iadb.org)
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

clear all
set more off
use "`base_in'", clear


		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


************
* Region_c *
************

gen region_c= r003_dep
label define region_c 1 "Ahuachapán" ///
2 "Santa Ana" ///
3 "Sonsonate" ///
4 "Chalatenango" ///
5 "La Libertad" ///
6 "San Salvador" ///
7 "Cuscatlán" ///
8 "La Paz" ///
9 "Cabañas" ///
10 "San Vicente" ///
11 "Usulután" ///
12 "San Miguel" ///
13 "Morazán" ///
14 "La Unión" 
label value region_c region_c
label var region_c "División política"

***************
***factor_ch***
***************

gen factor_ch=fac31_fa
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

sort folio tipo_de_
egen idh_ch= group(folio tipo)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci=r101_num
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if area==2
replace zona_c=1 if area==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="SLV"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1997
label variable anio_c "Anio de la encuesta"


*********
***mes***
*********
gen byte mes_c=.
/*No disponible en la encuesta*/

label variable mes_c "Mes de la encuesta"


*****************
***relacion_ci***
*****************

gen relacion_ci = .
replace relacion_ci = 1 if r103_par==1
replace relacion_ci = 2 if r103_par==2
replace relacion_ci = 3 if r103_par==3
replace relacion_ci = 4 if r103_par>=4 & r103_par<=10
replace relacion_ci = 5 if r103_par==12 | r103_par==13
replace relacion_ci = 6 if r103_par==6

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=fac11_fa
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=r104_gen

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci
label var sexo_ci "Sexo del individuo"

**********
***edad***
**********

gen edad_ci=r106_eda
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci = .
replace civil_ci = 1 if r107_est==1
replace civil_ci = 2 if r107_est==2
replace civil_ci = 3 if r107_est==4
replace civil_ci = 4 if r107_est==3
replace civil_ci=. if edad_ci<12 /*Hay una persona menor a 12 que contesta esta pregunta*/

label var civil_ci "Estado civil del individuo"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci 

*************
***jefe_ci***
*************

gen jefe_ci=(relacion_ci==1)
replace jefe_ci = . if relacion_ci==.
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

gen byte clasehog_ch = .
**** unipersonal 
replace clasehog_ch = 1 if nconyuges_ch==0 & nhijos_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch = 2 if (nhijos_ch>0 | nconyuges_ch>0) & notropari_ch==0 & notronopari_ch==0
**** ampliado
replace clasehog_ch = 3 if notropari_ch>0 & notronopari_ch==0
**** compuesto  (some relatives plus non relative)
replace clasehog_ch = 4 if (nhijos_ch>0 | nconyuges_ch>0 |  notropari_ch>0)  & notronopari_ch>0
**** corresidente
replace clasehog_ch = 5 if nconyuges_ch==0 & nhijos_ch==0 & notropari_ch==0 & notronopari_ch>0 

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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci = (relacion_ci>=1 & relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .

gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"
notes raza_ci: En el cuestionario no consta una pregunta relacionada con raza.


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci = 1 if r201_tra==1 | r204_eve<6 | r203a_ti==1
replace condocup_ci = 2 if (r201_tra==2 | r204_eve==6 | r203a_ti==2) & r206_bus==1 & r207_que<=7
recode condocup_ci . = 3 if edad_ci>=10 
recode condocup_ci . = 4 if edad <10

label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

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

*****************
***desalent_ci***
*****************

gen desalent_ci = (emp_ci==0 & r209_bus==5)

*****************
***horaspri_ci***
*****************

recode r203b_hr (91=.) (98=.)
gen horaspri_ci = r202a_hr if r201_tra==1
replace horaspri_ci = r203b_hr if r201_tra==2 & r203a_ti==1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horassec_ci***
*****************

gen horassec_ci = r222b_hr  if r222_otr==1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"


*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci horassec_ci)
replace horastot_ci=. if horaspri_ci==. & horassec_ci==.

******************************
*	subemp_ci
******************************

gen subemp_ci=0
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (r212_men == 2 | r212_men ==3)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if emp_ci==1 & horaspri_ci<=30 & r212_men == 1

******************
***categopri_ci***
******************

gen categopri_ci  = . 
replace categopri_ci = 0 if r216_cal==9
replace categopri_ci = 1 if r216_cal==1
replace categopri_ci = 2 if r216_cal==2 | r216_cal==4 
replace categopri_ci = 3 if r216_cal==5 | r216_cal==6 | r216_cal==7 | r216_cal==8
replace categopri_ci = 4 if r216_cal==3 

label define categopri_ci 0 "Otro" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & r222_otr==1
replace nempleos_ci=. if pea_ci==0
/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if 
<5
replace firmapeq_ci=0 if tamest>=5 & tamest<99999
replace firmapeq_ci=. if emp==0

/*En este caso no se le pregunta a los que trabajan como servicio domestico.
Y estos son missings */
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci = 1 if r217_tra==2
replace spublico_ci = 0 if r217_tra==1

*Sólo se le hace esta pregunta a los asalariados, aprendices y otros


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (r213_ciu>=211 & r213_ciu<=369) & emp_ci==1
replace ocupa_ci=2 if (r213_ciu>=111 & r213_ciu<=131) & emp_ci==1
replace ocupa_ci=3 if (r213_ciu>=411 & r213_ciu<=422) & emp_ci==1
replace ocupa_ci=4 if ((r213_ciu>=520 & r213_ciu<=526) | r213_ciu==911) & emp_ci==1
replace ocupa_ci=5 if ((r213_ciu>=511 & r213_ciu<=516) | (r213_ciu>=912 & r213_ciu<=916)) & emp_ci==1
replace ocupa_ci=6 if ((r213_ciu>=611 & r213_ciu<=621) | (r213_ciu>=921 & r213_ciu<=922)) & emp_ci==1
replace ocupa_ci=7 if ((r213_ciu>=711 & r213_ciu<=834) | (r213_ciu>=931 & r213_ciu<=933)) & emp_ci==1
replace ocupa_ci=8 if r213_ciu==11 & emp_ci==1
replace ocupa_ci=. if emp_ci==0 | r213_ciu==999 | r213_ciu==0

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (r214_cii>=11 & r214_cii<=50) & emp_ci==1
replace rama_ci=2 if (r214_cii>=101 & r214_cii<=142) & emp_ci==1
replace rama_ci=3 if (r214_cii>=151 & r214_cii<=372) & emp_ci==1
replace rama_ci=4 if (r214_cii>=401 & r214_cii<=410) & emp_ci==1
replace rama_ci=5 if (r214_cii>=451 & r214_cii<=455) & emp_ci==1
replace rama_ci=6 if (r214_cii>=501 & r214_cii<=552) & emp_ci==1 
replace rama_ci=7 if (r214_cii>=601 & r214_cii<=642) & emp_ci==1
replace rama_ci=8 if (r214_cii>=651 & r214_cii<=749) & emp_ci==1
replace rama_ci=9 if (r214_cii>=751 & r214_cii<=990) & emp_ci==1


****************
***durades_ci***
****************
gen sem=.
replace sem=r208a_se/4.3 /*La variable debe llevarse a meses*/
gen mesess=.
replace mesess=r208b_me  
gen anosb=.
replace anosb=r208c_an/12  /*La variable debe llevarse a meses*/

egen durades_ci=rsum(sem mesess anosb)
replace durades_ci=. if sem==. & mesess==. & anosb==.
*replace durades_ci=. if emp==1

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.
   
****************
*cotizando_ci***
****************

gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (r220_seg==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************

gen tamemp_ci=1 if r227_num==1 | r227_num==2
replace tamemp_ci=2 if r227_num==3 | r227_num==4
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
gen pension_ci=(r228h_ju>=1 & r228h_ju<999999)
label var pension_ci "1=Recibe pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************

gen cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if r210_tra==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*li_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*gen salmm_ci= 132

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=78 if rama_ci==1
replace salmm_ci=132 if rama_ci==3
replace salmm_ci=132 if rama_ci==6
replace salmm_ci=114 if salmm_ci==.
label var salmm_ci "Salario minimo legal"

*****************
**categoinac_ci**
*****************	

gen categoinac_ci=.	
replace categoinac_ci=1 if r209_bus==11 & condocup_ci==3
replace categoinac_ci=2 if (r209_bus==7 | r209_bus==13) & condocup_ci==3
replace categoinac_ci=3 if r209_bus==10 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
*****************
***formal_ci*****
*****************
gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"




*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************


*Conversión Colones a dólares 
/*
Fuente: http://www.iadb.org/en/research-and-data/latin-american-and-caribbean-macro-watch,8633.html

1997-Jan	8.76
1997-Feb	8.76
1997-Mar	8.76
1997-Apr	8.76
1997-May	8.76
1997-Jun	8.76
1997-Jul	8.76
1997-Aug	8.76
1997-Sep	8.76
1997-Oct	8.76
1997-Nov	8.76
1997-Dec	8.76

*/
        
gen bonif=.
replace bonif=r228a_bo*r228a1/12
replace bonif=0 if emp_ci==1 & r228a_bo==.

gen comisiones=.
replace comisiones=r228b_co*r228b1/12
replace comisiones=0 if emp==1 & r228b_co==.
replace comisiones=. if r228b_co==999999

gen dietass=.
replace dietass=r228c_di*r228c1/12
replace dietass=0 if emp==1 & r228c_di==.

gen viaticoss=.
replace viaticoss=r228d_vi*r228d1/12
replace viaticoss=0 if emp==1 & r228d_vi==.

gen especiess=.
replace especiess=r228e_es*r228e1/12
replace especiess=0 if emp==1 & r228e_es==.

gen deveh=.
replace deveh=r228f_de*r228f1/12
replace deveh=0 if emp==1 & r228f_de==.

gen combuss=.
replace combuss=r228g_co*r228g1/12
replace combuss=0 if emp==1 & r228g_co==.

gen jubilacion=.
replace jubilacion=r228h_ju*r228h1/12
replace jubilacion=0 if emp==1 & r228h_ju==.
replace jubilacion=. if r228h_ju==999999 

gen acteventual=.
replace acteventual=r228i_ev* r228i1/12
replace acteventual=0 if emp==1 & r228i_ev==.

gen cuotalim=.
replace cuotalim=r228j_al*r228j1/12
replace cuotalim=0 if emp==1 & r228j_al==.

gen alquileres=.
replace alquileres=r228k_al*r228k1/12
replace alquileres=0 if emp==1 & r228k_al==.

gen alqneg=.
replace alqneg=r228l_ne*r228l1/12
replace alqneg=0 if emp==1 & r228l_ne==.

gen arrendamiento=.
replace arrendamiento=r228m_te*r228m1/12
replace arrendamiento=0 if emp==1 & r228m_te==.

gen ayuda=.
replace ayuda=r228n_ay*r228n1/12
replace ayuda=0 if emp==1 & r228n_ay==.

gen otross=.
replace otross=r228o_ot*r228o1/12
replace otross=0 if emp==1 & r228o_ot==.

gen aguinals=.
replace aguinals=r22801_a/12
replace aguinals=0 if emp==1 & r22801_a==.
replace aguinals=. if r22801_a==999999 

gen dividendos=.
replace dividendos=r22802_d/12
replace dividendos=0 if emp==1 & r22802_d==.
replace dividendos=. if r22802_d==999999

gen bonificaciones=.
replace bonificaciones=r22803_b/12
replace bonificaciones=0 if emp==1 & r22803_b==.

gen depauto=.
replace depauto=r22804_d/12
replace depauto=0 if emp==1 & r22804_d==.

gen vtainm=.
replace vtainm=r22805_v/12
replace vtainm=0 if emp==1 & r22805_v==.

***************
***ylmpri_ci***
***************

/*Para todos los trabajadores empleados*/

gen ypri=.
replace ypri= r219_mon*30 if r219b_fo==1
replace ypri= r219_mon*4.3 if r219b_fo==2
replace ypri= r219_mon*2 if r219b_fo==3
replace ypri= r219_mon if r219b_fo==4 | r219b_fo==8
replace ypri= r219_mon/3 if r219b_fo==5
replace ypri= r219_mon/6 if r219b_fo==6
replace ypri= r219_mon/12 if r219b_fo==7
replace ypri= 0 if r219b_fo==9
replace ypri=. if r219_mon==999999
replace ypri=. if emp_ci==0

* Conversión de colones a dólares
egen ylmpri_ci = rsum(comisiones dietass viaticoss bonif aguinals bonificaciones ypri), missing
replace ylmpri_ci=. if r219_mon==999999 
replace ylmpri_ci=. if emp==0
replace ylmpri_ci= ylmpri_ci/8.76
 
gen ylmpri1_ci=. /*NA, esta variable aparece recién en 1998*/

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=. /*NA, esta variable aparece recién en 1998*/

*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

egen ylnmpri_ci=rsum(especiess combuss), missing
replace ylnmpri_ci=. if especiess==. & combuss==. 
replace ylnmpri_ci=. if emp_ci==0
*Conversión colones a dólares
replace ylnmpri_ci= ylnmpri_ci/8.76 

gen ylnmpri1_ci=. /*NA, esta variable aparece recién en 1998*/

***************
***ylmsec_ci***
***************

gen ylmsec_ci=.
replace ylmsec_ci=r222c_in
replace ylmsec_ci=. if emp==0 | r222_otr==2
replace ylmsec_ci=. if r222c_in==999999 
*Conversión colones a dólares
replace ylmsec_ci= ylmsec_ci/8.76 

gen ylmsec1_ci=.

****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=. /*NA, esta variable aparece recién en 1998*/

****************
***remesas_ci***
****************

rename r422a2_c r422a1_c
rename r422b2_c r422b1_c
rename r422c2_c r422c1_c
rename r422d2_c r422d1_c
rename r422e2_c r422e1_c

foreach i in a b c d e {
gen remesas`i' = r422`i'1_c*2 if r422a1_f==0
replace remesas`i' = r422`i'1_c if r422`i'1_f==1
replace remesas`i' = r422`i'1_c/2 if r422`i'1_f==2
replace remesas`i' = r422`i'1_c/3 if r422`i'1_f==3
replace remesas`i' = r422`i'1_c/4 if r422`i'1_f==4
replace remesas`i' = r422`i'1_c/5 if r422`i'1_f==5
replace remesas`i' = r422`i'1_c/6 if r422`i'1_f==6
replace remesas`i' = r422`i'1_c/8 if r422`i'1_f==8
replace remesas`i' = r422`i'1_c/12 if r422`i'1_f==12
replace remesas`i'=.  if r422`i'1_c==99999
}



egen remesaux=rsum(remesasa remesasb remesasc remesasd remesase), missing
replace remesaux=. if remesasa==. & remesasb==. & remesasc==. & remesasd==. & remesase==.

gen remesas_ci=. 
replace remesas_ci = remesaux

*Conversión colones a dólares
replace remesas_ci= remesas_ci/8.76 


**************************
***ynlm0_ci & ynlnm0_ci***
**************************

egen ynlm_ci=rsum(ayuda cuotalim alquileres alqneg jubilacion deveh otross dividendos acteventual arrendamiento depauto vtainm remesaux), missing
replace ynlm_ci=. if emp_ci==0
*Conversión colones a dólares
replace ynlm_ci= ynlm_ci/8.76 

gen ynlnm_ci=.

* Para capturar el ingreso de los desocupados 
egen ylmotros_ci = rsum(comisiones dietass viaticoss bonif aguinals bonificaciones), missing
replace ylmotros_ci =. if emp_ci ==1
*Conversión colones a dólares
replace ylmotros_ci= ylmotros_ci/8.76 

egen ylnmotros_ci=rsum(especiess combuss), missing
replace ylnmotros_ci=. if emp_ci==1
*Conversión colones a dólares
replace ylnmotros_ci= ylnmotros_ci/8.76 


**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing

*gen ylm1_ci=. /*NA, esta variable aparece recién en 1998*/

************************
***ylnm_ci & ylnm1_ci***
************************
*gen ylnm_ci=. /*NA, esta variable aparece recién en 1998*/

egen ylnm_ci= rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci), missing

*gen ylnm1_ci=. /*NA, esta variable aparece recién en 1998*/

*************
*ypen_ci*
*************

gen ypen_ci = r228h_ju*r228h1/12 if pension_ci==1
replace ypen_ci = . if r228h_ju== 999999

*Conversión colones a dólares
replace ypen_ci= ypen_ci/8.76
label var ypen_ci "Valor de la pension contributiva"


************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

**********************************
*** nrylmpri_ch & nrylmpri1_ch ***
**********************************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar"

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1


**************************
*** ylnm_ch & ylnm1_ch ***
**************************

*Modificación Mayra Sáenz - Septiembre 2014
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch  "Ingreso laboral no monetario del Hogar"

******************
*** remesas_ch ***
******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 



***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del Hogar"

****************
*** ynlnm_ch ***
****************

by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1

*******************
*** autocons_ci ***
*******************

gen autocons_ci=. /*NA, esta variable aparece recién en 1999*/

*******************
*** autocons_ch ***
*******************

gen autocons_ch=. /*NA, esta variable aparece recién en 1999*/

*******************
*** rentaimp_ch ***
*******************

by idh_ch, sort: egen rentaimp_ch=sum(alquileres) if miembros_ci==1, missing
*Conversión colones a dólares
replace rentaimp_ch= rentaimp_ch/8.76 

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

**************
***ylmho_ci***
**************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

**************************
***EDUCATION INDICATORS***
**************************

/* La variable r110_que nos permiten identificar los años de educación
para aquellos individuos que actualmente estan estudiando. 
La variable r118_ult indica el último nivel alcanzado y el año 
alcanzado en dicho nivel, permiten calcular los años de educación para aquellos que
actualmente no asisten a un establecimiento escolar.
En El Salvador, la educación básica dura nueve años y la educación media tres años*/

gen byte aedu_ci=.

* Años de educacion para aquellos que actualmente están estudiando, no consideramos aquellos que tienen educacion especial

replace aedu_ci=0 if r110_que==0 & r109_est==1
replace aedu_ci=0 if r110_que==11 |  r110_que==12 | r110_que==13

replace aedu_ci=1 if  r110_que==21
replace aedu_ci=2 if  r110_que==22
replace aedu_ci=3 if  r110_que==23
replace aedu_ci=4 if  r110_que==24
replace aedu_ci=5 if  r110_que==25
replace aedu_ci=6 if  r110_que==26
replace aedu_ci=7 if  r110_que==27
replace aedu_ci=8 if  r110_que==28
replace aedu_ci=9 if  r110_que==29
replace aedu_ci=10 if  r110_que==41
replace aedu_ci=11 if  r110_que==42
replace aedu_ci=12 if  r110_que==43
replace aedu_ci=12 if  r110_que==44
replace aedu_ci=13 if  r110_que==51 | r110_que==61 
replace aedu_ci=14 if  r110_que==52 | r110_que==62
replace aedu_ci=15 if  r110_que==53 | r110_que==63
replace aedu_ci=16 if  r110_que==64
replace aedu_ci=17 if  r110_que==65
replace aedu_ci=18 if  r110_que==66
replace aedu_ci=19 if  r110_que==67
replace aedu_ci=20 if  r110_que==68
replace aedu_ci=21 if  r110_que==69
replace aedu_ci=22 if  r110_que==70

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0


/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if r118_ult==0 & r109_est==2
replace aedu_ci=0 if r118_ult==12 | r118_ult==13

replace aedu_ci=1 if  r118_ult==21
replace aedu_ci=2 if  r118_ult==22
replace aedu_ci=3 if  r118_ult==23
replace aedu_ci=4 if  r118_ult==24
replace aedu_ci=5 if  r118_ult==25
replace aedu_ci=6 if  r118_ult==26
replace aedu_ci=7 if  r118_ult==27
replace aedu_ci=8 if  r118_ult==28
replace aedu_ci=9 if  r118_ult==29
replace aedu_ci=10 if  r118_ult==41
replace aedu_ci=11 if  r118_ult==42
replace aedu_ci=12 if  r118_ult==43
replace aedu_ci=12 if  r118_ult==44
replace aedu_ci=13 if  r118_ult==51 | r118_ult==61 
replace aedu_ci=14 if  r118_ult==52 | r118_ult==62
replace aedu_ci=15 if  r118_ult==53 | r118_ult==63
replace aedu_ci=16 if  r118_ult==64
replace aedu_ci=17 if  r118_ult==65
replace aedu_ci=18 if  r118_ult==66
replace aedu_ci=19 if  r118_ult==67
replace aedu_ci=20 if  r118_ult==68
replace aedu_ci=21 if  r118_ult==69
replace aedu_ci=22 if  r118_ult==70

replace aedu_ci=. if aedu_ci>edad & aedu_ci~=. 


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
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la eecundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"
***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=0
replace edupre_ci=1 if r110_que==11 | r110_que==12 | r110_que==13 | r118_ult==11 | r118_ult==12 | r118_ult==13 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (r110_que>=61 & r110_que<=69) | (r118_ult>=61 & r118_ult<=69) & aedu_ci~=.
replace eduac_ci=0 if (r110_que>=51 & r110_que<=53) | (r118_ult>=51 & r118_ult<=53) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if r109_est==1
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=r119_no_
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 
label define pqnoasis_ci 2 "Muy caro" 3 "Muy lejos" 4 "No hay profesor", add 
label define pqnoasis_ci 5 "Cerro el centro" 6 "Repitio mucho" , add
label define pqnoasis_ci 7 "No vale la pena" 8 "Por la edad" 9 " No hay escuela nocturna", add
label define pqnoasis_ci 10 " Finalizo sus estudios"  11 " Causas del hogar" 12 " No existe otro grado" 13 "Otros" , add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if r119_no_ ==2
replace pqnoasis1_ci = 2 if r119_no_ ==1
replace pqnoasis1_ci = 4 if r119_no_ ==7
replace pqnoasis1_ci = 5 if r119_no_ ==11
replace pqnoasis1_ci = 6 if r119_no_ ==10
replace pqnoasis1_ci = 7 if r119_no_ ==8
replace pqnoasis1_ci = 8 if r119_no_ ==3  | r119_no_ ==4  | r119_no_ ==5  | r119_no_ ==9 | r119_no_ ==12 
replace pqnoasis1_ci = 9 if r119_no_ ==6  | r119_no_ ==13

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
gen repite_ci=.

/*NA*/

******************
***repiteult_ci***
******************

gen repiteult_ci=(r111_rep==1)
replace repiteult_ci=. if asiste_ci==0
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if r112_ens==1 
replace edupub_ci=2 if r112_ens==2 | r112_ens==3

/* Variable centroen:
1: Centro de enseñanza oficial: 
Es aquel cuya administración y funcionamiento depende del gobierno.
2: Centro de Enseñanza Laico: 
Son todos los centros educativos privados no religiosos. 
3: Centro de Enseñanza religioso: 
Son todos los centros educativos que pertenecen a una Congregación Religiosa. 
*/


***************
***tecnica_ci**
***************
gen tecnica_ci=((r110_que>=51 & r110_que<=53) | (r118_ult>=51 & r118_ult<=53))
label var tecnica_ci "=1 formacion terciaria tecnica"	


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(r305b_ti==1 | r305b_ti==2)

gen aguadist_ch=1 if r305b_ti==1
replace aguadist_ch=2 if r305b_ti==2
replace aguadist_ch=3 if r305c_ag>=1 & r305c_ag<=8

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(r305a_al==1 | r305a_al==2)

gen luzmide_ch=.
/*NA*/

gen combust_ch=(r305a_al>=1 & r305a_al<=4)

gen bano_ch = (r305f_sa>=1 & r305f_sa<=4) | (r305g_no==1 | r305g_no==3)

gen banoex_ch=(r305g_no>=1 & r305g_no<=3)

gen des1_ch=.
replace des1_ch = 0 if r305f_sa==5
replace des1_ch = 1 if r305f_sa==1 | r305f_sa==2
replace des1_ch = 2 if r305f_sa==3 | r305f_sa==4

gen des2_ch=.
replace des2_ch = 0 if r305f_sa==5
replace des2_ch = 1 if r305f_sa==1 | r305f_sa==2 | r305f_sa==3 | r305f_sa==4

gen piso_ch=.
replace piso_ch=0 if r302c_pi==5
replace piso_ch=1 if r302c_pi>=1 & r302c_pi<=4
replace piso_ch=2 if r302c_pi==6

gen pared_ch=0 if r302b_pa==2 | r302b_pa==3 | r302b_pa==6 | r302b_pa==7
replace pared_ch=1 if r302b_pa==1 | r302b_pa==4 | r302b_pa==5

gen techo_ch=0 if r302a_te==5 | r302a_te==6
replace techo_ch=1 if r302a_te>=1 & r302a_te<=4

gen resid_ch=0 if r305k_ba==1 | r305k_ba==2
replace resid_ch=1 if r305k_ba==4 | r305k_ba==5
replace resid_ch=2 if r305k_ba==6
replace resid_ch=3 if r305k_ba==3 | r305k_ba==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (r305b_ti >=1 & r305b_ti <=2) | r305c_ag == 1 | r305c_ag == 4 | r305c_ag == 8
replace aguamejorada_ch = 0 if (r305b_ti >=3 & r305b_ti <=4) | r305c_ag == 2 | r305c_ag == 3 | (r305c_ag >=5 & r305c_ag <=7) | r305c_ag == 9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (r305f_sa>=1 & r305f_sa <=4)
replace banomejorado_ch = 0 if  r305d_ba ==3 | r305f_sa ==5

gen dorm_ch=r3032_do

gen cuartos_ch=r3031_ha

gen cocina_ch=.
/*NA*/

gen telef_ch=(r305j_te==1)
replace telef_ch = . if r305j_te==.

gen refrig_ch=(r306e_re==1)
replace refrig_ch=. if r306e_re==.

gen freez_ch=.
/*NA*/

gen auto_ch=(r306k_ve==1)
replace auto_ch=. if r306k_ve==.


gen compu_ch=(r306i_co==1)
replace compu_ch=. if r306i_co==.

gen internet_ch=.
/*NA*/

gen cel_ch=.
/*NA*/

gen vivi1_ch=1 if r301_tip==1
replace vivi1_ch=2 if r301_tip>=2 & r301_tip<=4
replace vivi1_ch=3 if r301_tip>4 & r301_tip<.

gen vivi2_ch=(r301_tip>=1 & r301_tip<=4)
replace vivi2_ch=. if r301_tip==.

gen viviprop_ch=0 if r304a_te==3
replace viviprop_ch=1 if r304a_te==1
replace viviprop_ch=2 if r304a_te==2
replace viviprop_ch=3 if r304a_te>=4 & r304a_te<=6


gen viviitit_ch=.
/*NA*/

gen vivialq_ch=r304e_cu if r304a_te==3
replace vivialq_ch=. if r304e_cu==99999

*Conversión colones a dólares
replace vivialq_ch= vivialq_ch/8.76 


gen vivialqimp_ch=r304d_si 
replace vivialqimp_ch=. if r304d_si==99999
*Conversión colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76 

*variables que faltan generar
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen tipopen_ci=.
gen vivitit_ch=.
gen categosec_ci=.

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




compress


saveold "`base_out'", replace


log close



