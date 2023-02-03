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

local PAIS SLV
local ENCUESTA EHPM
local ANO "1998"
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

gen factor_ch=fac00
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

sort folio tipo_de_
egen idh_ch= group(folio tipo_de_)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci=nrorden
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

gen anio_c=1998
label variable anio_c "Anio de la encuesta"


*********
***mes***
*********

recode r113_mes (99=.)
gen byte mes_c=r113_mes

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

gen factor_ci=fac00
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

gen edad_ci=r106a_ed
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci = .
replace civil_ci = 1 if r107_est==1
replace civil_ci = 2 if r107_est==2
replace civil_ci = 3 if r107_est==4
replace civil_ci = 4 if r107_est==3

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
replace condocup_ci = 1 if r401_tra==1 | r404_eve<7 | r403a_ti==1
replace condocup_ci = 2 if (r401_tra==2 | r404_eve==7 | r403a_ti==2) & r406_bus==1 & r407_que<=7
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

gen desalent_ci = (emp_ci==0 & (r409_bus==3 | r409_bus==5))

*****************
***horaspri_ci***
*****************

gen horaspri_ci = r402a_hr if r401_tra==1
replace horaspri_ci = r403b_hr if r401_tra==2 & r403a_ti==1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horassec_ci***
*****************

gen horassec_ci = r431_hrs  if r430_otr==1
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
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (r412_men== 2 | r412_men==3)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if emp_ci==1 & horaspri_ci<=30 & r412_men== 1

******************
***categopri_ci***
******************

gen categopri_ci  = . 
replace categopri_ci = 0 if r416_cal==10
replace categopri_ci = 1 if r416_cal==1
replace categopri_ci = 2 if r416_cal==2 | r416_cal==3 | r416_cal==4 
replace categopri_ci = 3 if r416_cal==6 | r416_cal==7 | r416_cal==8 | r416_cal==9
replace categopri_ci = 4 if r416_cal==5

label define categopri_ci 0 "Otro" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & r430_otr==1
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
replace spublico_ci = 1 if r418_tra==2
replace spublico_ci = 0 if r418_tra==1

*Sólo se le hace esta pregunta a los asalariados, aprendices y otros


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (r413_ciu>=211 & r413_ciu<=369) 
replace ocupa_ci=2 if (r413_ciu>=111 & r413_ciu<=131)
replace ocupa_ci=3 if (r413_ciu>=411 & r413_ciu<=422) 
replace ocupa_ci=4 if ((r413_ciu>=520 & r413_ciu<=526) | r413_ciu==911) 
replace ocupa_ci=5 if ((r413_ciu>=511 & r413_ciu<=516) | (r413_ciu>=912 & r413_ciu<=916)) 
replace ocupa_ci=6 if ((r413_ciu>=611 & r413_ciu<=621) | (r413_ciu>=921 & r413_ciu<=922)) 
replace ocupa_ci=7 if ((r413_ciu>=711 & r413_ciu<=834) | (r413_ciu>=931 & r413_ciu<=933)) 
replace ocupa_ci=8 if r413_ciu==11 
replace ocupa_ci=. if emp_ci==0 | r413_ciu==999 

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (r414_cii>=11 & r414_cii<=50) 
replace rama_ci=2 if (r414_cii>=101 & r414_cii<=142) 
replace rama_ci=3 if (r414_cii>=151 & r414_cii<=372) 
replace rama_ci=4 if (r414_cii>=401 & r414_cii<=410) 
replace rama_ci=5 if (r414_cii>=451 & r414_cii<=455) 
replace rama_ci=6 if (r414_cii>=501 & r414_cii<=552)  
replace rama_ci=7 if (r414_cii>=601 & r414_cii<=642) 
replace rama_ci=8 if (r414_cii>=651 & r414_cii<=749)
replace rama_ci=9 if (r414_cii>=751 & r414_cii<=990) 

****************
***durades_ci***
****************
gen sem=.
replace sem=r408a_se/4.3 /*La variable debe llevarse a meses*/
replace sem=0.5/4.3 if r408a_se==0

gen mesess=.
replace mesess=r408b_me
replace mesess=0.5 if r408b_me==0

*Modificación David Cornejo - Febrero 2023
* Para convertir años a meses se debe multiplicar por 12
gen anosb=.
replace anosb=r408c_an*12  /*La variable debe llevarse a meses*/
replace anosb=6 if r408c_an==0

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
replace cotizando_ci=1 if (r422_afi==1) & cotizando_ci==0 
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

gen tamemp_ci=1 if r421_num>=1 & r421_num<=5 
replace tamemp_ci=2 if r421_num>=6 & r421_num<=50
replace tamemp_ci=3 if r421_num>50 & r421_num<999
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
gen pension_ci=(r439g1_j>=1 &  r439g1_j<999999)
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
*Modificación David Cornejo - Febrero 2023
* En los ocasiones que r439g2_v ==999 no se puede conocer el valor de la pension anual

gen ypen_ci=r439g1_j*r439g2_v/12 if pension_ci==1
replace ypen_ci = . if r439g1_j==999999 | r439g2_v ==999
* Conversión Colones a dólares
replace ypen_ci= ypen_ci/8.76
label var ypen_ci "Valor de la pension contributiva"

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
replace cesante_ci=1 if r410_tra==1 & condocup_ci==2
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
*gen salmm_ci= 144

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=78 if rama_ci==1
replace salmm_ci=144 if rama_ci==3
replace salmm_ci=144 if rama_ci==6
replace salmm_ci=122 if salmm_ci==.
label var salmm_ci "Salario minimo legal"

*****************
**categoinac_ci**
*****************	

gen categoinac_ci=.	
replace categoinac_ci=1 if r409_bus==10 
replace categoinac_ci=2 if (r409_bus==6 | r409_bus==12)
replace categoinac_ci=3 if r409_bus==9 
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

*Modificación Mayra Sáenz - Septiembre 2014
*Conversión Colones a dólares
/*
Fuente: http://www.iadb.org/en/research-and-data/latin-american-and-caribbean-macro-watch,8633.html
1998-Jan	8.76
1998-Feb	8.76
1998-Mar	8.76
1998-Apr	8.76
1998-May	8.76
1998-Jun	8.76
1998-Jul	8.76
1998-Aug	8.76
1998-Sep	8.76
1998-Oct	8.76
1998-Nov	8.76
1998-Dec	8.76
*/


****************************
***ylmpri_ci & ylmpri1_ci***
****************************

/*Para los trabajadores dependientes*/

gen yprid=.
replace yprid=r424_sue*30 if r423_per==1
replace yprid=r424_sue*4.3 if r423_per==2
replace yprid=r424_sue*2 if r423_per==3
replace yprid=r424_sue if r423_per==4 | r423_per==5
replace yprid=0 if r423_per==6
replace yprid=. if r424_sue==999999

gen hrsextrasd=.
replace hrsextrasd=r425a1_h*r425a2_v/12
replace hrsextrasd=999999 if r425a1_h==999999 | r425a2_v==999

gen vacacionesd=.
replace vacacionesd=r425b1_s*r425b2_v/12
replace vacacionesd=999999 if r425b1_s==999999 | r425b2_v==999

gen aguinaldod=.
replace aguinaldod=r425c1_a*r425c2_v/12
replace aguinaldod=999999 if r425c1_a==999999 | r425c2_v==999

gen bonificacionesd=.
replace bonificacionesd=r425d1_b *r425d2_v/12
replace bonificacionesd=999999 if r425d1_b==999999 | r425d2_v==999

egen yprijbd=rsum( yprid hrsextrasd vacacionesd aguinaldod bonificacionesd), missing
replace yprijbd=999999 if yprid==999999 | hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999 
replace yprijbd=yprid if yprid>0 & yprid~=999999 & ( hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999)
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. & hrsextrasd==. & vacacionesd==. & aguinaldod==. & bonificacionesd==. 

/*Para los trabajadores independientes*/

gen ingrneto=r428_ing-r429_gas
replace ingrneto=0 if ingrneto<0
replace ingrneto=999999 if r428_ing==999999 | r429_gas==999999

gen yprijbi=.
replace yprijbi=ingrneto*30 if r427_per==1
replace yprijbi=ingrneto*4.3 if r427_per==2
replace yprijbi=ingrneto*2 if r427_per==3
replace yprijbi=ingrneto if r427_per==4 | r427_per==6
replace yprijbi=ingrneto/12 if r427_per==5
replace yprijbi=999999 if ingrneto>=999999 | r427_per==9
replace yprijbi=. if categopri_ci>2


/*Ojo con esto último. Originalmente la encuesta conputa una serie de 
missings que no corresponden a casos de no respuesta, sino
que es un grupo de trabajadores independientes considerados como productores 
agropecuarios, para ser consistente con el tratamiento de las encuestas anteriores
se le asigna ingreso cero a estas personas*/


egen ylmpri_ci=rsum(yprijbi yprid), missing
replace ylmpri_ci=. if yprijbi==999999 | yprid==999999
replace ylmpri_ci=. if yprid==. & yprijbi==.
replace ylmpri_ci=. if emp==0
* Conversión Colones a dólares
replace ylmpri_ci= ylmpri_ci/8.76


egen ylmpri1_ci=rsum(yprijbi yprijbd), missing
replace ylmpri1_ci=. if yprijbi==999999 | yprijbd==999999
replace ylmpri1_ci=. if yprijbd==. & yprijbi==.
replace ylmpri1_ci=. if emp==0

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=(ylmpri1_ci==. & emp_ci==1)

*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

gen food1=.
replace food1=r425e1_a*r425e2_v/12 
replace food1=0 if emp==1 & r425e1_a==.
replace food1=. if r425e1_a==999999 | r425e2_v==999

gen ropa1=.
replace ropa1=r425f1_r*r425f2_v/12 
replace ropa1=0 if emp==1 & r425f1_r==.
replace ropa1=. if r425f1_r==999999 | r425f2_v==999

gen merca1=.
replace merca1=r425g1_m*r425g2_v/12 
replace merca1=0 if emp==1 & r425g1_m==.
replace merca1=. if r425g1_m==999999 | r425g2_v==999

gen vivi1=.
replace vivi1=r425h1_v*r425h2_v/12 
replace vivi1=0 if emp==1 & r425h1_v==.
replace vivi1=. if r425h1_v==999999 | r425h2_v==999

gen trans1=.
replace trans1=r425i1_t*r425i2_v/12 
replace trans1=0 if emp==1 & r425i1_t==.
replace trans1=. if r425i1_t==999999 | r425i2_v==999

gen segur1=.
replace segur1=r425j1_s*r425j2_v/12 
replace segur1=0 if emp==1 & r425j1_s==.
replace segur1=. if r425j1_s==999999 | r425j2_v==999

gen otross1=.
replace otross1=r425k1_o*r425k2_v/12 
replace otross1=0 if emp==1 & r425k1_o==.
replace otross1=. if r425k1_o==999999 | r425k2_v==999

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0
* Conversión Colones a dólares
replace ylnmpri_ci= ylnmpri_ci/8.76

gen ylnmpri1_ci=.
/*No hay autoconsumo en esta encuesta*/

***************
***ylmsec_ci***
***************

gen ysec1=r432_ing
replace ysec1=. if r432_ing==999999

gen hrsextras=.
replace hrsextras=r433a1_h*r433a2_v/12
replace hrsextras=. if r433a1_h==999999 | r433a2_v==999

gen vacaciones=.
replace vacaciones=r433b1_s*r433b2_v/12
replace vacaciones=. if r433b1_s==999999 | r433b2_v==999

gen aguinaldo=.
replace aguinaldo=r433c1_a*r433c2_v/12
replace aguinaldo=. if r433c1_a==999999 | r433c2_v==999

gen bonificaciones=.
replace bonificaciones=r433d1_b*r433d2_v/12
replace bonificaciones=. if r433d1_b==999999 | r433d2_v==999

gen ylmsec_ci=ysec1

egen ylmsec1_ci=rsum(ysec1 hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if ysec1==. & hrsextras==. & vacaciones==. & aguinaldo==. & bonificaciones==. 
replace ylmsec1_ci=. if emp_ci==0 | r430_otr==2

* Conversión Colones a dólares
replace ylmsec_ci= ylmsec_ci/8.76

******************
****ylnmsec_ci****
******************

gen food2=.
replace food2=r433e1_a*r433e2_v/12 
replace food2=0 if emp==1 & r433e1_a==.

gen ropa2=.
replace ropa2=r433f1_r*r433f2_v/12 
replace ropa2=0 if emp==1 & r433f1_r==.

gen merca2=.
replace merca2=r433g1_m*r433g2_v/12 
replace merca2=0 if emp==1 & r433g1_m==.

gen vivi2=.
replace vivi2=r433h1_v*r433h2_v/12 
replace vivi2=0 if emp==1 & r433h1_v==.

gen trans2=.
replace trans2=r433i1_t*r433i2_v/12 
replace trans2=0 if emp==1 & r433i1_t==.
replace trans2=. if r433i1_t==999999 | r433i2_v==999

gen segur2=.
replace segur2=r433j1_s*r433j2_v/12 
replace segur2=0 if emp==1 & r433j1_s==.

gen otross2=.
replace otross2=r433k1_o*r433k2_v/12 
replace otross2=0 if emp==1 & r433k1_o==.

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
replace ylnmsec_ci=. if food2==. &  ropa2==. & merca2==. & vivi2==. & trans2==. & segur2==. & otross2==. 
replace ylnmsec_ci=. if emp_ci==0
* Conversión Colones a dólares
replace ylnmsec_ci= ylnmsec_ci/8.76

**********************
***ylm_ci & ylm1_ci***
**********************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylm1_ci=rsum(ylmpri1_ci ylmsec1_ci), missing
replace ylm1_ci=. if ylmpri1_ci==. & ylmsec1_ci==.


************************
***ylnm_ci & ylnm1_ci***
************************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

gen ylnm1_ci=.
/*Como no ahy autoconsumo no tenemos este bloque*/


*************
***ynlm_ci***
*************

gen remesasext=.
replace remesasext=r439a1_r*r439a2_v/12 
replace remesasext=0 if emp==1 & r439a1_r==.
replace remesasext=. if r439a1_r==999999 | r439a2_v==999

gen ayuda=.
replace ayuda=r439b1_a*r439b2_v/12
replace ayuda=0 if emp==1 & r439b1_a==.
replace ayuda=. if r439b1_a==999999 | r439b2_v==999

gen cuotalim=.
replace cuotalim=r439c1_c*r439c2_v/12
replace cuotalim=0 if emp==1 & r439c1_c==.

gen alqui=.
replace alqui=r439d1_a*r439d2_v/12
replace alqui=0 if emp==1 & r439d1_a==.
replace alqui=. if r439d1_a==999999 | r439d2_v==999

gen alqneg=.
replace alqneg=r439e1_a*r439e2_v/12
replace alqneg=0 if emp==1 & r439e1_a==.
replace alqneg=. if r439e1_a==999999 | r439e2_v==999

gen alqter=.
replace alqter=r439f1_a*r439f2_v/12
replace alqter=0 if emp==1 & r439f1_a==.
replace alqter=. if r439f1_a==999999 | r439f2_v==999

gen jubil=.
replace jubil=r439g1_j*r439g2_v/12
replace jubil=0 if emp==1 & r439g1_j==.
replace jubil=. if r439g1_j==999999 | r439g2_v==999

gen deveh=.
replace deveh=r439h1_d*r439h2_v/12
replace deveh=0 if emp==1 & r439h1_d==.
replace deveh=. if r439h1_d==999999 | r439h2_v==999

gen otros=.
replace otros=r439i1_o*r439i2_v/12
replace otros=0 if emp==1 & r439i1_o==.
replace otros=. if r439i1_o==999999 | r439i2_v==999


gen utilidades=.
replace utilidades=r440a_ut/12
replace utilidades=0 if emp==1 & r440a_ut==.

gen dividendos=.
replace dividendos=r440b_di/12
replace dividendos=0 if emp==1 & r440b_di==.
replace dividendos=. if r440b_di==999999

gen intereses=.
replace intereses=r440c_in/12
replace intereses=0 if emp==1 & r440c_in==.
replace intereses=. if r440c_in==999999

gen herencias=.
replace herencias=r440d_he/12
replace herencias=0 if emp==1 & r440d_he==.

gen vtain=.
replace vtain=r440e_ve/12
replace vtain=0 if emp==1 & r440e_ve==.

gen indemnizacion=.
replace indemnizacion=r440f_in/12
replace indemnizacion=0 if emp==1 & r440f_in==.
replace indemnizacion=. if r440f_in==999999

gen ayudagob=.
replace ayudagob=r440g_ay/12
replace ayudagob=0 if emp==1 & r440g_ay==.

gen otross=.
replace otross=r440h_ot/12
replace otross=0 if emp==1 & r440h_ot==.
replace otross=. if r440h_ot==999999

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg alqter jubil deveh otros utilidades dividendos intereses herencias vtain indemnizacion ayudagob otross), missing
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & alqter==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & vtain==. & indemnizacion==. & ayudagob==.  & otross==. 
* Conversión Colones a dólares
replace ynlm_ci= ynlm_ci/8.76

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=remesasext
replace remesas_ci=. if remesasext==0

*Modificación Mayra Sáenz - Septiembre 2014 Conversión Colones a dólares
replace remesas_ci= remesas_ci/8.76
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

by idh_ch, sort: egen nrylmpri1_ch=sum(nrylmpri1_ci) if miembros_ci==1
replace nrylmpri1_ch=1 if nrylmpri1_ch>0 & nrylmpri1_ch<.
replace nrylmpri1_ch=. if nrylmpri1_ch==.


************************
*** ylm_ch & ylm1_ch ***
************************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1

****************************
*** ylmnr_ch & ylmnr1_ch ***
****************************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1
replace ylmnr1_ch=. if nrylmpri1_ch==1

**************************
*** ylnm_ch & ylnm1_ch ***
**************************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
gen ylnm1_ch=.

******************
*** remesas_ch ***
******************

gen remesash=.
replace remesash=r703_cua/12 if  r704_fre==1
replace remesash=r703_cua/3 if  r704_fre==2
replace remesash=r703_cua if  r704_fre==3
replace remesash=. if r703_cua==999999
* Conversión Colones a dólares
replace remesash= remesash/8.76

gen remesasnm=.
replace remesasnm=r706_cua/12
replace remesasnm=. if r706_cua==999999
* Conversión Colones a dólares
replace remesasnm= remesasnm/8.76

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash remesasnm)
replace remesas_ch=. if remesasi==. & remesash==. & remesasnm==.

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1
egen ynlm_ch=rsum(ynlm remesash)
replace ynlm_ch=. if ynlm==. & remesash==.
drop ynlm

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=remesasnm

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=.

*******************
*** rentaimp_ch ***
*******************

by idh_ch, sort: egen rentaimp_ch=sum(alqui) if miembros_ci==1, missing
* Conversión Colones a dólares
replace rentaimp_ch= rentaimp_ch/8.76

******************************
***ylhopri_ci & ylhopri1_ci***
******************************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

**************************
***ylmho_ci & ylm1ho_ci***
**************************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)


**************************
***EDUCATION INDICATORS***
**************************

/* Las variables r205_niv y r205_niv indican el nivel que cursan y el año 
alcanzado en dicho nivel, permiten calcular los años de educación para aquellos individuos
que actualmente estan estudiando. 
Las variables r220a_la y r220b_ul indican el último nivel alcanzado y el año 
alcanzado en dicho nivel, permiten calcular los años de educación para aquellos que
actualmente no asisten a un establecimiento escolar.
En El Salvador, la educación básica dura nueve años y la educación media tres años*/

gen byte aedu_ci=.

/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/

replace aedu_ci=0 if r205_niv==0 
replace aedu_ci=0 if r205_niv==1 | r205_niv==3

replace aedu_ci=1 if r205_niv==2 & r206a_gr==1
replace aedu_ci=2 if r205_niv==2 & r206a_gr==2
replace aedu_ci=3 if r205_niv==2 & r206a_gr==3
replace aedu_ci=4 if r205_niv==2 & r206a_gr==4
replace aedu_ci=5 if r205_niv==2 & r206a_gr==5
replace aedu_ci=6 if r205_niv==2 & r206a_gr==6
replace aedu_ci=7 if r205_niv==2 & r206a_gr==7
replace aedu_ci=8 if r205_niv==2 & r206a_gr==8
replace aedu_ci=9 if r205_niv==2 & r206a_gr==9

replace aedu_ci=1 if r205_niv==4 & r206a_gr==1
replace aedu_ci=2 if r205_niv==4 & r206a_gr==2
replace aedu_ci=3 if r205_niv==4 & r206a_gr==3
replace aedu_ci=4 if r205_niv==4 & r206a_gr==4
replace aedu_ci=5 if r205_niv==4 & r206a_gr==5
replace aedu_ci=6 if r205_niv==4 & r206a_gr==6
replace aedu_ci=7 if r205_niv==4 & r206a_gr==7
replace aedu_ci=8 if r205_niv==4 & r206a_gr==8
replace aedu_ci=9 if r205_niv==4 & r206a_gr==9

replace aedu_ci=10 if r205_niv==5 & r206a_gr==10
replace aedu_ci=11 if r205_niv==5 & r206a_gr==11
replace aedu_ci=12 if r205_niv==5 & r206a_gr==12

replace aedu_ci=13 if r205_niv==6 & r206a_gr==1
replace aedu_ci=14 if r205_niv==6 & r206a_gr==2
replace aedu_ci=15 if r205_niv==6 & r206a_gr==3
replace aedu_ci=16 if r205_niv==6 & r206a_gr==4
replace aedu_ci=17 if r205_niv==6 & r206a_gr==5
replace aedu_ci=18 if r205_niv==6 & (r206a_gr==6 | r206a_gr==7 | r206a_gr==11 | r206a_gr==12)

replace aedu_ci=13 if r205_niv==7 & r206a_gr==1
replace aedu_ci=14 if r205_niv==7 & r206a_gr==2
replace aedu_ci=15 if r205_niv==7 & r206a_gr==3
replace aedu_ci=16 if r205_niv==7 & r206a_gr==4

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0

/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if r220a_la==0 
replace aedu_ci=0 if r220a_la==1 | r220a_la==3

replace aedu_ci=1 if r220a_la==2 & r220b_ul==1
replace aedu_ci=2 if r220a_la==2 & r220b_ul==2
replace aedu_ci=3 if r220a_la==2 & r220b_ul==3
replace aedu_ci=4 if r220a_la==2 & r220b_ul==4
replace aedu_ci=5 if r220a_la==2 & r220b_ul==5
replace aedu_ci=6 if r220a_la==2 & r220b_ul==6
replace aedu_ci=7 if r220a_la==2 & r220b_ul==7
replace aedu_ci=8 if r220a_la==2 & r220b_ul==8
replace aedu_ci=9 if r220a_la==2 & r220b_ul==9

replace aedu_ci=1 if r220a_la==4 & r220b_ul==1
replace aedu_ci=2 if r220a_la==4 & r220b_ul==2
replace aedu_ci=3 if r220a_la==4 & r220b_ul==3
replace aedu_ci=4 if r220a_la==4 & r220b_ul==4
replace aedu_ci=5 if r220a_la==4 & r220b_ul==5
replace aedu_ci=6 if r220a_la==4 & r220b_ul==6
replace aedu_ci=7 if r220a_la==4 & r220b_ul==7
replace aedu_ci=8 if r220a_la==4 & r220b_ul==8
replace aedu_ci=9 if r220a_la==4 & r220b_ul==9

replace aedu_ci=10 if r220a_la==5 & r220b_ul==10
replace aedu_ci=11 if r220a_la==5 & r220b_ul==11
replace aedu_ci=12 if r220a_la==5 & (r220b_ul==12 | r220b_ul==13)

replace aedu_ci=13 if r220a_la==6 & r220b_ul==1
replace aedu_ci=14 if r220a_la==6 & r220b_ul==2
replace aedu_ci=15 if r220a_la==6 & r220b_ul==3
replace aedu_ci=16 if r220a_la==6 & r220b_ul==4
replace aedu_ci=17 if r220a_la==6 & (r220b_ul==5 | r220b_ul==6 | r220b_ul==7 | r220b_ul==10 | r220b_ul==12)

replace aedu_ci=13 if r220a_la==7 & r220b_ul==1
replace aedu_ci=14 if r220a_la==7 & r220b_ul==2
replace aedu_ci=15 if r220a_la==7 & r220b_ul==3
replace aedu_ci=16 if r220a_la==7 & r220b_ul==4
replace aedu_ci=17 if r220a_la==7 & r220b_ul==5

replace aedu_ci=0 if r204_est==2 & r219_asi==2
replace aedu_ci=. if edad<=3
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
replace edupre_ci=1 if r205_ni==0 | r205_ni==1 | r205_ni==3 | r220a_la==0 | r220a_la==1 | r220a_la==3 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (r205_niv==6 | r220a_la==6)& aedu_ci~=.
replace eduac_ci=0 if (r205_niv==7 | r220a_la==7) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if r204_est==1
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

recode r222_pq_ (99=.)
gen pqnoasis_ci=r222_pq_
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 
label define pqnoasis_ci 2 " Causas del hogar" 3 "Muy caro" 4 "Muy lejos" 5 "No existe otro grado", add 
label define pqnoasis_ci 6 "No vale la pena" 7 "Por enfermedad"  8 "Por embarazo" , add
label define pqnoasis_ci 9 "Considera que ya terminó sus estudios" 10 "No hay escuela nocturna" 11 "Por invalidez", add
label define pqnoasis_ci 12 "Rquiere educación especial" 13 "Los padres no quieren" 14 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if r222_pq_ ==3
replace pqnoasis1_ci = 2 if r222_pq_ ==1
replace pqnoasis1_ci = 3 if r222_pq_ ==11 | r222_pq_ ==12  | r222_pq_ ==13
replace pqnoasis1_ci = 4 if r222_pq_ ==6
replace pqnoasis1_ci = 5 if r222_pq_ ==2
replace pqnoasis1_ci = 6 if r222_pq_ ==9
replace pqnoasis1_ci = 8 if r222_pq_ ==4  | r222_pq_ ==5  | r222_pq_ ==10 
replace pqnoasis1_ci = 9 if r222_pq_ ==14

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

gen repiteult_ci=(r207_rep==1)
replace repiteult_ci=. if asiste_ci==0
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if r209_cen==1 
replace edupub_ci=2 if r209_cen==2 | r209_cen==3

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
gen tecnica_ci= (r205_niv==6 | r205_niv==7 | r220a_la==6 | r220a_la==7)
label var tecnica_ci "=1 formacion terciaria tecnica"	

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(r305b1_t==1 | r305b1_t==2)

gen aguadist_ch=1 if r305b1_t==1
replace aguadist_ch=2 if r305b1_t==2
replace aguadist_ch=3 if r305c1_a>=1 & r305c1_a<=8

gen aguamala_ch= (r305c1_a>=6 & r305c1_a<=9)
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=(r305a1_a==1 | r305a1_a==2)

gen luzmide_ch=.
/*NA*/

gen combust_ch=(r305a1_a>=1 & r305a1_a<=4)

gen bano_ch = ( r305i_sa>=1 & r305i_sa<=5) | (r305g_ti==1 | r305g_ti==2)

gen banoex_ch=(r305j_no>=1 & r305j_no<=3)

gen des1_ch=.
replace des1_ch = 0 if r305i_sa==6
replace des1_ch = 1 if r305i_sa==1 | r305i_sa==2
replace des1_ch = 2 if r305i_sa>=3 & r305i_sa<=5

gen des2_ch=.
replace des2_ch = 0 if r305i_sa==6
replace des2_ch = 1 if r305i_sa>=1 & r305i_sa<=5

gen piso_ch=0 if r302c_pi==5
replace piso_ch=1 if r302c_pi>=1 & r302c_pi<=4
replace piso_ch=2 if r302c_pi==6

gen pared_ch=0 if r302b_pa==2 | r302b_pa==3 | r302b_pa==6 | r302b_pa==7
replace pared_ch=1 if r302b_pa==1 | r302b_pa==4 | r302b_pa==5
replace pared_ch=2 if r302b_pa==8

gen techo_ch=0 if r302a_te==5 | r302a_te==6
replace techo_ch=1 if r302a_te>=1 & r302a_te<=4

gen resid_ch=0 if r305n1_b==1 | r305n1_b==2
replace resid_ch=1 if r305n1_b==4 | r305n1_b==5
replace resid_ch=2 if r305n1_b==6
replace resid_ch=3 if r305n1_b==3 | r305n1_b==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (r305b1_t >=1 & r305b1_t <=2) | r305c1_a == 1 | r305c1_a == 4 | r305c1_a == 8
replace aguamejorada_ch = 0 if (r305b1_t >=3 & r305b1_t <=4) | r305c1_a == 2 | r305c1_a == 3 | (r305c1_a >=5 & r305c1_a <=7) | r305c1_a == 9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (r305i_sa>=1 & r305i_sa <=4)
replace banomejorado_ch = 0 if  r305g_ti ==3 | r305i_sa ==5

gen dorm_ch=r303b_do
replace dorm_ch=. if r303b_do==9

gen cuartos_ch=r303a_ha
replace cuartos_ch=. if r303a_ha==99

gen cocina_ch=.
/*NA*/

gen telef_ch=(r305m1_t>=1 & r305m1_t<=3)

gen refrig_ch=(r306e1_r==1)
replace refrig_ch=. if r306e1_r==9

gen freez_ch=.
/*NA*/

gen auto_ch=(r306k1_v==1)
replace auto_ch=. if r306k1_v==9


gen compu_ch=(r306i1_c==1)
replace compu_ch=. if r306i1_c==9

gen internet_ch=.
/*NA*/

gen cel_ch=(r305m1_t==2 | r305m1_t==3)

gen vivi1_ch=1 if r301_tip==1
replace vivi1_ch=2 if r301_tip>=2 & r301_tip<=4
replace vivi1_ch=3 if r301_tip>=5 & r301_tip<=8

gen vivi2_ch=(r301_tip>=1 & r301_tip<=4)

gen viviprop_ch=0 if r304a_te==3
replace viviprop_ch=1 if r304a_te==1
replace viviprop_ch=2 if r304a_te==2
replace viviprop_ch=3 if r304a_te>=4 & r304a_te<=6


gen vivitit_ch=.
/*NA*/

gen vivialq_ch=r304e_cu if r304a_te==3
replace vivialq_ch=. if r304e_cu==99999
* Conversión Colones a dólares
replace vivialq_ch= vivialq_ch/8.76


gen vivialqimp_ch=r304d_si
replace vivialqimp_ch=. if r304d_si==99999
* Conversión Colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76

*variables que faltan generar
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen tipopen_ci=.
gen ylmotros_ci=.
gen categosec_ci=.
gen ylnmotros_ci=.

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


saveold "`base_out'", replace


log close



