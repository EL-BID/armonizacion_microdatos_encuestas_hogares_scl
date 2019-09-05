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
local ANO "1999"
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

gen region_c= r004
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

gen factor_ch=fac01
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
**************

sort folio tipo
egen idh_ch= group(folio tipo)
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

gen anio_c=1999
label variable anio_c "Anio de la encuesta"


*********
***mes***
*********

gen byte mes_c=r016m 
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril"
label define mes_c 5 "Mayo" 6 " Junio" 7 "Julio" 8 "Agosto", add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci = .
replace relacion_ci = 1 if r105==1
replace relacion_ci = 2 if r105==2
replace relacion_ci = 3 if r105==3
replace relacion_ci = 4 if r105>=4 & r105<=8
replace relacion_ci = 5 if r105==10 | r105==11
replace relacion_ci = 6 if r105==9

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

gen factor_ci=fac01
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=r106

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci
label var sexo_ci "Sexo del individuo"

**********
***edad***
**********

gen edad_ci=r108
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci = .
replace civil_ci = 1 if r109==6
replace civil_ci = 2 if r109==1 | r109==2
replace civil_ci = 3 if r109==4 | r109==5
replace civil_ci = 4 if r109==3

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
replace condocup_ci = 1 if r403==1 | r404==1 | r4050a==1 | r4050b==1 | r4050c==1 | r4050d==1 | r4050e==1 | r4050f==1 | r4050h==1 
replace condocup_ci = 2 if (r403==2 | r404==2) & r406==1 & r408<=8
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

gen desalent_ci = (emp_ci==0 & r407==1)

*****************
***horaspri_ci***
*****************

gen horaspri_ci = r411a if r403==1
replace horaspri_ci = r412a  if r403==2 & r404==1
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horassec_ci***
*****************

gen horassec_ci = r432 if r431==1 
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
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (r413 == 2 | r413 ==3)

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=0
replace tiempoparc_ci=1 if emp_ci==1 & horaspri_ci<=30 & r413 == 1

******************
***categopri_ci***
******************

gen categopri_ci  = . 
replace categopri_ci = 0 if r417==10
replace categopri_ci = 1 if r417==1
replace categopri_ci = 2 if r417==2 | r417==3 | r417==4
replace categopri_ci = 3 if r417==6 | r417==7 | r417==8 | r417==9 
replace categopri_ci = 4 if r417==5 

label define categopri_ci 0 "Otro" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & r431==1
replace nempleos_ci=. if pea_ci==0

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
*/

*****************
***spublico_ci***
*****************

gen spublico_ci=.
replace spublico_ci = 1 if r419==2
replace spublico_ci = 0 if r419==1

**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (r414>=211 & r414 <=369) & emp_ci==1
replace ocupa_ci=2 if (r414 >=111 & r414 <=131) & emp_ci==1
replace ocupa_ci=3 if (r414 >=411 & r414 <=422) & emp_ci==1
replace ocupa_ci=4 if ((r414 >=520 & r414 <=526) | r414 ==911) & emp_ci==1
replace ocupa_ci=5 if ((r414 >=511 & r414 <=516) | (r414 >=912 & r414 <=916)) & emp_ci==1
replace ocupa_ci=6 if ((r414 >=611 & r414 <=621) | (r414 >=921 & r414 <=922)) & emp_ci==1
replace ocupa_ci=7 if ((r414 >=711 & r414 <=834) | (r414 >=931 & r414 <=933)) & emp_ci==1
replace ocupa_ci=8 if r414 ==11 & emp_ci==1
replace ocupa_ci=. if emp_ci==0 | r414 ==999 | r414 ==0

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if (r416 >=11 & r416 <=50) & emp_ci==1
replace rama_ci=2 if (r416 >=101 & r416 <=142) & emp_ci==1
replace rama_ci=3 if (r416 >=151 & r416 <=372) & emp_ci==1
replace rama_ci=4 if (r416 >=401 & r416 <=410) & emp_ci==1
replace rama_ci=5 if (r416 >=451 & r416 <=455) & emp_ci==1
replace rama_ci=6 if (r416 >=501 & r416 <=552) & emp_ci==1 
replace rama_ci=7 if (r416 >=601 & r416 <=642) & emp_ci==1
replace rama_ci=8 if (r416 >=651 & r416 <=749) & emp_ci==1
replace rama_ci=9 if (r416 >=751 & r416 <=990) & emp_ci==1


****************
***durades_ci***
****************

gen durades_ci=r409/4.3  if r409>0
*replace durades_ci=. if r409==.
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
replace cotizando_ci=1 if (r421==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
replace tipocontrato_ci=1 if (r418==1 & r417==6) & categopri_ci==3
replace tipocontrato_ci=2 if (r418==1 & r417==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (r418==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************

recode r420 (98=.)

gen tamemp_ci=.
replace tamemp_ci=1 if r420>=1 & r420<=5
replace tamemp_ci=2 if r420>=6 & r420<=50
replace tamemp_ci=3 if r420>50 & r420<.

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de la empresa"

*************
**pension_ci*
*************
gen pension_ci=(r4420m >=1 & r4420m <999999)
replace pension_ci=. if r4420m==.

label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=r4420m*r4420n/12 if pension_ci==1
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
replace cesante_ci=1 if r410==1 & condocup_ci==2
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
replace categoinac_ci=1 if r407==8 & condocup_ci==3
replace categoinac_ci=2 if (r407==4 | r407==10) & condocup_ci==3
replace categoinac_ci=3 if r407==7 & condocup_ci==3
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
1999-Jan	8.76
1999-Feb	8.76
1999-Mar	8.76
1999-Apr	8.76
1999-May	8.76
1999-Jun	8.76
1999-Jul	8.76
1999-Aug	8.76
1999-Sep	8.76
1999-Oct	8.76
1999-Nov	8.76
1999-Dec	8.76
*/

****************************
***ylmpri_ci & ylmpri1_ci***
****************************

/*Para los trabajadores dependientes*/

gen yprid=.
replace yprid=r423*30 if r422==1
replace yprid=r423*4.3 if r422==2
replace yprid=r423*2 if r422==3
replace yprid=r423 if r422==4
replace yprid=r423 if r422==5
replace yprid=0 if emp_ci==1 & r423==.

gen hrsextrasd=.
replace hrsextrasd=r4240a*r4240b/12

gen vacacionesd=.
replace vacacionesd=r4240c*r4240d/12

gen aguinaldod=.
replace aguinaldod=r4240e*r4240f /12

gen bonificacionesd=.
replace bonificacionesd=r4240g*r4240h/12

egen yprijbd=rsum( yprid hrsextrasd vacacionesd aguinaldod bonificacionesd), missing
replace yprijbd=. if emp_ci==0
replace yprijbd=. if yprid==. & hrsextrasd==. & vacacionesd==. & aguinaldod==. & bonificacionesd==.

/*Para los trabajadores independientes*/

gen ingrneto=r427-r428
replace ingrneto=0 if ingrneto<0 /*Son 6 observaciones*/

gen yprijbi=.
replace yprijbi=ingrneto*30 if r426==1
replace yprijbi=ingrneto*4.3 if r426==2
replace yprijbi=ingrneto*2 if r426==3
replace yprijbi=ingrneto if r426==4
replace yprijbi=ingrneto/12 if r426==5
replace yprijbi=ingrneto if r426==6
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
* Conversión de colones a dolares
replace ylmpri_ci= ylmpri_ci/8.76

egen ylmpri1_ci=rsum(yprijbi yprijbd), missing
replace ylmpri1_ci=. if yprijbi==999999 | yprijbd==999999
replace ylmpri1_ci=. if yprijbd==. & yprijbi==.
replace ylmpri1_ci=. if emp==0
* Conversión de colones a dolares
replace ylmpri1_ci =ylmpri1_ci/8.76

********************************
***nrylmpri_ci & nrylmpri1_ci***
********************************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)

gen nrylmpri1_ci=(ylmpri1_ci==. & emp_ci==1)


*******************************
*** ylnmpri_ci & ylnmpri1_ci***
*******************************

gen food1=.
replace food1=r4240i*r4240j/12 
replace food1=0 if emp==1 & r4240i==.

gen ropa1=.
replace ropa1=r4240k*r4240l/12 
replace ropa1=0 if emp==1 & r4240k==.

gen merca1=.
replace merca1=r4240m*r4240n/12 
replace merca1=0 if emp==1 & r4240m==.

gen vivi1=.
replace vivi1=r4240o*r4240p/12 
replace vivi1=0 if emp==1 & r4240o==.

gen trans1=.
replace trans1=r4240q*r4240r/12 
replace trans1=0 if emp==1 & r4240q==.

gen segur1=.
replace segur1=r4240s*r4240t/12 
replace segur1=0 if emp==1 & r4240s==.

gen otross1=.
replace otross1=r4240u*r4240v/12 
replace otross1=0 if emp==1 & r4240u==.

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if food1==. &  ropa1==. & merca1==. & vivi1==. & trans1==. & segur1==. & otross1==. 
replace ylnmpri_ci=. if emp_ci==0
* Conversión de colones a dolares
replace ylnmpri_ci= ylnmpri_ci/8.76


* Ingreso laboral no monetario de la actividad principal 1 (SUMAMOS AUTOCONSUMO PARA LOS INDEPENDIENTES)

rename r430 valaut

egen ylnmpri1_ci=rsum(ylnmpri_ci valaut), missing
replace ylnmpri1_ci=. if ylnmpri_ci==. & valaut==.
replace ylnmpri1_ci=. if emp_ci==0
* Conversión de colones a dolares
replace ylnmpri1_ci=ylnmpri1_ci/8.76

***************
***ylmsec_ci***
***************

gen ysec1=r433

gen hrsextras=.
replace hrsextrasd=r4340a*r4340b/12

gen vacaciones=.
replace vacacionesd=r4340c*r4340d/12

gen aguinaldo=.
replace aguinaldod=r4340e*r4340f /12

gen bonificaciones=.
replace bonificacionesd=r4340g*r4340h/12

gen ylmsec_ci=ysec1
* Conversión de colones a dolares
replace ylmsec_ci=ylmsec_ci/8.76

egen ylmsec1_ci=rsum(ysec1 hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if ysec1==. & hrsextras==. & vacaciones==. & aguinaldo==. & bonificaciones==. 
replace ylmsec1_ci=. if emp_ci==0 | r431 ==2
* Conversión de colones a dolares
replace ylmsec1_ci=ylmsec1_ci/8.76

******************
****ylnmsec_ci****
******************

gen food2=.
replace food2=r4340i*r4340j/12 
replace food2=0 if emp==1 & r4340i==.

gen ropa2=.
replace ropa2=r4340k*r4340l/12 
replace ropa2=0 if emp==1 & r4340k==.

gen merca2=.
replace merca2=r4340m*r4340n/12 
replace merca2=0 if emp==1 & r4340m==.

gen vivi2=.
replace vivi2=r4340o*r4340p/12 
replace vivi2=0 if emp==1 & r4340o==.

gen trans2=.
replace trans2=r4340q*r4340r/12 
replace trans2=0 if emp==1 & r4340q==.

gen segur2=.
replace segur2= r4340s* r4340t/12 
replace segur2=0 if emp==1 & r4340s==.

gen otross2=.
replace otross2=r4340u*r4340v/12 
replace otross2=0 if emp==1 & r4340u==.

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
replace ylnmsec_ci=. if food2==. &  ropa2==. & merca2==. & vivi2==. & trans2==. & segur2==. & otross2==. 
replace ylnmsec_ci=. if emp_ci==0
* Conversión de colones a dolares
replace ylnmsec_ci=ylnmsec_ci/8.76

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

egen ylnm1_ci=rsum(ylnmpri1_ci ylnmsec_ci), missing
replace ylnm1_ci=. if ylnmpri1_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

gen remesasext=.
replace remesasext=r4420a*r4420b/12

gen ayuda=.
replace ayuda=r4420c*r4420d/12

gen cuotalim=.
replace cuotalim=r4420e* r4420f/12

gen alqui=.
replace alqui=r4420g*r4420h/12

gen alqneg=.
replace alqneg=r4420i*r4420j/12

gen jubil=.
replace jubil=r4420m*r4420n/12

gen deveh=.
replace deveh=r4420o*r4420p/12

gen otros=.
replace otros=r4420q*r4420r/12

gen utilidades=.
replace utilidades=r4430a/12

gen dividendos=.
replace dividendos=r4430b/12

gen intereses=.
replace intereses=r4430c/12

gen herencias=.
replace herencias=r4430d/12

gen indemnizacion=.
replace indemnizacion=r4430e/12

gen ayudagob=.
replace ayudagob=r4430f/12

gen otross=.
replace otross=r4430g/12

egen ynlm_ci=rsum(remesasext ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob otross), missing
replace ynlm_ci=. if remesasext==. & ayuda==. & cuotalim==. & alqui==. & alqneg==. & jubil==. & deveh==. & otros==. & utilidades==. & dividendos==. & intereses==. & herencias==. & indemnizacion==. & ayudagob==. & otross==. 
* Conversión de colones a dolares
replace ynlm_ci=ynlm_ci/8.76

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=remesasext
* Conversión de colones a dolares
replace remesas_ci=remesas_ci/8.76

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

gen autocons_ci=valaut

* Conversión de colones a dolares
replace autocons_ci= autocons_ci/8.76

*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=r311
* Conversión de colones a dolares
replace rentaimp_ch= rentaimp_ch/8.76 

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)

**************
***ylmho_ci***
**************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

****************************
***VARIABLES DE EDUCACION***
****************************


gen byte aedu_ci=.

/* Primero obtenemos los años de educacion para aquellos que 
actualmente están estudiando, no consideramos aquellos que tienen
educacion especial*/

replace aedu_ci=0 if r204==0 
replace aedu_ci=0 if r204==1 

replace aedu_ci=1 if r204==2 & r205==1 
replace aedu_ci=2 if r204==2 & r205==2 
replace aedu_ci=3 if r204==2 & r205==3 
replace aedu_ci=4 if r204==2 & r205==4 
replace aedu_ci=5 if r204==2 & r205==5 
replace aedu_ci=6 if r204==2 & r205==6 
replace aedu_ci=7 if r204==2 & r205==7 
replace aedu_ci=8 if r204==2 & r205==8 
replace aedu_ci=9 if r204==2 & r205==9 

replace aedu_ci=10 if r204==3 & r205==10 
replace aedu_ci=11 if r204==3 & r205==11 
replace aedu_ci=12 if r204==3 & r205==12 

replace aedu_ci=13 if r204==4 & r205==1 
replace aedu_ci=14 if r204==4 & r205==2 
replace aedu_ci=15 if r204==4 & r205==3 
replace aedu_ci=16 if r204==4 & r205==4 
replace aedu_ci=17 if r204==4 & r205==5 
replace aedu_ci=18 if r204==4 & r205==6 
replace aedu_ci=19 if r204==4 & r205==7
replace aedu_ci=20 if r204==4 & r205==8  
replace aedu_ci=22 if r204==4 & r205==10 
replace aedu_ci=23 if r204==4 & r205==11 
replace aedu_ci=24 if r204==4 & r205==12
replace aedu_ci=25 if r204==4 & r205==13 
replace aedu_ci=25 if r204==4 & r205==14 
replace aedu_ci=27 if r204==4 & r205==15 

replace aedu_ci=13 if r204==5 & r205==1 
replace aedu_ci=14 if r204==5 & r205==2 
replace aedu_ci=15 if r204==5 & r205==3 

* MGR Aug, 2015: se resta 1 a los que asisten ya que pregunta se hace sobre grado o curso que estudia actualmente, no el que ya completó
replace aedu_ci=aedu_ci-1 if aedu_ci!=0


/* Ahora obtenemos los años de educación para aquellos que
actualmente no asisten a un establecimiento educativo, no se tiene en
cuenta la educación especial*/

replace aedu_ci=0 if r219a==0 & r217==1
replace aedu_ci=0 if r219a==1 & r217==1

replace aedu_ci=1 if r219a==2 & r219b==1 & r217==1
replace aedu_ci=2 if r219a==2 & r219b==2 & r217==1
replace aedu_ci=3 if r219a==2 & r219b==3 & r217==1
replace aedu_ci=4 if r219a==2 & r219b==4 & r217==1
replace aedu_ci=5 if r219a==2 & r219b==5 & r217==1
replace aedu_ci=6 if r219a==2 & r219b==6 & r217==1
replace aedu_ci=7 if r219a==2 & r219b==7 & r217==1
replace aedu_ci=8 if r219a==2 & r219b==8 & r217==1
replace aedu_ci=9 if r219a==2 & r219b==9 & r217==1
replace aedu_ci=9 if r219a==2 & r219b==10 & r217==1

replace aedu_ci=10 if r219a==3 & r219b==10 & r217==1 
replace aedu_ci=11 if r219a==3 & r219b==11 & r217==1
replace aedu_ci=12 if r219a==3 & r219b==12 & r217==1

replace aedu_ci=13 if r219a==4 & r219b==1 & r217==1
replace aedu_ci=14 if r219a==4 & r219b==2 & r217==1
replace aedu_ci=15 if r219a==4 & r219b==3 & r217==1
replace aedu_ci=16 if r219a==4 & r219b==4 & r217==1
replace aedu_ci=17 if r219a==4 & r219b==5 & r217==1
replace aedu_ci=18 if r219a==4 & r219b==6 & r217==1
replace aedu_ci=19 if r219a==4 & r219b==7 & r217==1
replace aedu_ci=20 if r219a==4 & r219b==8 & r217==1
replace aedu_ci=21 if r219a==4 & r219b==9 & r217==1
replace aedu_ci=22 if r219a==4 & r219b==10 & r217==1
replace aedu_ci=23 if r219a==4 & r219b==11 & r217==1
replace aedu_ci=24 if r219a==4 & r219b==12 & r217==1
replace aedu_ci=25 if r219a==4 & r219b==13 & r217==1
replace aedu_ci=26 if r219a==4 & r219b==14 & r217==1
replace aedu_ci=27 if r219a==4 & r219b==15 & r217==1


replace aedu_ci=13 if r219a==5 & r219b==1 & r217==1
replace aedu_ci=14 if r219a==5 & r219b==2 & r217==1
replace aedu_ci=15 if r219a==5 & r219b==3 & r217==1

replace aedu_ci=0 if r203==2 & r217==2
replace aedu_ci=. if edad<=3

replace aedu_ci=. if aedu_ci>edad & aedu_ci~=. 
/*Hay 3 casos en donde los años de educación son mayores a la edad por eso lo ajustamos de 
esta forma*/

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
replace edupre_ci=1 if r219a==1 | r204==1 
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (r219a==4 | r204==4) & aedu_ci~=.
replace eduac_ci=0 if (r219a==5 | r204==5) & aedu_ci~=.
label variable eduac_ci "Superior universitario vs superior no universitario"

*************
***tecnica_ci**
*************
gen tecnica_ci=(r219a==5 | r204==5)
label var tecnica_ci "=1 formacion terciaria tecnica"	


***************
***asiste_ci***
***************

gen asiste_ci=0
replace asiste_ci=1 if r203==1
label variable asiste_ci "Asiste actualmente a la escuela"


*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=r221
label variable pqnoasis_ci "Reasons  for not attending school"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 " Enfermedad", add 
label define pqnoasis_ci 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis_ci 7 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if r221 ==3
replace pqnoasis1_ci = 2 if r221 ==1
replace pqnoasis1_ci = 3 if r221 ==4 | r221 ==5
replace pqnoasis1_ci = 5 if r221 ==2
replace pqnoasis1_ci = 7 if r221 ==6
replace pqnoasis1_ci = 9 if r221 ==7

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
gen repite_ci=(r207==1)
replace repite_ci=. if r207==.

/*NA*/

******************
***repiteult_ci***
******************

gen repiteult_ci=(r207==1)
replace repiteult_ci=. if asiste_ci==0
replace repiteult_ci=. if r207==0
label variable repiteult_ci "Esta repitiendo el ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if r210==1 
replace edupub_ci=2 if r210==2 | r210==3



/* Variable centroen:
1: Centro de enseñanza oficial: 
Es aquel cuya administración y funcionamiento depende del gobierno.
2: Centro de Enseñanza Laico: 
Son todos los centros educativos privados no religiosos. 
3: Centro de Enseñanza religioso: 
Son todos los centros educativos que pertenecen a una Congregación Religiosa. 
*/


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************


gen aguared_ch=(r313==1 | r313==2)

gen aguadist_ch=1 if r313==1
replace aguadist_ch=2 if r313==2
replace aguadist_ch=3 if r313>=2 & r313<=9

gen aguamala_ch=(r314==2)
replace aguamala_ch=. if r314==9

gen aguamide_ch=.
/*NA*/

gen luz_ch=(r312==1)
replace luz_ch=. if r312==9

gen luzmide_ch=.
/*NA*/

gen combust_ch=(r312>=1 & r312<=2)
replace combust_ch=. if r312==9

gen bano_ch=(r317>=1 & r317<=6)

gen banoex_ch=(r317>=4 & r317<=6)

* MGR Jul, 2015: variable generada como missing ya que no tenemos opción 3, pero genero de la misma manera que los años anteriores
gen des1_ch=.
replace des1_ch = 0 if r317==7
replace des1_ch = 1 if r317>=1 & r317<=2
replace des1_ch = 2 if r317>=3 & r317<=6

* MGR Jul, 2015: variable generada como missing ya que no tenemos opción 2, pero genero de la misma manera que los años anteriores
gen des2_ch=.
replace des2_ch = 0 if r317==7
replace des2_ch = 1 if des1_ch==1 | des1_ch==2

gen piso_ch=0 if r304==4
replace piso_ch=1 if r304>=1 & r304<=3
replace piso_ch=2 if r304==5

gen pared_ch=0 if r303==2 | r303==3 | r303==6 | r303==7
replace pared_ch=1 if r303==1 | r303==4 | r303==5
replace pared_ch=2 if r303==8

gen techo_ch=0 if r302==5 | r302==6
replace techo_ch=1 if r302>=1 & r302<=4
replace techo_ch=2 if r302==7

gen resid_ch=0 if r322==1 | r322==2
replace resid_ch=1 if r322==4 | r322==5
replace resid_ch=2 if r322==6
replace resid_ch=3 if r322==3 | r322==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (r313 >=1 & r313 <=3) | r313 == 6 
replace aguamejorada_ch = 0 if (r313 >=4 & r313 <=5) | (r313 >=7 & r313 <=9)

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (r317>=1 & r317 <=3)
replace banomejorado_ch = 0 if (r317>=4 & r317 <=7)

gen dorm_ch=r306

gen cuartos_ch=r305

gen cocina_ch=.
/*NA*/

gen telef_ch=(r321>=1 & r321<=4)
replace telef_ch=. if r321==9

gen refrig_ch=(r32305==1)
replace refrig_ch=. if r32305==9

gen freez_ch=.
/*NA*/

gen auto_ch=(r32311==1)
replace auto_ch=. if r32311==9


gen compu_ch=(r32309==1)
replace compu_ch=. if r32309==9

gen internet_ch=.
/*NA*/

gen cel_ch=(r321>=2 & r321<=4)
replace cel_ch=. if r321==9


gen vivi1_ch=1 if r301==1
replace vivi1_ch=2 if r301==2
replace vivi1_ch=3 if r301>2 & r301<9

gen vivi2_ch=(r301>=1 & r301<=2)
replace vivi2_ch=. if r301==9

gen viviprop_ch=0 if r308a==1
replace viviprop_ch=1 if r308a==3
replace viviprop_ch=2 if r308a==2
replace viviprop_ch=3 if r308a>=4 & r308a<=6


gen viviitit_ch=.
/*NA*/

gen vivialq_ch=r308c if r308a==1
replace vivialq_ch=. if r308c==99999
*Conversión Colones a dólares
replace vivialq_ch= vivialq_ch/8.76

gen vivialqimp_ch=r311
replace vivialqimp_ch=. if r311==99999
* Conversión Colones a dólares
replace vivialqimp_ch= vivialqimp_ch/8.76

*variables que faltan generar
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen tipopen_ci=.
gen ylmotros_ci=.
gen ylnmotros_ci=.
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



