
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
 


*global ruta = "${surveysFolder}"

local PAIS CRI
local ENCUESTA EHPM
local ANO "2004"
local ronda m7 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Costa Rica
Encuesta: EHPM
Round: m7
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 1 de octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


*2004*

qui destring _all, replace

************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=1 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


************
* Region_c *
************
*Inclusión Mayra Sáenz - Abril 2014
gen region_c=provincia

label define region_c  ///
1	"San José" ///
2	"Alajuela" ///
3	"Cartago" ///
4	"Heredia" ///
5	"Guanacaste" ///
6	"Puntarenas" ///
7	"Limón" 
      
           
label value region_c region_c
label var region_c "División política, provincias"


***************
***factor_ch***
***************

gen factor_ch=factor
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************

sort consecu vivienda hogar
egen idh_ch = group(consecu vivienda hogar)
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=linea_b
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if zona==2
replace zona_c=1 if zona==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


************
****pais****
************

gen str3 pais_c="CRI"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2004
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=7

label variable mes_c "Mes de la encuesta"
label define mes_c 7 "Julio" 
label value mes_c mes_c


*****************
***relacion_ci***
*****************

gen relacion_ci=1 if b03==1
replace relacion_ci=2 if b03==2
replace relacion_ci=3 if b03==3
replace relacion_ci=4 if (b03>=4 & b03<=7) | b03==11
replace relacion_ci=5 if b03==9 | b03==10
replace relacion_ci=6 if b03==8

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

gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=b04

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=b05
replace edad_ci=. if b05==98 | b05==99 /*Casos ignorados*/
label variable edad_ci "Edad del individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
*En este año  no se dispone de esta variable.
gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo"  

*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if b13==6
replace civil_ci=2 if b13==1 | b13==2
replace civil_ci=3 if b13==3| b13==4
replace civil_ci=4 if b13==5

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


**************
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


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci = .
replace lp_ci =     47134 if zona_c==1
replace lp_ci =     35919 if zona_c==0

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = .
replace lpe_ci =     18869 if zona_c==1
replace lpe_ci =     15654 if zona_c==0

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=1 if (b07>=1 & b07<=3) | b073==1

recode cotizando_ci .=0 
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
replace tipopen_ci=b072
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=b071
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact>=2 & condact<=7
recode condocup_ci .=3 if condact>=8 & condact<=14 & edad_ci>=12
recode condocup_ci .=4 if  condact==0 | edad_ci<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 12" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
*MGD 12/4/2015: se condiciona a que este desocupado
gen cesante_ci=1 if c13==1 & condocup_ci==2
recode cesante_ci .=0 if condocup_ci==2
* No todos los desempleados respondieron si han trabajado antes
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************
gen aux_p= c35a if c35a1==5

replace aux_p = c35a*4 if c35a1==3
replace aux_p = c35a*2 if c35a1==4
replace aux_p = c35a/4 if c35a1==6
replace aux_p = c35a/6 if c35a1==7
replace aux_p = c35a/12 if c35a1==8
replace aux_p = . if c35a==9999999

gen pension_ci=1 if (aux_p>0 & aux_p!=.) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen aux_ps= c35b if c35b1==5

replace aux_ps = c35b*4 if c35b1==3
replace aux_ps = c35b*2 if c35b1==4
replace aux_ps = c35b/4 if c35b1==6
replace aux_ps = c35b/6 if c35b1==7
replace aux_ps = c35b/12 if c35b1==8
replace aux_ps = . if c35b==9999999


gen byte pensionsub_ci= 1 if aux_ps>0 & aux_ps!=.
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring aux_ps, replace
gen  ypensub_ci=aux_ps
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**salmm_ci***
*************

* 2004 junio (fuente OIT-CEPAL observatorio de la crisis) 	89381.00
* 2015 MGD: se cambia por dato oficial publicado por el M. de Trabajo

gen salmm_ci= 95004
label var salmm_ci "Salario minimo legal"

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"


*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & (c10==4 | c10==5))
*4. no cree poder encontrar, 5. espera periodo de mayor actividad*

*****************
***horaspri_ci***
*****************

gen horaspri_ci=c22a 
replace horaspri_ci=. if c22a==99
replace horaspri_ci=. if emp_ci==0


*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci c22b), missing 
replace horastot_ci=. if horaspri_ci==. & c22b==.
replace horastot_ci=horaspri_ci if c22b==99 & horaspri_ci~=.
replace horastot_ci=. if horaspri_ci==.
replace horastot_ci=. if emp_ci==0


***************
***subemp_ci***
***************
gen subemp_ci=(horaspri_ci<=30 & c23==1) & emp_ci==1

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(horastot_ci<=30 & c23==2)
replace tiempoparc_ci=. if emp_ci==0


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if c18==1
replace categopri_ci=2 if c18==2 
replace categopri_ci=3 if c18==3 | c18==4 |c18==5 
replace categopri_ci=4 if c18==6 
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"



******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if c29==1
replace categosec_ci=2 if c29==2 
replace categosec_ci=3 if c29==3 | c29==4 |c29==5 
replace categosec_ci=4 if c29==6 
replace categosec_ci=. if emp_ci==0

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

*MGD 06/13/2014
* Alternativa con p25. ¿El trabajo que realizó la semana pasada en su ocupación principal...(todo el anio, ocasional/estacional, otro)
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if c25==1 & categopri_ci==3
replace tipocontrato_ci=2 if (c25==2 | c25==3 | c25==4 | c25==9) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & c21==2
replace nempleos_ci=2 if emp_ci==1 & c21==1
replace nempleos_ci=. if emp_ci==0
/*
*****************
***firmapeq_ci***
*****************

destring c20, replace

gen firmapeq_ci=.
replace firmapeq_ci=1 if c20<=5
replace firmapeq_ci=0 if c20>5 & c20<99
replace firmapeq_ci=. if emp_ci==0
*/

*****************
***spublico_ci***
*****************
destring c16, replace

gen spublico_ci=(c16>=100 & c16<=500) 
replace spublico_ci=. if emp==0 | c16==999 |c16==0


**************
***ocupa_ci***
**************
destring c15, replace

gen ocupa_ci=.
replace ocupa_ci=1 if c15>=2111 & c15<=3492 & emp_ci==1
replace ocupa_ci=2 if c15>=1111 & c15<=1133 & emp_ci==1
replace ocupa_ci=3 if c15>=4111 & c15<=4224 & emp_ci==1
replace ocupa_ci=4 if ((c15>=5210 & c15<=5222) | (c15>=9111 & c15<=9115)) & emp_ci==1
replace ocupa_ci=5 if ((c15>=5110 & c15<=5169) | (c15>=9120 & c15<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((c15>=6111 & c15<=6210) | (c15>=9211 & c15<=9233)) & emp_ci==1
replace ocupa_ci=7 if ((c15>=7111 & c15<=8330) | (c15>=9311 & c15<=9334)) & emp_ci==1
replace ocupa_ci=9 if c15>=9800 & emp_ci==1
replace ocupa_ci=. if emp_ci==0


label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************
destring c17, replace

gen rama_ci=.
replace rama_ci=1 if (c17>=1110 & c17<=5002) & emp_ci==1
replace rama_ci=2 if (c17>=10101 & c17<=14292) &  emp_ci==1
replace rama_ci=3 if (c17>=15111 & c17<=37203) & emp_ci==1
replace rama_ci=4 if (c17>=40100 & c17<=41000) & emp_ci==1
replace rama_ci=5 if (c17>=45100 & c17<=45500) & emp_ci==1
replace rama_ci=6 if (c17>=50101 & c17<=55300) & emp_ci==1
replace rama_ci=7 if (c17>=60100 & c17<=64203) & emp_ci==1
replace rama_ci=8 if (c17>=65110 & c17<=70200) & emp_ci==1
replace rama_ci=9 if (c17>=71111 & c17<=99999) & emp_ci==1



****************
***durades_ci***
****************

/*Esta variable no se puede estandarizar y llevar a meses como en las otra encuestas
Genero una variable alternativa que esta estandarizada para Costa Rica nada mas*/
/*
gen durades_ci=.

gen durades1_ci=1 if c07>=1 & c07<=2
replace durades1_ci=2 if c07==3
replace durades1_ci=3 if c07==4
replace durades1_ci=4 if c07==5
replace durades1_ci=. if c07==9
replace durades1_ci=. if emp_ci==1

label variable durades1_ci "Duracion del desempleo"
label define durades1_ci 1 "menos de 2 meses"
label define durades1_ci 2 "2 a menos de 4 meses", add
label define durades1_ci 3 "4 a menos de 12 meses", add
label define durades1_ci 4 "1 anio o mas", add
label values durades1_ci durades1_ci
*/

/* 
Modificado Mayra Sáenz - Julio 2013
Se homologa esta variable de acuerdo al resto de años.
           c07:
           1 menos de 1 mes
           2 de 1 a menos de 2 meses
           3 de 2 a menos de 4 meses
           4 de 4 a menos de 1 año
           5 de 1 año o más
           9 ignorado
*/

gen durades_ci=.
*menos de un mes
replace durades_ci=(1+4.3)/2/4.3 if c07==1
*de 1 a menos de 2 meses
replace durades_ci=(1+2)/2 if c07==2
*de 2 a menos de 4 meses
replace durades_ci=(2+4)/2 if c07==3
*de 4 a menos de 1 año
replace durades_ci=(4+12)/2 if c07==4
*de 1 año o más
replace durades_ci=(12+24)/2 if c07==5

label variable durades_ci "Duracion del desempleo en meses"


 
*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.
*******************
***tamemp_ci***
*******************
         
*Costa Rica Pequeña 1 a 5, Mediana 6 a 19, Grande Más de 19
/*
gen tamemp_ci = 1 if (c20>=1 & c20<=5)
replace tamemp_ci = 2 if (c20>=6 & c20<=19)
replace tamemp_ci = 3 if (c20>19)

*/
* 2014, 06 modificacion MLO
gen tamemp_ci = 1 if (c20>=1 & c20<=5)
replace tamemp_ci = 2 if (c20>=6 & c20<=10)
replace tamemp_ci = 3 if (c20>10 & c20!=.)
replace tamemp_ci = . if (c20==99)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************
/*
c08:
           0 no aplic
           1 pensiona
           2 rentista
           3 estudian
           4 labores 
           5 incapaci
           6 otro ina
*/


gen categoinac_ci = 1 if (c08 == 1 & condocup_ci==3)
replace categoinac_ci = 2 if (c08 == 3 & condocup_ci==3)
replace categoinac_ci = 3 if (c08 == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

gen byte formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************


gen ylmpri_ci=ingprinci
replace ylmpri_ci=. if ingprinci==99999999
replace ylmpri_ci=. if emp_ci==0
*Modificación Mayra Sáenz - Mayo 2014
replace ylmpri_ci=. if ingtothog==99999999
replace ylmpri_ci=. if ingtothog==.
replace ylmpri_ci=0 if ingtothog==0

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ingprinc==99999999)
replace nrylmpri_ci=. if emp_ci==0 

***************
***ylmsec_ci***
***************

gen ylmsec_ci=ingprisec
replace ylmsec_ci=. if  ingprisec==99999999
replace ylmsec_ci=. if emp_ci==0 | nempleos_ci==1
*Modificación Mayra Sáenz - Mayo 2014
replace ylmsec_ci=. if ingtothog==99999999
replace ylmsec_ci=0 if ingtothog==0
replace ylmsec_ci=. if ingtothog==.

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
***N/A***
gen ylnmpri_ci=.
gen ylnmsec_ci=.
gen ylnm_ci=.
gen ynlnm_ci=.
gen autocons_ci=.
gen remesas_ci=.

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*************
***ynlm_ci***
*************

gen ynlm_ci=otrosingme
replace ynlm_ci=. if otrosingme==99999999
*Modificación Mayra Sáenz - Mayo 2014
replace ynlm_ci=. if ingtothog==99999999
replace ynlm_ci=0 if ingtothog==0
replace ynlm_ci=. if ingtothog==.


************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************

*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

* N/A*
gen ylnm_ch=.
gen ynlnm_ch=.
gen rentaimp_ch=.
gen autocons_ch=.
gen remesas_ch=.

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

*****************************************************************
**YLMOTROS_CI : Ingreso laboral monetario otros trabajos.    ***
*********************************************************************

gen ylmotros_ci=.


*********************************************************************
***  YLNMOTROS_CI : Ingreso laboral no monetario otros trabajos.
*********************************************************************

gen ylnmotros_ci=.



****************************
***VARIABLES DE EDUCACION***
****************************

gen byte aedu_ci=.

replace aedu_ci=0 if b08==0 | b08==1 

*Primaria
replace aedu_ci=1 if b08==11 
replace aedu_ci=2 if b08==12
replace aedu_ci=3 if b08==13
replace aedu_ci=4 if b08==14
replace aedu_ci=5 if b08==15
replace aedu_ci=6 if b08==16

*Secundaria (académica y técnica)
replace aedu_ci=7 if b08==21 | b08==31
replace aedu_ci=8 if b08==22 | b08==32
replace aedu_ci=9 if b08==23 | b08==33
replace aedu_ci=10 if b08==24 | b08==34
replace aedu_ci=11 if b08==25 | b08==35
replace aedu_ci=12 if b08==36 

*Superior (universitario o para-universitario)
replace aedu_ci=13 if b08==41 | b08==51
replace aedu_ci=14 if b08==42 | b08==52
replace aedu_ci=15 if b08==43 | b08==53
replace aedu_ci=16 if b08==44 | b08==54
replace aedu_ci=17 if b08==55
replace aedu_ci=18 if b08==56
replace aedu_ci=19 if b08==57
replace aedu_ci=20 if b08==58

**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if b08==0 | b08==1 
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if (b08>=11 & b08<16) 
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if b08==16
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (b08>=21 & b08<=25) 
replace edusi_ci=1 if (b08>=31 & b08<=35) 
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if b08==36
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if (b08>=21 & b08<=22)
replace edus1i_ci=1 if (b08>=31 & b08<=32)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if b08==23 | b08==33
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if (b08>=24 & b08<=25)
replace edus2i_ci=1 if (b08>=34 & b08<=35)
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if b08==36
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if (b08>=41 & b08<=44)
replace eduui_ci=1 if (b08>=51 & b08<=54)
label variable eduui_ci "Superior incompleto"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (b08>=55 & b08<=58)
label variable eduuc_ci "Superior completo"



local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}


***************
***edupre_ci***
***************

gen byte edupre_ci=.
replace edupre_ci=(b08==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=0 if (b08>=41 & b08<=49) 
replace eduac_ci=1 if (b08>=51 & b08<=59) 
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if b09>=1 & b09<=7
replace asiste_ci=0 if b09==8
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=b11

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

destring b11, replace
g       pqnoasis1_ci = 1 if b11==3
replace pqnoasis1_ci = 2 if b11==1
replace pqnoasis1_ci = 3 if b11==6 | b11==7
replace pqnoasis1_ci = 4 if b11==8 | b11==11
replace pqnoasis1_ci = 5 if b11==2 | b11==5
replace pqnoasis1_ci = 7 if b11==9 
replace pqnoasis1_ci = 8 if b11==4
replace pqnoasis1_ci = 9 if b11==10

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.

***************
***edupub_ci***
***************

gen edupub_ci=.
replace edupub_ci=1 if b091==1 | b091==2
replace edupub_ci=0 if b091==3

*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if b08>=41 & b08<=44
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=.
*acceso a una fuente de agua por red*
replace aguared_ch=1 if v10>=1 & v10<=3
replace aguared_ch=0 if v10==4
*acordado en meeting sept/7/05*

gen aguadist_ch=.
replace aguadist_ch=1 if v10==1
replace aguadist_ch=2 if v10==2
replace aguadist_ch=3 if v10==3


gen aguamala_ch=.
replace aguamala_ch=0 if v11>=1 & v11<=5
replace aguamala_ch=1 if v11==6 | v11==7
*segun lo acordado en reunion sept/7/05 pozo es agua BUENA*

gen aguamide_ch=.
/*NA*/

gen luz_ch=.
replace luz_ch=1 if v14>=1 & v14<=9
replace luz_ch=0 if v14==10| v14==11 
*10 otra fuente, 11 no tiene energia electrica*

gen luzmide_ch=.
/*NA*/

gen combust_ch=.
replace combust_ch=1 if  v15==1 | v15==2
replace combust_ch=0 if  v15==3 | v15==4
*v15=5 no cocina o ignorado se queda como missing*

gen bano_ch=.
replace bano_ch=1 if v13==1
replace bano_ch=0 if v13==2

gen banoex_ch=.
/*NA*/

gen des1_ch=.
replace des1_ch=0 if v12==5
replace des1_ch=1 if v12==1 |v12==2
replace des1_ch=2 if v12==3
*/replace des1_ch=NUNCA ES 3*/
*v12=4 SIGNIFICA OTRO, pero no existe esta categoria*

gen des2_ch=des1_ch

gen piso_ch=.
replace piso_ch=0 if v06==5
replace piso_ch=1 if v06>=1 & v06<=3
replace piso_ch=2 if v06==4

gen pared_ch=.
replace pared_ch=0 if v03==7
replace pared_ch=1 if v03>=1  & v03<=5
replace pared_ch=2 if v03==6
**revisar como categorizar prefabricado, perm, temp , otro...esta en permanente*

gen techo_ch=.
replace techo_ch=0 if v04==5
replace techo_ch=1 if v04>=1 & v04<=3
replace techo_ch=2 if v04==4

gen resid_ch=.
replace resid_ch=1 if v15a==1
replace resid_ch=2 if v15a==2 |v15a==3
replace resid_ch=3 if v15a==4 |v15a==5
replace resid_ch=4 if v15a==6
/* nueva variable, solo disponible para 2004,*
Sitema de eliminación de las basuras*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
		g       aguamejorada_ch = 1 if (v11 >=1 & v11 <=3)
		replace aguamejorada_ch = 0 if (v11 >=4 & v11 <=7)
			
 *********************
 ***banomejorado_ch***
 *********************
		g       banomejorado_ch = 1 if  (v12 >=1 & v12 <=3)
		replace banomejorado_ch = 0 if  (v12 >=4 & v12 <=5) |  v13 == 2
		
gen dorm_ch=.
replace dorm_ch=v08 if v08>=0 & v08<=25

gen cuartos_ch=.
replace cuartos_ch=v09 if v09>=1 & v09<=25

gen cocina_ch=.
/*NA*/

gen telef_ch=.
replace telef_ch=1 if  v16b==1
replace telef_ch=0 if  v16b==2

gen refrig_ch=.
replace refrig_ch=1 if  v16c==1
replace refrig_ch=0 if  v16c==2

gen freez_ch=.
/*NA*/

gen auto_ch=.
replace auto_ch=1 if  v16m==1
replace auto_ch=0 if  v16m==2

gen compu_ch=.
replace compu_ch=1 if  v16i==1
replace compu_ch=0 if  v16i==2

**********************************************************
**INTERNET_CH : El hogar posee conexión a Intern
**********************************************************
gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"

gen cel_ch=.
replace cel_ch=1 if  v16a==1
replace cel_ch=0 if  v16a==2

gen vivi1_ch=.
replace vivi1_ch=1 if  v01==1 |v01==2
replace vivi1_ch=2 if  v01==3
replace vivi1_ch=3 if  v01==4|v01==5


gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 |vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if v02==3
replace viviprop_ch=1 if v02==1
replace viviprop_ch=2 if v02==2
replace viviprop_ch=3 if v02==4
replace viviprop_ch=4 if v02==5
*en precario es de facto??, lo pongo como si sí*
*casi un 10% dice otr y no tengo como clasificarla, invente una categoria llamada 4 OTRA*


gen vivitit_ch=.
/*NA*/

gen vivialq_ch=.
replace vivialq_ch=v02a
replace vivialq_ch=. if v02a==99999999

gen vivialqimp_ch=.
/*NA*/


***********************
*** COSTA RICA 2004 ***
***********************

 destring, replace
 
/*
RELACION DE PARENTESCO	(b03)
 1   Jefe(a)
 2   Esposo(a)
 3   Hijo
 4   Yerno o Nuera
 5   Nieto(a)
 6   Padres o Suegros
 7   Otros Familiares
 8   Servidor Doméstico
 9   Pensionista
 10  Otros No Familiares
 11  Hernano(a)
*/

 gen	 incl=1 if (b03>=1 & b03<=11)
 replace incl=0 if (b03>=8 & b03<=9 )

* Variables

 

  /*
  Edad
  00  Menor de 1 año		
  01 - 97		
  98  Menor de 12 con edad ignorada		
  99  Mayor de 12 con edad ignorada		
  */
 rename b04 sexo
 rename b03 parentco
 rename b08 niveled
 rename b09 asiste
 rename zona area
 rename c17 rama
 rename c18 categ
 rename c15 ocup
 *rename c20 tamest
 label var c20 "tamanio de establecimiento"
 rename c28 ramasec
 rename c29 categsec
 rename c31 tamestsec
 rename c26 ocupsec
 rename c08 condinac
 rename v11 abagua
 rename v12 servsani
 rename v15 solidfuels
 rename v01 tipoviv
 rename v02 tenviv
 rename v09 nrocuar
 rename v03 paredes
 rename v06 piso
 rename v16a celular
 rename v16b telefono
 rename v16i computa

** AREA

 tab area [w=factor]

** Gender classification of the population refering to the head of the household.

 sort segmento vivienda hogar total linea_b

** ID Hogar

 gen y=1 if linea_b==1
 gen id_hogar=sum(y)

** Dwelling ID

 gen x=1 if linea_b==1 & hogar==1
 gen id_viv=sum(x)

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 
 tab sexo   [w=factor]
 tab sexo_d [w=factor]

 tab sexo sexo_d if parentco==1

 sort segmento vivienda hogar total linea_b

** Years of education. 

* Population with five or more years of age *

/*
niveled
¿Cuál es el último grado o año aprobado?
Nivel 				Año
 0. Ninguno 			0
 0. Preparatoria		1 
 0. Enseñanza Especial 		2
 1. Primaria 			Último año aprobado
 2. Sec. Académica		Último año aprobado
 3. Sec. Técnica		Último año aprobado
 4. Parauniversitaria		Último año aprobado
 5. Universidad 		Último año aprobado
*/

 gen	 anoest=0  if niveled==0  | niveled==1  | niveled==2
 replace anoest=1  if niveled==11
 replace anoest=2  if niveled==12
 replace anoest=3  if niveled==13
 replace anoest=4  if niveled==14
 replace anoest=5  if niveled==15 
 replace anoest=6  if niveled==16
 replace anoest=7  if niveled==21 | niveled==31 
 replace anoest=8  if niveled==22 | niveled==32
 replace anoest=9  if niveled==23 | niveled==33
 replace anoest=10 if niveled==24 | niveled==34 
 replace anoest=11 if niveled==25 | niveled==35 
 replace anoest=12 if niveled==41 | niveled==51 | niveled==36 
 replace anoest=13 if niveled==42 | niveled==52
 replace anoest=14 if niveled==43 | niveled==53 
 replace anoest=15 if niveled==44 | niveled==54
 replace anoest=16 if niveled==55 
 replace anoest=17 if niveled==56
 replace anoest=18 if niveled==57 
 replace anoest=19 if niveled==58
 replace anoest=99 if niveled==99 | niveled==.  | niveled==19  | niveled==29 | niveled==39 | niveled==49 | niveled==59

 tab anoest [w=factor], missing
 tab anoest niveled, missing
 

** Economic Active Population 
 gen	 peaa=1 if condact==1
 replace peaa=2 if condact>=2 & condact<=7
 replace peaa=3 if condact>=8 & condact<=14
 replace peaa=0 if condact==0

 gen	 tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 


************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)
*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* Official School Attendance Age: 6 years and 6 months., thus 7 years is the age of the survey used.
* ISCED 1

 gen	 NERP=0 if (edad>=7 & edad<=12) & (asiste>=1 & asiste<=8)
 replace NERP=1 if (edad>=7 & edad<=12) & (asiste==2)
  
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=17) & (asiste>=1 & asiste<=8)
 replace NERS=1 if (edad>=13 & edad<=17) & (asiste==3)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  (asiste==2)
 gen sec=1 if   (asiste==3)
 gen ter=1 if   (asiste==5 & niveled<=54) | (asiste==4 & niveled<=43) 

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

 gen RATIOALL=0 if     (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    
	
** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen RATIOLIT=0 if     ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

* Without domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categ==3 | categ==4) & (rama>=10101 & rama<=99000) & peaa==1
 replace WENAS=1 if (edad>=15 & edad<=64) & (categ==3 | categ==4) & (rama>=10101 & rama<=99000) & sexo==2 & peaa==1

* RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categ==3 | categ==4 | categ==5) & (rama>=10101 & rama<=99000) & peaa==1
 replace WENASD=1 if (edad>=15 & edad<=64) & (categ==3 | categ==4 | categ==5) & (rama>=10101 & rama<=99000) & sexo==2 & peaa==1

* RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

* The dwelling related variables are only available for the first household

* For the dwelling related indicators a command will be applied in order
* to impute the first household characteristic to the other households
* in the dwelling

** Access to Electricity ** Additional Indicator

 egen v14_d=max(v14), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen     ELEC=0 if (v14_d>=1 & v14_d<=11)  /* Total population excluding missing information */
 replace ELEC=1 if (v14_d>=1 & v14_d<=10)
 
 	
** Target 9, Indicator: Proportion of the population using solidfuels (%)

 egen solidfuels_d=max(solidfuels), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen     SFUELS=0 if (solidfuels_d>=1 & solidfuels_d<=5)  /* Total population excluding missing information */
 replace SFUELS=1 if (solidfuels_d==3)
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
 egen abagua_d=max(abagua), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (abagua_d>=1 & abagua_d<=7)  /* Total population excluding missing information */
 replace WATER=1 if (abagua_d>=1 & abagua_d<=5)
	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

 egen servsani_d=max(servsani), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (servsani_d>=1 & servsani_d<=5)  /* Total population excluding missing information */
 replace SANITATION=1 if (servsani_d>=1 & servsani_d<=2)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

 egen nrocuar_d=max(nrocuar), by(id_viv) 
 egen tenviv_d=max(tenviv), by(id_viv) 
 egen tipoviv_d=max(tipoviv), by(id_viv) 
 egen paredes_d=max(paredes), by(id_viv) 
 egen piso_d=max(piso), by(id_viv) 

 egen total_d=sum(incl), by(id_viv) /* Personas en la vivienda. Excluye servicio doméstico y huéspedes */

 gen persroom=total_d/nrocuar_d if nrocuar_d<99

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenviv_d>=1 & tenviv_d<=5) & (tipoviv_d>=1 & tipoviv_d<=5)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenviv_d>=4 & tenviv_d<=5) | (tipoviv_d>=4 & tipoviv_d<=5))

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if ((paredes_d>=1 & paredes_d<=7) & (piso_d>=1 & piso_d<=5))     /* Total population excluding missing information */
 replace secten_2=1 if ((paredes_d>=5 & paredes_d<=7) | (piso_d>=4 & piso_d<=5))

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0) 
 
* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  ((secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1))    /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator
* 6. ¿Cuál es el material predominante en el piso?

* Gender classification of the population refers to the head of the household.

 gen     DIRT=0 if (piso_d>=1 & piso_d<=5)  /* Total population excluding missing information */
 replace DIRT=1 if (piso_d==5)
 	
** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)
* No incluye los trabajadores domésticos y sus familiares, ni los huéspedes.

 gen     UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (tasadeso==1) 

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

 egen tel=max(telefono), by(id_viv) 
 egen cel=max(celular), by(id_viv) 

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if (tel==1 | tel==2) & (cel==1 | cel==2)  /* Total population excluding missing information */
 replace TELCEL=1 if (tel==1) 	       | (cel==1)

* Gender classification of the population refers to the head of the household.

** FIXED LINES

 gen     TEL=0 if (tel==1 | tel==2) /* Total population excluding missing information */
 replace TEL=1 if (tel==1)

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (cel==1 | cel==2) /* Total population excluding missing information */
 replace CEL=1 if (cel==1)

** Target 18, Indicator: "Personal computers in use per 100 population"

 egen computa_d=max(computa), by(id_viv) 

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if (computa_d==1 | computa_d==2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computa_d==1)

* Target 18, Indicator: "Internet users per 100 population"
* Not available for this year

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working

 gen     CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & peaa==1

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if linea_b==1

 gen     popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

** Disconnected Youths

 gen stu_sd=0 if (edad>=15 & edad<=24)
 replace stu_sd=1 if (condinac==3 | condinac==4) & (edad>=15 & edad<=24)

 gen DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & ((condinac==1| condinac==6) | ((c09==3 | (c10==4 | c10==7))  & stu_sd==0))

******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

*** Proportion of population below corresponding grade for age
* Official School Attendance Age: 6 years and 6 months., thus 7 years is the age of the survey used.


 gen     rezago=0       if (anoest>=0 & anoest<99)  & edad==7 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==8
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==8
 
 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==9
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==10
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==11
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==11

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==12
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==12

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==13
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==14
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==15
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==16
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==17
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if  (edad>=8 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if  (edad>=8 & edad<=17) & (rezago==1)
		
* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
	
* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15 & edad<98) & (anoest>=0 & anoest<99))
	
 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if  ((edad>=25 & edad<98) & (anoest>=0 & anoest<99))

* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=17) & (anoest>=0 & anoest<99)

* Grade for age primary

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)

* Grade for age Secondary

 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=17) & (anoest>=0 & anoest<99)

gen INTUSERS=.                      


ren ocup ocup_old

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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

*rename c17 rama
*rename c18 categ
*rename c15 ocup
*ren ocup ocup_old

rename rama codindustria
rename ocup_old codocupa
compress


saveold "`base_out'", replace


log close

