
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
local ANO "2002"
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
Autores: Mayra Sáenz Julio 2013
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 1 de octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

*2002*

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

rename zona area

************
****pais****
************

gen str3 pais_c="CRI"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2002
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

gen raza_ci=1 if b16 == 1
replace raza_ci=2 if b16 == 2 | b16 == 3
replace raza_ci=3 if b16 == 4 | b16 == 5 | b16 == 9

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo"  

gen raza_idioma_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if b15==6
replace civil_ci=2 if b15==1 | b15==2
replace civil_ci=3 if b15==3| b15==4
replace civil_ci=4 if b15==5

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
replace lp_ci =    28895 if zona_c==1
replace lp_ci =    22714 if zona_c==0

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = .
replace lpe_ci =   13255 if zona_c==1
replace lpe_ci =   11530 if zona_c==0

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (b06>=1 & b06<=3) 
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
replace instcot_ci=b061
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 



****************
****condocup_ci*
****************

* se pregunta a los de 12+
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
replace aux_ps = c35b/6 if c35b1==7
replace aux_ps = c35b/12 if c35b1==8
replace aux_ps = . if c35b==99999999


gen byte pensionsub_ci= 1 if aux_ps>0 & aux_ps!=.
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=aux_ps
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**salmm_ci***
*************

* 2002 junio (fuente OIT-CEPAL observatorio de la crisis) 74328.00
* 2015 MGD: se cambia por dato oficial publicado por el M. de Trabajo
gen salmm_ci= 77584	
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

gen firmapeq_ci=.
replace firmapeq_ci=1 if c20<=5
replace firmapeq_ci=0 if c20>5 & c20<99
replace firmapeq_ci=. if emp_ci==0
*/

*****************
***spublico_ci***
*****************

gen spublico_ci=(c16>=100 & c16<=500) 
replace spublico_ci=. if emp==0 | c16==999


**************
***ocupa_ci***
**************


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

*************
***rama_ci***
*************


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


gen ylmpri_ci=ingprinc
replace ylmpri_ci=. if ingprinci>=99999999
replace ylmpri_ci=. if emp_ci==0
*Modificación Mayra Sáenz - Mayo 2014
replace ylmpri_ci=. if ingtothog==99999999
replace ylmpri_ci=. if ingtothog==.
replace ylmpri_ci=0 if ingtothog==0

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ingprinci==99999999)
replace nrylmpri_ci=. if emp_ci==0 

***************
***ylmsec_ci***
***************

gen ylmsec_ci=ingprisec
replace ylmsec_ci=. if  ingprisec==99999999
replace ylmsec_ci=. if emp_ci==0 | nempleos_ci==1
*Modificación Mayra Sáenz - Mayo 2014
replace ylmsec_ci=. if ingtothog==99999999
replace ylmsec_ci=. if ingtothog==.
replace ylmsec_ci=0 if ingtothog==0

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

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ynlm_ci***
*************

gen ynlm_ci=otrosingme
replace ynlm_ci=. if otrosing==99999999
*Modificación Mayra Sáenz - Mayo 2014
replace ynlm_ci=. if ingtothog==99999999
replace ynlm_ci=. if ingtothog==.
replace ynlm_ci=0 if ingtothog==0

***N/A***
gen ylnmpri_ci=.
gen ylnmsec_ci=.
gen ylnm_ci=.
gen ynlnm_ci=.
gen autocons_ci=.
gen remesas_ci=.


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

replace aedu_ci=0 if b10==0 | b10==1 

*Primaria
replace aedu_ci=1 if b10==11 
replace aedu_ci=2 if b10==12
replace aedu_ci=3 if b10==13
replace aedu_ci=4 if b10==14
replace aedu_ci=5 if b10==15
replace aedu_ci=6 if b10==16

*Secundaria (académica y técnica)
replace aedu_ci=7 if b10==21 | b10==31
replace aedu_ci=8 if b10==22 | b10==32
replace aedu_ci=9 if b10==23 | b10==33
replace aedu_ci=10 if b10==24 | b10==34
replace aedu_ci=11 if b10==25 | b10==35
replace aedu_ci=12 if b10==36 

*Superior (universitario o para-universitario)
replace aedu_ci=13 if b10==41 | b10==51
replace aedu_ci=14 if b10==42 | b10==52
replace aedu_ci=15 if b10==43 | b10==53
replace aedu_ci=16 if b10==44 | b10==54
replace aedu_ci=17 if b10==55
replace aedu_ci=18 if b10==56
replace aedu_ci=19 if b10==57
replace aedu_ci=20 if b10==58


**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if b10==0 | b10==1 
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if (b10>=11 & b10<16) 
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if b10==16
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (b10>=21 & b10<=25) 
replace edusi_ci=1 if (b10>=31 & b10<=35) 
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if b10==36
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if (b10>=21 & b10<=22)
replace edus1i_ci=1 if (b10>=31 & b10<=32)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if b10==23 | b10==33
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if (b10>=24 & b10<=25)
replace edus2i_ci=1 if (b10>=34 & b10<=35)
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if b10==36
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if (b10>=41 & b10<=44)
replace eduui_ci=1 if (b10>=51 & b10<=54)
label variable eduui_ci "Superior incompleto"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (b10>=55 & b10<=58)
label variable eduuc_ci "Superior completo"



local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}


***************
***edupre_ci***
***************

gen byte edupre_ci=.
replace edupre_ci=(b10==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=0 if (b10>=41 & b10<=49) 
replace eduac_ci=1 if (b10>=51 & b10<=59) 
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if b11>=1 & b11<=6
replace asiste_ci=0 if b11==7
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=b12

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if b12==3
replace pqnoasis1_ci = 2 if b12==1
replace pqnoasis1_ci = 3 if b12==6 | b12==7
replace pqnoasis1_ci = 4 if b12==8
replace pqnoasis1_ci = 5 if b12==2 | b12==5
replace pqnoasis1_ci = 7 if b12==9 
replace pqnoasis1_ci = 8 if b12==4
replace pqnoasis1_ci = 9 if b12==10

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

*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if b10>=41 & b10<=44
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
replace aguamala_ch=0 if v11==1| v11==2 | v11==3
replace aguamala_ch=1 if  v11==4 | v11==5
*segun lo acordado en reunion sept/7/05 pozo es agua BUENA*

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if v14==1 | v14==2
replace luz_ch=0 if v14==3 | v14==4

gen luzmide_ch=.


gen combust_ch=.
replace combust_ch=1 if  v15==1 | v15==2
replace combust_ch=0 if  v15==3 | v15==4
*v15=5 no cocina o ignorado se queda como missing*

gen bano_ch=.
replace bano_ch=1 if v13==1
replace bano_ch=0 if v13==2

gen banoex_ch=.

gen des1_ch=.
replace des1_ch=0 if v12==5
replace des1_ch=1 if v12==1 |v12==2
replace des1_ch=2 if v12==3
*/replace des1_ch=NUNCA ES 3*/
*v12=4 SIGNIFICA OTRO, pero no existe esta categoria*

gen des2_ch=des1_ch

gen piso_ch=.
replace piso_ch=0 if v06==5
replace piso_ch=1 if v06==1 |v06==2 | v06==3
replace piso_ch=2 if v06==4

gen pared_ch=.
replace pared_ch=0 if v03==6
replace pared_ch=1 if v03>=1  & v03<=4
replace pared_ch=2 if v03==5
**revisar como categorizar prefabricado: perm, temp u otro...está en permanente*

gen techo_ch=.
replace techo_ch=0 if v04==4
replace techo_ch=1 if v04==1 | v04==2
replace techo_ch=2 if v04==3


gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v11 >=1 & v11 <=3)
replace aguamejorada_ch = 0 if (v11 >=4 & v11 <=5)
			
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v12 >=1 & v12 <=3)
replace banomejorado_ch = 0 if  (v12 >=4 & v12 <=5) |  v13 == 2

gen dorm_ch=.
replace dorm_ch=v08 if v08>=0 & v08<=10

gen cuartos_ch=.
replace cuartos_ch=v09 if v09>=1 & v09<=25

gen cocina_ch=.
/*NA*/

gen telef_ch=.
replace telef_ch=1 if  v16a==1
replace telef_ch=0 if  v16a==2

**********************************************************
**INTERNET_CH : El hogar posee conexión a Intern
**********************************************************
gen internet_ch=.
replace internet_ch=1 if  v16i==1
replace internet_ch=0 if  v16i==2
label var internet_ch "El hogar posee conexión a Internet"

gen refrig_ch=.
replace refrig_ch=1 if  v16b==1
replace refrig_ch=0 if  v16b==2

gen freez_ch=.
/*NA*/

gen auto_ch=.
replace auto_ch=1 if  v16m==1
replace auto_ch=0 if  v16m==2

gen compu_ch=.
replace compu_ch=1 if  v16h==1
replace compu_ch=0 if  v16h==2

gen cel_ch=.


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
replace vivialq_ch=. if v02a==999999

gen vivialqimp_ch=.
/*NA*/



/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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



rename c17 codindustria
rename c15 codocupa
compress


saveold "`base_out'", replace


log close




