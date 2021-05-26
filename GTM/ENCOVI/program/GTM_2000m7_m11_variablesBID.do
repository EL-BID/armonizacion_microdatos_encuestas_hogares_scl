
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

local PAIS GTM
local ENCUESTA ENCOVI
local ANO "2000"
local ronda m7_m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENCOVI
Round: m7_m11
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear



*****************************************************************************************************************
******                                    GUATEMALA 2000                                                   ******
******                ENCOVI 2000 (ENCUESTA NACIONAL DE CONDICIONES DE VIDA)                               ******
******                                    37.771 personas                                                  ****** 
*****************************************************************************************************************

*************************************************************************/


gen factor_ci=FACTOR
label var factor_ci "Factor de Expansion del Individuo"
**************************************************************************************************************
* HOUSEHOLD VARIABLES
**************************************************************************************************************

gen factor_ch=factorh
label var factor_ch "Factor de expansion del Hogar"


	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

* ZONA
gen byte zona_c=1 if AREA==1 /* Urbana */
replace zona_c=0 if AREA==2 /* Rural */
label variable zona_c "ZONA GEOGRAFICA"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

* COUNTRY - YEAR
gen str3 pais_c="GTM"
label variable pais_c "Nonmbre del Pais"

gen anio_c=2000
label variable anio_c "Año de la Encuesta"

* Periodo de Referencia: del 07/00 al 11/00.
* This is the middle of the reference period
gen byte mes_c=9
label variable mes_c "Mes de la Encuesta"

* SEXO
gen sexo_ci=SEXO
label variable sexo "Sex of the individual"
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"  
label value sexo_ci sexo_ci

* PARENTESCO
gen relacion_ci=1 if P05A02==1
replace relacion_ci=2 if P05A02==2
replace relacion_ci=3 if P05A02==3
replace relacion_ci=4 if ( P05A02==4 | P05A02==5 | P05A02==6 | P05A02==7 | P05A02==8 | P05A02==9 | P05A02==10 )
replace relacion_ci=5 if ( P05A02==12 | P05A02==13 )
replace relacion_ci=6 if P05A02==11
label var relacion_ci "Parentesco o relacion con el Jefe del Hogar"
label define relacion_ci 1 "Jefe(a)" 2 "Esposo(a) o compañero(a)" 3 "Hijo(a)" 4 "Otro pariente" 5 "Otro NO pariente" 6 "Empleada domestica" 
label value relacion_ci relacion_ci

* EDAD
* 99 is the top-code, not that age is missing 
* meses is also available
gen edad_ci=EDAD

label var edad_ci "Edad del Individuo"

* IDENTIFICADOR DEL HOGAR
egen idh_ch=group(HOGAR) 
label var idh_ch "Identificador Unico del Hogar"

* IDENTIFICADOR DE LA PERSONA
gen idp_ci=CASO
label var idp_ci "Identificador Individual dentro del Hogar"

sort idh_ch idp_ci

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "Numero de Conyuges"

egen nhijos_ch=sum(relacion_ci==3), by (idh_ch)
label variable nhijos_ch "Numero de Hijos"
egen notropari_ch=sum(relacion_ci>3 & relacion_ci<5), by (idh_ch)
label variable notropari_ch "Numero de Otros Parientes "
egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label variable notronopari_ch "Numero de Otros NO Parientes "
egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label variable nempdom_ch "Numero de Empleados Domesticos"

* HOUSEHOLD TYPE (unipersonal, nuclear, ampliado, compuesto, corresidentes)    
* note: These are all defined in terms of relationship to household head

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

* Checking 'clasehog' (que todas las personas pertenezcan a un hogar con 'clasehog' definido)
assert clasehog==int(clasehog) 
assert clasehog>=1 & clasehog<=5 
sum clasehog
scalar claseT=r(N)
assert claseT==37771


* HOUSEHOLD COMPOSITION VARIABLES 
/* note: These are unrelated to who is the head
   note: That childh denotes the number of children of the head, while numkids counts the number of all kids in the household */

sort idh_ch

* NUMBER OF PERSONS IN THE HOUSEHOLD (not including domestic employees or other relatives)
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5 ), by (idh_ch)
label variable nmiembros_ch "Numero de miembros en el Hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<6)), by (idh_ch)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<1)),  by (idh_ch)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"


*** ESTADO CIVIL PARA PERSONAS DE 10 AÑOS O MAS DE EDAD
gen civil_ci=.  
replace civil_ci=1 if P05A03==7 /* SOLTERO */
replace civil_ci=2 if P05A03==1 | P05A03==2 /* UNION FORMAL O INFORMAL */
replace civil_ci=3 if P05A03==3 | P05A03==4 | P05A03==5 /* SEPARADO O DIVORCIADO */
replace civil_ci=4 if P05A03==6 /* VIUDO */
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*** REPORTED HEAD OF HOUSEHOLD
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de Hogar Declarado"

*** We want to know if there is only one head in each hh and if there is a hh with no head:
egen hh=sum(jefe_ci), by (idh_ch)
capture assert hh==1

/* El hogar 2921 no reporta jefe:
   idh_ch   relacion_ci   parent~2 
       
    2921          2          2 
    2921          3          3 
    2921          3          3 
    2921          3          3 
    2921          3          3 */

**********************************
*** INCOME VARIABLES (MONTHLY) ***
**********************************

* Create a dummy indicating this person's income should NOT be included in y*_ch
gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

sort idh_ch

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/* RAZA
gen ethnic2=1 if ( P05B05>=1 & P05B05<=5 )  MAYA 
replace ethnic2=2 if ( P05B05==6 | P05B05==7 )  NO MAYA 
replace ethnic2=3 if  P05B05==8  NO INDIGENA 

gen ethnic=1 if ethnic2==1 | ethnic2==2  INDIGENA 
replace ethnic=2 if ethnic2==3  NO INDIGENA */



gen raza_aux=.
replace raza_aux=1 if  (P05B05>=1 & P05B05<=5) | P05B05==7
replace raza_aux=4 if  (P05B05==6) & raza_aux==.
replace raza_aux=3 if  (P05B05==8 |  P05B05==9) & raza_aux==.
bys idh_ch: gen aux=raza_aux if P05A02==1
bys idh_ch: egen aux1 = max(aux)
replace raza_aux=aux1 if (raza_aux ==. & (P05A02 ==3|P05A02==5))  
replace raza_aux=3 if raza_aux==. 
drop aux aux1
label define raza_aux 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 4 "Afro-indígena"
label value raza_aux raza_aux 
label var raza_aux "Raza o etnia del individuo" 

gen raza_ci=.
replace raza_ci=1 if  (P05B05>=1 & P05B05<=5) | P05B05==7
replace raza_ci=2 if  (P05B05==6) & raza_ci==.
replace raza_ci=3 if  (P05B05==8 |  P05B05==9) & raza_ci==.
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_aux==1 | raza_aux==4
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_aux==2 | raza_aux==4
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

/*
1 KICHE
           2 QEQCHI
           3 KAQCHIKE
           4 MAM
           5 OTRO MAY
           6 GARIFUNA
           7 XINKA
           8 NO INDIG
           9 OTRO PAI
*/

*** HOUSING ***


* MGR Jul, 2015: corrección en sintáxis

/*
gen aguared_ch=.
replace aguared_ch=1 if P01A05A==1
replace aguared_ch=0 if P01A05A==2
*/
gen aguared_ch= (P01A12>=1 & P01A12<=3)
replace aguared_ch=. if P01A12==. 

gen aguadist_ch=.
replace aguadist_ch=1 if P01A12==1
replace aguadist_ch=2 if P01A12==2
replace aguadist_ch=3 if P01A12>=3 & P01A12<=8

gen aguamala_ch=.
replace aguamala_ch=1 if P01A12==5 | P01A12==6 | P01A12==7 | P01A12==8
replace aguamala_ch=0 if P01A12==1 | P01A12==2 | P01A12==3 | P01A12==4

gen aguamide_ch=.
replace aguamide_ch=1 if P01A05E==1
replace aguamide_ch=0 if P01A05E==2

gen luz_ch=.
replace luz_ch=1 if P01A05C==1
replace luz_ch=0 if P01A05C==2

gen luzmide_ch=.
replace luzmide_ch=1 if P01A05F==1
replace luzmide_ch=0 if P01A05F==2

/*gen combust_ch=.
replace combust_ch=1 if 
replace combust_ch=0 if 
*/

gen bano_ch=.
replace bano_ch=1 if P01A26==1 | P01A26==2 | P01A26==3 | P01A26==4
replace bano_ch=0 if P01A26==5

gen banoex_ch=.
replace banoex=1 if P01A27==1
replace banoex=0 if P01A27==2

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing

gen des1_ch=.
replace des1_ch=0 if P01A26==5
replace des1_ch=1 if P01A26==1 | P01A26==2 | P01A26==3
replace des1_ch=2 if P01A26==4

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing
 
gen des2_ch=.
replace des2_ch=0 if P01A26==5
replace des2_ch=1 if P01A26==1 | P01A26==2 | P01A26==3 | P01A26==4

gen piso_ch=.
replace piso_ch=0 if P01A04==5
replace piso_ch=1 if P01A04>=1 & P01A04<=4
replace piso_ch=2 if P01A04==6

gen pared_ch=.
replace pared_ch=0 if P01A02==7 | P01A02==8
replace pared_ch=1 if P01A02>=1 & P01A02<=6
replace pared_ch=2 if P01A02==9

gen techo_ch=.
replace techo_ch=0 if P01A03==5 | P01A03==6
replace techo_ch=1 if P01A03>=1 & P01A03<=4
replace techo_ch=2 if P01A03==7

gen resid_ch=.
replace resid_ch=0 if P01A32==1 | P01A32==2
replace resid_ch=1 if P01A32==3 | P01A32==4
replace resid_ch=2 if P01A32==5
replace resid_ch=3 if P01A32==6 | P01A32==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (P01A12 >=1 & P01A12 <=4) | P01A12 ==7
replace aguamejorada_ch = 0 if (P01A12 >=5 & P01A12 <=6) | P01A12 ==8
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (P01A26 >=1 & P01A26 <=4) & P01A27 == 1
replace banomejorado_ch = 0 if ((P01A26 >=1 & P01A26 <=4) & P01A27 == 2) & P01A26 ==5 

gen dorm_ch=.
replace dorm_ch=P01A08 if P01A08>=0

gen cuartos_ch=.
replace cuartos_ch=P01A07 if P01A07>=0

gen cocina_ch=.
replace cocina_ch=1 if P01A10==1
replace cocina_ch=0 if P01A10>=2 & P01A10<=7

gen telef_ch=.
replace telef_ch=1 if P01A29A==1
replace telef_ch=0 if P01A29A==2

gen refrig_ch=.
replace refrig_ch=1 if p14a014==1
replace refrig_ch=0 if p14a014==2

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if p14a0160==1
replace auto_ch=0 if p14a0160==2

gen compu_ch=.
replace compu_ch=1 if p14a0120==1
replace compu_ch=0 if p14a0120==2

gen internet_ch=.
replace internet_ch=1 if P01A29D==1 
replace internet_ch=0 if P01A29D==2

gen cel_ch=.
replace cel_ch=1 if P01A29B==1
replace cel_ch=0 if P01A29B==2

gen vivi1_ch=.
replace vivi1_ch=1 if P01A01==1
replace vivi1_ch=2 if P01A01==2
replace vivi1_ch=3 if P01A01>=3 & P01A01<=6

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if P01C01==5
replace viviprop_ch=1 if P01C01==1
replace viviprop_ch=2 if P01C01==2
replace viviprop_ch=3 if P01C01==3 | P01C01==4 | P01C01==6 | P01C01==7

gen vivitit_ch=.
replace vivitit_ch=1 if P01C02>=2 & P01C02<=6
replace vivitit_ch=0 if P01C02==1

gen vivialq_ch=.
replace vivialq_ch=P01C08 if P01C08<99999

gen vivialqimp_ch=.



*******************************************************************************************
* VARIABLES DEL MERCADO LABORAL

* PERSONAS DE 5 AÑOS Y MAS DE EDAD *
* EN 1998 ESTE BLOQUE DE PREGUNTAS ESTABA DIRIGIDO A LAS PERSONAS DE 7 AÑOS Y MAS DE EDAD *
*******************************************************************************************
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

*1 = GUA 2000
*09/09/2015 MGD: el salario anterior  de 791.25 solamente fue fijado a mediados de diciembre de este año.
gen salmm_ci= 	682.05

label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********

gen lp_ci =6596
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =2920
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
****condocup_ci*
****************
* No se consideraba la busqueda de empleo ys e tomaba en cuenta el hecho de tener trabajo.  Se corrigio: 05/19/2014 MGD
* MGR: Modifico serie en base a correcciones Laura Castrillo: delimitar la condición de edad para que no tome los missing en caso que existan
gen condocup_ci=.
replace condocup_ci=1 if P10A01==1 | P10A03==1
replace condocup_ci=2 if P10A01==2 & P10A03==2 & P10A04==1 & P10A05<=4
recode condocup_ci .=3 if edad_ci>=7 & edad_ci!=.
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if P10B13A==1
recode afiliado_ci .=0 if P10B13A!=1
recode afiliado_ci .=0 if condocup_ci==1 | condocup_ci==2 | condocup_ci==3

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
gen instcot_ci=P10B13A==1
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*************
*cesante_ci* 
*************
/* CESANTE: Solo a los DESOCUPADOS: Trabajo antes?
gen cesante_ci=1 if P10A06==1  SI 
replace cesante_ci=0 if P10A06==2  NO */

gen cesante_ci=1 if P10A06==1 
replace cesante_ci=0 if P10A06==2 
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

gen tamemp_ci=1 if P10B12==1 | P10B12==2
replace tamemp_ci=2 if P10B12>=3 & P10B12<=5
replace tamemp_ci=3 if P10B12>=6 & P10B12<=7
replace tamemp_ci=. if P10B12==9

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"



/*
* EMP
gen byte emp_ci=1 if ( P10A01==1 | P10A02==1| P10A03==1 )
replace emp_ci=0 if ( P10A01==2 & P10A02==2 & P10A03==2 )
label var emp_ci "Empleado en la semana de referencia"

* DESEMP1
* Isolating workers self declared searchers that did not work (at all) last week
gen byte desemp1_ci=1 if ( P10A04==1 & emp~=1 )
replace desemp1_ci=0 if emp_ci==1 | (P10A04==2 & emp~=1 )
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

* DESEMP2
gen byte desemp2_ci=desemp1_ci
replace desemp2_ci=1 if (emp_ci~=1 & P10A04~=1 & P10A09==1)
replace desemp2_ci=0 if emp_ci==1 | (P10A04~=1 & P10A09>1 & P10A09<13)
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista" /* No incluye temporada agricola para comparar con 2002*/

gen byte desemp20_ci=desemp1_ci
replace desemp20_ci=1 if (emp_ci~=1 & P10A04~=1 & (P10A09==1 | P10A09==2))
replace desemp20_ci=0 if emp_ci==1 | (P10A04~=1 & P10A09>2 & P10A09<13)
label var desemp20_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola" /* Incluye temporada agricola */

* DESEMP3: No se puede crear, la pregunta 'vdp25' de 1998 no se capta en 2000
* 'vdp25': Aunque no busco trabajo, habia buscado trabajo o trato de establecer su propia empresa o negocio en las 4 semanas anteriores?
gen byte desemp3_ci=.
/*replace unemp3=unemp2
replace unemp3=1 if (emp~=1 & P10A04~=1 & vdp25==1)*/
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

* PEA: economically active population
gen byte pea1_ci=1 if ( emp==1 | desemp1_ci==1 )
label var pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"
gen byte pea2_ci=1 if ( emp==1 | desemp2_ci==1 )
label var pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"
gen byte pea3_ci=.
label var pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"

replace pea1_ci=0 if pea1_ci~=1
rep
*/

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

****************
* categopri_ci *
****************
/* P10B14 CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL
1 - 4 ASALARIADOS
5 - 6 INDEPENDIENTES
7 - 8 TRAB SIN PAGO
*/
gen categopri_ci=.
replace categopri_ci=1 if P10B14==5 & emp_ci==1
replace categopri_ci=2 if P10B14==6 & emp_ci==1
replace categopri_ci=3 if (P10B14>=1 & P10B14<=4) & emp_ci==1
replace categopri_ci=4 if (P10B14==7 | P10B14==8) & emp_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (P10B16==1 & P10B17==1) & categopri_ci==3
replace tipocontrato_ci=2 if (P10B16==1 & P10B17==2) & categopri_ci==3
replace tipocontrato_ci=3 if (P10B16==2  | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


* TRABAJADORES DESALENTADOS
gen byte desalent_ci=1 if pea_ci~=1  & (P10A09==9 | P10A09==11) 
replace desalent_ci=0 if pea_ci==1 | (P10A09!=9 & P10A09!=11)
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

/* egen horaspri_ci=rsum(p05a31a p05a31b p05a31c p05a31d p05a31e p05a31f p05a31g) if  p05a31a!=.

*** Horas trabajadas en la Actividad Secundaria
*** 98 is top code
gen horassec_ci=p05b09  if p05b09<99

gen horastot_ci=horaspri_ci+horassec_ci if horaspri_ci!=. & horassec_ci!=.
replace horastot_ci=horaspri_ci if horassec_ci==.
*/
*** HORAS ACTIVIDAD PRINCIPAL
gen horaspri_ci=P10B07 if P10B07<999
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

*** HORAS ACTIVIDAD SECUNDARIA
*** 98 is top code
gen horassec_ci=P10C08 if P10C08<99
label variable horassec_ci "Horas totales trabajadas la semana pasada en la Actividad Secundaria"

gen horastot_ci=horaspri_ci+horassec_ci if horaspri_ci!=. & horassec_ci!=.
replace horastot_ci=horaspri_ci if horassec_ci==.
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

** Hay individuos que trabajan mas de 24 horas al dia, es decir mas de 168 horas por semana
/* replace horastot_ci=. if horastot_ci>*****

 horastot_ci |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         10        0.07        0.07
          1 |         14        0.09        0.16
          2 |         45        0.30        0.45
          3 |         35        0.23        0.69
          4 |         76        0.50        1.19
      *********************************************
        126 |          6        0.04       99.67
        131 |          2        0.01       99.68
        132 |         11        0.07       99.76
        133 |          3        0.02       99.78
        134 |          2        0.01       99.79
        137 |          1        0.01       99.80
        138 |          1        0.01       99.80
        140 |          2        0.01       99.82
        141 |          1        0.01       99.82
        143 |          3        0.02       99.84
        144 |          3        0.02       99.86
        147 |          3        0.02       99.88
        150 |          1        0.01       99.89
        151 |          1        0.01       99.89
        156 |          2        0.01       99.91
        161 |          3        0.02       99.93
        162 |          1        0.01       99.93
        164 |          1        0.01       99.94
        168 |          4        0.03       99.97
        180 |          1        0.01       99.97
        182 |          1        0.01       99.98
        189 |          2        0.01       99.99
        200 |          1        0.01      100.00
------------+-----------------------------------
      Total |     15,169      100.00 */

* SUBEMPLEADO
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & P10B10A==1 & emp==1
label var subemp_ci "Trabajadores subempleados"

* Modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). MGD 06/19/2014
gen subemp_ci1=0
replace subemp_ci1=1 if horaspri_ci<=30 &  P10D01==1 & emp_ci==1 & P10D05==1
label var subemp_ci "Trabajadores subempleados"

* Trabajadores a Medio Tiempo
gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30  & (P10B10A==2) & emp==1
replace tiempoparc_ci=0 if ((horastot_ci>30  & P10B10A==2) | P10B10A==1) & emp_ci==1
label var tiempoparc_ci "Trabajadores a medio tiempo"

* Contratos
*gen contrato_ci=.
*label var contrato_ci "Personas empleadas que han firmado un contrato de trabajo"
/*
* BENEFICIOS (Seguridad Social)
** La pregunta exacta, dirigida a todos los trabajadores, de la encuesta es: Paga una cuota al Seguro Social (IGSS) por el trabajo que tuvo?
gen segsoc_ci=1 if P10B13A==1
replace segsoc_ci=0 if P10B13A==2
label variable segsoc_ci "Personas que cuentan con seguro social"
*/
* Numero de ocupaciones
gen nempleos_ci=1 if emp_ci==1 /* 1 empleo */
replace nempleos_ci=2 if ( emp==1 & P10C01==1 ) /* 'P10C01' pregunta si trabajo en una ocupacion secundaria */ 
replace nempleos_ci=0 if emp_ci~=1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

/*
*Modificada Mayra Sáenz Octubre 2013.
gen firmapeq_ci= .
replace firmapeq_ci= 1 if P10B12==1 | P10B12==2
recode firmapeq_ci .=0 if P10B12>=3 & P10B12<=7

* Tamaño de la firma
gen byte tamfirma_ci=.
replace tamfirma_ci=0 if P10B12<=2
replace tamfirma_ci=1 if ( P10B12>=3 & P10B12<=7 )
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 5 trabajadores" 0 "5 o menos trabajadores"
label values tamfirma_ci tamfirma_ci
*/
*** Sector Publico
gen spublico_ci=1 if P10B14==1
replace spublico_ci=0 if P10B14>=2 & P10B14<=8
label var spublico_ci "Personas que trabajan en el sector publico"

/* SELF-EMPLOYMENT
* Cuenta propia
gen byte selfemp=1 if P10B14==6
   
* INDEPENDENT WORKERS
* Patrono, cuenta propia 
gen byte indep=1 if ( P10B14==6 | P10B14==5 )*/

************************************************************************************************************
*** VARIABLES DE DEMANDA LABORAL
************************************************************************************************************

* P10B01 is the ocupa variable

* OCCUPACION
gen ocupa_ci=.
* P10B01
* 2014,10 MLO preparado por MLO segun excel ${surveysFolder}\survey\GTM\ENCOVI\2000\m7_m11\docsocup_ci_clasificacion_propuesta.xlsx

replace ocupa_ci=1 if (P10B01>=21 & P10B01<=34) & emp_ci==1
replace ocupa_ci=2 if (P10B01>=11 & P10B01<=16) & emp_ci==1
replace ocupa_ci=3 if (P10B01>=41 & P10B01<=45) & emp_ci==1
replace ocupa_ci=4 if (P10B01==52 | P10B01==95) & emp_ci==1
replace ocupa_ci=5 if (P10B01==51 | P10B01==91 | P10B01==53 | P10B01==90)  & emp_ci==1
replace ocupa_ci=6 if ((P10B01>=60 & P10B01<=62) | P10B01==92) & emp_ci==1
replace ocupa_ci=7 if ((P10B01>=71 & P10B01<=85) | P10B01==93) & emp_ci==1
replace ocupa_ci=8 if (P10B01>=0 & P10B01<=3)& emp_ci==1
replace ocupa_ci=9 if (P10B01==94 | P10B01==4 | P10B01==9 | P10B01==57 | P10B01==98 | P10B01==99) & emp_ci==1
label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "Profesionales y tecnicos" 2 "Directores y funcionarios superiores" 3 "Personal administrativo y nivel intermedio" 4 "Comerciantes y vendedores" 5 "Trabajadores en servicios" 6 "Trabajadores agricolas y afines" 7 "Obreros no agricolas, conductores de maquinas y vehiculos de transporte y similares" 8 "Fuerzas armadas" 9 "Otras ocupaciones no clasificadas en las anteriores"
label values ocupa_ci ocupa_ci


/*replace ocupa=1 if (vdp33>=2110 & vdp33<=3480) & emp==1
replace ocupa=2 if (vdp33>=1110 & vdp33<=1319) & emp==1
replace ocupa=3 if (vdp33>=4111 & vdp33<=4223) & emp==1
replace ocupa=4 if (vdp33>=5210 & vdp33<=5260) & emp==1 
*** look at the anexo to see how it was separated
replace ocupa=5 if (vdp33>=5112 & vdp33<=5169) & emp==1
replace ocupa=6 if (vdp33>=6111 & vdp33<=6210) & emp==1
replace ocupa=7 if (vdp33>=7111 & vdp33<=8340) & emp==1
replace ocupa=8 if (vdp33==110) & emp==1
replace ocupa=9 if (vdp33>=9111 & vdp33<=9333) & emp==1

tab P10B01 emp if ocupa==.*/

* P10B02 is the rama variable

* RAMA
gen rama_ci=.
* MLO = supongo que se mantiene CIIU rev 3
replace rama_ci=1 if (P10B02>=1 &  P10B02<=5) & emp_ci==1
replace rama_ci=2 if (P10B02>=10 & P10B02<=14) & emp_ci==1
replace rama_ci=3 if (P10B02>=15 & P10B02<=37) & emp_ci==1
replace rama_ci=4 if (P10B02>=40 & P10B02<=41) & emp_ci==1
replace rama_ci=5 if (P10B02==45) & emp_ci==1
replace rama_ci=6 if (P10B02>=50 & P10B02<=55) & emp_ci==1
replace rama_ci=7 if (P10B02>=60 & P10B02<=64) & emp_ci==1
replace rama_ci=8 if (P10B02>=65 & P10B02<=74) & emp_ci==1
replace rama_ci=9 if (P10B02>=75 & P10B02<=99) & emp_ci==1
replace rama_ci=. if emp_ci!=1
tab P10B02  rama_ci

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, sivicultura y pesca" 2 "Explotacion de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construccion" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

*P10B02


*** ANTIGUEDAD (AÑOS) (En total, cuantos años lleva trabajando (ocup principal)?) (NOT AVAILABLE IN 1998)
* En 2002: Cuantos años continuos lleva trabajando en esta empresa o negocio? 
* En 2000: en total, cuantos años lleva trabajando? (No menciona que sean continuos) 
gen antiguedad_ci=.
replace antiguedad_ci=P10B03 if P10B03!=99 & emp_ci==1
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en años)"

*** DURACION DEL DESEMPLEO
gen durades_ci=P10A05/4.3 /* Solo a los DESOCUPADOS: Cuantas semanas hace que esta buscando trabajo? */
replace durades_ci=0.23 if P10A05==0
replace durades_ci=. if P10A05==999
label var durades_ci "Duracion del Desempleo (en meses)"


******************************************************************************************************
* VARIABLES DE EDUCACION

* PARA PERSONAS DE 7 AÑOS O MAS DE EDAD 

* EXISTE UN BLOQUE DE EDUCACION PREESCOLAR PARA PERSONAS DE MENOS DE 7 AÑOS, AUNQUE NO APARECE EN 1998
* Los que tienen edad<=6 tienen 'missings' en estas variables
******************************************************************************************************

* AÑOS DE EDUCACION

* There are two variables 'P07B27A' & 'P07B27B' that have nivel and grado 

/* 
  NIVEL=P07B27A
1 Ninguno
2 Preparatoria
3 Primaria (6 grados)
4 Educacion Media
5 Educacion Superior
6 Post-grado
7 Educacion Adultos
*/

gen byte aedu_ci=.

*** Ninguno 
replace aedu_ci=0 if P07B27A==1 

*** Preparatoria
replace aedu_ci=0 if P07B27A==2

*** Primaria
replace aedu_ci=P07B27B if P07B27A==3 & P07B27B!=9
replace aedu_ci=6 if P07B27A==3 & P07B27B>6 & P07B27B<9 
/* 1 obs reporta 7 grados de primaria, se le asignan 6 grados */

*** Educacion Media
replace aedu_ci=6 + P07B27B if P07B27A==4 & P07B27B!=9
replace aedu_ci=12 if P07B27A==4 & P07B27B>6 & P07B27B<9
/* 6 obs reportan 7 años de secundaria, se le asignan 6 años */

*** Educacion Superior
replace aedu_ci=12 + P07B27B if P07B27A==5 & P07B27B!=9

*** Post-grado
replace aedu_ci=17 + P07B27B if P07B27A==6 & P07B27B!=9

label variable aedu_ci "Años de Educacion"

* Educacion Adultos
/* Quedan con 'missing', no se puede identificar si corresponden a nivel primario o secundario */

* Groups based on the years of schooling

/* 6 cumulative categories
variable name:
NOSCHOOL: no school 
ALSPRI: at least some primary
ALCPRI: at least complete primary
ALSSEC: at least some secondary
ALCLSEC: at least 9th grade completed
ALCSEC: at least complete secondary
ALSHIGH: at least some higher

gen byte noschool=.
replace noschool=1 if aedu_ci==0
replace noschool=0 if aedu_ci!=. & aedu_ci>0
label variable noschool "Completed zero years of schooling"

* SOME PRIMARY
gen byte alspri=0
replace alspri=1 if aedu_ci>0
replace alspri=. if aedu_ci==.
label variable alspri "Completed at least SOME primary"

* COMPL. PRIMARY
gen byte alcpri=0
replace alcpri=1 if aedu_ci>=6
replace alcpri=. if aedu_ci==.
label variable alcpri "Completed at least primary"

* AT LEAST SOME SECONDARY EDUCATION
gen byte alssec=0
replace alssec=1 if aedu_ci>=7
replace alssec=. if aedu_ci==.
label variable alssec "Completed at least SOME secondary school"

* COMPL. 1st CYCLE SECONDARY
gen byte alc1csec=0
replace alc1csec=1 if aedu_ci>=9
replace alc1csec=. if aedu_ci==.
label variable alc1csec "Completed at least first cycle secondary"

* COMPL. 2nd CYCLE SECONDARY (completed all of secondary school)
gen byte alcsec=0
replace alcsec=1 if aedu_ci>=12
replace alcsec=. if aedu_ci==.
label variable alcsec "Completed at least secondary school"

* SOME HIGHER EDUCATION
gen byte alshigh=0
replace alshigh=1 if  ( P07B27A==5 | P07B27A==6 )
replace alshigh=. if aedu_ci==.
label variable alshigh "Completed at least SOME higher schooling"

For the technical school
* Technical Secondary School
gen techschl=.
NOT AVAILABLE  

* SHIGHACA: This variable will be zero for higher technical education and one for higher academic education

NOT AVAILABLE
gen shighaca=. */

** Categorias educativas excluyentes

gen eduno_ci=.
replace eduno_ci=1 if aedu_ci==0 & P07B27A!=2
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupre_ci=.
replace edupre=1 if P07B27A==2
replace edupre=0 if P07B27A!=2 & P07B27A<9
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu_ci>=6 & aedu_ci!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu_ci!=.) 
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=12 & aedu_ci!=.)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=0 if (aedu_ci>=0 & aedu_ci<12) | (aedu_ci>12 & aedu_ci!=.) 
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=0 if (aedu_ci>=0 & aedu_ci<=12) | (aedu_ci>=17 & aedu_ci!=.)
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc_ci=1 if aedu_ci>=17 & aedu_ci!=.
replace eduuc_ci=0 if aedu_ci>=0 & aedu_ci<17
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"


gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (P07B27A==4 & (P07B27B==1 | P07B27B==2))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (P07B27A==4 & P07B27B==3)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (P07B27A==4 & (P07B27B==4 | P07B27B==5)) 
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

gen repiteult_ci=.
replace repiteult_ci=1 if P07B08A==2
replace repiteult_ci=0 if P07B08A==1
label var repiteult_ci "Personas que han repetido el ultimo grado"


* ASISTENCIA ESCOLAR
gen asiste_ci=.
replace asiste_ci=1 if P07B23==1
replace asiste_ci=0 if P07B23==2
replace asiste_ci=0 if P07B05==2
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

* POR QUE NO ASISTE (En 1998 la pregunta sobre causa de inasistencia es solo para personas de 7 a 14 años de edad)

gen pqnoasis_ci=.
replace pqnoasis_ci=P07B25 if P07B25>0 & P07B25<99
label var pqnoasis_ci "Razon principal por la cual ha abandonado o ha dejado de asistir a clases este año"
label define pqnoasis_ci 1 "Enfermedad" 2 "Falta de maestro" 3 "La madre trabaja" 4 "Oficios de la casa" 5 "Huelga magisterial" 6 "Falta de dinero" 7 "Trabajo" 8 "No le interesa" 9 "Mal tiempo" 10 "Embarazo" 11 "Migracion temporal" 12 "Otra"
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==6
replace pqnoasis1_ci= 2 if  pqnoasis_ci==7
replace pqnoasis1_ci= 3 if  pqnoasis_ci==1 | pqnoasis_ci==3
replace pqnoasis1_ci= 4 if  pqnoasis_ci==8
replace pqnoasis1_ci= 5 if  pqnoasis_ci==4 | pqnoasis_ci==10
replace pqnoasis1_ci= 9 if  pqnoasis_ci==12 | pqnoasis_ci==11 | pqnoasis_ci==10 | pqnoasis_ci==9 | pqnoasis_ci==2 | pqnoasis_ci==5

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

* EDUCACION PUBLICA OR PRIVATDA
/* 1998: needs to be used with attend variable since it only holds for those who are currently attending school (Las opciones son: No Asiste, Asiste a PUBLICO, Asiste a PRIVADO)

   2000: Solo para los que se han inscripto para el año escolar 2000:
         El plantel educativo donde se inscribio es: 
	 1 Ministerio de Educacion
	 2 PRONADE
	 3 Nuevas Escuelas Unitarias (NEUS)
         4 Otra Institucion del Gobbierno Central
	 5 Municipal
	 6 Cooperativa
	 7 Comunitario
	 8 Privado
	 9 ONG's
	 10 Otro

Se han considerado como PUBLICAS las opciones 1, 2, 3, 4 y 5 */
gen edupub_ci=0
replace edupub_ci=1 if ( P07B09==1 | P07B09==2 | P07B09==3 | P07B09==4 | P07B09==5 )
replace edupub_ci=. if ( P07B09==. | P07B09==99)
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************

/* ILLITERACY
gen alfabet=.
replace alfabet=1 if P07B01==1
replace alfabet=0 if ( P07B01==2 | P07B01==3 )

Estrictamente, la variable 'alfabet' esta creada igual que en 1998. 
Pero entre 1998 y 2000 han una modificacion en la pregunta. 
     En 1998 la encuesta pregunta: Sabe leer Y escribir? SI - NO
     En 2000 la encuesta pregunta: Sabe leer y escribir en español? Lee Y escribe - Solo lee - No lee ni escribe
 Este cambio podria explicar parte de la reduccion en la tasa de alfabetismo en Guatemala:
 1998: 79.3 %
 2000: 70.9 % 

* Si se incluyen a las personas que solo leen como alfabetas:
gen alfabet2=.
replace alfabet2=1 if ( P07B01==1 | P07B01==2 )
replace alfabet2=0 if P07B01==3 

* La tasa de alfabetismo aumenta a 76.8 % */



*****************************************************************
*** INGRESOS LABORALES (PARA PESONAS DE 5 AÑOS O MAS DE EDAD) ***
*****************************************************************

***************************
*** OCUPACION PRINCIPAL ***
***************************

****** INGRESOS MONETARIOS ******

* INGRESO INDEPENDIENTE (SE DEBE MENSUALIZAR)
gen iindep=.
replace iindep=(P10B15A*P10B15C)/12 if (P10B14==5 | P10B14==6) & P10B15C!=999
replace iindep=. if P10B15A==99999
replace iindep=. if P10B15B==1 & P10B15C>365 /* No existen mas de 365 dias al año */
replace iindep=. if P10B15B==2 & P10B15C>52 /* No existen mas de 52 semanas al año */
replace iindep=. if P10B15B==3 & P10B15C>24 /* No existen mas de 24 quincenas al año */
replace iindep=. if P10B15B==4 & P10B15C>12 /* No existen mas de 12 meses al año */
replace iindep=. if P10B15B==5 & P10B15C>4 /* No existen mas de 4 trimestres al año */
replace iindep=. if P10B15B==6 & P10B15C>2 /* No existen mas de 2 semestres al año*/
replace iindep=. if P10B15B==8 & P10B15C>1
replace iindep=0 if P10B15A==0
label var iindep "Ingreso de los Independientes ocupacion principal "

* SALARIO O SUELDO BRUTO MENSUAL (Incluye horas extra, comisiones, dietas y otas prestaciones de ley)
gen salario=.
replace salario=P10B22 if ( P10B14>=1 & P10B14<=4 ) & P10B22!=99999
label var salario "Salario o Sueldo Mensual Bruto ocupacion principal"

* BONO 14
gen bono14=.
replace bono14=P10B20B/12 if ( P10B14>=1 & P10B14<=4 ) & P10B20B!=99999 & P10B20A==1

* TIPS
gen tips=.
replace tips=P10B21B if ( P10B14>=1 & P10B14<=4 ) & P10B21B!=9999 & P10B21A==1

* AGUINALDO
gen aguin=.
replace aguin=P10B27B/12 if ( P10B14>=1 & P10B14<=4 ) & P10B27B!=99999 & P10B27A==1

****** INGRESO MONETARIO LABORAL ACTIVIDAD PRINCIPAL ******
egen ylmpri_ci=rsum(salario bono14 tips aguin) if ( P10B14>=1 & P10B14<=4 ), missing
replace ylmpri_ci=. if salario==. & bono14==. & tips==. & aguin==.
replace ylmpri_ci=iindep if ( P10B14==5 | P10B14==6 ) 
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****** INGRESOS NO MONETARIOS ******
gen alim=0
replace alim=P10B23B if ( P10B14>=1 & P10B14<=4 ) & P10B23B!=9999 & P10B23A==1
gen vivi=0
replace vivi=P10B24B if ( P10B14>=1 & P10B14<=4 ) & P10B24B!=9999 & P10B24A==1
gen ropa=0
replace ropa=P10B25B/12 if ( P10B14>=1 & P10B14<=4 ) & P10B25B!=9999 & P10B25A==1
gen transp=0
replace transp=P10B26B if ( P10B14>=1 & P10B14<=4 ) & P10B26B!=9999 & P10B26A==1
replace transp=P10B26B if P10B26B!=. & P10B26B!=9999 & (P10B26A==2 | P10B26A==3 | P10B26A==4 )  & ( P10B14>=1 & P10B14<=4 ) /* Hay valores raros en P10B26A como 2, 3 y 4 con valores positivos en P10B26B */

****** INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL ******
gen ylnmpri_ci=alim+vivi+ropa+transp
replace ylnmpri_ci=. if (P10B23A==. & P10B24A==. & P10B25A==. & P10B26A==.) | P10B14>4   
label var ylnmpri_ci " Ingreso Laboral NO Monetario ocupacion principal"

****************************
*** OCUPACION SECUNDARIA ***
****************************

/* P10C12 CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA
1 - 4 ASALARIADOS
5 - 6 INDEPENDIENTES
7 - 8 TRAB SIN PAGO
*/

gen categosec_ci=.
replace categosec_ci=1 if P10C12==5 & emp_ci==1
replace categosec_ci=2 if P10C12==6 & emp_ci==1
replace categosec_ci=3 if (P10C12>=1 & P10C12<=4) & emp_ci==1
replace categosec_ci=4 if (P10C12==7 | P10C12==8) & emp_ci==1
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"
label define categosec_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

****** INGRESOS MONETARIOS ******

* INGRESO INDEPENDIENTE (SE DEBE MENSUALIZAR)
gen iindep2=.
replace iindep2=(P10C13A*P10C13C)/12 if (P10C12==5 | P10C12==6) & P10C13C!=999
replace iindep2=. if P10C13A==99999
replace iindep2=. if P10C13B==1 & P10C13C>365 /* No existen mas de 365 dias al año */
replace iindep2=. if P10C13B==2 & P10C13C>52 /* No existen mas de 52 semanas al año */
replace iindep2=. if P10C13B==3 & P10C13C>24 /* No existen mas de 24 quincenas al año */
replace iindep2=. if P10C13B==4 & P10C13C>12 /* No existen mas de 12 meses al año */
replace iindep2=. if P10C13B==5 & P10C13C>4 /* No existen mas de 4 trimestres al año */
replace iindep2=. if P10C13B==6 & P10C13C>2 /* No existen mas de 2 semestres al año*/
replace iindep2=. if P10C13B==8 & P10C13C>1
replace iindep2=0 if P10C13A==0
label var iindep2 "Ingreso de los Independientes ocupacion secundaria "

* SALARIO O SUELDO BRUTO MENSUAL (Incluye horas extra, comisiones, dietas y otas prestaciones de ley)
gen salario2=.
replace salario2=P10C16 if ( P10C12>=1 & P10C12<=4 ) & P10C16!=99999
label var salario2 "Salario o Sueldo Mensual Bruto ocupacion secundaria"

* BONO 14
gen bono142=.
replace bono142=P10C14B/12 if ( P10C12==1 | P10C12==2 ) & P10C14B!=99999 & P10C14A==1

* TIPS
gen tips2=.
replace tips2=P10C15B if ( P10C12==1 | P10C12==2 ) & P10C15B!=9999 & P10C15A==1


****** INGRESO LABORAL MONETARIO ACTIVIDAD SECUNDARIA ******
egen ylmsec_ci=rsum(salario2 bono142 tips2) if ( P10C12>=1 & P10C12<=4 ) , missing
replace ylmsec_ci=. if salario2==. & bono142==. & tips2==. 
replace ylmsec_ci=iindep2 if ( P10C12==5 | P10C12==6 ) 


****** INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA ******
gen ylnmsec_ci=0
replace ylnmsec_ci=P10C17B/12 if P10C17A==1 & P10C17B!=99999 & ( P10C12>=1 & P10C12<=4 )
replace ylnmsec_ci=. if P10C17A==. | P10C17A==9

*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 
*****************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos"
****************************************************************************
*** INGRESO LABORAL MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
****************************************************************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
replace ylmpri_ci=. if iindep==. & salario==. & bono14==. & aguin==. & tips==.
replace ylm_ci=. if ylmpri_ci==. & iindep2==. & salario2==. & bono142==. & tips2==.
label var ylm_ci "Ingreso Laboral Monetario Total"


*******************************************************************************
*** INGRESO LABORAL NO MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
*******************************************************************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
replace ylnm_ci=. if P10B14>4 & P10C12>4
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

/* Existen personas que reportan ingresos en la actividad secundaria pero no en la principal: Son los trabajadores sin pago en la actividad principal a quienes no se les hacen las preguntas sobre ingresos de la actividad principal. */

****************************************************************************************************
*** OTRAS FUENTES DE INGRESOS RELACIONADAS CON EL TRABAJO(PARA PERSONAS DE 7 AÑOS O MAS DE EDAD) ***
****************************************************************************************************

*** PENSIONES ***
gen pensa=. /* dinero por pension alimentos */
replace pensa=P10E01B if P10E01B!=9999 & P10E01A==1

gen penso=. /* dinero por pension orfandad */
replace penso=P10E02B if P10E02B!=9999 & P10E02B!=9998 & P10E02A==1

gen pensj=. /* dinero por pension jubilacion */
replace pensj=P10E03B if P10E03B!=9999 & P10E03B!=9995 & P10E03A==1

*** AYUDAS EN DINERO ***
gen ayins=. /* ayuda de instituciones */
replace ayins=P10E06/12 if P10E06!=999999 & P10E04==1

* Modificacion MR - Octubre 2014: el valor de remesas es un monto anual por lo que hay que dividirse entre 12
/*
gen remesas_ci=. /* ayudas de familiares o amigos */
replace remesas_ci=P10E09 if P10E09!=999999 & P10E07==1
label var remesas_ci "Remesas Individuales"
*/
gen remesas_ci=. /* ayudas de familiares o amigos */
replace remesas_ci=P10E09/12 if P10E09!=999999 & P10E07==1
label var remesas_ci "Remesas Individuales"


egen ynlm_ci=rsum(pensa penso pensj ayins remesas_ci), missing
replace ynlm_ci=. if pensa==. & penso==. & pensj==. & ayins==. & remesas_ci==.
replace ynlm_ci=0 if P10E01A==2 & P10E02A==2 & P10E03A==2 & P10E04==2 & P10E07==2
label var ynlm_ci "Ingreso NO Laboral Monetario"

gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

************************************************************************************
*** INGRESOS DISTINTOS DEL TRABAJO EN DINERO O BIENES (MONTOS TOTALES DEL HOGAR) ***
************************************************************************************
* Quienes tienen 0 en estos ingresos es porque reportan que no los han recibido en el hogar * 
gen halquiler=0 /* Alquileres de edificios, casas, etc */
replace halquiler=monto101/12 if recibio101==1
gen hdividendo=0 /* Dividendos de acciones.... */
replace hdividendo=monto102/12 if recibio102==1
gen hindemn=0 /* Indemnizaciones por terminacion de obras o contrato */
replace hindemn=monto103/12 if recibio103==1
gen hindemn2=0 /* Indemnizacions de seguros medicos */
replace hindemn2=monto104/12 if recibio104==1
gen hindemn3=0 /* Indemnizaciones de seguros de vida */
replace hindemn3=monto105/12 if recibio105==1
gen hherencia=0 if monto106!=9999999 /* Herencias */
replace hherencia=monto106/12 if monto106!=9999999 & recibio106==1
gen hindemn4=0 /* Indemnizacione spor accidentes de trabajo */
replace hindemn4=monto107/12 if recibio107==1
gen hbeca=0  /* Becas y prestamos para estudios recibidos en efectivo */
replace hbeca=monto108/12 if recibio108==1
gen hbeca2=0 /* Becas pagadas directamente a las instituciones */
replace hbeca2=monto109/12 if recibio109==1
gen hazar=0 /* Loteria y juegos de azar */
replace hazar=monto110/12 if recibio110==1
gen hoy=0 /* Otros ingresos */
replace hoy=monto111/12 if monto111==1
gen hoy2=0 /* Otros ingresos */
replace hoy2=monto112/12 if monto112==1

*Modificado Mayra Sáenz - Febrero 2014
/*
gen ynl_ch=halquiler+hdividendo+hindemn+hindemn2+hindemn3+hherencia+hindemn4+hbeca+hbeca2+hazar+hoy+hoy2 if miembros_ci==1, missing
label var ynl_ch "Ingresos distintos del trabajo en dinero o bienes recibidos en el hogar" 
*/
egen ynl_ch= rsum(halquiler hdividendo hindemn hindemn2 hindemn3 hherencia hindemn4 hbeca hbeca2 hazar hoy hoy2) if miembros_ci==1, missing
label var ynl_ch "Ingresos distintos del trabajo en dinero o bienes recibidos en el hogar" 


*** FLAGS
gen byte nrylmpri_ci=0
replace nrylmpri_ci=1 if P10B15A==99999 | P10B22==99999
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

*** Dummy para el Hogar
capture drop nrylmpri_ch
sort idh_ch
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh) missing
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch) missing
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch) missing
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch) missing
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch) missing
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch) missing
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch) missing
label var autocons_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch) missing
label var remesas_ch "Remesas del Hogar (monetario + especies)"

replace ynlnm_ch=. if ynlnm_ci==.
replace autocons_ch=. if autocons_ci==.
replace ylm_ch =. if miembros_ci==0
replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0

*** INGRESO HORARIO DE TODOS LOS TRABAJOS ***
/* this is not accurate in the sense that if you have more than one job
you will have wage averaged over several jobs */
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*** INGRESO HORARIO DE LA OCUPACION PRINCIPAL ***
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"


drop iindep* salario* bono14* tips* aguin* alim vivi ropa transp hh pensa penso pensj ayins

* incorporacion de variables LMK

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*************
**pension_ci*
*************

gen pension_ci=1 if P10E03B>0 & P10E03B!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************
* ingreso por jubilacion-monto
gen ypen_ci=P10E03B if P10E03B>0 & P10E03B!=. & P10E03B!=9999 

label var ypen_ci "Valor de la pension contributiva"


*************
**tecnica_ci*
*************

gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"



******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if P10A09 ==4 & condocup_ci==3
replace categoinac_ci=2 if P10A09 ==3 & condocup_ci==3
replace categoinac_ci=3 if P10A09 ==5 & condocup_ci==3
recode categoinac_ci .= 0 if condocup_ci==3

label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
*******************
***formal_ci*******
*******************
	
gen formal_aux=1 if cotizando_ci==1
replace formal_aux=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
gen byte formal_ci=1 if formal_aux==1 & condocup_ci==1
recode formal_ci .=0 if condocup_ci==1
label var formal_ci "1=afiliado o cotizante / ocupados"
drop formal_aux

gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen rentaimp_ch =.
gen combust_ch=.
gen region_c=.


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



compress


saveold "`base_out'", replace


log close





