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
local ANO "1997"
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

**** revision August 21, 2006 (Victoria) 
codification of education dummy variables was changed because if not they
were not coherent with years of education.
old code can be seen below in the education section. 
****

**** revision October 27, 2006 (Victoria) 
codification of years of education was changed because there were no
observations with 0 years of education
old code can be seen below in the education section. 
****

*** revision August 2007 (Victoria) ***
With the unification Sociometro/Equis we decided to add two new varibales: howner and floor.
This variables were already created for Atlas

gen howner=(viviprop_ch==1 | viviprop==2);
replace howner=. if viviprop_ch==.;
gen floor=(piso_ch==1);
replace floor=. if piso_ch==.;

Also, the orginal data was replaced with the new Mecovi versions

*****
*/


use `base_in', clear
/************************************
 Del '92 al '98 es la misma encuesta. 
2000 es otra y2001-2003 otra.
*************************************/
/********************************/
/*    VARIABLES DEL HOGAR	*/
/********************************/


/*Nota técnica de URY: Para los cálculos del total país año 1997, se ponderan los datos de Montevideo e interior con los factores 0,469 y 0,531 respectivamente.*/

/*Ojo! Los identificadores del '92 no sirven para hacer panel. Se supone que a partir del '95 si*/
gen idh_ch=id_hogar
gen idp_ci=norden
gen factor_ch=factorex
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=1997
gen mes_c=0
forvalues j=1(1)12{
local h=`j'*4
local m=`h'-3
replace mes_c=`j' if semana>=`m'& semana<=`h'
}
gen relacion_ci=parentco
replace relacion_ci=4 if parentco==5
replace relacion_ci=5 if parentco==6
replace relacion_ci=6 if parentco==7
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
destring depto, replace
gen region_c=depto
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
gen aguared_ch=(agua==1)
gen aguadist_ch=insagua
replace aguadist_ch=0 if insagua==4
gen aguamala_ch=(agua==4) /*Cachimba=ojo de agua*/	
gen aguamide_ch=.
gen luz_ch=(luz==1 | luz==2)
gen luzmide_ch=.
gen combust_ch=(energia==1 | energia==2 | energia==3)
gen bano_ch=(servsan!=3)
gen banoex_ch=(usoserv==1)
replace banoex_ch=. if bano_ch==0
gen des1_ch=evacuac
gen des2_ch=des1_ch
replace des2_ch=0 if des2_ch==3
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if agua ==1 | agua ==3
replace aguamejorada_ch = 0 if agua ==2 | (agua >=4 & agua <=6)
		
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (servsan ==1 | servsan ==2) & usoserv ==1 & (evacuac  ==1 | evacuac  ==2)
replace banomejorado_ch = 0 if ((servsan ==1 | servsan ==2) & usoserv ==2) | servsan ==3 | ((servsan ==1 | servsan ==2) & usoserv ==1 & (evacuac==3))

gen dorm_ch=nrodorm
replace dorm_ch=. if nrodorm==9
gen cuartos_ch=tothabit
replace cuartos_ch=. if tothabit==99
gen cocina_ch=.
gen refrig_ch=(refri==1)
gen freez_ch=(refrizer==1)
gen auto_ch=(auto==1)
gen telef_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch=.
gen vivi2_ch=(tipoviv==1)
gen viviprop_ch=0 if tenviv==3
replace viviprop_ch=1 if tenviv==1
replace viviprop_ch=2 if tenviv==2
replace viviprop_ch=3 if tenviv==4 | tenviv==5
replace viviprop_ch=4 if tenviv==6
gen vivialq_ch=ealq if viviprop_ch==0
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"
sort idh_ch
by idh_ch: egen vivialqimp_ch=max(ylocali)

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


/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
gen factor_ci=factorex
gen sexo_ci=sexo
gen edad_ci=edad
replace edad_ci=. if edad==99
gen civil_ci=1 if estcivil==5
replace civil_ci=2 if estcivil==1 | estcivil==2
replace civil_ci=3 if estcivil==3
replace civil_ci=4 if estcivil==4
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

/******************************************************************************/
/*				VARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/
****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if pobpcoac>=11 & pobpcoac<=12
replace condocup_ci=2 if pobpcoac>=21 & pobpcoac<=23
replace condocup_ci=3 if pobpcoac>=30 & pobpcoac<=38
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen disse       =(atenmed==4) 
gen bps         =(atenmed==7) 
gen iamc        =(atenmed==5) 

gen afiliado_ci=(disse==1 | bps==1 | iamc==1)
replace afiliado_ci=. if disse==. & bps==. & iamc==.
label var afiliado_ci "Afiliado a la Seguridad Social"
drop disse bps iamc
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"
*Nota: solo seguro social publico, con el cual tenga derecho a pensiones en el futuro.

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 


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


*************
*ypen_ci*
*************
gen yjubbb=yjubpa
gen ypennn=ypenpa
egen ypen_ci=rsum(yjubbb ypennn)
replace ypen_ci=. if yjubbb==. & ypennn==.
drop yjubbb ypennn
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
replace cesante_ci=1 if trabante==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen salmm_ci = .
replace salmm_ci = 800   if mes_c==1 | mes_c==2  | mes_c==3  | mes_c==4
replace salmm_ci = 840   if mes_c==5 | mes_c==6  | mes_c==7  | mes_c==8
replace salmm_ci = 900   if mes_c==9 | mes_c==10 | mes_c==11 | mes_c==12
label var	salmm_ci	"Salario minimo legal 1997"
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

*gen emp_ci=(pobpcoac==11|pobpcoac==12)

gen ocupa_ci=.
replace ocupa_ci=1 if ocup>=0 & ocup<=98 & emp_ci==1
replace ocupa_ci=2 if ocup>=100 & ocup<=186 & emp_ci==1
replace ocupa_ci=3 if ocup>=200 & ocup<=290 & emp_ci==1
replace ocupa_ci=4 if ocup>=300 & ocup<=361 & emp_ci==1
replace ocupa_ci=5 if ocup>=900 & ocup<=990 & emp_ci==1
replace ocupa_ci=6 if ocup>=400 & ocup<=453 & emp_ci==1
replace ocupa_ci=7 if ocup>=500 & ocup<=792 & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=9 if ocup>=800 & ocup<=890 & emp_ci==1
/*No hay una categoria para las fuerzas armadas*/
gen ramaaux=substr(string(ramactv),1,1)
gen rama_ci=real(ramaaux)
drop ramaaux
replace rama_ci=. if rama_ci==0
gen horaspri_ci=hrsing
replace horaspri_ci=. if hrsing==99
replace horaspri_ci=. if emp_ci==0
gen horastot_ci=tothrs
replace horastot_ci=. if tothrs==99
replace horastot_ci=. if horaspri_ci==.
/*
gen durades_ci=tpobus/4 if pobpcoac==21 | pobpcoac==23 /*De los que estan desempleados cuanto hace (en meses) que buscan*/
replace durades_ci=. if tpobus==99
gen antiguedad_ci=ctosanos
replace antiguedad_ci=. if ctosanos==99
*/

gen desalent_ci=(pobpcoac==37)


* Modificacion MGD 06/23/2014: horas de la actividad principal.
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30 & deseamas==1)


gen durades_ci=tpobus/4.3  if tpobus>0
replace durades_ci=. if tpobus==99

* ctosanos  antiguedad en anos
* ctosmese antiguedad en meses
replace ctosanos =. if ctosanos==99
replace ctosmese=. if ctosmese ==99 

gen ctosmesem = ctosmese/12
egen antiguedad_ci=rsum(ctosanos ctosmesem), missing
*Mod 2015,11 MLO
*gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & deseamas==2)
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30 & deseamas==2)
replace tiempoparc_ci=. if emp_ci==0
gen categopri_ci=1 if catego==4
replace categopri_ci=2 if catego==5 | catego==6 | catego==3
replace categopri_ci=3 if catego==1 | catego==2 
replace categopri_ci=4 if catego==7 | catego==8
replace categopri_ci=. if emp_ci!=1
gen categosec_ci=.
gen contrato_ci=.
gen segsoc_ci=.
gen nempleos_ci= 1 if nrocup==1
replace nempleos_ci=2 if nrocup>1
/*gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & trabmeno==1 & tamest>=1 & tamest<=5
replace firmapeq_ci=0 if emp_ci==1 & (trabmeno==1 & tamest>5)|trabmeno==2*/
gen spublico_ci=(emp_ci==1 & catego==2)

*Genera la variable para empresas pequeñas
gen tamemp_ci=1 if tamest>=1 & tamest<=4 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if tamest>=5 & tamest<=9
*Empresas grandes
replace tamemp_ci=3 if trabmeno==2 & tamemp_ci!=1 & tamemp_ci!=2
label define tamemp_ci 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamemp_ci
tab tamemp_ci [iw=factor_ci]

*Genera la variable para clasificar a los inactivos
*Jubilados y pensionados
*drop categoinac_ci
gen categoinac_ci=1 if pensioni==1 | jubilado==1
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if estudian==1
*Quehaceres del Hogar
replace categoinac_ci=3 if quehog==1
*Otra razon
replace categoinac_ci=4 if rentista==1 | incapaci==1 | otro==1
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
tab categoinac_ci [iw=factor_ci]


*******************
***formal***
*******************
/*gen formal=1 if cotizando_ci==1
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
label var formal_ci "1=afiliado o cotizante / PEA"*/

*Modificación Mayra Sáenz - Septiembre 2014
*También se incluye como formales a los empleados públicos o si son beneficiarios de disse, 
*pues en este año no se dispone de la variable cotizando
gen formal=1 if cotizando_ci==1
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1
replace formal=1 if (catego==2 | atenmed==4) & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"


*************
***INGRESOS
*************
*************
* ylmpri_ci *
*************

/* Primaria		
privados - sueldos - principal	ypsyspr	
privados - horas extras - principal	ypextpr	
privados - beneficios sociales - princip	ypbenpr	
privados - aguinaldo - principal	ypagupr	
privados - salario vacacional - principa	ypvacpr	
privados - propinas - principal	yppropr	
p£blicos - sueldos - principal	ypsyspu	
p£blicos - horas extras - principal	ypextpu	
p£blicos - beneficios sociales - princip	ypbenpu	
p£blicos - aguinaldo - principal	ypagupu	
p£blicos - salario vacacional - principa	ypvacpu	
p£blicos - propinas - principal	yppropu	
cuenta propia sin local - dinero	ypctasl	
cuenta propia sin local - beneficios soc	ypbensl	
cuenta propia con local - dinero	ypctacl	
cuenta propia con local - beneficios soc	ypbencl	
patr¢n - dinero - principal	yppatef	
patr¢n - utilidades dinero - principal	yputief	(ultimos 12 meses)
cooperado - dinero - principal	ypcopef	
cooperado - beneficios sociales - princi	ypcopben	
cooperado - utilidades dinero - principa	ypcoputf	(ultimos 12 meses)
*/		
          
gen yputiefm = yputief/12
gen ypcoputfm = ypcoputf/12

egen ylmpri_ci=rsum(ypsyspr ypextpr ypbenpr ypagupr ypvacpr yppropr ypsyspu ypextpu ypbenpu ypagupu ypvacpu yppropu ypctasl ypbensl ypctacl ypbencl yppatef yputiefm ypcopef ypcopben ypcoputfm) if emp_ci==1, missing

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
/* Especies primaria		
		
privados - especies - principal	ypesppr	
p£blicos - especies - principal	ypesppu	
cuenta propia sin local - especies	ypespsl	(autoconsumo)
cuenta propia con local - especies	ypespcl	(autoconsumo)
patr¢n - especies - principal	yppates	
patr¢n - utilidades especies - principal	yputies	(ultimos 12 meses)
cooperado - especies - principal	ypcopes	(autoconsumo)
cooperado - utilidades especies - princi	ypcoputs	(ultimos 12 meses)
*/		

gen yputiesm = yputies/12
gen ypcoputsm = ypcoputs/12	
	egen ylnmpri_ci= rsum(ypesppr ypesppu ypespsl ypespcl yppates yputiesm ypcopes ypcoputsm) if emp_ci==1, missing
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

	***************
	***ylmsec_ci***
	***************
	 /* Secundaria		
privados - sueldos - otras	yssyspr	
privados - horas extras - otras	ysextpr	
privados - beneficios sociales - otras	ysbenpr	
privados - aguinaldo - otras	ysagupr	
privados - salario vacacional - otras	ysvacpr	
privados - propinas - otras	yspropr	
p£blicos - sueldos - otras	yssyspu	
p£blicos - horas extras - otras	ysextpu	
p£blicos - beneficios sociales - otras	ysbenpu	
p£blicos - aguinaldo - otras	ysagupu	
p£blicos - salario vacacional - otras	ysvacpu	
p£blicos - propinas - otras	yspropu	
cuenta propia sin local - dinero	ysctasl	
cuenta propia sin local - beneficios soc	ysbensl	
cuenta propia con local - dinero	ysctacl	
cuenta propia con local - beneficios soc	ysbencl	
patr¢n - dinero - otras	yspatef	
patr¢n - utilidades dinero - otras	ysutief	(ultimos 12 meses)
cooperado - dinero - otras	yscopef	
cooperado - beneficios sociales - otras	yscopben	
cooperado - utilidades dinero - otras	yscoputf	(ultimos 12 meses)
*/		


gen ysutiefm = ysutief/12
gen yscoputfm = yscoputf/12
	
	
	egen ylmsec_ci=rsum(yssyspr ysextpr ysbenpr ysagupr ysvacpr yspropr yssyspu ysextpu ysbenpu ysagupu ysvacpu yspropu ysctasl ysbensl ysctacl ysbencl yspatef ysutiefm yscopef yscopben yscoputfm) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
	****************
	***ylnmsec_ci***
	****************
	 /* Especies secundaria		
privados - especies - otras	ysesppr	
p£blicos - especies - otras	ysesppu	
cuenta propia sin local - especies	ysespsl	(autoconsumo)
cuenta propia con local - especies	ysespcl	(autoconsumo)
patr¢n - especies - otras	yspates	
patr¢n - utilidades especies - otras	ysuties	(ultimos 12 meses)
cooperado - especies - otras	yscopes	(autoconsumo)
cooperado - utilidades especies - otras	yscoputs	(ultimos 12 meses)
*/		


gen ysutiesm = ysuties/12
gen yscoputsm = yscoputs/12
	
	egen ylnmsec_ci=rsum(ysesppr ysesppu ysespsl ysespcl yspates ysutiesm yscopes yscoputsm) if emp_ci==1, missing
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

	egen ylmotros_ci= rsum(ypsyspr ypextpr ypbenpr ypagupr ypvacpr yppropr ypsyspu ypextpu ypbenpu ypagupu ypvacpu yppropu ypctasl ypbensl ypctacl ypbencl yppatef yputiefm ypcopef ypcopben ypcoputfm yssyspr ysextpr ysbenpr ysagupr ysvacpr yspropr yssyspu ysextpu ysbenpu ysagupu ysvacpu yspropu ysctasl ysbensl ysctacl ysbencl yspatef ysutiefm yscopef yscopben yscoputfm) if emp_ci==0, missing
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 
	
	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral monetario otros trabajos para todos los años
	
	
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci=rsum(ypesppr ypesppu ypespsl ypespcl yppates yputiesm ypcopes ypcoputsm ysesppr ysesppu ysespsl ysespcl yspates ysutiesm yscopes yscoputsm) if emp_ci==0, missing
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 
	
	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral no monetario otros trabajos para todos los años
	
	************
	***ylm_ci***
	************
	egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
	replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==. & ylmotros_ci==.
	label var ylm_ci "Ingreso laboral monetario total"  

	*************
	***ylnm_ci***
	*************
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  

	*************
	***ynlm_ci***
	*************
 /* Otros ingresos monetarios		
jubilaciones del pa¡s	yjubpa	
pensiones del pa¡s	ypenpa	
jubilaciones del exterior	yjubex	
pensiones del exterior	ypenex	
becas u otros subsidios del pa¡s	ybecpa	
becas u otros subsidios del exterior	ybecex	
ayudas familiares del pa¡s	yayupa	
ayudas familiares del exterior	yayuex	
alquileres o arrendamientos del pa¡s	yarrepa	
alquileres o arrendamientos del exterior	yarrex	
intereses y otros ingresos del pa¡s	yintpa	(ultimos 12 meses)
intereses y otros ingresos del exterior	yintex	(ultimos 12 meses)
*/		


gen yintpam = yintpa/12 
gen yintexm = yintex/12

egen ynlm_ci=rsum(yjubpa ypenpa yjubex ypenex ybecpa ybecex yayupa yayuex yarrepa yarrex yintpam yintexm), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci= yayuex
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
* Act. principal ypespsl ypespcl ypcopes
* Act. secundaria ysespsl ysespcl yscopes
	
	egen autocons_ci= rsum(ypespsl ypespcl ypcopes ysespsl ysespcl yscopes), missing
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
/****************************************YLMPRI_CI**********************************************/
egen ylmpri_ci=rsum(ypsyspr ypextpr ypbenpr ypagupr ypvacpr yppropr) if catego==1 & emp_ci==1
replace ylmpri_ci=. if ypsyspr==. & ypextpr==. & ypbenpr==. & ypagupr==. & ypvacpr==. & yppropr==. & catego==1  
egen ylmpri_ci2=rsum(ypsyspu ypextpu ypbenpu ypagupu ypvacpu yppropu) 
replace ylmpri_ci2=. if ypsyspu==. & ypextpu==. & ypbenpu==. & ypagupu==. & ypvacpu==. & yppropu==. 
replace ylmpri_ci=ylmpri_ci2 if catego==2 & emp_ci==1

gen ypcoputfmo=ypcoputf/12 if emp_ci==1/*ypcoputf esta definido en una base anual!*/
egen ylmpri_ci3=rsum(ypcopef ypcoputfmo) 
replace ylmpri_ci3=. if ypcopef==. & ypcoputfmo==. 
replace ylmpri_ci=ylmpri_ci3 if catego==3 & emp_ci==1

gen yputiefmo=yputief/12 if emp_ci==1/*yputief esta definido en una base anual!*/
egen ylmpri_ci4=rsum(yppatef yputiefmo) 
replace ylmpri_ci4=. if yppatef==. & yputiefmo==. 
replace ylmpri_ci=ylmpri_ci4 if catego==4 & emp_ci==1

egen ylmpri_ci5=rsum(ypctasl ypbensl) 
replace ylmpri_ci5=. if ypctasl==. & ypbensl==. 
replace ylmpri_ci=ylmpri_ci5 if catego==5 & emp_ci==1

egen ylmpri_ci6=rsum(ypctacl ypbencl) 
replace ylmpri_ci6=. if ypctacl==. & ypbencl==.
replace ylmpri_ci=ylmpri_ci6 if catego==6 & emp_ci==1

drop ylmpri_ci2 ylmpri_ci3 ylmpri_ci4 ylmpri_ci5 ylmpri_ci6


/******************************************YLMPRI1_CI**********************************************/
gen ylmpri1_ci=ylmpri_ci if emp_ci==1
egen ylmpri1_ci1=rsum(ypcopef ypcopben ypcoputfmo) 
replace ylmpri1_ci1=. if ypcopef==. & ypcopben==. & ypcoputfmo==. 
replace ylmpri1_ci=ylmpri1_ci1 if catego==3 & emp==1
drop ylmpri1_ci1

/*********************************************YLNMPRI_CI******************************************/
gen ypcoputsmo=ypcoputs/12 if emp_ci==1/*ypcoputs esta definido en una base anual!*/
egen ylnmpri_ci=rsum(ypcopes ypcoputsmo) if catego==3 & emp_ci==1
replace ylnmpri_ci=. if ypcopes==. & ypcoputsmo==. & catego==3
replace ylnmpri_ci=ypesppr if catego==1 & emp_ci==1
replace ylnmpri_ci=ypesppu if catego==2 & emp_ci==1

gen yputiesmo=yputies/12 if emp_ci==1/*yputies esta definido en una base anual!*/
egen ylnmpri_ci4=rsum(yppates yputiesmo) 
replace ylnmpri_ci4=. if yppates==. & yputiesmo==. 
replace ylnmpri_ci=ylnmpri_ci4 if catego==4 & emp==1

replace ylnmpri_ci=ypespsl if catego==5 & emp_ci==1
replace ylnmpri_ci=ypespcl if catego==6 & emp_ci==1

drop ylnmpri_ci4
/*******************************************************************************************************/

gen ylmsec_ci=.
gen ylnmsec_ci=.

gen ysutiefmo=ysutief/12 if emp_ci==1/*ysutief esta definido en una base anual!*/
gen yscoputfmo=yscoputf/12 if emp_ci==1/*yscoputf esta definido en una base anual!*/
egen ylmaux=rsum(yssyspr ysextpr ysbenpr ysagupr ysvacpr yspropr yssyspu ysextpu ysbenpu ysagupu ysvacpu yspropu ysctasl ysbensl ysctacl ysbencl yspatef ysutiefmo yscopef yscopben yscoputfmo) if emp_ci==1
replace ylmaux=. if yssyspr==. & ysextpr==. & ysbenpr==. & ysagupr==. & ysvacpr==. & yspropr==. & yssyspu==. & ysextpu==. & ysbenpu==. & ysagupu==. & ysvacpu==. & yspropu==. & ysctasl==. & ysbensl==. & ysctacl==. & ysbencl==. & yspatef==. & ysutiefmo==. & yscopef==. & yscopben==. & yscoputfmo==.

gen ysutiesmo=ysuties/12 if emp_ci==1/*ysuties esta definido en una base anual!*/
gen yscoputsmo=yscoputs/12 if emp_ci==1/*yscoputs esta definido en una base anual!*/
egen ylnmaux=rsum(ysesppr ysesppu ysespsl ysespcl yspates ysutiesmo yscopes yscoputsmo) if emp_ci==1
replace ylnmaux=. if ysesppr==. & ysesppu==. & ysespsl==. & ysespcl==. & yspates==. & ysutiesmo==. & yscopes==. & yscoputsmo==.

gen nrylmpri_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmaux)
replace ylm_ci=. if ylmpri_ci==. & ylmaux==.

egen ylm1_ci=rsum(ylmpri1_ci ylmaux)
replace ylm1_ci=. if ylmpri1_ci==. & ylmaux==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmaux)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmaux==.
drop ylnmaux ylmaux

gen yintpamo=yintpa/12 /*yintpa esta definido en una base anual!*/
gen yintexmo=yintex/12 /*yintex esta definido en una base anual!*/
egen ynlm_ci=rsum(yjubpa ypenpa ybecpa yayupa yarrepa yintpamo yjubex ypenex ybecex yayuex yarrex yintexmo)
replace ynlm_ci=. if yjubpa==. & ypenpa==. & ybecpa==. & yayupa==. & yarrepa==. & yintpamo==. & yjubex==. & ypenex==. & ybecex==. & yayuex==. & yarrex==. & yintexmo==.
gen ynlnm_ci=.

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if relacion_ci!=6
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0
gen rentaimp_ch=vivialqimp_ch
gen autocons_ch=.
gen autocons_ci=.

egen remesas_ci=rsum(yjubex ypenex ybecex yayuex yarrex yintexmo)
replace remesas_ci=. if yjubex==. & ypenex==. & ybecex==. & yayuex==. & yarrex==. & yintexmo==.

drop ypcoputfmo yputiefmo ypcoputsmo yputiesmo ysutiefmo yscoputfmo ysutiesmo yscoputsmo yintpamo yintexmo
sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6

*/
/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/
gen aedu_ci=.
replace aedu_ci=0 if nivel==8 | nivel==0 
replace aedu_ci=ultano if nivel==1
replace aedu_ci=0 if nivel==1 & ultano==0
replace aedu_ci=6 if nivel==1 & ultano>6
replace aedu_ci=6+ultano if nivel==2 
replace aedu_ci=6 if (nivel==2 & ultano==0) 
replace aedu_ci=10+ultano if nivel==3
replace aedu_ci=10 if nivel==3 & ultano==0
replace aedu_ci=12 if nivel==3 & ultano>3
replace aedu_ci=9+ultano if nivel==4
replace aedu_ci=9 if nivel==4 & ultano==0
replace aedu_ci=12+ultano if nivel==5 | nivel==6 | nivel==7 
replace aedu_ci=12 if (nivel==5 & ultano==0) | (nivel==6 & ultano==0)
replace aedu_ci=. if nivel==9 | nivel==.



/*Estamos droppeando a los del "Instituto Militar" porque son muy pocos (algo asi como el
0.2% de la muestra) y no esta claro en que categoría deberían entrar*/
/*U.T.U (nivel=4)>><< Enseñanza técnica. Aunque se llama Universidad Tecnica de Uruguay no esta contado como una carrera universitaria, sino que, dado
que solo pide como requisito el primer ciclo, es como una enseñanza secundaria de segundo ciclo que puede durar muchos años.*/



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



gen edupre_ci=(nivel==8)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"
gen byte eduac_ci=.
replace eduac_ci=1 if (nivel==6)
replace eduac_ci=0 if (nivel==4 | nivel==5 | nivel==7)

foreach var of varlist edu* {
replace `var'=. if aedu_ci==.| finalizo==0
}
gen asiste_ci=(asist==1)
gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci =.

gen repite_ci=.

******************************
*	repiteult_ci 
******************************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"
gen edupub_ci=(tipoense==1)
label var  aedu_ci "Anios de Educacion"
cap rename ocup ocuprinc   
****************
***tecnica_ci **
****************
gen tecnica_ci=.
replace tecnica_ci=1 if nivel==4
replace tecnica_ci=0 if tecnica_ci ~=1 & ( nivel!=9)
label var tecnica_ci "1=formacion terciaria tecnica"               

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

	
