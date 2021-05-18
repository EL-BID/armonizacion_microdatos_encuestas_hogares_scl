
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

local PAIS NIC
local ENCUESTA EMNV
local ANO "2005"
local ronda m7_m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: EMNV
Round: Febrero-Junio
Autores: Yessenia Loayza
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"


**********************
* Variables de Hogar *
**********************

gen factor_ch=factor // Factor de expansion

gen idh_ch=id_hogar

gen idp_ci=s2p00

gen zona_c=i06
replace zona_c=0 if i06==2

gen pais_c="NIC"

gen anio_c=2005

gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" | pais=="PAN" | pais=="DOM" | pais=="CRI" | pais=="BLZ" | pais=="GTM" | pais=="SLV" | pais=="HON" | pais=="NIC"
replace region_BID_c=2 if pais=="BAH" | pais=="BAR" | pais=="GUY" | pais=="JAM" | pais=="SUR" | pais=="TOT"
replace region_BID_c=3 if pais=="ECU" | pais=="COL" | pais=="PER" | pais=="VEN" | pais=="BOL" 
replace region_BID_c=4 if pais=="ARG" | pais=="BRA" | pais=="CHL" | pais=="PRY" | pais=="URU" 
replace region_BID_c=5 if pais=="HAI"
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

gen mes_c=mesent1

gen relacion_ci=s2p2
replace relacion_ci=4 if s2p2>=5 & s2p2<=8
replace relacion_ci=5 if s2p2==9 | s2p2==11
replace relacion_ci=6 if s2p2==10

/*************************************
Variables de Infraestructura del Hogar
**************************************/

gen aguared_ch=(s1p20a==1 | s1p20a==2) /*Esta definido como acceso a agua de red!*/

gen aguadist_ch=0
replace aguadist_ch=1 if s1p20a==1
replace aguadist_ch=2 if s1p20a==2 & s1p24m<=100
replace aguadist_ch=3 if s1p20a==2 & (s1p24m>100 & s1p24m!=.)
replace aguadist_ch=3 if s1p20a>=3 & s1p20a<=10

gen aguamala_ch=(s1p20a>=3 & s1p20a<=10)

gen aguamide_ch=(s1p27==1)

gen luz_ch=(s1p36==1)

gen luzmide_ch=(s1p37==1)

gen cocina_ch=(s1p41==1)

gen combust_ch=(s1p42==2 | s1p42==3 | s1p42==5)

gen bano_ch=(s1p32!=6) /*Esta definido como si posee servicio higienico: inodoro o letrina*/

gen banoex_ch=.


gen des1_ch=0
replace des1_ch=1 if s1p32==3 | s1p32==4 /*Red o pozo septico*/
replace des1_ch=2 if s1p32==1 | s1p32==2 /*Pozo comun  - Letrina */
replace des1_ch=3 if s1p32==5 /*Superficie*/

gen des2_ch=0
replace des2_ch=1 if s1p32==3 | s1p32==4 /*Red o pozo septico*/
replace des2_ch=2 if s1p32==1 | s1p32==2 /*Pozo comun  - Letrina */
gen dessup=(s1p32==5) /*Superficie*/

gen piso_ch=(s1p6==1 | s1p6==2 | s1p6==4)
replace piso_ch=2 if s1p6==3 | s1p6==6 /*Vale 0 si es piso de tierra, 1 si es permanente y 2 si es otros (o de ladrillo de barro)*/

gen pared_ch=(s1p5==1 | s1p5==2 |s1p5==4 |s1p5==6 |s1p5==7 |s1p5==8)
replace pared_ch=2 if s1p5==10 /*Otros*/

gen techo_ch=(s1p7==1 | s1p7==3)
replace techo_ch=2 if s1p7==6 /*Otros*/

gen resid_ch=0 /*Recoleccion*/
replace resid_ch=1 if s1p34==2 | s1p34==3 /*Quemada o enterrada*/
replace resid_ch=2 if s1p34==4 /*Tirada*/
replace resid_ch =3 if s1p34!=1 & s1p34!=2 & s1p34!=3 & s1p34!=4 /*"Otros: llevada al lugar"*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .

gen dorm_ch=s1p13

gen cuartos_ch = s1p14

gen vivi1_ch=(s1p3==1) /*Casa*/
replace vivi1_ch=2 if s1p3==3/*Departamento*/
replace vivi1_ch=3 if vivi1_ch==0

gen vivi2_ch = (s1p3==1|s1p3==3)

gen viviprop_ch = 0 if s1p15 == 1 /*Alquilada*/
replace viviprop_ch = 1 if s1p15==3 | s1p15==4 /*Propia*/
replace viviprop_ch = 2 if s1p15==2 /*Pagando*/
replace viviprop_ch = 4 if s1p15!=1 & s1p15!=2 & s1p15!=3 & s1p15!=4

gen vivitit_ch = (s1p15==4) /*Con titulo que certifique la propiedad de la vivienda*/

gen vivialq_ch= s1p16a /* falta la parte reportada en dolares */
gen vivialqimp_ch= s1p19a /* falta la parte reportada en dolares */
gen telef_ch=(s1p49a==1 | s1p49a==3)
gen cel_ch=  (s1p49a==2 | s1p49a==3)

gen freez_ch=.
****************
**internet_ch***
****************

gen internet_ch=.

/*********************
Variables Demograficas
*********************/
gen factor_ci=factor

gen sexo_ci=s2p3

gen edad_ci=s2p4a // años cumplidos

gen civil_ci=1 if s2p6==6 | s2p6==7
replace civil_ci=2 if s2p6==1 | s2p6==2
replace civil_ci=3 if s2p6==3 | s2p6==4
replace civil_ci=4 if s2p6==5 

gen jefe_ci=(s2p2==1)

sort idh_ch
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2) 
by idh_ch: egen byte nhijos_ch=sum(relacion_ci==3)
by idh_ch: egen byte notropari_ch=sum(relacion_ci==4)
by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)
by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)

gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0)  /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/

sort idh_ch
by idh_ch:egen byte nmiembros_ch=sum((relacion_ci>0 & relacion_ci<5)|s2p2==9) if relacion_ci~=6 
by idh_ch:egen byte nmayor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum(((relacion_ci>0 & relacion_ci<5)|s2p2==9) & (edad_ci<1)) /*Hay que tener en cuenta que en /// 
este año, se pregunto si existen pensionistas en la casa, que tecnicamente son "otros no parientes", pero que en la practica ///
no deben ser incluídos en las variables de hogar (eg: _ch)*/

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"


/***************************
Variables de Demanda Laboral
****************************/

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if s5p1==1 | s5p2==1 |  (s5p3>=1 & s5p3<=7)
replace condocup_ci=2 if s5p4==1 /*utilizo como referencia la ultimas semana lo cual cambia con la metodologia del 2009. PEro si utlizo el ultimo mes el indicador cae a 0.8*/
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/
* Alternativa 2 con variables originales tomando en cuenta la definicion de armonizacion MGD 06/05/2014
gen condocup_ci=.
replace condocup_ci=1 if s5p1==1 | s5p2==1 | (s5p3>=1 & s5p3<=7)
replace condocup_ci=2 if condocup_ci!=1 & (s5p4==1 | s5p5==1)
recode condocup_ci .=3 if edad_ci>=5
recode condocup_ci .=4 if edad_ci<5
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

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
replace cotizando_ci=1 if (s5p30a==1 | s5p48a==1 | s5p66a ==1) & cotizando_ci==0 /*solo a ocupados*/
*considero primer y segundo trabajo. NO el trabajo de los ultimos 12 meses

****************
*cotizapri_ci***
****************
gen cotizapri_ci=.
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=.
label var cotizasec_ci "Cotizante a la Seguridad Social en actividad sec."

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*************
**pension_ci*
*************
gen pension_ci=(i_pensi>0 & i_pensi!=.)
label var pension_ci "1=Recibe pension contributiva"
*No encuentro la variable original

*************
*   ypen_ci *
*************
gen ypen_ci=i_pensi if pension_ci==1
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
*No encuentro la variable original

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
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if s5p12==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =linea_ge 
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =linea_ex 
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*Se utiliza clasificacion CUAEN

gen rama_ci=.
replace rama_ci=1 if (s5p15>=100& s5p15<=599) & emp_ci==1
replace rama_ci=2 if (s5p15>=1010 & s5p15<=1499) & emp_ci==1
replace rama_ci=3 if (s5p15>=1510 & s5p15<=3729) & emp_ci==1
replace rama_ci=4 if (s5p15>=4010 & s5p15<=4199) & emp_ci==1
replace rama_ci=5 if (s5p15>=4510 & s5p15<=4559) & emp_ci==1
replace rama_ci=6 if (s5p15>=5010 & s5p15<=5529) & emp_ci==1
replace rama_ci=7 if (s5p15>=6010 & s5p15<=6429) & emp_ci==1
replace rama_ci=8 if (s5p15>=6510 & s5p15<=7029) & emp_ci==1
replace rama_ci=9 if (s5p15>=7111 & s5p15<=9800) & emp_ci==1


*************
**salmm_ci***
*************
/*generat salmm_ci=.
replace salmm_ci=769.4+1191.4/2   if rama_ci==1
replace salmm_ci=1439.8           if rama_ci==2
replace salmm_ci=1032.7+1298.4/2  if rama_ci==3
replace salmm_ci=1474.3           if rama_ci==4  
replace salmm_ci=1838.4           if rama_ci==5 
replace salmm_ci=1474.3           if rama_ci==6 | rama_ci==7
replace salmm_ci=1838.4           if rama_ci==8
replace salmm_ci=1114.4+1013.3/2  if rama_ci==9
replace salmm_ci=(769.40+1191.40+1439.8+1032.7+1298.4+1474.3+1838.4+1474.3+1474.3+1838.4+1114.4+1013.3)/12  if salmm_ci==. */

* 2015 MGD: actualizan salarios minimos segun la ctividad
generat salmm_ci=.
replace salmm_ci=966.9   if rama_ci==1
replace salmm_ci=1420.2  if rama_ci==2
replace salmm_ci=1149.9  if rama_ci==3
replace salmm_ci=1454.1  if rama_ci==4  
replace salmm_ci=1813.2  if rama_ci==5 
replace salmm_ci=1454.1  if rama_ci==6 | rama_ci==7
replace salmm_ci=1813.2  if rama_ci==8
replace salmm_ci=1099.2  if rama_ci==9
replace salmm_ci=1224.4  if salmm_ci==. 

label var salmm_ci "Salario minimo legal"

*Se utiliza ciuo88
/*
gen ocupa_ci=.
replace ocupa_ci=1 if real(substr(string(s5p14),1,1))==2 | real(substr(string(s5p13),1,1))==3
replace ocupa_ci=2 if real(substr(string(s5p14),1,1))==1
replace ocupa_ci=3 if real(substr(string(s5p14),1,1))==4
replace ocupa_ci=4 if real(substr(string(s5p14),1,2))==52
replace ocupa_ci=5 if real(substr(string(s5p14),1,2))==51
replace ocupa_ci=6 if real(substr(string(s5p14),1,1))==6
replace ocupa_ci=7 if real(substr(string(s5p14),1,1))==7 | real(substr(string(s5p13),1,1))==8
replace ocupa_ci=8 if real(substr(string(s5p14),1,1))==0
replace ocupa_ci=9 if real(substr(string(s5p14),1,1))==9
replace ocupa_ci=. if emp_ci==0
*/
*Modificacion MGD 07/07/2014: reclasificacion para corregir la tendencia.
tab s5p14
tostring s5p14, replace force
gen digito = "0"
egen x = concat(digito s5p14) if length(s5p14)==3
replace s5p14=x if length(s5p14)==3
gen ocupa= real(substr(s5p14,1,2))

gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=34)  & emp_ci==1
replace ocupa_ci=2 if (ocupa>=11 & ocupa<=13)  & emp_ci==1
replace ocupa_ci=3 if (ocupa>=41 & ocupa<=42)  & emp_ci==1
replace ocupa_ci=4 if (ocupa==52)              & emp_ci==1
replace ocupa_ci=5 if (ocupa==51)              & emp_ci==1
replace ocupa_ci=6 if (ocupa>=61 & ocupa<=62)  & emp_ci==1
replace ocupa_ci=7 if (ocupa>=71 & ocupa<=83)  & emp_ci==1
replace ocupa_ci=8 if (ocupa==1)               & emp_ci==1
replace ocupa_ci=9 if (ocupa>=91 & ocupa<=94)  & emp_ci==1

gen horaspri_ci=s5p18
replace horaspri_ci=. if emp_ci==0 | s5p18>168

gen horastot_ci=s5p49
replace horastot_ci=. if emp_ci==0


*************
*tamemp_ci
*************
*Nicaragua Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50
/*
gen tamemp_ci=s5p19
label define tamemp_ci 1 "1 persona" 2 "2-4 personas" 3 "5 personas" ///
4 "6-20 personas" 5 "21-100 personas" 6 "101 y más personas"
label var tamemp_ci "# empleados en la empresa de la actividad principal"

s5p19:
           1 trabaja solo
           2 2 a 4
           3 5
           4 6 a 20
           5 21 a 100
           6 101 y más
           8 n/s
           9 ignorado
*/

gen tamemp_ci = 1 if s5p19>=1 & s5p19<=3
replace tamemp_ci = 2 if (s5p19>=4)
replace tamemp_ci = 3 if (s5p19>=5 & s5p19<=6)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if (( s5p7==3 | s5p7==4) & condocup_ci==3)
replace categoinac_ci = 2 if  ( s5p7==1 & condocup_ci==3)
replace categoinac_ci = 3 if  ( s5p7==5 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

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

************************
*** HOUSEHOLD INCOME ***
************************
*Programo neuvamente las variables: ylmpri_ci, ylnmpri_ci, ylmsec_ci, y lnmsec_ci // Yessenial
gen salario= .
replace salario=s5p20a*4.3  if s5p20b==1
replace salario=s5p20a*2  if s5p20b==2 | s5p20b==3
replace salario=s5p20a    if s5p20b==4
replace salario=s5p20a/3  if s5p20b==5
replace salario=s5p20a/6  if s5p20b==6
replace salario=s5p20a/12 if s5p20b==7
replace salario=.         if emp_ci==0
replace salario=.         if s5p20b==999999 | s5p20b==999998

gen comision=s5p23b       if s5p23a==1
replace comision =.       if s5p23b==999999 | s5p23b==999998
gen dectercero=s5p24b/12  if s5p24a==1  /*El aguinaldo esta preguntado con frecuencia anual*/
gen alimentos=s5p25b       if s5p25a==1

gen vivienda=s5p26b       if s5p26a==1
replace vivienda=.        if s5p26b==999998

gen transporte=s5p27b     if s5p27a==1 | s5p27a==2 
replace transporte=.      if s5p27b==999998
*Se pregunta cuantas veces recibio ingresos por uniforme al año
generat uniforme = s5p28b/12      if (s5p28a==1 | s5p28a==2) & s5p28c==1
replace uniforme = (s5p28b*2)/12  if (s5p28a==1 | s5p28a==2) & s5p28c==2
replace uniforme = (s5p28b*3)/12  if (s5p28a==1 | s5p28a==2) & s5p28c==3
replace uniforme = (s5p28b*4.3)/12  if (s5p28a==1 | s5p28a==2) & s5p28c==4
replace uniforme = (s5p28b*5)/12  if (s5p28a==1 | s5p28a==2) & s5p28c==5
replace uniforme = (s5p28b*6)/12  if (s5p28a==1 | s5p28a==2) & s5p28c==6


egen ylmpri_ci=rsum(salario comision dectercero)
replace ylmpri_ci=. if (salario==. & comision==. & dectercero==.) | emp_ci==0
replace ylmpri_ci=. if  ylmpri_ci==9999998 
replace ylmpri_ci=. if ylmpri_ci==314156 
*replace ylmpri_ci=0 if s5p22==3
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

****************
***ylnmpri_ci***
****************
egen ylnmpri_ci=rsum(alimentos vivienda transporte uniforme)
replace ylnmpri_ci=. if (alimentos==. & vivienda==. & transporte==. & uniforme==.) | emp_ci==0
replace ylnmpri_ci=. if  ylnmpri_ci==9999998 |  ylnmpri_ci==999998 
replace ylnmpri_ci=. if ylnmpri_ci>6000
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal" 
*Nota: no se esta considerando el autoconsumo como ingreso

***************
***ylmsec_ci***
***************

*para empleados
gen salariosec= .
replace salariosec=s5p38a*4.3  if s5p38b==1
replace salariosec=s5p38a*2  if s5p38b==2 | s5p38b==3
replace salariosec=s5p38a    if s5p38b==4
replace salariosec=s5p38a/3  if s5p38b==5
replace salariosec=s5p38a/6  if s5p38b==6
replace salariosec=s5p38a/12 if s5p38b==7
replace salariosec=.         if emp_ci==0 
replace salariosec=.         if s5p38a==999999 | s5p38a==999998

*Comision = comision + horas extras + propinas + otros (promedio del mes)
gen comisionsec   =  s5p41b         if s5p41a==1 
*Decimo tercer sueldo lo da anual pero se detalla a cuantos meses corresponde.
gen dectercerosec =  s5p42b/12      if s5p42a==1 
gen alimentossec  =  s5p43b         if s5p43a==1 | s5p43a==2 
gen viviendasec   =  s5p44b         if s5p44a==1 | s5p44a==2
gen transportesec =  s5p45b         if s5p45a==1 | s5p45a==2
replace transportesec =. if s5p45b==  999999
*Se pregunta cuantas veces recibio ingresos por uniforme al año
generat uniformesec = s5p46b/12      if (s5p46a==1 | s5p46a==2) & s5p46c==1
replace uniformesec = (s5p46b*2)/12  if (s5p46a==1 | s5p46a==2) & s5p46c==2
replace uniformesec = (s5p46b*3)/12  if (s5p46a==1 | s5p46a==2) & s5p46c==3


egen ylmsec_ci=rsum(salariosec comisionsec dectercerosec)
replace ylmsec_ci=. if (salariosec==. & comisionsec==. & dectercerosec==.) | emp_ci==0
replace ylmsec_ci=. if  ylmsec_ci==9999998 | ylmsec_ci==1999998
*replace ylmsec_ci=0 if s5p40==3
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************

egen ylnmsec_ci=rsum(alimentossec viviendasec transportesec uniformesec)
replace ylnmsec_ci=. if (alimentossec==. & viviendasec==. & transportesec==. & uniformesec==.) | emp_ci==0
replace ylnmsec_ci=. if  ylnmsec_ci==1999998 |  ylnmsec_ci==999999
drop salario* comision* dectercero* alimentos* vivienda* transporte* uniforme*

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1) /*ylmpri tiene valores o missing, no 0. COmo estamos interesados en las no respuestas, no fijamos en
quienes contestaron con .*/
replace nrylmpri_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.  



gen ylmotros_ci=.
gen ylnmotros_ci = .


sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if s2p2<10
by idh_ch: egen ylm_ch=sum(ylm_ci)if s2p2<10
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if s2p2<10

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.
gen ylmnr_ch=ylm_ch
replace ylmnr=. if nrylmpri_ch==1


*Córdobas
foreach var of varlist intahoc_ch	intprestc_ch	indemc_ch	cesantc_ch	dividc_ch	loterc_ch	compc_ch	///
donacc_ch	herenc_ch	otrosic_ch{
gen `var'mo=`var'/12
} /*Todas estas variables estan preguntadas con una frecuencia anual*/
*Dólares
/*
 /*Tipo de cambio promedio para el 2001*/
Fuente: Banco Central de Nicaragua
31-07-2005	16.7984
31-08-2005	16.8682
30-09-2005	16.936
31-10-2005	17.0063
Promedio	16.902225

*/

foreach var of varlist intahod_ch	intprestd_ch	indemd_ch	cesantd_ch	dividd_ch ///
loterd_ch	compd_ch	donacd_ch	herend_ch	otrosid_ch {
recode `var'(9999999=.) (9999998=.)
capture drop `var'cord
gen `var'cord = .
replace `var'cord =`var'*16.7984 if mes_c ==7
replace `var'cord = `var'*16.8682 if mes_c ==8
replace `var'cord = `var'*16.936 if mes_c ==9
replace `var'cord = `var'*17.0063 if mes_c ==10 
replace `var'cord = `var'*16.902225 if mes_c ==99
capture drop `var'mo
gen `var'mo=(`var'cord)/12
}

drop *cord



foreach var of varlist alquilercd alquilervd  becasd ayudasd pensionad pensionjd pensionod {
recode `var'(9999999=.)
capture drop `var'cord
gen `var'cord = .
replace `var'cord =`var'*16.7984 if mes_c ==7
replace `var'cord = `var'*16.8682 if mes_c ==8
replace `var'cord = `var'*16.936 if mes_c ==9
replace `var'cord = `var'*17.0063 if mes_c ==10 
replace `var'cord = `var'*16.902225 if mes_c ==99
capture drop `var'_cord
gen `var'mo=(`var'cord)/12
}
drop *cord



foreach var of varlist regintd_ch dinintd_ch regexd_ch dinextd_ch {
capture drop `var'cor
recode `var'(9999999=.)
gen `var'cor = .
replace `var'cor =`var'*16.7984 if mes_c ==7
replace `var'cor = `var'*16.8682 if mes_c ==8
replace `var'cor = `var'*16.936 if mes_c ==9
replace `var'cor = `var'*17.0063 if mes_c ==10 
replace `var'cor = `var'*16.902225 if mes_c ==99
}


gen remesasintc_c=dinintc_ch*4.3 if frecdiin_ch==1
replace remesasintc_c=dinintc_ch*2 if frecdiin_ch==2
replace remesasintc_c=dinintc_ch if frecdiin_ch==3
replace remesasintc_c=dinintc_ch/3 if frecdiin_ch==4
replace remesasintc_c=dinintc_ch/6 if frecdiin_ch==5
replace remesasintc_c=dinintc_ch/12 if frecdiin_ch==6
replace remesasintc_c=. if frecdiin_ch==9

*FRECUENCIA: -Semana..…1   -Quincena..…2   -Mes..…3   -Trimeste..…4   -Semestre…..5   -Año…..6

gen remesasintc_d=dinintd_chcor*4.3 if frecdiin_ch==1
replace remesasintc_d=dinintd_chcor*2 if frecdiin_ch==2
replace remesasintc_d=dinintd_chcor if frecdiin_ch==3
replace remesasintc_d=dinintd_chcor/3 if frecdiin_ch==4
replace remesasintc_d=dinintd_chcor/6 if frecdiin_ch==5
replace remesasintc_d=dinintd_chcor/12 if frecdiin_ch==6
replace remesasintc_d=. if frecdiin_ch==9


gen remesasextc_c=dinextc_ch*22 if frecdiex_ch==1
replace remesasextc_c=dinextc_ch*4.3 if frecdiex_ch==2
replace remesasextc_c=dinextc_ch*2 if frecdiex_ch==3
replace remesasextc_c=dinextc_ch if frecdiex_ch==4
replace remesasextc_c=dinextc_ch/3 if frecdiex_ch==5
replace remesasextc_c=dinextc_ch/6 if frecdiex_ch==6
replace remesasextc_c=dinextc_ch/12 if frecdiex_ch==7


gen remesasextc_d=dinextd_chcor*4.3 if frecdiex_ch==1
replace remesasextc_d=dinextd_chcor*2 if frecdiex_ch==2
replace remesasextc_d=dinextd_chcor if frecdiex_ch==3
replace remesasextc_d=dinextd_chcor/3 if frecdiex_ch==4
replace remesasextc_d=dinextd_chcor/6 if frecdiex_ch==5
replace remesasextc_d=dinextd_chcor/12 if frecdiex_ch==6



egen ynlm_ch=rsum(alquilercc_ch alquilervc_ch becasc_ch ayudasc_ch pensionac_ch pensionjc_ch pensionoc_ch *mo  *c_c *c_d)
replace ynlm_ch=. if ynlm_ch> 30000

/*Yo cree estas variables para que no tengas missing, asi que esta variable -ynlm- va a tener 0 en vez de .*/
drop alquilercc_ch alquilervc_ch becasc_ch ayudasc_ch pensionac_ch pensionjc_ch pensionoc_ch *mo 

/*
gen ynlm_ci=. /*EL ingreso no laboral esta calculado a nivel hogar, no a nivel persona*/
*/
* Se genera como el promedio entre los miembros del hogar  para mantener metodología de 1998.
by idh_ch: egen nper = sum(miembros_ci) if miembros_ci==1
by idh_ch: gen ynlm_ci=ynlm_ch/nper if miembros_ci==1

gen bienesintc_c=regintc_ch*4.3 if frecregint_ch==1
replace bienesintc_c=regintc_ch*2 if frecregint_ch==2
replace bienesintc_c=regintc_ch if frecregint_ch==3
replace bienesintc_c=regintc_ch/3 if frecregint_ch==4
replace bienesintc_c=regintc_ch/6 if frecregint_ch==5
replace bienesintc_c=regintc_ch/12 if frecregint_ch==6


gen bienesintc_d=regintd_chcor*4.3 if frecregint_ch==1
replace bienesintc_d=regintd_chcor*2 if frecregint_ch==2
replace bienesintc_d=regintd_chcor if frecregint_ch==3
replace bienesintc_d=regintd_chcor/3 if frecregint_ch==4
replace bienesintc_d=regintd_chcor/6 if frecregint_ch==5
replace bienesintc_d=regintd_chcor/12 if frecregint_ch==6

gen bienesextc_c=regexc_ch*4.3 if frecregex_ch==1
replace bienesextc_c=regexc_ch*2 if frecregex_ch==2
replace bienesextc_c=regexc_ch if frecregex_ch==3
replace bienesextc_c=regexc_ch/3 if frecregex_ch==4
replace bienesextc_c=regexc_ch/6 if frecregex_ch==5
replace bienesextc_c=regexc_ch/12 if frecregex_ch==6


gen bienesextc_d=regexd_chcor*4.3 if frecregex_ch==1
replace bienesextc_d=regexd_chcor*2 if frecregex_ch==2
replace bienesextc_d=regexd_chcor if frecregex_ch==3
replace bienesextc_d=regexd_chcor/3 if frecregex_ch==4
replace bienesextc_d=regexd_chcor/6 if frecregex_ch==5
replace bienesextc_d=regexd_chcor/12 if frecregex_ch==6


egen ynlnm_ch=rsum(bienes*)

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen rentaimp_ch=s1p19a
replace rentaimp_ch=. if s1p19a==99999 | s1p19a==99998

gen autocons_ci=.
gen autocons_ch= . /* s8p27b antes */
gen remesas_ci=.
gen remesas_ch=.
*egen remesas_ch=rsum(bienesext* remesasext*) /*Solo toma en cuenta las remesas del exterior*/
/*drop *c_c *c_d*/
gen ynlnm_ci=.

* MGD 04/09/2015: se genera con promedio como en anños posteriores.
gen durades_ci= . /* para el 2005 la respuesta viene por tramos de meses "s5p10" */
replace durades_ci = 0.5 if s5p10 ==1
replace durades_ci = (1+6)/2 if s5p10 ==2
replace durades_ci = (7+12)/2 if s5p10 ==3
replace durades_ci = (13+24)/2 if s5p10 ==4
label variable durades_ci "Duracion del desempleo en meses"


gen antiguedad_ci=s5p16a if s5p16b==4
replace antiguedad_ci=s5p16a /12 if s5p16b==3
replace antiguedad_ci=s5p16a /48 if s5p16b==2
replace antiguedad_ci=s5p16a /365 if s5p16b==1
replace antiguedad_ci=. if emp_ci==0

******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 
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


/****************************
Variables del Mercado Laboral
****************************/
gen desalent_ci=(s5p6==4 | s5p6==6)
replace desalent_ci=. if emp_ci==1

gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci>=1 & horaspri_ci<=30 & s5p50==1 & emp_ci==1


gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & s5p50==2)
replace tiempoparc_ci=. if emp_ci==0 | emp_ci==.


gen categopri_ci=.
replace categopri_ci=1 if s5p22==4
replace categopri_ci=2 if s5p22==5
replace categopri_ci=3 if s5p22==1 | s5p22==2 | s5p22==6
replace categopri_ci=4 if s5p22==3
replace categopri_ci=. if emp_ci==0

gen categosec_ci=.
replace categosec_ci=1 if s5p40==4
replace categosec_ci=2 if s5p40==5
replace categosec_ci=3 if s5p40==1 | s5p40==2 | s5p40==6
replace categosec_ci=4 if s5p40==3
replace categosec_ci=. if emp_ci==0

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


gen segsoc_ci=(s5p30a==1)
replace segsoc_ci=. if emp_ci==0

gen nempleos_ci=1 if s5p31==2
replace nempleos_ci=2 if s5p31==1
replace nempleos_ci=. if emp_ci==0
/*
gen firmapeq_ci=1 if s5p19<=5
replace firmapeq_ci=0 if s5p19>5 & s5p19<.
replace firmapeq_ci=. if emp_ci==0
*/
gen spublico_ci=0 if (s5p21>=5 & s5p21!=.) & emp_ci==1
replace spublico_ci=1 if s5p21<=4
replace spublico_ci=. if emp_ci==0 

/*******************
Variables Educativas
********************/
*Programo nuevamente la variable aedu //yessenial//

gen aedu_ci=.
replace aedu_ci=0         if s4p18a==0 | s4p18a==1
replace aedu_ci=2         if s4p18a==2 & s4p18b==1 //educacion adultos/
replace aedu_ci=4         if s4p18a==2 & s4p18b==2 //educacion adultos/
replace aedu_ci=6         if s4p18a==2 & s4p18b==3 //educacion adultos/
replace aedu_ci=s4p18b    if s4p18a==3
replace aedu_ci=6+s4p18b  if s4p18a==4
replace aedu_ci=11+s4p18b if s4p18a>=5 & s4p18a<=9
replace aedu_ci=16+s4p18b if s4p18a>=10 & s4p18a<=12 //educacion especial incluida//
label var aedu_ci "Anios de educacion aprobados" 

/* OLD CODE
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(s4p17a==3 & s4p17b<6)
gen edupc_ci=(s4p18==3 & s4p17b>=6)
gen edusi_ci=(s4p17a==4 & s4p17b<6) 
gen edusc_ci=(s4p18==4)
gen eduui_ci=(s4p17a==9 & s4p18!=8)
gen eduuc_ci=((s4p17a==9 & s4p18==9) | s4p17a==10 | s4p17a==11|s4p18==10 | s4p18==11)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.
*/

gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(s4p18a==3 & s4p18b<6)
gen edupc_ci=(s4p18a==3 & s4p18b>=6)

gen edusi_ci=(s4p18a==4 & s4p18b<5) | (s4p18a==5) | (s4p18a==6 & s4p18b<2)
gen edusc_ci=(s4p18a==4 & s4p18b>=5) | (s4p18a==6 & s4p18b>=2)


gen eduui_ci=(s4p18a==7 & s4p18b<5) | (s4p18a==8 & s4p18b<5) | (s4p18a==9 & s4p18b<5) 
gen eduuc_ci=(s4p18a==7 & s4p18b>=5) | (s4p18a==8 & s4p18b>=5) | (s4p18a==9 & s4p18b>=5) | (s4p18a>=10)

gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.

gen edupre_ci=(s4p2==3)

gen eduac_ci=.

*Nota: se debe tener en cuenta que esta es tasa de matricula no solo 
*asistencia, pero se la toma como proxy.
gen     asiste_ci = (s4p20==1 & edad_ci>=7 & edad_ci!=.)
*Modificación Mayra Sáenz - 10/22/2015 : Se incluyen a los menores de 7 años
replace asiste_ci = 1 if  (s4p2 ==3 | s4p2 ==4)
 
gen edupub_ci=0
replace edupub_ci=1 if s4p33<=3 & edad_ci>=6
replace edupub_ci=. if edad_ci<6 & (s4p33==8 | s4p33==9)

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

*****************
***pqnoasis_ci***
*****************

*gen pqnoasis_ci=s4p20

*Modificado Mayra Sáenz: Junio, 2016
gen pqnoasis_ci= s4p46

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if s4p46==11
replace pqnoasis1_ci = 2 if s4p46==4
replace pqnoasis1_ci = 3 if s4p46==2 | s4p46==10
replace pqnoasis1_ci = 4 if s4p46==12
replace pqnoasis1_ci = 5 if s4p46==3 | s4p46==9
replace pqnoasis1_ci = 8 if s4p46==5 | s4p46==7
replace pqnoasis1_ci = 9 if s4p46==1 | s4p46==6 | s4p46==8 

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
*Mayra Sáenz - Septiembre 2013: La pregunta acerca de repite sólo hace referencia al último año.
*Por lo tanto, se utiliza esta variable para generar repiteul_ci
gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************
gen repiteult_ci=(s4p29a==2)
label variable repiteult_ci "Esta repitendo ultimo grado o curso"


*************
***tecnica_ci**
*************
gen tecnica_ci=(s4p18a==8)
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
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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



