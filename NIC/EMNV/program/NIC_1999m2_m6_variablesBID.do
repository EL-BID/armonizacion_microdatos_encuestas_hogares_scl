








*:':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':*
*Mayra Sáenz Septiembre 2013                                                                                      *
* Esta base de datos está armonizada pero no es representativa. Se levantó sólo para los damnificados del         *
* Huracán Mitch.                                                                                                  *
*:':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':':*







/*
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
local ANO "1999"
local ronda m2_m6

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
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear


/***** revision August 03,2005

removed condition (& edad_ci<18) froom the following two lines:

by idh_ch: egen byte nhijos_ch=sum((relacion_ci==3) & edad_ci<18)
by idh_ch: egen byte notropari_ch=sum((relacion_ci==4) & edad_ci>=18)

***/

/** revision August 18,2006 (Victoria) 

Change in the variable aedu beacuse secundary education lasts 5 years in 
Nicaragua. 

Code for education dummies was changed beacuse there were mistakes in it. 
Old code can be seen in education section below. 
**/
* January 25 2007 (MFP)
*Change in firmapeq variable. It was created to reflect big firms instead of small firm


************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

foreach v of varlist _all {
local lowname = lower( "`v'")
qui rename  `v'  `lowname'
}



/*****************
Variables de Hogar
******************/
gen factor_ch=peso3pm 
gen idh_ch=id_hogar
gen idp_ci=p00 
gen zona_c=i06m
replace zona_c=0 if i06m==2
gen pais_c="NIC"
gen anio_c=1999
gen region_BID_c=.
replace region_BID_c=1 if pais=="MEX" | pais=="PAN" | pais=="DOM" | pais=="CRI" | pais=="BLZ" | pais=="GTM" | pais=="SLV" | pais=="HON" | pais=="NIC"
replace region_BID_c=2 if pais=="BAH" | pais=="BAR" | pais=="GUY" | pais=="JAM" | pais=="SUR" | pais=="TOT"
replace region_BID_c=3 if pais=="ECU" | pais=="COL" | pais=="PER" | pais=="VEN" | pais=="BOL" 
replace region_BID_c=4 if pais=="ARG" | pais=="BRA" | pais=="CHL" | pais=="PRY" | pais=="URU" 
replace region_BID_c=5 if pais=="HAI"
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

gen mes_c=mes

/*
p02:
           1 Jefe(a)
           2 Esposa(o
           3 Hijos(as
           4 Padres /
           5 Yerno /N
           6 Nieto(a)
           7 Hermano(
           8 Otros Pa
           9 Sin Panr
          10 Empleada
          11 Pensioni

		  */
*****************
***relacion_ci***
*****************

gen paren=p03
gen relacion_ci=.
replace relacion_ci=1 if p02==1
replace relacion_ci=2 if p02==2 
replace relacion_ci=3 if p02==3
replace relacion_ci=4 if p02>=4 & p02<=8
replace relacion_ci=5 if p02==9 | p02==11
replace relacion_ci=6 if p02==10

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

/*************************************
Variables de Infraestructura del Hogar
**************************************/
gen aguared_ch=(v26m==1 |v26m==2) /*Esta definido como acceso a agua de red!*/
gen aguadist_ch=0
replace aguadist_ch=1 if v26m==1
replace aguadist_ch=2 if v26m==2 | v28mtm<=100
replace aguadist_ch=3 if (v26m >=3 & v26m <=8) | v28mtm>100
gen aguamala_ch=(v26m ==5 | v26m ==6)
gen aguamide_ch=(v29m==1)
gen luz_ch=(v38m ==1)
gen luzmide_ch=(v39m==1)
gen cocina_ch=(v42m==1)
gen combust_ch=(v43m==2 | v43m==3 | v43m==5)
gen bano_ch=(v32m!=5) /*Esta definido como si posee servicio higienico: inodoro o letrina*/
gen banoex_ch=(v33m==1)
replace banoex_ch=. if bano_ch==0

gen des1_ch=0
replace des1_ch=1 if v32m==2 | v32m==3 /*Red o pozo septico*/
replace des1_ch=2 if v32m==1 /*Pozo comun  - Letrina */
replace des1_ch=3 if v32m==4 /*Superficie*/

gen des2_ch=0
replace des2_ch=1 if v32m==2 | v32m==3 /*Red o pozo septico*/
replace des2_ch=2 if v32m==1 /*Pozo comun  - Letrina */
gen dessup=(v32m==4)

gen piso_ch=(v13m==1 | v13m==2 | v13m==4)
replace piso_ch=2 if v13m==3 | v13m==6 /*Vale 0 si es piso de tierra, 1 si es permanente y 2 si es otros (o de ladrillo de barro)*/

gen pared_ch= (v12m==2 |v12m==4 |v12m==6 |v12m==7 |v12m==8)
replace pared_ch=2 if v12m==10 |v12m==1 /*Otros*/

gen techo_ch=(v14m==1 | v14m==3)
replace techo_ch=2 if v14m==6 /*Otros*/


gen resid_ch=0 if v35m==1/*Recoleccion*/
replace resid_ch=1 if v35m==2 | v35m==3 /*Quemada o enterrada*/
replace resid_ch=2 if v35m==5 /*Tirada*/
replace resid_ch =3 if v35m!=1 & v35m!=2 & v35m!=3 & v35m!=5 /*"Otros: Hecha abono o llevada al lugar"*/

gen dorm_ch=v19m
gen cuartos_ch = v18m

gen vivi1_ch = (v11m==1) /*Casa*/
replace vivi1_ch=2 if v11m==3 /*Departamento*/
replace vivi1_ch=3 if vivi1_ch==0

gen vivi2_ch = (v11m==1|v11m==3)

gen viviprop_ch = 0 if v21m ==7/*Alquilada*/
replace viviprop_ch = 1 if v21m ==1 | v21m==2 /*Propia*/
replace viviprop_ch = 2 if v21m ==3 /*Pagando*/
replace viviprop_ch = 4 if v21m!=1 & v21m!=2 & v21m!=3 & v21m!=7

gen vivitit_ch = (v21m==1) /*Con titulo que certifique la propiedad de la vivienda*/

gen vivialq_ch= v22m
gen vivialqimp_ch= v25m
replace vivialqimp_ch=. if v25m==99999
gen telef_ch=(v49m!=4)
/*
Ya se crearon en un do file anterior.
refrig_ch, auto_ch, compu_ch

*/
****************
****freez_ch****
****************

gen freez_ch=.

****************
**internet_ch***
****************

gen internet_ch=.

****************
****cel_ch******
****************
* Aunque existe la pregunta En el mes pasado algunas personas del Hogar compraron o autosuministraron de>
* Tel'efonos celulares...
* No se contabiliza a los miembros del hogar que ya tenían celulares antes del periodo de referencia.

gen cel_ch=.


/*********************
Variables Demograficas
*********************/
gen factor_ci=peso3pm
gen sexo_ci=p03
gen edad_ci=p04a
gen civil_ci=1 if p06==6 | p06==7
replace civil_ci=2 if p06==1 | p06==2
replace civil_ci=3 if p06==3 | p06==4
replace civil_ci=4 if p06==5 
gen jefe_ci=(relacion_ci==1)
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
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
sort idh_ch
by idh_ch:egen byte nmiembros_ch=sum((relacion_ci>0 & relacion_ci<5)|p02==9) if relacion_ci~=6 
by idh_ch:egen byte nmayor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|p02==9) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum(((relacion_ci>0 & relacion_ci<5)|p02==9) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum(((relacion_ci>0 & relacion_ci<5)|p02==9) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum(((relacion_ci>0 & relacion_ci<5)|p02==9) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum(((relacion_ci>0 & relacion_ci<5)|p02==9) & (edad_ci<1)) /*Hay que tener en cuenta que en 
este año, se pregunto si existen pensionistas en la casa, que tecnicamente son "otros no parientes", pero que en la practica
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
gen condocup_ci=.
replace condocup_ci=1 if (s5p1a ==1 | (s5p1a==2 & s5p1b == 1) | s5p1c ==1)
replace condocup_ci=2 if (s5p1a==2 & s5p1b==2 & s5p1c==2 & s5p2==1)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

/*
gen condocup1_ci=.
replace condocup1_ci=1 if p5a01==1 | p5a02==1 | p5a03==1 
replace condocup1_ci=2 if p5a03==2 
replace condocup1_ci=3 if p5a05>=6 & p5a05<=11
replace condocup1_ci=4 if edad_ci<14
*/

/* Mayra Sáenz - Septiembre 2013: Para la primera categoría 1 de condocup_ci, estaba esta observaci'on desde años anteriores: "En las Bananas que se usaron para armar los Apendices del IPES '03
no se cuentan como empleados a aquellos individuos que hayan realizado alguna tarea no formal la semana pasada (p5a02==1). 
Sin embargo, el Instituto de Estadistica de Nicaragua si los tiene en cuenta para calcular la tasa de empleo, por lo que a 
de ahora si los incluiremos==> Las nuevas tasas de empleo seran mas altas que las del apendice del 2003"*/


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
*En 1999 eliminan las preguntas referentes a si está cotizando al seguro o no.
gen cotizando_ci=.

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
*tamemp_ci***
*************
gen tamemp_ci= s5p14 if  (s5p14 >=1 &  s5p14 <=7)
label define tamemp_ci 1 "1 persona" 2 "2-5 personas" 3 "6-10 personas" ///
4 "11-30 personas" 5 "31-50 personas" 6 "51-100 personas" 7"101 y más personas" 
label var tamemp_ci "# empleados en la empresa de la actividad principal"

*************
**pension_ci*
*************

gen pension_ci=. 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=.
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
replace cesante_ci=1 if s5p4==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =. 
label var lpe_ci "Linea de indigencia oficial del pais"

*************
* lp25_ci ***
*************
gen lp25_ci = 376.4876
label var lp25_ci "Linea de pobreza 2.5 dólares, año base 2005"

***********
*lp4_ci ***
***********
gen lp4_ci = 602.3802

label var lp4_ci "Linea de pobreza 4 dólares, año base 2005"

gen ocupa_ci=.
replace ocupa_ci=1 if real(substr(string(s5p8),1,1))==2 | real(substr(string(s5p8),1,1))==3
replace ocupa_ci=2 if real(substr(string(s5p8),1,1))==1
replace ocupa_ci=3 if real(substr(string(s5p8),1,1))==4
replace ocupa_ci=4 if real(substr(string(s5p8),1,2))==52
replace ocupa_ci=5 if real(substr(string(s5p8),1,2))==51
replace ocupa_ci=6 if real(substr(string(s5p8),1,1))==6
replace ocupa_ci=7 if real(substr(string(s5p8),1,1))==7 | real(substr(string(s5p8),1,1))==8
replace ocupa_ci=8 if real(substr(string(s5p8),1,1))==0
replace ocupa_ci=9 if real(substr(string(s5p8),1,1))==9
replace ocupa_ci=. if emp_ci==0

gen rama_ci=.
replace rama_ci=1 if s5p9>=111 & s5p9<=500
replace rama_ci=2 if s5p9>=1110 & s5p9<=1429
replace rama_ci=3 if s5p9>=1511 & s5p9<=3720
replace rama_ci=4 if s5p9>=4010 & s5p9<=4100
replace rama_ci=5 if s5p9>=4510 & s5p9<=4550
replace rama_ci=6 if s5p9>=5010 & s5p9<=5520
replace rama_ci=7 if s5p9>=6010 & s5p9<=6420
replace rama_ci=8 if s5p9>=6511 & s5p9<=7020
replace rama_ci=9 if s5p9>=7111 & s5p9<=9500
replace rama_ci=. if emp_ci==0

*************
**salmm_ci***
*************
/*
Salario Mínimo Oficial
Fuente: 
http://www.bcn.gob.ni/estadisticas/economicas_anuales/50_anios/BD/Capitulo_III-Sector_empleo_y_salarios/III_10.pdf
Conceptos	1999
	Julio
	
 Agropecuario 	 450.0 
 Pesca 	 700.0 
 Minería 	 850.0 
 Industria manufacturera 	 600.0 
 Industria zona franca 	 800.0 
 Electricidad, gas y agua  	 900.0 
 Construcción 	 1,200.0 
 Comercio 	 900.0 
 Transporte y comunicaciones 	 900.0 
 Establecimientos financieros 	 1,000.0 
 Servicios comunales, sociales y personales 	 700.0 
 Gobierno central y municipal 	 550.0 
*/

generat salmm_ci=.
replace salmm_ci=450+700/2 if rama_ci==1
replace salmm_ci=850       if rama_ci==2
replace salmm_ci=600       if rama_ci==3
replace salmm_ci=900       if rama_ci==4  
replace salmm_ci=1200       if rama_ci==5 
replace salmm_ci=900       if rama_ci==6 
replace salmm_ci=900       if rama_ci==7
replace salmm_ci=1000       if rama_ci==8
replace salmm_ci=700       if rama_ci==9
replace salmm_ci=(450+700+850+600+900+1200+900+900+1000+700)/10 if salmm_ci==. 
label var salmm_ci "Salario minimo legal"

gen horaspri_ci=s5p13*s5p12
replace horaspri_ci=. if emp_ci==0 | s5p13>24 | s5p12>7

gen horastot_ci=s5p33 
replace horastot_ci=. if emp_ci==0


gen salario=s5p15a*22 if s5p15b==1 
replace salario=s5p15a*4.3 if s5p15b==2
replace salario=s5p15a*2 if s5p15b==3 | s5p15b==4
replace salario=s5p15a if s5p15b==5
replace salario=s5p15a/3 if s5p15b==6
replace salario=s5p15a/6 if s5p15b==7
replace salario=s5p15a/12 if s5p15b==8
replace salario=. if s5p15b==9 | emp_ci==0 
/*Se mantiene el criterio de asignar missing a aquellas personas que declaran cobrar con una frecuencia incierta(17).*/

gen agui=s5p18b/12 /*El aguinaldo esta preguntado para todo el año*/
egen ylmpri_ci=rsum(salario s5p17b agui)
replace ylmpri_ci=. if (salario==. & s5p17b==. & s5p18b==.) |emp_ci==0 /*INCLUYE LO MISMO QUE INCLUYE EL YLMPRI_CI DEL '93*/
drop salario agui

/*Para los ingresos laborales no monetarios de la actividad principal, el rubro mensual de uniformes
* debe ser calculado en base a la p5b25b, que hace referencia a cuántas veces al año recibe los uniformes.
    recibe |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |        241       42.36       42.36
          2 |         83       14.59       56.94
          3 |         55        9.67       66.61
          4 |          2        0.35       66.96
          5 |          1        0.18       67.14
          6 |         58       10.19       77.33
         22 |          1        0.18       77.50
   Ignorado |        128       22.50      100.00
------------+-----------------------------------
      Total |        569      100.00

*/
gen unif=s5p21b if s5p21c==12
replace unif=(s5p21b*22)/12 if s5p21c==22 
replace unif=s5p21b/2 if s5p21c==6 
replace unif=s5p21b/3 if s5p21c==4
replace unif=s5p21b/4 if s5p21c==3
replace unif=s5p21b/6 if s5p21c==2
replace unif=s5p21b/12 if s5p21c==1
replace unif=. if emp_ci==0 | s5p21b==.

egen ylnmpri_ci=rsum(s5p19b s5p20b unif s5p22b)
replace ylnmpri_ci=. if (s5p19b==. & s5p20b==. & unif==. & s5p22b==.) | emp_ci==0
/*
s5p28b:
           1 Día
           2 Semana
           3 Catorcen
           4 Quincena
           5 Mes
           6 Trimestr
           7 Semestre
           8 Año
           9 Otro
          99 Ignorado

		  */

gen salariosec=s5p28a*22 if s5p28b==1 
replace salariosec=s5p28a*4.3 if s5p28b==2
replace salariosec=s5p28a*2 if s5p28b==3 | s5p28b==4
replace salariosec=s5p28a if s5p28b==5
replace salariosec=s5p28a/3 if s5p28b==6
replace salariosec=s5p28a/6 if s5p28b==7
replace salariosec=s5p28a/12 if s5p28b==8
replace salariosec=. if s5p28b==9 | emp_ci==0 
/*Se mantiene el criterio de asignar missing a aquellas personas que declaran cobrar con una frecuencia incierta(17).*/



gen aguis=s5p31b/12 /*Esta definido por año*/
egen ylmsec_ci=rsum(salariosec s5p30b aguis)
replace ylmsec_ci=. if (salariosec==. & s5p30b==. & s5p31b==.) |emp_ci==0 
drop salariosec aguis

gen ylnmsec_ci=s5p32a
replace ylnmsec_ci=. if emp_ci==0

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.  


* Otros ingresos del hogar en montos anuales:
*  s9d2p21 s9d2p22 s9d2p23 s9d2p24 s9d2p25 s9d2p26 s9d2p27 s9d2p28 s9d2p29a s9d2p29b s9d2p29c s9d2p210 s9d2p211


  foreach var of varlist s9d2p21 s9d2p22 s9d2p23 s9d2p24 s9d2p25 s9d2p26 s9d2p27 s9d2p28 s9d2p29a s9d2p29b s9d2p29c s9d2p210 s9d2p211 {
                gen `var'_m = `var'/12 
                        }
global ot_ing s9d2p21_m s9d2p22_m s9d2p23_m s9d2p24_m s9d2p25_m s9d2p26_m s9d2p27_m s9d2p28_m s9d2p29a_m s9d2p29b_m s9d2p29c_m s9d2p210_m s9d2p211_m					
						
egen ynlm_ch=rsum(s9d1p21 s9d1p22 s9d1p23 s9d1p24 s9d1p25 s9d1p26 s9d1p27 $ot_ing)
replace ynlm_ch=. if  s9d1p21==. & s9d1p22==. & s9d1p23==. & s9d1p24==. & s9d1p25==. & s9d1p26==. & s9d1p27==. /// 
 & s9d2p21_m ==. & s9d2p22_m==. & s9d2p23_m==. & s9d2p24_m==. & s9d2p25_m==. & s9d2p26_m==. & s9d2p27_m==. & ///
s9d2p28_m==. & s9d2p29a_m==. & s9d2p29b_m==. & s9d2p29c_m==. & s9d2p210_m==. & s9d2p211_m==. 
replace ynlm_ch=. if miembros_ci ==0

drop $ot_ing

gen ylmotros_ci=.
gen ylnmotros_ci = .


sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.


gen ylmnr_ch=ylm_ch
replace ylmnr=. if nrylmpri_ch==1

bys id_hogar: egen nper= sum(miembros_ci)
bys idh_ch: gen ynlm_ci=ynlm_ch/nper if miembros_ci==1
gen ynlnm_ch=.



gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

gen rentaimp_ch=v25m
replace rentaimp_ch=. if v25m==99999

gen autocons_ci=.
gen autocons_ch=.
gen remesas_ci=.
gen remesas_ch=.
gen ynlnm_ci=.

/*
s5p6b:
           1 Dias
           2 Semanas
           3 Meses
           4 Años
           9 Ignorado
*/

 
gen durades_ci=s5p6a/22 if s5p6b == 1
replace durades_ci=s5p6a/4.3 if s5p6b == 2
replace durades_ci=s5p6a if s5p6b == 3
replace durades_ci=s5p6a/12 if s5p6b == 4
replace durades_ci=. if s5p6b == 9
replace durades_ci=. if emp_ci==1


/*
s5p10b:
           1 Dias
           2 Semanas
           3 Meses
           4 Años
           9 Ignorado
*/
 

gen antiguedad_ci=s5p10a if s5p10b==4
replace antiguedad_ci=s5p10a/12 if s5p10b==3
replace antiguedad_ci=s5p10a/48 if s5p10b==2
replace antiguedad_ci=s5p10a/365 if s5p10b==1
replace antiguedad_ci=. if emp_ci==0

/****************************
Variables del Mercado Laboral
****************************/


gen desalent_ci=(s5p5==10)
replace desalent_ci=. if emp_ci==1

*En 1999 eliminan la pregunta: Desea trabajar más horas?
gen subemp_ci=.
replace subemp_ci=. if emp_ci==0

gen tiempoparc_ci=.
replace tiempoparc_ci=. if emp_ci==0

/*
s5p16:
           1 Empleado
           2 Jornaler
           3 Cuenta P
           4 Patron o
           5 Miembro 
           6 Trabajad
           7 Otro
           9 Ignorado
*/

gen categopri_ci=.
replace categopri_ci=1 if s5p16==4
replace categopri_ci=2 if s5p16==3
replace categopri_ci=3 if s5p16==1 | s5p16==2 | s5p16==5
replace categopri_ci=4 if s5p16==6
replace categopri_ci=. if emp_ci==0
label define categopri_ci 1 "Patron" 2 "Cuenta propia" 
label define categopri_ci 3 "Empleado" 4 " Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

/*
s5p29:
           1 Empleado
           2 Jornaler
           3 Cuenta P
           4 Patron o
           5 Miembro 
           6 Trabajad
           7 Otro
           9 Ignorado
*/


gen categosec_ci=.
replace categosec_ci=1 if s5p29==4
replace categosec_ci=2 if s5p29==3
replace categosec_ci=3 if s5p29==1 | s5p29==2 | s5p29==5
replace categosec_ci=4 if s5p29==6
replace categosec_ci=. if emp_ci==0


*****************
*tipocontrato_ci*
*****************
*En 1999 eliminan la pregunta que hace referencia al contrato.
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

gen nempleos_ci=1 if s5p23==2
replace nempleos_ci=2 if s5p23==1
replace nempleos_ci=. if emp_ci==0

gen firmapeq_ci=(s5p14<=2)
replace firmapeq_ci=. if emp_ci==0 |s5p14==.

gen spublico_ci=.



/*******************
Variables Educativas
********************/
/*
s4p7a:
           0 Ninguno
           1 Preescol
           2 Educacio
           3 Primaria
           4 Secundar
           5 Tecnico 
           6 Tecnico 
           7 Tecnico 
           8 Universi
           9 Ignorado
*/
gen aedu_ci=.
replace aedu_ci=0 if s4p7a==0
replace aedu_ci=s4p7b if s4p7a==3
replace aedu_ci=6 if s4p7b>6 & s4p7a==3

replace aedu_ci=6+s4p7b if s4p7a==4
replace aedu_ci=11 if s4p7b>5 & s4p7a==4

replace aedu_ci=6+s4p7b if s4p7a==5
replace aedu_ci=9+s4p7b if s4p7a==6
replace aedu_ci=11 if s4p7a==6 & s4p7b>2
replace aedu_ci=11+s4p7b if s4p7a==7
replace aedu_ci=11+s4p7b if s4p7a==8

/*
OLD CODE:
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(p4b08a==3 & p4b08b <6)
gen edupc_ci=(p4b09==3 & p4b08b>=6)
gen edusi_ci=(p4b08a==4 & p4b08b <6) 
gen edusc_ci=(p4b09==4 & p4b08b>=6)
gen eduui_ci=(p4b08a==8 & p4b09!=8)
gen eduuc_ci=(p4b08a==8 & p4b09==8)
gen edus1i_ci=.
gen edus1c_ci=.
gen edus2i_ci=.
gen edus2c_ci=.
*/

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
label variable edus1c_ci "1er ciclo de la secundaria completo"

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
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"



gen edupre_ci=(s4p7a==1)

gen eduac_ci=.

gen asiste_ci=0
replace asiste_ci=1 if s4p1!=4 & edad_ci<6
replace asiste_ci=1 if s4p8==1 & edad_ci>=6

gen edupub_ci=0
replace edupub_ci=1 if s4p3==1 & edad_ci<6
replace edupub_ci=1 if s4p13<=3 & edad_ci>=6

*****************
***pqnoasis_ci***
*****************

gen pqnoasis=s4p9
replace pqnoasis = . if  s4p9 ==99

***************
***repite_ci***
***************

gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo ultimo grado o curso"


*************
***tecnica_ci**
*************
gen tecnica_ci=( s4p7a==7)
label var tecnica_ci "=1 formacion terciaria tecnica"

*********
*raza_ci*
*********

gen raza_ci=.


/*
Comentario Mayra Sáenz - Septiembre 2013
Existe la pregunta: La lengua materna que habla desde la ninez en su casa es:
Espanol 1
Miskito 2
Sumo o sumu 3
Ingles 4
Otro 5 
* Sin embargo se genera como missing hasta definir criterios teóricos para
* la creación de la variable raza_ci.
*/




**Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
qui sum factor_ch	idh_ch	idp_c	zona_c	pais_c	anio_c	mes_c	relacion_ci	factor_ci	sexo_ci	edad_ci	civil_ci	///
jefe_ci	nconyuges_ch	nhijos_ch	notropari_ch	notronopari_ch	nempdom_ch	clasehog_ch	nmiembros_ch	///
miembros_ci	nmayor21_ch	nmenor21_ch	nmayor65_ch	nmenor6_ch	nmenor1_ch	ocupa_ci	rama_ci	horaspri_ci	///
horastot_ci	ylmpri_ci	ylnmpri_ci	ylmsec_ci	ylnmsec_ci	ylmotros_ci	ylnmotros_ci	nrylmpri_ci	tcylmpri_ci ///
ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci	nrylmpri_ch	tcylmpri_ch	ylm_ch	ylnm_ch	ylmnr_ch	ynlm_ch	ynlnm_ch	///
ylmhopri_ci	ylmho_ci	rentaimp_ch	autocons_ci	autocons_ch	remesas_ci	remesas_ch	durades_ci	antiguedad_ci ///
emp_ci	desemp_ci	pea_ci	 desalent_ci	subemp_ci	tiempoparc_ci ///
categopri_ci	categosec_ci	nempleos_ci	firmapeq_ci	spublico_ci	aedu_ci	eduno_ci ///
edupi_ci	edupc_ci	edusi_ci	edusc_ci	eduui_ci	eduuc_ci	edus1i_ci	edus1c_ci	edus2i_ci ///
edus2c_ci	edupre_ci	eduac_ci	asiste_ci	pqnoasis	repite_ci	repiteult_ci	edupub_ci	///
aguared_ch	aguadist_ch	aguamala_ch	aguamide_ch	luz_ch	luzmide_ch	combust_ch	bano_ch	banoex_ch	///
des1_ch	des2_ch	piso_ch	pared_ch	techo_ch	resid_ch	dorm_ch	cuartos_ch	cocina_ch	telef_ch ///
refrig_ch	freez_ch	auto_ch	compu_ch	internet_ch	cel_ch	vivi1_ch	vivi2_ch	viviprop_ch	///
vivitit_ch	vivialq_ch	vivialqimp_ch region_BID_c region_c raza_ci        lp25_ci	       lp4_ci	 ///
lp_ci	       lpe_ci	       cotizando_ci	       cotizapri_ci	       cotizasec_ci	       afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
tamemp_ci 	   pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	


qui destring $var, replace
 

* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress



saveold "`base_out'", replace


log close



