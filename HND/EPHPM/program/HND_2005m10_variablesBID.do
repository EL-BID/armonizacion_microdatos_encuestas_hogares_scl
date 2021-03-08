
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

local PAIS HND
local ENCUESTA EPHPM
local ANO "2005"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*****                            HONDURAS 2005 - OCTUBRE                                              *****
*****                EPHPM 2005 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                   41.003 personas                                               ***** 
*****                                    8.057 hogares                                                *****
*** Revised March, 2008 (by tede) ***

****************************************************************************/

use "`base_in'", clear

foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

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
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  depto
label define region_c  1 "1. atlantida" ///
           2 "2. colón" ///
           3 "3. comayagua" ///
           4 "4. copán" ///
           5 "5. cortés" ///
           6 "6. choluteca" ///
           7 "7. el paraíso" ///
           8 "8. francisco morazan" ///
           9 "9. gracias a dios" ///
          10 "10. intibuca" ///
          11 "11. islas de la bahía" ///
          12 "12. la paz" ///
          13 "13. lempira" ///
          14 "14. ocotepeque" ///
          15 "15. olancho" ///
          16 "16. santa bárbara" ///
          17 "17. valle" ///
          18 "18. yoro" 

label var region_c "División política"

***************
*pais_c       *
***************

gen pais_c="HND"

***************
*anio_c       *
***************

gen anio_c=2005

***************
*mes_c        *
***************

gen mes_c=10
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre" 10 "Octubre"
label value mes_c mes_c

***************
*idh_ch       *
***************

replace numhog=1 if numhog==.
egen idh_ch=group(hogar depto domi numhog) 

***************
*factor_ch    *
***************

gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"


***************
*zona_c       *
***************

gen zona_c=1 if domi==1 | domi==2 | domi==3 
replace zona_c=0 if domi==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

***************
*relacion_ci  *
***************

gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j==7 | rela_j== 8 
replace relacion_ci=5 if rela_j==9 | rela_j==11
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion relacion


***************
*idp_ci       *	
***************

gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"
	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
*factor_ci    *
***************

gen factor_ci=factor_ch

***************
*sexo_ci      *
***************

gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

***************
*edad_ci      *
***************

gen edad_ci=edad
label var edad_ci "Edad del Individuo"
drop edad

***************
*civil_ci     *
***************

gen civil_ci=.
replace civil_ci=1 if civil==5
replace civil_ci=2 if civil==1 | civil==6
replace civil_ci=3 if civil==3 | civil==4
replace civil_ci=4 if civil==2
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



         ******************************
         *** VARIABLES DE DIVERSIDAD **
         ******************************
*Nathalia Maya & Antonella Pereira
*Feb 2021	

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



************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=condact
replace condocup_ci=4 if condact == 4 | edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Comprobacion con variables originales.  Se considera ocupado a quienes estan en trabajos no remunerados. 5/28/2014 MGD
* La edad minima de la encuesta se cambia a 5 anios.

g condocup_ci=.
replace condocup_ci=1 if (p10==1 | p11==1 | p12==1)
replace condocup_ci=2 if (p10==2 | p11==2 | p12==2) & (p14==1) 
recode condocup_ci (.=3) if edad_ci>=5
recode condocup_ci (.=4) if edad_ci<5
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci  1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci


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
gen desalent_ci=.
replace desalent_ci=0 if emp_ci==1
replace desalent_ci=1 if p17==6
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

******************
***categopri_ci***
******************
gen categopri_ci=1 if p37==7 | p37==6 | p37==10| p37==11 /* p37 for October 2005 */
replace categopri_ci=2 if p37==4 | p37==5 | p37==8 | p37==9
replace categopri_ci=3 if p37==1 | p37==2 | p37==3
replace categopri_ci=4 if p37==12 | p37==13
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

*****************
***horaspri_ci***
*****************
gen horaspri_ci=p36 if p36<=168 
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"



******************
***categosec_ci***
******************
gen categosec_ci=1 if p63==7 | p63==6 | p63==10 | p63==11  /* p63 for October 2005 */
replace categosec_ci=2 if p63==4 | p63==5 | p63==8 | p63==9
replace categosec_ci=3 if p63==1 | p63==2 | p63==3
replace categosec_ci=4 if p63==12 | p63==13
recode categosec_ci (1=2) if (p63==6 | p63==10) &  p75==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

*****************
***horastot_ci***
*****************
gen horastot_ci=thoras
replace horastot_ci=. if thoras==.
replace horastot_ci = . if horastot_ci>168

label var horastot_ci "Horas totales trabajadas en todas las Actividades"


***************
*subemp_ci    *	
***************
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot_ci<30 & p85==1
label var subemp_ci "Trabajadores subempleados"
tab subemp_ci [w=int(factor_ci)]
*/

* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p85==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p20/30      if p20a==1
replace durades_ci=p20/4.3 if p20a==2
replace durades_ci=p20               if p20a==3
label var durades_ci "Duracion del Desempleo (en meses)"
 

*******************
***antiguedad_ci***
*******************
gen b= p39/365 if p39a == 1
replace b= p39/51.6 if p39a ==2
replace b= p39/12 if p39a ==3
replace b= p39 if p39a ==4

* Nota MGD 09/25/2014: faltaba agregar a la antiguedad de los cueta propia.
gen c= p50/365 if p50a == 1
replace c= p50/51.6 if p50a ==2
replace c= p50/12 if p50a ==3
replace c= p50 if p50a ==4

egen antiguedad_ci=rsum(b c) if emp_ci==1 , m 
drop b c
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot_ci<30 & p85==2
label var tiempoparc_ci "Trabajadores a medio tiempo"
*****************
***nempleos_ci***
*****************

*gen segsoc_ci=.
*label var segsoc "Personas que cuentan con seguro social"
* MGD 9/25/2014: mal codificada la variable.
gen nempleos_ci=.
replace nempleos_ci=1 if p58==2
replace nempleos_ci=2 if p58==1
/* MLO: modificacion 2013
gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if emp_ci==1 & p58==1
replace nempleos=0 if emp_ci==0*/
replace nempleos_ci=. if emp_ci!=1
label var nempleos "Numero de empleos"


/*
*************
*firmapeq_ci*
*************
*Asalariados y cuenta propia de la actividad principal
gen firmapeq_ci=0 if (p49a>5 & p49a<99999) | (p57a>5 & p57a<99999) 
replace firmapeq_ci=1 if (p49a<=5 & p49a!=0) | (p57a<=5 & p57a!=0) 
*/

*****************
***spublico_ci***
*****************

gen spublico_ci= 1 if  p37==1
replace spublico_ci= 0 if  p37!=1
label var spublico_ci "Personas que trabajan en el sector publico"


*************
**ocupa_ci***
*************
gen ocupa_ci =.
replace ocupa_ci=1 if p33a >=1 & p33a <=1955
replace ocupa_ci=2 if p33a >=1993 & p33a <=2545
replace ocupa_ci=3 if p33a >=2569 & p33a <=3484
replace ocupa_ci=4 if p33a >=3505 & p33a <=3940
replace ocupa_ci=5 if p33a >=9073 & p33a <=9988 
replace ocupa_ci=6 if p33a >=3962 & p33a <=4541
replace ocupa_ci=7 if (p33a >=4561 & p33a <=9059) 
*recode ocupa_ci (5=8) if (p33a ==9193) 
*replace ocupa_ci=9 
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci


***********************
*** rama_ci         ***
***********************

*** ocupacion principal ***

gen rama_ci=.
replace  rama_ci=1 if (p34b>=111005 & p34b<=500057) & emp_ci==1
replace  rama_ci=2 if (p34b>=1010000 & p34b<=1429034) & emp_ci==1
replace  rama_ci=3 if (p34b>=1511015 & p34b<=3720013) & emp_ci==1
replace  rama_ci=4 if (p34b>=4010001 & p34b<=4100011) & emp_ci==1
replace  rama_ci=5 if (p34b>=4500028 & p34b<=4550000) & emp_ci==1
replace  rama_ci=6 if (p34b>=5010000 & p34b<=5520039) & emp_ci==1
replace  rama_ci=7 if (p34b>=6010002 & p34b<=6420027) & emp_ci==1
replace  rama_ci=8 if (p34b>=6511000 & p34b<=7020015) & emp_ci==1
replace  rama_ci=9 if (p34b>=7111000 & p34b<=9900023) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
/*YL ->linea estimada (no encuentro las originales) correspond al mes de septiembre*/
generat lp_ci =1608.48 if zona_c==1
replace lp_ci = 894.72 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (p31_1==1|p31_2==2|p31_3==3|p31_4==4|p31_5==5|p31_6==6) |(p68_1==1|p68_2==2|p68_3==3|p68_4==4|p68_5==5|p68_6==6)
recode cotizando_ci . = 0 if condact==1 | condact==2

label var cotizando_ci "Cotizante a la Seguridad Social"

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
 
*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if p40==1 | p40==2
replace tipocontrato_ci=3 if p40==3 | p40==4
*/

* Modificacion: la variable se crea solamente considerando firma de contrato sin temporalidad. MGD 06/16/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if ((p40<=3) & p43==2) & categopri_ci==3
replace tipocontrato_ci=2 if ((p40<=3) & p43==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p40==4 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if p21 ==1 & condocup_ci==2
replace cesante_ci=0 if p21 ==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

foreach var in p49a p57a   {
recode `var' (99999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (p49a>=1 & p49a<=5) | (p57a>=1 & p57a<=5) 
replace tamemp_ci = 2 if (p49a>=6 & p49a<=50) | (p57a>=6 & p57a<=50)
replace tamemp_ci = 3 if (p49a>50 & p49a~=.) | (p57a>50 & p57a~=.) 
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande", modify
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"



*************
*ypen_ci*
*************
**Se transforman los dolares a lempiras**
foreach i in p88l01c2 p88l02c2{
gen `i'2 = `i'*19.03 
replace `i'= `i'2
}  
drop p88l01c22 p88l02c22

egen ypen_ci=rowtotal(p88l01c2 p88l02c2 p88l01c1 p88l02c1), missing
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci =0
replace pension_ci=1 if ypen_ci!=. & ypen_ci!=0
label var pension_ci "1=Recibe pension contributiva"


****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 


***************
*pensionsub_ci*
***************
gen byte pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci= 19.03


label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HON 2005
/*gen salmm_ci= 2515.10
label var salmm_ci "Salario minimo legal"*/

* HON 2005: mod 2015 MGD salario segun rama
gen salmm_ci=. 	/*2515.10*/
replace salmm_ci=2108.4 if rama_ci==1
replace salmm_ci=2275.2 if rama_ci==2 | rama_ci==3 | rama_ci==5 | rama_ci==6 | rama_ci==9
replace salmm_ci=2376.15 if rama_ci==7 
replace salmm_ci=2654.63 if rama_ci==8
replace salmm_ci=2403.6 if rama_ci==4 | salmm_ci==.
label var salmm_ci "Salario minimo legal"

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ((p16 ==1 | p16==2) & condocup_ci==3)
replace categoinac_ci = 2 if  (p16==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (p16==5 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
/*
gen formal_ci=1 if afiliado_ci==1 & condocup_ci==1 
label var formal_ci "Formal"
*/
*Modificación Mayra Sáenz- Febrero 2014

capture gen formal=1 if cotizando_ci==1
gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"



************************************************************************
**************************INGRESOS**************************************
************************************************************************
*Daniela Zuluaga- Noviembre 2017: Se decide reemplazar las variables del ingreso laboral (Monetario y no Monetario) por las que ya están construidas en la base original**

***************
***ylmpri_ci***
***************
egen ylmpri_ci=rowtotal(ysmop ycmop yagmop ygamop) if (ysmop>=0 & ycmop>=0 & yagmop>=0 & ygamop>=0), missing
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"


*****************
***nrylmpri_ci***
*****************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

************
*ylnmpri_ci*
************
egen ylnmpri_ci=rowtotal(yseop yceop yageop ygaeop ) if (yseop>=0 & yceop>=0 & yageop>=0 & ygaeop>=0), missing
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********
egen ylmsec_ci=rowtotal(ysmos ycmos yagmos ygamos) if (ysmos>=0 & ycmos>=0 & yagmos>=0 & ygamos>=0), missing
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
*ylnmsec_ci*
************
egen ylnmsec_ci=rowtotal(yseos yceos yageos ygaeos) if (yseos>=0 & yceos>=0 & yageos>=0 & ygaeos>=0), missing
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"


*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 
 

************
***ylm_ci***
************

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"
*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

 
*************
***ynlm_ci***
*************
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a agosto 2005: 	19.00		
                                                                     septiembre 2005: 	19.01 
								     octubre 2005: 	19.03 
								     promedio 3 meses: 	19.01 */
/*
otros ingresos en dólares
p88l01c2	p88-pension (vlr. del mes pasado efvo us$)
p88l02c2	p88-jubilación (vlr. del mes pasado efvo us$)
p88l03c2	p88-alquileres (vlr. del mes pasado efvo us$)
p88l05c2	p88-intereses bco (vlr. del mes pasado efvo us$)
p88l06c2	p88-remesas del exterior (vlr. del mes pasado efvo us$)
p88l07c2	p88-pensión x divorcio (vlr. del mes pasado efvo us$)
p88l08c2	p88-ayudas familiares (vlr. del mes pasado efvo us$)
p88l09c2	p88-ayudas particulares (vlr. del mes pasado efvo us$)
p88l11c2	p88-prestaciones laborales (vlr. del mes pasado efvo us$)
p88l12c2	p88-otros (vlr. del mes pasado efvo us$)
*/

*Conversión de otros ingresos en dólares a lempiras . Tipo de cambio 19.03
foreach i in  p88l03c2 p88l05c2 p88l06c2 p88l07c2 p88l08c2 p88l09c2 p88l11c2 p88l12c2 {
gen `i'_ = `i'*19.03 
replace `i'= `i'_
}  
/*
Otros ingresos en lempiras	
p88l01c1	p88-pension (vlr. del mes pasado efvo lps)
p88l02c1	p88-jubilación (vlr. del mes pasado efvo lps)
p88l03c1	p88-alquileres (vlr. del mes pasado efvo lps)
p88l04c1	p88-subsidios (vlr. del mes pasado efvo lps)
p88l05c1	p88-intereses bco (vlr. del mes pasado efvo lps)
p88l06c1	p88-remesas del exterior (vlr. del mes pasado efvo lps)
p88l07c1	p88-pensión x divorcio (vlr. del mes pasado efvo lps)
p88l08c1	p88-ayudas familiares (vlr. del mes pasado efvo lps)
p88l09c1	p88-ayudas particulares (vlr. del mes pasado efvo lps)
p88l10c1	p88-bonos (vlr. del mes pasado efvo lps)
p88l11c1	p88-prestaciones laborales (vlr. del mes pasado efvo lps)
p88l12c1	p88-otros (vlr. del mes pasado efvo lps)
*/

egen ynlm_ci=rsum(p88l01c1 p88l02c1 p88l03c1 p88l04c1 p88l05c1 p88l06c1 p88l07c1 p88l08c1 p88l09c1 p88l10c1 p88l12c1 p88l01c2 p88l02c2 p88l03c2 p88l05c2 p88l06c2 p88l07c2 p88l08c2 p88l09c2 p88l12c2), missing
label var ynlm_ci "Ingreso No Laboral Monetario"

**************
***ynlnm_ci***
**************
/*
Ingresos en especies dólares	
p88l08c4	p88-ayudas familiares (vlr. del mes pasado esp us$)
p88l06c4	p88-remesas del exterior (vlr. del mes pasado esp us$)
p88l09c4	p88-ayudas particulares (vlr. del mes pasado esp us$)
p88l12c4	p88-otros (vlr. del mes pasado esp us$)
*/

*Conversión de otros ingresos en dólares a lempiras . Tipo de cambio 19.03
foreach i in p88l08c4 p88l06c4 p88l09c4 p88l12c4 {
gen `i'_ = `i'*19.03 
replace `i'= `i'_
}  

/*
Ingresos en especies Lempiras	
p88l09c3	p88-ayudas particulares (vlr. del mes pasado esp lps)
p88l12c3	p88-otros (vlr. del mes pasado esp lps)
p88l06c3	p88-remesas del exterior (vlr. del mes pasado esp lps)
p88l08c3	p88-ayudas familiares (vlr. del mes pasado esp lps)
*/
egen ynlnm_ci=rsum(p88l08c4 p88l06c4 p88l09c4 p88l12c4 p88l09c3 p88l12c3 p88l06c3 p88l08c3), missing

label var ynlnm_ci "Ingreso no laboral no monetario" 


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
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing 
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing 
label var ylnm_ch "Ingreso laboral no monetario del hogar"

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing 
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing 
label var ynlm_ch "Ingreso no laboral monetario del hogar"


**************
***ynlnm_ch***
**************

by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing 
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"


***********************
*** autoconsumo_ci  ***
***********************

gen autoconsumop_ci=p56 if p56<99999 & p56>=0 /* p56 for 2005 */ 
replace autoconsumop_ci=0 if p56==. & edad>4 & (categopri==1 | categopri==2) & (p10==1 | p11==1) /* p56 for 2005 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p83 if p83<99999 & p83>=0 /* p83 for 2005 */ 
replace autoconsumos_ci=0 if p83==. & edad>4 & (categosec==1 | categosec==2) & p58==1 
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autocons_ci=rsum(autoconsumop_ci autoconsumos_ci), missing
replace autocons_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autocons_ci "Autoconsumo Individual (Trabajadores Independientes)"

******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing 
la var autocons_ch "Autoconsumo del Hogar"

****************
***remesas_ci***
****************
*Los valores en dólares ya fueron convertidos a lempiras para el cálculo del ingreso no laboral monetario.
egen remesas_ci=rsum(p88l06c1 p88l06c2 p88l06c3 p88l06c4), missing
label var remesas_ci "Remesas Individuales"

****************
***remesas_ch***
****************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing 
label var remesas_ch "Remesas mensuales del hogar" 

*****************
***ylmhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades"

**************************INGRESOS-TRANSFERENCIAS**************************************

* Daniela Zuluaga-Noviembre 2017: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

***************
***trapri_ci***
***************
egen trapri_ci= rowtotal(remesas_ci p88l07c2 p88l08c2 p88l09c2 p88l07c1 p88l08c1 p88l09c1 p88l08c4 p88l09c4 p88l09c3 p88l08c3), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

****************
***progpub_ci***
****************
gen progpub_ci= .
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas"

***************
***progpub_ch***
***************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci p88l04c1 p88l10c1 ypensub_ci), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(p88l03c2 p88l05c2 p88l03c1 p88l05c1), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(p88l12c2 p88l12c1 p88l12c4 p88l12c3), missing
label var otros_ci "Otros Ingresos" 

***************
***otros_ch***
***************
bys idh_ch: egen otros_ch=sum(otros_ci) if miembros_ci==1, missing
label var otros_ch "Otros Ingresos del hogar" 

***************
***ypen_ch***
***************
bys idh_ch: egen ypen_ch=sum(ypen_ci) if miembros_ci==1, missing
label var ypen_ch "Ingresos del hogar por jubilaciones y pensiones contributivas" 


***************
***ytotal_ci***
***************
egen ytotal_ci= rowtotal (ylm_ci ylnm_ci trapri_ci trapub_ci capital_ci otros_ci ypen_ci), missing
label var ytotal_ci "Ingreso total individual" 

***************
***ytotal_ch***
***************
egen ytotal_ch=rowtotal(ylm_ch  ylnm_ch  trapri_ch  trapub_ch  capital_ch  otros_ch  ypen_ch) if miembros_ci==1, missing
label var ytotal_ch "Ingreso total del hogar"

***************
***ytotalpc_ch***
***************
gen ytotalpc_ch=(ytotal_ch/nmiembros_ch) if miembros_ci==1
label var ytotalpc_ch "Ingreso per capita del hogar"


***************
***quintil_ci***
***************
xtile quintil_ci=ytotalpc_ch if ytotalpc_ch>0 & ytotalpc_ch!=. [pw=(factor_ch)], nq(5)
label var quintil_ci "Quintil de ingreso"
label define quintil_ci 1 "Quintil 1" 2 "Quintil 2" 3 "Quintil 3" 4 "Quintil 4" 5 "Quintil 5"
label values quintil_ci quintil_ci
 
 /*NOTA: El ingreso total aquí construido difiere de la variable de ingreso total de la base original . Esto ocurre ya que al agregar algunas de las variables
desagregadas en la base, no coinciden con las variables agregadas que están construidas en la base original (para los ingresos no laborales), no hay información 
suficiente para saber cómo se agregaron dichas variables en la base original;
En particular, hay inconsistencia con la periodicidad de las variables de otros ingresos.En todo caso, construimos los ingresos no laborales con las variables desagregadas, 
teniendo como base la periodicidad específicada en el cuestionario.
LA DIFERENCIA ES MENOR AL 10% 
No se consideran ingresos laborales negativos*/

******************************************************************************
*	Educación
*****************************************************************************

************
* asiste_ci*
************
*Modificación Mayra Sáenz Mayo 2014
gen asiste_ci=p02==1
*gen asiste_ci=.
*replace asiste_ci=1 if p02==1
*replace asiste_ci=0 if p02==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"


***************
*aedu_ci      *	
***************
/*
* Años de educacion aprobados **
replace p04b=. if p04b>9
replace p05b=. if p05b>9
** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if p04a>=1 & p04a<=3
replace aedu_ci=p04b if p04a==4 
replace aedu_ci=p04b+6 if p04a==5 | p04a==6
* MGR Aug, 2015: correción en sintáxis
*replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+12 if p04a==7 | p04a==8 | p04a==9 /*se incluye superior no universitaria*/
*replace aedu_ci=p04b+17 if p04a==9
replace aedu_ci=p04b+17 if p04a==10
** para quienes asisten actualmente
/*
replace aedu_ci=0 if p05a==1 | p05a==2
replace aedu_ci=p05b-1 if p05a==3
replace aedu_ci=p05b+6-1 if p05a==4 | p05a==5
replace aedu_ci=p05b+12-1 if p05a==6 | p05a==7
replace aedu_ci=p05b+17-1 if p05a==8
label var aedu_ci "Años de educacion aprobados"
*/
replace aedu_ci=0 if p05a==2 | p05a==3
replace aedu_ci=p05b-1 if p05a==4
replace aedu_ci=p05b+6-1 if p05a==5 | p05a==6
replace aedu_ci=p05b+12-1 if p05a==7 | p05a==8 | p05a==9 
replace aedu_ci=p05b+17-1 if p05a==10
label var aedu_ci "Años de educacion aprobados"
*/

*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU

***************
*aedu_ci      *	
***************

* Años de educacion aprobados **
replace p04b=. if p04b>9
replace p05b=. if p05b>9
** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if p04a>=1 & p04a<=3
replace aedu_ci=p04b if p04a==4 
replace aedu_ci=p04b+6 if p04a==5 
replace aedu_ci=p04b+9 if p04a==6
* MGR Aug, 2015: correción en sintáxis
*replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+12 if p04a==7 | p04a==8 | p04a==9 /*se incluye superior no universitaria*/
*replace aedu_ci=p04b+17 if p04a==9
replace aedu_ci=p04b+17 if p04a==10
** para quienes asisten actualmente
/*
replace aedu_ci=0 if p05a==1 | p05a==2
replace aedu_ci=p05b-1 if p05a==3
replace aedu_ci=p05b+6-1 if p05a==4 | p05a==5
replace aedu_ci=p05b+12-1 if p05a==6 | p05a==7
replace aedu_ci=p05b+17-1 if p05a==8
label var aedu_ci "Años de educacion aprobados"
*/

replace aedu_ci=0 if p05a==2 | p05a==3
replace aedu_ci=p05b-1 if p05a==4
replace aedu_ci=p05b+6-1 if p05a==5 
replace aedu_ci=p05b+9-1 if p05a==6
replace aedu_ci=p05b+12-1 if p05a==7 | p05a==8 | p05a==9 
replace aedu_ci=p05b+17-1 if p05a==10
label var aedu_ci "Años de educacion aprobados"


******************************
*	eduno_ci
******************************
g byte eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
la var eduno_ci "Personas sin educacion. Excluye preescolar"
******************************
*	edupi_ci 
******************************
g byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
la var edupi_ci "Personas que no han completado Primaria"
******************************
*	edupc_ci 
******************************
g byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
la var edupc_ci "Primaria Completa"
******************************
*	edusi_ci 
******************************
g byte edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edusi_ci=. if aedu_ci==.
la var edusi_ci "Secundaria Incompleta"
******************************
*	edusc_ci 
******************************
g byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
la var edusc_ci "Secundaria Completa"
******************************
*	edus1i_ci 
******************************
g byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
la var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"
******************************
*	edus1c_ci 
******************************
g byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
la var edus1c_ci "1er ciclo de Educacion Secundaria Completo"
******************************
*	edus2i_ci 
******************************
g byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
la var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"
******************************
*	edus2c_ci 
******************************
g byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
la var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media
******************************
*	eduui_ci 
******************************
g byte eduui_ci=(aedu_ci>12 & aedu_ci<17) 
replace eduui_ci=. if aedu_ci==.
la var eduui_ci "Universitaria o Terciaria Incompleta"
******************************
*	eduuc_ci 
******************************
g byte eduuc_ci=(aedu_ci>=17)
replace eduuc_ci=. if aedu_ci==.
la var eduuc_ci "Universitaria o Terciaria Completa"
******************************
*	edupre_ci 
******************************
g byte edupre_ci=.
replace edupre_ci=1 if ((p04a==3 | p05a==3) & aedu_ci ~=.)
replace edupre_ci=0 if (edupre_ci~=1 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

***************
***asispre_ci***
***************
*Creación de la variable asistencia a preescolar por Iván Bornacelly - 08/09/18
	g asispre_ci=.
	la var asispre_ci "Asiste a educacion prescolar"	

******************************
*	pqnoasis 
******************************

rename p03 pqnoasis_ci
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

******************************
*	repite_ci 
******************************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

***************
*repiteult_ci *	
***************

gen repiteult_ci=.
replace repiteult_ci=1 if p05c==1
replace repiteult_ci=0 if p05c==2
label var repiteult_ci "Personas que han repetido el ultimo grado"


******************************
*	edupub_ci 
******************************

gen edupub_ci=.
replace edupub_ci=1 if (p07==1|p07==2|p07==3|p07==4|p07==7) & p02 ==1
replace edupub_ci=0 if (p07==5|p07==6|p07==8|p07==9)& p02 ==1
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p04a==7 | p05a==7
replace tecnica_ci=0 if tecnica_ci ~=1 & ( p04a!=99 & p05a!=99)
label var tecnica_ci "1=formacion terciaria tecnica"

**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci= 0 if tecnica_ci ==1
replace eduac_ci=1 if eduuc_ci ==1 | eduui_ci ==1
label variable eduac_ci "Superior universitario vs superior no universitario"

**DZ Noviembre 2017: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
*****************
***pqnoasis1_ci***
*****************
g       pqnoasis1_ci = 1 if pqnoasis_ci==7
replace pqnoasis1_ci = 2 if pqnoasis_ci==3
replace pqnoasis1_ci = 3 if pqnoasis_ci==6
replace pqnoasis1_ci = 4 if pqnoasis_ci==2
replace pqnoasis1_ci = 5 if pqnoasis_ci==4  | pqnoasis_ci==10
replace pqnoasis1_ci = 6 if pqnoasis_ci==1
replace pqnoasis1_ci = 7 if pqnoasis_ci==8  | pqnoasis_ci==9
replace pqnoasis1_ci = 8 if pqnoasis_ci==5
replace pqnoasis1_ci = 9 if pqnoasis_ci==11 | pqnoasis_ci==12

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


***************
*** HOUSING ***
***************

***************
**aguared_ch **
***************

gen aguared_ch=.
replace aguared_ch=1 if v05a==1
replace aguared_ch=0 if v05a==2 

***************
**aguadist_ch *
***************

gen aguadist_ch=.
replace aguadist_ch=1 if v05e==1
replace aguadist_ch=2 if v05e==2 | v05e==3
replace aguadist_ch=3 if v05e==4

***************
**aguamala_ch *
***************

gen aguamala_ch=.
replace aguamala_ch=1 if v05b>=5 & v05b<=8
replace aguamala_ch=0 if v05b>=1 & v05b<=4

***************
**aguamide_ch *
***************

gen aguamide_ch=.

***************
**luz_ch      *
***************

gen luz_ch=.
replace luz_ch=1 if v07==1 | v07==2 | v07==3
replace luz_ch=0 if v07>=4 & v07<=8

***************
**luzmide_ch  *
***************

gen luzmide_ch=.

***************
**combust_ch  *
***************

gen combust_ch=.

***************
**bano_ch     *
***************

gen bano_ch=.
replace bano_ch=1 if v06a==1
replace bano_ch=0 if v06a==2

***************
**banoex_ch   *
***************

gen banoex_ch=.
replace banoex=1 if v06c==1
replace banoex=0 if v06c==2

***************
**des1_ch     *
***************


gen des1_ch=.
replace des1_ch=0 if v06a==2
replace des1_ch=1 if v06b==1 | v06b==2
replace des1_ch=2 if v06b==5 | v06b==6 | v06b==7
replace des1_ch=3 if v06b==3 | v06b==4

***************
**des2_ch     *
***************

/*
gen des2_ch=.
replace des2_ch=1 if v06b==1 | v06b==2 | v06b==3 
replace des2_ch=2 if v06b==4 | v06b==5 | v06b==6 | v06b==7
replace des2_ch=0 if v06a==2
*/

* MGR Jul, 2015: corrección en sintáxis 
gen des2_ch=.
replace des2_ch=1 if v06b==1 | v06b==2 | v06b==5 | v06b==6 | v06b==7
replace des2_ch=2 if v06b==4 | v06b==3
replace des2_ch=0 if v06a==2

***************
**piso_ch     *
***************

gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8

***************
**pared_ch    *
***************

gen pared_ch=.
replace pared_ch=0 if v02==5 | v02==6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7

***************
**techo_ch    *
***************

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8

***************
**resid_ch    *
***************

gen resid_ch=.
replace resid_ch=0 if v08a==1 | v08a==3 /* v08a for 2005 */
replace resid_ch=1 if v08a==4 | v08a==6
replace resid_ch=2 if v08a==7
replace resid_ch=3 if v08a==2 | v08a==5 | v08a==8

***************
**dorm_ch     *
***************

gen dorm_ch=.
replace dorm_ch=v16b if v16b>=0 
replace dorm_ch=. if v16b==99
***************
**cuartos_ch  *
***************

gen cuartos_ch=.
replace cuartos_ch=v16a if v16a>=0 /* v16a for 2005 */ 
replace cuartos_ch =. if v16a ==99 
***************
**cocina_ch   *
***************

gen cocina_ch=.

***************
**telef_ch    *
***************

gen telef_ch=.
replace telef_ch=1 if v13f==1 /* v13f for 2005 */
replace telef_ch=0 if v13f==2

***************
**refrig_ch   *
***************

gen refrig_ch=.
replace refrig_ch=1 if v13a==1 /* v13a for 2005 */
replace refrig_ch=0 if v13a==2

***************
**freez_ch    *
***************

gen freez_ch=.

***************
**auto_ch     *
***************

gen auto_ch=.
replace auto_ch=1 if v13h==1 /* v13h for 2005 */
replace auto_ch=0 if v13h==2

***************
**compu_ch    *
***************

gen compu_ch=.
replace compu_ch=1 if v13k==1 /* v13k for 2005 */
replace compu_ch=0 if v13k==2

***************
**internet_ch *
***************

gen internet_ch=.

***************
**cel_ch      *
***************

gen cel_ch=.
replace cel_ch=1 if v13g==1 /* v13g for 2005 */
replace cel_ch=0 if v13g==2

***************
**vivi1_ch    *
***************

gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=8) | v01==2 | v01==3

***************
**vivi2_ch    *
***************

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

***************
**viviprop_ch *
***************

gen viviprop_ch=.
replace viviprop_ch=0 if v14==5 /* v14 for 2005 */
replace viviprop_ch=1 if v14==1
replace viviprop_ch=2 if v14==4
replace viviprop_ch=3 if v14==6 | v14==2 | v14==3 | v14==7 /* v14==7 included for 2005 */

***************
**vivitit_ch  *
***************

gen vivitit_ch=.
replace vivitit_ch=1 if v17a==1 /* v17a */
replace vivitit_ch=0 if v17a==2

/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a agosto 2005: 	19.00		
                                                                     septiembre 2005: 	19.01 
								     octubre 2005: 	19.03 
								     promedio 3 meses: 	19.01 */


***************
**vivialq_ch  *
***************
/*
gen vivialq_ch=.
replace vivialq_ch=v15b if v15b<99999 & v14==5 /* v15b & v14 for 2005 */

* replace vivialq_ch=v10c/17.73 if v10c<99999 & v10b==2 *


*/
gen vivialq_ch=.
replace vivialq_ch=v15a if v15b==1
replace vivialq_ch=v15a*19.03 if v15b==2


***************
*vivialqimp_ch*
***************

gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v05b >=1 & v05b <=4)
replace aguamejorada_ch = 0 if (v05b >=5 & v05b <=8)

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=7)) & v06c ==1)
replace banomejorado_ch = 0 if ( v06a ==1 & ((v06b >=1 & v06b <=2) | (v06b >=5 & v06b <=7)) & v06c ==2) | (v06b >=3 & v06b <=4) | (v06a==2)


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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
trapri_ci trapri_ch progpub_ci progpub_ch trapub_ci  trapub_ch capital_ci capital_ch otros_ci otros_ch ypen_ch ytotal_ci  ytotal_ch ytotalpc_ch quintil_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close



/*

**************************************
****** EQXIS: MDGs VARIABLES    ****** 
**************************************

**************************************
****** Last update: May, 2008   ****** 
**************************************

* Instituto Nacional de Estadística, Encuesta Permanente de Hogares de Propósitos Múltiples. Oct, 2005.

/*
rela_j
 1. jefe del hogar
 2. esposa(o) o compañera(o) 
 3. hijos
 4. hijastros 
 5. padres 
 6. hermanos 
 7. yernos y nueras 
 8. otros parientes 
 9. otros no parientes 
 10. servicio domestico
*/

* Variables

 rename rela_j parentco
 rename ur area
 rename p01 alfabet
 rename p02 asiste
 rename p05a nivasiste
 rename p05b grado

 tab area [iw=factor]

* Gender classification of the population refering to the head of the household.

 sort hogar nper

 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(hogar)

 tab sexo   [iw=factor]
 tab sexo_d [iw=factor]
 
 tab sexo sexo_d if parentco==1

 sort hogar nper

* Dwelling ID

 gen str7 hogar_s=string(hogar)
 gen id_viv=substr(hogar_s,1,6)

** Years of education. 
/*

2. ¿Asiste actualmente a algún centro educativo?
1. SI ==> 5

4. ¿Cuál es el nivel y año educativo más alto que aprobó?

Nivel(p04a)				
 1. Ninguno			
 2. Centro de Alfabetización	
 3. Pre-Escolar			
 4. Primaria			
 5. Ciclo común			
 6. Secundaria Diversificado	
 7. Técnico Superior		
 8. Superior Universitario	
 9. Post-grado			
99. No sabe			

Año Educativo (p04b)

5. ¿Qué nivel y año educativo cursa actualmente?

Nivel(p05a)				
 2. Centro de Alfabetización	 
 3. Pre-Escolar			 
 4. Primaria			 
 5. Ciclo común			 
 6. Secundaria Diversificado	 
 7. Técnico Superior		 
 8. Superior Universitario	 
 9. Post-grado			 
99. No sabe			 

Año Educativo (p05b)

*/
* Included in the database

 rename anosest a_orig

 gen	 anoest=0  if (p04a==1 | p04a==2 | p04a==3) | (nivasiste==2 | nivasiste==3) | (nivasiste==4 & grado==1)
 replace anoest=1  if (p04a==4 & p04b==1) 		       | (nivasiste==4 & grado==2)
 replace anoest=2  if (p04a==4 & p04b==2) 		       | (nivasiste==4 & grado==3)
 replace anoest=3  if (p04a==4 & p04b==3) 		       | (nivasiste==4 & grado==4)
 replace anoest=4  if (p04a==4 & p04b==4) 		       | (nivasiste==4 & grado==5)
 replace anoest=5  if (p04a==4 & p04b==5) 		       | (nivasiste==4 & grado==6)
 replace anoest=6  if (p04a==4 & p04b==6) 		       | (nivasiste==4 & grado==7) | (nivasiste==5 & grado==1)
 replace anoest=7  if (p04a==4 & p04b==7) | (p04a==5 & p04b==1) | (nivasiste==4 & grado==8) | (nivasiste==5 & grado==2)
 replace anoest=8  if (p04a==4 & p04b==8) | (p04a==5 & p04b==2) | (nivasiste==4 & grado==9) | (nivasiste==5 & grado==3)
 replace anoest=9  if (p04a==4 & p04b==9) | (p04a==5 & p04b==3) | (nivasiste==6 & grado==1) | (nivasiste==7 & grado==1)
 replace anoest=10 if (p04a==6 & p04b==1) | (p04a==7 & p04b==1) | (nivasiste==6 & grado==2) | (nivasiste==7 & grado==2)
 replace anoest=11 if (p04a==6 & p04b==2) | (p04a==7 & p04b==2) | (nivasiste==6 & grado==3) | (nivasiste==7 & grado==3)
 replace anoest=12 if (p04a==6 & p04b==3) | (p04a==7 & p04b==3) | (nivasiste==6 & grado==4) | (nivasiste==8 & grado==1)
 replace anoest=13 if (p04a==6 & p04b==4) | (p04a==8 & p04b==1) | (nivasiste==8 & grado==2)
 replace anoest=14 if (p04a==8 & p04b==2) | (nivasiste==8 & grado==3)
 replace anoest=15 if (p04a==8 & p04b==3) | (nivasiste==8 & grado==4)
 replace anoest=16 if (p04a==8 & p04b==4) | (nivasiste==8 & grado==5)
 replace anoest=17 if (p04a==8 & p04b==5) | (nivasiste==8 & grado==6) | (nivasiste==9 & grado==1) 
 replace anoest=18 if (p04a==8 & p04b==6) | (p04a==9 & p04b==1) | (nivasiste==8 & grado==7) | (nivasiste==9 & grado==2) 
 replace anoest=19 if (p04a==8 & p04b==7) | (p04a==9 & p04b==2) | (nivasiste==8 & grado==8) | (nivasiste==9 & grado==3) 
 replace anoest=20 if (p04a==8 & p04b==8) | (p04a==9 & p04b==3) | (nivasiste==9 & grado==4) 
 replace anoest=21 if (p04a==9 & p04b==4)

** Economic Active Population  (10 years or more of age)
* Included in the database
* condact
/* Condición de Actividad
 1. Ocupados	
 2. Desocupados
 3. Inactivos
*/

 gen	 TASADESO=0 if condact==1
 replace TASADESO=1 if condact==2

********************
*** Strata & PSU ***
********************

* The definition of the survey design for the svymean commands
* is based on the available survey's sample design documentation, 
* and the variables available in the database, therefore the following 
* specification might be an approximation.

* Domains

/*
Strata

Domi (vs. Dominio)
 1. Tegucigalpa 
 2. San pedro sula
 3. Ciudades medianas
 4. Ciudades pequeñas
 5. Rural 
 */

 gen str1 estrato1=substr(hogar_s,2,1)
 destring estrato1, replace

 gen	 strata=domi if domi<5
 replace strata=5 if domi==5 & estrato1==0
 replace strata=6 if domi==5 & estrato1==1
 
* PSU

 gen str5 psu=substr(hogar_s,1,5)

 svyset [pweight=factor_ch], strata(strata) psu(psu)
 svydes

************
** ETHNIC **
************

* NA

***************
*** REGIONS ***
***************

/*
 1. Tegucigalpa 
 2. San pedro sula
 3. Resto urbano
 4. Rural 
*/

 gen region=dominio

 gen region2=depto
 
************************
*** MDGs CALCULATION ***
************************

/*

2. ¿Asiste actualmente a algún centro educativo?
1. SI ==> 5

4. ¿Cuál es el nivel y año educativo más alto que aprobó?

Nivel(p04a)				
 1. Ninguno			
 2. Centro de Alfabetización	
 3. Pre-Escolar			
 4. Primaria			
 5. Ciclo común			
 6. Secundaria Diversificado	
 7. Técnico Superior		
 8. Superior Universitario	
 9. Post-grado			
99. No sabe			

Año Educativo (p04b)

5. ¿Qué nivel y año educativo cursa actualmente?

Nivel(p05a)				
 2. Centro de Alfabetización	 
 3. Pre-Escolar			 
 4. Primaria			 
 5. Ciclo común			 
 6. Secundaria Diversificado	 
 7. Técnico Superior		 
 8. Superior Universitario	 
 9. Post-grado			 
99. No sabe			 

Año Educativo (p05b)

*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if (edad>=7 & edad<=12) & (asiste>=1 & asiste<=2) 
 replace NERP=1 if (edad>=7 & edad<=12) & (nivasiste==4 & (grado>=1 & grado<=6))
 label var NERP "Net Enrolment Ratio in Primary" 

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS=1 if (edad>=13 & edad<=18) & ((nivasiste==4 & (grado>=7 & grado<=9)) | (nivasiste==5 | nivasiste==6))
 label var NERS "Net Enrolment Ratio in Secondary"

* Upper secondary
* Secundaria Diversificado

 gen	 NERS2=0 if (edad>=16 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS2=1 if (edad>=16 & edad<=18) & (nivasiste==6)
 label var NERS2 "Net Enrolment Ratio in Secondary - upper"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen	 ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 
 label var ALFABET "Literacy Rate of 15-24 Years Old"
 
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen	 ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1)
 label var ALFABET2 "Literacy Rate of 15-24 Years Old INE"

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  (nivasiste==4 & (grado>=1 & grado<=6))
 gen sec=1  if ((nivasiste==4 & (grado>=7 & grado<=9)) | (nivasiste==5 | nivasiste==6))
 gen ter=1  if  (nivasiste==8)

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
 label var RATIOPRIM "Ratio of Girls to Boys in School - Primary"
 
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
 label var RATIOSEC "Ratio of Girls to Boys in School - Secondary"

** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  
 label var RATIOTER "Ratio of Girls to Boys in School - Tertiary"

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.
 
 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 label var RATIOALL "Ratio of Girls to Boys in School - All"

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.
 
 gen     RATIOLIT2=0 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 label var RATIOLIT2 "Ratio of Literate Women to Men 15-24 year olds INE"
 
** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 label var RATIOLIT2 "Ratio of Literate Women to Men 15-24 year olds"
 
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*
29.A. ¿Cuál es o era su categoría ocupacional en la ocupación principal?
 1. Empleado u obrero público 
 2. Empleado u obrero privado 
 3. Empleado doméstico 
TRABAJADORES CUENTA PROPIA
 4. Miembro de cooperativa de producción 
 5. Cuenta propia que no contrata mano de obra temporal 
 6. Cuenta propia que contrata mano de obra temporal 
 7. Empleador o socio activo 
PRODUCTORES AGROPECUARIOS
 8. Miembro de cooperativa, asentamiento o grupo
 9. Cuenta propia que no contrata mano de obra temporal
 10. Cuenta propia que contrata mano de obra temporal
 11. Patrón o socio de la finca
 12. Trabajador familiar no remunerado
 13. Trabajador no remunerado

categop (variable included in the database) =>
categoria ocupacional (ocupacion principal)
 1. Empleado publico 
 2. Empleado privado
 3. Empleada domestica
 4. Cuenta propia 
 5. Trabajador no remunerado 
*/

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=2) & (ramaop>=2 & ramaop<=9) & (condact==1)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=2) & (ramaop>=2 & ramaop<=9) & (condact==1) & (sexo==2)
 label var WENAS "WENAS without domestic servants"

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (ramaop>=2 & ramaop<=9) & (condact==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (ramaop>=2 & ramaop<=9) & (condact==1) & (sexo==2)
 label var WENASD "WENAS with domestic servants"

** Access to Electricity ** Additional Indicator
/*
7. ¿Qué tipo de alumbrado utiliza en la vivienda?
 1. Servicio Público
 2. Servicio privado colectivo
 3. Planta propia
 4. Energía solar
 5. Vela
 6. Candil o lámpara de gas 
 7. Ocote
 8. Otro:________________________ */

 egen elec=max(v07), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0  if (elec>=1 & elec<=8) /* Total population excluding missing information */
 replace ELEC=1  if (elec>=1 & elec<=4)
 label var ELEC "Proportion of Population with access to electricity"

/*
5. Servicio de Agua (v05a)
a) ¿Tiene en la vivienda o en la propiedad, tubería instalada para agua?
 1. Sí  2. No

b) ¿Cómo obtiene el agua que utiliza en la vivienda? (v05b)
 1. Servicio Público por tubería
 2. Servicio Privado por tubería
 3. Pozo malacate
 4. Pozo con bomba
 5. Río, riachuelo, manantial, ojo de agua
 6. Carro Cisterna
 7. Pick-Up barriles o con drones o barriles
 8. Otro

c) ¿Cómo es el suministro de agua?
 1. Permanente
 2. Irregular

d) En los últimos quince días, ¿Con qué frecuencia tuvo el suministro de agua en su vivienda?
	- Cuántos días
	- Horas promedio por día

e) ¿Dónde obtiene el agua?
 1. Dentro de la vivienda
 2. Fuera de la vivienda y dentro de la propiedad
 3. Fuera de la propiedad a menos de 100 metros
 4. Fuera de la propiedad a más de 100 metros
*/

 
 egen agua=max(v05b), by(id_viv)
 egen lugabast=max(v05e), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (agua>=1 & agua<=8) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4)
 label var WATER "Improved Water Source"

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*

6. Servicio Sanitario
a) ¿Tiene algún tipo de servicio sanitario?
 1. Sí 
 2. No ==> 7

b) ¿Qué tipo de servicio sanitario tiene?
 1. Inodoro conectado a alcantarilla
 2. Inodoro conectado a pozo séptico
 3. Inodoro con desagüe a río, laguna, mar
 4. Letrina con descarga a río, laguna, mar
 5. Letrina con cierre hidráulico
 6. Letrina con pozo séptico
 7. Letrina con pozo negro

c) El uso del servicio sanitario es:
 1. Exclusivo de la vivienda
 2. Compartido con otras viviendas
*/

 egen servsani=max(v06a), by(id_viv)
 egen sanita=  max(v06b), by(id_viv)
 egen servexc= max(v06c), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (servsani>=1 & servsani<=2)	/* Total population excluding missing information */
 replace SANITATION=1 if ((sanita>=1 & sanita<=2) | (sanita==6))
 label var SANITATION "Improved Sanitation"
 
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*
1. Tipo de vivienda			10. ¿Cómo es la tenencia de esta vivienda?
v01					v10a
 1. Casa individual			 1. Propietario y completamente pagada
 2. Casa de mat. nat. (Rancho)		 2. Propietario recuperada legalizada
 3. Casa Improvisada (Desechos)		 3. Propietario recuperada sin legalizar
 4. Apartamento				 4. Propietario y la está pagando
 5. Cuarto en mesón o cuartería 	 5. Alquilada
 6. Barracón				 6. Cedida sin pago
 7. Local no construido para habitación 	
 pero usado como vivienda
 8. Otro

2. ¿Cuál es el material predominante 	3. ¿Cuál es el material predominante en el piso?
en la construcción de las paredes?
 1. Ladrillo, piedra o bloque		 1. Cerámica
 2. Adobe				 2. Ladrillo de cemento
 3. Material prefabricado		 3. Ladrillo de granito
 4. Madera				 4. Ladrillo de barro
 5. Bahareque, vara o caña		 5. Plancha de cemento
 6. Desechos				 6. Madera
 7. Otro				 7. Tierra
				 	 8. Otro

12. Cantidad de Piezas de la Vivienda
a). ¿Cuántas piezas tiene esta vivienda? (incluya la cocina pero no el baño)
*/

 egen tenencia=max(v14), by(id_viv)
 egen tipoviv=max(v01), by(id_viv)
 egen piso=max(v03), by(id_viv)
 egen pared=max(v02), by(id_viv)
 egen nrocuart=max(v16a), by(id_viv)
 replace nrocuart=. if v16a==99
 egen pers=max(totpervi), by(id_viv) /* Total de personas de la vivienda */

 gen persroom=pers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=8) & (tenencia>=1 & tenencia<=6)  /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==2 | tipoviv==3 | tipoviv==6 | tipoviv==7 | tipoviv==8) | (tenencia==6)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (pared>=1 & pared<=7) & (piso>=1 & piso<=8)  /* Total population excluding missing information */
 replace secten_2=1 if (pared>=5 & pared<=7) | (piso==7 | piso==8)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 gen	 SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1)  /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)
 label var SECTEN "Secure Tenure"

* Dirt floors

* Gender classification of the population refers to the head of the household.

* 3. ¿Cuál es el material predominante en el piso?

 gen	 DIRT=0 if (piso>=1 & piso<=8) /* Total population excluding missing information */
 replace DIRT=1 if (piso==7)
 label var DIRT "Proportion of Population living in dwellings with dirt floors"
 
** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (TASADESO==0 | TASADESO==1) 
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (TASADESO==1 )
 label var UNMPLYMENT15 "Unemployment Rate 15 to 24"

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

/*
11. ¿Tiene en esta vivienda los siguientes bienes?
f. Teléfono fijo 
g. Teléfono celular 
*/

 egen telefono=max(v13f), by(id_viv)
 egen celular=max(v13g), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 TELCEL=0 if (telefono>=1 & telefono<=2) & (celular>=1 & celular<=2) /* Total population excluding missing information */
 replace TELCEL=1 if (celular==1 | telefono==1)
 label var TELCEL "Telephone Lines & Cellular Phones" 
 global indicador 47 " "

** FIXED LINES
* Gender classification of the population refers to the head of the household.

 gen	 TEL=0 if (telefono>=1 & telefono<=2) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1)
 label var TEL "Telephone Lines"
	
** CEL LINES
* Gender classification of the population refers to the head of the household.

 gen	 CEL=0 if (celular>=1 & celular<=2) /* Total population excluding missing information */
 replace CEL=1 if (celular==1)
 label var CEL "Cellular Phones"
	
** Target 18, Indicator: "Personal computers in use per 100 population"
/*
11. ¿Tiene en esta vivienda los siguientes bienes?
i. Computadora 
*/

 egen computadora=max(v13k), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 COMPUTER=0 if (computadora>=1 & computadora<=2) /* Total population excluding missing information */
 replace COMPUTER=1 if (computadora==1)
 label var COMPUTER "Personal Computers"

** Target 18, Indicator: "Internet users per 100 population"
/*
10. Durante el mes pasado, ¿tuvo acceso a internet?
 1. Sí
 2. No 
 9. No sabe

11.¿En que sitio tuvo acceso a internet? (p11)
 1. En su casa (a)
 2. En un cyber-café o negocio de internet (b)
 3. En su trabajo (c)
 4. En la escuela, colegio o universidad (d)
 5. En una oficina de HONDUTEL (e)
 6. Otro (f)
*/

** In order to mantain comparability with other countries the following
** restricted definition is used.

 * egen internet=max(p11a), by(hogar) /* Tuvo acceso a internet en su casa*/

* Gender classification of the population refers to the head of the household.

 * gen	 INTUSERS=0 
 * replace INTUSERS=1 if (internet==1)
 * label var INTUSERS "Internet Users"
 	
************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen     CHILDREN=0 if  (edad>=12 & edad<=14) 
 replace CHILDREN=1 if  (edad>=12 & edad<=14) & (condact==1)
 label var CHILDREN "Children Under Age 15 who are Working"

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1
 label var PERSROOM2 "Persons per Room"
  
 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
 label var PLT2 "Population living in households with less than 2 persons (inclusive) per room"

** Disconnected Youths
/*
17. ¿Por qué no buscó trabajo ni trató de establecer su propio
negocio o finca la semana pasada?
 1. Se incorporará a un trabajo antes de un mes			==>22
 2. Tiene trabajo asegurado después de un mes 			==>22
 3. Espera respuesta a gestiones 				==>22
 4. Está esperando la próxima temporada de trabajo 		==>22
 5. Cree que no encontrará trabajo				==>22
 6. Dejó de buscar trabajo momentáneamente			==>22
 7. No tiene tierra, capital, ni materia prima			==>22
 8. No tiene tiempo para buscar trabajo				==>18
 9. No tiene necesidad de trabajar				==>18
 10. Por su edad no puede trabajar				==>18
 11.  Otro							==>18

16. ¿Cuál es su condición actual?
 1. Jubilado
 2. Pensionista
 3. Rentista
 4. Estudiante
 5. Realiza los quehaceres del hogar
 6. Discapacitado
 7. Vejez
 8. Menor de edad
 9. Otro
*/

 gen	 DISCONN=0 if (edad>=15 & edad<=24) 
 replace DISCONN=1 if (edad>=15 & edad<=24) & ((p17==5) | ((p17>=8 & p17<=11) & (p16==9)))
 label var DISCONN "Disconnected Youths"


*** Rezago escolar

 gen	 rezago=0	if (anoest>=0 & anoest<99)  & edad==7 /* This year of age is not included in the calculations */
	 
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

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==18
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==18

* Primary and Secondary [ISCED 1, 2 & 3]

 gen	 REZ=0 if (edad>=8 & edad<=18) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=8 & edad<=18) & (rezago==1)
 label var REZ "Rezago Escolar"

* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
 label var PRIMCOMP "Primary completion rate [15 - 24 years of age]"
	
* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))
 label var AEDUC_15 "Average Years of Education 15+"
	
 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))
 label var AEDUC_15_24 "Average Years of Education 15-24"
 
 noisily display "Average Years of Education 25+"

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
 label var AEDUC_25 "Average Years of Education 25+"
 
 	
* Grade for age

 gen GFA=(anoest/(edad-7)) if (edad>=8 & edad<=18) & (anoest>=0 & anoest<99)
 label var GFA "Grade for age"
 
* Grade for age primary

 gen GFAP=(anoest/(edad-7)) if (edad>=8 & edad<=12) & (anoest>=0 & anoest<99)
 label var GFAP "Grade for age primary" 
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=18) & (anoest>=0 & anoest<99)
 labe var GFAS "Grade for age secondary"






