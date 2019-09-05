
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
local ANO "2002"
local ronda m9

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m9
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017
			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
***********************************************************************************************************
*****                            HONDURAS 2002 - SEPTIEMBRE                                           *****
*****                EPHPM 2002 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                  110.222 personas                                               ***** 
*****                                    22.010 hogares                                               *****
***********************************************************************************************************
****************************************************************************/

clear all
set more off
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
***************
***factor_ch***
***************
gen factor_ch=factor

***************
****idh_ch*****
*************** 
egen idh_ch=group(hogar depto domi ur numhog) 


*************
****idp_ci****
**************
gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"
                
**********
***zona***
**********
gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4
replace zona_c=0 if domi==5
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

************
****pais****
************
gen pais_c="HND"

**********
***anio***
**********
gen anio_c=2002

*********
***mes***
*********
gen mes_c=9
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"
label value mes_c mes_c

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j==7 
replace relacion_ci=5 if rela_j==8 
replace relacion_ci=6 if rela_j==9
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion relacion


***************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************
gen factor_ci=factor_ch

/* Hay 3 hogares con dos jefes: 3434, 5043, 11004 */
gen uno=1
egen njefe=sum(uno) if rela_j==1, by(idh_ch)
tab njefe
tab idh if njefe==2
/*    njefe |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     22,007       99.97       99.97
          2 |          6        0.03      100.00
------------+-----------------------------------
      Total |     22,013      100.00
group(hogar |
 depto domi |
 ur numhog) |      Freq.     Percent        Cum.
------------+-----------------------------------
       3434 |          2       33.33       33.33
       5043 |          2       33.33       66.67
      11004 |          2       33.33      100.00
------------+-----------------------------------
      Total |          6      100.00
*/


**********
***sexo***
**********

gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=edad if edad<999
label var edad_ci "Edad del Individuo"
drop edad

*****************
***civil_ci***
*****************
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
replace condocup_ci=2 if (p10==2 | p11==2 | p12==2) & (p14==1 | p14==2) 
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
gen desalent_ci=(emp_ci==0 & (p15==5))
replace desalent_ci=. if p15==.
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

******************
***categopri_ci***
******************
gen categopri_ci=1 if p30==7| p30==6
replace categopri_ci=2 if p30==4 | p30==5 
replace categopri_ci=3 if p30==1 | p30==2 | p30==3
replace categopri_ci=4 if p30==8 | p30==9
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci


*****************
***horaspri_ci***
*****************
gen horaspri_ci=p23 if p23<=168
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p41 if p41<=168


******************
***categosec_ci***
******************
gen categosec_ci=1 if p42==7| p42==6
replace categosec_ci=2 if p42==4 | p42==5 
replace categosec_ci=3 if p42==1 | p42==2 | p42==3
replace categosec_ci=4 if p42==8 | p42==9
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci


*****************
***horastot_ci***
*****************
egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace  horastot_ci=. if horastot_ci>168 
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las actividades"


***************
***subemp_ci***
***************
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & p50==1
label var subemp_ci "Trabajadores subempleados"
*/
* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p50==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"


************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p20
replace durades_ci=0.5 if p19==1 & p20==.
label var durades_ci "Duracion del Desempleo (en meses)"


*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=p27 if emp_ci==1 & p27>0 & p27<99
replace antiguedad_ci=0.5 if p26==1 & emp_ci==1 & antiguedad_ci==.
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"


*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p50==2
label var tiempoparc_ci "Trabajadores a medio tiempo"
*****************
***nempleos_ci***
*****************

gen nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p38==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"

/*
*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p29>=1 & p29<=5
replace firmapeq_ci=0 if p29>=6 & p29<9999
*/

*****************
***spublico_ci***
*****************
gen spublico_ci= 1 if  p30==1
replace spublico_ci= 0 if  p30!=1
label var spublico_ci "Personas que trabajan en el sector publico"


*************
**ocupa_ci***
*************
gen ocupa_ci =.
replace ocupa_ci=1 if p24a >=1 & p24a <=1955
replace ocupa_ci=2 if p24a >=1993 & p24a <=2545
replace ocupa_ci=3 if p24a >=2569 & p24a <=3484
replace ocupa_ci=4 if p24a >=3505 & p24a <=3940
replace ocupa_ci=5 if p24a >=9073 & p24a <=9988 
replace ocupa_ci=6 if p24a >=3962 & p24a <=4541
replace ocupa_ci=7 if (p24a >=4561 & p24a <=9059) 
*recode ocupa_ci (5=8) if (p30 ==9193) 
*replace ocupa_ci=9 
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

*************
***rama_ci***
*************
/*
gen rama_ci=rama
replace rama_ci=. if rama==10 | rama==11 | emp_ci==0
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
*/

gen rama_ci=.
replace  rama_ci=1 if (p25a>=111 & p25a<=500) & emp_ci==1
replace  rama_ci=2 if (p25a>=1010 & p25a<=1429) & emp_ci==1
replace  rama_ci=3 if (p25a>=1511 & p25a<=3720) & emp_ci==1
replace  rama_ci=4 if (p25a>=4010 & p25a<=4100) & emp_ci==1
replace  rama_ci=5 if (p25a>=4510 & p25a<=4550) & emp_ci==1
replace  rama_ci=6 if (p25a>=5010 & p25a<=5520) & emp_ci==1
replace  rama_ci=7 if (p25a>=6010 & p25a<=6420) & emp_ci==1
replace  rama_ci=8 if (p25a>=6511 & p25a<=7020) & emp_ci==1
replace  rama_ci=9 if (p25a>=7111 & p25a<=9900) & emp_ci==1
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
/*YL ->linea estimada (no encuentro las originales) correspond al mes de agosto*/
gen lp_ci =.
replace lp_ci = 1386.24 if zona_c == 1
replace lp_ci =  771.10 if zona_c == 0
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

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if p21==1 & condocup_ci==2
replace cesante_ci=0 if p21==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.

gen tamemp_ci = 1 if (p29>=1 & p29<=5)
replace tamemp_ci = 2 if (p29>=6 & p29<=50)
replace tamemp_ci = 3 if (p29>50) & p29!=.

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"


*************
*ypen_ci*
*************

gen ypenju2=v53au3m/3 if v53au3m>=0 
replace ypenju2=v53aum if v53aum>=0 & (v53au3m==. | v53au3m==0)
gen yjub2=v53bu3m/3 if v53bu3m>=0
replace yjub2=v53bum if v53bum>=0 & (v53bu3m==. | v53bu3m==0)

egen ypen_ci=rowtotal(ypenju2 yjub2), missing
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************

gen pension_ci =.
replace pension_ci=1 if ypen_ci!=. & ypen_ci!=0
replace pension_ci=0 if ypen_ci==0 | ypen_ci==.
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
gen tc_ci=17.07

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2002: mod 2015 MGD salario segun rama
gen salmm_ci=. 	/*1810.20*/
replace salmm_ci=1572.45 if rama_ci==1
replace salmm_ci=1695.75 if rama_ci==2 | rama_ci==3 | rama_ci==5 | rama_ci==6 | rama_ci==9
replace salmm_ci=1765.2 if rama_ci==7 
replace salmm_ci=2015.1 if rama_ci==8
replace salmm_ci=1959.56 if rama_ci==4 | salmm_ci==.
label var salmm_ci "Salario minimo legal"


*******************
***categoinac_ci***
*******************
/*

p16:
           1 01. jubilado
           2 02. pensionista
           3 03. rentista
           4 04. estudiante
           5 05. realiza los quehaceres del hogar
           6 06. incapacitado temporalmente
           7 07. incapacitado permanentemente
           8 08. vejez
           9 09. menor de edad
          10 10. otros
*/
gen categoinac_ci =1 if ((p16==1 | p16==2) & condocup_ci==3)
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
gen formal_ci=.
label var formal_ci "Formal"


************************************************************************
**************************INGRESOS**************************************
************************************************************************
*Daniela Zuluaga- Noviembre 2017: Se deciden reemplazar las variables del ingreso laboral (Monetario y no Monetario) por las que ya están construidas en la base original**

***************
***ylmpri_ci***
***************
egen ylmpri_ci=rowtotal(ysmop ycmop), missing
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
egen ylnmpri_ci=rowtotal(yseop yceop), missing
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********
egen ylmsec_ci=rowtotal(ysmos ycmos), missing
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
*ylnmsec_ci*
************
egen ylnmsec_ci=rowtotal(yseos yceos), missing
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

gen yalquile2=v53cu3m/3 if v53cu3m>=0
replace yalquile2=v53cum if v53cum>=0 & (v53cu3m==. | v53cu3m==0)
gen ysubsi2=v53du3m/3 if v53du3m>=0
replace ysubsi2=v53dum if v53dum>=0 & (v53du3m==. | v53du3m==0)
gen ybonos2=v53ju3m/3 if v53ju3m>=0 
replace ybonos2=v53jum if v53jum>=0 & (v53ju3m==. | v53ju3m==0)
gen yremesa2=v53gu3m/3 if v53gu3m>=0
replace yremesa2=v53gum if v53gum>=0 & (v53gu3m==. | v53gu3m==0)
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a septiembre 2002: 16.82 
                                                                     agosto 2002: 16.73 
								     julio 2002: 16.64 
								     promedio 3 meses: 16.73 */
gen yremesad2=v53fu3m/3*16.73 if v53fu3m>=0
replace yremesad2=v53fum*16.82 if v53fum>=0 & (v53fu3m==. | v53fu3m==0)
gen yayuda2=v53hu3m/3 if v53hu3m>=0
replace yayuda2=v53hum if v53hum>=0 & (v53hu3m==. | v53hu3m==0)
gen yayupar2=v53iu3m/3 if v53iu3m>=0 
replace yayupar2=v53ium if v53ium>=0 & (v53iu3m==. | v53iu3m==0)
gen yotros2=v53lu3m/3 if v53lu3m>=0 
replace yotros2=v53lum if v53lum>=0 & (v53lu3m==. | v53lu3m==0)

gen interes2=v53eu3m/3 if v53eu3m>=0 
replace interes2=v53eum if v53eum>=0 & (v53eu3m==. | v53eu3m==0)
gen prestlab2=v53ku3m/3 if v53ku3m>=0 
replace prestlab2=v53kum if v53kum>=0 & (v53ku3m==. | v53ku3m==0)

/*No estan las herencias: gen herencia2=v46ju3m/3 if v46ju3m>=0 
replace herencia2=v46jum if v46jum>=0 & v46ju3m==.*/

gen yremesade=v53fu3e/3*16.73 if v53fu3e>=0
replace yremesade=v53fue*16.82 if v53fue>=0 & (v53fu3e==. | v53fu3e==0)
gen yremesae=v53gu3e/3 if v53gu3e>=0
replace yremesae=v53gue if v53gue>=0 & (v53gu3e==. | v53gu3e==0)
gen yayudae=v53hu3e/3 if v53hu3e>=0
replace yayudae=v53hue if v53hue>=0 & (v53hu3e==. | v53hu3e==0)
gen yayupare=v53iu3e/3 if v53iu3e>=0
replace yayupare=v53iue if v53iue>=0 & (v53iu3e==. | v53iu3e==0)


egen ynlm_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2), missing
/*replace ynlm_ci=0 if v53au3m==. & v53bu3m==. & v53du3m==. & v53cu3m==. & v53ju3m==. & v53gu3m==. & v53hu3m==. & v53iu3m==. & v53lu3m==. & v53fu3m==. & v53aum==. & v53bum==. & v53dum==. & v53cum==. & v53jum==. & v53gum==. & v53hum==. & v53iu3m==. & v53lu3m==. & v53fum==.
replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & yjub2==. & yremesad2==. */
label var ynlm_ci "Ingreso No Laboral Monetario"

**************
***ynlnm_ci***
**************
egen ynlnm_ci=rsum(yremesade yremesae yayudae yayupare), missing
/*replace ynlnm_ci=. if yremesade==. & yremesae==. & yayudae==. & yayupare==.
replace ynlnm_ci=0 if v53fu3e==. & v53fue==. & v53gu3e==. & v53gue==. & v53hu3e==. & v53hue==. */
label var ynlnm_ci "Ingreso No Laboral No Monetario"

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

******************************
*	autocons_ci 
******************************
gen autoconsumop_ci=p37 if p37<99999 & p37>=0 
replace autoconsumop_ci=0 if p37==. & edad>4 & (categopri==1 | categopri==2) & (p10==1 | p11==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p49 if p49<99999 & p49>=0 
replace autoconsumos_ci=0 if p49==. & edad>4 & (categosec==1 | categosec==2) & p38==1
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
*El valor de remesas es alto en relaci'on al resto de la serie, pero coincide con el valor
*de la variable generada por el Instituto de Estad'istica yremexhg

egen remesasm_ci=rsum(yremesa2 yremesad2), missing
replace remesasm_ci=. if yremesa2==. & yremesad2==. 
replace remesasm_ci=0 if v53gu3m==0 & v53gum==0 & v53fu3m==0 & v53fum==0 
label var remesasm_ci "Remesas Individuales (monetario)"

egen remesas_ci=rsum(yremesa2 yremesad2 yremesade yremesae), missing
replace remesas_ci=. if yremesa2==. & yremesad2==. & yremesade==. & yremesae==.
replace remesas_ci=0 if v53gu3m==0 & v53gum==0 & v53fu3m==0 & v53fum==0 & v53fu3e==0 & v53fue==0 & v53gu3e==0 & v53gue==0 
label var remesas_ci "Remesas Individuales (monetario + especies)"


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
egen trapri_ci= rowtotal(remesas_ci yayudae yayupare yayuda2 yayupar2  ), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

***************
***progpub_ci***
***************
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
egen trapub_ci= rowtotal(progpub_ci ysubsi2 ybonos2  ypensub_ci), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
egen capital_ci= rowtotal(interes2 yalquile2), missing
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
egen otros_ci= rowtotal(yotros2), missing
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
LA DIFERENCIA ES MENOR AL 5% */

******************************************************************************
*	Educación
*****************************************************************************

************
* asiste_ci*
************
gen asiste_ci=p04==1
*gen asiste_ci=.
*replace asiste_ci=1 if p04==1
*replace asiste_ci=0 if p04==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

* Años de educacion aprobados **
replace p03=. if p03==99
gen aedu_ci=.
replace aedu_ci=0 if p02>=1 & p02<=3
replace aedu_ci=p03 if p02==4 
replace aedu_ci=p03+6 if p02==5 | p02==6
replace aedu_ci=p03+12 if p02==7 | p02==8
replace aedu_ci=p03+17 if p02==9
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
replace edupre_ci=1 if (p02==3 & aedu_ci ~=.)
replace edupre_ci=0 if (p02~=3 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

******************************
*	pqnoasis 
******************************
g pqnoasis_ci=. /*NA*/
******************************
*	repite_ci 
******************************
g repite_ci=.  /*NA*/
******************************
*	repiteult_ci 
******************************
g repiteult_ci=. /*NA*/
******************************
*	edupub_ci 
******************************
g edupub_ci=.
la var edupub_ci "Personas que asisten a centros de ensenanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p02==7 
replace tecnica_ci=0 if p02!=7 & p02!=99
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
g       pqnoasis1_ci =.

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

* MGR Jul, 2015: la base disponible no asigna a todos los miembros del hogar  el valor de la variable de vivienda, únicamente al jefe.
* Ya que no contamos con bases originales para hacer merge nuevamente, expandimos variables de vivienda a nivel de hogar. 

foreach var of varlist v01 v02 v03 v04 v05a v05b v05c v06a v06b v07 v08 v09 v10a v10b v11a v11b {
bys idh_ch: egen `var'_hog = max(`var') 
}

* MGR Jul, 2015: se modifican todas las variables de vivienda utilizando la variable expandida al hogar en lugar de la variable original

/*
gen aguared_ch=0
replace aguared_ch = 1 if v05b ==1 | v05b==2
*/

gen aguared_ch=0
replace aguared_ch = 1 if v05b_hog ==1 | v05b_hog==2

/*
gen aguadist_ch=.
replace aguadist_ch=1 if v05c==1
replace aguadist_ch=2 if v05c==2 | v05c==3
replace aguadist_ch=3 if v05c==4
*/

gen aguadist_ch=.
replace aguadist_ch=1 if v05c_hog==1
replace aguadist_ch=2 if v05c_hog==2 | v05c_hog==3
replace aguadist_ch=3 if v05c_hog==4

/*
gen aguamala_ch=.
replace aguamala_ch=1 if v05b>=5 & v05b<=7
replace aguamala_ch=0 if v05b>=1 & v05b<=4
*/

gen aguamala_ch=.
replace aguamala_ch=1 if v05b_hog>=5 & v05b_hog<=7
replace aguamala_ch=0 if v05b_hog>=1 & v05b_hog<=4

gen aguamide_ch=.

/*
gen luz_ch=.
replace luz_ch=1 if v07==1 | v07==2 | v07==3
replace luz_ch=0 if v07>=4 & v07<=8
*/

gen luz_ch=.
replace luz_ch=1 if v07_hog==1 | v07_hog==2 | v07_hog==3
replace luz_ch=0 if v07_hog>=4 & v07_hog<=8

gen luzmide_ch=.

gen combust_ch=.

/*
gen bano_ch=.
replace bano_ch=1 if v06a>=1 & v06a<=6
replace bano_ch=0 if v06a==7
*/

gen bano_ch=.
replace bano_ch=1 if v06a_hog>=1 & v06a_hog<=6
replace bano_ch=0 if v06a_hog==7

/*
gen banoex_ch=.
replace banoex=1 if v06b==1
replace banoex=0 if v06b==2
*/

gen banoex_ch=.
replace banoex=1 if v06b_hog==1
replace banoex=0 if v06b_hog==2

/*
gen des1_ch=.
replace des1_ch=0 if v06a==7
replace des1_ch=1 if v06a==1 | v06a==2
replace des1_ch=2 if v06a==5 | v06a==6 | v06a==4
replace des1_ch=3 if v06a==3 
*/

gen des1_ch=.
replace des1_ch=0 if v06a_hog==7
replace des1_ch=1 if v06a_hog==1 | v06a_hog==2
replace des1_ch=2 if v06a_hog==5 | v06a_hog==6 | v06a_hog==4
replace des1_ch=3 if v06a_hog==3 

/*
gen des2_ch=.
replace des2_ch=1 if v06a==1 | v06a==2 | v06a==3 
replace des2_ch=2 if v06a==4 | v06a==5 | v06a==6 
replace des2_ch=0 if v06a==7
*/

gen des2_ch=.
replace des2_ch=1 if v06a_hog==1 | v06a_hog==2 | v06a_hog==5 | v06a_hog==6 | v06a_hog==4
replace des2_ch=2 if v06a_hog==3
replace des2_ch=0 if v06a_hog==7

/*
gen piso_ch=.
replace piso_ch=0 if v03==6
replace piso_ch=1 if v03>=1 & v03<=5 
replace piso_ch=2 if v03==7
*/

gen piso_ch=.
replace piso_ch=0 if v03_hog==6
replace piso_ch=1 if v03_hog>=1 & v03_hog<=5 
replace piso_ch=2 if v03_hog==7

/*
gen pared_ch=.
replace pared_ch=0 if v02==4 | v02==5
replace pared_ch=1 if v02>=1 & v02<=3
replace pared_ch=2 if v02==6
*/

gen pared_ch=.
replace pared_ch=0 if v02_hog==4 | v02_hog==5
replace pared_ch=1 if v02_hog>=1 & v02_hog<=3
replace pared_ch=2 if v02_hog==6

/*
gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8
*/

gen techo_ch=.
replace techo_ch=0 if v04_hog==6 | v04_hog==7
replace techo_ch=1 if v04_hog>=1 & v04_hog<=5
replace techo_ch=2 if v04_hog==8

/*
gen resid_ch=.
replace resid_ch=0 if v08==1
replace resid_ch=1 if v08==3 | v08==4
replace resid_ch=2 if v08==2 | v08==5
replace resid_ch=3 if v08==6 
*/

gen resid_ch=.
replace resid_ch=0 if v08_hog==1
replace resid_ch=1 if v08_hog==3 | v08_hog==4
replace resid_ch=2 if v08_hog==2 | v08_hog==5
replace resid_ch=3 if v08_hog==6 

/*
gen dorm_ch=.
replace dorm_ch=v11b if v11b>=0 
*/

gen dorm_ch=.
replace dorm_ch=v11b_hog if v11b_hog>=0 

/*
gen cuartos_ch=.
replace cuartos_ch=v11a if v11a>=0 
*/

gen cuartos_ch=.
replace cuartos_ch=v11a_hog if v11a_hog>=0 

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.


/*
gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==2
replace vivi1_ch=3 if v01>=3 & v01<=8
*/

gen vivi1_ch=.
replace vivi1_ch=1 if v01_hog==1
replace vivi1_ch=2 if v01_hog==2
replace vivi1_ch=3 if v01_hog>=3 & v01_hog<=8

/*
gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3
*/

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

/*
gen viviprop_ch=.
replace viviprop_ch=0 if v10a==5
replace viviprop_ch=1 if v10a==1 
replace viviprop_ch=2 if v10a==4
replace viviprop_ch=3 if v10a==6 | v10a==2 | v10a==3
*/

gen viviprop_ch=.
replace viviprop_ch=0 if v10a_hog==5
replace viviprop_ch=1 if v10a_hog==1 
replace viviprop_ch=2 if v10a_hog==4
replace viviprop_ch=3 if v10a_hog==6 | v10a_hog==2 | v10a_hog==3

gen vivitit_ch=.

/*
gen vivialq_ch=.
replace vivialq_ch=v10b if viviprop_ch==0
*/

gen vivialq_ch=.
replace vivialq_ch=v10b_hog if viviprop_ch==0

gen vivialqimp_ch=.

**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g  aguamejorada_ch = 1 if (v05b >=1 & v05b <=4)
replace aguamejorada_ch = 0 if (v05b >=5 & v05b <=7)

*********************
***banomejorado_ch***
*********************
g  banomejorado_ch = 1 if ( v06a >=1 & v06a <=5) & v06b ==1
replace banomejorado_ch = 0 if ((v06a >=1 & v06a <=5) & v06b ==2) | (v06a==6)


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

* Instituto Nacional de Estadística, Encuesta Permanente de Hogares de Propósitos Múltiples. Sept, 2002.

          
/*
rela_j
 1. jefe del hogar
 2. esposa(o) o compañera(o) 
 3. hijos o hijastros de mayor a menor
 4. padres 
 5. yernos y nueras 
 6. otros parientes(nietos,abuelos,tios...)
 7. otros no parientes(suegros,cuñados...)
 8. servicio doméstico
*/

* Variables

 rename rela_j parentco
 rename ur area
 rename p01 alfabet
 rename p04 asiste

 recode parentco (6=1) if nper==1

* Gender classification of the population refering to the head of the household.

 sort hogar nper

 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(hogar)

* Dwelling ID

 tostring hogar,gen(hogar_s) format(%12.0f) force

 gen id_viv=substr(hogar_s,1,7)	/* hogar_s => String variable identical to "hogar" */

** Years of education. 
/*
2. ¿Asiste actualmente a algún establecimiento de enseñanza?
 1. SI / 2. NO

 
3. ¿Cuál es el nivel mas alto de estudio que esta cursando o curso? y
¿cuál es el último año aprobado en ese nivel? (p03b)
			
 01. ninguno	
 02. pre primaria
 03. centro de alfabetización	
 04. maestro en casa			Ultimo año aprobado
 05. primaria(1-9)				==> p03b
 06. secundaria ciclo común(1-3)		==> p03b
 07. secundaria diversificada(1-4)		==> p03b
 08. superior no universitaria(1-4)		==> p03b
 09. superior universitaria(1-8)		==> p03b
 10. post-grado(1-4)				==> p03b	
 11. no sabe, no respondió	
			
*/
* Included in the database

 rename anosest a_orig

 gen	 anoest=0  if (p02==1 | p02==2 | p02==3 | p02==4) | (p02==5 & p03==0) 
 replace anoest=1  if (p02==5 & p03==1) 	
 replace anoest=2  if (p02==5 & p03==2) 	
 replace anoest=3  if (p02==5 & p03==3) 	
 replace anoest=4  if (p02==5 & p03==4) 	
 replace anoest=5  if (p02==5 & p03==5) 	
 replace anoest=6  if (p02==5 & p03==6) | (p02==6 & p03==0) 	
 replace anoest=7  if (p02==5 & p03==7) | (p02==6 & p03==1) 
 replace anoest=8  if (p02==5 & p03==8) | (p02==6 & p03==2) 
 replace anoest=9  if (p02==5 & p03==9) | (p02==6 & p03==3) | (p02==7 & p03==0) 
 replace anoest=10 if (p02==7 & p03==1) 
 replace anoest=11 if (p02==7 & p03==2) 
 replace anoest=12 if (p02==7 & p03==3) | (p02==9 & p03==0) | (p02==8 & p03==0)  
 replace anoest=13 if (p02==7 & p03==4) | (p02==9 & p03==1) | (p02==8 & p03==1)  
 replace anoest=14 if (p02==9 & p03==2) | (p02==8 & p03==2)  
 replace anoest=15 if (p02==9 & p03==3) | (p02==8 & p03==3)  
 replace anoest=16 if (p02==9 & p03==4) | (p02==8 & p03==4)
 replace anoest=17 if (p02==9 & p03==5) | (p02==10 & p03==0) 
 replace anoest=18 if (p02==9 & p03==6) | (p02==10 & p03==1) 
 replace anoest=19 if (p02==9 & p03==7) | (p02==10 & p03==2) 
 replace anoest=20 if (p02==9 & p03==8) | (p02==10 & p03==3) 
 replace anoest=21 if (p02==10 & p03==4)


** Economic Active Population (10 years or more of age)
* Included in the database
* condact
/* Condición de Actividad
1. Ocupados	
2. Desocupados
3. Inactivos
*/

 gen 	 TASADESO=0 if condact==1
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

 gen strata=domi if domi<5
 replace strata=5 if domi==5 & estrato1==0
 replace strata=6 if domi==5 & estrato1==1

* PSU

 gen str5 psu=substr(hogar_s,1,5)
 
 svyset [pweight=factor_ch], strata(strata) psu(psu)
 svydes

************
** ETHNIC **
************

/*
5 years or more of age.

9. ¿a que grupo etnico o raza considera que pertenece(...)? 
p09

01. garifuna    
02. negro ingles 
03. tolupán	
04. pech (paya)	
05. misquito 
06. lenca
07. tawahka (sumo)
08. chorti
09. mestizo ó ladino
10. otro
99. ns/nr
*/

* gen	 indigena = .

/*
 gen	 indigena=1 if (p09==1 | (p09>=3 & p09<=8))
 replace indigena=0 if (p09==2 | p09==9 | p09==10)
*/

** Missings. Persons with less than 5 years of age
* Filling missings using the mother auto identification.
* 3. "hijos o hijastros de mayor a menor"

* gen pertn_m=indigena if (parentco==2 & sexo==2) | (parentco==1 & sexo==2)
 
* egen pertn_mh=max(pertn_m), by(hogar)
 
* replace indigena=pertn_mh if edad<=4 & (parentco==3)

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

/*
Departamentos

 1. Atlantida 
 2. Colon 
 3. Comayagua 
 4. Copan 
 5. Cortes 
 6. Choluteca 
 7. El Paraiso 
 8. Francisco Morazan
 9. Gracias a Dios	==> NOT INCLUDED IN THE SAMPLE
10. Intibuca
11. Islas de la Bahía   ==> NOT INCLUDED IN THE SAMPLE
12. La Paz 
13. Lempira 
14. Ocotepeque 
15. Olancho 
16. Santa Barbara 
17. Valle
18. Yoro 
*/ 

 gen region2=depto
 
************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*

2. ¿Asiste actualmente a algún establecimiento de enseñanza?
1. SI 

3. ¿Cuál es el nivel mas alto de estudio que esta cursando o curso? y
¿cuál es el último año aprobado en ese nivel? (p03)
p02. 
			
01. ninguno	
02. pre primaria
03. centro de alfabetización	
04. maestro en casa			Ultimo año aprobado
05. primaria(1-9)				==> p03
06. secundaria ciclo común(1-3)			==> p03
07. secundaria diversificada(1-4)		==> p03
08. superior no universitaria(1-4)		==> p03
09. superior universitaria(1-8)			==> p03
10. post-grado(1-4)				==> p03	
11. no sabe, no respondió	
			
*/

 rename p02 niveduc
 rename p03 ultgrad

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if (edad>=7 & edad<=12) & (asiste>=1 & asiste<=2) 
 replace NERP=1 if (edad>=7 & edad<=12) & (asiste==1) & (niveduc==5 & (ultgrad>=0 & ultgrad<=5))
 label var NERP "Net Enrolment Ratio in Primary"	

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS=1 if (edad>=13 & edad<=18) & (asiste==1) & ((niveduc==5 & (ultgrad>=6 & ultgrad<=9)) | ((niveduc==6)  | (niveduc==7 & (ultgrad>=0 & ultgrad<=2))))
 label var NERS "Net Enrolment Ratio in Primary"

* Upper secondary
* Secundaria Diversificado

 gen	 NERS2=0 if (edad>=16 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS2=1 if (edad>=16 & edad<=18) & ((niveduc==7 & (ultgrad>=0 & ultgrad<=2)) | (niveduc==6 & ultgrad==3) )
 label var NERS2 "Net Enrolment Ratio in Secondary - upper"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 
 label var ALFABET "Literacy Rate of 15-24 Years Old"

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen     ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1)
 label var ALFABET2 "Literacy Rate of 15-24 Years Old INE"

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if (asiste==1) & (niveduc==5 & (ultgrad>=0 & ultgrad<=5))
 gen sec=1  if (asiste==1) & ((niveduc==5 & (ultgrad>=6 & ultgrad<=9)) | ((niveduc==6)  | (niveduc==7 & (ultgrad>=0 & ultgrad<=2))))
 gen ter=1  if (asiste==1) & (((niveduc==8 & (ultgrad>=0 & ultgrad<=3)) | (niveduc==9 & (ultgrad>=0 & ultgrad<=4))) | (niveduc==7 & (ultgrad==4 | ultgrad==3)))

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
 label var RATIOLIT "Ratio of Literate Women to Men 15-24 year olds INE"

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*

34. ¿Cuál es o era su categoría ocupacional 		rama
en la ocupación principal?			 	rama de actividad economica
1. Empleado u obrero publico	
2. Empleado u obrero privado(exc.serv.dom)	
3. Servicio domestico	
4. Miembro de cooperativa, asentamiento	
5. Trabajador por cuenta propia que no contrata mano de obra temporal	
6. Trabajador por cuenta propia que contrata mano de obra temporal	
7. Empleador o socio activo
8. Trabajado familiar no remunerado
9. Trabajador no remunerado
99. ns/nr	

categop (variable included in the database) =>
categoria ocupacional (ocupacion principal)

1. Empleado publico 
2. Empleado privado
3. Empleada domestica
4. Cuenta propia 
5. Trabajador no remunerado 

*/

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=2) & (rama>=2 & rama<=9) & (condact==1)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=2) & (rama>=2 & rama<=9) & (condact==1) & (sexo==2)
 label var WENAS "WENAS without domestic servants"
 
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (rama>=2 & rama<=9) & (condact==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categop>=1 & categop<=3)  & (rama>=2 & rama<=9) & (condact==1) & (sexo==2)
 label var WENASD "WENAS with domestic servants"

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

/*These variables are  available in the database only for the household head*/

** Access to Electricity ** Additional Indicator
/*
6. ¿Qué tipo de alumbrado tiene?
 1. Servicio público
 2. Planta privada colectiva
 3. Planta privada individual
 4. Energia solar
 5. Vela
 6. Candil o lampara de gas
 7. Otro
 */

 egen elec=max(v07), by(hogar)

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (elec>=1 & elec<=7) /* Total population excluding missing information */
 replace ELEC=1 if (elec>=1 & elec<=4)
 label var ELEC "Proportion of Population with access to electricity"
 
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

/*
4. Servicio de Agua (v05a)
a) ¿Tiene tubería instalada para agua?
 1. Sí  2. No

b) ¿De dónde proviene el agua que utiliza? (v05b)
 1. Servicio Público 
 2. Servicio Privado colectivo
 3. Pozo malacate
 4. Pozo con bomba
 5. Río, riachuelo, manantial, ojo de agua
 6. Cisterna
 7. Otro

c) ¿De dónde la obtiene? (v05c)
 1. Dentro de la vivienda
 2. Fuera de la vivienda y dentro de la propiedad
 3. Fuera de la propiedad a menos de 100 metros
 4. Fuera de la propiedad a más de 100 metros

*/
/*These variables are  available in the database only for the household head*/

 egen agua=max(v05b), by(hogar)
 egen lugabast=max(v05c), by(hogar)

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (agua>=1 & agua<=7) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4) 
 label var WATER "Improved Water Source"

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*
5. Servicio Sanitario
a) ¿Qué tipo de servicio sanitario tiene? (v05a)
 1. Inodoro conectado a red de alcantarilla
 2. Inodoro conectado a pozo séptico
 3. Letrina con cierre hidráulico
 4. Letrina con pozo séptico
 5. Letrina con pozo negro
 6. No tiene

b) El uso del servicio sanitario es:
 1. Exclusivo de la vivienda
 2. Compartido con otras viviendas
*/

/*These variables are  available in the database only for the household head*/

 egen sanita=max(v06a), by(hogar)
 egen servexc=max(v06b), by(hogar)

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (sanita>=1 & sanita<=6) /* Total population excluding missing information */
 replace SANITATION=1 if (sanita>=1 & sanita<=2) | sanita==4
 label var SANITATION "Improved Sanitation"

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*

1. Tipo de vivienda			10. ¿En caracter de qué ocupan esta vivienda?
v01					v10a
1. Casa o apartamento independiente	1. Propietario y esta pagada totalmente
2. Rancho				2. Propietario y la esta pagando
3. Cuarto en mesón o cuartería 		3. Alquilada
4. Barracón				4. Cedida sin pago
5. Casa improvisada			5. Recuperada legalizada
6. Local no construido para habitación	6. Recuperada sin legalizar
pero usado como vivienda		9. ns/n
7. Otro							
9. ns/nr 
		
2. Material predominante en		3. ¿Cuál es el material predominante en el piso?
la construcción de las paredes(v02)	v03
1. Ladrillo, piedra o bloque		1. Cerámica
2. Adobe				2. Ladrillo de cemento
3. Madera				3. Ladrillo de barro
4. Bahareque, vara o caña		4. Plancha de cemento
5. Desechos				5. Madera
6. Otro					6. Tierra
9. ns/nr				7. Otro
					9. ns/nr	

11. Cantidad de Piezas de la Vivienda
a). ¿Cuántas piezas tiene esta vivienda?
(incluya la cocina pero no el baño)
v11a
*/

/*These variables are  available in the database only for the household head*/

 egen tenencia=max(v10a), by(hogar)
 egen tipoviv=max(v01), by(hogar)
 egen piso=max(v03), by(hogar)
 egen pared=max(v02), by(hogar)
 recode v11a (40=.)
 egen nrocuart=max(v11a), by(id_viv)
 egen pers=max(totpervi ), by(hogar) /* Total de personas de la vivienda */

 gen persroom=pers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=7) & (tenencia>=1 & tenencia<=6) /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==2 | tipoviv==4 | tipoviv==5 | tipoviv==6 | tipoviv==7) | (tenencia==4)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (pared>=1 & pared<=6) & (piso>=1 & piso<=7) /* Total population excluding missing information */
 replace secten_2=1 if (pared>=4 & pared<=6) | (piso==6 | piso==7)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 gen	 SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)
 label var SECTEN "Secure Tenure"

* Dirt floors

* Gender classification of the population refers to the head of the household.

* 3. ¿Cuál es el material predominante en el piso?

 gen	 DIRT=0 if (piso>=1 & piso<=7) /* Total population excluding missing information */
 replace DIRT=1 if (piso==6)
 label var DIRT "Proportion of Population living in dwellings with dirt floors"

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (TASADESO==0 | TASADESO==1) 
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (TASADESO==1 )
 label var UNMPLYMENT15 "Unemployment Rate 15 to 24"

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen CHILDREN=0 if      (edad>=12 & edad<15) 
 replace CHILDREN=1 if ((edad>=12 & edad<15) & (condact==1))
 label var CHILDREN "Children Under Age 15 who are Working"

** CCA 41 Number of Persons per Room*

 gen PERSROOM2=persroom if parentco==1
 global variable PERSROOM2
 label var PERSROOM2 "Persons per Room" 

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
 label var PLT2 "Population living in households with less than 2 persons (inclusive) per room"

** Disconnected Youths
/*
p15. ¿Por qué no buscó trabajo ni trató de establecer su propio
negocio o finca?
1. Se incorporará a un trabajo antes de un mes			
2. Tiene trabajo asegurado después de un mes 			
3. Tiene respuesta a gestiones 						
4. Está esperando la siguiente temporada de trabajo 		
5. Cree que no encontrará trabajo					
6. Dejó de buscar trabajo momentáneamente			
7. No tiene tierra ni capital 						
8. No tiene tiempo para buscar trabajo				
9. No tiene necesidad de trabajar					
10. Por su edad no puede trabajar					
11.  Otros										

p16. ¿Cuál es su condición actual?
1. Jubilado o pensionista
2. Rentista
3. Estudiante
4. Realiza los quehaceres del hogar
5. Incapacitado temporalmente
6. Incapacitado permanentemente
7. Otros
*/

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & ((p16==7) & (p15==5 | p15==6 | (p15>=8 & p15<=11))) 
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
 label var GFAS "Grade for age secondary"



