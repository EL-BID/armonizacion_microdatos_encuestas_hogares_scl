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
local ANO "2007"
local ronda m9 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Yessenia Loaysa (abr-2013)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Armonización: Mayra Sáenz  - 8 de Octubre de 2013 - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha última modificación: Noviembre de 2017

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
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

	****************
	* region_c *
	****************
gen region_c= depto

label define region_c  ///
           1 "Atlantida" ///
           2 "Colon" ///
           3 "Comayagua" ///
           4 "Copan" ///
           5 "Cortes" ///
           6 "Choluteca" ///
           7 "El Paraiso" ///
           8 "Francisco Morazan" ///
           9 "Gracias a Dios" ///
          10 "Intibuca" ///
          11 "Islas de la bahia" ///
          12 "La paz" ///
          13 "Lempira" ///
          14 "Ocotepeque" ///
          15 "Olancho" ///
          16 "Santa Barbara " ///
          17 "Valle" ///
          18 "Yoro"
 
label value region_c region_c
label var region_c "Division política, departamentos"


/*
*gen mes_c=""
tostring fecha, replace
replace mes_c =substr(fecha,2,2) if length(fecha)==7
replace mes_c =substr(fecha,3,2) if length(fecha)==8
destring mes, replace
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre"
label value mes_c mes_c*/


***************
***factor_ch***
***************

****** ASK ABOUT EXPANSION FACTOR FOR THE HOUSEHOLD *****
gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"

***************
****idh_ch*****
*************** 
gen double idh_ch=hogar
format idh_ch %20.0g 
                
**********
***zona***
**********
gen zona_c=1 if domi==1 | domi==2 | domi==3 
replace zona_c=0 if domi==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

************
****pais****
************
gen pais_c="HND"


**********
***anio***
**********
gen anio_c=2007


*********
***mes***
*********
gen mes_c=9

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
*MGR: correción en sintaxis 5/2016
*replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j== 8 
*replace relacion_ci=5 if rela_j==7 | rela_j==9 | rela_j==11 
replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j==7 | rela_j== 8 
replace relacion_ci=5 if rela_j==9 
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

***************
*idp_ci       *	
***************
gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"

	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************
gen factor_ci=factor_ch
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
gen edad_ci=edad if edad <99
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
gen condocup_ci=condact
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/
* Comprobacion con variables originales.  Se considera ocupado a quienes estan en trabajos no remunerados. 5/28/2014 MGD
* La edad minima de la encuesta se cambia a 5 anios.

g condocup_ci=.
replace condocup_ci=1 if (p34==1 | p35==1 | p36==1)
replace condocup_ci=2 if (p34==2 | p35==2 | p36==2) & (p38==1 | p39==1) 
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
replace desalent_ci=1 if p42==6
replace desalent_ci=0 if p42!=6 & p42!=.
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

/* CATEGOPRI
   En 2003, 
   p027. ¿Cuál ES o ERA su categoría ocupacional PRINCIPAL?

   1. Empleado u obrero publico
   2. Empleado u obrero privado
   3. Empleado domestico
   4. Miembro de cooperativa, asentamiento o grupo
   5. Trab. por cta. propia que no contr. mano de obra temporal
   6. Trab. por cta. propia que contr. mano de obra temporal
   7. Empleador o socio activo
   8. Trabajado familiar no remunerado
   9. Trabajador no remunerado
  99. No sabe, no responde
   
   En 2007,    
   RP50. Ultima Ocupación para Desocupados
   53.  En la ocupación de [LEER RP50] ¿usted trabajó como:
   
        1. Empleado(a) u obrero público          
        2. Empleado(a) u obrero privado     
        3. Empleado(a) doméstico(a) 
                                    
        4. Miembro de cooperativa, asentamiento o grupo                                   
        5. Cuenta propia que no contrata mano de obra temporal
        6. Cuenta propia que contrata mano de obra temporal
        7. Empleador o patrón   
                                         
        8. Trabajador familiar no remunerado 
        9. Trabajador no remunerado 

    RP60. Ultima Ocupación para Ocupados
    66.  En la ocupación de [LEER RP60] ¿usted trabajó como:
    
	TRABAJADORES ASALARIADOS
	     1. Empleado(a) u obrero público 
	     2. Empleado(a) u obrero privado 
	     3. Empleado(a) doméstico(a)               

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
            13. Trabajador no remunerado	*/


gen categopri_ci=.
/*
replace categopri_ci=1 if (p53==7)
replace categopri_ci=2 if (p53==4 | p53==5 | p53==6)
replace categopri_ci=3 if (p53==1 | p53==2 | p53==3)
replace categopri_ci=4 if (p53==8 | p53==9)
*/
replace categopri_ci=1 if ( p66==6 | p66==7 | p66==10| p66==11)
replace categopri_ci=2 if (p66==4 | p66==5 |p66==8 | p66==9 )
replace categopri_ci=3 if (p66==1 | p66==2 | p66==3)
replace categopri_ci=4 if (p66==12| p66==13)
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci


/* CATEGOSEC_CI
   Ocupacion Secundaria,
   
                p96 Usted trabaja como: |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
               1. Empl u obrero publico |        103        1.23        1.23
               2. Empl u obrero privado |      1,715       20.48       21.70
                  3. Empleado domestico |         23        0.27       21.98
4. Miembro de cooperativa de producción |          5        0.06       22.04
5. Cta. propia que no contrata mano de  |      1,539       18.37       40.41
6. Cta. propia que contrata mano de obr |        259        3.09       43.51
           7. Empleador o socio activo  |        109        1.30       44.81
8. Miembro de cooperativa, asentamiento |          9        0.11       44.91
9. Cuenta propia que no contrata mano d |      1,886       22.52       67.43
10. Cuenta propia que contrata mano de  |      1,249       14.91       82.34
         11. Patron o socio de la finca |         71        0.85       83.19
  12. Trabajador familiar no remunerado |      1,352       16.14       99.33
           13. Trabajador no remunerado |         56        0.67      100.00
----------------------------------------+-----------------------------------
                                  Total |      8,376      100.00  	
*/

gen categosec_ci=.
replace categosec_ci=1 if (p96==6 | p96==7 | p96==11 | p96==10)
replace categosec_ci=2 if (p96==4 | p96==5 |  p96==8 | p96==9 )
replace categosec_ci=3 if (p96==1 | p96==2 | p96==3)
replace categosec_ci=4 if (p96==12| p96==13)
recode categosec_ci (1=2) if (p96==6 | p96==10) &  p109==4
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci


*****************
***horaspri_ci***
*****************
gen horaspri_ci=p65 if p65<=168
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p95 if p95<168

************
*horastot_ci
************

egen horastot_ci = rsum(horaspri_ci horassec_ci), missing
replace horastot_ci=. if horaspri_ci==. & horassec_ci==.
replace horastot_ci = thoras if horastot_ci <thoras
replace horastot_ci = thoras if horastot_ci ==. & thoras >=0
replace horastot_ci = . if horastot_ci>168

label var horastot_ci "Horas totales trabajadas en todas las Actividades"

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=p44a/30   if p44b==1
replace durades_ci=p44a/4.3  if p44b==2
replace durades_ci=p44a      if p44b==3
label var durades_ci "Duracion del Desempleo (en meses)"


*MLO: modificacion

tostring p58_1,replace
gen aux_a=substr(p58_1,1,1) if p58_1!="9999999999" & p58_1!="."
gen aux_b=substr(p58_1,2,1) if p58_1!="9999999999" & p58_1!="."
gen aux_c=substr(p58_1,3,1) if p58_1!="9999999999" & p58_1!="."
gen aux_d=substr(p58_1,4,1) if p58_1!="9999999999" & p58_1!="."
gen aux_e=substr(p58_1,5,1) if p58_1!="9999999999" & p58_1!="."
gen aux_f=substr(p58_1,6,1) if p58_1!="9999999999" & p58_1!="."
destring aux_*, replace

***************
***subemp_ci***
***************
/*
gen subemp_ci=.
replace subemp_ci=0 if emp_ci==0 | emp_ci==1
replace subemp_ci=1 if horastot<30 & p119==1
label var subemp_ci "Trabajadores subempleados"
*/
* Modificacion MGD 06/20/2014: solo horas del trabajo principal.
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p119==1 & emp_ci==1
label var subemp_ci "Trabajadores subempleados"


*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
replace antiguedad_ci=p68a/365           if p68b==1
replace antiguedad_ci=p68a/51.6            if p68b==2
replace antiguedad_ci=p68a/12            if p68b==3
replace antiguedad_ci=p68a               if p68b==4
*Mayra Sáenz Octubre 2013/ Se incluye a los cuenta propia
replace antiguedad_ci=p81a/365           if p81b==1
replace antiguedad_ci=p81a/51.6            if p81b==2
replace antiguedad_ci=p81a/12            if p81b==3
replace antiguedad_ci=p81a               if p81b==4
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & p119==2
label var tiempoparc_ci "Trabajadores a medio tiempo"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p89==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Numero de empleos"

/*
*************
*firmapeq_ci*
*************
*Asalariados, cuenta propia, y cuenta propia actividades no agrícolas de la actividad principal
gen firmapeq_ci=0 if (p67b>5 & p67b<99999) | (p80b>5 & p80b<99999) | (p88b >5 & p88b <99999)
replace firmapeq_ci=1 if (p67b<=5 & p67b!=0) | (p80b<=5 & p80b!=0) | (p88b <=5 & p88b !=0)
 */

*****************
***spublico_ci***
**************
gen spublico_ci=1 if p66==1 
replace spublico_ci=0 if p66!=1 

label var spublico_ci "Personas que trabajan en el sector publico"

*************
**ocupa_ci***
*************
tostring p60, replace
replace p60 = "0" + p60 if length(p60)==6
gen labor=substr(p60,1,4)
destring labor, replace 

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999 & emp_ci==1
replace ocupa_ci=2 if labor>=1000 & labor<=1999 & emp_ci==1
replace ocupa_ci=3 if labor>=4000 & labor<=4999 & emp_ci==1
replace ocupa_ci=4 if ((labor>=5200 & labor<=5299) | (labor>=9100 & labor<=9119)) & emp_ci==1
replace ocupa_ci=5 if ((labor>=5100 & labor<=5199) | (labor>=9120 & labor<=9191)) & emp_ci==1
replace ocupa_ci=6 if ((labor>=6000 & labor<=6999) | (labor>=9210 & labor<=9231)) & emp_ci==1
replace ocupa_ci=7 if ((labor>=7000 & labor<=8999) | (labor>=9300 & labor<9410)) & emp_ci==1
replace ocupa_ci=8 if labor>0 & labor<=999 & emp_ci==1
replace ocupa_ci=9 if (labor==9999 | labor==9410 | labor==9500 | labor==5430 | labor==5520 | labor==5020) & emp_ci==1
drop labor 

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

tostring p90, replace
replace p90 = "0" + p90 if length(p90)==6
gen labor2=substr(p90,1,4)
destring labor2, replace 

gen ocupasec_ci=.
replace ocupasec_ci=1 if labor>=2000 & labor<=3999
replace ocupasec_ci=2 if labor>=1000 & labor<=1999
replace ocupasec_ci=3 if labor>=4000 & labor<=4999
replace ocupasec_ci=4 if labor>=5200 & labor<=5999
replace ocupasec_ci=5 if labor>=5000 & labor<=5199
replace ocupasec_ci=6 if labor>=6000 & labor<=6999
replace ocupasec_ci=7 if labor>=7000 & labor<=8999
replace ocupasec_ci=8 if labor>=0 & labor<=999
replace ocupasec_ci=9 if (labor>=9000 & labor<=9996) | labor==9999
 
label var ocupasec_ci "Ocupacion Laboral en la Actividad Secundaria"
label define ocupasec_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupasec_ci ocupasec_ci

*************
***rama_ci***
*************
gen rama_ci=.
replace  rama_ci=1 if (p63>=111005 & p63<=751305) & emp_ci==1
replace  rama_ci=2 if (p63>=1010000 & p63<=1429034) & emp_ci==1
replace  rama_ci=3 if (p63>=1511000 & p63<=3910075) & emp_ci==1
replace  rama_ci=4 if (p63>=4010001 & p63<=4215003) & emp_ci==1
replace  rama_ci=5 if (p63>=4500028 & p63<=4550002) & emp_ci==1
replace  rama_ci=6 if (p63>=5010000 & p63<=5939035) & emp_ci==1
replace  rama_ci=7 if (p63>=6003003 & p63<=6499003) & emp_ci==1
replace  rama_ci=8 if (p63>=6500008 & p63<=7020019) & emp_ci==1
replace  rama_ci=9 if (p63>=7111000 & p63<=9900027) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

**********
**tc_ci***
**********
gen tc_ci=19.03
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HON 2007
gen salmm_ci=. 	/*3024.90*/
replace salmm_ci=2537.55 if rama_ci==1
replace salmm_ci=2739.15 if rama_ci==2 | rama_ci==3 | rama_ci==5 | rama_ci==6 | rama_ci==9
replace salmm_ci=2865.6 if rama_ci==7 
replace salmm_ci=3223.28 if rama_ci==8
replace salmm_ci=2807.96 if rama_ci==4 | salmm_ci==.
label var salmm_ci "Salario minimo legal"



****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (aux_a>=1 & aux_a<=6) |  (aux_b>=1 & aux_b<=6)| (aux_c>=1 & aux_c<=6)| (aux_d>=1 & aux_d<=6) | (aux_e>=1 & aux_e<=6) | (aux_f>=1 & aux_f<=6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

*****************
*tipocontrato_ci*
*****************
/*
recode p69 (1=1) (3=2 3) (nonmissing=.), gen(tipocontrato_ci)
label drop tipocontrato_ci
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1"Permanente/indefinido" 2"Temporal" 3"Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

* Modificacion: la variable se crea solamente considerando firma de contrato sin temporalidad. MGD 06/16/2014
g tipocontrato_ci=.
replace tipocontrato_ci=1 if (p69==1 & p70==2) & categopri_ci==3
replace tipocontrato_ci=2 if (p69==1 & p70==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p69==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=p48 if p48==1 & condocup_ci==2
replace cesante_ci=0 if p48==2 & condocup_ci==2
replace cesante_ci=. if p48==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	


*************
*tamemp_ci
*************

foreach var in p67b p80b p88b  {
recode `var' (99999=.)
}
* Honduras. Pequeña 1-5, Mediana 6-50, Grande Más de 50.
gen tamemp_ci=.
replace tamemp_ci = 1 if (p67b>=1 & p67b<=5) | (p80b>=1 & p80b<=5) | (p88b >=1 & p88b <=5)
replace tamemp_ci = 2 if (p67b>=6 & p67b<=50) | (p80b>=6 & p80b<=50) | (p88b >=6 & p88b <=50)
replace tamemp_ci = 3 if (p67b>50 & p67b~=.) | (p80b>50 & p80b~=.) | (p88b >50 & p88b ~=.)
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande", modify
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

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

*************
**ypen_ci*
*************
*MGD 07/2015: sin considerar ceros
egen ypen_ci=rsum(pension_esp pension_efe jubilacion_esp jubilacion_efe), missing
label var ypen_ci "Valor de la pension contributiva"


****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*************
**pension_ci*
*************
gen pension_ci=1 if ypen_ci!=0 & ypen_ci!=.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************
**DZ Octubre 2017,PRAF corresponde al programa de transferencias monetarias condicionadas, por lo cual hay más beneficiarios aparte de los adultos mayores. Se restringe esta variable.
egen ypensub_ci=rowtotal(bonpraf_esp bonpraf_efe) if edad_ci>=65, missing
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=ypensub_ci!=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


/*
/*Incorporada en 2007
69. ¿Esta trabajando bajo contrato?
    1. Contrato individual
    2. Acuerdo Individual
    3. Otro
    4. No sabe/No responde  */
/*
gen contrato_ci=.
replace contrato_ci=1 if p69==1
replace contrato_ci=0 if (p69==2 | p69==3)
label var contrato "Peronas empleadas que han firmado un contrato de trabajo"
*/
/*
Asalariados, Ocupacion Principal

74. En el trabajo, ¿tiene usted  derecho a:                  

      1.    O     Pensión? 
      2.    O     Prestaciones laborales?
      3.    O     Vacaciones?
      4.    O     Pago de horas extra?
      5.    O     Seguro por accidente?
      6.    O     Aguinaldo (décimo tercer salario)?
      7.    O     Décimo cuarto salario?
      8.    O     Bonificaciones?
      9.    O     Seguro de vida?
     10.    O     Ninguno de los anteriores?
     99.    O     No sabe / no responde?

gen segsoc_ci=.
label var segsoc "Personas que cuentan con seguro social"

Asalariados, Ocupacion Secundaria

104. En el trabajo, ¿tiene usted derecho a:                              

      1.   O     Pensión? 
      2.   O     Prestaciones laborales?
      3.   O     Vacaciones?
      4.   O     Pago de horas extra?
      5.   O     Seguro por accidente?
      6.   O     Aguinaldo (decimo tercer salario)?
      7.   O     Décimo cuarto salario?
      8.   O     Bonificaciones?
      9.   O     Seguro de vida?
     10.   O     Ninguno de los anteriores?
     99.   O     No sabe / no responde?

*/

tostring p74, replace
replace p74="." if p74=="9999999999"

replace p74="0"     +p74 if length(p74)==9 & p74!="."
replace p74="00"     +p74 if length(p74)==8 & p74!="."
replace p74="000"     +p74 if length(p74)==7 & p74!="."
replace p74="0000"     +p74 if length(p74)==6 & p74!="."
replace p74="00000"     +p74 if length(p74)==5 & p74!="."
replace p74="000000"     +p74 if length(p74)==4 & p74!="."
replace p74="0000000"     +p74 if length(p74)==3 & p74!="."
replace p74="00000000"     +p74 if length(p74)==2 & p74!="."
replace p74="000000000"     +p74 if length(p74)==1 & p74!="."

forval i=1/10 {
gen xx`i' = substr(p74,`i',1)
destring xx`i', replace
}

gen p74_1=0
replace p74_1=1 if xx1==1|xx2==1|xx3==1|xx4==1|xx5==1|xx6==1|xx7==1|xx8==1|(xx9==1 & xx10!=0)|xx10==1

forval i=2/9 {
gen p74_`i'=0
replace p74_`i'=1 if xx1==`i'|xx2==`i'|xx3==`i'|xx4==`i'|xx5==`i'|xx6==`i'|xx7==`i'|xx8==`i'|xx9==`i'|xx10==`i'
}

* Valor 10 = Ninguna de las anteriores
destring p74, replace
forval i=1/9 {
replace p74_`i'=0 if p74==10
}

gen p74_10=0
replace p74_10=1 if p74==10
drop xx*
*/


*******************
***categoinac_ci***
*******************


gen categoinac_ci =1 if ((p40 ==2 | p40==3) & condocup_ci==3)
replace categoinac_ci = 2 if  (p40==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (p40==6 & condocup_ci==3)
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


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
/*YL ->linea estimada (no encuentro las originales) correspond al mes de agosto*/
generat lp_ci =1919.16 if zona_c==1
replace lp_ci =1067.53  if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

*gen lpe_ci =pobreza==3
*Modificación Mayra Sáenz - Marzo 2014
g lpe_ci =.
replace lpe_ci = 982.96 if zona_c == 1
replace lpe_ci = 753.48 if zona_c == 0
label var lpe_ci "Linea de indigencia oficial del pais"

************************************************************************
**************************INGRESOS**************************************
************************************************************************

local varing p76  p77 p86 p66 p55 p57 p78_5 p78_6 p78_7 p78_8 p78_1 p78_2 p78_3 p78_4 p78_9 p87 p106 p116 p117 p108_1 p108_2 p108_3 p108_4 p108_5 p108_6 p108_7 p108_8 ///
pension_esp jubilacion_esp alquileres_esp destos_3edad_esp subs_enee_esp bono80_esp int_bancarios_esp pens_divorcio_esp ayud_fam_esp ayud_part_esp bonpraf_esp meresc_esp bolspraf_esp becas_esp remesaext_esp otbon_esp otros_esp ///
pension_efe jubilacion_efe alquileres_efe destos_3edad_efe subs_enee_efe bono80_efe int_bancarios_efe pens_divorcio_efe ayud_fam_efe ayud_part_efe bonpraf_efe meresc_efe bolspraf_efe becas_efe remesaext_efe otbon_efe otros_efe

foreach i of local varing {
qui recode `i' (99999999 = .) (99999=.)
qui sum `i'
}

*Daniela Zuluaga- Noviembre 2017: Se deciden reemplazar las variables del ingreso laboral (Monetario y no Monetario) por las que ya están construidas en la base original**

***************
***ylmpri_ci***
***************
egen ylmpri_ci=rowtotal(ysmop ycmop yagmop ygamop), missing
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
egen ylnmpri_ci=rowtotal(yseop yceop yageop ygaeop ), missing
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********
egen ylmsec_ci=rowtotal(ysmos ycmos yagmos ygamos), missing
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
*ylnmsec_ci*
************
egen ylnmsec_ci=rowtotal(yseos yceos yageos ygaeos), missing
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
egen ynlm_ci=rsum( pension_efe jubilacion_efe alquileres_efe destos_3edad_efe subs_enee_efe bono80_efe int_bancarios_efe remesaext_efe pens_divorcio_efe ayud_fam_efe ayud_part_efe bonpraf_efe meresc_efe bolspraf_efe becas_efe otbon_efe otros_efe), missing
replace ynlm_ci=. if (pension_efe==. & jubilacion_efe==. &  alquileres_efe==. & bono80_efe==. & destos_3edad_efe==. &  subs_enee_efe==. &  int_bancarios_efe==. &  pens_divorcio_efe==. &  ayud_fam_efe==. &  ayud_part_efe==. &  bonpraf_efe==. &  meresc_efe==. &  bolspraf_efe==. &  becas_efe==. &  remesaext_efe==. & otbon_efe==. &  otros_efe==.)
label var ynlm_ci "Ingreso No Laboral Monetario" 
  
/* Mayra Sáenz - Octubre 2013. No se utiliza la variable original yotrf porque incluye los ingresos no laborales en especies.
gen ynlm_ci=yotrf
label var ynlm_ci "Ingreso No Laboral Monetario"
*/

**************
***ynlnm_ci***
**************
egen ynlnm_ci=rsum(pension_esp jubilacion_esp alquileres_esp destos_3edad_esp subs_enee_esp bono80_esp int_bancarios_esp remesaext_esp pens_divorcio_esp ayud_fam_esp ayud_part_esp bonpraf_esp meresc_esp bolspraf_esp becas_esp otbon_esp otros_esp), missing
replace ynlnm_ci=. if (pension_esp==. & jubilacion_esp==. &  alquileres_esp==. & bono80_esp==. & destos_3edad_esp==. &  subs_enee_esp==. &  int_bancarios_esp==. &  pens_divorcio_esp==. &  ayud_fam_esp==. &  ayud_part_esp==. &  bonpraf_esp==. &  meresc_esp==. &  bolspraf_esp==. &  becas_esp==. &  remesaext_esp==. & otbon_esp==. &  otros_esp==.)
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

****************
*autoconsumo_ci*
****************
gen autoconsumop_ci=p87 if p87 >=0 
replace autoconsumop_ci=0 if p87==. & edad_ci>4 & (categopri==1 | categopri==2) & (p34==1 | p35==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=p117 if p117 >=0 
replace autoconsumos_ci=0 if p117==. & edad_ci>4 & (categosec==1 | categosec==2) & p89==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autocons_ci=rsum(autoconsumop_ci autoconsumos_ci), missing
replace autocons_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autocons_ci "Autoconsumo Individual (Trabajadores Independientes)"


******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing 
la var autocons_ch "Autoconsumo del Hogar"


************
*remesas_ci*
************


egen remesas_ci=rsum(remesaext_esp remesaext_efe), missing
*Tipo de cambio: 19.03 
/*
gen remesas_ci=p122_ymensual_15
replace remesas_ci=. if p122_ymensual_15==.
*/

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

* Daniela Zuluaga-Noviembre  2017: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

***************
***trapri_ci***
***************
egen trapri_ci= rowtotal(remesas_ci pens_divorcio_esp pens_divorcio_efe ayud_fam_esp ayud_fam_efe ayud_part_esp ayud_part_efe ), missing
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

****************
***progpub_ci***
****************
/*Se suman unicamente los beneficicarios del bonopraf con edad_ci<65 dado que los beneficiarios de edad_ci>=65 ya fueron incluidos en la variable ypensub_ci.
para esto se genera una variable auxiliar*/

egen aux=rowtotal(bonpraf_esp bonpraf_efe) if edad_ci<65, missing
egen progpub_ci= rowtotal(bolspraf_esp bolspraf_efe aux), missing
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas"
drop aux

****************
***progpub_ch***
****************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
egen trapub_ci= rowtotal(progpub_ci ypensub_ci bono80_efe bono80_esp destos_3edad_esp destos_3edad_efe subs_enee_esp subs_enee_efe meresc_esp meresc_efe  becas_esp becas_efe otbon_esp otbon_efe), missing
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

****************
***capital_ci***
****************
egen capital_ci= rowtotal(alquileres_esp alquileres_efe int_bancarios_esp int_bancarios_efe), missing
label var capital_ci "Ingreso por renta del capital" 

****************
***capital_ch***
****************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

*****************
***otros_ci***
*****************
egen otros_ci= rowtotal(otros_esp otros_efe), missing
label var otros_ci "Otros Ingresos" 

*****************
***otros_ch***
*****************
bys idh_ch: egen otros_ch=sum(otros_ci) if miembros_ci==1, missing
label var otros_ch "Otros Ingresos del hogar" 

*************
***ypen_ch***
*************
bys idh_ch: egen ypen_ch=sum(ypen_ci) if miembros_ci==1, missing
label var ypen_ch "Ingresos del hogar por jubilaciones y pensiones contributivas" 


*************
***ytotal_ci***
*************
egen ytotal_ci= rowtotal (ylm_ci ylnm_ci trapri_ci trapub_ci capital_ci otros_ci ypen_ci), missing
label var ytotal_ci "Ingreso total individual" 

*************
***ytotal_ch***
*************
egen ytotal_ch=rowtotal(ylm_ch  ylnm_ch  trapri_ch  trapub_ch  capital_ch  otros_ch  ypen_ch) if miembros_ci==1, missing
label var ytotal_ch "Ingreso total del hogar"

***************
***ytotalpc_ch***
***************
gen ytotalpc_ch=(ytotal_ci/nmiembros_ch) if miembros_ci==1
label var ytotalpc_ch "Ingreso per capita del hogar"


****************
***quintil_ci***
****************
xtile quintil_ci=ytotalpc_ch[fw=round(factor_ch)], nq(5)
label var quintil_ci "Quintil de ingreso"
label define quintil_ci 1 "Quintil 1" 2 "Quintil 2" 3 "Quintil 3" 4 "Quintil 4" 5 "Quintil 5"
label values quintil_ci quintil_ci


******************************************************************************
*	Educación
*****************************************************************************

************
* asiste_ci*
************
gen asiste_ci=.
replace asiste_ci=1 if p03==1
replace asiste_ci=0 if p03==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci***
*************
		/*
		1 1. ninguno
           2 2. programa de alfabetizacion
           3 3. pre-básica
           4 4. básica
           5 5. ciclo comun
           6 6. diversificado
           7 7. tecnico superior
           8 8. superior no universitaria
           9 9. superior universitaria
          10 10. post-grado
          99 99. no sabe/no responde
*/
* Años de educacion aprobados **

/*
replace p08=. if p08>9
replace p15=. if p15>9

** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if  p05>=1 & p05<=3
replace aedu_ci=p08 if p05==4 
replace aedu_ci=p08+6 if p05==5 | p05==6
replace aedu_ci=p08+12 if p05==7 | p05==8 | p05==9
replace aedu_ci=p08+17 if p05==10
** para quienes asisten actualmente
replace aedu_ci=0 if p11==1 | p11==2 | p11==3 
replace aedu_ci=p15-1 if p11==4
replace aedu_ci=p15+6-1 if p11==5 | p11==6
replace aedu_ci=p15+12-1 if p11==7 | p11==8 | p11==9
replace aedu_ci=p15+17-1 if p11==10
label var aedu_ci "Años de educacion aprobados"		
*/

*Modificación Mayra Sáenz - Octubre 2016: Corrección enviada por Ivan Bornacelly SCL/EDU
* Años de educacion aprobados **
replace p08=. if p08>9
replace p15=. if p15>9

** para quienes ya no asisten
gen aedu_ci=.
replace aedu_ci=0 if  p05>=1 & p05<=3
replace aedu_ci=p08 if p05==4 
replace aedu_ci=p08+6 if p05==5
replace aedu_ci=p08+9 if p05==6
replace aedu_ci=p08+12 if p05==7 | p05==8 | p05==9
replace aedu_ci=p08+17 if p05==10

** para quienes asisten actualmente
replace aedu_ci=0 if p11==1 | p11==2 | p11==3 
replace aedu_ci=p15-1 if p11==4 & p15 >0
replace aedu_ci=p15+6-1 if p11==5
replace aedu_ci=p15+9-1 if p11==6
replace aedu_ci=p15+12-1 if p11==7 | p11==8 | p11==9
replace aedu_ci=p15+17-1 if p11==10
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
replace edupre_ci=1 if (( p03== 1 & p11==3) & aedu_ci ~=.)
replace edupre_ci=0 if (edupre_ci~=1 & aedu_ci ~=.)
la var edupre_ci "Asiste a Educacion preescolar"

***************
***asipre_ci***
***************

	g asispre_ci=.
	replace asispre_ci=1 if p03==1 & p11==3 
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"

******************************
*	pqnoasis 
******************************
ren p04 pqnoasis_ci
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"


******************************
*	repite_ci 
******************************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************************
*	repiteult_ci 
******************************
gen repiteult_ci=.
replace repiteult_ci=1 if p13==1
replace repiteult_ci=0 if p13==2
label var repiteult_ci "Personas que han repetido el ultimo grado"


******************************
*	edupub_ci 
******************************

gen edupub_ci=.
/*Mayra Sáenz Octubre 2013. La variable sólo se refiere a los que estudian actualmente.
replace edupub_ci=1 if (p09==1|p09==2|p09==3|p09==4|p09==7|p09==8|p09==13)
replace edupub_ci=0 if (p09==5|p09==6|p09==9|p09==10|p09==11|p09==12)
*/
replace edupub_ci=1 if (p16==1|p16==2|p16==3|p16==4|p16==7|p16==8|p16==13)
replace edupub_ci=0 if (p16==5|p16==6|p16==9|p16==10|p16==11|p16==12)

label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p05==7 | p11==7
replace tecnica_ci=0 if tecnica_ci ~=1 & ( p05!=99 & p11!=99)
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
replace pqnoasis1_ci = 2 if pqnoasis_ci==11
replace pqnoasis1_ci = 3 if pqnoasis_ci==6
replace pqnoasis1_ci = 4 if pqnoasis_ci==3
replace pqnoasis1_ci = 5 if pqnoasis_ci==4 | pqnoasis_ci==10
replace pqnoasis1_ci = 6 if pqnoasis_ci==2
replace pqnoasis1_ci = 7 if pqnoasis_ci==8 | pqnoasis_ci==9
replace pqnoasis1_ci = 8 if pqnoasis_ci==5
replace pqnoasis1_ci = 9 if pqnoasis_ci==1 | pqnoasis_ci==12

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

	
**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
gen aguared_ch=.
replace aguared_ch=1 if v05==1
replace aguared_ch=0 if v05==2 

gen aguadist_ch=.
replace aguadist_ch=1 if v09==1
replace aguadist_ch=2 if (v09==2 | v09==3)
replace aguadist_ch=3 if v09==4

gen aguamala_ch=.
replace aguamala_ch=1 if v06>=5 & v06<=9
replace aguamala_ch=0 if v06>=1 & v06<=4

gen aguamide_ch=.


gen luz_ch=.
replace luz_ch=1 if (v10==1|v10==2|v10==3)
replace luz_ch=0 if (v10>=4 & v10<=8)

gen luzmide_ch=.


gen combust_ch=.
replace combust_ch=1 if h04==2 | h04==3 | h04==4
replace combust_ch=0 if h04==1 | h04==5


gen bano_ch=.
replace bano_ch=1 if h05==1
replace bano_ch=0 if h05==2

gen banoex_ch=.
replace banoex_ch=1 if h07==1
replace banoex_ch=0 if h07==2

gen des1_ch=.
replace des1_ch=0 if h05==2
replace des1_ch=1 if (h06==1|h06==2)
replace des1_ch=2 if (h06==5|h06==6|h06==7)
replace des1_ch=3 if (h06==3|h06==4)

* MGR Jul, 2015: corrección en sintáxis
/*
gen des2_ch=.
replace des2_ch=1 if (h06==1|h06==2|h06==3)
replace des2_ch=2 if (h06==4|h06==5|h06==6|h06==7|h06==8)
replace des2_ch=0 if h05==2
*/
gen des2_ch=.
replace des2_ch=1 if (h06==1|h06==2|h06==5|h06==6|h06==7)
replace des2_ch=2 if (h06==4|h06==3|h06==8)
replace des2_ch=0 if h05==2

gen pared_ch=.
replace pared_ch=0 if v02>=5 & v02<=6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7


gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8

gen resid_ch=.
replace resid_ch=0 if (v11==1|v11==3)
replace resid_ch=1 if (v11==4|v11==6)
replace resid_ch=2 if (v11==2|v11==7)
replace resid_ch=3 if (v11==5|v11==8)

gen dorm_ch=.
replace dorm_ch=h01 if h01>=0 

gen cuartos_ch=.
replace cuartos_ch=v16 if v16>=0 


gen cocina_ch=.
replace cocina_ch=1 if (h03==1)
replace cocina_ch=0 if (h03==2)


gen telef_ch=.
replace telef_ch=1 if (h08_07==1 | h08_08==1)
replace telef_ch=0 if (h08_07==2 & h08_08==2)

gen refrig_ch=.
replace refrig_ch=1 if h08_01==1
replace refrig_ch=0 if h08_01==2


gen freez_ch=.


 
gen auto_ch=.
replace auto_ch=1 if (h08_09==1 | h08_10==1)
replace auto_ch=0 if (h08_09==2 & h08_10==2)

gen compu_ch=.
replace compu_ch=1 if h08_14==1
replace compu_ch=0 if h08_14==2


/*
**** If any household member has accessed ****
**** the internet from home, the variable ****
**** receives a value of 1                ****
gen internet_ch_=.
replace internet_ch_=1 if p31_1==1
by hogar, sort: egen internet_ch = max(internet_ch_)
drop internet_ch_
replace internet_ch=0 if internet_ch==.
*/

gen internet_ch=.
replace internet_ch=1 if p31_1==1
replace internet_ch=0 if internet_ch==.
/*
**** If any household member has celular  ****
**** phone, the variable receives a value ****
**** of 1                                 ****
gen cel_ch_=.
replace cel_ch_=1 if p33==1
by hogar, sort: egen cel_ch = max(cel_ch_)
drop cel_ch_
replace cel_ch=0 if cel_ch==.
*/

gen cel_ch=.
replace cel_ch=1 if p33==1
replace cel_ch=0 if cel_ch==.

gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=8) | v01==2 | v01==3

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if v14==1
replace viviprop_ch=1 if v14==3
replace viviprop_ch=2 if v14==2
replace viviprop_ch=3 if (v14==4 | v14==5 | v14==6 | v14==7)

gen vivitit_ch=.
replace vivitit_ch=1 if v17==1
replace vivitit_ch=0 if v17==2

/* Tipo de cambio lempiras por dolares = 19.03 
   Variable cambio en la Base de otros Ingresos */

gen vivialq_ch=.
replace vivialq_ch=v15a if v15b==1
replace vivialq_ch=v15a*19.03 if v15b==2


gen vivialqimp_ch=.


**DZ Noviembre 2017: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v06 >=1 & v06 <=4) | v06==8
replace aguamejorada_ch = 0 if (v06 >=5 & v06 <=7) | v06==9

*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if ( h05 ==1 & ((h06 >=1 & h06 <=2) | (h06 >=5 & h06 <=8)) & h07 ==1)
replace banomejorado_ch = 0 if ( h05 ==1 & ((h06 >=1 & h06 <=2) | (h06 >=5 & h06 <=8)) & h07 ==2) | (h06 >=3 & h06 <=4) | (h05==2)

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

















