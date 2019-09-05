* (Versi�n Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS COL
local ENCUESTA ECH
local ANO "2002"
local ronda t3 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

  
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Colombia
Encuesta: ECH
Round: t3
Autores: Analia
Generaci�n nuevas variables LMK: 
�ltima versi�n: Marcela Rubio - Email: marcelarubio28@gmail.com | mrubio@iadb.org
Fecha �ltima modificaci�n: Mayo 2014

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

************
* Region_BID *
************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroam�rica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
***region_c ***
***************
gen region_c= dpto
label define region_c       /// 
	5  "Antioquia"	        ///
	8  "Atl�ntico"	        ///
	11 "Bogot�, D.C"	    ///
	13 "Bol�var" 	        ///
	15 "Boyac�"	            ///
	17 "Caldas"	            ///
	18 "Caquet�"	        ///
	19 "Cauca"	            ///
	20 "Ces�r"	            ///
	23 "C�rdoba"	        ///
	25 "Cundinamarca"       ///
	27 "Choc�"	            ///
	41 "Huila"	            ///
	44 "La Guajira"	        ///
	47 "Magdalena"	        ///
	50 "Meta"	            ///
	52 "Nari�o"	            ///
	54 "Norte de Santander"	///
	63 "Quind�o"	        ///
	66 "Risaralda"	        ///
	68 "Santander"	        ///
	70 "Sucre"	            ///
	73 "Tolima"	            ///
	76 "Valle"	
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

***************
***factor_ch***
***************
gen factor_ch=fex_c_2011
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************
*id del hogar tiene nombre idh_ch en la base original
sort idh_ch
label variable idh_ch "ID del hogar"


**************
****idp_ci****
**************
g idp_ci=orden
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
/*la definicion de rural tiene que ser tomada con cuidado
puesto que es una definicion politico-geografica, no por 
densidad demografica*/
destring clase, replace
g zona_c = clase == 1
la var zona_c "Zona del pa�s"
la de zona_c 1 "Urbana" 0 "Rural"
la val zona_c zona_c

************
****pais****
************
gen str3 pais_c="COL"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2002
label variable anio_c "Anio de la encuesta"

**********
***mes****
**********

gen mes_c = mes_c1
label variable mes_c "Mes de la encuesta"
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo"
label define mes_c 6 "Junio" 7 "Julio" 8 "Agosto" 9 "Septiembre", add
label define mes_c 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
label value mes_c mes_c

***************
***factor_ci***
***************
gen  factor_ci=fex_c_2011
la var factor_ci "Factor de expansi�n del individuo"

br
		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if p3==1
replace relacion_ci=2 if p3==2
replace relacion_ci=3 if p3==3
replace relacion_ci=4 if p3>=4 & p3<=9 
replace relacion_ci=5 if p3>=10 & p3<=11 
replace relacion_ci=5 if p3>=13 & p3<=15
replace relacion_ci=6 if p3==12
label var relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes"
label define relacion_ci 5 "Otros no pariente" 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

***************
*****sexo******
***************

gen sexo_ci=p4
label var sexo_ci "Sexo del individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

***************
*****edad******
***************

gen edad_ci = p5
label var edad_ci "Edad del individuo (a�os)"

***************
****raza_ci****
***************

gen raza_ci=.
label var raza_ci "Raza o etnia del individuo"
label define raza_ci 1 "Ind�gena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
notes raza_ci: En el cuestionario no consta una pregunta relacionada con raza.

*****************
*****civil_ci****
*****************

gen civil_ci=.
replace civil_ci=1 if p6==5
replace civil_ci=2 if p6>=1 & p6<=2
replace civil_ci=3 if p6==4
replace civil_ci=4 if p6==3
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Uni�n formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci

******************
******jefe_ci*****
******************

gen jefe_ci = relacion_ci==1
label var jefe_ci "Jefe de hogar"

*******************
****nconyuges_ch***
*******************

bys idh_ch: egen nconyuges_ch = sum(relacion_ci==2)
label var nconyuges_ch "N�mero de c�nyuges"

***************
***nhijos_ch***
***************

bys idh_ch: egen nhijos_ch= sum(relacion_ci==3)
label var nhijos_ch "N�mero de hijos"

******************
***notropari_ch***
******************

bys idh_ch: egen notropari_ch = sum(relacion_ci==4)
label var notropari_ch "N�mero de otros familiares"

********************
***notronopari_ch***
********************

bys idh_ch: egen notronopari_ch = sum(relacion_ci==5)
label var notronopari_ch "N�mero de no familiares"

****************
***nempdom_ch***
****************

bys idh_ch: egen nempdom_ch = sum(relacion_ci==6)
label var nempdom_ch "N�mero de empleados dom�sticos"

*****************
***clasehog_ch***
*****************

gen byte clasehog_ch = 0
replace clasehog_ch = 1 if nconyuges_ch==0 & nhijos_ch==0 & notropari_ch==0 & notronopari_ch==0
replace clasehog_ch = 2 if(nconyuges_ch>0 | nhijos_ch>0) & (notropari_ch==0 & notronopari_ch==0)
replace clasehog_ch = 3 if notropari_ch>0 & notronopari_ch==0
replace clasehog_ch = 4 if(nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & notronopari_ch>0
replace clasehog_ch = 5 if(nconyuges_ch==0 & nhijos_ch==0 & notropari_ch==0) | notronopari_ch>0
label var clasehog_ch "Tipo de hogar"
label define clasehog_ch 	1 "Unipersonal" ///
							2 "Nuclear"		///
							3 "Ampliado"	///
							4 "Compuesto"	///
							5 "Corresidente"
label value clasehog_ch clasehog_ch							


******************
***nmiembros_ch***
******************

bys idh_ch: egen nmiembros_ch = sum(relacion_ci>=1 & relacion_ci<=4)
label var nmiembros_ch "N�mero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

bys idh_ch: egen nmayor21_ch = sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>21 & edad_ci!=.)  
label var nmayor21_ch "N�mero de familiares mayores a 21 a�os"


*****************
***nmenor21_ch***
*****************

bys idh_ch: egen nmenor21_ch = sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)  
label var nmenor21_ch "N�mero de familiares mayores a 21 a�os"

*****************
***nmayor65_ch***
*****************

bys idh_ch: egen nmayor65_ch = sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>65)  
label var nmayor65_ch "N�mero de familiares mayores a 65 a�os"

****************
***nmenor6_ch***
****************

bys idh_ch: egen nmenor6_ch = sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)  
label var nmenor6_ch "N�mero de familiares menores a 6 a�os"

****************
***nmenor1_ch***
****************

bys idh_ch: egen nmenor1_ch = sum((relacion_ci>= 1 & relacion_ci<= 4) & edad_ci<1)
label var nmenor1_ch "N�mero de familiares menores a 1 a�o"

****************
***miembros_ci***
****************

gen miembros_ci = relacion_ci <=4
label var miembros_ci "Miembro del hogar"

		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
