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

local PAIS HND
local ENCUESTA EPHPM
local ANO "2007"
local ronda m9 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Yessenia Loaysa (abr-2013)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 9 de Septiembre de 2013

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
gen region_c=.

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

**** MISSING: ASK DEFINITION ****
gen aguamide_ch=.
*********************************

gen luz_ch=.
replace luz_ch=1 if (v10==1|v10==2|v10==3)
replace luz_ch=0 if (v10>=4 & v10<=8)

**** MISSING: ASK DEFINITION ****
gen luzmide_ch=.
*********************************

**** MISSING: ASK DEFINITION ****
gen combust_ch=.
*********************************

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

gen des2_ch=.
replace des2_ch=1 if (h06==1|h06==2|h06==3)
replace des2_ch=2 if (h06==4|h06==5|h06==6|h06==7)
replace des2_ch=0 if h05==2

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

**** MISSING: ASK DEFINITION ****
gen cocina_ch=.
*********************************

gen telef_ch=.
replace telef_ch=1 if (h08_07==1 | h08_08==1)
replace telef_ch=0 if (h08_07==2 & h08_08==2)

gen refrig_ch=.
replace refrig_ch=1 if h08_01==1
replace refrig_ch=0 if h08_02==2

**** MISSING: ASK DEFINITION ****
gen freez_ch=.
*********************************

* Question, ask about the car. 
gen auto_ch=.
replace auto_ch=1 if (h08_09==1 | h08_10==1)
replace auto_ch=0 if (h08_09==2 & h08_10==2)

gen compu_ch=.
replace compu_ch=1 if h08_14==1
replace compu_ch=0 if h08_14==2


**** If any household member has accessed ****
**** the internet from home, the variable ****
**** receives a value of 1                ****
gen internet_ch_=.
replace internet_ch_=1 if p31_1==1
by hogar, sort: egen internet_ch = max(internet_ch_)
drop internet_ch_
replace internet_ch=0 if internet_ch==.


**** If any household member has celular  ****
**** phone, the variable receives a value ****
**** of 1                                 ****
gen cel_ch_=.
replace cel_ch_=1 if p33==1
by hogar, sort: egen cel_ch = max(cel_ch_)
drop cel_ch_
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
replace vivialq_ch=v15a/19.03 if v15b==2

**** MISSING: ASK DEFINITION ****
gen vivialqimp_ch=.
*********************************

gen pais_c="HND"
gen anio_c=2007
gen mes_c=9
/*
*gen mes_c=""
tostring fecha, replace
replace mes_c =substr(fecha,2,2) if length(fecha)==7
replace mes_c =substr(fecha,3,2) if length(fecha)==8
destring mes, replace
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre"
label value mes_c mes_c*/

gen double idh_ch=hogar
format idh_ch %20.0g 

****** ASK ABOUT EXPANSION FACTOR FOR THE HOUSEHOLD *****
gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"
gen factor_ci=factor_ch


gen zona_c=1 if domi==1 | domi==2 | domi==3 
replace zona_c=0 if domi==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

gen edad_ci=edad
label var edad_ci "Edad del Individuo"
drop edad

gen civil_ci=.
replace civil_ci=1 if civil==5
replace civil_ci=2 if civil==1 | civil==6
replace civil_ci=3 if civil==3 | civil==4
replace civil_ci=4 if civil==2
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

gen jefe_ci=0
replace jefe_ci=1 if rela_j==1
label var jefe_ci "Jefe de Hogar Declarado"


gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j== 8 
replace relacion_ci=5 if rela_j==7 | rela_j==9 | rela_j==11
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

egen byte nconyuges_ch=sum(rela_j==2), by (idh)
label variable nconyuges "Numero de Conyuges"

egen byte nhijos_ch=sum((rela_j==3 | rela_j==4)), by (idh)
label variable nhijos_ch "Numero de Hijos"

egen byte notropari_ch=sum(rela_j==5 | rela_j==6 | rela_j==8),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

egen byte notronopari_ch=sum(rela_j==7 | rela_j==9 | rela_j==11), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

egen byte nempdom_ch=sum(rela_j==10), by (idh)
label variable nempdom_ch "Numero de Empleados Domesticos"

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "clasehog_ch - Clase de Hogar"
label define clasehog_ch 1"Unipersonal" 2"Nuclear" 3"Ampliado" 4"Compuesto" 5"Corresidente"
label value clasehog_ch clasehog_ch

egen nmayor21_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"

gen asiste_ci=.
replace asiste_ci=1 if p03==1
replace asiste_ci=0 if p03==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

**** This variable is not fully harmonized ****
gen pqnoasis_ci=.
replace pqnoasis_ci=1	if p04==2
replace pqnoasis_ci=2	if p04==3
replace pqnoasis_ci=3	if p04==11
replace pqnoasis_ci=4	if p04==4
replace pqnoasis_ci=5	if p04==5
*replace pqnoasis_ci=6	if p04==
replace pqnoasis_ci=7	if p04==6
replace pqnoasis_ci=8	if p04==9
replace pqnoasis_ci=9	if p04==7
*replace pqnoasis_ci=10	if p04==
replace pqnoasis_ci=11	if p04==9
replace pqnoasis_ci=12	if p04==1
replace pqnoasis_ci=99	if p04==99
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

gen repiteult_ci=.
replace repiteult_ci=1 if p13==1
replace repiteult_ci=0 if p13==2
label var repiteult_ci "Personas que han repetido el ultimo grado"

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

** para quienes ya no asisten
gen aedu_ci=anosest
label var aedu_ci "Años de educacion aprobados"


/* En 2007, en la variable P05 y P11
   
   P05. ¿Cual es el nivel educativo más alto que alcanzó?
   P11. ¿Cual es el nivel educativo en el que estudia actualmente?
   
   apareció la clasificación 8. Superior no Universitario. 
   Para ser consistentes con las anteriores harmonizaciones
   se decidió incluirla dentro de la categoría: 7. Técnico Superior
   
   También recodificamos los valores 9 a 8 y 10 a 9, para asegurar la consistencia. */
   
replace p05=7 if p05==8
replace p05=8 if p05==9
replace p05=9 if p05==10

label define nivel_educativo_nuevo 1"1. Ninguno" 2"2. Programa de alfabetizacion" 3"3. Pre-escolar"	///
                                   4"4. Primaria" 5"5. Ciclo comun" 6"6. Diversificado" 		///
                                   7"7. Tecnico superior" 8"8. Superior universitaria" 9"9. Post-grado" ///
                                   99"99. No sabe/no responde"
label value p05 nivel_educativo_nuevo

replace p11=7 if p11==8
replace p11=8 if p11==9
replace p11=9 if p11==10
label value p11 nivel_educativo_nuevo

gen eduno_ci=.
replace eduno=1 if (p05==1 & edad_ci>=5) | (p11==3 & p15==1)
replace eduno=0 if (p05>3 & p05<=9 & edad_ci>=5) | ((p11>2 & p11<9) | (p11==3 & p15>1))    
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupi_ci=.
replace edupi=1 if (p05==4 & p08<6 & p08>=0) | (p11==3 & p15<7 & p15>=1)
replace edupi=0 if ((p05>=5 & p05<=9) | (p05==4 & p08>=6)) | ((p11>=4 & p11<9) | (p11==3 & p15>=7)) | (eduno==1)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc=1 if (p05==4 & p08==6) 
replace edupc=0 if (edupi==1 | eduno==1) | (p05==4 & p08>6) | (p11==3 & p15>=7) | (p05>4 & p05<=9) | (p11>3 & p11<9)
replace edupi=1 if p05==4 & (p08==0 | p08==.)
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi=1 if (p05==4 & p08>6 & p08<.) | (p11==3 & p15>=7 & p15<.) | (p05==5 & p08<3) | (p05==6 & p08<4) | (p11==4 & p15<=3) | (p11==5 & p15<=4)
replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (p05==5 & p08>=3 & p08<.) | (p05==6 & p08>=4 & p08<.) | (p05>=7 & p05<=9) | (p11>=6 & p11<9) 
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc=1 if (p05==5 & p08>=3 & p08<.) | (p05==6 & p08>=4 & p08<.) 
replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (p05>6 & p05<=9) | ( p11>5& p11<9)
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui=1 if (p05==7 & p08<3) | (p05==8 & p08<5) | (p11==6 & p15<3) | (p11==7 & p15<5)
replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (p05==7 & p08>=3) | (p05==8 & p08>=5) | (p11==6 & p15>=3) | (p11==7 & p15>=5) | (p05==9) | (p11==8)
label var eduui_ci "1 = personas que no han completado el nivel universitario"

gen eduuc_ci=.
replace eduuc=1 if (p05==7 & p08>=3 & p08<.) | (p05==8 & p08>=5 & p08<.) | (p11==6 & p15>=3 & p15<.) | (p11==7 & p15>=5 & p15<.) | p05==9 | p11==8
replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 
label var eduuc_ci "1 = personas que han completado el nivel universitario"

replace edupi=1 if p05==4 & p08==.
replace edupc=0 if p05==4 & p08==.
replace edusi=1 if (p05==5 | p05==6) & p08==.
replace edusc=0 if (p05==5 | p05==6) & p08==.
replace eduui=1 if (p05==7 | p05==8) & p08==.
replace eduuc=0 if (p05==7 | p05==8) & p08==.

gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (p05==4 & (p08==7 | p08==8)) | (p11==3 & (p15==7| p15==8| p15==9))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (p05==4 & p08==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (p05==5 & p08<3) | (p05==6 & p08<4) | (p11==4) | (p11==5)
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen edupre_ci=.
replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1
replace edupre=1 if p05==3 | p11==2
label var edupre_ci "Educacion preescolar"

gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if p05==8 | p11==7
label var eduac_ci "Educacion universitaria vs educacion terciaria"

/* RECODIFIED, Option 10, 11 and 12 were considered Private = 246 obs

      p09 A traves de que sistema estudió |      Freq.     Percent        Cum.
    --------------------------------------+-----------------------------------
                               1. PROHECO |         94        0.20        0.20
                            2. EDUCATODOS |        285        0.61        0.81
                              3. PRALEBAH |        149        0.32        1.12
          4. Presencial en centro público |     42,841       91.20       92.33
          5. Presencial en centro privado |      2,917        6.21       98.54
 6. Presencial en centro privado bilingue |         32        0.07       98.61
            7. Por radio(maestro en casa) |         90        0.19       98.80
         8. A distancia en centro publico |        144        0.31       99.10
         9. A distancia en centro privado |         37        0.08       99.18
                     10. En el extranjero |        203        0.43       99.61
  11. Educacion virtual publica o privada |          3        0.01       99.62
                    12. ONG,s o Fundación |         40        0.09       99.71
                              13. CCEPREB |          3        0.01       99.71
                  99. No sabe/no responde |        135        0.29      100.00
    --------------------------------------+-----------------------------------
                                    Total |     46,973      100.00          */

gen edupub_ci=.
replace edupub_ci=1 if (p09==1|p09==2|p09==3|p09==4|p09==7|p09==8|p09==13)
replace edupub_ci=0 if (p09==5|p09==6|p09==9|p09==10|p09==11|p09==12)

replace edupub_ci=1 if (p16==1|p16==2|p16==3|p16==4|p16==7|p16==8|p16==13)
replace edupub_ci=0 if (p16==5|p16==6|p16==9|p16==10|p16==11|p16==12)



label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

gen miembros_ci=1 if rela_j>=1 & rela_j<=9
replace miembros_ci=0 if rela_j==10 | rela_j==11

gen uno=1 if miembros_ci==1
egen nmiembros_ch=sum(uno), by(idh_ch)
replace nmiembros_ch=. if miembros_ci!=1
label var nmiembros_ch "Numero de miembros del Hogar"
drop uno

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
replace categopri_ci=1 if (p53==7)
replace categopri_ci=2 if (p53==4 | p53==5 | p53==6)
replace categopri_ci=3 if (p53==1 | p53==2 | p53==3)
replace categopri_ci=4 if (p53==8 | p53==9)

replace categopri_ci=1 if (p66==7 | p66==11)
replace categopri_ci=2 if (p66==4 | p66==5 | p66==6 | p66==8 | p66==9 | p66==10)
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
replace categosec_ci=1 if (p96==7 | p96==11)
replace categosec_ci=2 if (p96==4 | p96==5 | p96==6 | p96==8 | p96==9 | p96==10)
replace categosec_ci=3 if (p96==1 | p96==2 | p96==3)
replace categosec_ci=4 if (p96==12| p96==13)
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

tostring p60, replace
replace p60 = "0" + p60 if length(p60)==6
gen labor=substr(p60,1,4)
destring labor, replace 

gen ocupapri_ci=.
replace ocupapri_ci=1 if labor>=2000 & labor<=3999
replace ocupapri_ci=2 if labor>=1000 & labor<=1999
replace ocupapri_ci=3 if labor>=4000 & labor<=4999
replace ocupapri_ci=4 if labor>=5200 & labor<=5999
replace ocupapri_ci=5 if labor>=5000 & labor<=5199
replace ocupapri_ci=6 if labor>=6000 & labor<=6999
replace ocupapri_ci=7 if labor>=7000 & labor<=8999
replace ocupapri_ci=8 if labor>=0 & labor<=999
replace ocupapri_ci=9 if (labor>=9000 & labor<=9996) | labor==9999
drop labor 

label var ocupapri_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupapri_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupapri_ci ocupapri_ci

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
/*
gen emp_ci=.
replace emp_ci=1 if p34==1 | p35==1
replace emp_ci=0 if p34==2 & p35==2
label var emp_ci "Empleado en la semana de referencia"*/

****************
****condocup_ci*
****************

gen condocup_ci=condact
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
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

gen ylmpri_ci=.
replace ylmpri_ci=p76 if edad_ci>4 & p75==1 & p77==1
replace ylmpri_ci=p76*p77 if edad_ci>4 & ((p75==2 & (p77==1 | p77==2)) | (p75==3 & p77>=1 & p77<=4) | (p75==4 & p77>=1 & p77<=31))
replace ylmpri_ci=p86 if edad_ci>4 & p86>=0 & p86~=.
replace ylmpri_ci=0 if (p76==0 | p86==0) & edad_ci>4 & (p34==1 | p35==1)
* Change in Code
* Note: ylmpri_ci == ycmop
replace ylmpri_ci=0 if (p66==12 | p66==13) & edad_ci>4 & (p34==1 | p35==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"



gen ylmsec_ci=.
replace ylmsec_ci=p106 if p106<99999 & edad_ci>4 & p105==1 & p107==1
replace ylmsec_ci=p106*p107 if p106<99999 & edad_ci>4 & ((p105==2 & (p107==1 | p107==2)) | (p105==3 & p107>=1 & p107<=4) | (p105==4 & p107>=1 & p107<=31))
replace ylmsec_ci=p116 if p116<99999 & edad_ci>4 & p116>=0
replace ylmsec_ci=0 if p106==0 & p116==0 & edad_ci>4 & p89==1
replace ylmsec_ci=0 if (p96==12 | p96==13) & edad_ci>4 & p89==1
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso Laboral Monetario Total"

gen horaspri_ci=p64 if p64<=168
label var horaspri_ci "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p94 if p94<168

************
*horastot_ci
************

egen horastot_ci=rsum(horaspri horassec)
replace horastot_ci=. if horaspri_ci==. & horassec_ci==.
replace horastot_ci=horaspri_ci if horassec_ci==.
replace horastot_ci=horassec_ci if horaspri_ci==.
label var horastot_ci "Horas totales trabajadas en todas las Actividades"


gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"
gen ylmho_ci=ylm_ci/(4.3*horastot)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

gen durades_ci=.
replace durades_ci=p44a/(365/12)      if p44b==1
replace durades_ci=p44b/((365/7)/12)  if p44b==2
replace durades_ci=p44a               if p44b==3
label var durades "Duracion del Desempleo (en meses)"
/* 
gen antiguedad=.
replace antiguedad=p68a/365           if p68b==1
replace antiguedad=p68a/52            if p68b==2
replace antiguedad=p68a/12            if p68b==3
replace antiguedad=p68a               if p68b==4

replace antiguedad=p81a/365           if p81b==1 // Dias
replace antiguedad=p81a/52            if p81b==2 // Semanas
replace antiguedad=p81a/12            if p81b==3 // Meses
replace antiguedad=p81a               if p81b==4 // Anios
*/
*MLO: modificacion

tostring p58_1,replace
gen aux_a=substr(p58_1,1,1) if p58_1!="9999999999" & p58_1!="."
gen aux_b=substr(p58_1,2,1) if p58_1!="9999999999" & p58_1!="."
gen aux_c=substr(p58_1,3,1) if p58_1!="9999999999" & p58_1!="."
gen aux_d=substr(p58_1,4,1) if p58_1!="9999999999" & p58_1!="."
gen aux_e=substr(p58_1,5,1) if p58_1!="9999999999" & p58_1!="."
gen aux_f=substr(p58_1,6,1) if p58_1!="9999999999" & p58_1!="."
destring aux_*, replace

gen antiguedad_ci	=.
replace antiguedad_ci= p68a/30 if p68b==1
replace antiguedad_ci= p68a/4 if p68b==1
replace antiguedad_ci= p68a if p68b==3
replace antiguedad_ci= p68a*12 if p68b==4

 

label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"

/*Double check
gen desemp1_ci=.
replace desemp1_ci=1 if p38==1 
replace desemp1_ci=0 if p38==2 | emp==1
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

gen desemp2_ci=.
replace desemp2_ci=0 if desemp1==0
replace desemp2_ci=1 if desemp1_ci==1 | p42==3 | p42==4
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola"

gen desemp3_ci=.
replace desemp3_ci=0 if desemp2==0 
replace desemp3_ci=1 if desemp2==1 | (p38==1 & p39==1)
label var desemp3_ci "desemp2 + personas que no tienen trabajopero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

gen byte pea1_ci=.
replace pea1=1 if emp==1 | desemp1==1
replace pea1=0 if emp==0 & desemp1==0
label variable pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"

gen byte pea2_ci=.
replace pea2=1 if emp==1 | desemp2==1
replace pea2=0 if emp==0 & desemp2==0
label variable pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"

gen byte pea3_ci=.
replace pea3=1 if emp==1 | desemp3==1
replace pea3=0 if emp==0 & desemp3==0
label variable pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"
*/
gen desalent_ci=.
replace desalent=0 if condocup_ci==3
replace desalent=1 if p42==5
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

gen subemp_ci=.
replace subemp=0 if emp_ci==0 | emp_ci==1
replace subemp=1 if horastot<30 & p119==1
label var subemp "Trabajadores subempleados"

gen tiempoparc=.
replace tiempoparc=0 if emp_ci==0 | emp_ci==1
replace tiempoparc=1 if horastot<30 & p119==2
label var tiempoparc "Trabajadores a medio tiempo"

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


gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if emp_ci==1 & p89==1
replace nempleos=0 if emp_ci==0
label var nempleos "Numero de empleos"


tab sexo_ci
tab relacion_ci if edad_ci>=18
tab sexo_ci [aw=factor_ci]
tab relacion_ci [aw=factor_ci] if edad_ci>=18


preserve
keep if edad_ci>=18
sample 1, by(idh_ch) count

tab sexo_ci
tab relacion_ci
tab sexo_ci [aw=factor_ci]
tab relacion_ci [aw=factor_ci]

restore


*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p67b>=1 & p67b<=5 /*asalariados*/
replace firmapeq_ci=1 if p80b>=1 & p80b<=5 /*cuenta propia*/
replace firmapeq_ci=0 if p67b>=6 & p67b<9999	
replace firmapeq_ci=0 if p80b>=6 & p80b<9999	
tab firmapeq_ci [w=int(factor_ci)] 

* Asalariados en Ocupacion Principal
gen tamfirmapri_ci=.
replace tamfirmapri_ci=0 if p67a==1
replace tamfirmapri_ci=1 if p67a==2 
label var tamfirmapri_ci "Trabajadores formales de la empresa en la ocupacion principal 1 = mas de 10 empleados"

* Cuenta Propia Principal
replace tamfirmapri_ci=0 if p80a==1 & tamfirmapri_ci==.
replace tamfirmapri_ci=1 if p80a==2 & tamfirmapri_ci==.

* Asalariados en Ocupacion Secundaria
gen tamfirmasec_ci=.
replace tamfirmasec_ci=0 if p97a==1
replace tamfirmasec_ci=1 if p97a==2 

* Cuenta Propia Secundaria
replace tamfirmasec_ci=0 if p110a==1 & tamfirmasec_ci==.
replace tamfirmasec_ci=1 if p110a==2 & tamfirmasec_ci==.
label var tamfirmasec_ci "Trabajadores formales de la empresa en la ocupacion secundaria 1 = mas de 10 empleados"

 



gen spublico_ci=.
label var spublico "Personas que trabajan en el sector publico"

gen yalim2    =   p78_1	if p78_1>=0
gen yropa2    =   p78_2	if p78_2>=0
gen yhabita2  =   p78_3	if p78_3>=0
gen ytrans2   =   p78_4	if p78_4>=0
egen yotro2   =  rsum(p78_5 p78_6 p78_7 p78_8 p78_9) 
replace yotro2=. if yotro2==0 & p78_5==.
gen yprodu2   = p87 if p87>=0 

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p78_1==. & p78_2==. & p78_3==. & p78_4==. & ///
                          p78_5==. & p78_6==. & p78_7==. & p78_9==. & ///
                          categopri==3) | (p87==. & (categopri==1 | categopri==2))) ///
                          & edad_ci>4 & (p34==1 | p35==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

gen yalim3    =   p108_1	if p108_1>=0
gen yropa3    =   p108_2	if p108_2>=0
gen yhabita3  =   p108_3	if p108_3>=0
gen ytrans3   =   p108_4	if p108_4>=0
egen yotro3   =  rsum(p108_5 p108_6 p108_7 p108_8 p108_9) 
replace yotro3=. if yotro3==0 & p108_5==.
gen yprodu3   = p117 if p117>=0 

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.

replace ylnmsec_ci=0 if ((p108_1==. & p108_2==. & p108_3==. & p108_4==. & ///
                          p108_5==. & p108_6==. & p108_7==. & p108_9==. & ///
                          categosec==3) | (p117==. & (categosec==1 | categosec==2))) ///
                          & edad_ci>4 & p89==1
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso Laboral No Monetario Total"

gen autoconsumop_ci=yprodu2 
replace autoconsumop_ci=0 if p87==. & edad_ci>4 & (categopri==1 | categopri==2) & (p34==1 | p35==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p117==. & edad_ci>4 & (categosec==1 | categosec==2) & p89==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

gen remesasm_ci=p122_lps_15
label var remesasm_ci "Remesas Individuales (monetario)"

gen remesas_ci=p122_ymensual_15
label var remesasm_ci "Remesas Individuales (monetario)"


egen ynlm_ci=rsum(p122_lps_1 p122_lps_2 p122_lps_3 p122_lps_4 p122_lps_5 p122_lps_6 	  ///
                  p122_lps_11 p122_lps_12 p122_lps_13 p122_lps_14 p122_lps_15 p122_lps_16 ///
                  p122_lps_17 p122_lps_9 p122_lps_10 					  ///
                  p122_dls2_1 p122_dls2_2 p122_dls2_3 p122_dls2_4 p122_dls2_5 p122_dls2_6 	  ///
		  p122_dls2_11 p122_dls2_12 p122_dls2_13 p122_dls2_14 p122_dls2_15 p122_dls2_16   ///
		  p122_dls2_17 p122_dls2_9 p122_dls2_10 )

replace ynlm_ci=. if p122_lps_1==. & p122_lps_2==. & p122_lps_3==. & p122_lps_4==. & p122_lps_5==. & p122_lps_6==. & 	     ///
                     p122_lps_11==. & p122_lps_12==. & p122_lps_13==. & p122_lps_14==. & p122_lps_15==. & p122_lps_16==. & ///
                     p122_lps_17==. & p122_lps_9==. & p122_lps_10==. & ///
                     p122_dls2_1==. & p122_dls2_2==. & p122_dls2_3==. & p122_dls2_4==. & p122_dls2_5==. & p122_dls2_6==. & ///     ///
		     p122_dls2_11==. & p122_dls2_12==. & p122_dls2_13==. & p122_dls2_14==. & p122_dls2_15==. & p122_dls2_16==. & ///
                     p122_dls2_17==. & p122_dls2_9==. & p122_dls2_10==.
                     
label var ynlm_ci "Ingreso No Laboral Monetario"

egen ynlm2_ci=rsum(p122_lps_1 p122_lps_2 p122_lps_3 p122_lps_4 p122_lps_5 p122_lps_6 	  ///
		   p122_lps_11 p122_lps_12 p122_lps_13 p122_lps_14 p122_lps_15 p122_lps_16 ///
		   p122_lps_17 p122_lps_9 p122_lps_10 p122_lps_7 ///
		   p122_dls2_1 p122_dls2_2 p122_dls2_3 p122_dls2_4 p122_dls2_5 p122_dls2_6 	  ///
		   p122_dls2_11 p122_dls2_12 p122_dls2_13 p122_dls2_14 p122_dls2_15 p122_dls2_16   ///
		   p122_dls2_17 p122_dls2_9 p122_dls2_10 p122_dls2_7)

replace ynlm2_ci=. if p122_lps_1==. & p122_lps_2==. & p122_lps_3==. & p122_lps_4==. & p122_lps_5==. & p122_lps_6==. & 	     ///
		      p122_lps_11==. & p122_lps_12==. & p122_lps_13==. & p122_lps_14==. & p122_lps_15==. & p122_lps_16==. & ///
		      p122_lps_17==. & p122_lps_9==. & p122_lps_10==. & p122_lps_7==. & ///
		      p122_dls2_1==. & p122_dls2_2==. & p122_dls2_3==. & p122_dls2_4==. & p122_dls2_5==. & p122_dls2_6==. & ///     ///
		      p122_dls2_11==. & p122_dls2_12==. & p122_dls2_13==. & p122_dls2_14==. & p122_dls2_15==. & p122_dls2_16==. & ///
		      p122_dls2_17==. & p122_dls2_9==. & p122_dls2_10==. & p122_dls2_7==.

label var ynlm2_ci "Ingreso No Laboral Monetario 2"

egen ynlm4_ci=rsum( p122_lps_1 p122_lps_2 p122_lps_3 p122_lps_4 p122_lps_5 p122_lps_6 	  ///
		    p122_lps_11 p122_lps_12 p122_lps_13 p122_lps_14 p122_lps_15 p122_lps_16 ///
		    p122_lps_17 p122_lps_9 p122_lps_10 p122_lps_7 p122_lps_7 ///
		    p122_dls2_1 p122_dls2_2 p122_dls2_3 p122_dls2_4 p122_dls2_5 p122_dls2_6 	  ///
		    p122_dls2_11 p122_dls2_12 p122_dls2_13 p122_dls2_14 p122_dls2_15 p122_dls2_16   ///
		    p122_dls2_17 p122_dls2_9 p122_dls2_10 p122_dls2_7 p122_dls2_8)

replace ynlm4_ci=. if p122_lps_1==. & p122_lps_2==. & p122_lps_3==. & p122_lps_4==. & p122_lps_5==. & p122_lps_6==. & 	     ///
		      p122_lps_11==. & p122_lps_12==. & p122_lps_13==. & p122_lps_14==. & p122_lps_15==. & p122_lps_16==. & ///
		      p122_lps_17==. & p122_lps_9==. & p122_lps_10==. & p122_lps_7==. & p122_lps_8==. & ///
		      p122_dls2_1==. & p122_dls2_2==. & p122_dls2_3==. & p122_dls2_4==. & p122_dls2_5==. & p122_dls2_6==. & ///     
		      p122_dls2_11==. & p122_dls2_12==. & p122_dls2_13==. & p122_dls2_14==. & p122_dls2_15==. & p122_dls2_16==. & ///
		      p122_dls2_17==. & p122_dls2_9==. & p122_dls2_10==. & p122_dls2_7==. & p122_dls2_8==.

label var ynlm4_ci "Ingreso No Laboral Monetario 4"



/*
 Ingreso No Laboral No Monetario  (Remesas en dolares en especie, Remesas en lempiras en especie, 
				     Ayuda de familiares en especie, Ayuda de particulares en especia
				     Merienda escolar en lempiras en especie, 
				     
				     Otros ingresos no laborales en especie en lempiras, 
				     Otros ingresos no laborales en especie en dolares) */

egen ynlnm_ci = rsum(p122_lps_9  p122_lps_10  p122_lps_12  p122_lps_13  p122_lps_15  p122_lps_17   ///
		     p122_dls2_9 p122_dls2_10 p122_dls2_12 p122_dls2_13 p122_dls2_15 p122_dls2_17)
		     
replace ynlnm_ci =. if p122_lps_9==. & p122_lps_10==. & p122_lps_12==. & p122_lps_13==. & p122_lps_15==. & p122_lps_17==. & ///
		       p122_dls2_9==. & p122_dls2_10==. & p122_dls2_12==. & p122_dls2_13==. & p122_dls2_15==. & p122_dls2_17==.


* Ingreso No Laboral

egen ynl_ci = rsum( p122_ymensual_1  p122_ymensual_2  p122_ymensual_3  p122_ymensual_4  p122_ymensual_5  p122_ymensual_6 ///
                    p122_ymensual_7  p122_ymensual_8  p122_ymensual_9  p122_ymensual_10 p122_ymensual_11 p122_ymensual_12 ///
                    p122_ymensual_13 p122_ymensual_14 p122_ymensual_15 p122_ymensual_16 p122_ymensual_17)

replace ynl_ci=. if p122_ymensual_1==. & p122_ymensual_2==. & p122_ymensual_3==. & p122_ymensual_4==. & p122_ymensual_5==. & p122_ymensual_6==. & ///
		    p122_ymensual_7==. & p122_ymensual_8==. & p122_ymensual_9==. & p122_ymensual_10==. & p122_ymensual_11==. & p122_ymensual_12==. & ///
		    p122_ymensual_13==. & p122_ymensual_14==. & p122_ymensual_15==. & p122_ymensual_16==. & p122_ymensual_17==.
		    
gen nrylmpri_ci=0 
replace nrylmpri_ci=1 if p76==.
replace nrylmpri_ci=1 if p86==.
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

egen ylmnr_ci=rsum(ylmpri_ci ylmsec_ci) 
replace ylmnr_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylmnr_ci "Ingreso Laboral Monetario Total, considera 'missing' la No Respuesta "


egen yl_ci=rsum(ylmnr_ci ylnm_ci)
replace yl_ci=. if ylmnr_ci==. & ylnm_ci==.
label var yl_ci "Ingreso Laboral Individual (Monetario + No Monetario)"

egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh_ch)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch<.
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

egen ynlm2_ch=sum(ynlm2_ci) if miembros_ci==1, by(idh_ch)
label var ynlm2_ch "Ingreso No Laboral Monetario 2 del Hogar"

egen ynlm4_ch=sum(ynlm4_ci) if miembros_ci==1, by(idh_ch)
label var ynlm4_ch "Ingreso No Laboral Monetario del Hogar 4"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen ynl_ch=sum(ynl_ci) if miembros_ci==1, by(idh_ch)
label var ynl_ch "Ingreso No Laboral del Hogar (monetario + no monetario)"

egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)
label var autoconsumo_ch "Autoconsumo del Hogar"

egen remesasm_ch=sum(remesasm_ci) if miembros_ci==1, by(idh_ch)
label var remesasm_ch "Remesas del Hogar (monetario)"
egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"

gen rama_ci=ramaop
replace rama_ci=. if ramaop==10 | ramaop==11 | emp_ci==0

*drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 autoconsumop_ci autoconsumos_ci   


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
* HON 2010
gen salmm_ci= 	3024.90
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

recode p69 (1=1) (3=2 3) (nonmissing=.), gen(tipocontrato_ci)
label drop tipocontrato_ci
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1"Permanente/indefinido" 2"Temporal" 3"Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=p48 if p48==1 & condocup_ci==2
replace cesante_ci=. if p48==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	


*************
*tamemp_ci
*************

egen tamemp_ci=rsum(p67b p80b) if p80b!= 99999 & p67b!=99999 
replace tamemp_ci= p67b  if p80b==99999 & p67b!= .
replace tamemp_ci= p80b  if p67b==99999 & p80b!= .

label var tamemp_ci "# empleados en la empresa"

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
gen pension= p122_ymensual_1
gen jubilacion= p122_ymensual_2
egen ypen_ci=rsum(pension jubilacion)
label var ypen_ci "Valor de la pension contributiva"


*************
**pension_ci*
*************

gen pension_ci=1 if ypen_ci!=0 & ypen_ci!=.
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************

*gen ypensub_ci=bonpraf

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=ypensub_ci!=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if p05==7
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

*Poverty

*********
*lp25_ci
*********
gen lp25_ci = 829.5079

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = 1327.213

label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"


*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci =pobreza==2
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =pobreza==3
label var lpe_ci "Linea de indigencia oficial del pais"


* falta construir 
gen ocupa_ci=.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen tiempoparc_ci=.
gen pared_ch=.
gen raza_ci=.
gen instcot_ci=.
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
lp_ci	       lpe_ci	       cotizando_ci	             afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
tamemp_ci 	   pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	


qui destring $var, replace
 

* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress



saveold "`base_out'", replace


log close
















