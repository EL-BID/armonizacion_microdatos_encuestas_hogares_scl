
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
local ANO "2005"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 9 de Septiembre de 2013

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

gen region_c=  .
label var region_c "División política"

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
replace aguadist_ch=1 if v05c==1
replace aguadist_ch=2 if v05c==2 | v05c==3
replace aguadist_ch=3 if v05c==4

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

gen des2_ch=.
replace des2_ch=1 if v06b==1 | v06b==2 | v06b==3 
replace des2_ch=2 if v06b==4 | v06b==5 | v06b==6 | v06b==7
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
replace resid_ch=0 if v08a==1 /* v08a for 2005 */
replace resid_ch=1 if v08a==3 | v08a==5
replace resid_ch=2 if v08a==2 | v08a==6
replace resid_ch=3 if v08a==7 | v08a==4

***************
**dorm_ch     *
***************

gen dorm_ch=.
replace dorm_ch=v16b if v16b>=0 

***************
**cuartos_ch  *
***************

gen cuartos_ch=.
replace cuartos_ch=v16a if v16a>=0 /* v16a for 2005 */ 

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
replace viviprop_ch=3 if v14==6 | v14==12 | v14==3 | v14==7 /* v14==7 included for 2005 */

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

gen vivialq_ch=.
replace vivialq_ch=v15b if v15b<99999 & v14==5 /* v15b & v14 for 2005 */

* replace vivialq_ch=v10c/17.73 if v10c<99999 & v10b==2 *

***************
*vivialqimp_ch*
***************

gen vivialqimp_ch=.

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
*factor_ci    *
***************

gen factor_ci=factor_ch

***************
*zona_c       *
***************

gen zona_c=1 if domi==1 | domi==2 | domi==3 
replace zona_c=0 if domi==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

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

***************
*jefe_ci      *
***************

gen jefe_ci=0
replace jefe_ci=1 if rela_j==1
label var jefe_ci "Jefe de Hogar Declarado"

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
*nconyuges_ch *
***************

egen byte nconyuges_ch=sum(rela_j==2), by (idh)
label variable nconyuges "Numero de Conyuges"

***************
*nhijos_ch    *
***************

egen byte nhijos_ch=sum((rela_j==3 | rela_j==4)), by (idh)
label variable nhijos_ch "Numero de Hijos"

***************
*notropari_ch *
***************

egen byte notropari_ch=sum(rela_j==5 | rela_j==6 | rela_j==7 | rela_j==8),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

****************
*notronopari_ch*
****************

egen byte notronopari_ch=sum(rela_j==9 | rela_j==11), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

***************
*nempdom_ch   *
***************

egen byte nempdom_ch=sum(rela_j==10), by (idh)
label variable nempdom_ch "Numero de Empleados Domesticos"

***************
*clasehog_ch  *
***************

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

***************
*nmayor21_ch  *
***************

egen nmayor21_ch=sum((rela_j>0 & rela_j<=9) & (edad>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

***************
*nmenor21_ch  *
***************

egen nmenor21_ch=sum((rela_j>0 & rela_j<=9) & (edad<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

***************
*nmayor65_ch  *
***************

egen nmayor65_ch=sum((rela_j>0 & rela_j<=9) & (edad>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

***************
*nmenor6_ch   *
***************

egen nmenor6_ch=sum((rela_j>0 & rela_j<=9) & (edad<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

***************
*nmenor1_ch   *
***************

egen nmenor1_ch=sum((rela_j>0 & rela_j<=9) & (edad<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

***************
*idp_ci       *	
***************

gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"

***************
*asiste_ci    *	
***************

gen asiste_ci=.
replace asiste_ci=1 if p02==1
replace asiste_ci=0 if p02==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

***************
*pqnoasis_ci  *	
***************

gen pqnoasis_ci=p03
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

***************
*repiteult_ci *	
***************

gen repiteult_ci=.
replace repiteult_ci=1 if p05c==1
replace repiteult_ci=0 if p05c==2
label var repiteult_ci "Personas que han repetido el ultimo grado"

***************
*repite_ci    *	
***************

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"


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
replace aedu_ci=p04b+6 if p04a==5 | p04a==6
replace aedu_ci=p04b+12 if p04a==7 | p04a==8
replace aedu_ci=p04b+17 if p04a==9
** para quienes asisten actualmente
replace aedu_ci=0 if p05a==1 | p05a==2
replace aedu_ci=p05b-1 if p05a==3
replace aedu_ci=p05b+6-1 if p05a==4 | p05a==5
replace aedu_ci=p05b+12-1 if p05a==6 | p05a==7
replace aedu_ci=p05b+17-1 if p05a==8
label var aedu_ci "Años de educacion aprobados"

***************
*eduno_ci     *	
***************

gen eduno_ci=.
replace eduno=1 if (p04a==1 & edad>=5) | (p05a==3 & p05b==1)
replace eduno=0 if (p04a>3 & p04a<=9 & edad>=5) | ((p05a>2 & p05a<9) | (p05a==3 & p05b>1))    
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

***************
*edupi_ci     *	
***************

gen edupi_ci=.
replace edupi=1 if (p04a==4 & p04b<6 & p04b>=0) | (p05a==3 & p05b<7 & p05b>=1)
replace edupi=0 if ((p04a>=5 & p04a<=9) | (p04a==4 & p04b>=6)) | ((p05a>=4 & p05a<9) | (p05a==3 & p05b>=7)) | (eduno==1)
label var edupi_ci "1 = personas que no han completado el nivel primario"

***************
*edupc_ci     *	
***************

gen edupc_ci=.
replace edupc=1 if (p04a==4 & p04b==6) 
replace edupc=0 if (edupi==1 | eduno==1) | (p04a==4 & p04b>6) | (p05a==3 & p05b>=7) | (p04a>4 & p04a<=9) | (p05a>3 & p05a<9)
replace edupi=1 if p04a==4 & (p04b==0 | p04b==.)
label var edupc_ci "1 = personas que han completado el nivel primario"

***************
*edusi_ci     *	
***************

gen edusi_ci=.
replace edusi=1 if (p04a==4 & p04b>6 & p04b<.) | (p05a==3 & p05b>=7 & p05b<.) | (p04a==5 & p04b<3) | (p04a==6 & p04b<4) | (p05a==4 & p05b<=3) | (p05a==5 & p05b<=4)
replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (p04a==5 & p04b>=3 & p04b<.) | (p04a==6 & p04b>=4 & p04b<.) | (p04a>=7 & p04a<=9) | (p05a>=6 & p05a<9) 
label var edusi_ci "1 = personas que no han completado el nivel secundario"

***************
*edusc_ci     *	
***************

gen edusc_ci=.
replace edusc=1 if (p04a==5 & p04b>=3 & p04b<.) | (p04a==6 & p04b>=4 & p04b<.) 
replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (p04a>6 & p04a<=9) | ( p05a>5& p05a<9)
label var edusc_ci "1 = personas que han completado el nivel secundario"

***************
*eduui_ci     *	
***************

gen eduui_ci=.
replace eduui=1 if (p04a==7 & p04b<3) | (p04a==8 & p04b<5) | (p05a==6 & p05b<3) | (p05a==7 & p05b<5)
replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (p04a==7 & p04b>=3) | (p04a==8 & p04b>=5) | (p05a==6 & p05b>=3) | (p05a==7 & p05b>=5) | (p04a==9) | (p05a==8)
label var eduui_ci "1 = personas que no han completado el nivel universitario"

***************
*eduuc_ci     *	
***************

gen eduuc_ci=.
replace eduuc=1 if (p04a==7 & p04b>=3 & p04b<.) | (p04a==8 & p04b>=5 & p04b<.) | (p05a==6 & p05b>=3 & p05b<.) | (p05a==7 & p05b>=5 & p05b<.) | p04a==9 | p05a==8
replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 
label var eduuc_ci "1 = personas que han completado el nivel universitario"


***************
*edus1i_ci    *	
***************

replace edupi=1 if p04a==4 & p04b==.
replace edupc=0 if p04a==4 & p04b==.
replace edusi=1 if (p04a==5 | p04a==6) & p04b==.
replace edusc=0 if (p04a==5 | p04a==6) & p04b==.
replace eduui=1 if (p04a==7 | p04a==8) & p04b==.
replace eduuc=0 if (p04a==7 | p04a==8) & p04b==.

gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (p04a==4 & (p04b==7 | p04b==8)) | (p05a==3 & (p05b==7| p05b==8| p05b==9))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

***************
*edus1c_ci    *	
***************

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (p04a==4 & p04b==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

***************
*edus2i_ci    *	
***************

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (p04a==5 & p04b<3) | (p04a==6 & p04b<4) | (p05a==4) | (p05a==5)
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

***************
*edus2c_ci    *	
***************
 
gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

***************
*edupre_ci    *	
***************

gen edupre_ci=.
replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1
replace edupre=1 if p04a==3 | p05a==2
label var edupre_ci "Educacion preescolar"

***************
*eduac_ci     *	
***************

gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if p04a==8 | p05a==7
label var eduac_ci "Educacion universitaria vs educacion terciaria"

***************
*edupub_ci    *	
***************

gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

***************
* miembros_ch *	
***************

gen miembros_ci=1 if rela_j>=1 & rela_j<=9
replace miembros_ci=0 if rela_j==10 | rela_j==11

***************
*nmiembros_ch *	
***************

gen uno=1 if miembros_ci==1
egen nmiembros_ch=sum(uno), by(idh_ch)
replace nmiembros_ch=. if miembros_ci!=1
label var nmiembros_ch "Numero de miembros del Hogar"
drop uno

******************
***categopri_ci***
******************

gen categopri_ci=1 if p37==7 /* p37 for October 2005 */
replace categopri_ci=2 if p37==4 | p37==5 | p37==6
replace categopri_ci=3 if p37==1 | p37==2 | p37==3
replace categopri_ci=4 if p37==12 | p37==13
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************
***categosec_ci***
******************

gen categosec_ci=1 if p63==7 /* p63 for October 2005 */
replace categosec_ci=2 if p63==4 | p63==5 | p63==6
replace categosec_ci=3 if p63==1 | p63==2 | p63==3
replace categosec_ci=4 if p63==12 | p63==13
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

/*************
***emp_ci***
************

gen emp_ci=.
replace emp_ci=1 if p10==1 | p11==1 /* p10 & p11 for 2005 */
replace emp_ci=0 if p10==2 & p11==2 
label var emp_ci "Empleado en la semana de referencia"*/


****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=condact
replace condocup_ci=4 if condact == 4 | edad_ci<10
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

****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.
replace ylmpri_ci=p46 if p46<99999 & edad>4 & p44==1 /* p46 & p45 for 2005 */
replace ylmpri_ci=p46*p45 if p46<99999 & edad>4 & (p44==2 | p44==3 | p44==4) /* p45 for 2005 */
replace ylmpri_ci=p55 if p55<999999 & edad>4 & p55>=0 /* p55 for 2005 */
replace ylmpri_ci=0 if p46==0 & p55==0 & edad>4 & (p10==1 | p11==1)
replace ylmpri_ci=0 if (p37==12 | p37==13) & edad>4 & (p10==1 | p11==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****************
***ylmsec_ci ***
****************

gen ylmsec_ci=.
replace ylmsec_ci=p73 if p73<99999 & edad>4 & p71==1 /* p73 & p71 for 2005 */
replace ylmsec_ci=p73*p72 if p73<99999 & edad>4 & (p71==2 | p71==3 | p71==4)
replace ylmsec_ci=p82 if p82<99999 & edad>4 & p82>=0 /* p82 for 2005 */ 

replace ylmsec_ci=0 if p73==0 & p82==0 & edad>4 & p61==1 /* p61 for 2005 */
replace ylmsec_ci=0 if (p63==12 | p63==13) & edad>4 & p61==1 /* p63 for 2005 */
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso Laboral Monetario Total"

*****************
***ylnmpri_ci ***
*****************

gen yalim2=p47l1 if p47l1<99999 & p47l1>=0 /* p47 for 2005 */
gen yropa2=p47l2 if p47l2<99999 & p47l2>=0
gen yhabita2=p47l3 if p47l3<99999 & p47l3>=0
gen ytrans2=p47l4 if p47l4<99999 & p47l4>=0
gen yotro2=p47l5 if p47l5<99999 & p47l5>=0
gen yprodu2=p56 if p56<99999 & p56>=0 /* p56 for 2005 */

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p47l1==. & p47l2==. & p47l3==. & p47l4==. & p47l5==. & categopri==3) | (p56==. & (categopri==1 | categopri==2))) & edad>4 & (p10==1 | p11==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

*****************
***ylnmsec_ci ***
*****************

gen yalim3=p74l1 if p74l1<99999 & p74l1>=0 /* p74 for 2005 */
gen yropa3=p74l2 if p74l2<99999 & p74l2>=0
gen yhabita3=p74l3 if p74l3<99999 & p74l3>=0
gen ytrans3=p74l4 if p74l4<99999 & p74l4>=0
gen yotro3=p74l5 if p74l5<99999 & p74l5>=0
gen yprodu3=p83 if p83<99999 & p83>=0 /* p83 for 2005 */

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((p74l1==. & p74l2==. & p74l3==. & p74l4==. & p74l5==. & categosec==3) | (p83==. & (categosec==1 | categosec==2))) & edad>4 & p58==1
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"

*****************
*** ylnm_ci *****
*****************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso Laboral No Monetario Total"

***********************
*** autoconsumop_ci ***
***********************

gen autoconsumop_ci=yprodu2 
replace autoconsumop_ci=0 if p56==. & edad>4 & (categopri==1 | categopri==2) & (p10==1 | p11==1) /* p56 for 2005 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

***********************
*** autoconsumos_ci ***
***********************

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p83==. & edad>4 & (categosec==1 | categosec==2) & p58==1 
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

***********************
*** autoconsumo_ci  ***
***********************

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

*************
***ypenju2***
*************

gen ypenju2=p88l01c5/3 if p88l01c5>=0 /* p88l01c5 for Oct 2005 */ 
replace ypenju2=p88l01c1 if p88l01c1>=0 & (p88l01c5==. | p88l01c5==0) /* p88l01c1 for Oct 2005 */

***********
***yjub2***
***********

gen yjub2=p88l02c5/3 if  p88l02c5>=0 /*  p88l02c5 for Oct 2005 */
replace yjub2=p88l02c1 if p88l02c1>=0 & (p88l02c5==. | p88l02c5==0) /* p88l02c1 for Oct 2005 */

***************
***yalquile2***
***************

gen yalquile2=p88l03c5/3 if  p88l03c5>=0 /*  p88l03c5 for Oct 2005 */
replace yalquile2= p88l03c1 if  p88l03c1>=0 & (p88l03c5==. | p88l03c5==0) /*  p88l03c1 for Oct 2005 */

***************
***ysubsi2*****
***************

gen ysubsi2=p88l04c5/3 if p88l04c5>=0 /* p88l04c5 for Oct 2005 */
replace ysubsi2= p88l04c1 if  p88l04c1>=0 & (p88l04c5==. | p88l04c5==0) /*  p88l04c1 for Oct 2005 */

***************
***ybonos2*****
***************

gen ybonos2=p88l10c5/3 if p88l10c5>=0 /*  p88l10c5 for Oct 2005 */
replace ybonos2=p88l10c1 if  p88l10c1>=0 & (p88l10c5==. | p88l10c5==0) /* p88l10c1 for Oct 2005 */

****************
***yremesa2*****
****************

gen yremesa2=p88l06c5/3 if p88l06c5>=0 /* p88l06c5 for Oct 2005 */
replace yremesa2=p88l06c2 if p88l06c2>=0 & (p88l06c5==. | p88l06c5==0) /* p88l06c2 for Oct 2005 */ 

/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a agosto 2005: 	19.00		
                                                                     septiembre 2005: 	19.01 
								     octubre 2005: 	19.03 
								     promedio 3 meses: 	19.01 */

****************
***yremesad2****
****************

gen yremesad2=(p88l06c6/3)*19.01 if p88l06c6>=0 /* p88l06c6 for Oct 2005 */
replace yremesad2=p88l06c2*19.01 if p88l06c2>=0 & (p88l06c6==. | p88l06c6==0) /* p88l06c2 for Oct 2005 */

****************
***yayuda2******
****************

gen yayuda2=p88l08c5/3 if p88l08c5>=0 /* p88l08c5 for Oct 2005 */
replace yayuda2=p88l08c1 if p88l08c1>=0 & (p88l08c5==. | p88l08c5==0) /* p88l08c1 for Oct 2005*/ 

****************
***yayupar2*****
****************

gen yayupar2=p88l09c5/3 if p88l09c5>=0 /* p88l09c5 for Oct 2005 */ 
replace yayupar2=p88l09c1 if p88l09c1>=0 & (p88l09c5==. | p88l09c5==0) /* p88l09c1 for Oct 2005 */

****************
***yotros2******
****************

gen yotros2=p88l12c5/3 if p88l12c5>=0 /*  p88l12c5 for Oct 2005 */ 
replace yotros2= p88l12c1 if p88l12c1>=0 & (p88l12c5==. | p88l12c5==0) /*  p88l12c1 for Oct 2005 */

****************
***interes2*****
****************

gen interes2=p88l05c5/3 if p88l05c5>=0 /* p88l05c5 for Oct 2005 */
replace interes2=p88l05c1 if p88l05c1>=0 & (p88l05c5==. | p88l05c5==0) /* p88l05c1 for Oct 2005 */

****************
***prestlab2****
**************** 

gen prestlab2=p88l11c5/3 if p88l11c5>=0 /* p88l11c5 for Oct 2005 */ 
replace prestlab2=p88l11c1 if p88l11c1>=0 & (p88l11c5==. | p88l11c5==0) /* p88l11c1 for Oct 2005 */

****************
***yremerasde***
**************** 

gen yremesade=(p88l06c8/3)*19.01 if p88l06c8>=0 /* p88l06c8 for Oct 2005 */ 
replace yremesade=p88l06c4*19.01 if p88l06c4>=0 & (p88l06c8==. | p88l06c8==0) /* p88l06c4 for Oct 2005 */

****************
***yremerase****
**************** 

gen yremesae=p88l06c7/3 if p88l06c7>=0 /* p88l06c7 for Oct 2005 */
replace yremesae=p88l06c3 if p88l06c3>=0 & (p88l06c7==. | p88l06c7==0) /* p88l06c3 for Oct 2005 */

****************
***yayudae******
**************** 

gen yayudae=p88l08c7/3 if p88l08c7>=0 /* p88l08c7 for Oct 2005 */
replace yayudae=p88l08c3 if p88l08c3>=0 & (p88l08c7==. | p88l08c7==0) /* p88l08c3 for Oct 2005 */

/* No estan las ayudas de particulares en especie:*/

/* No estan las herencias */

*Hay pension por divorcio: p5213c03 & p5213c02 for Oct 2005 */ 

****************
***ypdiv********
**************** 

gen ypdiv=p88l07c5/3 if p88l07c5>=0
replace ypdiv=p88l07c1 if p88l07c1>=0 & (p88l07c5==. | p88l07c5==0)

sum ypdiv if ypdiv>=0

***********************
*** remesasm_ci     ***
***********************

egen remesasm_ci=rsum(yremesa2 yremesad2)
replace remesasm_ci=. if yremesa2==. & yremesad2==. 
replace remesasm_ci=0 if p88l06c5==0 & p88l06c2==0 & p88l06c6==0 & p88l06c1==0 
label var remesasm_ci "Remesas Individuales (monetario)"

***********************
*** remesas_ci     ***
***********************

egen remesas_ci=rsum(yremesa2 yremesad2 yremesade yremesae)
replace remesas_ci=. if yremesa2==. & yremesad2==. & yremesade==. & yremesae==.
replace remesas_ci=0 if p88l06c5==0 & p88l06c2==0 & p88l06c6==0 & p88l06c1==0 & p88l06c8==0 & p88l06c4==0 & p88l06c7==0 & p88l06c3==0 
label var remesas_ci "Remesas Individuales (monetario + especies)"

***********************
*** ynlm_ci         ***
***********************

egen ynlm_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2)
replace ynlm_ci=0 if p88l01c5==. & p88l01c1==. & p88l02c5==. & p88l02c1==. & p88l03c5==. & p88l03c1==. & p88l04c5==. & p88l04c1==. & p88l10c5==. & p88l10c1==. & p88l06c5==. & p88l06c2==. & p88l06c6==. & p88l06c2==. & p88l08c5==. & p88l08c1==. & p88l09c5==. & p88l09c1==. & p88l12c5==. & p88l12c1==.
replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & yjub2==. & yremesad2==.
label var ynlm_ci "Ingreso No Laboral Monetario"

***********************
*** ynlm2_ci        ***
***********************

egen ynlm2_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2)
replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==.
replace ynlm2_ci=0 if p88l01c5==. & p88l01c1==. & p88l02c5==. & p88l02c1==. & p88l03c5==. & p88l03c1==. & p88l04c5==. & p88l04c1==. & p88l10c5==. & p88l10c1==. & p88l06c5==. & p88l06c2==. & p88l06c6==. & p88l06c2==. & p88l08c5==. & p88l08c1==. & p88l09c5==. & p88l09c1==. & p88l12c5==. & p88l12c1==. & p88l05c5==. & p88l05c1==. & p88l11c5==. & p88l11c1==.   
label var ynlm2_ci "Ingreso No Laboral Monetario 2"

***********************
*** ynlm4_ci        ***
***********************

egen ynlm4_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypdiv)
replace ynlm4_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypdiv
replace ynlm4_ci=0 if p88l01c5==. & p88l01c1==. & p88l02c5==. & p88l02c1==. & p88l03c5==. & p88l03c1==. & p88l04c5==. & p88l04c1==. & p88l10c5==. & p88l10c1==. & p88l06c5==. & p88l06c2==. & p88l06c6==. & p88l06c2==. & p88l08c5==. & p88l08c1==. & p88l09c5==. & p88l09c1==. & p88l12c5==. & p88l12c1==. & p88l05c5==. & p88l05c1==. & p88l11c5==. & p88l11c1==.  & p88l07c5==. & p88l07c1==. 
label var ynlm4_ci "Ingreso No Laboral Monetario 4"

***********************
*** ynlnm_ci        ***
***********************

egen ynlnm_ci=rsum(yremesade yremesae yayudae) 
replace ynlnm_ci=. if yremesade==. & yremesae==. & yayudae==. 
replace ynlnm_ci=0 if p88l06c8==. & p88l06c4==. & p88l06c7==. & p88l06c3==. & p88l08c7==. & p88l08c3==.
label var ynlnm_ci "Ingreso No Laboral No Monetario"

***********************
*** ynl_ci          ***
***********************

egen ynl_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yremesade yremesae yayudae ypdiv)
replace ynl_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & yremesade==. & yremesae==. & yayudae==. & ypdiv==.
replace ynl_ci=0 if p88l01c5==. & p88l01c1==. & p88l02c5==. & p88l02c1==. & p88l03c5==. & p88l03c1==. & p88l04c5==. & p88l04c1==. & p88l10c5==. & p88l10c1==. & p88l06c5==. & p88l06c2==. & p88l06c6==. & p88l06c2==. & p88l08c5==. & p88l08c1==. & p88l09c5==. & p88l09c1==. & p88l12c5==. & p88l12c1==. & p88l05c5==. & p88l05c1==. & p88l11c5==. & p88l11c1==.  & p88l07c5==. & p88l07c1==. & p88l06c8==. & p88l06c4==. & p88l06c7==. & p88l06c3==. & p88l08c7==. & p88l08c3==.
label var ynl_ci "Ingreso No Laboral (Monetario + No Monetario)"


***********************
*** nrylmpri_ci     ***
*********************** 

gen nrylmpri_ci=0 
replace nrylmpri_ci=1 if p46==99999
replace nrylmpri_ci=1 if p55==999999
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

***********************
*** ylmnr_ci        ***
***********************

egen ylmnr_ci=rsum(ylmpri_ci ylmsec_ci) if nrylmpri_ci==0
replace ylmnr_ci=. if ylmpri_ci==. 
label var ylmnr_ci "Ingreso Laboral Monetario Total, considera 'missing' la No Respuesta "

***********************
*** yl_ci           ***
***********************

egen yl_ci=rsum(ylmnr_ci ylnm_ci)
replace yl_ci=. if ylmnr_ci==. & ylnm_ci==.
label var yl_ci "Ingreso Laboral Individual (Monetario + No Monetario)"

***********************
*** nrylmpri_ch     ***
***********************

egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh_ch)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch<.
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

***********************
*** ylm_ch          ***
***********************

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

***********************
*** ylmnr_ch        ***
***********************

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

***********************
*** ylnm_ch         ***
***********************

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

***********************
*** ynlm_ch         ***
***********************

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

***********************
*** ynlm2_ch        ***
***********************

egen ynlm2_ch=sum(ynlm2_ci) if miembros_ci==1, by(idh_ch)
label var ynlm2_ch "Ingreso No Laboral Monetario 2 del Hogar"

***********************
*** ynlm4_ch        ***
***********************

egen ynlm4_ch=sum(ynlm4_ci) if miembros_ci==1, by(idh_ch)
label var ynlm4_ch "Ingreso No Laboral Monetario 4 del Hogar"

***********************
*** ynlnm_ch        ***
***********************

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

***********************
*** ynl_ch          ***
***********************

egen ynl_ch=sum(ynl_ci) if miembros_ci==1, by(idh_ch)
label var ynl_ch "Ingreso No Laboral del Hogar (monetario + no monetario)"

***********************
*** autoconsumo_ch  ***
***********************

egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)
label var autoconsumo_ch "Autoconsumo del Hogar"

***********************
*** remesasm_ch     ***
***********************

egen remesasm_ch=sum(remesasm_ci) if miembros_ci==1, by(idh_ch)
label var remesasm_ch "Remesas del Hogar (monetario)"
egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"

***********************
*** rama_ci         ***
***********************

*** ocupacion principal ***

gen rama_ci=ramaop
replace rama_ci=. if ramaop==10 | ramaop==11 | emp_ci==0

drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ypenju2 ysubsi2 yalquile2 ybonos2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 yjub2 yremesade yremesae yayudae ypdiv autoconsumop_ci autoconsumos_ci

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


gen horaspri_ci=p36 if p36<=168 
label var horaspri "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p62 if p62<168
egen horastot=rsum(horaspri horassec)
replace horastot=. if horaspri==. & horassec==.
label var horassec "Horas totales trabajadas en todas las Actividades"
drop horassec

gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"
gen ylmho_ci=ylm_ci/(4.3*horastot)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

gen b= p39/365 if p39a == 1
replace b= p39/52 if p39a ==2
replace b= p39/12 if p39a ==3
replace b= p39 if p39a ==4

gen antiguedad=b if emp_ci==1
replace antiguedad=. if p39a==. & b==.
drop b
label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"

gen tamfirma_ci=1 if p38==2
replace tamfirma_ci=0 if p38==1
label var tamfirma "Trabajadores formales: 1 = + de 10 empleados"


gen durades_ci =.
replace durades_ci= p20 if p20a==3
replace durades_ci= p20/30 if p20a ==1
replace durades_ci=p20/4 if p20a==2

/*
gen durades_ci=.
replace durades_ci=p25 if p25a==3
replace durades_ci=p25/4.3 if p25a==2
replace durades_ci=p25/12 if p25a==4
replace durades_ci=p25/30 if p25a==1
label var durades "Duracion del Desempleo (en meses)"*/

*gen segsoc_ci=.
*label var segsoc "Personas que cuentan con seguro social"
gen nempleos_ci=.
replace nempleos_ci=p58
/* MLO: modificacion 2013
gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if emp_ci==1 & p58==1
replace nempleos=0 if emp_ci==0*/
label var nempleos "Numero de empleos"

***************
*subemp_ci    *	
***************
egen toths=rsum(p36 p62)

gen subemp_ci=.
replace subemp_ci=1 if toths>=1 & toths<30 & p85==1
recode subemp_ci .=0 if  condact==1
tab subemp_ci [w=int(factor_ci)]
/*
gen subemp_ci=.
replace subemp=0 if emp==0 | emp==1
replace subemp=1 if horastot<30 
label var subemp "Trabajadores subempleados"*/

*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p38a>=1 & p38a<=5
replace firmapeq_ci=0 if p38a>=6 & p38a<9999	

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp25_ci
*********
gen lp25_ci = 734.7224


label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = 1175.556


label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"

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

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if p40==1 | p40==2
replace tipocontrato_ci=3 if p40==3 | p40==4
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

gen tamemp_ci=p38a
replace tamemp_ci=. if p38a==99999
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
gen yjub= p88l01c1 
gen ypensi=p88l02c1 
egen aux1=rowtotal(yjub ypensi), missing


gen pension_ci =0
replace pension_ci=1 if aux1!=. & aux1!=0

drop aux1
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

egen ypen_ci=rowtotal(yjub ypensi), missing
label var ypen_ci "Valor de la pension contributiva"

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
gen salmm_ci= 2515.10

label var salmm_ci "Salario minimo legal"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if  nivasiste==7 
replace tecnica_ci=0 if  nivasiste!=7
label var tecnica_ci "1=formacion terciaria tecnica"


*YL: genero las siguientes variables con missing para correr sociometro. Estas variables deben ser creadas correctamente.
gen antiguedad_ci=.
gen ocupa_ci=.
gen horastot_ci=.
gen desemp2_ci=.
gen pea2_ci=.
gen semestre_c=.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen desalent_ci=.
gen tiempoparc_ci=.
gen spublico_ci=.
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


