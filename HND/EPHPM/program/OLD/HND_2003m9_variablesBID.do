

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
 


*global ruta = "\\Sdssrv03\surveys"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2003"
local ronda m9

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
Round: m9
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 9 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*****                            HONDURAS 2003 - SEPTIEMBRE                                           *****
*****                EPHPM 2003 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                   41.003 personas                                               ***** 
*****                                    8.057 hogares                                                *****

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

*** HOUSING ***
gen aguared_ch=.
replace aguared_ch=1 if v05a==1
replace aguared_ch=0 if v05a==2 

gen aguadist_ch=.
replace aguadist_ch=1 if v05c==1
replace aguadist_ch=2 if v05c==2 | v05c==3
replace aguadist_ch=3 if v05c==4

gen aguamala_ch=.
replace aguamala_ch=1 if v05b>=5 & v05b<=8
replace aguamala_ch=0 if v05b>=1 & v05b<=4

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if v07==1 | v07==2 | v07==3
replace luz_ch=0 if v07>=4 & v07<=8

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.
replace bano_ch=1 if v06a==1
replace bano_ch=0 if v06a==2

gen banoex_ch=.
replace banoex=1 if v06c==1
replace banoex=0 if v06c==2

gen des1_ch=.
replace des1_ch=0 if v06a==2
replace des1_ch=1 if v06b==1 | v06b==2
replace des1_ch=2 if v06b==5 | v06b==6 | v06b==7
replace des1_ch=3 if v06b==3 | v06b==4

gen des2_ch=.
replace des2_ch=1 if v06b==1 | v06b==2 | v06b==3 
replace des2_ch=2 if v06b==4 | v06b==5 | v06b==6 | v06b==7
replace des2_ch=0 if v06a==2

gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8

gen pared_ch=.
replace pared_ch=0 if v02==5 | v02==6
replace pared_ch=1 if v02>=1 & v02<=4
replace pared_ch=2 if v02==7

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8

gen resid_ch=.
replace resid_ch=0 if v08==1
replace resid_ch=1 if v08==3 | v08==5
replace resid_ch=2 if v08==2 | v08==6
replace resid_ch=3 if v08==7 | v08==4

gen dorm_ch=.
replace dorm_ch=v12b if v12b>=0 

gen cuartos_ch=.
replace cuartos_ch=v12a if v12a>=0 

gen cocina_ch=.

gen telef_ch=.
replace telef_ch=1 if v11f==1
replace telef_ch=0 if v11f==2

gen refrig_ch=.
replace refrig_ch=1 if v11a==1
replace refrig_ch=0 if v11a==2

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if v11h==1
replace auto_ch=0 if v11h==2

gen compu_ch=.
replace compu_ch=1 if v11i==1
replace compu_ch=0 if v11i==2

gen internet_ch=.

gen cel_ch=.
replace cel_ch=1 if v11g==1
replace cel_ch=0 if v11g==2

gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=8) | v01==2 | v01==3

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if v10a==5
replace viviprop_ch=1 if v10a==1
replace viviprop_ch=2 if v10a==4
replace viviprop_ch=3 if v10a==6 | v10a==2 | v10a==3

gen vivitit_ch=.
replace vivitit_ch=1 if v13a==1
replace vivitit_ch=0 if v13a==2

/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a julio 2003: 17.56		
                                                                     agosto 2003: 17.65 
								     septiembre 2003: 17.73 
								     promedio 3 meses: 17.65 */



gen vivialq_ch=.
replace vivialq_ch=v10c if v10c<99999 & v10b==1
replace vivialq_ch=v10c/17.73 if v10c<99999 & v10b==2

gen vivialqimp_ch=.

gen pais_c="HND"
gen anio_c=2003
gen mes_c=9
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"
label value mes_c mes_c

replace numhog=1 if numhog==.
egen idh_ch=group(hogar depto domi numhog) 

gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"
gen factor_ci=factor_ch

gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4
replace zona_c=0 if domi==5
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
replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j==7 | rela_j== 8 
replace relacion_ci=5 if rela_j==9 | rela_j==11
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion relacion


egen byte nconyuges_ch=sum(rela_j==2), by (idh)
label variable nconyuges "Numero de Conyuges"

egen byte nhijos_ch=sum((rela_j==3 | rela_j==4)), by (idh)
label variable nhijos_ch "Numero de Hijos"

egen byte notropari_ch=sum(rela_j==5 | rela_j==6 | rela_j==7 | rela_j==8),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

egen byte notronopari_ch=sum(rela_j==9 | rela_j==11), by (idh)
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

label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch


egen nmayor21_ch=sum((rela_j>0 & rela_j<=9) & (edad>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((rela_j>0 & rela_j<=9) & (edad<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((rela_j>0 & rela_j<=9) & (edad>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((rela_j>0 & rela_j<=9) & (edad<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((rela_j>0 & rela_j<=9) & (edad<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

gen idp_ci=nper
label var idp_ci "Identificador Individual dentro del Hogar"

gen asiste_ci=.
replace asiste_ci=1 if p02==1
replace asiste_ci=0 if p02==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

gen pqnoasis_ci=p03
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

gen repiteult_ci=.
replace repiteult_ci=1 if p05c==1
replace repiteult_ci=0 if p05c==2
label var repiteult_ci "Personas que han repetido el ultimo grado"

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

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

gen eduno_ci=.
replace eduno=1 if (p04a==1 & edad>=5) | (p05a==3 & p05b==1)
replace eduno=0 if (p04a>3 & p04a<=9 & edad>=5) | ((p05a>2 & p05a<9) | (p05a==3 & p05b>1))    
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupi_ci=.
replace edupi=1 if (p04a==4 & p04b<6 & p04b>=0) | (p05a==3 & p05b<7 & p05b>=1)
replace edupi=0 if ((p04a>=5 & p04a<=9) | (p04a==4 & p04b>=6)) | ((p05a>=4 & p05a<9) | (p05a==3 & p05b>=7)) | (eduno==1)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc=1 if (p04a==4 & p04b==6) 
replace edupc=0 if (edupi==1 | eduno==1) | (p04a==4 & p04b>6) | (p05a==3 & p05b>=7) | (p04a>4 & p04a<=9) | (p05a>3 & p05a<9)
replace edupi=1 if p04a==4 & (p04b==0 | p04b==.)
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi=1 if (p04a==4 & p04b>6 & p04b<.) | (p05a==3 & p05b>=7 & p05b<.) | (p04a==5 & p04b<3) | (p04a==6 & p04b<4) | (p05a==4 & p05b<=3) | (p05a==5 & p05b<=4)
replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (p04a==5 & p04b>=3 & p04b<.) | (p04a==6 & p04b>=4 & p04b<.) | (p04a>=7 & p04a<=9) | (p05a>=6 & p05a<9) 
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc=1 if (p04a==5 & p04b>=3 & p04b<.) | (p04a==6 & p04b>=4 & p04b<.) 
replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (p04a>6 & p04a<=9) | ( p05a>5& p05a<9)
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui=1 if (p04a==7 & p04b<3) | (p04a==8 & p04b<5) | (p05a==6 & p05b<3) | (p05a==7 & p05b<5)
replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (p04a==7 & p04b>=3) | (p04a==8 & p04b>=5) | (p05a==6 & p05b>=3) | (p05a==7 & p05b>=5) | (p04a==9) | (p05a==8)
label var eduui_ci "1 = personas que no han completado el nivel universitario"

gen eduuc_ci=.
replace eduuc=1 if (p04a==7 & p04b>=3 & p04b<.) | (p04a==8 & p04b>=5 & p04b<.) | (p05a==6 & p05b>=3 & p05b<.) | (p05a==7 & p05b>=5 & p05b<.) | p04a==9 | p05a==8
replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 
label var eduuc_ci "1 = personas que han completado el nivel universitario"

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

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (p04a==4 & p04b==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (p04a==5 & p04b<3) | (p04a==6 & p04b<4) | (p05a==4) | (p05a==5)
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen edupre_ci=.
replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1
replace edupre=1 if p04a==3 | p05a==2
label var edupre_ci "Educacion preescolar"

gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if p04a==8 | p05a==7
label var eduac_ci "Educacion universitaria vs educacion terciaria"

gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"


gen miembros_ci=1 if rela_j>=1 & rela_j<=9
replace miembros_ci=0 if rela_j==10 | rela_j==11

gen uno=1 if miembros_ci==1
egen nmiembros_ch=sum(uno), by(idh_ch)
replace nmiembros_ch=. if miembros_ci!=1
label var nmiembros_ch "Numero de miembros del Hogar"
drop uno


gen categopri_ci=1 if p27==7
replace categopri_ci=2 if p27==4 | p27==5 | p27==6
replace categopri_ci=3 if p27==1 | p27==2 | p27==3
replace categopri_ci=4 if p27==8 | p27==9
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

gen categosec_ci=1 if p39==7
replace categosec_ci=2 if p39==4 | p39==5 | p39==6
replace categosec_ci=3 if p39==1 | p39==2 | p39==3
replace categosec_ci=4 if p39==8 | p39==9
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci


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
/*
gen emp_ci=.
replace emp_ci=1 if p10==1 | p11==1
replace emp_ci=0 if p10==2 & p11==2
label var emp_ci "Empleado en la semana de referencia"*/

gen ylmpri_ci=.
replace ylmpri_ci=p31 if p31<999999 & edad>4 & p29==1 & p30==1
replace ylmpri_ci=p31*p30 if p31<999999 & edad>4 & ((p29==2 & (p30==1 | p30==2)) | (p29==3 & p30>=1 & p30<=4) | (p29==4 & p30>=1 & p30<=31))
replace ylmpri_ci=p33 if p33<999999 & edad>4 & p33>=0
replace ylmpri_ci=0 if p31==0 & p33==0 & edad>4 & (p10==1 | p11==1)
replace ylmpri_ci=0 if (p27==8 | p27==9) & edad>4 & (p10==1 | p11==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

gen ylmsec_ci=.
replace ylmsec_ci=p43 if p43<99999 & edad>4 & p41==1 & p42==1
replace ylmsec_ci=p43*p42 if p43<99999 & edad>4 & ((p41==2 & (p42==1 | p42==2)) | (p41==3 & p42>=1 & p42<=4) | (p41==4 & p42>=1 & p42<=31))
replace ylmsec_ci=p45 if p45<99999 & edad>4 & p45>=0
replace ylmsec_ci=0 if p43==0 & p45==0 & edad>4 & p35==1
replace ylmsec_ci=0 if (p39==8 | p39==9) & edad>4 & p35==1
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso Laboral Monetario Total"

gen horaspri_ci=p21 if p21<=168
label var horaspri "Horas totales trabajadas en la Actividad Principal"
gen horassec_ci=p36 if p36<168
egen horastot=rsum(horaspri horassec)
replace horastot=. if horaspri==. & horassec==.
label var horassec "Horas totales trabajadas en todas las Actividades"
drop horassec

gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"
gen ylmho_ci=ylm_ci/(4.3*horastot)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

gen durades_ci=.
replace durades_ci=p19b
replace durades=0.5 if p19a==1 & p19b==.
label var durades "Duracion del Desempleo (en meses)"

gen b=p25b/12
egen antiguedad=rsum(p25a b) if emp_ci==1
replace antiguedad=. if p25a==. & b==.
drop b
label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"
/*
gen desemp1_ci=.
replace desemp1_ci=1 if p14a==1 & p14b==1
replace desemp1_ci=0 if p14a==2 | emp==1
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

gen desemp2_ci=.
replace desemp2_ci=0 if desemp1==0
replace desemp2_ci=1 if desemp1_ci==1 | p15==3 | p15==4
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola"

gen desemp3_ci=.
replace desemp3_ci=0 if desemp2==0 
replace desemp3_ci=1 if desemp2==1 | (p14a==1 & p14b==2)
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
replace desalent=0 if pea==1
replace desalent=1 if p15==5
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

gen subemp_ci=.
replace subemp=0 if emp_ci==0 | emp_ci==1
replace subemp=1 if horastot<30 & p47==1
label var subemp "Trabajadores subempleados"
gen tiempoparc=.
replace tiempoparc=0 if emp_ci==0 | emp_ci==1
replace tiempoparc=1 if horastot<30 & p47==2
label var tiempoparc "Trabajadores a medio tiempo"

*gen contrato_ci=.
*label var contrato "Peronas empleadas que han firmado un contrato de trabajo"

*gen segsoc_ci=.
*label var segsoc "Personas que cuentan con seguro social"

gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if emp_ci==1 & p35==1
replace nempleos=0 if emp_ci==0
label var nempleos "Numero de empleos"

gen tamfirma_ci=1 if p26a==2
replace tamfirma_ci=0 if p26a==1
label var tamfirma "Trabajadores formales: 1 = + de 10 empleados"

gen spublico_ci=.
label var spublico "Personas que trabajan en el sector publico"

gen yalim2=p32a if p32a<99999 & p32a>=0
gen yropa2=p32b if p32b<99999 & p32b>=0
gen yhabita2=p32c if p32c<99999 & p32c>=0
gen ytrans2=p32d if p32d<99999 & p32d>=0
gen yotro2=p32e if p32e<99999 & p32e>=0
gen yprodu2=p34 if p34<99999 & p34>=0

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p32a==. & p32b==. & p32c==. & p32d==. & p32e==. & categopri==3) | (p34==. & (categopri==1 | categopri==2))) & edad>4 & (p10==1 | p11==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

gen yalim3=p44a if p44a<99999 & p44a>=0
gen yropa3=p44b if p44b<99999 & p44b>=0
gen yhabita3=p44c if p44c<99999 & p44c>=0
gen ytrans3=p44d if p44d<99999 & p44d>=0
gen yotro3=p44e if p44e<99999 & p44e>=0
gen yprodu3=p46 if p46<99999 & p46>=0

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((p44a==. & p44b==. & p44c==. & p44d==. & p44e==. & categosec==3) | (p46==. & (categosec==1 | categosec==2))) & edad>4 & p35==1
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso Laboral No Monetario Total"

gen autoconsumop_ci=yprodu2 
replace autoconsumop_ci=0 if p34==. & edad>4 & (categopri==1 | categopri==2) & (p10==1 | p11==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p46==. & edad>4 & (categosec==1 | categosec==2) & p35==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

gen ypenju2=p501b/3 if p501b>=0 
replace ypenju2=p501a if p501a>=0 & (p501b==. | p501b==0)
gen yjub2=p503b/3 if p503b>=0
replace yjub2=p503a if p503a>=0 & (p503b==. | p503b==0)
gen yalquile2=p505b/3 if p505b>=0
replace yalquile2=p505a if p505a>=0 & (p505b==. | p505b==0)
gen ysubsi2=p507b/3 if p507b>=0
replace ysubsi2=p507a if p507a>=0 & (p507b==. | p507b==0)
gen ybonos2=p5017b/3 if p5017b>=0 
replace ybonos2=p5017a if p5017a>=0 & (p5017b==. | p5017b==0)
gen yremesa2=p509b/3 if p509b>=0
replace yremesa2=p509a if p509a>=0 & (p509b==. | p509b==0)
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a julio 2003: 17.56		
                                                                     agosto 2003: 17.65 
								     septiembre 2003: 17.73 
								     promedio 3 meses: 17.65 */
gen yremesad2=p5011b/3*17.65 if p5011b>=0
replace yremesad2=p5011a*17.73 if p5011a>=0 & (p5011b==. | p5011b==0)
gen yayuda2=p5014b/3 if p5014b>=0
replace yayuda2=p5014a if p5014a>=0 & (p5014b==. | p5014b==0)
gen yayupar2=p5016b/3 if p5016b>=0 
replace yayupar2=p5016a if p5016a>=0 & (p5016b==. | p5016b==0)
gen yotros2=p5019b/3 if p5019b>=0 
replace yotros2=p5019a if p5019a>=0 & (p5019b==. | p5019b==0)

** Pensiones, jubilaciones y alquileres en dolares **
gen ypenjud2=p502b*17.65/3 if p502b>=0 
replace ypenjud2=p502a*17.73 if p502a>=0 & (p502b==. | p502b==0) 
gen yjubd2=p504b*17.65/3 if p504b>=0
replace yjubd2=p504a*17.73 if p504a>=0 & (p504b==. | p504b==0)
gen yalquiled2=p506b*17.65/3 if p506b>=0
replace yalquiled2=p506a*17.73 if p506a>=0 & (p506b==. | p506b==0)

gen interes2=p508b/3 if p508b>=0 
replace interes2=p508a if p508a>=0 & (p508b==. | p508b==0)
gen prestlab2=p5018b/3 if p5018b>=0 
replace prestlab2=p5018a if p5018a>=0 & (p5018b==. | p5018b==0)

** Especies **
gen yremesade=p5012b/3*17.65 if p5012b>=0
replace yremesade=p5012a*17.73 if p5012a>=0 & (p5012b==. | p5012b==0)
gen yremesae=p5010b/3 if p5010b>=0
replace yremesae=p5010a if p5010a>=0 & (p5010b==. | p5010b==0)
gen yayudae=p5015b/3 if p5015b>=0
replace yayudae=p5015a if p5015a>=0 & (p5015b==. | p5015b==0)
/* No estan las ayudas de particulares en especie:
gen yayupare=v53iu3e/3 if v53iu3e>=0
replace yayupare=v53iue if v53iue>=0 & v53iu3e==.*/

/*No estan las herencias: gen herencia2=v46ju3m/3 if v46ju3m>=0 
replace herencia2=v46jum if v46jum>=0 & v46ju3m==. */

** Hay pension por divorcio: p5013b 
gen ypdiv=p5013b/3 if p5013b>=0
replace ypdiv=p5013a if p5013a>=0 & (p5013b==. | p5013b==0)
sum ypdiv if ypdiv>=0

egen remesasm_ci=rsum(yremesa2 yremesad2)
replace remesasm_ci=. if yremesa2==. & yremesad2==. 
replace remesasm_ci=0 if p509b==0 & p509a==0 & p5011b==0 & p5011a==0 
label var remesasm_ci "Remesas Individuales (monetario)"

egen remesas_ci=rsum(yremesa2 yremesad2 yremesade yremesae)
replace remesas_ci=. if yremesa2==. & yremesad2==. & yremesade==. & yremesae==.
replace remesas_ci=0 if p509b==0 & p509a==0 & p5011b==0 & p5011a==0 & p5012b==0 & p5012a==0 & p5010b==0 & p5010a==0 
label var remesas_ci "Remesas Individuales (monetario + especies)"

egen ynlm_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 ypenjud2 yjubd2 yalquiled2 yremesad2)
replace ynlm_ci=0 if p501b==. & p501a==. & p503b==. & p503a==. & p507b==. & p507a==. & p505b==. & p505a==. & p5017b==. & p5017a==. & p509b==. & p509a==. & p5014b==. & p5014a==. & p5016b==. & p5016a==. & p5019b==. & p5019a==. & p502b==. & p502a==. & p504b==. & p504a==. & p506b==. & p506a==. & p5011b==. & p5011a==.
replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & yjub2==. & ypenjud2==. & yjubd2==. & yalquiled2==. & yremesad2==.
label var ynlm_ci "Ingreso No Laboral Monetario"

egen ynlm2_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypenjud2 yjubd2 yalquiled2)
replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypenjud2==. & yjubd2==. & yalquiled2==.
replace ynlm2_ci=0 if p501b==. & p501a==. & p503b==. & p503a==. & p507b==. & p507a==. & p505b==. & p505a==. & p5017b==. & p5017a==. & p509b==. & p509a==. & p5014b==. & p5014a==. & p5016b==. & p5016a==. & p5019b==. & p5019a==. & p508b==. & p508a==. & p5018b==. & p5018a==. & p5011b==. & p5011a==. & p502b==. & p502a==. & p504b==. & p504a==. & p506b==. & p506a==.
label var ynlm2_ci "Ingreso No Laboral Monetario 2"

egen ynlm4_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypdiv ypenjud2 yjubd2 yalquiled2)
replace ynlm4_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypdiv==. & ypenjud2==. & yjubd2==. & yalquiled2==.
replace ynlm4_ci=0 if p501b==. & p501a==. & p503b==. & p503a==. & p507b==. & p507a==. & p505b==. & p505a==. & p5017b==. & p5017a==. & p509b==. & p509a==. & p5014b==. & p5014a==. & p5016b==. & p5016a==. & p5019b==. & p5019a==. & p508b==. & p508a==. & p5018b==. & p5018a==. & p5013b==. & p5013a==. & p5011b==. & p5011a==. & p502b==. & p502a==. & p504b==. & p504a==. & p506b==. & p506a==.
label var ynlm4_ci "Ingreso No Laboral Monetario 4"

egen ynlnm_ci=rsum(yremesade yremesae yayudae)
replace ynlnm_ci=. if yremesade==. & yremesae==. & yayudae==. 
replace ynlnm_ci=0 if p5012b==. & p5012a==. & p5010b==. & p5010a==. & p5015b==. & p5015a==. 
label var ynlnm_ci "Ingreso No Laboral No Monetario"

egen ynl_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yremesade yremesae yayudae ypdiv ypenjud2 yjubd2 yalquiled2)
replace ynl_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & yremesade==. & yremesae==. & yayudae==. & ypdiv==. & ypenjud2==. & yjubd2==. & yalquiled2==.
replace ynl_ci=0 if p501b==. & p501a==. & p503b==. & p503a==. & p507b==. & p507a==. & p505b==. & p505a==. & p5017b==. & p5017a==. & p509b==. & p509a==. & p5014b==. & p5014a==. & p5016b==. & p5016a==. & p5019b==. & p5019a==. & p508b==. & p508a==. & p5018b==. & p5018a==. & p5013b==. & p5013a==. & p5011b==. & p5011a==. & p5012b==. & p5012a==. & p5010b==. & p5010a==. & p5015b==. & p5015a==. & p502b==. & p502a==. & p504b==. & p504a==. & p506b==. & p506a==.
label var ynl_ci "Ingreso No Laboral (Monetario + No Monetario)"


gen nrylmpri_ci=0 
replace nrylmpri_ci=1 if p31==999999
replace nrylmpri_ci=1 if p33==999999
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

egen ylmnr_ci=rsum(ylmpri_ci ylmsec_ci) if nrylmpri_ci==0
replace ylmnr_ci=. if ylmpri_ci==. 
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

gen rama_ci=rama
replace rama_ci=. if rama==10 | rama==11 | emp_ci==0

drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ysubsi2 ybonos2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 yremesade yremesae yayudae ypdiv autoconsumop_ci autoconsumos_ci   

**************************************
****** EQXIS: MDGs VARIABLES    ****** 
**************************************

**************************************
****** Last update: May, 2008   ****** 
**************************************


* Instituto Nacional de Estadística, Encuesta Permanente de Hogares de Propósitos Múltiples. March, 2003.

/*
rela_j
 1. jefe del hogar
 2. esposa(o) o compañera(o) 
 3. hijos de mayor a menor
 4. hijastros de mayor a menor
 5. padres 
 6. yernos y nueras 
 7. otros parientes(nietos,abuelos,tios...)
 8. otros no parientes(suegros,cuñados...)
 9. servicio doméstico
 99. ns/nr
*/

* Variables

 rename rela_j parentco
 rename ur area
 rename p01 alfabet
 rename p02 asiste

 tab area [iw=factor]

* Gender classification of the population refering to the head of the household.

 sort hogar nper
 
 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(hogar)

* Dwelling ID

 gen str7 hogar_s=string(hogar)
 gen id_viv=substr(hogar_s,1,6)

** Years of education. 
/*
2. ¿Recibe actualmente enseñanza de algún centro educativo?
 1. SI /2. NO

3. ¿Cuál es el nivel más alto de estudio que está cursoando o que
cursó? y ¿Cuál es el último año aprobado en ese nivel?

niveduc(p03a)				
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

ultgrad (p03b)

*/

* Included in the database

 rename anosest a_orig

 gen	 anoest=0  if (p04a==1 | p04a==2 | p04a==3) | (p04a==4 & p04b==0) 
 replace anoest=1  if (p04a==4 & p04b==1) 	
 replace anoest=2  if (p04a==4 & p04b==2) 	
 replace anoest=3  if (p04a==4 & p04b==3) 	
 replace anoest=4  if (p04a==4 & p04b==4) 	
 replace anoest=5  if (p04a==4 & p04b==5) 	
 replace anoest=6  if (p04a==4 & p04b==6) | (p04a==5 & p04b==0) 	
 replace anoest=7  if (p04a==4 & p04b==7) | (p04a==5 & p04b==1) 
 replace anoest=8  if (p04a==4 & p04b==8) | (p04a==5 & p04b==2) 
 replace anoest=9  if (p04a==4 & p04b==9) | (p04a==5 & p04b==3) | (p04a==6 & p04b==0) | (p04a==7 & p04b==0) 
 replace anoest=10 if (p04a==6 & p04b==1) | (p04a==7 & p04b==1) 
 replace anoest=11 if (p04a==6 & p04b==2) | (p04a==7 & p04b==2) 
 replace anoest=12 if (p04a==6 & p04b==3) | (p04a==7 & p04b==3) | (p04a==8 & p04b==0) 
 replace anoest=13 if (p04a==6 & p04b==4) | (p04a==8 & p04b==1) 
 replace anoest=14 if (p04a==8 & p04b==2) 
 replace anoest=15 if (p04a==8 & p04b==3) 
 replace anoest=16 if (p04a==8 & p04b==4) 
 replace anoest=17 if (p04a==8 & p04b==5) | (p04a==9 & p04b==0) 
 replace anoest=18 if (p04a==8 & p04b==6) | (p04a==9 & p04b==1) 
 replace anoest=19 if (p04a==8 & p04b==7) | (p04a==9 & p04b==2) 
 replace anoest=20 if (p04a==8 & p04b==8) | (p04a==9 & p04b==3) 
 replace anoest=21 if (p04a==9 & p04b==4)
 replace anoest=22 if (p04a==9 & p04b==5)
 replace anoest=23 if (p04a==9 & p04b==6)
 replace anoest=24 if (p04a==9 & p04b==7)


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
2. ¿Recibe actualmente enseñanza de algún centro educativo?
 1. SI /2. NO

3. ¿Cuál es el nivel más alto de estudio que está cursoando o que
cursó? y ¿Cuál es el último año aprobado en ese nivel?

niveduc(p04a)				
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

ultgrad (p04b)

*/

 rename p04a niveduc
 rename p04b ultgrad

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if (edad>=7 & edad<=12) & (asiste==1 | asiste==2)
 replace NERP=1 if (edad>=7 & edad<=12) & (asiste==1) & (niveduc==4 & (ultgrad>=0 & ultgrad<=5))
 label var NERP "Net Enrolment Ratio in Primary"


** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=13 & edad<=18) & (asiste==1 | asiste==2)
 replace NERS=1 if (edad>=13 & edad<=18) & (asiste==1) & ((niveduc==4 & (ultgrad>=6 & ultgrad<=9)) | ((niveduc==5)  | (niveduc==6 & (ultgrad>=0 & ultgrad<=2))))
 label var NERS "Net Enrolment Ratio in Secondary"
 
* Upper secondary
* Secundaria Diversificado

 gen	 NERS2=0 if (edad>=16 & edad<=18) & (asiste>=1 & asiste<=2) 
 replace NERS2=1 if (edad>=16 & edad<=18) & ( (niveduc==6 & (ultgrad>=0 & ultgrad<=2)) | (niveduc==5 & ultgrad==3) )
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

 gen prim=1 if  (asiste==1) &  (niveduc==4 & (ultgrad>=0 & ultgrad<=5))
 gen sec=1  if  (asiste==1) & ((niveduc==4 & (ultgrad>=6 & ultgrad<=9)) | (niveduc==5)  | (niveduc==6 & (ultgrad>=0 & ultgrad<=2)))
 gen ter=1  if  (asiste==1) & ((niveduc==6 & (ultgrad>=3 & ultgrad<=4)) | (niveduc==8 & (ultgrad>=0 & ultgrad<=4)) )

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

 noisily display "Tertiary"
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
 label var RATIOLIT2 "Ratio of Literate Women to Men 15-24 year olds"
 
** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.
 
 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 label var RATIOLIT "Ratio of Literate Women to Men 15-24 year olds"
 
 
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*

24. ¿Cuál es o era su categoría ocupacional 		rama
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
 
 
 * RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 
** Access to Electricity ** Additional Indicator
/*
7. ¿Qué tipo de alumbrado utiliza en la vivienda?
 1. Servicio Público
 2. Planta privada colectiva
 3. Planta privada individual
 4. Energía solar
 5. Vela
 6. Candil o lámpara de gas 
 7. Ocote
 8. Otro:________________________ */

 egen elec=max(v07), by(id_viv)

* Gender classification of the population refers to the head of the household.
 
 gen	 ELEC=0 if (elec>=1 & elec<=8) /* Total population excluding missing information */
 replace ELEC=1 if (elec>=1 & elec<=4)
 label var ELEC "Proportion of Population with access to electricity"
 	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

/*
5. Servicio de Agua (v05a)
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

 egen agua=max(v05b), by(id_viv)
 egen lugabast=max(v05c), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (agua>=1 & agua<=7) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4)
 label var WATER "Improved Water Source"
 
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*

6. Servicio Sanitario

a) ¿Qué tipo de servicio sanitario tiene?
 1. Inodoro conectado a alcantarilla
 2. Inodoro conectado a pozo séptico
 3. Inodoro con desagüe a río, laguna, mar
 4. Letrina con cierre hidráulico
 5. Letrina con pozo séptico
 6. Letrina con pozo negro
 7. No tiene

b) El uso del servicio sanitario es:
 1. Exclusivo de la vivienda
 2. Compartido con otras viviendas
*/

 egen servsani=max(v06a), by(id_viv)
 egen servexc= max(v06b), by(id_viv)

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (servsani>=1 & servsani<=7) /* Total population excluding missing information */
 replace SANITATION=1 if ((servsani>=1 & servsani<=2) | (servsani==5))
 label var SANITATION "Improved Sanitation" 
 
 
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*
1. Tipo de vivienda			10. ¿Cómo es la tenencia de esta vivienda?
v01					v10a
 1. Casa independiente			 1. Propietario y completamente pagada
 2. Apartamento				 2. Propietario recuperada legalizada
 3. Rancho 				 3. Propietario recuperada sin legalizar
 4. Cuarto en mesón o cuartería		 4. Propietario y la está pagando			
 5. Barracón				 5. Alquilada
 6. Casa improvisada 		         6. Cedida sin pago
 7. Local no construido para habitación 	
 pero usado como vivienda
 8. Albergue (not included in the database)
 9. Otro

2. ¿Cuál es el material predominante 	3. ¿Cuál es el material predominante en el piso?
en la construcción de las paredes?
 1. Ladrillo, piedra o bloque		 1. Cerámica
 2. Adobe				 2. Ladrillo de cemento
 3. Material prefabricado		 3. Ladrillo de barro
 4. Madera				 4. Plancha de cemento
 5. Bahareque, vara o caña		 5. Madera
 6. Desechos				 6. Tierra
 7. Otro				 7. Otro
				 
11. Cantidad de Piezas de la Vivienda
a). ¿Cuántas piezas tiene esta vivienda? (incluya la cocina pero no el baño)
*/

 egen tenencia=max(v10a), by(id_viv)
 egen tipoviv=max(v01), by(id_viv)
 egen piso=max(v03), by(id_viv)
 egen pared=max(v02), by(id_viv)
 egen nrocuart=max(v12a), by(id_viv)
 replace nrocuart=. if v12a==99
 egen pers=max(totpervi), by(id_viv) /* Total de personas de la vivienda */

 gen persroom=pers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=9) & (tenencia>=1 & tenencia<=6) /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==3 | tipoviv==5 | tipoviv==6 | tipoviv==7 | tipoviv==9) | (tenencia==6)

* 2. Low quality of the floor or walls materials.

 gen 	 secten_2=0 if (pared>=1 & pared<=7) & (piso>=1 & piso<=7) /* Total population excluding missing information */
 replace secten_2=1 if (pared>=5 & pared<=7) | (piso==6 | piso==7)

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
 
** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

* NA

** Target 18, Indicator: "Personal computers in use per 100 population"

* NA

** Target 18, Indicator: "Internet users per 100 population"

* NA

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen	 CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & (condact==1)
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
15. ¿Por qué no buscó trabajo ni trató de establecer su propio
negocio o finca la semana pasada?
 1. Se incorporará a un trabajo antes de un mes			==>17
 2. Tiene trabajo asegurado después de un mes 			==>17
 3. Espera respuesta a gestiones 				==>17
 4. Está esperando la próxima temporada de trabajo 		==>17
 5. Cree que no encontrará trabajo				==>17
 6. Dejó de buscar trabajo momentáneamente			==>17
 7. No tiene tierra, capital, ni materia prima			==>17
 8. No tiene tiempo para buscar trabajo				==>13
 9. No tiene necesidad de trabajar				==>13
 10. Por su edad no puede trabajar				==>13
 11.  Otro							==>13

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
 replace DISCONN=1 if (edad>=15 & edad<=24) & ((p15==5) | ((p15>=8 & p15<=11) & (p16==9)))
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
 global variable AEDUC_15
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


 noisily display "Grade for age secondary"
 gen GFAS=(anoest/(edad-7)) if (edad>=13 & edad<=18) & (anoest>=0 & anoest<99)
 label var GFAS "Grade for age secondary"
 
 
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp25_ci
*********
gen lp25_ci = 624.5643

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci =999.3029

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

gen cesante_ci=1 if p20==1 & condocup_ci==2
replace cesante_ci=0 if p20==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

gen tamemp_ci=p26b
replace tamemp_ci=. if  p26b>=9999
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
egen aux1=rsum(p501a p503a)if (p501a>100 & p501a!=.)  | (p503a>100 & p503a!=.), missing

gen pension_ci =0
replace pension_ci=1 if aux1!=. & aux1!=0

drop aux1
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
egen ypen_ci=rowtotal(p501a p503a), missing
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
gen tc_ci=17.93
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2003
gen salmm_ci= 	2042.4
label var salmm_ci "Salario minimo legal"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if niveduc==7 
replace tecnica_ci=0 if niveduc!=7
label var tecnica_ci "1=formacion terciaria tecnica"


*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p26b>=1 & p26b<=5
replace firmapeq_ci=0 if p26b>=6 & p26b<9999

	
gen antiguedad_ci=.
gen ocupa_ci=. 
gen horastot_ci=.
gen ylmotros_ci=.
gen  ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen tiempoparc_ci=.
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


