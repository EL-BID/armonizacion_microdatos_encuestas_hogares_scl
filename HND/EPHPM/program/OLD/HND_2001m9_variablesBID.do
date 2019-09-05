

use "X:\ARM\HON\2001\Septiembre\Orig_data\hnd01sep.dta", clear


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
local ANO "2001"
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
 opened on:  16 Aug 2006, 12:40:44
*******************************************************************************************************
*****                                   HONDURAS 2001 (SEPTIEMBRE)                                    *
*****                EPHPM 2001 (Encuesta Permanente de Hogares de Propositos Multiples)              *
*****                                   82.495 personas                                               *
*****                                    16.198 hogares                                               *
*******************************************************************************************************
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
compress

gen pais_c="HND"


*** HOUSING ***
gen aguared_ch=.

gen aguadist_ch=.

replace aguadist_ch=1 if v04b==6

replace aguadist_ch=2 if v04b==7 

replace aguadist_ch=3 if v04b==8

gen aguamala_ch=.

replace aguamala_ch=1 if v04a>=4 & v04a<=5

replace aguamala_ch=0 if v04a>=1 & v04a<=3

gen aguamide_ch=.

gen luz_ch=.

replace luz_ch=1 if v06==1 | v06==2 | v06==3

replace luz_ch=0 if v06>=4 & v06<=5

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.
replace bano_ch=1 if v05a>=1 & v05a<=5
replace bano_ch=0 if v05a==6


gen banoex_ch=.
replace banoex=1 if v05b==7
replace banoex=0 if v05b==8

gen des1_ch=.
gen des2_ch=.

replace des2_ch=1 if v05a==1 | v05a==2 

replace des2_ch=2 if v05a==3| v05a==4 | v05a==5 

replace des2_ch=0 if v05a==6

gen piso_ch=.

replace piso_ch=0 if v03==6

replace piso_ch=1 if v03>=1 & v03<=5 

replace piso_ch=2 if v03==7

gen pared_ch=.

replace pared_ch=0 if v02==4 | v02==5

replace pared_ch=1 if v02>=1 & v02<=3

replace pared_ch=2 if v02==6

gen techo_ch=.

gen resid_ch=.
replace resid_ch=0 if v07==1
replace resid_ch=1 if v07==3 | v07==4
replace resid_ch=2 if v07==2 | v07==5
replace resid_ch=3 if v07==6 

gen dorm_ch=.
replace dorm_ch=v10b if v10b>=0 

gen cuartos_ch=.
replace cuartos_ch=v10a if v10a>=0 

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.

gen vivi2_ch=.

replace vivi2_ch=1 if v01==1

replace vivi2_ch=0 if v01>=2 & v01<=7

gen viviprop_ch=.
replace viviprop_ch=0 if v08a==3
replace viviprop_ch=1 if v08a==1
replace viviprop_ch=2 if v08a==2
replace viviprop_ch=3 if v08a==4 | v08a==5 | v08a==6

gen vivitit_ch=.
gen vivialq_ch=.
replace vivialq_ch=v08b 

gen vivialqimp_ch=.

gen anio_c=2001

gen mes_c=9

label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"

label value mes_c mes_c

egen idh_ch=group(hogar depto domi vivi)  

gen factor_ch=factor

gen factor_ci=factor_ch

gen miembros_ci=1 if rela_j>=1 & rela_j<=8

replace miembros_ci=0 if rela_j>8

gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4

replace zona_c=0 if domi==5

label define zona_c 0 "Rural" 1 "Urbana" 

label value zona_c zona_c


gen uno=1 if miembros_ci==1

egen nmiembros_ch=sum(uno), by(idh_ch)

replace nmiembros_ch=0 if miembros_ci!=1

label var nmiembros_ch "Numero de miembros del Hogar"

drop uno


gen sexo_ci=sexo

label var sexo "Sexo del Individuo"

label define sexo_ci 1 "Masculino" 2 "Femenino"

label value sexo_ci sexo_ci


gen edad_ci=edad if edad<999

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

replace relacion_ci=4 if rela_j==5 | rela_j==6 | rela_j==7 | rela_j==8

label var relacion_ci "Relacion con el Jefe de Hogar"

label define relacion 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"

label value relacion relacion


egen byte nconyuges_ch=sum(rela_j==2), by (idh)

label variable nconyuges "Numero de Conyuges"


egen byte nhijos_ch=sum((rela_j==3 | rela_j==4)), by (idh)

label variable nhijos_ch "Numero de Hijos menores de 18"


egen byte notropari_ch=sum(rela_j==5 | rela_j==6 | rela_j==7 | rela_j==8),by (idh)

label variable notropari_ch "Numero de Otros Parientes "


gen byte notronopari_ch=.
label variable notronopari_ch "Numero de Otros NO Parientes "


gen byte nempdom_ch=.

label variable nempdom_ch "Numero de Empleados Domesticos"


*Aclaracion de Analia: Como notronopari_ch=., si lo ponemos en las condiciones que siguen, nunca se cumplen. Entonces, hay 
 *que quitarlo.
gen clasehog_ch=.

replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0/* nuclear (child with or without spouse but with out other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 /* ampliado*/

label variable clasehog_ch "CLASE HOGAR"

label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"

label value clasehog_ch clasehog_ch


egen nmayor21_ch=sum((rela_j>0 & rela_j<=8) & (edad>=21)), by (idh)

label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"


egen nmenor21_ch=sum((rela_j>0 & rela_j<=8) & (edad<21)), by (idh)

label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"


egen nmayor65_ch=sum((rela_j>0 & rela_j<=8) & (edad>=65)), by (idh)

label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"


egen nmenor6_ch=sum((rela_j>0 & rela_j<=8) & (edad<6)), by (idh)

label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"


egen nmenor1_ch=sum((rela_j>0 & rela_j<=8) & (edad<1)),  by (idh)

label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"


gen idp_ci=nper

label var idp_ci "Identificador Individual dentro del Hogar"


gen asiste_ci=.
replace asiste_ci=1 if p05==1

replace asiste_ci=0 if p05==2

label var asiste "Personas que actualmente asisten a centros de enseñanza"


gen pqnoasis_ci=.

label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

gen repiteult_ci=.

label var repiteult_ci "Personas que han repetido el ultimo grado"

gen repite_ci=.

label var repite_ci "Personas que han repetido al menos un año o grado"


* Años de educacion aprobados **
replace p06b=. if p06b==9

gen aedu_ci=.

replace aedu_ci=0 if (p06a>=1 & p06a<=3) 

replace aedu_ci=p06b if p06a==4 

replace aedu_ci=p06b+6 if p06a==5 | p06a==6

replace aedu_ci=p06b+12 if p06a==7 | p06a==8

replace aedu_ci=p06b+17 if p06a==9

label var aedu_ci "Años de educacion aprobados"


gen eduno_ci=.

replace eduno=1 if (p06a==1 & edad>=5) 
replace eduno=0 if (p06a>3 & p06a<=9 & edad>=5)     

label var eduno_ci "1 = personas sin educacion (excluye preescolar)"


gen edupi_ci=.

replace edupi=1 if (p06a==4 & p06b<6 & p06b>=0) 
replace edupi=0 if ((p06a>=5 & p06a<=9) | (p06a==4 & p06b>=6)) | (eduno==1)

label var edupi_ci "1 = personas que no han completado el nivel primario"


gen edupc_ci=.

replace edupc=1 if (p06a==4 & p06b==6) 

replace edupc=0 if (edupi==1 | eduno==1) | (p06a==4 & p06b>6) | (p06a>4 & p06b<=9 & p06a<=9) 

replace edupi=1 if p06a==4 & (p06b==0 | p06b==.)

label var edupc_ci "1 = personas que han completado el nivel primario"


gen edusi_ci=.

replace edusi=1 if (p06a==4 & p06b>6 & p06b<.) | (p06a==5 & p06b<6) | (p06a==6 & p06b<3)

replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (p06a==5 & p06b==6 & p06b<.) | (p06a==6 & p06b==3 & p06b<.) | (p06a>=7 & p06a<=9) 

label var edusi_ci "1 = personas que no han completado el nivel secundario"


gen edusc_ci=.

replace edusc=1 if (p06a==5 & p06b==6 & p06b<.) | (p06a==6 & p06b==3 & p06b<.) 

replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (p06a>6 & p06a<=9) 

label var edusc_ci "1 = personas que han completado el nivel secundario"


gen eduui_ci=.

replace eduui=1 if (p06a==7 & p06b<3) | (p06a==8 & p06b<5) 
replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (p06a==7 & p06b>=3) | (p06a==8 & p06b>=5) | (p06a==9) 

label var eduui_ci "1 = personas que no han completado el nivel universitario"


gen eduuc_ci=.

replace eduuc=1 if (p06a==7 & p06b>=3 & p06b<.) | (p06a==8 & p06b>=5 & p06b<.) | p06a==9 

replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 

label var eduuc_ci "1 = personas que han completado el nivel universitario"

replace edupi=1 if p06a==4 & p06b==.
replace edupc=0 if p06a==4 & p06b==.
replace edusi=1 if (p06a==5 | p06a==6) & p06b==.
replace edusc=0 if (p06a==5 | p06a==6) & p06b==.
replace eduui=1 if (p06a==7 | p06a==8) & p06b==.
replace eduuc=0 if (p06a==7 | p06a==8) & p06b==.

gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (p06a==5 & p06b<3) | (p06a==6 & p06b<3)

label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"


gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (p06a==5 & p06b==3) | (p06a==6 & p06b==3)

label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (p06a==5 & p06b>3 & p06b<6) 

label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"


gen edus2c_ci=.
replace edus2c=0 if edusi==1 | edusc==1 
replace edus2c=1 if edusi==1 & (p06a==5 & p06b==6) 

label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"


gen edupre_ci=.
replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1
replace edupre=1 if p06a==2 

label var edupre_ci "Educacion preescolar"


gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if p06a==8 

label var eduac_ci "Educacion universitaria vs educacion terciaria"

gen edupub_ci=.

label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"


gen categopri_ci=1 if p28==7
replace categopri_ci=2 if p28==4 | p28==5 | p28==6
replace categopri_ci=3 if p28==1 | p28==2 | p28==3
replace categopri_ci=4 if p28==8 | p28==9

label var categopri_ci "Categoria ocupacional actividad principal"

label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"

label value categopri_ci categopri_ci


gen categosec_ci=1 if p33==7
replace categosec_ci=2 if p33==4 | p33==5 | p33==6
replace categosec_ci=3 if p33==1 | p33==2 | p33==3
replace categosec_ci=4 if p33==8 | p33==9
label var categosec_ci "Categoria ocupacional actividad secundaria"

label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"

label value categosec_ci categosec_ci

/*
gen emp_ci=.
replace emp_ci=1 if p07==1 
replace emp_ci=0 if p07==2 
label var emp_ci "Empleado en la semana de referencia"
*/

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

gen ylmpri_ci=.
replace ylmpri_ci=p29a if (p29a<89999 & p29a!=9999) & edad>4 & p29b==1
replace ylmpri_ci=p29a*p29c if (p29a<89999 & p29a!=9999) & edad>4 & p29b==2 & (p29c==1 | p29c==2)
replace ylmpri_ci=p29a*p29c if (p29a<89999 & p29a!=9999) & edad>4 & p29b==3 & (p29c>=1 & p29c<=4)
replace ylmpri_ci=p29a*p29c if (p29a<89999 & p29a!=9999) & edad>4 & p29b==4 & (p29c>=1 | p29c<=31)
replace ylmpri_ci=p29d if edad>4 & ((p29b==2 & p29c>2 & p29c<.) | (p29b==3 & p29c>4 & p29c<.) | (p29b==4 & p29c>31 & p29c<.))
replace ylmpri_ci=p31 if p31<99999 & edad>4 & p31>0
replace ylmpri_ci=0 if p29a==0 & p31==0 & edad>4 & p07==1
replace ylmpri_ci=0 if (p28==8 | p28==9) & edad>4 & p07==1
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"


gen ylmsec_ci=.
replace ylmsec_ci=p34a if (p34a<99999 & p34a!=9999) & edad>4 & p34b==1
replace ylmsec_ci=p34a*p34c if (p34a<99999 & p34a!=9999) & edad>4 & p34b==2 & (p34c==1 | p34c==2)
replace ylmsec_ci=p34a*p34c if (p34a<99999 & p34a!=9999) & edad>4 & p34b==3 & (p34c>=1 & p34c<=4)
replace ylmsec_ci=p34a*p34c if (p34a<99999 & p34a!=9999) & edad>4 & p34b==4 & (p34c>=1 | p34c<=31)
replace ylmsec_ci=p34d if edad>4 & ((p34b==2 & p34c>2 & p34c<.) | (p34b==3 & p34c>4 & p34c<.) | (p34b==4 & p34c>31 & p34c<.))
replace ylmsec_ci=p36 if p36<99999 & edad>4 & p36>0
replace ylmsec_ci=0 if p34a==0 & p36==0 & edad>4 & p11a==1
replace ylmsec_ci=0 if (p33==8 | p33==9) & edad>4 & p11a==1

label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

label var ylm_ci "Ingreso Laboral Monetario Total"


gen horaspri_ci=p12a if p12a<=168

label var horaspri "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen horassec_ci=p12b if p12b<168

egen horastot=rsum(horaspri horassec)

replace horastot=. if horaspri==. & horassec==.

label var horassec "Horas totales trabajadas la semana pasada en todas las Actividades"

drop horassec
gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri)

label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

gen ylmho_ci=ylm_ci/(4.3*horastot)

label var ylmho_ci "Salario Horario Monetario de todas las Actividades"


gen durades_ci=.
replace durades_ci=p19b
replace durades=0.5 if p19a==1 & p19b==.
label var durades "Duracion del Desempleo (en meses)"


gen antiguedad=p26b if emp_ci==1
replace antiguedad=0.5 if p26a==1

label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"

/*
gen desemp1_ci=.
replace desemp1_ci=1 if p17==1 
replace desemp1_ci=0 if (p17==2 | p17==3) | emp==1

label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"


gen desemp2_ci=.
replace desemp2_ci=0 if desemp1==0
replace desemp2_ci=1 if desemp1_ci==1 | p20==3 | p20==4
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola"

gen desemp3_ci=.
replace desemp3_ci=0 if desemp2==0 
replace desemp3_ci=1 if desemp2==1 | p17==2 

label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

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
replace desalent=1 if p20==5
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

gen subemp_ci=.
replace subemp=0 if emp_ci==0 | emp_ci==1
replace subemp=1 if horastot<30 & p14==1

label var subemp "Trabajadores subempleados"

gen tiempoparc=.
replace tiempoparc=0 if emp_ci==0 | emp_ci==1
replace tiempoparc=1 if horastot<30 & p14==2

label var tiempoparc "Trabajadores a medio tiempo"


*gen contrato_ci=.
*label var contrato "Peronas empleadas que han firmado un contrato de trabajo"


*gen segsoc_ci=.

*label var segsoc "Personas que cuentan con seguro social"


gen nempleos_ci=1 if emp_ci==1
replace nempleos=2 if emp_ci==1 & p11a==1
replace nempleos=0 if emp_ci==0

label var nempleos "Numero de empleos"


gen tamfirma_ci=.
replace tamfirma=1 if p27a==2
replace tamfirma=0 if p27a==1
label var tamfirma "Trabajadores formales: 1 = + de 10 empleados"


gen spublico_ci=.
label var spublico "Personas que trabajan en el sector publico"

gen yalim2=p30av if p30av<99999 & p30av>=0
gen yropa2=p30bv if p30bv<99999 & p30bv>=0
gen yhabita2=p30cv if p30cv<99999 & p30cv>=0
gen ytrans2=p30dv if p30dv<99999 & p30dv>=0
gen yotro2=p30ev if p30ev<99999 & p30ev>=0
gen yprodu2=p32b if p32b<66666 & p32b>=0

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p30a==2 & p30b==2 & p30c==2 & p30d==2 & p30e==2) | p32a==2) & edad>4
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

gen yalim3=p35av if p35av<99999 & p35av>=0
gen yropa3=p35bv if p35bv<99999 & p35bv>=0
gen yhabita3=p35cv if p35cv<99999 & p35cv>=0
gen ytrans3=p35dv if p35dv<99999 & p35dv>=0
gen yotro3=p35ev if p35ev<99999 & p35ev>=0
gen yprodu3=p37b if p37b<99999 & p37b>=0

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)

replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((p35a==2 & p35b==2 & p35c==2 & p35d==2 & p35e==2) | p37a==2) & edad>4
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)

replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

label var ylnm_ci "Ingreso Laboral No Monetario Total"

gen autoconsumop_ci=yprodu2 

replace autoconsumop_ci=0 if p32b==. & p32a==2 & edad>4 & (categopri==1 | categopri==2) & p07==1 

label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"


gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p37b==. & p37a==2 & edad>4 & (categosec==1 | categosec==2) & p11a==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

gen ypenju2=p3801u3m/3 if p3801u3m>=0 & p3801u3m<999999
replace ypenju2=p3801um if p3801um>=0 & p3801um<999999 & (p3801u3m==. | p3801u3m==0)

gen yalquile2=p3802u3m/3 if p3802u3m>=0 & p3802u3m<999999
replace yalquile2=p3802um if p3802um>=0 & p3802um<999999 & (p3802u3m==. | p3802u3m==0)

gen ysubsi2=p3803u3m/3 if p3803u3m>=0 & p3803u3m<999999
replace ysubsi2=p3803um if p3803um>=0 & p3803um<999999 & (p3803u3m==. | p3803u3m==0)

gen ybonos2=p3808u3m/3 if p3808u3m>=0 & p3808u3m<999999
replace ybonos2=p3808um if p3808um>=0 & p3808um<999999 & (p3808u3m==. | p3808u3m==0)

gen yremesa2=p3805u3m/3 if p3805u3m>=0 & p3805u3m<999999
replace yremesa2=p3805um if p3805um>=0 & p3805um<999999 & (p3805u3m==. | p3805u3m==0)

gen yayuda2=p3806u3m/3 if p3806u3m>=0 & p3806u3m<999999

replace yayuda2=p3806um if p3806um>=0 & p3806um<999999 & (p3806u3m==. | p3806u3m==0)

gen yayupar2=p3807u3m/3 if p3807u3m>=0 & p3807u3m<999999

replace yayupar2=p3807um if p3807um>=0 & p3807um<999999 & (p3807u3m==. | p3807u3m==0)

gen yotros2=p3811u3m/3 if p3811u3m>=0 & p3811u3m<999999

replace yotros2=p3811um if p3811um>=0 & p3811um<999999 & (p3811u3m==. | p3811u3m==0)


gen interes2=p3804u3m/3 if p3804u3m>=0 & p3804u3m<999999

replace interes2=p3804um if p3804um>=0 & p3804um<999999 & (p3804u3m==. | p3804u3m==0)

gen prestlab2=p3809u3m/3 if p3809u3m>=0 & p3809u3m<999999

replace prestlab2=p3809um if p3809um>=0 & p3809um<999999 & (p3809u3m==. | p3809u3m==0)

gen herencia2=p3810u3m/3 if p3810u3m>=0 & p3810u3m<999999

replace herencia2=p3810um if p3810um>=0 & p3810um<999999 & (p3810u3m==. | p3810u3m==0)

gen remesas_ci=yremesa2

replace remesas_ci=0 if p3805u3m==0 & p3805um==0

label var remesas_ci "Remesas Individuales"

egen ynlm_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2)

replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==.

replace ynlm_ci=0 if p3801u3m==. & p3802u3m==. & p3803u3m==. & p3808u3m==. & p3805u3m==. & p3806u3m==. & p3807u3m==. & p3811u3m==. & p3801um==. & p3802um==. & p3803um==. & p3808um==. & p3805um==. & p3806um==. & p3807um==. & p3811um==.

label var ynlm_ci "Ingreso No Laboral Monetario"


egen ynlm2_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 interes2 prestlab2)

replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. &  yayupar2==. & yotros2==. & interes2==. & prestlab2==. 

replace ynlm2_ci=0 if p3801u3m==. & p3802u3m==. & p3803u3m==. & p3808u3m==. & p3805u3m==. & p3806u3m==.  & p3807u3m==. & p3804u3m==. & p3809u3m==. & p3811u3m==. & p3801um==. & p3802um==. & p3803um==. & p3808um==. & p3805um==. & p3806um==. & p3807um==. & p3804um==. & p3809um==. & p3811um==.

label var ynlm2_ci "Ingreso No Laboral Monetario 2"


egen ynlm3_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 interes2 prestlab2 herencia2)

replace ynlm3_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & herencia2==.

replace ynlm3_ci=0 if p3801u3m==. & p3802u3m==. & p3803u3m==. & p3808u3m==. & p3805u3m==. & p3806u3m==. & p3807u3m==. & p3804u3m==. & p3809u3m==. & p3810u3m==. & p3811u3m==. & p3801um==. & p3802um==. & p3803um==. & p3808um==. & p3805um==. & p3806um==. & p3807um==. & p3804um==. & p3809um==. & p3810um==. & p3811um==.

label var ynlm3_ci "Ingreso No Laboral Monetario 3"


gen nrylmpri_ci=0 

replace nrylmpri_ci=1 if (p29a==89999 | p29a==9999)

replace nrylmpri_ci=1 if p31==99999

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


egen ynlm3_ch=sum(ynlm3_ci) if miembros_ci==1, by(idh_ch)

label var ynlm3_ch "Ingreso No Laboral Monetario 3 del Hogar"


egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)

label var autoconsumo_ch "Autoconsumo del Hogar"


egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)

label var remesas_ch "Remesas del Hogar"


gen rama_ci=rama

replace rama_ci=. if rama==10 | emp_ci==0

*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
replace firmapeq_ci=1 if p27b>=1 & p27b<=5
replace firmapeq_ci=0 if p27b>=6 & p27b<9999

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp25_ci
*********
gen lp25_ci = 491.1154

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = 785.7846


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

gen cesante_ci=1 if p23==1 & condocup_ci==2
replace cesante_ci=0 if p23==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

gen tamemp_ci=p27b
replace tamemp_ci=. if  p27b>=9999
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
egen aux1=rowtotal(p3801um), missing


gen pension_ci =0
replace pension_ci=1 if aux1!=. & aux1!=0
drop aux1
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
egen ypen_ci=rowtotal(p3801um), missing
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
gen tc_ci=16.05


label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2001
gen salmm_ci= 	1707.60

label var salmm_ci "Salario minimo legal"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p06a==7 
replace tecnica_ci=0 if p06a!=7
label var tecnica_ci "1=formacion terciaria tecnica"



*drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 interes2 herencia2 prestlab2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 autoconsumop_ci autoconsumos_ci

*falta armar las siguientes variables:
gen ynlnm_ci =.	
gen antiguedad_ci=.
gen ocupa_ci=.
gen horastot_ci=.
gen ylmotros_ci =.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch =.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen tiempoparc_ci=.
gen raza_ci=.
gen instcot_ci=.
gen ynlnm_ch=.

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


