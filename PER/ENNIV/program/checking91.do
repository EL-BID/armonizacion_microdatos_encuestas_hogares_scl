**** PROGRAMA DE CHEQUEO DE PERU 1994 ****

capture log close
clear
set mem 150m
set more off

local in="D:\Data.idb\Eugenia\Peru\ENNIV\"

log using "`in'checking91.log", replace

use "`in'per1991_enniv_idb.dta"


sum idp_ci if parentco_ci==1 [w=factor_ci]
sum idp_ci [w=factor_ci]

tab zona, m
sum zona
tab pais, m /*Verificar que el nombre del país esté con Mayúsculas*/
tab anio, m /*Verificar que sea un número de 4 cifras*/
tab mes, m

tab parentco_ci, m /*El número de jefes tiene que ser igual al número de hogares*/

tab sexo_ci, m
sum sexo_ci

sum edad_ci

tab estcivil_ci, m 
/*Verficar a quienes se le hace esta pregunta, puede ser que a los menores se 
los excluya del grupo cuestionado*/
sum estcivil_ci

tab jefe_ci, m 
sum jefe_ci

tab miembro_ci, m

tab nconyuges_ch if parentco_ci==1, m  /*Tiene que dar igual al número de obs. con parentco_ci==2*/


tab pea1_ci, m
tab emp_ci, m
tab desemp1_ci, m

tab emp_ci if pea1_ci==1, m
tab desemp1_ci if pea1_ci==1, m

tab pea2_ci, m
tab desemp2_ci, m
tab desemp2_ci if pea2_ci==1, m

tab pea3_ci, m 
tab desemp3_ci, m
tab desemp3_ci if pea3_ci==1, m

tab categopri_ci, m
tab categopri_ci if emp_ci==1, m /*Catego deber ser igual a emp_ci==1*/

tab categosec_ci, m
tab categosec_ci if emp_ci==1, m /*Catego deber ser igual a emp_ci==1*/
tab categosec_ci if emp_ci==1 & nempleos_ci==2

tab ocupa_ci if emp_ci==1, m
tab rama_ci if emp_ci==1, m

sum horaspri_ci
sum horaspri_ci if emp_ci==1
sum horastot_ci
sum horastot_ci if emp_ci==1
compare horaspri_ci horastot_ci

/*Ingreso laboral monetario de la actividad principal*/

sum ylmpri_ci
sum ylmpri_ci if emp_ci==1
by categopri_ci, sort: sum ylmpri_ci

count if ylmpri_ci==. & emp_ci==1
tab nrylmpri_ci if emp_ci==1

/*Fijarse que los familiares no remunerados (categopri_ci==4) tengan yprijb=0*/

/*Ingreso laboral no monetario de la actividad principal*/

sum ylnmpri_ci
sum ylnmpri_ci if emp_ci==1
by categopri_ci, sort: sum ylnmpri_ci


/*Ingreso laboral monetario de la actividad secundaria*/

sum ylmsec_ci
sum ylmsec_ci if emp_ci==1
by categosec_ci, sort: sum ylmsec_ci

count if ylmsec_ci==. & emp_ci==1
/*yprijb por lo general es mayor que ysecjb*/

/*Ingreso laboral total*/

sum ylm_ci
sum ylm_ci if emp_ci==1
by categopri_ci, sort: sum ylm_ci


sum ylnm_ci
sum ylnm_ci if emp_ci==1
by categopri_ci, sort: sum ylnm_ci


sum ynlm_ci
sum ynlm_ci if emp_ci==1
by categopri_ci, sort: sum ynlm_ci


************************
*** HOUSEHOLD INCOME ***
************************
by idh_ch, sort: egen nrylmpri2_ch=sum(nrylmpri_ci) if miembro_ci==1

*flag del hogar

tab nrylmpri_ch if parentco_ci==1
tab nrylmpri2_ch if parentco_ci==1



sum ylm_ch if miembro_ci==1 & parentco_ci==1
sum ylmnr_ch if miembro_ci==1 & parentco_ci==1


sum ylm_ch if miembro_ci==0
sum ylmnr_ch if miembro_ci==0


compare ylm_ch ylmnr_ch if parentco_ci==1

**************************
***ylmho_ci & ylm1ho_ci***
**************************

gen wageho_ci=ylmpri_ci/(horaspri_ci*4.3)

compare ylmhopri_ci wageho_ci

gen wagesho_ci=ylm_ci/(horastot_ci*4.3)

compare ylmho_ci wagesho_ci


sum durades_ci
sum antiguedad_ci

tab desalent_ci, m
tab desalent_ci if pea1_ci==1 /*No deben ser parte de la fuerza laboral*/

tab subemp_ci, m
tab subemp_ci if horastot_ci<=30 /*Debe coincidir con subemp_ci*/
tab subemp_ci if horaspri_ci<=30

tab tiempoparc_ci, m
tab tiempoparc_ci if horastot_ci<=30 /*Debe coincidir con subemp_ci*/
tab tiempoparc_ci if horaspri_ci<=30


tab contrato_ci, m
tab contrato_ci if emp_ci==1, m 
by categopri_ci, sort: tab contrato_ci if emp_ci==1, m 


tab segsoc_ci, m
tab segsoc_ci if emp_ci==1, m
by categopri_ci, sort: tab segsoc_ci if emp_ci==1, m

tab nempleos_ci, m
tab nempleos_ci if emp_ci==1, m


tab firmapeq_ci, m 
tab firmapeq_ci if emp_ci==1, m
by categopri_ci, sort: tab firmapeq_ci if emp_ci==1, m

tab spublico_ci, m
tab spublico_ci if emp_ci==1, m
by categopri_ci, sort: tab spublico_ci if emp_ci==1, m

sum aedu_ci
compare aedu_ci edad_ci if aedu_ci>0 /*Por lo general la edad_ci debe ser mayor al número de anos de estudio*/

tab eduno_ci, m
tab eduno_ci if aedu_ci==0, m

tab edupi_ci, m
tab edupc_ci, m
tab edusi_ci, m
tab edusc_ci, m
tab eduui_ci, m
tab eduuc_ci, m

egen xx=rsum(eduno edupi edupc edusi edusc eduui eduuc)
tab xx, m

tab edus1i_ci, m
tab edus1c_ci, m
tab edus2i_ci, m
tab edus2c_ci, m

compare edus2c_ci edusc_ci

tab edupre_ci, m
tab eduac_ci, m


tab asiste_ci, m
tab pqnoasis_ci, m
by asiste_ci, sort: tab pqnoasis_ci, m

tab edupub_ci, m

capture log close







