version 7.0
clear
capture log close
program drop _all
set memory 600m
set more off

local in = "X:\ARM\"
log using "Consistencia.log", replace

local	in1="`in'CRI\1990\Arm_data\CRI1990EA_BID.dta";
local	in2="`in'CRI\1991\Arm_data\CRI1991EA_BID.dta";
local	in3="`in'CRI\1992\Arm_data\CRI1992EA_BID.dta";
local	in4="`in'CRI\1993\Arm_data\CRI1993EA_BID.dta";
local	in5="`in'CRI\1994\Arm_data\CRI1994EA_BID.dta";
local	in6="`in'CRI\1996\Arm_data\CRI1996EA_BID.dta";
local	in7="`in'CRI\1997\Arm_data\CRI1997EA_BID.dta";
local	in8="`in'CRI\1998\Arm_data\CRI1998EA_BID.dta";
local	in9="`in'CRI\1999\Arm_data\CRI1999EA_BID.dta";
local	in10="`in'CRI\2000\Arm_data\CRI2000EA_BID.dta";
local	in11="`in'CRI\2003\Arm_data\CRI2003EA_BID.dta";
local	in12="`in'CRI\2005\Arm_data\CRI2005EA_BID.dta";
local	in13="`in'CRI\2006\Arm_data\CRI2006EA_BID.dta";
local	in14="`in'CRI\2001\Arm_data\CRI2001EA_BID.dta";
local	in15="`in'CRI\2004\Arm_data\CRI2004EA_BID.dta";
local	in16="`in'CRI\1995\Arm_data\CRI1995EA_BID.dta";
local	in17="`in'CRI\2007\Arm_data\CRI2007EA_BID.dta";
local	in18="`in'CRI\2008\Arm_data\CRI2008EA_BID.dta";
local	in19="`in'CRI\2009\Arm_data\CRI2009EA_BID.dta";

forvalues i=1(1)19 {
use `in`i'', clear

keep *_c *_ci *_ch
tempfile consist
save `consist', replace
if `i'~=1 {append using "Consistencia.dta"}
save "Consistencia.dta", replace


}

keep if zona_c==1


sort anio_c

cap noi table anio_c [aw=factor_ci], c(mean  zona_c) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  relacion_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  sexo_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edad_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  estcivil_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  jefe_ci) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nconyuges_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nhijos_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  notropari_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean   notronopari_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nempdom_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  clasehog_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nmiembros_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nmayor21_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nmenor21_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nmayor65_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nmenor6_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nmenor1_ch) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  miembro_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  emp_ci) format(%9.4f)
cap noi table anio_c if pea1_ci==1 & edad_ci>=10 [aw=factor_ci], c(mean  desemp1_ci) format(%9.4f)
cap noi table anio_c if pea2_ci==1 & edad_ci>=10 [aw=factor_ci], c(mean  desemp2_ci) format(%9.4f)
cap noi table anio_c if pea3_ci==1 & edad_ci>=10 [aw=factor_ci], c(mean  desemp3_ci) format(%9.4f)
cap noi table anio_c [aw=factor_ci], c(mean  pea1_ci) format(%9.4f)
cap noi table anio_c [aw=factor_ci], c(mean  pea2_ci) format(%9.4f)
cap noi table anio_c [aw=factor_ci], c(mean  pea3_ci) format(%9.4f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  desalent_ci) format(%9.4f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  horaspri_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  horastot_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  subemp_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  tiempoparc_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  categopri_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  categosec_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  contrato_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  segsoc_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  nempleos_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  firmapeq_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  spublico_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ocupa_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  rama_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  durades_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  antiguedad_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylmpri_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  nrylmpri_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylnmpri_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylmsec_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylnmsec_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylm_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylnm_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  ynlm_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  remesas_ci) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  nrylmpri_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  ylm_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  ylmnr_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  ylnm_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  remesas_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  ynlm_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  ynlnm_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  autocons_ci) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  autocons_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  rentaimp_ch) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylmhopri_ci) format(%9.2f)
cap noi table anio_c if emp_ci==1 [aw=factor_ci], c(mean  ylmho_ci) format(%9.2f)


cap noi table anio_c [aw=factor_ci], c(mean  aedu_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  eduno_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edupi_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edupc_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edusi_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edusc_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edus1i_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edus1c_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edus2i_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edus2c_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  eduui_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  eduuc_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edupre_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  eduac_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  asiste_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  repite_ci)format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  repiteult_ci) format(%9.2f)
cap noi table anio_c [aw=factor_ci], c(mean  edupub_ci) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  aguared_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  aguadist_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  aguamala_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  aguamide_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  luz_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  luzmide_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  combust_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  bano_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  banoex_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  des1_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  des2_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  piso_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  pared_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  techo_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  resid_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  dorm_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  cuartos_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  cocina_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  telef_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  refrig_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  freez_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  auto_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  compu_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  internet_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  cel_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  vivi1_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  vivi2_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  viviprop_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  vivitit_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean  vivialq_ch) format(%9.2f)
cap noi table anio_c if relacion_ci==1 [aw=factor_ci], c(mean   vivialqimp_ch) format(%9.2f)

capture log close
