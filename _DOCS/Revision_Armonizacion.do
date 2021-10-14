*** REVISIÓN DE LA BASE ARMONIZADA - EJEMPLO: EL SALVADOR
*** Editado por Marta Luzes en 16 de Julio del 2021

*------------------------------------------------------------------------------------*
*                       (1)  Sources and merges                                      *
*------------------------------------------------------------------------------------*

* (1) VARIABLES ARMONIZADAS EN ULTIMOS 4 ANOS

clear all
set more off

* Paths 
global ruta = "${surveysFolder}"
global home = "${localpath}"

local PAIS  CHL
local ENCUESTA CASEN

// change according to years 

local ANO1 "2013"  // record each year	
local ANO2 "2015"
local ANO3 "2017"
local ANO4 "2020"
global years "`ANO1' `ANO2' `ANO3' `ANO4'"
local ronda m11_m12_m1 

local in29 = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO1'`ronda'_BID.dta"
local in30 = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO2'`ronda'_BID.dta"
local in31 = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO3'`ronda'_BID.dta"
local in32 = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO4'`ronda'_BID.dta"
 
* START **

forvalues k = 29(1)32 {
tempfile temp`k'
use `in`k'', clear
keep *_c*
egen ing   = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci) if miembros_ci==1, missing
egen sing  = sum(ing), by(idh_ch)
g ipcm     = sing  / nmiembros_ch
save `temp'`k', replace
}
set more off
use `temp'29, clear
forvalues i = 30(1)32 {
append using `temp'`i', force
}
save "$home/temporal_`PAIS'", replace

* (2) REVISION DE LAS VARIABLES ARMONIZADAS

use "$home/temporal_`PAIS'", replace
collapse zona_c - ipcm, by(anio_c)
xpose, clear varname
br

* (3) REVISION DE 15 INDICADORES CLAVES CONSTRUIDAS CON LAS ARMONIZADAS

use "$home/temporal_`PAIS'", replace

* sample (hogare = # jefes)
tab  relacion_ci anio_c

* household size, schooling, formal employment, old age pension, employment rate

g ocup = condocup_ci==1

* population 
gen uno = 1 

* Labor participation
g plf1824 = (condocup_ci==1 | condocup_ci==2) if (edad_ci>=18 & edad_ci<=24)
g plf2564 = (condocup_ci==1 | condocup_ci==2) if (edad_ci>=25 & edad_ci<=64) 


* Unemployment; Not study, work or seek employment
g unem1524 = condocup_ci==2 if (condocup_ci==1 | condocup_ci==2) & (edad_ci>=15& edad_ci<=24)
g unem2564 = condocup_ci==2 if (condocup_ci==1 | condocup_ci==2) & (edad_ci>=25& edad_ci<=64) 
g ninik = (asiste_ci==0 & condocup_ci==3) if (edad_ci>=15 & edad_ci<=24) & asiste_ci~=. 


* School attendance (6-12, 13-17,18-24); Two or more year overage for grade (13-17)
gen rezago  = (edad_ci - 6 - aedu_ci)>=2 if (edad_ci>=13 & edad_ci<=17) & aedu_ci~=.

* pobre, vulnerable, clase media; ingreso per capita PPP

keep if ipcm>0 & ipcm~=. & factor_ch~=.
g ppoor31 = (ipcm < lp31_ci)*100
g ppoor50 = (ipcm < lp31_ci*1.6)*100
g vulnera = ((ipcm >= lp31_ci*1.6) & (ipcm < lp31_ci*4))*100
g middle  = ((ipcm >= lp31_ci*4)   & (ipcm < lp31_ci*20))*100


*-----------------------------------------------------------------------------*
*                       Export all results                                    *
*-----------------------------------------------------------------------------*

g female = (sexo_ci == 2)
		local 1_sexo = "masc_"
		local 2_sexo = "fem_"
	

foreach k of global years {

sum uno if anio_c ==`k' [iw=factor_ci]
scalar pob_tot_`k'  = `=r(sum_w)'

sum female if anio_c ==`k' [iw=factor_ch]
scalar mean_female_`k' = `=r(mean)'
scalar desves_female_`k' = `=r(sd)'

sum nmiembros_ch if anio_c ==`k' &  relacion_ci==1 [iw=factor_ch]
scalar mean_nmiembros_ch_`k' = `=r(mean)'
scalar desves_nmiembros_ch_`k' = `=r(sd)'

sum aedu_ci if anio_c ==`k' & (edad_ci>=18&edad_ci<=64) & aedu_ci~=. [iw=factor_ch]
scalar mean_aedu_ci_`k' = `=r(mean)'
scalar desves_aedu_ci_`k' = `=r(sd)'

sum formal_ci if anio_c ==`k'& (edad_ci>=25&edad_ci<=64) & condocup_ci==1 [iw=factor_ch]
scalar mean_formal_ci_`k' = `=r(mean)'
scalar desves_formal_ci_`k' = `=r(sd)'

sum pension_ci if anio_c ==`k' &  (edad_ci>=65&edad_ci<=130) [iw=factor_ch]
scalar mean_pension_ci_`k' = `=r(mean)'
scalar desves_pension_ci_`k' = `=r(sd)'

sum ocup if anio_c ==`k' & (edad_ci>=25&edad_ci<=64) [iw=factor_ch]
scalar mean_ocup_25_64_`k' = `=r(mean)'
scalar desves_ocup_25_64_`k' = `=r(sd)'

sum ocup if anio_c ==`k' & (edad_ci>=15&edad_ci<=24) [iw=factor_ch]
scalar mean_ocup_15_24_`k' = `=r(mean)'
scalar desves_ocup_15_24_`k' = `=r(sd)'

sum unem1524 if anio_c ==`k' [iw=factor_ch]
scalar mean_unem1524_`k' = `=r(mean)'
scalar desves_unem1524_`k' = `=r(sd)'

sum unem2564 if anio_c ==`k'  [iw=factor_ch]
scalar mean_unem2564_`k' = `=r(mean)'
scalar desves_unem2564_`k' = `=r(sd)'

sum ninik if anio_c ==`k' [iw=factor_ch]
scalar mean_ninik_`k' = `=r(mean)'
scalar desves_ninik_`k' = `=r(sd)'

sum asiste_ci if anio_c ==`k' & (edad_ci>=6  & edad_ci<=12) & asiste_ci~=. [iw=factor_ch]
scalar mean_asiste_ci_612_`k' = `=r(mean)'
scalar desves_asiste_ci_612_`k' = `=r(sd)'

sum asiste_ci if anio_c ==`k'  & (edad_ci>=13 & edad_ci<=17) & asiste_ci~=. [iw=factor_ch]
scalar mean_asiste_ci_1317_`k'  = `=r(mean)'
scalar desves_asiste_ci_1317_`k' = `=r(sd)'

sum asiste_ci if anio_c ==`k' & (edad_ci>=18 & edad_ci<=24) & asiste_ci~=. [iw=factor_ch]
scalar mean_asiste_ci_1824_`k'  = `=r(mean)'
scalar desves_asiste_ci_1824_`k' = `=r(sd)'

sum rezago if anio_c ==`k' [iw=factor_ch]
scalar mean_rezago_`k' = `=r(mean)'
scalar desves_rezago_`k' = `=r(sd)'

sum ppoor31 if anio_c ==`k' [iw=factor_ch]
scalar mean_ppoor31_`k' = `=r(mean)'
scalar desves_ppoor31_`k' = `=r(sd)'

sum ppoor50 if anio_c ==`k' [iw=factor_ch]
scalar mean_ppoor50_`k' = `=r(mean)'
scalar desves_ppoor50_`k' = `=r(sd)'

sum vulnera if anio_c ==`k' [iw=factor_ch]
scalar mean_vulnera_`k' = `=r(mean)'
scalar desves_vulnera_`k' = `=r(sd)'

sum middle if anio_c ==`k' [iw=factor_ch]
scalar mean_middle_`k' = `=r(mean)'
scalar desves_middle_`k' = `=r(sd)'

sum ipcm if anio_c ==`k' [iw=factor_ch] 
scalar mean_ipcm_`k' = `=r(mean)'  
scalar desves_ipcm_`k' = `=r(sd)'

}

*

foreach k of global years {

sum plf1824 if anio_c ==`k'  [iw=factor_ch]
scalar mean_plf1824_`k' = `=r(mean)'
scalar desves_plf1824_`k' = `=r(sd)'

sum plf2564 if anio_c ==`k'  [iw=factor_ch]
scalar mean_plf2564_`k' = `=r(mean)'
scalar desves_plf2564_`k' = `=r(sd)'

}


* generate overall avarage per indicator, accounting for years
local indicadores female nmiembros_ch aedu_ci formal_ci pension_ci ocup_25_64 ocup_15_24 ///
plf2564 plf1824 unem1524 unem2564 ninik asiste_ci_612 asiste_ci_1317 asiste_ci_1824 rezago ///
ppoor31 ppoor50 vulnera middle ipcm


foreach var of local indicadores {

gen med_`var'_`ANO1' = ( `=mean_`var'_`ANO2'' + `=mean_`var'_`ANO3'' + `=mean_`var'_`ANO4'')/3
gen med_`var'_`ANO2' = ( `=mean_`var'_`ANO1'' + `=mean_`var'_`ANO3'' + `=mean_`var'_`ANO4'')/3
gen med_`var'_`ANO3' = ( `=mean_`var'_`ANO1'' + `=mean_`var'_`ANO2'' + `=mean_`var'_`ANO4'')/3
gen med_`var'_`ANO4' = ( `=mean_`var'_`ANO1'' + `=mean_`var'_`ANO2'' + `=mean_`var'_`ANO3'')/3
}

* generate Z score

foreach var of local indicadores {
foreach k of global years  {

gen Z_`var'_`k'= (`=mean_`var'_`k''-med_`var'_`k')/`=desves_`var'_`k''
sum Z_`var'_`k'
scalar Z_`var'_`k'= `=r(mean)'

}
}

*********************************
** EXCEL
*********************************

*change path to excel masterfile (CHOOSE PATH)
putexcel set "$home/`PAIS'_`ENCUESTA'_check_`ANO1'_`ANO4'.xlsx", sheet("Total") modify

* mean

	putexcel describe
	putexcel D4 = (`=mean_female_`ANO1'') D5 = (`=mean_nmiembros_ch_`ANO1'') D6 = (`=mean_aedu_ci_`ANO1'' ) D7 = (`=mean_formal_ci_`ANO1'') D8=(`=mean_pension_ci_`ANO1'')
	putexcel E4 = (`=mean_female_`ANO2'') E5 = (`=mean_nmiembros_ch_`ANO2'') E6 = (`=mean_aedu_ci_`ANO2'' ) E7 = (`=mean_formal_ci_`ANO2'') E8=(`=mean_pension_ci_`ANO2'')
	putexcel F4 = (`=mean_female_`ANO3'') F5 = (`=mean_nmiembros_ch_`ANO3'') F6 = (`=mean_aedu_ci_`ANO3'' ) F7 = (`=mean_formal_ci_`ANO3'') F8=(`=mean_pension_ci_`ANO3'')
	putexcel G4 = (`=mean_female_`ANO4'') G5 = (`=mean_nmiembros_ch_`ANO4'') G6 = (`=mean_aedu_ci_`ANO4'' ) G7 = (`=mean_formal_ci_`ANO4'') G8=(`=mean_pension_ci_`ANO4'')

	putexcel D9 = (`=mean_ocup_25_64_`ANO1'') D10 = (`=mean_ocup_15_24_`ANO1'')
	putexcel E9 = (`=mean_ocup_25_64_`ANO2'') E10 = (`=mean_ocup_15_24_`ANO2'')
	putexcel F9 = (`=mean_ocup_25_64_`ANO3'') F10 = (`=mean_ocup_15_24_`ANO3'')	
	putexcel G9 = (`=mean_ocup_25_64_`ANO4'') G10 = (`=mean_ocup_15_24_`ANO4'')	

	putexcel D11 = (`=mean_plf2564_`ANO1'') D12 = (`=mean_plf1824_`ANO1'')
	putexcel E11 = (`=mean_plf2564_`ANO2'') E12 = (`=mean_plf1824_`ANO2'')	
	putexcel F11 = (`=mean_plf2564_`ANO3'') F12 = (`=mean_plf1824_`ANO3'')	
	putexcel G11 = (`=mean_plf2564_`ANO4'') G12 = (`=mean_plf1824_`ANO4'')	

	putexcel D13 = (`=mean_unem1524_`ANO1'') D14 = (`=mean_unem2564_`ANO1'') D15 = (`=mean_ninik_`ANO1'') 
	putexcel E13 = (`=mean_unem1524_`ANO2'') E14 = (`=mean_unem2564_`ANO2'') E15 = (`=mean_ninik_`ANO2'')
	putexcel F13 = (`=mean_unem1524_`ANO3'') F14 = (`=mean_unem2564_`ANO3'') F15 = (`=mean_ninik_`ANO3'')
	putexcel G13 = (`=mean_unem1524_`ANO4'') G14 = (`=mean_unem2564_`ANO4'') G15 = (`=mean_ninik_`ANO4'')

	putexcel D16 = (`=mean_asiste_ci_612_`ANO1'') D17 = (`=mean_asiste_ci_1317_`ANO1'') D18 = (`=mean_asiste_ci_1824_`ANO1'')  D19 = (`=mean_rezago_`ANO1'') 
	putexcel E16 = (`=mean_asiste_ci_612_`ANO2'') E17 = (`=mean_asiste_ci_1317_`ANO2'') E18 = (`=mean_asiste_ci_1824_`ANO2'')  E19 = (`=mean_rezago_`ANO2'') 
	putexcel F16 = (`=mean_asiste_ci_612_`ANO3'') F17 = (`=mean_asiste_ci_1317_`ANO3'') F18 = (`=mean_asiste_ci_1824_`ANO3'')  F19 = (`=mean_rezago_`ANO3'') 
	putexcel G16 = (`=mean_asiste_ci_612_`ANO4'') G17 = (`=mean_asiste_ci_1317_`ANO4'') G18 = (`=mean_asiste_ci_1824_`ANO4'')  G19 = (`=mean_rezago_`ANO4'') 

	putexcel D20 = (`=mean_ppoor31_`ANO1'') D21 = (`=mean_ppoor50_`ANO1'') D22 = (`=mean_vulnera_`ANO1'')  D23 = (`=mean_middle_`ANO1'') D24 = (`=mean_ipcm_`ANO1'')
	putexcel E20 = (`=mean_ppoor31_`ANO2'') E21 = (`=mean_ppoor50_`ANO2'') E22 = (`=mean_vulnera_`ANO2'')  E23 = (`=mean_middle_`ANO2'') E24 = (`=mean_ipcm_`ANO2'')
	putexcel F20 = (`=mean_ppoor31_`ANO3'') F21 = (`=mean_ppoor50_`ANO3'') F22 = (`=mean_vulnera_`ANO3'')  F23 = (`=mean_middle_`ANO3'') F24 = (`=mean_ipcm_`ANO3'')
	putexcel G20 = (`=mean_ppoor31_`ANO4'') G21 = (`=mean_ppoor50_`ANO4'') G22 = (`=mean_vulnera_`ANO4'')  G23 = (`=mean_middle_`ANO4'') G24 = (`=mean_ipcm_`ANO4'')

	putexcel D25 = (`=pob_tot_`ANO1'') 
	putexcel E25 = (`=pob_tot_`ANO2'')
	putexcel F25 = (`=pob_tot_`ANO3'') 
	putexcel G25 = (`=pob_tot_`ANO4'') 
	
*std. Dev
	
	putexcel describe
	putexcel H4 = (`=desves_female_`ANO1'') H5 = (`=desves_nmiembros_ch_`ANO1'') H6 = (`=desves_aedu_ci_`ANO1'' ) H7 = (`=desves_formal_ci_`ANO1'') H8=(`=desves_pension_ci_`ANO1'')
	putexcel I4 = (`=desves_female_`ANO2'') I5 = (`=desves_nmiembros_ch_`ANO2'') I6 = (`=desves_aedu_ci_`ANO2'' ) I7 = (`=desves_formal_ci_`ANO2'') I8=(`=desves_pension_ci_`ANO2'')
	putexcel J4 = (`=desves_female_`ANO3'') J5 = (`=desves_nmiembros_ch_`ANO3'') J6 = (`=desves_aedu_ci_`ANO3'' ) J7 = (`=desves_formal_ci_`ANO3'') J8=(`=desves_pension_ci_`ANO3'')
	putexcel K4 = (`=desves_female_`ANO4'') K5 = (`=desves_nmiembros_ch_`ANO4'') K6 = (`=desves_aedu_ci_`ANO4'' ) K7 = (`=desves_formal_ci_`ANO4'') K8=(`=desves_pension_ci_`ANO4'')

	putexcel H9 = (`=desves_ocup_25_64_`ANO1'') H10 = (`=desves_ocup_15_24_`ANO1'')
	putexcel I9 = (`=desves_ocup_25_64_`ANO2'') I10 = (`=desves_ocup_15_24_`ANO2'')
	putexcel J9 = (`=desves_ocup_25_64_`ANO3'') J10 = (`=desves_ocup_15_24_`ANO3'')	
	putexcel K9 = (`=desves_ocup_25_64_`ANO4'') K10 = (`=desves_ocup_15_24_`ANO4'')	

	putexcel H11 = (`=desves_plf2564_`ANO1'') H12 = (`=desves_plf1824_`ANO1'')
	putexcel I11 = (`=desves_plf2564_`ANO2'') I12 = (`=desves_plf1824_`ANO2'')	
	putexcel J11 = (`=desves_plf2564_`ANO3'') J12 = (`=desves_plf1824_`ANO3'')	
	putexcel K11 = (`=desves_plf2564_`ANO4'') K12 = (`=desves_plf1824_`ANO4'')	

	putexcel H13 = (`=desves_unem1524_`ANO1'') H14 = (`=desves_unem2564_`ANO1'') H15 = (`=desves_ninik_`ANO1'') 
	putexcel I13 = (`=desves_unem1524_`ANO2'') I14 = (`=desves_unem2564_`ANO2'') I15 = (`=desves_ninik_`ANO2'')
	putexcel J13 = (`=desves_unem1524_`ANO3'') J14 = (`=desves_unem2564_`ANO3'') J15 = (`=desves_ninik_`ANO3'')
	putexcel K13 = (`=desves_unem1524_`ANO4'') K14 = (`=desves_unem2564_`ANO4'') K15 = (`=desves_ninik_`ANO4'')

	putexcel H16 = (`=desves_asiste_ci_612_`ANO1'') H17 = (`=desves_asiste_ci_1317_`ANO1'') H18 = (`=desves_asiste_ci_1824_`ANO1'')  H19 = (`=desves_rezago_`ANO1'') 
	putexcel I16 = (`=desves_asiste_ci_612_`ANO2'') I17 = (`=desves_asiste_ci_1317_`ANO2'') I18 = (`=desves_asiste_ci_1824_`ANO2'')  I19 = (`=desves_rezago_`ANO2'') 
	putexcel J16 = (`=desves_asiste_ci_612_`ANO3'') J17 = (`=desves_asiste_ci_1317_`ANO3'') J18 = (`=desves_asiste_ci_1824_`ANO3'')  J19 = (`=desves_rezago_`ANO3'') 
	putexcel K16 = (`=desves_asiste_ci_612_`ANO4'') K17 = (`=desves_asiste_ci_1317_`ANO4'') K18 = (`=desves_asiste_ci_1824_`ANO4'')  K19 = (`=desves_rezago_`ANO4'') 

	putexcel H20 = (`=desves_ppoor31_`ANO1'') H21 = (`=desves_ppoor50_`ANO1'') H22 = (`=desves_vulnera_`ANO1'')  H23 = (`=desves_middle_`ANO1'') H24 = (`=desves_ipcm_`ANO1'')
	putexcel I20 = (`=desves_ppoor31_`ANO2'') I21 = (`=desves_ppoor50_`ANO2'') I22 = (`=desves_vulnera_`ANO2'')  I23 = (`=desves_middle_`ANO2'') I24 = (`=desves_ipcm_`ANO2'')
	putexcel J20 = (`=desves_ppoor31_`ANO3'') J21 = (`=desves_ppoor50_`ANO3'') J22 = (`=desves_vulnera_`ANO3'')  J23 = (`=desves_middle_`ANO3'') J24 = (`=desves_ipcm_`ANO3'')
	putexcel K20 = (`=desves_ppoor31_`ANO4'') K21 = (`=desves_ppoor50_`ANO4'') K22 = (`=desves_vulnera_`ANO4'')  K23 = (`=desves_middle_`ANO4'') K24 = (`=desves_ipcm_`ANO4'')

* Z score

	putexcel describe
	putexcel L4 = (`=Z_female_`ANO1'') L5 = (`=Z_nmiembros_ch_`ANO1'') L6 = (`=Z_aedu_ci_`ANO1'' ) L7 = (`=Z_formal_ci_`ANO1'') L8=(`=Z_pension_ci_`ANO1'')
	putexcel M4 = (`=Z_female_`ANO2'') M5 = (`=Z_nmiembros_ch_`ANO2'') M6 = (`=Z_aedu_ci_`ANO2'' ) M7 = (`=Z_formal_ci_`ANO2'') M8=(`=Z_pension_ci_`ANO2'')
	putexcel N4 = (`=Z_female_`ANO3'') N5 = (`=Z_nmiembros_ch_`ANO3'') N6 = (`=Z_aedu_ci_`ANO3'' ) N7 = (`=Z_formal_ci_`ANO3'') N8=(`=Z_pension_ci_`ANO3'')
	putexcel O4 = (`=Z_female_`ANO4'') O5 = (`=Z_nmiembros_ch_`ANO4'') O6 = (`=Z_aedu_ci_`ANO4'' ) O7 = (`=Z_formal_ci_`ANO4'') O8=(`=Z_pension_ci_`ANO4'')

	putexcel L9 = (`=Z_ocup_25_64_`ANO1'') L10 = (`=Z_ocup_15_24_`ANO1'')
	putexcel M9 = (`=Z_ocup_25_64_`ANO2'') M10 = (`=Z_ocup_15_24_`ANO2'')
	putexcel N9 = (`=Z_ocup_25_64_`ANO3'') N10 = (`=Z_ocup_15_24_`ANO3'')	
	putexcel O9 = (`=Z_ocup_25_64_`ANO4'') O10 = (`=Z_ocup_15_24_`ANO4'')	

	putexcel L11 = (`=Z_plf2564_`ANO1'') L12 = (`=Z_plf1824_`ANO1'')
	putexcel M11 = (`=Z_plf2564_`ANO2'') M12 = (`=Z_plf1824_`ANO2'')	
	putexcel N11 = (`=Z_plf2564_`ANO3'') N12 = (`=Z_plf1824_`ANO3'')	
	putexcel O11 = (`=Z_plf2564_`ANO4'') O12 = (`=Z_plf1824_`ANO4'')	

	putexcel L13 = (`=Z_unem1524_`ANO1'') L14 = (`=Z_unem2564_`ANO1'') L15 = (`=Z_ninik_`ANO1'') 
	putexcel M13 = (`=Z_unem1524_`ANO2'') M14 = (`=Z_unem2564_`ANO2'') M15 = (`=Z_ninik_`ANO2'')
	putexcel N13 = (`=Z_unem1524_`ANO3'') N14 = (`=Z_unem2564_`ANO3'') N15 = (`=Z_ninik_`ANO3'')
	putexcel O13 = (`=Z_unem1524_`ANO4'') O14 = (`=Z_unem2564_`ANO4'') O15 = (`=Z_ninik_`ANO4'')

	putexcel L16 = (`=Z_asiste_ci_612_`ANO1'') L17 = (`=Z_asiste_ci_1317_`ANO1'') L18 = (`=Z_asiste_ci_1824_`ANO1'')  L19 = (`=Z_rezago_`ANO1'') 
	putexcel M16 = (`=Z_asiste_ci_612_`ANO2'') M17 = (`=Z_asiste_ci_1317_`ANO2'') M18 = (`=Z_asiste_ci_1824_`ANO2'')  M19 = (`=Z_rezago_`ANO2'') 
	putexcel N16 = (`=Z_asiste_ci_612_`ANO3'') N17 = (`=Z_asiste_ci_1317_`ANO3'') N18 = (`=Z_asiste_ci_1824_`ANO3'')  N19 = (`=Z_rezago_`ANO3'') 
	putexcel O16 = (`=Z_asiste_ci_612_`ANO4'') O17 = (`=Z_asiste_ci_1317_`ANO4'') O18 = (`=Z_asiste_ci_1824_`ANO4'')  O19 = (`=Z_rezago_`ANO4'') 

	putexcel L20 = (`=Z_ppoor31_`ANO1'') L21 = (`=Z_ppoor50_`ANO1'') L22 = (`=Z_vulnera_`ANO1'')  L23 = (`=Z_middle_`ANO1'') L24 = (`=Z_ipcm_`ANO1'')
	putexcel M20 = (`=Z_ppoor31_`ANO2'') M21 = (`=Z_ppoor50_`ANO2'') M22 = (`=Z_vulnera_`ANO2'')  M23 = (`=Z_middle_`ANO2'') M24 = (`=Z_ipcm_`ANO2'')
	putexcel N20 = (`=Z_ppoor31_`ANO3'') N21 = (`=Z_ppoor50_`ANO3'') N22 = (`=Z_vulnera_`ANO3'')  N23 = (`=Z_middle_`ANO3'') N24 = (`=Z_ipcm_`ANO3'')
	putexcel O20 = (`=Z_ppoor31_`ANO4'') O21 = (`=Z_ppoor50_`ANO4'') O22 = (`=Z_vulnera_`ANO4'')  O23 = (`=Z_middle_`ANO4'') O24 = (`=Z_ipcm_`ANO4'')


