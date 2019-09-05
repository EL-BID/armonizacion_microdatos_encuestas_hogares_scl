cd "Y:\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig\"

use "T_MORADOR_S.dta", clear
sort cod_uf num_seq cod_domc num_informante
saveold "T_MORADOR_S.dta", replace

use "T_DOMICILIO_S.dta", clear
duplicates report cod_uf num_seq cod_domc
sort cod_uf num_seq cod_domc 
saveold "T_DOMICILIO_S.dta", replace

use "T_RENDIMENTOS_S.dta", clear
rename num_ordem_inform num_informante
duplicates report cod_uf num_seq cod_domc num_informante
sort cod_uf num_seq cod_domc num_informante
saveold "T_RENDIMENTOS_S.dta", replace

use "T_OUTROS_RECI_S.dta", clear
rename num_ord_inform num_informante
duplicates report cod_uf num_seq cod_domc num_informante
sort cod_uf num_seq cod_domc num_informante
saveold "T_OUTROS_RECI_S.dta", replace


use "T_MORADOR_S.dta", clear
merge m:1 cod_uf num_seq cod_domc using "T_DOMICILIO_S.dta"
drop _merge

merge m:1 cod_uf num_seq cod_domc using "T_RENDIMENTOS_S.dta"
drop _merge

merge m:1 cod_uf num_seq cod_domc using "T_OUTROS_RECI_S.dta"
drop _merge

