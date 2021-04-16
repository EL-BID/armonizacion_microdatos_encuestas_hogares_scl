clear
set more off
cd "${surveysFolder}\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig\"
infile using "input_T_MORADOR_S.do", using("T_MORADOR_S.txt")
saveold "T_MORADOR_S.dta", replace

clear
set more off
cd "${surveysFolder}\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig\"
infile using "input_T_DOMICILIO_S.do", using("T_DOMICILIO_S.txt")
saveold "T_DOMICILIO_S.dta", replace
