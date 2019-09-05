clear
set more off
cd "Y:\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig\"
infile using "input_T_MORADOR_S.do", using("T_MORADOR_S.txt")
saveold "T_MORADOR_S.dta", replace

clear
set more off
cd "Y:\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig\"
infile using "input_T_DOMICILIO_S.do", using("T_DOMICILIO_S.txt")
saveold "T_DOMICILIO_S.dta", replace


clear
set more off
cd "Y:\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig"
infile using "input_T_RENDIMENTOS_S.do", using("T_RENDIMENTOS_S.txt")
saveold "T_RENDIMENTOS_S.dta", replace


clear
set more off
cd "Y:\SALUD\bases\Encuestas\BRA\POF\2008_2009\data_orig"
infile using "input_T_OUTROS_RECI_S.do", using("T_OUTROS_RECI_S.txt")
saveold "T_OUTROS_RECI_S.dta", replace
** BOLSA FAMILICIA CÓDIGO: 01001
** BENEFICIO DE PRESTACAO CONTINUADA (BPC) CÓDIGO: 01101 
