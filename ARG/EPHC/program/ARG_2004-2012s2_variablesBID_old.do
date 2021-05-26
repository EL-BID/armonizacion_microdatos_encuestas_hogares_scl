*Yessenia Loayza
*Junio 2013
*desloay@hotmail.com

*--------------------------------------------------------------*
*                          ARGENTINA                           *
*     PROGRAMA ELABORADO PARA CONSTRUCCION SEMESTRAL           *
*         DE LAS BASES DE DATOS DE LOS AÑOS 2004 AL 2012       *
*--------------------------------------------------------------*

clear all
set more off
local anio  2012  
local outS1 = "${surveysFolder}\Argentina\\`anio'\1erSem\Data\"
local outS2 = "${surveysFolder}\Argentina\\`anio'\2doSem\Data\"
local outS1S= "${surveysFolder}\ARM\ARG\EPHC\\`anio'_Sem1\Orig_Data\"
local outS2S= "${surveysFolder}\ARM\ARG\EPHC\\`anio'_Sem2\Orig_Data\"               

foreach x in `anio' {

 
if `x'==2007 {
local n 07
		use "${surveysFolder}\Argentina\\`x'\1erSem\Datos Originales\t1`n'.dta",clear
		append using "${surveysFolder}\Argentina\\`x'\1erSem\Datos Originales\t2`n'.dta"
		save "`outS1'arg`n'_1erSem.dta",replace
		save "`outS1S'arg`n'_1erSem.dta",replace
		
		use "${surveysFolder}\Argentina\\`x'\2doSem\Datos Originales\t4`n'.dta",clear
		append using "${surveysFolder}\Argentina\2008\1erSem\Datos Originales\t108.dta" /*4to trim 2007 se une con 1er trim 2008*/
		save "`outS2'arg`n'_2doSem.dta",replace
		save "`outS2S'arg`n'_2doSem.dta",replace
            }
	 
else {

if `x'==2004 {
local n 04
}
if `x'==2005 {
local n 05
}
if `x'==2006 {
local n 06
}

if `x'==2008 {
local n 08
}
if `x'==2009 {
local n 09
}
if `x'==2010 {
local n 10
}
if `x'==2011 {
local n 11
}
if `x'==2012 {
local n 12
}
		use "${surveysFolder}\Argentina\\`x'\1erSem\Datos Originales\t1`n'.dta",clear
		append using "${surveysFolder}\Argentina\\`x'\1erSem\Datos Originales\t2`n'.dta"
		save "`outS1'arg`n'_1erSem.dta",replace
		save "`outS1S'arg`n'_1erSem.dta",replace
		
		use "${surveysFolder}\Argentina\\`x'\2doSem\Datos Originales\t3`n'.dta",clear
		append using "${surveysFolder}\Argentina\\`x'\2doSem\Datos Originales\t4`n'.dta"
		save "`outS2'arg`n'_2doSem.dta",replace
		save "`outS2S'arg`n'_2doSem.dta",replace
     }
}























