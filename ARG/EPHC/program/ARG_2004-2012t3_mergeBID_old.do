*Yessenia Loayza
*Junio 2013
*desloay@hotmail.com

*--------------------------------------------------------------*
*                          ARGENTINA                           *
*     PROGRAMA ELABORADO PARA UNIR LOS DIFERENTES MODULOS Y    *
* GENERAR UNA ENCUESTA POR TRIMESTRE PARA LOS AÑOS 2004 AL 2012*           *
*--------------------------------------------------------------*

clear all
set more off
local anio  2007  
local outS1 = "Y:\Argentina\\`anio'\1erSem\Datos Originales\"
local outS2 = "Y:\Argentina\\`anio'\2doSem\Datos Originales\"
               
foreach x in `anio' {
di  `anio'

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

if `x'==2007 {
local n 07
		forvalues i=4/4 {  /*no existe 3trimestre 2007, pero si 4to*/
		cd  "Y:\Argentina\\`x'\t`i'`n'_dta\Datos Originales\"
		use "Hogar_t`i'`n'.dta",clear
		keep if realizada==1
		sort CODUSU nro_hogar
		save "Hogar_t`i'`n'_modifi.dta", replace

		use "Individual_t`i'`n'.dta", clear
		sort CODUSU nro_hogar componente
		merge CODUSU nro_hogar using "Hogar_t`i'`n'_modifi.dta"
		tab _merge
		drop _merge
		sort CODUSU nro_hogar componente

		if `i'==1 | `i'==2 {
		save "`outS1't`i'`n'.dta",replace
		} 
		if `i'==4 {
		save "`outS2't`i'`n'.dta",replace
		} 
		} 
            }

else {
		forvalues i=1/4 {
		cd  "Y:\Argentina\\`x'\t`i'`n'_dta\Datos Originales\"
		use "Hogar_t`i'`n'.dta",clear
		keep if realizada==1
		sort CODUSU nro_hogar
		save "Hogar_t`i'`n'_modifi.dta", replace

		use "Individual_t`i'`n'.dta", clear
		sort CODUSU nro_hogar componente
		merge CODUSU nro_hogar using "Hogar_t`i'`n'_modifi.dta"
		tab _merge
		drop _merge
		sort CODUSU nro_hogar componente

		if `i'==1 | `i'==2 {
		save "`outS1't`i'`n'.dta",replace
		} 
		if `i'==3 | `i'==4 {
		save "`outS2't`i'`n'.dta",replace
		}
        } 
     }

}























