*********************************************
****** Merge modulos de Vivienda y Hogar ****
******       Diciembre 2003-2011         ****
*********************************************

*Elaborado por: Yessenia Loayza
*Date: July, 2012 / cambio rutas en octubre 2013

clear all
set more off


local in = "\\Sdssrv03\surveys\survey\ECU\ENEMDU\"

local in2003 = "`in'\2003\m12\data_orig\"
local in2004 = "`in'\2004\m12\data_orig\"
local in2005 = "`in'2005\m12\data_orig\"
local in2006 = "`in'2006\m12\data_orig\"
local in2007 = "`in'2007\m12\data_orig\"
local in2008 = "`in'2008\m12\data_orig\"
local in2009 = "`in'2009\m12\data_orig\"
local in2010 = "`in'2010\m12\data_orig\"
local in2011 = "`in'2011\m12\data_orig\"
local in2012 = "`in'2012\m12\data_orig\"
local in2013 = "`in'2013\m12\data_orig\"

forvalues i= 2013(1)2013 {

use "`in`i''viv12_`i'.dta", clear
cap destring area ciudad zona sector panelm vivienda hogar, replace
sort area ciudad zona sector panelm vivienda hogar
save, replace

use "`in`i''per12_`i'.dta", clear
cap destring area ciudad zona sector panelm vivienda hogar, replace
sort area ciudad zona sector panelm vivienda hogar
merge area ciudad zona sector panelm vivienda hogar  using "`in`i''viv12_`i'.dta"
tab _merge
drop _merge
cap drop idhogar

saveold "\\Sdssrv03\surveys\survey\ECU\ENEMDU\\`i'\m12\data_merge\ECU_`i'm12.dta", replace
}


