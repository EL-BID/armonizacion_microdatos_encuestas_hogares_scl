* (Versi�n Stata 12)
clear
set more off
*Mayra S�enz
*Marzo 2015
*mayras@iadb.org

*Este do file est� en proceso de elaboraci�n.


*______________________________*
*       Mortalidad Infantil
*______________________________*

*Las restricciones de edad aplico cuando ya est�n unificadas todas las bases.
	*drop if edad_ci >=40
	*keep if (edad_ci >=25 & edad_ci <=39)
	drop if edadm_ni <12
	drop if yrsbefint >10

	g mortality = edadalmorm_ni <12
	g mortalitynnm = (edadalmord_ni >=100 & edadalmord_ni <=128)
	g mortalitypnm = (edadalmord_ni >128 & edadalmord_ni <=211)

	tabstat mortality*, by(anacim_ni) 
	*replace nvivo_ni =1 if nvivo_ni ==0 & mortality==0
