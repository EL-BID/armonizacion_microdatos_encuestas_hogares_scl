			**************************************************
			********CONSTRUCCIÓN DE ALGUNOS INDICADORES******* 
			*****DE MDO LABORAL PARA VERIFICACIÓN DE BASES****
			**************************************************


*PET
gen PET=(edad_ci>=12 & CLASE==1)
replace PET=1 if edad_ci>=10 & CLASE==2

tab PET [fw=FEX1]
bysort CLASE: tab PET [fw=FEX1]

*Ocupados
gen ocupados=(edad_ci>=12 & CLASE==1 & OFICIO!=.)
replace ocupados=1 if edad_ci>=10 & CLASE==2 & OFICIO!=.
tab ocupados  [fw=FEX1]
bysort CLASE: tab ocupados if PET==1 [fw=FEX1]


*Desempleados
gen desempleados=(edad_ci>=12 & CLASE==1 & P7250!=.)
replace desempleados=1 if edad_ci>=10 & CLASE==2 & P7250!=.

*PEA
gen PEA =(ocupados==1 | desempleados==1)

