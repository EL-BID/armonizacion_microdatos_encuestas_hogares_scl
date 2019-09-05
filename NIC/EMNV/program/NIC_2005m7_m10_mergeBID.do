clear all
use "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\emnv05_15_parte_c1_de_la_seccion_7.dta"
/*
s7c1cod:
           1 alquiler casa/apartamento/terreno
           2 alquiler vehículos/maquinaria
           3 becas para estudios
           4 ayudas en dinero
           5 pensión alimenticia
           6 pensión por jubilación
           7 pensión por orfandad/viudez
		   
*/

*Córdobas
		   
gen alquilercc=0
replace alquilercc= s7p26a if s7c1cod==1  
gen alquilervc=0
replace alquilervc= s7p26a if s7c1cod==2  
gen becasc=0
replace becasc= s7p26a if s7c1cod==3  
gen ayudasc=0
replace ayudasc= s7p26a if s7c1cod==4  
gen pensionac=0
replace pensionac= s7p26a if s7c1cod==5  
gen pensionjc=0
replace pensionjc= s7p26a if s7c1cod==6  
gen pensionoc=0
replace pensionoc= s7p26a if s7c1cod==7  


*Dólares
gen alquilercd=0
replace alquilercd= s7p26a if s7c1cod==1  
gen alquilervd=0
replace alquilervd= s7p26a if s7c1cod==2  
gen becasd=0
replace becasd= s7p26a if s7c1cod==3  
gen ayudasd=0
replace ayudasd= s7p26a if s7c1cod==4  
gen pensionad=0
replace pensionad= s7p26a if s7c1cod==5  
gen pensionjd=0
replace pensionjd= s7p26a if s7c1cod==6  
gen pensionod=0
replace pensionod= s7p26a if s7c1cod==7  

capture drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador

*Córdobas
by id_hogar: egen alquilercc_ch=max(alquilercc)
by id_hogar: egen alquilervc_ch=max(alquilervc)
by id_hogar: egen becasc_ch=max(becasc)
by id_hogar: egen ayudasc_ch=max(ayudasc)
by id_hogar: egen pensionac_ch=max(pensionac)
by id_hogar: egen pensionjc_ch=max(pensionjc)
by id_hogar: egen pensionoc_ch=max(pensionoc)

*Dólares
by id_hogar: egen alquilercd_ch=max(alquilercd)
by id_hogar: egen alquilervd_ch=max(alquilervd)
by id_hogar: egen becasd_ch=max(becasd)
by id_hogar: egen ayudasd_ch=max(ayudasd)
by id_hogar: egen pensionad_ch=max(pensionad)
by id_hogar: egen pensionjd_ch=max(pensionjd)
by id_hogar: egen pensionod_ch=max(pensionod)


saveold  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c15_seccion7.dta", replace


clear
use "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\emnv05_16_parte_c2_de_la_seccion_7.dta"
/*

s7c2cod:
           1 intereses recibidos por ahorro
           2 intereses recibidos por prstamos
           3 indemnización de seguros
           4 cesantía e indemnización de trabajo
           5 dividendos de acciones
           6 lotería y juegos de azar
           7 compensación por accidentes de trabajo
           8 donación de instituciones
           9 herencias
          10 otros ingresos
*/
		  
*Córdobas
gen intahorrosc=0
replace intahorrosc=s7p28a if s7c2cod==1
gen intprestc=0
replace intprestc=s7p28a if s7c2cod==2
gen indemc=0
replace indemc=s7p28a if s7c2cod==3
gen cesantc=0
replace cesantc=s7p28a if s7c2cod==4
gen dividc=0
replace dividc=s7p28a if s7c2cod==5
gen loterc=0
replace loterc=s7p28a if s7c2cod==6
gen compc=0
replace compc=s7p28a if s7c2cod==7
gen donacc=0
replace donacc=s7p28a if s7c2cod==8
gen herenc=0
replace herenc=s7p28a if s7c2cod==9 
gen otrosic=0
replace otrosic=s7p28a if s7c2cod==10

*Dólares
gen intahorrosd=0
replace intahorrosd=s7p28b if s7c2cod==1
gen intprestd=0
replace intprestd=s7p28b if s7c2cod==2
gen indemd=0
replace indemd=s7p28b if s7c2cod==3
gen cesantd=0
replace cesantd=s7p28b if s7c2cod==4
gen dividd=0
replace dividd=s7p28b if s7c2cod==5
gen loterd=0
replace loterd=s7p28b if s7c2cod==6
gen compd=0
replace compd=s7p28b if s7c2cod==7
gen donacd=0
replace donacd=s7p28b if s7c2cod==8
gen herend=0
replace herend=s7p28b if s7c2cod==9 
gen otrosid=0
replace otrosid=s7p28b if s7c2cod==10


*********************************

capture drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
*Córdobas
by id_hogar: egen intahoc_ch=max(intahorrosc)
by id_hogar: egen intprestc_ch=max(intprestc)
by id_hogar: egen indemc_ch=max(indemc)
by id_hogar: egen cesantc_ch=max(cesantc)
by id_hogar: egen dividc_ch=max(dividc)
by id_hogar: egen loterc_ch=max(loterc)
by id_hogar: egen compc_ch=max(compc)
by id_hogar: egen donacc_ch=max(donacc)
by id_hogar: egen herenc_ch=max(herenc)
by id_hogar: egen otrosic_ch=max(otrosic)

*Dólares
by id_hogar: egen intahod_ch=max(intahorrosd)
by id_hogar: egen intprestd_ch=max(intprestd)
by id_hogar: egen indemd_ch=max(indemd)
by id_hogar: egen cesantd_ch=max(cesantd)
by id_hogar: egen dividd_ch=max(dividd)
by id_hogar: egen loterd_ch=max(loterd)
by id_hogar: egen compd_ch=max(compd)
by id_hogar: egen donacd_ch=max(donacd)
by id_hogar: egen herend_ch=max(herend)
by id_hogar: egen otrosid_ch=max(otrosid)

sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
saveold  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c16_seccion7.dta", replace


use "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\emnv05_17_parte_c3_de_la_seccion_7.dta"
/*
 label list s7p31
s7p31:
           1 semanal
           2 quincena
           3 mes
           4 trimestre
           5 semestre
           6 a¤o
           8 n/s
           9 ignorado
*/


/*
1 regalos o bienes de familiares o amigos del interior
           2 dinero de familiares o amigos del interior
           3 regalos o bienes de familiares o amigos del exterior
           4 dinero de familiares o amigos del exterior
		   
		   s7c3cod:
           1 regalo/bienes del interior
           2 dinero del interior
           3 regalo/bienes del exterior
           4 dinero del exterior

*/

gen regalosintc=0
replace regalosintc= s7p32a if  s7c3cod==1
gen regalosintd=0
replace regalosintd= s7p32b if  s7c3cod==1
gen frecregint=0
replace frecregint=s7p31 if  s7c3cod==1

gen dinerointc=0
replace dinerointc= s7p32a if  s7c3cod==2
gen dinerointd=0
replace dinerointd= s7p32b if  s7c3cod==2
gen frecdinint=0
replace frecdinint=s7p31 if  s7c3cod==2

gen regalosextc=0
replace regalosextc= s7p32a if  s7c3cod==3
gen regalosextd=0
replace regalosextd= s7p32b if  s7c3cod==3
gen frecregext=0
replace frecregext=s7p31 if  s7c3cod==3

gen dineroextc=0
replace dineroextc= s7p32a if  s7c3cod==4
gen dineroextd=0
replace dineroextd= s7p32b if  s7c3cod==4
gen frecdinext=0
replace frecdinext=s7p31 if  s7c3cod==4
capture drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
by id_hogar: egen regintc_ch=max(regalosintc)
by id_hogar: egen regintd_ch=max(regalosintd)
by id_hogar: egen frecregint_ch=max(frecregint)
by id_hogar: egen dinintc_ch=max(dinerointc)
by id_hogar: egen dinintd_ch=max(dinerointd)
by id_hogar: egen frecdiin_ch=max(frecdinint)
by id_hogar: egen regexc_ch=max(regalosextc)
by id_hogar: egen regexd_ch=max(regalosextd)
by id_hogar: egen frecregex_ch=max(frecregext)
by id_hogar: egen dinextc_ch=max(dineroextc)
by id_hogar: egen dinextd_ch=max(dineroextd)
by id_hogar: egen frecdiex_ch=max(frecdinext)
sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador

saveold  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c17_seccion7.dta", replace

clear

use "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\emnv05_18_parte_d_de_la_seccion_7.dta", replace
gen refrig_chaux=(s7dcod==4 & s7p35==1)
gen auto_chaux=(s7dcod==22 & s7p35==1)
gen compaux=(s7dcod==21 & s7p35==1)

gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
by id_hogar: egen refrig_ch=max(refrig_chaux)
by id_hogar: egen auto_ch=max(auto_chaux)
by id_hogar: egen compu_ch=max(compaux)
sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
saveold  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c18_seccion7.dta", replace

clear
use "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\emnv05_19_parte_d_de_la_seccion_7_equipos_adicionales.dta"
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
saveold  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c19_seccion7.dta", replace

clear
use "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\caratula.dta"
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
saveold  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\caratula_id", replace

clear
use  "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Nic05_2.dta"
capture drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
saveold "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\NIC_2005m7_m10.dta", replace
clear
local in1 "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\NIC_2005m7_m10.dta"
local in3 "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c15_seccion7.dta"
local in4 "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c16_seccion7.dta"
local in5 "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c17_seccion7.dta"
local in6 "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c18_seccion7.dta"
local in7 "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\c19_seccion7.dta"

use `in1'
capture drop _merge
forvalues h=3(1)7 {
merge m:m id_hogar using `in`h''
tab _merge
capture drop _merge
sort id_hogar
}

saveold "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\NIC_2005m7_m10.dta", replace
merge m:m id_hogar using "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\Stata\caratula_id"
*La mayor parte son hogares que no existen.
drop if _merge ==2
drop _merge
saveold "Y:\survey\NIC\EMNV\2005\m7_m10\data_orig\NIC_2005m7_m10.dta", replace
