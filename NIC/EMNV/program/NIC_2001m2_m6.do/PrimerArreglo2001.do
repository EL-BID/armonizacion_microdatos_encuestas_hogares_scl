clear
use "${surveysFolder}\ARM\NIC\2001\Orig_data\emnv19 gastos equipo.dta"
${surveysFolder}\ARM\NIC\2001\Orig_data
gen refrig_chaux=(s9pecod==4 & s9pe1==1)
gen auto_chaux=(s9pecod==22 & s9pe1==1)
gen compaux=(s9pecod==21 & s9pe1==1)

gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
by id_hogar: egen refrig_ch=max(refrig_chaux)
by id_hogar: egen auto_ch=max(auto_chaux)
by id_hogar: egen compu_ch=max(compaux)
sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
save "${surveysFolder}\ARM\NIC\2001\Van_data\Equipos01.dta", replace

clear
use "${surveysFolder}\ARM\NIC\2001\Orig_data\emnv16gastosparted1.dta"
gen alquilerc=0
replace alquilerc=s9pd12 if s9pd1cod==1  
gen alquilerv=0
replace alquilerv=s9pd12 if s9pd1cod==2  
gen becas=0
replace becas=s9pd12 if s9pd1cod==3  
gen ayudas=0
replace ayudas=s9pd12 if s9pd1cod==4  
gen pensiona=0
replace pensiona=s9pd12 if s9pd1cod==5  
gen pensionj=0
replace pensionj=s9pd12 if s9pd1cod==6  
gen pensiono=0
replace pensiono=s9pd12 if s9pd1cod==7  
drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
by id_hogar: egen alquilerc_ch=max(alquilerc)
by id_hogar: egen alquilerv_ch=max(alquilerv)
by id_hogar: egen becas_ch=max(becas)
by id_hogar: egen ayudas_ch=max(ayudas)
by id_hogar: egen pensiona_ch=max(pensiona)
by id_hogar: egen pensionj_ch=max(pensionj)
by id_hogar: egen pensiono_ch=max(pensiono)

sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
save "${surveysFolder}\ARM\NIC\2001\Van_data\ynlmmo.dta", replace

clear
use "${surveysFolder}\ARM\NIC\2001\Orig_data\emnv17gastosparted2.dta"
gen intahorros=0
replace intahorros=s9pd22 if s9pd2cod==1
gen intprest=0
replace intprest=s9pd22 if s9pd2cod==2
gen indem=0
replace indem=s9pd22 if s9pd2cod==3
gen cesant=0
replace cesant=s9pd22 if s9pd2cod==4
gen divid=0
replace divid=s9pd22 if s9pd2cod==5
gen loter=0
replace loter=s9pd22 if s9pd2cod==6
gen comp=0
replace comp=s9pd22 if s9pd2cod==7
gen donac=0
replace donac=s9pd22 if s9pd2cod==8
gen heren=0
replace heren=s9pd22 if s9pd2cod==9 
gen otrosi=0
replace otrosi=s9pd22 if s9pd2cod==10
drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
by id_hogar: egen intaho_ch=max(intahorros)
by id_hogar: egen intprest_ch=max(intprest)
by id_hogar: egen indem_ch=max(indem)
by id_hogar: egen cesant_ch=max(cesant)
by id_hogar: egen divid_ch=max(divid)
by id_hogar: egen loter_ch=max(loter)
by id_hogar: egen comp_ch=max(comp)
by id_hogar: egen donac_ch=max(donac)
by id_hogar: egen heren_ch=max(heren)
by id_hogar: egen otrosi_ch=max(otrosi)

sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
save "${surveysFolder}\ARM\NIC\2001\Van_data\ynlman.dta", replace

clear
use "${surveysFolder}\ARM\NIC\2001\Orig_data\emnv18gastosparted3.dta"
gen regalosintc=0
replace regalosintc=s9pd33a if s9pd3cod==1
gen regalosintd=0
replace regalosintd=s9pd33b if s9pd3cod==1
gen frecregint=0
replace frecregint=s9pd32 if s9pd3cod==1

gen dinerointc=0
replace dinerointc=s9pd33a if s9pd3cod==2
gen dinerointd=0
replace dinerointd=s9pd33b if s9pd3cod==2
gen frecdinint=0
replace frecdinint=s9pd32 if s9pd3cod==2

gen regalosextc=0
replace regalosextc=s9pd33a if s9pd3cod==3
gen regalosextd=0
replace regalosextd=s9pd33b if s9pd3cod==3
gen frecregext=0
replace frecregext=s9pd32 if s9pd3cod==3

gen dineroextc=0
replace dineroextc=s9pd33a if s9pd3cod==4
gen dineroextd=0
replace dineroextd=s9pd33b if s9pd3cod==4
gen frecdinext=0
replace frecdinext=s9pd32 if s9pd3cod==4
drop id_hogar
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
save "${surveysFolder}\ARM\NIC\2001\Van_data\remesas.dta", replace

clear 
use "${surveysFolder}\Nicaragua\2001\Datos originales\CONSING.dta", clear
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
save "${surveysFolder}\ARM\NIC\2001\Van_data\CONSING.dta", replace
