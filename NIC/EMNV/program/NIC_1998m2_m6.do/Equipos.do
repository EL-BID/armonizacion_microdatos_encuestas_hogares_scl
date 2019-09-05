clear
use "\\\resb636-a\database\ARM\NIC\1998\Orig_data\equiponv.dta"
gen refrig_chaux=(codibien==4 & ge1==1)
gen auto_chaux=(codibien==22 & ge1==1)
gen compaux=(codibien==21 & ge1==1)

****************************************************************************************************
/*CREAMOS EL ID_HOGAR DE LA MISMA FORMA QUE ESTA CREADO EN LAS BANANAS DEL '98 (Solo para equipos)*/
sort i00

gen first=0
quietly by i00: replace first=1 if _n==1

gen vectoru=1
gen double idy=sum(vectoru) if first==1
recode idy .=0
egen double id_hogar=sum(idy), by (i00)

drop first vectoru idy
sort id_hogar
*****************************************************************************************************

by id_hogar: egen refrig_ch=max(refrig_chaux)
by id_hogar: egen auto_ch=max(auto_chaux)
by id_hogar: egen compu_ch=max(compaux)
sort id_hogar
by id_hogar:gen contador=(_n==1)
keep if contador==1
drop contador
save "\\\resb636-a\database\ARM\NIC\1998\Van_data\Equipos98.dta", replace
