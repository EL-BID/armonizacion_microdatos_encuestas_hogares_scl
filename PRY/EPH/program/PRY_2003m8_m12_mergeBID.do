* MERGE PARAGUAY 2003

* Households

use r01_04.dta, clear
sort upm nvivi nhoga
save r01_04_s.dta, replace

use ingresofam.dta, clear
sort upm nvivi nhoga
save ingresofam_s.dta, replace

* Individuals

use r02_04.dta, clear
sort upm nvivi nhoga l02 
save r02_04_s.dta, replace

use r02_04_s.dta, clear

merge upm nvivi nhoga using r01_04_s.dta
tab _merge
drop _merge
sort upm nvivi nhoga l02 

merge upm nvivi nhoga using ingresofam_s.dta
tab _merge
drop _merge
sort upm nvivi nhoga l02 

save pry04.dta,replace

clear
