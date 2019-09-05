clear
set mem 700m
use "X:\ARM\URU\ECH\1999\Orig_data\h99s1mon.dta"
append using "X:\ARM\URU\ECH\1999\Orig_data\h99s2mon.dta"
append using "X:\ARM\URU\ECH\1999\Orig_data\h99s1int.dta"
append using "X:\ARM\URU\ECH\1999\Orig_data\h99s2int.dta"
compress
sort ident
save "X:\ARM\URU\ECH\1999\Van_data\uryhog99.dta", replace
clear

use "X:\ARM\URU\ECH\1999\Orig_data\p99s1mon.dta"
append using "X:\ARM\URU\ECH\1999\Orig_data\p99s2mon.dta"
append using "X:\ARM\URU\ECH\1999\Orig_data\p99s1int.dta"
append using "X:\ARM\URU\ECH\1999\Orig_data\p99s2int.dta"
compress
rename correlativ ident
sort ident
save "X:\ARM\URU\ECH\1999\Van_data\uryper99.dta", replace

merge ident using "X:\ARM\URU\ECH\1999\Van_data\uryhog99.dta"
save "X:\ARM\URU\ECH\1999\Van_data\ury99.dta", replace


