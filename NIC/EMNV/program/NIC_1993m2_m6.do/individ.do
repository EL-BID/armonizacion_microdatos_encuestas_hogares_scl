clear
set mem 400m 
infile using "\\\resb636-a\database\ARM\NIC\1993\Programs\Van_prog\individ.dct"

compress

save "\\\resb636-a\database\ARM\NIC\1993\Van_data\pemnv93.dta", replace

