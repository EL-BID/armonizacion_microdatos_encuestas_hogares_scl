** Para solucionar el problema de subtipo*****
***No lo he podido solucionar*****************

*1. Urbano 1992

/*infix  s67 67 s68 68 s69 69 s71 71 s72 72 s73 73 using"${surveysFolder}\ARM\COL\ENH\1992\Orig_data\urbn.p99"
tab1 s67- s73

-> tabulation of s67  

        s67 |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |        343       14.98       14.98
          1 |        188        8.21       23.19
          2 |        312       13.62       36.81
          3 |        608       26.55       63.36
          4 |        128        5.59       68.95
          5 |        470       20.52       89.48
          6 |        137        5.98       95.46
          7 |        103        4.50       99.96
          9 |          1        0.04      100.00
------------+-----------------------------------
      Total |      2,290      100.00    
 -> tabulation of s68  

        s68 |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |      6,223       82.63       82.63
          1 |        826       10.97       93.60
          2 |        283        3.76       97.36
          3 |         64        0.85       98.21
          4 |         11        0.15       98.35
          5 |         45        0.60       98.95
          6 |         55        0.73       99.68
          7 |         23        0.31       99.99
          9 |          1        0.01      100.00
------------+-----------------------------------
      Total |      7,531      100.00

-> tabulation of s69  
no observations

-> tabulation of s71  
no observations

-> tabulation of s72  
no observations
*/

infile using "${surveysFolder}\ARM\COL\ENH\1992\Van_data\diccionario_temp.dct", using("${surveysFolder}\ARM\COL\ENH\1992\Orig_data\urbn.p99")
keep if registro=="2"
gen uno=1
bysort ident ide009 : gen dos=sum(uno)
gen  cuantas=wordcount(prueba)
destring prueba, generate(prueba_num) force
gen  cuantas1=wordcount(prueba1)
destring prueba1, generate(prueba1_num) force


gen educa=0
replace educa=1 if sena=="1"|sena=="2"
replace educa=1 if  horas!=""
replace educa=1 if  porq!=""
replace educa=1 if cuantas==1 & prueba_num>100000 & prueba_num!=.
/*
replace educa=1 if cuantas1==1 & prueba1_num>=10000 & prueba1_num!=.
