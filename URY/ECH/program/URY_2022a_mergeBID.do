* Autor: Daniela Zuluaga
* Fecha: Mayo de 2020

global ruta "${surveysFolder}\survey\URY\ECH\2022\a\data_orig"
global out "${surveysFolder}\survey\URY\ECH\2022\a\data_merge"
set more off


import delimited "\\sdssrv03.idb.iadb.org\surveys\survey\URY\ECH\2022\a\data_orig\ECH_2022.csv", clear
save "$out\URY_2022a.dta", replace