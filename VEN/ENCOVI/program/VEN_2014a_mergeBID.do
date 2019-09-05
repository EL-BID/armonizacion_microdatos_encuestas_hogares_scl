* (Versi�n Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys"

local PAIS VEN
local ENCUESTA ENCOVI
local ANO "2014"
local ronda a 

local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: 
Encuesta: ENCOVI
Round: a
Autores: Mayra S�enz - saenzmayra.a@gmail.com - mayras@iadb.org 
Fecha �ltima modificaci�n: Marzo 2017

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use "$ruta\harmonized\VEN\ENCOVI\program\ENCOVI_14_personas.dta", clear

