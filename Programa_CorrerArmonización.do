/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
						Programa para correr Do de armonización

Versión 2012:
Última versión:  05/20/2021

							SCL - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

1. Para correr la actualización de las bases solo es necesario modificar los global de acuerdo a las bases, anhos y rondas necesarias para actualizar
2. Recuerde definir los global de las carpetas de las bases ${surveysFolder} y de los Do-file $gitFolder en su computador con la ruta local. 

****************************************************************************/
*1. Merge de las bases orginales 

global PAIS COL
global ENCUESTA GEIH
global ANO "2014 2015 2016 2017 2018"
global ronda  "t3"

				foreach pais of global PAIS {
					foreach enc of global ENCUESTA {
						foreach a of global ANO {
							foreach r of global ronda {
					
					do "$gitFolder/armonizacion_microdatos_encuestas_hogares_scl/`pais'/`enc'/program/`pais'_`a'`r'_mergeBID.do"
			
							}
						}
					}
				}

*2. Armonización de Variables
global PAIS COL
global ENCUESTA GEIH
global ANO "2019"
global ronda  "t3"

				foreach pais of global PAIS {
					foreach enc of global ENCUESTA {
						foreach a of global ANO {
							foreach r of global ronda {
					
					do "$gitFolder/armonizacion_microdatos_encuestas_hogares_scl/`pais'/`enc'/program/`pais'_`a'`r'_variablesBID.do"
			
							}
						}
					}
				}


				
*3.UNICODE

clear all
cd "$gitFolder/armonizacion_microdatos_encuestas_hogares_scl\URY\ECH\program\" 

unicode analyze  "URY_2011a_variablesBID.do" 
unicode encoding set "latin1"
unicode translate  "URY_2011a_variablesBID.do" 

*************
global PAIS COL
global ENCUESTA GEIH
*global ANO "2015"
global ronda  "a"


				foreach pais of global PAIS {
					foreach enc of global ENCUESTA {
							foreach r of global ronda {
								forvalues x=18(1)19 {
					use "${surveysFolder}/survey/`pais'/`enc'/20`x'/`r'/data_orig/anual_homologado_DANE/Hogares.dta", clear
					rename DIRECTORIO directorio
					rename SECUENCIA_P secuencia_p
					*rename ORDEN orden
					save, replace
								}
							}
					}
				}
