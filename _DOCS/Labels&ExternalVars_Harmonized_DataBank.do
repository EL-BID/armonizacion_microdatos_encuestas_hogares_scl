

* labels de variables
*====================================================================================================================================*
*                                                     INCLUSIóN DE VARIABLES EXTERNAS                                                *
*====================================================================================================================================*
capture drop _merge
merge m:1 pais_c anio_c using "$ruta\general_documentation\data_externa\poverty\International_Poverty_Lines\5_International_Poverty_Lines_LAC_long",   keepusing (ppp_2011 cpi_2011 lp19_2011 lp31_2011 lp5_2011 tc_wdi ppp_wdi2011)

drop if _merge ==2

g tc_c     = tc_wdi
g ipc_c    = cpi_2011
*g ppa_c    = ppp_wdi2011
g lp19_ci  = lp19_2011 
g lp31_ci  = lp31_2011 
g lp5_ci   = lp5_2011

drop ppp_2011 cpi_2011 lp19_2011 lp31_2011 lp5_2011 tc_wdi _merge


*====================================================================================================================================*
*                                                         VARIABLES DE IDENTIFICACION                                                *
*====================================================================================================================================*
label var region_BID_c "Regiones BID"
	label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)", add modify
	label value region_BID_c region_BID_c

label var factor_ci "Factor de expansion del individuo"
label var factor_ch "Factor de expansion del hogar"

label var idh_ch "ID del hogar"
label var idp_ci "ID de la persona en el hogar"

label var region_c "Regiones especifica de cada país"

label var zona_c "Zona del pais"
	label define zona_c 1 "urbana" 0 "rural", add modify
	label value zona_c zona_c

	
label var pais_c "Nombre del País"
label var anio_c "Anio de la encuesta" 
*label var semestre_c "Semestre de la encuesta" /* No existe en todas las encuestas*/
label var mes_c "Mes de la encuesta" 
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo" 6 "Junio" 7 "Julio" 8 "Agosto" 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add modify 
label value mes_c mes_c

*====================================================================================================================================*
*                                                          VARIABLES DEMOGRAFICAS                                                    *
*====================================================================================================================================*
label var relacion_ci "Relacion o parentesco con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Conyuge/esposo/compañero" 3 "Hijo/a" 4 "Otros_parientes" 5 "Otros_no_Parientes" 6 "Empleado/a_domestico/a", add modify 
	label values relacion_ci relacion_ci
	
label var sexo_ci "Sexo del individuo" 
	label define sexo_ci 1 "Hombre" 2 "Mujer", add modify
	label value sexo_ci sexo_ci

label var edad_ci "Edad del individuo en años"
label var civil_ci "Estado civil"
	label define civil_ci 1 "soltero/a" 2 "union_formal/informal" 3 "divorciado/a_o_separado/a" 4 "Viudo/a" , add modify
	label value civil_ci civil_ci
	
label var dis_ci "Personas con discapacidad"
	label define dis_ci 1 "Con Discapacidad" 0 "Sin Discapacidad"
	label val dis_ci dis_ci
	
label var dis_ch "Hogares con miembros con discapacidad"
	label define dis_ch 0 "Hogares sin miembros con discapacidad"1 "Hogares con al menos un miembro con discapacidad" 
	label val dis_ch dis_ch 

label var afroind_ci "Raza o etnia del individuo"
	label define afroind_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 9 "No se le pregunta"
	label val afroind_ci afroind_ci 

label var afroind_ch "Raza/etnia del hogar en base a raza/etnia del jefe de hogar"
	label define afroind_ch 1 "Hogares con Jefatura Indígena" 2 "Hogares con Jefatura Afro-descendiente" 3 "Hogares con Jefatura Otra" 9 "Hogares sin Información étnico/racial"
	label val afroind_ch afroind_ch 

label var afroind_ano_c "Año Cambio de Metodología Medición Raza/Etnicidad"
	
label var jefe_ci "Jefe/a de hogar"
label var nconyuges_ch "# de conyuges en el hogar"
label var nhijos_ch "# de hijos en el hogar"
label var notropari_ch "# de otros familiares en el hogar"	
label var notronopari_ch "# de no familiares en el hogar"
label var nempdom_ch "# de empleados domesticos"
label var clasehog_ch "Tipo de hogar"
	label define clasehog_ch 1 "unipersonal" 2 "nuclear" 3 "ampliado" 4 "compuesto" 5 "corresidente", add modify
	label value clasehog_ch clasehog_ch
	

label var nmayor21_ch "# de familiares mayores a 21 anios en el hogar"
label var nmenor21_ch "# de familiares menores a 21 anios en el hogar"
label var nmayor65_ch "# de familiares mayores a 65 anios en el hogar"
label var nmenor6_ch "# de familiares menores a 6 anios en el hogar"
label var nmenor1_ch "# de familiares menores a 1 anio en el hogar"
label var miembros_ci "=1: es miembro del hogar"
label var nmiembros_ch "# de miembros en el hogar"

*====================================================================================================================================*
*                                                          VARIABLES DEL MERCADO LABORAL                                              *
*====================================================================================================================================*
label var condocup_ci "Condicion de ocupación de acuerdo a def armonizada para cada pais"
	label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "No_responde_por_menor_edad", add modify
	label value condocup_ci condocup_ci
	
label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos", add modify
	label value categoinac_ci categoinac_ci

label var cesante_ci "Desocupado cesante-trabajo anteriormente"	

label var ocupa_ci "Ocupacion laboral en la actividad principal"  
	label define ocupa_ci 1"profesional_y_tecnico" 2"director_o_funcionario_sup" 3"administrativo_y_nivel_intermedio", add modify
	label define ocupa_ci  4 "comerciantes_y_vendedores" 5 "en_servicios" 6 "trabajadores_agricolas", add modify
	label define ocupa_ci  7 "obreros_no_agricolas,_conductores_de_maq_y_ss_de_transporte", add modify
	label define ocupa_ci  8 "FFAA" 9 "otras", add modify
	label value ocupa_ci ocupa_ci

label var emp_ci "=1: si ocupado (empleado)"

label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"

label var pea_ci "Población Económicamente Activa"

label var desalent_ci "Trabajadores desalentados: creen q no conseguiran trabajo"
label var antiguedad_ci "Antiguedad en la actividad actual"
label var formal_ci "Formalidad Laboral"


label var horaspri_ci "Horas trabajadas en la actividad principal"
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

label var subemp_ci "Personas en subempleo por horas (<30 horas y dispuestas a trabajar +)"
label var tiempoparc_ci "Personas que trabajan medio tiempo (<30 horas y NO dispuestas a trabajar +)" 
	label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4" No_remunerado" 0 "Otro" , add modify
	label value categopri_ci categopri_ci
label var categopri_ci "Categoria ocupacional en la actividad principal"
label var categosec_ci "Categoria ocupacional en la actividad secundaria"
	label define categosec_ci 1"Patron" 2"Cuenta_propia" 3"Empleado" 4" No_remunerado" 0 "Otro" , add modify
	label value categosec_ci categosec_ci
	
*label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin_contrato/verbal", add modify
label value tipocontrato_ci tipocontrato_ci

*label var segsoc_ci "Personas que tienen seguridad social en SALUD por su trabajo" - ver si se incluye
label var nempleos_ci "# de empleos" 
	capture label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
	capture label value nempleos_ci nempleos_ci
*label var firmapeq_ci "=1: Trabajadores en empresas de <5 personas ~informales" /*esta variable se reemplaza por tamemp_ci*/
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande", add modify
	label value tamemp_ci tamemp_ci
	
label var spublico_ci "=1: Personas que trabajan en el sector público"

label var rama_ci "Rama de actividad laboral de la ocupacion principal-Grandes Divisiones (ISIC Rev. 2)"
	label def rama_ci 1"Agricultura,_caza,_silvicultura_y_pesca" 2"Explotación_de_minas_y_canteras" 3"Industrias_manufactureras", add modify
	label def rama_ci 4"Electricidad,_gas_y_agua" 5"Construcción" 6"Comercio,_restaurantes_y_hoteles" 7"Transporte_y_almacenamiento", add modify
	label def rama_ci 8"Establecimientos_financieros,_seguros_e_inmuebles" 9"Servicios_sociales_y_comunales", add modify
	label val rama_ci rama_ci
	
	
label var durades_ci "Duracion del desempleo en meses"
	
	* Ingresos
	*Actividad Principal
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 
label var nrylmpri_ci "ID de no respuesta ingreso de la actividad principal"  
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"  
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal" 
	* Actividad secundaria
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"
	* Otros ingresos laborales
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos"
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 
	* Ingresos no laborales
label var autocons_ci "Autoconsumo reportado por el individuo"
label var remesas_ci "Remesas mensuales reportadas por el individuo" 
label var ylmhopri_ci "Salario monetario horario de la actividad principal" 
label var ylmho_ci "Salario monetario horario de todas las actividades" 

			* Totales individuales
label var ylnm_ci "Ingreso laboral NO monetario total individual"  	
label var ylm_ci "Ingreso laboral monetario total individual"  
label var ynlm_ci "Ingreso no laboral monetario total individual"  
label var ynlnm_ci "Ingreso no laboral no monetario total individual" 
			
			* Totales a nivel de hogar
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"
label var ylm_ch "Ingreso laboral monetario del hogar"
label var ylnm_ch "Ingreso laboral no monetario del hogar"
label var ylmnr_ch "Ingreso laboral monetario del hogar con missing en NR"
label var ynlm_ch "Ingreso no laboral monetario del hogar"
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"
label var rentaimp_ch "Rentas imputadas del hogar"

label var autocons_ch "Autoconsumo reportado por el hogar"
label var remesas_ch "Remesas mensuales del hogar"	
label var ypen_ci "Monto de ingreso por pension contributiva"
label var ypensub_ci "Monto de ingreso por pension subsidiada / no contributiva"

* LINEAS DE POBREZA y OTRAS VARIABLES EXTERNAS DE REFERENCIA
capture label var lp19_ci  "Línea de pobreza USD1.9 día en moneda local a precios corrientes a PPA 2011"
capture label var lp31_ci  "Línea de pobreza USD3.1 día en moneda local a precios corrientes a PPA 2011"
capture label var lp5_ci "Línea de pobreza USD5 por día en moneda local a precios corrientes a PPA 2011"
capture label var tc_c "Tasa de cambio LCU/USD Fuente: WB/WDI"
capture label var ipc_c "Índice de precios al consumidor base 2011=100 Fuente: IMF/WEO"
capture label var ppa_c "Factor de conversión Paridad de Poder Adquisitivo PPA LCU/USD 2011 Fuente: WB/WDI"
label var lp_ci "Linea de pobreza oficial del pais en moneda local a precios corrientes"
label var lpe_ci "Linea de indigencia oficial del pais en moneda local a precios corrientes"
label var salmm_ci "Salario minimo legal a precios corrientes"


*====================================================================================================================================*
*                                                          VARIABLES DE SEGURIDAD SOCIAL                                             *
*====================================================================================================================================*
label var cotizando_ci "Cotizante a la Seguridad Social (SS)"
	label define cotizando_ci 0"No_cotiza" 1"Cotiza_a_SS", add modify 
	label value cotizando_ci cotizando_ci
label var afiliado_ci "Afiliado a la Seguridad Social"
	label define afiliado_ci 0"No_afiliado" 1"Afiliado_a_SS", add modify 
	label value afiliado_ci afiliado_ci
	
label var tipopen_ci "Tipo de pension - variable original de cada pais" 
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
label var instcot_ci "Institucion a la cual cotiza o es afiliado - variable original de cada pais" 
label var pension_ci "=1: Recibe pension contributiva"
label var pensionsub_ci "=1: recibe pension subsidiada / no contributiva"



*====================================================================================================================================*
*                                                          VARIABLES DE EDUCACION                                             *
*====================================================================================================================================*
label var aedu_ci "Anios de educacion aprobados"
label var eduno_ci "Sin educacion"
label var edupi_ci "Primaria incompleta"
label var edupc_ci "Primaria completa"	
label var edusi_ci "Secundaria incompleta"	
label var edusc_ci "Secundaria completa"	
label var eduui_ci "Superior incompleto"	
label var eduuc_ci "Superior completo"
label var edus1i_ci "1er ciclo de la secundaria incompleto"
label var edus1c_ci "1er ciclo de la secundaria completo"
label var edus2i_ci "2do ciclo de la secundaria incompleto"
label var edus2c_ci "2do ciclo de la secundaria completo"
label var edupre_ci "Educacion preescolar"
label var eduac_ci "Superior universitario vs superior no universitario"	
label var asiste_ci "=1 si asiste actualmente a la escuela"
								
label var pqnoasis_ci "Razones para no asistir a la escuela-variable original de cada pais"
label var pqnoasis1_ci "Razones para no asistir a la escuela-variable armonizada"
label var repite_ci "Ha repetido al menos un grado"
label var repiteult_ci "Ha repetido el último grado"
label var edupub_ci "Asiste a un centro de ensenanza público"


*====================================================================================================================================*
*                                                          VARIABLES DE INFRAESTRUCTURA DEL HOGAR                                    *
*====================================================================================================================================*

label var aguared_ch "Acceso a fuente de agua por red"
label var aguadist_ch "Ubicación de la principal fuente de agua"
	label def aguadist_ch 1"Dentro_de_la_vivienda" 2"Fuera_de_la_vivienda_pero_en_el_terreno", add modify
	label def aguadist_ch 3"Fuera_de_la_vivienda_y_del_terreno", add modify
	label val aguadist_ch aguadist_ch
label var aguamala_ch "Agua unimproved según MDG" 
label var aguamide_ch "Usan medidor para pagar consumo de agua"
label var luz_ch  "La principal fuente de iluminación es electricidad"
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"
label var combust_ch "Principal combustible gas o electricidad" 
label var bano_ch "El hogar tiene servicio sanitario"
label var banoex_ch "El servicio sanitario es exclusivo del hogar"
label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No_tiene_servicio_sanitario" 1"Conectado_a_red_general_o_cámara_séptica", add modify
	label def des1_ch 2"Letrina_o_conectado_a_pozo_ciego" 3"Desemboca_en_río_o_calle", add modify
	label val des1_ch des1_ch
label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No_tiene_servicio_sanitario" 1"Conectado_a_red_general,_cámara_séptica,_pozo_o_letrina", add modify
	label def des2_ch 2"Cualquier_otro_caso", add modify
	label val des2_ch des2_ch
label var piso_ch "Materiales de construcción del piso"  
	label def piso_ch 0"Piso_de_tierra" 1"Materiales_permanentes", add modify
	label val piso_ch piso_ch
label var pared_ch "Materiales de construcción de las paredes"
label var techo_ch "Materiales de construcción del techo" 
	label def techo_ch 1"Materiales_permanentes"  0"Materiales_no_permanentes" 2 "Otros_materiales", add modify
label var techo_ch techo_ch 
label var resid_ch "Método de eliminación de residuos"
label var dorm_ch "# de habitaciones exclusivas para dormir"
label var cuartos_ch "# Habitaciones en el hogar"
label var cocina_ch "Cuarto separado y exclusivo para cocinar"
label var telef_ch "El hogar tiene servicio telefónico fijo"
label var refrig_ch "El hogar posee refrigerador o heladera"
label var freez_ch "El hogar posee freezer o congelador"
label var auto_ch "El hogar posee automovil particular"
label var compu_ch "El hogar posee computador"
label var internet_ch "El hogar posee conexión a internet"
label var cel_ch "El hogar tiene servicio telefonico celular"
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros", add modify
	label val vivi1_ch vivi1_ch
label var vivi2_ch "=1: la vivienda es casa o departamento"
		
label var viviprop_ch "Propiedad de la vivienda" 
	label def viviprop_ch 0"Alquilada" 1"Propia" 3"Ocupada_(propia_de_facto)", add modify
	label val viviprop_ch viviprop_ch
	
label var vivitit_ch "El hogar posee un título de propiedad"
label var vivialq_ch "Alquiler mensual"
label var vivialqimp_ch "Alquiler mensual imputado"
label var aguamejorada_ch "El hogar tiene acceso a agua potable de fuente mejorada"
label var banomejorado_ch "El hogar tiene acceso a saneamiento de fuente mejorada"






