* (Versi�n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


global ruta = "${surveysFolder}"

local PAIS DOM
local ENCUESTA ENCFT
local ANO "2017"
local ronda t4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   

*capture log close
*log using "`log_file'", replace 


/*************************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Republica Dominicana
Encuesta: ENCFT
Round: m10
Autores: Mayra Saenz mayras@iadb.org
Fecha última modificación: 12 de Junio 2018, by Alvaro Altamirano/ alvaroalt@iadb.org

							SCL/LMK - IADB
**************************************************************************************/

*Conversi�n de bases de formato excel a stata
/*
clear all
global modulos "Vivienda Hogar Miembros Ocupacion Ingresos_Exterior Remesas Calculadas"
foreach mod of global modulos {
import excel "`base_in'\Base ENFT Octubre 2016.xls", sheet("`mod'") firstrow case(lower) clear
saveold "`base_in'\`mod'", replace
clear
}
*/

*Consolidando la informaci�n*
*****************************
use "`base_in'\miembros.dta", clear

merge m:m vivienda using "`base_in'\vivienda.dta", force
tab _merge
drop _merge
sort vivienda hogar

merge m:m vivienda hogar using "`base_in'\hogares.dta", force
tab _merge
drop _merge

* Comprime y guarda base
compress


saveold "`base_out'", version(12) replace


/* Update 2018, no se usan labels porque cambió la base, ahora el gobierno solo publicará la ocntinua (ENCFT) y no más la ENFT
*Coloco las etiquetas
label var	cesante	"cesante"
label var	desocupado	"desocupado "
label var	eft_abanico_elect	"abanico_elect"
label var	eft_aceptaria_trab_sem_ant	"aceptaria un trabajo la semana anterior"
label var	eft_afiliado_afp	"esta afiliado a una afp"
label var	eft_afiliado_seguro_salud	"esta afiliado a seguro de saludo"
label var	eft_afiliado_seguro_vida	"esta afiliado a seguro de vida"
label var	eft_agua_red_publica	"instalacion agua red publica"
label var	eft_aire_acondicionado	"aire_acondicionado"
label var	eft_alfabetismo	"sabe leer y escribir"
label var	eft_alquiler_ing_ext	"recibio alquiler del exterior"
label var	eft_alquiler_ing_nac	"recibio ingresos por alquiler nacional"
label var	eft_alumbrado_publico	"las calles poseen alumbrado publico"
label var	eft_anio_pasado_alquiler	"recibio alquiler a񯠰asado"
label var	eft_anio_pasado_gobierno	"recibio ayuda del gobierno a񯠰asado"
label var	eft_anio_pasado_interes	"recibio interes a񯠰asado"
label var	eft_anio_pasado_monto_alquiler	"monto alquiler a񯠰asado"
label var	eft_anio_pasado_monto_gobierno	"monto ayuda gobierno a񯠰asado"
label var	eft_anio_pasado_monto_interes	"monto interes a񯠰asado"
label var	eft_anio_pasado_monto_ocasion	"monto ocasional a񯠰asado"
label var	eft_anio_pasado_monto_otros	"monto otros ingresos a񯠰asado"
label var	eft_anio_pasado_monto_pension	"monto pension a񯠰asado"
label var	eft_anio_pasado_monto_remesas	"monto de remesas a񯠰asado"
*label var	eft_anio_pasado_ocacional	"recibio ingreso ocasional a񯠰asado"
label var	eft_anio_pasado_otros	"recibio otros ingresos a񯠰asado"
label var	eft_anio_pasado_otros_esp	"otros ingresos a񯠰asado especificar"
label var	eft_anio_pasado_pension	"recibio ing. pension a񯠰asado"
label var	eft_anio_pasado_remesas	"recibio remesas a񯠰asado"
label var	eft_ano	"a񯢍
label var	eft_anos_encontrar_empleo	"a񯠥n encontrar empleo"
label var	eft_apariencia_vivienda	"apariencia de la vivienda"
label var	eft_aspirador_elect	"aspirador_elect"
label var	eft_automovil	"automovil"
label var	eft_ayuda_familiares_anual	"monto de ayuda de familiares annual"
label var	eft_ayuda_familiares_mensual	"monto de ayuda de familiares mensual"
label var	eft_ayudo_fam_sem_ant	"ayudo a familiar"
label var	eft_barrio_seccion	"barrio o seccion"
label var	eft_bienes_consumo_anual	"bienes de consumo annual"
label var	eft_bienes_consumo_mensual	"bienes de consumo mensual"
label var	eft_busco_otra_act_mes_ant	"busco otra actividad economica el mes ant."
label var	eft_busco_trab_mes_ant	"busco trabajo el mes anterior"
label var	eft_busco_trab_sem_ant	"busco trabajo la semana anterior"
label var	eft_calentador	"calentador"
*label var	eft_calle_lugar	"calle o lugar"
label var	eft_calles_asfaltadas	"las calles estas asfaltadas"
label var	eft_camara_video	"camara_video"
label var	eft_cant_cuartos	"cantidad de cuartos"
label var	eft_cant_dormitorios	"cantidad de dormitorios"
label var	eft_cant_hogares	"cantidad de hogares en la vivienda"
label var	eft_cant_miembros	"cantidad de miembros en el hogar"
label var	eft_cant_pers_trab	"cant. personas que trabajan empresa"
label var	eft_cant_sem_ausentes	"cantidad de semanas ausentes"
label var	eft_categoria_ocup_princ	"categoria de ocupacion principal"
label var	eft_celular	"celular"
label var	eft_combustible_cocina	"combustible usa cocinar"
label var	eft_combustible_cocina_esp	"combustible usa cocinar especificar"
label var	eft_computadora	"computadora"
label var	eft_cosio_lavo_sem_ant	"cosio o lavo "
label var	eft_cuanto_ganaba	"cuanto ganaba en su empleo"
label var	eft_cuantos_empleos_desoc	"cuantos empleos ha tenido"
label var	eft_cuantos_empleos_ocup	"cuantos empleos ha tenido"
label var	eft_cultivo_sem_ant	"cultivo "
label var	eft_desc_ocupacion	"descripcion de su ocupacion"
label var	eft_desc_rama	"descripcion de rama donde trabaja"
label var	eft_desea_trab_mas_horas	"desea trabajar mas horas"
label var	eft_dias_encontrar_empleo	"dias en encontrar empleo"
label var	eft_dias_sem_ocup_princ	"dias a la semana ocup. principal"
label var	eft_dias_sem_ocup_secun	"dias a la semana ocup. secundaria"
label var	eft_dvd	"dvd"
label var	eft_edad	"edad"
label var	eft_elab_prod_sem_ant	"elaboro producto"
label var	eft_empresa_tiene_licencia	"la empresa tiene licencia de operar"
label var	eft_en	"edificacion nueva"
label var	eft_encuestado_anteriormente	"el hogar fue encuestado anterior mente"
label var	eft_estado_civil	"estado civil"
label var	eft_estado_vivienda	"estado de la vivienda"
label var	eft_estrato	"estrato"
label var	eft_estufa	"estufa"
label var	eft_factor_exp	"factor de expansion (peso)"
label var	eft_fecha_entrevista	"fecha de la entrevista"
label var	eft_firmo_contrato	"firmo contrato de trabajo"
label var	eft_forma_envio_remesa	"forma envio remesa (no usada)"
label var	eft_frec_ing_remesa_sem	"frecuencia ing. remesa (no usada)"
label var	eft_gobierno_ing_nac	"recibio ingresos por ayuda del gobierno"
label var	eft_hogar	"numero de hogar"
label var	eft_horas_sem_ocup_princ	"horas trabajadas por semana"
label var	eft_horas_sem_ocup_secun	"horas a la semana ocup. secundaria"
label var	eft_horno_elect	"horno_elect"
label var	eft_horno_microndas	"horno_microndas"
*label var	eft_informante	"persona que dio la informacion del hogar"
label var	eft_ing_ocup_princ	"ingreso de ocupacion principal"
label var	eft_ing_ocup_secun	"ingreso de la ocupacion secundaria"
label var	eft_ingreso_hora	"ingreso por hora"
label var	eft_ingreso_mensual	"ingreso mensual"
*label var	eft_ingresos_hogar	"ingresos del hogar per capita"
label var	eft_interes_ing_nac	"recibio ingresos por interes nacional"
label var	eft_intereses_ing_ext	"recibio interes del exterior"
label var	eft_inversor	"inversor"
label var	eft_lavadora	"lavadora"
label var	eft_licuadora	"licuadora"
label var	eft_maquina_coser	"maquina_coser"
label var	eft_mes_pasado_comisiones	"ingresos por comisiones mes pasado"
label var	eft_mes_pasado_horas_extras	"ingresos por horas extra mes pasado"
label var	eft_mes_pasado_propinas	"ingresos por propinas mes pasado"
label var	eft_meses_encontrar_empleo	"meses en encontrar empleo"
label var	eft_meses_ultimo_empleo	"cuantos meses han pasado de su ult. empleo"
label var	eft_miembro	"numero de miembro"
label var	eft_moneda_ing_interes_mes	"moneda interes exterior"
label var	eft_moneda_ing_pension_mes	"moneda de la pension del exterior"
label var	eft_moneda_ing_remesa_sem	"moneda ingreso remesa (no usada)"
*label var	eft_moneda_otros_ing_sem	"moneda otros ingresos exterior"
label var	eft_monto_alquiler	"monto de pago de alquiler pesos"
label var	eft_monto_alquiler_dolares	"monto de pago de alquiler dolares"
label var	eft_monto_alquiler_ing_nac	"monto alquiler nacional"
label var	eft_monto_cesantia	"monto de la cesantia"
label var	eft_monto_equiv_regalo	"monto equivalente del regalo pesos"
label var	eft_monto_gobierno_ing_nac	"monto ayuda del gobierno nacional"
label var	eft_monto_ing_interes_mes	"monto interes exterior"
label var	eft_monto_ing_pension_mes	"monto pension del exterior"
label var	eft_monto_ing_remesa_sem	"monto ingreso remesa (no usada)"
label var	eft_monto_interes_ing_nac	"monto interes nacional"
label var	eft_monto_ocasional_ing_nac	"monto ocasional nacional"
label var	eft_monto_otros_ing_nac	"monto de otros ingresos nacional"
label var	eft_monto_otros_ing_sem	"monto otros ingresos exterior "
label var	eft_monto_pension_ing_nac	"monto pension nacional"
label var	eft_monto_probable_alq	"monto probable de alquiler de vivienda"
label var	eft_monto_probable_alq_dolares	"monto probable de alquiler de vivienda us$"
label var	eft_monto_remesas_ing_nac	"monto remesas nacional"
label var	eft_motivo_dejo_trabajar	"por que dejo de trabajar"
label var	eft_motivo_dejo_trabajar_esp	"por que dejo de trabajar especificar"
label var	eft_motivo_no_asiste	"motivo no asiste"
label var	eft_motivo_no_asiste_esp	"motivo no asiste especificar"
label var	eft_motivo_no_busca_trab	"motivo no busca trabajo"
label var	eft_motivo_no_trab_sem_ant	"motivo no trabajo"
label var	eft_motivo_no_trab_sem_ant_esp	"motivo no trabajo especificar"
label var	eft_motocicleta	"motocicleta"
label var	eft_municipio	"municipio"
label var	eft_municipio_reside	"municipio de residencia"
label var	eft_no_apto_por_piso	"numero de apartamentos por piso"
label var	eft_no_de_casas	"numero de casas"
label var	eft_no_de_pisos	"numero de piso"
label var	eft_no_orden_upm	"numero de orden en la upm"
label var	eft_no_tiene_ninguno	"no tiene ningun articulo"
*label var	eft_nombre_empresa	"nombre de la empresa que labora"
*label var	eft_nombre_miembro	"nombre del miembro"
label var	eft_ocasional_ing_nac	"recibio ingresos ocasionales nacional"
label var	eft_ocupacion_princ	"ocupacion principal"
label var	eft_otro	"otro"
label var	eft_otros_ing_ext	"recibio otros ingresos del exterior"
label var	eft_otros_ing_nac	"recibio otros ingresos nacional"
label var	eft_otros_ing_nac_esp	"especifique los otros ingresos"
label var	eft_paga_alquiler	"paga alquiler por la vivienda"
label var	eft_pago_alimentos_monto	"monto de pago en alimentos"
label var	eft_pago_alimentos_op	"rebicio pago en alimentos"
label var	eft_pago_otros_monto	"monto de pagos por otros motivos"
label var	eft_pago_otros_op	"recibio pago de otros motivos"
label var	eft_pago_otros_op_esp	"pago de otros motivos especificos"
label var	eft_pago_transporte_monto	"monto de pago en transporte"
label var	eft_pago_transporte_op	"recibio pago en transporte"
label var	eft_pago_vestido_monto	"monto de pago en vestidos"
label var	eft_pago_vestido_op	"recibio pago en vestidos"
label var	eft_pago_vivienda_op	"recibio pago en vivienda"
label var	eft_pago_viviendas_monto	"monto de pago en viviendas"
label var	eft_pais_ing_interes_mes	"pais interes exterior"
label var	eft_pais_ing_pension_mes	"pais de la pension del exterior"
label var	eft_pais_ing_remesa_sem	"pais ingreso remesa (no usada)"
*label var	eft_pais_otros_ing_sem	"pais otros ingresos exterior"
label var	eft_pais_reside	"pais de residencia"
label var	eft_parabola	"parabola"
label var	eft_paraje	"paraje"
label var	eft_pared_exterior	"pared exterior"
label var	eft_pared_exterior_esp	"pared exterior especificar"
label var	eft_pared_interior	"pared interior"
label var	eft_pared_interior_esp	"pared interior especificar"
label var	eft_parentesco_con_jefe	"parentesco con jefe del hogar"
label var	eft_pension_ing_ext	"recibio pension del exterior"
label var	eft_pension_ing_nac	"recibio ingresos por pension nacional"
label var	eft_periodo	"periodo de la encuesta"
label var	eft_periodo_ing_ocup_princ	"periodo de ingreso ocup. principal"
label var	eft_periodo_ing_ocup_secun	"periodo de ingreso ocup. secundaria"
label var	eft_periodo_pago_alq	"periodo de pago del alquiler"
label var	eft_pertenece_org_sindicato	"pertenece a sindicato"
label var	eft_piso	"piso"
label var	eft_piso_esp	"piso especificar"
label var	eft_plancha_elect	"plancha_elect"
label var	eft_provincia	"provincia"
label var	eft_que_hizo	"que hizo para buscar trabajo"
label var	eft_que_hizo_esp	"que hizo para buscar trabajo especificar"
label var	eft_radio	"radio"
label var	eft_rama_princ	"rama principal"
label var	eft_razon_menos_40_horas	"por que trabaja menos de 40 horas"
label var	eft_razon_menos_40_horas_esp	"razon menos de 40 horas esp."
label var	eft_razon_traslado	"razon de traslado"
label var	eft_recibio_cesantia	"recibio cesantia"
label var	eft_recibio_ing_interes_mes	"recibio ingresos por interes exterior"
label var	eft_recibio_ing_pension_mes	"recibio pension del exterior"
label var	eft_recibio_ing_remesa_sem	"recibio ingreso por remesa (no usada)"
label var	eft_recibio_otros_ing_sem	"recibio otros ingresos del exterior"
label var	eft_recibio_regalos	"recibio regalos exterior "
label var	eft_refrigerador	"refrigerador"
label var	eft_remesas_ing_ext	"recibio remesas del exterior"
label var	eft_remesas_ing_nac	"recibio ingreso por remesas nacionales"
label var	eft_rotacion	"rotacion de muestra"
label var	eft_se_matriculo	"se matriculo en"
label var	eft_se_matriculo_texto	"indique que cursa"
label var	eft_semestre	"semestre"
label var	eft_sexo	"sexo"
label var	eft_tanda_asiste	"tanda a la que asiste"
label var	eft_techo	"techo "
label var	eft_techo_esp	"techo especificar"
label var	eft_telefono	"telefono"
label var	eft_televisor	"televisor"
label var	eft_tenencia_esp	"tipo de tenencia especificar"
label var	eft_tiempo_busca_trab	"tiempo que busca trabajo"
label var	eft_tiempo_lab_anos	"tiempo laborando en a񯳢
label var	eft_tiempo_lab_dias	"tiempo laborando en dias"
label var	eft_tiempo_lab_meses	"tiempo laborando en meses"
label var	eft_tiempo_residencia	"tiempo de residencia"
label var	eft_tiempo_sin_trabajar	"que tiempo tiene sin trabajar"
label var	eft_tiene_cond_jornada	"tiene condicion de jornana completa"
label var	eft_tiene_ocup_secun	"tiene ocupacion secundaria"
label var	eft_tipo_centro	"tipo centro de estudios"
label var	eft_tipo_contrato	"tipo de contrato "
label var	eft_tipo_de_uso	"tipo de uso de la vivienda"
label var	eft_tipo_establecimiento	"tipo de establecimiento donde labora"
label var	eft_tipo_relacion	"tipo de relacion"
label var	eft_tipo_sanitario	"forma de eliminacion de excreta"
label var	eft_tipo_tenencia	"tipo de tenencia de la vivienda"
label var	eft_tipo_vivienda	"vivienda"
label var	eft_tipo_vivienda_esp	"tipo de vivienda especificar"
label var	eft_trabajo_antes	"trabajo anteriormente"
label var	eft_trabajo_sem_ant	"trabajo la semana anterior"
label var	eft_tuvo_act_econ_sem_ant	"tuvo actividad economica "
label var	eft_ubicacion_cocina	"ubicacion de la cocina"
label var	eft_ult_ano_aprobado	"ultimo a񯠡probado"
label var	eft_ult_doce_beneficios_marg	"beneficios marginales ult. doce meses"
label var	eft_ult_doce_bonificacion	"bonificacion ult. doce meses"
label var	eft_ult_doce_dividendos	"divivendos ult. doce meses"
label var	eft_ult_doce_regalia_pascual	"regalia pascual ult. doce meses"
label var	eft_ult_doce_utilidades_emp	"utilidades emp. ult. doce meses"
label var	eft_ult_doce_vacaciones_pagas	"vacaciones ult. doce meses"
label var	eft_ult_nivel_alcanzado	"ultimo nivel alcanzado"
label var	eft_upm	"unidad parcial de muestreo"
label var	eft_video	"video"
label var	eft_vivienda	"numero de vivienda"
label var	eft_zona	"zona(urbana o rural)"
label var	eft_zona_reside	"zona de residencia"
label var	formal	"formal"
label var	inactivo	"inactivo"
label var	informal	"informal"
label var	nuevo	"nuevo "
label var	ocupado	"ocupado "
label var	pea	"poblacion economicamente activa"
label var	pet	"poblacion en edad de trabajar"
*label var	remesas	"ingresos por remesas exterior mensual"












