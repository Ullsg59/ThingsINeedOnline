#### Libraries ----
library(tidyverse)
library(lubridate)
library(data.table)
library(effsize)
library(scales)
library(rstatix)


#### Data ----
encuestas = fread("C:/Users/juann/OneDrive/Desktop/School/UNAM - Eco/Semestre1/MetodologiaInvestigacion/Uso de chatGPT y otros LLMs (respuestas) - Respuestas de formulario 1.csv", stringsAsFactors = TRUE)

encuestas = encuestas %>% 
    janitor::clean_names() %>% 
    mutate(marca_temporal = dmy_hms(marca_temporal))

summary(encuestas)


#### Procesamiento ----
encuestas = encuestas %>% 
    mutate(frecuencia = case_when(con_que_frecuencia_utilizas_herramientas_de_ia == 'Diario' ~ 7,
                                  con_que_frecuencia_utilizas_herramientas_de_ia == '2-3 veces por semana' ~ 2.5,
                                  con_que_frecuencia_utilizas_herramientas_de_ia == 'Un par de veces al mes' ~ 1,
                                  con_que_frecuencia_utilizas_herramientas_de_ia == 'Mensualmente o menos' ~ 0.5,
                                  TRUE ~ 0)) %>% 
    mutate(sesiones = case_when(cuando_lo_usas_cuantas_sesiones_usas_en_promedio_una_sesion_se_refiere_a_todas_las_preguntas_dentro_de_una_misma_conversacion == '1 sesión por uso' ~ 1,
                                cuando_lo_usas_cuantas_sesiones_usas_en_promedio_una_sesion_se_refiere_a_todas_las_preguntas_dentro_de_una_misma_conversacion == '2-3 sesiones por uso' ~ 2.5,
                                cuando_lo_usas_cuantas_sesiones_usas_en_promedio_una_sesion_se_refiere_a_todas_las_preguntas_dentro_de_una_misma_conversacion == '3-5 sesiones por uso' ~ 4,
                                cuando_lo_usas_cuantas_sesiones_usas_en_promedio_una_sesion_se_refiere_a_todas_las_preguntas_dentro_de_una_misma_conversacion == 'Más de 5 sesiones por uso' ~ 7,
                                  TRUE ~ 0)) %>% 
    mutate(preguntas = case_when(aproximadamente_cuantas_preguntas_realizas_por_sesion_cada_prompt_cuenta_como_una_pregunta_aunque_sea_una_correccion_al_prompt_anterior == '1 a 3 consultas' ~ 2.5,
                                 aproximadamente_cuantas_preguntas_realizas_por_sesion_cada_prompt_cuenta_como_una_pregunta_aunque_sea_una_correccion_al_prompt_anterior == '4 a 10 consultas' ~ 7,
                                 aproximadamente_cuantas_preguntas_realizas_por_sesion_cada_prompt_cuenta_como_una_pregunta_aunque_sea_una_correccion_al_prompt_anterior == 'Más de 10 consultas' ~ 14,
                                  TRUE ~ 0)) %>% 
    mutate(coefuso = frecuencia * sesiones * preguntas)
    
# Revisar que la transformación sea correcta

encuestas %>% 
    group_by(con_que_frecuencia_utilizas_herramientas_de_ia) %>% 
    summarise(mean = mean(frecuencia))

encuestas %>% 
    group_by(cuando_lo_usas_cuantas_sesiones_usas_en_promedio_una_sesion_se_refiere_a_todas_las_preguntas_dentro_de_una_misma_conversacion) %>% 
    summarise(mean = mean(sesiones))

encuestas %>% 
    group_by(aproximadamente_cuantas_preguntas_realizas_por_sesion_cada_prompt_cuenta_como_una_pregunta_aunque_sea_una_correccion_al_prompt_anterior) %>% 
    summarise(mean = mean(preguntas))

# Ocupaciones
encuestas = encuestas %>% 
    mutate(ocupacion = case_when(
        cual_es_tu_ocupacion_principal == 'Agente inmobiliaria' ~	'Empleado en empresa no tecnológica',
        cual_es_tu_ocupacion_principal == 'Consultor en empresa tecnologica' ~	'Empleado en empresa tecnológica',
        cual_es_tu_ocupacion_principal == 'Consultor' ~	'Empleado en empresa no tecnológica',
        cual_es_tu_ocupacion_principal == 'Consultora' ~	'Empleado en empresa no tecnológica',
        cual_es_tu_ocupacion_principal == 'Desempleado'	~ 'Otros',
        cual_es_tu_ocupacion_principal == 'Empleado en empresa no tecnológica' ~	'Empleado en empresa no tecnológica',
        cual_es_tu_ocupacion_principal == 'Empleado en empresa tecnológica' ~	'Empleado en empresa tecnológica',
        cual_es_tu_ocupacion_principal == 'Estudiante licenciatura'	~ 'Estudiante licenciatura',
        cual_es_tu_ocupacion_principal == 'Inversionista' ~	'Otros',
        cual_es_tu_ocupacion_principal == 'Jubilado' ~	'Otros',
        cual_es_tu_ocupacion_principal == 'Profesionista independiente' ~	'Otros',
        cual_es_tu_ocupacion_principal == 'Profesor o investigador' ~	'Profesor o investigador',
        cual_es_tu_ocupacion_principal == 'Tengo una empresa que desarrolla proyectos de tecnología' ~	'Empleado en empresa tecnológica',
        TRUE ~ 'error'
    )) %>% 
    mutate(ocupacion = as.factor(ocupacion))

# checar que no haya errores
encuestas %>% count(ocupacion)


# uso simple
encuestas = encuestas %>% 
    mutate(consultas_Simples = case_when(
        que_tan_frecuente_haces_este_tipo_de_consultas_de_tareas_simples == "Nunca" ~ 0,
        que_tan_frecuente_haces_este_tipo_de_consultas_de_tareas_simples == "Ocasionalmente" ~ 1,
        que_tan_frecuente_haces_este_tipo_de_consultas_de_tareas_simples == "Muy frecuente" ~ 2,
        TRUE ~ 1000000
    ))

encuestas %>% 
    group_by(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares) %>% 
    summarise(mean = mean(consultas_Simples))


# uso simple
encuestas = encuestas %>% 
    mutate(cambio_comportamiento = case_when(
        ya_conociendo_el_impacto_ambiental_del_uso_de_estas_herramientas_crees_que_esto_influya_en_el_uso_que_le_das_o_darias_a_consultas_para_solucionar_tareas_simples == "Definitivamente no" ~ 1,
        ya_conociendo_el_impacto_ambiental_del_uso_de_estas_herramientas_crees_que_esto_influya_en_el_uso_que_le_das_o_darias_a_consultas_para_solucionar_tareas_simples == "Probablemente no" ~ 2,
        ya_conociendo_el_impacto_ambiental_del_uso_de_estas_herramientas_crees_que_esto_influya_en_el_uso_que_le_das_o_darias_a_consultas_para_solucionar_tareas_simples == "Probablemente sí" ~ 3,
        ya_conociendo_el_impacto_ambiental_del_uso_de_estas_herramientas_crees_que_esto_influya_en_el_uso_que_le_das_o_darias_a_consultas_para_solucionar_tareas_simples == "Definitivamente sí" ~ 4,
        TRUE ~ 100000000
    ))

encuestas %>% 
    group_by(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares) %>% 
    summarise(mean = mean(cambio_comportamiento))


#### Análisis ----
# edades
encuestas %>% 
    summarize(mean = mean(cuantos_anos_tienes),
              median = median(cuantos_anos_tienes),
              min = min(cuantos_anos_tienes),
              max = max(cuantos_anos_tienes))

encuestas %>% 
    group_by(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares) %>% 
    summarize(mean = mean(cuantos_anos_tienes),
              median = median(cuantos_anos_tienes),
              min = min(cuantos_anos_tienes),
              max = max(cuantos_anos_tienes))

# Esta gráfica es más bonita, pero es misleading por el bajo número de personas
ggplot(encuestas, aes(x = cuantos_anos_tienes, 
                      fill = has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares,
                      color = has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares)) +
    geom_density(alpha = 0.3) +
    labs(
        title = "Distribución de edades según uso de herramientas de IA",
        x = "Edad",
        y = "Densidad",
        fill = "¿Ha usado herramientas de IA?",
        color = "¿Ha usado herramientas de IA?"
    ) +
    theme_minimal()

# Boxplots del uso
ggplot(encuestas, aes(
    x = has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares,
    y = cuantos_anos_tienes,
    fill = has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares
)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.4) +
    geom_jitter(width = 0.1, alpha = 0.7) +
    labs(
        title = "Comparación de edades y uso de LLMs según encuesta.",
        x = "¿Has utilizado alguna vez herramientas de IA?",
        y = "Edad"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

# Es significativa la diferencia de edades? (No, no se puede afirmar con certeza)
wilcox.test(
    cuantos_anos_tienes ~ has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares,
    data = encuestas
)

cliff.delta(
    cuantos_anos_tienes ~ has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares,
    data = encuestas
)

# Intensidad de uso

ggplot(encuestas, aes(x = coefuso)) +
    geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
    scale_y_continuous(breaks = pretty_breaks()) + 
    labs(title = "Distribución del Coeficiente de Uso",
         x = "Coeficiente de Uso",
         y = "Número de Usuarios") + 
    theme_minimal()

nrow(encuestas %>% filter(coefuso <= 50)) / nrow(encuestas)


# Y si medimos intensidad de uso con edad?
encuestas = encuestas %>% 
    mutate(log_use = log1p(coefuso))

modelo <- lm(log_use ~ cuantos_anos_tienes, data = encuestas)

coeficiente <- round(coef(modelo)[2], 3)
intercepto <- round(coef(modelo)[1], 3)

r_cuadrado_ajustado <- round(summary(modelo)$adj.r.squared, 3)

etiqueta <- paste0("y = ", coeficiente, "x + ", intercepto, "\nR² ajustado = ", r_cuadrado_ajustado)

ggplot(encuestas, aes(x = cuantos_anos_tienes, y = log_use)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    annotate("text", 
             x = max(encuestas$cuantos_anos_tienes), 
             y = max(encuestas$log_use),        
             label = etiqueta,
             hjust = 1,
             vjust = 1) +
    labs(
        title = "Correlación entre la edad y el coeficiente de uso.",
        x = "Edad del encuestado",
        y = "Coeficiente de uso"
    )

print(coeficiente)
print(intercepto)
print(r_cuadrado_ajustado)
# Otra vez, negativo, pero no significativo


# trabajos
summary(encuestas$ocupacion)

encuestas %>% count(ocupacion) %>% mutate(pct = n/sum(n))

encuestas %>% 
    count(ocupacion, has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares) %>% 
    group_by(ocupacion) %>% 
    mutate(pct = n/sum(n)) %>% 
    select(-n) %>% 
    pivot_wider(names_from = has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares,
                values_from = pct,
                values_fill = 0)

# Región
encuestas %>% count(en_donde_vives)


# Ya enfoque en uso
encuestas %>%
    filter(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == 'Sí') %>% 
    count(que_tan_frecuente_haces_este_tipo_de_consultas_de_tareas_simples) %>% 
    mutate(pct = n/sum(n))


encuestas %>%
    filter(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == 'Sí') %>% 
    count(conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua) %>% 
    mutate(pct = n/sum(n))

encuestas %>%
    filter(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == 'Sí') %>% 
    count(que_tan_de_acuerdo_estas_con_la_siguiente_frase_el_uso_cotidiano_de_la_inteligencia_artificial_genera_un_impacto_ambiental_significativo) %>% 
    mutate(pct = n/sum(n))


encuestas %>% 
    filter(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == 'Sí') %>% 
    summary()


ggplot(encuestas, aes(x = del_1_al_10_que_tanto_consideras_el_impacto_ambiental_a_la_hora_de_seleccionar_el_modelo_que_usas_para_tus_tareas_cotidianas_con_estas_herramientas_1_se_refiere_a_que_no_lo_consideras_10_es_que_es_el_factor_determinante)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_x_continuous(breaks = pretty_breaks()) + 
    labs(title = "Histograma de Consideración del Impacto Ambiental para la Selección de Modelo",
         x = "Qué tan determinante es el impacto ambiental (10 es el principal factor)",
         y = "Número de Usuarios") + 
    theme_minimal()


# Análisis de objetivos

# Relación de edad con el coeficiente de uso
cor_age_int <- cor.test(
    ~ cuantos_anos_tienes + coefuso,
    data   = encuestas,
    method = "spearman",   
    exact  = FALSE)        

cor_age_int


# Relación de edad con conciencia
wilc_age_aw <- wilcox.test(
    cuantos_anos_tienes ~ conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua,
    data = encuestas,
    exact = FALSE)    

wilc_age_aw

encuestas %>% 
    filter(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == 'Sí') %>% 
    group_by(conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua) %>%
    summarise(median_age = median(cuantos_anos_tienes),
              n = n())

# Intensidad de uso y uso banal
cor_int_banal <- cor.test(
    ~ coefuso + consultas_Simples,
    data   = filter(encuestas, has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == "Sí"),
    method = "spearman",
    exact  = FALSE)

cor_int_banal


# La oregunta principal
encuestados_si <- encuestas %>% filter(has_utilizado_alguna_vez_herramientas_como_chat_gpt_gemini_deep_seek_o_similares == "Sí")

wilc_banal_aw <- wilcox_test(
    data  = encuestados_si,
    consultas_Simples ~ conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua,
    exact = FALSE,  
    detailed = TRUE)

wilc_banal_aw

eff_banal_aw <- wilcox_effsize(
    data = encuestados_si,
    consultas_Simples ~ conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua)

eff_banal_aw

tab_banal_aw <- encuestados_si %>% 
    count(conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua, consultas_Simples) %>% 
    group_by(conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua) %>% 
    mutate(pct = n / sum(n))

tab_banal_aw


ggplot(tab_banal_aw, aes(factor(consultas_Simples), pct, fill = conoces_el_impacto_ambiental_que_genera_cada_pregunta_realizada_a_herramientas_como_chat_gpt_al_consumir_recursos_como_energia_y_agua)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Gráfico de Barras con la Propensión a Consultas Banales y el Porcentaje de Usuarios que Representa",
         x = "Propensión a consultas banales (0–2)",
         y = "% de usuarios", fill = "Conoce impacto")


# Consistencia entre factor determinante y voluntad al cambio
cor_sal_beh <- cor.test(
    ~ del_1_al_10_que_tanto_consideras_el_impacto_ambiental_a_la_hora_de_seleccionar_el_modelo_que_usas_para_tus_tareas_cotidianas_con_estas_herramientas_1_se_refiere_a_que_no_lo_consideras_10_es_que_es_el_factor_determinante + cambio_comportamiento,
    data   = encuestados_si,
    method = "spearman",
    exact  = FALSE)

cor_sal_beh




cor_int_beh <- cor.test(
    ~ coefuso + cambio_comportamiento,
    data   = encuestados_si,
    method = "spearman",
    exact  = FALSE)

cor_int_beh


sessionInfo(
)
