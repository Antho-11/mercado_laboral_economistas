################################################################################
# PASO 4:llamar a la API de google para que realice la limpieza complementaria #
################################################################################

df_complementaria <- tibble(
  nivel_educativo = NA_character_,
  edad = NA_character_,
  experiencia = NA_character_,
  habilidades = NA_character_,
  conocimientos_tecnicos = NA_character_,
  ID = map_chr(df_completo$descripción, rlang::hash)
)

for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  tryCatch({
    # Extraer el ID de la observación actual
    id_actual <- df_completo$ID[i]  # Ajusta "ID" al nombre real de la columna de identificación
    
    # Extraer la descripción de la oferta de trabajo
    datos_texto_oferta_de_trabajo <- df_completo$descripción[i]
    
    # Generar el prompt
    dato_texto_prompt_gooaistudio <- paste(prompt, datos_texto_oferta_de_trabajo, post_texto)
    
    # Configuración de la API
    respuesta_api <- POST(
      url = gooai_studio_url,
      add_headers("Content-Type" = "application/json"),
      body = toJSON(list(
        contents = list(
          list(parts = list(list(text = dato_texto_prompt_gooaistudio)))
        )
      ), auto_unbox = TRUE),
      query = list(key = api_key),
      encode = "json"
    )
    
    # Procesar la respuesta de la API
    datos_goo_respuesta <- content(respuesta_api, as = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      .$candidates %>%
      .$content %>%
      .$parts %>%
      .[[1]] %>%
      str_squish() %>%
      str_remove_all('```json|```') %>%
      fromJSON()
    
    # Extraer los requisitos del JSON
    requisitos <- datos_goo_respuesta$requisitos_del_puesto_de_trabajo
    
    # Encontrar la fila correspondiente en df_complementaria según el ID
    fila_correspondiente <- which(df_complementaria$ID == id_actual)
    
    # Si existe una fila con ese ID, actualizarla con los datos extraídos
    if (length(fila_correspondiente) == 1) {
      df_complementaria$nivel_educativo[fila_correspondiente] <- ifelse(
        length(requisitos$nivel_educativo) > 0, requisitos$nivel_educativo[[1]], NA
      )
      
      df_complementaria$edad[fila_correspondiente] <- ifelse(
        length(requisitos$edad) > 0, requisitos$edad[[1]], NA
      )
      
      df_complementaria$experiencia[fila_correspondiente] <- ifelse(
        length(requisitos$experiencia) > 0, requisitos$experiencia[[1]], NA
      )
      
      # Convertir listas en strings para evitar errores de longitud
      df_complementaria$habilidades[fila_correspondiente] <- ifelse(
        length(requisitos$habilidades) > 0, paste(requisitos$habilidades, collapse = ",// "), NA
      )  
      
      df_complementaria$conocimientos_tecnicos[fila_correspondiente] <- ifelse(
        length(requisitos$conocimientos_tecnicos) > 0, paste(requisitos$conocimientos_tecnicos, collapse = ",// "), NA
      )  
    } else {
      cat("No se encontró una fila correspondiente en df_complementaria para el ID:", id_actual, "\n")
    }
    
    # Pausa de 3 segundos para evitar bloqueos de la API
    Sys.sleep(3)
    
  }, error = function(e) {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  })
}

#Hacemos un full_join para agregar ambas df's

df_pre_filtros <- full_join(df_completo, df_complementaria, by = "ID") %>%
  mutate(
    nivel_educativo = coalesce(nivel_educativo.x, nivel_educativo.y),
    edad = coalesce(edad.x, edad.y),
    experiencia = coalesce(experiencia.x, experiencia.y),
    habilidades = coalesce(habilidades.x, habilidades.y),
    conocimientos_tecnicos = coalesce(conocimientos_tecnicos.x, conocimientos_tecnicos.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

saveRDS(df_pre_filtros, "04_datos_limpios/df_datos_limpio.rds")




################################################################################
#                  PASO 5: Comenzar a filtrar los datos obtenidos              #
################################################################################

df_pre_filtros <- df_datos_limpio
cat(paste
    (df_pre_filtros$nivel_educativo, collapse = './// ') 
)

habilidades_categorizadas <- tibble(
  análisis_y_gestión_financiera = NA_character_,
  comunicación_asertiva = NA_character_,
  liderazgo = NA_character_,
  logística = NA_character_,
  herramientas_digitales = NA_character_,
  cumplimiento_legal_y_administrativo = NA_character_,
  ID = map_chr(df_completo$descripción, rlang::hash)
)

#Construimos el prompt para filtrar las habilidades

prompt_habilidades <- "Eres un experto en análisis de ofertas profesionales. Analiza las habilida contenidas entre triple asterisco ***"
post_habilidades <- 'y categorízalas en formato JSON, evaluando si coinciden con las siguientes categorías: análisis_y_gestión_financiera, comunicación_asertiva, liderazgo, logística, herramientas_digitales, cumplimiento_legal_y_administrativo. Sigue estrictamente estas reglas:\n\n1. **Estructura de salida**: {\n  \"categorización_habilidades\": {\n    \"análisis_y_gestión_financiera\": boolean,\n    \"comunicación_asertiva\": boolean,\n    \"liderazgo\": boolean,\n    \"logística\": boolean,\n    \"herramientas_digitales\": boolean,\n    \"cumplimiento_legal_y_administrativo\": boolean\n  }\n}\n\n2. **Criterios de categorización**:\n   - `análisis_y_gestión_financiera`: Habilidades relacionadas con finanzas, contabilidad, presupuestos, análisis de riesgo o inversiones.\n   - `comunicación_asertiva`: Habilidades de redacción, negociación, atención al cliente o expresión oral/escrita.\n   - `liderazgo`: Habilidades de gestión de equipos, toma de decisiones, motivación o resolución de conflictos.\n   - `logística`: Habilidades de operaciones, cadena de suministro, inventarios o planificación estratégica.\n   - `herramientas_digitales`: Habilidades técnicas como programación, SEO, blockchain o manejo de software especializado.\n   - `cumplimiento_legal_y_administrativo`: Habilidades vinculadas a normativas, gobierno corporativo, auditorías o gestión legal.\n\n3. **Instrucciones clave**:\n   - Asigna `true` solo si **al menos una habilidad** coincide claramente con la categoría.\n   - Asigna `false` si ninguna habilidad coincide.\n   - No infieras información ni agregues categorías no especificadas.\n   - Mantén el formato JSON sin comentarios adicionales."'

for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  id_actual <- df_pre_filtros$ID[i]
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_pre_filtros$habilidades[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(prompt_habilidades, datos_texto_oferta_de_trabajo, post_habilidades)
    
    # Configuración de la API
    
    respuesta_api <- POST(
      url = gooai_studio_url,
      add_headers("Content-Type" = "application/json"),
      body = toJSON(list(
        contents = list(
          list(parts = list(list(text = dato_texto_prompt_gooaistudio)))
        )
      ), auto_unbox = TRUE),
      query = list(key = api_key),
      encode = "json"
    )
    
    # Procesamiento de la respuesta
    datos_goo_respuesta <- content(respuesta_api, as = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      .$candidates %>%
      .$content %>%
      .$parts %>%
      .[[1]] %>%
      str_squish() %>%
      str_remove_all('```json|```') %>%
      fromJSON()
    
    # Extraer las habilidades del JSON
    habilidades <- datos_goo_respuesta$categorización_habilidades
    
    # Encontrar la fila correspondiente en df_complementaria según el ID
    fila_correspondiente <- which(habilidades_categorizadas$ID == id_actual)
    
    # Si existe una fila con ese ID, actualizarla con los datos extraídos
    if (length(fila_correspondiente) == 1) {
      habilidades_categorizadas$análisis_y_gestión_financiera[fila_correspondiente] <- ifelse(
        length(habilidades$análisis_y_gestión_financiera) > 0, habilidades$análisis_y_gestión_financiera[[1]], NA
      )
      
      habilidades_categorizadas$comunicación_asertiva[fila_correspondiente] <- ifelse(
        length(habilidades$comunicación_asertiva) > 0, habilidades$comunicación_asertiva[[1]], NA
      )
      
      habilidades_categorizadas$liderazgo[fila_correspondiente] <- ifelse(
        length(habilidades$liderazgo) > 0, habilidades$liderazgo[[1]], NA
      )
      
      habilidades_categorizadas$logística[fila_correspondiente] <- ifelse(
        length(habilidades$logística) > 0, habilidades$logística[[1]], NA
      )
      
      habilidades_categorizadas$herramientas_digitales[fila_correspondiente] <- ifelse(
        length(habilidades$herramientas_digitales) > 0, habilidades$herramientas_digitales[[1]], NA
      )
      
      habilidades_categorizadas$cumplimiento_legal_y_administrativo[fila_correspondiente] <- ifelse(
        length(habilidades$cumplimiento_legal_y_administrativo) > 0, habilidades$cumplimiento_legal_y_administrativo[[1]], NA
      )
      
    } else {
      cat("No se encontró una fila correspondiente en df_complementaria para el ID:", id_actual, "\n")
    }
    
    # Pausa de 3 segundos para evitar bloqueos de la API
    Sys.sleep(3)
    
  }, error = function(e) {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  })
}

saveRDS(habilidades_categorizadas, "04_datos_limpios/habilidades.rds")



#Ahora vamos con el nivel educativo 


educación_categorizadas <- tibble(
  bachiller = NA_character_,
  pregrado = NA_character_,
  postgrado = NA_character_,
  maestria = NA_character_,
  doctorado = NA_character_,
  ID = map_chr(df_completo$descripción, rlang::hash)
)

prompt_educacion <- "Eres un experto en análisis de ofertas de trabajo. Extrae y categoriza el nivel educativo especificado en la descripción del puesto de trabajo contenida entre triple asterisco ***"
post_educacion <- '***Debes estructurar los datos en el siguiente formato JSON:
{
  "nivel_educativo": {
    "bachiller": ,
    "pregrado": ,
    "postgrado": ,
    "maestría": ,
    "doctorado":
  }
}
Instrucciones clave: Analiza el campo "nivel_educativo" de la oferta de trabajo y determina si coincide con alguna de las siguientes categorías: Bachiller, Pregrado, Postgrado, Maestría, Doctorado Si el nivel educativo mencionado en el texto coincide con una categoría, asígnale true a esa categoría en el JSON. Si no se menciona explícitamente una categoría o el valor de la categoria es N/A, asígnale false. No infieras información ni hagas suposiciones. Usa solo los datos presentes en el texto. Mantén el formato JSON sin modificaciones y asegúrate de que la extracción sea precisa y uniforme en cada iteración del loop. Importante siempre manten el formato JSON indicado'



for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  id_actual <- df_pre_filtros$ID[i]
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_pre_filtros$nivel_educativo[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(prompt_educacion, datos_texto_oferta_de_trabajo, post_educacion)
    
    # Configuración de la API
    
    respuesta_api <- POST(
      url = gooai_studio_url,
      add_headers("Content-Type" = "application/json"),
      body = toJSON(list(
        contents = list(
          list(parts = list(list(text = dato_texto_prompt_gooaistudio)))
        )
      ), auto_unbox = TRUE),
      query = list(key = api_key),
      encode = "json"
    )
    
    # Procesamiento de la respuesta
    datos_goo_respuesta <- content(respuesta_api, as = "text", encoding = "UTF-8") %>%
      fromJSON() %>%
      .$candidates %>%
      .$content %>%
      .$parts %>%
      .[[1]] %>%
      str_squish() %>%
      str_remove_all('```json|```') %>%
      fromJSON()
    
    
    educacion <- datos_goo_respuesta$nivel_educativo
    
    fila_correspondiente <- which(educación_categorizadas$ID == id_actual)
    
    # Encontrar la fila correspondiente en educación_categorizadas según el ID
    if (length(fila_correspondiente) == 1) {
      educación_categorizadas$bachiller[fila_correspondiente] <- ifelse(
        length(educacion$bachiller) > 0, educacion$bachiller[[1]], NA
      )
      
      educación_categorizadas$pregrado[fila_correspondiente] <- ifelse(
        length(educacion$pregrado) > 0, educacion$pregrado[[1]], NA
      )
      
      educación_categorizadas$postgrado[fila_correspondiente] <- ifelse(
        length(educacion$postgrado) > 0, educacion$postgrado[[1]], NA
      )
      
      educación_categorizadas$maestria[fila_correspondiente] <- ifelse(
        length(educacion$maestria) > 0, educacion$maestria[[1]], NA
      )
      
      educación_categorizadas$doctorado[fila_correspondiente] <- ifelse(
        length(educacion$doctorado) > 0, educacion$doctorado[[1]], NA
      )
      
    } else {
      cat("No se encontró una fila correspondiente en educación_categorizadas para el ID:", id_actual, "\n")
    }
    
    
    # Pausa de 3 segundos para evitar bloqueos de la API
    Sys.sleep(3)
    
  }, error = function(e) {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  })
}

saveRDS(educación_categorizadas, '04_datos_limpios/educación.rds')