#En este script nos encargaremos de la limpieza y presentación de los datos 
#NOTA: falta por crear un documento quarto para la presentación final 

################################################################################
#                              COMPUTRABAJO                                    #
################################################################################

################################################################################
#                         PASO 1:llamar librerias                              #
################################################################################
library(httr2)
library(httr)
library(jsonlite)
library(dplyr)
library(plotly)
library(apexcharter)
library(stringr)
library(rvest)
library(tidyverse)
library(magrittr)
library(readxl)

################################################################################
#        PASO 2:crear nueva df con la que tengamos todos los datos             #
################################################################################

#Primero llamamos a todas las df que tenemos guardadas 

df_1 <- readRDS("01_datos_sin_limpiar/demanda_economista_freelancer.rds")
df_2 <- readRDS("01_datos_sin_limpiar/df_datos_computrabajo_2025_01.rds")
df_3 <- readRDS("01_datos_sin_limpiar/df_datos_empleate_2025_01.rds")
df_5 <- readRDS("01_datos_sin_limpiar/df_datos_trabajo_org.rds")
df_6 <- readRDS("01_datos_sin_limpiar/df_datos_trabajosdiarios_2025_01.rds")

df_1 <- df_1%>%
  rename(descripcción = descripcion_del_trabajo)
df_5 <- df_5%>% select(-descripción)

df_completo <- bind_rows(df_1, df_2, df_3, df_5, df_6)
df_completo <- df_completo%>%
  rename(descripción = descripcción)%>%
  distinct (descripción, .keep_all = TRUE)%>%
  filter(!is.na(descripción)) %>%
  mutate(
    nivel_educativo = NA_character_,
    edad = NA_character_,
    experiencia = NA_character_,
    habilidades = NA_character_,
    conocimientos_tecnicos = NA_character_,
    ID = map_chr(descripción, rlang::hash)
  )%>%
  as_tibble()

saveRDS(df_completo,"01_datos_sin_limpiar/df_completo.rds")

ofertas_extraidas <- length(df_completo$descripción)

str_length(df_completo$descripción)%>%
  sum()

################################################################################
#        PASO 3:llamar a la API de google para que realice la limpieza         #
################################################################################


gooai_studio_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
api_key <- "AIzaSyBJn8eAI6ypj3MY3fWemQHsXIeQ5gQlGOw"  

# Construcción del prompt
prompt <- "Eres un experto en análisis de ofertas de trabajo. Extrae en formato JSON los requisitos especificados en la descripción del puesto de trabajo contenida entre triple asterisco***" 
post_texto <- '***. Debes estructurar los datos siguiendo estrictamente esta plantilla: {
  "requisitos_del_puesto_de_trabajo": {
    "nivel_educativo": "",
    "edad": "",
    "experiencia": "",
    "habilidades": [],
    "conocimientos_tecnicos": []
  }
}, instrucciones claves: Extrae solo los valores explícitos en el texto. No infieras información ni agregues datos adicionales, Si un campo no está presente en la descripción, asígnale "N/A", Mantén la estructura JSON sin modificar y Asegúrate de que la extracción sea precisa y consistente en cada iteración del loop'


for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_completo$descripción[i]
    
   
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
    
    # Extraer datos del JSON
    requisitos <- datos_goo_respuesta$requisitos_del_puesto_de_trabajo
    
    # Guardar los datos en df_completo
    df_completo$nivel_educativo[i] <- list(requisitos$nivel_educativo)
    df_completo$edad[i] <- list(requisitos$edad)        #volver mas adelante as.numeric
    df_completo$experiencia[i] <- list(requisitos$experiencia)
    df_completo$habilidades[i] <- list(requisitos$habilidades)  
    df_completo$conocimientos_tecnicos[i] <- list(requisitos$conocimientos_tecnicos)  # Estos 'ultimos dos los paso a string, pero es m'as facil trabajarlo como lista? PREGUNTAR
    
    
    
    
    Sys.sleep(3)
    
  }, error = function(e) {
    #if (grepl("HTTP 429", e$message)) {
    # cat("Error 429 detectado en la oferta", i, ". Esperando 30 segundos antes de reintentar...\n")
      #Sys.sleep(30)
    #} else {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  }
  #}
  )
}

saveRDS(df_completo, "03_datos_limpios/df_datos_limpio.rds")


################################################################################
#                  Recomendación del profesor                                  #
################################################################################

#reestablecemos valores NA

df_datos_limpio[df_datos_limpio == "N/A"] <- NA

#Eliminamos las variables que por ahora no nos interesan

df_para_atlas <- df_datos_limpio%>%
  select(-c(descripción, salario_por_hora, fecha, empresa, ubicación, web,
            url_visitado, experiencia, nivel_educativo,puesto_de_trabajo, edad))

df_para_atlas <- as_tibble(df_para_atlas)

#Establecemos el formato de list para todas las observaciones

df_para_atlas <- df_para_atlas %>%
  mutate(habilidades = map(habilidades, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))%>%
  mutate(conocimientos_tecnicos = map(conocimientos_tecnicos, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))

df_para_atlas <- df_para_atlas %>%
  mutate(habilidades = map(habilidades, ~ flatten(.x))) %>%
  mutate(conocimientos_tecnicos = map(conocimientos_tecnicos, ~ flatten(.x)))


# Ahora se puede desanidar sin problemas
df_desanidado <- df_para_atlas %>%
  unnest(habilidades, keep_empty = TRUE) %>%
  unnest(conocimientos_tecnicos, keep_empty = TRUE)

#Guardamos .csv

#Eliminamos NA's

df_desanidado_clean <- df_desanidado %>%
  mutate(across(where(is.list), ~ sapply(., function(x) {
    if (is.null(x)) {
      NA_character_
    } else {
      paste(unlist(x), collapse = ", ")
    }
  })))

df_final_atlas <- df_desanidado_clean %>%
  pivot_longer(
    cols = c(habilidades, conocimientos_tecnicos), 
    names_to = "Origen",   
    values_to = "Requisito"  
  )%>%
  filter(!is.na(Requisito))

write.csv(df_final_atlas, "df_final_para_atlas.csv", row.names = FALSE)


datos_atlas <- read.csv("datos_atlas.csv")

datos_atlas <- datos_atlas%>%
  distinct(Requisito, .keep_all = TRUE)

################################################################################
#                         Establecer las categorias                            #
################################################################################

#Con las categorias extraidas del ATLAS conseguimos: Finanzas, Software especializados, 
#Planificacion estrategica, negociacion, Contabilidad y  Tributacion vamos a crear la nueva clasificacion de los datos


#creamos nuestro prompt

parte_1_prompt <- 'Actúa como un clasificador experto de descripciones laborales. Tu tarea es analizar la descripción laboral que se te proporciona (delimitada por *** al inicio y al final) y determinar cuáles de las siguientes categorías aplican, basándote en los requisitos y habilidades mencionadas. Si encuentras elementos correspondientes a una categoría, incluye el número asignado a dicha categoría en un arreglo. Las categorías son:

- Finanzas (7): Manejo de datos financieros, portafolios de inversión, análisis financiero, etc.
- Software especializados (6): Uso de herramientas como Excel, Eviews, programación, Power BI, Tableau, etc.
- Planificación estratégica (5): Toma de decisiones, planificación en costos, ventas, marketing, etc.
- Negociación (4): Resolución de problemas, liderazgo, atención a clientes o socios, etc.
- Contabilidad y Tributación (3): Recursos contables, declaración de impuestos y aspectos afines.

Instrucciones:
1. Analiza únicamente la descripción laboral proporcionada, que se encuentra entre los delimitadores ***.
2. Identifica todas las categorías aplicables.
3. Devuelve solo la salida en formato JSON, exactamente de la forma:  
   {"labels": [números]}
4. Si ninguna categoría aplica, devuelve {"labels": []}.
5. No incluyas ningún otro texto o explicación en la respuesta.

Ahora, analiza la siguiente descripción laboral y clasifícala en las categorías indicadas:
***' 
parte_2_prompt <- '*** unicamente vas a analizar y clasifícalar en las categorías indicadas el texto que esta entre el triple asterisco, no dejes sin clasificar ninguna descripcion laboral, si no logras clasificarlo la primera vez que analices el texto hazlo otra vez hasta que lo puedas clasificar. Recuerda no agregar nada mas ademas de lo que se te esta pidiendo a la respuesta'

#creamos nuestra df

df_completo <- bind_rows(df_1, df_2, df_3, df_5, df_6)
df_completo <- df_completo%>%
  rename(descripción = descripcción)%>%
  distinct (descripción, .keep_all = TRUE)%>%
  filter(!is.na(descripción)) %>%
  mutate(
    nivel_educativo = NA_character_,
    edad = NA_character_,
    experiencia = NA_character_,
    habilidades = NA_character_,
    conocimientos_tecnicos = NA_character_,
    ID = map_chr(descripción, rlang::hash)
  )%>%
  as_tibble()

df_completo <- df_completo%>%
 select(-c(salario_por_hora, fecha, empresa, ubicación, web, conocimientos_tecnicos, url_visitado, nivel_educativo, edad, experiencia, habilidades))%>%
  mutate(clasificación = NA_character_)
 



#cargamos datos para el funcionamiento de la api
gooai_studio_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
api_key <- "AIzaSyBJn8eAI6ypj3MY3fWemQHsXIeQ5gQlGOw"  

for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_completo$descripción[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(parte_1_prompt, datos_texto_oferta_de_trabajo, parte_2_prompt)
    
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
    
    # Extraer datos del JSON
    clasificacion <- datos_goo_respuesta$labels
    
    # Guardar los datos en df_completo
    df_completo$clasificación[i] <- list(clasificacion)
    
    
    
    
    Sys.sleep(3.5)
    
  }, error = function(e) {
    #if (grepl("HTTP 429", e$message)) {
    # cat("Error 429 detectado en la oferta", i, ". Esperando 30 segundos antes de reintentar...\n")
    #Sys.sleep(30)
    #} else {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  }
  #}
  )
}


df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))

df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, ~ flatten(.x)))

df_completo <- df_completo %>%
  unnest(clasificación, keep_empty = TRUE)

# Primero, creamos un dataframe para almacenar los resultados
df_recolectado <- df_completo %>%
  select(puesto_de_trabajo, ID) %>%
  mutate(
    Finanzas = FALSE,
    Software_especializados = FALSE,
    Planificación_estratégica = FALSE,
    Negociación = FALSE,
    Contabilidad_y_Tributación = FALSE
  )

# Iteramos sobre cada fila para asignar TRUE en la categoría correspondiente
for(i in 1:nrow(df_completo)){
  # Extraemos la clasificación (se asume que es una lista o vector de números)
  categorias <- df_completo$clasificación[[i]]
  
  # Si la fila tiene clasificación definida, evaluamos cada categoría
  if(!is.null(categorias)){
    if(7 %in% categorias) {
      df_recolectado$Finanzas[i] <- TRUE
    }
    if(6 %in% categorias) {
      df_recolectado$Software_especializados[i] <- TRUE
    }
    if(5 %in% categorias) {
      df_recolectado$Planificación_estratégica[i] <- TRUE
    }
    if(4 %in% categorias) {
      df_recolectado$Negociación[i] <- TRUE
    }
    if(3 %in% categorias) {
      df_recolectado$Contabilidad_y_Tributación[i] <- TRUE
    }
  }
}

saveRDS(df_recolectado, "03_datos_limpios/df_recolectado.rds")


################################################################################
#              Establecer las categorias para puestos                          #
################################################################################

parte_1_prompt <- 'Actúa como un clasificador experto de puestos laborales. Tu tarea es analizar el nombre de la oferta laboral que se te proporciona (delimitada por *** al inicio y al final) y determinar cuáles de las siguientes categorías aplican, basándote en los requisitos y habilidades mencionadas. Si encuentras elementos correspondientes a una categoría, incluye el número asignado a dicha categoría en un arreglo. Las categorías son:

- Finanzas (7): Manejo de datos financieros, portafolios de inversión, análisis financiero,Uso de herramientas como Excel, Eviews, programación, Power BI, Tableau, etc.
- Investigación (6): Encargarse de investigación acerca de temas económicos como inflación, creación de modelos econométricos, entre otros.
- Administrador (5): Toma de decisiones, planificación en costos, ventas, marketing, etc.
- Gerencia (4): Resolución de problemas, liderazgo, atención a clientes o socios, etc.
- Contabilidad y Tributación (3): Recursos contables, declaración de impuestos y aspectos afines.

Instrucciones:
1. Analiza únicamente el puesto laboral proporcionado, que se encuentra entre los delimitadores ***.
2. Identifica todas las categorías aplicables.
3. Devuelve solo la salida en formato JSON, exactamente de la forma:  
   {"labels": [números]}
4. Si ninguna categoría aplica, devuelve {"labels": []}.
5. No incluyas ningún otro texto o explicación en la respuesta.

Ahora, analiza la siguiente descripción laboral y clasifícala en las categorías indicadas:
***' 
parte_2_prompt <- '*** unicamente vas a analizar y clasifícalar en las categorías indicadas el texto que esta entre el triple asterisco, no dejes sin clasificar ninguna descripcion laboral, si no logras clasificarlo la primera vez que analices el texto hazlo otra vez hasta que lo puedas clasificar. Recuerda no agregar nada mas ademas de lo que se te esta pidiendo a la respuesta'

#creamos nuestra df

df_completo <- bind_rows(df_1, df_2, df_3, df_5, df_6)
df_completo <- df_completo%>%
  rename(descripción = descripcción)%>%
  distinct (descripción, .keep_all = TRUE)%>%
  filter(!is.na(descripción)) %>%
  mutate(
    nivel_educativo = NA_character_,
    edad = NA_character_,
    experiencia = NA_character_,
    habilidades = NA_character_,
    conocimientos_tecnicos = NA_character_,
    ID = map_chr(descripción, rlang::hash)
  )%>%
  as_tibble()

df_completo <- df_completo%>%
  select(-c(salario_por_hora, fecha, empresa, ubicación, web, conocimientos_tecnicos, url_visitado, nivel_educativo, edad, experiencia, habilidades, descripción))%>%
  mutate(clasificación = NA_character_)

#cargamos datos para el funcionamiento de la api
gooai_studio_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
api_key <- "AIzaSyBJn8eAI6ypj3MY3fWemQHsXIeQ5gQlGOw"  

for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_completo$puesto_de_trabajo[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(parte_1_prompt, datos_texto_oferta_de_trabajo, parte_2_prompt)
    
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
    
    # Extraer datos del JSON
    clasificacion <- datos_goo_respuesta$labels
    
    # Guardar los datos en df_completo
    df_completo$clasificación[i] <- list(clasificacion)
    
    
    
    
    Sys.sleep(3.5)
    
  }, error = function(e) {
    #if (grepl("HTTP 429", e$message)) {
    # cat("Error 429 detectado en la oferta", i, ". Esperando 30 segundos antes de reintentar...\n")
    #Sys.sleep(30)
    #} else {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  }
  #}
  )
}


df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))

df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, ~ flatten(.x)))

df_completo <- df_completo %>%
  unnest(clasificación, keep_empty = TRUE)


# Primero, creamos un dataframe para almacenar los resultados
df_recolectado <- df_completo %>%
  select(puesto_de_trabajo, ID) %>%
  mutate(
    Finanzas = FALSE,
    Investigación = FALSE,
    Administrador = FALSE,
    Gerencia = FALSE,
    Contabilidad_y_Tributación = FALSE,
    Otro = FALSE
    )

# Iteramos sobre cada fila para asignar TRUE en la categoría correspondiente
for(i in 1:nrow(df_completo)){
  # Extraemos la clasificación (se asume que es una lista o vector de números)
  categorias <- df_completo$clasificación[[i]]
  
  # Si la fila tiene clasificación definida, evaluamos cada categoría
  if(!is.null(categorias)){
    if(7 %in% categorias) {
      df_recolectado$Finanzas[i] <- TRUE
    }
    if(6 %in% categorias) {
      df_recolectado$Investigación[i] <- TRUE
    }
    if(5 %in% categorias) {
      df_recolectado$Administrador[i] <- TRUE
    }
    if(4 %in% categorias) {
      df_recolectado$Gerencia[i] <- TRUE
    }
    if(3 %in% categorias) {
      df_recolectado$Contabilidad_y_Tributación[i] <- TRUE
    }
    if(NA %in% categorias) {
      df_recolectado$Otro[i] <- TRUE
    }
  }
}


saveRDS(df_recolectado, "03_datos_limpios/df_puestos.rds")

################################################################################
#              Establecer las categorias para Experiencia                      #
################################################################################

parte_1_prompt <- 'Actúa como un clasificador experto de puestos laborales. Tu tarea es analizar si solicitan experiencia en la oferta laboral que se te proporciona (delimitada por *** al inicio y al final) y determinar cuáles de las siguientes categorías aplican, basándote en los requisitos y habilidades mencionadas. Si encuentras elementos correspondientes a una categoría, incluye el número asignado a dicha categoría en un arreglo. Las categorías son:

- Expereciencia de un año en adelante (7)
- Experiencia de 5 años en adelante (6)
- Experiencia de mas de 10 años (5)
- No especifíca si se necesita experiencia (4)


Instrucciones:
1. Analiza únicamente la descripción laboral proporcionado, que se encuentra entre los delimitadores ***.
2. Identifica todas las categorías aplicables.
3. Devuelve solo la salida en formato JSON, exactamente de la forma:  
   {"labels": [números]}
4. Si ninguna categoría aplica, devuelve {"labels": []}.
5. No incluyas ningún otro texto o explicación en la respuesta.

Ahora, analiza la siguiente descripción laboral y clasifícala en las categorías indicadas:
***' 
parte_2_prompt <- '*** unicamente vas a analizar y clasifícalar en las categorías indicadas el texto que esta entre el triple asterisco, no dejes sin clasificar ninguna descripcion laboral, si no logras clasificarlo la primera vez que analices el texto hazlo otra vez hasta que lo puedas clasificar. Recuerda no agregar nada mas ademas de lo que se te esta pidiendo a la respuesta'

#creamos nuestra df

df_completo <- bind_rows(df_1, df_2, df_3, df_5, df_6)
df_completo <- df_completo%>%
  rename(descripción = descripcción)%>%
  distinct (descripción, .keep_all = TRUE)%>%
  filter(!is.na(descripción)) %>%
  mutate(
    nivel_educativo = NA_character_,
    edad = NA_character_,
    experiencia = NA_character_,
    habilidades = NA_character_,
    conocimientos_tecnicos = NA_character_,
    ID = map_chr(descripción, rlang::hash)
  )%>%
  as_tibble()

df_completo <- df_completo%>%
  select(-c(salario_por_hora, fecha, empresa, ubicación, web, conocimientos_tecnicos, url_visitado, nivel_educativo, edad, experiencia, habilidades))%>%
  mutate(clasificación = NA_character_)




#cargamos datos para el funcionamiento de la api
gooai_studio_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
api_key <- "AIzaSyBJn8eAI6ypj3MY3fWemQHsXIeQ5gQlGOw"  

for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_completo$descripción[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(parte_1_prompt, datos_texto_oferta_de_trabajo, parte_2_prompt)
    
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
    
    # Extraer datos del JSON
    clasificacion <- datos_goo_respuesta$labels
    
    # Guardar los datos en df_completo
    df_completo$clasificación[i] <- list(clasificacion)
    
    
    
    
    Sys.sleep(3.5)
    
  }, error = function(e) {
    #if (grepl("HTTP 429", e$message)) {
    # cat("Error 429 detectado en la oferta", i, ". Esperando 30 segundos antes de reintentar...\n")
    #Sys.sleep(30)
    #} else {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  }
  #}
  )
}


df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))

df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, ~ flatten(.x)))

df_completo <- df_completo %>%
  unnest(clasificación, keep_empty = TRUE)


df_recolectado <- df_completo %>%
  select(puesto_de_trabajo, ID) %>%
  mutate(
    Experiencia_1_año = FALSE,
    Experiencia_5_años = FALSE,
    Experiencia_10_años = FALSE,
    No_especifica_experiencia = FALSE
  )

# Iteramos sobre cada fila para asignar TRUE en la categoría correspondiente
for(i in 1:nrow(df_completo)){
  # Extraemos la clasificación (se asume que es una lista o vector de números)
  categorias <- df_completo$clasificación[[i]]
  
  # Si la fila tiene clasificación definida, evaluamos cada categoría
  if(!is.null(categorias)){
    if(7 %in% categorias) {
      df_recolectado$Experiencia_1_año[i] <- TRUE
    }
    if(6 %in% categorias) {
      df_recolectado$Experiencia_5_años[i] <- TRUE
    }
    if(5 %in% categorias) {
      df_recolectado$Experiencia_10_años[i] <- TRUE
    }
    if(4 %in% categorias) {
      df_recolectado$No_especifica_experiencia[i] <- TRUE
    }
  }
}

saveRDS(df_recolectado, "03_datos_limpios/df_experiencia.rds")
################################################################################
#              Establecer las categorias para Experiencia                          #
################################################################################

parte_1_prompt <- 'Actúa como un clasificador experto de puestos laborales. Tu tarea es analizar que nivel educativo solicitan en la oferta laboral que se te proporciona (delimitada por *** al inicio y al final) y determinar cuáles de las siguientes categorías aplican, basándote en la descripción mencionada. Si encuentras elementos correspondientes a una categoría, incluye el número asignado a dicha categoría en un arreglo. Las categorías son:

- Estudiante de pregrado (7)
- Graduado de carrera de pregrado, por ejemplo como licenciatura (6)
- Graduado de postgrado (5)
- Graduado de Maestria o Doctorado (4)


Instrucciones:
1. Analiza únicamente la descripción laboral proporcionado, que se encuentra entre los delimitadores ***.
2. Identifica todas las categorías aplicables.
3. Devuelve solo la salida en formato JSON, exactamente de la forma:  
   {"labels": [números]}
4. Si ninguna categoría aplica, devuelve {"labels": []}.
5. No incluyas ningún otro texto o explicación en la respuesta.

Ahora, analiza la siguiente descripción laboral y clasifícala en las categorías indicadas:
***' 
parte_2_prompt <- '*** unicamente vas a analizar y clasifícalar en las categorías indicadas el texto que esta entre el triple asterisco, no dejes sin clasificar ninguna descripcion laboral, si no logras clasificarlo la primera vez que analices el texto hazlo otra vez hasta que lo puedas clasificar. Recuerda no agregar nada mas ademas de lo que se te esta pidiendo a la respuesta'

#creamos nuestra df

df_completo <- bind_rows(df_1, df_2, df_3, df_5, df_6)
df_completo <- df_completo%>%
  rename(descripción = descripcción)%>%
  distinct (descripción, .keep_all = TRUE)%>%
  filter(!is.na(descripción)) %>%
  mutate(
    nivel_educativo = NA_character_,
    edad = NA_character_,
    experiencia = NA_character_,
    habilidades = NA_character_,
    conocimientos_tecnicos = NA_character_,
    ID = map_chr(descripción, rlang::hash)
  )%>%
  as_tibble()

df_completo <- df_completo%>%
  select(-c(salario_por_hora, fecha, empresa, ubicación, web, conocimientos_tecnicos, url_visitado, nivel_educativo, edad, experiencia, habilidades))%>%
  mutate(clasificación = NA_character_)




#cargamos datos para el funcionamiento de la api
gooai_studio_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
api_key <- "AIzaSyBJn8eAI6ypj3MY3fWemQHsXIeQ5gQlGOw"  

for (i in 1:ofertas_extraidas) {
  
  cat("Procesando oferta", i, "de", ofertas_extraidas, "\n")
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_completo$descripción[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(parte_1_prompt, datos_texto_oferta_de_trabajo, parte_2_prompt)
    
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
    
    # Extraer datos del JSON
    clasificacion <- datos_goo_respuesta$labels
    
    # Guardar los datos en df_completo
    df_completo$clasificación[i] <- list(clasificacion)
    
    
    
    
    Sys.sleep(3.5)
    
  }, error = function(e) {
    #if (grepl("HTTP 429", e$message)) {
    # cat("Error 429 detectado en la oferta", i, ". Esperando 30 segundos antes de reintentar...\n")
    #Sys.sleep(30)
    #} else {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  }
  #}
  )
}


df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))

df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, ~ flatten(.x)))

df_completo <- df_completo %>%
  unnest(clasificación, keep_empty = TRUE)


df_recolectado <- df_completo %>%
  select(puesto_de_trabajo, ID) %>%
  mutate(
    Estudiante_de_pregrado = FALSE,
    Pregrado = FALSE,
    Postgrado = FALSE,
    Maestria_o_Doctorado = FALSE,
    No_especifíca = FALSE
    )

# Iteramos sobre cada fila para asignar TRUE en la categoría correspondiente
for(i in 1:nrow(df_completo)){
  # Extraemos la clasificación (se asume que es una lista o vector de números)
  categorias <- df_completo$clasificación[[i]]
  
  # Si la fila tiene clasificación definida, evaluamos cada categoría
  if(!is.null(categorias)){
    if(7 %in% categorias) {
      df_recolectado$Estudiante_de_pregrado[i] <- TRUE
    }
    if(6 %in% categorias) {
      df_recolectado$Pregrado[i] <- TRUE
    }
    if(5 %in% categorias) {
      df_recolectado$Postgrado[i] <- TRUE
    }
    if(4 %in% categorias) {
      df_recolectado$Maestria_o_Doctorado[i] <- TRUE
    } 
    if(NA %in% categorias) {
      df_recolectado$No_especifíca[i] <- TRUE
    }
  }
}

saveRDS(df_recolectado, "03_datos_limpios/df_educacion.rds")


################################################################################
#              Establecer las categorias de la encuesta                        #
################################################################################


df_completo <- read_excel("01_datos_sin_limpiar/Experiencia_con_el_Mercado_Laboral_de_Economistas_(Respuestas).xlsx") 
df_completo <- df_completo%>%
  as.tibble()

parte_1_prompt <- 'Actúa como un clasificador experto de puestos laborales. Tu tarea es analizar el nombre de la oferta laboral que se te proporciona (delimitada por *** al inicio y al final) y determinar cuáles de las siguientes categorías aplican, basándote en los requisitos y habilidades mencionadas. Si encuentras elementos correspondientes a una categoría, incluye el número asignado a dicha categoría en un arreglo. Las categorías son:

- Finanzas (7): Manejo de datos financieros, portafolios de inversión, análisis financiero,Uso de herramientas como Excel, Eviews, programación, Power BI, Tableau, etc.
- Investigación (6): Encargarse de investigación acerca de temas económicos como inflación, creación de modelos econométricos, entre otros.
- Administrador (5): Toma de decisiones, planificación en costos, ventas, marketing, etc.
- Gerencia (4): Resolución de problemas, liderazgo, atención a clientes o socios, etc.
- Contabilidad y Tributación (3): Recursos contables, declaración de impuestos y aspectos afines.

Instrucciones:
1. Analiza únicamente el puesto laboral proporcionado, que se encuentra entre los delimitadores ***.
2. Identifica todas las categorías aplicables.
3. Devuelve solo la salida en formato JSON, exactamente de la forma:  
   {"labels": [números]}
4. Si ninguna categoría aplica, devuelve {"labels": []}.
5. No incluyas ningún otro texto o explicación en la respuesta.

Ahora, analiza la siguiente descripción laboral y clasifícala en las categorías indicadas:
***' 
parte_2_prompt <- '*** unicamente vas a analizar y clasifícalar en las categorías indicadas el texto que esta entre el triple asterisco, no dejes sin clasificar ninguna descripcion laboral, si no logras clasificarlo la primera vez que analices el texto hazlo otra vez hasta que lo puedas clasificar. Recuerda no agregar nada mas ademas de lo que se te esta pidiendo a la respuesta'


#cargamos datos para el funcionamiento de la api
gooai_studio_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
api_key <- "AIzaSyBJn8eAI6ypj3MY3fWemQHsXIeQ5gQlGOw"  

for (i in 1:10) {
  
  cat("Procesando oferta", i, "de", 13, "\n")
  
  tryCatch({
    datos_texto_oferta_de_trabajo <- df_completo$`¿Qué conocimientos o habilidades crees que deberían incluirse en la formación de los economistas para adaptarse mejor al mercado laboral actual?`[i]
    
    
    dato_texto_prompt_gooaistudio <- paste(parte_1_prompt, datos_texto_oferta_de_trabajo, parte_2_prompt)
    
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
    
    # Extraer datos del JSON
    clasificacion <- datos_goo_respuesta$labels
    
    # Guardar los datos en df_completo
    df_completo$clasificación[i] <- list(clasificacion)
    
    
    
    
    Sys.sleep(3.5)
    
  }, error = function(e) {
    #if (grepl("HTTP 429", e$message)) {
    # cat("Error 429 detectado en la oferta", i, ". Esperando 30 segundos antes de reintentar...\n")
    #Sys.sleep(30)
    #} else {
    cat("Error en la oferta", i, ": ", conditionMessage(e), "\n")
  }
  #}
  )
}


df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, function(x) {
    if (!is.list(x)) {
      list(x)
    } else {
      x
    }
  }))

df_completo <- df_completo%>%
  mutate(clasificación = map(clasificación, ~ flatten(.x)))

df_completo <- df_completo %>%
  unnest(clasificación, keep_empty = TRUE)

# Primero, creamos un dataframe para almacenar los resultados
df_recolectado <- df_completo %>%
  select(`Nombre y Apellido`) %>%
  mutate(
    Finanzas = FALSE,
    Software_especializados = FALSE,
    Planificación_estratégica = FALSE,
    Negociación = FALSE,
    Contabilidad_y_Tributación = FALSE
  )

# Iteramos sobre cada fila para asignar TRUE en la categoría correspondiente
for(i in 1:nrow(df_completo)){
  # Extraemos la clasificación (se asume que es una lista o vector de números)
  categorias <- df_completo$clasificación[[i]]
  
  # Si la fila tiene clasificación definida, evaluamos cada categoría
  if(!is.null(categorias)){
    if(7 %in% categorias) {
      df_recolectado$Finanzas[i] <- TRUE
    }
    if(6 %in% categorias) {
      df_recolectado$Software_especializados[i] <- TRUE
    }
    if(5 %in% categorias) {
      df_recolectado$Planificación_estratégica[i] <- TRUE
    }
    if(4 %in% categorias) {
      df_recolectado$Negociación[i] <- TRUE
    }
    if(3 %in% categorias) {
      df_recolectado$Contabilidad_y_Tributación[i] <- TRUE
    }
  }
}

saveRDS(df_recolectado, "03_datos_limpios/df_encuestas.rds")
