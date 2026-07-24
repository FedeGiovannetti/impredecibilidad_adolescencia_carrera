
orientacion_vocacional_processing_cuanti <- function(data){
  
  # Extract relevant columns from data
  processed_data <- data %>% 
    select(ID,
           "1.a. ¿Cuán probable es que continúes los estudios superiores al terminar la secundaria?",                               
           # "1.b Nombrá 3 motivos por los cuáles creés que SÍ podrías continuar los estudios superiores al terminar la secundaria.", 
           # "1.C. Nombrá 3 motivos por los cuáles creés que NO podrías continuar los estudios superiores al terminar la secundaria.",
           # "2.a. Al finalizar la secundaria consideras:",                                                                           
           # "2.b Nombrá 3 motivos de tu decisión.",                                                                                  
           '3. En una escala del 1 al 10 ¿Cuán identificado te sentís con la frase: ”Siento que estudiar vale la pena”',            
           "4. ¿Considerás necesario trabajar inmediatamente al egresar?",
           # "5. ¿Sentís que tenés suficiente información para elegir una carrera o trabajo?",
           
           "35.a ¿Hasta qué punto creés que sea probable que tengas que cambiar tus planes de carrera y/o trabajo a futuro?",                                                                    
           # "35.b Nombrá 3 motivos por los cuales pensás que podrían cambiar o no tus planes a futuro",                                                                                           
           "36 ¿Cuán probable es que elijas una carrera que no te gusta tanto, pero que te dé estabilidad económica?",                                                                           
           "37. ¿Cuánto sentís que influye la situación social y económica del país en tu decisión vocacional/laboral?",                                                                         
           # "38. A la hora de pensar en tu futura carrera y/o trabajo, ¿Tenés en cuenta las materias de la escuela secundaria (es decir si debes materias y cuánto tiempo te tomaría rendirlas)?",
           '39. ¿En una escala del 1 al 5 (siendo 1 nada de acuerdo y 5 totalmente de acuerdo): “Creo que el esfuerzo personal es suficiente para lograr mis objetivos”',                        
           "40 ¿Cuán probable es que puedas sostener económicamente estudiar una carrera?"                                                                                                      
           # '41. ¿Cuán de acuerdo estás con la siguiente frase? “Las oportunidades dependen más del contexto que de la persona\"',                                                                
           # "42 ¿Alguna vez cambiaste de idea sobre tu futuro? ¿Por qué? (Breve explicación)"      
           ) %>% 
    tibble::column_to_rownames("ID") %>% 
    mutate(across(where(is.character), ~ case_when(
      .x == "Si" ~ 1,
      .x == "Sí" ~ 1,
      .x == "No" ~ 0,
      TRUE ~ NA_real_
    ))) %>%
    tibble::rownames_to_column("ID") %>% 
    pivot_longer(cols = -ID, names_to = "item_text", values_to = "response") %>%

    mutate(item_name = replace_when(item_text,
                                  item_text == "1.a. ¿Cuán probable es que continúes los estudios superiores al terminar la secundaria?" ~ "probable_continuar_estudios",
                                  # item_text = "1.b Nombrá 3 motivos por los cuáles creés que SÍ podrías continuar los estudios superiores al terminar la secundaria." ~ motivos_si_continuar_estudios,
                                  # item_text = "1.C. Nombrá 3 motivos por los cuáles creés que NO podrías continuar los estudios superiores al terminar la secundaria." ~ motivos_no_continuar_estudios,
                                  # item_text = "2.a. Al finalizar la secundaria consideras:" ~ al_finalizar,
                                  # item_text = "2.b Nombrá 3 motivos de tu decisión." ~ motivos_decision,
                                  item_text == '3. En una escala del 1 al 10 ¿Cuán identificado te sentís con la frase: ”Siento que estudiar vale la pena”' ~ "estudiar_vale_la_pena",
                                  item_text == "4. ¿Considerás necesario trabajar inmediatamente al egresar?" ~ "considera_trabajar",
                                  # item_text = "5. ¿Sentís que tenés suficiente información para elegir una carrera o trabajo?" ~ suficiente_info,

                                  item_text == "35.a ¿Hasta qué punto creés que sea probable que tengas que cambiar tus planes de carrera y/o trabajo a futuro?"  ~"probabilidad_cambiar_planes",
                                  # item_text = "35.b Nombrá 3 motivos por los cuales pensás que podrían cambiar o no tus planes a futuro" ~ motivos_cambiar_planes,
                                  item_text == "36 ¿Cuán probable es que elijas una carrera que no te gusta tanto, pero que te dé estabilidad económica?" ~ "probabilidad_carrera_no_gusta",
                                  item_text == "37. ¿Cuánto sentís que influye la situación social y económica del país en tu decisión vocacional/laboral?" ~ "influencia_situacion_pais",
                                  # item_text = "38. A la hora de pensar en tu futura carrera y/o trabajo, ¿Tenés en cuenta las materias de la escuela secundaria (es decir si debes materias y cuánto tiempo te tomaría rendirlas)?" ~materias_adeudaddas,
                                  item_text == '39. ¿En una escala del 1 al 5 (siendo 1 nada de acuerdo y 5 totalmente de acuerdo): “Creo que el esfuerzo personal es suficiente para lograr mis objetivos”' ~ "esfuerzo_es_suficiente",
                                  item_text == "40 ¿Cuán probable es que puedas sostener económicamente estudiar una carrera?" ~ "sosten_economico_carrera"
                                  # item_text = '41. ¿Cuán de acuerdo estás con la siguiente frase? “Las oportunidades dependen más del contexto que de la persona\"' ~ oportunidades_dependen_contexto,
                                  # item_text = "42 ¿Alguna vez cambiaste de idea sobre tu futuro? ¿Por qué? (Breve explicación)" ~ cambio_idea_futuro
      )
    ) %>%
    select(ID, item_name, item_text, response)

  return(processed_data)
  
}



  

