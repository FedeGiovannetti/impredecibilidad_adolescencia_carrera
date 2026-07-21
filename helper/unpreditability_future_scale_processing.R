
unpredictability_future_processing <- function(data){
  
  # Extract relevant columns from data
  processed_data <- data %>% 
    select(ID,`28. ¿Hasta qué punto creés que sabes cómo será tu vida el año que viene?`,
           `29. ¿Hasta qué punto creés que podes predecir tu situación futura a nivel general?`,
           `30. ¿Hasta qué punto creés que podés predecir tu situación futura en términos económicos?`, 
           `31. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus futuros estudios?`, 
           `32. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto al trabajo?`,
           `33. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus relaciones familiares?`,
           `34. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus amistades?`) %>% 
    pivot_longer(cols = -ID, names_to = "complete_question", values_to = "response") %>% 
    
    # Separate item_number and item_text
    separate_wider_regex(
      complete_question,
      patterns = c(
        item_number = "^\\d+",    
        "\\. ",           
        item_text = ".*"       
      ) 
    ) %>% 
    mutate(item_name = replace_when(item_text,
      item_text ==  "¿Hasta qué punto creés que sabes cómo será tu vida el año que viene?" ~ "unpred_life_1_year",
      item_text ==  "¿Hasta qué punto creés que podes predecir tu situación futura a nivel general?" ~ "unpred_future_general",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
      item_text ==  "¿Hasta qué punto creés que podés predecir tu situación futura en términos económicos?" ~ "unpred_future_economy",  
      item_text ==  "¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus futuros estudios?"  ~ "unpred_future_study",  
      item_text ==  "¿Hasta qué punto creés que podés predecir tu situación futura en cuanto al trabajo?"  ~ "unpred_future_work",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
      item_text ==  "¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus relaciones familiares?"  ~ "unpred_future_family",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
      item_text ==  "¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus amistades?"  ~ "unpred_future_friends"
      )
    ) %>% 
    select(ID, item_number, item_name, item_text, response) %>% 
    mutate(response_inverted = 1 + 10 - response)
  
  return(processed_data)
  
}



unpreditability_future_items <- function(data){
  
  # This receives the long-format df created by unpreditability_future_processing
  # and extract the distinct information of each item
  
  questionnaire_items <- unpredictability_future_processing(data) %>% 
    select(-ID, -response, -response_inverted) %>% 
    distinct()
  
  return(questionnaire_items)
  
}

