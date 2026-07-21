
unpredictability_future_processing <- function(data){
  
  # Extract relevant columns from data
  raw_data %>% 
    select(ID,`1. ¿Hasta qué punto creés que sabes cómo será tu vida el año que viene?`,
           `2. ¿Hasta qué punto creés que podes predecir tu situación futura a nivel general?`,
           `3. ¿Hasta qué punto creés que podés predecir tu situación futura en términos económicos?`, 
           `4. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto al trabajo?`,
           `5. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus relaciones familiares?`,
           `6. ¿Hasta qué punto creés que podés predecir tu situación futura en cuanto a tus amistades?`) %>% 
    pivot_longer(cols = -ID, names_to = "complete_question", values_to = "response") %>% 
    
    # Separate item_number and item_text
    separate_wider_regex(
      complete_question,
      patterns = c(
        item_number = "^\\d+",    
        "\\. ",           
        item_text = ".*"       
      )
    )
  
}

