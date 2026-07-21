
career_indecision_processing <- function(data){
  
  # Extract relevant columns from data
  processed_data <-data %>% 
    mutate(across(starts_with("CIP_CC"), ~ case_when(
      .x == "Muy en desacuerdo" ~ 1,
      .x == "Moderadamente en desacuerdo" ~ 2,
      .x == "Levemente en desacuerdo" ~ 3,
      .x == "Moderadamente de acuerdo" ~ 4,
      .x == "Muy de acuerdo" ~ 5,
      TRUE ~ NA_real_
    ))) %>% 
    mutate(across(starts_with("CIP_01"), ~ case_when(
      .x == "Completamente indeciso" ~ 1,
      .x == "Moderadamente indeciso" ~ 2,
      .x == "Levemente indeciso" ~ 3,
      .x == "Levemente decidido" ~ 4,
      .x == "Moderadamente decidido" ~ 5,
      .x == "Muy decidido" ~ 6,
      TRUE ~ NA_real_
    ))) %>% 
    mutate(across(starts_with("CIP_02"), ~ case_when(
      .x == "Sin importancia" ~ 1,
      .x == "Moderadamente sin importancia" ~ 2,
      .x == "Levemente sin importancia" ~ 3,
      .x == "Levemente importante" ~ 4,
      .x == "Moderadamente importante" ~ 5,
      .x == "Muy importante" ~ 6,
      TRUE ~ NA_real_
    ))) %>% 
    mutate(CIP_CC_score = CIP_CC01 + CIP_CC02 + CIP_CC03 + CIP_CC04 + CIP_CC05) %>% 
    pivot_longer(cols = -ID, names_to = "item_name",
                 values_to = "value")
    
  
  return(processed_data)
  
}



career_indecision_items <- function(data){
  
  questionnaire_items <- data.frame(
    "CIP_01_decision" = "6. ¿Qué tan decidido te sentís actualmente sobre tu futuro (carrera). (CIP SHORT 1)",
    "CIP_02" = "7. ¿Qué tan importante es tomar o rehacer una decisión sobre tu carrera en este momento? (CIP SHORT 2)",
  
  
    "CIP_CC01" = '8. A menudo me siento desanimado por tener que tomar una decisión sobre mi carrera" (CC1)',
    "CIP_CC02" = '9. Debo conocer más sobre mí mismo antes de tomar una decisión sobre mi carrera (CC2)',
    "CIP_CC03" = '10. "Es difícil para mí elegir una carrera porque me gustan muchas cosas". (CC3)',
    "CIP_CC04" = '11. "No estoy seguro de poder comprometerme a una carrera específica porque no sé qué otras opciones pueden haber". (CC4)',
    "CIP_CC05" = '12. "Me preocupa que mis metas puedan cambiar después de elegir una carrera." (CC5)') %>% 
    
    pivot_longer(cols = everything(),
                 names_to = "item_name",
                 values_to = "item_text" )
  
  
  return(questionnaire_items)
  
}

