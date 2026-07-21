
library(dplyr)



clean_colnames <- function(data){
  
  subject_info_columns <- c("gender" = "Género",
                            "birth_date" = "Fecha de nacimiento")
  
  home_environment_columns <- c("max_educ_level_mother" = "Máximo nivel de estudios alcanzado por tu madre",
                                "max_educ_level_father" = "Máximo nivel de estudios alcanzado por tu padre")
  
  CIP_columns <- c("CIP_01_decision" = "6. ¿Qué tan decidido te sentís actualmente sobre tu futuro (carrera). (CIP SHORT 1)",
                   "CIP_02" = "7. ¿Qué tan importante es tomar o rehacer una decisión sobre tu carrera en este momento? (CIP SHORT 2)",
                   
                   
                   "CIP_CC01" = '8. A menudo me siento desanimado por tener que tomar una decisión sobre mi carrera" (CC1)',
                   "CIP_CC02" = '9. Debo conocer más sobre mí mismo antes de tomar una decisión sobre mi carrera (CC2)',
                   "CIP_CC03" = '10. "Es difícil para mí elegir una carrera porque me gustan muchas cosas". (CC3)',
                   "CIP_CC04" = '11. "No estoy seguro de poder comprometerme a una carrera específica porque no sé qué otras opciones pueden haber". (CC4)',
                   "CIP_CC05" = '12. "Me preocupa que mis metas puedan cambiar después de elegir una carrera." (CC5)'
  )
  
  
  renamed_data <- data %>% 
    rename(any_of(subject_info_columns)) %>% 
    rename(any_of(home_environment_columns))%>% 
    rename(any_of(CIP_columns)) %>% 
    rename_with(~ "society_ladder", contains("Imagine que esta escalera")) 
    
  
  return(renamed_data)
  
  
}


