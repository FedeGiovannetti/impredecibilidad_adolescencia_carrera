
library(dplyr)



clean_colnames <- function(data){
  
  subject_info_columns <- c("gender" = "Género",
                            "birth_date" = "Fecha de nacimiento")
  
  home_environment_columns <- c("max_educ_level_mother" = "Máximo nivel de estudios alcanzado por tu madre",
                                "max_educ_level_father" = "Máximo nivel de estudios alcanzado por tu padre")

  
  renamed_data <- data %>% 
    rename(any_of(subject_info_columns)) %>% 
    rename(any_of(home_environment_columns))%>% 
    # rename(any_of(CIP_columns)) %>% 
    rename_with(~ "society_ladder", contains("Imagine que esta escalera")) 
    
  
  return(renamed_data)
  
  
}


