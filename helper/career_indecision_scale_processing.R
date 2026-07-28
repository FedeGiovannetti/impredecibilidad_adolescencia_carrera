
career_indecision_items_dict <- c(
  "CIP_01_decision" = "(CIP SHORT 1)",
  "CIP_02" = "(CIP SHORT 2)",
  
  
  "CIP_CC01" = '(CC1)',
  "CIP_CC02" = '(CC2)',
  "CIP_CC03" = '(CC3)',
  "CIP_CC04" = '(CC4)',
  "CIP_CC05" = '(CC5)',
  
  "CIP_LR01" = '(LR1)',
  "CIP_LR02" = '(LR2)',                                                           
  "CIP_LR03" = '(LR3)',                     
  "CIP_LR04" = '(LR4)',                                     
  "CIP_LR05" = '(LR5)',
  
  "CIP_IC01"  = '(IC1)',
  "CIP_IC02"  = '(IC2)',    
  "CIP_IC03"  = '(IC3)',                             
  "CIP_IC04"  = '(IC4)',                
  "CIP_IC05"  = '(IC5)',
  
  "CIP_NNA01" = '(NNA1)',
  "CIP_NNA02" = '(NNA2)',                                               
  "CIP_NNA03" = '(NNA3)',                             
  "CIP_NNA04" = '(NNA4)',                                       
  "CIP_NNA05" = '(NNA5)'
)


career_indecision_processing <- function(data){
  
  # Extract exact colnames
  exact_matches <- career_indecision_items_dict %>% 
    map_chr(~ str_subset(names(data), fixed(.x))[1]) #%>% 
    # na.omit()
  
  # Extract relevant columns from data
  processed_data <<- data %>%
    rename(any_of(exact_matches)) %>%
    mutate(across(starts_with(c("CIP_CC","CIP_LR", "CIP_IC", "CIP_NNA")), ~ case_when(
      .x == "Muy en desacuerdo" ~ 1,
      .x == "Muy en desacuerdo." ~ 1,
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
    mutate(CIP_CC_score = CIP_CC01 + CIP_CC02 + CIP_CC03 + CIP_CC04 + CIP_CC05,
           CIP_LR_score = CIP_LR01 + CIP_LR02 + CIP_LR03 + CIP_LR04 + CIP_LR05,
           CIP_IC_score = CIP_IC01 + CIP_IC02 + CIP_IC03 + CIP_IC04 + CIP_IC05,
           CIP_NNA_score = CIP_NNA01 + CIP_NNA02 + CIP_NNA03 + CIP_NNA04 + CIP_NNA05) %>%

    select(ID, contains("CIP_")) %>%
    pivot_longer(cols = -ID, names_to = "item_name",
                 values_to = "value")


  return(processed_data)

}



career_indecision_items <- function(data){
  
  # Extract exact colnames
  processed_items <- career_indecision_items_dict %>% 
    map_chr(~ str_subset(names(data), fixed(.x))[1]) %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("item_name") %>% 
    rename("item_text" = ".") 
  
  
  return(processed_items)
  
}

