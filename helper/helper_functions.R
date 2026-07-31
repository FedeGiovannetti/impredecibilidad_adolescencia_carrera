        
correlation_table <- function(data){
  
  correlations = data %>%
    psych::corr.test(.)
  
  
  p_correlations = correlations$p %>%
    as.data.frame() %>%
    rownames_to_column("variable_1") %>%
    pivot_longer(cols = -variable_1,
                 names_to = "variable_2",
                 values_to = "p.value") %>%
    mutate(p.value = round(p.value, digits = 5))
  
  
  r_correlations = correlations$r %>% 
    as.data.frame() %>% 
    rownames_to_column("variable_1") %>% 
    pivot_longer(cols = -variable_1,
                 names_to = "variable_2",
                 values_to = "r")
  
  
  correlation_table <- left_join(r_correlations, p_correlations, by = c("variable_1", "variable_2")) %>% 
    filter(duplicated(r)) %>% 
    filter(r != 1,
           p.value != 1)
  
  return(correlation_table)
  
  
}



mean_sd <- function(variable){
  
  paste(
    round(
      mean(variable, na.rm = TRUE),
      digits = 2),
    " (",
    
    round(
      sd(variable, na.rm = TRUE),
      digits = 2),
    ")",
    
    sep = "")
  
}
