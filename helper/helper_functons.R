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
