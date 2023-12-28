#custom min and max functions to avoid unnecessary warning for zero length vector
custom_min <- function(x, na.rm = T) {if_else(length(x)>0, min(x, na.rm = na.rm), NA)}
custom_max <- function(x, na.rm = T) {if_else(length(x)>0, max(x, na.rm = na.rm), NA)}
#Define function that takes the min of two elements (Vectorized)
Vmin <- function(a, b){
  return(if_else(!is.na(a) & !is.na(b) & a < b, a, b))
}
Vmax <- function(a, b){
  return(if_else(!is.na(a) & !is.na(b) & a < b, b, a))
}
