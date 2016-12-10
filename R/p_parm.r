#' p_parm Function
#' This function calculates the p-parameter from Table 1 of Tobiason and O'Melia (1988). The p-parameter is based on soil porosity.
#' @param POR porosity. Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @keywords filtration, porosity, p-parameter
#' @export
#' @examples
#' p_parm(POR = 0.3)
#' [1] 0.887904
#' 
#' p_parm(POR = 21)
#' [1] "Error: Porosity value must be less than 1"

p_parm <- function(POR = 0.21){
          val <- (1 - POR)^(1 / 3)
          ifelse(POR < 1, return(val), print("Error: Porosity value must be less than 1"))
          }