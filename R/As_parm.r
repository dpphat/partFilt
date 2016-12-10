#' As_parm Function
#' This function calculates the As-parameter from Table 1 of Tobiason and O'Melia (1988). The As-parameter is based on the p-parameter.
#' @param POR porosity. Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @keywords filtration, porosity, As-parameter
#' @export
#' @examples
#' As_parm(POR = 0.3)
#' [1] 75.49263
#' 
#' As_parm(POR = 21)
#' [1] "Error: Porosity value must be less than 1"

As_parm <- function(POR = 0.21){
           num <- 2 * (1 - p_parm(POR = POR)^5)
           den <- 2 - 3 * p_parm(POR = POR) + 3 * p_parm(POR = POR)^5 - 2 * p_parm(POR = POR)^6
           val <- num / den
           ifelse(POR < 1, return(val), print("Error: Porosity value must be less than 1"))
           }