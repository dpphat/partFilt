#' Nr_parm Function
#' This function calculates the ratio of the suspended particle to soil particle size ratio
#' @param DIA_SUSP Diameter of the suspended solids in units of cm
#' @param DIA_SOIL Diameter of the soil particles in units of cm
#' @keywords filtration, Ratio of particle sizes
#' @export
#' @examples
#' Nr_parm()
#' [1] 1.886792e-05

Nr_parm <- function(DIA_SUSP = 1e-4,       # cm
                    DIA_SOIL = 0.053       # cm
                    ){
                    val <- DIA_SUSP / DIA_SOIL
                    return(val)
                    }