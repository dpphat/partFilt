#' visc_wat Function
#' This function calculates the viscosity of water for a given temperature (°C). The units of viscosity are g/cm/s
#' @param TEMP porosity. Defaults to a value of 25°C.
#' @keywords filtration, temperature, viscosity
#' @export
#' @examples
#' visc_wat(TEMP = 25)
#' [1] 0.0089044

visc_wat <- function(TEMP = 25){
            TEMP_K <- TEMP + 273.15
            kg2g <- 1000
            m2cm <- 100
            val <- 2.414 * 10^-5 * 10^(247.8 / (TEMP_K - 140)) * kg2g / m2cm  # g/cm/s
            return(val)
            }