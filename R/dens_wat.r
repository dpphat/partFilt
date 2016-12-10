#' dens_wat Function
#' This function calculates the density of water at the specified temperature (°C). The density units are g / cm3.
#' @param TEMP Temperature in units of °C. Defaults to a value of 25°C
#' @keywords filtration, Density of Water
#' @export
#' @examples
#' dens_wat()
#' [1] 0.99708

dens_wat <- function(TEMP = 25){
            val <- 1000 * (1 - (TEMP + 288.9414) / (508929.2 * (TEMP + 68.12963)) * (TEMP - 3.9863)^2) / 1000
            return(val)
            }