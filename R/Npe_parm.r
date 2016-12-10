#' Npe_parm Function
#' This function calculates the dimensionless Peclet number.
#' @param Kx Defaults to a value of 0.02 cm/s
#' @param GRAD Defaults to a value of 0.0007 
#' @param POR Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @param K_units Units of hydraulic conductivity. Defaults to cm/s but also accepts ft/day. Units should be surrounded
#' by quotation marks to ensure it is treated as a string of text.
#' @param TEMP Temperature in units of °C. Defaults to a value of 25°C
#' @param DIA_SUSP Diameter of the suspended solids in units of cm
#' @param DIA_SOIL Diameter of the soil particles in units of cm
#' @keywords filtration, peclet
#' @export
#' @examples
#' Npe_parm(DIA_SOIL = c(0.001, 0.01, 0.1, 1))
#' [1]   0.5436584   5.4365837  54.3658370 543.6583699

Npe_parm <- function(Kx = 0.02,             # cm/s
                     GRAD = 0.0007,         # cm/cm
                     POR = 0.21,            # cm³/cm³
                     K_units = "cm/s", 
                     TEMP = 25,             # °C
                     DIA_SUSP = 1e-4,       # cm
                     DIA_SOIL = 0.053       # cm
                     ){
                     TEMP_K <- TEMP + 273.15
                     m2_to_cm2 <- 100^2
                     kg2g <- 1000
                     BOLTZ <- 1.38064852 * 10^-23  # cm²kg/s²K
                     
                     DEN <- BOLTZ * m2_to_cm2 * kg2g * TEMP_K
                     val <- 12 * pi * visc_wat(TEMP = TEMP) * 
                                      DIA_SUSP * DIA_SOIL * 
                                      U_parm(Kx = Kx, GRAD = GRAD, POR = POR, K_units = K_units) / DEN
                     return(val)
                     }