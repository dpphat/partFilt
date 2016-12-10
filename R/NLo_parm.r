#' NLo_parm Function
#' This function calculates the dimensionless London Van-Der Walls force number.
#' @param Kx Defaults to a value of 0.02 cm/s
#' @param GRAD Defaults to a value of 0.0007 
#' @param POR Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @param K_units Units of hydraulic conductivity. Defaults to cm/s but also accepts ft/day. Units should be surrounded
#' by quotation marks to ensure it is treated as a string of text.
#' @param TEMP Temperature in units of °C. Defaults to a value of 25°C
#' @param DIA_SUSP Diameter of the suspended solids in units of cm
#' @keywords filtration, Van-Der Walls
#' @export
#' @examples
#'  NLo_parm()
#' [1] 0.0002383168

NLo_parm <- function(Kx = 0.02,             # cm/s
                     GRAD = 0.0007,         # cm/cm
                     POR = 0.21,            # cm³/cm³
                     K_units = "cm/s", 
                     TEMP = 25,             # °C
                     DIA_SUSP = 1e-4        # cm
                     ){
                     HAM <- 10^-20          # Joules (kg*m²/s²)
                     sqm_to_sqcm <- 100^2
                     kg2g <- 1000
                     RAD_SUSP <- DIA_SUSP / 2
                     DEN <- 9 * pi * visc_wat(TEMP) * RAD_SUSP^2 * U_parm(Kx = Kx, GRAD = GRAD, POR = POR, K_units = K_units)
                     val <- HAM * sqm_to_sqcm * kg2g / DEN
                     return(val)
                     }