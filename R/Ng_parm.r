#' Ng_parm Function
#' This function calculates the ratio of the Stokes' settling velocity of the particle to the superficial filtration velocity
#' @param Kx Defaults to a value of 0.02 cm/s
#' @param GRAD Defaults to a value of 0.0007 
#' @param POR Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @param K_units Units of hydraulic conductivity. Defaults to cm/s but also accepts ft/day. Units should be surrounded
#' by quotation marks to ensure it is treated as a string of text.
#' @param TEMP Temperature in units of °C. Defaults to a value of 25°C
#' @param DIA_SUSP Diameter of the suspended solids in units of cm
#' @param DIA_SOIL Diameter of the soil particles in units of cm
#' @param PART_DENS Density of the suspended particle in units of g/cm³
#' @keywords filtration, Ng
#' @export
#' @examples
#' Ng_parm()
#' [1] 0.2422003

Ng_parm <- function(Kx = 0.02,              # cm/s
                     GRAD = 0.0007,         # cm/cm
                     POR = 0.21,            # cm³/cm³
                     K_units = "cm/s", 
                     TEMP = 25,             # °C
                     DIA_SUSP = 1e-4,       # cm
                     DIA_SOIL = 0.053,      # cm
                     PART_DENS = 2.64       # g/cm³
                     ){
                     GRAV <- 9.80665        # m / s²
                     m2cm <- 100
                     RAD_SUSP <- DIA_SUSP / 2
                     NUM <- 2 * (PART_DENS - dens_wat(TEMP)) * GRAV * m2cm * RAD_SUSP^2
                     DEN <- 9 * visc_wat(TEMP) * U_parm(Kx = Kx, GRAD = GRAD, POR = POR, K_units = K_units)
                     val <- NUM / DEN
                     return(val)
                     }