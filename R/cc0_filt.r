#' cc0_filt Function
#' This function calculates the ratio of TSS concentration to original TSS concentration at a distance
#' of LENGTH. The default units are "m" but distance units (LENGTH_UNIT) of cm and ft are also applicable.
#' 
#' @param LENGTH Distance at which to calculate the ratio of TSS concentration to original TSS concentration.
#' Defaults to a value of 500
#' @param LENGTH_UNIT Unit of length measurement. Defaults to a value of "m". Other options are "meter", 
#' "meters", "cm", "centimeter", "centimeters", "ft", "foot", "feet".
#' @param alpha Factor that accounts for chemical affects on particle removal. Defaults to a value of 1 (i.e., 
#' chemical affects do not enhance particle removal
#' @param Kx Defaults to a value of 0.02 cm/s
#' @param GRAD Defaults to a value of 0.0007 
#' @param POR Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @param K_units Units of hydraulic conductivity. Defaults to cm/s but also accepts ft/day. Units should be surrounded
#' by quotation marks to ensure it is treated as a string of text.
#' @param TEMP Temperature in units of °C. Defaults to a value of 25°C
#' @param DIA_SUSP Diameter of the suspended solids in units of cm
#' @param DIA_SOIL Diameter of the soil particles in units of cm
#' @param PART_DENS Density of the suspended particle in units of g/cm³
#' @keywords filtration, concentration
#' @export
#' @examples
#' L_filt()
#' [1] 0.2625636

cc0_filt <- function(LENGTH = 500, 
                     LENGTH_UNIT = "m",     # Distance units, other available units include cm, feet
                     alpha = 1,             # Influence of chemical effects on particle removal
                     Kx = 0.02,             # cm/s
                     GRAD = 0.0007,         # cm/cm
                     POR = 0.21,            # cm³/cm³
                     K_units = "cm/s", 
                     TEMP = 25,             # °C
                     DIA_SUSP = 1e-4,       # cm
                     DIA_SOIL = 0.053,      # cm
                     PART_DENS = 2.64       # g/cm³
                     ){
                     
                     ft2cm <- 12 * 2.54
                     LENGTH_FACT <- ifelse(LENGTH_UNIT %in% c("m", "meter", "meters"), 100, 
                                    ifelse(LENGTH_UNIT %in% c("cm", "centimeter", "centimeters"), 1, 
                                    ifelse(LENGTH_UNIT %in% c("ft", "foot", "feet"), ft2cm, 
                                    print("Error: Convert to units of either m, cm, or ft"))))
                    
                     NUM <- 3 * (1 - POR) * alpha * 
                                LENGTH * LENGTH_FACT * 
                                eta_parm(Kx = Kx,                     # cm/s
                                         GRAD = GRAD,                 # cm/cm
                                         POR = POR,                   # cm³/cm³
                                         K_units = K_units, 
                                         TEMP = TEMP,                 # °C
                                         DIA_SUSP = DIA_SUSP,         # cm
                                         DIA_SOIL = DIA_SOIL,         # cm
                                         PART_DENS = PART_DENS)       # g/cm³)
                     DEN <- 2 * DIA_SOIL
                     val <- exp(-NUM / DEN)
                     return(val)
                     }