#' L_filt Function
#' This function calculates the distance in centimeters to reduce the suspended concentration by a
#' factor of C_C0 (i.e., C/C0).
#' 
#' @param C_CO Target TSS concentration reduction. Defaults to a value of 1e-6. 
#' That is to say that the TSS reduction is a factor of 1,000,000 times
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
#' @keywords filtration, Length
#' @export
#' @examples
#' L_filt()
#' [1] 0.2625636

L_filt <- function(C_C0 = 1e-6,           # Target concentration reduction
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
                   NUM <- 2 * DIA_SOIL * log(C_C0)
                   DEN <- 3 * (1 - POR) * alpha * eta_parm(Kx = Kx,                     # cm/s
                                                            GRAD = GRAD,                 # cm/cm
                                                            POR = POR,                   # cm³/cm³
                                                            K_units = K_units, 
                                                            TEMP = TEMP,                 # °C
                                                            DIA_SUSP = DIA_SUSP,         # cm
                                                            DIA_SOIL = DIA_SOIL,         # cm
                                                            PART_DENS = PART_DENS)       # g/cm³)
                   val <- -NUM / DEN
                   return(val)
                   }             