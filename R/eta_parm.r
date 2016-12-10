#' eta_parm Function
#' This function calculates the single particle/collector removal efficiency [Equation 6 from Tobiason and O'Melia (1988)]
#' @param Kx Defaults to a value of 0.02 cm/s
#' @param GRAD Defaults to a value of 0.0007 
#' @param POR Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @param K_units Units of hydraulic conductivity. Defaults to cm/s but also accepts ft/day. Units should be surrounded
#' by quotation marks to ensure it is treated as a string of text.
#' @param TEMP Temperature in units of °C. Defaults to a value of 25°C
#' @param DIA_SUSP Diameter of the suspended solids in units of cm
#' @param DIA_SOIL Diameter of the soil particles in units of cm
#' @param PART_DENS Density of the suspended particle in units of g/cm³
#' @keywords filtration, Removal Efficiency
#' @export
#' @examples
#' eta_parm()
#' [1] 11.59751

eta_parm <- function(Kx = 0.02,             # cm/s
                     GRAD = 0.0007,         # cm/cm
                     POR = 0.21,            # cm³/cm³
                     K_units = "cm/s", 
                     TEMP = 25,             # °C
                     DIA_SUSP = 1e-4,       # cm
                     DIA_SOIL = 0.053,      # cm
                     PART_DENS = 2.64       # g/cm³
                     ){
                     val <- 4 * As_parm(POR = POR)^(1/3) * 
                                Npe_parm(Kx = Kx,                        # cm/s                       
                                         GRAD = GRAD,                    # cm/cm
                                         POR = POR,                      # cm³/cm³
                                         K_units = K_units,              
                                         TEMP = TEMP,                    # °C
                                         DIA_SUSP = DIA_SUSP,            # cm
                                         DIA_SOIL = DIA_SOIL)^(-2 / 3) + # cm
                                As_parm(POR = POR) * 
                                NLo_parm(Kx = Kx,                        # cm/s
                                         GRAD = GRAD,                    # cm/cm
                                         POR = POR,                      # cm³/cm³
                                         K_units = K_units, 
                                         TEMP = TEMP,                    # °C
                                         DIA_SUSP = DIA_SUSP)^(1 / 8) *  # cm
                                Nr_parm(DIA_SUSP = DIA_SUSP,                 # cm
                                        DIA_SOIL = DIA_SOIL)^(15/8) +
                                3.38 * 10^-3 * As_parm(POR = POR) * 
                                Ng_parm(Kx = Kx,                     # cm/s
                                        GRAD = GRAD,                 # cm/cm
                                        POR = POR,                   # cm³/cm³
                                        K_units = K_units, 
                                        TEMP = TEMP,                 # °C
                                        DIA_SUSP = DIA_SUSP,         # cm
                                        DIA_SOIL = DIA_SOIL,         # cm
                                        PART_DENS = PART_DENS)^1.2 * # g/cm³
                                Nr_parm(DIA_SUSP = DIA_SUSP,                 # cm
                                        DIA_SOIL = DIA_SOIL)^-0.4
                     return(val)
                     }