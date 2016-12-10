#' U_parm Function to calculate groundwater velocity
#' This function calculates the U-parameter grounwdater velocity from Table 1 of Tobiason and O'Melia (1988). 
#' The U-parameter is based on the Kx (hydraulic conductivity), GRAD (hydraulic gradient), and POR (porosity).
#' The default units are cm/s but can be changed with K_units. Only ft/day are accepted as alternative units.
#' The units of the output are cm/s.
#' @param Kx Hydraulic conductivity. Defaults to a value of 0.02 cm/s
#' @param GRAD Hydraulic gradient. Defaults to a value of 0.0007
#' @param POR porosity. Defaults to a value of 0.21. The value of 0.21 is the average for medium sand as reported by Johnson (1967)
#' @param K_units Units of hydraulic conductivity. Defaults to cm/s but also accepts ft/day. Units should be surrounded
#' by quotation marks to ensure it is treated as a string of text.
#' @keywords filtration, groundwater velocity, U-parameter
#' @export
#' @examples
#' U <- U_parm(Kx = 0.02, GRAD = 0.0007, POR = 0.21, K_units = "cm/s")
#' U
#' [1] 6.666667e-05

U_parm <- function(Kx = 0.02, 
                   GRAD = 0.0007, 
                   POR = 0.21, 
                   K_units = "cm/s"){
                   `%ni%` = negate(`%in%`)
                   if(K_units %ni% c("cm/s", "ft/day", "feet/day")){print("Error: Convert Kx values to cm/s or ft/day")}
                   ft_day_to_cm_s <- 1 / 3600 / 24 * 2.54 * 12
                   Kx_FACT <- ifelse(K_units == "cm/s", 1, ft_day_to_cm_s)                   
                   val <- Kx * Kx_FACT * GRAD / POR
                   return(val)
                   }
                   