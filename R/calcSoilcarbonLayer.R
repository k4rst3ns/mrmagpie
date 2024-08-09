#' @title calcSoilcarbonLayer
#' @description This function extracts soil carbon densities from LPJmL for PNV to MAgPIE
#'
#' @param lpjml       Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#' @param weight      Switch between different weights:
#'                    'totalPNV' for LandArea as weight,
#'                    'PotForest' for PotentialForestArea
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("calcSoilcarbonLayer", aggregate = FALSE)
#' }
#'
calcSoilcarbonLayer <- function(lpjml       = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                                crop   = "ggcmi_phase3_nchecks_9ca735cb"),
                                climatetype = "MRI-ESM2-0:ssp370",
                                weight      = "totalPNV") {

  soilcLayerNatveg <- calcOutput("LPJmL_new", version = lpjml["natveg"], climatetype = climatetype,
                                 subtype = "soilc_layer", stage = "harmonized2020", aggregate = FALSE)
  topsoilc           <- soilcLayerNatveg[, , 1] + 1 / 3 * soilcLayerNatveg[, , 2]
  subsoilc           <- dimSums(soilcLayerNatveg, dim = 3) - topsoilc

  out <- mbind(setNames(topsoilc, "topsoilc"),
               setNames(subsoilc, "subsoilc"))

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA Carbon")
  }

  if (weight == "totalPNV") {
    weight <- calcOutput("LandArea", aggregate = FALSE) + 10^-10

  } else if (weight == "PotForest") {
    weight <- calcOutput("PotentialForestArea", refData = "lpj",
                         lpjml = lpjml, climatetype = climatetype,
                         aggregate = FALSE)[, getYears(out), ] + 10^-10
  } else {
    stop("weight setting is not valid")
  }

  return(list(x = out,
              weight = weight,
              unit = "t per ha",
              description = "Soil carbon (top- and subsoil) in tons per hectar for potential natural vegetation.",
              isocountries = FALSE))
}
