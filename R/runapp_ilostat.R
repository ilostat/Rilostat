#' @title Play with ILOSTAT data apps
#' @description Open various data exploration and analysis applications hosted by the ILO.
#' @param name `chr` The name of the application to open.
#' 
#'   Available applications:
#'   \itemize{
#'     \item{"dataexplorer"}: Quickly find, filter, pivot, and download ILOSTAT data.
#'     \item{"microquery"}: Explore data availability and create custom queries from the ILO Harmonized Microdata Collection. Users can select variables, filters, and indicators to generate tailored datasets.
#'     \item{"regionalaggregate"}: Generate custom regional estimates by defining or selecting country groupings. The app automatically aggregates country-level modelled data for selected indicators.
#'   }
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#' 
#' @seealso 
#' Additional documentation for microdata:
#' \itemize{
#'   \item{\href{https://www.ilo.org/publications/ilostat-microdata-processing-quick-guide-principles-and-methods-underlying}{Quick guide on microdata processing}}
#'   \item{\href{https://rplumber.ilo.org/files/website/Ad_Hoc_Micro_ISIC_ISCO_Adjustment_procedure.pdf}{Adjustment procedure for ISCO- and ISIC-based estimates}}
#'   \item{\href{https://rplumber.ilo.org/files/website/Ad_Hoc_Micro_ISIC_ISCO_Global_Regional_Estimates.pdf}{Methodology for ad hoc global and regional estimates}}
#'   \item{\href{https://rplumber.ilo.org/files/website/Ad_Hoc_Micro_Query_Citation.pdf}{Citations}}
#' }
#' 
#' @examples
#' \dontrun{
#'
#'   runapp_ilostat(name = "dataexplorer")
#'
#'   # You can also run other apps like:
#'   runapp_ilostat(name = "microquery")
#'   runapp_ilostat(name = "regionalaggregate")
#'
#'
#' }
#' @export
runapp_ilostat <- function(name = "dataexplorer"){

  # CRITICAL: Check if the 'shiny' package is installed.
  # If 'shiny' is only a suggested package in the DESCRIPTION, it might be missing.
  if (!requireNamespace("shiny", quietly = TRUE)) {
    # If the package is missing, stop execution and give the user clear instructions.
    # 'call. = FALSE' prevents the function call itself from being printed in the error message,
    # making the output cleaner.
    stop(
      "The 'shiny' package is required to run the ILOSTAT web applications.\n",
      "Please install it first: install.packages('shiny')",
      call. = FALSE
    )
  }

  # If 'shiny' is available, proceed to run the application using the internal function.
  .runapps(name)

}


#' @export
#' @rdname runapp_ilostat
dataexplorer <- function() {
	
	  # CRITICAL: Check if the 'shiny' package is installed.
  # If 'shiny' is only a suggested package in the DESCRIPTION, it might be missing.
  if (!requireNamespace("shiny", quietly = TRUE)) {
    # If the package is missing, stop execution and give the user clear instructions.
    # 'call. = FALSE' prevents the function call itself from being printed in the error message,
    # making the output cleaner.
    stop(
      "The 'shiny' package is required to run the ILOSTAT web applications.\n",
      "Please install it first: install.packages('shiny')",
      call. = FALSE
    )
  }

  # If 'shiny' is available, proceed to run the application using the internal function.
  .runapps(name="dataexplorer")

}


  
