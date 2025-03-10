#' @title Play with ilostat data explorer
#' @description open ilostat data explorer app on your computer.
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#'
#' @examples 
#' \dontrun{
#'
#'	require(shiny)
#'	
#'  dataexplorer()
#'
#' }
#' @export
dataexplorer <- function() {


  production_status <- FALSE

  
  myGlobal 	<- readr::read_lines('https://rplumber.ilo.org/files/apps/dataexplorer/GLOBAL.r')
  myUi 		<- readr::read_lines('https://rplumber.ilo.org/files/apps/dataexplorer/UI.r')
  myServer 	<- readr::read_lines('https://rplumber.ilo.org/files/apps/dataexplorer/SERVER.r')
  

  shiny::runApp( 	
					appDir  = list(
								global = eval(parse(text =myGlobal)), 
								ui = function(){eval(parse(text =myUi))}, 
								server = function(input, output, session) {eval(parse(text= myServer))}
								), 
					3838, 
					FALSE, 
					"127.0.0.1", 
					"", 
					FALSE, 
					"normal"
			)
   

				
}
  
