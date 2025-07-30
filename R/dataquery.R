#' @title Play with ilostat micro data query
#' @description open ilostat micro data query app on your computer.
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#'
#' @examples 
#' \dontrun{
#'
#'	require(shiny)
#'	
#'  dataquery()
#'
#' }
#' @export
dataquery <- function() {


  production_status <- FALSE

  
  myGlobal 	<- readr::read_lines('https://rplumber.ilo.org/files/apps/microquery/GLOBAL.r')
  myUi 		<- readr::read_lines('https://rplumber.ilo.org/files/apps/microquery/UI.r')
  myServer 	<- readr::read_lines('https://rplumber.ilo.org/files/apps/microquery/SERVER.r')
  

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
  
