#' @title Play with ilostat data explorer
#' @description open ilostat data explorer app on your computer.
#' @param port The TCP port that the application should listen on. If the
#'   `port` is not specified, and the `shiny.port` option is set (with
#'   `options(shiny.port = XX)`), then that port will be used. Otherwise,
#'   use the 3838 port.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only. This value of this parameter can also be a
#'   function to call with the application's URL.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the `shiny.host` option, if set, or `"127.0.0.1"` if not. See
#'   Details.
#' @param display.mode The mode in which to display the application. 
#' If set to the value "showcase", shows application code and metadata 
#' from a DESCRIPTION file in the application directory alongside the application. 
#' If set to "normal", displays the application normally. Defaults to "auto", which 
#' displays the application in the mode given in its DESCRIPTION file, if any.
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
dataexplorer <- function(
				port = getOption("shiny.port", 3838), 
				launch.browser = getOption("shiny.launch.browser", interactive()),
				host = getOption("shiny.host", "127.0.0.1"),
				display.mode = "normal") {


  production_status <- FALSE

  
  myGlobal 	<- readr::read_lines('https://rplumber.ilo.org/website/file/?folder=apps/dataexplorer&filename=GLOBAL&format=.r')
  myUi 		<- readr::read_lines('https://rplumber.ilo.org/website/file/?folder=apps/dataexplorer&filename=UI&format=.r')
  myServer 	<- readr::read_lines('https://rplumber.ilo.org/website/file/?folder=apps/dataexplorer&filename=SERVER&format=.r')
  

  shiny::runApp( 	
					appDir  = list(
								global = eval(parse(text =myGlobal)), 
								ui = function(){eval(parse(text =myUi))}, 
								server = function(input, output, session) {eval(parse(text= myServer))}
								), 
					host = host,
					launch.browser  = launch.browser,
					port = port,
					display.mode = display.mode
			)
   

				
}
  
