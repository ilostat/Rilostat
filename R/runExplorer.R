#' @title play with ilostat explorer
#'
#' @description open ilostat explorer app on your computer.
#'
#' @name runExplorer
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#'
#' @export
runExplorer <- function(display.mode = "normal") {

  production_status <- FALSE
  if(dir.exists("J:/COMMON/")){
	Sys.setenv(ftp_proxy="proxyos.ilo.org:8080") 
  }
  
  myGlobal 	<- readLines(url("ftp://ilostatRO:1l0st4tR0@ftp.ilo.org/Rilostat/opensource/bulkexplorer/bulkexplorer_GLOBAL.r", method = 'libcurl', open = 'r'))
  myUi 		<- readLines(url("ftp://ilostatRO:1l0st4tR0@ftp.ilo.org/Rilostat/opensource/bulkexplorer/bulkexplorer_UI.r", method = 'libcurl', open = 'r'))
  myServer 	<- readLines(url("ftp://ilostatRO:1l0st4tR0@ftp.ilo.org/Rilostat/opensource/bulkexplorer/bulkexplorer_SERVER.r", method = 'libcurl', open = 'r'))
  closeAllConnections()
  
   
  shiny::runApp( list(
				global = eval(parse(text =myGlobal)), 
				ui = function(){eval(parse(text =myUi))}, 
				server = function(input, output, session) {eval(parse(text= myServer))}
				) 
				, display.mode = display.mode)
   closeAllConnections()
 

				
}
  
