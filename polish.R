#' Convert a function into a simple shiny appliction
#' 
#' @param fn Function taking named arguments with defaults that will be converted
#' @param types Vector of argument types going into the function. Possible values are 'numeric', 'text', 'logical'
#' @param name Name of directory to write ui.R and server.R to
#' @param plot Logical, if true results in a plot output instead of verbatim text output


polish <- function(fn, types = rep("numeric", length(as.list(fn)) - 1), 
                   name = paste("polished", deparse(substitute(fn)), sep = "-"), 
                   plot = FALSE){
  
  lookup <- c("numericInput('", "textInput('", "checkboxInput('")
  names(lookup) <- c("numeric", "text", "logical")
  
  fnls <- as.list(fn)[-length(as.list(fn))]
  inputnames <- names(fnls)
  inputdefs <- as.character(fnls)
  
  inputdefs[types == "text" & inputdefs != ""] <- paste0("'", inputdefs[types == "text" & inputdefs != ""], "'")
  
  defvals <- ifelse(inputdefs == "", "", paste0(", value = ", inputdefs))
  inputtypes <- lookup[types]
  
  inputstatements <- paste(paste0(inputtypes, inputnames, "'", 
                                  paste0(", label = ", paste0("'", inputnames, "'")), 
                                  defvals   ,")"), collapse = ", \n")
  outputstatement <- ifelse(plot, "plotOutput('res')", "verbatimTextOutput('res')")
    
  ui <- paste0("shinyUI(bootstrapPage(\n", inputstatements, ", \n", outputstatement, "))", sep = "\n")
  
  # server
  
  arglist <- paste(paste(inputnames, ifelse(inputdefs != "", paste0(" = ", inputdefs), "")), collapse = ", ")
  
  fnbody <- paste(as.list(fn)[length(as.list(fn))])
  fndef <- paste(deparse(substitute(fn)), " <- function(", arglist, ")", fnbody)
  
  fncall <- paste0(deparse(substitute(fn)), "(", paste(paste("input", inputnames, sep = "$"), collapse = ", "), ")")
  
  outputdef <- paste(ifelse(plot, "output$res <- renderPlot(", "output$res <- renderPrint("), fncall, ")")
  
  server <- paste(fndef, "shinyServer(function(input, output){", outputdef, "})", sep = "\n")
  
  # file write
  
  if(!file.exists(name)) { 
    dir.create(name)
  }
  cat(ui, file = paste0(name, "/ui.R"))
  cat(server, file = paste0(name, "/server.R"))
  
  message(paste0("Type runApp('", name, "') to run the shiny application."))
  invisible(list(fn, ui, server))
  
}
