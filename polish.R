#' Convert a function into a simple shiny appliction
#' 
#' @param fn Function taking named arguments with defaults that will be converted
#' @param name Name of directory to write ui.R and server.R to
#' @param plot Logical, if true results in a plot output instead of verbatim text output


polish <- function(fn,
                   name = paste("polished", deparse(substitute(fn)), sep = "-"), 
                   plot = FALSE){
  
  lookup <- c("numericInput('", "textInput('", "checkboxInput('")
  names(lookup) <- c("numeric", "character", "logical")
  
  fnls <- as.list(fn)[-length(as.list(fn))]
  
  types <- sapply(fnls, class)
  argcheck <- sapply(types, function(x) x %in% names(lookup))
  
  if(!all(argcheck)) stop(paste("Argument", paste(names(types)[types == "name"], collapse = ", "), "default not character, numeric, or logical."))
  
  inputnames <- names(fnls)
  inputdefs <- as.character(fnls)
  
  inputdefs[types == "character"] <- paste0("'", inputdefs[types == "character"], "'")
  
  defvals <- ifelse(inputdefs == "", "", paste0(", value = ", inputdefs))
  

  inputtypes <- lookup[types]
  
  inputstatements <- paste(paste0(inputtypes, inputnames, "'", 
                                  paste0(", label = ", paste0("'", inputnames, "'")), 
                                  defvals   ,")"), collapse = ", \n")
  outputstatement <- ifelse(plot, "plotOutput('res')", "verbatimTextOutput('res')")
    
  ui <- paste0("shinyUI(bootstrapPage(\n", inputstatements, ", \n", outputstatement, "))", sep = "\n")
  
  # server
  
  arglist <- paste(paste(inputnames, paste0(" = ", inputdefs)), collapse = ", ")
  
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
