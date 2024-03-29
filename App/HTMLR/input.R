actionButtonHalf <- function(inputId, value){
  tags$button(id = inputId, type="button", style="margin-top:10px", class = "btn btn-primary action-button shiny-bound-input", value)
}

actionButtonRow <- function(inputId, value){
  div(tags$button(id = inputId, type="button", class = "btn btn-primary action-button shiny-bound-input", value))
}

disableInputSmall <- function(session){
  session$sendCustomMessage(type="jsCode", list(code= "$('.input-small').attr('disabled',true)"))
}

enableInputSmall <- function(session){
  session$sendCustomMessage(type="jsCode", list(code= "$('.input-small').attr('disabled',false)"))
}

disableUIElement <- function(id, session){
  session$sendCustomMessage(type="jsCode", list(code=paste0("$('#",id,"').attr('disabled',true)")))
}

enableUIElement <- function(id, session){
  session$sendCustomMessage(type="jsCode", list(code=paste0("$('#",id,"').attr('disabled',false)")))
}
