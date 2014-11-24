actionButtonRow <- function(inputId, value){
  div(
    tags$button(id = inputId, type="button", style="margin-top:10px", class = "btn btn-primary action-button shiny-bound-input", value))
}