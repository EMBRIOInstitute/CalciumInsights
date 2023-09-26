#' Denoising_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Denoising_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Denoising_data Server Functions
#'
#' @noRd 
mod_Denoising_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Denoising_data_ui("Denoising_data_1")
    
## To be copied in the server
# mod_Denoising_data_server("Denoising_data_1")
