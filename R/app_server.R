#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_Raw_data_server("Raw_data_1")
  mod_Smoothed_data_server("Smoothed_data_1")
  mod_Denoising_data_server("Denoising_data_1")
  mod_FunctionalANOVA_server("FunctionalANOVA_1")
  mod_Comparison_of_Two_Means_server("Comparison_of_Two_Means_1")
  mod_Special_Case_Circular_Scanning_server("Special_Case_Circular_Scanning_1")
}
