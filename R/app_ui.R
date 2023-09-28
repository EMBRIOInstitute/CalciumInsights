#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  options(spinner.color="#337ab7", spinner.color.background="#ffffff", spinner.size = 2)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      navbarPage("CalcioInsights",
                  tabPanel("Home",icon = icon("home", lib = "glyphicon") #,
                             # htmltools::includeHTML(
                             #  system.file("app/www/home.html", package = "CalcioInsights")
                             # )
                          ),
                 navbarMenu("Descriptive Analysis",
                            tabPanel("Raw data",
                                     mod_Raw_data_ui("Raw_data_1")
                            ),
                            tabPanel("Smoothed data",
                                     mod_Smoothed_data_ui("Smoothed_data_1")
                            ),
                            tabPanel("Denoising data",
                                     mod_Denoising_data_ui("Denoising_data_1")
                            )
                 ),
                 navbarMenu("Inference",
                            tabPanel("Functional ANOVA",
                                     mod_FunctionalANOVA_ui("FunctionalANOVA_1")
                            )

                 ),

                 )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CalcioInsights"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
