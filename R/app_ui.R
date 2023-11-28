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
      navbarPage("CalciumInsights",
                 tabPanel("Home", icon = icon("home", lib = "glyphicon"),
                          tags$iframe(src = "www/index.html", height = "900px", width = "100%")
                 ),
                 navbarMenu("Descriptive Analysis",

                            # tabPanel("Savitzky-Golay",
                            #          mod_Smoothed_data_ui("Smoothed_data_1")
                            # ),
                            tabPanel("Loess",
                                     mod_Denoising_data_ui("Denoising_data_1")
                            ),
                            tabPanel("Raw data",
                                     mod_Raw_data_ui("Raw_data_1")
                            ),
                            tabPanel("Special Case: Circular Scanning",
                            mod_Special_Case_Circular_Scanning_ui("Special_Case_Circular_Scanning_1")
                            )


                 ),
                 navbarMenu("Inference",
                            tabPanel("Functional ANOVA",
                                     mod_FunctionalANOVA_ui("FunctionalANOVA_1")
                            ),
                            tabPanel("Hypothesis for the Comparison of Two Groups",
                            mod_Comparison_of_Two_Means_ui("Comparison_of_Two_Means_1")
                            ),
                            tabPanel("Fourier transform",
                            mod_Fourier_transform_ui("Fourier_transform_1")
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
