# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
usethis::use_package( "shiny" )
usethis::use_package( "ggplot2" )
usethis::use_package( "DT" )
usethis::use_package( "gridExtra" )
usethis::use_package( "mdatools" )
usethis::use_package( "pracma" )
usethis::use_package( "tidyverse" )
usethis::use_package( "dplyr" )
usethis::use_package( "reshape2" )
usethis::use_package( "refund" )
usethis::use_package( "fda" )
usethis::use_package( "fds" )
usethis::use_package( "FoSIntro" )
usethis::use_package( "reshape2" )
usethis::use_package( "shinyjs" )



attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "Raw_data") # Name of the module
golem::add_module(name = "Smoothed_data") # Name of the module  Denoising data
golem::add_module(name = "Denoising_data") # Name of the module
golem::add_module(name = "Special_Case_Circular_Scanning") # Name of the module
golem::add_module(name = "FunctionalANOVA")
golem::add_module(name = "Comparison_of_Two_Means")
golem::add_module(name = "Fourier_transform")
## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)
golem::add_utils("load_file")
golem::add_utils("peaks")
golem::add_utils("prominens")
golem::add_utils("prominens2")
golem::add_utils("prominens_case")
golem::add_utils("FWHM")
golem::add_utils("FWHM2")
golem::add_utils("FWHM_case")
golem::add_utils("right_left_FWHM")
golem::add_utils("Savitzky_Golay")
golem::add_utils("response_time")
golem::add_utils("model_functional")
golem::add_utils("AUC")
golem::add_utils("AUC2")
golem::add_utils("AUC_case")
golem::add_utils("half_prominance")
golem::add_utils("Time_of_the_first_peak")


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("CalcioInsights")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
