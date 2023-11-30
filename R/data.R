# NOTE - Fields are public as coders will be able to add their own logic on any
#        thing framework that may not yet be available in the methods.
# try using base split to pass via a map2 or pmap
#
# N O T E !!!!!
# New fully recursive graph code will be generated with # new graph comment. All old code kept for now for backward compatability commented # old method
#
# The new method philosophy.
# A header field with the parent project header properties - id, name etc. All methods referring to parent will be via the header property id
# No more parent data structures with the signifiers
# No more linked framework data structures with the signiries
# Just  signifier data structures - type/id and id/type by framework - parent simply being one of them.
# Will still have the list of linked headers, each with id.
# Will store the framework structure graph for navigating through the stuctures.
# Getting linked framework ids is getting all framework ids less the parent.
# Method for getting children frameworks from parent
# Method for getting parent framework from child.
#
#' R6 class that contains and manipulates framework data.
#'
#' @description
#' The `data` class is the primary class for
#' representing a framework's data. it will either download or receive the
#' framework data, then perform transformations and extensions, including creating
#' title versions, new columns for %age dyads, transform stone and triad data to absolute
#' proportion from %age values, add zones and if requested add frameworkr metadata.
#' If there is a dashboard requested it will combine data if a combined dashboard, update
#' frameworkr with additional signifier definitions and combine and filter data where appropriate.data
#'
#' @docType class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # The package vignettes include extensive examples of working with the
#' # data class.
#' library(sensemakerdatar)
#' my_fw <- signifiers$new("mydir/projectFramework.json", NULL, NULL, NULL)
#' fw_triads <- self$get_signifier_ids_by_type("triad")
#' triad_01_image <- pt$get_triad_background_image(fw_triads[[1]])
Data <- R6::R6Class("Data",
                    public = list(
                      # this will be a list of lists containing the data.
                      #' @field data The full list of data
                      data = list(NULL),
                      # Common lists used from within the data list- all initially set to full dataset
                      #' @field df1 The full dataset for any given fw or linked fw data
                      df1 = NULL,
                      #' @field dat  Filtered data so always the data being displayed based on a filter
                      dat = NULL,
                      #' @field df_keep Always the full dataset even through linked fw selections
                      df_keep = NULL,
                      #' @field title_data data containing titles, not ids
                      title_data = NULL,
                      #' @field title_use Filtered data containing titles not ids, used for display on a filter
                      title_use = NULL,
                      #' @field sm_framework the framework definition if it is passed in
                      sm_framework = NULL,
                      #' @field dashboard_definition_v1 The dashboard json for version 1 because we need to do work with it after declaring it
                      dashboard_definition_v1 = NULL,
                      #' @field dashboard_definition_v2 The dashboard json for version 2 because we need to do work with it after declaring it
                      dashboard_definition_v2 = NULL,
                      #' @field dashboard_layout_v2 The dashboard layout json for version 2 because we need to do work with it after declaring it
                      dashboard_layout_v2 = NULL,
                      #' @field dashboard_filters_v2 The dashboard filters json for version 2 because we need to do work with it after declaring it
                      dashboard_filters_v2 = NULL,
                      #' @field framework_id The framework id
                      framework_id = NULL,
                      #' @field dashboard_id The dashboard id
                      dashboard_id = NULL,
                      #' @field dashboard_version The dashboard version - whether 1 or 2
                      dashboard_version = NULL,
                      #' @description
                      #' Create a new `data` object.
                      #' @details
                      #' The csv is downloaded or passed or read and processed to prepare data for analytical work
                      #' @param csvfilename if loading directly from a pre-existing csv file on disk. The file is an id, not label/title file.
                      #' @param csvfiledf if passing in an already read csv file data frame.
                      #' @param framework_id The id of the framework to load.
                      #' @param dashboard_id The id of the dashboard to load (if framework or dashboard id passed, only one not the other must be non NULL).
                      #' @param token if using the platform security, the token to gain access to the data. Used only when framework_id or dashboard_id passed.
                      #' @param sensemakerframeworkrobject - optional sensemakerframeworkr R6 class object which would be added to based on data loaded (e.g. meta columns)
                      #' @return A new `signifier` R6 class object and fields type by signifier id, signifier ids by type, and
                      initialize = function(csvfilename = NA, csvfiledf = NA, framework_id = NA, dashboard_id = NA,
                                            token = NA, sensemakerframeworkrobject = NA) {
                        sensemakerdata <- private$get_data(csvfilename, csvfiledf, framework_id, dashboard_id, token, sensemakerframeworkrobject)

                      }

                    ),

                    private = list(
                      # function handling the initialisation - get the data, process the data and load appropriate fields
                      get_data = function(csvfilename = NA, csvfiledf = NA, framework_id = NA, dashboard_id = NA, token = NA, sensemakerframeworkrobject = NA) {
                        # checking that the parameters are correct.
                        # either one of filename/csvfiledf OR framework_id/dashboard_id
                        # if dashboard_id or framework_id then must have token.
                        # we don't worry about testing if token when csvfile or dataframe passed - just not used.
                        end_point <- "openapi"
                        token <- "eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCIsImtpZCI6IjIzYjM4ZjE4OGMzY2IzOWYwOGZkOTdmZTdiNmJlZDAzYjRmNGM5M2MifQ.eyJhdWQiOiJodHRwczovL3BsYXRmb3JtLnNlbnNlbWFrZXItc3VpdGUuY29tIiwiZXhwIjoxNzAxMDcwMTg1LCJpYXQiOjE3MDEwNjY1ODUsImlzcyI6Imh0dHBzOi8vYXBpLnNpbmd1bGFyaXR5LmljYXRhbHlzdC5jb20vdjEvaXNzdWVyLzg5NjlhM2I4LWU5YmEtNGQ2ZC1iNjA4LTc0YzVjOTI5NmUxNCIsInN1YiI6IjI1YTAzM2NlYWFkOWU4NzA0MTFkNDdkOGNlOTU3ZWI2ZGE2NTc0MWM2Y2YwMzkyNzcwYmYzMGRjM2FiODRkZDciLCJub25jZSI6MC4yNjAzODMyOTE4OTA2NTE1Nywic2NvcGUiOiJhdXRoIHByb2ZpbGUiLCJjbGllbnRfaWQiOiI4OTY5YTNiOC1lOWJhLTRkNmQtYjYwOC03NGM1YzkyOTZlMTQiLCJncmFudF90eXBlIjoiYXV0aG9yaXphdGlvbl9jb2RlIiwicmVkaXJlY3RfdXJpIjoiaHR0cHM6Ly9wbGF0Zm9ybS5zZW5zZW1ha2VyLXN1aXRlLmNvbS9jYXB0dXJlcy9iMWFkNzZjYS1kNjBhLTRjOWQtOWI2My1kZTNkNzg5Mzc4NGIvZGFzaGJvYXJkLzc3NGU4NTUyLTAzNTItNDZlZS05NGU2LTEwM2RlOTRhNWJkNiIsInJlZnJlc2hfdG9rZW4iOiJmOWE2OGNkOC02YTEzLTQ1M2QtYjhkNy04ZDcxMDRjYTljNDIifQ.AFob2upoRWI-lp8yK9lCcF_d3oafzQyKZDLBCcuEiwWZyp9t3cm0k0WcaO4fYcbtIYSrdX0bjaWlaCGEc-L-U6cPABCyzPb8vSb4Kd_ACPQiu3354nqL8L4baI6k26CTQMxHLcEyoPGEc-eKR2DQ6xL_XpEYf63jAK2mVAsAVcCaM2U5"
                        assertive::assert_any_are_not_na(c(csvfilename, csvfiledf, framework_id, dashboard_id, token), severity = "stop")
                        assertive::assert_any_are_na(c(csvfilename, csvfiledf), severity = "stop")
                        assertive::assert_any_are_na(c(framework_id, dashboard_id), severity = "stop")
                        if  (length(which(!is.na(c(csvfilename, csvfiledf)) == TRUE)) == 1 & length(which(!is.na(c(framework_id, dashboard_id)) == TRUE)) >= 1) {
                          print("you cannot have file name or file while also passing framework id or dashboard id")
                          stop()
                        }

                        if (any(c(csvfilename, csvfiledf, framework_id, dashboard_id) == "", na.rm = TRUE)) {
                          print("no parameter should be a blank string")
                          stop()
                        }

                        if (length(which(!is.na(c(framework_id, dashboard_id)))) == 1 & is.na(token)) {
                          print("if providing a framework id or dashboard id you must provide a token")
                          stop()
                        }

                        if (!is.data.frame(csvfiledf)) {
                          if ((any(!is.na(csvfiledf)))) {
                            print("csvfiledf should be a dataframe")
                            stop()
                          }
                        } else {
                          if (!('project_id' %in% colnames(csvfiledf))) {
                            print("data frame 'csvfiledf' must have a column 'project_id' with the framework id present")
                            stop()
                          }

                        }

                        if (!is.na(csvfilename)) {
                          assertive::assert_all_are_existing_files(x = csvfilename, severity = "stop")
                        }

                        # end of parameter checking.
                        # "df" to store the data frame read or downloaded
                        df <- NULL

                        if (!is.na(csvfilename)) {
                          df <- read.csv(csvfilename, check.names = FALSE, stringsAsFactors = FALSE, as.is = TRUE, encoding = "UTF-8")
                          if (!("project_id" %in% colnames(df))) {
                            print("file 'csvfilename' must have a column 'project_id' with the framework id present")
                            stop()
                          }
                        }

                        if (is.data.frame(csvfiledf)) {
                          df <- csvfiledf
                        }

                        if (!is.na(framework_id)) {
                          # is this a demonstrator account
                          is_demonstrator <- private$is_demonstrator(token)
                          df <- private$get_API_framework_data(end_point, framework_id, token, is_demonstrator)
                          self$framework_id <- framework_id
                        }
                        # preliminary process of the dashboard - get the framework id and the frqmework data.
                        if (!is.na(dashboard_id)) {
                          # get the dashboard definition.
                          # try version one of the dashboard
                          dashboard_definition <- private$get_v1_DashboardDefinition(end_point, dashboard_id, token)
                          if (!is.null(dashboard_definition$framework_id)) {
                            self$dashboard_definition_v1 <- dashboard_definition
                            self$framework_id <- dashboard_definition$framework_id
                            self$dashboard_id <- dashboard_id
                            self$dashboard_version <- "v1"
                          } else {

                          # try version 2
                              dashboard_definition <- private$get_v2_DashboardDefinition(end_point, dashboard_id, token)
                              self$dashboard_definition_v2 <- dashboard_definition[["v2_definition"]]
                              self$framework_id <- self$dashboard_definition_v2$frameworkID
                              self$dashboard_id <- dashboard_id
                              self$dashboard_layout_v2 <- dashboard_definition[["v2_layout"]]
                              self$dashboard_filters_v2 <- dashboard_definition[["v2_filters"]]
                              self$dashboard_version = "v2"
                          }

                        }

                        # Get the data
                        is_demonstrator <- private$is_demonstrator(token)
                        df <- private$get_API_framework_data(end_point, self$framework_id, token, is_demonstrator)

                        # Now we have df as the data frame required for processing - but does the dataframe match the definition
                        # get the framework_id if not present
                        if (is.na(framework_id)) {
                          framework_id <- df[1, "project_id"]
                        }

                        # get the sensemakerframeworkr object if it hasn't been passed
                        if (!("Signifiers" %in% class(sensemakerframeworkrobject))) {
                          sensemakerframeworkrobject <- sensemakerframeworkr::Signifiers$new(jsonfilename = NULL, layoutfilename = NULL, parsedjson = NULL, parsedlayout = NULL, workbenchid = framework_id, token = token)
                        }

                        # check that the sensemakerframeworkr projectid is the same as the project id passed in or obtained in the data
                        if (framework_id != sensemakerframeworkrobject$get_parent_framework_id()) {
                         print(paste("the framework_id", framework_id, "passed or obtained from the data, not the same as", sensemakerframeworkrobject$get_parent_framework_id(), "in the sensemakerframeworkr package object"))
                          stop()
                        }
                        # now process the dashboard completely - this function has side effects on the dashboard definition,
                        # data and framework definition
                        if (!is.na(dashboard_id)) {
                          do.call(paste0("process_dashboard_definition_", self$dashboard_version), args = list(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject), envir = private)
                        }

                        # assign the sensemaker framework
                        self$sm_framework <- sensemakerframeworkrobject



                        # start processing data
                        private$process_data(df, sensemakerframeworkrobject)


                      },

                      process_data = function(df, sensemakerframeworkrobject) {


                        # add the date fields

                      },

                      get_API_framework_data = function(end_point, framework_id, token, isDemonstratorAccount = FALSE) {

                        # Get the data from the API saving to temp folder, read csv into dataframe, remove temp file and return dataframe
                        df1FileName <- tempfile(pattern = "", fileext = ".csv")
                        r1 <- httr::GET(paste0("http://", end_point, ".sensemaker-suite.com/apis/capture/", framework_id,"/?type=csv", sep=""),
                                  httr::add_headers(.headers = c('Authorization'= paste('Bearer ', token))),
                                  httr::write_disk(df1FileName, overwrite=TRUE), httr::verbose())
                        DF1I <- read.csv(df1FileName, stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = "", as.is = TRUE, check.names = FALSE, nrows = ifelse(isDemonstratorAccount, 20, -5))
                         unlink(df1FileName)
                        return(DF1I)
                      },

                      # Process the dashboad definition. This has side effects on the data and framework definition object
                      #  1. Set include FALSE for any signifier not defined to the dashboard
                      #  2. Add any linked framework signifier definitions if this is a linked dashboard, if they are included in the secondary definitions
                      #  3. Add linked framework data to the data frame mapping ids where required
                      #. 4. Filter the data if any filters defined
                      process_dashboard_definition_v1 = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject) {

                      dashboard_definition <- self$dashboard_definition_v1

                      # Set include FALSE for any signifier not defined to the dashboard.
                      framework_signifier_ids <- sensemakerframeworkrobject$get_all_signifier_ids(keep_only_include = TRUE)
                      dashboard_signifiers <- dashboard_definition$layout$attribute_id[which(!is.na(dashboard_definition$layout$attribute_id) & dashboard_definition$layout$attribute_id != "00000000-0000-0000-0000-000000000000")]
                      framework_signifiers_missing <- framework_signifier_ids[which(!(framework_signifier_ids %in% dashboard_signifiers))]
                      purrr::walk(framework_signifiers_missing, ~ {sensemakerframeworkrobject$change_signifier_include(.x, value = FALSE)}, sensemakerframeworkrobject)






                      },


                      process_dashboard_definition_v2 = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator) {


                        print("processing v2 of the dashboard")




                      },

                      get_v1_DashboardDefinition = function(end_point, dashboard_id, token) {
                        # we are in the get dashboard definition

                        out <- try( {
                          # get the json from the returned project definition

                          return(jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://", end_point, ".sensemaker-suite.com/apis/dashboards/",  dashboard_id),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                     , 'Content-Type' = 'application/json'))
                          ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))
                        }
                        )
                        if(inherits(out, "try-error"))
                        {
                          print("we are in the error")
                          return(NULL)
                        }
                        if(inherits(out, "try-warning"))
                        {
                          print("we are in the warning")
                          return(NULL)
                        }
                        return(out)
                      },

                      get_v2_DashboardDefinition = function(end_point, dashboard_id, token) {
                        # we are in the get dashboard definition


                          # get the json from the returned project definition

                          v2_definition <- jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://api-gateway.sensemaker-suite.com/v2/dashboards/", dashboard_id),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                           , 'Content-Type' = 'application/json')),  httr::verbose()
                          ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                          v2_layout <- jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://api-gateway.sensemaker-suite.com/v2/dashboards/", dashboard_id, "/layouts"),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                           , 'Content-Type' = 'application/json')),  httr::verbose()
                          ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                          # get the json from the returned project definition

                          v2_filters <- jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://api-gateway.sensemaker-suite.com/v2/dashboards/", dashboard_id, "/filters"),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                           , 'Content-Type' = 'application/json')),  httr::verbose()
                          ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                          ret_list <- list(v2_definition = v2_definition, v2_layout = v2_layout, v2_filters = v2_filters)

                        return(ret_list)
                      },



                      is_demonstrator = function(trToken) {

                        out <- try({
                          #
                          isDemonstratorAccount <- FALSE
                          sessionDetails <- jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://api.singularity.icatalyst.com/api/session"),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", trToken, sep = " ")
                                                     , 'Content-Type' = 'application/json'))
                          ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                          # if there is only one true accessrole line here and the  accessrole:true and code for that is "f48150ea-ba3d-4108-9575-94408ff4cc99" then demonstrator true
                          trueRoles <-  sessionDetails$user$roles  %>% dplyr::filter(sessionDetails$user$roles$accessrole == TRUE)

                          if (nrow(trueRoles) == 1) {
                            if (trueRoles$code == "f48150ea-ba3d-4108-9575-94408ff4cc99") {
                              isDemonstratorAccount <- TRUE
                            }
                          }

                          return(isDemonstratorAccount)
                        }
                        )
                        if(inherits(out, "try-error"))
                        {
                          return(NULL)
                        }
                        if(inherits(out, "try-warning"))
                        {
                          return(NULL)
                        }
                        return(out)
                      }



                    )

)
