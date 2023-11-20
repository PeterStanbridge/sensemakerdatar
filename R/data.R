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
                      #' @field dashboard_definition The dashboard json because we need to do work with it after declaring it
                      dashboard_definition = NULL,
                      #' @description
                      #' Create a new `data` object.
                      #' @details
                      #' The csv is downloaded or passed or read and processed to prepare data for analytical work
                      #' @param csvfilename if loading directly from a pre-existing csv file on disk. The file is an id, not label/title file.
                      #' @param csvfiledf if passing in an already read csv file data frame.
                      #' @param frameworkid The id of the framework to load.
                      #' @param dashboardid The id of the dashboard to load (if framework or dashboard id passed, only one not the other must be non NULL).
                      #' @param token if using the platform security, the token to gain access to the data. Used only when frameworkid or dashboardid passed.
                      #' @param sensemakerframeworkrobject - optional sensemakerframeworkr R6 class object which would be added to based on data loaded (e.g. meta columns)
                      #' @return A new `signifier` R6 class object and fields type by signifier id, signifier ids by type, and
                      initialize = function(csvfilename = NA, csvfiledf = NA, frameworkid = NA, dashboardid = NA,
                                            token = NA, sensemakerframeworkrobject = NA) {
                        sensemakerdata <- private$get_data(csvfilename, csvfiledf, frameworkid, dashboardid, token, sensemakerframeworkrobject)

                      }

                    ),

                    private = list(
                      # function handling the initialisation - get the data, process the data and load appropriate fields
                      get_data = function(csvfilename = NA, csvfiledf = NA, frameworkid = NA, dashboardid = NA, token = NA, sensemakerframeworkrobject = NA) {
                        # checking that the parameters are correct.
                        # either one of filename/csvfiledf OR frameworkid/dashboardid
                        # if dashboardid or frameworkid then must have token.
                        # we don't worry about testing if token when csvfile or dataframe passed - just not used.
                        end_point <- "openapi"
                        token <- "eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCIsImtpZCI6IjIzYjM4ZjE4OGMzY2IzOWYwOGZkOTdmZTdiNmJlZDAzYjRmNGM5M2MifQ.eyJhdWQiOiJodHRwczovL3BsYXRmb3JtLnNlbnNlbWFrZXItc3VpdGUuY29tIiwiZXhwIjoxNzAwNDYyODQ5LCJpYXQiOjE3MDA0NTkyNDksImlzcyI6Imh0dHBzOi8vYXBpLnNpbmd1bGFyaXR5LmljYXRhbHlzdC5jb20vdjEvaXNzdWVyLzg5NjlhM2I4LWU5YmEtNGQ2ZC1iNjA4LTc0YzVjOTI5NmUxNCIsInN1YiI6IjI1YTAzM2NlYWFkOWU4NzA0MTFkNDdkOGNlOTU3ZWI2ZGE2NTc0MWM2Y2YwMzkyNzcwYmYzMGRjM2FiODRkZDciLCJub25jZSI6MC4xNjM2ODA3Njk5MDg4ODE5Miwic2NvcGUiOiJhdXRoIHByb2ZpbGUiLCJjbGllbnRfaWQiOiI4OTY5YTNiOC1lOWJhLTRkNmQtYjYwOC03NGM1YzkyOTZlMTQiLCJncmFudF90eXBlIjoiYXV0aG9yaXphdGlvbl9jb2RlIiwicmVkaXJlY3RfdXJpIjoiaHR0cHM6Ly9vcGVuYXBpLnNlbnNlbWFrZXItc3VpdGUuY29tL3Npbmd1bGFyaXR5L3Rva2VuIiwicmVmcmVzaF90b2tlbiI6IjAzM2UzNDZhLTFkYzMtNDM1Zi04MDQwLWQ2YjY4MjA5NDg4MCJ9.AYqmT1_VLMkN4mQZcyPh3u4goEMzBCvhn6J6QRGlSl7iQDcIhrLAL9KkDXqkVtBDNGFLcCMI_gNFy9EYTFUbeyuVAS6JXb3J9m_H9vFMloPJmOUAQk3gYv51SasIIP5_XErzzEtq8B7ElPBW-VDPB_SMauVFcH6FrmxC-KL6k10UBED4"
                        assertive::assert_any_are_not_na(c(csvfilename, csvfiledf, frameworkid, dashboardid, token), severity = "stop")
                        assertive::assert_any_are_na(c(csvfilename, csvfiledf), severity = "stop")
                        assertive::assert_any_are_na(c(frameworkid, dashboardid), severity = "stop")
                        if  (length(which(!is.na(c(csvfilename, csvfiledf)) == TRUE)) == 1 & length(which(!is.na(c(frameworkid, dashboardid)) == TRUE)) >= 1) {
                          print("you cannot have file name or file while also passing framework id or dashboard id")
                          stop()
                        }

                        if (length(which(!is.na(c(frameworkid, dashboardid)))) == 1 & is.na(token)) {
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

                        if (!is.na(frameworkid)) {
                          # is this a demonstrator account
                          is_demonstrator <- private$is_demonstrator(token)
                          df <- private$get_API_framework_data(end_point, frameworkid, token, is_demonstrator)
                        }
                        # preliminary process of the dashboard - get the framework id and the frqmework data.
                        if (!is.na(dashboardid)) {
                          # get the dashboard definition.
                          dashboard_definition <- private$getDashboardDefinition(end_point, dashboardid, token)
                          # add to the field as we need to use this later.
                          self$dashboard_definition <- dashboard_definition
                          # pick out the parent framework definition and get the data.
                          frameworkid <- dashboard_definition$framework_id
                          is_demonstrator <- private$is_demonstrator(token)
                          df <- private$get_API_framework_data(end_point, frameworkid, token, is_demonstrator)
                        }

                        # Now we have df as the data frame required for processing - but does the dataframe match the definition
                        # get the frameworkid if not present
                        if (is.na(frameworkid)) {
                          frameworkid <- df[1, "project_id"]
                        }

                        # get the sensemakerframeworkr object if it hasn't been passed
                        if (!("Signifiers" %in% class(sensemakerframeworkrobject))) {
                          sensemakerframeworkrobject <- sensemakerframeworkr::Signifiers$new(jsonfilename = NULL, layoutfilename = NULL, parsedjson = NULL, parsedlayout = NULL, workbenchid = frameworkid, token = token)
                        }

                        # check that the sensemakerframeworkr projectid is the same as the project id passed in or obtained in the data
                        if (frameworkid != sensemakerframeworkrobject$get_parent_framework_id()) {
                         print(paste("the frameworkid", frameworkid, "passed or obtained from the data, not the same as", sensemakerframeworkrobject$get_parent_framework_id(), "in the sensemakerframeworkr package object"))
                          stop()
                        }
                        # now process the dashboard completely - this function has side affects on self$dashboard_definition
                        if (!is.na(dashboardid)) {
                          private$get_API_dashboard_data(df, frameworkid, end_point, dashboardid, token, is_demonstrator)

                        }
                        # assign the sensemaker framework field
                        self$sm_framework <- sensemakerframeworkrobject

                        # start processing data
                        private$process_data(df, sensemakerframeworkrobject)


                      },

                      process_data = function(df, sensemakerframeworkrobject) {


                        # add the date fields

                      },

                      get_API_framework_data = function(end_point, frameworkID, token, isDemonstratorAccount = FALSE) {
                        # Get the data from the API saving to temp folder, read csv into dataframe, remove temp file and return dataframe
                        df1FileName <- tempfile(pattern = "", fileext = ".csv")
                        r1 <- httr::GET(paste0("http://", end_point, ".sensemaker-suite.com/apis/capture/", frameworkID,"/?type=csv", sep=""),
                                  httr::add_headers(.headers = c('Authorization'= paste('Bearer ', token))),
                                  httr::write_disk(df1FileName, overwrite=TRUE), httr::verbose())
                        DF1I <- read.csv(df1FileName, stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = "", as.is = TRUE, check.names = FALSE, nrows = ifelse(isDemonstratorAccount, 20, -5))
                         unlink(df1FileName)
                        return(DF1I)
                      },

                      # Get the dashboard data - df already contains the parent data.
                      #  1. Remove any primary framework signifier definitions that are not defined to the framework
                      #  2. Add any linked framework signifier definitions if this is a linked dashboard, if they are included in the secondary definitions
                      #  3. Add linked framework data to the data frame mapping ids where required
                      #. 2. Filter the data if any filters defined

                      get_API_dashboard_data = function(df, frameworkid, end_point, dashboardid, token, is_demonstrator) {





                      },

                      getDashboardDefinition = function(end_point, dashboardid, token) {

                        out <- try( {
                          # get the json from the returned project definition

                          return(jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://", end_point, ".sensemaker-suite.com/apis/dashboards/",  dashboardid),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                     , 'Content-Type' = 'application/json'))
                          ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))

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
