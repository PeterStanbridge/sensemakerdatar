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
                        if (is.data.frame(csvfiledf)) {
                          if ("Signifiers" %in% class(sensemakerframeworkrobject)) {
                            if (!all(unlist(unname(purrr::map(sensemakerframeworkrobject$get_all_signifier_ids(), ~ {length(grep(.x, colnames(csvfiledf))) > 0})))) == TRUE) {
                              print("File doesn't match the framework definition passed")
                              stop()
                            }
                          }
                        }
                        if (!is.data.frame(csvfiledf)) {
                          if ((any(!is.na(csvfiledf)))) {
                            print("csvfiledf should be a dataframe")
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
                          # if the framework definition passed in, check this
                          if (!all(unlist(unname(purrr::map(sensemakerframeworkrobject$get_all_signifier_ids(), ~ {length(grep(.x, colnames(df))) > 0})))) == TRUE) {
                            print("File doesn't match the framework definition passed")
                            stop()
                          }
                        }

                        if (is.data.frame(csvfiledf)) {
                          df <- csvfiledf
                        }

                        if (!is.na(frameworkid)) {
                          # is this a demonstrator account
                          is_demonstrator <- private$is_demonstrator(token)
                          df <- get_API_framework_data(end_point, frameworkid, token, is_demonstrator)
                        }

                        if (!is.na(dashboardid)) {
                          is_demonstrator <- private$is_demonstrator(token)
                          df <- get_API_dashboard_data(end_point, frameworkid, token, is_demonstrator)
                        }


                      },


                      get_API_framework_data = function(end_point, frameworkID, token, isDemonstratorAccount) {
                        # Get the data from the API saving to temp folder, read csv into dataframe, remove temp file and return dataframe
                        df1FileName <- tempfile(pattern = "", fileext = ".csv")
                        r1 <- GET(paste0("http://", end_point, ".sensemaker-suite.com/apis/capture/", frameworkID,"/?type=csv", sep=""),
                                  add_headers(.headers = c('Authorization'= paste('Bearer ', tparams))),
                                  write_disk(df1FileName, overwrite=TRUE),verbose())

                        DF1I <- read.csv(df1FileName, stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = "", as.is = TRUE, check.names = FALSE, nrows = ifelse(isDemonstratorAccount, 20, -5))
                        unlink(df1FileName)
                        return(DF1I)
                      },


                      is_demonstrator = function(trToken) {

                        out <- try({
                          #
                          isDemonstratorAccount <- FALSE
                          sessionDetails <- fromJSON(content(GET(
                            paste0("https://api.singularity.icatalyst.com/api/session"),
                            add_headers(.headers = c('Authorization' = paste("Bearer", trToken, sep = " ")
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
