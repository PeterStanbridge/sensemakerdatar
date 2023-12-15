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
                      #' @field is_demonstrator Boolean - TRUE if the token is a demonstrator account.
                      is_demonstrator = NULL,
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
                      #' @field sm_framework the framework definition
                      sm_framework = NULL,
                      #' @field framework_definition The framework json for non-dashboard or dashbpard parent
                      framework_definition = NULL,
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

                      },
                      get_is_dashboard = function() {
                        if (!is.null(self$dashboard_id)) {return(TRUE)}
                        return(FALSE)
                      },

                      get_dashboard_definition = function() {
                        if (self$get_dashboard_version() == "v1") {
                          return(self$dashboard_definition_v1) } else {
                            return(self$dashboard_definition_v2)
                          }

                      },

                      get_is_combined = function() {
                        if (!self$get_is_dashboard()) {return(FALSE)}
                        if (self$get_dashboard_version() == "v1") {v_length = length(self$get_dashboard_definition()$config$frameworks_mapping)}
                        if (self$get_dashboard_version() == "v2") {v_length = length(self$get_dashboard_definition()$settings$idMap)}
                        if (v_length > 0) {
                          return(TRUE)
                        } else {
                          return(FALSE)
                        }
                      },
                      get_has_filters = function() {
                        if (self$get_dashboard_version() == "v1") {
                          ret_val <- any(self$dashboard_definition_v1$filters$showFilter == TRUE)
                          if (!ret_val) {
                            ret_val <- nrow(self$dashboard_definition_v1$filters  %>% dplyr::filter(id == "date_range_filter")) > 0
                          }
                        } else {
                          if (self$get_dashboard_version() == "v2") {
                            ret_val <- ifelse(is.null(self$dashboard_filters_v2[["value"]][["query"]]), FALSE, TRUE)
                          }
                        }

                        return(ret_val)

                      },

                      get_dashboard_version = function() {
                        if (!self$get_is_dashboard()) {return(NULL)}
                        return(self$dashboard_version)
                      },

                      get_dashboard_combined_mappings = function() {
                        if (self$get_dashboard_version() == "v1") {
                          return(self$get_dashboard_definition()$config$frameworks_mapping)
                        } else {
                          return(self$get_dashboard_definition()$settings$idMap)
                        }
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
                        token <- "eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCIsImtpZCI6IjIzYjM4ZjE4OGMzY2IzOWYwOGZkOTdmZTdiNmJlZDAzYjRmNGM5M2MifQ.eyJhdWQiOiJodHRwczovL3BsYXRmb3JtLnNlbnNlbWFrZXItc3VpdGUuY29tIiwiZXhwIjoxNzAyNjMyMTI3LCJpYXQiOjE3MDI2Mjg1MjcsImlzcyI6Imh0dHBzOi8vYXBpLnNpbmd1bGFyaXR5LmljYXRhbHlzdC5jb20vdjEvaXNzdWVyLzg5NjlhM2I4LWU5YmEtNGQ2ZC1iNjA4LTc0YzVjOTI5NmUxNCIsInN1YiI6IjI1YTAzM2NlYWFkOWU4NzA0MTFkNDdkOGNlOTU3ZWI2ZGE2NTc0MWM2Y2YwMzkyNzcwYmYzMGRjM2FiODRkZDciLCJub25jZSI6MC43MTcxMjQ4Nzc4NTcxMzc0LCJzY29wZSI6ImF1dGggcHJvZmlsZSIsImNsaWVudF9pZCI6Ijg5NjlhM2I4LWU5YmEtNGQ2ZC1iNjA4LTc0YzVjOTI5NmUxNCIsImdyYW50X3R5cGUiOiJhdXRob3JpemF0aW9uX2NvZGUiLCJyZWRpcmVjdF91cmkiOiJodHRwczovL3BsYXRmb3JtLnNlbnNlbWFrZXItc3VpdGUuY29tL3BsYXRmb3JtL3Rvb2xzL2ZyYW1ld29ya3MvYjFhZDc2Y2EtZDYwYS00YzlkLTliNjMtZGUzZDc4OTM3ODRiIiwicmVmcmVzaF90b2tlbiI6IjdjZDVlN2VlLThmMDktNDQ1Yy1iNzg5LWRhNzJkYjhiYjNmYyJ9.ACI3MvgYlNu1kHd9xPcIuZm09UhS9RbFsdynNhTt5RlDczr4OUJoVKMwHW80OrxYaL4upZrSnMqqTA41abR8Z9UnAbRdeq1IfB2GDpbWLyVJYgmFNSaNyzxEGNoA02Qga0RJ2MHwbCDs6aDvlQc3xbCqDsqJmmhmIOngpK8j96yfrEXD"
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
                          is_demonstrator <- private$get_is_demonstrator(token)
                          self$is_demonstrator <- is_demonstrator
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
                        is_demonstrator <- private$get_is_demonstrator(token)
                        self$is_demonstrator <- is_demonstrator
                        df <- private$get_API_framework_data(end_point, self$framework_id, token, is_demonstrator)
                        # apply dates as we wll need them for filtering
                        df <- private$apply_dates(df)


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

                        self$framework_definition <- sensemakerframeworkrobject$framework_json

                        # now process the dashboard completely - this function has side effects on the dashboard definition,
                        # data and framework definition
                        if (!is.na(dashboard_id)) {
                          df <- do.call(paste0("process_dashboard_definition_", self$dashboard_version), args = list(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject), envir = private)
                        }


                        # assign the sensemaker framework
                        self$sm_framework <- sensemakerframeworkrobject

                        # start processing data
                        df <- private$process_data(df, sensemakerframeworkrobject)
                        self$df1 <- df

                      },

                      process_data = function(df, sensemakerframeworkrobject) {

                        if ("meta_platform_verion" %in% colnames(df)) {
                          df[["meta_platform_verion"]] <- as.character( df[["meta_platform_verion"]])
                        }


                        # process dyads
                        # Add the new percentage X columns for the dyads, keeping the decemal columns in place - side effects on df
                        if(sensemakerframeworkrobject$get_dyad_count() > 0) {
                          purrr::walk(sensemakerframeworkrobject$get_dyad_ids(), ~ {df[[sensemakerframeworkrobject$get_dyad_aspercent_x_column_name(.x)]] <<- df[[sensemakerframeworkrobject$get_dyad_column_name(.x, column = "X", original = TRUE)]] * 100}, df)
                          # rename the dyad x column names - heaps of legacy code - side effect on df column names
                          purrr::walk(sensemakerframeworkrobject$get_dyad_ids(), ~ {names(df)[which(names(df) == sensemakerframeworkrobject$get_dyad_column_name(.x, column = "X", original = TRUE))] <<- sensemakerframeworkrobject$get_dyad_column_name(.x, column = "X", original = FALSE)}, df)
                        }


                        # process triads
                        if(sensemakerframeworkrobject$get_triad_count() > 0) {
                          # the triad Y column has to be multiplied by 0.8660254 as this is in reality not a percentage - side effects on df
                          purrr::walk(sensemakerframeworkrobject$get_triad_ids(), ~
                                        {df[[sensemakerframeworkrobject$get_triad_column_name(id = .x, column = "Y", original = TRUE)]] <<-
                                          df[[sensemakerframeworkrobject$get_triad_column_name(id = .x, column = "Y", original = TRUE)]] * 0.8660254}, df)
                          # turn triad anchor columns into %ages (multiply by 100) - side effects on df
                          purrr::walk(sensemakerframeworkrobject$get_triad_ids(), ~
                                        {purrr::walk(sensemakerframeworkrobject$get_triad_anchor_column_names(.x, delist = TRUE, exclude_na = TRUE),
                                                     ~ {df[[.x]] <<- df[[.x]] * 100}, df)}, df)
                          # Adjust and triad anchor data <= 0 - make slightly positive (important for compositional stats) - side effects on df
                          purrr::walk(sensemakerframeworkrobject$get_triad_ids(), ~
                                        {purrr::walk(sensemakerframeworkrobject$get_triad_anchor_column_names(.x, delist = TRUE, exclude_na = TRUE),
                                                     ~ {ifelse(df[[.x]] <= 0, 0.00001 , df[[.x]])}, df)}, df)
                          # rename the triad x and y columns names - heaps of legacy code - side effect on df column names
                          purrr::walk(sensemakerframeworkrobject$get_triad_ids(), ~
                                        {names(df)[which(names(df) == sensemakerframeworkrobject$get_triad_column_name(.x, column = "X", original = TRUE))] <<-
                                          sensemakerframeworkrobject$get_triad_column_name(.x, column = "X", original = FALSE);
                                        names(df)[which(names(df) == sensemakerframeworkrobject$get_triad_column_name(.x, column = "Y", original = TRUE))] <<-
                                          sensemakerframeworkrobject$get_triad_column_name(.x, column = "Y", original = FALSE)}, df)
                        }

                        # Process stones
                        if(sensemakerframeworkrobject$get_stones_count() > 0) {
                          # rename the stone column names - heaps of legacy code - side effect on df column names
                          purrr::walk(sensemakerframeworkrobject$get_stones_ids(), ~
                                        {purrr::walk2(sensemakerframeworkrobject$get_stones_compositional_column_names(id = .x, original = TRUE),
                                                      sensemakerframeworkrobject$get_stones_compositional_column_names(id = .x, original = FALSE),
                                                      ~ {names(df)[which(names(df) %in% .x)] <<- .y}, df)}, df)

                        }

                        # Processing lists
                        # holding of list levels until we have added all the other
                        # Add new columns and list definitions for each multi-select MCQ column to have "selected" or "not selected" - side effects on df
                        purrr::walk(sensemakerframeworkrobject$get_multiselect_list_ids(), ~
                                      { purrr::walk(sensemakerframeworkrobject$get_list_column_names(.x), ~
                                                      {df[[paste0(.x, "_selected")]] <<- private$process_col(.x, df)}, df)}, df)
                        # add these new columns to the framework definitions - side effect on the sensemakerframeworkrobject object

                        # add the NarrID column (used for filter indexing) and set the "id" column to "FragmentID" (reads better in the code)
                        df[["NarrID"]] <- 1:nrow(df)
                        names(df)[[which(names(df) == "id")]] <- "FragmentID"
                        sensemakerframeworkrobject$change_signifier_title(id = "e533b24f-1ac4-4506-8b10-0b1169cc7282", value = "fuck off if this works - wow")

                        return(df)


                      },

                      # set up the selected and not selected columns for a multi-select column
                      process_col = function(x, df) {
                        col_vals <- df[[x]]
                        return(unlist(unname(purrr::map(col_vals, ~ {ifelse(is.na(.x), "Not Selected", "Selected")}))))
                      },

                      apply_dates = function(df) {

                        df[["started"]] <- as.character(lubridate::as_datetime(df[["server_upload_time"]]/1000, tz = "GMT"))
                        df[["ServerEntryDate"]] <-  lubridate::as_date(df[["started"]] )
                        df[["EntryYrMth"]] <- as.integer(strftime(df[["ServerEntryDate"]], format = "%Y%m"))
                        df[["EntryYrMthDay"]] <- as.integer(strftime(df[["ServerEntryDate"]], format = "%Y%m%d"))
                        return(df)
                      },

                      get_API_framework_data = function(end_point, framework_id, token, isDemonstratorAccount = FALSE) {

                        # Get the data from the API saving to temp folder, read csv into dataframe, remove temp file and return dataframe
                        df1FileName <- tempfile(pattern = "", fileext = ".csv")
                        r1 <- httr::GET(paste0("http://", end_point, ".sensemaker-suite.com/apis/capture/", framework_id,"/?type=csv", sep=""),
                                        httr::add_headers(.headers = c('Authorization'= paste('Bearer ', token))),
                                        httr::write_disk(df1FileName, overwrite=TRUE), httr::verbose())
                        DF1I <- read.csv(df1FileName, stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = "", as.is = TRUE, check.names = FALSE, nrows = ifelse(isDemonstratorAccount, 20, -5))
                        unlink(df1FileName)

                        if (nrow(DF1I) > 0) {
                          # all meta-columns should be character
                          if ("definition_version" %in% colnames(DF1I)) {
                            DF1I[["definition_version"]] <- as.character(DF1I[["definition_version"]])
                          }
                          meta_cols <-  colnames(DF1I)[stringr::str_starts(colnames(DF1I), pattern = "meta_")]
                          for (col in meta_cols) {
                            DF1I[[col]] <- as.character( DF1I[[col]])
                          }
                        }


                        return(DF1I)
                      },

                      # Process the dashboad definition. This has side effects in the framework definition object and returns data
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

                        # if this is a combined dashboard, then get each mapping framework and process
                        if (self$get_is_combined()) {
                          df <- private$combine_data(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject)
                        }

                        if(self$get_has_filters()) {
                          df <- private$filter_data_v1(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject)
                        }

                        return(df)

                      },

                      filter_data_v1 = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject) {

                        # Get the framework signifier ids that are set to show filters TRUE
                        # signifier_ids <- unlist(unname(purrr::keep(unlist(unname(purrr::imap(self$dashboard_definition_v1$filters$attribute_id, ~ private$get_show_filters(.x, .y, self$dashboard_definition_v1$filters)))), ~ {!is.na(.x)})))

                        # Filter out those that have showFilter
                        filter_defs <- self$dashboard_definition_v1$filters %>% dplyr::filter(showFilter == TRUE)
                        query_string <- NULL

                        if (nrow(filter_defs) > 0) {
                          for (sig_id_idx in seq_along(filter_defs[, "attribute_id"]) ) {
                            attribute_id <- filter_defs[sig_id_idx, "attribute_id"]
                            if (sensemakerframeworkrobject$get_signifier_type(attribute_id) == "list") {
                              qry <- paste0(paste0("`", attribute_id, "`"), " == ", paste0("\"", filter_defs[sig_id_idx, "value"][[1]][["id"]], "\""))
                              query_string <-  ifelse(is.null(query_string), qry, paste(query_string, " & ", qry, collapse = ""))
                            }

                            if (sensemakerframeworkrobject$get_signifier_type(attribute_id) == "triad") {
                              # qry <- paste0(attribute_id, " == ", filter_defs[sig_id_idx, "value"][[1]][["id"]])
                              maxX <- filter_defs[sig_id_idx, "value"][[1]][["maxX"]]
                              maxY <- filter_defs[sig_id_idx, "value"][[1]][["maxY"]]
                              minX <- filter_defs[sig_id_idx, "value"][[1]][["minX"]]
                              minY <- filter_defs[sig_id_idx, "value"][[1]][["minY"]]
                              qry <- paste0(paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "X", original = TRUE), "`"), " >= ",  minX, " & ",
                                            paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "Y", original = TRUE), "`"), " >= ", minY, " & ",
                                            paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "X", original = TRUE), "`"), " <= ", maxX, " & ",
                                            paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "Y", original = TRUE), "`"), " <= ", maxY)
                              query_string <-  ifelse(is.null(query_string), qry, paste(query_string, " & ", qry, collapse = ""))
                            }

                            if (sensemakerframeworkrobject$get_signifier_type(attribute_id) == "dyad") {
                              maxX <- filter_defs[sig_id_idx, "value"][[1]][["maxX"]]
                              minX <- filter_defs[sig_id_idx, "value"][[1]][["minX"]]
                              qry <- paste0(paste0("`", sensemakerframeworkrobject$get_dyad_column_name(attribute_id, "X", original = TRUE), "`"), " >= ",  minX, " & ",
                                            paste0("`", sensemakerframeworkrobject$get_dyad_column_name(attribute_id, "X", original = TRUE), "`"), " <= ",  maxX)
                              query_string <-  ifelse(is.null(query_string), qry, paste(query_string, " & ", qry, collapse = ""))
                            }

                            if (sensemakerframeworkrobject$get_signifier_type(attribute_id) == "stones") {
                              maxX <- filter_defs[sig_id_idx, "value"][[1]][["maxX"]]
                              maxY <- filter_defs[sig_id_idx, "value"][[1]][["maxY"]]
                              minX <- filter_defs[sig_id_idx, "value"][[1]][["minX"]]
                              minY <- filter_defs[sig_id_idx, "value"][[1]][["minY"]]
                              qry <- NULL
                              for (stone_item_id in sensemakerframeworkrobject$get_stones_items_ids(attribute_id)) {
                                col_ids <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(attribute_id, stone_item_id, original = TRUE)
                                qry_entry <- paste0( "`", col_ids[[1]], "`", " >= ", minX, " & ", "`",  col_ids[[1]], "`", " <= ", maxX, " & ",
                                                     "`", col_ids[[2]], "`", " >= ", minY, " & ", "`",  col_ids[[2]], "`", " <= ", maxY, ")")
                                qry <- ifelse(is.null(qry), paste0("((", qry_entry), paste0(qry, " | ", "(", qry_entry))
                              }
                              qry <- paste0(qry, ")")
                              query_string <-  ifelse(is.null(query_string), qry, paste(query_string," & ", qry, collapse = ""))
                            }
                          }
                        }

                        # now do the date if exists
                        filter_defs <- self$dashboard_definition_v1$filters %>% dplyr::filter(id == "date_range_filter")
                        if (nrow(filter_defs) > 0) {
                          from_dte <- lubridate::as_date(filter_defs[["value"]][[1]][[1]])
                          to_dte <- lubridate::as_date(filter_defs[["value"]][[1]][[2]])
                          qry <- paste0("ServerEntryDate >= ", "\"",  from_dte, "\"",  " & ServerEntryDate <= ", "\"", to_dte, "\"")
                          query_string <- ifelse(is.null(query_string), qry, paste0(query_string, " & ", qry))
                        }
                        if (is.null(query_string)) {return(df)}
                        df_ret <- df %>% dplyr::filter(eval(parse(text = query_string)))
                        return(df_ret)
                      },

                      combine_data = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject) {
                        fw_mappings <- self$get_dashboard_combined_mappings()
                        for (fw_id in names(fw_mappings)) {
                          mcq_data <- NULL
                          # get combined framework data and load definition (maybe not)
                          fw_data <- private$get_API_framework_data(end_point, fw_id, token, self$is_demonstrator)

                          # fw_json <- private$get_framework_definition(end_point, fw_id, token)

                          if (nrow(fw_data) == 0) {next}
                          # map the fields - "end" is primary and "start" is the combined. "_" means a list item
                          for (i in seq_along(fw_mappings[[fw_id]][["end"]])) {
                            # A non list item
                            if (all(is.na(stringr::str_locate(fw_mappings[[fw_id]][i, "end"], "_")) == TRUE)) {
                              if ((any(grepl(fw_mappings[[fw_id]][i, "end"], colnames(df), fixed = TRUE) == TRUE)) & (any(grepl(fw_mappings[[fw_id]][i, "start"], colnames(fw_data), fixed = TRUE) == TRUE))) {
                                # if this is a multi-select list then process the underscore versions so next
                                if (sensemakerframeworkrobject$get_signifier_type(fw_mappings[[fw_id]][i, "end"]) == "list" &&  sensemakerframeworkrobject$get_list_max_responses(fw_mappings[[fw_id]][i, "end"]) > 1) {next}
                                # change the column names
                                colnames(fw_data) <- stringr::str_replace_all(colnames(fw_data), fw_mappings[[fw_id]][i, "start"], fw_mappings[[fw_id]][i, "end"])
                              }
                            } else {
                              # replace the column header if exists (will be multi-select)
                              if ( (fw_mappings[[fw_id]][i, "start"] %in% colnames(fw_data)) && (fw_mappings[[fw_id]][i, "end"] %in% colnames(df))) {
                                colnames(fw_data) <- stringr::str_replace_all(colnames(fw_data), fw_mappings[[fw_id]][i, "start"], fw_mappings[[fw_id]][i, "end"])
                              } else {
                                # single select - update the column and the data
                                # split end and start
                                end_sig_id <- unlist(stringi::stri_split(fw_mappings[[fw_id]][i, "end"], regex = "_"))[[1]]
                                end_item_id <- unlist(stringi::stri_split(fw_mappings[[fw_id]][i, "end"], regex = "_"))[[2]]
                                start_sig_id <- unlist(stringi::stri_split(fw_mappings[[fw_id]][i, "start"], regex = "_"))[[1]]
                                start_item_id <- unlist(stringi::stri_split(fw_mappings[[fw_id]][i, "start"], regex = "_"))[[2]]
                                if (is.null(mcq_data)) {
                                  mcq_data <- data.frame(end_sig_id = end_sig_id, end_item_id = end_item_id, start_sig_id = start_sig_id, start_item_id = start_item_id)
                                } else {
                                  temp_data <- data.frame(end_sig_id = end_sig_id, end_item_id = end_item_id, start_sig_id = start_sig_id, start_item_id = start_item_id)
                                  mcq_data <- dplyr::bind_rows(mcq_data, temp_data)
                                }
                              }
                            }
                          }
                          mcq_item_list <- vector("list", length = length(unique(mcq_data[["start_sig_id"]]))  )
                          names(mcq_item_list) <- unique(mcq_data[["start_sig_id"]])
                          mcq_start_list <- unique(mcq_data[["start_sig_id"]]) #names(mcq_item_list)
                          mcq_end_list <- unique(mcq_data[["end_sig_id"]])
                          mcq_list <- data.frame(start = mcq_start_list, end = mcq_end_list)

                          for (k in seq_along(mcq_list[["end"]])) {
                            entries <- mcq_data %>% dplyr::filter(end_sig_id == mcq_list[k, "end"])
                            sig_id  <- entries[1, "end_sig_id"]
                            sig_col <- fw_data[[sig_id]]
                            for (data_idx in seq_along(sig_col)) {
                              sig_col[[data_idx]] <- entries %>% dplyr::filter(start_item_id == sig_col[[data_idx]]) %>% dplyr::select(end_item_id)
                            }
                            fw_data[[sig_id]] <- as.character(sig_col)
                          }
                          df <- dplyr::bind_rows(df, fw_data)
                        }
                        return(df)
                      },


                      process_dashboard_definition_v2 = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject) {
                        dashboard_definition <- self$dashboard_layout_v2

                        # Set include FALSE for any signifier not defined to the dashboard.
                        framework_signifier_ids <- sensemakerframeworkrobject$get_all_signifier_ids(keep_only_include = TRUE)
                        dashboard_signifiers <- dashboard_definition$signifierID[which(!is.na(dashboard_definition$signifierID) & dashboard_definition$signifierID != "00000000-0000-0000-0000-000000000000")]
                        framework_signifiers_missing <- framework_signifier_ids[which(!(framework_signifier_ids %in% dashboard_signifiers))]
                        purrr::walk(framework_signifiers_missing, ~ {sensemakerframeworkrobject$change_signifier_include(.x, value = FALSE)}, sensemakerframeworkrobject)

                        # if this is a combined dashboard, then get each mapping framework and process
                        if (self$get_is_combined()) {
                          df <- private$combine_data(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject)
                        }

                        # If filters are defined, filter the data
                        if (self$get_has_filters()) {
                          df <-  private$filter_data_v2(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject)
                        }
                        return(df)
                      },

                      filter_data_v2 = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject) {
                        # Merely to help readability and help with coding.
                        filter_definitions <- self$dashboard_filters_v2
                        query_string <- NULL
                        for (i in seq_along(filter_definitions[["id"]])) {
                          # we are working with a real filter - this is in the JSON where there are other things going on in the filter JSON
                          if (!is.null(filter_definitions[i,]$value$query$bool$must[[1]])) {

                            if (filter_definitions[i,][["title"]] == "Timeline") {
                              gte <- filter_definitions[i, ]$value$query$bool$must[[1]][["range"]][["server_upload_time"]][["gte"]]
                              lte <- filter_definitions[i, ]$value$query$bool$must[[1]][["range"]][["server_upload_time"]][["lte"]]
                              qry <- paste0("server_upload_time >= ", gte, " & server_upload_time <= ", lte)
                              query_string <- ifelse(is.null(query_string), qry, paste0(" & ", qry))

                            } else {
                              attribute_id <- filter_definitions[i, ][["signifierID"]]
                              sig_type <- sensemakerframeworkrobject$get_signifier_type(attribute_id)
                              if (sig_type == "triad") {
                                x_gte <- filter_definitions[i, ][["value"]][["query"]][["bool"]][["must"]][[1]][["range"]][[1]]$gte[[1]]
                                x_lte <- filter_definitions[i, ][["value"]][["query"]][["bool"]][["must"]][[1]][["range"]][[1]]$lte[[1]]
                                y_gte <- filter_definitions[i, ][["value"]][["query"]][["bool"]][["must"]][[1]][["range"]][[2]]$gte[[2]]
                                y_lte <- filter_definitions[i, ][["value"]][["query"]][["bool"]][["must"]][[1]][["range"]][[2]]$lte[[2]]
                                qry <- paste0(paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "X", original = TRUE), "`"), " >= ",  x_gte, " & ",
                                              paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "Y", original = TRUE), "`"), " >= ", y_gte, " & ",
                                              paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "X", original = TRUE), "`"), " <= ", x_lte, " & ",
                                              paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "Y", original = TRUE), "`"), " <= ", y_lte)
                                query_string <-  ifelse(is.null(query_string), qry, paste(query_string, " & ", qry, collapse = ""))
                              }

                              if (sig_type == "dyad") {
                                x_gte <- filter_definitions[i, ][["value"]][["query"]][["bool"]][["must"]][[1]][["range"]][[1]]$gte[[1]]
                                x_lte <- filter_definitions[i, ][["value"]][["query"]][["bool"]][["must"]][[1]][["range"]][[1]]$lte[[1]]
                                qry <- paste0(paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "X", original = TRUE), "`"), " >= ",  x_gte, " & ",
                                              paste0("`", sensemakerframeworkrobject$get_triad_column_name(attribute_id, "X", original = TRUE), "`"), " <= ", x_lte)
                                query_string <-  ifelse(is.null(query_string), qry, paste(query_string, " & ", qry, collapse = ""))
                              }

                              if (sig_type == "list") {
                                term <- filter_definitions[i,]$value$query$bool$must[[1]][["term"]][[1]]
                                qry <- paste0("`", attribute_id, "`",  " == ", "\"", term, "\"")
                                query_string <- ifelse(is.null(query_string), qry, paste(query_string, " & ", qry, collapse = ""))
                              }

                              if (sig_type == "stones") {
                                stones_stones <- filter_definitions[i, ]$value$query$bool$must[[1]]$bool$should[[1]]$bool$must
                                x_gte <- filter_definitions[i, ]$value$query$bool$must[[1]]$bool$should[[1]]$bool$must[[1]]$range[[1]]$gte[[1]]
                                x_lte <- filter_definitions[i, ]$value$query$bool$must[[1]]$bool$should[[1]]$bool$must[[1]]$range[[1]]$lte[[1]]
                                y_gte <- filter_definitions[i, ]$value$query$bool$must[[1]]$bool$should[[1]]$bool$must[[1]]$range[[2]]$gte[[2]]
                                y_lte <- filter_definitions[i, ]$value$query$bool$must[[1]]$bool$should[[1]]$bool$must[[1]]$range[[2]]$lte[[2]]
                                qry <- NULL
                                for (stone_idx in 1:length(stones_stones)) {

                                  stones_stones_id <- stringr::str_split_i(string = colnames(filter_definitions[i, ]$value$query$bool$must[[1]]$bool$should[[1]]$bool$must[[stone_idx]]$range)[[1]], pattern = ".xVal", i = 1)
                                  col_ids <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(attribute_id, stones_stones_id, original = TRUE)
                                  qry_entry <- paste0( "`", col_ids[[1]], "`", " >= ", x_gte, " & ", "`",  col_ids[[1]], "`", " <= ", x_lte, " & ",                                                       "`", col_ids[[2]], "`", " >= ", y_gte, " & ", "`",  col_ids[[2]], "`", " <= ", y_lte, ")")
                                  qry <- ifelse(is.null(qry), paste0("((", qry_entry), paste0(qry, " | ", "(", qry_entry))
                                }
                                qry <- paste0(qry, ")")
                                query_string <-  ifelse(is.null(query_string), qry, paste(query_string," & ", qry, collapse = ""))
                              }
                            }
                          }
                        }

                        if(is.null(query_string)) {return(df)}
                        ret_df <- df %>% dplyr::filter(eval(parse(text = query_string)))
                        return(ret_df)




                      },

                      get_framework_definition = function(end_point, framework_id, token) {

                        return(jsonlite::fromJSON(httr::content(httr::GET(
                          paste0("https://", end_point, ".sensemaker-suite.com/apis/projectdefinition/",  framework_id),
                          httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                         , 'Content-Type' = 'application/json'))
                        ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))


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



                      get_is_demonstrator = function(trToken) {

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
