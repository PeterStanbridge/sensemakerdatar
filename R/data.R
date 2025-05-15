# NOTE - Fields are public as coders will be able to add their own logic on any
#        thing framework that may not yet be available in the methods.
# try using base split to pass via a map2 or pmap
# ToDo - remove the individual data fields and keep only "data"
# N O T E !!!!!
# New fully recursive graph code will be generated with # new graph comment. All old code kept for now for backward compatibility commented # old method
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
                      #' @field export_data_list_names List of the data list names that are standard export data frames. NOTE - this will be user (coder) extensible
                      export_data_list_names = c("df1", "dat", "df_keep", "title_data", "df_chat_titles"),
                      #' @field is_invalid Boolean - TRUE if the attempt is invalid
                      is_invalid = FALSE,
                      #' @field has_zero_records Boolean - TRUE if the data retrieval returns no records
                      has_zero_records = FALSE,
                      #' @field demonstrator Boolean - TRUE if the token is a demonstrator account.
                      demonstrator = NULL,
                      # this will be a list of lists containing the data.
                      #' @field data The full list of data objects keyed by the name of the field. NOTE - this will be user (coder) extensible
                      data = list(NULL),
                      # Common lists used from within the data list- all initially set to full dataset
                      #  #' @field df1 The full dataset for any given framework, thus if linked framework selected, will have only the linked framework data, not the full set
                      #  #' NOTE Depreciated
                      #  df1 = NULL,
                      # #' @field dat  Filtered data so always the data being displayed based on a filter on the main df1 dataframe
                      # #'  NOTE Depreciated
                      # dat = NULL,
                      #  #' @field df_keep Always the full dataset even through linked fw selections- enables restore of full dataset when deselecting linked frameworks
                      #  #'  NOTE Depreciated
                      #  df_keep = NULL,
                      #  #' @field df_multi_select The transformed multi-select MCQ data
                      #  #'  NOTE Depreciated
                      #  df_multi_select = NULL,
                      #  #' @field df_multi_select_full The transformed multi-select MCQ data - always full set
                      #  #'  NOTE Depreciated
                      #  df_multi_select_full = NULL,
                      #  #' @field df_chat_titles same as df_titles but only those columns that are categorical and text
                      #   #' df_chat_titles
                      #   df_chat_titles = NULL,
                      #' @field stone_ratios Stone ratios of each of the stone canvases
                      stone_ratios = NULL,
                      #  #' @field stone_data Stone transformed data into long from from column form
                      #  stone_data = NULL,
                      #   #' @field title_data data containing titles, not ids
                      #   title_data = NULL,
                      #   #' @field title_use Filtered data containing titles not ids, used for display on a filter
                      #  title_use = NULL,
                      #' @field sm_framework the framework definition
                      sm_framework = NULL,
                      #' @field framework_definition_json The framework json for non-dashboard or dashbpard parent
                      framework_definition_json = NULL,
                      #' @field dashboard_definition_v1 The dashboard json for version 1 because we need to do work with it after declaring it
                      dashboard_definition_v1 = NULL,
                      #' @field dashboard_definition_v2 The dashboard json for version 2 because we need to do work with it after declaring it
                      dashboard_definition_v2 = NULL,
                      #' @field dashboard_layout_v2 The dashboard layout json for version 2 because we need to do work with it after declaring it
                      dashboard_layout_v2 = NULL,
                      #' @field dashboard_filters_v2 The dashboard filters json for version 2 because we need to do work with it after declaring it
                      dashboard_filters_v2 = NULL,
                      #' @field framework_id The framework id, if a dashboard, the underlying main framework id for dashboard
                      framework_id = NULL,
                      #' @field framework_title The title of the framework (dashboard will take the underlying framework title)
                      framework_title = NULL,
                      #' @field dashboard_id The dashboard id
                      dashboard_id = NULL,
                      #' @field dashboard_title The title of the dashboard
                      dashboard_title = NULL,
                      #' @field dashboard_combined_frameworks_names Named vector of the combined dashboard frameworks. Names with ids as list title
                      dashboard_combined_frameworks_names = NULL,
                      #' @field dashboard_combined_frameworks_ids reverse of above. ids as content, titles as names
                      dashboard_combined_frameworks_ids = NULL,
                      #' @field dashboard_version The dashboard version - whether 1 or 2
                      dashboard_version = NULL,
                      #' @field polymorphic_definition_json The jsonlite package parsed json for polymorphic definitions if they exist
                      polymorphic_definition_json = NULL,
                      #' @field polymorphic_signifiers The parsed jsonlite package parsed json for polymorphic definitions if they exist
                      polymorphic_signifiers = NULL,
                      #' @field has_polymorphic_signifiers The parsed framework has polymorphic signifieres
                      has_polymorphic_signifiers = FALSE,
                      #' @field fragment_level_upload dataframe of fragment level list updates to data
                      fragment_level_upload = NULL,
                      #' @field FK_level_upload dataframe of fragment level list updates to data
                      FK_level_upload = NULL,
                      #' @field stop_words - stop words specific to this framework to use in any NLP requirements.
                      stop_words = NULL,
                      #' @field col_names_to_remove - column names to remove from exports.
                      col_names_to_remove = NULL,
                      #' @field sig_ids_to_remove - Signifier ids to remove from exports.
                      sig_ids_to_remove = NULL,
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
                      #' @param polymorphic_definition_json - jsonlite parsed json file of the polymorphic definition - default blank
                      #' @param fragment_level_csv - a csv file containing fragment level additional data columns to add to the dataset and define as list items for analysis
                      #' @param fragment_level_parsed - an already parsed (read) csv as fragment level additional data columns
                      #' @param FK_level_csv - a csv file containing additional data columns linked by a FK - i.e. the id values of a signifier other than the fragmentID - e.g. county linked to city id
                      #' @param FK_level_parsed - an already parsed (read) csv as additional data columns linked by FK
                      #' @param upload_na_identifier - The identifier in the csv files for NA values - this should be consistent through all files. Typically "" or "NA"
                      #' @return A new `signifier` R6 class object and fields type by signifier id, signifier ids by type, and
                      initialize = function(csvfilename = "", csvfiledf = "", framework_id = "", dashboard_id = "",
                                            token = "", sensemakerframeworkrobject = "", polymorphic_definition_json = "", fragment_level_csv = "",
                                            fragment_level_parsed = "", FK_level_csv = "", FK_level_parsed = "", upload_na_identifier = "") {
                        sensemakerdata <- private$get_data(csvfilename, csvfiledf, framework_id, dashboard_id, token, sensemakerframeworkrobject,
                                                           polymorphic_definition_json, fragment_level_csv, fragment_level_parsed,
                                                           FK_level_csv, FK_level_parsed, upload_na_identifier)

                      },
                      #' @description Add stop words associated with the fragments in this framework data
                      #' @param stop_words - A vector of stopwords, setting to NULL will clear the stop_list
                      #' @param add_replace - default "replace". "append" or "replace" any stopwords previously added.
                      add_stop_words = function(stop_words, add_replace = "replace") {
                        stopifnot(add_replace %in% c("replace", "append"))
                        if (!is.null(stop_words)) {
                          stopifnot(is.vector(stop_words))
                          stopifnot(length(stop_words) > 0)
                        } else {
                          self$stop_words <- NULL
                          #return()
                        }
                        if (add_replace == "replace") {
                          self$stop_words <- stop_words
                        } else {
                          self$stopwords <- append(self$stop_words, stop_words)
                        }
                        #return()
                      },
                      #' @description Add a new data frame name to the export data list names. These are the data frames in list "data" that are
                      #' standard export format.
                      #' @param new_name - the name of a new data frame
                      add_export_data_list_names = function(new_name) {
                        self$export_data_list_names <- append(self$export_data_list_names, new_name)
                      },
                      #' @description Add a new data frame name to the data list object
                      #' @param data_frame - the data frame to add
                      #' @param name - the name to give the data frame in the data list.
                      #' @param add_to_export_list_names - if this is a standard data data frame then whether to add to the
                      add_data_data_frame = function(data_frame, name, add_to_export_list_names = FALSE) {
                        temp_list <- vector("list", length = 1)
                        names(temp_list) <- name
                        temp_list[[1]] <- data_frame
                        self$data <- append(self$data, temp_list)
                        if (add_to_export_list_names) {self$add_export_data_list_names(name)}
                      },
                      #' @description get the data lists names
                      #' @param export_only default FALSE, if TRUE return only those that are in the export list names
                      #' These are names of data entries that are standard SM data frames
                      get_data_list_names = function(export_only = FALSE) {
                        if (export_only) {return(self$get_export_data_list_names())}
                        return(names(self$data))
                      },
                      #' @description get the data list names that are standard export data data frames
                      #' @returns A vector of export data list names
                      get_export_data_list_names = function() {
                        return(self$export_data_list_names)
                      },
                      #' @description get the data object (data frame) from data list object
                      #' @param name - the data list name to return (for names use "get_export_data_list_names function
                      #' @param as_tibble - default FALSE, if TRUE, export as a tidyverse tibble object.
                      #' for those that are standard export data frames or get_data_list_names function for all of them)
                      #' @returns A data list data object (normally data frame)
                      get_data_dataframe = function(name, as_tibble = FALSE) {
                        if (as_tibble) {
                          return(as_tibble(self$data[[name]]))
                        } else {
                          return(self$data[[name]])
                        }
                      },
                      #' @description Add stone regions to the data and framework definition from a file of region zones for a single stones id.
                      #' @param region_file - Default "dat", one of the data frames in the export data list (get_export_data_list_names method)
                      #' @param stones_id - Default NULL, a signifier id from the framework. Either this or the col_name must be provided.
                      #' @returns
                      add_stone_regions = function(region_file, stones_id) {
                        stopifnot(stones_id %in% self$sm_framework$get_stones_ids())
                        stopifnot(!is.null(region_file))
                        stopifnot(trimws(region_file) != "")
                        stopifnot((class(region_file) %in% c("data.frame", "character")))
                        if (class(region_file) == "character") {
                          stopifnot(stringr::str_ends(region_file, ".csv"))
                          stopifnot(file.exists(region_file))
                          region_file <- read.csv(region_file, check.names = FALSE, stringsAsFactors = FALSE)
                          stopifnot(all(c("name",	"xmin",	"xmax",	"ymin",	"ymax") %in% colnames(region_file)))
                        } else {
                          stopifnot(all(c("name",	"xmin",	"xmax",	"ymin",	"ymax") %in% colnames(region_file)))
                        }
                        if ("seq" %in% colnames(region_file)) {
                          stopifnot(is.numeric(region_file[["seq"]]))
                          # sort in sequence order
                          region_file <- region_file |> dplyr::arrange(seq)
                        }


                        # get the stone ids for the stones id passed in
                        stone_ids <- self$sm_framework$get_stones_stone_ids(id = stones_id)

                        # we process one at a time.
                        purrr::walk(stone_ids, function(stone_id) {
                          region_values <<- vector("list", length = nrow(self$data[["df1"]]))
                          names(region_values) <- self$data[["df1"]][["FragmentID"]]
                          FragmentIDs <- self$data[["df1"]][["FragmentID"]]
                          col_names <- self$sm_framework$get_stones_stone_compositional_column_names(sig_id = stones_id, stone_id = stone_id)
                          col_xs <- self$data[["df1"]][[col_names[[1]]]]
                          col_ys <- self$data[["df1"]][[col_names[[2]]]]
                          purrr::pwalk(list(FragmentIDs, col_xs, col_ys), function(fragment_id, x_value, y_value) {
                            region_values[[fragment_id]] <<- self$get_stone_region(region_file, x_value, y_value)
                          })

                          temp_df <- data.frame(FragmentID = names(region_values), place_holder = unlist(unname(region_values)))
                          names(temp_df)[2] <-  paste0(stones_id, "_", stone_id, "_Region")
                          self$add_column_to_dataframe(temp_df)
                          # Now add the values as a list in the framework definition
                          reg_title <-  paste(self$sm_framework$get_signifier_title(stones_id), self$sm_framework$get_stones_stone_title_by_id(stones_id, stone_id), "region")
                          items <- sort(region_file[["name"]])
                          item_df <- data.frame(id = items, title = items, tooltip = items, visible = rep_len(TRUE, length(items)), other_signifier_id = rep_len("", length(items)))
                          self$sm_framework$add_list(title = reg_title, tooltip = reg_title, allow_na = FALSE, fragment = FALSE, required = TRUE, sticky = FALSE, items = item_df,
                                                     max_responses = 1, min_responses = 1, other_item_id = "", other_signifier_id = "", sig_class = "region", id = paste0(stones_id, "_", stone_id, "_Region"))

                        })

                      },
                      #' @description Add a column to one or more of the project data frames based on one record per fragment.
                      #' @param df - Data frame to add to the data sets
                      #' @param data_frame - Default NULL, the name of the data frame to add the data too; must be in the Data list.
                      #' @param add_to_data_list_dfs - Default TRUE, add to all of the standard fragment data frames in DATA.
                      #' @returns
                      add_column_to_dataframe = function(df, data_frame = NULL, add_to_data_list_dfs = TRUE) {
                        stopifnot(is.logical(add_to_data_list_dfs))
                        stopifnot((!is.null(data_frame) & isFALSE(add_to_data_list_dfs)) | (is.null(data_frame) & isTRUE(add_to_data_list_dfs)))
                        if (!is.null(data_frame)) {
                          stopifnot(is.character(data_frame))
                          stopifnot(data_frame %in% self$get_export_data_list_names())
                        }
                        stopifnot(is.data.frame(df))
                        stopifnot(colnames(df)[[1]] == "FragmentID")
                        if (isTRUE(add_to_data_list_dfs)) {
                          data_frame <- self$get_export_data_list_names()
                        }
                        purrr::walk(data_frame, function(df_name) {
                          if (length(colnames(self$data[[df_name]])) == length(unique(colnames(self$data[[df_name]])))) {
                            self$data[[df_name]] <<- dplyr::left_join(x = self$data[[df_name]], y = df, by = dplyr::join_by(FragmentID))
                          }
                        })


                      },

                      #' @description Add a foreign key column to one or more of the project data frames based on a column value in the data frame.
                      #' @param df - Data frame to add to the data sets
                      #' @param fk_column - The column name from the data frame to act as the foreign key lookup. Must also exist in df.
                      #' @param data_frame - Default NULL, the name of the data frame to add the data too; must be in the Data list and have FragmentID as column name. .
                      #' @param add_to_data_list_dfs - Default TRUE, add to all of the standard fragment data frames in DATA with FragmentID as column name.
                      #' @returns
                      add_fk_column_to_dataframe = function(df, fk_column, data_frame = NULL, add_to_data_list_dfs = TRUE) {

                        stopifnot(is.logical(add_to_data_list_dfs))
                        stopifnot((!is.null(data_frame) & isFALSE(add_to_data_list_dfs)) | (is.null(data_frame) & isTRUE(add_to_data_list_dfs)))
                        stopifnot(is.data.frame(df))
                        stopifnot(fk_column %in% colnames(df))
                        if (!is.null(data_frame)) {
                          stopifnot(is.character(data_frame))
                          stopifnot(data_frame %in% self$get_export_data_list_names())
                          stopifnot("FragmentID" %in% colnames(self$data[[data_frame]]))
                          stopifnot(fk_column %in% colnames(self$data[[data_frame]]))
                          # cannot have a value in the data frame being updated not in the df file. Otherwise a missing value to swap.
                          stopifnot(all(unique(self$data[[data_frame]][[fk_column]]) %in% unique(df[[fk_column]])))
                        }





                      },


                      #' @description Add a multi_select MCQ interpretive column to one or more of the project data frames based on a column value in the data frame.
                      #' @param ms_df - Data frame to add to the data sets, which must have the column names "new_id"	"sig_id"	"content_id"	"value". sig_id is the multiselect list id, content_id the list item ids.
                      #' @param data_frame - Default NULL, the name of the data frame to add the data too; must be in the Data list and have FragmentID as column name. .
                      #' @param add_to_data_list_dfs - Default TRUE, add to all of the standard fragment data frames in DATA with FragmentID as column name.
                      #' @returns NULL - adds columns to the data frames.
                      add_ms_column_to_dataframe = function(ms_df, data_frame = NULL, add_to_data_list_dfs = TRUE) {

                        stopifnot(is.logical(add_to_data_list_dfs))
                        stopifnot((!is.null(data_frame) & isFALSE(add_to_data_list_dfs)) | (is.null(data_frame) & isTRUE(add_to_data_list_dfs)))
                        stopifnot(is.data.frame(ms_df))
                        # The dataframe ms_df must have the columns "new_id"	"sig_id"	"content_id"	"value"
                        stopifnot(all(c("new_id",	"sig_id",	"content_id",	"value") %in% colnames(ms_df)))
                        if (!is.null(data_frame)) {
                          stopifnot(is.character(data_frame))
                          stopifnot(data_frame %in% self$get_export_data_list_names())
                        }
                        # add the col_name column if it isn't there
                        if (!("col_name" %in% colnames(ms_df))) {
                          ms_df <- ms_df |> dplyr::mutate(col_name = paste0(sig_id, "_", content_id))
                        }

                        # get the data frames in the data list that have the multi-select mcq (as an id) in it and use these for transform.

                        if (is.null(data_frame)) {
                          df_list <<- NULL
                          purrr::walk(self$get_data_list_names(export_only = TRUE), function(df_name) {
                            if (all(unique(ms_df[["col_name"]]) %in% colnames(self$data[[df_name]]))) {
                              df_list <<- append(df_list, df_name)
                            }


                          })
                          data_frame <- df_list
                          df_list <<- NULL
                        }

                        # go through each of the new columns to add
                        purrr::walk(unique(ms_df[["new_id"]]), function(name) {
                          # filter out this data
                          mcq_values <- ms_df |> dplyr::filter(new_id == name)
                          # There should only be one value for the sig_id column now filtered
                          id <- unique(mcq_values[["sig_id"]])
                          stopifnot(length(id) == 1)
                          # go through each of the data frames and add the new column
                          purrr::walk(data_frame, function(df_name) {

                            self$data[[df_name]] <<- self$data[[df_name]] |> dplyr::mutate(!! name := self$get_ms_value(self$data[[df_name]], id, mcq_values))
                            # now add the new mcq to the framework definition
                            if (!(name %in% self$sm_framework$get_list_ids())) {
                              items <- sort(unique(self$data[[df_name]][[name]]))
                              list_items <- data.frame(id = items, title = items, tooltip = items, visible = c(TRUE, TRUE), other_signifier_id = c("", ""))

                              self$sm_framework$add_list(title = name, tooltip = name, allow_na = FALSE, fragment = FALSE, required = TRUE, sticky = FALSE,items = list_items,
                                                         max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL, sig_class = "signifier",
                                                         theader = NULL, id = name)
                            }
                          })
                        })
                        return(NULL)
                      },

                      #' @description Returns a list of mapped multi-select list values from a matching table and applies to a main fragment data dataframe. This is a bit of a helper function for the add_ms_column_to_dataframe function above, but could be useful for public use.
                      #' @param df - A main fragment signified data frame such as the df1 or dat dataframe to apply a multi-select map process.
                      #' @param id - The multi-select list signifier id to map.
                      #' @param mcq_values - The mapping for the passed in id. Columns should have col_name a concatenation of the id and item id for each item for the list and a value column for the mapped value.
                      #' @returns A vector containing for each row of the data frame df, the mapped value.
                      get_ms_value = function(df, id, mcq_values) {
                        ret_vec <- vector(mode = "list", length = nrow(df))

                        data_values <- df[, self$sm_framework$get_list_column_names(id = id)]

                        for (i in 1:nrow(df)) {
                          ret_vec[[i]] <- self$get_single_ms_value(data_values[i, ], mcq_values)

                        }

                        return(unlist(ret_vec))

                      },

                      #' @description Returns a single value for the mapping of a multi-select row from the signifier data frame and a mapping file. This is a bit of a helper function for the get_ms_value function above, but could be useful for public use.
                      #' @param dta - A single instance of a multi-select MCQ data columns - will contain a set of 1 or NA values.
                      #' @param mcq_values - The mapping for the data for the list being processed. Columns should have col_name a concatenation of the id and item id for each item for the list and a value column for the mapped value.
                      #' @returns A single value, which will either be "none" (if all the data values are NA), "multiple" (if the mapping maps to more than one value) or the value mapped.
                      get_single_ms_value = function(dta, mcq_values) {

                        long_df <- tidyr::gather(data = dta, key = col, sel, colnames(dta))

                        if (all(is.na(long_df[["sel"]]))) {
                          return("none")
                        }
                        long_df_sel <- long_df |> dplyr::filter(sel == 1)
                        mcq_values_selected <- mcq_values |> dplyr::filter(col_name %in% long_df_sel[["col"]]) |> dplyr::select(value)
                        if (length(unique(mcq_values_selected[["value"]])) == 1) {
                          return(unique(mcq_values_selected[["value"]]))
                        } else {
                          return("multiple")
                        }

                      },



                      #' @description return the region name from a stone region data frame.
                      #' @param region_df - a data frame with cols name, xmin, xmax, ymin, ymax and seq  (df sorted by seq)
                      #' @param col_x - value of xRight in stone data. (or NA).
                      #' @param col_y - value of yTop in stone data. (or NA)
                      #' @returns The stone region the stone drop is placed in.
                      get_stone_region = function(region_df, col_x, col_y) {
                        stopifnot(class(region_df) == "data.frame")
                        # if NA then there will be no zone
                        if (any(is.na(c(col_x, col_y)))) {
                          return("no_value")
                        }
                        # Set up lists for pwalk through each of the region columns
                        names <- region_df[["name"]]
                        xmins <- region_df[["xmin"]]
                        xmaxs <- region_df[["xmax"]]
                        ymins <- region_df[["ymin"]]
                        ymaxs <- region_df[["ymax"]]
                        ret_value <<- "outside_regions"
                        # simple check the passed in x and y value with the ranges and set the name for the region
                        for (i in seq_along(names)) {
                          name <- names[[i]]
                          xmin <- xmins[[i]]
                          xmax <- xmaxs[[i]]
                          ymin <- ymins[[i]]
                          ymax <- ymaxs[[i]]
                          if (col_x >= xmin & col_x <= xmax &  col_y >= ymin & col_y <= ymax) {
                            ret_value <- name
                            break
                          }
                        }
                        # purrr::pwalk(list(names, xmins, xmaxs, ymins, ymaxs), function(name, xmin, xmax, ymin, ymax) {
                        #  if (col_x >= xmin & col_x <= xmax &  col_y >= ymin & col_y <= ymax) {ret_value <<- name}
                        #})
                        return(ret_value)
                      },

                      #' @description get the data count of those responends not responding to a signifier. If the signifier is required then count is zero.
                      #' @param df_name - Default "dat", one of the data frames in the export data list (get_export_data_list_names method)
                      #' @param sig_id - Default NULL, a signifier id from the framework. Either this or the col_name must be provided.
                      #' @param col_name - Default NULL, a column name from the data. Either this or the sig_id must be provided
                      #' @returns Count of the number of non-responses by the respondent.
                      get_not_responded_count = function(df_name = "dat", sig_id = NULL, col_name = NULL) {

                        stopifnot(df_name %in% self$get_export_data_list_names())
                        stopifnot(any(c(is.null(sig_id), is.null(col_name))))
                        stopifnot(!all(c(is.null(sig_id), is.null(col_name))))

                        if (!is.null(col_name)) {
                          stopifnot((col_name %in% colnames(self$data[[df_name]])))
                          if (col_name %in% self$sm_framework$get_all_signifier_ids(sig_class = "signifier")) {
                            sig_id <- col_name
                          } else {
                            sig_id <- get_signifier_id_from_column_name(col_name)
                          }
                          if (is.null(sig_id)) {
                            return(NULL)
                          }
                        }

                        if (!is.null(sig_id)) {
                          stopifnot(sig_id %in% self$sm_framework$get_all_signifier_ids(sig_class = c("signifier", "date")))
                          # if (self$sm_framework$get_signifier_required(sig_id)) {
                          #   return(0)
                          # } else {
                          non_entry <- length(which(is.na(self$data[[df_name]][[self$sm_framework$get_a_col_name(sig_id)]]) == TRUE))
                          if (self$sm_framework$get_signifier_allow_na(sig_id)) {
                            not_applicables_count <- length(which(self$data[[df_name]][[self$sm_framework$get_signifier_na_column_name(sig_id)]] == 1))
                            non_entry <- non_entry - not_applicables_count
                          }
                          return(non_entry)
                          # }
                        }
                      },
                      #' @description get the data count of those responends not responding to a signifier. If the signifier is required then count is zero.
                      #' @param df_name - Default "dat", one of the data frames in the export data list (get_export_data_list_names method)
                      #' @param sig_id - Default NULL, a signifier id from the framework. Either this or the col_name must be provided.
                      #' @param col_name - Default NULL, a column name from the data. Either this or the sig_id must be provided
                      #' @returns Count of the number of non-responses by the respondent.
                      get_not_applicable_count = function(df_name = "dat", sig_id = NULL, col_name = NULL) {

                        stopifnot(df_name %in% self$get_export_data_list_names())
                        stopifnot(any(c(is.null(sig_id), is.null(col_name))))
                        stopifnot(!all(c(is.null(sig_id), is.null(col_name))))

                        if (!is.null(col_name)) {
                          stopifnot((col_name %in% colnames(self$data[[df_name]])))
                          if (col_name %in% self$sm_framework$get_all_signifier_ids(sig_class = "signifier")) {
                            sig_id <- col_name
                          } else {
                            sig_id <- get_signifier_id_from_column_name(col_name)
                          }
                          if (is.null(sig_id)) {
                            return(NULL)
                          }
                        }

                        if (!is.null(sig_id)) {
                          stopifnot(sig_id %in% self$sm_framework$get_all_signifier_ids(sig_class = c("signifier", "date")))
                          if (!self$sm_framework$get_signifier_allow_na(sig_id)) {
                            return(0)
                          } else {
                            return(length(which(self$data[[df_name]][[self$sm_framework$get_signifier_na_column_name(sig_id)]] == 1)))
                          }
                        }
                      },
                      #' @description get the signifier ID from a signifier/zone etc column name.
                      #' @param col_name - Column name
                      #' @returns The signifier ID associated with the column name else NULL
                      get_signifier_id_from_column_name = function(col_name) {
                        if (col_name %in% self$sm_framework$get_all_signifier_ids(sig_class = "signifier")) {return(col_name)}
                        split_string <- stringr::str_split_1(string = col_name, pattern = "_")
                        if (length(split_string) == 1) {
                          if (stringr::str_ends(col_name, "XR")) {
                            return(stringr::str_remove(col_name, "XR"))
                          }
                          if (stringr::str_ends(col_name, "X")) {
                            return(stringr::str_remove(col_name, "X"))
                          }
                          if (stringr::str_ends(col_name, "Y")) {
                            return(stringr::str_remove(col_name, "Y"))
                          }
                          if (col_name %in% self$sm_framework$get_all_signifier_ids(sig_class = "date")) {
                            return(col_name)
                          }
                        } else {
                          return(split_string[[1]])
                        }
                        return(NULL)
                      },
                      #' @description
                      #' is the account accessing this framework a demonstrator account, TRUE or FALSE.
                      #' @return TRUE or FALSE
                      is_demonstrator = function() {
                        return(self$demonstrator)
                      },
                      #' @description
                      #' is the definition being accessed a dashboard, TRUE or FALSE.
                      #' @return TRUE or FALSE
                      is_dashboard = function() {
                        if (!is.null(self$dashboard_id)) {return(TRUE)}
                        return(FALSE)
                      },
                      #' @description
                      #' get the dashboard parsed json definition.
                      #' @return The parsed dashboard definition
                      get_dashboard_definition = function() {
                        if (self$get_dashboard_version() == "v1") {
                          return(self$dashboard_definition_v1) } else {
                            return(self$dashboard_definition_v2)
                          }
                      },
                      #' @description
                      #' get wheter the dashboard is a combined dashboard or not, TRUE or FALSE.
                      #' @return TRUE or FALSE
                      is_combined_dashboard = function() {
                        if (!self$is_dashboard()) {return(FALSE)}
                        if (self$get_dashboard_version() == "v1") {v_length = length(self$get_dashboard_definition()$config$frameworks_mapping)}
                        if (self$get_dashboard_version() == "v2") {v_length = length(self$get_dashboard_definition()$settings$idMap)}
                        if (v_length > 0) {
                          return(TRUE)
                        } else {
                          return(FALSE)
                        }
                      },
                      #' @description
                      #' get whether the dashboard is a combined dashboard or not, TRUE or FALSE.
                      #' @return TRUE or FALSE
                      dashboard_has_filters = function() {
                        if (!self$is_dashboard()) {return(NULL)}
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
                      #' @description
                      #' Get the dashboard version.
                      #' @return v1 or v2
                      get_dashboard_version = function() {
                        if (!self$is_dashboard()) {return(NULL)}
                        return(self$dashboard_version)
                      },
                      #' @description
                      #' Get the dashboard combined signifier mappings if it is a combined dashboard
                      #' @return Parsed json of the signifier mapping specifications
                      get_dashboard_combined_mappings = function() {
                        if (!self$is_dashboard()) {return(NULL)}
                        if (!self$is_combined_dashboard()) {return(NULL)}
                        if (self$get_dashboard_version() == "v1") {
                          return(self$get_dashboard_definition()$config$frameworks_mapping)
                        } else {
                          return(self$get_dashboard_definition()$settings$idMap)
                        }
                      },
                      #' @description
                      #' Get the framework_id (if a dashboard, the framework id will be different to the dashboard id passed in)
                      #' @return The framework id
                      get_framework_id = function() {
                        return(self$framework_id)
                      },
                      #' @description
                      #' Get the framework title (if a dashboard, the framework title will be different to the dashboard title)
                      #' @return The framework id
                      get_framework_title = function() {
                        return(self$framework_title)
                      },
                      #' @description
                      #' Get the dashboard id
                      #' @return The dashboard id
                      get_dashboard_id = function() {
                        if (!self$is_dashboard()) {return(NULL)}
                        return(self$dashboard_id)
                      },
                      #' @description
                      #' Get the dashboard title
                      #' @return The dashboard title
                      get_dashboard_title = function() {
                        if (!self$is_dashboard()) {return(NULL)}
                        return(self$dashboard_title)
                      },
                      #' @description
                      #' Get the names and ids for the frameworks making up the combined dashboard
                      #' @param include_id - if TRUE, return the ids as the list elements (Default TRUE)
                      #' @param only_id - if TRUE, return the ids and not the names (as list names)
                      #' @param include_parent - if TRUE, incudes the parent framework along with the combined ones .
                      #' @return List of framework ids with title as names (or vector of ids if only_id TRUE)
                      get_combined_dashboard_frameworks_for_dropdown = function(include_id = TRUE, only_id = FALSE, include_parent = FALSE) {
                        if (!self$is_dashboard()) {return(NULL)}
                        if (!self$is_combined_dashboard()) {return(NULL)}
                        parent_fw <- vector("list", length = 1)
                        parent_fw[[1]] <- self$get_framework_id()
                        names(parent_fw) <-  self$get_framework_title()
                        if (only_id) {
                          if (include_parent) {
                            return(unlist(unname((append(parent_fw, self$dashboard_combined_frameworks_ids)))))
                          } else {
                            return(unlist(unname((self$dashboard_combined_frameworks_ids))))
                          }
                        }
                        if (include_id) {
                          if (include_parent) {
                            return(append(parent_fw, self$dashboard_combined_frameworks_ids))
                          } else {
                            return(self$dashboard_combined_frameworks_ids)
                          }
                        }
                        if (include_parent) {
                          return(names(append(parent_fw, self$dashboard_combined_frameworks_names)))
                        } else {
                          return(names(self$dashboard_combined_frameworks_names))
                        }
                        return(ret_val)
                      },
                      #' @description
                      #' Get the data entry names (names in the data field)
                      #' @param return_type "system" returns on the systemm ones, "user" only the user defined, "both" both
                      #' @return The names in the data data.frame
                      get_framework_data_names = function(return_type = "system") {
                        if (!(return_type %in% c("user", "system", "both"))) {return(NULL)}
                        system <- c("df1", "dat", "df_keep", "df_multi_select", "df_multi_select_full", "stone_data", "title_data", "title_use")
                        if (return_type == "system") {return(system)}
                        if (return_type == "both") {return(names(self$data))}
                        if (return_type == "user") {return(names(self$data[-pmatch(system, names(self$data))]))}
                      },
                      #' @description
                      #' Get the df1 data
                      #' @return The df1 data data.frame
                      get_df1_data = function() {
                        return(self$data[["df1"]])
                      },
                      #' @description
                      #' Get the dat data
                      #' @return The dat data data.frame
                      get_dat_data = function() {
                        return(self$data[["dat"]])
                      },
                      #' @description
                      #' Get the df_keep data
                      #' @return The df_keep data data.frame
                      get_df_keep_data = function() {
                        return(self$data[["df_keep"]])
                      },
                      #' @description
                      #' Get the df_multi_select data
                      #' @param id - the mcq to be returned. Optional, NULL will bring back all MCQs.
                      #' @return The df_multi_select data data.frame
                      get_df_multi_select_data = function(id = NULL) {
                        if(is.null(id)) {
                          return(self$data[["df_multi_select"]])
                        } else {
                          return(self$data[["df_multi_select"]][[id]])
                        }
                      },
                      #' @description
                      #' Get the df_multi_select_full data
                      #' @param id - the mcq to be returned. Optional, NULL will bring back all MCQs.
                      #' @return A list of data frames if id is NULL otherwise a single data frame for the passed in id.
                      get_df_multi_select_full_data = function(id = NULL) {
                        if(is.null(id)) {
                          return(self$data[["df_multi_select_full"]])
                        } else {
                          return(self$data[["df_multi_select_full"]][[id]])
                        }
                      },
                      #' @description
                      #' Get the stone_data data
                      #' @param id Stone ID to return - defalt NULL, return all
                      #' @param include_id default FALSE, TRUE to return ID as names.
                      #' @return List of stone data frames
                      get_stone_data_data = function(id = NULL, include_id = TRUE) {
                        if (is.null(id)) {
                          if (include_id) {
                            return(self$data[["stone_data"]])
                          } else {
                            return(unlist(self$data[["stone_data"]]))
                          }
                        }
                        if (id %in% names(self$data[["stone_data"]])) {
                          if (include_id) {
                            return(self$data[["stone_data"]][[id]])
                          } else {
                            return(unlist(self$data[["stone_data"]][[id]]))
                          }
                        }
                      },
                      #' @description
                      #' Get the stone_data data
                      #' @param stones_id vector of stone IDs to return stone data
                      #' @param include_ids if TRUE returns stoneID as return list name
                      #' @return The list of stone ratios
                      get_stones_ratio = function(stones_id = "", include_ids = FALSE) {
                        if (stones_id != "") {
                          if (!(stones_id %in% names(self$stone_ratios))) {return(NULL)}
                        }
                        if (stones_id == "") {
                          if (include_ids) {return(self$stone_ratios)}
                          if (!include_ids) {return(unlist(unname(self$stone_ratios)))}
                        } else {
                          if (include_ids) {return(self$stone_ratios[stones_id])}
                          if (!include_ids) {return(unlist(unname(self$stone_ratios[[stones_id]])))}
                        }
                      },

                      #' @description
                      #' Get the title_data data
                      #' @return The title_data data data.frame
                      get_title_data_data = function() {
                        return(self$data[["title_data"]])
                      },
                      #' @description
                      #' Get the title_use data
                      #' @return The title_use data data.frame
                      get_title_use_data = function() {
                        return(self$data[["title_use"]])
                      },
                      #' @description
                      #' Get the title_use data
                      #' @return The title_use data data.frame
                      get_sm_framework = function() {
                        return(self$sm_framework)
                      },
                      #' @description
                      #' Get the framework definition parsed json
                      #' @return The framework definition parsed json
                      get_framework_definition_json = function() {
                        return(self$framework_definition_json)
                      },
                      #' @description
                      #' Get the dashboard definition parsed json
                      #' @return The dashboard definition parsed json
                      get_dashboard_definition_json = function() {
                        if (!(self$is_dashboard())) {return(NULL)}
                        return(eval(parse(text = paste0("self$dashboard_definition_", self$get_dashboard_version()))))
                      },
                      #' @description
                      #' Get the dashboard layout parsed json
                      #' @return The dashboard layout parsed json
                      get_dashboard_layout_json = function() {
                        if (!(self$is_dashboard())) {return(NULL)}
                        if (!(self$get_dashboard_version() == "v2")) {return(NULL)}
                        return(self$dashboard_layout_v2)
                      },
                      #' @description
                      #' Get the dashboard layout parsed json
                      #' @return The dashboard layout parsed json
                      get_dashboard_filters_json = function() {
                        if (!(self$is_dashboard())) {return(NULL)}
                        if (!(self$get_dashboard_version() == "v2")) {return(NULL)}
                        return(self$dashboard_filters_v2)
                      },
                      #' @description
                      #' Get the dashboard combined frameworks names
                      #' @param include_ids if TRUE return the framework ids as the list names, defaults to FALSE
                      #' @return The dashboard combined framework names
                      get_dashboard_combined_frameworks_names = function(include_ids = FALSE) {
                        if (!(self$is_dashboard())) {return(NULL)}
                        if (include_ids) {
                          return(self$dashboard_combined_frameworks_names)
                        }
                        if (!include_ids) {return(unlist(unname(self$dashboard_combined_frameworks_names)))}
                      },
                      #' @description
                      #' Get the dashboard combined frameworks ids
                      #' @param include_names if TRUE return the framework names as the list names, defaults to FALSE
                      #' @return The dashboard combined framework ids
                      get_dashboard_combined_frameworks_ids = function(include_names = FALSE) {
                        if (!(self$is_dashboard())) {return(NULL)}
                        if (include_names) {
                          return(self$dashboard_combined_frameworks_ids)
                        }
                        if (!include_names) {return(unlist(unname(self$dashboard_combined_frameworks_ids)))}
                      },
                      #' @description
                      #' Get whether the framework has polymorphic signifiers
                      #' @return TRUE if framework has polymorphic signifiers otherwise FALSE
                      get_has_polymorphic_signifiers = function() {
                        return(self$has_polymorphic_signifiers)
                      },
                      #' @description
                      #' Get the polymorphic signifiers
                      #' @return Parsed JSON file of the polymorphic signifier definitions
                      get_polymorphic_signifiers = function() {
                        return(self$polymorphic_signifiers)
                      },
                      #'
                      #' @description
                      #' Create an accumulated data set based on specified column (default to EntryYrMth) for animation graphics.
                      #' This method creates a side effect by adding a new data frame to the data field list.
                      #' @param df_name The name of the data frame to use to accumulate the data.
                      #' @param col_name The column name to accumulate on. Defaults to EntryYrMth
                      #' @param data_name The data list name to hold the data. Defaults to NULL in which case the name will be a paste of "accumulate_" and the data_name and col_name
                      #' @returns NULL
                      add_accumulation_data = function(df_name, col_name = "EntryYrMth", data_name = NULL) {
                        stopifnot(df_name %in% names(self$data))
                        df <- self$data[[df_name]]
                        stopifnot(col_name %in% colnames(df))
                        if (is.null(data_name)) {
                          data_name <- paste0("accumulate_",df_name, "_", col_name)
                        } else {
                          stopifnot(is.name(data_name))
                        }

                        unique_breaks <- sort(unique(df[[col_name]]))
                        stopifnot(length(unique_breaks) > 0)

                        df_list <- vector("list", length = length(unique_breaks))
                        df_acc_list <- vector("list", length = length(unique_breaks))
                        accumulate_db <- NULL
                        purrr::iwalk(unique_breaks, ~ {
                          df_list[[.y]] <<- df[df[[col_name]] == .x, ]
                          purrr::iwalk(unique_breaks, ~ {df_acc_list[[.y]] <<- dplyr::bind_rows(df_list[1:.y])})
                          purrr::iwalk(unique_breaks, ~ {df_acc_list[[.y]][[col_name]] <<- .x})
                          accumulate_db <<- dplyr::bind_rows(df_acc_list[1:length(df_acc_list)])
                        })
                        self$add_data_data_frame(data_frame = accumulate_db, name = data_name, add_to_export_list_names = FALSE)
                      },

                      #' @description
                      #' Executes a list of queryies on the database and either updates existing data frames or adds a new one
                      #' @param query_file - default NULL. A file with columns containing at least "name" and "expression" and "include"
                      #' @param query_df - default NULL, a data frame previously read from a query_file file.
                      #' @param queries - a vector of the queries to execute. In double quotes so internal quotes to be single.
                      #' @param data_names - The names of the new data list data frame entries A vector the same length as queries
                      #' @returns NULL
                      execute_queries = function(query_file = NULL, query_df = NULL, queries = NULL, data_names = NULL) {
                        # queries and data_names vectors must be the same length and data_names unique
                        if (!is.null(query_file)) {
                          stopifnot(file.exists(query_file))
                          df <- read.csv(query_file, stringsAsFactors = FALSE)
                          stopifnot(all(c("name", "expression", "include") %in% colnames(df)))
                          df <- df %>% dplyr::filter(include == "Y")
                          queries <- df[["expression"]]
                          data_names <- df[["name"]]
                        }
                        if (!is.null(query_df)) {
                          stopifnot(is.data.frame(query_df))
                          df <- query_df
                          stopifnot(all(c("name", "expression", "include") %in% colnames(df)))
                          df <- df %>% dplyr::filter(include == "Y")
                          queries <- df[["expression"]]
                          data_names <- df[["name"]]
                        }
                        stopifnot(length(queries) == length(data_names))
                        stopifnot(length(queries) == length(unique(data_names)))
                        purrr::walk2(queries, data_names, ~ {self$execute_data_query(.x, NULL, .y)})
                      },

                      #' @description
                      #' Executes a query on the database and either updates existing data frames or adds a new one
                      #'
                      #' @param query - the query to execute. In double quotes so internal quotes to be single.
                      #' @param data_frames - default NULL, values "ALL" or a list of data names in the data list.
                      #' @param data_name - default NULL, the name of the new data list data frame entry if query to create a new entry
                      #' @returns the filtered data frame (note: the internal dataframe list will be also updated in accordance with whether data_frames or data_name passed)
                      execute_data_query = function(query, data_frames = NULL, data_name = NULL) {
                        # one and only one of data_frames and data_name can be passed.
                        stopifnot((is.null(data_frames) & !is.null(data_name)) | (!is.null(data_frames) & is.null(data_name)))
                        # data_name should only have one entry. Name should not already exist.
                        if (!is.null(data_name)) {
                          stopifnot(length(data_name) == 1)
                          stopifnot(!(data_name %in% self$get_data_list_names()))
                        }
                        # If data_frames contains an ALL it should be the only entry
                        # otherwise the names in the list should all be in the names already there.
                        if (!is.null(data_frames)) {
                          if ("ALL" %in% data_frames) {
                            stopifnot(length(data_frames) == 1)
                          } else {
                            stopifnot(all(data_frames %in% self$get_data_list_names()))
                          }
                        }
                        # query must not be null or blank
                        if (is.null(query)) {return(NULL)}
                        if (trimws(query) == "") {return(NULL)}
                        # set up parse query and catch a parse error - return ERROR if there is one.
                        parse_query <- function(query) {
                          return(parse(text = query))
                        }
                        tryCatch(withCallingHandlers(parse_query(query), error = function(e) {write.to.log(sys.calls())}
                                                     , warning=function(w) {write.to.log(sys.calls())
                                                       invokeRestart("muffleWarning")})
                                 , error = function(e) { return("ERROR") })

                        # do the main query
                        filtered_data_string <- parse(text = paste0("self$data[['df1']] %>% dplyr::filter(", query, ")"))
                        filtered_data <- eval(filtered_data_string)
                        if (!is.null(data_name)) {
                          self$add_data_data_frame(data_frame = filtered_data, name = data_name, add_to_export_list_names = TRUE)
                          return(filtered_data)
                        }
                        if (!is.null(data_frames)) {
                          if (data_frames == "ALL") {
                            data_frames <- self$get_data_list_names()
                          }
                          #use_data_frames <- purrr::keep(data_frames, ~ {!is.null(self$data[[.x]]) && "FragmentID" %in% colnames(self$data[[.x]])})
                          # Only use those data frames within this list that are not null and have fragmentid in one of their column names. Made complicated in that some of the entries
                          # in the list of data frames are themselves lists of data frames. The recursion shouldn't go below this so existing code should always work.
                          use_data_frames <- purrr::keep(data_frames, function(x) {
                            if (is.data.frame(self$data[[x]])) {
                              !is.null(self$data[[x]]) && "FragmentID" %in% colnames(self$data[[x]])
                            } else {
                              !is.null(self$data[[x]]) && unlist(purrr::map(names(self$data[[x]]), function(y) {
                                "FragmentID" %in% colnames(self$data[[x]][[y]])}))
                            }
                          })
                          #purrr::walk(use_data_frames, ~ {self$data[[.x]] <- self$data[[.x]] %>% dplyr::filter(FragmentID %in% filtered_data$FragmentID)})
                          purrr::walk(use_data_frames, function(x) {
                            if (is.data.frame(self$data[[x]])) {
                              self$data[[x]] <- self$data[[x]] %>% dplyr::filter(FragmentID %in% filtered_data$FragmentID)
                            } else {
                              purrr::walk(names(self$data[[x]]), function(y) {
                                self$data[[x]][[y]] <- self$data[[x]][[y]] %>% dplyr::filter(FragmentID %in% filtered_data$FragmentID)
                              })
                            }
                          })
                        }
                      },
                      #' @description
                      #' Creates new list data and list definitions for image select signifier types - this function has side effects and will be removed once the imageselect widget made compatable with list definition widgets
                      #' @returns NULL
                      turn_imageselect_to_list = function() {

                        # get the data frames to update and the image select ids
                        data_frames <- self$get_export_data_list_names()
                        is_ids <- self$sm_framework$get_imageselect_ids()
                        # Each image select id
                        purrr::walk(is_ids, function(id) {
                          # data frame of ids and titles for the image select items to join into the dataframe
                          look_up <- data.frame(ids = self$sm_framework$get_imageselect_items(id), titles = self$sm_framework$get_imageselect_items_titles(id))
                          join_vars <- c('ids')
                          names(join_vars) <- id
                          added_name <- paste0(id, "_list")

                          # each data frame
                          purrr::walk(data_frames, function(df) {
                            # if the id isn't there don't try to join it (the text/title versions of the data won't have the column)
                            if (id %in% colnames(self$data[[df]])) {
                              out_think <- unname(unlist(dplyr::left_join(x = self$data[[df]], y =  look_up,  by = join_vars) |> dplyr::select(titles)))
                              self$data[[df]] <<-self$data[[df]] |> dplyr::mutate(!! sym(added_name) := out_think)
                            }
                          })
                          # add the new list to the framework definition currentposition
                          list_items <- data.frame(id = look_up[["titles"]], title = look_up[["titles"]],
                                                   tooltip = look_up[["titles"]], visible = rep_len(TRUE, length.out = nrow(look_up)),
                                                   other_signifier_id = rep_len("", length.out = nrow(look_up)))
                          self$sm_framework$add_list(title = self$sm_framework$get_signifier_title(id), tooltip = self$sm_framework$get_signifier_title(id), allow_na = FALSE,
                                                     fragment = FALSE, required = TRUE, sticky = FALSE,
                                                     items = list_items,  max_responses = 1, min_responses = 1,
                                                     other_item_id = NULL, other_signifier_id = NULL, sig_class = "image_select",
                                                     theader = NULL, id = added_name)

                        })
                        return(NULL)
                      }

                    ),

                    private = list(
                      # columns to omit from list creation for meta-filtering
                      omit_meta_cols = c("meta_platform", "meta_completed", "meta_started",
                                         "meta_application", "meta_platform_verion",  "meta_platform_version", "meta_selectedLanguage", "meta_fbclid", "meta_started_format",
                                         "meta_completed_format", "meta_selected_language", "meta_started_location_speed", "meta_started_location_latitude",
                                         "meta_started_location_longitude", "meta_started_location_accuracy", "meta_started_location_heading",
                                         "meta_started_location_altitude", "meta_completed_location_speed", "meta_completed_location_heading",
                                         "meta_completed_location_accuracy", "meta_completed_location_altitude", "meta_completed_location_latitude",
                                         "meta_completed_location_longitude", "meta_collection_time_Mins", "meta_completed_location_altitudeAccuracy",
                                         "meta_started_location_altitudeAccuracy"),
                      # function handling the initialisation - get the data, process the data and load appropriate fields
                      get_data = function(csvfilename, csvfiledf, framework_id, dashboard_id, token, sensemakerframeworkrobject, polymorphic_definition_json,
                                          fragment_level_csv, fragment_level_parsed, FK_level_csv, FK_level_parsed, upload_na_identifier) {

                        # populate the data class fields including the generic array "data" which can be extended by application developers
                        self$data <- vector("list", length = 9)
                        names(self$data) <- c("df1", "dat", "df_keep",  "df_multi_select", "df_multi_select_full", "stone_data", "title_data", "title_use", "df_chat_titles")

                        if (all(is.null(c(framework_id, dashboard_id)))) {
                          self$is_invalid <- TRUE
                          return(self)
                        }

                        if (is.null(framework_id)) {framework_id <- ""}
                        if (is.null(dashboard_id)) {dashboard_id <- ""}
                        # checking that the parameters are correct.
                        # either one of filename/csvfiledf OR framework_id/dashboard_id
                        # if dashboard_id or framework_id then must have token.
                        # we don't worry about testing if token when csvfile or dataframe passed - just not used.
                        end_point <- "openapi"
                        #    assertive::assert_any_are_not_na(c(csvfilename, csvfiledf, framework_id, dashboard_id, token), severity = "stop")
                        if (all(c(csvfilename, csvfiledf, framework_id, dashboard_id, token) == "")) {
                          print("at least one parameter needs to be provided")
                          stop()
                        }

                        #   assertive::assert_any_are_na(c(framework_id, dashboard_id), severity = "stop")
                        if  (all(c(csvfilename, csvfiledf) != "")) {
                          print("you cannot provide a file name and a data frame together")
                          stop()
                        }

                        if  (all(c(framework_id, dashboard_id) != "")) {
                          print("you cannot provide a framework_id and a dashboard_id together")
                          stop()
                        }

                        #   if  (any(c(csvfilename, csvfiledf) != "")  & any(c(framework_id, dashboard_id) != "")) {
                        #   print("you cannot have file name or file while also passing framework id or dashboard id")
                        #   stop()
                        #    }


                        if (any(c(framework_id, dashboard_id) != "") &  token == "") {
                          print("if providing a framework id or dashboard id you must provide a token")
                          stop()
                        }

                        #   if (csvfiledf != "") {
                        #     if (is.data.frame(csvfiledf)) {
                        #       print("csvfiledf should be a dataframe")
                        #       stop()
                        #     } else {
                        #       if (!('project_id' %in% colnames(csvfiledf))) {
                        #        print("data frame 'csvfiledf' must have a column 'project_id' with the framework id present")
                        #        stop()
                        #      }
                        #    }
                        #  }

                        if (csvfilename != "") {
                          stopifnot(all(unlist(unname(purrr::map(csvfilename, ~ {file.exists(.x)})))))
                          # assertive::assert_all_are_existing_files(x = csvfilename, severity = "stop")
                        }

                        # =========== fragment level level update files or parsed data =================

                        # validate fragment level csv stuff if passed in
                        if (any(fragment_level_csv != "") & any(fragment_level_parsed != "")) {
                          print("only one of fragment level csv or fragment level parsed can be non blank")
                          stop()
                        }


                        if (any(fragment_level_csv != "")) {
                          if (any(!unlist(unname(purrr::map(fragment_level_csv, ~ {file.exists(.x)}))))) {
                            print("at least one of the passed in fragment level csv files does not exist")
                            stop()
                          }


                          if (any(unlist(unname(purrr::map(fragment_level_csv, ~ {!stringr::str_ends(fragment_level_csv, ".csv")}))))) {
                            print("at least one of the passed in fragment level csv files does not have a .csv extension")
                            stop()
                          }


                          # read the fragment level files
                          fl_list <- NULL
                          purrr::walk(fragment_level_csv, ~ {fl_list[[.x]] <<- read.csv(.x, na.strings = upload_na_identifier, check.names = FALSE,stringsAsFactors = FALSE)})
                          # first columns in each must be "FragmentID" or "id"
                          if (any(unlist(unname(purrr::map(names(fl_list), ~ {colnames(fl_list[[.x]])[[1]] == "FragmentID" | colnames(fl_list[[.x]])[[1]] == "id"}))) == FALSE)) {
                            print("The first column in each file must be named 'Fragment_ID' or 'id'")
                            stop()
                          }
                          self$fragment_level_upload <- fl_list
                        }

                        if (any(fragment_level_parsed != "")) {
                          if (any(!unlist(unname(purrr::map(fragment_level_parsed, ~ {is.data.frame(.x)}))))) {
                            print("at least one of the passed in fragment level parsed is not a dataframe")
                            stop()
                          }

                          if(any(unlist(unname(purrr::map(fragment_level_parsed, ~ {colnames(.x)[[1]] == "id" | colnames(.x)[[1]] == "FragmentID"}))) == FALSE)) {
                            print("at least one of the passed in fragment level parsed does not have 'id' or 'FragmentID' as first column ")
                            stop()
                          }
                          self$fragment_level_upload <- fragment_level_parsed
                        }

                        # =========== FK level update files or parsed data =================

                        if (any(FK_level_csv != "") & any(FK_level_parsed != "")) {
                          print("only one of foreign key level csv or fragment level parsed can be non blank")
                          stop()
                        }


                        if (any(FK_level_csv != "")) {
                          if (any(!unlist(unname(purrr::map(FK_level_csv, ~ {file.exists(.x)}))))) {
                            print("at least one of the passed in foreign key level csv files does not exist")
                            stop()
                          }
                          if (any(unlist(unname(purrr::map(FK_level_csv, ~ {!stringr::str_ends(FK_level_csv, ".csv")}))))) {
                            print("at least one of the passed in oreign key level csv files does not have a .csv extension")
                            stop()
                          }
                          # read the fragment level files
                          fl_list <- NULL
                          purrr::walk(FK_level_csv, ~ {fl_list[[.x]] <<- read.csv(.x, na.strings = upload_na_identifier, check.names = FALSE,stringsAsFactors = FALSE)})
                          # Must only have 2 columns
                          if (any(unlist(unname(purrr::map(names(fl_list), ~ {length(colnames(fl_list[[.x]])) != 2}))))) {
                            print("Each foreign key file must have only two columns. The FK column and the new values column")
                            stop()
                          }
                          # first column must exist in the data csv
                          if (any(unlist(unname(purrr::map(names(fl_list), ~ {colnames(fl_list[[.x]])[[1]] %in% colnames(df)}))))) {
                            print("The first column name in each file must exist in the data file as a foreign key link")
                            stop()
                          }
                          self$FK_level_upload <- fl_list
                        }

                        # now if FK parsed data passed in
                        if (any(FK_level_parsed != "")) {
                          # must be dataframes
                          if (any(!unlist(unname(purrr::map(FK_level_parsed, ~ {is.data.frame(.x)}))))) {
                            print("at least one of the passed in FK level parsed is not a dataframe")
                            stop()
                          }

                          if(any(unlist(unname(purrr::map(names(FK_level_parsed), ~ {length(colnames(FK_level_parsed[[.x]])) != 2}))))) {
                            print("Each foreign key file must have only two columns. The FK column and the new values column")
                            stop()
                          }

                          # first column must exist in the data csv
                          if (any(unlist(unname(purrr::map(names(FK_level_parsed), ~ {colnames(FK_level_parsed[[.x]])[[1]] %in% colnames(df)}))))) {
                            print("The first column name in each foreign key file must exist in the data file as a foreign key link")
                            stop()
                          }

                          self$FK_level_upload <- FK_level_parsed


                        }


                        # end of parameter checking.
                        # "df" to store the data frame read or downloaded
                        df <- NULL

                        if (csvfilename != "") {
                          df <- read.csv(csvfilename, check.names = FALSE, stringsAsFactors = FALSE, as.is = TRUE, encoding = "UTF-8")
                          if (!("project_id" %in% colnames(df))) {
                            print("file 'csvfilename' must have a column 'project_id' with the framework id present")
                            stop()
                          }
                          is_demonstrator <- FALSE
                          if (is.null(df)) {
                            is_demonstrator <- private$get_is_demonstrator(token)
                          }
                          self$demonstrator <- is_demonstrator
                          #  print("df is")
                          #  print(head(df))
                        }

                        if (is.data.frame(csvfiledf)) {
                          df <- csvfiledf

                          is_demonstrator <- private$get_is_demonstrator(token)
                          self$demonstrator <- is_demonstrator
                        }

                        # polymorphic parsed json passed in
                        if (polymorphic_definition_json != "") {
                          self$polymorphic_definition_json <- polymorphic_definition_json
                        }

                        # framework id is passed (so at some stage will have to use the API to get the data. )
                        if (framework_id != "") {
                          self$framework_id <- framework_id
                        }
                        # preliminary process of the dashboard - get the framework id and the frqmework data.
                        if (dashboard_id != "") {
                          # get the dashboard definition.
                          # try version one of the dashboard
                          dashboard_definition <- private$get_v1_DashboardDefinition(end_point, dashboard_id, token)
                          if (!is.null(dashboard_definition$framework_id)) {
                            self$dashboard_definition_v1 <- dashboard_definition
                            self$framework_id <- dashboard_definition$framework_id
                            self$dashboard_id <- dashboard_id
                            self$dashboard_title <- dashboard_definition$name
                            self$dashboard_version <- "v1"
                          } else {
                            # try version 2
                            dashboard_definition <- private$get_v2_DashboardDefinition(end_point, dashboard_id, token)
                            self$dashboard_definition_v2 <- dashboard_definition[["v2_definition"]]
                            self$framework_id <- self$dashboard_definition_v2$frameworkID
                            self$dashboard_id <- dashboard_id
                            self$dashboard_title <- self$dashboard_definition_v2$name
                            self$dashboard_layout_v2 <- dashboard_definition[["v2_layout"]]
                            self$dashboard_filters_v2 <- dashboard_definition[["v2_filters"]]
                            self$dashboard_version = "v2"
                          }

                        }

                        # Get the data
                        if (is.null(df)) {
                          is_demonstrator <- private$get_is_demonstrator(token)
                          self$demonstrator <- is_demonstrator

                          df <- private$get_API_framework_data(end_point, self$framework_id, token, is_demonstrator)
                          if (nrow(df) == 0) {
                            self$has_zero_records <- TRUE
                            return(self)
                          }
                        }
                        # Now we have df as the data frame required for processing - but does the dataframe match the definition
                        # get the framework_id if not present
                        if (framework_id == "") {
                          framework_id <- df[1, "project_id"]
                          self$framework_id <- framework_id
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


                        # apply dates as we wll need them for filtering
                        df <- private$apply_dates(df)

                        # assign the framework json definition to field
                        self$framework_definition_json <- sensemakerframeworkrobject$framework_json

                        # now process the dashboard completely - this function has side effects on the dashboard definition,
                        # data and framework definition
                        if (dashboard_id != "") {

                          df <- do.call(paste0("process_dashboard_definition_", self$dashboard_version), args = list(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject), envir = private)
                        }

                        # assign the sensemaker framework
                        self$sm_framework <- sensemakerframeworkrobject
                        # Main framework title
                        self$framework_title <- self$sm_framework$get_parent_framework_name()
                        # start processing data
                        #self$df1 <- private$process_data(df, sensemakerframeworkrobject)
                        self$data[["df1"]] <- private$process_data(df, sensemakerframeworkrobject)



                        if (!is.null(self$data[["df1"]])) {
                          #self$data[["df1"]] <- self$df1
                          #self$dat <- self$df1
                          # self$data[["dat"]] <- self$df1
                          self$data[["dat"]] <- self$data[["df1"]]
                          # self$df_keep <- self$df1
                          #self$data[["df_keep"]] <- self$df1
                          self$data[["df_keep"]] <- self$data[["df1"]]
                        }

                        #self$data[["title_data"]] <- self$title_data
                        # self$data[["df_chat_titles"]] <- self$df_chat_titles

                        # if (!is.null(self$data[["df_multi_select"]])) {
                        #   self$data[["df_multi_select"]] <- self$df_multi_select
                        # }
                        # if (!is.null(self$df_multi_select_full)) {
                        if (!is.null(self$data[["df_multi_select_full"]])) {
                          # self$data[["df_multi_select_full"]] <- self$df_multi_select_full
                          #self$df_multi_select <- self$df_multi_select_full
                          self$data[["df_multi_select"]] <- self$data[["df_multi_select_full"]]
                        }

                        #  if (!is.null(self$stone_data)) {
                        #  self$data[["stone_data"]] <- self$stone_data
                        #}
                      },


                      # Do all the things needed to be done to the main data data frame including creating new transformed data frames for multi-select MCQs and stones
                      process_data = function(df, sensemakerframeworkrobject) {
                        # we need the framework id for some of the calls
                        framework_id <- sensemakerframeworkrobject$get_parent_framework_id()

                        # a funny thing we have to do so stuff doesn't fall over - turn into character datatype
                        if ("meta_platform_verion" %in% colnames(df)) {
                          df[["meta_platform_verion"]] <- as.character( df[["meta_platform_verion"]])
                        }

                        # adding in commen
                        # add the NarrID column (used for filter indexing) and set the "id" column to "FragmentID" (reads better in the code)
                        df[["NarrID"]] <- 1:nrow(df)
                        if (!("FragmentID" %in% colnames(df))) {
                          names(df)[[which(names(df) == "id")]] <- "FragmentID"
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
                          # Adjust  triad anchor data that equals 0 to make them slightly positive (important for compositional stats) - side effects on df
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
                        # NOTE In setting the "include" to false for these lists, we are compromising on the old WB way of dealing with
                        # multi-select attributes, which is here set at the proper place as the signifier class.
                        list_items <- data.frame(id = c("selected", "notselected"), title = c("selected", "not selected"), tooltip = c("selected", "not selected"), visible = c(TRUE, TRUE), other_signifier_id = c("", ""))
                        #      purrr::walk(sensemakerframeworkrobject$get_multiselect_list_ids(), ~
                        #                    {purrr::walk2(sensemakerframeworkrobject$get_list_items_ids(.x), .x, ~
                        #                                   {ttitle <- paste0(sensemakerframeworkrobject$get_signifier_title(.y), "_",
                        #                                                     sensemakerframeworkrobject$get_list_item_title(.y, .x));
                        #                                   sensemakerframeworkrobject$add_list(title = ttitle, tooltip = ttitle, allow_na = FALSE,
                        #                                                                      fragment = FALSE, required = FALSE, sticky = FALSE,
                        #                                                                      items = list_items,  max_responses = 1, min_responses = 1,
                        #                                                                      other_item_id = NULL, other_signifier_id = NULL, sig_class = "multi_select_item",
                        #                                                                      theader = NULL, id = paste0(ttitle, "_selected"));
                        #                                  sensemakerframeworkrobject$change_signifier_include(id = paste0(ttitle, "_selected"), value = FALSE)})},
                        #               list_items)
                        purrr::walk(sensemakerframeworkrobject$get_multiselect_list_ids(), ~
                                      {purrr::walk2(sensemakerframeworkrobject$get_list_items_ids(.x), .x, ~
                                                      {ttitle <- paste0(sensemakerframeworkrobject$get_signifier_title(.y), "_",
                                                                        sensemakerframeworkrobject$get_list_item_title(.y, .x));
                                                      sensemakerframeworkrobject$add_list(title = ttitle, tooltip = ttitle, allow_na = FALSE,
                                                                                          fragment = FALSE, required = FALSE, sticky = FALSE,
                                                                                          items = list_items,  max_responses = 1, min_responses = 1,
                                                                                          other_item_id = NULL, other_signifier_id = NULL, sig_class = "multi_select_item",
                                                                                          theader = NULL, id = paste0(.y, "_", .x, "_selected"));
                                                      sensemakerframeworkrobject$change_signifier_include(id = paste0(.y, "_", .x, "_selected"), value = FALSE)})},
                                    list_items)

                        # add the date and columns as filters to framework definition - side effects on the sensemakerframeworkrobject object
                        date_cols <- c("EntryYr", "EntryYrMth", "EntryYrMthDay")
                        purrr::walk(date_cols, ~ {data_vals <- sort(unique(df[[.x]])); list_items <- data.frame(id = data_vals, title = data_vals,
                                                                                                                tooltip = data_vals, visible = rep_len(TRUE, length.out = length(data_vals)),
                                                                                                                other_signifier_id = rep_len("", length(data_vals)));
                        sensemakerframeworkrobject$add_list(title = .x, tooltip = .x, allow_na = FALSE,
                                                            fragment = FALSE, required = FALSE, sticky = FALSE,
                                                            items = list_items,  max_responses = 1, min_responses = 1,
                                                            other_item_id = NULL, other_signifier_id = NULL, sig_class = "date",
                                                            theader = NULL, id = .x)}, df)

                        # add the meta columns - those that have more than 1 unique option, but not more than 30. (and we omit crazy duplicates that can come in with numeric values with decimals)
                        metaColNamesX <- purrr::keep(colnames(df)[grepl("meta", colnames(df))][!(colnames(df)[grepl("meta", colnames(df))] %in%
                                                                                                   private$omit_meta_cols)],
                                                     ~ {dplyr::between(length(sort(unique(df[!is.na(df[[.x]]), .x]))),  2, 40) & all(duplicated(sort(unique(df[!is.na(df[[.x]]), .x]))) == FALSE)}, df)

                        # Add to the sensemaker frameworkr object - side effects on the sensemakerframeworkrobject object
                        purrr::walk(metaColNamesX, ~ {data_vals <- sort(unique(df[!is.na(df[[.x]]), .x])); list_items <- data.frame(id = data_vals, title = data_vals,
                                                                                                                                    tooltip = data_vals, visible = rep_len(TRUE, length.out = length(data_vals)),
                                                                                                                                    other_signifier_id = rep_len("", length(data_vals)));
                        sensemakerframeworkrobject$add_list(title = .x, tooltip = .x, allow_na = FALSE,
                                                            fragment = FALSE, required = FALSE, sticky = FALSE,
                                                            items = list_items,  max_responses = 1, min_responses = 1,
                                                            other_item_id = NULL, other_signifier_id = NULL, sig_class = "meta",
                                                            theader = NULL, id = .x)}, df)


                        # adding the project ids into the mcq list if this is a combined one and more than one frameworkid in the data
                        # is combined will only be true if it is a dashboard and a combined one
                        if (self$is_combined_dashboard()) {
                          combined_list <- self$get_combined_dashboard_frameworks_for_dropdown(include_id = TRUE, only_id = FALSE, include_parent = TRUE)
                          if (length(combined_list) > 1 & length(sort(unique(df[["project_id"]]))) > 1) {
                            list_items <- data.frame(id = unlist(unname(combined_list)), title = names(combined_list),
                                                     tooltip = names(combined_list), visible = rep_len(TRUE, length.out = length(combined_list)),
                                                     other_signifier_id = rep_len("", length.out = length(combined_list)))
                            sensemakerframeworkrobject$add_list(title = "project_id", tooltip = "project_id", allow_na = FALSE,
                                                                fragment = FALSE, required = FALSE, sticky = FALSE,
                                                                items = list_items,  max_responses = 1, min_responses = 1,
                                                                other_item_id = NULL, other_signifier_id = NULL, sig_class = "project_id",
                                                                theader = NULL, id = "project_id")
                          }
                        }

                        # Add selected MCQ for any single select MCQ that has only one item. The single item one cannot be used in many stats operations
                        single_item_mcqs <- unlist(unname(purrr::keep(sensemakerframeworkrobject$get_single_select_list_ids(), ~ {sensemakerframeworkrobject$get_list_num_items(.x) == 1})))
                        for (list_id in single_item_mcqs) {
                          for (col_name in sensemakerframeworkrobject$get_list_column_names(list_id)) {
                            df[[paste0(col_name, "_selected")]] <- unlist(unname(purrr::map(df[[col_name]], ~ {ifelse(is.na(.x), "notselected", "selected")})))
                            # now create a list item in the new sensemakerframeworkr
                            list_items <- data.frame(id = c("selected", "notselected"), title = c("selected", "not selected"), tooltip = c("selected", "not selected"), visible = c(TRUE, TRUE), other_signifier_id = c("", ""))
                            temp_list_item_id <- col_name
                            temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(list_id), "-", sensemakerframeworkrobject$get_list_item_title(list_id, temp_list_item_id))
                            sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL, sig_class = "single_select_item",  theader = NULL, id = paste0(col_name, "_selected"))
                          }
                        }

                        # multi-select MCQ data into new long form tables
                        #self$df_multi_select_full <- private$transform_multi_select(df, sensemakerframeworkrobject)
                        self$data[["df_multi_select_full"]] <- private$transform_multi_select(df, sensemakerframeworkrobject)

                        # ------------- Process stones ---------------
                        # Calculate stone ratios
                        self$stone_ratios <- private$getStoneRatios(df, sensemakerframeworkrobject)
                        # stone transform - column to row for each stone in the framework definition.
                        stones <- sensemakerframeworkrobject$get_stones_ids()
                        if (length(stones) > 0) {
                          stone_data <- vector("list", length = length(stones))
                          names(stone_data) <- stones
                          for (stones_id in stones) {
                            # data file columns for stone
                            cols <- sensemakerframeworkrobject$get_stones_compositional_column_names(stones_id)
                            # rest of columns for the transform
                            exl <-colnames(df)[-which(colnames(df) %in% cols)]
                            # gather the cols - i.e. turn the x value columns then the y value columns in cols to a single column named "cols" and place the stone value
                            #  for each in "x_value" and "y_value" accordingly
                            df_row_x <- df %>% tidyr::gather(cols, "x_value", -exl) %>% dplyr::filter(grepl("XRight", cols)) %>% dplyr::select("FragmentID", "cols", "x_value")
                            df_row_y <- df %>% tidyr::gather(cols, "y_value", -exl) %>% dplyr::filter(grepl("YTop", cols)) %>% dplyr::select("FragmentID", "cols", "y_value")
                            # Now pull out the stones id from the stone id and get the stone and stones label to add for the x-value
                            df_row_x_stone <- df_row_x %>% dplyr::mutate(stones_label = rep_len(sensemakerframeworkrobject$get_signifier_title(stones_id), nrow(df_row_x)), stone_id = stringr::str_remove(unlist(unname(purrr::map(df_row_x[["cols"]], ~ {stringr::str_split(.x, "_")[[1]][2]} ))), "XRight"),
                                                                         stone_label = unlist(unname(purrr::map(stringr::str_remove(unlist(unname(purrr::map(df_row_x[["cols"]], ~ {stringr::str_split(.x, "_")[[1]][2]} ))), "XRight"), ~ {sensemakerframeworkrobject$get_stones_stone_title_by_id(stones_id, .x)})))) %>%
                              dplyr::select(-"cols")

                            # y-value same as above but we don't need the repleats of all the label and id columns - just the keys and value for the coming join
                            df_row_y_stone <- df_row_y %>% dplyr::mutate(stone_id = stringr::str_remove(unlist(unname(purrr::map(df_row_y[["cols"]], ~ {stringr::str_split(.x, "_")[[1]][2]} ))), "YTop")) %>%
                              dplyr::select("FragmentID", "stone_id", "y_value")

                            # Join into a single dataframe then add to the out list
                            stonedf1 <- dplyr::left_join(df_row_x_stone, df_row_y_stone, by = c("FragmentID", "stone_id"))
                            stone_data[[stones_id]] <- stonedf1
                          }
                          # set the field
                          # self$stone_data <- stone_data
                          self$data[["stone_data"]] <- stone_data
                        }


                        # add polymorphic signifiers extra column data
                        # if the existing directory has a polymorphic definition file in it, or passed in as parameter than add to the framework definition

                        if (!is.null(self$polymorphic_definition_json)) {
                          self$polymorphic_signifiers <- jsonlite::fromJSON(txt = self$polymorphic_definition_json, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = FALSE)
                          sensemakerframeworkrobject$add_polymorphic_signifiers(tpoly_data = self$polymorphic_sigifiers, NULL)
                          self$has_polymorphic_signifiers <- TRUE
                        } else {
                          if (file.exists(paste0("PolymorphismDefinition_", framework_id, ".json"))) {
                            self$polymorphic_signifiers <- jsonlite::fromJSON(paste0("PolymorphismDefinition_", framework_id, ".json"), simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = FALSE)
                            sensemakerframeworkrobject$add_polymorphic_signifiers(tpoly_data = NULL, tpoly_data_file = paste0("PolymorphismDefinition_", framework_id, ".json"))
                            self$has_polymorphic_signifiers <- TRUE
                          }
                        }

                        # apply the polymorphic definition if it is polymorphic
                        if(sensemakerframeworkrobject$is_polymorphic()) {
                          df <- private$apply_polymorphic_definition(df, sensemakerframeworkrobject)
                        }

                        # add the zones - triad, dyad and stones
                        df <- private$add_triad_zones(df, sensemakerframeworkrobject)

                        df <- private$add_dyad_zones(df, sensemakerframeworkrobject)

                        df <- private$add_stones_zones(df, sensemakerframeworkrobject)


                        # create a title version of the data frame. ToDo this isn't finished because of query with Manami/Ramya
                        # self$title_data <- private$convert_data_to_titles(df, sensemakerframeworkrobject, column_type = "ALL")
                        self$data[["title_data"]] <- private$convert_data_to_titles(df, sensemakerframeworkrobject, column_type = "ALL")
                        # self$data[["title_data"]] <- self$title_data
                        df_chat <- df %>% dplyr::select(c("FragmentID", sensemakerframeworkrobject$get_freetext_ids(), unlist(unname(purrr::map(sensemakerframeworkrobject$get_list_ids(exclude_multiple = TRUE), ~ {sensemakerframeworkrobject$get_list_column_names(.x)})))))

                        #self$df_chat_titles <- private$convert_data_to_titles(df_chat, sensemakerframeworkrobject, column_type = "CHAT")
                        self$data[["df_chat_titles"]] <- private$convert_data_to_titles(df_chat, sensemakerframeworkrobject, column_type = "CHAT")
                        #self$add_data_data_frame(self$df_chat_titles, name =  "df_chat_titles", add_to_export_list_names = TRUE)
                        # self$data[["df_chat_titles"]] <- self$df_chat_titles



                        # add any fragmentID level extra column data
                        if (!is.null(self$fragment_level_upload)) {
                          df <- private$apply_fragment_level_updates(df, sensemakerframeworkrobject)
                        }

                        # add any FK level extra column data
                        if (!is.null(self$FK_level_upload)) {
                          df <- private$apply_FK_level_updates(df, sensemakerframeworkrobject)
                        }


                        # add any foreign key level extra column data



                        # add any new fragments data

                        return(df)
                      },

                      apply_polymorphic_definition = function(df, sensemakerframeworkrobject) {

                        modified_ids <- names(sensemakerframeworkrobject$get_poly_anchor_modification_ids_with_type())

                        for (i in seq_along(modified_ids)) {
                          id <- modified_ids[[i]]
                          type <- sensemakerframeworkrobject$get_poly_anchor_modification_ids_with_type()[id]
                          new_id <- sensemakerframeworkrobject$get_poly_modification_sig_id(id)
                          if (type == "triad") {
                            new_anchor_ids <- sensemakerframeworkrobject$get_triad_anchor_ids(new_id)
                            top_list <- vector(mode = "list", length = nrow(df))
                            left_list <- vector(mode = "list", length = nrow(df))
                            right_list <- vector(mode = "list", length = nrow(df))
                            top_list <- df[[paste0(id, "_", new_anchor_ids[["top"]])]]
                            left_list <- df[[paste0(id, "_", new_anchor_ids[["left"]])]]
                            right_list <- df[[paste0(id, "_", new_anchor_ids[["right"]])]]
                            new_df <- data.frame(x = left_list, y = top_list, z = right_list)
                            new_xy <- ggtern::tlr2xy(new_df, ggtern::coord_tern())
                            new_cols_df <- NULL
                            if (!sensemakerframeworkrobject$get_signifier_allow_na(id)) {
                              new_cols_df <- data.frame(x = new_xy[["x"]], y = new_xy[["y"]], t <- top_list, l = left_list, r = right_list)
                            } else {
                              new_cols_df <- data.frame(x = new_xy[["x"]], y = new_xy[["y"]], t <- top_list, l = left_list, r = right_list, na = df[[sensemakerframeworkrobject$get_triad_all_column_names(id)[["na"]]]])
                            }
                            colnames(new_cols_df) <- sensemakerframeworkrobject$get_triad_all_column_names(new_id, delist = TRUE)
                            df <- dplyr::bind_cols(df, new_cols_df)

                          } else {

                            # need to test this part - not tested for dyads
                            new_anchor_ids <- sensemakerframeworkrobject$get_dyad_anchor_ids(new_id)
                            new_col_ids <- sensemakerframeworkrobject$get_dyad_all_column_names(new_id, delist = TRUE)
                            new_col_ids_L <- sensemakerframeworkrobject$get_dyad_all_column_names(new_id, delist = FALSE)
                            left_list <- vector(mode = "list", length = nrow(df))
                            right_list <- vector(mode = "list", length = nrow(df))
                            left_list <- df[[paste0(id, "_", new_anchor_ids[["left"]])]]
                            right_list <- df[[paste0(id, "_", new_anchor_ids[["right"]])]]

                            x_list <- (1 - df[[paste0(id, "X")]])
                            x_listR <- x_list * 100
                            new_cols_df <- NULL
                            if (!sensemakerframeworkrobject$get_signifier_allow_na(id)) {
                              new_cols_df <- data.frame(x = x_list, l = left_list, r = right_list, xR = x_listR)
                            } else {
                              new_cols_df <- data.frame(x = x_list, l = left_list, r = right_list, na = df[[paste0(id, "_NA")]],  xR = x_listR)
                              # new_cols_df <- data.frame(x = new_df[["x"]], l = new_df[["l"]], r <- new_df[["r"]], na = df[[new_col_ids[["na"]]]])
                            }
                            colnames(new_cols_df) <- c(sensemakerframeworkrobject$get_dyad_all_column_names(new_id, delist = TRUE), paste0(new_col_ids_L[["x"]], "R"))
                            df <- dplyr::bind_cols(df, new_cols_df)
                          }
                        }

                        # Now add in the polymorphic triads for overlay display
                        poly_triad_ids <- sensemakerframeworkrobject$get_poly_sig_ids_for_type("triad")

                        for (i in seq_along(poly_triad_ids)) {

                          poly_triad_id <- poly_triad_ids[[i]]
                          to_triad_ids <- sensemakerframeworkrobject$get_poly_sig_to_ids(poly_triad_id[[i]])

                          top_col <- as.numeric((df %>% tidyr::unite("top_col", all_of(unlist(purrr::map(to_triad_ids, ~ {sensemakerframeworkrobject$get_triad_top_column_name(.x)}))), remove = FALSE, na.rm = TRUE))[, "top_col"])
                          left_col <- as.numeric((df %>% tidyr::unite("left_col", all_of(unlist(purrr::map(to_triad_ids, ~ {sensemakerframeworkrobject$get_triad_left_column_name(.x)}))), remove = FALSE, na.rm = TRUE))[, "left_col"])
                          right_col <- as.numeric((df %>% tidyr::unite("right_col", all_of(unlist(purrr::map(to_triad_ids, ~ {sensemakerframeworkrobject$get_triad_right_column_name(.x)}))), remove = FALSE, na.rm = TRUE))[, "right_col"])
                          x_col <- as.numeric((df %>% tidyr::unite("x_col", all_of(unlist(purrr::map(to_triad_ids, ~ {sensemakerframeworkrobject$get_triad_x_column_name(.x)}))), remove = FALSE, na.rm = TRUE))[, "x_col"])
                          y_col <- as.numeric((df %>% tidyr::unite("y_col", all_of(unlist(purrr::map(to_triad_ids, ~ {sensemakerframeworkrobject$get_triad_y_column_name(.x)}))), remove = FALSE, na.rm = TRUE))[, "y_col"])
                          temp_dataframe <- data.frame(top_col, left_col, right_col, x_col, y_col)
                          colnames(temp_dataframe) <- c(sensemakerframeworkrobject$get_triad_top_column_name(poly_triad_id), sensemakerframeworkrobject$get_triad_left_column_name(poly_triad_id), sensemakerframeworkrobject$get_triad_right_column_name(poly_triad_id),
                                                        sensemakerframeworkrobject$get_triad_x_column_name(poly_triad_id), sensemakerframeworkrobject$get_triad_y_column_name(poly_triad_id))
                          df <- dplyr::bind_cols(df, temp_dataframe)
                          # add in the colour by column
                          left_col_names <- unlist(purrr::map(to_triad_ids, ~ {sensemakerframeworkrobject$get_triad_left_column_name(.x)}))
                          poly_source <- rep_len(NA, length.out = nrow(df))
                          for (i in 1:nrow(df)) {
                            if (length(which(!is.na(df[i, left_col_names])) != 0)) {
                              poly_source[[i]] <- sensemakerframeworkrobject$get_signifier_title(stringr::str_split_1(left_col_names[[which(!is.na(df[i, left_col_names]))]], "_")[[1]])
                            }
                          }
                          poly_source <- stringr::str_remove_all(string = poly_source, pattern = ":")

                          poly_source <- stringr::str_remove_all(string = poly_source, pattern = "\\...")
                          poly_id <- stringr::str_remove_all(string = poly_source, pattern = " ")

                          temp_id <- sensemakerframeworkrobject$get_poly_overlay_id(poly_triad_id)
                          df[[temp_id]] <- poly_id
                          meta_id <- sort(unique(poly_id))  # sort(unique(df[[sensemakerframeworkrobject$get_poly_overlay_id(poly_triad_id)]]))
                          meta_title <- sort(unique(poly_source))

                          temp_items <- data.frame(id = meta_id, title = meta_title, tooltip = meta_title, visible = rep_len("TRUE", length(meta_title)), other_signifier_id = rep_len("", length(meta_title)))
                          temp_title <- sensemakerframeworkrobject$get_signifier_title(poly_triad_id)

                          # temp_id <- sensemakerframeworkrobject$get_poly_overlay_id(poly_triad_id)
                          # do we need to add this to the lists in the new_json and old for the purposes of filtering and colouring
                          # temp_items <- data.frame(id = meta_values, title = meta_values, tooltip = meta_values, visible = rep_len("TRUE", length(meta_values)), other_signifier_id = rep_len("", length(meta_values)))

                          sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = temp_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL, theader = NULL, id = temp_id)

                        }

                        return(df)

                      },

                      add_triad_zones = function(df, sensemakerframeworkrobject) {

                        for (triad_id in sensemakerframeworkrobject$get_triad_ids()) {
                          df[[paste0(triad_id, "_Zone")]] <- private$get_triad_zone(triad_id, df, sensemakerframeworkrobject)
                          df[[paste0(triad_id, "_Zone")]] <- factor(df[[paste0(triad_id, "_Zone")]], levels = c("L", "R", "T", "Centre", "LR", "LT", "TR"), ordered = TRUE)
                          # add the zones as list item
                          list_items <- data.frame(id = c("L", "R", "T", "Centre", "LR", "LT", "TR"), title = c("Left", "Right", "Top", "Centre", "Left Right", "Left Top", "Top Right"), tooltip = c("Left", "Right", "Top", "Centre", "Left Right", "Left Top", "Top Right"), visible = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), other_signifier_id = c("", "", "", "", "", "", ""))
                          temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(triad_id), "-", "_Zone")
                          sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL,  sig_class = "zone", theader = NULL, id = paste0(triad_id, "_Zone"))
                        }
                        return(df)
                      },

                      get_triad_zone = function(id, df, sensemakerframeworkrobject) {

                        leftLabel <- sensemakerframeworkrobject$get_triad_left_column_name(id)
                        topLabel <- sensemakerframeworkrobject$get_triad_top_column_name(id)
                        rightLabel <- sensemakerframeworkrobject$get_triad_right_column_name(id)

                        triadZone <- vector("list", length = nrow(df))
                        for (i in 1:nrow(df)) {
                          if (is.na(df[i,leftLabel])) {
                            triadZone[[i]] <- NA
                            next
                          }

                          if (df[i,leftLabel] >= 60) {triadZone[[i]] <- "L"}
                          if (df[i,topLabel] >= 60) {triadZone[[i]] <- "T"}
                          if (df[i,rightLabel] > 60) {triadZone[[i]] <- "R"}
                          if (df[i,leftLabel] < 60 & df[i,leftLabel] >= 20 & df[i,topLabel] < 60 & df[i,topLabel] >= 20 & df[i,rightLabel] < 20) {triadZone[[i]] <- "LT"}
                          if (df[i,leftLabel] <= 20 & df[i,topLabel] < 60 & df[i,topLabel] >= 20 & df[i,rightLabel] <= 60 & df[i,rightLabel] >= 20) {triadZone[[i]] <- "TR"}
                          if (df[i,leftLabel] < 60 & df[i,leftLabel] >= 20 & df[i,topLabel] < 20 & df[i,rightLabel] <= 60 & df[i,rightLabel] >= 20) {triadZone[[i]] <- "LR"}
                          if (df[i,leftLabel] < 60 & df[i,leftLabel] >= 20 & df[i,topLabel] >= 20 & df[i,topLabel] < 60 & df[i,rightLabel] < 60 & df[i,rightLabel] >= 20) {triadZone[[i]] <- "Centre"}
                        }
                        return(unlist(triadZone))
                      },

                      add_dyad_zones = function(df, sensemakerframeworkrobject) {

                        for (dyad_id in sensemakerframeworkrobject$get_dyad_ids()) {
                          df[[paste0(dyad_id, "_Zone")]] <- private$get_dyad_zone(dyad_id, df, sensemakerframeworkrobject)
                          df[[paste0(dyad_id, "_Zone")]] <- factor(df[[paste0(dyad_id, "_Zone")]], levels = c("Left", "Centre_Left", "Centre", "Centre_Right", "Right"), ordered = TRUE)
                          # add the zones as list item
                          list_items <- data.frame(id = c("Left", "Centre_Left", "Centre", "Centre_Right", "Right"), title = c("Left", "Centre Left", "Centre", "Centre Right", "Right"), tooltip = c("Left", "Centre Left", "Centre", "Centre Right", "Right"), visible = c(TRUE, TRUE, TRUE, TRUE, TRUE), other_signifier_id = c("", "", "", "", ""))
                          temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(dyad_id), "-", "_Zone")
                          sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL,  sig_class = "zone", theader = NULL, id = paste0(dyad_id, "_Zone"))
                        }
                        return(df)
                      },

                      get_dyad_zone = function(id, df, sensemakerframeworkrobject) {
                        leftLabel <- sensemakerframeworkrobject$get_dyad_aspercent_x_column_name(id)
                        dyadZone <- vector("list", length = nrow(df))
                        for (i in 1:nrow(df)) {
                          if (is.na(df[i, leftLabel])) {
                            dyadZone[[i]] <- NA
                            next
                          }
                          if (df[i,leftLabel] > 80) {dyadZone[[i]] <- "Right"}
                          if (df[i,leftLabel] > 60 & df[i,leftLabel] <= 80) {dyadZone[[i]] <- "Centre_Right"}
                          if (df[i,leftLabel] > 40 & df[i,leftLabel] <= 60) {dyadZone[[i]] <- "Centre"}
                          if (df[i,leftLabel] > 20 & df[i,leftLabel] <= 40) {dyadZone[[i]] <- "Centre_Left"}
                          if (df[i,leftLabel] <= 20) {dyadZone[[i]] <- "Left"}
                        }
                        return(unlist(dyadZone))
                      },

                      add_stones_zones = function(df, sensemakerframeworkrobject) {
                        for (stones_id in sensemakerframeworkrobject$get_stones_ids()) {
                          for (stone_id in sensemakerframeworkrobject$get_stones_items_ids(stones_id)) {
                            #  x axis zones
                            df[[paste0(stones_id, "_", stone_id, "_x_Zone")]] <- private$get_stone_zone(stones_id, stone_id, df, sensemakerframeworkrobject, "x")
                            df[[paste0(stones_id, "_", stone_id, "_y_Zone")]] <- private$get_stone_zone(stones_id, stone_id, df, sensemakerframeworkrobject, "y")
                            df[[paste0(stones_id, "_", stone_id, "_4_Zone")]] <- private$get_stone_4_zone(stones_id, stone_id, df, sensemakerframeworkrobject)
                            df[[paste0(stones_id, "_", stone_id, "_9_Zone")]] <- private$get_stone_9_zone(stones_id, stone_id, df, sensemakerframeworkrobject)
                            df[[paste0(stones_id, "_", stone_id, "_x_Zone")]] <- factor(df[[paste0(stones_id, "_", stone_id, "_x_Zone")]], levels = c("Left", "Centre_Left", "Centre", "Centre_Right", "Right"), ordered = TRUE)
                            df[[paste0(stones_id, "_", stone_id, "_y_Zone")]] <- factor(df[[paste0(stones_id, "_", stone_id, "_y_Zone")]], levels = c("Left", "Centre_Left", "Centre", "Centre_Right", "Right"), ordered = TRUE)
                            df[[paste0(stones_id, "_", stone_id, "_4_Zone")]] <- factor(df[[paste0(stones_id, "_", stone_id, "_4_Zone")]], levels =  c("Top_Left", "Top_Right", "Bottom_Left", "Bottom_Right"), ordered = TRUE)
                            df[[paste0(stones_id, "_", stone_id, "_9_Zone")]] <- factor(df[[paste0(stones_id, "_", stone_id, "_9_Zone")]], levels =  c("Top_Left", "Top_Centre", "Top_Right",  "Centre_Left", "Centre", "Centre_Right", "Bottom_Left", "Bottom_Centre", "Bottom_Right"), ordered = TRUE)
                            # add the zones as list item
                            # 1. x zones
                            list_items <- data.frame(id = c("Left", "Centre_Left", "Centre", "Centre_Right", "Right"), title = c("Left", "Centre Left", "Centre", "Centre Right", "Right"), tooltip = c("Left", "Centre Left", "Centre", "Centre Right", "Right"), visible = c(TRUE, TRUE, TRUE, TRUE, TRUE), other_signifier_id = c("", "", "", "", ""))
                            temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(stones_id), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(stones_id, stone_id),  "_", "x_Zone")
                            sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL,  sig_class = "zone", theader = NULL, id = paste0(stones_id, "_", stone_id, "_x_Zone"))
                            # 2. y zones
                            list_items <- data.frame(id = c("Left", "Centre_Left", "Centre", "Centre_Right", "Right"), title = c("Left", "Centre Left", "Centre", "Centre Right", "Right"), tooltip = c("Left", "Centre Left", "Centre", "Centre Right", "Right"), visible = c(TRUE, TRUE, TRUE, TRUE, TRUE), other_signifier_id = c("", "", "", "", ""))
                            temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(stones_id), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(stones_id, stone_id),  "_", "y _Zone")
                            sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL,  sig_class = "zone", theader = NULL, id = paste0(stones_id, "_", stone_id, "_y_Zone"))
                            # 3. 4 zones
                            list_items <- data.frame(id = c("Top_Left", "Top_Right", "Bottom_Left", "Bottom_Right"), title = c("Top Left", "Top Right", "Bottom Left", "Bottom Right"), tooltip = c("Top Left", "Top Right", "Bottom Left", "Bottom Right"), visible = c(TRUE, TRUE, TRUE, TRUE), other_signifier_id = c("", "", "", ""))
                            temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(stones_id), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(stones_id, stone_id),  "_", "4_Zone")
                            sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL,  sig_class = "zone", theader = NULL, id = paste0(stones_id, "_", stone_id, "_4_Zone"))
                            # 4. 9 zones
                            list_items <- data.frame(id = c("Top_Left", "Top_Centre", "Top_Right",  "Centre_Left", "Centre", "Centre_Right", "Bottom_Left", "Bottom_Centre", "Bottom_Right"), title = c("Top Left", "Top Centre", "Top Right",  "Centre Left", "Centre", "Centre Right", "Bottom Left", "Bottom Centre", "Bottom Right"), tooltip = c("Top Left", "Top Centre", "Top Right",  "Centre Left", "Centre", "Centre Right", "Bottom Left", "Bottom Centre", "Bottom Right"), visible = c(TRUE, TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, TRUE, TRUE), other_signifier_id = c("", "", "", "", "", "", "", "", ""))
                            temp_title <- paste(sensemakerframeworkrobject$get_signifier_title(stones_id), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(stones_id, stone_id),  "_", "9_Zone")
                            sensemakerframeworkrobject$add_list(title = temp_title, tooltip = temp_title, allow_na = FALSE, fragment = FALSE, required = FALSE, sticky = FALSE, items = list_items,  max_responses = 1, min_responses = 1, other_item_id = NULL, other_signifier_id = NULL,  sig_class = "zone", theader = NULL, id = paste0(stones_id, "_", stone_id, "_9_Zone"))

                          }
                        }

                        # In the original workbench we now would add all the MCQ factors that would include the additional MCQs - BUT
                        # because we always do our graphing and statistical output by title/labels and not IDs then the factoring levels has to be
                        # calculated at the output point, not in the datar package.


                        return(df)

                      },

                      get_stone_zone = function(stones_id, stone_id, df, sensemakerframeworkrobject, axis) {

                        label <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(stones_id, stone_id, axis = axis)
                        stone_zone <- vector("list", length = nrow(df))

                        for (i in 1:nrow(df)) {
                          if (is.na(df[i, label])) {
                            stone_zone[[i]] <- NA
                            next
                          }
                          if (df[i,label] > 0.8) {stone_zone[[i]] <- "Right"}
                          if (df[i,label] > 0.6 & df[i,label] <= 0.8) {stone_zone[[i]] <- "Centre_Right"}
                          if (df[i,label] > 0.4 & df[i,label] <= 0.8) {stone_zone[[i]] <- "Centre"}
                          if (df[i,label] > 0.2 & df[i,label] <= 0.4) {stone_zone[[i]] <- "Centre_Left"}
                          if (df[i,label] <= 0.2) {stone_zone[[i]] <- "Left"}
                        }
                        return(unlist(stone_zone))
                      },


                      get_stone_4_zone = function(stones_id, stone_id, df, sensemakerframeworkrobject, axis) {
                        x_label <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(stones_id, stone_id, axis = "x")
                        y_label <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(stones_id, stone_id, axis = "y")
                        stone_zone <- vector("list", length = nrow(df))
                        for (i in 1:nrow(df)) {
                          if (is.na(df[i, x_label])) {
                            stone_zone[[i]] <- NA
                            next
                          }
                          if (df[i, x_label] >= 0.5 & df[i, y_label] >= 0.5) {stone_zone[[i]] <- "Top_Right"}
                          if (df[i, x_label] >= 0.5 & df[i, y_label] < 0.5) {stone_zone[[i]] <- "Bottom_Right"}
                          if (df[i, x_label] < 0.5 & df[i, y_label] < 0.5) {stone_zone[[i]] <- "Bottom_Left"}
                          if (df[i, x_label] < 0.5 & df[i, y_label] >= 0.5) {stone_zone[[i]] <- "Top_Left"}
                        }
                        return(unlist(stone_zone))
                      },


                      get_stone_9_zone = function(stones_id, stone_id, df, sensemakerframeworkrobject, axis) {
                        x_label <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(stones_id, stone_id, axis = "x")
                        y_label <- sensemakerframeworkrobject$get_stones_stone_compositional_column_names(stones_id, stone_id, axis = "y")
                        stone_zone <- vector("list", length = nrow(df))
                        for (i in 1:nrow(df)) {
                          if (is.na(df[i, x_label])) {
                            stone_zone[[i]] <- NA
                            next
                          }
                          if (df[i, x_label] >= 0.66666666 & df[i, y_label] >= 0.66666666) {stone_zone[[i]] <- "Top_Right"}
                          if (df[i, x_label] >= 0.66666666 & df[i, y_label] >= 0.33333333 & df[i, y_label] < 0.66666666) {stone_zone[[i]] <- "Centre_Right"}
                          if (df[i, x_label] >= 0.66666666 & df[i, y_label] < 0.33333333) {stone_zone[[i]] <- "Bottom_Right"}
                          if (df[i, x_label] >= 0.33333333 & df[i, x_label] < 0.66666666 & df[i, y_label] >= 0.66666666) {stone_zone[[i]] <- "Top_Centre"}
                          if (df[i, x_label] >= 0.33333333 & df[i, x_label] < 0.66666666 & df[i, y_label] >= 0.33333333 & df[i, y_label] < 0.66666666) {stone_zone[[i]] <- "Centre"}
                          if (df[i, x_label] >= 0.33333333 & df[i, x_label] < 0.66666666 &  df[i, y_label] < 0.33333333) {stone_zone[[i]] <- "Bottom_Centre"}
                          if (df[i, x_label] < 0.33333333 & df[i, y_label] >= 0.66666666) {stone_zone[[i]] <- "Top_Left"}
                          if (df[i, x_label] < 0.33333333 & df[i, y_label] >= 0.33333333 & df[i, y_label] < 0.66666666) {stone_zone[[i]] <- "Centre_Left"}
                          if (df[i, x_label] < 0.33333333 & df[i, y_label] < 0.33333333) {stone_zone[[i]] <- "Bottom_Left"}
                        }
                        return(unlist(stone_zone))
                      },


                      # set up the selected and not selected columns for a multi-select column
                      process_col = function(x, df) {
                        col_vals <- df[[x]]
                        return(unlist(unname(purrr::map(col_vals, ~ {ifelse(is.na(.x), "Not Selected", "Selected")}))))
                      },

                      apply_dates = function(df) {
                        df[["started"]] <- as.character(lubridate::as_datetime(df[["server_upload_time"]]/1000, tz = "GMT"))
                        df[["ServerEntryDate"]] <-  lubridate::as_date(df[["started"]] )
                        df[["EntryYr"]] <- as.integer(strftime(df[["ServerEntryDate"]], format = "%Y"))
                        df[["EntryYrMth"]] <- as.integer(strftime(df[["ServerEntryDate"]], format = "%Y%m"))
                        df[["EntryYrMthDay"]] <- as.integer(strftime(df[["ServerEntryDate"]], format = "%Y%m%d"))
                        return(df)
                      },

                      get_API_framework_data = function(end_point, framework_id, token, isDemonstratorAccount = FALSE, out_msg = FALSE) {

                        # Get the data from the API saving to temp folder, read csv into dataframe, remove temp file and return dataframe
                        df1FileName <- tempfile(pattern = "", fileext = ".csv")
                        r1 <-  httr::GET(paste0("https://api-gateway.sensemaker-suite.com/v2/frameworks/", framework_id, "/captures?includeLabels=false"),
                                         httr::add_headers(.headers = c('Authorization'= paste('Bearer ', token), 'Accept' = 'text/csv; version=1')),
                                         httr::write_disk(df1FileName, overwrite=TRUE), httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE))
                        DF1I <- as.data.frame(readr::read_csv(file = df1FileName, col_names = TRUE))
                        unlink(df1FileName)

                        #write.csv(DF1I, file = "api_data.csv", row.names = FALSE, na = "")

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
                        self$sig_ids_to_remove <- framework_signifiers_missing
                        self$col_names_to_remove <- sensemakerframeworkrobject$get_col_names_ids(framework_signifiers_missing)
                       # purrr::walk(col_names, ~ {df <<- df |> dplyr::select(-.x)})
                      #  df <- df |>  dplyr::select(-!!col_names)
                       # df <- df |> dplyr::select(-any_of(col_names[[2]]))
                        purrr::walk(framework_signifiers_missing, ~ {sensemakerframeworkrobject$change_signifier_include(.x, value = FALSE)}, sensemakerframeworkrobject)


                        # if this is a combined dashboard, then get each mapping framework and process
                        if (self$is_combined_dashboard()) {
                          df <- private$combine_data(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject)
                        }
                        #  write.csv(x = df, file = "data.csv", na = "", row.names = FALSE)
                        if(self$dashboard_has_filters()) {
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
                            if (attribute_id != "00000000-0000-0000-0000-000000000000") {

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
                        }


                        # now do the date if exists
                        filter_defs <- self$dashboard_definition_v1$filters %>% dplyr::filter(id == "date_range_filter")
                        if (nrow(filter_defs) > 0) {
                          from_dte <- lubridate::as_date(filter_defs[["value"]][[1]][[1]])
                          to_dte <- lubridate::as_date(filter_defs[["value"]][[1]][[2]])
                          diff_in_days<- difftime(to_dte, from_dte, units = c("days"))
                          # If the miin/max of the actual data don't overlay with the min/max of the query then you
                          # know that the query was set up for dates that are not applicable so return full dataset
                          min_max_date <- df %>%
                            # transform to date format with lubridate
                            dplyr::mutate(ServerEntryDate = lubridate::ymd(ServerEntryDate)) %>%
                            # find min and max
                            dplyr::summarise(
                              min = min(ServerEntryDate),
                              max = max(ServerEntryDate))
                          if (difftime(min_max_date[, "max"], from_dte, units = c("days")) > 0) {return(df)}
                          if (difftime(to_dte, min_max_date[, "min"], units = c("days")) < 0) {return(df)}

                          print("we are here doing the date filter")
                          print(paste("from date", from_dte, "to date", to_dte))
                          qry <- paste0("ServerEntryDate >= ", "\"",  from_dte, "\"",  " & ServerEntryDate <= ", "\"", to_dte, "\"")
                          query_string <- ifelse(is.null(query_string), qry, paste0(query_string, " & ", qry))

                        }

                        if (is.null(query_string)) {return(df)}
                        df_ret <- df %>% dplyr::filter(eval(parse(text = query_string)))
                        return(df_ret)
                      },

                      combine_data = function(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject) {
                        fw_mappings <- self$get_dashboard_combined_mappings()
                        combined_fw <- vector("list", length = length(names(fw_mappings)))
                        names(combined_fw) <- names(fw_mappings)
                        for (fw_id in names(fw_mappings)) {
                          combined_fw_obj <- sensemakerframeworkr::Signifiers$new(jsonfilename = NULL, layoutfilename = NULL, parsedjson = NULL, parsedlayout = NULL, workbenchid = fw_id, token = token)
                          combined_fw[[fw_id]] <- combined_fw_obj$get_parent_framework_name()
                          mcq_data <- NULL
                          # get combined framework data and load definition (maybe not) and do the date application (needed for filtering so done now)

                          fw_data <- private$get_API_framework_data(end_point, fw_id, token, self$demonstrator)

                          fw_data <- private$apply_dates(fw_data)


                          # Don't need this now since we get this from the sensemakerframeworkr object.
                          #fw_json <- private$get_framework_definition(end_point, fw_id, token)

                          if (nrow(fw_data) == 0) {next}
                          # map the fields - "end" is primary and "start" is the combined. "_" means a list item
                          for (i in seq_along(fw_mappings[[fw_id]][["end"]])) {
                            # A non list item
                            if (all(is.na(stringr::str_locate(fw_mappings[[fw_id]][i, "end"], "_")) == TRUE)) {
                              if ((any(grepl(fw_mappings[[fw_id]][i, "end"], colnames(df), fixed = TRUE) == TRUE)) & (any(grepl(fw_mappings[[fw_id]][i, "start"], colnames(fw_data), fixed = TRUE) == TRUE))) {
                                # if the signifier id is not in the list of signifiers (for whatever reason) then next
                                if (!(fw_mappings[[fw_id]][i, "end"] %in% sensemakerframeworkrobject$get_all_signifier_ids())) {next}
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
                              entry <- entries %>% dplyr::filter(start_item_id == sig_col[[data_idx]]) %>% dplyr::select(end_item_id)
                              entry <- entry$end_item_id
                              if (length(entry) == 0) {entry <- NA}
                              sig_col[[data_idx]] <- entry
                              # sig_col[[data_idx]] <- entries %>% dplyr::filter(start_item_id == sig_col[[data_idx]]) %>% dplyr::select(end_item_id)
                            }

                            sig_col <- unname(unlist(sig_col))
                            if (!is.null(sig_col)) {
                              fw_data[[sig_id]] <- as.character(sig_col)
                            }
                          }

                          df <- dplyr::bind_rows(df, fw_data)
                        }

                        # for drop downs - e.g. in project list on the workbench, we will be using the rev version
                        combined_fw_rev <-setNames(names(combined_fw), combined_fw)
                        self$dashboard_combined_frameworks_names <- combined_fw
                        self$dashboard_combined_frameworks_ids <- combined_fw_rev

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
                        if (self$is_combined_dashboard()) {
                          df <- private$combine_data(df, framework_id, end_point, dashboard_id, token, is_demonstrator, sensemakerframeworkrobject)
                        }

                        # If filters are defined, filter the data
                        if (self$dashboard_has_filters()) {
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
                              if (attribute_id != "00000000-0000-0000-0000-000000000000") {
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
                        }

                        if(is.null(query_string)) {return(df)}
                        ret_df <- df %>% dplyr::filter(eval(parse(text = query_string)))
                        return(ret_df)
                      },

                      get_framework_definition = function(end_point, framework_id, token, out_msg = FALSE) {

                        return(jsonlite::fromJSON(httr::content(httr::GET(
                          paste0("https://", end_point, ".sensemaker-suite.com/apis/projectdefinition/",  framework_id),
                          httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                         , 'Content-Type' = 'application/json')), httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE)
                        ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))


                      },
                      get_v1_DashboardDefinition = function(end_point, dashboard_id, token) {
                        # we are in the get dashboard definition
                        out <- try( {
                          # get the json from the returned project definition

                          return(jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://", end_point, ".sensemaker-suite.com/apis/dashboards/",  dashboard_id),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                           , 'Content-Type' = 'application/json')), httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE)
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

                      get_v2_DashboardDefinition = function(end_point, dashboard_id, token, out_msg = FALSE) {
                        # we are in the get dashboard definition


                        # get the json from the returned project definition

                        v2_definition <- jsonlite::fromJSON(httr::content(httr::GET(
                          paste0("https://api-gateway.sensemaker-suite.com/v2/dashboards/", dashboard_id),
                          httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                         , 'Content-Type' = 'application/json')),  httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE)
                        ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                        v2_layout <- jsonlite::fromJSON(httr::content(httr::GET(
                          paste0("https://api-gateway.sensemaker-suite.com/v2/dashboards/", dashboard_id, "/layouts"),
                          httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                         , 'Content-Type' = 'application/json')),  httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE)
                        ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                        # get the json from the returned project definition

                        v2_filters <- jsonlite::fromJSON(httr::content(httr::GET(
                          paste0("https://api-gateway.sensemaker-suite.com/v2/dashboards/", dashboard_id, "/filters"),
                          httr::add_headers(.headers = c('Authorization' = paste("Bearer", token, sep = " ")
                                                         , 'Content-Type' = 'application/json')),  httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE)
                        ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE)

                        ret_list <- list(v2_definition = v2_definition, v2_layout = v2_layout, v2_filters = v2_filters)

                        return(ret_list)
                      },

                      # Demonstrator accounts only allowed a subset of the data.
                      get_is_demonstrator = function(trToken, out_msg = FALSE) {

                        out <- try({
                          #
                          isDemonstratorAccount <- FALSE
                          sessionDetails <- jsonlite::fromJSON(httr::content(httr::GET(
                            paste0("https://api.singularity.icatalyst.com/api/session"),
                            httr::add_headers(.headers = c('Authorization' = paste("Bearer", trToken, sep = " ")
                                                           , 'Content-Type' = 'application/json')), httr::verbose(data_out = out_msg, data_in = FALSE, info = FALSE, ssl = FALSE)
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
                      },

                      # put the multi-seect mcq data into long form structure ready for graphing
                      transform_multi_select = function(df, sensemakerframeworkrobject) {
                        # No data at all so just return with the emmty tdf1
                        if (nrow(df) == 0) {
                          return(df)
                        }
                        multi_IDs <- sensemakerframeworkrobject$get_multiselect_list_ids()
                        # No multi-select MCQs in project so return just the df
                        if (is.null(multi_IDs)) {
                          return(df)
                        }

                        multi_MCQs <- vector("list", length(multi_IDs))
                        names(multi_MCQs) <- multi_IDs
                        for (i in seq_along(multi_IDs)) {
                          mcqItems <- sensemakerframeworkrobject$get_list_items_mcq_list(multi_IDs[[i]])
                          #mcqColumns <- paste0(multi_IDs[[i]], "_", unlist(mcqItems))
                          mcqColumns <- sensemakerframeworkrobject$get_list_column_mcq_names(multi_IDs[[i]], delist = TRUE)
                          multi_MCQs[[i]] <- df %>% tidyr::gather(FileattributeKey, Occurs, mcqColumns) %>% dplyr::select(FragmentID, FileattributeKey, Occurs) %>% dplyr::filter(!is.na(Occurs))
                          MCQItemNames <- data.frame(attributeKey =   rep(multi_IDs[[i]], length(mcqColumns)),  MCQItemID = mcqColumns, DisplayValue = names(unlist(mcqItems)))
                          multi_MCQs[[i]] <- dplyr::left_join(multi_MCQs[[i]], MCQItemNames, by = c("FileattributeKey" = "MCQItemID"))
                          multi_MCQs[[i]][["FileattributeKey"]] <- factor(multi_MCQs[[i]][["FileattributeKey"]], levels = mcqColumns)
                          multi_MCQs[[i]][["DisplayValue"]] <- factor(multi_MCQs[[i]][["DisplayValue"]], levels = names(unlist(mcqItems)))
                          multi_MCQs[[i]][["attributeKey"]] <- factor(multi_MCQs[[i]][["attributeKey"]], levels = multi_IDs[[i]])
                        }
                        return(multi_MCQs)
                      },

                      # get the stone ratios of each of the stone canvases - note legacy commented out code still left here for now.
                      # ToDo - remove commented code when safe
                      getStoneRatios = function(data, sensemakerframeworkrobject) {

                        if (sensemakerframeworkrobject$get_stones_count() == 0) {return(NULL)}

                        stoneIDs <- sensemakerframeworkrobject$get_stones_ids()

                        stoneRatios <- vector('list', length = length(stoneIDs))

                        for (i in seq_along(stoneIDs)) {

                          imagePath <- sensemakerframeworkrobject$get_stones_background_image(stoneIDs[[i]])
                          imagePath <- utils::URLencode(imagePath)

                          imageSplit <- stringr::str_split(imagePath, "\\.")
                          if (!(grepl("https", imageSplit[[1]][[1]], fixed = TRUE))) {
                            stoneRatios[[i]] <- 1.5
                            next
                          }

                          if (tools::file_ext(imagePath) == "svg") {
                            img <- svgtools::read_svg(file = imagePath)
                            imgXML <- XML::xmlParse(img)
                            rootnode <- XML::xmlRoot(imgXML)
                            rn1 <- XML::xmlToList(rootnode[[1]][1][1][["clipPath"]][1][["rect"]])
                            width <- as.numeric(rn1[["width"]])
                            height <- as.numeric(rn1[["height"]])
                            stoneRatios[[i]] <- width / height
                            next
                          }


                          #   imageType <- imageSplit[[1]][[length(imageSplit[[1]])]]
                          z <- tempfile()
                          file_extension <- tools::file_ext(imagePath)
                          download.file(imagePath, paste0(z, ".", file_extension), mode="wb")
                          img <- imager::load.image(file = paste0(z, ".", file_extension))
                          calRatio <- imager::width(img) / imager::height(img)
                          #    if (imageType %in% c("jpg", "jpeg")) {
                          # pic <- readJPEG(paste0(z, ".jpeg"))
                          #    } else {
                          #      if (imageType %in% c("png")) {
                          #        pic <- readPNG(z)
                          #      }
                          #    }
                          # file.remove(z)

                          # calRatio <- dim(pic[[2]]) / dim(pic[[1]])

                          if (length(calRatio) == 0) {
                            calRatio <- 1.5
                          }
                          stoneRatios[[i]] <- calRatio
                        }
                        names(stoneRatios) <- stoneIDs
                        return(stoneRatios)
                        #  }
                      },

                      convert_data_to_titles = function(data, sensemakerframeworkrobject, column_type = "ALL") {

                        # update the projectID with the name
                        framework_name <- sensemakerframeworkrobject$get_parent_framework_name()

                        data[["project_id"]] <- unlist(purrr::map(data[["project_id"]], ~ {framework_name}))

                        # update the single select signifier list data
                        lists <- sensemakerframeworkrobject$get_single_select_list_ids(sig_class = "signifier")
                        purrr::walk(lists, function(x) data[[x]] <<-
                                      unlist(purrr::map(data[[x]], function(y) ifelse(is.na(y), NA,
                                                                                      sensemakerframeworkrobject$get_list_item_title(x, y)) )))
                        # update multi_select_id data column names
                        purrr::walk(sensemakerframeworkrobject$get_multiselect_list_ids(),
                                    function(x) purrr::walk(sensemakerframeworkrobject$get_list_items_ids(x), function(y)
                                      colnames(data)[colnames(data) == paste0(x, "_", y)] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "_",
                                                                                                     sensemakerframeworkrobject$get_list_item_title(x, y))))
                        # update multi_select selected column names
                        purrr::walk(sensemakerframeworkrobject$get_multiselect_list_ids(),
                                    function(x) purrr::walk(sensemakerframeworkrobject$get_list_items_ids(x), function(y)
                                      colnames(data)[colnames(data) == paste0(x, "_", y, "_selected")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "_",
                                                                                                                  sensemakerframeworkrobject$get_list_item_title(x, y), "_selected")))
                        # update free text box entries
                        purrr::walk(sensemakerframeworkrobject$get_freetext_ids(),
                                    function(x) {colnames(data)[colnames(data) == x] <<- sensemakerframeworkrobject$get_signifier_title(x);
                                    if (sensemakerframeworkrobject$get_signifier_allow_na(x)) {colnames(data)[colnames(data) == paste0(x, "_NA")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "_NA")}})

                        # update single select entries
                        purrr::walk(sensemakerframeworkrobject$get_single_select_list_ids(sig_class = "signifier"),
                                    function(x) {colnames(data)[colnames(data) == x] <<- sensemakerframeworkrobject$get_signifier_title(x);
                                    if (sensemakerframeworkrobject$get_signifier_allow_na(x)){colnames(data)[colnames(data) == paste0(x, "_NA")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "_NA") }})
                        purrr::walk(sensemakerframeworkrobject$get_single_select_list_ids(sig_class = "single_select_item"),
                                    function(x) colnames(data)[colnames(data) == x] <<- paste0(sensemakerframeworkrobject$get_signifier_title(stringr::str_split(x, "_")[[1]][[1]]), "_selected"))

                        # zones
                        purrr::walk(sensemakerframeworkrobject$get_triad_ids(), ~ {colnames(data)[colnames(data) == paste0(.x, "_Zone")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(.x), "_Zone")})
                        purrr::walk(sensemakerframeworkrobject$get_dyad_ids(), ~ {colnames(data)[colnames(data) == paste0(.x, "_Zone")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(.x), "_Zone")})
                        purrr::walk(sensemakerframeworkrobject$get_stones_ids(), function(x)
                          purrr::walk(sensemakerframeworkrobject$get_stones_stone_ids(x), function(y) {
                            colnames(data)[colnames(data) == paste0(x, "_", y, "_x_Zone")] <<-
                              paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(x, y), "_x_Zone");
                            colnames(data)[colnames(data) == paste0(x, "_", y, "_y_Zone")] <<-
                              paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(x, y), "_y_Zone");
                            colnames(data)[colnames(data) == paste0(x, "_", y, "_4_Zone")] <<-
                              paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(x, y), "_4_Zone");
                            colnames(data)[colnames(data) == paste0(x, "_", y, "_9_Zone")] <<-
                              paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(x, y), "_9_Zone")
                          }))
                        # we are adding the shape sliders now
                        if (column_type == "ALL") {
                          # triads
                          purrr::walk(sensemakerframeworkrobject$get_triad_ids(),
                                      function(x) {colnames(data)[colnames(data) == paste0(x, "X")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "X");
                                      colnames(data)[colnames(data) == paste0(x, "Y")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "Y");
                                      if (sensemakerframeworkrobject$get_signifier_allow_na(x))
                                      {colnames(data)[colnames(data) == paste0(x, "_NA")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "_NA")}
                                      })
                          purrr::walk(sensemakerframeworkrobject$get_triad_ids(),
                                      function(x) purrr::walk(sensemakerframeworkrobject$get_triad_anchor_ids(x),
                                                              function(y)  colnames(data)[colnames(data) == paste0(x, "_", y)] <<-
                                                                paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_triad_anchor_text_by_id(x, y))))

                          # dyads
                          purrr::walk(sensemakerframeworkrobject$get_dyad_ids(),
                                      function(x) {colnames(data)[colnames(data) == paste0(x, "X")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "X");
                                      colnames(data)[colnames(data) == paste0(x, "XR")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "XR");
                                      if (sensemakerframeworkrobject$get_signifier_allow_na(x))
                                      {colnames(data)[colnames(data) == paste0(x, "_NA")] <<- paste0(sensemakerframeworkrobject$get_signifier_title(x), "_NA")}
                                      })
                          purrr::walk(sensemakerframeworkrobject$get_dyad_ids(),
                                      function(x) purrr::walk(sensemakerframeworkrobject$get_dyad_anchor_ids(x),
                                                              function(y)  colnames(data)[colnames(data) == paste0(x, "_", y)] <<-
                                                                paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_dyad_anchor_text_by_id(x, y))))

                          # stones
                          purrr::walk(sensemakerframeworkrobject$get_stones_ids(), function(x)
                            purrr::walk(sensemakerframeworkrobject$get_stones_stone_ids(x), function(y) {
                              colnames(data)[colnames(data) == paste0(x, "_", y, "XRight")] <<-
                                paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(x, y), "XRight");
                              colnames(data)[colnames(data) == paste0(x, "_", y, "YTop")] <<-
                                paste0(sensemakerframeworkrobject$get_signifier_title(x), "_", sensemakerframeworkrobject$get_stones_stone_title_by_id(x, y), "YTop")}))

                          # unique_id
                          purrr::walk(sensemakerframeworkrobject$get_uniqueid_ids(), ~ {colnames(data)[colnames(data) == .x] <<- sensemakerframeworkrobject$get_signifier_title(.x)})
                          # image select
                          purrr::walk(sensemakerframeworkrobject$get_imageselect_ids(), ~ {colnames(data)[colnames(data) == .x] <<- sensemakerframeworkrobject$get_signifier_title(.x)})
                          # audio
                          purrr::walk(sensemakerframeworkrobject$get_audio_ids(), ~ {colnames(data)[colnames(data) == .x] <<- sensemakerframeworkrobject$get_signifier_title(.x)})
                          # photo
                          purrr::walk(sensemakerframeworkrobject$get_photo_ids(), ~ {colnames(data)[colnames(data) == .x] <<- sensemakerframeworkrobject$get_signifier_title(.x)})

                          # NOTE and ToDo - we can't finish this because of the issues with differences in syntax for the other list item options.

                        }
                        #  print(colnames(data))

                        return(data)

                      },

                      apply_fragment_level_updates = function(df, sensemakerframeworkrobject) {

                        # so much could go wrong here but we are allowing this to crash if it does

                        for (i in 1:length(self$fragment_level_upload)) {
                          load_data <- self$fragment_level_upload[[i]]
                          # make sure that the column names are R compatable names
                          # purrr::walk(colnames(load_data), ~ {colnames(load_data)[colnames(load_data) == .x] <<- make.names(stringr::str_remove_all(.x, " "))})
                          colnames(load_data)[[1]] <- "FragmentID"
                          # stop if any of the columns already exist in the main data frame
                          if (any(unlist(purrr::map(colnames(load_data)[2:length(colnames(load_data))], ~ {.x %in% colnames(df)} )) == TRUE)) {
                            print("upload file should not have column names already as signifier IDs")
                            stop()
                          }
                          # add the new data to the main data frame
                          df <-  dplyr::left_join(x = df, y = load_data, by = c("FragmentID"))
                          # now update the frameworkr to the new list items
                          purrr::walk(colnames(load_data)[2:length(colnames(load_data))], ~ {
                            list_item_ids <- sort(unique(load_data[[.x]]));
                            temp_items <- data.frame(id = list_item_ids, title = list_item_ids, tooltip = list_item_ids,
                                                     visible = rep_len("TRUE", length(list_item_ids)),
                                                     other_signifier_id = rep_len("", length(list_item_ids)));
                            sensemakerframeworkrobject$add_list(title = .x, tooltip = .x, allow_na = FALSE, fragment = FALSE, required = FALSE,
                                                                sticky = FALSE, items = temp_items,  max_responses = 1, min_responses = 1,
                                                                other_item_id = NULL, other_signifier_id = NULL, theader = NULL,
                                                                id = .x)

                          })
                        }
                        return(df)
                      },

                      apply_FK_level_updates = function(df, sensemakerframeworkrobject) {

                        for (i in 1:length(self$FK_level_upload)) {
                          load_data <- self$FK_level_upload[[i]]
                          # make sure that the column names are R compatable names
                          # purrr::walk(colnames(load_data), ~ {colnames(load_data)[colnames(load_data) == .x] <<- make.names(stringr::str_remove_all(.x, " "))})
                          df <-  dplyr::left_join(x = df, y = load_data, by = c(colnames(load_data)[[1]]))
                          col_name <- colnames(load_data)[[2]]
                          col_values <- sort(unique(load_data[[2]]))
                          temp_items <- data.frame(id = col_values, title = col_values, tooltip = col_values, visible = rep_len("TRUE", length(col_values)), other_signifier_id = rep_len("", length(col_values)))
                          self$sm_framework$add_list(title = col_name, tooltip = col_name, allow_na = FALSE, fragment = FALSE, required = TRUE, sticky = FALSE,
                                                     items = temp_items, max_responses = 1, min_responses = 1, other_item_id = NA, other_signifier_id = NA, id = col_name)
                        }
                        return(df)


                      }




                    )

)
