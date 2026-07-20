


# Function to pull data per .query ----
query_db <-
  function(db_scenario_name, ds_scenario_name, db_path, db_name, .query, .region = c()) {
    # expect errors as we expect partial updates (i.e., not all DBs will exist)
    tryCatch(
      {
        # turn off validatedb given we are already checking for errors
        # to reduce the number of messages
        localDBConn(db_path, db_name, validatedb = FALSE, maxMemory = '8g') %>%
          runQuery(query = .query, db_scenario_name, region = .region) %>%
          mutate(scenario = ds_scenario_name)
      },
      # in case of error just the return an empty tibble with the correct columns
      error = function(e) {
        warning(paste0("No results for: ", db_scenario_name, " in ", db_path, "/", db_name))
        tibble(scenario = character(),
               region = character(),
               year = integer(),
               productivity = numeric(),
               gcam.version = character())
      }
    )
  }

# Function to search query and arrange data ----

Pull_Query_Title <- function(.QUERY_TITLE,
                             .XML_QUERY = Main_Query_XML,
                             .SCENARIO_DB_MAP = SCENARIO_DB_MAP,
                             .REG = c()){

  # Safely escape XPath string (handles single quotes)
  safe_xpath_string <- function(s) {
    if (grepl("'", s)) {
      parts <- strsplit(s, "'")[[1]]
      paste0("concat('", paste(parts, collapse = "', \"'\", '"), "')")
    } else {
      paste0("'", s, "'")
    }
  }

  # Build XPath with safe title string
  xpath_expr <- paste0("//*[contains(@title, ", safe_xpath_string(.QUERY_TITLE), ")]")
  matched_nodes <- xml_find_all(.XML_QUERY, xpath_expr)

  if (length(matched_nodes) == 0) {
    stop("No query found with a title containing: ", .QUERY_TITLE)
  } else if (length(matched_nodes) > 1) {
    warning(length(matched_nodes), " queries matched title '", .QUERY_TITLE, "'. Using the first one.")

    matched_nodes[1] -> matched_nodes

    # Optional: print titles to console for inspection
    matched_titles <- xml_attr(matched_nodes, "title")
    message("Matched titles:\n", paste0(" - ", matched_titles, collapse = "\n"))
  }

  rlang::inform(paste0("grepping ", .QUERY_TITLE, ", found 1 query, extracting data..."))

  # Find a specific query from the main query
  QUERY_CHAR <-
    matched_nodes %>%
    as.character()

  .SCENARIO_DB_MAP %>%
    group_by_all() %>%
    mutate(data =
             list(query_db(db_scenario_name, ds_scenario_name, db_path, db_name,
                           .query = QUERY_CHAR, .region = .REG))) %>%
    pull(data) %>%
    bind_rows() %>%
    { if ("rundate" %in% names(.)) dplyr::select(., -rundate) else . } %>%
    rename_with(tolower) %>%
    rename_with(~ gsub("[\\. ]+", "_", .))

}





