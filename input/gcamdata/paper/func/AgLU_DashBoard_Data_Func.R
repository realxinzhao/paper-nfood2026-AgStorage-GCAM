# AgLU_DashBoard_func


# define cleaning funcs----
Reg_Clean <- function(.df){

  # map regions
  .df %>%
    left_join_error_no_match(
      Regmapping %>%
        distinct(region, REG10_AR6),
      by = "region"
    ) %>%
    select(-region) %>%
    rename(region = REG10_AR6) %>%
    group_by_at(vars(-value)) %>%
    summarize(value = sum(value, na.rm = T), .groups = "drop") ->
    .df1

  # add/agg world
  .df1 %>%
    group_by_at(vars(-region, -value)) %>%
    summarize(value = sum(value, na.rm = T), .groups = "drop") %>%
    mutate(region = "World") %>%
    bind_rows(.df1) ->
    .df2

  return(.df2)

  "World" %>%
    c(Regmapping %>% distinct(REG10_AR6) %>% arrange() %>% pull) ->
    RegLab

  # factor regions
  .df2 %>%
    mutate(region = factor(region, levels = RegLab)) %>%
    return()
}


# AgSector_Clean <- function(.df){
#
#   .df %>%
#     mutate(sector = tolower(sector)) %>%
#     left_join_error_no_match(
#       MapAgCOMM %>% distinct(sector = tolower(AgCOMM), AgCOMM5),
#       by = "sector") %>%
#     filter(AgCOMM5 %in% c("Grains", "Oil crops", "Sugar crops", "Fruit & Veg", "Other crops")) %>%
#     select(-AgCOMM5) %>%
#     group_by_at(vars(-value)) %>%
#     summarize(value = sum(value), .groups = "drop")
# }



AgSector_Clean <- function(.df){

  COMMLab <- c("Energy crops", "Staple crops", "Oil crops", "Fruit & Veg", "Fodder crops",
               "Other crops", "Livestock", "Pasture", "Forest")

  COMMLab <- c("Energy crops", "Grains", "Oil crops", "Sugar crops", "Fruit & Veg", "Fodder crops",
               "Other crops", "Livestock", "Pasture", "Forest")

  .df %>%
    mutate(sector = tolower(sector)) %>%
    left_join_error_no_match(
      MapAgCOMM %>% distinct(sector = tolower(AgCOMM), AgCOMM5),
      by = "sector") %>%
    select(-sector) %>%
    rename(sector = AgCOMM5) %>%
    group_by_at(vars(-value)) %>%
    summarize(value = sum(value), .groups = "drop") %>%
    mutate(sector = factor(sector, levels = COMMLab))
}



Year_Clean <- function(.df){
  .df %>%
    filter(year >= 2021)
  #filter(year %in% c(2010, 2015, 2021, seq(2030, 2100, 10)))
}
