utils::globalVariables(c("en", "id", "description", "label.en", "."))

#' Get the data structure of a dataset.
#'
#' Returns a list of data frames containing names and descriptions of the variables
#' of a specified series.
#'
#' @param dataset A string containing the code for a dataset

#' @export
#' @import methods
get_structure <- function(dataset) {
  
  dataset <- gsub(",", "/", dataset)
  
  url <- paste0(
    "https://sdmx.oecd.org/public/rest/dataflow/", dataset, "?references=all"
  )
  data_structure <- rsdmx::readSDMX(url)
  
  # First data frame in returned list: data frame with full variables names
  variable_desc <- suppressWarnings(
    rsdmx:::as.data.frame.SDMXConcepts(data_structure@concepts)
  )
  
  # Clean up names (keeping only id and English description)
  variable_desc[] <- lapply(variable_desc, as.character)
  
  names(variable_desc)[length(names(variable_desc))] <- "description"
  variable_desc <- variable_desc[, c("id", "description")]
  
  # List of data frames in returned list: descriptions of factor levels
  code_names <- data_structure@codelists@codelists
  code_names <- vapply(code_names, function(x) x@id, "character")
  
  code_list <- lapply(code_names, function(x) {
    df <- as.data.frame(data_structure@codelists, codelistId = x)
    try(
      {
        df <- df[, c("id", "label.en")]
        names(df)[2] <- "label"
      },
      silent = TRUE
    )
    df
  })
  
  names(code_list) <- gsub(paste0("CL_", dataset, "_"), "", code_names)
  
  full_df_list <- c("VAR_DESC" = list(variable_desc), code_list)
  full_df_list
}

#' Download OECD data sets.
#'
#' Returns a data frame with the requested data, downloaded through the OECD's API.
#'
#' @param dataset A string with the code for the desired data set
#'
#' @param filter A character vectors specifying the filter to be applied to
#' each dimension of the dataset (see \code{examples} below).
#'
#' @param start_time Starting time/period for data. If left blank, no time filter is
#' applied (i.e. all observations since the earliest available observation are
#' downloaded).
#'
#' @param end_time End time/period for data.
#'
#' @param last_n_observations Number of most recent observations to download.
#'
#' @param ... Additional parameters passed to \code{read_sdmx}
#'
#' @return A data frame
#'
#' @examples
#' # Get entire dataset
#' \dontrun{
#' df <- get_dataset("OECD.SDD.NAD.SEEA/DSD_NAT_RES@DF_NAT_RES,1.0", "AUS+CAN.A....")
#' }
#' \dontrun{
#' head(df, 10)
#' }
#'
#' @export
get_data <- function(dataset, filter = NULL, start_time = NULL, end_time = NULL,
                        last_n_observations = NULL, ...) {
  
  dataset <- gsub("/", ",", dataset)
  
  path <- paste0(
    "/public/rest/data/", dataset, "/", filter
  )
  
  url_list <- list(
    "scheme" = "https",
    "hostname" = "sdmx.oecd.org",
    "path" = path,
    "query" = list(
      "startPeriod" = start_time,
      "endPeriod" = end_time,
      "lastNObservations" = last_n_observations,
      "dimensionAtObservation" = "AllDimensions"
    )
  )
  class(url_list) <- "url"
  
  url <- httr::build_url(url_list)
  
  df <- readsdmx::read_sdmx(url, ...)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

wap <- get_data("OECD.SDD.TPS,DSD_LFS@DF_IALFS_INDIC,1.0",".WAP...Y.M+F+_T.Y_GE15..A") %>%
  subset(select = c(REF_AREA, TIME_PERIOD, ObsValue,SEX)) %>%
  transform(
    ObsValue = as.numeric(ObsValue),
    TIME_PERIOD = as.numeric(TIME_PERIOD)
  ) %>%
  rename(country = REF_AREA,
         year = TIME_PERIOD,
         pop = ObsValue,
         sex = SEX)

#Measure: Employment rate
#Unit of measure: Percentage of working age population in the same subgroup
emp = get_data("OECD.SDD.TPS,DSD_LFS@DF_IALFS_INDIC,1.0",".EMP_WAP...Y.M+F+_T.Y_GE15..A")
emp <- emp %>% subset(select = c(REF_AREA, TIME_PERIOD, ObsValue, SEX)) %>%
  transform( ObsValue = as.numeric(ObsValue),
             TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
  rename(country = REF_AREA,
         year = TIME_PERIOD,
         emp = ObsValue,
         sex = SEX)


# Tính toán chỉ số gradpc
gradpc <- emp %>%
  left_join(wap, by = c("year", "country", "sex")) %>%
  filter(!is.nan(emp) & !is.nan(pop) & nchar(country) == 3)

gdp = get_data("OECD.SDD.NAD,DSD_NAAG@DF_NAAG_I,1.0","A.CHN+OECD+EU+EA+ZAF+RUS+IDN+IND+BRA+AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.B1GQ.USD_PPP.")
head(gdp)
gdp <- gdp %>% subset(select = c(REF_AREA, TIME_PERIOD, ObsValue)) %>%
  transform( ObsValue = as.numeric(ObsValue),
             TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
  rename(country = REF_AREA,
         year = TIME_PERIOD,
         gdpAbb = ObsValue)

gdp_growth = get_data("OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0","A..ZAF+SAU+RUS+IDN+IND+CHN+BRA+ARG+AUS+AUT+BEL+CAN+CHE+CHL+COL+CRI+CZE+DEU+DNK+ESP+FIN+EST+FRA+GBR+GRC+HUN+ISL+LTU+ISR+ITA+JPN+KOR+LUX+LVA+MEX+NLD+NOR+NZL+POL+PRT+SVK+SVN+SWE+TUR+USA+OECD+G20+G7+NAFTA+OECDE+EA20+EU27_2020...B1GQ......G1.")
head(gdp_growth)
gdp_growth <- gdp_growth %>% subset(select = c(REF_AREA, TIME_PERIOD, ObsValue)) %>%
  transform( ObsValue = as.numeric(ObsValue),
             TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
  rename(country = REF_AREA,
         year = TIME_PERIOD,
         gdpGrow = ObsValue)

# Tính tổng gdpAbb theo từng năm
total_gdp_year <- gdp %>%
  group_by(year) %>%
  summarise(total_gdp_year = sum(gdpAbb))

# Thêm cột shareGdp
gdp <- gdp %>%
  left_join(total_gdp_year, by = "year") %>%
  mutate(shareGdp = round(100 * gdpAbb / total_gdp_year, 1))


pop = get_data("OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE3_POP_EMPNC,1.0","A.ZMB+ZAF+SGP+SRB+SEN+SAU+RUS+ROU+MKD+MAR+MLT+MDG+IDN+IND+HKG+GEO+CYP+HRV+CHN+CMR+CPV+BGR+BRA+ARG+ALB+AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...POP.......")
head(pop)
pop <- pop %>% subset(select = c(REF_AREA, TIME_PERIOD, ObsValue)) %>%
  transform( ObsValue = as.numeric(ObsValue),
             TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
  rename(country = REF_AREA,
         year = TIME_PERIOD,
         pop = ObsValue)


# Lưu dữ liệu vào file RData
save(emp, wap, gradpc, gdp, file = "data.RData")
save(gdp,gdp_growth,pop, file ='data2.RData')


