library(tidyverse)
library(readxl)
library(cowplot)
library(owidapi)
theme_set(theme_cowplot())

#' Read annual ERCOT generation by fuel, in the older format used 2007 to 2016
#'
#' FORMAT ASSUMPTIONS
#' - Worksheet is named exactly the year, e.g. "2008".
#' - Unless the year is 2015, then it's "2015 Summary"
#' - Columns include:
#'     'Fuel Totals' | 'YYYY MWH' | Jan | Feb | ... | Dec
#' - Or (in 2007) the second column is named just "MWH"
#' - This second column contains annual totals by fuel
#'
#' @param path  Path to the Excel workbook (.xlsx)
#' @param year  Integer year (e.g., 2008). Used for sheet selection
#' @return tibble with columns: fuel, mwh_annual
read_ercot_old <- function(path, year)
{
    stopifnot(length(year) == 1, is.numeric(year), !is.na(year))
    sheet_name <- ifelse(year == 2015, '2015 Summary', as.character(year))
    # Read the year sheet with headers
    # Just use character vectors throughout; I'll handle type conversion later
    df <- readxl::read_excel(path, sheet = sheet_name, col_names = TRUE,
                             col_types = 'text')
  
    annual_regex <- 'MWH'
    annual_col <- names(df)[str_detect(names(df), annual_regex)]
  
    # Select only the fuel and year columns
    out <- df |>
        select(fuel = `Fuel Totals`, mwh_annual = all_of(annual_col)) |>
        # Drop blank/NA fuel rows
        filter(!is.na(fuel) & nzchar(fuel)) |>
        # Parse numbers in case they're formatted as text with commas
        mutate(mwh_annual = readr::parse_number(mwh_annual),
               year = as.integer(year)) |>
        select(fuel, mwh_annual)
  
    return(out)
}

#' Read annual ERCOT generation by fuel, format used 2017 and later
#'
#' FORMAT ASSUMPTIONS
#' - Worksheet is named 'Summary'
#' - Header row begins after a fixed number of lines; use skip = 7
#' - First column header is 'Energy, GWh'
#' - There is a 'Total' column with the annual GWh by fuel
#' - We ignore monthly columns and return only annual totals
#'
#' @param path  Path to the Excel workbook (.xlsx)
#' @return tibble with columns: fuel, gwh_annual
read_ercot_new <- function(path)
{
    df <- readxl::read_excel(path,
                             sheet = 'Summary',
                             col_names = TRUE,
                             col_types = 'text',
                             skip = 7)

    out <- df |>
        select(fuel = `Energy, GWh`, gwh_annual = Total) |>
        filter(!is.na(fuel) & nzchar(fuel)) |>
        mutate(gwh_annual = readr::parse_number(gwh_annual)) |>
        filter(!is.na(gwh_annual)) |>
        select(fuel, gwh_annual)

    return(out)
}

# ERCOT fuel mix reports, from "Fuel Mix Report: 2007 - 2024" at
# https://www.ercot.com/gridinfo/generation
ercot_url <- 'https://www.ercot.com/files/docs/2021/03/10/FuelMixReport_PreviousYears.zip'
ercot_dir <- file.path(tempdir(), 'ercot')
ercot_zip <- file.path(ercot_dir, basename(ercot_url))
dir.create(ercot_dir, showWarnings = FALSE)
# The zip is ~70 MB; the default 60 s timeout can be too short
options(timeout = max(600, getOption('timeout')))
download.file(ercot_url, ercot_zip, mode = 'wb')
infiles <- tibble(filename = unzip(ercot_zip, exdir = ercot_dir),
        year = as.integer(str_extract(basename(filename),
                                      '^IntGen[Bb]yFuel(\\d+)\\.xls',
                                      group = 1)))

# One megawatt hour per year in gigawatts
# Mathematica:
# UnitConvert[Quantity[1, ("Hours"*"Megawatts")/"Years"], "GW"]
mwh2gw <- 1/8760000
old_format_years <- tibble(year = 2007:2016)
old_format_values <- old_format_years |>
    left_join(infiles, by = 'year') |>
    group_by(year) |>
    reframe(read_ercot_old(filename, year)) |>
    mutate(gw = mwh_annual * mwh2gw) |>
    select(year, fuel, gw)

# One gigawatt hour per year in gigawatts
# Mathematica:
# UnitConvert[Quantity[1, ("Hours"*"Gigawatts")/"Years"], "GW"]
gwh2gw <- 1/8760
new_format_years <- tibble(year = 2017:2024)
new_format_values <- new_format_years |>
    left_join(infiles, by = 'year') |>
    group_by(year) |>
    reframe(read_ercot_new(filename)) |>
    mutate(gw = gwh_annual * gwh2gw) |>
    select(year, fuel, gw)

texas_generation <- bind_rows(old_format_values, new_format_values) |>
    arrange(year, fuel)

texas_wind_generation <- texas_generation |>
    filter(fuel == 'Wind' | fuel == 'Wnd') |>
    select(year, gw)
# Check that all years are present
stopifnot(all(2007:2024 %in% texas_wind_generation$year))

# UK wind generation from Our World in Data:
# https://ourworldindata.org/grapher/wind-generation

# Converting terawatt hours per year to gigawatts
# Mathematica:
# UnitConvert[Quantity[1, "Hours" "Terawatts"]*Quantity[1, (1/("Years"))], "GW"]
twh2gw <- 25/219

uk_wind <- owid_get('wind-generation', entities = 'GBR', start_date = 2007) |>
    select(year, twh_annual = wind_generation__twh) |>
    mutate(gw = twh_annual * twh2gw) |>
    select(year, gw)

combined_wind <- bind_rows(
    texas = texas_wind_generation,
    uk = uk_wind,
    .id = 'place')

wind_plot <- ggplot(combined_wind,
                    aes(x = year, y = gw, color = place)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(name = 'Year') +
    scale_y_continuous(name = 'Average generation (GW)', limits = c(0, NA)) +
    # Default color scale, but remove the label from the guide, and capitalize
    # properly
    scale_color_discrete(name = NULL,
                         labels = c('texas' = 'Texas', 'uk' = 'UK')) +
    theme(legend.position = 'bottom') +
    ggtitle('Wind power generation')
ggsave('wind_generation.png', wind_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))
