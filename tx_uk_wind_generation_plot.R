library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(readxl)
library(cowplot)
library(owidapi)
theme_set(theme_cowplot() +
          theme(plot.background = element_rect(fill = 'white')))

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

# Downloaded data is saved in 'data' and reused on later runs; delete the
# folder (or individual files) to download fresh copies
data_dir <- 'data'
dir.create(data_dir, showWarnings = FALSE)
# The ERCOT zip is ~70 MB; the default 60 s timeout can be too short
options(timeout = max(600, getOption('timeout')))

# ERCOT fuel mix reports, from https://www.ercot.com/gridinfo/generation
# "Fuel Mix Report: 2007 - 2024" is a zip of one workbook per year
ercot_url <- 'https://www.ercot.com/files/docs/2021/03/10/FuelMixReport_PreviousYears.zip'
ercot_zip <- file.path(data_dir, basename(ercot_url))
if (!file.exists(ercot_zip)) {
    download.file(ercot_url, ercot_zip, mode = 'wb')
    unzip(ercot_zip, exdir = data_dir)
}
# 2025 is published as its own file, "Fuel Mix Report: 2025"
ercot_2025_url <- 'https://www.ercot.com/files/docs/2025/02/07/IntGenbyFuel2025.xlsx'
ercot_2025 <- file.path(data_dir, basename(ercot_2025_url))
if (!file.exists(ercot_2025))
    download.file(ercot_2025_url, ercot_2025, mode = 'wb')
infiles <- tibble(
        filename = c(file.path(data_dir, unzip(ercot_zip, list = TRUE)$Name),
                     ercot_2025),
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
new_format_years <- tibble(year = 2017:2025)
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
stopifnot(all(2007:2025 %in% texas_wind_generation$year))

# Total generation is the sum over fuels, excluding the workbooks' own total
# row. The 2007 workbook has no total row; in other years the sum matches it.
# This includes 'WSL' (wholesale storage load), which is negative
texas_total_generation <- texas_generation |>
    filter(toupper(fuel) != 'TOTAL') |>
    group_by(year) |>
    summarise(gw = sum(gw))
stopifnot(all(2007:2025 %in% texas_total_generation$year))

# UK wind generation from Our World in Data:
# https://ourworldindata.org/grapher/wind-generation

# Converting terawatt hours per year to gigawatts
# Mathematica:
# UnitConvert[Quantity[1, "Hours" "Terawatts"]*Quantity[1, (1/("Years"))], "GW"]
twh2gw <- 25/219

uk_file <- file.path(data_dir, 'uk_wind_generation.rds')
if (!file.exists(uk_file))
    saveRDS(owid_get('wind-generation', entities = 'GBR', start_date = 2007),
            uk_file)
uk_wind <- readRDS(uk_file) |>
    select(year, twh_annual = wind_generation__twh) |>
    mutate(gw = twh_annual * twh2gw) |>
    select(year, gw)

# UK total electricity generation from Our World in Data:
# https://ourworldindata.org/grapher/electricity-generation
uk_total_file <- file.path(data_dir, 'uk_total_generation.rds')
if (!file.exists(uk_total_file))
    saveRDS(owid_get('electricity-generation', entities = 'GBR',
                     start_date = 2007),
            uk_total_file)
uk_total <- readRDS(uk_total_file) |>
    select(year, twh_annual = total_generation__twh) |>
    mutate(gw = twh_annual * twh2gw) |>
    select(year, gw)

combined_wind <- bind_rows(
    texas = texas_wind_generation,
    uk = uk_wind,
    .id = 'place')

combined_total <- bind_rows(
    texas = texas_total_generation,
    uk = uk_total,
    .id = 'place')

# Wind share of total generation, as a percentage
combined_share <- inner_join(combined_wind, combined_total,
                             by = c('place', 'year'),
                             suffix = c('_wind', '_total')) |>
    mutate(share_pct = 100 * gw_wind / gw_total)

#' Line plot of a yearly value for Texas and the UK
#'
#' @param data  tibble with columns place, year, and the y variable
#' @param y     Name of the column to plot on the y axis
#' @param ylab  Y axis label
#' @param title Plot title
#' @return ggplot object
place_plot <- function(data, y, ylab, title)
{
    ggplot(data, aes(x = year, y = .data[[y]], color = place)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(name = 'Year') +
        scale_y_continuous(name = ylab, limits = c(0, NA)) +
        # Default color scale, but remove the label from the guide, and
        # capitalize properly
        scale_color_discrete(name = NULL,
                             labels = c('texas' = 'Texas', 'uk' = 'UK')) +
        theme(legend.position = 'bottom') +
        ggtitle(title)
}

wind_plot <- place_plot(combined_wind, 'gw', 'Average generation (GW)',
                        'Wind power generation')
ggsave('wind_generation.png', wind_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

share_plot <- place_plot(combined_share, 'share_pct',
                         'Share of generation (%)',
                         'Wind share of electricity')
ggsave('wind_share.png', share_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

total_plot <- place_plot(combined_total, 'gw', 'Average generation (GW)',
                         'Total electricity generation')
ggsave('total_generation.png', total_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

# 2025 values for all three plots
values_2025 <- combined_share |>
    filter(year == 2025) |>
    transmute(place = c('texas' = 'Texas', 'uk' = 'UK')[place],
              year,
              wind_gw = gw_wind,
              total_gw = gw_total,
              wind_share_pct = share_pct)
stopifnot(nrow(values_2025) == 2)
write_csv(values_2025, 'values_2025.csv')
