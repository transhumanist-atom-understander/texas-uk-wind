library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
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

# UK onshore and offshore wind generation, from DESNZ's Digest of UK Energy
# Statistics (DUKES) table 6.2, "Capacity of, generation from renewable
# sources and shares of total generation", 1990 onward:
# https://www.gov.uk/government/statistics/renewable-sources-of-energy-chapter-6-digest-of-united-kingdom-energy-statistics-dukes
# Our World in Data doesn't split wind into onshore and offshore. DUKES total
# wind is within 1% of Our World in Data's (86.4 vs 85.8 TWh in 2025)
dukes_6_2_url <- 'https://assets.publishing.service.gov.uk/media/6a6a27620c36759b5ccaa1d1/DUKES_6.2.xlsx'
dukes_6_2 <- file.path(data_dir, basename(dukes_6_2_url))
if (!file.exists(dukes_6_2))
    download.file(dukes_6_2_url, dukes_6_2, mode = 'wb')

#' Read UK onshore and offshore wind generation, and total electricity
#' generation, from DUKES table 6.2
#'
#' FORMAT ASSUMPTIONS
#' - Worksheet is named '6.2'
#' - Three tables stacked vertically (capacity, generation, shares), each
#'   starting with a header row of years, with the table name in column 1,
#'   and separated by a blank row
#' - The generation table's header starts with 'Generation (GWh)', and its
#'   first 'Onshore' and 'Offshore' rows are the wind rows
#' - The shares table's header starts with 'Shares of total electricity
#'   generation', and it has a row starting with 'Total electricity
#'   generation (GWh)'
#'
#' @param path  Path to the Excel workbook (.xlsx)
#' @return tibble with columns: year, type ('onshore', 'offshore', or
#'   'all_sources' for total electricity generation), gwh_annual
read_dukes_wind <- function(path)
{
    df <- readxl::read_excel(path, sheet = '6.2', col_names = FALSE,
                             col_types = 'text', .name_repair = 'minimal')
    labels <- df[[1]]

    # Row numbers of the table whose header starts with header_prefix,
    # header row first. A table ends at the first blank row after its header
    # (or the end of the sheet)
    table_rows <- function(header_prefix)
    {
        header_row <- which(str_starts(labels, fixed(header_prefix)))
        stopifnot(length(header_row) == 1)
        blank <- which(is.na(labels[-(1:header_row)]))
        end_row <- if (length(blank) > 0) header_row + blank[1] - 1
                   else length(labels)
        header_row:end_row
    }

    # Annual values from the first row in rows whose label starts with label
    read_row <- function(rows, label)
    {
        years <- as.integer(unlist(df[rows[1], -1]))
        stopifnot(!anyNA(years))
        row <- rows[-1][str_starts(labels[rows[-1]], fixed(label))][1]
        stopifnot(!is.na(row))
        tibble(year = years,
               gwh_annual = readr::parse_number(unlist(df[row, -1])))
    }

    generation <- table_rows('Generation (GWh)')
    shares <- table_rows('Shares of total electricity generation')
    bind_rows(onshore = read_row(generation, 'Onshore'),
              offshore = read_row(generation, 'Offshore'),
              all_sources = read_row(shares,
                                     'Total electricity generation (GWh)'),
              .id = 'type')
}

dukes_generation <- read_dukes_wind(dukes_6_2)
uk_wind_by_type <- dukes_generation |>
    filter(type %in% c('onshore', 'offshore')) |>
    filter(year >= 2007, year <= 2025) |>
    mutate(gw = gwh_annual * gwh2gw) |>
    select(year, type, gw)
stopifnot(nrow(uk_wind_by_type) == 2 * length(2007:2025),
          !anyNA(uk_wind_by_type$gw))

# Onshore and offshore as percentages of UK wind generation, and of UK total
# electricity generation, all from DUKES so that they're consistent
uk_dukes_total <- dukes_generation |>
    filter(type == 'all_sources') |>
    select(year, gw_all_sources = gwh_annual) |>
    mutate(gw_all_sources = gw_all_sources * gwh2gw)
uk_wind_type_shares <- uk_wind_by_type |>
    group_by(year) |>
    mutate(pct_of_wind = 100 * gw / sum(gw)) |>
    ungroup() |>
    inner_join(uk_dukes_total, by = 'year') |>
    mutate(pct_of_all_generation = 100 * gw / gw_all_sources)
stopifnot(nrow(uk_wind_type_shares) == 2 * length(2007:2025))

uk_onshore_wind <- uk_wind_by_type |>
    filter(type == 'onshore') |>
    select(year, gw)

# Population, from FRED (https://fred.stlouisfed.org), both through 2025:
# - Texas: TXPOP, US Census Bureau resident population, in thousands
# - UK: POPTOTGBA647NWDB, World Bank total population
# Our World in Data's population series currently stops at 2023, so it isn't
# used here

#' Read an annual FRED series, downloading it to data_dir if needed
#'
#' @param series_id FRED series ID
#' @return tibble with columns: year, value
read_fred_annual <- function(series_id)
{
    path <- file.path(data_dir, paste0(series_id, '.csv'))
    if (!file.exists(path))
        download.file(paste0('https://fred.stlouisfed.org/graph/fredgraph.csv?id=',
                             series_id),
                      path, mode = 'wb')
    readr::read_csv(path, col_types = 'Dd') |>
        select(date = observation_date, value = all_of(series_id)) |>
        mutate(year = as.integer(format(date, '%Y'))) |>
        select(year, value)
}

texas_population <- read_fred_annual('TXPOP') |>
    mutate(population = value * 1000) |>
    select(year, population)
uk_population <- read_fred_annual('POPTOTGBA647NWDB') |>
    rename(population = value)

combined_population <- bind_rows(
    texas = texas_population,
    uk = uk_population,
    .id = 'place') |>
    filter(year >= 2007, year <= 2025)
stopifnot(nrow(combined_population) == 2 * length(2007:2025))

# Land area, excluding inland water, in square kilometres
# - Texas: US Census Bureau 2025 Gazetteer, ALAND (square metres)
#   https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html
# - UK: ONS Standard Area Measurements for the Countries (December 2024),
#   AREALHECT (hectares), summed over the four countries
#   https://www.data.gov.uk/dataset/standard-area-measurements-for-the-countries-december-2024-in-the-uk
census_gazetteer_url <- 'https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2025_Gazetteer/2025_Gaz_state_national.zip'
census_gazetteer <- file.path(data_dir, basename(census_gazetteer_url))
if (!file.exists(census_gazetteer))
    download.file(census_gazetteer_url, census_gazetteer, mode = 'wb')
ons_area_url <- 'https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/6472b111caef42d793fe9d263643dcfb/csv?layers=0'
ons_area <- file.path(data_dir, 'uk_standard_area_measurements_2024.csv')
if (!file.exists(ons_area))
    download.file(ons_area_url, ons_area, mode = 'wb')

texas_land_km2 <- readr::read_delim(
        unz(census_gazetteer, '2025_Gaz_state_national.txt'),
        delim = '|', col_types = readr::cols(.default = 'c')) |>
    filter(USPS == 'TX') |>
    pull(ALAND) |>
    as.numeric() / 1e6
uk_land_areas <- readr::read_csv(ons_area, col_types = readr::cols())
stopifnot(nrow(uk_land_areas) == 4)
uk_land_km2 <- sum(uk_land_areas$AREALHECT) / 100
stopifnot(length(texas_land_km2) == 1, !is.na(texas_land_km2),
          !is.na(uk_land_km2))
land_area <- tibble(place = c('texas', 'uk'),
                    land_km2 = c(texas_land_km2, uk_land_km2))

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

# Per capita generation, as average watts per person
gw2w <- 1e9
combined_per_capita <- inner_join(combined_share, combined_population,
                                  by = c('place', 'year')) |>
    mutate(wind_w_per_capita = gw_wind * gw2w / population,
           total_w_per_capita = gw_total * gw2w / population)

# Onshore wind. All Texas wind is onshore
combined_onshore_wind <- bind_rows(
    texas = texas_wind_generation,
    uk = uk_onshore_wind,
    .id = 'place')

# Power density, as average milliwatts per square metre of land. Wind density
# uses onshore wind only
gw2mw <- 1e12
km2_to_m2 <- 1e6
combined_density <- combined_onshore_wind |>
    rename(gw_onshore_wind = gw) |>
    inner_join(combined_total |> rename(gw_total = gw),
               by = c('place', 'year')) |>
    inner_join(land_area, by = 'place') |>
    mutate(onshore_wind_mw_per_m2 =
               gw_onshore_wind * gw2mw / (land_km2 * km2_to_m2),
           total_mw_per_m2 = gw_total * gw2mw / (land_km2 * km2_to_m2))
stopifnot(nrow(combined_density) == 2 * length(2007:2025))

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

population_plot <- place_plot(combined_population |>
                                  mutate(population_m = population / 1e6),
                              'population_m', 'Population (millions)',
                              'Population')
ggsave('population.png', population_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

wind_per_capita_plot <- place_plot(combined_per_capita, 'wind_w_per_capita',
                                   'Average W per person',
                                   'Wind generation per capita')
ggsave('wind_generation_per_capita.png', wind_per_capita_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

total_per_capita_plot <- place_plot(combined_per_capita, 'total_w_per_capita',
                                    'Average W per person',
                                    'Total generation per capita')
ggsave('total_generation_per_capita.png', total_per_capita_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

#' Line plot of a yearly value for UK onshore and offshore wind
#'
#' @param data  tibble with columns type, year, and the y variable
#' @param y     Name of the column to plot on the y axis
#' @param ylab  Y axis label
#' @param title Plot title
#' @param ylim  Y axis limits
#' @return ggplot object
wind_type_plot <- function(data, y, ylab, title, ylim = c(0, NA))
{
    ggplot(data, aes(x = year, y = .data[[y]], color = type)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(name = 'Year') +
        scale_y_continuous(name = ylab, limits = ylim) +
        scale_color_discrete(name = NULL,
                             breaks = c('onshore', 'offshore'),
                             labels = c('onshore' = 'Onshore',
                                        'offshore' = 'Offshore')) +
        theme(legend.position = 'bottom') +
        ggtitle(title)
}

uk_wind_type_plot <- wind_type_plot(uk_wind_by_type, 'gw',
                                    'Average generation (GW)',
                                    'UK wind generation')
ggsave('uk_onshore_offshore_wind.png', uk_wind_type_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

uk_wind_type_share_plot <- wind_type_plot(uk_wind_type_shares, 'pct_of_wind',
                                          'Share of wind generation (%)',
                                          'UK wind by type',
                                          ylim = c(0, 100))
ggsave('uk_onshore_offshore_share_of_wind.png', uk_wind_type_share_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

uk_wind_type_total_share_plot <- wind_type_plot(
    uk_wind_type_shares, 'pct_of_all_generation',
    'Share of all generation (%)', 'UK wind share by type')
ggsave('uk_onshore_offshore_share_of_generation.png',
       uk_wind_type_total_share_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

wind_density_plot <- place_plot(combined_density, 'onshore_wind_mw_per_m2',
                                'Average mW per square metre',
                                'Onshore wind power density')
ggsave('onshore_wind_density.png', wind_density_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

total_density_plot <- place_plot(combined_density, 'total_mw_per_m2',
                                 'Average mW per square metre',
                                 'Total generation density')
ggsave('total_generation_density.png', total_density_plot,
       width = unit(4, 'in'), height = unit(4, 'in'))

# 2025 values for all plots, one row per metric with a column per place,
# plus the ratios between places in both directions
values_2025 <- combined_per_capita |>
    filter(year == 2025) |>
    transmute(place,
              wind_gw = gw_wind,
              total_gw = gw_total,
              wind_share_pct = share_pct,
              population,
              wind_w_per_capita,
              total_w_per_capita) |>
    inner_join(combined_density |>
                   filter(year == 2025) |>
                   select(place, onshore_wind_gw = gw_onshore_wind, land_km2,
                          onshore_wind_mw_per_m2, total_mw_per_m2),
               by = 'place') |>
    left_join(uk_wind_by_type |>
                  filter(year == 2025, type == 'offshore') |>
                  transmute(place = 'uk', offshore_wind_gw = gw),
              by = 'place') |>
    # Texas has no offshore wind
    mutate(offshore_wind_gw = coalesce(offshore_wind_gw, 0),
           onshore_pct_of_wind = 100 * onshore_wind_gw /
               (onshore_wind_gw + offshore_wind_gw),
           offshore_pct_of_wind = 100 * offshore_wind_gw /
               (onshore_wind_gw + offshore_wind_gw)) |>
    pivot_longer(-place, names_to = 'metric') |>
    pivot_wider(names_from = place, values_from = value) |>
    mutate(texas_to_uk_ratio = texas / uk,
           uk_to_texas_ratio = uk / texas)
stopifnot(nrow(values_2025) == 13,
          identical(names(values_2025),
                    c('metric', 'texas', 'uk',
                      'texas_to_uk_ratio', 'uk_to_texas_ratio')))
write_csv(values_2025, 'values_2025.csv')
