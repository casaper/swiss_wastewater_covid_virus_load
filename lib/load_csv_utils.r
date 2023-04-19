library(xml2)
library(rvest)
library(readr)
library(dplyr)
library(xts)
library(tidyverse)

date_col <- col_date(format = "%Y-%m-%d")
version_col <- col_datetime(format = "%Y-%m-%d_%H-%M-%S")
data_locale <- locale(date_names = "de")

get_csv_url <- function(websiteUrl) {
  links <-
    websiteUrl %>% read_html() %>% html_elements('a[data-format="CSV"]') %>% html_attr("href")
  return <- links[1]
}


load_COVID19Wastewater_vl <- function(csv_url) {
  return <-
    read_csv(
      csv_url,
      col_types = cols(
        geoRegion = col_character(),
        name = col_character(),
        pop = col_integer(),
        canton = col_character(),
        date = date_col,
        version = version_col,
        flow = col_double(),
        conc = col_double(),
        vl = col_double(),
        vl_percentile_7d = col_double(),
        vl_mean7d = col_double(),
        vl_percentile_mean7d = col_double(),
        vl_percentile_mean7d_diff_pp = col_double(),
        delay_7d = col_logical(),
        data_expected = col_logical()
      ),
      locale = data_locale
    )
}

get_localities <- function(COVID19Wastewater_vl) {
  localities <-
    COVID19Wastewater_vl %>% distinct(geoRegion, name, canton, pop)
  return <- localities[order(localities$pop, decreasing = TRUE),]
}

get_COVID19Cases_geoRegion_w <- function(website_url) {
  return <- read_csv(
    get_csv_url(website_url),
    col_types = cols(
      datum = col_character(),
      entries = col_integer(),
      timeframe_all = col_skip(),
      sumTotal = col_integer(),
      freq = col_skip(),
      prct = col_skip(),
      pop = col_integer(),
      entries_diff_inz = col_double(),
      inz_entries = col_double(),
      inzsumTotal = col_double(),
      type = col_skip(),
      type_variant = col_skip(),
      version = version_col,
      datum_unit = col_skip(),
      entries_diff_abs = col_integer(),
      entries_diff_inz = col_double(),
      entries_diff_pct = col_double(),
      sum2w = col_integer(),
      sum4w = col_integer(),
      mean2w = col_double(),
      mean4w = col_double(),
      sumTotal_last2w = col_integer(),
      sumTotal_last4w = col_integer(),
      inzmean2w = col_double(),
      inzmean4w = col_double(),
      inzsumTotal_last2w = col_double(),
      inzsumTotal_last4w = col_double(),
      inzsum2w = col_double(),
      inzsum4w = col_double(),
      entries_letzter_stand = col_integer(),
      entries_neu_gemeldet = col_integer(),
      entries_diff_last_age = col_integer(),
      entries_diff_last = col_integer(),
      timeframe_2w = col_logical(),
      timeframe_4w = col_logical()
    ),
    locale = data_locale
  )
}

get_COVID19Death_geoRegion_w <- function(website_url) {
  return <- read_csv(
    get_csv_url(website_url),
    col_types = cols(
      geoRegion = col_character(),
      datum = col_character(),
      entries = col_integer(),
      timeframe_all = col_logical(),
      sumTotal = col_integer(),
      freq = col_integer(),
      prct = col_integer(),
      pop = col_integer(),
      inz_entries = col_double(),
      inzsumTotal = col_double(),
      type = col_skip(),
      type_variant = col_skip(),
      version = col_character(),
      datum_unit = col_skip(),
      datum_dboardformated = col_character(),
      entries_diff_abs = col_integer(),
      entries_diff_inz = col_double(),
      entries_diff_pct = col_double(),
      prct_diff = col_skip(),
      timeframe_2w = col_logical(),
      timeframe_4w = col_logical(),
      sum2w = col_integer(),
      sum4w = col_integer(),
      mean2w = col_double(),
      mean4w = col_double(),
      sumTotal_last2w = col_skip(),
      sumTotal_last4w = col_skip(),
      inzmean2w = col_double(),
      inzmean4w = col_double(),
      inzsumTotal_last2w = col_skip(),
      inzsumTotal_last4w = col_skip(),
      inzsum2w  = col_double(),
      inzsum4w = col_double(),
      entries_letzter_stand = col_integer(),
      entries_neu_gemeldet = col_integer(),
      entries_diff_last_age = col_integer(),
      entries_diff_last = col_integer()
    ),
    locale = data_locale
  )
}

get_COVID19Cases_geoRegion_AKL10_w <- function(websit_url) {
  return <- read_csv(
    get_csv_url(websit_url),
    col_types = cols(
      altersklasse_covid19 = col_character(),
      geoRegion = col_character(),
      datum = col_double(),
      entries = col_integer(),
      timeframe_all = col_logical(),
      sumTotal = col_integer(),
      freq = col_double(),
      prct = col_double(),
      pop = col_integer(),
      inz_entries = col_double(),
      inzsumTotal = col_double(),
      type = col_character(),
      type_variant = col_character(),
      version = version_col,
      datum_unit = col_character(),
      datum_dboardformated = col_character(),
      entries_diff_abs = col_integer(),
      entries_diff_inz = col_double(),
      entries_diff_pct = col_double(),
      prct_diff = col_double()
    ),
    locale = data_locale
  )
}