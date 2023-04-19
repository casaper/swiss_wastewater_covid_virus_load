library(xts)
library(scales)
library(stringr)
library(knitr)
library(tidyverse)
library(shiny)
library(dplyr)

get_locality_data = function(ww_data, geoRegion) {
  return <- ww_data[ww_data$geoRegion == as.character(geoRegion), ]
}

render_locality_plot =
  function(locality_data, plot_main) {
    vl_percentile_mean7d_clean <-
      locality_data %>% filter(vl_percentile_mean7d != 'NA')
    vl_percentile_7d_clean <-
      locality_data %>% filter(vl_percentile_7d != 'NA')
    vl_percentile_mean7d_diff_pp_clean <-
      locality_data %>% filter(vl_percentile_mean7d_diff_pp != 'NA')
    
    vl_percentile_mean7d_ts <-
      xts(
        vl_percentile_mean7d_clean$vl_percentile_mean7d,
        order.by = vl_percentile_mean7d_clean$date,
        unique = TRUE,
        check = TRUE,
        descr = 'Rolling 7 day average'
      )
    plot(
      vl_percentile_mean7d_ts,
      col = 2,
      type = "l",
      main = plot_main,
      xlab = 'Date',
      ylab = 'Rolling 7 day average',
      lty = 1
    )
    
    vl_percentile_mean7d_diff_pp_ts <-
      xts(
        vl_percentile_mean7d_diff_pp_clean$vl_percentile_mean7d_diff_pp,
        order.by = vl_percentile_mean7d_diff_pp_clean$date,
        unique = TRUE,
        check = TRUE,
        descr = 'Percentage points difference'
      )
    lines(vl_percentile_mean7d_diff_pp_ts,
          col = 3,
          type = "l")
    
    vl_percentile_7d <-
      xts(
        vl_percentile_7d_clean$vl_percentile_7d,
        order.by = vl_percentile_7d_clean$date,
        unique = TRUE,
        check = TRUE,
        descr = 'Percentile value (0-100%)'
      )
    lines(vl_percentile_7d,
          col = 4,
          type = "l")
  }
