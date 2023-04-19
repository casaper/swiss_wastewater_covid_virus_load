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
      locality_data %>% dplyr::filter(vl_percentile_mean7d != 'NA')
    vl_percentile_mean7d_diff_pp_clean <-
      locality_data %>% dplyr::filter(vl_percentile_mean7d_diff_pp != 'NA')
    
    xmin <-
      min(c(
        min(vl_percentile_mean7d_clean$date),
        min(vl_percentile_mean7d_diff_pp_clean$date)
      ))
    xdiff <-
      max(c(
        max(vl_percentile_mean7d_clean$date),
        max(vl_percentile_mean7d_diff_pp_clean$date)
      )) - xmin
    
    plot(
      vl_percentile_mean7d_clean$date,
      vl_percentile_mean7d_clean$vl_percentile_mean7d,
      col = 'red',
      type = "l",
      main = plot_main,
      cex.main = 1,
      # magnification of main label
      xlab = 'date',
      ylab = 'Percentile',
      cex.lab = 0.7,
      # magnification of axis labels
      ann = TRUE,
      # show annotations
      frame.plot = TRUE,
      # xpd = TRUE,
      axes = TRUE,
      adj = 1
    )
    grid(
      nx = NA,
      ny = NULL,
      lty = 2,
      col = "gray",
      lwd = 2
    )
    par(xpd = NA)
    
    lines(
      vl_percentile_mean7d_diff_pp_clean$date,
      vl_percentile_mean7d_diff_pp_clean$vl_percentile_mean7d_diff_pp,
      col = 'green',
      type = "l"
    )
    
    legend(
      as.integer(xmin - (xdiff * 0.038)),
      120,
      legend = c('Rolling 7 day average',
                 'Percentage points difference'),
      col = c('red', 'green'),
      lty = 1,
      bg = 'transparent',
      cex = 0.7
    )
    
    
    conc_clean <-
      locality_data %>% dplyr::filter(conc != 'NA')
    flow_clean <-
      locality_data %>% dplyr::filter(flow != 'NA')
    
    ymax <- max(c(max(conc_clean$conc), max(flow_clean$flow)))
    xmin <- min(c(min(conc_clean$date), min(flow_clean$date)))
    xdiff <-
      max(c(max(conc_clean$date), max(flow_clean$date))) - xmin
    
    #     desc = 'Measured concentration in the sample in gene copies per cubic meter'
    plot(
      conc_clean$date,
      conc_clean$conc,
      col = 'blue',
      type = "l",
      xlab = 'date',
      ylab = 'conc',
      cex.lab = 0.7,
      # main = 'Concentration',
      cex.main = 0.8,
      frame.plot = TRUE,
      ann = FALSE,
      # show annotations,
      adj = 1
    )
    grid(
      nx = NA,
      ny = NULL,
      lty = 2,
      col = "gray",
      lwd = 2
    )
    par(xpd = NA)
    
    lines(flow_clean$date,
          flow_clean$flow,
          col = 'magenta',
          type = 'l')
    legend(
      as.integer(xmin - (xdiff * 0.038)),
      ymax + (ymax * 0.20),
      legend = c(
        'Concentration of gene copies per m^3',
        'Days water flow rate in m^3'
      ),
      col = c('blue', 'magenta'),
      lty = 1,
      bg = 'transparent',
      cex = 0.7
    )
  }
