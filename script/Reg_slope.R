###
# DEM correction Rhone river
###

## Set directory
setwd('/Bureau/Fabio/')

# Libraries
library(sf)
library(mapview)
library(plotly)
library(ggrepel)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(forestmangr)
library(zoo)

# import data
df <- read.csv('Rhone/slope_reaches.csv')
df$elev_m <- na.approx(df$reg)
df$elev_m1 <- na.approx(df$reg1)

plot_ly() %>%
  add_lines(data = df, ~dist_acc_km, ~elev_m) %>%
  add_lines(data = df, ~dist_acc_km, ~elev_m1) %>%
  add_lines(data = df, ~dist_acc_km, ~elevation_m)

## interpolation
# create data frame
df_int <- data.frame(meters = 0:525262)
df$meters <- round(df$dist_acc_m, 0)

df_reg <- left_join(df_int, df[,c("fid","reg1", "meters")], by = 'meters')
df_reg[!is.na(df_reg$fid),]
df_reg$elevation <- na.approx(df_reg$reg1)

plot_ly() %>%
  add_lines(data = df, ~meters, ~elevation_m) %>%
  add_lines(data = df, ~meters, ~elev_m) %>%
  add_lines(data = df, ~meters, ~elev_m1) %>%
  add_lines(data = df_reg, ~meters, ~elevation)

df_done <- left_join(df, df_reg[,c("fid","elevation")], by = 'fid')
df_done <- df_done[,c("fid","meters","elevation")]
df_done$diff_meters <- c(0,diff(df_done$meters))
df_done$diff_elev <- abs(c(0,diff(df_done$elevation)))
df_done$slope <- df_done$diff_elev/df_done$diff_meters

subplot(
  plot_ly() %>%
    add_lines(data = df_done, ~meters, ~elevation, name = 'Smooth elevation (m)') %>%
    add_lines(data = df, ~meters, ~elevation_m, name = 'Elevation with dams (m)') %>%
    layout(title = 'Dem smooth Rhone river', yaxis = list(title = 'Altitude (m)'), xaxis = list(title = 'Distance (m)')),
  
  plot_ly() %>%
    add_lines(data = df_done, ~meters, ~slope, name = 'Slope (m/m)') %>%
    layout(yaxis = list(title = 'Slope (m/m)'), xaxis = list(title = 'Distance (m)')),
  
  nrows = 2, shareX = T, shareY = F, titleX = T, titleY = T
)

# write.csv(df_done[,c("fid","meters","elevation","slope")], 'Rhone/df_rhone_elev.csv', row.names = F)
