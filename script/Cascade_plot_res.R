###
# Process result Rhone (Cascade 2.1)
###

## Set directory
setwd('k:/Labo/20_OSR/Rhone_Fabio_Schneider/Result_Cascade_21/')

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
library(egg)

#####
# Lista de pastas onde estão os shapefiles
directory <- 'res_E2_eE&H/'
pastas <- list.dirs(path = directory, recursive = FALSE)

## Transported
# Loop pelas pastas
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Volume transported ###
  # ggarange
  
  # Vout Silt
  p_vout_silt <- ggplot(shp) +
    geom_col(aes(FromN, (Vout_silt)/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Silt transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  # Vout Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Vout_sand/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Vout Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Vout_gravel/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Gravel transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Vout Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Vout_boulder/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Boulder transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Vout
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, Vout_Tot_classes/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Total transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p_vout_silt, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  # ggsave(filename = paste0(directory, 'plots_transp/transp_', ano, '.tiff'),
  #        plot = pvout, width = 20, height = 30, units = 'cm', dpi = 300)
}


## Deposited
# Loop pelas pastas
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Volume deposited ###
  # ggarange

  # Dep Silt
  p_vout_silt <- ggplot(shp) +
    geom_col(aes(FromN, Dep_silt/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Silt deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  # Dep Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Dep_sand/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Dep Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Dep_gravel/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Gravel deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Dep Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Dep_boulder/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Boulder deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Dep
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, Dep_Tot_classes/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Total deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p_vout_silt, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  # ggsave(filename = paste0(directory, 'plots_dep/dep_', ano, '.tiff'),
  #        plot = pvout, width = 20, height = 30, units = 'cm', dpi = 300)
}


## Budget
# Loop pelas pastas
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Budget ###
  # ggarange
  
  # Budget Silt
  p_vout_silt <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_silt/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Silt budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  
  # Budget Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_sand/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Budget Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_gravel/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Gravel budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Budget Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_boulder/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Boulder budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Budget
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_Tot_classes/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Budget total (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p_vout_silt, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)

  # ggsave(filename = paste0(directory, 'plots_sed_budget/sed_bud', ano, '.tiff'),
  #        plot = pvout, width = 20, height = 30, units = 'cm', dpi = 300)
}





#####
## without silt
#Lista de pastas onde estão os shapefiles 
directory <- 'res_E2_eE&H/'
pastas <- list.dirs(path = directory, recursive = FALSE)

## Transported
# Loop pelas pastas
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Volume transported ###
  # ggarange
  
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = -0.01, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = -0.01, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = -0.01, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = expression('Discharge (m'^3~'.s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  # Vout Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Vout_sand/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Vout Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Vout_gravel/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Gravel transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Vout Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Vout_boulder/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Boulder transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Vout
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, (Vout_sand+Vout_gravel+Vout_boulder)/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Total transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  # ggsave(filename = paste0(directory, 'plots_transp-silt/transp_', ano, '.tiff'),
  #        plot = pvout, width = 20, height = 30, units = 'cm', dpi = 300)
}


## Deposited
# Loop pelas pastas
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Volume deposited ###
  # ggarange
  
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = -0.01, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = -0.01, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = -0.01, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = expression('Discharge (m'^3~'.s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  # Dep Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Dep_sand/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Dep Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Dep_gravel/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Gravel deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Dep Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Dep_boulder/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Boulder deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Dep
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, (Dep_sand+Dep_gravel+Dep_boulder)/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Total deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  # ggsave(filename = paste0(directory, 'plots_dep-silt/dep_', ano, '.tiff'),
  #        plot = pvout, width = 20, height = 30, units = 'cm', dpi = 300)
}


## Budget
# Loop pelas pastas
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Budget ###
  # ggarange
  
  # Discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = -0.01, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = -0.01, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = -0.01, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2.5, y = Inf) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_text(data = l_trib, aes(FromN, elevation, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2.5, y = -Inf, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = expression('Discharge (m'^3~'.s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 

  # Budget Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_sand/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Budget Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_gravel/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Gravel budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Budget Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_boulder/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Boulder budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Budget
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_Tot_classes/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_trib, aes(xintercept = FromN, colour = 'blue'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Budget total (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 10, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  # ggsave(filename = paste0(directory, 'plots_sed_budget-silt/sed_bud', ano, '.tiff'),
  #        plot = pvout, width = 20, height = 30, units = 'cm', dpi = 300)
}











#####
## without silt trib = arrows
#Lista de pastas onde estão os shapefiles 
directory <- 'res_E3_eW&C/'
pastas <- list.dirs(path = directory, recursive = FALSE)

## Transported
# Loop by folders
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  shp$sum_cls_vout <- (shp$Vout_boulder+shp$Vout_gravel+shp$Vout_sand)/1000

  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Volume transported ###
  # ggarange
  
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~'.s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  # Vout Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Vout_sand/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Vout_sand/1000)+(max(shp$Vout_sand/1000)*0.2)), yend = (Vout_sand/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Vout_sand/1000)+(max(shp$Vout_sand/1000)*0.2)), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Sand transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Vout Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, (Vout_gravel/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Vout_gravel/1000)+(max((shp$Vout_gravel/1000))*0.2), yend = (Vout_gravel/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Vout_gravel/1000)+(max((shp$Vout_gravel/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Vout Boulder
  p_vout_boulder <- ggplot(shp) +
    geom_col(aes(FromN, (Vout_boulder/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Vout_boulder/1000)+(max((shp$Vout_boulder/1000))*0.2), yend = (Vout_boulder/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Vout_boulder/1000)+(max((shp$Vout_boulder/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Vout
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, sum_cls_vout, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = sum_cls_vout+(max(shp$sum_cls_vout)*0.2), yend = sum_cls_vout, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, sum_cls_vout+(max(shp$sum_cls_vout)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Total transported (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  ggsave(filename = paste0(directory, 'plots_transp-silt/transp_', ano, '.tiff'),
         plot = pvout, width = 16, height = 20, units = 'cm', dpi = 300)
}


## Deposited
# Loop by folders
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  shp$sum_cls_dep <- (shp$Dep_boulder+shp$Dep_gravel+shp$Dep_sand)/1000
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Volume deposited ###
  # ggarange
  
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~'.s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  
  # Dep Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Dep_sand/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Dep_sand/1000)+(max((shp$Dep_sand/1000))*0.2), yend = (Dep_sand/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Dep_sand/1000)+(max((shp$Dep_sand/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Dep Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Dep_gravel/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Dep_gravel/1000)+(max((shp$Dep_gravel/1000))*0.2), yend = (Dep_gravel/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Dep_gravel/1000)+(max((shp$Dep_gravel/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Dep Boulder
  p_vout_boulder <- ggplot(shp) +
    geom_col(aes(FromN, Dep_boulder/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Dep_boulder/1000)+(max((shp$Dep_boulder/1000))*0.2), yend = (Dep_boulder/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Dep_boulder/1000)+(max((shp$Dep_boulder/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Dep
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, sum_cls_dep, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = sum_cls_dep+(max(shp$sum_cls_dep)*0.2), yend = sum_cls_dep, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, sum_cls_dep+(max(shp$sum_cls_dep)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Total deposited (Mg)', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  ggsave(filename = paste0(directory, 'plots_dep-silt/dep_', ano, '.tiff'),
         plot = pvout, width = 16, height = 20, units = 'cm', dpi = 300)
}


## Budget
# Loop by folders
for (pasta in pastas) {
  
  # Tenta encontrar o shapefile na pasta (assumindo que tem só um ou padrão)
  arquivo_shp <- list.files(pasta, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(arquivo_shp) == 0) {
    message(paste("Nenhum shapefile encontrado na pasta:", pasta))
    next
  }
  
  # Lê o shapefile
  shp <- st_read(arquivo_shp[1])
  
  shp$sum_cls_bud <- (shp$Sed_bud_boulder+shp$Sed_bud_gravel+shp$Sed_bud_sand)/1000
  
  l_dams <- shp[!is.na(shp$la_prise0),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Budget ###
  # ggarange
  
  # Discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~'.s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank()) 
  
  # Budget Sand
  p_vout_sand <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_sand/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Sed_bud_sand/1000)+(max((shp$Sed_bud_sand/1000))*0.2), yend = (Sed_bud_sand/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Sed_bud_sand/1000)+(max((shp$Sed_bud_sand/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Sand budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Budget Gravel
  p_vout_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_gravel/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Sed_bud_gravel/1000)+(max((shp$Sed_bud_gravel/1000))*0.2), yend = (Sed_bud_gravel/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Sed_bud_gravel/1000)+(max((shp$Sed_bud_gravel/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Budget Boulder
  p_vout_boulder <-   ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_boulder/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Sed_bud_boulder/1000)+(max((shp$Sed_bud_boulder/1000))*0.2), yend = (Sed_bud_boulder/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Sed_bud_boulder/1000)+(max((shp$Sed_bud_boulder/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder budget (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Total Budget
  
  p_vout <- ggplot(shp) +
    geom_col(aes(FromN, sum_cls_bud, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = sum_cls_bud+(max(shp$sum_cls_bud)*0.2), yend = sum_cls_bud, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, sum_cls_bud+(max(shp$sum_cls_bud)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Budget total (Mg)', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 10) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  # 
  ggsave(filename = paste0(directory, 'plots_sed_budget-silt/sed_bud_', ano, '.tiff'),
         plot = pvout, width = 16, height = 20, units = 'cm', dpi = 300)
}




#####
### Variables ###
# ggarange

# Altitude

p_alt <- ggplot(shp) +
  geom_line(aes(pk/1000, elevation, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(pk/1000, altitude, colour = 'black'), shape = 18, size = 2) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(expand = c(0,0), limits = c(0,500)) +
  geom_text(aes(pk/1000, altitude*10, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = pk/1000, xend = pk/1000, y = elevation+50, yend = elevation, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(pk/1000, elevation+50, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = pk/1000, colour = 'black'), linetype = 5, lwd = 0.2) +
  geom_text(aes(0,0,label = 'Confluence of the Rhône and Arve rivers'), angle = 90, size = 2, y = 180, color = 'gray20') +
  geom_text(aes(525,0,label = 'Beaucaire monitoring site'), angle = 90, size = 2, y = 110, color = 'gray20') +
  scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'pk (Km)', y = 'Altitude (m)') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'))
p_alt

ggsave(filename = 'Common_attributes/alt_pk.tiff', plot = p_alt, width = 16, height = 8, units = 'cm', dpi = 300)


# Manning's coefficient

p_n <- ggplot(shp) +
  geom_col(aes(FromN, n, fill = 'gray75'), color = 1, lwd = 0.1) +
  geom_line(aes(FromN, elevation/5000, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(FromN, altitude/5000, colour = 'black'), shape = 18, size = 2) +
  scale_y_continuous(name = expression('Manning`s coefficient (m'^{1/3}~'.s'^-1~')'), n.breaks = 10, expand = c(0,0), limits = c(0,0.1),
                     sec.axis = sec_axis(~.*5000, name = 'Altitude (m)', breaks = seq(0,500,100))) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(FromN, altitude/5000, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = elevation/5000+0.01, yend = elevation/5000, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(FromN, elevation/5000+0.01, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  scale_fill_manual(breaks = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                    values = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                    labels = c('Manning`s coefficient', 'Slope', 'GSD', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                      values = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                      labels = c('Manning`s coefficient', 'Slope', 'GSD', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'))
p_n

ggsave(filename = 'Common_attributes/manning.tiff', plot = p_n, width = 16, height = 8, units = 'cm', dpi = 300)


# Slope

p_S <- ggplot(shp) +
  geom_col(aes(FromN, Slope, fill = 'chocolate1'), color = 1, lwd = 0.1) +
  geom_line(aes(FromN, elevation/100000, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(FromN, altitude/100000, colour = 'black'), shape = 18, size = 2) +
  scale_y_continuous(name = expression('Slope (m.m'^-1~')'), n.breaks = 10, expand = c(0,0), limits = c(0,0.005),
                     sec.axis = sec_axis(~.*100000, name = 'Altitude (m)', breaks = seq(0,500,100))) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(FromN, altitude/100000, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = elevation/100000+0.0005, yend = elevation/100000, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(FromN, elevation/100000+0.0005, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  scale_fill_manual(breaks = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                    values = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                    labels = c('Manning`s coefficient', 'Slope', 'GSD', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                      values = c('gray75', 'chocolate1', 'brown', 'coral4', 'black', 'blue'),
                      labels = c('Manning`s coefficient', 'Slope', 'GSD', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'))
p_S

ggsave(filename = 'Common_attributes/slope.tiff', plot = p_S, width = 16, height = 8, units = 'cm', dpi = 300)


## GSD parrot
gsd_P <- read.csv('//calebasse/andress251/Bureau/Fabio/CASCADE_model_fabio/RhoneCatchment/DataBase/Parrot_et_al/GSD_Parrot_et_al_Fusion_data.csv')
gsd_P <- gsd_P[,c("ID_fabio", "D16mm", "D50mm", "D84mm")]
df_gsd <- aggregate(.~ID_fabio, data = gsd_P, FUN = max) %>% 
  rename(D16 = D16mm, D50 = D50mm, D84 = D84mm) %>% na.omit()

# D16

p_D16 <- ggplot(shp) +
  geom_col(aes(FromN, D16, fill = 'brown1'), color = 1, lwd = 0.1) +
  geom_col(data = df_gsd, aes(ID_fabio, D16/1000, fill = 'gray10'), color = 1, lwd = 0.1) +
  geom_line(aes(FromN, elevation/2000, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(FromN, altitude/2000, colour = 'black'), shape = 18, size = 2) +
  scale_y_continuous(name = expression('D16 (m)'), n.breaks = 10, expand = c(0,0), limits = c(0,0.25),
                     sec.axis = sec_axis(~.*2000, name = 'Altitude (m)', breaks = seq(0,500,100))) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(FromN, altitude/2000, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = elevation/2000+0.025, yend = elevation/2000, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(FromN, elevation/2000+0.025, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  scale_fill_manual(breaks = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                    values = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                    labels = c('GSD estimated', 'GSD measured', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                      values = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                      labels = c('GSD estimated', 'GSD measured', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'),
        axis.text.x = element_blank(), axis.title.x = element_blank()) 
p_D16


# D50

p_D50 <- ggplot(shp) +
  geom_col(aes(FromN, D50, fill = 'brown1'), color = 1, lwd = 0.1) +
  geom_col(data = df_gsd, aes(ID_fabio, D50/1000, fill = 'gray10'), color = 1, lwd = 0.1) +
  geom_line(aes(FromN, elevation/2000, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(FromN, altitude/2000, colour = 'black'), shape = 18, size = 2) +
  scale_y_continuous(name = expression('D50 (m)'), n.breaks = 10, expand = c(0,0), limits = c(0,0.25),
                     sec.axis = sec_axis(~.*2000, name = 'Altitude (m)', breaks = seq(0,500,100))) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(FromN, altitude/2000, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = elevation/2000+0.025, yend = elevation/2000, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(FromN, elevation/2000+0.025, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  scale_fill_manual(breaks = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                    values = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                    labels = c('GSD estimated', 'GSD measured', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                      values = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                      labels = c('GSD estimated', 'GSD measured', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'),
        axis.text.x = element_blank(), axis.title.x = element_blank()) 

p_D50

# D84

p_D84 <- ggplot(shp) +
  geom_col(aes(FromN, D84, fill = 'brown1'), color = 1, lwd = 0.1) +
  geom_col(data = df_gsd, aes(ID_fabio, D84/1000, fill = 'gray10'), color = 1, lwd = 0.1) +
  geom_line(aes(FromN, elevation/2000, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(FromN, altitude/2000, colour = 'black'), shape = 18, size = 2) +
  scale_y_continuous(name = expression('D84 (m)'), n.breaks = 10, expand = c(0,0), limits = c(0,0.25),
                     sec.axis = sec_axis(~.*2000, name = 'Altitude (m)', breaks = seq(0,500,100))) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(FromN, altitude/2000, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = elevation/2000+0.025, yend = elevation/2000, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(FromN, elevation/2000+0.025, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  scale_fill_manual(breaks = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                    values = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                    labels = c('GSD estimated', 'GSD measured', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                      values = c('brown1', 'gray10', 'coral4', 'black', 'blue'),
                      labels = c('GSD estimated', 'GSD measured', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black')) 

p_D84

p_GSD <- ggarrange(plots = list(p_D16, p_D50, p_D84), nrow = 3, ncol = 1)

ggsave(filename = 'Common_attributes/GSD.tiff', plot = p_GSD, width = 16, height = 18, units = 'cm', dpi = 300)


p_wac <- ggplot(shp) +
  geom_col(aes(FromN, Wac, fill = 'green4'), color = 1, lwd = 0.1) +
  geom_line(aes(FromN, elevation, colour = 'coral4'), lwd = 0.5) +
  geom_point(aes(FromN, altitude, colour = 'black'), shape = 18, size = 2) +
  scale_y_continuous(name = expression('Width channel (m)'), n.breaks = 10, expand = c(0,0), limits = c(0,500),
                     sec.axis = sec_axis(~., name = 'Altitude (m)', breaks = seq(0,500,100))) +
  scale_x_continuous(n.breaks = 10) +
  geom_text(aes(FromN, altitude, label = la_prise0), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = elevation+50, yend = elevation, colour = 'blue'),
               arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = l_trib, aes(FromN, elevation+50, label = Trib), angle = 90, vjust = -0.2, hjust = 0, size = 2, color = 'blue') +
  geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  scale_fill_manual(breaks = c('gray75', 'chocolate1', 'brown', 'green4', 'coral4', 'black', 'blue'),
                    values = c('gray75', 'chocolate1', 'brown', 'green4', 'coral4', 'black', 'blue'),
                    labels = c('Manning`s coefficient', 'Slope', 'GSD', 'Width channel', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('gray75', 'chocolate1', 'brown', 'green4', 'coral4', 'black', 'blue'),
                      values = c('gray75', 'chocolate1', 'brown', 'green4', 'coral4', 'black', 'blue'),
                      labels = c('Manning`s coefficient', 'Slope', 'GSD', 'Width channel', 'Altitude', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  geom_hline(yintercept = 0, lwd = 0.2) +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black')) 
p_wac

ggsave(filename = 'Common_attributes/width_channel.tiff', plot = p_wac, width = 16, height = 8, units = 'cm', dpi = 300)








