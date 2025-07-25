###
# Process result Rhone (Cascade 2.1)
###

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
## without silt trib = arrows
#Lista de pastas onde estão os shapefiles 

ref <- '0_eW&C_noDams'

directory <- paste0('k:/Labo/20_OSR/Rhone_Fabio_Schneider/Result_Cascade_21_67reaches/res_E', ref, '/')
pastas <- list.dirs(path = directory, recursive = FALSE)
save_directory <- paste0('img/res_cascade/res_E', ref, '/')

{
  ## Transport capacity
  {
## Transport capacity
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
  
  shp$sum_cls_trc <- (shp$Tr_cap_boulder+shp$Tr_cap_gravel+shp$Tr_cap_sand)/1000
  
  l_dams <- shp[!is.na(shp$Dam),]
  l_trib <- shp[!is.na(shp$Trib),]
  
  # Nome do ano (assumindo que o nome da pasta tem o ano, adapte se necessário)
  ano <- basename(pasta)
  
  
  ### Transport capacity ###
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
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  
  
  # Trcap Sand
  p_trc_sand <- ggplot(shp) +
    geom_col(aes(FromN, Tr_cap_sand/1000, fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Tr_cap_sand/1000)+(max(shp$Tr_cap_sand/1000)*0.2)), yend = (Tr_cap_sand/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Tr_cap_sand/1000)+(max(shp$Tr_cap_sand/1000)*0.2)), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Sand (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Trcap gravel
  p_trc_gravel <- ggplot(shp) +
    geom_col(aes(FromN, Tr_cap_gravel/1000, fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Tr_cap_gravel/1000)+(max(shp$Tr_cap_gravel/1000)*0.2)), yend = (Tr_cap_gravel/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Tr_cap_gravel/1000)+(max(shp$Tr_cap_gravel/1000)*0.2)), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Trcap boulder
  p_trc_boulder <- ggplot(shp) +
    geom_col(aes(FromN, Tr_cap_boulder/1000, fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Tr_cap_boulder/1000)+(max(shp$Tr_cap_boulder/1000)*0.2)), yend = (Tr_cap_boulder/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Tr_cap_boulder/1000)+(max(shp$Tr_cap_boulder/1000)*0.2)), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Trcap total
  p_trc_total <- ggplot(shp) +
    geom_col(aes(FromN, sum_cls_trc, fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((sum_cls_trc)+(max(shp$sum_cls_trc)*0.2)), yend = (sum_cls_trc), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((sum_cls_trc)+(max(shp$sum_cls_trc)*0.2)), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Total (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  ptrc <- ggarrange(plots = list(p, p_trc_sand, p_trc_gravel, p_trc_boulder, p_trc_total), nrow = 5, ncol = 1)
  
  ggsave(filename = paste0(directory, 'plots_tr_cap-silt/tr_cap_', ano, '.png'),
         plot = ptrc, width = 16, height = 20, units = 'cm', dpi = 300)
}

  ### Transport capacity total per year ###
  # ggarange
    
    shp <- st_read(paste0(
      'k:/Labo/20_OSR/Rhone_Fabio_Schneider/Result_Cascade_21_67reaches/res_E', ref, '_stat/res_stat_hy_E', ref, '/df_res_output_ext_stats_hy_E', ref, '.gpkg'
    ))
    
    l_dams <- shp[!is.na(shp$Dam),]
    l_trib <- shp[!is.na(shp$Trib),]
    
    
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  
  
  # Trcap Sand
  p_trc_sand <- ggplot(shp) +
    geom_col(aes(FromN, (Tr_cap_sand_mean/1000), fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (((Tr_cap_sand_mean/1000))+(max((shp$Tr_cap_sand_mean/1000))*0.2)),
                                    yend = (Tr_cap_sand_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (((Tr_cap_sand_mean/1000))+(max((shp$Tr_cap_sand_mean/1000))*0.2)),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Sand (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Trcap gravel
  p_trc_gravel <- ggplot(shp) +
    geom_col(aes(FromN, (Tr_cap_gravel_mean/1000), fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (((Tr_cap_gravel_mean/1000))+(max((shp$Tr_cap_gravel_mean/1000))*0.2)),
                                    yend = (Tr_cap_gravel_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (((Tr_cap_gravel_mean/1000))+(max((shp$Tr_cap_gravel_mean/1000))*0.2)),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  # Trcap boulder
  p_trc_boulder <- ggplot(shp) +
    geom_col(aes(FromN, (Tr_cap_boulder_mean/1000), fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (((Tr_cap_boulder_mean/1000))+(max((shp$Tr_cap_boulder_mean/1000))*0.2)),
                                    yend = (Tr_cap_boulder_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (((Tr_cap_boulder_mean/1000))+(max((shp$Tr_cap_boulder_mean/1000))*0.2)),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'none', legend.spacing = unit(0, 'cm')) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black'),
          axis.text.x = element_blank(), axis.title.x = element_blank())
  
  
  
  
  # Trcap total
  p_trc_total <- ggplot(shp) +
    geom_col(aes(FromN, Dep_Tot_classes_mean/1000, fill = 'red'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Dep_Tot_classes_mean/1000)+(max(shp$Dep_Tot_classes_mean/1000)*0.2)),
                                    yend = (Dep_Tot_classes_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Dep_Tot_classes_mean/1000)+(max(shp$Dep_Tot_classes_mean/1000)*0.2)),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Total (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'red', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Transport capacity', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  ptrc <- ggarrange(plots = list(p, p_trc_sand, p_trc_gravel, p_trc_boulder, p_trc_total), nrow = 5, ncol = 1)
  
  
  
  ggsave(filename = paste0(save_directory, 'plots_tr_cap-silt/tr_cap_', ano, '.png'),
         plot = ptrc, width = 16, height = 20, units = 'cm', dpi = 300)
  
  }
  
  ## Sediment transported
  {
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

  l_dams <- shp[!is.na(shp$Dam),]
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
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Sand (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Gravel (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Boulder (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Total (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  ggsave(filename = paste0(directory, 'plots_transp-silt/transp_', ano, '.png'),
         plot = pvout, width = 16, height = 20, units = 'cm', dpi = 300)
}

  
  ### Volume transported per year ###
  # ggarange
  
    shp <- st_read(paste0(
      'k:/Labo/20_OSR/Rhone_Fabio_Schneider/Result_Cascade_21_67reaches/res_E', ref, '_stat/res_stat_hy_E', ref, '/df_res_output_ext_stats_hy_E', ref, '.gpkg'
    ))
    
    l_dams <- shp[!is.na(shp$Dam),]
    l_trib <- shp[!is.na(shp$Trib),]
    
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    geom_col(aes(FromN, (Vout_sand_mean/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (((Vout_sand_mean/1000))+(max((shp$Vout_sand_mean/1000))*0.2)),
                                    yend = (Vout_sand_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (((Vout_sand_mean/1000))+(max((shp$Vout_sand_mean/1000))*0.2)),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Sand (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    geom_col(aes(FromN, (Vout_gravel_mean/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Vout_gravel_mean/1000))+(max(((shp$Vout_gravel_mean/1000)))*0.2),
                                    yend = (Vout_gravel_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Vout_gravel_mean/1000))+(max(((shp$Vout_gravel_mean/1000)))*0.2),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    geom_col(aes(FromN, (Vout_boulder_mean/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Vout_boulder_mean/1000))+(max(((shp$Vout_boulder_mean/1000)))*0.2), 
                                    yend = (Vout_boulder_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Vout_boulder_mean/1000))+(max(((shp$Vout_boulder_mean/1000)))*0.2),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    geom_col(aes(FromN, Vout_Tot_classes_mean/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Vout_Tot_classes_mean/1000+(max(shp$Vout_Tot_classes_mean/1000)*0.2), 
                                    yend = Vout_Tot_classes_mean/1000, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Vout_Tot_classes_mean/1000+(max(shp$Vout_Tot_classes_mean/1000)*0.2), 
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Total (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                      labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        values = c('blue4', 'orangered', 'coral4', 'black', 'blue'),
                        labels = c('Discharge', 'Sediment transported', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pvout <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  ggsave(filename = paste0(save_directory, 'plots_transp-silt/transp_', ano, '.png'),
         plot = pvout, width = 16, height = 20, units = 'cm', dpi = 300)
  
  }
  
  ## Sediment deposited
  {
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
  
  l_dams <- shp[!is.na(shp$Dam),]
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
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Sand (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Dep_sand/1000)+(max((shp$Dep_sand/1000))*0.2), yend = (Dep_sand/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Dep_sand/1000)+(max((shp$Dep_sand/1000))*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Gravel (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Boulder (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Total (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pdep <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
  
  ggsave(filename = paste0(directory, 'plots_dep-silt/dep_', ano, '.png'),
         plot = pdep, width = 16, height = 20, units = 'cm', dpi = 300)
}
  
  
  ### Volume deposited per year ###
  # ggarange
    
    shp <- st_read(paste0(
      'k:/Labo/20_OSR/Rhone_Fabio_Schneider/Result_Cascade_21_67reaches/res_E', ref, '_stat/res_stat_hy_E', ref, '/df_res_output_ext_stats_hy_E', ref, '.gpkg'
    ))
  
    l_dams <- shp[!is.na(shp$Dam),]
    l_trib <- shp[!is.na(shp$Trib),]
    
  # discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  p_dep_sand <- ggplot(shp) +
    geom_col(aes(FromN, (Dep_sand_mean/1000), fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    scale_y_continuous(name = 'Sand (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Dep_sand_mean/1000))+(max(((shp$Dep_sand_mean/1000)))*0.2),
                                    yend = (Dep_sand_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Dep_sand_mean/1000))+(max(((shp$Dep_sand_mean/1000)))*0.2),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  p_dep_gravel <- ggplot(shp) +
    geom_col(aes(FromN, (Dep_gravel_mean/1000), fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Dep_gravel_mean/1000))+(max(((shp$Dep_gravel_mean/1000)))*0.2),
                                    yend = (Dep_gravel_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Dep_gravel_mean/1000))+(max(((shp$Dep_gravel_mean/1000)))*0.2),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  p_dep_boulder <- ggplot(shp) +
    geom_col(aes(FromN, (Dep_boulder_mean/1000), fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Dep_boulder_mean/1000))+(max(((shp$Dep_boulder_mean/1000)))*0.2), 
                                    yend = (Dep_boulder_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Dep_boulder_mean/1000))+(max(((shp$Dep_boulder_mean/1000)))*0.2), 
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  
  p_dep <- ggplot(shp) +
    geom_col(aes(FromN, Dep_Tot_classes_mean/1000, fill = 'darkorange'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Dep_Tot_classes_mean/1000)+(max((shp$Dep_Tot_classes_mean/1000))*0.2), 
                                    yend = Dep_Tot_classes_mean/1000, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Dep_Tot_classes_mean/1000)+(max((shp$Dep_Tot_classes_mean/1000))*0.2),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Total (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                      values = c('darkorange', 'coral4', 'black', 'blue'),
                      labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('darkorange', 'coral4', 'black', 'blue'),
                        values = c('darkorange', 'coral4', 'black', 'blue'),
                        labels = c('Sediment deposited', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pdep <- ggarrange(plots = list(p, p_dep_sand, p_dep_gravel, p_dep_boulder, p_dep), nrow = 5, ncol = 1)
  
  
  
  ggsave(filename = paste0(save_directory, 'plots_dep-silt/dep_', ano, '.png'),
         plot = pdep, width = 16, height = 20, units = 'cm', dpi = 300)
  }
  
  ## Sediment budget
  {
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
  
  l_dams <- shp[!is.na(shp$Dam),]
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
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Sand (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Gravel (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Boulder (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
    scale_y_continuous(name = 'Total (1000 m'^3~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pbudget <- ggarrange(plots = list(p, p_vout_sand, p_vout_gravel, p_vout_boulder, p_vout), nrow = 5, ncol = 1)
   
  ggsave(filename = paste0(directory, 'plots_sed_budget-silt/sed_bud_', ano, '.png'),
         plot = pbudget, width = 16, height = 20, units = 'cm', dpi = 300)
}
  
  
  ### Budget per year ###
  # ggarange
  
    shp <- st_read(paste0(
      'k:/Labo/20_OSR/Rhone_Fabio_Schneider/Result_Cascade_21_67reaches/res_E', ref, '_stat/res_stat_hy_E', ref, '/df_res_output_ext_stats_hy_E', ref, '.gpkg'
    ))
    
    l_dams <- shp[!is.na(shp$Dam),]
    l_trib <- shp[!is.na(shp$Trib),]
    
  # Discharge
  p <- ggplot(shp) +
    geom_line(aes(FromN, Qmean, colour = 'blue4'), lwd = 0.5) +
    geom_text(aes(max(FromN), max(Qmean), label = 'Q mean'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmin, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmin), label = 'Q min'), vjust = 1, hjust = 0.2, size = 2) +
    geom_line(aes(FromN, Qmax, colour = 'blue4'), lwd = 0.3, linetype = 5) +
    geom_text(aes(max(FromN), max(Qmax), label = 'Q max'), vjust = 1, hjust = 0.2, size = 2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_text(aes(FromN, altitude, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = Qmean+(max(Qmax)*0.2), yend = Qmean, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, Qmean+(max(Qmax)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = expression('Discharge (m'^3~' s'^-1~')'), n.breaks = 10, expand = expansion(c(0,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  p_bg_sand <- ggplot(shp) +
    geom_col(aes(FromN, (Sed_bud_sand_mean/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Sed_bud_sand_mean/1000))+(max(((shp$Sed_bud_sand_mean/1000)))*0.2),
                                    yend = (Sed_bud_sand_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Sed_bud_sand_mean/1000))+(max(((shp$Sed_bud_sand_mean/1000)))*0.2), 
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Sand (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  p_bg_gravel <- ggplot(shp) +
    geom_col(aes(FromN, (Sed_bud_gravel_mean/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Sed_bud_gravel_mean/1000))+(max(((shp$Sed_bud_gravel_mean/1000)))*0.2), 
                                    yend = (Sed_bud_gravel_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Sed_bud_gravel_mean/1000))+(max(((shp$Sed_bud_gravel_mean/1000)))*0.2), 
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Gravel (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  p_bg_boulder <- ggplot(shp) +
    geom_col(aes(FromN, (Sed_bud_boulder_mean/1000), fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = ((Sed_bud_boulder_mean/1000))+(max(((shp$Sed_bud_boulder_mean/1000)))*0.2), 
                                    yend = (Sed_bud_boulder_mean/1000), colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, ((Sed_bud_boulder_mean/1000))+(max(((shp$Sed_bud_boulder_mean/1000)))*0.2), 
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Boulder (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
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
  
  p_bg <- ggplot(shp) +
    geom_col(aes(FromN, Sed_bud_Tot_classes_mean/1000, fill = 'orangered'), color = 1, lwd = 0.1) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_vline(data = l_dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
    geom_segment(data = l_trib, aes(x = FromN, xend = FromN, y = (Sed_bud_Tot_classes_mean/1000)+(max((shp$Sed_bud_Tot_classes_mean/1000))*0.2), 
                                    yend = Sed_bud_Tot_classes_mean/1000, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
    geom_text(data = l_trib, aes(FromN, (Sed_bud_Tot_classes_mean/1000)+(max(shp$Sed_bud_Tot_classes_mean/1000)*0.2),
                                 label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
    scale_y_continuous(name = 'Total (1000 m'^3~' y'^-1~')', n.breaks = 10, expand = expansion(c(0.05,0.05))) +
    scale_x_continuous(n.breaks = 15, limits = c(0,70), expand = c(0,0)) +
    scale_fill_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                      values = c('orangered', 'coral4', 'black', 'blue'),
                      labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    scale_colour_manual(breaks = c('orangered', 'coral4', 'black', 'blue'),
                        values = c('orangered', 'coral4', 'black', 'blue'),
                        labels = c('Sediment budget', 'Altitude', 'Dams', 'Tributaries'), name = '') +
    labs(x = 'Reach ID') +
    theme_article(base_family = "Times New Roman") +
    geom_hline(yintercept = 0, lwd = 0.2) +
    theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm'), legend.text = element_text(size = 8)) +
    theme(text = element_text(size = 8, colour = 'black'),
          axis.text = element_text(size = 8, colour = 'black')) 
  
  
  pbudget <- ggarrange(plots = list(p, p_bg_sand, p_bg_gravel, p_bg_boulder, p_bg), nrow = 5, ncol = 1, align = "v")
  
  
  
  ggsave(filename = paste0(save_directory, 'plots_sed_budget-silt/sed_bud_', ano, '.png'),
         plot = pbudget, width = 16, height = 20, units = 'cm', dpi = 300)
  }
  
}



