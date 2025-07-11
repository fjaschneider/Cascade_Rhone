###
# Imput data Rhone (Cascade 2.0)
###

## Set directory
setwd('/Bureau/Fabio/Rhone/')

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


# Old script
#####

# Import data frame (DEM smooth)
df <- read.csv('df_rhone_elev.csv')

# Import shp rhone river
shp <- read_sf('../Cascade_python/dcascade-py-2.0.0_Rhone/res/resresult_cascade_2019.gpkg')
plot(shp)

shp_in_cascade <- shp %>% select(reach_id,FromN,ToN,Wac,Length,n,D16,D50,D84,x_FN,x_TN,y_FN,y_TN) %>%
  rename(fid = reach_id) %>%
  left_join(df, by = 'fid')
 
# shp_in_cascade$D16 <- shp_in_cascade$D16/1000
# shp_in_cascade$D50 <- shp_in_cascade$D50/1000
# shp_in_cascade$D84 <- shp_in_cascade$D84/1000

shp_cascade20 <- shp_in_cascade %>% rename(pk=meters, Slope=slope) 
shp_cascade20$el_FN <- c(370, shp_in_cascade$elevation[-length(shp_in_cascade$elevation)])
shp_cascade20$el_TN <- shp_in_cascade$elevation

# write_sf(shp_cascade20, 'input_data_rhone/shp_rhone_smooth_C20.shp')

# Import data dams
df_dams <- read.csv('input_data_rhone/Barrages.csv')


shp_river <- read_sf('input_data_rhone/shp_rhone_smooth_C20.shp')

shp_river_dams <- shp_river %>%
  rename(id = fid) %>%
  left_join(df_dams[,-c(1,10)], by = 'id')

mapview(shp_river_dams[,"retenue"])

# write_sf(shp_river_dams, 'input_data_rhone/rhone_river_dams_C20.shp')


###
# Discharge quantile
###

# read daily data Q
df_q_day <- read.csv('../Cascade_python/dcascade-py-2.0.0_Rhone/Input/Rhone/Daily_Q_2009-2019_rhone.csv')
df_q <- df_q_day %>% select(where(is.numeric))
colnames(df_q) <- c(1:195)

# transform the data to long format
df_q_long <- df_q %>%
  pivot_longer(
    cols = everything(),
    names_to = "reach",
    values_to = "discharge"
  )

# calculate the quantis
df_q_quantis <- df_q_long %>%
  group_by(reach) %>%
  summarise(q001 = quantile(discharge, 0.0001, na.rm = TRUE),
            q010 = quantile(discharge, 0.1, na.rm = TRUE),
            q030 = quantile(discharge, 0.3, na.rm = TRUE),
            q050 = quantile(discharge, 0.5, na.rm = TRUE),
            q070 = quantile(discharge, 0.7, na.rm = TRUE),
            q090 = quantile(discharge, 0.9, na.rm = TRUE),
            q099 = quantile(discharge, 0.99, na.rm = TRUE),
            q100 = quantile(discharge, 1, na.rm = TRUE)) %>%
  arrange(as.numeric(reach))

# write.csv(df_q_quantis, 'input_data_rhone/Quantis_discharge_2009-2019.csv', row.names = F)
# write.table(df_q_quantis, 'input_data_rhone/Quantis_discharge_2009-2019.txt', sep = '\t', dec ='.', row.names = F)


###
# Process data width (Wac)
###

# read data coefficients
df_wav <- read.csv('data_brut/wav_coefs.csv')
summary(df_wav)

# # coefficients correction
# min_l50 <- min(df_wav$l50_Morel[df_wav$l50_Morel != 0])
# df_wav$l50_Morel[df_wav$l50_Morel == 0] <- min_l50
# min_b <- min(df_wav$b_Morel[df_wav$b_Morel != 0])
# df_wav$b_Morel[df_wav$b_Morel == 0] <- min_b

# Read old river network
Riv_Net <- read_sf('../CASCADE_model_fabio/GIS_data_fabio/Rhone_stream_CASCADE/Rhone_stream.shp')

df_wav_id <- Riv_Net %>%
  select(reach_id, ID_fabio) %>%
  left_join(df_wav, by = 'reach_id') %>%
  select(ID_fabio, l50_Morel, b_Morel)
  
df_wav_id$ID_fabio <- as.character(df_wav_id$ID_fabio)

df_w_q <- df_q_quantis %>%
  rename(ID_fabio = reach) %>% 
  left_join(df_wav_id, by = 'ID_fabio')

df_wac <- df_w_q %>%
  mutate(wc_p1 =  l50_Morel*(q001/q050)^b_Morel,
         wc_p10 = l50_Morel*(q010/q050)^b_Morel,
         wc_p30 = l50_Morel*(q030/q050)^b_Morel,
         wc_p50 = l50_Morel*(q050/q050)^b_Morel,
         wc_p70 = l50_Morel*(q070/q050)^b_Morel,
         wc_p90 = l50_Morel*(q090/q050)^b_Morel,
         wc_p99 = l50_Morel*(q099/q050)^b_Morel,
         wc_p100 = l50_Morel*(q100/q050)^b_Morel) %>%
  select(ID_fabio, wc_p1, wc_p10, wc_p30, wc_p50, wc_p70, wc_p90, wc_p99, wc_p100) %>%
  rename(reach_id = ID_fabio)

summary(df_wac)

# write.table(df_wac, 'input_data_rhone/Quantis_Wac_2009-2019.txt', sep = '\t', dec ='.', row.names = F)

shp_dams <- read_sf('../CASCADE_model_fabio/RhoneCatchment/cascademodel-master/CASCADE_toolbox_master/CASCADE_model/rhone_data/just_rhone/dams.shp')

shp_dams_rename <- shp_dams %>%
  select(node_id,cobble,gravel,sand,silt) %>%
  rename(cobble_trap = cobble,
         gravel_trap = gravel,
         sand_trap = sand,
         silt_trap = silt)
                    


# write_sf(shp_dams_rename, '../CASCADE_model_fabio/RhoneCatchment/cascademodel-master/CASCADE_toolbox_master/CASCADE_model/rhone_data/just_rhone/dams_rename.shp')

# read data Q
df_q_2009_2019 <- read.csv('../Cascade_python/dcascade_py_21/Input_data/Daily_Q/Daily_Q_2009-2019_rhone.csv')
colnames(df_q_2009_2019) <- c('yyyy/mm/dd', 1:195)

df_q_2009_2012 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` < '2012-01-01',]
df_q_2012_2015 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2012-01-01' & df_q_2009_2019$`yyyy/mm/dd` < '2015-01-01',]
df_q_2015_2017 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2015-01-01' & df_q_2009_2019$`yyyy/mm/dd` < '2017-01-01',]
df_q_2017_2019 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2017-01-01',]

# write.csv(df_q_2009_2019, '../Cascade_python/dcascade_py_21/Input_data/Daily_Q/Daily_Q_2009-2019_rhone.csv', row.names = F)
# write.csv(df_q_2009_2012, '../Cascade_python/dcascade_py_21/Input_data/Daily_Q/Daily_Q_2009-2012_rhone.csv', row.names = F)
# write.csv(df_q_2012_2015, '../Cascade_python/dcascade_py_21/Input_data/Daily_Q/Daily_Q_2012-2015_rhone.csv', row.names = F)
# write.csv(df_q_2015_2017, '../Cascade_python/dcascade_py_21/Input_data/Daily_Q/Daily_Q_2015-2017_rhone.csv', row.names = F)
# write.csv(df_q_2017_2019, '../Cascade_python/dcascade_py_21/Input_data/Daily_Q/Daily_Q_2017-2019_rhone.csv', row.names = F)

df_q_2009_2019$`yyyy/mm/dd` <- ymd(df_q_2009_2019$`yyyy/mm/dd`)

plot_ly(data = df_q_2009_2019) %>%
  add_bars(x = ~`yyyy/mm/dd`, y = ~`195`)

# Daily discharge by hydrological cycle - start September 21
dfq_HC_2009 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` < '2009-09-21',]
dfq_HC_2010 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2009-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2010-09-21',]
dfq_HC_2011 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2010-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2011-09-21',]
dfq_HC_2012 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2011-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2012-09-21',]
dfq_HC_2013 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2012-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2013-09-21',]
dfq_HC_2014 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2013-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2014-09-21',]
dfq_HC_2015 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2014-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2015-09-21',]
dfq_HC_2016 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2015-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2016-09-21',]
dfq_HC_2017 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2016-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2017-09-21',]
dfq_HC_2018 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2017-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2018-09-21',]
dfq_HC_2019 <- df_q_2009_2019[df_q_2009_2019$`yyyy/mm/dd` >= '2018-09-21' & df_q_2009_2019$`yyyy/mm/dd` < '2019-09-21',]

# write.csv(dfq_HC_2009, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2009.csv', row.names = F)
# write.csv(dfq_HC_2010, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2010.csv', row.names = F)
# write.csv(dfq_HC_2011, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2011.csv', row.names = F)
# write.csv(dfq_HC_2012, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2012.csv', row.names = F)
# write.csv(dfq_HC_2013, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2013.csv', row.names = F)
# write.csv(dfq_HC_2014, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2014.csv', row.names = F)
# write.csv(dfq_HC_2015, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2015.csv', row.names = F)
# write.csv(dfq_HC_2016, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2016.csv', row.names = F)
# write.csv(dfq_HC_2017, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2017.csv', row.names = F)
# write.csv(dfq_HC_2018, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2018.csv', row.names = F)
# write.csv(dfq_HC_2019, '../Cascade_python/dcascade_py_21/Input_data/Dayly_Q_Hydro_Cycle/dfq_HC_2019.csv', row.names = F)

plot_ly(data = dfq_HC_2010) %>%
  add_bars(x = ~`yyyy/mm/dd`, y = ~`5`)


seq(-9.5, 7.5, length.out = 18)

#####
# New data with less reaches

read_shp <- read_sf('GIS_data/Rhone_network/Rhone_network_r66.shp')
shp <- read_shp %>%
  rename(Dam = la_prise0) %>%
  arrange(reach_ID)

shp$pk <- cumsum(shp$Length)/1000
dams <- shp[!is.na(shp$Dam),]
trib <- shp[!is.na(shp$Trib),]

plt_elev <- ggplot(shp) +
  geom_col(aes(reach_ID,el_FN, fill = 'green4')) +
  geom_col(aes(reach_ID,el_TN, fill = 'green')) +
  geom_vline(data = dams, aes(xintercept = FromN, colour = 'black'), linetype = 5, lwd = 0.2) +
  geom_text(aes(FromN, el_FN, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  geom_segment(data = trib, aes(x = FromN, xend = FromN, y = el_FN+(max(el_FN)*0.2), yend = el_FN, colour = 'blue'), arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5) +
  geom_text(data = trib, aes(FromN, el_FN+(max(el_FN)*0.2), label = Trib), angle = 90, vjust = 0, hjust = 0, size = 2, color = 'blue') +
  scale_y_continuous(name = expression('Elevation (m)'), n.breaks = 10, expand = expansion(c(0,0.05))) +
  scale_x_continuous(n.breaks = 10) +
  scale_fill_manual(breaks = c('green4', 'black', 'blue'),
                    values = c('green4', 'black', 'blue'),
                    labels = c('Elevation', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('green4', 'black', 'blue'),
                      values = c('green4', 'black', 'blue'),
                      labels = c('Elevation', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black')) +
  ggtitle('DEM Elevation data')
plt_elev

ggsave(filename = 'temp_plots/elevation_dem.tiff', plot = plt_elev, width = 16, height = 8, units = 'cm', dpi = 300)

## Slope calculation
shp$Slope <- (shp$el_FN-shp$el_TN)/shp$Length

ggplot(shp) +
  geom_line(aes(pk,el_FN, fill = 'green4')) +
  geom_vline(data = dams, aes(xintercept = pk, colour = 'black'), linetype = 5, lwd = 0.2) +
  geom_text(aes(pk, el_FN, label = Dam), angle = 90, vjust = -0.2, hjust = 1, size = 2, y = Inf) +
  scale_y_continuous(name = expression('Elevation (m)'), n.breaks = 10, expand = expansion(c(0,0.05))) +
  scale_x_continuous(n.breaks = 10) +
  scale_fill_manual(breaks = c('green4', 'black', 'blue'),
                    values = c('green4', 'black', 'blue'),
                    labels = c('Elevation', 'Dams', 'Tributaries'), name = '') +
  scale_colour_manual(breaks = c('green4', 'black', 'blue'),
                      values = c('green4', 'black', 'blue'),
                      labels = c('Elevation', 'Dams', 'Tributaries'), name = '') +
  labs(x = 'Reach ID') +
  theme_article(base_family = "Times New Roman") +
  theme(legend.key.height = unit(0.5, 'cm'), legend.position = 'bottom', legend.spacing = unit(0, 'cm')) +
  theme(text = element_text(size = 8, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black')) +
  ggtitle('DEM Elevation data') 



shp_po <- read_sf('../Cascade_python/dcascade-py-2.0.0_Rhone/Input/Po_case/Po_river_network.shp')
shp_po <- shp_po[shp_po$River == 'Po',]
shp_po_cres <- shp_po %>% arrange(FromN)
shp_po_cres$pk <- cumsum(shp_po_cres$Length)/1000

ggplot(shp_po_cres) +
  geom_line(aes(pk,el_FN))

