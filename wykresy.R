library(tmap)
library(raster)
library(rgdal)
library(ggplot2)
library(plotly)
library(spatialEco)
library(dplyr)

# wczytanie rastrów
ndvi_miedzyzdroje <- raster("dane_gotowe/ndvi_miedzyzdroje.tif")
temp_miedzyzdroje <- raster("dane_gotowe/temp_miedzyzdroje.tif")
ndvi_WPN <- raster("dane_gotowe/ndvi_WPN.tif")
temp_WPN <- raster("dane_gotowe/temp_WPN.tif")
grupa <- c(ndvi_WPN, temp_WPN)

# podglad warstwy
tm_shape(ndvi_miedzyzdroje) + tm_raster()
tm_shape(cor) + tm_raster()

# zapisanie rastra do ramki danych
ndvi_miedzyzdroje_dt <- values(ndvi_miedzyzdroje)
ndvi_miedzyzdroje_dt <- as.data.frame(ndvi_miedzyzdroje_dt) %>% na.omit()

temp_miedzyzdroje_dt <- values(temp_miedzyzdroje)
temp_miedzyzdroje_dt <- as.data.frame(temp_miedzyzdroje_dt) %>% na.omit

ndvi_WPN_dt <- values(ndvi_WPN)
ndvi_WPN_dt <- as.data.frame(ndvi_WPN_dt) %>% na.omit()

temp_WPN_dt <- values(temp_WPN)
temp_WPN_dt <- as.data.frame(temp_WPN_dt) %>% na.omit()

# stworzenie histogramu
p = ggplot(as.data.frame(ndvi_miedzyzdroje_dt), aes(x = ndvi_miedzyzdroje_dt)) +
  geom_histogram(fill = hcl.colors(40, palette = "RdYlGn"), bins = 40) +
  labs(title = "piwo", x = "Wskaznik NDVI", y = "Liczba") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  theme(
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "#222222"),
    panel.background = element_rect(fill = "#222222"), 
    axis.title = element_text(size = 15,
                              color = "#dddddd"), 
    plot.title = element_text(size = 18,
                              color = "#dddddd",
                              vjust = 2,
                              hjust = 0.5), 
    legend.background = element_rect(color = "#222222", 
                                     fill = "#777777"),  
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12, 
                             color = "#dddddd"),
    axis.text.x = element_text(angle = 60, vjust = 0.95, hjust=1))
ggplotly(p)
#Sys.setenv("plotly_username" = "adryanqe")
#Sys.setenv("plotly_api_key" = "BWYeEqc9Tcu65gh28WEw")
#api_create(p, "NDVI Miedzyzdroje")



# korelacje
cor <- layerStats(temp_WPN, "pearson", ndvi_WPN, na.rm = T)
cor(temp_WPN_dt, ndvi_WPN_dt, method = "pearson")
cor(temp_miedzyzdroje_dt, ndvi_miedzyzdroje_dt, method = "pearson")



