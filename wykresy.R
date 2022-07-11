library(tmap)
library(raster)
library(rgdal)
library(ggplot2)
library(plotly)

ndvi_miedzyzdroje <- raster("dane_gotowe/ndvi_miedzyzdroje.tif")
ndvi_WPN <- raster("dane_gotowe/ndvi_WPN.tif")

tm_shape(ndvi_miedzyzdroje) + tm_raster()
hist(ndvi_miedzyzdroje)

val = values(ndvi_miedzyzdroje)
val <- as.data.frame(val)
val <- na.omit(val)
p = ggplot(as.data.frame(val), aes(x = val)) +
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
Sys.setenv("plotly_username" = "adryanqe")
Sys.setenv("plotly_api_key" = "BWYeEqc9Tcu65gh28WEw")
api_create(p, "NDVI Miedzyzdroje")
