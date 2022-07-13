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
ndvi_WPN <- raster("ndvi_probne.tif")
temp_WPN <- raster("temp_probne.tif")

# podglad warstwy
tm_shape(ndvi_miedzyzdroje) + tm_raster()

# zapisanie rastra do ramki danych
ndvi_miedzyzdroje_dt <- values(ndvi_miedzyzdroje)
ndvi_miedzyzdroje_dt <- as.data.frame(ndvi_miedzyzdroje_dt) %>% na.omit()
colnames(ndvi_miedzyzdroje_dt) <- "ndvi"

temp_miedzyzdroje_dt <- values(temp_miedzyzdroje)
temp_miedzyzdroje_dt <- as.data.frame(temp_miedzyzdroje_dt) %>% na.omit

ndvi_WPN_dt <- values(ndvi_WPN)
ndvi_WPN_dt <- as.data.frame(ndvi_WPN_dt) %>% na.omit()
colnames(ndvi_WPN_dt) <- "ndvi"

temp_WPN_dt <- values(temp_WPN)
temp_WPN_dt <- as.data.frame(temp_WPN_dt) %>% na.omit()
colnames(temp_WPN_dt) <- "temp"

# styl wykresow
  style <- theme(
  text = element_text(family = "Trebuchet MS"),
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


# wyodrebnienie polowy danych ze wzgledu na zbyt duzy rozmiar pliku do eksportu

ndvi_WPN_dt <- ndvi_WPN_dt[seq(1, nrow(ndvi_WPN_dt), 2), ]
ndvi_WPN_dt <- as.data.frame(ndvi_WPN_dt)

temp_WPN_dt <- temp_WPN_dt[seq(1, nrow(temp_WPN_dt), 2), ]
temp_WPN_dt <- as.data.frame(temp_WPN_dt)



### Histogramy NDVI

# stworzenie histogramu NDVI dla Miedzyzdrojow
  # histogram_miedzyzdroje = ggplot(ndvi_miedzyzdroje_dt, aes(ndvi)) +
  #   geom_histogram(fill = hcl.colors(40, palette = "RdYlGn"), bins = 40) +
  #   labs(title = "piwo", x = "Wskaznik NDVI", y = "Liczba") +
  #   theme(plot.title = element_text(hjust = 0.5, face = "bold")) + style
  # 
  # ggplotly(histogram_miedzyzdroje)
  # 
  # 

  # Stworzenie histogramu NDVI dla Miedzyzdrojow
  hist_ndvi_miedzyzdroje <- ggplotly(
    ggplot_build(
      ndvi_miedzyzdroje_dt %>% 
        ggplot(aes(x=ndvi)) +
        geom_histogram(bins = 40)
    )$data[[1]] %>% 
      ggplot(aes(x=x, y = count, text = paste('<span style = " font-weight:bold"> Liczba: </span>',
                                              '<span>', count ,'</span>',
                                              '</br></br><span style = " font-weight:bold"> Wartość NDVI: </span>',
                                              '<span>', round(x, 2) ,'</span>'))) + 
      geom_bar(stat="identity", fill = hcl.colors(40, palette = "RdYlGn")) + style +
      labs(title = "piwo", x = "Wskaźnik NDVI", y = "Liczba") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")),
    tooltip = c("text"))
  hist_ndvi_miedzyzdroje
  
  # hist_ndvi_WPN = ggplot(as.data.frame(ndvi_WPN_dt), aes(x = ndvi_WPN_dt)) +
  #   geom_histogram(fill = hcl.colors(40, palette = "RdYlGn"), bins = 40) +
  #   labs(x = "NDVI", y = "Liczba") +
  #   theme(plot.title = element_text(hjust = 0.5, face = "bold")) + style
  # ggplotly(hist_ndvi_WPN)

  # stworzenie histogramu NDVI dla WPN
  hist_ndvi_wpn <- ggplotly(
    ggplot_build(
      ndvi_WPN_dt %>% 
        ggplot(aes(x=ndvi)) +
        geom_histogram(bins = 40)
    )$data[[1]] %>% 
      ggplot(aes(x=x, y = count, text = paste('<span style = " font-weight:bold"> Liczba: </span>',
                                              '<span>', count ,'</span>',
                                              '</br></br><span style = " font-weight:bold"> Wartość NDVI: </span>',
                                              '<span>', round(x, 2) ,'</span>'))) + 
      geom_bar(stat="identity", fill = hcl.colors(40, palette = "RdYlGn")) + style +
      labs(title = "piwo", x = "Wskaźnik NDVI", y = "Liczba") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")),
    tooltip = c("text"))
  hist_ndvi_wpn
  
  
  
  
  
### Histogramy temperatury
  
# stworzenie histogramu temperatury dla WPN
  # hist_temp_WPN = ggplot(as.data.frame(temp_WPN_dt), aes(x = temp_WPN_dt)) +
  #   geom_histogram(fill = hcl.colors(40, palette = "viridis"), bins = 40) +
  #   labs(x = "Temperatura [°C]", y = "Liczba") +
  #   theme(plot.title = element_text(hjust = 0.5, face = "bold")) + style
  # ggplotly(hist_temp_WPN)
  # 
  hist_temp_wpn <- ggplotly(
    ggplot_build(
      temp_WPN_dt %>% 
        ggplot(aes(x=temp)) +
        geom_histogram(bins = 40)
    )$data[[1]] %>% 
      ggplot(aes(x=x, y = count, text = paste('<span style = " font-weight:bold"> Liczba: </span>',
                                              '<span>', count ,'</span>',
                                              '</br></br><span style = " font-weight:bold"> Temperatura [°C]: </span>',
                                              '<span>', round(x, 2) ,'</span>'))) + 
      geom_bar(stat="identity", fill = hcl.colors(40, palette = "viridis")) + style +
      labs(title = "piwo", x = "Wskaźnik NDVI", y = "Liczba") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold")),
    tooltip = c("text"))
  hist_temp_wpn
  
  
  
  
  
  
# stworzenie histogramu temp dla Miedzyzdrojow
  hist_temp_miedzyzdroje = ggplot(as.data.frame(temp_miedzyzdroje_dt), aes(x = temp_miedzyzdroje_dt)) +
    geom_histogram(fill = hcl.colors(40, palette = "viridis"), bins = 40) +
    labs(x = "Temperatura [°C]", y = "Liczba") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) + style
  ggplotly(hist_temp_miedzyzdroje)
  

  
  
  
  
  
  
######## KORELACJE NDVI Z TEMPERATURA GRUNTU
  
# korelacja dla obszaru WPN
cor(temp_WPN_dt, ndvi_WPN_dt, method = "pearson")



# korelacja dla obszaru Miedzyzdrojow
cor(temp_miedzyzdroje_dt, ndvi_miedzyzdroje_dt, method = "pearson")
dane <- temp_WPN_dt
dane$ndvi <- ndvi_WPN_dt$ndvi_WPN_dt

dane_miasto <- round(temp_miedzyzdroje_dt, 1)
dane_miasto$ndvi <- round(ndvi_miedzyzdroje_dt$ndvi, 2)
object.size(dane_miasto)


# wyodrebnienie polowy danych ze wzgledu na zbyt duzy rozmiar pliku do eksportu
dane_miasto <- as.data.frame(dane_miasto)
dane_miasto <- dane_miasto[seq(1, nrow(dane_miasto), 3.2), ]
dane_miasto <- as.data.frame(dane_miasto)
colnames(dane_miasto) <- c("temp", "ndvi")

korelacja_miedzyzdroje <- ggplot(dane_miasto, aes(x = temp, y = ndvi, color = ndvi)) + 
  geom_point(shape = 21, size = 2, show.legend = FALSE) + xlab("Temperatura [°C]") + 
  ylab("NDVI") + 
  scale_x_continuous(breaks = c(20, 25, 30, 35)) +
  geom_smooth(method = lm, color="darkred") + 
  geom_abline(color = 'grey') +  style + scale_color_gradient2(midpoint=mean(dane_miasto$ndvi), low="#e63900", mid = "#ffff66",  high="#00802b")
  
korelacja_miedzyzdroje <- korelacja_miedzyzdroje %>% 
  style(text = paste('<span style = " font-weight:bold"> Wartość NDVI: </span>',
                     '<span style = " color:#ffffff">', dane_miasto$ndvi ,'</span>',
                     '</br></br><span style = "font-weight:bold"> Temperatura [°C]: </span>',
                     '<span style = " color:#ffffff">', dane_miasto$temp ,'</span>'), traces = 1) %>%
  style(text = "linia regresji", traces = 2)

ggplotly(korelacja_miedzyzdroje)

  

object.size(korelacja_miedzyzdroje)

### Eksport wykresow do platformy plotly, aby zamiescic w przegladarce

Sys.setenv("plotly_username" = "adryanqe")
Sys.setenv("plotly_api_key" = "BWYeEqc9Tcu65gh28WEw")
api_create(korelacja_miedzyzdroje, "Korelacja Miedzyzdroje")











############ STATYSTYKI STREFOWE WARSTW CLC


##### NDVI WPN
stats <- function(x){
  paste("Srednia:", cellStats(x, mean), '   ', 
        "Minimum:", cellStats(x, min), '    ', 
        "Maximum:", cellStats(x, max))
}
stats(ndvi_WPN)

##### NDVI Miedzyzdroje
stats(ndvi_miedzyzdroje)
##### Temperatura WPN
stats(temp_WPN)
##### Temperatura Miedzyzdroje
stats(temp_miedzyzdroje)

plot(ndvi_miedzyzdroje)


