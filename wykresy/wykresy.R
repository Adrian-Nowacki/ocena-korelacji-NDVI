library(tmap)
library(raster)
library(rgdal)
library(ggplot2)
library(plotly)
library(spatialEco)
library(dplyr)

# wczytanie rastrów
ndvi_miedzyzdroje <- raster("dane/miedzyzdroje_ndvi.tif")
temp_miedzyzdroje <- raster("dane/miedzyzdroje_temp.tif")
ndvi_WPN <- raster("dane/WPN_ndvi.tif")
temp_WPN <- raster("dane/WPN_temp.tif")

# podglad warstwy
tm_shape(temp_miedzyzdroje) + tm_raster()

# zapisanie rastra do ramki danych
ndvi_miedzyzdroje_dt <- values(ndvi_miedzyzdroje)
ndvi_miedzyzdroje_dt <- as.data.frame(ndvi_miedzyzdroje_dt) %>% na.omit()
colnames(ndvi_miedzyzdroje_dt) <- "ndvi"

temp_miedzyzdroje_dt <- values(temp_miedzyzdroje)
temp_miedzyzdroje_dt <- as.data.frame(temp_miedzyzdroje_dt) %>% na.omit
colnames(temp_miedzyzdroje_dt) <- "temp"

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
  axis.title = element_text(size = 9,
                            color = "#dddddd"), 
  plot.title = element_text(size = 11,
                            color = "#dddddd",
                            vjust = 2,
                            hjust = 0.5), 
  legend.background = element_rect(color = "#222222", 
                                   fill = "#777777"),  
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 9),
  axis.text = element_text(size = 9, 
                           color = "#dddddd"),
  axis.text.x = element_text(angle = 60, vjust = 0.95, hjust=1))


# wyodrebnienie polowy danych ze wzgledu na zbyt duzy rozmiar pliku do eksportu

# ndvi_WPN_dt <- ndvi_WPN_dt[seq(1, nrow(ndvi_WPN_dt), 2), ]
# ndvi_WPN_dt <- as.data.frame(ndvi_WPN_dt)
# 
# temp_WPN_dt <- temp_WPN_dt[seq(1, nrow(temp_WPN_dt), 2), ]
# temp_WPN_dt <- as.data.frame(temp_WPN_dt)
# 


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
      labs(title = "Rozkład wartości wskaźnika NDVI dla Międzyzdrojów", x = "Wskaźnik NDVI", y = "Liczba") +
      theme(plot.title = element_text(size = 11, hjust = 0.5)),
    tooltip = c("text"))
  hist_ndvi_miedzyzdroje <- hist_ndvi_miedzyzdroje %>%
    config(displayModeBar = FALSE)
  
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
      labs(title = "Rozkład wartości wskaźnika NDVI dla WPN", x = "Wskaźnik NDVI", y = "Liczba") +
      theme(plot.title = element_text(size = 11, hjust = 0.5)),
    tooltip = c("text"))
  hist_ndvi_wpn <- hist_ndvi_wpn %>%
    config(displayModeBar = FALSE)
  
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
                                              '<span>', round(x, 1) ,'</span>'))) + 
      geom_bar(stat="identity", fill = hcl.colors(40, palette = "viridis")) + style +
      labs(title = "Rozkład wartości temperatury dla WPN", x = "Temperatura [°C]", y = "Liczba") + 
      theme(plot.title = element_text(size = 11, hjust = 0.5)),
    tooltip = c("text")) %>%
    config(displayModeBar = FALSE)
  hist_temp_wpn 
  
 


  # stworzenie histogramu temp dla Miedzyzdrojow
  # hist_temp_miedzyzdroje = ggplot(as.data.frame(temp_miedzyzdroje_dt), aes(x = temp_miedzyzdroje_dt)) +
  #   geom_histogram(fill = hcl.colors(40, palette = "viridis"), bins = 40) +
  #   labs(x = "Temperatura [°C]", y = "Liczba") +
  #   theme(plot.title = element_text(hjust = 0.5, face = "bold")) + style
  # ggplotly(hist_temp_miedzyzdroje)
  
  hist_temp_miedzyzdroje <- ggplotly(
    ggplot_build(
      temp_miedzyzdroje_dt %>% 
        ggplot(aes(x=temp)) +
        geom_histogram(bins = 40)
    )$data[[1]] %>% 
      ggplot(aes(x=x, y = count, text = paste('<span style = " font-weight:bold"> Liczba: </span>',
                                              '<span>', count ,'</span>',
                                              '</br></br><span style = " font-weight:bold"> Temperatura [°C]: </span>',
                                              '<span>', round(x, 1) ,'</span>'))) + 
      geom_bar(stat="identity", fill = hcl.colors(40, palette = "viridis")) + style +
      labs(title = "Rozkład wartości temperatury dla Międzyzdrojów", x = "Temperatura [°C]", y = "Liczba") + 
      theme(plot.title = element_text(size = 11, hjust = 0.5)),
    tooltip = c("text"))
  hist_temp_miedzyzdroje <- hist_temp_miedzyzdroje %>%
    config(displayModeBar = FALSE)
  
  hist_temp_miedzyzdroje  
  
  
  
  
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
  geom_point(shape = 21, size = 2, show.legend = FALSE) + 
  scale_x_continuous(breaks = c(20, 25, 30, 35)) +
  geom_smooth(method = lm, color="darkred") + 
  labs(title = "Korelacja wskaźnika NDVI z temperaturą gruntu dla Międzydzrojów", 
       x = "Temperatura [°C]",
       y = "NDVI") +
  geom_abline(color = 'grey') +  style + scale_color_gradient2(midpoint=mean(dane_miasto$ndvi), low="#e63900", mid = "#ffff66",  high="#00802b")
  
korelacja_miedzyzdroje <- korelacja_miedzyzdroje %>% 
  style(text = paste('<span style = " font-weight:bold"> Wartość NDVI: </span>',
                     '<span style = " color:#ffffff">', dane_miasto$ndvi ,'</span>',
                     '</br></br><span style = "font-weight:bold"> Temperatura [°C]: </span>',
                     '<span style = " color:#ffffff">', dane_miasto$temp ,'</span>'), traces = 1) %>%
  style(text = "linia regresji", traces = 2)

korelacja_miedzyzdroje <- ggplotly(korelacja_miedzyzdroje) %>%
  config(displayModeBar = FALSE)
korelacja_miedzyzdroje
  


dane_wpn<- round(temp_WPN_dt, 1)
dane_wpn$ndvi <- round(ndvi_WPN_dt$ndvi, 2)
object.size(dane_wpn)


# wyodrebnienie polowy danych ze wzgledu na zbyt duzy rozmiar pliku do eksportu
dane_wpn <- as.data.frame(dane_wpn)
dane_wpn <- dane_wpn[seq(1, nrow(dane_wpn), 45.5), ]
dane_wpn <- as.data.frame(dane_wpn)
colnames(dane_wpn) <- c("temp", "ndvi")

korelacja_wpn <- ggplot(dane_wpn, aes(x = temp, y = ndvi, color = ndvi)) + 
  geom_point(shape = 21, size = 2, show.legend = FALSE) +
  labs(title = "Korelacja wskaźnika NDVI z temperaturą gruntu dla WPN", 
       x = "Temperatura [°C]",
       y = "NDVI") +
  geom_smooth(method = lm, color="darkred") + 
  geom_abline(color = 'grey') +  style + scale_color_gradient2(midpoint= mean(dane_miasto$ndvi), low="#e63900", mid = "#ffff66",  high="#00802b")

korelacja_wpn <- korelacja_wpn %>% 
  style(text = paste('<span style = " font-weight:bold"> Wartość NDVI: </span>',
                     '<span style = " color:#ffffff">', dane_wpn$ndvi ,'</span>',
                     '</br></br><span style = "font-weight:bold"> Temperatura [°C]: </span>',
                     '<span style = " color:#ffffff">', dane_wpn$temp ,'</span>'), traces = 1) %>%
  style(text = "linia regresji", traces = 2)

korelacja_wpn <- ggplotly(korelacja_wpn) %>%
  config(displayModeBar = FALSE)

korelacja_wpn
#object.size(korelacja_wpn)
####probne

# 
# korelacja_miedzyzdroje_pr <- ggplotly(
#   ggplot_build(
#     dane_miasto %>% 
#       ggplot(aes(x=temp, y = ndvi)) +
#       geom_point(shape = 21, size = 2, show.legend = FALSE)
#   )$data[[1]] %>% 
#     ggplot(aes(x=x, y = y, text = paste('<span style = " font-weight:bold"> Liczba: </span>',
#                                             '<span>', x ,'</span>',
#                                             '</br></br><span style = " font-weight:bold"> Temperatura [°C]: </span>',
#                                             '<span>', y ,'</span>'))) + 
#     geom_smooth(method = lm, color="darkred") + 
#     geom_abline(color = 'grey') +  style + scale_color_gradient2(midpoint=mean(y), low="#e63900", mid = "#ffff66",  high="#00802b") +
#     labs(title = "Rozkład wartości temperatury dla Międzyzdrojów", x = "Temperatura [°C]", y = "NDVI") + 
#     theme(plot.title = element_text(size = 14, hjust = 0.5)),
#   tooltip = c("text"))
# korelacja_miedzyzdroje_pr











object.size(korelacja_miedzyzdroje)

### Eksport wykresow do platformy plotly, aby zamiescic w przegladarce

  Sys.setenv("plotly_username" = "adryanqe")
  Sys.setenv("plotly_api_key" = "BWYeEqc9Tcu65gh28WEw")
  api_create(hist_temp_wpn, "Temperatura WPN")











############ STATYSTYKI STREFOWE WARSTW CLC


##### NDVI WPN
stats <- function(x){
  paste("Srednia:", cellStats(x, mean), '   ', 
        "Minimum:", cellStats(x, min), '    ', 
        "Maximum:", cellStats(x, max),
        "Maximum:", cellStats(x, sd))
}
stats(ndvi_WPN)

##### NDVI Miedzyzdroje
stats(ndvi_miedzyzdroje)
##### Temperatura WPN
stats(temp_WPN)
##### Temperatura Miedzyzdroje
stats(temp_miedzyzdroje)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)


ndvi <- read.csv("statystyki/statystyki_NDVI.csv", sep = ';', encoding = "UTF-8")
temp <- read.csv("statystyki/statystyki_temperatura.csv", sep = ';', encoding = "UTF-8")

ndvi <- ndvi %>% mutate_if(is.numeric, round, 2)
temp <- temp %>% mutate_if(is.numeric, round, 1)

ndvi <- ndvi[, -c(1, 3, 5, 9)]
temp <- temp[, -c(1, 3, 5, 9)]

colnames(ndvi) <- c("pokrycie_terenu", "srednia_ndvi", "odchylenie_ndvi", "min_ndvi", "max_ndvi")

stats <- merge(temp, ndvi, by = "pokrycie_terenu")

stats <- stats2


a <- ggplot(stats, aes(x = reorder(pokrycie_terenu, -srednia_temp), y = srednia_temp, fill = srednia_temp)) + geom_col() + style +
  labs(x = "siema",
       y = "piwo",
       title = "duzo piwa")

ggplotly(a)


#statystyki temperatur
miasto_temp_30 <- as.data.frame(temp_miedzyzdroje_dt[temp_miedzyzdroje_dt$temp > 30,])

wpn_temp_30 <- as.data.frame(temp_WPN_dt[temp_WPN_dt$temp >30,])

powierzchnia_temp <- function(){
  paste(cat(c("% pow. Miedzyzdrojow powyżej 30:", round((nrow(miasto_temp_30)/nrow(temp_miedzyzdroje_dt)) * 100, 2), "",
        "pow. Miedzyzdrojow powyżej 30:", round((nrow(miasto_temp_30) * 900)/1000000, 3), "",
        "% pow. WPN powyżej 30:", round((nrow(wpn_temp_30)/nrow(temp_WPN_dt)) * 100, 2), "",
        "pow. WPN powyżej 30:", round((nrow(wpn_temp_30) * 900)/1000000, 3), ""), sep = "\n"))
}

powierzchnia_temp()


miasto_ndvi_02 <- as.data.frame(ndvi_miedzyzdroje_dt[ndvi_miedzyzdroje_dt$ndvi < 0.2,])

wpn_ndvi_02 <- as.data.frame(ndvi_WPN_dt[ndvi_WPN_dt$ndvi < 0.2,])


powierzchnia_ndvi <- function(){
  paste(cat(c("% pow. Miedzyzdrojow poniżej 0.2:", round((nrow(miasto_ndvi_23)/nrow(ndvi_miedzyzdroje_dt)) * 100, 2), "",
              "pow. Miedzyzdrojow ponizej 0.2:", round((nrow(miasto_temp_23) * 900)/1000000, 3), "",
              "% pow. WPN poniżej 0.2:", round((nrow(wpn_temp_23)/nrow(temp_WPN_dt)) * 100, 2), "",
              "pow. WPN ponizej 0.2:", round((nrow(wpn_temp_23) * 900)/1000000, 3), ""), sep = "\n"))
}

powierzchnia_ndvi()
