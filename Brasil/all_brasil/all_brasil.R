# Endereço
setwd("C:/Users/Administrador/Documents/Pos/ITA/BID 2/BID 2/Brasil/all_brasil")


library(tidyverse)
library(comtradr)
library(visNetwork)
library(grid)
library(png)
library(RCurl)
library(scales)

library(ggplot2)
library(extrafont)
loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))

# ----- Time -----
ct_get_remaining_hourly_queries()
#_______________________________ 
# verificar o nome do país
#ct_country_lookup("Brazil", "reporter")

#ct_country_lookup("United Kingdom", "partner")

#_______________________________ 


#_______________________________ 
billion <- function(value) {
  value/1000000000
}
# ----- Ano -----
anos <- 2000:2020
