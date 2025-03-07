# As exportações dos EUA correspondem às importações do Mundo e vice- versa
# Endereço
setwd("C:/Users/Administrador/Documents/Pos/ITA/BID 2/BID2/Brasil/brasil_all")

# Pacotes -----------------------------------------------------------------

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



# Funções -----------------------------------------------------------------

billion <- function(value) {
  value/1000000000
}

million <- function(value) {
  value/1000000
}
## ----- Ano -----
anos <- 2014:2024

# Selecionar variáveis ----------------------------------------------------
all_brasil_total <- read_rds("C:/Users/Administrador/Documents/Pos/ITA/BID 2/BID2/Brasil/all_brasil/all_brasil_total.rds")

all_brasil <- all_brasil_total %>% 
  select(period,primary_value,reporter_desc,reporter_iso,partner_desc,partner_iso,flow_desc, cmd_code, cmd_desc, `VC stage` )

# Permutar  Export por Import ---------------------------------------------

brasil_all_1 <- all_brasil %>% 
  mutate(flow_desc = stringr::str_replace_all(flow_desc, c(
    "Export" = "EEE",
    "Import" = "III"
  )))

brasil_all_2 <- brasil_all_1 %>% 
  mutate(flow_desc = stringr::str_replace_all(flow_desc, c(
    "EEE" = "Import",
    "III" = "Export"
  )))


# Permutar reporter  por partner  --------------

brasil_all_3 <- brasil_all_2 %>% 
  rename(PARTNER = reporter_desc,
         PARTNER_ISO = reporter_iso)

brasil_all <- brasil_all_3 %>% 
  rename(reporter = partner_desc,
         reporter_iso = partner_iso,
         partner = PARTNER,
         partner_iso = PARTNER_ISO)

write_rds(brasil_all, "brasil_all.rds")

writexl::write_xlsx(brasil_all, "brasil_all.xlsx")


# Balança comercial ----------------------------------------------------------------

## Group Brasil all --------------------------------------------------------

# Grandes grupos ("`VC stage`") por ano

group_brasil_all <- brasil_all %>% #distinct() %>% 
  mutate(primary_value = billion(primary_value)) %>% 
  group_by(flow_desc, period, `VC stage`) %>% 
  # filter(flow_desc != 'Import') %>% 
  summarise(primary_value = sum(primary_value, na.rm = T))


# Salvando
write_rds(group_brasil_all, "group_brasil_all.rds")
group_brasil_all <- read_rds("group_brasil_all.rds")

writexl::write_xlsx(group_brasil_all,  "group_brasil_all.xlsx"
           ,col_names = TRUE, format_headers = TRUE)


## ----- VC stage por ano ----
# saldo comercial

balanco_brasil_all_a <- pivot_wider(group_brasil_all, names_from = "flow_desc", values_from = "primary_value") %>% 
  mutate(`Balance` = Export-Import)

balanco_brasil_all <- balanco_brasil_all_a %>% 
  pivot_longer(cols = c("Export", "Import", "Balance"), names_to = "flow_desc", values_to = "primary_value")

write_rds(balanco_brasil_all, "balanco_brasil_all.rds")

balanco_brasil_all <- read_rds("balanco_brasil_all.rds")


# Imagens -----------------------------------------------------------------

##----- Cor das barras ----

#cor <- c("#bbbbbb","#767676", "#949494")
cor <- c("#c3d4df","#5d8aa8", "#90afc3")

##----- Ordenar as colunas ----
group_brasil_all$`VC stage` <- factor(group_brasil_all$`VC stage`,c('Final Products','Sub-assemblies','Components'))
balanco_brasil_all$`VC stage` <- factor(balanco_brasil_all$`VC stage`,c('Final Products','Sub-assemblies','Components'))



##-----ggplot -----


ggplot(group_brasil_all, aes(period), ylim(-3:6)) + 
  geom_bar(data = subset(group_brasil_all, flow_desc == "Export"), 
           aes(y = primary_value, fill = `VC stage`), stat = "identity", position = "dodge") +
  geom_bar(data = subset(group_brasil_all, flow_desc == "Import"), 
           aes(y = -primary_value, fill = `VC stage`), stat = "identity", position = "dodge") + 
  geom_point(data = subset(balanco_brasil_all, flow_desc == "Balance"),
             aes(y = primary_value,col=`VC stage`), stat = "identity",size = 3.5,position = position_jitterdodge( jitter.width = 0,
                                                                                                                 jitter.height = 0,
                                                                                                                 dodge.width = .91,
                                                                                                                 seed = NA)) + 
  geom_hline(yintercept = 0,colour = "goldenrod2")+  
  # scale_fill_manual(values=cor) + 
  theme_bw() +
  #theme(plot.background = element_rect(color="goldenrod2", size=2))
  theme(text=element_text(family="Times", size=25), legend.text=element_text(size=25)) +
  theme(legend.position = "bottom")+
 # scale_x_continuous(limits = c(2014, 2024),breaks = seq(2014, 2024, 1), expand = c(0.000, 0.000)) +
  scale_fill_manual(values = c("Final Products"="#54809d", "Sub-assemblies"="#83a6bd","Components"="#b6cbd8")) +
  scale_colour_manual(values = c("Final Products" = "goldenrod2", "Sub-assemblies" = "goldenrod2", "Components"="goldenrod2"), label = c("Final Products"="Trade Balance"), name="",breaks = unique("Final Products")) +
  labs(title = "", x = "", y = "US$ Billion",caption = "", color = "", fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust=1)) +
  theme(axis.line.x = element_line(color = "gray"))


