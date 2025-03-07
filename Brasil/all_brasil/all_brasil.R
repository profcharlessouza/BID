# Endereço
setwd("C:/Users/Administrador/Documents/Pos/ITA/BID 2/BID2/Brasil/all_brasil")

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

# Habilitar chave do COMTRADE ---------------------------------------------

# Habilitar chave do COMTRADE
#install.packages("usethis")
#library(usethis)
#edit_r_environ()

# Algumas funções ---------------------------------------------------------

billion <- function(value) {
  value/1000000000
}
# ----- Ano -----
anos <- 2014:2024



# Produtos finais ---------------------------------------------------------

all_Brazil_final <-  ct_get_data(
  reporter = "all_countries",
  partner = "BRA",
  flow_direction = c("Import","Export"),
  freq = "A",
  start_date = 2014,
  end_date = 2024,
  commodity_code = c("880100", # Planadores e asas voadoras
                   "880190", # Balões, dirigíveis e outros veículos aéreos, não motorizados
                   "880211", # Helicópteros de peso não superior a 2.000 kg, vazios (sem carga)
                   "880212", # Helicópteros de peso superior a 2.000 kg, vazios (sem carga)
                   "880220", # Aviões e outros veículos aéreos, de peso não superior a 2.000 kg, vazios (sem carga)     
                   "880230", # Aviões e outros veículos aéreos, de peso superior a 2.000 kg, mas não superior a 15.000 kg, vazios (sem carga)
                   "880240", # Aviões e outros veículos aéreos, de peso superior a 15.000 kg, vazios (sem carga)
                   "880260") # Veículos espaciais (incluindo os satélites) e seus veículos de lançamento, e veículos suborbitais
)



write_rds(all_Brazil_final, "all_Brazil_final.rds")



# Subconjuntos ------------------------------------------------------------

all_Brazil_sub <-  ct_get_data(
  reporter = "all_countries",
  partner = "BRA",
  flow_direction = c("Import","Export"),
  freq = "A",
  start_date = 2014,
  end_date = 2024, 
  commodity_code = c("840710", # Motores para aviação 
                   "840890", ## Motores de aeronaves de pistão, de ignição por compressão (motores diesel ou semidiesel)
                   "840910", # Partes de motores para aviação
                   "841191", # Partes de turborreatores ou de turbopropulsores 
                  #"841199", # Partes de turbinas a gás, exceto turbo-jatos ou turbo-hélices
                   "841111", # Turborreatores de  empuxo (impulso*) não superior a 25 kN  
                   "841112", # Turborreatores de empuxo (impulso*) superior a 25 kN
                   "841121", # Turbopropulsores de  potência não superior a 1.100 kW
                   "841122", # Turbopropulsores de  potência superior a 1.100 kW
                  #"841181", # Turbinas a gás, exceto turbo-jatos ou turbo-hélices, de potência não superior a 5.000 kW
                  #"841182", # Turbinas a gás, exceto turbo-jatos ou turbopropulsores, de potência superior a 5.000 kW
                   "841210") ## Propulsores a reação, excluindo os turborreatores 
  
  
) 

write_rds(all_Brazil_sub, "all_Brazil_sub.rds")



# Componentes -------------------------------------------------------------

all_Brazil_comp <-  ct_get_data(
  reporter = "all_countries",
  partner = "BRA",
  flow_direction = c("Import","Export"),
  freq = "A",
  start_date = 2014,
  end_date = 2024, 
  commodity_code = c("401130", # Pneumáticos novos de borracha do tipo utilizado em veículos aéreos
                     "401213", # Pneumáticos recauchutados ou usados de borracha do tipo utilizado em veículos aéreos,
                     "880310", # Hélices e rotores, e suas partes
                     "880320", # Trens de aterrissagem e suas partes
                     "880330", # Outras partes de aviões ou de helicópteros 
                     "880390", # Outras partes para veículos aéreos/espaciais 
                    #"880400", # Paraquedas 
                     "880510", # Aparelhos e dispositivos para lançamento de veículos aéreos, e suas partes; aparelhos e dispositivos para aterrissagem de veículos aéreos em porta-aviões e aparelhos e dispositivos semelhantes, e suas partes
                     "880521", # Aparelhos de treinamento de voo em terra e suas partes: simuladores de combate aéreo e suas partes 
                     "880529", # Aparelhos de treinamento de voo em terra e suas partes: outros
                     "901420", # Instrumentos e aparelhos para navegação aérea ou espacial (exceto bússolas) 
                     "940110") # Assentos do tipo utilizado em veículos aéreos
)

write_rds(all_Brazil_comp, "all_Brazil_comp.rds")


# Total -------------------------------------------------------------------

all_brasil_total <- bind_rows(all_Brazil_final, all_Brazil_sub, all_Brazil_comp, ) %>% 
  mutate( `VC stage` = stringr::str_replace_all(cmd_code, c("880100" = "Final Products",
                                                            "880190" = "Final Products",
                                                            "880211" = "Final Products",
                                                            "880212" = "Final Products",
                                                            "880220" = "Final Products",
                                                            "880230" = "Final Products",
                                                            "880240" = "Final Products",
                                                            "880260" = "Final Products",
                                                            "840710" = "Sub-assemblies",
                                                            "840890" = "Sub-assemblies",
                                                            "840910" = "Sub-assemblies",
                                                            "841191" = "Sub-assemblies",
                                                            #"841199" = "Sub-assemblies",
                                                            "841111" = "Sub-assemblies",
                                                            "841112" = "Sub-assemblies",
                                                            "841121" = "Sub-assemblies",
                                                            "841122" = "Sub-assemblies",
                                                            #"841181" = "Sub-assemblies",
                                                            #"841182" = "Sub-assemblies",
                                                            "841210" = "Sub-assemblies",
                                                            "401130" = "Components",
                                                            "401213" = "Components",
                                                            "880310" = "Components",
                                                            "880320" = "Components",
                                                            "880330" = "Components",
                                                            "880390" = "Components",
                                                            #"880400" = "Components",
                                                            "880510" = "Components",
                                                            "880521" = "Components",
                                                            "880529" = "Components",
                                                            "901420" = "Components",
                                                            "940110" = "Components"))) %>% 
write_rds("all_brasil_total.rds")

writexl::write_xlsx(all_brasil_total, "all_brasil_total.xlsx")

all_brasil_total <- read_rds("all_brasil_total.rds")


# Tabela ------------------------------------------------------------------


Tabela_Brasil <- all_brasil_total %>%
  select(`VC stage`, cmd_code, cmd_desc) %>%
  distinct() %>% 
  rename(HS = cmd_code , Description = cmd_desc)

writexl::write_xlsx(Tabela_Brasil, "Tabela_Brasil.xlsx")
