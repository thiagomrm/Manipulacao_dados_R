if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_sifilis_adquirida <- read.dbf("Teste/sifilis_adquirida2.dbf")

inicio <- Sys.time()

dados_sifilis_adquirida <- read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2007-2011.dbf")
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2012.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2013-2017.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2018.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2019.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2020.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2021.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2022.dbf"))
dados_sifilis_adquirida <- rbind(dados_sifilis_adquirida, read.dbf("//arquivos/SINAN/BaseDBF_RD/NINDINET2023.dbf"))

dados_sifilis_adquirida <- subset(dados_sifilis_adquirida, dados_sifilis_adquirida$ID_AGRAVO == "A539" & dados_sifilis_adquirida$SG_UF == "31")

dados_sifilis_adquirida <- dados_sifilis_adquirida %>%
  
  select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, ID_REGIONA, DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA,
         CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI) 

write.xlsx(dados_sifilis_adquirida, "Atualizar/dados_sifilis_adquirida.xlsx")

dados_sifilis_adquirida <- dados_sifilis_adquirida %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    DT_NASC = as.Date(DT_NASC, format = "%d/%m/%Y"),
    
    NU_IDADE_N = if_else( str_sub(NU_IDADE_N,1,1) == "4", as.numeric(str_sub(NU_IDADE_N, 2,4)), 
                          floor(as.integer(DT_NOTIFIC - DT_NASC) / 365.25)),
    
    CS_SEXO = case_when(
      CS_SEXO == "F" ~ "Feminino",
      CS_SEXO == "M" ~ "Masculino"
    ),
    
    CS_RACA = case_when(
      CS_RACA == "1" ~ "Branca",
      CS_RACA == "2" ~ "Preta",
      CS_RACA == "3" ~ "Amarela",
      CS_RACA == "4" ~ "Parda",
      CS_RACA == "5" ~ "Indígena",
      CS_RACA == "9" ~ "Ignorado"
    ),
    
    CS_ESCOL_N = case_when(
      CS_ESCOL_N == "01" ~ "1ª a 4ª série incompleta do EF",
      CS_ESCOL_N == "02" ~ "4ª série completa EF",
      CS_ESCOL_N == "03" ~ "5ª a 8ª série incompleta do EF",
      CS_ESCOL_N == "04" ~ "Ensino fundamental completo",
      CS_ESCOL_N == "05" ~ "Ensino médio incompleto",
      CS_ESCOL_N == "06" ~ "Ensino médio completo",
      CS_ESCOL_N == "07" ~ "Educação superior incompleta",
      CS_ESCOL_N == "08" ~ "Educação superior completa",
      CS_ESCOL_N == "09" ~ "Ignorado",
      CS_ESCOL_N == "10" ~ "Não se aplica"
    )
    
  )

write.xlsx(dados_sifilis_adquirida, "Manipulados/dados_sifilis_adquirida.xlsx")

fim <- Sys.time()

print(fim - inicio)
