if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_sifilis_gestante <- read.dbf("Dados/sifilis_gestante2.dbf")

dados_sifilis_gestante <- read.dbf("//arquivos/SINAN/BaseDBF_RD/SIFGENET.dbf")

dados_sifilis_gestante <- subset(dados_sifilis_gestante, year(dados_sifilis_gestante$DT_DIAG) > 2009 & str_sub(dados_sifilis_gestante$ID_MN_RESI,1,2) == "31")

dados_sifilis_gestante <- dados_sifilis_gestante %>%
  
  select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, ID_REGIONA, DT_DIAG, DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA,
         CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI, TPESQUEMA, TRATPARC) 

write.xlsx(dados_sifilis_gestante, "Atualizar/dados_sifilis_gestante.xlsx")

dados_sifilis_gestante <- dados_sifilis_gestante %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    DT_DIAG = as.Date(DT_DIAG, format = "%d/%m/%Y"),
    
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
    ),
    
    TPESQUEMA = case_when(
      TPESQUEMA == "1" ~ "Penicilina G benzatina 2.400.000UI",
      TPESQUEMA == "2" ~ "Penicilina G benzatina 4.800.000UI",
      TPESQUEMA == "3" ~ "Penicilina G benzatina 7.200.000UI",
      TPESQUEMA == "4" ~ "Outro esquema",
      TPESQUEMA == "5" ~ "Não realizado",
      TPESQUEMA == "9" ~ "Ignorado"
    ),
    
    TRATPARC = case_when(
      TRATPARC == "1" ~ "Sim",
      TRATPARC == "2" ~ "Não",
      TRATPARC == "9" ~ "Ignorado"
    )
  )

write.xlsx(dados_sifilis_gestante, "Manipulados/dados_sifilis_gestante.xlsx")