if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_sifilis_congenita <- read.dbf("Dados/sifilis_congenita2.dbf")

dados_sifilis_congenita <- read.dbf("//arquivos/SINAN/BaseDBF_RD/SIFICNET.dbf")

dados_sifilis_congenita <- subset(dados_sifilis_congenita, year(dados_sifilis_congenita$DT_DIAG) > 2009)

dados_sifilis_congenita <- subset(dados_sifilis_congenita, str_sub(dados_sifilis_congenita$ID_MN_RESI,1,2) == "31")

dados_sifilis_congenita <- dados_sifilis_congenita %>%
  
  select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, ID_REGIONA, DT_DIAG, DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA,
         CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI, ANTSIFIL_N, TRA_ESQUEM, LABC_IGG, EVOLUCAO) 

write.xlsx(dados_sifilis_congenita, "Atualizar/dados_sifilis_congenita.xlsx")

dados_sifilis_congenita <- dados_sifilis_congenita %>%
  
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
    
    ANTSIFIL_N = case_when(
      ANTSIFIL_N == "1" ~ "Durante o pré-natal",
      ANTSIFIL_N == "2" ~ "No momento do parto/curetagem",
      ANTSIFIL_N == "3" ~ "Após o parto",
      ANTSIFIL_N == "4" ~ "Não realizado",
      ANTSIFIL_N == "9" ~ "Ignorado"
    ),
    
    TRA_ESQUEM = case_when(
      TRA_ESQUEM == "1" ~ "Adequado",
      TRA_ESQUEM == "2" ~ "Inadequado",
      TRA_ESQUEM == "3" ~ "Não realizado",
      TRA_ESQUEM == "9" ~ "Ignorado"
    ),
    
    LABC_IGG = case_when(
      LABC_IGG == "1" ~ "Reagente",
      LABC_IGG == "2" ~ "Não reagente",
      LABC_IGG == "3" ~ "Não realizado",
      LABC_IGG == "4" ~ "Não se aplica",
      LABC_IGG == "9" ~ "Ignorado"
    ),
    
    EVOLUCAO = case_when(
      EVOLUCAO == "1" ~ "Vivo",
      EVOLUCAO == "2" ~ "Óbito por sífilis congênita",
      EVOLUCAO == "3" ~ "Óbito por outras causas",
      EVOLUCAO == "4" ~ "Aborto",
      EVOLUCAO == "5" ~ "Natimorto",
      EVOLUCAO == "9" ~ "Ignorado"
    )
  )

write.xlsx(dados_sifilis_congenita, "Manipulados/dados_sifilis_congenita.xlsx")