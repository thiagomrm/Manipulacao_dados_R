if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

dados_hiv_gestante <- read.dbf("//arquivos/SINAN/BaseDBF_RD/HIVGENET.dbf")

dados_hiv_gestante <- subset(dados_hiv_gestante, year(dados_hiv_gestante$DT_DIAG) > 2009 & str_sub(dados_hiv_gestante$ID_MN_RESI,1,2) == "31")

dados_hiv_gestante <- dados_hiv_gestante %>%
  
  select(NU_NOTIFIC,	ID_AGRAVO,	DT_NOTIFIC,	NU_ANO,	SG_UF_NOT,	ID_MUNICIP,	ID_REGIONA,	DT_DIAG,
         DT_NASC,	NU_IDADE_N,	CS_RACA,	CS_ESCOL_N,	SG_UF,	ID_MN_RESI,	ID_RG_RESI,	PRE_PRENAT,	PRE_ANTRET,
         PRE_MUNIPA,	PAR_TIPO,	PAR_ANTIDU,	PAR_EVOLUC,	PAR_INICPR )

write.xlsx(dados_hiv_gestante, "Atualizar/dados_hiv_gestante.xlsx")

dados_hiv_gestante <- dados_hiv_gestante %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    DT_DIAG = as.Date(DT_DIAG, format = "%d/%m/%Y"),
    
    NU_IDADE_N = if_else( str_sub(NU_IDADE_N,1,1) == "4", as.numeric(str_sub(NU_IDADE_N, 2,4)), 
                          floor(as.integer(DT_NOTIFIC - DT_NASC) / 365.25)),
    
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
    
    PRE_PRENAT = case_when(
      PRE_PRENAT == "1" ~ "Sim",
      PRE_PRENAT == "2" ~ "Não",
      PRE_PRENAT == "9" ~ "Ignorado"
    ),
    
    PRE_ANTRET = case_when(
      PRE_ANTRET == "1" ~ "Sim",
      PRE_ANTRET == "2" ~ "Não",
      PRE_ANTRET == "9" ~ "Ignorado"
    ),
    
    PAR_TIPO = case_when(
      PAR_TIPO == "1" ~ "Vaginal",
      PAR_TIPO == "2" ~ "Cesárea eletiva",
      PAR_TIPO == "3" ~ "Cesárea de urgência",
      PAR_TIPO == "4" ~ "Não se aplica"
    ),
    
    PAR_ANTIDU = case_when(
      PAR_ANTIDU == "1" ~ "Sim",
      PAR_ANTIDU == "2" ~ "Não",
      PAR_ANTIDU == "9" ~ "Ignorado"
    ),
    
    PAR_EVOLUC = case_when(
      PAR_EVOLUC == "1" ~ "Nascido vivo",
      PAR_EVOLUC == "2" ~ "Natimorto",
      PAR_EVOLUC == "3" ~ "Aborto",
      PAR_EVOLUC == "4" ~ "Não se aplica"
    ),
    
    PAR_INICPR = case_when(
      PAR_INICPR == "1" ~ "Nas primeiras 24h",
      PAR_INICPR == "2" ~ "Após 24h do nascimento",
      PAR_INICPR == "3" ~ "Não se aplica ",
      PAR_INICPR == "4" ~ "Não realizado",
      PAR_INICPR == "9" ~ "Ignorado"
    )
    
  )

write.xlsx(dados_hiv_gestante, "Manipulados/dados_hiv_gestante.xlsx")
  
  