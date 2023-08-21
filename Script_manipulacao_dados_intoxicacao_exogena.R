if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

dados_intoxicacao_exogena <- read.dbf("//arquivos/SINAN/BaseDBF_RD/IEXOGNET.dbf")

#dados_intoxicacao_exogena <- read.dbf("Dados/intoxicacao_exogena2.dbf")

dados_intoxicacao_exogena <- subset(dados_intoxicacao_exogena, dados_intoxicacao_exogena$CIRCUNSTAN == "10" & 
                                      year(dados_intoxicacao_exogena$DT_NOTIFIC) > 2009)

dados_intoxicacao_exogena <- subset(dados_intoxicacao_exogena, str_sub(dados_intoxicacao_exogena$ID_MN_RESI,1,2) == "31")

dados_intoxicacao_exogena <- dados_intoxicacao_exogena %>%
  
  select(TP_NOT, ID_AGRAVO, DT_NOTIFIC, DT_NASC, NU_IDADE_N, CS_SEXO, CS_RACA, ID_MN_RESI, ID_RG_RESI, LOC_EXPO, AGENTE_TOX,
         TPEXP, CLASSI_FIN) 

write.xlsx(dados_intoxicacao_exogena,"Atualizar/dados_intoxicacao_exogena.xlsx")

dados_intoxicacao_exogena <- dados_intoxicacao_exogena %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format(DT_NOTIFIC, "%d/%m/%Y"), locale = "pt_BR"),
    
    DT_NASC = as.Date(DT_NASC, format(DT_NASC, "%d/%m/%Y"), locale = "pt_BR"),
    
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
    
    LOC_EXPO = case_when(
      LOC_EXPO == "1" ~ "Residência",
      LOC_EXPO == "2" ~ "Ambiente de trabalho",
      LOC_EXPO == "3" ~ "Trajeto do trabalho",
      LOC_EXPO == "4" ~ "Serviços de saúde",
      LOC_EXPO == "5" ~ "Escola/creche",
      LOC_EXPO == "6" ~ "Ambiente Externo",
      LOC_EXPO == "7" ~ "Outro",
      LOC_EXPO == "9" ~ "Ignorado",
    ),
    
    AGENTE_TOX = case_when(
      AGENTE_TOX == "01" ~ "Medicamento",
      AGENTE_TOX == "02" ~ "Agrotóxico/uso agrícola",
      AGENTE_TOX == "03" ~ "Agrotóxico/uso doméstico",
      AGENTE_TOX == "04" ~ "Agrotóxico/uso saúde pública",
      AGENTE_TOX == "05" ~ "Raticida",
      AGENTE_TOX == "06" ~ "Produto Veterinário",
      AGENTE_TOX == "07" ~ "Produto de uso domiciliar",
      AGENTE_TOX == "08" ~ "Cosmético/higiene pessoal ",
      AGENTE_TOX == "09" ~ "Produto químico de uso industrial",
      AGENTE_TOX == "10" ~ "Metal",
      AGENTE_TOX == "11" ~ "Drogas de abuso",
      AGENTE_TOX == "12" ~ "Planta tóxica",
      AGENTE_TOX == "13" ~ "Alimento e bebida",
      AGENTE_TOX == "14" ~ "Outro",
      AGENTE_TOX == "99" ~ "Ignorado"
    ),
    
    TPEXP = case_when(
      TPEXP == "1" ~ "Aguda - única",
      TPEXP == "2" ~ "Aguda - repetida",
      TPEXP == "3" ~ "Crônica",
      TPEXP == "4" ~ "Aguda sobre crônica",
      TPEXP == "9" ~ "Ignorado"
    ),
    
    CLASSI_FIN = case_when(
      CLASSI_FIN == "1" ~ "Intoxicação Confirmada",
      CLASSI_FIN == "2" ~ "Exposição",
      CLASSI_FIN == "3" ~ "Reação adversa",
      CLASSI_FIN == "4" ~ "Diagnóstico diferencial",
      CLASSI_FIN == "5" ~ "Síndrome de abstinência",
      CLASSI_FIN == "9" ~ "Ignorado"
    )
  )

write.xlsx(dados_intoxicacao_exogena,"Manipulados/dados_intoxicacao_exogena.xlsx")
