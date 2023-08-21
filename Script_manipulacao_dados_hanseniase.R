if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_hanseniase <- read.dbf("Dados/hanseniase2.dbf")

dados_hanseniase <- read.dbf("//arquivos/SINAN/BaseDBF_RD/HANSNET.dbf")

dados_hanseniase <- subset(dados_hanseniase, year(dados_hanseniase$DT_NOTIFIC) > 2009)

dados_hanseniase <- subset(dados_hanseniase, str_sub(dados_hanseniase$ID_MN_RESI,1,2) == "31")

dados_hanseniase <- dados_hanseniase %>%
  
  select(NU_NOTIFIC, DT_NOTIFIC, NU_ANO, ID_MUNICIP, ID_REGIONA, ID_UNIDADE, DT_DIAG, SEM_DIAG, DT_NASC, NU_IDADE_N, CS_SEXO,
         CS_GESTANT, CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI, ID_OCUPA_N, NU_LESOES, FORMACLINI, AVALIA_N, CLASSOPERA,
         MODOENTR, MODODETECT, BACILOSCO, DTINICTRAT, ESQ_INI_N, CONTREG, NERVOSAFET, DT_NOTI_AT, DTULTCOMP, CLASSATUAL,
         AVAL_ATU_N, ESQ_ATU_N, DOSE_RECEB, CONTEXAM, DTALTA_N, TPALTA_N) 

write.xlsx(dados_hanseniase, "Atualizar/dados_hanseniase.xlsx")

dados_hanseniase <- dados_hanseniase %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    DT_DIAG = as.Date(DT_DIAG, format = "%d/%m/%Y"),
    
    DT_NASC = as.Date(DT_NASC, format = "%d/%m/%Y"),
    
    SEM_DIAG = epiweek(DT_DIAG),
    
    NU_IDADE_N = if_else( str_sub(NU_IDADE_N,1,1) == "4", as.numeric(str_sub(NU_IDADE_N, 2,4)), 
                          floor(as.integer(DT_NOTIFIC - DT_NASC) / 365.25)),
    
    CS_SEXO = case_when(
      CS_SEXO == "F" ~ "Feminino",
      CS_SEXO == "M" ~ "Masculino"
    ),
    
    CS_GESTANT = case_when(
      CS_GESTANT == "1" ~ "1° Trimestre",
      CS_GESTANT == "2" ~ "2° Trimestre",
      CS_GESTANT == "3" ~ "3° Trimestre",
      CS_GESTANT == "4" ~ "Idade gestacional ignorada",
      CS_GESTANT == "5" ~ "Não",
      CS_GESTANT == "6" ~ "Não se aplica",
      CS_GESTANT == "9" ~ "Ignorado"
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
    
    FORMACLINI = case_when(
      FORMACLINI == "1" ~ "I(indeterminada)",
      FORMACLINI == "2" ~ "T(tuberculose)",
      FORMACLINI == "3" ~ "D(dimorfa)",
      FORMACLINI == "4" ~ "V(virchowiana)",
      FORMACLINI == "5" ~ "Não classificado"
    ),
    
    AVALIA_N = case_when(
      AVALIA_N == "0" ~ "Grau zero",
      AVALIA_N == "1" ~ "Grau I",
      AVALIA_N == "2" ~ "Grau II",
      AVALIA_N == "3" ~ "Não avaliado"
    ),
    
    CLASSOPERA = case_when(
      CLASSOPERA == "1" ~ "PB - Paucibacilar",
      CLASSOPERA == "2" ~ "MB - Multibacilar",
    ),
    
    MODOENTR = case_when(
      MODOENTR == "1" ~ "Caso novo",
      MODOENTR == "2" ~ "Transferência do mesmo município",
      MODOENTR == "3" ~ "Transferência de outro município",
      MODOENTR == "4" ~ "Transferência de outro estado",
      MODOENTR == "5" ~ "Transferência de outro país",
      MODOENTR == "6" ~ "Recidiva",
      MODOENTR == "7" ~ "Outros reingressos",
      MODOENTR == "9" ~ "Ignorado"
    ),
    
    MODODETECT = case_when(
      MODODETECT == "1" ~ "Encaminhamento",
      MODODETECT == "2" ~ "Demanda espontânea",
      MODODETECT == "3" ~ "Exame de coletividade",
      MODODETECT == "4" ~ "Exame de contatos",
      MODODETECT == "5" ~ "Outros modos",
      MODODETECT == "9" ~ "Ignorado"
    ),
    
    BACILOSCO = case_when(
      BACILOSCO == "1" ~ "Positiva",
      BACILOSCO == "2" ~ "Negativa",
      BACILOSCO == "3" ~ "Não realizada",
      BACILOSCO == "9" ~ "Ignorado"
    ),
    
    DTINICTRAT = as.Date(DTINICTRAT, format = "%d/%m/%Y"),
    
    ESQ_INI_N = case_when(
      ESQ_INI_N == "1" ~ "PQT/PB/6 doses",
      ESQ_INI_N == "2" ~ "PQT/MB/12 doses",
      ESQ_INI_N == "3" ~ "Outros esquemas substitutos"
    ),
    
    DT_NOTI_AT = as.Date(DT_NOTI_AT, format = "%d/%m/%Y"),
    
    DTULTCOMP = as.Date(DTULTCOMP, format = "%d/%m/%Y"),
    
    CLASSATUAL = case_when(
      CLASSATUAL == "1" ~ "PB(Paucibacilar)",
      CLASSATUAL == "2" ~ "MB(Multibacilar)"
    ),
    
    AVAL_ATU_N = case_when(
      AVAL_ATU_N == "0" ~ "Grau zero",
      AVAL_ATU_N == "1" ~ "Grau I",
      AVAL_ATU_N == "2" ~ "Grau II",
      AVAL_ATU_N == "3" ~ "Não avaliado",
      AVAL_ATU_N == "9" ~ "Ignorado"
    ),
    
    DTALTA_N = as.Date(DTALTA_N, format = "%d/%m/%Y"),
    
    TPALTA_N = case_when(
      TPALTA_N == "1" ~ "Cura",
      TPALTA_N == "2" ~ "Transferência para mesmo município",
      TPALTA_N == "3" ~ "Transferência para outro município",
      TPALTA_N == "4" ~ "transferência para outro estado",
      TPALTA_N == "5" ~ "Transferência para outro país",
      TPALTA_N == "6" ~ "Óbito",
      TPALTA_N == "7" ~ "Abandono",
      TPALTA_N == "8" ~ "Erro diagnóstico",
      TPALTA_N == "9" ~ "Transferência não especificada"
    )
    
  )

write.xlsx(dados_hanseniase, "Manipulados/dados_hanseniase.xlsx")