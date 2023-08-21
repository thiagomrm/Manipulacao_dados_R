if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_aids_hiv <- read.dbf("Dados/aids_hiv2.dbf")

dados_aids_hiv <- read.dbf("//arquivos/SINAN/BaseDBF_RD/AIDSANET.dbf")

dados_aids_hiv <- subset(dados_aids_hiv, year(dados_aids_hiv$DT_NOTIFIC) > 2009 & dados_aids_hiv$CRITERIO == "100" | 
                           dados_aids_hiv$CRITERIO == "300" | dados_aids_hiv$CRITERIO == "600" | dados_aids_hiv$CRITERIO == "901")

dados_aids_hiv <- subset(dados_aids_hiv, str_sub(dados_aids_hiv$ID_MN_RESI,1,2) == "31")

dados_aids_hiv <- dados_aids_hiv %>%
  
  select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, ID_REGIONA, DT_DIAG, DT_NASC, NU_IDADE_N, CS_SEXO,
         CS_GESTANT, CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI, ANT_TRASMI, ANTRELSE_N, ANT_DROGA, LAB_TRIAGE,
         ANT_TUBERC, ANT_CANDID, ANT_PULMON, ANT_HERPES, ANT_DISFUN, ANT_DIARRE, ANT_FEBRE, ANT_CAQUEX, ANT_ASTERI,
         ANT_DERMAT, ANT_ANEMIA, ANT_TOSSE, ANT_LINFO, ANT_ESOF_N, ANT_PNEUMO, ANT_TOXO, ANT_CONTAG, DEF_DIAGNO, 
         EVOLUCAO, ANT_REL_CA ) 

write.xlsx(dados_aids_hiv, "Atualizar/dados_aids_hiv.xlsx")

dados_aids_hiv <- dados_aids_hiv %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    DT_DIAG = as.Date(DT_DIAG, format = "%d/%m/%Y"),
    
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
    
    ANT_TRASMI = case_when(
      ANT_TRASMI == "1" ~ "Sim",
      ANT_TRASMI == "2" ~ "Não foi transmissão vertical",
      ANT_TRASMI == "9" ~ "Ignorado"
    ),
    
    ANTRELSE_N = case_when(
      ANTRELSE_N == "1" ~ "Relações sexuais com Homens",
      ANTRELSE_N == "2" ~ "Relações sexuais com Mulheres",
      ANTRELSE_N == "3" ~ "Relações sexuais com homens e mulheres",
      ANTRELSE_N == "4" ~ "Não foi transmissão sexual",
      ANTRELSE_N == "9" ~ "Ignorado"
    ),
    
    ANT_DROGA = case_when(
      ANT_DROGA == "1" ~ "Sim",
      ANT_DROGA == "2" ~ "Não",
      ANT_DROGA == "9" ~ "Ignorado"
    ),
    
    LAB_TRIAGE = case_when(
      LAB_TRIAGE == "1" ~ "Positivo/Reagente",
      LAB_TRIAGE == "2" ~ "Negativo/Não reagente",
      LAB_TRIAGE == "3" ~ "Inconclusivo",
      LAB_TRIAGE == "4" ~ "Não realizado",
      LAB_TRIAGE == "9" ~ "Ignorado"
    ),
    
    ANT_TUBERC = case_when(
      ANT_TUBERC == "1" ~ "Sim",
      ANT_TUBERC == "2" ~ "Não",
      ANT_TUBERC == "9" ~ "Ignorado"
    ),
    
    ANT_CANDID = case_when(
      ANT_CANDID == "1" ~ "Sim",
      ANT_CANDID == "2" ~ "Não",
      ANT_CANDID == "9" ~ "Ignorado"
    ),
    
    ANT_PULMON = case_when(
      ANT_PULMON == "1" ~ "Sim",
      ANT_PULMON == "2" ~ "Não",
      ANT_PULMON == "9" ~ "Ignorado"
    ),
    
    ANT_HERPES = case_when(
      ANT_HERPES == "1" ~ "Sim",
      ANT_HERPES == "2" ~ "Não",
      ANT_HERPES == "9" ~ "Ignorado"
    ),
    
    ANT_DISFUN = case_when(
      ANT_DISFUN == "1" ~ "Sim",
      ANT_DISFUN == "2" ~ "Não",
      ANT_DISFUN == "9" ~ "Ignorado"
    ),
    
    ANT_DIARRE = case_when(
      ANT_DIARRE == "1" ~ "Sim",
      ANT_DIARRE == "2" ~ "Não",
      ANT_DIARRE == "9" ~ "Ignorado"
    ),
    
    ANT_FEBRE = case_when(
      ANT_FEBRE == "1" ~ "Sim",
      ANT_FEBRE == "2" ~ "Não",
      ANT_FEBRE == "9" ~ "Ignorado"
    ),
    
    ANT_CAQUEX = case_when(
      ANT_CAQUEX == "1" ~ "Sim",
      ANT_CAQUEX == "2" ~ "Não",
      ANT_CAQUEX == "9" ~ "Ignorado"
    ),
    
    ANT_ASTERI = case_when(
      ANT_ASTERI == "1" ~ "Sim",
      ANT_ASTERI == "2" ~ "Não",
      ANT_ASTERI == "9" ~ "Ignorado"
    ),
    
    ANT_DERMAT = case_when(
      ANT_DERMAT == "1" ~ "Sim",
      ANT_DERMAT == "2" ~ "Não",
      ANT_DERMAT == "9" ~ "Ignorado"
    ),
    
    ANT_ANEMIA = case_when(
      ANT_ANEMIA == "1" ~ "Sim",
      ANT_ANEMIA == "2" ~ "Não",
      ANT_ANEMIA == "9" ~ "Ignorado"
    ),
    
    ANT_TOSSE = case_when(
      ANT_TOSSE == "1" ~ "Sim",
      ANT_TOSSE == "2" ~ "Não",
      ANT_TOSSE == "9" ~ "Ignorado"
    ),
    
    ANT_LINFO = case_when(
      ANT_LINFO == "1" ~ "Sim",
      ANT_LINFO == "2" ~ "Não",
      ANT_LINFO == "9" ~ "Ignorado"
    ),
    
    ANT_ESOF_N = case_when(
      ANT_ESOF_N == "1" ~ "Sim",
      ANT_ESOF_N == "2" ~ "Não",
      ANT_ESOF_N == "9" ~ "Ignorado"
    ),
    
    ANT_PNEUMO = case_when(
      ANT_PNEUMO == "1" ~ "Sim",
      ANT_PNEUMO == "2" ~ "Não",
      ANT_PNEUMO == "9" ~ "Ignorado"
    ),
    
    ANT_TOXO = case_when(
      ANT_TOXO == "1" ~ "Sim",
      ANT_TOXO == "2" ~ "Não",
      ANT_TOXO == "9" ~ "Ignorado"
    ),
    
    ANT_CONTAG = case_when(
      ANT_CONTAG == "1" ~ "Sim",
      ANT_CONTAG == "2" ~ "Não",
      ANT_CONTAG == "9" ~ "Ignorado"
    ),
    
    DEF_DIAGNO = case_when(
      DEF_DIAGNO == "1" ~ "Sim",
      DEF_DIAGNO == "2" ~ "Não",
      DEF_DIAGNO == "9" ~ "Ignorado"
    ),
    
    EVOLUCAO = case_when(
      EVOLUCAO == "1" ~ "Vivo",
      EVOLUCAO == "2" ~ "Óbito por Aids",
      EVOLUCAO == "3" ~ "Óbito por outras causas",
      EVOLUCAO == "9" ~ "Ignorado"
    ),
    
    ANT_REL_CA = case_when(
      ANT_REL_CA == "10" ~ "Homossexual",
      ANT_REL_CA == "11" ~ "Homo/Drogas",
      ANT_REL_CA == "12" ~ "Homo/Hemofílico",
      ANT_REL_CA == "13" ~ "Homo/Transfusão",
      ANT_REL_CA == "14" ~ "Homo/Droga/Hemofílico",
      ANT_REL_CA == "15" ~ "Homo/Droga/Transfusão",
      ANT_REL_CA == "20" ~ "Bissexual",
      ANT_REL_CA == "21" ~ "Bi/Drogas",
      ANT_REL_CA == "22" ~ "Bi/Hemofílico",
      ANT_REL_CA == "23" ~ "Bi/Transfusão",
      ANT_REL_CA == "24" ~ "Bi/Droga/Hemofílico",
      ANT_REL_CA == "25" ~ "Bi/Droga/Transfusão",
      ANT_REL_CA == "30" ~ "Heterossexual",
      ANT_REL_CA == "31" ~ "Hetero/Droga",
      ANT_REL_CA == "32" ~ "Hetero/Hemofílico",
      ANT_REL_CA == "33" ~ "Hetero/Transfusão",
      ANT_REL_CA == "34" ~ "Hetero/Droga/Hemofílico"
    )
    
  )

write.xlsx(dados_aids_hiv, "Manipulados/dados_aids_hiv.xlsx")