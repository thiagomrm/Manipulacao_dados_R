if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(tcltk)) install.packages("tcltk");library(tcltk)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_acidente_trabalho <- read.dbf("Dados/acidente_trabalho_grave2.dbf")

dados_acidente_trabalho <- read.dbf("//arquivos/SINAN/BaseDBF_RD/ACGRANET.dbf")

dados_acidente_trabalho <- subset(dados_acidente_trabalho, year(dados_acidente_trabalho$DT_NOTIFIC) > 2009)

dados_acidente_trabalho <- dados_acidente_trabalho %>%
  
  select(NU_NOTIFIC, TP_NOT, ID_AGRAVO, DT_NOTIFIC, SEM_NOT, NU_ANO, SG_UF_NOT, ID_MUNICIP, ID_UNIDADE, DT_ACID,
         SEM_ACID, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI, CS_ZONA, ID_OCUPA_N,
         SIT_TRAB, NUTEMPO, TPTEMPO, LOCAL_ACID, CNAE, HORA_ACID, MIN_ACID, HORA_JOR, MIN_JOR, MUN_ACID,
         CID_ACID, TIPO_ACID, MAIS_TRAB, NU_TRAB, ATENDE_MED, DT_ATENDE, UF_ATENDE, MUN_ATENDE, UNI_ATENDE,
         PART_CORP1, PART_CORP2, PART_CORP3, CID_LESAO, REGIME, EVOLUCAO, DT_OBITO, CAT
  ) 

write.xlsx(dados_acidente_trabalho, "Atualizar/dados_acidente_trabalho_grave.xlsx")
 
dados_acidente_trabalho <- dados_acidente_trabalho %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    SEM_NOT = epiweek(DT_NOTIFIC),
    
    DT_ACID = as.Date(DT_ACID, format = "%d/%m/%Y"),
    
    SEM_ACID = epiweek(DT_ACID),
    
    NU_IDADE_N = if_else( str_sub(NU_IDADE_N,1,1) == "4", as.numeric(str_sub(NU_IDADE_N, 2,4)), 0),
    
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
    
    CS_ZONA = case_when(
      CS_ZONA == "1" ~ "Urbana",
      CS_ZONA == "2" ~ "Rural",
      CS_ZONA == "3" ~ "Periurbana",
      CS_ZONA == "9" ~ "Ignorado"
    ),
    
    SIT_TRAB = case_when(
      SIT_TRAB == "01" ~ "Empregado registrado com carteira assinada",
      SIT_TRAB == "02" ~ "Empregado não registrado",
      SIT_TRAB == "03" ~ "Autônomo/conta própria",
      SIT_TRAB == "04" ~ "Servidor público estatutário",
      SIT_TRAB == "05" ~ "Servidor público celetista",
      SIT_TRAB == "06" ~ "Aposentado",
      SIT_TRAB == "07" ~ "Desempregado",
      SIT_TRAB == "08" ~ "Trabalhador temporário",
      SIT_TRAB == "09" ~ "Cooperativado",
      SIT_TRAB == "10" ~ "Trabalhador avulso",
      SIT_TRAB == "11" ~ "Empregador",
      SIT_TRAB == "12" ~ "Outros",
      SIT_TRAB == "99" ~ "Ignorado"
    ),
    
    LOCAL_ACID = case_when(
      LOCAL_ACID == "1" ~ "Instalações do Contratante",
      LOCAL_ACID == "2" ~ "Via Pública",
      LOCAL_ACID == "3" ~ "Instalações de terceiros",
      LOCAL_ACID == "4" ~ "Domicilio próprio",
      LOCAL_ACID == "9" ~ "Ignorado"
    ),
    
    TIPO_ACID = case_when(
      TIPO_ACID == "1" ~ "Típico",
      TIPO_ACID == "2" ~ "Trajeto",
      TIPO_ACID == "9" ~ "Ignorado"
    ),
    
    MAIS_TRAB = case_when(
      MAIS_TRAB == "1" ~ "Sim",
      MAIS_TRAB == "2" ~ "Não",
      MAIS_TRAB == "9" ~ "Ignorado"
    ),
    
    ATENDE_MED = case_when(
      ATENDE_MED == "1" ~ "Sim",
      ATENDE_MED == "2" ~ "Não",
      ATENDE_MED == "9" ~ "Ignorado"
    ),
    
    PART_CORP1 = case_when(
      PART_CORP1 == "01" ~ "Olho",
      PART_CORP1 == "02" ~ "Cabeça",
      PART_CORP1 == "03" ~ "Pescoço",
      PART_CORP1 == "04" ~ "Tórax",
      PART_CORP1 == "05" ~ "Abdome",
      PART_CORP1 == "06" ~ "Mão",
      PART_CORP1 == "07" ~ "Membro superior",
      PART_CORP1 == "08" ~ "Membro inferior",
      PART_CORP1 == "09" ~ "Pé",
      PART_CORP1 == "10" ~ "Todo o corpo",
      PART_CORP1 == "11" ~ "Outro",
      PART_CORP1 == "99" ~ "Ignorado"
    ),
    
    PART_CORP2 = case_when(
      PART_CORP2 == "01" ~ "Olho",
      PART_CORP2 == "02" ~ "Cabeça",
      PART_CORP2 == "03" ~ "Pescoço",
      PART_CORP2 == "04" ~ "Tórax",
      PART_CORP2 == "05" ~ "Abdome",
      PART_CORP2 == "06" ~ "Mão",
      PART_CORP2 == "07" ~ "Membro superior",
      PART_CORP2 == "08" ~ "Membro inferior",
      PART_CORP2 == "09" ~ "Pé",
      PART_CORP2 == "10" ~ "Todo o corpo",
      PART_CORP2 == "11" ~ "Outro",
      PART_CORP2 == "99" ~ "Ignorado"
    ),
    
    PART_CORP3 = case_when(
      PART_CORP3 == "01" ~ "Olho",
      PART_CORP3 == "02" ~ "Cabeça",
      PART_CORP3 == "03" ~ "Pescoço",
      PART_CORP3 == "04" ~ "Tórax",
      PART_CORP3 == "05" ~ "Abdome",
      PART_CORP3 == "06" ~ "Mão",
      PART_CORP3 == "07" ~ "Membro superior",
      PART_CORP3 == "08" ~ "Membro inferior",
      PART_CORP3 == "09" ~ "Pé",
      PART_CORP3 == "10" ~ "Todo o corpo",
      PART_CORP3 == "11" ~ "Outro",
      PART_CORP3 == "99" ~ "Ignorado"
    ),
    
    REGIME = case_when(
      REGIME == "1" ~ "Hospitalar",
      REGIME == "2" ~ "Ambulatório",
      REGIME == "3" ~ "Ambos",
      REGIME == "9" ~ "Ignorado"
    ),
    
    EVOLUCAO = case_when(
      EVOLUCAO == "1" ~ "Cura",
      EVOLUCAO == "2" ~ "Incapacidade temporária",
      EVOLUCAO == "3" ~ "Incapacidade parcial permanente",
      EVOLUCAO == "4" ~ "Incapacidade total permanente",
      EVOLUCAO == "5" ~ "Óbito por acidente de trabalho grave",
      EVOLUCAO == "6" ~ "Óbito por outras causas",
      EVOLUCAO == "7" ~ "Outro",
      EVOLUCAO == "9" ~ "Ignorado"
    ),
    
    DT_OBITO = as.Date(DT_OBITO, format = "%d/%m/%Y"),
    
    CAT = case_when(
      CAT == "1" ~ "Sim",
      CAT == "2" ~ "Não",
      CAT == "3" ~ "Não se aplica",
      CAT == "9" ~ "Ignorado"
    )
  )

write.xlsx(dados_acidente_trabalho, "Manipulados/dados_acidente_trabalho_grave.xlsx")