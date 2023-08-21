if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#dados_hepatite <- read.dbf("Dados/hepatite2.dbf")

dados_hepatite <- read.dbf("//arquivos/SINAN/BaseDBF_RD/HEPANET.dbf")

dados_hepatite <- subset(dados_hepatite, year(dados_hepatite$DT_NOTIFIC) > 2009 & dados_hepatite$SG_UF == "31")

dados_hepatite <- subset(dados_hepatite, str_sub(dados_hepatite$ID_MN_RESI,1,2) == "31")

dados_hepatite <- dados_hepatite %>%
  
  select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, ID_REGIONA, ID_UNIDADE, DT_SIN_PRI, DT_NASC, NU_IDADE_N,
         CS_SEXO, CS_GESTANT, CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI, ID_RG_RESI, HEPATITA, HEPATITB, HIV, OUTRA_DST, BANCOSANGU,
         CLASSI_FIN, FORMA, CLAS_ETIOL, FONTE )

write.xlsx(dados_hepatite, "Atualizar/dados_hepatite.xlsx")
 
dados_hepatite <- dados_hepatite %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format(DT_NOTIFIC, "%d/%m/%Y"), locale = "pt_BR"),
    
    DT_SIN_PRI = as.Date(DT_SIN_PRI, format(DT_NASC, "%d/%m/%Y"), locale = "pt_BR"),
    
    DT_NASC = as.Date(DT_NASC, format(DT_NASC, "%d/%m/%Y"), locale = "pt_BR"),
    
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
    
    HEPATITA = case_when(
      HEPATITA == "1" ~ "Completa",
      HEPATITA == "2" ~ "Incompleta",
      HEPATITA == "3" ~ "Não Vacinado",
      HEPATITA == "4" ~ "Ignorado"
    ),
    
    HEPATITB = case_when(
      HEPATITB == "1" ~ "Completa",
      HEPATITB == "2" ~ "Incompleta",
      HEPATITB == "3" ~ "Não Vacinado",
      HEPATITB == "4" ~ "Ignorado"
    ),
    
    HIV = case_when(
      HIV == "1" ~ "Sim",
      HIV == "2" ~ "Não",
      HIV == "9" ~ "Ignorado"
    ),
    
    OUTRA_DST = case_when(
      OUTRA_DST == "1" ~ "Sim",
      OUTRA_DST == "2" ~ "Não",
      OUTRA_DST == "9" ~ "Ignorado"
    ),
    
    BANCOSANGU = case_when(
      BANCOSANGU == "1" ~ "Banco de Sangue",
      BANCOSANGU == "2" ~ "Centro de Testagem e aconselhamento",
      BANCOSANGU == "3" ~ "Não se aplica"
    ),
    
    CLASSI_FIN = case_when(
      CLASSI_FIN == "1" ~ "Confirmação laboratorial",
      CLASSI_FIN == "2" ~ "Confirmação clínico epidemiológico",
      CLASSI_FIN == "3" ~ "Descartado",
      CLASSI_FIN == "4" ~ "Cicatriz sorológica",
      CLASSI_FIN == "8" ~ "Inconclusivo"
    ),
    
    FORMA = case_when(
      FORMA == "1" ~ "Hepatite Aguda",
      FORMA == "2" ~ "Hepatite Crônica/Portador Assintomático",
      FORMA == "3" ~ "Hepatite Fulminante",
      FORMA == "4" ~ "Inconclusivo"
    ),
    
    CLAS_ETIOL = case_when(
      CLAS_ETIOL == "01" ~ "Virus A",
      CLAS_ETIOL == "02" ~ "Virus B",
      CLAS_ETIOL == "03" ~ "Virus C",
      CLAS_ETIOL == "04" ~ "Virus B e D",
      CLAS_ETIOL == "05" ~ "Virus E",
      CLAS_ETIOL == "06" ~ "Virus B e C",
      CLAS_ETIOL == "07" ~ "Virus A e B",
      CLAS_ETIOL == "08" ~ "Virus A e C",
      CLAS_ETIOL == "09" ~ "Outras Hepatites virais",
      CLAS_ETIOL == "99" ~ "Ignorado"
    ),
    
    FONTE = case_when(
      FONTE == "01" ~ "Sexual",
      FONTE == "02" ~ "Transfusional",
      FONTE == "03" ~ "Uso de drogas",
      FONTE == "04" ~ "Vertical",
      FONTE == "05" ~ "Acidente de Trabalho",
      FONTE == "06" ~ "Hemodiálise",
      FONTE == "07" ~ "Domiciliar",
      FONTE == "08" ~ "Tratamento cirúrgico",
      FONTE == "09" ~ "Tratamento dentário",
      FONTE == "10" ~ "Pessoa/pessoa",
      FONTE == "11" ~ "Alimento/água contaminada",
      FONTE == "12" ~ "Outros",
      FONTE == "99" ~ "Ignorado",
    )
  )

write.xlsx(dados_hepatite, "Manipulados/dados_hepatite.xlsx")