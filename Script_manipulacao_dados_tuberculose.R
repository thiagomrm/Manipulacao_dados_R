if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#Criando o objeto do tipo dataframe
#dados_tuberculose <- read.dbf("Dados/tuberculose.dbf")

dados_tuberculose <- read.dbf("//arquivos/SINAN/BaseDBF_RD/TUBENET.dbf")

dados_tuberculose <- subset(dados_tuberculose, year(dados_tuberculose$DT_NOTIFIC) > 2009 & dados_tuberculose$SG_UF == "31")

#Manipulando os dados do dataframe
dados_tuberculose <- dados_tuberculose %>%
  
  #Selecionando as colunas que serão trabalhadas 
  select(NU_NOTIFIC, TP_NOT,	ID_AGRAVO,	DT_NOTIFIC,	NU_ANO,	SG_UF_NOT,	ID_MUNICIP,	ID_REGIONA,	ID_UNIDADE,	DT_DIAG,
         NU_IDADE_N,	CS_SEXO,	CS_GESTANT,	CS_RACA,	CS_ESCOL_N,	SG_UF,	ID_MN_RESI,	ID_RG_RESI,	CS_ZONA,	AGRAVAIDS,
         AGRAVALCOO,	AGRAVDIABE,	AGRAVDOENC,	AGRAVOUTRA,	AGRAVDROGA,	AGRAVTABAC,	TRATAMENTO,	CULTURA_ES,	HIV,	HISTOPATOL,
         DT_INIC_TR,	BACILOSC_1,	BACILOSC_2,	BACILOSC_3,	BACILOSC_4,	BACILOSC_5,	BACILOSC_6,	TRATSUP_AT, SITUA_ENCE,
         DT_ENCERRA,	POP_LIBER,	TEST_MOLEC,	TEST_SENSI,	RAIOX_TORA,	FORMA) 

write.xlsx(dados_tuberculose, "Atualizar/dados_tuberculose.xlsx")


dados_tuberculose <- dados_tuberculose %>%  
  #Fazendo a transformação dos dados dentro das colunas selecionadas
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
    
    DT_DIAG = as.Date(DT_DIAG, format = "%d/%m/%Y"),
    
    NU_IDADE_N = if_else( str_sub(NU_IDADE_N,1,1) == "4", as.numeric(str_sub(NU_IDADE_N, 2,4)), 0),
    
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
    
    CS_ZONA = case_when(
      CS_ZONA == "1" ~ "Urbana",
      CS_ZONA == "2" ~ "Rural",
      CS_ZONA == "3" ~ "Periurbana",
      CS_ZONA == "9" ~ "Ignorado"
    ),
    
    AGRAVAIDS = case_when(
      AGRAVAIDS == "1" ~ "Sim",
      AGRAVAIDS == "2" ~ "Não",
      AGRAVAIDS == "9" ~ "Ignorado"
    ),
    
    AGRAVALCOO = case_when(
      AGRAVALCOO == "1" ~ "Sim",
      AGRAVALCOO == "2" ~ "Não",
      AGRAVALCOO == "9" ~ "Ignorado"
    ),
    
    AGRAVDIABE = case_when(
      AGRAVDIABE == "1" ~ "Sim",
      AGRAVDIABE == "2" ~ "Não",
      AGRAVDIABE == "9" ~ "Ignorado"
    ),
    
    AGRAVDOENC = case_when(
      AGRAVDOENC == "1" ~ "Sim",
      AGRAVDOENC == "2" ~ "Não",
      AGRAVDOENC == "9" ~ "Ignorado"
    ),
    
    AGRAVOUTRA = case_when(
      AGRAVOUTRA == "1" ~ "Sim",
      AGRAVOUTRA == "2" ~ "Não",
      AGRAVOUTRA == "9" ~ "Ignorado"
    ),
    
    AGRAVDROGA = case_when(
      AGRAVDROGA == "1" ~ "Sim",
      AGRAVDROGA == "2" ~ "Não",
      AGRAVDROGA == "9" ~ "Ignorado"
    ),
    
    AGRAVTABAC = case_when(
      AGRAVTABAC == "1" ~ "Sim",
      AGRAVTABAC == "2" ~ "Não",
      AGRAVTABAC == "9" ~ "Ignorado"
    ),
    
    TRATAMENTO = case_when(
      TRATAMENTO == "1" ~ "Caso Novo",
      TRATAMENTO == "2" ~ "Recidiva",
      TRATAMENTO == "3" ~ "Reingresso após Abandono",
      TRATAMENTO == "4" ~ "Não sabe",
      TRATAMENTO == "5" ~ "Transferência",
      TRATAMENTO == "6" ~ "Pós-óbito"
    ),
    
    CULTURA_ES = case_when(
      CULTURA_ES == "1" ~ "Positiva",
      CULTURA_ES == "2" ~ "Negativa",
      CULTURA_ES == "3" ~ "Em andamento",
      CULTURA_ES == "4" ~ "Não realizada"
    ),
    
    HIV = case_when(
      HIV == "1" ~ "Positiva",
      HIV == "2" ~ "Negativa",
      HIV == "3" ~ "Em andamento",
      HIV == "4" ~ "Não realizada"
    ),
    
    HISTOPATOL = case_when(
      HISTOPATOL == "1" ~ "Baar Positivo",
      HISTOPATOL == "2" ~ "Sugestivo de TB",
      HISTOPATOL == "3" ~ "Não sugestivo de TB",
      HISTOPATOL == "4" ~ "Em andamento",
      HISTOPATOL == "5" ~ "Não realizado"
    ),
    
    BACILOSC_1 = case_when(
      BACILOSC_1 == "1" ~ "Positiva",
      BACILOSC_1 == "2" ~ "Negativa",
      BACILOSC_1 == "3" ~ "Não realizada",
      BACILOSC_1 == "4" ~ "Não se aplica"
    ),
    
    BACILOSC_2 = case_when(
      BACILOSC_2 == "1" ~ "Positiva",
      BACILOSC_2 == "2" ~ "Negativa",
      BACILOSC_2 == "3" ~ "Não realizada",
      BACILOSC_2 == "4" ~ "Não se aplica"
    ),
    
    BACILOSC_3 = case_when(
      BACILOSC_3 == "1" ~ "Positiva",
      BACILOSC_3 == "2" ~ "Negativa",
      BACILOSC_3 == "3" ~ "Não realizada",
      BACILOSC_3 == "4" ~ "Não se aplica"
    ),
    
    BACILOSC_4 = case_when(
      BACILOSC_4 == "1" ~ "Positiva",
      BACILOSC_4 == "2" ~ "Negativa",
      BACILOSC_4 == "3" ~ "Não realizada",
      BACILOSC_4 == "4" ~ "Não se aplica"
    ),
    
    BACILOSC_5 = case_when(
      BACILOSC_5 == "1" ~ "Positiva",
      BACILOSC_5 == "2" ~ "Negativa",
      BACILOSC_5 == "3" ~ "Não realizada",
      BACILOSC_5 == "4" ~ "Não se aplica"
    ),
    
    BACILOSC_6 = case_when(
      BACILOSC_6 == "1" ~ "Positiva",
      BACILOSC_6 == "2" ~ "Negativa",
      BACILOSC_6 == "3" ~ "Não realizada",
      BACILOSC_6 == "4" ~ "Não se aplica"
    ),
    
    TRATSUP_AT = case_when(
      TRATSUP_AT == "1" ~ "Sim",
      TRATSUP_AT == "2" ~ "Não",
      TRATSUP_AT == "9" ~ "Ignorado"
    ),
    
    SITUA_ENCE = case_when(
      SITUA_ENCE == "1" ~ "Cura",
      SITUA_ENCE == "2" ~ "Abandono",
      SITUA_ENCE == "3" ~ "Óbito por TB",
      SITUA_ENCE == "4" ~ "Óbito por outras causas",
      SITUA_ENCE == "5" ~ "Transferência",
      SITUA_ENCE == "6" ~ "Mudança de Diagnóstico",
      SITUA_ENCE == "7" ~ "TB-DR",
      SITUA_ENCE == "8" ~ "Mudança de Esquema",
      SITUA_ENCE == "9" ~ "Falência",
      SITUA_ENCE == "10" ~ "Abandono Primário"
    ),
    
    POP_LIBER = case_when(
      POP_LIBER == "1" ~ "Sim",
      POP_LIBER == "2" ~ "Não",
      POP_LIBER == "9" ~ "Ignorado"
    ),
    
    TEST_MOLEC = case_when(
      TEST_MOLEC == "1" ~ "Detectável sensível à Rifampicina",
      TEST_MOLEC == "2" ~ "Detectável resistente à Rifampicina",
      TEST_MOLEC == "3" ~ "Não detectável",
      TEST_MOLEC == "4" ~ "Inconclusivo",
      TEST_MOLEC == "5" ~ "Não realizado"
    ),
    
    TEST_SENSI = case_when(
      TEST_SENSI == "1" ~ "Resistente somente à Isoniazida",
      TEST_SENSI == "2" ~ "Resistente somente à Rifampicina",
      TEST_SENSI == "3" ~ "Resistente à Isoniazida e Rifampicina",
      TEST_SENSI == "4" ~ "Resistente a outras drogas de 1ª linha",
      TEST_SENSI == "5" ~ "Sensível",
      TEST_SENSI == "6" ~ "Em andamento",
      TEST_SENSI == "7" ~ "Não realizado"
    ),
    
    RAIOX_TORA = case_when(
      RAIOX_TORA == "1" ~ "Suspeito",
      RAIOX_TORA == "2" ~ "Normal",
      RAIOX_TORA == "3" ~ "Outra patologia",
      RAIOX_TORA == "4" ~ "Não realizado"
    ),
    
    FORMA = case_when(
      FORMA == "1" ~ "Pulmonar",
      FORMA == "2" ~ "Extrapulmonar",
      FORMA == "3" ~ "Pulmonar + Extrapulmonar"
    )
  )

#Fazendo a escrita do dataframe em forma de arquivo xlsx
write.xlsx(dados_tuberculose, "Manipulados/dados_tuberculose.xlsx")

gc()
