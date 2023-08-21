if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

inicio <- Sys.time()

dados_violencia_geral <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2000-2011.dbf")

dados_violencia_geral <- dados_violencia_geral %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2012.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2013-2017.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2018.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2019.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2020.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2021.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2022.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

dados_violencia_geral2 <- read.dbf("//arquivos/SINAN/BaseDBF_RD/VIOLENET2023.dbf")

dados_violencia_geral2 <- dados_violencia_geral2 %>%
  
  select(DT_NOTIFIC,	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_SEXU, NUM_ENVOLV,	AUTOR_SEXO, ORIENT_SEX,	IDENT_GEN) 

dados_violencia_geral <- rbind(dados_violencia_geral, dados_violencia_geral2)

#write_csv2(dados_violencia_geral, "C:/Users/x14339682/Desktop/Teste CGE/Teste/violencia.csv")

dados_violencia_geral <- dados_violencia_geral %>%
  
  mutate(
    
    DT_NOTIFIC = as.Date(DT_NOTIFIC, format(DT_NOTIFIC, "%d/%m/%Y"), locale = "pt_BR"),
    
    DT_NASC = as.Date(DT_NASC, format(DT_NASC, "%d/%m/%Y"), locale = "pt_BR"),
    
    #SEM_PRI = epiweek(DT_SIN_PRI),
    
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
    
    LOCAL_OCOR = case_when(
      LOCAL_OCOR == "01" ~ "Residencia",
      LOCAL_OCOR == "02" ~ "Habitação coletiva",
      LOCAL_OCOR == "03" ~ "Escola",
      LOCAL_OCOR == "04" ~ "Local de pratica esportiva",
      LOCAL_OCOR == "05" ~ "Bar ou similar",
      LOCAL_OCOR == "06" ~ "Via pública",
      LOCAL_OCOR == "07" ~ "Comercio/Serviços",
      LOCAL_OCOR == "08" ~ "Industria/Construção",
      LOCAL_OCOR == "09" ~ "Outro",
      LOCAL_OCOR == "99" ~ "Ignorado"
    ),
    
    OUT_VEZES = case_when(
      OUT_VEZES == "1" ~ "Sim",
      OUT_VEZES == "2" ~ "Não",
      OUT_VEZES == "9" ~ "Ignorado"
    ),
    
    LES_AUTOP = case_when(
      LES_AUTOP == "1" ~ "Sim",
      LES_AUTOP == "2" ~ "Não",
      LES_AUTOP == "8" ~ "Não se aplica",
      LES_AUTOP == "9" ~ "Ignorado"
    ),
    
    VIOL_FISIC = case_when(
      VIOL_FISIC == "1" ~ "Sim",
      VIOL_FISIC == "2" ~ "Não",
      VIOL_FISIC == "9" ~ "Ignorado"
    ),
    
    VIOL_PSICO = case_when(
      VIOL_PSICO == "1" ~ "Sim",
      VIOL_PSICO == "2" ~ "Não",
      VIOL_PSICO == "9" ~ "Ignorado"
    ),
    
    VIOL_SEXU = case_when(
      VIOL_SEXU == "1" ~ "Sim",
      VIOL_SEXU == "2" ~ "Não",
      VIOL_SEXU == "9" ~ "Ignorado"
    ),
    
    NUM_ENVOLV = case_when(
      NUM_ENVOLV == "1" ~ "Um",
      NUM_ENVOLV == "2" ~ "Dois ou mais",
      NUM_ENVOLV == "9" ~ "Ignorado"
    ),
    
    AUTOR_SEXO = case_when(
      AUTOR_SEXO == "1" ~ "Masculino",
      AUTOR_SEXO == "2" ~ "Feminino",
      AUTOR_SEXO == "3" ~ "Ambos os sexos",
      AUTOR_SEXO == "9" ~ "Ignorado" 
    ),
    
    ORIENT_SEX = case_when(
      ORIENT_SEX == "1" ~ "Heterossexual",
      ORIENT_SEX == "2" ~ "Homossexual",
      ORIENT_SEX == "3" ~ "Bissexual",
      ORIENT_SEX == "8" ~ "Não se aplica",
      ORIENT_SEX == "9" ~ "Ignorado"
    ),
    
    IDENT_GEN = case_when(
      IDENT_GEN == "1" ~ "Travesti",
      IDENT_GEN == "2" ~ "Transsexual Mulher",
      IDENT_GEN == "3" ~ "Transsexual Homem",
      IDENT_GEN == "8" ~ "Não se aplica",
      IDENT_GEN == "9" ~ "Ignorado"
    )
  )

dados_violencia_geral <- subset(dados_violencia_geral, dados_violencia_geral$CS_SEXO == "Feminino")

s2010 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2010)
write.csv2(s2010, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2010.csv")

s2011 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2011)
write.csv2(s2011, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2011.csv")

s2012 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2012)
write.csv2(s2012, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2012.csv")

s2013 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2013)
write.csv2(s2013, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2013.csv")

s2014 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2014)
write.csv2(s2014, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2014.csv")

s2015 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2015)
write.csv2(s2015, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2015.csv")

s2016 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2016)
write.csv2(s2016, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2016.csv")

s2017 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2017)
write.csv2(s2017, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2017.csv")

s2018 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2018)
write.csv2(s2018, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2018.csv")

s2019 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2019)
write.csv2(s2019, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2019.csv")

s2020 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2020)
write.csv2(s2020, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2020.csv")

s2021 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2021)
write.csv2(s2021, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2021.csv")

s2022 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2022)
write.csv2(s2022, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2022.csv")

s2023 <- subset(dados_violencia_geral, year(dados_violencia_geral$DT_NOTIFIC) == 2023)
write.csv2(s2023, "C:/Users/x14339682/Desktop/Teste CGE/dados_violencia_mulheres_SES_2023.csv")

fim <- Sys.time()

print(fim - inicio)