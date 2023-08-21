if(!require(foreign)) install.packages("foreign");library(foreign)
if(!require(writexl)) install.packages("writexl");library(writexl)
if(!require(lubridate)) install.packages("lubridate");library(lubridate)
if(!require(readxl)) install.packages("readxl");library(readxl)
if(!require(tidyverse)) install.packages("tidyverse");library(tidyverse)
if(!require(readr)) install.packages("readr");library(readr)
if(!require(openxlsx)) install.packages("openxlsx");library(openxlsx)

#Criando o objeto do tipo dataframe
dados_violencia_geral <- read.dbf("Dados/dados_violencia_geral.dbf")

dados_violencia_geral <- dados_violencia_geral %>%
  
  select(DT_NOTIFIC,	ID_RG_RESI, 	DT_NASC,	NU_IDADE_N,	CS_SEXO,	CS_RACA,	ID_MN_RESI,	LOCAL_OCOR,	OUT_VEZES,	LES_AUTOP,
         VIOL_FISIC,	VIOL_PSICO,	VIOL_TORT,	VIOL_SEXU,	VIOL_TRAF,	VIOL_FINAN,	VIOL_NEGLI,	VIOL_INFAN,	VIOL_LEGAL,
         VIOL_OUTR,	SEX_ASSEDI,	SEX_ESTUPR,	SEX_PUDOR,	SEX_PORNO,	SEX_EXPLO,	SEX_OUTRO,	NUM_ENVOLV,	AUTOR_SEXO,
         DEF_TRANS,	DEF_FISICA,	DEF_MENTAL,	DEF_VISUAL,	DEF_AUDITI,	TRAN_MENT,	TRAN_COMP,	DEF_OUT,	ORIENT_SEX,	IDENT_GEN) 

write.xlsx(dados_violencia_geral, "Atualizar/dados_violencia_geral.xlsx")

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
    
    VIOL_OUTR = case_when(
      VIOL_OUTR == "1" ~ "Sim",
      VIOL_OUTR == "2" ~ "Não",
      VIOL_OUTR == "9" ~ "Ignorado"
    ),
    
    VIOL_TORT = case_when(
      VIOL_TORT == "1" ~ "Sim",
      VIOL_TORT == "2" ~ "Não",
      VIOL_TORT == "9" ~ "Ignorado"
    ),
    
    VIOL_SEXU = case_when(
      VIOL_SEXU == "1" ~ "Sim",
      VIOL_SEXU == "2" ~ "Não",
      VIOL_SEXU == "9" ~ "Ignorado"
    ),
    
    VIOL_TRAF = case_when(
      VIOL_TRAF == "1" ~ "Sim",
      VIOL_TRAF == "2" ~ "Não",
      VIOL_TRAF == "9" ~ "Ignorado"
    ),
    
    VIOL_FINAN = case_when(
      VIOL_FINAN == "1" ~ "Sim",
      VIOL_FINAN == "2" ~ "Não",
      VIOL_FINAN == "9" ~ "Ignorado"
    ),
    
    VIOL_NEGLI = case_when(
      VIOL_NEGLI == "1" ~ "Sim",
      VIOL_NEGLI == "2" ~ "Não",
      VIOL_NEGLI == "9" ~ "Ignorado"
    ),
    
    VIOL_INFAN = case_when(
      VIOL_INFAN == "1" ~ "Sim",
      VIOL_INFAN == "2" ~ "Não",
      VIOL_INFAN == "9" ~ "Ignorado"
    ),
    
    VIOL_LEGAL = case_when(
      VIOL_LEGAL == "1" ~ "Sim",
      VIOL_LEGAL == "2" ~ "Não",
      VIOL_LEGAL == "9" ~ "Ignorado"
    ),
    
    SEX_ASSEDI = case_when(
      SEX_ASSEDI == "1" ~ "Sim",
      SEX_ASSEDI == "2" ~ "Não",
      SEX_ASSEDI == "8" ~ "Não se aplica",
      SEX_ASSEDI == "9" ~ "Ignorado",
    ),
    
    SEX_ESTUPR = case_when(
      SEX_ESTUPR == "1" ~ "Sim",
      SEX_ESTUPR == "2" ~ "Não",
      SEX_ESTUPR == "8" ~ "Não se aplica",
      SEX_ESTUPR == "9" ~ "Ignorado",
    ),
    
    SEX_PORNO = case_when(
      SEX_PORNO == "1" ~ "Sim",
      SEX_PORNO == "2" ~ "Não",
      SEX_PORNO == "8" ~ "Não se aplica",
      SEX_PORNO == "9" ~ "Ignorado",
    ),
    
    SEX_PUDOR = case_when(
      SEX_PUDOR == "1" ~ "Sim",
      SEX_PUDOR == "2" ~ "Não",
      SEX_PUDOR == "8" ~ "Não se aplica",
      SEX_PUDOR == "9" ~ "Ignorado",
    ),
    
    SEX_EXPLO = case_when(
      SEX_EXPLO == "1" ~ "Sim",
      SEX_EXPLO == "2" ~ "Não",
      SEX_EXPLO == "8" ~ "Não se aplica",
      SEX_EXPLO == "9" ~ "Ignorado",
    ),
    
    SEX_OUTRO = case_when(
      SEX_OUTRO == "1" ~ "Sim",
      SEX_OUTRO == "2" ~ "Não",
      SEX_OUTRO == "8" ~ "Não se aplica",
      SEX_OUTRO == "9" ~ "Ignorado",
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
    
    DEF_TRANS = case_when(
      DEF_TRANS == "1" ~ "Sim",
      DEF_TRANS == "2" ~ "Não",
      DEF_TRANS == "9" ~ "Ignorado"
    ),
    
    DEF_FISICA = case_when(
      DEF_FISICA == "1" ~ "Sim",
      DEF_FISICA == "2" ~ "Não",
      DEF_FISICA == "8" ~ "Não se aplica",
      DEF_FISICA == "9" ~ "Ignorado"
    ),
    
    DEF_MENTAL = case_when(
      DEF_MENTAL == "1" ~ "Sim",
      DEF_MENTAL == "2" ~ "Não",
      DEF_MENTAL == "8" ~ "Não se aplica",
      DEF_MENTAL == "9" ~ "Ignorado"
    ),
    
    DEF_VISUAL = case_when(
      DEF_VISUAL == "1" ~ "Sim",
      DEF_VISUAL == "2" ~ "Não",
      DEF_VISUAL == "8" ~ "Não se aplica",
      DEF_VISUAL == "9" ~ "Ignorado"
    ),
    
    DEF_AUDITI = case_when(
      DEF_AUDITI == "1" ~ "Sim",
      DEF_AUDITI == "2" ~ "Não",
      DEF_AUDITI == "8" ~ "Não se aplica",
      DEF_AUDITI == "9" ~ "Ignorado"
    ),
    
    TRAN_MENT = case_when(
      TRAN_MENT == "1" ~ "Sim",
      TRAN_MENT == "2" ~ "Não",
      TRAN_MENT == "8" ~ "Não se aplica",
      TRAN_MENT == "9" ~ "Ignorado"
    ),
    
    TRAN_COMP = case_when(
      TRAN_COMP == "1" ~ "Sim",
      TRAN_COMP == "2" ~ "Não",
      TRAN_COMP == "8" ~ "Não se aplica",
      TRAN_COMP == "9" ~ "Ignorado"
    ),
    
    DEF_OUT = case_when(
      DEF_OUT == "1" ~ "Sim",
      DEF_OUT == "2" ~ "Não",
      DEF_OUT == "8" ~ "Não se aplica",
      DEF_OUT == "9" ~ "Ignorado"
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

write.xlsx(dados_violencia_geral,"Manipulados/dados_violencia_geral.xlsx")