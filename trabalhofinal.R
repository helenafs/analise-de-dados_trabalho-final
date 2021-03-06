# install.packages( c( "SAScii" , "downloader" , "survey" , "ggplot2" ) )

library(survey) 	# load survey package (analyzes complex design surveys)
library(SAScii) 	# load the SAScii package (imports ascii data with a SAS script)
library(downloader)	# downloads and then runs the source() function on scripts from github

tf <- tempfile()


# download the latest pns microdata
download.file("ftp://ftp.ibge.gov.br/PNS/2013/Microdados/Dados/PNS_2013.zip" , 
              tf , mode = 'wb' )

# extract all files to the local disk
setwd ("C:\\Users\\helen\\Documents\\Mestrado\\An�lise de Dados\\Trabalho final\\microdados")

z <- unzip("pns_2013_microdados_2017_03_23.zip")

# identify household (domicilio) data file
dd <- grep("Dados/DOMPNS", z, value = TRUE)

# identify person data file
pd <- grep("Dados/PESPNS", z, value = TRUE)

# identify household (domicilio) sas import script
ds <- grep("DOMPNS(.*)\\.sas", z, value = TRUE)

# identify person sas import script
ps <- grep("PESPNS(.*)\\.sas", z, value = TRUE)

# create a data.frame object `dom` containing one record per household
dom <- read.SAScii(dd, ds)

# create a data.frame object `pes` containing one record per person
pessoas <- read.SAScii(pd, ps)


# ## Salvar em excel
#install.packages("writexl")
#library(writexl)

#write_xlsx(dom, "domicilios.xlsx", use_zip64 = T)
#write_xlsx(pes, "pessoas.xlsx", use_zip64 = T)

#Selecionar as colunas de interesse

library(tidyverse)
glimpse (pessoas)

banco_selecionado <- pessoas %>%
  select (V0001, C001:G032, M001:N023, W00101:VDDATAM)

#Criar a vari�vel de categoriza��o PCD, categorias: intelectual, auditiva, visual, f�sica 
#sem defici�ncia (m�dulo g)
 
banco_deficiencia <- banco_selecionado %>%
  mutate(deficiencia = case_when(G001 == "1" ~ "dintelectual", 
                                 G006 == "1" ~ "df�sica",
                                 G014 == "1" ~"dauditiva", 
                                 G021 == "1" ~ "dvisual",
                                 G001 == "2" & G006 == "2" & G014 == "2" & G021 == "2" ~ "nenhuma"))


table(banco_deficiencia$deficiencia)

#Criar outra vari�vel com tr�s grupos deficientes com limita��o, deficientes sem limita��o e 
#n�o deficientes. As vari�veis que indicam limita��o: G004, G009, G017, G026. 

banco_deficiencia <- banco_deficiencia %>%
  mutate (limita = case_when (G004 == "1" ~ "n�o limita", G004 == "2" ~ "um pouco", G004 == "3" ~ "moderadamente", G004 == "4" ~ "intensamente", G004 == "5" ~ "intensamente", 
                              G009 == "1" ~ "n�o limita", G009 == "2" ~ "um pouco", G009 == "3" ~ "moderadamente", G009 == "4" ~ "intensamente", G009 == "5" ~ "intensamente", 
                              G017 == "1" ~ "n�o limita", G017 == "2" ~ "um pouco", G017 == "3" ~ "moderadamente", G017 == "4" ~ "intensamente", G017 == "5" ~ "intensamente",
                              G026 == "1" ~ "n�o limita", G026 == "2" ~ "um pouco", G026 == "3" ~ "moderadamente", G026 == "4" ~ "intensamente", G026 == "5" ~ "intensamente")) 

table(banco_deficiencia$limita)


#Criar a vari�vel de renda total (dependente): E01602 (sal�rio principal), 
#E01604 (produtos principal),E01802 (sal�rio secund�rio), E01804 (produtos secund�rios).

banco_renda <- banco_deficiencia %>% 
  rowwise() %>% 
  mutate(salario = sum(E01602,E01604,E01802,E01804, na.rm = TRUE))

summary (banco_renda$salario)

#Criar a vari�vel de controle de rendimentos domiciliares n�o provenientes do trabalho: 
#F00102 (pens�o do governo),F00702 (doa��o), F00802 (aluguel), VDF00102 (juros, seguros)


banco_renda <- banco_renda %>%
  rowwise() %>% 
  mutate(renda_domiciliar = sum(F00102,F00702,F00802,VDF00102, na.rm = TRUE))

summary (banco_renda$renda_domiciliar)

# Filtrar pela idade (C008) e renomear a vari�vel 

table(banco_renda$C008, banco_renda$VDE001)

filtrado_idade <- banco_renda %>%
  mutate(C008 = as.numeric(C008))%>%
  filter(C008 >= 14) %>%
  rename(idade = C008)

table(filtrado_idade$idade)

### Renomear e recodificar as vari�veis: C006 (sexo), VDE001 (Condi��o em rela��o � for�a de trabalho na semana de refer�ncia para pessoas de 14 anos ou mais de idade), VDD004 (n�vel de instru��o mais alto)


filtrado_idade <- filtrado_idade %>%
  mutate(sexo = case_when(C006 == "1" ~ "masculino",
                          C006 == "2" ~ "feminino")) %>%
  mutate (trabalho = case_when(VDE001 == "1" ~ "sim",
                               VDE001 == "2" ~ "n�o")) %>%
  mutate (escolaridade = case_when(VDD004 == "1" ~ "sem_instru��o",
                                   VDD004 == "2" ~ "fund_incompleto",
                                   VDD004 == "3" ~ "fund_completo",
                                   VDD004 == "4" ~ "m�dio_incompleto",
                                   VDD004 == "5" ~ "m�dio_completo",
                                   VDD004 == "6" ~ "sup_incompleto",
                                   VDD004 == "7" ~ "sup_completo"))


### Recategorizar a vari�vel de cor (C009), transformar em dummy - branco e n�o branco. Branco (1), n�o-branco (2, 3, 4, 5 e 9) (Becker, 2018). 

filtrado_idade <- filtrado_idade %>% 
  mutate(cor = case_when( C009 == "1" ~ "branco",
                          C009 == "2" ~ "n�o_branco", 
                          C009 == "3" ~ "n�o_branco",
                          C009 == "4" ~ "n�o_branco",
                          C009 == "5" ~ "n�o_branco",
                          C009 == "9" ~ "n�o_branco" )) %>%
  mutate(cor = as.factor(cor))

levels(filtrado_idade$cor)

### Criar a categoria de regi�o a partir da vari�vel (V0001) de Estados. 

filtrado_idade <- filtrado_idade %>% 
  mutate(regiao = case_when(V0001 >= 11 & V0001 < 18 ~ "Norte",
                            V0001 >= 21 & V0001 < 30 ~ "Nordeste",
                            V0001 >= 31 & V0001 < 36 ~ "Sudeste",
                            V0001 >= 41 & V0001 < 44 ~ "Sul",
                            V0001 >= 50 & V0001 < 54 ~ "Centro-oeste")) %>%
  mutate(regiao = as.factor(regiao))

levels(filtrado_idade$regiao)

### Vari�vel de percep��o de estado de sa�de 

filtrado_idade <- filtrado_idade %>% 
  mutate(estadosaude = case_when(N001 == "1" ~ "muito boa",
                                 N001 == "2" ~ "boa",
                                 N001 == "3" ~ "regular",
                                 N001 == "4" ~ "ruim",
                                 N001 == "5" ~ "muito ruim"))%>% 
  mutate(estadosaude = as.factor(estadosaude))
  
levels (filtrado_idade$estadosaude)

