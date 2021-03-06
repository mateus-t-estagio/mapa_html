library(leaflet)
library(mapview)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(rmarkdown)
library(lubridate)
library(plotly)
library(jpeg)
library(htmlTable)
library(htmlwidgets)
library(R3port)
library(ggplot2)
library(tidyverse)

####################### MAPA DOS ACIDENTES FERROVIÁRIOS #######################

# Leitura da base de dados de acidentes
acidentes <- readxl::read_excel("Dados/base_acidentes.xlsx")
str(acidentes)
acidentes$`Data da Ocorr�ncia` <- dmy(acidentes$'Data da Ocorr�ncia')

acidentes <- rename(acidentes, "Esta��o Anterior" = Esta��o)
  
acidentes <- rename(acidentes, "Esta��o Posterior" = '...10')

acidentes <- acidentes[2:7830,]

acidentes$N_acidentes <- 1

acidentes <- acidentes %>%
  mutate(mes = format(`Data da Ocorr�ncia`, "%m"),
         ano = format(`Data da Ocorr�ncia`, "%Y"))

acidentes2019 <- acidentes %>% 
  filter(ano == 2019)

base_nomes <- readxl::read_excel("Dados/nome_est.xlsx")
base_nomes <- base_nomes[,c(2,3)]

acidentes2019 <- rename(acidentes2019, "Nome" = "Esta��o Anterior")

acidentes2019 <- plyr::join(acidentes2019, base_nomes, by='Nome', match="first")

acidentes2019$NomeEsta <- paste(acidentes2019$Nome, " (", acidentes2019$C�digo, ")", sep="")
acidentes2019 <- acidentes2019 %>%
  group_by(NomeEsta) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`N� Feridos`),
    Obitos = sum(`N� �bitos`))

acidentes2019e <- acidentes %>% 
  filter(ano == 2019)

acidentes2019e <- rename(acidentes2019e, "Nome Esta��o A" = "Esta��o Anterior")
acidentes2019e <- rename(acidentes2019e, "Nome Esta��o B" = "Esta��o Posterior")
base_nomes <- rename(base_nomes, "Nome Esta��o A" = Nome)

acidentes2019e <- plyr::join(acidentes2019e, base_nomes, by='Nome Esta��o A', match="first")
acidentes2019e <- rename(acidentes2019e, "C�digoA" = C�digo)


base_nomes <- rename(base_nomes, "Nome Esta��o B" = `Nome Esta��o A`)

acidentes2019e <- plyr::join(acidentes2019e, base_nomes, by='Nome Esta��o B', match="first")
acidentes2019e <- rename(acidentes2019e, "C�digoB" = C�digo)

acidentes2019e$`Nome Esta��o A` <- paste(acidentes2019e$`Nome Esta��o A`, " (", acidentes2019e$C�digoA, ")", sep="")
acidentes2019e$`Nome Esta��o B` <- paste(acidentes2019e$`Nome Esta��o B`, " (", acidentes2019e$C�digoB, ")", sep="")

acidentes2019e <- acidentes2019e %>%
  group_by(`Nome Esta��o A`, `Nome Esta��o B`) %>% 
  summarise(
    Acidentes = sum(`N_acidentes`),
    Feridos = sum(`N� Feridos`),
    Obitos = sum(`N� �bitos`))

####################### MAPA DO SUBSISTEMA FERROVIÁRIO FEDERAL #######################



# Leitura das bases georreferenciadas
## myshp é um shapefile do tipo linestring das linhas do SFF. 
## myshp2 é um shapefile do tipo point das Esta��es cadastradas na Declaração de Rede
myshp <- readOGR(dsn=path.expand("shp_atualizado"),
                 layer="dbo_tblLinhaEstacao_spatial_linestring", stringsAsFactors = FALSE)
myshp2 <- readOGR(dsn=path.expand("shp_atualizado"),
                  layer="dbo_tblEstacao_spatial_point", stringsAsFactors = FALSE, encoding = "UTF-8")

# Substituição do CodigoFerr, antes numérico, agora string, com o nome das Esta��es cadastradas na DR
tblFerrovia <- readxl::read_excel("Dados/tblFerrovia.xlsx")
tblFerrovia$CodigoFerr <- tblFerrovia$CodigoFerrovia
myshp@data <- plyr::join(myshp@data,
                         tblFerrovia,
                         by='CodigoFerr')
myshp2@data <- plyr::join(myshp2@data,
                          tblFerrovia,
                          by='CodigoFerr')

# Substituição do CodigoLi00, antes numérico, agora string, com o nome das linhas cadastradas na DR
tblLinha <- readxl::read_excel("Dados/tblLinha.xlsx")
tblLinha$CodigoLi00 <- tblLinha$CodigoLinha
myshp@data <- plyr::join(myshp@data,
                         tblLinha,
                         by='CodigoLi00')

# Substituição do CodigoEsta, antes numérico, agora string, com o C�digo de tres letras
tblEstacao <- readxl::read_excel("Dados/tblEstacao.xlsx")
tblEstacao$CodigoEsta <- tblEstacao$CodigoEstacao
myshp@data <- plyr::join(myshp@data,
                         tblEstacao,
                         by='CodigoEsta')

# Merge da tabela de atributos do shapefile com a Declaração de Rede 2020
DR_2020 <- readxl::read_excel("Dados/dr2020_original.xlsx")
DR_2020$linesta <- paste(DR_2020$Linha, DR_2020$B, sep='!')
myshp@data$linesta <- paste(myshp@data$NomeLinha, myshp@data$CodigoTresLetrasEstacao, sep='!')
myshp@data <- plyr::join(myshp@data,
                         DR_2020,
                         by='linesta')

# Ajustes à tabela de atributos do shapefile: drop de colunas desnecessárias e 
# alteração dos nomes das variáveis
myshp2@data <- select(myshp2@data, -c(CodigoEsta, CodigoFerr, CodigoMuni, DataExclus,
                                      IndicadorP, CodigoPort, CodigoEsca, CodigoEsca,
                                      CodigoArqu, IndicadorT, IndicadorF,LogotipoFerrovia,
                                      DataExclusao, IndicadorObrigatorioDesempenhoProducao))

myshp2@data <- rename(myshp2@data, "Nome Esta��o" = NomeEstaca)
myshp2@data <- rename(myshp2@data, "C�digo Esta��o" = CodigoTres)
myshp2@data <- rename(myshp2@data, "Ferrovia" = NomeFerrovia)

## Composição do nome completo das Esta��es: Nome da Esta��o (C�digo de Três Letras)
df_estac <- tibble(myshp2@data$`C�digo Esta��o`, myshp2@data$`Nome Esta��o`)
colnames(df_estac) <- c('A','NomeA')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='A')
colnames(df_estac) <- c('B','NomeB')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='B')

myshp@data$`Nome Esta��o A` <- paste(myshp@data$NomeA, " (", myshp@data$A,")", sep="")
myshp@data$`Nome Esta��o B` <- paste(myshp@data$NomeB, " (", myshp@data$B,")", sep="")
myshp2@data$`C�digo Esta��o` <- paste(myshp2@data$`Nome Esta��o`, " (", myshp2@data$`C�digo Esta��o`,")", sep="")

myshp@data <- select(myshp@data, -c(B, A, linesta, CodigoEsta, CodigoLi00, CodigoFerr,
                                    CodigoLinh, NumeroSequ, IndicadorC, IndicadorE,
                                    NomeReduzidoFerrovia, LogotipoFerrovia, DataExclusao,
                                    IndicadorObrigatorioDesempenhoProducao, CodigoTresLetrasEstacao,
                                    Ferrovia, CodigoBito, NomeLinha, NomeA, NomeB,
                                    CodigoFerrovia, CodigoLinha, CodigoEstacao))

myshp@data <- rename(myshp@data, "Marco Quilométrico" = NumeroQuil)
myshp@data <- rename(myshp@data, "Extensão do Entre Pátio (km)" = NumeroExte)
myshp@data <- rename(myshp@data, "Ferrovia" = NomeFerrovia)
myshp@data <- rename(myshp@data, "Sigla - Ferrovia" = SiglaFerrovia)

myshp@data <- myshp@data %>%
  select("Ferrovia", "Sigla - Ferrovia", "Nome Esta��o A", "Nome Esta��o B", everything())

acidentes2019 <- rename(acidentes2019, "C�digo Esta��o" = NomeEsta)

myshp2@data <- plyr::join(myshp2@data, acidentes2019, by='C�digo Esta��o', match="first")

shp_acid <- myshp2

shp_acid@data$Acidentes[is.na(shp_acid@data$Acidentes)] <- 0

shp_acid.sub <- shp_acid[shp_acid$Acidentes > 0,]

shp_acid@data <- shp_acid@data %>% 
  filter(Acidentes != 0)

myshp@data <- plyr::join(myshp@data, acidentes2019e, by=c('Nome Esta��o A', 'Nome Esta��o B'), match="first")

# Logo ANTT
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Logo_ANTT.svg/1200px-Logo_ANTT.svg.png"

red = colorRampPalette(c('indianred1', 'red4'))

table_html <- acidentes %>% 
  filter(ano == 2019) %>% 
  group_by(Ferrovia) %>% 
  summarise(
    Acidentes = sum(`N_acidentes`),
    Feridos = sum(`N� Feridos`),
    Obitos = sum(`N� �bitos`)
  )
  
table_html <- htmlTable(table_html)

table_html2 <- acidentes %>% 
  filter(ano==2019) %>% 
  group_by(Natureza) %>% 
  summarise(
    Acidentes = sum(`N_acidentes`),
    Feridos = sum(`N� Feridos`),
    Obitos = sum(`N� �bitos`)
  )

table_html2 <- htmlTable(table_html2)

# Criação do mapa
mapa <- mapview(myshp, zcol="Acidentes",
                legend = TRUE,
                layer.name = '',
                color = red) %>% 
  
  leafem::addLogo(img, width = 120, height = 60, url = "http://www.antt.gov.br/", position="topleft") %>%

  addControl("Pesquisa de Esta��es",
             position = "topleft") %>%
  
  addControl(html=table_html,
             position = "topright") %>%
  
  addControl(html=table_html2,
             position = "topright") %>%
  
  addCircleMarkers(data = myshp2, lng = myshp2@coords[,1],
                   lat=myshp2@coords[,2],
                   popup = ~`C�digo Esta��o`,
                   label=~`C�digo Esta��o`,
                   group='C�digo Esta��o') %>%

  addSearchFeatures(targetGroups = 'C�digo Esta��o',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = FALSE, hideMarkerOnCollapse = FALSE)) %>%
  
  groupOptions('C�digo Esta��o', zoomLevels = 10:30)




               
mapshot(mapa, url = "index.html", selfcontained = TRUE)



