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

####################### MAPA DOS ACIDENTES FERROVIÃRIOS #######################

# Leitura da base de dados de acidentes
acidentes <- readxl::read_excel("Dados/base_acidentes.xlsx")
str(acidentes)
acidentes$`Data da Ocorrência` <- dmy(acidentes$'Data da Ocorrência')

acidentes <- rename(acidentes, "Estação Anterior" = Estação)
  
acidentes <- rename(acidentes, "Estação Posterior" = '...10')

acidentes <- acidentes[2:7830,]

acidentes$N_acidentes <- 1

acidentes <- acidentes %>%
  mutate(mes = format(`Data da Ocorrência`, "%m"),
         ano = format(`Data da Ocorrência`, "%Y"))

acidentes2019 <- acidentes %>% 
  filter(ano == 2019)

base_nomes <- readxl::read_excel("Dados/nome_est.xlsx")
base_nomes <- base_nomes[,c(2,3)]

acidentes2019 <- rename(acidentes2019, "Nome" = "Estação Anterior")

acidentes2019 <- plyr::join(acidentes2019, base_nomes, by='Nome', match="first")

acidentes2019$NomeEsta <- paste(acidentes2019$Nome, " (", acidentes2019$Código, ")", sep="")
acidentes2019 <- acidentes2019 %>%
  group_by(NomeEsta) %>% 
  summarise(
    Acidentes = sum(N_acidentes),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`))

acidentes2019e <- acidentes %>% 
  filter(ano == 2019)

acidentes2019e <- rename(acidentes2019e, "Nome Estação A" = "Estação Anterior")
acidentes2019e <- rename(acidentes2019e, "Nome Estação B" = "Estação Posterior")
base_nomes <- rename(base_nomes, "Nome Estação A" = Nome)

acidentes2019e <- plyr::join(acidentes2019e, base_nomes, by='Nome Estação A', match="first")
acidentes2019e <- rename(acidentes2019e, "CódigoA" = Código)


base_nomes <- rename(base_nomes, "Nome Estação B" = `Nome Estação A`)

acidentes2019e <- plyr::join(acidentes2019e, base_nomes, by='Nome Estação B', match="first")
acidentes2019e <- rename(acidentes2019e, "CódigoB" = Código)

acidentes2019e$`Nome Estação A` <- paste(acidentes2019e$`Nome Estação A`, " (", acidentes2019e$CódigoA, ")", sep="")
acidentes2019e$`Nome Estação B` <- paste(acidentes2019e$`Nome Estação B`, " (", acidentes2019e$CódigoB, ")", sep="")

acidentes2019e <- acidentes2019e %>%
  group_by(`Nome Estação A`, `Nome Estação B`) %>% 
  summarise(
    Acidentes = sum(`N_acidentes`),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`))

####################### MAPA DO SUBSISTEMA FERROVIÃRIO FEDERAL #######################



# Leitura das bases georreferenciadas
## myshp Ã© um shapefile do tipo linestring das linhas do SFF. 
## myshp2 Ã© um shapefile do tipo point das Estações cadastradas na DeclaraÃ§Ã£o de Rede
myshp <- readOGR(dsn=path.expand("shp_atualizado"),
                 layer="dbo_tblLinhaEstacao_spatial_linestring", stringsAsFactors = FALSE)
myshp2 <- readOGR(dsn=path.expand("shp_atualizado"),
                  layer="dbo_tblEstacao_spatial_point", stringsAsFactors = FALSE, encoding = "UTF-8")

# SubstituiÃ§Ã£o do CodigoFerr, antes numÃ©rico, agora string, com o nome das Estações cadastradas na DR
tblFerrovia <- readxl::read_excel("Dados/tblFerrovia.xlsx")
tblFerrovia$CodigoFerr <- tblFerrovia$CodigoFerrovia
myshp@data <- plyr::join(myshp@data,
                         tblFerrovia,
                         by='CodigoFerr')
myshp2@data <- plyr::join(myshp2@data,
                          tblFerrovia,
                          by='CodigoFerr')

# SubstituiÃ§Ã£o do CodigoLi00, antes numÃ©rico, agora string, com o nome das linhas cadastradas na DR
tblLinha <- readxl::read_excel("Dados/tblLinha.xlsx")
tblLinha$CodigoLi00 <- tblLinha$CodigoLinha
myshp@data <- plyr::join(myshp@data,
                         tblLinha,
                         by='CodigoLi00')

# SubstituiÃ§Ã£o do CodigoEsta, antes numÃ©rico, agora string, com o Código de tres letras
tblEstacao <- readxl::read_excel("Dados/tblEstacao.xlsx")
tblEstacao$CodigoEsta <- tblEstacao$CodigoEstacao
myshp@data <- plyr::join(myshp@data,
                         tblEstacao,
                         by='CodigoEsta')

# Merge da tabela de atributos do shapefile com a DeclaraÃ§Ã£o de Rede 2020
DR_2020 <- readxl::read_excel("Dados/dr2020_original.xlsx")
DR_2020$linesta <- paste(DR_2020$Linha, DR_2020$B, sep='!')
myshp@data$linesta <- paste(myshp@data$NomeLinha, myshp@data$CodigoTresLetrasEstacao, sep='!')
myshp@data <- plyr::join(myshp@data,
                         DR_2020,
                         by='linesta')

# Ajustes Ã  tabela de atributos do shapefile: drop de colunas desnecessÃ¡rias e 
# alteraÃ§Ã£o dos nomes das variÃ¡veis
myshp2@data <- select(myshp2@data, -c(CodigoEsta, CodigoFerr, CodigoMuni, DataExclus,
                                      IndicadorP, CodigoPort, CodigoEsca, CodigoEsca,
                                      CodigoArqu, IndicadorT, IndicadorF,LogotipoFerrovia,
                                      DataExclusao, IndicadorObrigatorioDesempenhoProducao))

myshp2@data <- rename(myshp2@data, "Nome Estação" = NomeEstaca)
myshp2@data <- rename(myshp2@data, "Código Estação" = CodigoTres)
myshp2@data <- rename(myshp2@data, "Ferrovia" = NomeFerrovia)

## ComposiÃ§Ã£o do nome completo das Estações: Nome da Estação (Código de TrÃªs Letras)
df_estac <- tibble(myshp2@data$`Código Estação`, myshp2@data$`Nome Estação`)
colnames(df_estac) <- c('A','NomeA')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='A')
colnames(df_estac) <- c('B','NomeB')
myshp@data <- plyr::join(myshp@data,
                         df_estac,
                         by='B')

myshp@data$`Nome Estação A` <- paste(myshp@data$NomeA, " (", myshp@data$A,")", sep="")
myshp@data$`Nome Estação B` <- paste(myshp@data$NomeB, " (", myshp@data$B,")", sep="")
myshp2@data$`Código Estação` <- paste(myshp2@data$`Nome Estação`, " (", myshp2@data$`Código Estação`,")", sep="")

myshp@data <- select(myshp@data, -c(B, A, linesta, CodigoEsta, CodigoLi00, CodigoFerr,
                                    CodigoLinh, NumeroSequ, IndicadorC, IndicadorE,
                                    NomeReduzidoFerrovia, LogotipoFerrovia, DataExclusao,
                                    IndicadorObrigatorioDesempenhoProducao, CodigoTresLetrasEstacao,
                                    Ferrovia, CodigoBito, NomeLinha, NomeA, NomeB,
                                    CodigoFerrovia, CodigoLinha, CodigoEstacao))

myshp@data <- rename(myshp@data, "Marco QuilomÃ©trico" = NumeroQuil)
myshp@data <- rename(myshp@data, "ExtensÃ£o do Entre PÃ¡tio (km)" = NumeroExte)
myshp@data <- rename(myshp@data, "Ferrovia" = NomeFerrovia)
myshp@data <- rename(myshp@data, "Sigla - Ferrovia" = SiglaFerrovia)

myshp@data <- myshp@data %>%
  select("Ferrovia", "Sigla - Ferrovia", "Nome Estação A", "Nome Estação B", everything())

acidentes2019 <- rename(acidentes2019, "Código Estação" = NomeEsta)

myshp2@data <- plyr::join(myshp2@data, acidentes2019, by='Código Estação', match="first")

shp_acid <- myshp2

shp_acid@data$Acidentes[is.na(shp_acid@data$Acidentes)] <- 0

shp_acid.sub <- shp_acid[shp_acid$Acidentes > 0,]

shp_acid@data <- shp_acid@data %>% 
  filter(Acidentes != 0)

myshp@data <- plyr::join(myshp@data, acidentes2019e, by=c('Nome Estação A', 'Nome Estação B'), match="first")

# Logo ANTT
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/2/29/Logo_ANTT.svg/1200px-Logo_ANTT.svg.png"

red = colorRampPalette(c('indianred1', 'red4'))

table_html <- acidentes %>% 
  filter(ano == 2019) %>% 
  group_by(Ferrovia) %>% 
  summarise(
    Acidentes = sum(`N_acidentes`),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`)
  )
  
table_html <- htmlTable(table_html)

table_html2 <- acidentes %>% 
  filter(ano==2019) %>% 
  group_by(Natureza) %>% 
  summarise(
    Acidentes = sum(`N_acidentes`),
    Feridos = sum(`Nº Feridos`),
    Obitos = sum(`Nº Óbitos`)
  )

table_html2 <- htmlTable(table_html2)

# CriaÃ§Ã£o do mapa
mapa <- mapview(myshp, zcol="Acidentes",
                legend = TRUE,
                layer.name = '',
                color = red) %>% 
  
  leafem::addLogo(img, width = 120, height = 60, url = "http://www.antt.gov.br/", position="topleft") %>%

  addControl("Pesquisa de Estações",
             position = "topleft") %>%
  
  addControl(html=table_html,
             position = "topright") %>%
  
  addControl(html=table_html2,
             position = "topright") %>%
  
  addCircleMarkers(data = myshp2, lng = myshp2@coords[,1],
                   lat=myshp2@coords[,2],
                   popup = ~`Código Estação`,
                   label=~`Código Estação`,
                   group='Código Estação') %>%

  addSearchFeatures(targetGroups = 'Código Estação',
                    options = searchFeaturesOptions(
                      zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
                      autoCollapse = FALSE, hideMarkerOnCollapse = FALSE)) %>%
  
  groupOptions('Código Estação', zoomLevels = 10:30)




               
mapshot(mapa, url = "index.html", selfcontained = TRUE)



addCircleMarkers(data = shp_acid.sub, lng = shp_acid.sub@coords[,1],
                 lat=shp_acid.sub@coords[,2],
                 #radius=5,
                 popup = paste("Estação: ", shp_acid.sub$`Código Estação`, "<br>",
                               "Acidentes: ", shp_acid.sub$Acidentes, "<br>",
                               "Feridos: ", shp_acid.sub$Feridos, "<br>",
                               "Óbitos: ", shp_acid.sub$Obitos),
                 label=~Acidentes,
                 color=~pal(Acidentes),
                 stroke = FALSE,
                 fillOpacity = 0.5,
                 group='Acidentes')
