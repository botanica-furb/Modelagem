install.packages("biomod2")
install.packages("ecospat")
library(biomod2)
library(ecospat)

#antes de começar, importe seus dados de ocorrência em ".csv", sendo:
# Uma coluna "id" com números que identifiquem seus pontos
# Uma coluna "x" com longitudes e outra "y" com latitudes
# Uma coluna indicando a presença da espécie (1) em cada coordenada.

DataSpecies <- Dicksonia.sellowiana #Nome do arquivo csv importado
myRespName <- 'Dsellowiana'          #Título da coluna que indica a presença da espécie em cada coordenada
myResp <- as.numeric(DataSpecies[,myRespName])
myRespXY <- DataSpecies[,c("x","y")]            #"x" e "y" correspondem aos títulos das colunas longitude e latitude, respectivamente


#Importando as varáveis ambientais que serão analisadas. As 19 do world clim por ex.
#Indique a pasta do seu computador em que as variáveis se encontram.

myExpl <- stack ("C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio01.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio02.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio03.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio04.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio05.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio06.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio07.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio08.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio09.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio10.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio11.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio12.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio13.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio14.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio15.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio16.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio17.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio18.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio19.asc")


myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 1,
                                     PA.nb.absences = 5000,
                                     PA.strategy = 'random',
                                     PA.table = NULL,
                                     na.rm = TRUE)


# se nenhum erro ocorreu, os valores ambientais foram extraídos
valoresAMB <- myBiomodData@data.env.var



#Calculando a correlação:
# será gerado uma tabela com a correlação entre as variáveis, exporte-a como pdf que ficará melhor para vizualizar os dados

data <- valoresAMB[,1:19]  # 1:19=intervalo de colunas contendo valores ambientais
ecospat.cor.plot(data)      

#Analisando os eixos da PCA

pca.env<-dudi.pca(rbind(valoresAMB)[,1:19],scannf=FALSE,nf=2)
pca.env[["co"]]     #será gerada uma tabela com a variação de cada uma das variáveis em cada eixo

# escolha as variáveis com maior pontuação no eixo 1, depois no eixo 2, sempre analisando o pdf gerado anteriormente para saber a correlação entre as variáveis escolhidas
# variáveis com mais de 75% de correlação entre si não podem ser utilizadas, nesse caso, escolha a de maior contribuição nos eixos

ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
