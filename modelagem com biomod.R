install.packages("biomod2")
library(biomod2)

#antes de começar, importe seus dados de ocorrência em ".csv", sendo:
# Uma coluna "id" com números que identifiquem seus pontos
# Uma coluna "x" com longitudes e outra "y" com latitudes
# Uma coluna indicando a presença da espécie (1) em cada coordenada.

DataSpecies <- Dicksonia.sellowiana #Nome do arquivo csv importado
myRespName <- 'Dsellowiana'          #Título da coluna que indica a presença da espécie em cada coordenada
myResp <- as.numeric(DataSpecies[,myRespName])
myRespXY <- DataSpecies[,c("x","y")]            #"x" e "y" correspondem aos títulos das colunas longitude e latitude, respectivamente


#Importando as varáveis ambientais que serão analisadas. 
#Indique a pasta do seu computador em que as variáveis se encontram.

myExpl <- stack ("C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio04.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio05.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio06.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio13.asc",
                 "C:/Users/chrussi/OneDrive/Faculdade/Mudancas climaticas/camadas/current/bio14.asc")


myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 1,
                                     PA.nb.absences = 10000,
                                     PA.strategy = 'random',
                                     PA.table = NULL,
                                     na.rm = TRUE)


########################## DEFININDO CONFIGURAÇÕES DOS ALGORITMOS #####################################

myBiomodOptions <- BIOMOD_ModelingOptions()



############################################### MODELAGEM ###############################################################
#### models = Algorítimos que serão utilizados na modelagem, cada um tem suas especificidaes. TÔRRES, N. M., 2010. explica alguns 
#### NbRunEval = Número de repetições dos testes de avaliação dos modelos
#### DataSplit = porcentagem de dados que serão usados para calibrar o modelo, o restante será usado para a avaliação
#### models.eval.meth = Modelos estatísticos usados para avaliar a modelagem, "ROC" é o mais utilizado, mas existem outros (TSS, por ex.).
####VarImport = Número de repetições para testar a importância de cada variável no modelo

myBiomodModelOut <- BIOMOD_Modeling( myBiomodData, 
                                     models = c('MAXENT.Phillips','GBM', 'RF'), 
                                     models.options = myBiomodOptions, 
                                     NbRunEval=10, 
                                     DataSplit=70, 
                                     VarImport=3, 
                                     models.eval.meth = c('ROC'), 
                                     SaveObj = TRUE,
                                     do.full.models=FALSE,
                                     modeling.id="modelo1")


get_evaluations(myBiomodModelOut) # Avaliações estatísticas por algoritmo
get_variables_importance(myBiomodModelOut) # Importância das variáveis


######################################### UNINDO OS MODELOS ######################################################
### Caso a modelagem tenha rodado perfeitamente, chegou a hora de unir os dados gerados.

myBiomodEM <- BIOMOD_EnsembleModeling( modeling.output = myBiomodModelOut,
                                       chosen.models = 'all',
                                       em.by = 'PA_dataset+repet',
                                       eval.metric = 'ROC',
                                       eval.metric.quality.threshold = c(0.700),
                                       models.eval.meth = c('ROC'),
                                       prob.mean = TRUE,
                                       prob.cv = FALSE,
                                       prob.ci = FALSE,
                                       prob.ci.alpha = 0.05,
                                       prob.median = FALSE,
                                       committee.averaging = FALSE,
                                       prob.mean.weight = TRUE,
                                       prob.mean.weight.decay = 'proportional' ) 


get_evaluations(myBiomodEM) # Avaliação estatística modelo consenso

########################################## PROJETANDO OS MODELOS INDIVIDUAIS ################################################# 		
#### Antes de obter a projeção consenso dos algorItmos, é necessário projetar os modelos individuais
#### new.env = variáveis ambientais em que os modelos serão projetados, é possível por ex. modelar para America do Sul e projetar só para SC. Depende da intenção do pesquisador.
#### ao deixar "new.env = myExpl" vc estará usando as variáveis ambientais selecionadas no começo, e é com base nelas que os modelos serão projetadas. 


myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = myExpl,
                                        proj.name = 'current',
                                        selected.models = 'all',
                                        binary.meth = NULL,
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)


############################################### UNINDO AS PROJEÇÕES #############################################################

mybiomodEF <- BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
                              EM.output = myBiomodEM)


# Um arquivo ".grd" será gerado na pasta do R com a distribuição da espécie, da seguinte forma: "nomedoprojeto_ensemble_nomedasp.grd" 




