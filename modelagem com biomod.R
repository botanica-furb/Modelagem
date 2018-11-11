install.packages("biomod2")
install.packages("ecospat")
library(biomod2)
library(ecospat)

#antes de come�ar, importe seus dados de ocorr�ncia em ".csv", sendo:
# Uma coluna "id" com n�meros que identifiquem seus pontos
# Uma coluna "x" com longitudes e outra "y" com latitudes
# Uma coluna indicando a presen�a da esp�cie (1) em cada coordenada.

DataSpecies <- Dicksonia.sellowiana #Nome do arquivo csv importado
myRespName <- 'Dsellowiana'          #T�tulo da coluna que indica a presen�a da esp�cie em cada coordenada
myResp <- as.numeric(DataSpecies[,myRespName])
myRespXY <- DataSpecies[,c("x","y")]            #"x" e "y" correspondem aos t�tulos das colunas longitude e latitude, respectivamente


#Importando as var�veis ambientais que ser�o analisadas. 
#Indique a pasta do seu computador em que as vari�veis se encontram.

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
                                     PA.nb.absences = 5000,
                                     PA.strategy = 'random',
                                     PA.table = NULL,
                                     na.rm = TRUE)


########################## DEFININDO CONFIGURA��ES DOS ALGORITMOS #####################################

myBiomodOptions <- BIOMOD_ModelingOptions()



############################################### MODELAGEM ###############################################################
#### models = Algor�timos que ser�o utilizados na modelagem, cada um tem suas especificidaes. T�RRES, N. M., 2010. explica alguns 
#### NbRunEval = N�mero de repeti��es dos testes de avalia��o dos modelos
#### DataSplit = porcentagem de dados que ser�o usados para calibrar o modelo, o restante ser� usado para a avalia��o
#### models.eval.meth = Modelos estat�sticos usados para avaliar a modelagem, "ROC" � o mais utilizado, mas existem outros (TSS, por ex.).
####VarImport = N�mero de repeti��es para testar a import�ncia de cada vari�vel no modelo

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


get_evaluations(myBiomodModelOut)
get_variables_importance(myBiomodModelOut)


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


get_evaluations(myBiomodEM)

########################################## PROJETANDO OS MODELOS INDIVIDUAIS ################################################# 		
#### Antes de obter a proje��o consenso dos algorItmos, � necess�rio projetar os modelos individuais
#### new.env = vari�veis ambientais em que os modelos ser�o projetados, � poss�vel por ex. modelar para America do Sul e projetar s� para SC. Depende da inten��o do pesquisador.
#### ao deixar "new.env = myExpl" vc estar� usando as vari�veis ambientais selecionadas no come�o, e � com base nelas que os modelos ser�o projetadas. 


myBiomodProjection <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                        new.env = myExpl,
                                        proj.name = 'currentFINAL',
                                        selected.models = 'all',
                                        binary.meth = NULL,
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)


############################################### UNINDO AS PROJE��ES #############################################################

mybiomodEF <- BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
                              EM.output = myBiomodEM)





