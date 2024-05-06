#loading libraries
library(psych)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(ggplot2)
library(daltoolbox)

source("funcoes_analise_ch.R")

#loading data
dados <-read.csv("event_export_2022-07-20.csv", header = TRUE, sep = ";")
#head(dados)
#nrow(dados)
#ncol(dados)

#List of players
players <- read.csv("player_export_2022-07-20.csv", header = TRUE, sep = ";")
#head(players)
#nrow(players)
#ncol(players)

#Avaliation Join Tables
aval_turma_bio <- read.csv("join_tabelas_aval-2022-07-20.CSV", header = TRUE, sep = ";")
#head(aval_turma_bio)
#nrow(aval_turma_bio)
#ncol(aval_turma_bio)

#Avaliation Join Tables
aval_turma_info <- read.csv("join_tabelas_aval-2022-06-09.CSV", header = TRUE, sep = ";")
#head(aval_turma_info)
#nrow(aval_turma_info)
#ncol(aval_turma_info)


grp3_joel<-aval_turma_info %>% filter(grp==3)
#grp3_joel
#nrow(grp3_joel)

##### filtrando turma do joel #####
set.seed(123)
grp3_joel<-sample_n(grp3_joel, 7)

aval_turma_info <- rbind(grp3_joel %>% select (key,grp),aval_turma_info %>% filter(grp==1|grp==2|grp==4) %>% select (key,grp))
#nrow(aval_turma_info)


aval_participantes <- rbind(aval_turma_bio %>% select (key,grp),aval_turma_info %>% select (key,grp))
#aval_participantes
aval_participantes %>% select (grp) %>% group_by(grp) %>% summarise(n = n())


#### equilibrando os grupos
grp1<-aval_participantes %>% filter(grp==1)
grp2<-aval_participantes %>% filter(grp==2)
grp3<-aval_participantes %>% filter(grp==3)
grp4<-aval_participantes %>% filter(grp==4)

set.seed(123)
grp1<-sample_n(grp1, 14)
grp2<-sample_n(grp2, 14)
grp3<-sample_n(grp3, 14)
grp4<-sample_n(grp4, 14)

aval_participantes <- rbind(grp1,grp2)
aval_participantes <- rbind(aval_participantes,grp3)
aval_participantes <- rbind(aval_participantes,grp4)
#aval_participantes
aval_participantes %>% select (grp) %>% group_by(grp) %>% summarise(n = n())


####################### filtrando alunos do grupo 2 e 4 ###################
aval_participantes_g2eg4 <- aval_participantes %>% filter (grp %in% c(2,4))
aval_participantes_g2eg4
#nrow(aval_participantes_g2eg4)


############# filtrando sessoes de jogo ###################
aval_sessions <- players %>% filter (expcode %in% aval_participantes_g2eg4$key)
#head(aval_sessions)
#nrow(aval_sessions)
#ncol(aval_sessions)

############### última sessão de cada jogador ############
ultimas_sessoes <- aval_sessions %>% select (expcode,id) %>% group_by(expcode) %>% summarise(session = max(id))
#ultimas_sessoes
#nrow(ultimas_sessoes)
ultimas_sessoes<-ultimas_sessoes$session
#ultimas_sessoes


dados$id <- NULL #removing the id column

#summary(dados)

#Turning categorical attributes into factors
cols = c("type", "agent")
fac.dados = dados
fac.dados[cols] = lapply(dados[cols], factor)

#Filtering events of actual avaliation sessions
#fac.dados <- fac.dados %>% filter (session %in% todas_sessoes)
fac.dados <- fac.dados %>% filter (session %in% ultimas_sessoes)


#plants
pop.plant = fac.dados[c(1,2,3,4)] %>% filter((type=="meta cumprida")|((type=="plantar"|type=="Predacao"|type=="colher") & (agent=="tomate"|agent=="tomate em desenvolvimento"|agent=="milho"|agent=="milho em desenvolvimento"|agent=="couve"|agent=="couve em desenvolvimento"|agent=="grama"|agent=="grama em desenvolvimento")))
pop.plant = pop.plant[order(pop.plant$session, pop.plant$time),]

#pests  
pop.plague = fac.dados[c(1,2,3,4)] %>% filter((type=="meta cumprida")|((type=="morte"|type=="Predacao"|type=="Novo inseto") & (agent=="grilo"|agent=="lagarta"|agent=="pulgao"|agent=="cigarrinha")))
pop.plague = pop.plague[order(pop.plague$session, pop.plague$time),]

#predators
pop.predator = fac.dados[c(1,2,3,4)] %>% filter((type=="meta cumprida")|((type=="morte"|type=="Novo inseto"|type=="remover predador") & (agent=="besouro"|agent=="joaninha")))
pop.predator = pop.predator[order(pop.predator$session, pop.predator$time),]

gap = 10


input <- fac.dados %>% filter(type=="meta cumprida") %>% group_by(session,type)
input = input[order(input$session, input$time),]
max_meta_time = 0
actual_session = 0
for(i in 1:nrow(input)){
  session <- input[i,]$session
  if(session != actual_session){
    actual_session <- session
    total_meta = 0
  }
  total_meta = total_meta + 1
  if(total_meta == 8){
    max_meta_time = max(input[i,]$time,max_meta_time)
  }
}
max_meta_time


#creating plant population series
#num_col = floor(max(pop.plant$time)/gap) + 2
num_col = floor(max_meta_time/gap) + 1
series.pop.plant <- create_dataframe(num_col, gap)
actual_session = 0
line = 0
total_plant = 0
total_meta = 0
for(i in 1:length(pop.plant$session)){
  session <- pop.plant[i,]$session
  if(session != actual_session){
    line = line + 1
    actual_session <- session
    total_plant = 0
    total_meta = 0
    series.pop.plant[line, 1] <- actual_session
  }
  if(pop.plant[i,]$type == "plantar"){
    total_plant = total_plant + 1
  }
  if(pop.plant[i,]$type == "Predacao" || pop.plant[i,]$type == "colher"){
    total_plant = total_plant - 1
  }
  if(pop.plant[i,]$type == "meta cumprida"){
    total_meta = total_meta + 1
  }
  if(total_meta < 8){
    col = floor(pop.plant[i,]$time/gap)+2
    if(total_plant < 0) total_plant = 0
    series.pop.plant[line, col] <- total_plant
  }
}
series.pop.plant <- fill_na(series.pop.plant, nrow(series.pop.plant), pop.plant)
#series.pop.plant

#creating pest population series
#num_col = floor(max(pop.plague$time)/gap) + 2
num_col = floor(max_meta_time/gap) + 1
series.pop.plague <- create_dataframe(num_col, gap)
actual_session = 0
line = 0
total_plague = 0
total_meta = 0
for(i in 1:length(pop.plague$session)){
  session <- pop.plague[i,]$session
  if(session != actual_session){
    line = line + 1
    actual_session <- session
    total_plague = 0
    total_meta = 0
    series.pop.plague[line, 1] <- actual_session
  }
  if(pop.plague[i,]$type == "Novo inseto"){
    total_plague = total_plague + 1
  }
  if(pop.plague[i,]$type == "Predacao" || pop.plague[i,]$type == "morte"){
    total_plague = total_plague - 1
  }
  if(pop.plague[i,]$type == "meta cumprida"){
    total_meta = total_meta + 1
  }
  if(total_meta < 8){
    col = floor(pop.plague[i,]$time/gap)+2
    if(total_plague<0) total_plague = 0
    series.pop.plague[line, col] <- total_plague
  }
}
series.pop.plague <- fill_na(series.pop.plague, nrow(series.pop.plague), pop.plague)

#creating predator population series
#num_col = floor(max(pop.predator$time)/gap) + 2
num_col = floor(max_meta_time/gap) + 1
series.pop.predator <- create_dataframe(num_col, gap)
actual_session = 0
line = 0
total_predator = 0
total_meta = 0
for(i in 1:length(pop.predator$session)){
  session <- pop.predator[i,]$session
  if(session != actual_session){
    line = line + 1
    actual_session <- session
    total_predator = 0
    total_meta = 0
    series.pop.predator[line, 1] <- actual_session
  }
  if(pop.predator[i,]$type == "Novo inseto"){
    total_predator = total_predator + 1
  }
  if(pop.predator[i,]$type == "morte" || pop.predator[i,]$type == "remover predador"){
    total_predator = total_predator - 1
  }
  if(pop.predator[i,]$type == "meta cumprida"){
    total_meta = total_meta + 1
  }
  if(total_meta < 8){
    col = floor(pop.predator[i,]$time/gap)+2
    if(total_predator < 0) total_predator = 0
    series.pop.predator[line, col] <- total_predator
  }
}
series.pop.predator <- fill_na(series.pop.predator, nrow(series.pop.predator), pop.predator)




#########################################
### Realizando a normalizacao dos dados
#########################################



###Winsorizing the series to remove outliers 2%
wseries.pop.plant <- series.pop.plant
wseries.pop.plant[,2:length(wseries.pop.plant)] <- psych::winsor(wseries.pop.plant[,2:length(wseries.pop.plant)], trim = 0.02)

wseries.pop.plague <- series.pop.plague
wseries.pop.plague[,2:length(wseries.pop.plague)] <- psych::winsor(wseries.pop.plague[,2:length(wseries.pop.plague)], trim = 0.02)

wseries.pop.predator <- series.pop.predator
wseries.pop.predator[,2:length(wseries.pop.predator)] <- psych::winsor(wseries.pop.predator[,2:length(wseries.pop.predator)], trim = 0.02)

#####APPLYING Z-SCORE
zscore_plant = wseries.pop.plant
zscore_plant[,2:length(zscore_plant)] = scale(zscore_plant[,2:length(zscore_plant)])
zscore_plant[is.na(zscore_plant)] <- 0

zscore_plague = wseries.pop.plague
zscore_plague[,2:length(zscore_plague)] = scale(zscore_plague[,2:length(zscore_plague)])
zscore_plague[is.na(zscore_plague)] <- 0

zscore_predator = wseries.pop.predator
zscore_predator[,2:length(zscore_predator)] = scale(zscore_predator[,2:length(zscore_predator)])
zscore_predator[is.na(zscore_predator)] <- 0


###############################
### Realizando a clusterizaçao
###############################


num_col <- min(ncol(series.pop.plant),ncol(series.pop.plant),ncol(series.pop.predator))  ##TOTAL DE COLUNAS
num_col
data_serie <- return_series_comb(series.pop.plant, series.pop.plague, series.pop.predator, ultimas_sessoes, num_col)


set.seed(222)

#####CLUSTERING

zdata_serie <- return_series_comb(zscore_plant, zscore_plague, zscore_predator, ultimas_sessoes, num_col)


#######  CLUSTERING WITH K=2  #############################

model_k2 <- return_cluster_model(zdata_serie,2)
result_cluster_k2 <- return_cluster(zdata_serie,model_k2)
##Visualizando dados
data_serie$k <- result_cluster_k2
line_cluster_sep(data_serie, 10, num_col, "Aggregate Plant series with k=2", "Aggregate Plague series with k=2", "Aggregate Predator series with k=2",0,10,0,12,0,12)
count(data_serie, var = k)


#######  CLUSTERING WITH K=3  #############################

model_k3 <- return_cluster_model(zdata_serie,3)
result_cluster_k3 <- return_cluster(zdata_serie,model_k3)
##Visualizando dados
data_serie$k <- result_cluster_k3
line_cluster_sep(data_serie, 10, num_col, "Aggregate Plant series with k=3", "Aggregate Plague series with k=3", "Aggregate Predator series with k=3",0,10,0,12,0,12)
count(data_serie, var = k)


#######  CLUSTERING WITH K=4  #############################

model_k4 <- return_cluster_model(zdata_serie,4)
result_cluster_k4 <- return_cluster(zdata_serie,model_k4)
##Visualizando dados
data_serie$k <- result_cluster_k4
line_cluster_sep(data_serie, 10, num_col, "Aggregate Plant series with k=4", "Aggregate Plague series with k=4", "Aggregate Predator series with k=4",0,10,0,12,0,12)
count(data_serie, var = k)


#######  CLUSTERING WITH K=5  #############################

model_k5 <- return_cluster_model(zdata_serie,5)
result_cluster_k5 <- return_cluster(zdata_serie,model_k5)
##Visualizando dados
data_serie$k <- result_cluster_k5
line_cluster_sep(data_serie, 10, num_col, "Aggregate Plant series with k=5", "Aggregate Plague series with k=5", "Aggregate Predator series with k=5",0,10,0,12,0,12)
count(data_serie, var = k)


##########################################################
#### Evaluating Clusters 
##########################################################

data_serie$k <- result_cluster_k2
evaluate(model_k2, result_cluster_k2, data_serie$k)
data_serie$k <- result_cluster_k3
evaluate(model_k3, result_cluster_k3, data_serie$k)
data_serie$k <- result_cluster_k4
evaluate(model_k4, result_cluster_k4, data_serie$k)
data_serie$k <- result_cluster_k5
evaluate(model_k5, result_cluster_k5, data_serie$k)



#sqrt(n/2) -> n pontos de amostra
# n = 27
# k = 3,6

# elbow method -> Technically, given a number, k > 0, we can form k clusters on the data set in question using a clustering 
# algorithm like k-means, and calculate the sum of within-cluster variances, var(k). We can
# then plot the curve of var with respect to k. The first (or most significant) turning point of the curve
# suggests the “right” number.


# silhouette coefficient
library(cluster)
library(factoextra)

####### K2  

sil <- silhouette(result_cluster_k2, dist(data_serie))
# plot silhouette
fviz_silhouette(sil)
#Average = 0.28

####### K3  

sil <- silhouette(result_cluster_k3, dist(data_serie))
# plot silhouette
fviz_silhouette(sil)
#Average = 0.12


####### K4  

sil <- silhouette(result_cluster_k4, dist(data_serie))
# plot silhouette
fviz_silhouette(sil)
#Average = 0.09


####### K5  

sil <- silhouette(result_cluster_k5, dist(data_serie))
# plot silhouette
fviz_silhouette(sil)
#Average = 0.05

avarage_sil = data.frame (
  K = c(2,3,4,5),
  Average = c(0.28,0.12,0.09,0.05)
)
  
ggplot(avarage_sil, aes(x=K, y=Average)) +
  geom_line(size=1)+
  labs(title="Average Silhouette",x ="Number of Clusters", y = "Average")+
  scale_y_continuous(limits = c(0, 0.3))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.line = element_line(size = 2, colour = "grey80"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size=14))