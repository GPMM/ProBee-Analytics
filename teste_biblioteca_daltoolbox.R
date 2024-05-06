# DAL ToolBox
# version 1.0.767

source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox/main/jupyter.R")

#loading DAL
load_library("daltoolbox")

#load dataset
data(iris)

# setup clustering
model <- cluster_kmeans(k=3)

# build model
model <- fit(model, iris[,1:4])
clu <- cluster(model, iris[,1:4])
table(clu)

iris_minmax <- transform(fit(minmax(), iris), iris)
model <- fit(model, iris_minmax[,1:4])
clu <- cluster(model, iris_minmax[,1:4])
table(clu)

# evaluate model using external metric
eval <- evaluate(model, clu, iris_minmax$Species)
eval

# evaluate model using external metric
eval <- evaluate(model, clu, iris$Species)
eval