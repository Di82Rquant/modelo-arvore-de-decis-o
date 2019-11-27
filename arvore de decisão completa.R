"www.outspokenmarket.com"

require(ggplot2)
library(data.table)
library(tidyr)
require(reshape2)
library(stats)
library(rattle)
library(caret)
library(ROCR)
library(party)
library(dplyr)
library(mlbench)
library(randomForest)
library(flexclust)
library(cluster)
library(Hmisc)
library(boot)
library(fpc)
library(xgboost)
library(readr)
library(stringr)
library(car)
library(pROC)
require(Matrix)
library(fTrading)
library(rpart.plot)
library(RColorBrewer)


# fread{data.table}
ibov <- fread("C:/Users/Di82R/Desktop/ibov_trees.csv", header=T, sep=";")
ibov <- as.data.frame(ibov)
names(ibov)

ibovModelDB <- ibov

#periodo de validação para confirmação de estrategia
#periodo de 2017 

ibov_validacao <- ibovModelDB[1960:dim(ibovModelDB)[1],]


#dados de amostra
ibovModelDB <- ibovModelDB[1:1959,]




seed = 19
set.seed(seed)
#createDatapartition {caret}
inTrain <- createDataPartition(y=ibovModelDB$TARGET,p = 0.8, list=FALSE)

#periodo de treinamento e teste
training <- ibovModelDB[inTrain,] # 80%
testing <- ibovModelDB[-inTrain,] # 20%

dim(training);dim(testing)

# criando arvore

fit <- rpart(TARGET ~ .,
             data = training[,8:21],
             method = "class",
             control = rpart.control(minsplit = 2,cp = 0.007))


#plotando arvore de decisão

fancyRpartPlot(fit)

#aplicando predição

prediction_testing <- predict(fit,testing,type = "class")
summary(prediction_testing)

table(testing$TARGET,prediction_testing)

table(testing$TARGET,prediction_testing)[4]/(table(testing$TARGET,prediction_testing
                                                   )[3]+table(testing$TARGET,
                                                              prediction_testing
                                                              )[4])*100

prediction_2017 <- predict(fit,ibov_validacao,type = "class")
ibov_validacao$prediction_2017 <- prediction_2017
buy <- subset(ibov_validacao,ibov_validacao$prediction_2017==1)[,1:7]
names(buy)
cumulative_buy_pontos <- cumsum(buy$Pontos)


#plotando

plot(cumulative_buy_pontos,type = "l",
     ylab = "pontos acumulados",
      xlab = "execusão em 2017")
title(main = list("resultado IBOV-arvore de decisão",cex = 1.5,
                  col = "red",font = 2))

# averiguação

sum(buy[6])
# 3.619 pontos

SHARPE <- mean(buy$Pontos,na.rm = T)/sd(buy$Pontos,na.rm = T)
SHARPE
# 0.1770588

maxDrawDown(cumulative_buy_pontos)
# drawdown 1.926 pontos de intervalo 9 a 14 que
# refere-se a linha de eixo X

maxDrawDown(cumulative_buy_pontos)$maxdrawndown[1]/sum(buy[6])*100
#representa a porcentagem de drawndown






















