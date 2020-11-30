library(tidyverse)
library(neuralnet)
library(caret)

# Import File
setwd("C:/Users/alfat/Desktop/STIS/5/DMNK/New folder")
data_echo<-read.csv("echo/echocardiogram.data", header = F)
View(data_echo)

data<-data_echo
View(data)
## Fungsi Normalisasi
normalisasi <- function(r){
  return((r-min(r))/(max(r)-min(r)))
}

# normalisasi semua atribut kecuali target class
for(i in colnames(data[-7])){
  data[ ,i]=normalisasi(data[ ,i])
}
## Split Data
set.seed(666)
sampel <- sample(2,nrow(data),replace = T, prob = c(0.8,0.2))
trainingdat <- data[sampel==1, ]
testingdat <- data[sampel==2, ]
print(paste("Jumlah train data :", nrow(trainingdat)))

set.seed(223)
## Model 1
#model dengan 1 hidden layer dan hidden node
model1<-neuralnet(V13~V3+V4+V5+V6+V7+V9, data=trainingdat,
                   hidden = 1,
                   err.fct = "ce",
                   linear.output = F)
plot(model1)
### Evaluasi Model 1
prediksi1 <- compute(model1, testingdat[ ,-7])
pred1 <- ifelse(prediksi1$net.result>0.5, 1, 0)
head(pred1)
confusionMatrix(table(pred1, testingdat$V13))
#Akurasi 73,33

set.seed(223)
## Model 2
#model dengan 1 hidden layer dan 5 hidden node
model2<-neuralnet(V13~V3+V4+V5+V6+V7+V9, data=trainingdat,
                  hidden = 6,
                  err.fct = "ce",
                  linear.output = F)
plot(model2)
### Evaluasi Model 2
prediksi2 <- compute(model2, testingdat[ ,-7])
pred2 <- ifelse(prediksi2$net.result>0.5, 1, 0)
head(pred2)
confusionMatrix(table(pred2, testingdat$V13))
#Akurasi 66,7
str(pred1)
View(testingdat$V13)
set.seed(223)
## Model 3
#model dengan 2 hidden layer dan masing-masing 5 hidden node dan 4 node
model3<-neuralnet(V13~V3+V4+V5+V6+V7+V9, data=trainingdat,
                  hidden = c(5,4),
                  err.fct = "ce",
                  linear.output = F)
plot(model3)
### Evaluasi Model 3
prediksi3 <- compute(model3, testingdat[ ,-7])
pred3 <- ifelse(prediksi3$net.result>0.5, 1, 0)
head(pred3)
confusionMatrix(table(pred3, testingdat$V13))
#Akurasi 60

set.seed(223)
## Model 4
#model dengan 3 hidden layer dan masing-masing 5 hidden node dan 4 node dan 2 node
model4<-neuralnet(V13~V3+V4+V5+V6+V7+V9, data=trainingdat,
                  hidden = c(5,4,2),
                  err.fct = "ce",
                  linear.output = F)
plot(model4)
### Evaluasi Model 4
prediksi4 <- compute(model4, testingdat[ ,-7])
pred4 <- ifelse(prediksi4$net.result>0.5, 1, 0)
head(pred4)
confusionMatrix(table(pred4, testingdat$V13))
#Akurasi 76,67


set.seed(223)
## Model 5
#model dengan 3 hidden layer dan masing-masing 5 hidden node dan 4 node dan 3 node
model5<-neuralnet(V13~V3+V4+V5+V6+V7+V9, data=trainingdat,
                  hidden = c(10,8,4,1),
                  err.fct = "ce",
                  linear.output = F)
plot(model5)
### Evaluasi Model 5
prediksi5 <- compute(model5, testingdat[ ,-7])
pred5 <- ifelse(prediksi5$net.result>0.5, 1, 0)
head(pred5)
confusionMatrix(table(pred5, testingdat$V13))
#Akurasi 80


















