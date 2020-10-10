library(lmtest)
library(corrplot)
library(skimr)
library(rpart)

# Preprocesado de los datos
air <- dataset_import()

# Regresión y ACP
corprint(air,'../Images/correlation1.png')

# Eliminar la variable dependiente (en este caso PT08.S4.NO2)
air.acp_data <- air[-c(9)]

air.acp <- acp_analysis(air.acp_data)
air.acp$rotation[,1:3]
components <- air.acp$x[,1:3]
air.regr <- cbind(scale(air.std$PT08.S4.NO2.), components)

colnames(air.regr)[1] <- "PT08.S4.NO2."

air.regr

corprint(air.regr, "../Images/correlation2.png")

png(filename="../Images/pairs1.png")
pairs(air.regr)
dev.off()

regression <- lm_analysis(air.regr, formula = "PT08.S4.NO2.~.")
summary(regression)


# fin regresión

#Clúster
cluster <- hclus_analysis(air.std, 3)

kmeans_analysis(air.std, 4)

kmeans_analysis(air.std, 3)

decision_tree_analysis(air, "PT08.S4.NO2.~.")

air.std <- as.data.frame(scale(air))

hclus_analysis(air.std, 3)

# Segundo intento sin las variables con mas valores faltantes

air.reduced <- air[-c(1,3,6,8)]

corprint(air.reduced, "../Images/corr_reduced.png")

symnum(cor(air.reduced))

air.reduced.std <- as.data.frame(scale(air.reduced))

air.reduced.acp <- acp_analysis(air.reduced.std[-c(3)]) #pt08.s2.nmhc

pt08.s2.nmhc <- as.data.frame(air.reduced.std[3])

lm.data <- cbind(pt08.s2.nmhc, air.reduced.acp$x[,1:3])

lm_analysis(as.data.frame(lm.data), formula = "PT08.S2.NMHC. ~.",pairpath = "../Images/pairs_second_try.png",qqplotpah = "../Images/qqplot_second_try", histogrampaht = "../Images/hist_second_try", homocedasticiditypath = "../Images/homocedasticity_second_try")

decision_tree_analysis(air.reduced, "PT08.S2.NMHC. ~.", '../Images/decision_tree_second_try.png')


# Tercer intento con la temperatura como variable dependiente 

air.reduced.acp2 <- acp_analysis(air.reduced.std[-c(7)]) #T

T <- as.data.frame(air.reduced.std[7])

lm.data2 <- cbind(T, air.reduced.acp2$x[,1:3])

lm_analysis(as.data.frame(lm.data2), formula = "T ~.",pairpath = "../Images/pairs_second_try2.png",qqplotpah = "../Images/qqplot_second_try2.png", histogrampaht = "../Images/hist_second_try2.png", homocedasticiditypath = "../Images/homocedasticity_second_try2.png")

decision_tree_analysis(air.reduced, "T ~.", '../Images/decision_tree_second_try2.png')


# ANOVA

columnsize <- length(air[,1])

sensors <- c(rep('PT08.S1(CO)', columnsize), rep('PT08.S2(NMHC)', columnsize), rep('PT08.S3(NOx)', columnsize), rep('PT08.S4(NO2)', columnsize), rep('PT08.S5(O3)', columnsize))

replies <- c(air$PT08.S1.CO., air$PT08.S2.NMHC., air$PT08.S3.NOx., air$PT08.S4.NO2., air$PT08.S5.O3.)

df <- data.frame(sensors, replies)

png(filename='../Images/boxplot_anova.png')
plot(replies~sensors, data=df)
dev.off()

sensors.anova <- aov(replies~sensors, data=df)
summary(sensors.anova)

res <- sensors.anova$residuals

png(filename='../Images/sup.png')
layout(matrix(c(1,2,3,4),2,2,byrow = TRUE))
plot(res)
plot(sensors.anova$fitted.values, rstudent(sensors.anova),
     main="Anova Studentized Residuals",
     xlab="Predictions",ylab= "Studentized Resid",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)

hist(res, main="Histograma de Residuos")
qqnorm(res)
qqline(res)
dev.off()

shapiro.test(res)

bartlett.test(res, df$sensors)

dwtest(sensors.anova)


summary(regression)

corprint(merge)
corprint(as.data.frame(cbind(air, news)))

gt <- air[,c(1,3,4,6,8,10:13)]

pt08 <- air[,c(2,4,5,7,9,10:13)]

corprint(gt)

corprint(pt08)


png(filename='../Images/boxplot.png')
pairs(data)
dev.off()

symnum(M)

data3[,1]

new

skim(new)
skim(data3)

write.csv(new, "new.csv")
kmeans_analysis <- function(dataset, k){
  km <- kmeans(dataset, k)
  
  print(km)
  
  png(filename='../Images/kmeans1.png')
  plot(dataset, col=km$cluster)
  dev.off()
}

decision_tree_analysis <- function(dataset, model, treepath = '../Images/decision_tree.png'){
  l <- length(dataset[,1])
  
  sub <- sample(1:l, 2*l/3)
  
  diag.tree <- rpart(model ,data = dataset[sub,], cp=0.01, maxdepth=3)
  
  summary(diag.tree)
  
  png(filename=treepath)
  plot(diag.tree)
  text(diag.tree, use.n = TRUE, pretty=1, xpd=TRUE)
  dev.off()
  
  plotcp(diag.tree)
  printcp(diag.tree)
  
  diag.pred <- predict(diag.tree, newdata = dataset[-sub,], type="vector")
  
  tb <- table(diag.pred, dataset[-sub,]$PT08.S4.NO2. )
  
  error.rpart <- 1-(sum(diag(tb))/sum(tb))
  print(tb)
  tb
  print(error.rpart)
}

hclus_analysis <- function(dataset, k,method = "euclidean"){
  
  d <- dist(dataset, method=method)
  
  fit <- hclust(d, method = 'complete')
  
  png("../Images/cluster1.png")
  plot(fit)
  #d2 <- as.dendrogram(fit)
  #plot(d2)
  rect.hclust(fit, k=k, border='red')
  dev.off()
  return(fit)
}

lm_analysis <- function(dataset, formula = "V1 ~ .", pairpath='../Images/pairs1.png', qqplotpah='../Images/qqplot1.png', histogrampaht='../Images/hist1.png', homocedasticiditypath='../Images/standard3.png'){
  regres <- lm(formula, data= as.data.frame(dataset))
  
  png(filename=pairpath)
  pairs(as.data.frame(dataset))
  dev.off()
  
  png(filename=qqplotpah)
  qqnorm(regres$residuals)
  qqline(regres$residuals)
  dev.off()
  
  png(filename=histogrampaht)
  hist(regres$residuals)
  dev.off()
  
  png(filename=homocedasticiditypath)
  plot(dataset[,1], rstandard(regres), ylab = "Residuos estandarizados", xlab = "PT08.S4.NO2")
  abline(h =0, lty = 2)
  dev.off()
  
  print(mean(regres$residuals))
  print(sum(regres$residuals))

  print(dwtest(regres))

  return(regres)
}

acp_analysis <- function(dataset){
  acp <- prcomp(dataset, scale=TRUE)
  print(summary(acp))
  return(acp)
}

corprint <- function(data, path){
  M <- cor(data)
  png(filename = path)
  corrplot(M,method = 'number')
  dev.off()
}

# Rellenar datos faltantes con la media de los mismos
fill <- function(dataset , start_index = 3){
  copy <- dataset
  for (i in start_index: ncol(copy)){
    target <- copy[,i]
    mean <- mean(target, na.rm = TRUE)
    for (j in 1:nrow(copy)){
      if(is.na(copy[j,i])){
        copy[j,i] <- mean
      }
    }
  }
  return(copy)
}

preprocess <- function(dataset){
  #v <- c()
  for (i in 1: nrow(dataset)){
    for (j in 1: ncol(dataset)){
      if(!is.na(dataset[i,j]) && is.numeric(dataset[i,j]) && dataset[i,j] == -200){
        dataset[i,j] <- NA
      }
    }
    #tmp <- as.numeric(strsplit(as.character(dataset[i,2]), ".00.00"))
    #v[i] <- tmp
  }
  #dataset$hours <- v
  return(dataset)
}

dataset_import <- function(){
  data <- read.csv2('../data/AirQualityUCI.csv', header = TRUE, na.strings = c(""," ","-200"))
  step1 <- preprocess(data)
  print(skim(step1))
  step2 <- fill(step1)
  print(skim(step2))
  return(step2[,3:15])
}