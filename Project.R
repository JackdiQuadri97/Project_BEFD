####libraries####
library(rvest)
library(dplyr)
library(readxl)
library(lmtest)
library(forecast)
library(DIMORA)
library(fpp2)
library(plotly)
library(MASS)
library(nnet)
library(tibble)
library(class)
library(caret)

####function to get ggm catching errors####
errorggm <- function(x){
  result <- tryCatch({
    return(GGM(x, display = F))
  }, error = function(e){
    return(NA)
  })
  return(result)
}
errorexp <- function(x,x_bm_data,display=F){
  result <- tryCatch({
    return(GBM(x,shock="exp", nshock=1,prelimestimates=c(x_bm_data$coefficients,1,-0.1,0.1),display=display))
  }, error = function(e){
    return(NA)
  })
  return(result)
}
errorrett <- function(x,x_bm_data,display=F){
  result <- tryCatch({
    return(GBM(x,shock="rett", nshock=1,prelimestimates=c(x_bm_data$coefficients,1,13,1),display=display))
  }, error = function(e){
    return(NA)
  })
  return(result)
}

#function to normalize
normalize_z_score <- function(x) {
  (x - mean(x)) / sd(x)
}

####scraping####
nums <- list("730","570","578080","1172470","1203220","271590","431960","252490","1938090","1086940","440","236390","1599340","2252570","1238810","1091500","230410","1174180","289070","1966720","1085660",
             "322330","359550","2195250","582010","394360","105600","304930","1449850","1296830","221100","251570","227300","381210","480","550","346110","413150","2399830","1281930","1245620","2357570",
             "218620","1517290","1623660","1222670","1920960","108600","1364780","1238840","238960","594650","513710","261550","1446780","1366540","1568590","892970","582660","438100","444200","250900",
             "1248130","292030","294100","766570","1904540","1466860","518790","813780","1329410","1063730","1551360","1142710","739630","1868140","236850","1928420","1238840","548430","489830","1794680",
             "436150","1919590","646570","1454400","284160","377160","39210","629520","552990","1158310","252950","427520","1665460","1811260","1506830","1313860","311210","476600","433850","1904540",
             "1569040","1263850","1100600","872790","624090","482730","378120","319630","945360","1659040","863550","203140","1240440","1046930","779340","214950","594570","1097150","268500","374320",
             "570940","335300","397540","49520","729040","304390","275850","1817070","1817190")
length(nums)

data_raw <- tibble()
for (num in nums){
  extr <- read_html(paste("https://steamcharts.com/app/",num, sep=""))
  nameextr <- extr %>% html_element("#app-title") %>% html_text()
  dataextr <- extr %>% html_elements("body") %>% html_table() %>%  .[[1]] %>% dplyr::select(c('Month','Avg. Players'))
  
  if (nrow(dataextr) > 12){
    if (nrow(data_raw) == 0){
      data_raw <- dataextr["Month"]
    }
    data_raw[nameextr] <- c(dataextr['Avg. Players'][[1]],rep(NA,nrow(data_raw)-nrow(dataextr)))
  }
}
data <- data_raw %>% filter(Month!="Last 30 Days")

#assigning labels, 0 -> gaas, 1 -> mixed, 2 -> sale
lab <- as.factor(c(1,0,1,0,1,1,1,1,0,2,0,0,0,2,2,0,2,1,1,2,1,2,1,2,0,0,2,1,2,1,
                   1,0,1,1,2,0,2,1,1,0,1,2,2,0,1,2,2,2,2,0,2,1,0,0,2,1,2,2,0,2,
                   1,2,0,2,2,1,2,2,2,2,2,2,2,0,1,2,2,2,2,1,2,0,1,1,2,0,1,1,1,2,
                   2,1,2,2,2,2,2,2,2,2,0,2,2,2,0,0,1,1,2,0,2,2,2,2,2,2,2,1,2,2,
                   2))
####exploratory####
data
str(data)

title = "Dota 2"
dati = rev(data[title][[1]][!is.na(data[title][[1]])])
dati
length(dati)
months = rev(data["Month"][[1]][!is.na(data[title][[1]])])
months

januarytf = grepl("January", months)
listjan = which(januarytf)

plot(dati, xlab="Year",ylab = "Average Players", pch=16, lty=3, xaxt ="n", cex=0.6)
axis(1, at=listjan, labels=months[januarytf])

plot(cumsum(dati), xlab="Year",ylab = "Average Players", pch=16, lty=3, xaxt ="n", cex=0.6)
axis(1, at=listjan, labels=months[januarytf])

bm_data<-BM(dati,display = T)
summary(bm_data)


# Mixed

errormixedauto <- function(x, x_bm_data, a1, b1, c1, a2, b2, c2, display=F){
  result <- tryCatch({
    return(GBM(x, shock="mixed", nshock = 2, prelimestimates = c(x_bm_data$coefficients, a1, b1, c1, a2, b2, c2), display=display))
  }, error = function(e){
    return(NA)
  })
  return(result)
}




##########################################


gbm_data_1<-GBM(dati,shock="exp", nshock=1,
              prelimestimates=c(bm_data$coefficients,
                                1,-0.1,0.1),
              display=T)
summary(gbm_data_1)

gbm_data_2<-GBM(dati,shock="rett", nshock=1,
                prelimestimates=c(bm_data$coefficients,
                                  1,13,1),
                display=T)
summary(gbm_data_2)



ggm_data<-GGM(dati,display = T)
summary(GGM(dati,display = F))
coefficients(GGM(dati,display = F))


colnames(data)

####debug check single bm bgm ggm####
z = 76

z=z+1
title = colnames(data)[z]
title
dati = rev(data[title][[1]][!is.na(data[title][[1]])])
length(dati)
months = rev(data["Month"][[1]][!is.na(data[title][[1]])])
januarytf = grepl("January", months)
listjan = which(januarytf)
plot(dati, xlab="Year",ylab = "Average Players", pch=16, lty=3, xaxt ="n", cex=0.6)
axis(1, at=listjan, labels=months[januarytf])
bm_data<-BM(dati,display = T)
exp_data<-errorexp(dati,bm_data, display = T)
rett_data<-errorrett(dati,bm_data, display = T)
ggm_data<-GGM(dati,display = T)


summary(bm_data)
summary(exp_data)
summary(rett_data)
summary(ggm_data)

####coefficients####

Rsq_bm = c()
Rsq_exp = c()
Rsq_rett = c()
Rsq_ggm = c()
coeffs_bm <- tibble(
  coeff = c("m","p","q")
)
coeffs_exp <- tibble(
  coeff = c("m_exp","p_exp","q_exp","a1_exp","b1_exp","c1_exp")
)
coeffs_rett <- tibble(
  coeff = c("m_rett","p_rett","q_rett","a1_rett","b1_rett","c1_rett")
)
coeffs_ggm <- tibble(
  coeff = c("K","pc","qc","ps","qs")
)

pvals_bm <- tibble(
  coeff = c("m","p","q")
)
pvals_exp <- tibble(
  coeff = c("m_exp","p_exp","q_exp","a1_exp","b1_exp","c1_exp")
)
pvals_rett <- tibble(
  coeff = c("m_rett","p_rett","q_rett","a1_rett","b1_rett","c1_rett")
)
pvals_ggm <- tibble(
  coeff = c("K","pc","qc","ps","qs")
)


for (title in colnames(data)[-1]){
  dati = rev(data[title][[1]][!is.na(data[title][[1]])])
  months = rev(data["Month"][[1]][!is.na(data[title][[1]])])
  januarytf = grepl("January", months)
  listjan = which(januarytf)
  
  bm_data<-BM(dati,display = F)
  ggm_data <- errorggm(dati)
  gbm_data_exp<-errorexp(dati,bm_data)
  gbm_data_rett<-errorrett(dati,bm_data)
  coeffs_bm[title] <- bm_data$coefficients
  pvals_bm[title] <- bm_data$Estimate$`p-value`
  Rsq_bm = c(Rsq_bm,bm_data$Rsquared)
  if (length(gbm_data_exp)==1){
    coeffs_exp[title] <- rep(NA,6)
    pvals_exp[title] <- rep(NA,6)
    Rsq_exp = c(Rsq_exp,NA)
  }
  else{
    coeffs_exp[title] <- gbm_data_exp$coefficients
    pvals_exp[title] <- gbm_data_exp$Estimate$`p-value`
    Rsq_exp = c(Rsq_exp,gbm_data_exp$Rsquared)
  }
  if (length(gbm_data_rett)==1){
    coeffs_rett[title] <- rep(NA,6)
    pvals_rett[title] <- rep(NA,6)
    Rsq_rett = c(Rsq_rett,NA)
  }
  else{
    coeffs_rett[title] <- gbm_data_rett$coefficients
    pvals_rett[title] <- gbm_data_rett$Estimate$`p-value`
    Rsq_rett = c(Rsq_rett,gbm_data_rett$Rsquared)
  }
  if (length(ggm_data)==1){
    coeffs_ggm[title] <- rep(NA,5)
    pvals_ggm[title] <- rep(NA,5)
    Rsq_ggm = c(Rsq_ggm,NA)
  }
  else{
    coeffs_ggm[title] <- ggm_data$coefficients
    pvals_ggm[title] <- ggm_data$Estimate$`p-value`
    Rsq_ggm = c(Rsq_ggm,ggm_data$Rsquared)
  }
}


## Automatic identification of GBM

# Exponential shock

errorexpauto <- function(x, x_bm_data, display=F, k, mem, int){
  result <- tryCatch({
    return(GBM(x,shock="exp", nshock = 1, 
               prelimestimates = c(x_bm_data$coefficients, k, mem, int),
               display=display))
  }, error = function(e){
    return(NA)
  })
  return(result)
}

memory = c(1, 0.5, 0.1, -0.1, -0.5, -1, -2)

intensity = c(-2, -1, 1, 2)

coeff_exp_auto <- tibble(
  coeff = c("m", "p", "q", "a1", "b1", "c1")
)

preliminary_est_esp <- tibble(
  coeff = c("m", "p", "q", "a1", "b1", "c1")
)

max_Rsq_exp_auto = rep(0, length(colnames(data)[-1]))
Rsq_exp_auto = rep(0, length(colnames(data)[-1]))

index = 1
for (title in colnames(data)[-1]){
  dati = rev(data[title][[1]][!is.na(data[title][[1]])])
  
  indeces = seq(from = 1, to = length(dati), by = round(length(dati)/10, 0))
  for (idx in indeces) {
    for (i in intensity) {
      for (m in memory) {
        model = errorexpauto(dati, bm_data, display=F, idx, m, i)
        if(length(model) != 1) {
          Rsq_exp_auto[index] = model$Rsquared
          if(Rsq_exp_auto[index] > max_Rsq_exp_auto[index]) {
            max_Rsq_exp_auto[index] = Rsq_exp_auto[index]
            coeff_exp_auto[index] = model$coefficients
            preliminary_est_esp[index] = c(bm_data$coefficients, idx, m, i)
          }
        }
      }
    }
  }
  print(index)
  index = index + 1
}

max_Rsq_exp_auto
coeff_exp_auto
preliminary_est_esp

# number of models that had a significant improvement in the Rsq
sum(((max_Rsq_exp_auto - Rsq_bm) / (1 - Rsq_bm)) > 0.3) # automatic 
sum(na.omit((Rsq_exp - Rsq_bm) / (1 - Rsq_bm)) > 0.3) # fixed

# Most improved Rsq

imp_exp = which.max(((max_Rsq_exp_auto - Rsq_bm) / (1 - Rsq_bm)))

title_imp_exp = colnames(data)[imp_exp]
title_imp_exp
dati_imp_exp = rev(data[title_imp_exp][[1]][!is.na(data[title_imp_exp][[1]])])

BM(dati_imp_exp, display=T)

GBM(dati_imp_exp, shock="exp", nshock = 1, 
    prelimestimates = preliminary_est_esp[imp_exp] %>% deframe,
    display=T)


# Rectangular shock

errorrettauto <- function(x, x_bm_data, display=F, start, end, int){
  result <- tryCatch({
    return(GBM(x, shock="rett", nshock = 1, 
               prelimestimates = c(x_bm_data$coefficients, start, end, int),
               display=display))
  }, error = function(e){
    return(NA)
  })
  return(result)
}


start = c(1, round(length(dati)/10, 0) ,round(length(dati) * (2/10), 0), round(length(dati) * (3/10), 0), round(length(dati) * (4/10), 0), round(length(dati) * (5/10), 0), round(length(dati) * (6/10), 0), round(length(dati) * (7/10), 0), round(length(dati) * (8/10), 0), round(length(dati) * (9/10), 0))
end = c(round(length(dati)/10, 0) ,round(length(dati) * (2/10), 0), round(length(dati) * (3/10), 0), round(length(dati) * (4/10), 0), round(length(dati) * (5/10), 0), round(length(dati) * (6/10), 0), round(length(dati) * (7/10), 0), round(length(dati) * (8/10), 0), round(length(dati) * (9/10), 0), length(dati))

coeff_rett_auto <- tibble(
  coeff = c("m", "p", "q", "a1", "b1", "c1")
)


preliminary_est_rett <- tibble(
  coeff = c("m", "p", "q", "a1", "b1", "c1")
)

max_Rsq_rett_auto = rep(0, length(colnames(data)[-1]))
Rsq_rett_auto = rep(0, length(colnames(data)[-1]))

index = 1
for (title in colnames(data)[-1]) {
  dati = rev(data[title][[1]][!is.na(data[title][[1]])])
  
  for (i in intensity) {
    for (j in 1:10) {
      s = start[j]
      e = end[j]
      model = errorrettauto(dati, bm_data, display=F, s, e, i)
      if(length(model) != 1) {
        Rsq_rett_auto[index] = model$Rsquared
        if(Rsq_rett_auto[index] > max_Rsq_rett_auto[index]) {
          max_Rsq_rett_auto[index] = Rsq_rett_auto[index]
          coeff_rett_auto[index] = model$coefficients
          preliminary_est_rett[index] = c(bm_data$coefficients, s, e, i)
        }
      }
    }
  }
  print(index)
  index = index + 1
}

Rsq_rett_auto
coeff_rett_auto
preliminary_est_rett

# number of models that had a significant improvement in the Rsq
sum(((max_Rsq_rett_auto - Rsq_bm) / (1 - Rsq_bm)) > 0.3) # automatic 
sum(na.omit((Rsq_rett - Rsq_bm) / (1 - Rsq_bm)) > 0.3) # fixed

# Most improved Rsq

imp_rett = which.max(((max_Rsq_rett_auto - Rsq_bm) / (1 - Rsq_bm)))

title_imp_rett = colnames(data)[imp_rett]
title_imp_rett
dati_imp_rett = rev(data[title_imp_rett][[1]][!is.na(data[title_imp_rett][[1]])])

BM(dati_imp_rett, display=T)

GBM(dati_imp_rett, shock="rett", nshock = 1, 
    prelimestimates = preliminary_est_rett[max_imp_rett] %>% deframe,
    display=T)


#########################################################


Rsq_bm
Rsq_exp
Rsq_rett
Rsq_ggm
Rsq_exp_auto
Rsq_rett_auto
coeffs_bm
coeffs_exp
coeffs_rett
coeffs_ggm
coeff_exp_auto
coeff_rett_auto
pvals_bm
pvals_exp
pvals_rett
pvals_ggm

colnames(coeffs_bm)[-1]

Rsq_exp_bm <- na.fill((1-Rsq_bm)/(1-Rsq_exp), 1)
Rsq_rett_bm <- na.fill((1-Rsq_bm)/(1-Rsq_rett), 1)
Rsq_ggm_bm <- na.fill((1-Rsq_bm)/(1-Rsq_ggm), 1)

plot_ly(x = Rsq_exp_bm, y = Rsq_rett_bm, z = Rsq_ggm_bm, color = lab,
        type = "scatter3d", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "exp"),
                      yaxis = list(title = "rett"),
                      zaxis = list(title = "ggm")))

plot_ly(x = log(Rsq_exp_bm), y = log(Rsq_rett_bm), z = log(Rsq_ggm_bm), color = lab,
        type = "scatter3d", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "log(exp)"),
                      yaxis = list(title = "log(rett)"),
                      zaxis = list(title = "log(ggm)")))

plot_ly(x = log(Rsq_exp_bm), y = log(Rsq_rett_bm), color = lab,
        type = "scatter", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "log(exp)"),
                      yaxis = list(title = "log(rett)")))

# Plots auto Rsq

plot_ly(x = Rsq_exp_auto, y = Rsq_rett_auto, z = Rsq_ggm_bm, color = lab,
        type = "scatter3d", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "exp"),
                      yaxis = list(title = "rett"),
                      zaxis = list(title = "ggm")))

plot_ly(x = log(Rsq_exp_auto), y = Rsq_rett_auto, z = log(Rsq_ggm_bm), color = lab,
        type = "scatter3d", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "log(exp)"),
                      yaxis = list(title = "log(rett)"),
                      zaxis = list(title = "log(ggm)")))

plot_ly(x = log(Rsq_exp_auto), y = Rsq_rett_auto, color = lab,
        type = "scatter", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "log(exp)"),
                      yaxis = list(title = "log(rett)")))

#########################


unlist(coeffs_bm[2,-1])
unlist(coeffs_bm[3,-1])

plot_ly(x = unlist(coeffs_bm[2,-1]), y = unlist(coeffs_bm[3,-1]), color = lab,
        type = "scatter", text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "Q")))


pairs(t(coeffs_bm[,colSums(is.na(coeffs_bm))==0][-1]),labels = coeffs_bm$coeff)


#### 3d plots coeffs_bm####
plot_ly(x = unlist(coeffs_bm[2,-1]), y = unlist(coeffs_bm[3,-1]),
        z = log(unlist(coeffs_bm[1,-1])), color = lab, type = "scatter3d",
        text = colnames(coeffs_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "Q"),
                      zaxis = list(title = "log(M)"),
                      aspectmode = "cube"))

plot_ly(y = unlist(coeffs_bm[2,-1]), x = lab, type = "box",
        text = colnames(coeffs_bm)[-1]) %>%
  layout(yaxis = list(title = "P"), xaxis = list(title = "Groups"))

plot_ly(y = log(unlist(coeffs_bm[2,-1])), x = lab, type = "box",
        text = colnames(coeffs_bm)[-1]) %>%
  layout(yaxis = list(title = "log(P)"), xaxis = list(title = "Groups"))

plot_ly(y = unlist(coeffs_bm[3,-1]), x = lab, type = "box",
        text = colnames(coeffs_bm)[-1]) %>%
  layout(yaxis = list(title = "Q"), xaxis = list(title = "Groups"))

plot_ly(y = unlist(coeffs_bm[1,-1]), x = lab, type = "box",
        text = colnames(coeffs_bm)[-1]) %>%
  layout(yaxis = list(title = "M"), xaxis = list(title = "Groups"))

plot_ly(y = log(unlist(coeffs_bm[1,-1])), x = lab, type = "box",
        text = colnames(coeffs_bm)[-1]) %>%
  layout(yaxis = list(title = "log(M)"), xaxis = list(title = "Groups"))


#### 3d plots pvals_bm####
plot_ly(x = unlist(pvals_bm[2,-1]), y = unlist(pvals_bm[3,-1]),
        z = unlist(pvals_bm[1,-1]), color = lab, type = "scatter3d",
        text = colnames(pvals_bm)[-1], mode = "markers",
        marker = list(size = 5)) %>%
  layout(scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "Q"),
                      zaxis = list(title = "M"),
                      aspectmode = "cube"))

plot_ly(y = unlist(pvals_bm[2,-1]), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "P"), xaxis = list(title = "Groups"))

plot_ly(y = log(unlist(pvals_bm[2,-1])), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "log(P)"), xaxis = list(title = "Groups"))

plot_ly(y = unlist(pvals_bm[3,-1]), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "Q"), xaxis = list(title = "Groups"))

plot_ly(y = log(unlist(pvals_bm[3,-1])), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "log(Q)"), xaxis = list(title = "Groups"))

plot_ly(y = unlist(pvals_bm[1,-1]), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "M"), xaxis = list(title = "Groups"))

plot_ly(y = log(unlist(pvals_bm[1,-1])), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "log(M)"), xaxis = list(title = "Groups"))

plot_ly(y = log(unlist(pvals_bm[1,-1])), x = lab, type = "box",
        text = colnames(pvals_bm)[-1]) %>%
  layout(yaxis = list(title = "log(M)"), xaxis = list(title = "Groups"))


#### table type vs. na_ggm####
addmargins(table(lab,is.na(unlist(pvals_ggm[1,-1]))))


#### modelling ####
labnum <- (as.numeric(lab)-1)/2

var1 <- normalize_z_score(log(unlist(coeffs_bm[1,-1])))
var2 <- normalize_z_score(unlist(coeffs_bm[2,-1]))
var3 <- normalize_z_score(unlist(coeffs_bm[3,-1]))

naexp <- is.na(unlist(pvals_exp[1,-1]))
narett<- is.na(unlist(pvals_rett[1,-1]))
naggm <- is.na(unlist(pvals_ggm[1,-1]))

modeldata <- data.frame(
  lab = lab,
  m = var1,
  p = var2,
  q = var3,
  Rsq_exp_bm = Rsq_exp_bm,
  Rsq_rett_bm = Rsq_rett_bm,
  Rsq_ggm_bm = Rsq_ggm_bm,
  naexp = naexp,
  narett = narett,
  naggm = naggm
)

ord_model <- polr(lab ~ ., data = modeldata, Hess = TRUE)
mult_model <- multinom(lab ~ ., data = modeldata, Hess = TRUE)
logit_model <- glm(labnum ~ var1 + var2 + var3 + Rsq_exp_bm + Rsq_rett_bm + Rsq_ggm_bm + naexp + narett + naggm, family = binomial(link = "logit"))

summary(ord_model)
summary(mult_model)
summary(logit_model)

predicted_values_ord <- predict(ord_model, newdata = modeldata[-1], type = "class")
predicted_values_ord

predicted_values_mult <- predict(mult_model, newdata = modeldata[-1], type = "class")
predicted_values_mult

predicted_values_logit <- predict(logit_model, newdata = modeldata[-1], type = "response")
predicted_values_logit

sum(lab == predicted_values_ord) / length(lab)
sum(lab == predicted_values_mult) / length(lab)


table(lab,predicted_values_ord)
table(lab,predicted_values_mult)

precision <- function(actual, predicted, positive_label) {
  tp <- sum(predicted == positive_label & actual == positive_label)
  fp <- sum(predicted == positive_label & actual != positive_label)
  if (tp + fp == 0) {
    return(0)  # Handling division by zero
  } else {
    return(tp / (tp + fp))
  }
}

recall <- function(actual, predicted, positive_label) {
  tp <- sum(predicted == positive_label & actual == positive_label)
  fn <- sum(predicted != positive_label & actual == positive_label)
  if (tp + fn == 0) {
    return(0)  # Handling division by zero
  } else {
    return(tp / (tp + fn))
  }
}

f1_score <- function(actual, predicted, positive_label) {
  prec <- precision(actual, predicted, positive_label)
  rec <- recall(actual, predicted, positive_label)
  if (prec + rec == 0) {
    return(0)  # Handling division by zero
  } else {
    return(2 * (prec * rec) / (prec + rec))
  }
}

micro_f1_score <- function(actual, predicted) {
  precisions <- recall_values <- f1_scores <- numeric(length = length(unique(actual)))
  for (i in seq_along(unique(actual))) {
    label <- unique(actual)[i]
    precisions[i] <- precision(actual, predicted, label)
    recall_values[i] <- recall(actual, predicted, label)
    f1_scores[i] <- f1_score(actual, predicted, label)
  }
  return(2 * mean(precisions) * mean(recall_values) / (mean(precisions) + mean(recall_values)))
}

micro_f1_ord <- micro_f1_score(lab, predicted_values_ord)
micro_f1_mult <- micro_f1_score(lab, predicted_values_mult)

micro_f1_ord
micro_f1_mult


# KNN

train = length(modeldata$lab)*0.7

train_labels <- modeldata$lab[0:train]
train_predictors <- modeldata[0:train, -1]  # Excluding the label column

test_labels <- modeldata$lab[train:length(modeldata$lab)]
test_predictors <- modeldata[train:length(modeldata$lab), -1]  # Excluding the label column

kmax = 100

test_error = numeric(kmax)

for (k in 1:kmax) {
  knn_pred = as.factor(knn(train = train_predictors, test = test_predictors,
                           cl = train_labels, k = k))
  cm = confusionMatrix(data = knn_pred, reference = test_labels)
  test_error[k] = 1 - cm$overall[1]
}


k_min = which.min(test_error)
k_min

knn = knn(train = train_predictors, test = test_predictors,
          cl = train_labels, k = k_min)

knn_pred_min = as.factor(knn)

table(test_labels, knn)

micro_f1_knn <- micro_f1_score(test_labels, knn_pred_min)

#### modelling [0:60] ####
modeldata <- data.frame(
  lab = lab[0:60],
  m = var1[0:60],
  p = var2[0:60],
  q = var3[0:60],
  Rsq_exp_bm = Rsq_exp_bm[0:60],
  Rsq_rett_bm = Rsq_rett_bm[0:60],
  Rsq_ggm_bm = Rsq_ggm_bm[0:60],
  naexp = naexp[0:60],
  narett = narett[0:60],
  naggm = naggm[0:60]
)

ord_model <- polr(lab ~ ., data=modeldata, Hess = TRUE)
mult_model <- multinom(lab ~ ., data=modeldata, Hess = TRUE)
logit_model <- glm(labnum ~ var1 + var2 + var3 + Rsq_exp_bm + Rsq_rett_bm + Rsq_ggm_bm + naexp + narett + naggm, family = binomial(link = "logit"))


summary(ord_model)
summary(mult_model)
summary(logit_model)

predicted_values_ord <- predict(ord_model, newdata = modeldata, type = "class")
predicted_values_ord

predicted_values_mult <- predict(mult_model, newdata = modeldata, type = "class")
predicted_values_mult

sum(lab[0:60] == predicted_values_ord) / length(lab[0:60])
sum(lab[0:60] == predicted_values_mult) / length(lab[0:60])


table(lab[0:60],predicted_values_ord)
table(lab[0:60],predicted_values_mult)


micro_f1_ord <- micro_f1_score(lab[0:60], predicted_values_ord)
micro_f1_mult <- micro_f1_score(lab[0:60], predicted_values_mult)

micro_f1_ord
micro_f1_mult


# KNN

train = length(modeldata$lab)*0.7

train_labels <- modeldata$lab[0:train]
train_predictors <- modeldata[0:train, -1]  # Excluding the label column

test_labels <- modeldata$lab[train:length(modeldata$lab)]
test_predictors <- modeldata[train:length(modeldata$lab), -1]  # Excluding the label column

kmax = 100

test_error = numeric(kmax)

for (k in 1:kmax) {
  knn_pred = as.factor(knn(train = train_predictors, test = test_predictors,
                           cl = train_labels, k = k))
  cm = confusionMatrix(data = knn_pred, reference = test_labels)
  test_error[k] = 1 - cm$overall[1]
}


k_min = which.min(test_error)
k_min

knn = knn(train = train_predictors, test = test_predictors,
          cl = train_labels, k = k_min)

knn_pred_min = as.factor(knn)

table(test_labels, knn)

micro_f1_knn <- micro_f1_score(test_labels, knn_pred_min)



################### ucrcd #################

errorucrcd <- function(x,y,bool){
  result <- tryCatch({
    return(UCRCD(x,y, display = bool))
  }, error = function(e){
    return(NA)
  })
  return(result)
}


#manual
title1 = colnames(data)[100]
title1
dati1 = rev(data[title1][[1]][!is.na(data[title1][[1]])])
length(dati1)
months1 = rev(data["Month"][[1]][!is.na(data[title1][[1]])])
januarytf = grepl("January", months1)
listjan = which(januarytf)

title2 = colnames(data)[99]
title2
dati2 = rev(data[title2][[1]][!is.na(data[title2][[1]])])
length(dati2)
months2 = rev(data["Month"][[1]][!is.na(data[title2][[1]])])

plot(dati1, xlab = "Year", ylab = "Average Players", pch = 16, lty = 3, xaxt = "n", cex = 0.6, col = 1)
axis(1, at = listjan, labels = months1[januarytf])
start_index_dati2 <- length(dati1) - length(dati2) + 1
points(start_index_dati2:(start_index_dati2 + length(dati2) - 1), dati2, type = "b", pch = 2, lty = 3, cex = 0.7, col = 3)
legend("topright", legend = c(title1, title2), pch = c(16, 2), col = c(1, 3))

ucrcd = NULL
ucrcd<- UCRCD(dati1,dati2,display=T) 
summary(ucrcd)
ucrcd$coefficients


#batch all titles

ucrcd_params <- tibble(
  param = c("title1","title2","ma","p1a","q1a","mc","p1c","q1c+delta","q1c","p2","q2", "q2-gamma","rsq")
)

ucrcd_pvals <- tibble(
  coeff = c("title1","title2","ma","p1a","q1a","mc","p1c","p2","q1c","q2", "delta","gamma")
)

i=1
for (title1 in colnames(data)[-1]){
  print(paste(i, (format(Sys.time(), "%Y-%m-%d %H:%M:%S")), sep= " "))
  dati1 = rev(data[title1][[1]][!is.na(data[title1][[1]])])
  months1 = rev(data["Month"][[1]][!is.na(data[title1][[1]])])
  
  for (title2 in colnames(data)[-1]){
    if (title1 != title2){
      dati2 = rev(data[title2][[1]][!is.na(data[title2][[1]])])
      months2 = rev(data["Month"][[1]][!is.na(data[title2][[1]])])
      
      if (length(months1) > length(months2)){
        ucrcd<- errorucrcd(dati1,dati2,F)
        s = paste(title1,title2,sep=" ** ")
        if (length(ucrcd)==1){
          ucrcd_params[s] <- c(title1,title2,rep(NA,11))
          ucrcd_pvals[s] <- c(title1,title2,rep(NA,10))
        } else{
          ucrcd_params[s] <- c(title1,title2,ucrcd$coefficients,ucrcd$Rsquared)
          ucrcd_pvals[s] <- c(title1,title2,ucrcd$Estimate$`p-value`)
        }
      }
    }
  }
  i = i+1
} 

ucrcd_params
ucrcd_pvals

v <- c(3,4,5,6,7,8,9,10,11,12)
#function to check if every pvalue in column is significant
f <- function(col){
  result <- TRUE
  
  for (i in v){
    if (is.na(as.numeric(col[i])) || as.numeric(col[i]) > 0.01){
      result <- FALSE
      return(result)
    }
  }
  return(result)
}

#params with significant pvalues
ucrdcd_params_significant <- tibble(
  param = c("title1","title2","ma","p1a","q1a","mc","p1c","q1c+delta","q1c","p2","q2", "q2-gamma","rsq")
)
ucrcd_pvals_significant <- tibble(
  coeff = c("title1","title2","ma","p1a","q1a","mc","p1c","p2","q1c","q2", "delta","gamma")
)
for (col in names(ucrcd_pvals)[-1]) {
  column <- ucrcd_pvals[[col]]
  if (f(column)){
    ucrdcd_params_significant[col] = c(ucrcd_params[[col]])
    ucrcd_pvals_significant[col] = c(ucrcd_pvals[[col]])
  }
}
ucrdcd_params_significant
ucrcd_pvals_significant
