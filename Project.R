####libraries####
library(rvest)
library(dplyr)
library(readxl)
library(lmtest)
library(forecast)
library(DIMORA)
library(fpp2)

####function to get ggm catching errors####
errorggm <- function(x){
  result <- tryCatch({
    return(GGM(dati, display = F))
  }, error = function(e){
    return(NA)
  })
  return(result)
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
  dataextr <- extr %>% html_elements("body") %>% html_table() %>%  .[[1]] %>% select(c('Month','Avg. Players'))
  
  if (nrow(dataextr) > 12){
    if (nrow(data_raw) == 0){
      data_raw <- dataextr["Month"]
    }
    data_raw[nameextr] <- c(dataextr['Avg. Players'][[1]],rep(NA,nrow(data_raw)-nrow(dataextr)))
  }
}
data <- data_raw %>% filter(Month!="Last 30 Days")

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
summary(BM(dati,display = T))

ggm_data<-GGM(dati,display = T)
summary(GGM(dati,display = T))

colnames(data)

####debug check single ggm####
z = 80

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
ggm_data<-GGM(dati,display = T)


####coefficients####

coeffs <- tibble(
  coeff= c("m","p","q","K","pc","qc","ps","qs")
)

for (title in colnames(data)[-1]){
  dati = rev(data[title][[1]][!is.na(data[title][[1]])])
  months = rev(data["Month"][[1]][!is.na(data[title][[1]])])
  januarytf = grepl("January", months)
  listjan = which(januarytf)
  
  bm_data<-BM(dati,display = F)
  ggm_data <- errorggm(dati)
  if (is.na(ggm_data)){
    coeffs[title] <- c(bm_data$coefficients,rep(NA,5))
  }
  else{
    coeffs[title] <- c(bm_data$coefficients,ggm_data$coefficients)
  }
}

coeffs
colnames(coeffs)

unlist(coeffs[2,-1])
unlist(coeffs[3,-1])

plot(x = unlist(coeffs[2,-1]), y = unlist(coeffs[3,-1]), pch=16, lty=3, cex=0.6)
pairs(coeffs)

pairs(t(coeffs[,colSums(is.na(coeffs))==0][-1]),labels = coeffs$coeff)