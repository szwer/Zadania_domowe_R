#java -jar selenium-server-standalone-3.0.1.jar -port 4444

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)
library(rvest)

remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port = 4444,
                  browserName = "chrome",
                  newSession = TRUE)


Url <- "https://www.otomoto.pl/osobowe/skoda/fabia/?page=1"
remDr %>% go(Url)
page <- read_html(Url)
last_page <- page %>% xml_find_all('/html/body/div[4]/div[2]/section/div[2]/div[2]/ul/li[6]/a/span')%>%html_text()
last_page <- as.numeric(last_page)

wektorLinkow<-c()
for(i in 1:last_page){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/skoda/fabia/?page=",i)
  remDr%>%go(newUrl)
  elems<- remDr %>% findElements("class name", "offer-title")
  for( j in 1: length(elems)){
    e<-findElementsFromElement(elems[[j]],using="tag name", "a")
    if( length(e)>0){
      link<- e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}

wektorLinkowU<- wektorLinkow%>%unique()
wektorLinkowU


zrobWiersz <- function(w, wektorLinkowU, remDr){
  remDr %>% go(wektorLinkowU[w])
 
  szczegoly <- remDr %>% findElements("class name", "offer-params__item")
  listaSzczegolowOpis <- c()
  listaSzczegolowWartosci <- c()
  for(i in 1:length(szczegoly)){
    listaSzczegolowOpis <- c(listaSzczegolowOpis, szczegoly[[i]] %>% findElementsFromElement("class name", "offer-params__label"))
    listaSzczegolowWartosci <- c(listaSzczegolowWartosci, szczegoly[[i]] %>% findElementsFromElement("class name", "offer-params__value"))
  }
  nazwyKolumn <- lapply(listaSzczegolowOpis, getElementText)%>%unlist()
  wartosci <- lapply(listaSzczegolowWartosci, getElementText)%>%unlist()
  df1 <- data.frame(matrix(wartosci, nrow = 1, ncol = length(wartosci)))
  names(df1) <- nazwyKolumn
  df1
}

auta <- NULL
for(w in 1:length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(auta)){
    auta<-df1
  }else{
    auta<-smartbind(auta,df1)
  }
}

auta$Przebieg <- gsub(" km", "", auta$Przebieg)
auta$Przebieg <- as.numeric(gsub(" ", "", auta$Przebieg))

auta$`Pojemność skokowa`<- gsub(" cm3", "", auta$`Pojemność skokowa`)
auta$`Pojemność skokowa` <- as.numeric(gsub(" ", "", auta$`Pojemność skokowa`))

auta$Moc<- gsub(" KM", "", auta$Moc)
auta$Moc <- as.numeric(gsub(" ", "", auta$Moc))

auta$`Emisja CO2`<- gsub(" g/km", "", auta$`Emisja CO2`)
auta$`Emisja CO2` <- as.numeric(gsub(" ", "", auta$`Emisja CO2`))

auta