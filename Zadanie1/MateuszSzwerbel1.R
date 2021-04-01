#Mateusz Szwerbel
#pd3348

#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

podzielna <- function(x,y){
  if (x%%y == 0){
    print(paste0("Liczba ",x, " jest podzielna przez ", y))} 
  else{
    print(paste0("Liczba ",x, " nie jest podzielna przez ", y))}
}
podzielna(20,4)
podzielna(20,3)


#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

predkosc <- function(v1, v2){
  t1 <- 1/(v1*2)
  t2 <- 1/(v2*2)
  V <- 1/(t1 + t2)
  print(paste0("Średnia prędkość pociągu to ", V, " km/h"))
}

predkosc(120, 90)
predkosc(90, 90)

#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

person <- function(x,y){
  if (length(x) == length(y)){
    wsp <- cor(x,y, method = 'pearson')
    print(paste0("Współcznnik korelacje Pearsona: ", wsp))
  } else{
    print("Wektory muszą być tej samej długości")
  }
}

person(c(1:10), c(11:20))
df <- read.csv2('dane.csv')
person(df$waga, df$wzrost)
#0.98 - duża liniowa zaleźność miedzy wagą i wzrostem

#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych 
#(tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

stworzDataFrame <- function(ile=1){
  VALUE <- c()
  if (ile < 1){
    pritn("Liczba wierszy musi być > 0")
  }
  else{
    name <- strsplit(readline(prompt = "Nazwy kolumn: "), " ")[[1]]
    i = 0
    while (i < ile){
      value <- as.integer(strsplit(readline(prompt = " "), " ")[[1]])
      VALUE <- rbind(VALUE, value)
      i = i + 1
    }
    X <- data.frame(VALUE, row.names = NULL)
    colnames(X) <- name
    X
  }
}

sdf <- stworzDataFrame(2)

#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
#UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.


liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  files <- list.files(sciezka)
  if (length(files) < DlaIluPlikow){
    print("W wybranym folderze nie ma tylu plików")
  }else{
    if (!is.na(as.integer(substring(nazwaKolumny, 1, 1)))){
      nazwaKolumny <- paste("X", nazwaKolumny, sep = "")
    }
    
    f <- c()
    for(i in 1:DlaIluPlikow) {
      fl <- read.csv(paste(sciezka,files[i],sep="/"), na.strings=c("","NA"))
      f <- rbind(f, fl)
    }
  df <- f[[nazwaKolumny]]
  df <- na.omit(df)  
  do.call(jakaFunkcja, list(df))
  }
  
}

lp <- liczZplikow("C:/Studia_podyplomowe/Analiza_danych_R/cwiczenia/R_zajecia1/smogKrakow", "140_pm1", "sum", 12)
lp
