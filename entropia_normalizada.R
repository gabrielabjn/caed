#install.packages('rjson')
library(rjson) 
library(jsonlite)
#install.packages('genTS')
library(genTS)

getwd()
setwd('C:/Users/55229/Desktop/scripts')
amostra<-readRDS('minha_lista.rds')
n<-length(amostra)

fones<- c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ',
          'tʃ', 'dʒ', 'm', 'n', 'ɲ', 'r', 'w', 'j', 'w~','j~', 'ʎ', 'l',
          'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'a~' ,'e~', 'i~' ,'o~' ,'u~',
          '<pad>')


# Entropia Normalizada ---------------------------------------------------------

#install.packages('entropy')
library(entropy)

# Function to calculate normalized entropy
normalized_entropy <- function(x) {
  ent <- Entropy(x)
  max_ent <- length(unique(x))
  ent / max_ent
}

entropias = lapply(amostra, function(x) normalized_entropy(x))
entropias = round(as.numeric(entropias),4)

df2 <- data.frame(entropia = entropias)

# Nomeie as linhas como 'entropia'
rownames(df) <- fones

library(openxlsx)
#write.xlsx(df,'entropias_v1.xlsx',rowNames=TRUE)




