
#install.packages('rjson')
library(rjson) 
library(jsonlite)

# Tente ler o arquivo com diferentes codificações

getwd()
setwd('C:/Users/55229/Desktop/scripts')

# amostra<-jsonlite::fromJSON('C:/Users/55229/Desktop/caed/arquivos_json/pseudo_gold_jonatas_and_v1_complete-15-04_processed.json') # converter p objeto R
# saveRDS(amostra, file = 'dados_processed_30-04.rds')

amostra<-readRDS(file = 'dados_processed_30-04.rds')

qtd_dis <- 0 # qtd de discordancias
caso_1<-0
caso_2<-0
ind<-1
guarda_i<-numeric()

for (i in 1: length(amostra)){
  
  chave<-amostra[[i]]
  
  if (chave$correction_gold != chave$correction_phoneme){
     qtd_dis<- qtd_dis+1
  
     if(chave$correction_gold == 'CERTO' & chave$correction_phoneme == 'ERRADO')
       caso_1<-1+caso_1
       
     else if(chave$correction_gold == 'ERRADO' & chave$correction_phoneme == 'CERTO')
       caso_2<-1+caso_2
     
     else{
       
       guarda_i[ind]<-i
       ind<-ind+1
       
     }
     
  }
    
  print(i)
  
  
}

caso_1+caso_2+length(guarda_i) == qtd_dis # deve ser TRUE

nao_lidas<-0

for (i in 1: length(amostra)){
  
  chave<-amostra[[i]]
  
  if (chave$correction_gold == 'NAO_LIDO')
    
    nao_lidas<-1+nao_lidas

}

nao_lidas == length(guarda_i) # deve bater

proporcao_discordancias<-qtd_dis/length(amostra)
proporcao_discordancias

# ------------------------------------------------------------------------------
letras <- c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'S', 'Z', 'R',
            'tS', 'dZ', 'm', 'n', 'J', 'r', 'w', 'j', 'w_til','j_til', 'L', 'l',
            'a', 'e', 'E', 'i', 'o', 'O', 'u', 'a_til' ,'e_til', 'i_til' ,'o_til' ,'u_til',
            '<pad>')

letras_exibicao<-c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ', 
                   'tʃ', 'dʒ', 'm', 'n', 'ɲ', 'ɾ', 'w', 'j', 'w~','j~', 'ʎ', 'l',
                   'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'a~' ,'e~', 'i~' ,'o~' ,'u~',
                   '<pad>')

medias_probs<-as.numeric(lapply(lista, function(x)mean(x)))
# media das probs por fonema

barplot(medias_probs, names.arg = letras_exibicao, ylim = c(0,1))

amostra[[1]]

letras

sum(medias_probs<1)
indices<-which(medias_probs0.8)
desvios<-numeric()

for (i in seq_along(indices)){
  
  desvios[i]<-sd(lista[[indices[i]]])
  
}


# GRAFICOS 4 FONEMAS COM PROB MEDIA ABAIXO DE 0.8 ------------------------------

par(mfrow=c(2,2))

hist(as.numeric(lista[[18]]), main = "ɲ (n = 25) ", cex.main = 2, xlab = 'probs de ɲ',
     col = 'lightblue', seq(0, 1, by = 0.1))
abline(v = 0.43, lwd = 2, col = 'red')
text(x = 0.45, y = 6, labels = 'média')

hist(as.numeric(lista[[23]]), main = "j~ (n = 34)", cex.main = 2, xlab = 'probs de j~',
     col = 'lightblue', seq(0, 1, by = 0.1))
abline(v = 0.59, lwd = 2, col = 'red')
text(x = 0.61, y = 7.5, labels = 'média')

hist(as.numeric(lista[[24]]), main = "ʎ (n = 106)", cex.main = 2, xlab = 'probs de ʎ',
     col = 'lightblue', seq(0, 1, by = 0.1))
abline(v = 0.65, lwd = 2, col = 'red')
text(x = 0.62, y = 50, labels = 'média')

hist(as.numeric(lista[[37]]), main = "u~ (n = 415)", cex.main = 2, xlab = 'probs de u~',
     col = 'lightblue', seq(0, 1, by = 0.1))
abline(v = 0.76, lwd = 2, col = 'red')
text(x = 0.73, y = 83, labels = 'média')

# ------------------------------------------------------------------------------

# GRAFICOS 4 FONEMAS COM MAIORES PROBS MEDIAS ----------------------------------

mean(lista[[5]])
length(lista[[5]])
letras_exibicao[5]

par(mfrow=c(2,2))

hist(as.numeric(lista[[5]]), main = "a (n = 27.848) ", cex.main = 2, xlab = 'probs de a',
     col = 'lightblue3', breaks = seq(0, 1, by = 0.1))
abline(v = 0.99, lwd = 2, col = 'red')
text(x = 1, y = 5000, labels = 'média')

hist(as.numeric(lista[[16]]), main = "m (n = 4.826)", cex.main = 2, xlab = 'probs de m',
     col = 'lightblue3', breaks = seq(0, 1, by = 0.1))
abline(v = 0.98, lwd = 2, col = 'red')
text(x = 1, y = 4000, labels = 'média')

hist(as.numeric(lista[[19]]), main = "ɾ (n = 4.553)", cex.main = 2, xlab = 'probs de ɾ',
     col = 'lightblue3', breaks = seq(0, 1, by = 0.1))
abline(v = 0.98, lwd = 2, col = 'red')
text(x = 1, y = 3500, labels = 'média')

hist(as.numeric(lista[[5]]), main = "k (n = 5.775)", cex.main = 2, xlab = 'probs de k',
     col = 'lightblue3', breaks = seq(0, 1, by = 0.1))
abline(v = 0.98, lwd = 2, col = 'red')
text(x = 1, y = 4000, labels = 'média')

# ------------------------------------------------------------------------------

order(medias_probs) # 19, 16, 26 indices dos fonemas com probs media mais altas

letras[c(5,19,16,26)]

par(mfrow=c(2,2))
hist(as.numeric(lista[[19]]), main = "Frequência dos Scores para k", xlab = 'scores de k',
     col = 'lightblue')
hist(as.numeric(lista[[16]]), main = "Frequência dos Scores para r", xlab = 'scores de r',
     col = 'lightblue')
hist(as.numeric(lista[[26]]), main = "Frequência dos Scores para m", xlab = 'scores de m',
     col = 'lightblue')
hist(as.numeric(lista[[5]]), main = "Frequência dos Scores para a", xlab = 'scores de a',
     col = 'lightblue')

