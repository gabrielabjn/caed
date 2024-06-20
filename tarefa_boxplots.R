#install.packages('rjson')
library(rjson) 
library(jsonlite)

# Tente ler o arquivo com diferentes codificações

getwd()
setwd('C:/Users/55229/Desktop/scripts')

#amostra<-readRDS('minha_lista.rds')

# amostra<-jsonlite::fromJSON('results_all_gabriela.json') # converter p objeto R
#
# length(amostra)
# class(amostra)
# amostra[1]

# Uma lista composta por vetores nomeados com as letras e tamanho 36760 preenchidos com zeros
letras <- c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'S', 'Z', 'R',
            'tS', 'dZ', 'm', 'n', 'J', 'r', 'w', 'j', 'w_til','j_til', 'L', 'l',
            'a', 'e', 'E', 'i', 'o', 'O', 'u', 'a_til' ,'e_til', 'i_til' ,'o_til' ,'u_til',
            '<pad>')

lista <- lapply(letras, function(letra) {
  vetor <- numeric()
})

vetor_prob_media<-numeric()

# ------------------------
# testes

# length(amostra)
# amostra[[1]]$prob_phoneme$word
# amostra[[1]]$word

# ------------------------

# pb <- txtProgressBar(min = 0, max = 100, style = 3)
# pb2 <- txtProgressBar(min = 0, max = 100, style = 3)
# 
# for (i in 1:36760){
#   
#   palavra<- amostra[[i]]$word
#   fones<-amostra[[i]]$prob_phoneme$word # pega palavra fonetica
#   num_fones<-length(amostra[[i]]$prob_phoneme$correction)
#   
#   setTxtProgressBar(pb2, i)
#   
#   if (num_fones > 0){
#     for (j in 1: num_fones){
#       tabela <- amostra[[i]]$prob_phoneme$correction[[j]] # pega tabela de probs associada a cada letra da palavra
#       # tabela <- tabela[1,] # pega fone e prob da primeira posicao de cada tabela
#       posicao <- which(letras == tabela[1,1]) # faz correspondencia da letra a posicao que ela ocupa no vetor'letras'
#       cont<-1
#       while(is.na(lista[[posicao]][cont]) == FALSE ){
#         cont<-cont+1
#       }
#       
#       lista[[posicao]][cont]<-tabela[1,2]
#       
#     }
#   }
#   
#   print(i)
#   setTxtProgressBar(pb, i)  # Atualiza a barra de progresso para o valor atual de i
# }
# 
# 
# close(pb)
# close(pb2)
# 
# which.min(vetor_prob_media)
# letras[18]
# vetor_prob_media[18]
# length(vetor_prob_media)
# length(vetor_prob_media)
# which.max(vetor_prob_media)
# letras[26]
# vetor_prob_media[26]

lista<-readRDS(lista, file = 'minha_lista.rds')

lista<-lapply(lista, function(x) as.numeric(x))
vetor_prob_media<-sapply(lista, function(x) mean(x))

qtd_fonemas<-numeric()

for(i in seq_along(lista)){
  
  qtd_fonemas[i]<-length(lista[[i]])
  
  
}

par('mar')

par(mar = c(6,5,6,3))
letras_exibicao[26]
letras_exibicao<-c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ', 
                              'tʃ', 'dʒ', 'm', 'n', 'ɲ', 'ɾ', 'w', 'j', 'w~','j~', 'ʎ', 'l',
                      'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'a~' ,'e~', 'i~' ,'o~' ,'u~',
                      '<pad>')
boxplot(
  lista,
  # main = "Distribui??o dos fonemas em 5 avalia??es (n = 36.760)",  
  xlab = "Fonemas",  
  ylab = "Scores",
  col = 'lightblue',
  names =  letras_exibicao
  
  
  
)

# Add text labels for amounts
text(x = 1:38, y = par("usr")[4] + 0.09, labels = qtd_fonemas, xpd = TRUE, srt = 45, adj = 1)

title("Distribuição dos fonemas em 5 avaliações (36.760 palavras, 179.722 fonemas) ", 
      line = 4, cex = 2.5)

# -------------------------------------------------------------------------------

sum(qtd_fonemas)

novos_indices<-order(qtd_fonemas, decreasing = TRUE)
qtd_fonemas_decresc<-qtd_fonemas[novos_indices]
nova_legenda<-letras_exibicao[novos_indices]

barplot(qtd_fonemas_decresc,main = 'Frequências dos Fonemas em 36.760 Pseudopalavras',
            col = 'lightblue', names.arg = nova_legenda)


