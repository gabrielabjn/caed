#install.packages('rjson')
library(rjson) 
library(jsonlite)

# Tente ler o arquivo com diferentes codificacoes
encodings <- c("UTF-8", "latin1")

getwd()
setwd('C:/Users/55229/Desktop/scripts')

amostra<-jsonlite::fromJSON('results_all_gabriela.json') # converter p objeto R

# Uma lista composta por vetores nomeados com as letras e tamanho 36760 preenchidos com zeros
letras <- c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'S', 'Z', 'R',
            'tS', 'dZ', 'm', 'n', 'J', 'r', 'w', 'j', 'w_til','j_til', 'L', 'l',
            'a', 'e', 'E', 'i', 'o', 'O', 'u', 'a_til' ,'e_til', 'i_til' ,'o_til' ,'u_til',
            '<pad>')


list_certas <- lapply(letras, function(letra) {
  vetor <- numeric()
})


list_erradas <- lapply(letras, function(letra) {
  vetor <- numeric()
})

n<-length(amostra)

for (i in 1:n){
  
  if (amostra[[i]]$correction_gold == 'CERTO'){ # fechar
  
  palavra_c<- amostra[[i]]$word
  fones_c<-amostra[[i]]$prob_phoneme$word # pega palavra fonetica
  num_fones_c<-length(amostra[[i]]$prob_phoneme$correction)

  if (num_fones_c > 0){
    for (j in 1: num_fones_c){
        tabela_c <- amostra[[i]]$prob_phoneme$correction[[j]] # pega tabela de probs associada a cada letra da palavra
        #tabela_c <- tabela_c[1,] # pega fone e prob da primeira posicao de cada tabela
        posicao_c <- which(letras == tabela_c[1,1]) # faz correspondencia da letra a posicao que ela ocupa no vetor'letras'
        cont<-1
        while(is.na(list_certas[[posicao_c]][cont]) == FALSE ){
          cont<-cont+1
        }

        list_certas[[posicao_c]][cont]<-tabela_c[1,2]

    }
  }

  }
  if (amostra[[i]]$correction_gold == 'ERRADO'){
    
      palavra_e<- amostra[[i]]$word
      fones_e<-amostra[[i]]$prob_phoneme$word # pega palavra fonetica
      num_fones_e<-length(amostra[[i]]$prob_phoneme$correction)
      
      if (num_fones_e > 0){
        for (j in 1: num_fones_e){
          tabela_e <- amostra[[i]]$prob_phoneme$correction[[j]] # pega tabela de probs associada a cada letra da palavra
          # tabela <- tabela[1,] # pega fone e prob da primeira posicao de cada tabela
          posicao_e <- which(letras == tabela_e[1,1]) # faz correspondencia da letra a posicao que ela ocupa no vetor'letras'
          cont<-1
          while(is.na(list_erradas[[posicao_e]][cont]) == FALSE ){
            cont<-cont+1
          }
          
          list_erradas[[posicao_e]][cont]<-tabela_e[1,2]
          
        }
      }
 
  }
  

  print(i)

}

list_certas<-lapply(list_certas, function(x) as.numeric(x))
list_erradas<-lapply(list_erradas, function(x) as.numeric(x))


qtd_fonemas_certos<-numeric()
for(i in seq_along(list_certas)){
  
  qtd_fonemas_certos[i]<-length(list_certas[[i]])
  
  
}

sum(qtd_fonemas_certos)

qtd_fonemas_errados<-numeric()
for(i in seq_along(list_erradas)){
  
  qtd_fonemas_errados[i]<-length(list_erradas[[i]])
  
  
}

sum(qtd_fonemas_errados) 

sum(qtd_fonemas_certos) + sum(qtd_fonemas_errados) 

boxplot(
  list_erradas,
  main = "Distribuição dos fonemas em 5 avaliações (n = 36.760)",
  xlab = "Fonemas",
  ylab = "Scores",
  col = 'coral1',
  names = c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ',
                    'ι', 'β', 'm', 'n', 'ɲ', 'ɾ', 'w', 'j', 'μ','ζ', 'ʎ', 'l',
                    'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'α' ,'γ', 'δ' ,'θ¸' ,'κ','<pad>')

)




