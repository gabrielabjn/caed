#install.packages('rjson')
library(rjson) 
library(jsonlite)
#install.packages('genTS')
library(genTS)

# Analise de pseudopalavras com <s> ou <ss>

getwd()
setwd('C:/Users/55229/Desktop/scripts')

list<-jsonlite::fromJSON('pseudo_gold_jonatas_and_v1_complete-15-04_processed.json') # converter p objeto R

setwd('C:/Users/55229/Desktop/scripts/p&b')

n<-length(list)

cont_b<-0 # qtd total de palavras com <b>
certas_b<-0 # qtd de palavras com <b> pronunciado corretamente
confusoes_b<-0 # qtd de palavras q [b] foi substituido por [p] 
b_repetido<-0 # palavras com mais de um <b>

cont_p<-0 # qtd total de palavras com <b>
certas_p<-0 # qtd de palavras com <b> pronunciado corretamente
confusoes_p<-0 # qtd de palavras q [b] foi substituido por [p] 
p_repetido<-0 # palavras com mais de um <b>

for (i in 1:n){
  
  cod<-list[i] # pega item com codigo identificador do audio/palavra
  word<- list[[i]] # pega item sem codigo identificador
  tabelas<-word$prob_phoneme$correction # pega tabelas de prob para cada fonema (caractere) pronunciado
  len<-length(tabelas) # qtd de fonemas (caracteres) atribuidos a palavra pelo pipeline
  phoneme_word<-word$prob_phoneme$word # pega palavra fonetica teorica
  l<-length(word$prob_phoneme$correction) # qtd de fonemas atribuidos a pronuncia
  
  spoken_word<-character()
  for (x in 1:l) spoken_word[x]<-word$prob_phoneme$correction[[x]][1,1]
  spoken_word <- paste(spoken_word, collapse = "")

  pos_ref_b<-gregexpr('b', phoneme_word)[[1]][1] # retorna a posicao de b (se houver) na palavra de referencia
  pos_ref_p<-gregexpr('p', phoneme_word)[[1]][1]
  pos_w_b<-gregexpr('b', spoken_word)[[1]][1] # retorna a posicao de b na palavra transcrita
  pos_w_p<-gregexpr('p', spoken_word)[[1]][1] # retorna a posicao de p na palavra transcrita
  
  if (length(pos_ref_b) > 1) b_repetido<-b_repetido+1 # contabiliza qtd de palavras com b repetido
  if (length(pos_ref_p) > 1) p_repetido<-p_repetido+1 # contabiliza qtd de palavras com p repetido
  
  if (pos_ref_b != -1 & length(pos_ref_b) == 1){ # se a palavras de referencia contem apenas um <b>
    
    cont_b<- cont_b+1 
    
    if (pos_ref_b %in% pos_w_b == TRUE) certas_b<-certas_b+1
    
    else if (pos_ref_b %in% pos_w_p == TRUE) confusoes_b<-confusoes_b+1
        # tem p onde deveria ter b
  } 
  
  if (pos_ref_p != -1 & length(pos_ref_p) == 1){ # se a palavras de referencia contem apenas um <b>
    
    cont_p<- cont_p+1 
    
    if (pos_ref_p %in% pos_w_p == TRUE) certas_p<-certas_p+1
    
    else if (pos_ref_p %in% pos_w_b == TRUE) confusoes_p<-confusoes_p+1
         # tem b onde deveria ter p
  } 
    
 
  print(i)
}

library(ggplot2)
library(tidyr)

# Defina os valores das diagonais
diagonal_values <- c(certas_b,certas_p)  # Substitua esses valores pelos seus dados reais

# Defina os valores fora da diagonal (se necessário)
off_diagonal_values <- c(confusoes_p,confusoes_b)  # Substitua esses valores pelos seus dados reais

# Preencha a matriz de confusao
confusion_matrix <- diag(diagonal_values)
confusion_matrix[2,1]<- confusoes_b
confusion_matrix[1,2]<- confusoes_p


matriz_confusao_valores <- matrix(c(confusoes_b,certas_p, certas_b, confusoes_p), nrow = 2)

ggplot() +
  geom_tile(data = as.data.frame(as.table(matriz_confusao_valores)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(data = as.data.frame(as.table(matriz_confusao_valores)), aes(x = Var1, y = Var2, label = Freq), vjust = 1, size = 5) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  scale_x_discrete(name = "Heuristic", labels = c("b", "p")) +
  scale_y_discrete(name = "Human", labels = c("p", "b")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
