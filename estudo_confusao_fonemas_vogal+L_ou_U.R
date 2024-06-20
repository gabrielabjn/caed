#install.packages('rjson')
library(rjson) 
library(jsonlite)
#install.packages('genTS')
library(genTS)

# Analise de palavras com terminacao em vogal + L ou U

getwd()
setwd('C:/Users/55229/Desktop/scripts')

list<-jsonlite::fromJSON('pseudo_gold_jonatas_and_v1_complete-15-04_processed.json') # converter p objeto R

length(list)
class(list)
# x<-list[[11]]$prob_phoneme$correction[length(list[[11]])]
# x<-as.data.frame(x)
# x[1,1]
# x

n<-length(list) # tamanho da lista de palavras pronunciadas 
df<-data.frame() # dataframe para armazenar nossas medidas de interesse
j <- 0 # contador (para transitar por df)


pipe_Word<-character() # para armazenar palavra fonética do modelo e salva-la em df

for (i in 1:n){
  
  word<-list[[i]] # pega palavra pronunciada
  tabelas<-word$prob_phoneme$correction # pega tabelas de prob para cada fonema (caractere) pronunciado
  len<-length(tabelas) # qtd de fonemas (caracteres) atribuidos a palavra pelo pipeline
  phoneme_word<-word$prob_phoneme$word # pega palavra fonetica teorica
  
  if (word$correction_gold == 'CERTO' & word$correction_phoneme == 'ERRADO'){
    
    # se o humano corrigiu como "certo" e o modelo como "errado", pega a palavra dita
    
    ufa<-word$prob_phoneme$correction[len] # ultimo fonema atribuido ao som pronunciado
    ufa<-as.data.frame(ufa) 
    ufa<-ufa[1,1]
    
    pfa<-word$prob_phoneme$correction[len-1] # penultimo fonema atribuido
    pfa<-as.data.frame(pfa)
    pfa<-pfa[1,1]
    
    
    if (ufa == 'w'){ # se termina com o fonema de interesse (w), realiza as operacoes 
      
      # o interesse eh por palavra que terminem com vogal + L ou U (w phoneme)
      
    j<-j+1 # contador
    
    df[j,1]<-word # salva palavra original  no dataframe (1ª COLUNA)
    df[j,2]<-phoneme_word # salva palavra fonetica no dataframe (2ª COLUNA)
    
    pipe_word<-character() # cria vetor pra pegar fonemas atribuidos pelo pipeline
    
    if (len>0){ # testa se o pipeline atribuiu algum fonema ao audio
  
    prob<-numeric() # vetor para as probs associadas a cada fonema atribuido 
    # "o quao certo o modelo esta em sua resposta?"
    
    for (ind in 1:len){
      pipe_word[ind]<-list[[i]]$prob_phoneme$correction[[ind]][1,1]
      #salva fonemas atribuidos pelo pipeline
      
      prob[ind]<-list[[i]]$prob_phoneme$correction[[ind]][1,2]
      # salva probs associadas a cada fonema
    }

    
    if (is_empty(pipe_Word) == TRUE){
      # verifica se foi atribuido algum fonema pelo modelo. se nao,
    
    pipe_Word[1] <- paste(pipe_word, collapse = '')
    # agrega fonemas atribuidos pelo pipeline em palavra unica (vai ficar vazia)
    
    df[j,3]<- pipe_Word[1]
    # salva palavra fonetica atribuida pelo pipeline ao dataframe (3ª COLUNA)
    
    }
    
    else{
      
      pipe_Word[length(pipe_Word)+1] <- paste(pipe_word, collapse = '')
      # armazena palavra fonetica (modelo) numa string de palavras foneticas
      
      df[j,3]<- pipe_Word[length(pipe_Word)]
      # salva palavra fonetica atribuida pelo pipeline ao dataframe (3ª COLUNA)
    }
    
    prob<-as.numeric(prob)
    
    df[j,4]<-round(mean(prob),2) # salva media das probs associadas a cada fonema da palavra ao df
    df[j,5]<-round(sd(prob),2) # salva desvio padrao das probabilidades
    df[j,6]<-pipe_word[which.min(prob)] # salva fonema com probabilidade minima (atribuido pelo modelo com "menos certeza")
    df[j,7]<-round(min(prob),2) # salva prob minima (associada ao fonema "menos certo")
    
    
    }
    }
    
  }
  print(i) # para verificacao da rodagem da iteracao
}

# Atribui nomes as colunas do dataframe  ---------------------------------------

colnames(df)[2] = 'phoneme'
colnames(df)[3] = 'pipe_phoneme'
colnames(df)[4] = 'probabilities mean' # errado segundo nosso pipeline, certo
# segundo o avaliador humano
colnames(df)[5] = 'probabilities sd'
colnames(df)[6] = 'phone_minimum'
colnames(df)[7] = 'minimum probability'
# ------------------------------------------------------------------------------


vetor<-character()
k = 0

for (i in 1:length(list)){ # contabiliza total de cada palavra de interesse no audio

if (list[[i]]$word %in% c('rival','oral','refil','seol','berreu','pel','jedal',
                          'piliu','tral','mital','meril','guil','siu', 'ropal',
                          'irril', 'gapilal', 'mal', 'funil','gel','papel','mel',
                          'cugol','vuvil','mel','rival','tou','entel',
                          'vissal','irril','rolta','falso','calça','tul','salto',
                          'fou', 'joula','xiu', 'nol','sapil','pou')){
 vetor[k]<-list[[i]]$word 
 
 k = k + 1

}
  print(i)

}

# agrega totais ao dataframe (abaixo)
apendice<-as.data.frame(table(vetor))
colnames(apendice)[1] = 'word'
colnames(apendice)[2] = 'total in sample'

df_new<-left_join(df,apendice, by = 'word')

df_new <- df_new %>%
  group_by(`pipe_phoneme`, `phone_minimum`, `total in sample`) %>%
  mutate(`probabilities sd` = round(sd(`probabilities mean`),2),
         `probabilities mean` = round(mean(`probabilities mean`),2),
         `mp mean` = round(mean(`minimum probability`),2),
         `mp sd` = round(sd(`minimum probability`),2),
         `disagreements` = sum(n())) # counts rows being grouped

df_new<- df_new[,-7]

# Reordenando as colunas
df_new <- df_new[, c("word", "phoneme", "pipe_phoneme", "probabilities mean",
                 "probabilities sd", "phone_minimum", "mp mean", "mp sd",
                 "disagreements", "total in sample")]

df_new<-unique(df_new)

df_new$`disagreements/total`<-round(df_new$`disagreements`/df_new$`total in sample`,2)

# se valor da ultima coluna (razao) eh NA, significa que a palavra nao termina
# com L (esta fora do padrao considerado). Podemos criar uma regra para eliminar
# essas palavras;


# Excluir linhas com NA na coluna 'disagreements/total'
df_new <- df_new[complete.cases(df_new$`disagreements/total`), ]

# as.data.frame (just to make sure)
df_new <- as.data.frame(df_new)


# library(writexl)
# write_xlsx(df_new, 'L_confusao.xlsx')


hist(df_new$`probabilities mean`, col = 'lightblue')




