install.packages('rjson')
library(rjson) 
library(jsonlite)
#install.packages('genTS')
library(genTS)

# Analise de pseudopalavras com <s> ou <ss>

getwd()
setwd('C:/Users/55229/Desktop/scripts')

list<-jsonlite::fromJSON('pseudo_gold_jonatas_and_v1_complete-15-04_processed.json') # converter p objeto R

setwd('C:/Users/55229/Desktop/scripts/15-05')

length(list)
class(list)
n<-length(list) # tamanho da lista de palavras pronunciadas 
df<-data.frame() # dataframe para armazenar nossas medidas de interesse
j <- 0 # contador (para transitar por df)

pipe_Word<-character() # para armazenar palavra fonética do modelo e salva-la em df

for (i in 1:n){
  
  cod<-list[i] # pega item com codigo identificador do audio/palavra
  word<- list[[i]] # pega item sem codigo identificador
  tabelas<-word$prob_phoneme$correction # pega tabelas de prob para cada fonema (caractere) pronunciado
  len<-length(tabelas) # qtd de fonemas (caracteres) atribuidos a palavra pelo pipeline
  phoneme_word<-word$prob_phoneme$word # pega palavra fonetica teorica
  
  if (word$correction_gold == 'ERRADO' & word$correction_phoneme == 'CERTO'){
    
    if (grepl('s',phoneme_word) == TRUE){ # "se o fonema z esta na transcricao teorica..."
      
      j<-j+1 # contador
      
      df[j,1]<-names(cod) # salva codigo identificador do audiio
      df[j,2]<-word$word # salva palavra original  no dataframe (1ª COLUNA)
      df[j,3]<-phoneme_word # salva palavra fonetica no dataframe (2ª COLUNA)
      
      pipe_word<-character() # cria vetor pra pegar fonemas atribuidos pelo pipeline
      
      if (len>0){ # testa se o pipeline atribuiu algum fonema ao audio
        
        prob<-numeric() # vetor para as probs associadas a cada fonema atribuido 
        # "o quao certo o modelo esta em sua resposta?"
        
        for (ind in 1:len){
          pipe_word[ind]<-list[[i]]$prob_phoneme$correction[[ind]][1,1]
          #salva fonemas de maior prob para cada posicao
          
          prob[ind]<-list[[i]]$prob_phoneme$correction[[ind]][1,2]
          # salva probs associadas a cada fonema anterior
        }
        
        
        pipe_Word[length(pipe_Word)+1] <- paste(pipe_word, collapse = '')
        # armazena palavra fonetica (modelo) numa string de palavras foneticas
        
        df[j,4]<- pipe_Word[length(pipe_Word)]
        # salva palavra fonetica atribuida pelo pipeline ao dataframe (3ª COLUNA)
        
        
        prob<-as.numeric(prob)
        
        df[j,5]<-round(mean(prob),2) # salva media das probs associadas a cada fonema da palavra ao df
        df[j,6]<-round(sd(prob),2) # salva desvio padrao das probabilidades
        df[j,7]<-pipe_word[which.min(prob)] # salva fonema com probabilidade minima (atribuido pelo modelo com "menos certeza")
        df[j,8]<-round(min(prob),2) # salva prob minima (associada ao fonema "menos certo")
        
        
      }
    }
    
    
  }
  print(i)
}


# Atribui nomes as colunas do dataframe  ---------------------------------------

colnames(df)[1] = 'audio_ID'
colnames(df)[2] = 'word'
colnames(df)[3] = 'phoneme'
colnames(df)[4] = 'pipe_phoneme'
colnames(df)[5] = 'probabilities mean' # errado segundo nosso pipeline, certo
# segundo o avaliador humano
colnames(df)[6] = 'probabilities sd'
colnames(df)[7] = 'phone_minimum'
colnames(df)[8] = 'minimum probability'

# ------------------------------------------------------------------------------

vetor<-character()
k = 0

for (i in 1:length(list)){ # contabiliza total de cada palavra de interesse no audio
  
  if (grepl('s',list[[i]]$word) == TRUE) {
    vetor[k]<-list[[i]]$word 
    
    k = k + 1
    
  }
  print(i)
  
}


# agrega totais ao dataframe (abaixo)
apendice<-as.data.frame(table(vetor))
colnames(apendice)[1] = 'word'
colnames(apendice)[2] = 'total in sample'

#install.packages('dplyr')
library(dplyr)

df_new<-left_join(df,apendice, by = 'word')

df_new <- df_new %>%
  group_by(`pipe_phoneme`, `phone_minimum`, `total in sample`) %>%
  mutate(`probabilities sd` = round(sd(`probabilities mean`),2),
         `probabilities mean` = round(mean(`probabilities mean`),2),
         `mp mean` = round(mean(`minimum probability`),2),
         `mp sd` = round(sd(`minimum probability`),2),
         `disagreements` = sum(n()),
         audio_ID = paste(audio_ID, collapse = ", ")) # counts rows being grouped

df_new<- df_new[,-8]

# Reordenando as colunas
df_new <- df_new[, c("audio_ID", "word", "phoneme", "pipe_phoneme", "probabilities mean",
                     "probabilities sd", "phone_minimum", "mp mean", "mp sd",
                     "disagreements", "total in sample")]

df_new<-unique(df_new)

df_new$`disagreements/total`<-round(df_new$`disagreements`/df_new$`total in sample`,2)

# Se valor de 'total in sample' eh NA, significa que a palavra nao eh uma
# das listadas (esta fora do padrao considerado). 
# Podemos criar uma regra para eliminar
# essas palavras;

r<-df_new %>%  filter(is.na(`total in sample`))

# Excluir linhas com NA na coluna 'disagreements/total'
df_new <- df_new[complete.cases(df_new$`total in sample`), ]

# as.data.frame (just to make sure)
df_new <- as.data.frame(df_new)


library(writexl)
write_xlsx(df_new, 's_h_certo2.xlsx')



