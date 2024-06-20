#install.packages('rjson')
library(rjson) 
library(jsonlite)

getwd()
setwd('C:/Users/55229/Desktop/scripts')

list<-jsonlite::fromJSON('pseudo_alinhamento_jonatas_prob_v1_processed.json') # converter p objeto R

length(list)
class(list)
# x<-list[[11]]$prob_phoneme$correction[length(list[[11]])]
# x<-as.data.frame(x)
# x[1,1]
# x

n<-length(list)
df<-data.frame()
j <- 0

for (i in 1:n){
  
  word<-list[[i]]
  tabelas<-word$prob_phoneme$correction
  len<-length(tabelas)
  phoneme_word<-word$prob_phoneme$word
  
  if (word$correction_gold == 'CERTO' & word$correction_phoneme == 'ERRADO'){
    
    ufa<-word$prob_phoneme$correction[len] # ultimo fonema atribuido
    ufa<-as.data.frame(ufa)
    ufa<-ufa[1,1]
    
    pfa<-word$prob_phoneme$correction[len-1] # ultimo fonema atribuido
    pfa<-as.data.frame(pfa)
    pfa<-pfa[1,1]
    
    #& pfa %in% c('a','e','i','o','u')
    
    if (ufa == 'w'){
      
    j<-j+1
    
    df[j,1]<-word
    df[j,2]<-phoneme_word
      
    }
    
  }
  print(i)
}


library(dplyr)

result <- df %>%
  group_by_all() %>%
  summarize(Frequency = n())

result = result[-nrow(result),]

colnames(result)[2] = 'phoneme'
colnames(result)[3] = 'disagreements ' # errado segundo nosso pipeline, certo
# segundo o avaliador humano


vetor<-character()
k = 0

for (i in 1:length(list)){

if (list[[i]]$word %in% c('rival','oral','refil','seol','berreu','pel','jedal',
                          'piliu','tral','mital','meril','guil','siu', 'ropal',
                          'irril', 'gapilal', 'mal', 'funil','gel','papel','mel',
                          'cugol','vuvil','papel','mel','rival','tou','entel',
                          'vissal','irril','rolta','falso', 'rolta', 'falso',
                          'calça','tul','vuvil','salto','fou', 'joula','xiu',
                          'nol','sapil','pou')){
 vetor[k]<-list[[i]]$word 
 
 k = k + 1

}
  print(i)

}

5/8


apendice<-as.data.frame(table(vetor))
colnames(apendice)[1] = 'word'
colnames(apendice)[2] = 'total in sample'

up_result<-left_join(result,apendice, by = 'word')
up_result$ratio<-round(up_result$`disagreements `/up_result$`total in sample`,2)

library(writexl)
write_xlsx(up_result, 'L_confusao.xlsx')
