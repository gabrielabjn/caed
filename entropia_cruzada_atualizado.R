setwd('C:/Users/55229/Desktop/caed/arquivos_json')
amostra<-jsonlite::fromJSON('pseudo_gold_jonatas_and_v1_complete-15-04_processed.json')
setwd('C:/Users/55229/Desktop/scripts')

# letras como utilizadas pelo pipeline (as do IPA sao outras)
letras <- c('p', 'b', 't', 'd', 'k', 'ɡ', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ',
            'ι', 'β', 'm', 'n', 'ɲ', 'ɾ', 'w', 'j', 'μ','ζ', 'ʎ', 'l',
            'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'α' ,'γ', 'δ' ,'θ' ,'κ',
            '<pad>')

list_vet_bin <- list()
count_palavras_com_fone<-list()
list_prob<-list()

# codigo elaborado de maneira que a palavra soh eh contabiliza como 1 caso todos os fonemas de interesse na palavra sejam pronunciados corretamente
# por ex: todos os 'p's sao pronunciados corretamente

#posicao <- which(names(amostra) == '62301612760132_zão')


for (ind in 1:length(letras)){
  
  f<-letras[ind] # fone para o qual será calculada a entropia cruzada
  list_vet_bin[[ind]]<-numeric() # vetor binario (1 se o fone na palavra tiver sido lido corretamente)
  count_palavras_com_fone[[ind]]<-numeric()  # contabiliza o numero de palavras com cada fone
  list_prob[[ind]]<-numeric() # lista de probabilidades top 1 de cada fone, considerando todas as palavras com cada fone 
  cont<-1
  
  for (i in 1:36760){
    
    palavra<- amostra[[i]]$word # pega palavra real
    fone<-amostra[[i]]$prob_phoneme$word # pega transcricao de referencia
    num_fones<-length(amostra[[i]]$prob_phoneme$correction) # numero de fones da palavra transcrita pelo modelo
    tabela<-amostra[[i]]$prob_phoneme$correction # pega tabela de fonemas cotados pras posicoes e probs
    posicao<-numeric() # vetor para armazenar a(s) posicao(oes) dos fones nas palavras
    phone<-numeric() # palavra transcrita (conforme pronunciada)
    
    for (ind2 in 1:num_fones) phone[ind2]<-tabela[[ind2]][1,1] # pega os fones da transcricao do modelo
    
    if (grepl(f,fone)){ # verifica se o fone esta na transcricao de referencia
      
      if (f %in% phone){ # verifica se o fone esta dentre os top 1 da transcricao da pronunciada
        
        for (j in 1:num_fones){ 
          
          if (tabela[[j]][1,1] == f){ # percorre a palavra transcrita e armazena as posicoes com o fone
            posicao[length(posicao)+1]<-j
            
            
          }
        }
        
        #if(length(posicao)>=1){ # se o fone aparece na transcricao fonetica
        
        for (k in 1:length(posicao)){
          # verifica se o fone aparece onde realmente deveria aparecer
          if(substr(fone,posicao[k],posicao[k]) == tabela[[posicao[k]]][1,1]) {list_vet_bin[[ind]][cont]<-1
          list_prob[[ind]][cont]<-tabela[[posicao[k]]][1,2]
          cont<-cont+1
          
          }
          else { 
            
            list_vet_bin[[ind]][cont]<-0
            list_prob[[ind]][cont]<-tabela[[posicao[k]]][1,2]
            cont<-cont+1
            
          } 
          
        }
        
      }
      
      else{ # if(f %in% phone)
      # else = fone nao esta dentre os top 1
      # mas esta dentre os top 3?
        
      # posicao(oes) do fone na transcricao de referencia
      posicao_ref <- as.numeric(gregexpr(f, fone)[[1]])
      
      for (ind3 in posicao_ref) {
        
        if (all(tabela[[ind3]][1,] == f) == TRUE){
          # se o fone apareceu com a segunda ou terceira maior probabilidade
        list_vet_bin[[ind]][cont]<-0
        
        u<-which(tabela[[ind3]][1,] == f)
        prob_<- tabela[[ind3]][2,u]
        
        list_prob[[ind]][cont]<-prob_
        
        cont<-cont+1
        
        }
        
       else # se o fone nao figurou entre os 3 fones de maiores probabilidades
        {
          list_prob[[ind]][cont]<-0 
          list_vet_bin[[ind]][cont]<-0
          cont<-cont+1
        }
        
      }
      
      
      }
      
    
  } #  if (grepl(f,fone))
  
  print(letras[ind])
  print(i)  
    
  }
}

# ------------------------------------------------------------------------------

letras_exibicao<- c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ',
                    'tʃ', 'dʒ', 'm', 'n', 'ɲ', 'r', 'w', 'j', 'w~','j~', 'ʎ', 'l',
                    'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'a~' ,'e~', 'i~' ,'o~' ,'u~',
                    '<pad>')


library(logicDT)

entro<-numeric()
cont<-1
for (i in 1:38){
  
  preds<-as.numeric(list_prob[[i]])
  y<- as.numeric(list_vet_bin[[i]])
  
  print(letras[i])
  entro[cont]<-calcNCE(preds = preds, y = y)
  cont<-cont+1
}

my_df<-as.data.frame(entro, row.names = letras_exibicao )
setwd('C:/Users/55229/Desktop/caed/saidas_codigos')
library(openxlsx)
write.xlsx(my_df, file = "output.xlsx",
           sheetName = 'Sheet2', rowNames=TRUE)



