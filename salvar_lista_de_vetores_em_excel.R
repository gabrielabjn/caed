
setwd('C:/Users/55229/Desktop/scripts')
amostra<-readRDS('minha_lista.rds')

letras_exibicao<-c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ', 
                   'tʃ', 'dʒ', 'm', 'n', 'ɲ', 'ɾ', 'w', 'j', 'w~','j~', 'ʎ', 'l',
                   'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'a~' ,'e~', 'i~' ,'o~' ,'u~',
                   '<pad>')

# Função para preencher vetores com NA até um certo comprimento
pad_to_length <- function(x, len) {
  if (length(x) < len) {
    c(x, rep(NA, len - length(x)))
  } else {
    x[1:len]
  }
}

# Encontrando o comprimento máximo dos vetores na lista
max_length <- max(sapply(amostra, length))

# Preenchendo os vetores com NA até o comprimento máximo
lista_preenchida <- lapply(amostra, pad_to_length, len = max_length)

library(openxlsx)
wb <- createWorkbook()
# Adicionar cada vetor como uma coluna no Excel
addWorksheet(wb, "Dados2")
for (i in seq_along(lista_preenchida)) {
  writeData(wb, "Dados2", lista_preenchida[[i]], startCol = i)
}

# Salvar o arquivo Excel
saveWorkbook(wb, "dados2.xlsx", overwrite = TRUE)


