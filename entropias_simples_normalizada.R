install.packages('DescTools')
library(DescTools)

entropias<-lapply(lista, function(x) Entropy(x))
entropias<-as.numeric(entropias)

df<-data.frame(entropias)

rownames(df)<-c('p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 's', 'z', 'ʃ', 'ʒ', 'ʁ',
    'tʃ', 'dʒ', 'm', 'n', 'ɲ', 'r', 'w', 'j', 'w~','j~', 'ʎ', 'l',
    'a', 'e', 'ɛ', 'i', 'o', 'ɔ', 'u', 'a~' ,'e~', 'i~' ,'o~' ,'u~',
    '<pad>')

install.packages("openxlsx")
library(openxlsx)
write.xlsx(df, "C:/Users/55229/Desktop/caed/notas/output.xlsx")
write.table(df, "C:/Users/55229/Desktop/caed/notas/output.xlsx", sep = "\t", row.names = FALSE)
