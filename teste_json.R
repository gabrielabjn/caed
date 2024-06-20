#install.packages('rjson')
library(rjson) 
library(jsonlite)

# Tente ler o arquivo com diferentes codificações
encodings <- c("UTF-8", "latin1")

getwd()
setwd('C:/Users/55229/Desktop/scripts')

berreu<-jsonlite::fromJSON('results_berreu.json')

length(berreu) # 93
class(berreu)

b_0<- list()
e_1<- list()
R_ud_2<- list()
e_3<- list()
w_4<- list()

berreu

# os fonemas mais provaveis sao iguais para todas as palavras no json.
# Apaguei o codigo em que conferia isso (sem me tocar de que poderia deixa-lo)

# Iterar sobre os elementos da estrutura de dados
for (i in 1:93) {
  
  # Extrair a prob de cada fonema mais provavel (por posicao na palavra)
  
    b_0[[i]]<-berreu[[i]]$prob_phoneme$correction$b_0[1,] 
    e_1[[i]]<-berreu[[i]]$prob_phoneme$correction$e_1[1,]
    R_ud_2[[i]]<-berreu[[i]]$prob_phoneme$correction$R_ud_2[1,]
    e_3[[i]]<-berreu[[i]]$prob_phoneme$correction$e_3[1,]
    w_4[[i]]<-berreu[[i]]$prob_phoneme$correction$w_4[1,]
    
    
}

# all(is.character(w_4[1:93][[2]])) # as letras sao todas caracteres!!!

# Extrair letras e probabilidades
vetor_b_0<- unlist(b_0)
vetor_e_1<- unlist(e_1)
vetor_R_ud_2<- unlist(R_ud_2)
vetor_e_3<- unlist(e_3)
vetor_w_4<- unlist(w_4)

# Separar letras e probs por alternância
letras_b_0 <- vetor_b_0[c(TRUE, FALSE)]
probs_b_0 <- as.numeric(vetor_b_0[c(FALSE, TRUE)])

letras_e_1 <- vetor_e_1[c(TRUE, FALSE)]
probs_e_1 <- as.numeric(vetor_e_1[c(FALSE, TRUE)])

letras_R_ud_2 <- vetor_R_ud_2[c(TRUE, FALSE)]
probs_R_ud_2 <- as.numeric(vetor_R_ud_2[c(FALSE, TRUE)])

letras_e_3 <- vetor_e_3[c(TRUE, FALSE)]
probs_e_3 <- as.numeric(vetor_e_3[c(FALSE, TRUE)])

letras_w_4 <- vetor_w_4[c(TRUE, FALSE)]
probs_w_4 <- as.numeric(vetor_w_4[c(FALSE, TRUE)])


table(letras_b_0)
b_posicoes<-which(letras_b_0 == 'b')
b_probs<-probs_b_0[b_posicoes]

d_posicoes<-which(letras_b_0 == 'd')
d_probs<-probs_b_0[d_posicoes]

p_posicoes<-which(letras_b_0 == 'p')
p_probs<-probs_b_0[p_posicoes]

v_posicoes<-which(letras_b_0 == 'v')
v_probs<-probs_b_0[v_posicoes]

# conferir
sum(length(c(b_probs,d_probs,p_probs,v_probs)))
length(letras_b_0)


table(letras_e_1)
c_flipped_posicoes<-which(letras_e_1 == 'c_flipped')
c_flipped_probs<-probs_e_1[c_flipped_posicoes]

e_posicoes<-which(letras_e_1 == 'e')
e_probs<-probs_e_1[e_posicoes]

e_c_posicoes<-which(letras_e_1 == 'e_cursive')
e_c_probs<-probs_e_1[e_c_posicoes]

# conferir
sum(length(c(c_flipped_probs,e_probs,e_c_probs)))
length(letras_e_1)


table(letras_e_3)
pad_posicoes<-which(letras_e_3 == '<pad>')
pad_probs<-probs_e_3[pad_posicoes]

e2_posicoes<-which(letras_e_3 == 'e')
e2_probs<-probs_e_3[e2_posicoes]

e_c2_posicoes<-which(letras_e_3 == 'e_cursive')
e_c2_probs<-probs_e_3[e_c2_posicoes]

e_u_posicoes<-which(letras_e_3 == 'u')
e_u_probs<-probs_e_3[e_u_posicoes]

# conferir
sum(length(c(pad_posicoes,e2_posicoes, e_c2_posicoes, e_u_posicoes)))
length(letras_e_3)


table(letras_R_ud_2)
pad_posicoes_2<-which(letras_R_ud_2 == '<pad>')
pad_probs_2<-probs_R_ud_2[pad_posicoes_2]

r_alv_posicoes<-which(letras_R_ud_2 == 'r_alveolar')
r_alv_probs<-probs_R_ud_2[r_alv_posicoes]

R_ud_posicoes<-which(letras_R_ud_2 == 'R_ud')
R_ud_probs<-probs_R_ud_2[R_ud_posicoes]

s_posicoes<-which(letras_R_ud_2 == 's')
s_probs<-probs_R_ud_2[s_posicoes]

# conferir
sum(length(c(pad_probs_2,r_alv_probs,R_ud_probs,s_probs)))
length(letras_e_3)


table(letras_w_4)
letras <- c('|','<pad>', 'e', 'o', 'u','w')
posicoes <- lapply(letras, function(letra) which(letras_w_4 == letra))
probs <- lapply(posicoes, function(pos) probs_w_4[pos])


table(letras_b_0)
table(letras_e_1)
table(letras_e_3)
table(letras_R_ud_2)
table(letras_w_4)

e_probs_final <- unlist(c(e_probs, e2_probs, probs[3]))
length(e_probs_final)

e_cursive_final <- unlist(c(e_c_probs, e_c2_probs))

pad_final <- c(pad_probs,pad_probs_2, probs[2])

u_final <- unlist(c(e_u_probs, probs[5]))

length(u_final)
length(unlist(probs[6]))

dados<- list(
  
  b = b_probs,
  d = d_probs,
  p = p_probs,
  e = e_probs_final,
  e_cursivo = e_cursive_final,
  r_alveolar = r_alv_probs,
  R_invertido = R_ud_probs,
  barra_vertical = unlist(probs[1]),
  u = u_final,
  w = unlist(probs[6])
  
)


boxplot(
  dados,
  main = "Distribuição dos fonemas de berreu (n = 93)",  # Título do gráfico
  xlab = "Fonemas",  # Rótulo do eixo x
  ylab = "Scores"  # Rótulo do eixo y
)
  

