# ------------------
# Minicurso R Caed
# Gabriela P.S.
# 08 de abril de 2024
# ------------------

# Comecar com o exemplo classico -----------------------------------------------

print('Hello Wold')


# Criacao de variaveis ---------------------------------------------------------

nome <- 'gabriela'
ano_nascimento <- 2001

nome
ano_nascimento


# Concatenacao de elementos ----------------------------------------------------

texto <- 'UFJF'
paste('Caed/',texto)

# Tipos de dados ---------------------------------------------------------------

# numeric
# integer
# complex
# character
# logical

class(texto)
class(ano_nascimento)
class(10 > 9)


# Percorrer um objeto ----------------------------------------------------------

for (x in 1:10) {
  print(x)
}


# Loop -------------------------------------------------------------------------

i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
}


# Criar uma funcao -------------------------------------------------------------

soma_elementos <- function(vetor) { 
  soma <- sum(vetor)
  return(soma)
}

soma_elementos(c(1,2,3,4,5))

# Recursao ---------------------------------------------------------------------

soma_com_anteriores <- function(k) {
  if (k > 0) {
    result <- k + tri_recursion(k - 1)
  } else {
    result = 0
    return(result)
  }
  
  return(result)
}

soma_com_anteriores(6)

sum(seq(1:6))

# Variaveis que sao criadas de fora de uma funcao sao chamadas de globais e 
# podem ser usadas em todo lugar

# Variaveis criadas dentro de uma funcao so existem dentro da funcao


# Vetores (comecam do 1) -------------------------------------------------------

frutas<-c('banana', 'melao', 'laranja')

frutas[2]<- 'pera' # altera segundo elemento do vetor

frutas

length(frutas) # verifica comprimento do vetor

# Obs: Nao confundir vetores com arrays; arrays sao dados multidimensionais 
# (a matriz eh um caso particular de array de 2 dimensoes), enquanto os 
# vetores possuem apenas uma dimensao.

# Listas -----------------------------------------------------------------------

lista_de_compras <- list(c(), c(), c())

names(lista_de_compras) <- c('higiene', 'frutas', 'verduras')

lista_de_compras

lista_de_compras[[1]]<- c('sabonete', 'pasta de dentes', 'fio dental')
lista_de_compras
# completar para frutas e verduras



# Acesso a elemento dentro de uma categoria
lista_de_compras[[1]][2]

# Acrescentar elemento
lista_de_compras<- append(lista_de_compras, 'categoria extra', after = 1)
names(lista_de_compras[[2]])<-'material_de_limpeza'
lista_de_compras

# Remover categorias da lista

lista_de_compras<-lista_de_compras[-2]
lista_de_compras

# Remover elementos dentro de uma categoria
lista_de_compras[[1]][-1]


# Matrizes ---------------------------------------------------------------------

minha_matriz <- matrix(seq(1:9), nrow = 3, ncol = 3)
minha_matriz

# Acesso a itens especificos

minha_matriz[1,2]
minha_matriz[3,3]

# Acesso a linhas

minha_matriz[1,] # linha 1

# Acesso a colunas

minha_matriz[,3] # coluna 3
minha_matriz[,c(2,3)] # colunas 2 e 3


# Obs2: Nao confundir matrizes com dataframes; dataframes podem conter varios
# tipos de dados, enquanto que as matrizes contem apenas um tipo.


# Dataframes -------------------------------------------------------------------

meu_df<- data.frame (
  Estagiarios_Caed= c('Caio', 'Romulo', 'Patrick', 'Gustavo', 'Gabriela'),
  Cursos = c('Eng Computacional', rep('CC', 3), 'Estatistica'),
  Tempo_de_Servico = c(rep('Acima de 18 meses',2), '2 meses', rep('1 mes', 2))
)

meu_df # printe o dataframe

summary(meu_df)

# Acesso a itens

meu_df$Estagiarios_Caed 

meu_df[1]

meu_df[['Estagiarios_Caed']]

# Filtrar linhas por valor de coluna

meu_df[meu_df$Estagiarios_Caed == 'Caio',]


# Factors ----------------------------------------------------------------------

# Sao usados para categorizar dados

generos_musicais <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"))
generos_musicais


# Graficos ---------------------------------------------------------------------

# Comum ----------------------------------------

plot(1:100, pch = 19, col = 'blue')

# add argumento type = 'l' para plotar linha
# xlab="The x-axis", ylab="The y axis"


# Pizza ----------------------------------------

x <- c(10,20,30,40)
pie(x)
# label = c() para acrescentar legendas


# Histograma -----------------------------------

hist(rnorm(100), col = 'lightblue', freq = FALSE)
curve(dnorm(x), add = TRUE, col = 'red', lwd = 2)


# Boxplot --------------------------------------

boxplot(rnorm(100), col = 'lightpink')


# Estatisticas -----------------------------------------------------------------

amostra<-rnorm(100,mean = 5, sd = 2) # Distribuicao original: N(5,4)
hist(amostra, col = 'lightgreen')

mean(amostra) # media
median(amostra) # mediana
quantile(amostra, 0.25 ) # quantil 
var(amostra) # sample variance
  

# O R conta com alguns conjuntos de dados para praticar

data('mtcars')
mean(mtcars$mpg)

data('iris')





