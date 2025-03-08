dados <- read.csv("Tab7.1-Costa.txt",dec=",",sep = "\n",header = F)
dados <- dados[,1]
alvo <- 100
peso <- 0.2

Y<-alvo
ewma.r<- function(x,lambda,mi0){
  i<-length(x)
  if(i==0) return(mi0)
  return(lambda*x[i] + (1-lambda)*ewma.r(x[-i],lambda,mi0))
}
#aplica a função para cada posição
Y<-append(Y, sapply(1:length(dados),FUN=function(x){
      ewma.r(x=dados[1:x], lambda=peso,mi0=Y)
    })
  )

# A funcao "ewma.r" para calcular o Y_n ela calcula tambem o Y_(n-1) e assim em diante.
# Ela funciona bem em um contexto isolado, mas no contexto de preencher um vetor
# o valor de Y_(n-1) ja foi calculado, entao se fizer uma "versao" diferente da funcao
# tem como diminuir muito a complexidade do algoritmo.
# Por exemplo na hora de calcular o N+1-esimo elemento ele vai calcular o N-esimo elemento
# uma segunda vez, e o N-1-esimo elemento uma terceira vez, e assim em diante.
# A minha ideia seria fazer uma funcao que usa o termo anterior de Y, e aplicar essa funcao
# no meio de uma recursividade

ewma.r2 <- function(n,lam,valor.anterior)
{
  # n - o k-esimo termo de X
  # lam - lambda
  # valor.anterior - Y_(k-1)
  return(lam*n + (1-lam)*valor.anterior)
}
# mi0 nao incluso, tem que atribuir ele previamente

Montador.de.vetores <- function(x,k,lambda,valor.atual)
{
  #interrompendo a recursividade
  if(k<1 | k>length(x))
    return()
  # eu defino o valor atual usando o "valor.atual" anterior.
  # o valor.atual seria o Y_(k-1) antes de passar pela funcao, que depois me retorna o Y_(k)
  
  valor.atual = ewma.r2(x[k],lambda,valor.atual)
  
  # depois retorna em um vetor o valor atual (Y_k) seguido a proxima iteracao dessa funcao
  # muita enfase no fato que do ponto de vista da proxima iteracao o valor.atual
  # na verdade seria o valor da anterior
  return( c(valor.atual, Montador.de.vetores(x,k+1,lambda,valor.atual) ))
}
y <- alvo
y <- append(y, Montador.de.vetores(a,k=1,lambda=0.2,y))


identical(y,Y)
#####################
#FUNDINDO AS FUNÇÕES#
#####################

#é possível juntar as funções Montador.de.vetores e ewma.r2 da seguinte forma:
ewma.r3 <- function(x,k,lambda,valor.atual)
{
  #interrompe a recursividade
  if(k<1 | k>length(x))
    return()
  #atualiza valor.atual (de Y[k-1] para Y[k]) sem necessidade
  #de função auxiliar (já que a auxiliar era suficientemente simples)
  valor.atual <- lambda*x[k] + (1-lambda)*valor.atual
  #recursão concatena o valor calculado com os seguintes
  return( c(valor.atual, ewma.r3(x,k+1,lambda,valor.atual) ))
}
y <- alvo
y <- append(y, ewma.r3(dados,k=1,lambda=0.2,y))

identical(y,Y)