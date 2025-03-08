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
