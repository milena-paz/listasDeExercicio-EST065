dia.semana <-
function(dia=as.numeric(substr(Sys.Date(),9,10)),
                      mes=as.numeric(substr(Sys.Date(),6,7)),
                      ano=as.numeric(substr(Sys.Date(),1,4))){
  #vetor com todos os nomes dos dias da semana
  diasSemana<- c("Domingo",paste0(c('Segunda','Terça','Quarta','Quinta','Sexta'),"-feira"),
                 "Sábado")
  #verifica quais sao janeiro ou fevereiro
  janfev <- mes<3
  #define ano no seculo Y
  Y<-ano%%100
  #define o número do século C
  C<-floor(ano/100)
  #define dia do mes K
  K<- dia
  #define o número do mês M
  M<- numeric(length(mes))
  M[which(janfev)] <- mes[which(janfev)]+10
  M[which(!janfev)] <- mes[which(!janfev)]-2
  #calcula a congruencia de Zeller
  f<-(floor(2.6*M+0.2)+K+Y+floor(Y/4)+floor(C/4)-2*C)%%7
  f[which(f==0)] <- 7
  return(diasSemana[f])
}
