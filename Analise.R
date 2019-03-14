install.packages('nortest')
library(nortest)
install.packages('ggplot2')
library(ggplot2)

#Função de moda:
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Função Tira NA
tiraNa = function(lista){
  lista2 = c()
  for( n in lista){
    if (!is.na(n)){
      lista2 = c(lista2,n)
    }
    
  }
  return(lista2)
}

#Função CV:
cv<-function(x){
  coef<-sd(x)/mean(x)*100 
  return(coef)
}

analiseRegiao = function(regiao, nomeRegiao) {
  
  idebFin = regiao[,c("Total.Investido", "IDEB...4.e.5", "IDEB...8.e.9")]
  
  idebI = tiraNa(idebFin$IDEB...4.e.5)
  idebF = tiraNa(idebFin$IDEB...8.e.9)
  
  idebFin$IDEB...4.e.5 = as.numeric(as.character(idebFin$IDEB...4.e.5))
  idebFin$IDEB...8.e.9 = as.numeric(as.character(idebFin$IDEB...8.e.9))
  
  cor1 = cor.test(idebFin$Total.Investido, idebFin$IDEB...4.e.5 )
  cor2 = cor.test(idebFin$Total.Investido, idebFin$IDEB...8.e.9 )
  
  regressaoLinear1 = lm(idebFin$IDEB...4.e.5 ~ idebFin$Total.Investido )
  regressaoLinear2 = lm(idebFin$IDEB...8.e.9 ~ idebFin$Total.Investido )
  
  print("Correlação anos iniciais: \n")
  print(cor1)
  print("\n")
  print("Regressao anos iniciais:\n")
  summary(regressaoLinear1)
  
  print("\n\n")
  
  print("Correlação anos iniciais: \n")
  print(cor2)
  print("\n")
  print("Regressao anos iniciais:\n")
  summary(regressaoLinear2)
}
dados = read.csv("C:\\Users\\aaron\\Documents\\Code\\pibit\\dados\\dados limpos\\ideb_investimento_relativo.csv", encoding = 'utf-8')

ideb_fin_iniciais = dados[,c("Total.Investido", "IDEB...4.e.5")]

ideb =  tiraNa(ideb_fin_iniciais$IDEB...4.e.5)


ideb_fin_iniciais$IDEB...4.e.5 = as.numeric(as.character(ideb_fin_iniciais$IDEB...4.e.5))
cor = cor.test(ideb_fin_iniciais$Total.Investido, ideb_fin_iniciais$IDEB...4.e.5 )


regressaoLinear = lm(ideb_fin_iniciais$IDEB...4.e.5 ~ ideb_fin_iniciais$Total.Investido )


for(value in ideb) {
  if(!is.numeric(value))
    print(value)
}

plot(regressaoLinear)
