teste <- read.csv("/home/fernando/Documentos/teste.csv")
dput(teste)
library(plyr)
a<-function(x){
      for (i in 1:length(x)){
           if (x[i] == 1) {
                   x[i] <- "Discordo totalmente"
           }
           if (x[i] == 2) {
                   x[i] <-"Discordo"
           }
           if (x[i] == 3) {
                   x[i]<-"Neutro"
           }
           if (x[i] == 4) {
                   x[i]<-"Concordo"
           }
           if (x[i] == 5) {
                   x[i]<-"concordo totalmente"
           }
   }
   return(x)
}
 nomes<-names
transformado<- ddply(teste[,4:9],.(c(names(teste)[4:9])),a)
tabelatransf <- cbind(teste[,1:3],transformado)
] tabelatransf

teste
media<-function(x){
        mean(x$age)
}
ddply(a21,.(n_vaca,dia_ciclo),media)

dfx <- data.frame(
        group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
        sex = sample(c("M", "F"), size = 29, replace = TRUE),
        age = runif(n = 29, min = 18, max = 54)
)
dfx
?round
?aggregate
