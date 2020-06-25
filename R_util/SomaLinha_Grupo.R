# O código abaixo, divide (split) a tabela em função de alguns fatores, soma as colunas e adiciona o resultado na última coluna da subtabela. Então recopõem as novas subtabelas e retorna um dataframe

dados <- structure(list(id = c(49L, 49L, 49L, 64L, 64L, 64L, 23L, 23L, 23L, 23L), evento1 = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("A", "D", "E"), class = "factor"), nomes = structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 1L), .Label = c("Anicuns", "Goiânia", "Santo Antônio do Descoberto"), class = "factor"), cons_jan_15 = c(9033.36, NA, NA, NA, NA, 4091.54, 6833.28, NA, NA, NA), valor_jan_15 = c(4848.67, NA, NA, NA, NA, 3076.74, 3772.8, NA, NA, NA), cons_fev_15 = c(13131.13, NA, NA, NA, NA, 5482.15, 8706.81, NA, NA, NA), valor_fev_15 = c(4320.57, NA, NA, NA, NA, 2243.94, 2961.72, NA, NA, NA), cons_mar_15 = c(8121.07, NA, NA, NA, NA, 3380.04, 5524.23, NA, NA, NA), valor_mar_15 = c(4410.8, NA, NA, NA, NA, 2033.13, 3285.14, NA, NA, NA), cons_abr_15 = c(8562.85, NA, NA, NA, NA, 3003.66, 6157.68, NA, NA, NA), valor_abr_15 = c(6241.59, NA, NA, NA, NA, 2250.64, 4381.01, NA, NA, NA), cons_mai_15 = c(8261.5, NA, NA, NA, NA, 3601.44, 5997.78, NA, NA, NA), valor_mai_15 = c(5586.92, NA, NA, NA, NA, 2550.23, 4178.95, NA, NA, NA)), .Names = c("id", "evento1", "nomes", "cons_jan_15", "valor_jan_15", "cons_fev_15", "valor_fev_15", "cons_mar_15", "valor_mar_15", "cons_abr_15", "valor_abr_15", "cons_mai_15", "valor_mai_15"), class = "data.frame", row.names = c(NA, -10L))

library(plyr)
dados <- transform(dados,id=factor(id),evento1=factor(evento1),nomes=factor(nomes))

adicLinha <- function(x) {
    soma<-numeric()
    for(i in 1:ncol(x)){
        
        if(is.numeric(x[,i])){

            soma[i] <-sum(x[,i],na.rm=TRUE)
            
        }           

   }
     return(rbind(x,soma)) 
    }
    
ddply(dados,.(id,evento1,nomes),adicLinha)



