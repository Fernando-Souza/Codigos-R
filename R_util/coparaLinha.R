Animal<-LETTERS[1:10]
ANO<-c("2018","2019")
MES<-month.name[10:11]
FAZENDA<-rep(1:3,each=3)
dados<-expand.grid(Animal=Animal,Ano=ANO,Mes=MES,Fazenda=FAZENDA)
head(dados)

library(dplyr)
library(plyr)
options(dplyr.print_max = 400)

novodados<-dados%>%group_by(Ano,Mes,Fazenda)%>%mutate(grupo=interaction(Ano,Mes,Fazenda))


    
novodados%>%mutate(grupo=recode_factor(grupo,"2018.October.1"='1',
                  "2019.October.1"='2',"2018.November.1"='3',
                  "2019.November.1"='4',"2018.October.2"='5',
                  "2019.October.2"='6',"2018.November.2"='7',
                  "2019.November.2"='8',"2018.October.3"='9',
                  "2019.October.3"='10',"2018.November.3"='11',
                  "2019.November.3"='12'))

