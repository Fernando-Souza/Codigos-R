library(nlme)
consumo <- read.csv("/home/fernando/Documentos/artigos R/Meta analise consumo/metaanalise.csv")
consumo<-na.omit(consumo[,2:16])
head(consumo)
str(consumo)
consumo<-transform(consumo, GENOTIPO=factor(GENOTIPO), SEXO=factor(SEXO),VOL=factor(VOL),Estudo=factor(Estudo))
dados1<-groupedData(CMS~PM+GPD+FDN|Estudo,data=consumo[ consumo$VOL==1 & consumo$SEXO==1,])
controle<-lmeControl(maxIter=200,opt="optim",msMaxIter=300)
modelo3<-lme(fixed= CMS~PM+GPD+FDN+I(FDN^2),data=dados1,random=~1+GPD+FDN|Estudo,control=controle,na.action=na.omit,method="REML")
modelo3.1 <- update(modelo3,weights=varIdent(form=~1+GPD|Estudo),correlation=corSymm(form=~1|Estudo),na.action=na.omit,control=controle)

da<-dados1 ## seu data.frame
L <- vector(mode="list", length=nrow(da))
for(i in 1:nrow(da)){
    L[[i]] <- sua_funcao_R(..., data=da[-i,])
}

## Medidas de deviance leave-one-out.
sapply(L, deviance)

## log-veross leave-one-out.
sapply(k, logLik)
#------------------------------------------------------------
da <- dados1
L <- vector(mode="list", length=nrow(da))
for(i in 1:nrow(da)){
    L[[i]] <-lme(fixed= CMS~PM+GPD+FDN+I(FDN^2),random=~1|Estudo,na.action=na.omit,method="REML", data=da[-i,])
    
}

## Medidas de deviance leave-one-out.

sapply(L, FUN=function(x){-2*logLik(x)}) #Deviance

## log-veross leave-one-out.
sapply(L, logLik)

library(MuMIn);library(lmmfit)
mean(sapply(L, r.squaredGLMM)[1,]) # average R2 marginal
mean(sapply(L, r.squaredGLMM)[2,]) # average R2 condicional
r.squaredGLMM(modelo3.1) # pseudo R2 comum
mean(sapply(L, function(x)sqrt(mean(x$residuals^2)))) # RMSE = desvio padrao da media
sapply(L, logLik)
lmmPRESS(modelo3.1) #Estatistica PRESS
r.squaredGLMM(modelo3.1)




#depende do pacote lmmfit para funçao lmmPRESS
#library(lmmfit)
   K <- vector(mode="list", length=nrow(dados1))
   PREDITO <- vector(mode="numeric", length=nrow(dados1))
   press <- vector(mode="numeric", length=nrow(dados1))
   erro <- vector(mode="numeric", length=nrow(dados1))
for(i in 1:nrow(dados1)){
    
    K[[i]] <-lme(CMS~PM+GPD,random=~1+FDN+GPD|Estudo,weights=varIdent(form=~1+FDN+GPD|Estudo),correlation=corSymm(),control=controle,na.action=na.omit,data=dados1[-i,],method="REML") # modelos gerados com remoçao de cada ponto observado
    PREDITO[i]<- predict(K[[i]],dados1[i,],asList=FALSE)# valores preditos pelo grupo teste (ponto removido durante ajuste)
    erro[i] <- dados1$CMS[i] - PREDITO[i] # Erro de prediçao = CMS OBSERVADO -CMS PREDITO
    LOOCV_MSE <- sum(na.omit(I(erro^2)))/nrow(dados1) #Quadrado medio do erro de prediçao
    LOOCV_RMSE <- sqrt(LOOCV_MSE) #Raiz do quadrado medio do erro de prediçao = desvio padrao
       
     
}
   cat("leave-on-out cross validation acuracia:\n")
   cat("----------------------\n")
   cat("LOOCV_MSE:",LOOCV_MSE,"\n")
   cat("LOOCV_RMSE:",LOOCV_RMSE,"\n")
   cat("LOOCV_PRESS:",LOOCV_PRESS,"\n")
summary(modelo3.1)
lmmPRESS(modelo3.1)

for(i in 1:nrow(dados1)){
    
    L <- vector(mode="list", length=nrow(dados1))
 L[[i]] <-lme(fixed= CMS~PM+GPD+FDN+I(FDN^2),random=~1|Estudo,weights=varIdent(form=~1+GPD|Estudo),correlation=corCompSymm(form=~1|Estudo),na.action=na.omit,control=controle,method="REML", data=dados1[-i,]) # modelos gerados com remoçao de cada ponto observado

}

validation <- function(x,dados){

        
    PREDITO<- predict(x,dados[i,],asList=FALSE)# valores preditos pelo grupo teste (ponto removido durante ajuste)
    erro <- dados1$CMS[i] - PREDITO[i] # Erro de prediçao = CMS OBSERVADO -CMS PREDITO
    LOOCV_MSE <- mean(sum(erro^2)/nrow(dados1)) #Quadrado medio do erro de prediçao
    LOOCV_RMSE <- mean(sqrt(LOOCV_MSE)) #Raiz do quadrado medio do erro de prediçao = desvio padrao
    press[i] <-lmmPRESS(L[[i]])
    LOOCV_PRESS <- mean(press)        
     
}
names(L)
    
