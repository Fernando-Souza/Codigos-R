library(nlme)

#------criando fun??o que representa o modelo,retorna gradidente e hessiano-----------
expo.der <- deriv3(~a*exp(c*Gest),c("a", "c"),function(Gest, a,c) NULL)
str(expo.der)
expo.der2<-deriv3(~a*exp(c*Gest),c("a", "c"),function(Gest, a,c) NULL)
str(expo.der2)
#----------------------------------------------------------------------------
diagnose grafica e primeiro chute

plot(NaGLAMA~Gest, data=GM, xlab="idade Gestacional (dias)",
     ylab="conteudo de sodio (g)")
a <- 0.04; c<- 0.03;
curve(expo.der(x,a,c), add=TRUE, col=2)
start <- list(a=a, c=c)

#------# ajustando o modelo aos dados a partir dos valores iniciais via gr?fico--------
n0 <-gnls(NaGLAMA~expo.der(Gest,a,c),weight=varIdent(form=~1|Gest),na.action=na.omit, data=GM[-c(59,57,61),],subset=c(Fetos==1),start=start)
n1<-gnls(NaGLAMA~expo.der(Gest,a,c),weight=varIdent(form=~1|Gest),na.action=na.omit, data=GM[-c(33,63,64),],subset=c(Fetos==2),start=start)
summary(n0)
confint(n0)
str(n0)
#-----------------------------------------------------------------------------
# valores preditos, gradiente e hessiano avaliado nos valores estimados
x <- seq(0,140,20)
m <- expo.der(Gest=x, a=coef(n0)["a"], c=coef(n0)["c"])
q<-expo.der(Gest=x, a=coef(n1)["a"], c=coef(n1)["c"])
#-------------- obten??o dos valores preditos-----------------------------------
pred <- data.frame(Gest=seq(90,150,0.5))
der <- do.call(expo.der, args=c(list(Gest=pred$Gest), as.list(coef(n0))))
der2 <- do.call(expo.der, args=c(list(Gest=pred$Gest), as.list(coef(n1))))
F <- attr(der, "gradient") # gradiente avaliado no novo t
U <- chol(vcov(n0))
se <- sqrt(apply(F%*%t(U), 1, function(x) sum(x^2))) # erro padr?o

G<-attr(der2, "gradient") # gradiente avaliado no novo t
T <- chol(vcov(n1))
se2 <- sqrt(apply(G%*%t(T), 1, function(x) sum(x^2))) # erro padr?o
#--------- gr?ficos dos observados, preditos com IC, legenda e equa??es-------------
#
png("f004.png", w=500, h=400); 
par(mar=c(3.1,4.8,2.1,4.8), usr=c(87.6,152.4,-0.0887938,5.0928551),cex=0.8)
plot(NaGLAMA~Gest, data=GM[GM[-c(59,57,61),]$Fetos==1,], xlab="Idade Gestacional(dias)",
     ylab="Conte?do de S?dio (g)",
     xlim=c(90,150))
matlines(pred$Gest,c(der)+
           outer(se, qt(c(.5,.025,.975), df=summary(n0)$dim$N-summary(n0)$dim$p)),
         type="l", col=c(1,1,1), lty=c(1,2,2))
legend("topleft",
       legend=c("valores observados", "valores preditos",
                "intervalo de confian?a (95%)"),
       lty=c(NA,1,2), col=c(1,1,1), pch=c(1,NA,NA), bty="n")
grid()
  cf <- format(coef(n0), digits=3)
text(x=c(105),y=c(3.5),adj=NULL,
     label=substitute(Na[g]==a%+-%(0.0027)%.%e^(c%+-%(0.007)%.%t),
                      list(a=cf[1], c=cf[2])),font=)

dev.off()

#-----------------------------------------------------------------------------
