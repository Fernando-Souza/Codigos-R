klib <- data.frame(k=c(51.03, 57.76, 26.60, 60.65, 87.07, 64.67,
                       91.28, 105.22, 72.74, 81.88, 97.62, 90.14,
                       89.88, 113.22, 90.91, 115.39, 112.63, 87.51,
                       104.69, 120.58, 114.32, 130.07, 117.65, 111.69,
                       128.54, 126.88, 127.00, 134.17, 149.66, 118.25,
                       132.67, 154.48, 129.11, 151.83, 147.66, 127.30),
                   t=rep(c(15, 30, 45, 60, 75, 90,
                           120, 150, 180, 210, 240, 270), each=3))

#-----------------------------------------------------------------------------
# ajustando o modelo de regress?o n?o linear aos dados

n0 <- gnls(k~A*t/(V+t)+D*t, data=klib, start=list(A=90, V=15, D=0.21))
summary(n0)

#-----------------------------------------------------------------------------
# extraindo a matriz gradiente avaliada nas estimativas dos par?metros

F <- attr(n0$m$fitted(), "gradient")
F

#-----------------------------------------------------------------------------
# passando a matriz gradiente para a lm(), importante remover intercepto

m0 <- lm(k~-1+F, data=klib)
summary(m)
#-----------------------------------------------------------------------------
# gr?fico de an?lise dos res?duos

#png("f007.png", w=500, h=500);
par(mfrow=c(2,2), mar=c(5.1,4.1,4.1,2.1))
plot(m0)
mtext("An?lise de res?duos para modelo de regress?o n?o linear",
      outer=TRUE, line=-2, cex=1.4)
layout(1)
#dev.off()

#-----------------------------------------------------------------------------
# veja que o ajuste ? o mesmo pelas medidas abaixo

cbind(fitted(m0), fitted(n0))       # valores ajustados
cbind(residuals(m0), residuals(n0)) # valores preditos
c(summary(m0)$sig, summary(n0)$sig) # estimativa do desvio padr?o residual
vcov(m0); vcov(n0)                  # matriz de covari?ncia das estimativas

#-----------------------------------------------------------------------------
# quadro de anova com SQ de regress?o e SQ de res?duo

anova(m0) # parti??o da soma de quadrados total
anova(n0, lm(k~1, klib)) # SQ do modelo n?o linear corrigido para intercepto

#-----------------------------------------------------------------------------
# fun??o que retorna a anova e R2 para modelos de regress?o n?o linear

R2 <- function(nls.obj){
  da <- eval(nls.obj$data)
  resp.name <- all.vars(summary(nls.obj)$formula)[1]
  form <- paste(resp.name, "~1", sep="")
  m0 <- lm(form, da)
  an <- anova(nls.obj, m0)
  sqn <- deviance(nls.obj)
  sqe <- deviance(m0)
  r2 <- 1-(sqn/sqe)
  aov <- data.frame(fv=c("regression","residuals"),
                    gl=c(-an$Df[2],an$Res.Df[1]),
                    sq=c(-an$Sum[2],an$Res.Sum[1]))
  aov$qm <- aov$sq/aov$gl
  aov$F <- c(aov$qm[1]/aov$qm[2], NA)
  aov$"Pr(>F)" <- c(1-pf(aov$F[1], df1=aov$gl[1], df2=aov$gl[2]), NA)
  names(aov) <- c(" ","Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  return(list(anova=aov, R2=r2))
}

R2(n0)