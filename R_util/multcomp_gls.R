#========================================================================================================================
#As funçoes abaixo foram criadas para permitir a utilizaçao da funçao 'gls' com a funçao glht() {'multcomp"}.
#para utiliza-la apenas ative as funçoes e em seguida execute a funçao glht(). Nao e necessario fornecer argumentos as
#funçoes abaixo.
#========================================================================================================================
model.matrix.gls <- function(object, ...) {
    model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
    model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
    terms(model.frame(object), ...)
}
#========================================================================================================================
library(  nlme)
