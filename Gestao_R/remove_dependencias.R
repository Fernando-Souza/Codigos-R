#Esta e uma funçao mais completa, disponivel em: http://stackoverflow.com/questions/26573368/uninstall-remove-r-package-with-dependencies
#Esta funçao remove as dependencias de pacotes os quais nao sao utilizados por nenhum outro pacote instalado. Ele oferece uma interface grafica o qual nos permite controlar qual dependencia desistalar.

#"Here is some code that will all you to remove a package and its unneeded dependencies. Note that its interpretation of "unneeded" depend#ent packages is the set of packages that this package depends on but that are not used in any other package. This means that it will also# default to suggesting to uninstall packages that have no reverse dependencies. Thus I've implemented it as an interactive menu (like in #update.packages) to give you control over what to uninstall."

library("tools")

removeDepends <- function(pkg, recursive = FALSE){
    d <- package_dependencies(,installed.packages(), recursive = recursive)
    depends <- if(!is.null(d[[pkg]])) d[[pkg]] else character()
    needed <- unique(unlist(d[!names(d) %in% c(pkg,depends)]))
    toRemove <- depends[!depends %in% needed]
    if(length(toRemove)){
         toRemove <- select.list(c(pkg,sort(toRemove)), multiple = TRUE,
                                 title = "Select packages to remove")
         remove.packages(toRemove)
         return(toRemove)
    } else {
        invisible(character())
    }
}
removeDepends("HH")
#----------------------------------------------------------------------
#Funçao para automatizar a remoçao de pacotes instalados.
#Autor: Fernando A. Souza
#Dependencias: pacote {tool}
#OBS: Essa funçao remove os pacotes instalados do R. Esta funçao nao verifica as dependencias dos pacotes
#por isso esteja certo sobre os pacotes que ira resmover.

remove.pacotes<-function(names) {
    x<-names
    library("tools")
    for (i in 1:length(x)){
   
         remove.packages(x[i])
    }
    return("pacotes removidos")
}

remove.pacotes(c("e1071", "RcmdrMisc", "tcltk2", "httpuv", "xtable", "leaps", "vcd", "Rcmdr"))
