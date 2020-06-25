# Comandos para listar todos os pacotes instalados pelo usuário, sua remoção
#e reinstalação
#--------------------------------------------------------------------
# create a list of all installed packages
ip <- as.data.frame(installed.packages())
head(ip)

# if you use MRO (Microsoft R Open), make sure that no packages in this library will be removed
# Not run
#ip <- subset(ip, !grepl("MRO", ip$LibPath))

# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
ip
# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)

# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- ip[,1]
head(pkgs.to.remove)

# Salva a lista de pacotes instalados pelo usuário a remover
save(pkgs.to.remove, file="/home/fernando/Documentos/Biblioteca/Analises Estatisticas R/Funcoes R/pkgsToRemove.rda")

# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)

load("~/Documentos/Biblioteca/Analises Estatisticas R/Funcoes R/pkgsToRemove.rda")
pkg.to.install <- c("agricolae","car","colorspace","data.table","dplyr","emmeans","ggplot2","gridExtra","gridBase","gtable","latticeExtra","plyr","lme4","lmmfit","lsmeans","multcomp","multcompView","MuMin","reshape","reshape2","rmarkdown","knitr","shiny","tidyr","xtable")
for (count in 1:length(pkg.to.install)) install.packages(pkg.to.install[count])
