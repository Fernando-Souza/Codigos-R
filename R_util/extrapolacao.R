dados2<-structure(list(Animal = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 8L,
                                            8L, 8L, 16L, 16L, 16L, 16L, 16L, 16L, 24L, 24L, 24L, 34L, 34L,
                                            34L, 37L, 37L, 37L, 37L, 37L, 37L), .Label = c("1", "12", "14",
                                                                                           "15", "17", "18", "19", "2", "21", "22", "23", "25", "26", "*27",
                                                                                           "28", "3", "30", "32", "34", "35", "37", "38", "39", "4", "40",
                                                                                           "41", "42", "43", "44", "46", "47", "48", "49", "5", "50", "53",
                                                                                           "7", "8", "9"), class = "factor"), Gest = c(140L, 140L, 140L,
                                                                                                                                       140L, 140L, 140L, 100L, 100L, 100L, 130L, 130L, 130L, 130L, 130L,
                                                                                                                                       130L, 100L, 100L, 100L, 100L, 100L, 100L, 140L, 140L, 140L, 140L,
                                                                                                                                       140L, 140L), Manej = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                                                                                                                              1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                                                                                                                              2L), Data = structure(c(-715270, -715256, -715241, -715228, -715214,
                                                                                                                                                                                      -715193, -715270, -715256, -715235, -715270, -715256, -715241,
                                                                                                                                                                                      -715228, -715214, -715200, -715270, -715256, -715235, -715270,
                                                                                                                                                                                      -715256, -715235, -715270, -715256, -715241, -715228, -715214,
                                                                                                                                                                                      -715193), class = "Date"), IntervaloPeso = c(14L, 15L, 13L, 14L,
                                                                                                                                                                                                                                   21L, 0L, 14L, 21L, 0L, 14L, 15L, 13L, 14L, 14L, 0L, 14L, 21L,
                                                                                                                                                                                                                                   0L, 14L, 21L, 0L, 14L, 15L, 13L, 14L, 21L, 0L), Peso = c(37,
                                                                                                                                                                                                                                                                                            38.2, 41, 42.9, 43, 49, 40, 41.8, 40.8, 38, 39.9, 41.9, 45.2,
                                                                                                                                                                                                                                                                                            46.2, 51.8, 40, 40.9, 41.9, 32.3, 34.9, 35, 35.1, 36.5, 35.2,
                                                                                                                                                                                                                                                                                            38.3, 38, 40.5)), .Names = c("Animal", "Gest", "Manej", "Data",
                                                                                                                                                                                                                                                                                                                         "IntervaloPeso", "Peso"), row.names = c(NA, 27L), class = "data.frame")
dados3 <- droplevels(dados2)
## lista com dados divididos por animal
s <- split(x = dados3, f = dados3$Animal)

## looping para na série de cada animal
l <- lapply(s, function(x) {
        # x <- s[[1]]
        print(unique(as.character(x$Animal)))
        x0 <- subset(x, sel = c("Data", "Animal", "Peso"))
        ## datas de referência, desde o primeiro dia ao último com peso
        dref <- data.frame(Data = seq(min(x$Data), 
                                      max(x$Data), 
                                      by = "days"))
        ## gera série de pesos com NAs para as datas sem medida
        m <- merge(dref, x, all = T)
        ## repete o ID do animal
        m$Animal <- sort(unique(m$Animal))
        ## interpola peso para cada dia do período entre a 1a  e última medida
        m$Peso_int <- approx(x = m$Data, 
                             y = m$Peso, 
                             xout = m$Data)$y
        m
}# end fun
)# end lapply
l
dados3
dados2
