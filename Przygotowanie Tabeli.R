

percent <- function(x, digits = 2, format = "f", ...) {
paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}



a$Zawodnik <- gsub("a", "a", a$Zawodnik)
a$Zawodnik <- gsub("c", "c", a$Zawodnik)
a$Zawodnik <- gsub("e", "e", a$Zawodnik)
a$Zawodnik <- gsub("l", "l", a$Zawodnik)
a$Zawodnik <- gsub("n", "n", a$Zawodnik)
a$Zawodnik <- gsub("ó", "o", a$Zawodnik)
a$Zawodnik <- gsub("s", "s", a$Zawodnik)
a$Zawodnik <- gsub("z", "z", a$Zawodnik)
a$Zawodnik <- gsub("z", "z", a$Zawodnik)
a$Zawodnik <- gsub("S", "S", a$Zawodnik)
a$Zawodnik <- gsub("L", "L", a$Zawodnik)


a <- read.xlsx("Strefy1617.xlsx", sheetIndex = 1)




n <- unique(a$Zawodnik)

rt <- function(x) {
d <- subset(a, a$Zawodnik == (x))

#Spod Kosza

kosz <- cbind(d[,2], d[,3])
kosz <- as.data.frame(kosz)
kosz$FGM <- kosz[,1]*kosz[,2]
kosz$Strefa <- "Spod Kosza"
kosz <-  kosz[,c(4,3,1,2)]
colnames(kosz) <- c("Strefa", "FGM", "FGA", "FG%")


#Midrange

mid <- cbind(d[,4], d[,5])
mid <- as.data.frame(mid)
mid$FGM <- mid[,1]*mid[,2]
mid$Strefa <- "Midrange"
mid <-  mid[,c(4,3,1,2)]
colnames(mid) <- c("Strefa", "FGM", "FGA", "FG%")

#3pt Srodek


sr3 <- cbind(d[,6], d[,7])
sr3 <- as.data.frame(sr3)
sr3$FGM <- sr3[,1]*sr3[,2]
sr3$Strefa <- "3pt Srodek"
sr3 <-  sr3[,c(4,3,1,2)]
colnames(sr3) <- c("Strefa", "FGM", "FGA", "FG%")


#3pt Lewy Naroznik


ln3 <- cbind(d[,8], d[,9])
ln3 <- as.data.frame(ln3)
ln3$FGM <- ln3[,1]*ln3[,2]
ln3$Strefa <- "3pt Lewy Naroznik"
ln3 <-  ln3[,c(4,3,1,2)]
colnames(ln3) <- c("Strefa", "FGM", "FGA", "FG%")

#3pt Prawy Naroznik


pn3 <- cbind(d[,10], d[,11])
pn3 <- as.data.frame(pn3)
pn3$FGM <- pn3[,1]*pn3[,2]
pn3$Strefa <- "3pt Prawy Naroznik"
pn3 <-  pn3[,c(4,3,1,2)]
colnames(pn3) <- c("Strefa", "FGM", "FGA", "FG%")

strefy <- rbind(kosz, mid, sr3, ln3, pn3)
strefy[,4] <- percent(strefy[,4])

strefy$zawodnik <- d$Zawodnik

f <- strefy


}


st <- data.frame()
for(i in n) {
st <- rbind(st, rt(i))
}

write.xlsx(st, "Strefy1617.xlsx")






