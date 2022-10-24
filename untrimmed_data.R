#Untrimmed datasheets - øvelse i R
setwd("C:/Users/Karoline/OneDrive/Speciale/Databehandling/Datasæt")
getwd()
library(vegan)

#Jeg importerer data via importerknappen
#Jeg importerer både sheet1 og 2 som jeg jeg har gemt som hver sin csv-fil
#kunne også gøres via read.table funktionen, men den ved jeg ikke helt, hvordan fungerer endnu

head(COSQ_All_MOTUs_classified_allMOTUS_sheet1)
#viser de første få rækker af din tabel

tail(COSQ_All_MOTUs_classified_uniquetaxa_sheet2)
#viser sidste rækker i tabel

str(COSQ_All_MOTUs_classified_allMOTUS_sheet1)

#viser hvilken type data der er tale om, kategoriske, nummeriske osv. 
#altid tjek hvilken type data du arbejder med før du går i gang. chr = character, int = integer
#integer = heltal


####Filtrering af data####
head(COSQ_All_MOTUs_classified_uniquetaxa_sheet2$ï..kingdom)
#viser de første rækker i kolonnen "kingdom"
#"Fungi" "Fungi" "Fungi" "Fungi" "Fungi" "Fungi"

class(COSQ_All_MOTUs_classified_uniquetaxa_sheet2$ï..kingdom)
#viser hvilken type variabel, der er tale om i kingdom-kolonnen
#i dette tilfælde en character


summary(COSQ_All_MOTUs_classified_uniquetaxa_sheet2$ï..kingdom)
#giver et summary af denne kolonne
#Length     Class      Mode 
#   686 character character

COSQ_All_MOTUs_classified_uniquetaxa_sheet2$ï..kingdom <- as.factor(COSQ_All_MOTUs_classified_uniquetaxa_sheet2$ï..kingdom)
#omdanner kingdom_kolonnen i sheet 1 til en faktor

class(COSQ_All_MOTUs_classified_uniquetaxa_sheet2$ï..kingdom)
#nu viser den "factor"

Metazoa <- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,ï..kingdom=="Metazoa")
summary(Metazoa)
Fungi <- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,ï..kingdom=="Fungi")
summary(Fungi)
Viridiplantae <- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,ï..kingdom=="Viridiplantae")
summary(Viridiplantae)

#Nu har jeg lavet 3 objekter, der angiver hver af de kingdoms der er fundet i vores samples

#Objekter der angiver antal af hvert kingdom (alså hvor mange der er fundet af hver)

a <- length(unique(Metazoa$score.id))
b <- length(unique(Fungi$score.id))
c <- length(unique(Viridiplantae$score.id))

#Length funktionen tæller antallet af et givent objekt
#Nu har jeg lavet 3 objekter, hvor jeg angiver hvor mange arter (score.id) der er fundet inden for hver kingdom (Metazoa, Fungi, Viridiplantae)

biodiv <- c(a,b,c)
#Jeg laver nu en vektor som indeholder antallet af arter inden for hver kingdom
biodiv
#382  56  27
names(biodiv) <- c("Metazoa",
                   "Fungi",
                   "Viridiplantae")
#nu har jeg navngivet alle værdierne i biodiv vektoren alt efter
#dvs at første tal 382 er navngivet Metazoa --> der er altså 382 forskellige arter af Metazoa

barplot(biodiv)
#her har jeg lavet et barplot over biodiversitetsvektoren 

barplot(biodiv, xlab="Kingdom", ylab="Number of species", ylim=c(0,400), cex.names=1, cex.axis=1, cex.lab=1.5)
#her har jeg tilføjet aksetitler, justeret limits på y-aksen og justeret skriftstørrelse


####barplot med antal arter pr phylum####

#Metazoa
Annelida <- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Annelida")
Arthropoda<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Arthropoda")
Bryozoa<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Bryozoa")
Chordata<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Chordata")
Cnidaria<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Cnidaria")
Ctenophora<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Ctenophora")
Echinodermata<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Echinodermata")
Mollusca<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Mollusca")
Nemertea<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Nemertea")
Platyhelminthes<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Platyhelminthes")
Porifera<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Porifera")
Xenacoelomorpha<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Xenacoelomorpha")
Rotifera<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Rotifera")
Tardigrada<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Tardigrada")
Gastrotricha<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Gastrotricha")
Gnathostomulida<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Gnathostomulida")
Nematoda<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Nematoda")
Nemertea<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Nemertea")

#Nu har vi lavet objekter for hvert phylum 
#vi har filtreret alle de rækker ud som passer til et givent phylum i phylumkolononnen
#fx har vi under Annelida, filterereret alle de rækker ud i phylum-kolonnen, hvor der står Annelida


d <- length(unique(Annelida$score.id))
e <- length(unique(Arthropoda$score.id))
f <- length(unique(Bryozoa$score.id))
g <- length(unique(Chordata$score.id))
h <- length(unique(Cnidaria$score.id))
i <- length(unique(Ctenophora$score.id))
j <- length(unique(Echinodermata$score.id))
k <- length(unique(Mollusca$score.id))
l <- length(unique(Nemertea$score.id))
m <- length(unique(Platyhelminthes$score.id))
n <- length(unique(Porifera$score.id))
o <- length(unique(Xenacoelomorpha$score.id))
p <- length(unique(Rotifera$score.id))
q <- length(unique(Tardigrada$score.id))
r <- length(unique(Gastrotricha$score.id))
s <- length(unique(Gnathostomulida$score.id))
t <- length(unique(Nematoda$score.id))


#tæller antallet af rækker i score.id-kollonnen der hører til de forskellige phylums
#fx er der 38 rækker i score.id kolonnen (dvs 38 arter) som hører til ud for Nemertea-kolonnen
#altså 38 Nemertea arter
#unique funktionen er en nest-funktion - der leder efter bestemte objekter i datasættet, i den specificererde kolonne man har sat den til

phylum_metazoa <- c(d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
#Nu har jeg lavet en vektor som indeholder antallet af alle de forskellige phyla/rækker af metazoa

names(phylum_metazoa) <- c("Annelida",
                           "Arthropoda",
                           "Bryozoa",
                           "Chordata",
                           "Cnidaria",
                           "Ctenophora",
                           "Echinodermata",
                           "Mollusca",
                           "Nemertea",
                           "Platyhelminthes",
                           "Porifera",
                           "Xenacoelomorpha",
                           "Rotifera",
                           "Tardigrada",
                           "Gastrotricha",
                           "Gnathostomulida",
                           "Nematoda")
#Nu har jeg navngivet alle objekterne i phylum_metazoa vektoren. 

par(mar = c(7, 4, 2, 2) + 0.2) 
#make room for the rotated labels
barplot(phylum_metazoa, 
        col='steelblue', 
        main="Metazoan Phyla", 
        xlab=NULL, #ingen x-akse titel
        las=2, #roterer aksetitler
        ylab="Number of species",
        ylim=c(0,130), 
        cex.main=1.5,
        cex.names=0.9, 
        cex.axis=0.8, 
        cex.lab=1.3) 
#Gem som PNG fil ved at klikke på exportknappen ude i plottvinduet 
#Vælg "Save as image"

#Fungi
Ascomycota <- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Ascomycota")
summary(Ascomycota)
Basidiomycota <- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Basidiomycota")

aa <- length(unique(Ascomycota$score.id))
bb <- length(unique(Basidiomycota$score.id))

phylum_fungi <- c(aa,bb)
names(phylum_fungi) <- c("Ascomycota",
                         "Basidiomycota")

par(mar = c(7, 4, 2, 2) + 0.2) 
#make room for the rotated labels
barplot(phylum_fungi, 
        col='steelblue', 
        main="Fungi Phyla", 
        xlab=NULL, #ingen x-akse titel
        las=2, #roterer aksetitler
        ylab="Number of species",
        ylim=c(0,60), 
        cex.main=1.5,
        cex.names=1, 
        cex.axis=0.8, 
        cex.lab=1.3)

#Viridiplantae

Chlorophyta<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Chlorophyta")
Streptophyta<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Streptophyta")
Prasinodermophyta<- filter(COSQ_All_MOTUs_classified_uniquetaxa_sheet2,phylum=="Prasinodermophyta")

aaa <- length(unique(Chlorophyta$score.id))
bbb <- length(unique(Streptophyta$score.id))
ccc <- length(unique(Prasinodermophyta$score.id))

phylum_viridiplantae <- c(aaa, bbb, ccc)
names(phylum_viridiplantae) <- c("Chlorophyta",
                         "Streptophyta",
                         "Prasinodermophyta")

par(mar = c(7, 5, 5, 5) + 0.2) 
#make room for the rotated labels
barplot(phylum_viridiplantae, 
        col='steelblue', 
        main="Viridiplantae Phyla", 
        xlab=NULL, #ingen x-akse titel
        las=2, #roterer aksetitler
        ylab="Number of species",
        ylim=c(0,25), 
        cex.main=1.5,
        cex.names=0.85, 
        cex.axis=0.8, 
        cex.lab=1.3)
