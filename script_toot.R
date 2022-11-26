library(tidyverse)
library(sf)
library(sysfonts)
library(jsonlite)
library(ragg)
library(rtoot)
# On s'enregistre


Virg <- function(x){ as.character( gsub("\\.",",",as.character(x)))}
`%!in%` <- function(x,y) !(x %in% y)

###########

#Série 4 : recherches sur wikipedia

library(jsonlite)
#PagesVuesWiki
jour<-gsub("-","/",Sys.Date()-1)
function(x,y) !(x %in% y)
SortLeTop3<-function(langue){
  PagesPlusVues<-fromJSON(paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/top/",langue,".wikipedia/all-access/",jour))[["items"]][["articles"]][[1]]
  PagesPlusVues<-PagesPlusVues[1:10,]
  PagesPlusVues<-PagesPlusVues[!grepl(":",PagesPlusVues$article),]
  PagesPlusVues<-PagesPlusVues%>%mutate(Titre=gsub("_"," ",article))
  PagesPlusVues<-PagesPlusVues%>%
    filter(Titre%!in%c("Main Page","Pagina principale","Accueil","Hoofdpagina","Página_principal","Cookie (informatique)","Cleopatra","Cleópatra","AMBEV","Google Traduction" ))%>%
    mutate(rank=row_number())%>%filter(!is.na(Titre))
  PagesPlusVues<-PagesPlusVues[1:3,1:4]
  PagesPlusVues%>%mutate(langue=langue,
                         lien=paste0("https://",langue,".wikipedia.org/wiki/",article))
}

sortlesposts<-function(x){
  x<-"rien"
DonneesEuro<-c("fr","en","de","it","es","pt","nl")%>%map_dfr(SortLeTop3)

PourGraph<-tibble(langue=c("fr","en","de","it","es","pt","nl"),
                  lat=c(47,52,50,43,43,43,52),
                  lon=c(-2,-8.5,11,10,0,-8.5,5.5))
PourGraph<-PourGraph%>%left_join(DonneesEuro%>%filter(rank==1))

agg_png(paste0("img/",Sys.Date(),"_wiki.png"), width = 900, height = 600, res = 144)
plot(PourGraph%>%
  ggplot()+
  #  geom_rect(xmin=-9,xmax=19,ymin=42,ymax=53,fill=NA,colour="#0F82BE")+
  geom_label(aes(lon,lat,label=str_wrap(paste0(langue,": ",Titre),16)),hjust=0,size=4,vjust=0,
             label.padding = unit(0.02,"npc"), label.r=unit(0.1,"npc"))+theme_void()+
    labs(title="Quelles ont été les pages les plus consultées sur Wikipédia hier ?",
         subtitle = paste0("A la date du ",substr(jour,9,10),"/",substr(jour,6,7),"/",substr(jour,1,4)),
                                       caption="Données Wikimedia. Viz V.Alexandre @humeursdevictor")+
         theme(text=element_text(family = "Corbel",size=12))+
  coord_cartesian(xlim=c(-9,19),ylim=c(42,55)))
invisible(dev.off())


Post1<-paste0("🇫🇷 Quelles ont été les pages les plus vues hier sur Wikipédia ? 
1. ", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="fr"],"
2. ", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="fr"],"
3. ", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="fr"],"

#WikipediaCuriosite")

rtoot::post_toot(status = Post1,media =  paste0("img/",Sys.Date(),"_wiki.png"), alt_text = "recherches wikipedia hier", 
                 token=Sys.getenv("RTOOT_DEFAULT_TOKEN"))
 
}

print("coucou")
aujourdhui <- Sys.Date()
maintenant<-substr(Sys.time(),12,13)

#if(substr(aujourdhui,10,10)%in%c(1,4,7) & as.numeric(maintenant) >=9){
#  sortunecartedesnaissancesetdecesparprenom(SHaz,PHaz)
#} else if (substr(aujourdhui,10,10)%in%c(2,5,8)  & as.numeric(maintenant) >=9) {
#  sortunecartedesdecesparprenom(sample(PrenomsDecedesParDepSexe$Prenom[PrenomsDecedesParDepSexe$Sexe==sample(1:2,1)],1),sample(DEPS$INSEE_DEP,1))
#} else if (substr(aujourdhui,10,10)%in%c(3,6,9) & as.numeric(maintenant) >=9) {
#  CreeUneCarteDeLEmploiSalarieParEPCI(sample(apeplus5K,1))
#} else if ( as.numeric(maintenant) <9){
  sortlesposts(x)
#}
