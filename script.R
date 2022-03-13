library(tidyverse)
library(sf)
library(rtweet)
library(sysfonts)
library(jsonlite)
library(ragg)
# On s'enregistre
tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token = Sys.getenv("T_ACCESS_TOKEN"),
  access_secret = Sys.getenv("T_ACCESS_SECRET")
)
rtweet::auth_as(tweetbot_token)
Virg <- function(x){ as.character( gsub("\\.",",",as.character(x)))}
`%!in%` <- function(x,y) !(x %in% y)


###########
#Serie 1 : où sont nés les XXXXX décédé.e.s dans un département entre 2015 et 2021 ?

DEPS<-st_read("https://raw.githubusercontent.com/Valexandre/france-geojson/master/Deps_PPC_DOMTOM_BAS_4326_V2.geojson")
DEPS<-DEPS%>%dplyr::filter(INSEE_DEP==INSEE_DEP2)%>%
  dplyr::filter(substr(INSEE_DEP,1,2)!=97)
DEPS$INSEE_DEP[DEPS$INSEE_DEP=="2B"]<-"2A"
NomsDep<- read_csv("data/TouslesLibellesDesDepartements.txt")
NomsDep$LieuDep[NomsDep$dep=="2A"]<-"en Corse"
CreeMoiUnGradient<-colorRampPalette(c("#005F89", "white"))
CouleursBornes<-rev(CreeMoiUnGradient(8))
PrenomsDecedesParDepSexe<-readRDS("data/PrenomsDecedesParDepSexe.Rdata")


sortunecartedesdecesparprenom<-function(PrenomS,DepDeDeces){
  tmpbdd<-PrenomsDecedesParDepSexe%>%
    filter(Prenom==PrenomS)%>%
    filter(DDD==DepDeDeces)%>%
    left_join(NomsDep,by=c("DDD"="dep"))%>%arrange(desc(PartDuPrenomNesDansDep))
  TotalDecesPrenom<-PrenomsDecedesParDepSexe%>%
    filter(Prenom==PrenomS)%>%
    filter(Sexe==tmpbdd$Sexe[1])%>%
    ungroup()%>%summarise(TotalPrenom=sum(Nombre))
  round(TotalDecesPrenom$TotalPrenom,-2)
  
   Carte<-tmpbdd%>%
    left_join(DEPS,by=c("DDN"="INSEE_DEP"))%>%
    st_as_sf()%>%
    ggplot()+
    geom_sf(aes(fill=PartDuPrenomNesDansDep),colour="lightgray")+
    geom_sf(data=DEPS, fill=NA,aes(colour=INSEE_DEP==DepDeDeces))+
    scale_colour_manual("",values=c("lightgray","black"))+
    theme_void()+
    labs(title=str_wrap(paste0("Où sont nées les personnes prénommées ",str_to_title(PrenomS)," décédées ",unique(tmpbdd$LieuDep) ," entre 2019 et 2021?"),60),
         subtitle=str_wrap(paste0("Part des ",sum(tmpbdd$Nombre)," personnes décédées ",unique(tmpbdd$LieuDep), " selon leur département de naissance."),85),  caption="Données Insee (Fichier des personnes décédées), calculs & carte V.Alexandre @humeursdevictor")+
    scale_fill_stepsn("", colours=CouleursBornes,breaks=c(0,2.5,5,7.5,10,50,100),limits=c(0,100),labels = function(x) {x}, show.limits = F,values = c(0/100,2.5/100,5/100,7.5/100,10/100,50/100,1))+
    guides(colour=FALSE,fill=guide_colorsteps(barwidth = 20, barheight = 1.5,even.steps = T))+
    theme(legend.position="top",plot.title.position = "plot",
          text=element_text(family = "Corbel",size=12))
  NomCarte<-paste0("img/",Sys.Date(),"_Carte_",PrenomS,"_",DepDeDeces,".png")
  agg_png(NomCarte, width = 900, height = 900, res = 144)
  plot(Carte)
  invisible(dev.off())
  
  TW1<-paste0("Entre 2019 et 2021, près de ",round(TotalDecesPrenom$TotalPrenom,-2)," personnes prénommées ",str_to_title(PrenomS)," sont mortes en France. ", sum(tmpbdd$Nombre)," ",str_to_title(PrenomS)," sont ",ifelse(tmpbdd$Sexe[1]=="1","décédés ","décédées "),tmpbdd$LieuDep[1],". ",Virg(round(tmpbdd$PartDuPrenomNesDansDep[1])),"% d'entre ", ifelse(tmpbdd$Sexe[1]=="1","eux y étaient nés.","elles y étaient nées."))
  
  rtweet::post_tweet(status=TW1,media = NomCarte,
                     media_alt_text = paste0("Carte des ",str_to_title(PrenomS)," ",ifelse(tmpbdd$Sexe[1]=="1","décédés ","décédées "),tmpbdd$LieuDep[1]," entre 2019 et 2021."), token = tweetbot_token)
}
  
#sortunecartedesdecesparprenom(sample(PrenomsDecedesParDepSexe$Prenom[PrenomsDecedesParDepSexe$Sexe==sample(1:2,1)],1),sample(DEPS$INSEE_DEP,1))

##########
#Serie 1 bis
Donnes<-readRDS("data/PrenomsDonnes2000_2020.Rdata")
Decedes<-readRDS("data/PrenomsDecedes2000_2020.Rdata")

PrenomsPrincipauxDecedes<-Decedes%>%
  group_by(Sexe,Prenom)%>%
  summarise(Nombre=sum(Nombre))%>%arrange(desc(Nombre))
PrenomsPrincipauxDecedes<-PrenomsPrincipauxDecedes%>%filter(Nombre>6)
Donnes<-Donnes%>%filter(preusuel%in%PrenomsPrincipauxDecedes$Prenom)

SHaz<-sample(1:2,1)
PHaz<-sample(PrenomsPrincipauxDecedes$Prenom[PrenomsPrincipauxDecedes$Sexe==SHaz],1)

sortunecartedesnaissancesetdecesparprenom<-function(SHaz,PHaz){
 BDDDCD<-Decedes%>%filter(Prenom==PHaz)%>%filter(Sexe==SHaz)%>%mutate(an=as.numeric(an))

Enfants<-Donnes%>%filter(sexe==SHaz)%>%
  filter(preusuel==PHaz)%>%mutate(annais=as.numeric(annais))%>%
  mutate(nombre=nombre-nombre-nombre)

Graph<-BDDDCD%>%ggplot()+
  geom_segment(aes(x=0,xend=Nombre,y=an,yend=an),stat="identity",colour="#727272")+
  geom_point(aes(x=Nombre,y=an),colour="#727272")+
  geom_text(aes(x=Nombre,y=an,label=Nombre),colour="#727272",hjust=-1)+
  geom_segment(data=Enfants,aes(x=0,xend=nombre,y=annais,yend=annais),stat="identity",colour=ifelse(SHaz==1,"#0F82BE","#CC2828"))+
  geom_point(data=Enfants,aes(x=nombre,y=annais),colour=ifelse(SHaz==1,"#0F82BE","#CC2828"))+
  geom_text(data=Enfants,aes(x=nombre,y=annais,label=abs(nombre)),colour=ifelse(SHaz==1,"#0F82BE","#CC2828"),hjust=2)+
  theme_minimal()+scale_y_reverse()+scale_x_continuous(labels=abs)+
  labs(title=str_wrap(paste0("Des ",str_to_title(PHaz)," qui naissent et des ",str_to_title(PHaz)," qui meurent"),60),
      subtitle=str_wrap(paste0("Entre 2000 et 2020, ",sum(BDDDCD$Nombre), " ",str_to_title(PHaz)," sont ",ifelse(SHaz==1,"décédés en France ","décédées en France "),  " tandis que ", abs(sum(Enfants$nombre))," enfants recevaient ce prénom sur cette même période."),70),  caption=str_wrap("Insee (F. des personnes décédées, F. des Prénoms), calculs & viz V.Alexandre @humeursdevictor",60),
     x="",y="")+
  annotate(geom="text",x=ifelse(is.infinite(min(Enfants$nombre)),-1,min(Enfants$nombre)),y=1999,label="Naissances",colour=ifelse(SHaz==1,"#0F82BE","#CC2828"),hjust="inward")+
  annotate(geom="text",x=max(BDDDCD$Nombre),y=1999,label="Décès",colour="#727272",hjust="inward")+
  geom_vline(xintercept = 0,colour="#141E28")+
  theme(plot.title.position = "plot",panel.grid.minor = element_blank(),panel.grid.major = element_blank(),text=element_text(family = "Corbel",size=12))
NomGraph<-paste0("img/",Sys.Date(),"_DecesNaissance_",PHaz,".png")
agg_png(NomGraph, width = 900, height = 900, res = 144)
plot(Graph)
invisible(dev.off())
  
   TW1<-paste0("Entre 2000 et 2020, ",sum(BDDDCD$Nombre), " ",str_to_title(PHaz)," sont ",ifelse(SHaz==1,"décédés en France ","décédées en France "),  " tandis que ", abs(sum(Enfants$nombre))," enfants recevaient ce prénom sur cette même période.")
   rtweet::post_tweet(status=TW1,media = NomGraph,token = tweetbot_token)
  
}
 
aujourdui <- Sys.Date()

if(substr(aujourdhui,10,10)%in%c(1,3,5,7,9)){
  sortunecartedesnaissancesetdecesparprenom(SHaz,PHaz)
} else if (substr(aujourdhui,10,10)%in%c(2,4,6,8,0)) {
  sortunecartedesdecesparprenom(sample(PrenomsDecedesParDepSexe$Prenom[PrenomsDecedesParDepSexe$Sexe==sample(1:2,1)],1),sample(DEPS$INSEE_DEP,1))
}

###########
#Serie 2 A: quelle est la carte des salariés du secteur XXXX?
#Serie 2 B: quelle est la carte des établissements du secteur XXXX?

###########
