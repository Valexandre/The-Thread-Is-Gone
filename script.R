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
    theme_map()+
    labs(title=str_wrap(paste0("Où sont nées les personnes prénommées ",str_to_title(PrenomS)," décédées ",unique(tmpbdd$LieuDep) ," entre 2019 et 2021?"),40),
         subtitle=str_wrap(paste0("Part des ",sum(tmpbdd$Nombre)," personnes décédées ",unique(tmpbdd$LieuDep), " selon leur département de naissance."),65),  caption="Données Insee (Fichier des personnes décédées), calculs & carte V.Alexandre @humeursdevictor")+
    scale_fill_stepsn("", colours=CouleursBornes,breaks=c(0,2.5,5,7.5,10,20,30,50,100),limits=c(0,100),labels = function(x) {x}, show.limits = F,values = c(0/100,2.5/100,5/100,7.5/100,10/100,20/100,30/100,50/100,1))+
    guides(colour=FALSE,fill=guide_colorsteps(barwidth = 30, barheight = 2,even.steps = T))+
    theme(legend.position="top",plot.title.position = "plot",
          text=element_text(family = "Corbel",size=24))
  NomCarte<-paste0("img/",Sys.Date(),"_Carte_",PrenomS,"_",DepDeDeces,".png")
  agg_png(NomCarte, width = 900, height = 900, res = 144)
  plot(Carte)
  invisible(dev.off())
  
  TW1<-paste0("Entre 2019 et 2021, près de ",round(TotalDecesPrenom$TotalPrenom,-2)," personnes prénommées ",str_to_title(PrenomS)," sont mortes en France. ", sum(tmpbdd$Nombre)," ",str_to_title(PrenomS)," sont ",ifelse(tmpbdd$Sexe[1]=="1","décédés ","décédées "),tmpbdd$LieuDep[1],". ",Virg(round(tmpbdd$PartDuPrenomNesDansDep[1])),"% d'entre ", ifelse(tmpbdd$Sexe[1]=="1","eux y étaient nés.","elles y étaient nées."))
  
  rtweet::post_tweet(statut=TW1,media = NomCarte,
                     media_alt_text = paste0("Carte des ",str_to_title(PrenomS)," ",ifelse(tmpbdd$Sexe[1]=="1","décédés ","décédées "),tmpbdd$LieuDep[1]," entre 2019 et 2021."), token = tweetbot_token)
}
  
sortunecartedesdecesparprenom(sample(PrenomsDecedesParDepSexe$Prenom[PrenomsDecedesParDepSexe$Sexe==sample(1:2,1)],1),sample(DEPS$INSEE_DEP,1))

###########
#Serie 2 A: quelle est la carte des salariés du secteur XXXX?
#Serie 2 B: quelle est la carte des établissements du secteur XXXX?

###########
