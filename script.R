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
    ungroup()%>%summarise(TotalPrenom=sum(Nombre,na.rm=T))
  round(TotalDecesPrenom$TotalPrenom,-2)
  
   Carte<-tmpbdd%>%
    left_join(DEPS,by=c("DDN"="INSEE_DEP"))%>%
    st_as_sf()%>%
    ggplot()+
    geom_sf(aes(fill=PartDuPrenomNesDansDep),colour="lightgray")+
    geom_sf(data=DEPS, fill=NA,aes(colour=INSEE_DEP==DepDeDeces))+
    scale_colour_manual("",values=c("lightgray","black"))+
    theme_void()+
    labs(title=str_wrap(paste0("Où sont nées les personnes prénommées ",str_to_title(PrenomS)," décédées ",unique(tmpbdd$LieuDep) ," entre 2019 et 2021?"),55),
         subtitle=str_wrap(paste0("Part des ",sum(tmpbdd$Nombre)," personnes décédées ",unique(tmpbdd$LieuDep), " selon leur département de naissance."),65),  caption=str_wrap("Données Insee (Fichier des personnes décédées), calculs & carte V.Alexandre @humeursdevictor",60))+
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

########## print
#Serie 1 bis
Donnes<-readRDS("data/PrenomsDonnes2000_2020_F.Rdata")
PrenomsPrincipauxDecedes<-readRDS("data/PrenomsDecedes2000_2020_F.Rdata")
Decedes<-readRDS("data/PrenomsDecedes2000_2020.Rdata")
#PrenomsPrincipauxDecedes<-Decedes%>%
#  group_by(Sexe,Prenom)%>%
#  summarise(Nombre=sum(Nombre,na.rm=T))%>%arrange(desc(Nombre))


SHaz<-sample(1:2,1)
PHaz<-sample(PrenomsPrincipauxDecedes$Prenom[PrenomsPrincipauxDecedes$Sexe==SHaz & PrenomsPrincipauxDecedes$Prenom %in% Donnes$preusuel],1)

sortunecartedesnaissancesetdecesparprenom<-function(SHaz,PHaz){
  print(SHaz)
print(PHaz)
 BDDDCD<-Decedes%>%filter(Prenom==PHaz)%>%filter(Sexe==SHaz)%>%mutate(an=as.numeric(an))

Enfants<-Donnes%>%filter(sexe==SHaz)%>%
  filter(preusuel==PHaz)%>%mutate(annais=as.numeric(annais))%>%
  mutate(nombre=nombre-nombre-nombre)

Graph<-BDDDCD%>%ggplot()+
  geom_segment(aes(x=0,xend=Nombre,y=an,yend=an),stat="identity",colour="#727272")+
  geom_point(aes(x=Nombre,y=an),colour="#727272")+
  geom_text(aes(x=Nombre,y=an,label=ifelse(an%in%c(2000,2020),Nombre,"")),colour="#727272",hjust=-1)+
  geom_segment(data=Enfants,aes(x=0,xend=nombre,y=annais,yend=annais),stat="identity",colour=ifelse(SHaz==1,"#0F82BE","#CC2828"))+
  geom_point(data=Enfants,aes(x=nombre,y=annais),colour=ifelse(SHaz==1,"#0F82BE","#CC2828"))+
  geom_text(data=Enfants,aes(x=nombre,y=annais,label=ifelse(annais %in% c(2000,2020),abs(nombre),"")),colour=ifelse(SHaz==1,"#0F82BE","#CC2828"),hjust=2)+
  theme_minimal()+scale_y_reverse()+scale_x_continuous(labels=abs,
                                                       limits=c(ifelse(min(Enfants$nombre,na.rm=T)< -10,min(Enfants$nombre,na.rm=T),-12),
                                                                ifelse(max(BDDDCD$Nombre,na.rm=T)> 10,max(BDDDCD$Nombre,na.rm=T),12)))+
  labs(title=str_wrap(paste0("Des ",str_to_title(PHaz)," qui naissent et des ",str_to_title(PHaz)," qui meurent"),60),
      subtitle=str_wrap(paste0("Entre 2000 et 2020, ",sum(BDDDCD$Nombre), " ",str_to_title(PHaz)," sont ",ifelse(SHaz==1,"décédés en France ","décédées en France "),  " tandis que ", abs(sum(Enfants$nombre))," enfants recevaient ce prénom sur cette même période."),70),  caption=str_wrap("Insee (F. des personnes décédées, F. des Prénoms), calculs & viz V.Alexandre @humeursdevictor",60),
     x="",y="")+
  annotate(geom="text",x=ifelse(min(Enfants$nombre,na.rm=T)< -10,min(Enfants$nombre,na.rm=T),-10), y=1999,label="Naissances",colour=ifelse(SHaz==1,"#0F82BE","#CC2828"),hjust=0)+
  annotate(geom="text",x=ifelse(max(BDDDCD$Nombre,na.rm=T)> 10,max(BDDDCD$Nombre,na.rm=T),10),y=1999,label="Décès",colour="#727272",hjust=1)+
  geom_vline(xintercept = 0,colour="#141E28")+
  theme(plot.title.position = "plot",panel.grid.minor = element_blank(),panel.grid.major = element_blank(),text=element_text(family = "Corbel",size=12))
NomGraph<-paste0("img/",Sys.Date(),"_DecesNaissance_",PHaz,".png")
agg_png(NomGraph, width = 900, height = 900, res = 144)
plot(Graph)
invisible(dev.off())
  
   TW1<-paste0("Entre 2000 et 2020, ",sum(BDDDCD$Nombre), " ",str_to_title(PHaz)," sont ",ifelse(SHaz==1,"décédés en France ","décédées en France "),  " tandis que ", abs(sum(Enfants$nombre))," enfants recevaient ce prénom sur cette même période.")
   rtweet::post_tweet(status=TW1,media = NomGraph,token = tweetbot_token,media_alt_text = paste0("graph des ",str_to_title(PHaz)))
  
}
 

########## 2
#Serie 2 A: quelle est la carte des salariés du secteur XXXX?
#Serie 2 B: quelle est la carte des établissements du secteur XXXX?
    
couleurs<-c("[0,1]"="#FEE5D9", "(1,5]"="#FCBBA1", "(5,10]"="#FC9272", "(10,50]"="#FB6A4A","(50,100]"= "#EF3B2C",
            "(100,1e+03]"="#CB181D")
nomslabels<-c("[0,1]"="<= 0,1%", "(1,5]"="0,1% < 0,5%", "(5,10]"="0,5% < 1%", "(10,50]"="1% < 5%","(50,100]"= "5% < 10%",
            "(100,1e+03]"=">= 10%")
apeplus5K<-readRDS("data/apeplus5K.Rdata")
BilanAPE_EPCI<-readRDS("data/BilanAPE_EPCI.Rdata")
PartJointureAPESF<-readRDS("data/PartEmploi1000EPCI.Rdata")
SimpleEPCIM<-readRDS("data/SimpleEPCIMetropole.Rdata")
JusteGrossesCommunesEPCI<-readRDS("data/GrandesCommunesEPCI.Rdata")
uniqueEPCIurssaf<-as.character(unique(PartJointureAPESF$code_epci))

CreeUneCarteDeLEmploiSalarieParEPCI<-function(codeAPE){
  proprecodeape<-unique(PartJointureAPESF$Libellé[PartJointureAPESF$ape==codeAPE])
  pourtitre<-str_replace_all(proprecodeape,pattern = "\\/","")
  Tout<-PartJointureAPESF%>%
    filter(ape==codeAPE)%>%
    arrange(desc(PartSecteurDansCommunePour1000))
  GrossesVilles<-Tout[1:3,]
  GrossesVilles$code_epci<-as.character(GrossesVilles$code_epci)
  GrossesVilles<-cbind(GrossesVilles%>%left_join(JusteGrossesCommunesEPCI,by=c("code_epci"="CODE_EPCI")),GrossesVilles%>%left_join(JusteGrossesCommunesEPCI,by=c("code_epci"="CODE_EPCI"))%>%st_as_sf()%>%st_coordinates())%>%
    mutate(XDEP=rep(136853,3),YDEP=c(6600000,6500000,6400000))%>%mutate(rang=row_number())
  TotalSal<-sum(BilanAPE_EPCI$NombreSalaries[BilanAPE_EPCI$ape==codeAPE])
  
  CarteAPE<-PartJointureAPESF%>%
         filter(ape==codeAPE)%>%
         left_join(SimpleEPCIM%>%filter(CODE_EPCI%in%uniqueEPCIurssaf),by=c("code_epci"="CODE_EPCI"))%>%st_as_sf()%>%
         ggplot() +
         geom_sf(data=SimpleEPCIM,fill="white",colour="#72727250")+
         geom_sf(aes(fill = cut(PartSecteurDansCommunePour1000,breaks=c(0,1,5,10,50,100,1000),
                                include.lowest=T)),colour="#72727250") +
         geom_curve(data=GrossesVilles,aes(x=XDEP+10000,xend=X,  y=YDEP, yend=Y),
                      arrow=arrow(length = unit(0.02, "npc")),
                    curvature=0.2,colour="#727272")+
         geom_label(data=GrossesVilles,aes(x=XDEP,y=YDEP,label=str_wrap(paste0(rang,". ",NOM_COM,": ",round(PartSecteurDansCommunePour1000/10,1),"%"),30)),hjust=0)+
         scale_fill_manual("Part des emplois du secteur", values=couleurs,labels=nomslabels) +
         theme_void()+guides(fill = guide_legend(nrow=2))+
         labs(title = str_wrap(paste0("Où se trouvent les ",TotalSal, " salariés du secteur ",proprecodeape," ?"),60),
                 subtitle = "Zones dans lesquelles travaillaient les salariés en 2020.",
              caption="Données URSSAF. Carte V.Alexandre @humeursdevictor")+
         theme(legend.position = "top",text=element_text(family = "Corbel",size=12))

   agg_png(paste0("img/",Sys.Date(),"_EPCI_Salaries ",pourtitre,".png"), width = 900, height = 900, res = 144)
  plot(CarteAPE)
  invisible(dev.off())
  
 TW1<-paste0("En 2020, ",TotalSal, " salariés travaillaient dans le secteur «", proprecodeape ,"».")
   rtweet::post_tweet(status=TW1,media =  paste0("img/",Sys.Date(),"_EPCI_Salaries ",pourtitre,".png"),token = tweetbot_token,media_alt_text = paste0("graph des ",proprecodeape))
  
}
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
    filter(Titre%!in%c("Main Page","Pagina principale","Accueil","Hoofdpagina","Página_principal"))%>%
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

rtweet::post_tweet(status=Post1,media =  paste0("img/",Sys.Date(),"_wiki.png"),token = tweetbot_token,media_alt_text = "recherches wikipedia")
 
Post2<-paste0("🇬🇧 🇺🇸 Côté anglophone ?
1. ", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="en"],"
2. ", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="en"],"
3. ", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="en"])

reply_id <- rtweet::get_timeline(user = "Data_threads", n = 1, token = tweetbot_token)$id_str
rtweet::post_tweet(status = Post2, in_reply_to_status_id = reply_id)
  
Post3<-paste0("🇩🇪 Et Outre-Rhin ?
1. ", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="de"]," 
2. ", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="de"]," 
3. ", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="de"])
  
reply_id2 <- rtweet::get_timeline(user = "Data_threads", n = 1, token = tweetbot_token)$id_str
rtweet::post_tweet(status = Post3, in_reply_to_status_id = reply_id2)

Post4<-paste0("🇮🇹 Du côté du Wikipédia italien, le classement est le suivant:
1. ", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="it"],"
2. ", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="it"]," 
3. ", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="it"])

reply_id3 <- rtweet::get_timeline(user = "Data_threads", n = 1, token = tweetbot_token)$id_str
rtweet::post_tweet(status = Post4, in_reply_to_status_id = reply_id3)

Post5<-paste0("🇪🇸 La curiosité des hispanophones s'est portée sur :
1.", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="es"]," 
2.", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="es"]," 
3.", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="es"])

reply_id4 <- rtweet::get_timeline(user = "Data_threads", n = 1, token = tweetbot_token)$id_str
rtweet::post_tweet(status = Post5, in_reply_to_status_id = reply_id4)
  
Post6<-paste0("🇵🇹 🇧🇷 Le Wikipédia portugais s'est intéressé aux sujets suivants :
1. ", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="pt"]," 
2. ", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="pt"]," 
3. ", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="pt"])

reply_id5 <- rtweet::get_timeline(user = "Data_threads", n = 1, token = tweetbot_token)$id_str
rtweet::post_tweet(status = Post6, in_reply_to_status_id = reply_id5)
  
Post7<-paste0("🇳🇱 Tandis que nos amis néerlandais cherchaient à en savoir plus sur :
1. ", DonneesEuro$Titre[DonneesEuro$rank==1 & DonneesEuro$langue=="nl"]," 
2. ", DonneesEuro$Titre[DonneesEuro$rank==2 & DonneesEuro$langue=="nl"]," 
3. ", DonneesEuro$Titre[DonneesEuro$rank==3 & DonneesEuro$langue=="nl"])
  
reply_id6 <- rtweet::get_timeline(user = "Data_threads", n = 1, token = tweetbot_token)$id_str
rtweet::post_tweet(status = Post7, in_reply_to_status_id = reply_id6)
}


aujourdhui <- Sys.Date()
maintenant<-substr(Sys.time(),12,13)

if(substr(aujourdhui,10,10)%in%c(1,4,7) & as.numeric(maintenant) >=9){
  sortunecartedesnaissancesetdecesparprenom(SHaz,PHaz)
} else if (substr(aujourdhui,10,10)%in%c(2,5,8)  & as.numeric(maintenant) >=9) {
  sortunecartedesdecesparprenom(sample(PrenomsDecedesParDepSexe$Prenom[PrenomsDecedesParDepSexe$Sexe==sample(1:2,1)],1),sample(DEPS$INSEE_DEP,1))
} else if (substr(aujourdhui,10,10)%in%c(3,6,9) & as.numeric(maintenant) >=9) {
  CreeUneCarteDeLEmploiSalarieParEPCI(sample(apeplus5K,1))
} else if ( as.numeric(maintenant) <9){
  sortlesposts(x)
}
