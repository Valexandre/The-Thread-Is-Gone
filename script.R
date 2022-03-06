library(tidyverse)
library(rtweet)
library(jsonlite)
# On s'enregistre
tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token = Sys.getenv("T_ACCESS_TOKEN"),
  access_secret = Sys.getenv("T_ACCESS_SECRET")
)
rtweet::auth_as(tweetbot_token)
#######
#Tous les jours pluie et température à 7h et 16h à Paris

URLParis<-"http://www.infoclimat.fr/public-api/gfs/json?_ll=48.85341,2.3488&_auth=CRNeSVMtBiQDLlptBXNXfgBoAzYPeVN0An4AYwBlUy5VPlc2AWFWMFI8VCkBLlBmWHVSMVliVGQHbAF5CHoEZQljXjJTOAZhA2xaPwUqV3wALgNiDy9TdAJgAGMAalMuVTdXNQFjVipSPFQ%2BATVQelhqUjtZZFRzB3sBZwhjBGMJbF4%2FUzkGYANtWj8FPVd8ACwDZQ8yU2kCZABnAGVTOFU0VzcBMFZmUm5UMQE5UHpYaFI1WW9UbAdnAWYIZARlCXVeJVNJBhcDcVp4BXdXNgB1A34PZVM1AjU%3D&_c=8f5a43200f9e580178156325786c4183"
DonneesVille<-fromJSON(URLParis)
datefixee<-Sys.Date()
datefixee<-paste0(substr(datefixee,1,8),str_pad(as.numeric(substr(datefixee,9,10))+1,width = 2,pad = "0"))
matin<-paste0(datefixee," 07:00:00")
am<-paste0(datefixee," 16:00:00")
DonneesVilleU<-DonneesVille %>% 
  enframe %>%
  filter(name%in%c(matin,am))%>%
  unnest
Donnees<-tibble(quand=c("matin","après-midi"),
                tempk=c(DonneesVilleU[[2]][[1]][["sol"]],DonneesVilleU[[2]][[13]][["sol"]]),
                pluie=c(DonneesVilleU[[2]][[3]],DonneesVilleU[[2]][[15]] ))
Donnees<-Donnees%>%mutate(tempC=tempk-273.15)
rtweet::post_message(user = "humeursdevictor",token = tweetbot_token, text = paste0("Ce matin, température à 7h : ",Donnees$tempC[1],"°C, et ",Donnees$pluie[1],"mm de pluie sur 3h.
Cet après-midi : ",Donnees$tempC[2],"°C à 16h et ",Donnees$pluie[2],"mm de pluie"))



###########
#Serie 1 : où sont nés les XXXXX décédé.e.s dans un département entre 2015 et 2021 ?


###########
#Serie 2 A: quelle est la carte des salariés du secteur XXXX?
#Serie 2 B: quelle est la carte des établissements du secteur XXXX?

###########
