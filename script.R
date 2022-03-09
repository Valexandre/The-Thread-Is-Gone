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
rtweet::post_tweet(status="retest")
###########
#Serie 1 : où sont nés les XXXXX décédé.e.s dans un département entre 2015 et 2021 ?


###########
#Serie 2 A: quelle est la carte des salariés du secteur XXXX?
#Serie 2 B: quelle est la carte des établissements du secteur XXXX?

###########
