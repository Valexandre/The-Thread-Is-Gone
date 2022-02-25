library(rtweet)
# On s'enregistre
tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token = Sys.getenv("T_ACCESS_TOKEN"),
  access_secret = Sys.getenv("T_ACCESS_SECRET")
)
rtweet::auth_as(tweetbot_token)
rtweet::post_tweet(status = "Test",token=tweetbot_token)


post_message(text = "coucou",user = "humeursdevictor",token = tweetbot_token)
