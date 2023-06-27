# 
# 
# install.packages("tidyverse")
# library(tidyverse)
# search()
# 
# install.packages("remotes")
# library(remotes)
# 
# install.packages("rtweet")
# library(rtweet)
# install.packages("tm")
# library(tm)
# install.packages("wordcloud")
# library(wordcloud)
# 
# install.packages("av")
# library(av)
 search()
# install.packages("rvest")
 library(rvest)
# 
# install.packages("magick")
 library(magick)
# 
# install.packages("jpeg")
library(jpeg)
# 
# install.packages("purrr")
library(purrr)

 #######################################################
 # API Key
 hOYS9D8Tjy4dnapYnurnQOzDC
 # API Key secret
 NCPRXyQea7ayyNyDdEK6Bear5mi92B0y2plffh0XnBTvbaujkb
 # access_token
 1653537211306967045-g6FT07rWR5wSnisZJSuSIEXYnoj1f6
 # access_token_secret
 ZMRv7ru9U00GpI2mXDBST0E9CAuZUnqI6jjfFcllxfhso
 
 # AUTHENTIFCATION
 
 consumer_key <- "hOYS9D8Tjy4dnapYnurnQOzDC"
 consumer_secret <- "NCPRXyQea7ayyNyDdEK6Bear5mi92B0y2plffh0XnBTvbaujkb"
 access_token <- "1653537211306967045-g6FT07rWR5wSnisZJSuSIEXYnoj1f6"
 access_secret <- "ZMRv7ru9U00GpI2mXDBST0E9CAuZUnqI6jjfFcllxfhso"
 
 setup_twitter_oauth(consumer_key,
                     consumer_secret,
                     access_token,
                     access_secret)
 
 # oubien
 token<-create_token(
   app = "1655175817675112449ThiamOmar77",consumer_key,
   consumer_secret,
   access_token,
   access_secret)
 
 #EXTRACTION
 
 library(twitteR)
 library(rtweet)
 search()
 list_macron <- searchTwitter("@EmmanuelMacron", n = 50)
 df_macron <- twListToDF(list_macron)
 
 
 
 # Récupération des tweets
 
 # Le flux de travail de base de cette méthode consiste à rechercher certains tweets
 # et à les ajouter à une base de données, puis à écrire la base de données dans un fichier.
 # ### Pour créer une liste des 3 200 tweets récents de Donald J. Trump :
 
 trumptweets<- userTimeline("gadogiovanni", n = 1)
 
 umptweets_df <- tbl_df(map_df(trumptweets, as.data.frame))
 write.csv(trumptweets_df, "trumptweets.csv")
 Obamatweets <- userTimeline("SaillieProfane", n = 320,includeRts=FALSE,
                             excludeReplies=FALSE)
 x<-searchTwitter("realDonaldTrump")
 ut <- userTimeline('barackobama', n=100)
 
 # Le fichier "trumptweets.csv" doit être généré dans le répertoire de travail et prêt à être visualisé et analysé.
 # Pour avoir une idée et un avant-goût de ce à quoi ressemble la trame de données de sortie :
 
 str(read.csv("trumptweets.csv"))
 
 
 # Pour créer une liste 3200 Tweets contenant un certain hashtag :
 daylightsavings <- searchTwitter("#daylightsavings exclude:retweets", n=3200)
 
 daylightsavings_df <- tbl_df(map_df(daylightsavings, as.data.frame))
 
 write.csv(daylightsavings_df, "daylightsavings.csv")
 
 
 
 ##########################
 # Extraction de données Twitter du debat d'entre deux tours avec R site:kaggle.com,
 
 # Récupérer 50 tweets mentionnant le compte officiel d’Emmanuel Macron et
 # les mettre sous forme de data frame :
 list_macron <- searchTwitter("@EmmanuelMacron", n = 50)
 df_macron <- twListToDF(list_macron)
 
 # Le nombre de tweets par minute pour chaque candidat
 # Mettre la date de création du tweet sous forme POSIXct
 df_macron$date <- format(df_macron$created, format="%Y-%m-%d")
 df_macron$date <- as.POSIXct(df_macron$date)
 # Histogramme du nombre de tweet par minute
 # En changeant binwidth, il est possible de modifier la largeur de l’intervalle, ici 1 minute.
 library(ggplot2)
 minutes <- 60
 ggplot(data=df_macron, aes(x=date)) +
   geom_histogram(aes(fill=..count..),binwidth= 1*minutes) +
   scale_x_datetime("Heure") +
   scale_y_continuous("Nombre de tweets")
 
 
 # Enlever les “mots vides” (exemple : le, la, du, etc…) + les noms des candidats
 library(tm)
 text_macron <- removeWords(text_macron,
                            c(stopwords("french"), stopwords("english"),
                              "est","marine","le pen", "debat"))
 # Mettre les mots sous formes de tableau de données et enlever tous les mots qui n’apparaissent pas au moins dans 1% des tweets (sparsity = 0.99)
 
 corpus_macron <- Corpus(VectorSource(text_macron))
 dtm_macron <- DocumentTermMatrix(corpus_macron)
 dtms_macron <- removeSparseTerms(dtm_macron, 0.99)
 sparseData_macron <- as.data.frame(as.matrix(dtms_macron))
 colnames(dtms_macron) <- make.names(colnames(dtms_macron))
 
 # Construction du nuage de mots (wordcloud)
 library(wordcloud)
 wordcloud(colnames(sparseData_macron), colSums(sparseData_macron),
           scale=c(2, 0.5), random.color=FALSE,
           colors = c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
           rot.per = 0.3)  # pourcentage de mots écrits à la verticale
 
 
 
 # Extraction des tweets:
 # Vous pouvez utiliser la fonction search_tweets() pour extraire des tweets en fonction de mots-clés, de comptes d'utilisateurs, de lieux, etc.
 
 tweets <- search_tweets("mot-clé",n = 100,  # Nombre de tweets à récupérer
                         include_rts = FALSE ) # Exclure les retweets,republication)
 # NB
 # Vous pouvez également utiliser d'autres fonctions comme get_timeline(),
 # get_favorites(), etc., pour extraire les tweets d'un utilisateur spécifique ou
 # les tweets favoris d'un utilisateur.
 
 
 
 # Manipulation et analyse des données:
 
 # Une fois que vous avez extrait les tweets, vous pouvez les manipuler et les analyser selon vos besoins.
 # Par exemple, vous pouvez extraire des informations telles que le texte du tweet, l'auteur, la date de création, le nombre de retweets, etc.
 
 # Accéder au texte des tweets
 tweet_texts <- tweets$text
 
 # Accéder à l'auteur des tweets
 tweet_authors <- tweets$screen_name
 
 # Accéder à la date de création des tweets
 tweet_dates <- tweets$created_at
 
 # Accéder au nombre de retweets des tweets
 tweet_retweets <- tweets$retweet_count
 
