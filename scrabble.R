

rm(list=ls())

#instal devtools package
install.packages("devtools")
#install the scrabblr package from github
devtools::install_github("jalapic/scrabblr")
# load the scrabblr package
library(scrabblr)


#instal DT package
install.packages("DT")
#library(DT)

#install.packages('Rcpp')
#library(Rcpp)

#install the DT package from github
#devtools::install_github("rstudio/DT")

DT::datatable(turns[,1:11], rownames = FALSE )

tiles
turns[1:6,c(5,7,21:24)]
turns[1:6,c(5,7,12:20)]

#final board matrices
finalboards[[17]]

#represent the location of bonus squares
notation

#make the board readable
board_clean(finalboards[[999]])

#putting dots in the empty spaces
board_dots(finalboards[[1999]])

#board state in a game
boards[[1807]][[9]]

#play triple-triple
turns[turns$gameid==1807 & turns$turn_game==9,c(4,5,7:11)]

boards[[1807]][[10]]

#ANALYSIS-Discriptive analysis
library(tidyverse)

#points scored in each game
turns %>% 
  group_by(gameid, player) %>% 
  filter(turn_player==max(turn_player)) %>%
  filter(row_number()==max(row_number())) %>%
  select(gameid, player, score) -> final_scores

final_scores %>%
  group_by(player) %>%
  summarize(min=min(score), max=max(score), mean=mean(score), median=median(score), sd=sd(score))

#score plots
final_scores %>%
  spread(player,score) %>%
  ggplot(aes(Player1, Player2)) + 
  geom_point(alpha=.15) + 
  theme_classic() 

#win proportions
final_scores %>% 
  spread(player,score) %>%
  ungroup()%>%
  summarise(P1 = sum(Player1>Player2), P2 = sum(Player2>Player1), tie=sum(Player1==Player2), total = n())

#since Only 11 out of 2566 games were ties (0.4%). Test if this is a significant difference using a binomial test.
binom.test(1404,2555)

#distribution of player1 vs player2
final_scores %>%
  ggplot(aes(score, fill = player)) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("firebrick", "dodgerblue")) + 
  theme_classic()

#to find top 5 highest scoring games
final_scores %>%
  arrange(-score) %>%
  head(5)

#highest score is 731. Following code is for the board of 731
gameid_maxscore <- as.numeric(final_scores[final_scores$score==max(final_scores$score),'gameid'])
finalboards[[422]]

#players1 moves during the above game
turns %>%
  filter(gameid==gameid_maxscore & player=="Player1") %>%
  select(position, rack, play, word,leave,points,score)

#best combined scores of p1 and p2
final_scores %>%
  spread(player,score) %>%
  mutate(total = Player1 + Player2) %>%
  arrange(-total) 

#plot of combined scores of p1 and p2
final_scores %>%
  spread(player,score) %>%
  mutate(total = Player1 + Player2) %>%
  ggplot(aes(total)) + 
  geom_histogram(bins=60, fill='gray88',color='black') + 
  theme_classic()

#points won on avg play. (consider plays when there are still 7 tiles on a player's rack)
turns %>% 
  filter(tiles_rack==7) %>%
  ggplot(aes(points)) + 
  geom_histogram(bins=60, fill='gray88',color='black') + 
  theme_classic()

#testing the binomial distribution by 'diptest' package
diptest::dip.test(turns %>% filter(tiles_rack==7) %>% .$points)

#non bingo distribution
ggplot(turns %>% filter(tiles_rack==7 & tiles_played<7), aes(points)) + geom_histogram(bins=60, fill='gray88',color='black') + theme_classic() + ggtitle("Non-Bingos") + xlim(0,250)

#bingo distribution
ggplot(turns %>% filter(tiles_rack==7 & tiles_played==7), aes(points)) + geom_histogram(bins=60, fill='gray88',color='black') + theme_classic() + ggtitle("Bingos") + xlim(0,250)

#highest scoring individual plays
turns %>%
  arrange(-points) %>%
  select(1,2,4,5,7:10,16,22,21) %>%
  head(10) %>%
  DT::datatable(.)

#visualize the highest scoring play by looking at the boards
boards[[1954]][[7]]

#highest non bingo plays
turns %>%
  filter(tiles_played<7) %>%
  arrange(-points) %>%
  select(1,2,4,5,7:10,16,22,21) %>%
  head(10) %>%
  DT::datatable(.)

#second highest play
boards[[245]][[25]]

#win proportions of player1 and player2 when they played 0, 1 or 2 blanks
turns[grepl("[a-z]",turns$move),] %>%
  mutate(player = factor(player)) %>%
  group_by(gameid,player) %>%
  summarize(total = n()) %>%
  complete(player, fill = list(total = 0)) %>%
  full_join(final_scores) %>%
  group_by(gameid)  %>%
  mutate(result = ifelse(score==max(score) & score==min(score),"T",
                         ifelse(score==max(score) & score>min(score), "W","L")))  %>% 
  filter(!is.na(total)) %>% #one game neither player played a blank
  group_by(player,total) %>% 
  summarise(wins = sum(result=="W"), losses=sum(result=="L"), ties=sum(result=="T")) %>%
  group_by(player) %>%
  mutate(winpct = wins/(wins+losses+ties)) %>%
  ggplot(aes(total,winpct,fill=player)) +
  geom_bar(position='dodge', stat='identity') + 
  theme_classic() + 
  scale_fill_manual(values=c("gray30", 'gray68')) +
  scale_x_continuous(breaks=0:2) + 
  xlab("Blanks Played") +
  ylab("Win Percentage")

#which tiles are played as blanks
blanks <- function(x) { x[grepl("[a-z]", x)] }
bls <- lapply(finalboards,blanks)

table(unlist(bls))

#length of words played
turns %>% filter(position!='end') %>% ggplot(aes(word_length)) + geom_bar() + theme_classic() + scale_x_continuous(breaks=0:12) + ggtitle("Word Length")

#tiles played
turns %>% filter(position!='end') %>% ggplot(aes(tiles_played)) + geom_bar() + theme_classic() + scale_x_continuous(breaks=0:7) + ggtitle("Tiles Played")

#proportion of exchanges or passes compared to regular plays 
turns %>%
  filter(position!='end') %>%
  summarise(pass = sprintf("%.4f", (sum(position=='pas') / n())), 
            exchange = sprintf("%.4f", (sum(position=='xch') / n()) )
  )

#understanding when exchanges occur
turns %>%
  filter(position=='xch') %>%
  summarize(mean = mean(turn_player),
            median = median(turn_player), 
            lqr=quantile(turn_player,.25), 
            uqr=quantile(turn_player,.75)
  )

#longest words played
table(turns$word_length)

#showing words of length 11 and 12
turns %>%
  arrange(-word_length) %>%
  filter(word_length>=11) %>%
  select(1,2,4,5,7:10,12,13) %>%
  head(10) %>%
  DT::datatable(.)

#most frequesntly played words
turns %>%
  filter(word_length>1) %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  arrange(-total) %>%
  mutate(pct = sprintf("%.1f", 100*total/2566)) %>%
  head(10)

#top 10 Q tiles played
head(qdf, 10)

#exponential curve CDF- more words known
qdf %>% 
  ggplot(aes(x=wordno, y=cum.pct)) + 
  geom_path() + 
  theme_classic() + 
  ggtitle("Cumulative Distribution of Q plays") +
  xlab("Unique Q Word") +
  ylab("Cumulative proportion of games")

#all words made on one board
finalboards[[60]]

#all words made on any board
board_words(finalboards[[60]])

# all power tiles QXJZKV
l <- lapply(finalboards, board_words)
l.u <- unlist(l)
qs <- l.u[grepl("Q", l.u)]
xs <- l.u[grepl("X", l.u)]
js <- l.u[grepl("J", l.u)]
zs <- l.u[grepl("Z", l.u)]
ks <- l.u[grepl("K", l.u)]
vs <- l.u[grepl("V", l.u)]

lapply(list(qs,xs,js,zs,ks,vs), 
       function(x) data.frame(table(as.character(x)))) %>%
  map(~ filter(., Var1!="")) %>%
  map(~ arrange(., -Freq)) %>%
  map(~ mutate(., 
               wordno = row_number(),
               pct = Freq/sum(Freq),
               cum.pct = cumsum(pct))) %>%
  Map(cbind, ., letter = c("Q","X","J","Z","K","V")) %>%
  do.call('rbind', .)  %>%
  ggplot(aes(x=wordno, y=cum.pct, color=letter)) + 
  geom_path() + 
  theme_classic() + 
  ggtitle("Cumulative Distribution of QXJZKV plays") +
  xlab("Unique Word") +
  ylab("Cumulative proportion of words played") 


#tile synergy
library(viridis)


# a function to get turn score for each pair of letters on rack
synergy <- function(s,tv){
  x <- combn(unlist(strsplit(s, split="")),2)
  v <- unique(apply(x,2,function(z) paste0(z[1],z[2])))
  out <- rep(tv, length(v))
  names(out)<-v
  return(out)
}

p  <- apply(turns[turns$tiles_rack==7,], 1, function(x) synergy(x[5], x[10]))

data.table::rbindlist(lapply(p, function(x) 
  data.frame(value=as.numeric(as.character(x)), 
             tiles=as.character(names(x))
  )
))  %>%
  group_by(tiles) %>%
  summarize(val = mean(value, na.rm=T)) %>%
  mutate(tiles = as.character(tiles),
         tile1 = substr(tiles,1,1),
         tile2 = substr(tiles,2,2)) -> df2

df2$tile1 <- factor(df2$tile1, levels=LETTERS[1:26])
df2$tile2 <- factor(df2$tile2, levels=LETTERS[26:1])

df2 %>%  
  filter(!grepl("\\?", tiles)) %>%
  ggplot(aes(tile1, tile2, fill = val)) + 
  geom_tile(colour="gray5", size=.75, stat="identity") + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis(guide = guide_legend(title = "Average Points Per Turn", title.position = "top"),
                     option="A",  limits=c(25,50)) +
  xlab("") + 
  ylab("") +
  ggtitle("Scrabble Tile Synergy") +
  theme(
    plot.title = element_text(color="black",hjust=0,vjust=2.5, size=rel(1.7)),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="black", size=rel(1.3)),
    axis.text.y  = element_text(hjust=1),
    legend.text = element_text(color="black", size=rel(1.3)),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom"
  )



# A function to calculate the frequency of times each square is played on by 
# any letter, a blank or a specific letter.

board_list2df <- function(l, type="alpha"){
  
  lval <- lapply(l, function(x) sub("[^[:alpha:]]+", "", unlist(matrix(x))))
  
  if(type!="alpha" & type!="blank") {  lval <- lapply(lval, function(z) ifelse(z %in% type,z,"") )}
  if(type=="blank") {  lval <- lapply(lval, function(z) ifelse(grepl("[a-z]",z),z,"") )}
  
  lvaldf <- as.data.frame.matrix(do.call('cbind',lval), stringsAsFactors = F)
  
  
  vals <- apply(lvaldf,1,function(y) sum(grepl("[[:alpha:]]",y)))
  
  valsdf <- data.frame(var1= rep(LETTERS[1:15],each=15),
                       var2 = rep(1:15,15),
                       vals, stringsAsFactors = F)
  valsdf$vals1 <- valsdf$vals/ncol(lvaldf)
  
  
  
  return(list('raw_data'=lvaldf,'summary'=valsdf))
}


# Get results for different tiles and
# scale data relative to each board in a 0-1 range

rbind(
  board_list2df(finalboards)[[2]] %>% mutate(tiles="Any", vals2=(vals1-min(vals1)) / (max(vals1)-min(vals1))),
  board_list2df(finalboards, "blank")[[2]] %>% mutate(tiles="Blank", vals2=(vals1-min(vals1))/(max(vals1)-min(vals1))),
  board_list2df(finalboards, "Q")[[2]] %>% mutate(tiles="Q", vals2=(vals1-min(vals1))/(max(vals1)-min(vals1))),
  board_list2df(finalboards, "Z")[[2]] %>% mutate(tiles="Z", vals2=(vals1-min(vals1)) /(max(vals1)-min(vals1)))
) -> tilespace


ggplot(tilespace, aes(var1, var2, fill = vals2)) + 
  geom_tile(colour="gray5", size=.75, stat="identity") + 
  scale_fill_viridis(guide = guide_legend(title = "Relative Occupancy Likelihood", title.position = "top"),
                     option="A",direction=-1) +
  scale_y_reverse(breaks=1:15) +
  xlab("") + 
  ylab("") +
  ggtitle("Likely Board Position of Scrabble Tiles") +
  facet_wrap(~tiles) +
  theme(
    plot.title = element_text(color="black",hjust=0,vjust=3, size=rel(1.7)),
    plot.background = element_rect(fill="white"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 15),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="black", size=rel(1.2)),
    axis.text.y  = element_text(hjust=1),
    axis.text.x = element_text(margin=unit(c(-0.2,-0.2,-0.2,-0.2), "cm")),
    legend.text = element_text(color="black", size=rel(1.3)),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom"
  )