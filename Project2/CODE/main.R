matches = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/deliveries.csv")
matches = as.data.frame(mdf)
deliveries = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/matches.csv")
deliveries
matches<-matches[matches$result=="normal",]



#c = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/baller.csv")

library(ggplot2)

#####

#RANKING BAllERS
b = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/baller.csv")
b['WKTS/MATCHES'] = b['WKTS']/b['MATCHES']
b['ECONOMY'] = b['RUNS']/(b['BALLS']/6)

#DEFINING weights for model

w1 = 5
w2 = -3

b['Score'] = w1*b['WKTS/MATCHES'] + w2*b['ECONOMY']
b['Score'] = b['Score'] + min(b$Score)*(-1)
b= b[order(b$Score,decreasing = TRUE),]

#TOP 10 ballers

b[1:10,]

#RANKING BATSMAN

bt = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/batsman.csv")
bt = bt[,c("PLAYER","INN","RUNS","AVG","SR","X4S", "X6S")]
bt['RUN/INN'] = bt['RUNS']/bt['INN']



#DEFINING weights for batsman

w1 = 7
w2 = 4
w3 = 5
w4 = 1
bt['Score'] = w1*bt$`RUN/INN` + w2*bt$X4S +w3*bt$X6S + w4*bt$SR
bt= bt[order(bt$Score,decreasing = TRUE),]


cd = data.frame(bt[1:10,])
cd

# plotting top 10 batsman


ggplot(cd,aes(x =factor( PLAYER),y=RUNS))+ylab("RUNS") + geom_bar(stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 players runs")

d = matches = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/deliveries.csv")
matches = as.data.frame(mdf)
deliveries = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/matches.csv")
deliveries
matches<-matches[matches$result=="normal",]



#c = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/baller.csv")

library(ggplot2)

#####

#RANKING BAllERS
b = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/baller.csv")
b
b['WKTS/MATCHES'] = b['WKTS']/b['MATCHES']
b['ECONOMY'] = b['RUNS']/(b['BALLS']/6)

#DEFINING weights for model

w1 = 5
w2 = -3

b['Score'] = w1*b['WKTS/MATCHES'] + w2*b['ECONOMY']
b['Score'] = b['Score'] + min(b$Score)*(-1)
b= b[order(b$Score,decreasing = TRUE),]

#TOP 10 ballers

b

#RANKING BATSMAN

bt = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/batsman.csv")
bt = bt[,c("PLAYER","INN","RUNS","AVG","SR","X4S", "X6S")]
bt
bt['RUN/INN'] = bt['RUNS']/bt['INN']
bt




#DEFINING weights for batsman

w1 = 7
w2 = 4
w3 = 5
w4 = 1
bt['Score'] = w1*bt$`RUN/INN` + w2*bt$X4S +w3*bt$X6S + w4*bt$SR
bt= bt[order(bt$Score,decreasing = TRUE),]



cd = data.frame(bt[1:10,])
cd

# plotting top 10 batsman


ggplot(cd,aes(x =factor( PLAYER),y=RUNS))+ylab("RUNS") + geom_bar(stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 players runs")
barplot(cd$RUNS,names.arg=cd$PLAYER,xlab='player',ylab='runs',las=2)


matches = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/deliveries.csv")
matches = as.data.frame(mdf)
deliveries = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/matches.csv")
deliveries
matches<-matches[matches$result=="normal",]



#c = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/baller.csv")

library(ggplot2)

#####

#RANKING BAllERS
b = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/baller.csv")
b['WKTS/MATCHES'] = b['WKTS']/b['MATCHES']
b['ECONOMY'] = b['RUNS']/(b['BALLS']/6)

#DEFINING weights for model

w1 = 5
w2 = -3

b['Score'] = w1*b['WKTS/MATCHES'] + w2*b['ECONOMY']
b['Score'] = b['Score'] + min(b$Score)*(-1)
b= b[order(b$Score,decreasing = TRUE),]

#TOP 10 ballers

b[1:10,]

#RANKING BATSMAN

bt = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/batsman.csv")
bt = bt[,c("PLAYER","INN","RUNS","AVG","SR","X4S", "X6S")]
bt['RUN/INN'] = bt['RUNS']/bt['INN']





#DEFINING weights for batsman

w1 = 7
w2 = 4
w3 = 5
w4 = 1
bt['Score'] = w1*bt$`RUN/INN` + w2*bt$X4S +w3*bt$X6S + w4*bt$SR
bt= bt[order(bt$Score,decreasing = TRUE),]


cd = data.frame(bt[1:10,])
cd

# plotting top 10 batsman


ggplot(cd,aes(x =factor( PLAYER),y=RUNS))+ylab("RUNS") + geom_bar(stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Top 10 players runs")

d = read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/deliveries.csv")
d
players_runs = d[,c('match_id','batsman','batsman_runs')]
players_runs

players = unique(players_runs$batsman)
match_id = unique(d$match_id)

batman = c('DA Warner','KL Rahul',	'S Dhawan',	'J Bairstow',	'SS Iyer',	'AD Russell',	'Q de Kock',	'HH Pandya',	'AB de Villiers','RR Pant')

View(d)
s = c()
rs = c()
for(match in match_id){
df = d[which(d$match_id == match),c('batsman','batsman_runs')]
player_name = c()
runs = c()
for(p in unique(df$batsman)){
  
  if(p %in% batman){
  player_name = c(player_name,p)
  runs =c(runs , sum(df[which(df$batsman==p),'batsman_runs']))}
  print(match)
}
  s = c(s,player_name )
  rs = c(rs,runs)
}
dfo = data.frame(s,rs)
dfo
total_run = c()
j = 1
for (pr in batman)
{
  
  run  = c()
for(i in rownames(dfo)){
  if(dfo[i,1]== pr){
      run=c(run,dfo[i,2])
  }
  
  }
  total_run[[j]] = run
  j = j+1
}
batman
total_run
dfd = data.frame(matrix(ncol = 10,nrow=10))
dfd
colnames(dfd) <- batman
dfd
for(i in rownames(dfd)){
  i= as.numeric(i)
  for(j in 1:10){
  dfd[i,j] = total_run[[j]][i]
  }}
dfd
check = dfd
check
write.csv(check, file = "/home/samroadie/Desktop/DA_Lab/LAB2/top10player.csv", row.names = c('match1','match2','match3','match4','match5','match6','match7','match8','match9','match10'))



########  loading data of top 10 players and their individual 10 match score ######
ind_ply <- read.csv("/home/samroadie/Desktop/DA_Lab/LAB2/topplayer.csv")
summary(ind_ply)
#BOX PLOT OF TOP MOST PLAYER DAVID WARNER
boxplot(ind_ply$DA.Warner,xlab = "DAVID WARNER",ylab="RUNS")

#Coefficient of variance of various players
sd(ind_ply$DA.Warner)/mean(ind_ply$DA.Warner)*100
sd(ind_ply$KL.Rahul)/mean(ind_ply$KL.Rahul)*100
sd(ind_ply$S.Dhawan)/mean(ind_ply$S.Dhawan)*100
sd(ind_ply$J.Bairstow)/mean(ind_ply$J.Bairstow)*100
sd(ind_ply$SS.Iyer)/mean(ind_ply$SS.Iyer)*100
sd(ind_ply$AD.Rusell)/mean(ind_ply$AD.Rusell)*100
sd(ind_ply$Q.de.Kock)/mean(ind_ply$Q.de.Kock)*100
sd(ind_ply$HH.Pandya)/mean(ind_ply$HH.Pandya)*100
sd(ind_ply$AB.de.Villiers)/mean(ind_ply$AB.de.Villiers)*100
sd(ind_ply$RR.Pant)/mean(ind_ply$RR.Pant)*100


cd
######## corelation between RUNS and AVG of various players  ############
cor.test(cd$AVG, cd$RUNS,  method = "pearson")

######## corelation between two players  ############

cor.test(ind_ply$DA.Warner, ind_ply$S.Dhawan ,method = "spear")

library(tabulizer)
library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(tidyr)

matches <- read.csv("/home/samroadie/Desktop/Link to Clg/Sem 6/Data Analytics/DA LAB/Lab 2/match.csv", stringsAsFactors = FALSE)

data <- read.csv("/home/samroadie/Desktop/Link to Clg/Sem 6/Data Analytics/DA LAB/Lab 2/data.csv", stringsAsFactors = FALSE)

matches <- matches[,-18]
data$wickets <- as.numeric(ifelse(data$player_dismissed =="" ,"",1))

#number of matches in the dataset
summarize(matches,no_of_matches = n())
# Output = 60

#which Team won by maximum runs
max_run <- matches[which.max(matches$win_by_runs),]
select(max_run, winner, win_by_runs)
# Sunrisers Hyderabad by 118 runs

#which Team won by maximum runs
max_run <- matches[which.max(matches$win_by_wickets),]
select(max_run, winner, win_by_wickets)
# Sunrisers Hyderabad by 9 wicket

########################### Teams and matches won
matches%>%
  group_by(winner)%>%
  summarize(most_win = n())%>%
  ggplot(aes(x = winner,y = most_win,fill = winner))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous("Matches won")

##########################################################################################################
teams <- data %>% select(batting_team)%>%
  distinct()
teams <- rename(teams, team = batting_team) 
teams
s_team <- c("RCB","CSK","SRH","KKR","DC","MI","KXIP","RR")
teams <- cbind(teams, s_team)
player_of_match <- matches%>% select(id,player_of_match,season) %>%
  distinct()
player_of_match <- rename(player_of_match, player=player_of_match)

matches$city <- as.character(matches$city)
matches$city[matches$city==""] <- "Dubai"
venue_city <- matches %>%
  select(city)%>%
  distinct()



########################## Dissmissal type and number of dismissal ###################################
dismissal <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  filter(dismissal_kind!="")%>%
  group_by(season,dismissal_kind,s_team)%>%
  summarize(wickets =n())
ggplot(dismissal,aes(x=dismissal_kind,y=wickets,colour=as.factor(season), fill=as.factor(season)))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="wickets")+
  scale_x_discrete(name="dismissal kind")+
  ggtitle("Breakdown of dismissal type ")


######################## Run scored in 1s to 7s
runs_cat <- data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(s_team,batsman_runs)%>%
  summarize(no=n(),runs=sum(total_runs))

runs_cat$batsman_runs <- as.factor(runs_cat$batsman_runs)

ggplot(runs_cat,aes(x=s_team,y=runs,colour=batsman_runs,fill=batsman_runs))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="Runs")+
  scale_x_discrete(name="Teams")+
  ggtitle("Total runs scored in 1s to 7s")


################## toss decision of toss winner
wins_1 <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner,toss_decision)%>%
  group_by(s_team,toss_decision)%>%
  summarize(wins=n())


ggplot(wins_1,aes(x=s_team,y=wins,colour=toss_decision,fill=toss_decision))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss decision")+
  scale_x_discrete(name="Toss winners and toss decisions")+
  ggtitle("Toss decisions by each Team")

######Strike rate of all batsman

Bat_sr<- data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(batsman)%>%
  summarize(balls=n(),runs=sum(batsman_runs))%>%
  mutate(sr=runs*100/balls)%>%
  arrange(desc(sr))%>%
  mutate(sr_grp=ifelse(sr<100,"100",ifelse(sr<150,"100-150","150+")))%>%
  mutate(player_lab=ifelse(batsman=="AD Russell","AD Russell",ifelse(batsman=="V Sehwag","V Sehwag",ifelse(batsman=="V Kohli","V Kohli",ifelse(batsman=="CH Gayle","CH Gayle","")))))



ggplot(Bat_sr,aes(x=sr,y=runs,colour=sr_grp,fill=sr_grp,size=runs))+
  geom_jitter(show.legend = TRUE,alpha=.75)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  geom_text(aes(label=player_lab,hjust=-.25, colour="red"))+
  scale_y_continuous(name="Runs")+
  scale_x_continuous(name="strike rate")+
  ggtitle("strike rate for batsman   ")

#######################Economy rate for all bowlers

ball_sr<- data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  group_by(bowler)%>%
  summarize(balls=n(),runs=sum(total_runs,na.rm=TRUE))

ball_wk <-data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("bowling_team"="team"))%>%
  filter(dismissal_kind!="run out")%>%
  group_by(bowler)%>%
  summarize(wickets=sum(wickets,na.rm=TRUE))

ball_sr <-ball_sr%>%
  left_join(ball_wk,by=c("bowler"="bowler"))%>%
  mutate(sr=runs/wickets)%>%
  mutate(er=runs/(balls/6))%>%
  arrange(desc(sr))%>%
  mutate(sr_grp=ifelse(sr<10,"10",ifelse(sr<40,"11-40","41+")))%>%
  mutate(er_grp=ifelse(er<6,"6",ifelse(er<10,"6-10","11+")))%>%
  mutate(player_l=ifelse(bowler=="SL Malinga","SL Malinga",ifelse(bowler=="DJ Bravo","DJ Bravo",ifelse(bowler=="R Ashwin","R Ashwin",ifelse(bowler=="DW Steyn","DW Steyn","")))))


ggplot(ball_sr,aes(x=er,y=runs,colour=er_grp,fill=er_grp,size=runs))+
  geom_jitter(show.legend = TRUE,alpha=.75)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  geom_text(aes(label=player_l,hjust=-.25, colour="red"))+
  scale_y_continuous(name="Runs")+
  scale_x_continuous(name="Economy rate ")+
  ggtitle("Economy rate for bowlers  ")


#######################Number of Toss and Match wins by each team

toss <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())

toss$type <- "toss"

wins <-matches%>%
  left_join(teams,by=c("winner"="team") )%>%
  select(s_team,winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())

wins$type <- "wins"

toss_w <- rbind(toss,wins)

toss_w <- toss_w %>%
  group_by(s_team, type)%>%
  summarize(wins=sum(wins))


ggplot(toss_w,aes(x=s_team,y=wins,colour=type,fill=type))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss and Match Wins")+
  scale_x_discrete(name="Toss and Match winner")+
  ggtitle("Toss and Match wins by each Team")



       


       