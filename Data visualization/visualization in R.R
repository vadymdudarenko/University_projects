###########################################################################
#				         Visualization in R
#                   World Cup 
#					          2022/2023
#			          Vadym Dudarenko 
#               Ismayil Ismayilov
#		University of Warsaw, Faculty of Economic Sciences                
###########################################################################

################################ LIBRARIES #####################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyr)
library(forcats)
library(fmsb)

################################################################################
######WORLD CUP 2022
df <- read.csv("/Users/smile/Desktop/Fifa_world_cup_matches.csv", sep = ',')

########################################
#### GENERAL DATA PREPARATION ##########
########################################

###CREATE A NEW COLUMN WITH FULL NAME OF MATCH
df$Match <- paste(df$team1, df$team2, sep = "-")

### WE DROP THE "%" SIGN
df$possession.team1 <- gsub('%', '', df$possession.team1)
df$possession.team2 <- gsub('%', '', df$possession.team2)
df$possession.in.contest <- gsub('%', '', df$possession.in.contest)

### Mutate to Numeric
df <- df %>% mutate(possession.team1 = as.numeric(as.character(possession.team1)))
df <- df %>% mutate(possession.team2 = as.numeric(as.character(possession.team2)))
df <- df %>% mutate(possession.in.contest = as.numeric(as.character(possession.in.contest)))

### Fix the Data
df$possession.team2[df$team1 == "SAUDI ARABIA"] <- 54
df$possession.team2[df$team1 == "POLAND" & df$team2 =="SAUDI ARABIA"] <- 56
df$possession.team2[df$team1 == "WALES" & df$team2 =="ENGLAND"] <- 57
df$possession.team2[df$team1 == "MOROCCO" & df$team2 =="CROATIA"] <- 56

########################################
#### GENERAL DATA PREPARATION ENDS ####
########################################


#####################################################
#### CHART 1: AVERAGE POSSESSION PER TEAM ##########
#### STYLE: CIRCULAR GRAPH #######################
#####################################################

###DATA PREPARATION
results <- data.frame(team = character(), possession_rate = numeric(), stringsAsFactors = FALSE)

# Get a list of unique team names
teams <- unique(c(df$team1, df$team2))

# Calculate the possession rate for each team
for (team in teams) {
  team_data <- df[df$team1 == team | df$team2 == team,]
  team_possession <- sum(team_data$possession.team1[team_data$team1 == team]) + sum(team_data$possession.team2[team_data$team2 == team])
  team_games <- sum(team_data$team1 == team) + sum(team_data$team2 == team)
  possession_rate <- team_possession / team_games
  results <- rbind(results, data.frame(team = team, possession_rate = possession_rate))
}

###GRAPH PREPARATION
###Circular Graph
empty_bar <- 10

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(results))
colnames(to_add) <- colnames(results)
data <- rbind(results, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$possession_rate <- round(label_data$possession_rate, 1)

#GRAPH
ggplot(data, aes(x=as.factor(id), y=possession_rate, fill=team)) +
  geom_bar(stat="identity") +
  labs(title = "Average Possession in World Cup 2022")+
  
  scale_fill_discrete(guide="none")+
  ylim(-50,100) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = 'darkred', 
                              size = 15,
                              face = 'bold',
                              hjust=0.5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),)+
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=possession_rate+25, label=team, hjust=hjust),color="black",
            fontface="bold",alpha=0.8, size=3, angle= label_data$angle, inherit.aes = FALSE )+
  geom_text(data=label_data, aes(label = possession_rate),size=3,fontface="bold", angle=label_data$angle, alpha=0.8)


#####################################################
#### CHART 2: POSSESSION OF THE EVERY MATCH ##########
#### STYLE: BARPLOT GEOM_BAR #######################
#####################################################
df_wide_pos <-  df %>% select(Match, possession.team1, possession.team2, possession.in.contest)
df_long_pos <- df_wide_pos %>% pivot_longer(cols=c('possession.team1', 'possession.team2', 'possession.in.contest'),
                                            names_to='Team',
                                            values_to='Possession')

ggplot(df_long_pos, aes(x = Match, y = Possession, group = desc(Team), label=Possession)) + 
  geom_bar(aes(fill= Team), stat="identity", position=position_fill()) +
  labs(x = "Matches",
       y = "Possession",
       title = "Possession of the every match in World Cup 2022") +
  geom_col(aes(fill = Team)) +
  geom_text(aes(label = Possession), position = position_stack(vjust = 0.5), size=3, angle=90, color='white') +
  theme_wsj() +
  scale_fill_wsj(labels=c('Contest', 'Home', 'Away')) +
  # scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "top",
        plot.title = element_text(color = '#301934', 
                                  size = 18,
                                  face = 'bold',
                                  hjust=0.5),
        axis.title.y = element_text(size = 14, color = 'darkred', face='bold'),
        axis.title.x = element_text(face = "bold", color='darkred'),
        axis.text.x = element_text(angle = 90, color='navyblue', size='8', hjust=0.95, vjust=0.5),
        axis.text.y = element_blank(),
        legend.title=element_blank())


#####################################################
#### CHART 3: GOALS IN EVERY KNOCKOUT MATCH ##########
#### STYLE: LOLLIPOP: CLEVELAND DOT PLOT #############
#####################################################
ggplot(df) +
  geom_segment(data=subset(df, category=='Round of 16' | 
                             category=='Semi-final' | 
                             category=='Quarter-final' |
                             category=='Final'), aes(x=Match, xend=Match, y=number.of.goals.team2, yend=number.of.goals.team1)) +
  geom_point(data=subset(df, category=='Round of 16' | 
                           category=='Semi-final' | 
                           category=='Quarter-final' |
                           category=='Final'), aes(x=Match, y=number.of.goals.team1, color="Home"), size=3 ) +
  geom_point(data=subset(df, category=='Round of 16' | 
                           category=='Semi-final' | 
                           category=='Quarter-final' |
                           category=='Final'), aes(x=Match, y=number.of.goals.team2,color="Away"), size=3 ) +
  coord_flip()+
  theme_wsj()+
  theme(legend.title=element_blank(),
        plot.title = element_text(color = '#301934', 
                                  size = 18,
                                  face = 'bold'))+
  labs(title='Knockout stage matches in World Cup 2022')+
  scale_y_continuous(breaks = seq(0, 10, by=1))

####FINAL THIRD ENTRIES OF ARGENTINA AND FRANCE
#### RADAR CHART

#################################################################################
#### CHART 4: FINAL THIRD ENTRIES OF ARGENTINA AND FRANCE DURING WC 2022 ##########
#### STYLE: RADAR CHART #######################################################
#################################################################################

#####DATA PREPARATION
#####ARGENTINA
df_arg <- data.frame(country = "Argentina")
df_arg$Left <- sum(df$left.channel.team1[df$team1 == 'ARGENTINA']) + 
  sum(df$left.channel.team2[df$team2 == 'ARGENTINA'])
df_arg$Left_Inside <- sum(df$left.inside.channel.team1[df$team1 == 'ARGENTINA']) + 
  sum(df$left.inside.channel.team2[df$team2 == 'ARGENTINA'])
df_arg$Central <- sum(df$central.channel.team1[df$team1 == 'ARGENTINA']) + 
  sum(df$central.channel.team2[df$team2 == 'ARGENTINA'])
df_arg$Right <- sum(df$right.channel.team1[df$team1 == 'ARGENTINA']) + 
  sum(df$right.channel.team2[df$team2 == 'ARGENTINA'])
df_arg$Right_Inside <- sum(df$right.inside.channel.team1[df$team1 == 'ARGENTINA']) + 
  sum(df$right.inside.channel.team2[df$team2 == 'ARGENTINA'])

#####FRANCE
france_left <- sum(df$left.channel.team1[df$team1 == 'FRANCE']) + 
  sum(df$left.channel.team2[df$team2 == 'FRANCE'])
france_left_inside <- sum(df$left.inside.channel.team1[df$team1 == 'FRANCE']) + 
  sum(df$left.inside.channel.team2[df$team2 == 'FRANCE'])
france_central <- sum(df$central.channel.team1[df$team1 == 'FRANCE']) + 
  sum(df$central.channel.team2[df$team2 == 'FRANCE'])
france_right <- sum(df$right.channel.team1[df$team1 == 'FRANCE']) + 
  sum(df$right.channel.team2[df$team2 == 'FRANCE'])
france_right_inside <- sum(df$right.inside.channel.team1[df$team1 == 'FRANCE']) + 
  sum(df$right.inside.channel.team2[df$team2 == 'FRANCE'])

df_arg <- bind_rows(df_arg, data.frame(country = "France"  ,"Left" = france_left,
                                       "Left_Inside" = france_left_inside,
                                       "Central" = france_central,
                                       "Right" = france_right,
                                       "Right_Inside" = france_right_inside))
df_arg <- df_arg[,-1]
rownames(df_arg) <- c("Argentina", "France")
df_arg <- rbind(rep(120,5) , rep(0,5) , df_arg)


#### COLORS
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

#### PLOT
radarchart( df_arg  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=1 , plty=4,
            #custom the grid
            cglcol= alpha("navyblue", 0.4), cglty=2, axislabcol="grey", caxislabels=seq(25,125,25),
            #custom labels
            vlcex=1,
            title=paste("Final Third Entries of Argentina & France in the All Matches"),
            vlabels=c("Left", "Left Inside", 
                      "Central", "Right", "Right Inside"),
            cex.main = 1.2)

#### LEGEND
legend(x=1, y=1.2, legend = rownames(df_arg[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1, pt.cex=3)

#### TEXT
mtext(side = 1, at = 0, cex = 0.8, "Definition: Final third entries shows where on the pitch a team
      develops its attacking play as they approach an opponent's goal. 
      It also exposes potential weaknesses in an opponent's defensive unit and structure.",
      col = 'black')

#################################################################################
#### CHART 5: Matches with the highest attendance in the World cup ##########
#### STYLE: Horizontal Bar CHART ################################################
#################################################################################

library(treemapify)
library(dplyr)
library(igraph)
library(visNetwork)
library(ggplot2)
library(plyr)
library(tidyverse)
library(rjson)
library(parallel)
library(DT)
library(lubridate)
library(gridExtra)

##filtering of top 10 attendance 
attendence <- WorldCupMatches%>%top_n(10,Attendance)%>%arrange(desc(Attendance))%>%
  select(Attendance, Year, Stadium, Datetime,City,Home.Team.Name, Away.Team.Name) 
##making labels and colors
text_inside_bar <- paste('stadium:', attendence$Stadium, ",", "Date:", attendence$Datetime)
number <- paste(attendence$Home.Team.Name, 'vs', attendence$Away.Team.Name)
cbbPalette <- c("#999999", "#FF0000FF","#AE4371", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
correct_order <- reorder(number, attendence$Attendance)

ggplot(data = attendence, aes(x = Attendance, y = correct_order), main="Mathes with the highest attendence") + 
  geom_bar(stat="identity", fill = cbbPalette, width = 0.9) +
  theme_minimal() +
  geom_text(aes(label = text_inside_bar),hjust = 0,position = position_fill(), size = 3.5)+
  ggtitle("Matches with the highest attendence") +
  xlab("Attendace") + ylab("Teams")+
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold"))


#################################################################################
#### CHART 6: Interactions between teams for year 1950 and 2010 ##########
#### STYLE: Network analysis ################################################
#################################################################################

connection<-WorldCupMatches %>% filter(Year == "1950")%>%select(Home.Team.Name, Away.Team.Name)
net <- graph.data.frame(connection, directed = F)
cnet <- cluster_edge_betweenness(net,
                                 edge.betweenness = TRUE, 
                                 merges = FALSE, 
                                 bridges = TRUE,
                                 modularity = TRUE) 
plot(cnet,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8)


#################################################################################
#### CHART 7: Total goals scored by year ######################################
########################## ################################################
#################################################################################


WorldCups<-WorldCups %>% add_row(Year = 2022, GoalsScored = 172, Winner = "Argentina")
WorldCups<-WorldCups %>% add_row(Year = 2018, GoalsScored = 169, Winner = "France")
textinside<- paste(WorldCups$GoalsScored)
Year1<- cut(WorldCups$Year, breaks = c(1928,1978,1980,1994,2022) ,dig.lab = 3)

ggplot(WorldCups,aes(y = GoalsScored, 
                   x = Year )) + geom_point(aes(color= Year1,size = GoalsScored))+
  scale_size(range = c(10, 20))+
  scale_colour_manual(values = c("red","blue","yellow"))+
  scale_x_continuous(breaks = seq(1930,2022,4))+
  scale_y_continuous(breaks = c(20,40,60,80,100,120,140,160,180))+
  ggtitle("Total goals scored by year") +
  xlab("Years") + ylab("Goals")+
  theme_minimal()+
  theme(plot.title = element_text(color = 'tomato4', 
                                  size = 20,
                                  face = 'bold.italic',
                                  hjust = 0.5),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"))+
        labs(color='Teams')+
  geom_text(aes(label = GoalsScored))+
  scale_colour_discrete(
    labels = c("16", "24", "32"))+
  guides(colour = guide_legend(override.aes = list(shape = 16, alpha = 1, size = 5)), size = FALSE)
   

#################################################################################
#### CHART 8: Teams with the most world cup victories ##########
#### STYLE: Treemap graph ################################################
#################################################################################  


WorldCups[WorldCups == "Germany FR"] <- "Germany"


Count <- WorldCups %>%
  group_by(Winner) %>%
  summarize(count_distinct = n_distinct(Country))

ggplot(Count, aes(area = count_distinct , fill =count_distinct,  label = Winner)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = F) + 
  labs(title = "Teams with the most world cup victories") +
  scale_fill_gradient(guide = guide_colorbar(barheight = 20))+
  theme(
    plot.title = element_text(color="black", size=20, face="bold.italic", hjust = 0.5))+
  labs(fill='Numbers')


#################################################################################
#### CHART 9: FIFA World Cup 18 Semi-Final(statistics of shots) ##########
########################## ################################################
###############################################################################

###POST function####
post <- function(fill_background = "white"){
  
  ggplot()+
    # Ground
    geom_line(aes(x = c(32, 48), y = c(0,0)))+
    # Post
    geom_rect(aes(xmin = 35.9, xmax = 44.1, ymin = 0, ymax = 2.75), fill = "#D3D3D3", color = "black")+
    geom_rect(aes(xmin = 36, xmax = 44, ymin = 0, ymax = 2.67), fill = fill_background, 
              color = "black", alpha = 0.7)+
    # Lines
    geom_line(aes(x = c(36, 36.3), y = c(2.67,2.58)), color = "gray")+ # Left Lines
    geom_line(aes(x = c(36.3, 36.7), y = c(2.58, 0.8)), color = "gray")+
    geom_line(aes(x = c(36.7, 36), y = c(0.8, 0)), color = "gray")+
    geom_line(aes(x = c(44, 43.7), y = c(2.67, 2.58)), color = "gray")+ # Right Lines
    geom_line(aes(x = c(43.7, 43.3), y = c(2.58, 0.8)), color = "gray")+
    geom_line(aes(x = c(43.3, 44), y = c(0.8, 0)), color = "gray")+
    geom_line(aes(x = c(36.7, 43.3), y = c(0.8,0.8)), color = "gray")+ # Ground Line
    # Theme
    theme(
      panel.background = element_rect(fill = fill_background),
      plot.background = element_rect(fill = fill_background),
      legend.background = element_rect(fill = fill_background),
      legend.key = element_rect(fill = fill_background,colour = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
      
    )
  
}


###import json file 
ENG_CR <- fromJSON(file= "8656.json")

# Shot Index
shot.index <- which(sapply(ENG_CR,function(x) x$type$name)=="Shot")

# Json to Data Frame
ENG_CR <- mclapply(ENG_CR[shot.index], function(x){ unlist(x)})

ENG_CR <- rbind.fill(mclapply(ENG_CR, 
                             function(x) do.call("data.frame", as.list(x))
))


ENG_CR <- ENG_CR %>% 
  filter(type.name == "Shot") %>% 
  select(-contains("freeze")) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(c(location1, location2, shot.end_location1, shot.end_location2, 
                   shot.end_location3, shot.statsbomb_xg)), funs(as.numeric))

ENG_CR <- ENG_CR %>% filter(shot.outcome.name %in% c('Goal','Post', 'Off T', 'Saved', 
                                                   'Saved to Post','Saved Off Target')) %>% 
  mutate(goal.seq = 1:length(shot.outcome.name))


post(fill_background = "#224C56")+
  geom_point(ENG_CR, mapping = aes(x = shot.end_location2, y = shot.end_location3, 
                                  color = shot.outcome.name), size = 7)+
  geom_text(ENG_CR, mapping = aes(x = shot.end_location2, y = shot.end_location3, label = goal.seq),
            size = 3, color = "darkslategray")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust=0.5, vjust = -5, size = 15),
    plot.subtitle =  element_text(hjust=0.5, vjust = -5),
    text = element_text(color = "white")
  )+
  labs(color = "Shot Outcome", title = "FIFA World Cup 18 Semi-Final", shape = NULL,
       subtitle = "England vs Croatia - 1:2 ")+
  scale_color_manual(values = c("green", "red", "blue", "orange"))+
  scale_shape_manual(values = c(16, 15, 17))+
  theme(plot.title = element_text(color = 'black', 
                                  size = 18,
                                  face = 'bold.italic',
                                  hjust = 0.5)
        )


