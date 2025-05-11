install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
install.packages("readr")
library("readr")
install.packages("tidyr")
library("tidyr")
install.packages("DT")
library("DT")

data <- read.csv("E:/RStudio/RStudio practice/sri-lanka-odi-analysis-upto-January-2023/sl_match_results_odi.csv")
View(data)
str(data)
data <- data %>%
  mutate(Result2 = case_when(
    Result == "won" ~ 1,
    Result == "lost" ~ 2,
    TRUE ~ 3
  ))

View(data)
plot(data)

#convert to factors
data <- data %>%
  mutate(Result2 = factor(Result2,
                          levels = c(1,2,3),
                          labels = c("Won", "Lost", "No Result")))

#summerize count & precentage
plot_data <-data %>%
  count(Result2) %>%
  mutate(Percent = n/sum(n)*100,
         Label = paste0(round(Percent,1),"%"))
View(plot_data)

#color
colorplot <- c("Won"="green","Lost"="red","No Result"="skyblue")


#plot 
ggplot(plot_data,aes(x=Result2,y=Percent,fill = Result2))+
  geom_col(width = 0.6)+
  geom_text(aes(label = Label), vjust=-0.5)+
  labs(title = "Overoll Match Results",
       x="Results",
       y="Percentage")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_fill_manual(values = colorplot)
theme_minimal()
View(plot_data)
View(data)

major_venues <- c("Colombo(PSS)", "Colombo(SSC)", "Colombo(RPS)",
                  "Kandy", "Moratuwa", "Galle", "Dambulla", "Pallekele", "Hambanthota")

data <- data %>%
  mutate(venue = if_else(Ground %in% major_venues, 1, 0))
View(data)

data <- data %>% select(-venue)
View(data)

#home & away recerntages
home <- c("Colombo (PSS)","Colombo (SSC)","Colombo (RPS)","Dambulla","Galle","Kandy","Moratuwa")

data <- data %>%
  mutate(location = if_else(Ground %in% home,"Home","Away"))

View(data)
#calc stat Home & away wins
lstat <- data %>%
  group_by(location) %>%
  summarise(Matches = n(),
            Wins = sum(Result2=="Won"),
            Losses = sum(Result2=="Lost"),
            NR = sum(Result2=="No Result"),
            Win_percentage = round(Wins/Matches*100,1))

View(lstat)

#plot lstat
ggplot(lstat,aes(x=location,y=Win_percentage,fill = location))+
  geom_col(width = 0.6)+
  geom_text(aes(label=paste0(Win_percentage,"%")),vjust=-0.5)+
  labs(title = "Win Percentge By Location",
       x="Match Venue",
       y="Win Percentage")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal()


#Get 1st Inn matches vs win & Lost percentage 
Batfirst <- data %>%
  filter(!Result2 %in% c("No Result")) %>%
  filter(Bat=="1st") %>%
  group_by(Result2) %>%
  summarise(count=n()) %>%
  mutate(Percentage = round(count/sum(count)*100,1),
         category = "Batting First")
View(Batfirst)

#2D pie chart
ggplot(Batfirst,aes(x="",y=count,fill=Result2))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(Percentage,"%")),
            position=position_stack(vjust=0.5))+
  labs(title=" 1st Inning ODI Result",
       fill="Result")+
  theme_void()+
  scale_fill_manual(values = c("Won"="lightgreen",
                               "Lost"="indianred1"))

#Get 2nd Inn matches vs win & Lost percentage 
Batsecond <- data %>%
  filter(!Result2 %in% c("No Result")) %>%
  filter(Bat=="2nd") %>%
  group_by(Result2) %>%
  summarise(count=n()) %>%
  mutate(Percentage = round(count/sum(count)*100,1),
         category = "Batting Second")
View(Batsecond)

#plot 3D pie chart
install.packages("plotrix")
library("plotrix")
par(mflow = c(1,2))
pie3D(Batsecond$count,
      labels = paste(Batsecond$Result2,"-",Batsecond$Percentage,"%"),
      labelcex = 1,
      explode = 0.2,
      main="2nd Inning ODI Results",
      col=c("firebrick1","dodgerblue","cornsilk"),
      theta = 1)

#compare between 1st & 2nd Innings winning rate
comwins <- data %>%
  filter(Result2=="Won") %>%
  filter(!Bat %in% c("-")) %>%
  group_by(Bat) %>%
  summarise(count=n()) %>%
  mutate(percentage =round(count/sum(count)*100,1))
View(comwins)

#plot bar chart
ggplot(comwins,aes(x=Bat,y=percentage,fill=Bat))+
  geom_col(width=0.5)+
  geom_text(aes(label = paste0(percentage,"%")),vjust=-0.5)+
  scale_x_discrete(labels=c("1"="Bat 1st","2"="Bat 2nd"))+
  labs(title = "Winning Percentage By Innings",
       x="Batting Order",
       y="Win Percentage")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal()

#calc win/loss by teams
team <- data %>%
  group_by(Opposition) %>%
  summarise(matches=n(),
            Wins=sum(Result2=="Won"),
            Losses=sum(Result2=="Lost"),
            NR =sum(Result2=="No Result"),
            win_percentage=round(Wins/matches*100,1))


team <-team %>%
  mutate(win_per_display=paste0(win_percentage,"%"))

View(team)
install.packages("scales")
library(scales)

#plot line Graph
ggplot(team,aes(x=reorder(Opposition, -win_percentage),y=win_percentage,group = 1))+
  geom_line(color="dodgerblue",linewidth = 1)+
  geom_point(color="gold2",size=3)+
  geom_text(aes(label=paste0(win_percentage,"%")),vjust=-0.8,size = 3)+
  labs(title = "Win Percentage By Opposition",
       x="Opposition",
       y="Win Percentage")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = function(x) paste0(x,"%"),limits = c(0,100))

# Calculate win/loss by decade
data <- data %>%
  mutate(
    Year = as.numeric(substr(Start.Date, nchar(Start.Date)-1, nchar(Start.Date))),
    Decade = case_when(
      Year >= 70 & Year <= 79 ~ "1970s",
      Year >= 80 & Year <= 89 ~ "1980s",
      Year >= 90 & Year <= 99 ~ "1990s",
      Year >= 0 & Year <= 9 ~ "2000s",
      Year >= 10 & Year <= 19 ~ "2010s",
      TRUE ~ "2020s"
    )
  )

View(data)

decade_stats <- data %>%
  group_by(Decade) %>%
  summarise(Matches = n(),
            Wins = sum(Result == "won"),
            Losses = sum(Result == "lost"),
            Win_Percentage = round(Wins/Matches * 100, 1))
View(data)

# Plot win percentage by decade
ggplot(decade_stats, aes(x = Decade, y = Win_Percentage)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  geom_text(aes(label = paste0(Win_Percentage, "%")), vjust = -0.5) +
  labs(title = "Sri Lanka ODI Win Percentage by Decade",
       x = "Decade", y = "Win Percentage") +
  theme_minimal()

#print data
print("Overall Match Results Summary:")
print(plot_data)

print("Home vs Away Win Stats:")
print(lstat)

print("Batting First - Win/Loss Distribution:")
print(Batfirst)

print("Batting Second - Win/Loss Distribution:")
print(Batsecond)

print("Comparison Between 1st & 2nd Innings Wins:")
print(comwins)

print("Win Percentage by Opposition:")
print(team)

















