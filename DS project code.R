##Project code:
```{r}
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
```


# Create state_data

```{r}
party_data <- read_excel("party_data.xlsx")
cases_state <- read.csv("us-states.csv")

#merge by state
state_data <- merge(cases_state, party_data, by = "state", all.x = TRUE)

# create dataset for cases from May 1, 2020
state_data <- subset(state_data, date >= "2020-05-01")

```

```{r}
#change state to abreviations and add population 
pop <- data.frame(read.csv("covid_county_population_usafacts.csv"))

pop <- aggregate(pop$population, by=list(state=pop$State), FUN=sum, na.rm=TRUE)
names(pop)[2] <- "state_pop"

#combine with state data by state
state_data <- merge(state_data, pop, by = "state", all.x = TRUE)

#all NA in pct_chng column = 0 
state_data$pct_change[is.na(state_data$pct_change)] <- 0

#shorten percentage to 2 decimal places
state_data[,'pct_change'] <- format(round(state_data[,'pct_change'],2),nsmall=2)
```

```{r}
#add voting data
voting_data <- read_excel("2018 Voting Data.xlsx")
names(voting_data)
state_data <- merge(state_data, voting_data, by = "state", all.x = TRUE)
```
```
#create and add variable of daily change in cases per state 
state_data <- state_data %>% 
      group_by(state) %>%
      mutate(change_cases = cases - cases[date=="2020-05-01"])
 #Get the daily percent change in total cases since May 1st for each state
state_data <-  state_data %>%
        group_by(state) %>% 
        arrange(date, .by_group = TRUE) %>%
        mutate(pct_change = (cases/lag(cases) - 1) * 100)
```



# Create data_by_state

#Sum all state level data  
state_cases <- aggregate(state_data$cases, by=list(state=state_data$state), FUN=sum)
names(state_cases)[2]<- "Cases"
state_cases<- state_cases[order(state_cases$state),]

state_population <- aggregate(state_data$state_pop, by=list(state=state_data$state), FUN=mean)
names(state_population)[2]<- "Population"
state_population <- state_population[order(state_population$state),]

state_chr <- aggregate(deaths~ state+party, state_data, sum)
state_chr <- state_chr[order(state_chr$state),]

#Merge datasets 
data_by_state <- cbind(state_cases, state_population, state_chr)
data_by_state <- data_by_state[,c(1,2,4,6,7)]
data_by_state

#Rename columns
names(data_by_state)[1]<- "State"
names(data_by_state)[4]<- "Party"
names(data_by_state)[5]<- "Deaths"

#Format Dataset
data_by_state$Party[data_by_state$Party== "rep"]<- "Republican"
data_by_state$Party[data_by_state$Party== "dem"]<- "Democratic"
data_by_state

data_by_state$Party <- factor(data_by_state$Party, levels= c("Republican", "Democratic"))
data_by_state$State <- factor(data_by_state$State)
data_by_state$Population <- as.integer(data_by_state$Population)
data_by_state

#Account for population
data_by_state <- transform(data_by_state, new= Population/1000)
names(data_by_state)[6]<- "P"
data_by_state$P <- as.integer(data_by_state$P)

data_by_state <- transform(data_by_state, new= Deaths / P)
names(data_by_state)[7]<- "D"
data_by_state$D <- as.integer(data_by_state$D)

data_by_state <- transform(data_by_state, new= Cases / P)
names(data_by_state)[8]<- "C"
data_by_state$C <- as.integer(data_by_state$C)

names(data_by_state)[6]<- "Pop.per.thou"
names(data_by_state)[7]<- "Deaths.per.thou"
names(data_by_state)[8]<- "Cases.per.thou"

write_xlsx(data_by_state, "data_by_state.xlsx")
```
```{r}
#Add the average percentage change over time for each state 
data_by_state$avg_change <- aggregate(Cases.per.thou~State, data_by_state, function(x) mean(c(NA, diff(x)), na.rm = TRUE))
```

                                      
# Create Data (Josh's dataframe)
#dataframe and setups 
```{r, include = FALSE}
confirmed <- read.csv("~/covid_confirmed_usafacts.csv")
vote <- read.csv("~/voteinfo.csv")
population <- read.csv("~/covid_county_population_usafacts.csv")
deaths <- read.csv("~/covid_deaths_usafacts.csv")
states <- read.csv("~/states.csv")
```
 ```{r, warning= FALSE}
deaths <- deaths %>%
  mutate(Death =  X7.18.2020)

deaths <- deaths %>%
  group_by(State) %>%
  summarise(Death = sum(Death))

confirmed <- confirmed %>%
  mutate(Confirmed = X7.18.2020)

confirmed <- confirmed %>%
  group_by(State) %>%
  summarise(Confirmed = sum(Confirmed))

data <- left_join(deaths,confirmed, by = c("State"))

vote$Name <- vote$States 
vote <- left_join(vote, states, by = c("Name"))
vote$Vote <- vote$N
vote$Democratic.advantage <- vote$Democratic.advantage*0.01
vote <- vote %>%
  select(State, Vote, Classification, Democratic.advantage)

data <- left_join(data, vote, by = c("State"))

population <- population %>%
  group_by(State) %>%
  summarise(Population = sum(population))

data <- left_join(data, population, by = c("State"))
data$ConfirmedRate <- data$Confirmed/data$Population
data$DeathRate <- data$Death/data$Confirmed
data$Vote <- as.numeric(data$Vote)
data = data[!data$State == "DC",]
colnames(data)   
                                      
                                      
                                      
##Code for Siyu's Analysis
loadPkg("tidyr")
loadPkg("usmap")
loadPkg("readr")
loadPkg("ggplot2")
loadPkg("dplyr")
loadPkg("scales")
covid=read.csv("state_data_add.csv")
str(covid)
ks=function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }
df=covid %>%
  select(date,cases,deaths) %>%
  gather(key = "variable", value = "value", -date)
df$date=as.Date(df$date,format = "%m/%d/%y")
ggplot(df, aes(x = date, y = value)) + 
  geom_area(aes(color = variable,fill = variable), 
            alpha = 0.5, position = position_dodge(0.8))+ggtitle("Covid-19 data from 05/01/2020 to 07/18/2020 in US")+scale_color_manual(values = c("blue","red")) +scale_fill_manual(values = c("blue","red"))+scale_y_continuous(labels = ks)
```

```{r,results='markup'}
dfa=covid %>%
  select(date,cases,deaths,hospitalized,recovered,tests) %>%
  gather(key = "variable", value = "value", -date)
dfa$date=as.Date(dfa$date,format = "%m/%d/%y")
ggplot(dfa, aes(x = date, y = value)) + 
  geom_area(aes(color = variable,fill = variable), 
            alpha = 0.5, position = position_dodge(0.8))+ggtitle("Covid-19 data from 05/01/2020 to 07/18/2020 in US")+scale_color_manual(values = c("blue","red","green","black","orange")) +scale_fill_manual(values = c("blue","red","green","black","orange"))+scale_y_continuous(labels = ks)
```

```{r,,results='markup',warning=FALSE}
covid_cum=subset(covid,date=="7/18/20")
ggplot(covid_cum,aes(x=state,y=cases,color=state))+geom_point()+ggtitle("Confirmed Coronavirus Cases until 07/18/2020 in US")+geom_text(aes(label=state))+scale_y_continuous(labels = ks)
plot_usmap(data = covid_cum, labels=TRUE, values = "cases", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Confirmed Coronavirus Cases until 07/18/2020 in US", label = scales::comma
  ) + theme(legend.position = "right")
```
```{r,results='markup',warning=FALSE}
ggplot(covid_cum,aes(x=state,y=deaths,color=state))+geom_point()+ggtitle("Deaths from Coronavirus until 07/18/2020 in US")+geom_text(aes(label=state))+scale_y_continuous(labels = ks)
plot_usmap(data = covid_cum, labels=TRUE, values = "deaths", color = "red") +
  scale_fill_continuous(
    low = "white", high = "red", name = "Deaths from Coronavirus from 05/01/2020 to 07/18/2020 in US", label = scales::comma
  ) + theme(legend.position = "right")
```

```{r,echo=FALSE,warning=FALSE}
ggplot(covid_cum,aes(x=state,y=population,color=state))+geom_point()+ggtitle("Population of each state in US")+geom_text(aes(label=state))+scale_y_continuous(labels = ks)
plot_usmap(data=covid_cum, labels=TRUE, values="population",color="green")+
  scale_fill_continuous(
    low = "white", high = "green", name = "Population of each state in US", label = scales::comma
  ) + theme(legend.position = "right")
                                      
```{r,echo=FALSE,warning=FALSE}
plot_usmap(data=covid, labels=TRUE, values="party",color="red")
```
                                      
```{r,results='markup',warning=FALSE}
ggplot(covid_cum,aes(x=state,y=tests,color=state))+geom_point()+ggtitle("Number of tests of each state in US")+geom_text(aes(label=state))+scale_y_continuous(labels = ks)
plot_usmap(data=covid_cum, labels=TRUE, values="tests",color="orange")+
  scale_fill_continuous(
    low = "white", high = "orange", name = "Number of tests of each state in US", label = scales::comma
  ) + theme(legend.position = "right")
```

```{r,results='markup',warning=FALSE}
ggplot(covid_cum,aes(x=state,y=hospitalized,color=state))+geom_point()+ggtitle("Hospitalization in each states")+geom_text(aes(label=state))+scale_y_continuous(labels = ks)
```
                                      
```{r,results='markup',warning=FALSE}
covidny=subset(covid,state=="NY")
df1=covidny %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df1$date=as.Date(df1$date,format = "%m/%d/%y")
ggplot(df1, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in NY")
```

```{r,results='markup',warning=FALSE}
covidtx=subset(covid,state=="TX")
df2=covidtx %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df2$date=as.Date(df2$date,format = "%m/%d/%y")
ggplot(df2, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in TX")
```
```{r,results='markup',warning=FALSE}
covidfl=subset(covid,state=="FL")
df3=covidfl %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df3$date=as.Date(df3$date,format = "%m/%d/%y")
ggplot(df3, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in FL")
```

```{r,results='markup',warning=FALSE}
covidca=subset(covid,state=="CA")
df4=covidca %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df4$date=as.Date(df4$date,format = "%m/%d/%y")
ggplot(df4, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in CA")
```
                                      
```{r,results='markup',warning=FALSE}
covidil=subset(covid,state=="IL")
df5=covidil %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df5$date=as.Date(df5$date,format = "%m/%d/%y")
ggplot(df5, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in IL")
```

```{r,results='markup',warning=FALSE}
covidnj=subset(covid,state=="NJ")
df6=covidnj %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df6$date=as.Date(df6$date,format = "%m/%d/%y")
ggplot(df6, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in NJ")
```

```{r,results='markup',warning=FALSE}
covidga=subset(covid,state=="GA")
df7=covidga %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df7$date=as.Date(df7$date,format = "%m/%d/%y")
ggplot(df7, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in GA")
```

```{r,results='markup',warning=FALSE}
covidaz=subset(covid,state=="AZ")
df8=covidaz %>%
  select(date,cases, deaths,hospitalized,tests,recovered) %>%
  gather(key = "variable", value = "value", -date)
df8$date=as.Date(df8$date,format = "%m/%d/%y")
ggplot(df8, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("blue", "red","green","yellow","orange")) +
  theme_minimal()+scale_y_continuous(labels = ks)+ggtitle("Covid-19 data in AZ")
```

                                      
##Code for Ngodoo's Analysis
library(ggplot2)
 #boxplot
 ggplot(data_by_state, aes(x=Party, y=Avg_change, fill = Party, na.action())) + 
  geom_boxplot() + scale_fill_manual(values = c("blue", "red")) +labs(title="Plot of Avg change in Cases \n by Party",
        x ="Political Party", y = "Avg Change in Covid-19 Cases (per 1,000)") + theme( plot.title = element_text(hjust = 0.5,  size=14, face="bold")) 

#bar graph 
ggplot(data=data_by_state, aes(x=reorder(State, -Avg_change), y=Avg_change, fill=Party)) + ggtitle("Avg Change in Covid-19 Cases \n by State and Political Affiliation") +
  xlab("State") + ylab("Avg Change in Cases (per 1,000)") +
  geom_bar(stat="identity") + scale_fill_manual(values = c("blue", "red")) + theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5,  size=14, face="bold"))

                                      
#test assumptions:

# Are the data normal? 
qqnorm(data_by_state$Avg_change)  

#Shapiro-Wilk test for normality
with(data_by_state, shapiro.test(Avg_change[party == "rep"]))# p = 0.8837
with(data_by_state, shapiro.test(Avg_change[party == "dem"])) # p = 0.9927

#test homogeneity using f-test
var.test(Avg_change ~ Party, data = data_by_state) # p = 0.03929

#Two sample t-test results
results <- t.test(Avg_change~Party, data = data_by_state, alternative = "less")
results


                                      
                                      
                                      
                                      

##Code for Brooke's Analysis

#Scatterplot with regression lines
ggplot(data_by_state_clean, aes(x=Cases.per.thou, y=Deaths.per.thou, color=Party))+ geom_point()+labs(title="Scatterplot", subtitle = "Deaths vs Cases", x= "Cases (per 1,000)", y= "Deaths (per 1,000)") +geom_smooth(method = "lm" )+ scale_color_manual(values=c("red", "blue"))+ xlim(c(0,850))+ ylim(c(0,50))

#Correlation 
cor.test(rep_data1$Cases.per.thou, rep_data1$Deaths.per.thou, method = "pearson", conf.level = 0.95)
cor.test(dem_data1$Cases.per.thou, dem_data1$Deaths.per.thou, method = "pearson", conf.level = 0.95)

#Linear Model
rep_lm <- glm(formula= Cases.per.thou~Deaths.per.thou, data=rep_data1)
summary(rep_lm)
dem_lm <- glm(formula= Cases.per.thou~Deaths.per.thou , data=dem_data1)
summary(dem_lm)

# Confidence Interval 
confint(rep_lm)
confint(dem_lm)


                                      
                                      
                                      
                                      


#Code for Josh's Analysis                                  
                 
```{r}
#ggplot                                        
ggplot(data)+
  geom_point(aes(x = Democratic.advantage, y = ConfirmedRate))
ggplot(data)+
  geom_boxplot(aes(x = Classification, y = ConfirmedRate))
```
#linear relationship                                      
```{r}
data <- data %>%
  mutate(Inclination = ifelse(Democratic.advantage > 0, "Democratic",
                             ifelse(Democratic.advantage == 0, "Competitive", "Republican")))
ggplot(data)+
  geom_boxplot(aes(x = Inclination, y = ConfirmedRate))
```                                      
```{r}
model1 <- lm(ConfirmedRate ~ Democratic.advantage, data = data)
summary(model1)
```
@Natural logs                                      
```{r}
data <- data %>%
  mutate(logCR = log(ConfirmedRate),
         logC = log(Confirmed))

ggplot(aes(x = Democratic.advantage, y = logCR), data = data)+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  labs(x = "Democratic Advantage (%)",
       y = "Logged confirmed rate")

ggplot(aes(x = Democratic.advantage, y = logC), data = data)+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  theme_classic()+
  labs(x = "Democratic Advantage (%)",
       y = "Logged number of confirmed case")
```
#Poisson                                      
```{r}
model2 <- glm(Confirmed ~ Population + 
                Democratic.advantage, 
              family = "poisson", data = data)
summary(model2)
```
```{r}
model3 <- glm(Confirmed ~ Population + Democratic.advantage + 
                Classification, family = "poisson", data = data)
summary(model3)
```
#Anova                                      
```{r}
anova(model2, model3)
```  
```{r}
model4 <- glm(Confirmed ~ Population + Democratic.advantage + 
                Classification + Democratic.advantage:Classification, 
              family = "poisson", data = data)
anova(model3,model4)
```
