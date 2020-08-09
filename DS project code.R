##Project code:




##Create state_data  



  #create dataset for cases from May 1, 2020
state_data <- subset(state_data, date >= "2020-05-01")

  #create variable of daily change in cases (May to mid-July) per state 
library(dplyr)
state_data <- state_data %>% 
      group_by(state) %>%
      mutate(change_cases = cases - cases[date=="2020-05-01"])
  #Get the daily percent change in total cases since May 1st for each state
state_data <-  state_data %>%
        group_by(state) %>% 
        arrange(date, .by_group = TRUE) %>%
        mutate(pct_change = (cases/lag(cases) - 1) * 100)

##Create data_by_state



  #Get the average percentage change over time for each state 
data_by_state$avg_change <- aggregate(Cases.per.thou~State, data_by_state, function(x) mean(c(NA, diff(x)), na.rm = TRUE))


##Code for Siyu's Analysis





                                      
                                      
                                      
##Code for Ngodoo's Analysis
library(ggplot2)
  #boxplot
ggplot(y, aes(x=party, y=avg_change, fill = party, na.action())) + 
  geom_boxplot() + scale_fill_manual(values = c("blue", "red")) +labs(title="Plot of Avg change in Cases \n by Party",
        x ="Political Party", y = "Avg Change in Covid-19 Cases (%)") + theme( plot.title = element_text(hjust = 0.5,  size=14, face="bold")) 

#bar graph 
ggplot(data=y, aes(x=reorder(state, -avg_change), y=avg_change, fill=party)) + ggtitle("Avg Change in Covid-19 Cases \n by State and Political Affiliation") +
  xlab("State") + ylab("Avg Change in Cases (%)") +
  geom_bar(stat="identity") + scale_fill_manual(values = c("blue", "red")) + theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5,  size=14, face="bold"))

#test assumptions:

# Are the data normal? 
qqnorm(y$avg_change)  

#Shapiro-Wilk test for normality
with(y, shapiro.test(avg_change[party == "rep"]))# p = 0.8837
with(y, shapiro.test(avg_change[party == "dem"])) # p = 0.9927

#test homogeneity using f-test
var.test(avg_change ~ party, data = y) # p = 0.03929

#Two sample t-test results
results <- t.test(avg_change~party, data = y, alternative = "less")
results



##Code for Brooke's Analysis






#Code for Josh's Analysis
#dataframe and setups 
```{r, include = FALSE}
library(dplyr)
library(ggplot2)
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
```                                    
#ggplot                   
```{r}
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
