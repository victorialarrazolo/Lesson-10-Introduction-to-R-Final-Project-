#Installing the package to be able to access my data.
install.packages("Ecdat")
library("Ecdat")
#Accessing the data from the head of the dataset
head(Cigarette)
View(Cigarette)

#Make a boxplot to show the average number of packs per capita per state
ggplot(Cigarette, aes(x=state, y=packpc)) + geom_boxplot()

#Find the median of packs per capita for each state
packpcmedian = Cigarette %>%group_by(year) %>% summarize(packpc = median(packpc))

#Make a plot to compare the means from the years 1985 to 1995
ggplot(packpcmedian) + geom_line(aes(x= year, y= packpc)) + ylab("Pack Per Capita Meadian")

#Make a scatterplot to compare the the price per packs and the number of packs per capita 
pricevppc = ggplot(Cigarette, aes(x= avgprs, y= packpc))
pricevppc + geom_point() + geom_smooth(method = lm) + ggtitle("Average Pack Price V Pack Per Capita") + xlab("Average Price Per Pack") + ylab("Packs Per Capita")

#See the correlation 
cor.test(Cigarette$packpc, Cigarette$avgprs , method="pearson" , use="complete.obs")

#Make the years different colors
pricevppc = ggplot(Cigarette, aes(x= avgprs, y= packpc, color=year))
pricevppc + geom_point() + geom_smooth(method = lm) + ggtitle("Average Pack Price V Pack Per Capita") + xlab("Average Price Per Pack") + ylab("Packs Per Capita")

#Make a linear regression 
regression = lm(avgprs~packpc, Cigarette)
summary(regression)

#Adjust the price for inflation 
adjustedprice = Cigarette %>% mutate(Inflation = avgprs/ cpi)
ggplot(adjustedprice, aes(x= Inflation, y= packpc)) + geom_point()


inflatedpricevppc = ggplot(adjustedprice, aes(x= Inflation, y= packpc, color= year))

inflatedpricevppc + geom_point()+ geom_smooth(method = lm) + ggtitle("Inflated Average Pack Price V Pack Per Capita") + xlab("Inflated Average Price Per Pack") + ylab("Packs Per Capita")

#Create a data frame with just the rows from 1985
data85 = Cigarette %>% filter(year == 1985)

#Create a data frame with just the rows from 1995
data95 = Cigarette %>% filter(year == 1995)

#Run a T test to compare the ppc from 1995 to the ppc from 1985
t.test(data95$packpc, data85$packpc, paired=TRUE)