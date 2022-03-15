#load data from github url
urlfile <- 'https://raw.githubusercontent.com/alicekeller/Phen_data/main/site_phenometrics_data.csv'
phen <- read.csv(urlfile)
head(phen)

#Has mean date of first breaking leaf buds changed over the course of 10 years
# in Colorado?
#What effect, if any, does elevation have on date of first bud break?
#Has the date of first bud break at a certain elevation become later?
#How does bud break vary by species at the same elevation?

#load packages
library(dplyr,
        ggplot2)
library(magrittr)
library(ggcorrplot)
library(GGally)
library(lubridate)
library(grid)
library(gtable)
library(gridExtra)
library(ggridges)
library(ggthemes)
library(showtext)

#load fonts
font_add_google("Smooch Sans", family = "smooch")
font_add_google("Permanent Marker", family = "permanent")
font_add_google("Josefin Sans", family = "josefin")
font_add_google("Noto Serif Display", family = "noto")
font_add_google("Nunito", family = "nunito")
font_add_google("Mulish", family = "mulish")
showtext_auto()

#remove unwanted columns
phen2 <- select(phen, -c(1, 3, 4, 5, 7, 11, 12, 14:16, 18, 20, 23:51, 65, 67))

#must change Phenophase_ID to factor variable, not integer
str(phen2)
phen2$Phenophase_ID <- as.factor(phen2$Phenophase_ID)
phen2$Site_ID <- as.factor(phen2$Site_ID)
phen2$Species_ID <- as.factor(phen2$Species_ID)
phen2$Species <- as.character(phen2$Species)
phen2$Genus <- as.character(phen2$Genus)
phen2$Phenophase_Description <- as.character(phen2$Phenophase_Description)
phen2$Species_Functional_Type <- as.character(phen2$Species_Functional_Type)
str(phen2)

#create new columns for average temps for each season
phen2 <- phen2 %>% 
  mutate(Tavg_Winter = rowMeans(cbind(Tmax_Winter, Tmin_Winter)),
         Tavg_Spring = rowMeans(cbind(Tmax_Spring, Tmin_Spring)),
         Tavg_Summer = rowMeans(cbind(Tmax_Summer, Tmin_Summer)),
         Tavg_Fall = rowMeans(cbind(Tmax_Fall, Tmin_Fall))
         )
head(phen2)

#rename mean daylength to be daylength in seconds and and mutate to get
#minutes of daylength
phen2 <- phen2 %>%
  rename(daylength_sec = Mean_Daylength) %>%
  mutate(daylength_min = daylength_sec/60)


#getting rid of missing values, which are reading as -9999
#must assign to NA first
phen2[phen2 < -1000] <- NA
summary(phen2)
colSums(is.na(phen2))

phen2 <- na.omit(phen2)
nrow(phen2)

#checking to make sure all variables are in right format
summary(phen2)
# must change site_id, species_id, year to factors

phen2$Site_ID <- as.factor(phen2$Site_ID)
phen2$Species_ID <- as.factor(phen2$Species_ID)
phen2$Mean_First_Yes_Year <- as.factor(phen2$Mean_First_Yes_Year)
str(phen2)

# move correlations up to this point

#correlation values matrix 
#res <- cor(phen2)
#round(res, 2)
#res

#correlation matrix
#p <- ggpairs(dat_indian2, progress=FALSE) 
# put scatterplots on top so y axis is vertical 
#p <- ggpairs(phen2, upper = list(continuous = "points") , 
#             lower = list(continuous = "cor") , 
#             progress=FALSE ) 
#print(p)  

#rename columns to be less cumbersome
phen2 <- phen2 %>%
  rename(Year = Mean_First_Yes_Year,
         DOY = Mean_First_Yes_DOY)
head(phen2)

#initial exploration plots
p1 <- ggplot(phen2, aes(DOY, daylength_min)) +
    geom_jitter(aes(color = Phenophase_ID), alpha = 0.5) +
    labs(title = "Daylength for each DOY", 
         x = "DOY", y = "Daylength (minutes)", color = "Phenophase ID")
p1

p2 <- ggplot(phen2, aes(Elevation_in_Meters, DOY)) +
    geom_jitter(aes(color = Phenophase_ID), alpha = 0.5)
p2

p3 <- ggplot(phen2, aes(Year)) +
    geom_bar()
p3

#TO DO NEXT: create subset for making new df with only breaking leaf bud
#data
leaf_bud <- subset(phen2, Phenophase_ID == 371 | Phenophase_ID == 373)

#omit points with DOY > 213 (end of July)
leaf_bud <- subset(leaf_bud, DOY < 213)

p4 <- ggplot(leaf_bud, aes(Year, DOY)) +
    geom_jitter(aes(color = Phenophase_ID), alpha = 0.5)
p4

#grid.arrange(p1, p2, p3, p4, nrow = 2)

p5 <- ggplot(leaf_bud, aes(DOY, Phenophase_ID)) +
    geom_jitter(aes(color = Year), alpha = 0.5)
p5

p5a <- ggplot(leaf_bud, aes(Species, DOY)) +
  geom_point(aes(color = Year), alpha = 0.5)
p5a


p6 <- ggplot(leaf_bud, aes(Elevation_in_Meters, DOY)) +
      geom_jitter(alpha = 0.5) +
      geom_smooth(method = lm)
p6

p7 <- ggplot(leaf_bud, aes(Year, DOY)) +
  geom_jitter(color = "tan4", alpha = 0.7) +
  geom_boxplot(fill = "olivedrab2", color = "black", alpha = 0.3) + 
  theme_minimal() +
  xlab("Year") + ylab("Day of bud break") +
  ggtitle("Day of leaf bud break over 10 years in Colorado")
p7

p7 <- p7 + theme(
  plot.title = element_text(color = "black", size = 20, face = "bold"),
  axis.title.x = element_text(color = "black", size = 14),
  axis.title.y = element_text(color = "black", size = 14)
)
p7

#new plot that shows average DOY of leaf bud break using ridgeline plot
p7s <- leaf_bud %>%
  ggplot(aes(x= DOY, y = Year, group = Year)) +
  geom_vline(xintercept = 79, size = 1, color = "tan4") +
  geom_text(x = 35, y = 11.7, label="First day \nof Spring", color = "tan4", family = "permanent", size = 6) +
  geom_segment(aes(x = 52, y = 11.5, xend = 76, yend = 11.3),
               arrow = arrow(length = unit(0.4, "cm")),
               color = "tan4") +
  geom_density_ridges(scale = 1.5, alpha = 0.7, fill = "olivedrab3", color = "black") +
  scale_y_discrete(limits = rev(levels(leaf_bud$Year))) +
  xlab("Day of Year") +
  labs(title = "When do leaves appear?",
       subtitle = "Distribution of leaf bud break over 10 years in Colorado",
       caption = "Plot: @alicemkeller | Data: USA National Phenology Network") +
  theme(axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),
        text = element_text(family = "mulish"),
        axis.title = element_text(color = "black", size = 28),
        axis.text = element_text(color = "black", size = 16),
        plot.title = element_text(hjust = 0.4, vjust = 1, size = 46, family = "noto", face = "bold", color = "black"),
        plot.subtitle = element_text(hjust = 0.4, vjust = 1.5, size = 24, family = "noto", color = "black"),
        plot.caption = element_text(size = 12),
        axis.ticks.y = element_blank())
p7s

#notice an earlier leaf appearance date over time, and a more bi-modal distribution -
#could have something to do with earlier seasonal warming, tricking plants
#into thinking it's spring when it's not yet

#color for see-through grey: "#0000001A"

#mean DOY leaf bud break for all data
mean_DOY <- mean(leaf_bud$DOY)

mean_by_year <- aggregate(leaf_bud$DOY, list(leaf_bud$Year), mean)
names(mean_by_year) <- c("Year", "Mean_DOY")

#aggregate by species to see mean DOY per species
mean_by_species <- aggregate(leaf_bud$DOY, list(leaf_bud$Genus, leaf_bud$Species),
                             mean)
names(mean_by_species) <- c("Genus","Species", "Mean_DOY")
by_species <- mean_by_species[order(mean_by_species$Genus),]

p7a <- ggplot(by_species, aes(Genus, Mean_DOY)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(title = "Mean bud break date by Genus", y = "Day of bud break") +
  my.theme
p7a

# make year read as date for plot
mean_by_year$Year <- as.Date(mean_by_year$Year,
                       format = "%Y")

#is there a way to only display year and no month/day?

p8 <- ggplot(mean_by_year, aes(Year, Mean_DOY)) +
  geom_point() +
  geom_smooth(method = lm)
p8

#fit linear model to the above plot so get r2 value
lm.DOY.year <- lm(Mean_DOY ~ Year, data = mean_by_year)
lm.DOY.year
summary(lm.DOY.year)
#anova(lm.DOY.year)

is.na(mean_by_year)
# if using plot and linear model below, must make sure Year is read as date

#leaf_bud$Year <- as.Date(leaf_bud$Year, format = "%Y")
p8b <- ggplot(leaf_bud, aes(Year, DOY)) + 
  geom_jitter() +
  geom_smooth(method = lm, aes(group = 1)) +
  annotate("text", label = "R-squared = 0.35", x = 4, y = 50, size = 8, colour = "black")
p8b

#fit linear model to day of leaf bud break by year
lm.DOY.year.2 <- lm(DOY ~ Year, data = leaf_bud)
lm.DOY.year.2 
summary(lm.DOY.year.2)


lm.DOY.elev <- lm(DOY ~ Elevation_in_Meters, data = leaf_bud)
#lm.DOY.elev.2 <- lm(log(DOY) ~ Elevation_in_Meters, data = leaf_bud)
lm.DOY.elev 
summary(lm.DOY.elev)

unseen <- data.frame(Elevation_in_Meters = 2500)

predict(lm.DOY.elev, unseen)
# predict tells that at an elevation of 2500 m, bud break should happen on day
# 125.31
# not very high confidence (r-squared is only 0.12)

#make plot with correlations of DOY and weather(temp, precip, daylength)
p9 <- ggplot(leaf_bud, aes(Mean_Accum_Prcp, DOY)) +
  geom_point() + 
  labs(x = "Accumulated precipitation (cm)") +
  theme_classic() +
  my.theme +
  theme(axis.title.y = element_blank())
p9

p10 <- ggplot(leaf_bud, aes(Mean_Accum_Prcp, DOY)) +
  geom_point(aes(color = Species), alpha = 0.5)
p10

p11 <- ggplot(leaf_bud, aes(daylength_min, DOY)) +
  geom_point() +
  labs(x = "Daylength (minutes)", y = "Day of bud break") +
  theme_classic() +
  my.theme
p11

p11a <- ggplot(leaf_bud, aes(Prcp_Spring, DOY)) +
  geom_point() + 
  labs(x = "Mean Spring Precipitation (cm)", y = "Day of bud break") +
  theme_classic() + 
  my.theme
p11a

p12 <- ggplot(leaf_bud, aes(Tavg_Spring, DOY)) +
  geom_point(aes(color = Year), alpha = 0.7) +
  labs(x = "Mean Spring Temperature (C)") +
  theme_classic() +
  my.theme +
  theme(axis.title.y = element_blank())
p12

grid.arrange(p11, p9, p11a, p12, nrow = 2)

ggplotly(p12)

#correlation of various factors
a <- leaf_bud$daylength_min
b <- leaf_bud$DOY
c <- leaf_bud$Mean_Accum_Prcp
d <- leaf_bud$Prcp_Spring
e <- leaf_bud$Tavg_Spring

cor(e, b)

#experiments with elevation
summary(leaf_bud$Elevation_in_Meters)

p13 <- ggplot(leaf_bud, aes(Elevation_in_Meters, fill = Species)) +
  geom_histogram(binwidth = 200, alpha = 0.8) + 
  labs(x = "Elevation (m)", y = "Count", title = "Bud break at each elevation") +
  theme_classic()
p13

p14 <- ggplot(leaf_bud, aes(Elevation_in_Meters)) +
  geom_histogram(aes(y =..density..), fill = "coral", color = "white", alpha = 0.7) +
  geom_density() +
  geom_rug() +
  labs(title = "Bud break at each elevation") + 
  theme_minimal()
p14

elevation1 <- filter(leaf_bud, Elevation_in_Meters > 1000 & 
                       Elevation_in_Meters < 1500)
elevation2 <- filter(leaf_bud, Elevation_in_Meters > 1500 & 
                       Elevation_in_Meters < 2000)
elevation3 <- filter(leaf_bud, Elevation_in_Meters > 2000 & 
                      Elevation_in_Meters < 2500)
elevation4 <- filter(leaf_bud, Elevation_in_Meters > 2500 &
                       Elevation_in_Meters < 3000)

p15 <- ggplot(elevation1, aes(Elevation_in_Meters, DOY)) +
  geom_point(aes(color = Year))
p15
meanelevation1 <- mean(elevation1$DOY)

p16 <- ggplot(elevation2, aes(Elevation_in_Meters, DOY)) +
  geom_point(aes(color = Year))
p16
meanelevation2 <- mean(elevation2$DOY)
summary(elevation2)

# breaking down leaf bud data by year for t-tests (see p7)
budbreak2009 <- filter(leaf_bud, Year == 2009)
budbreak2010 <- filter(leaf_bud, Year == 2010)
budbreak2016 <- filter(leaf_bud, Year == 2016)
budbreak2017 <- filter(leaf_bud, Year == 2017)
budbreak2019 <- filter(leaf_bud, Year == 2019)
t.test(budbreak2009$DOY, budbreak2016$DOY)

meanWtemp.2009 <- mean(budbreak2009$Tavg_Winter)
meanSptemp.2009 <- mean(budbreak2009$Tavg_Spring)

meanWtemp.2019 <- mean(budbreak2019$Tavg_Winter)
meanSptemp.2019 <- mean(budbreak2019$Tavg_Spring)




