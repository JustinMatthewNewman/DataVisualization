library(ggplot2)
library(cowplot)
library(gridGraphics)
library(grid)
library(gridExtra)
library(corrplot)
library(readxl)
library(mosaic)

finalData <- read_excel("Documents/Data/finalData.xlsx")
df = data.frame(finalData)

# ======================= SUBSETS ======================= #
allStudents = subset(df, df$Subgroup=="All Students")
allStudents = subset(allStudents, allStudents$X2020.2021.Pass.Rate!="<")
allStudents$X2020.2021.Pass.Rate = as.double(allStudents$X2020.2021.Pass.Rate)
allStudents$X2018.2019.Pass.Rate = as.double(allStudents$X2018.2019.Pass.Rate)
allStudents = subset(allStudents, allStudents$X2020.2021.Pass.Rate!="NA")
allStudents = subset(allStudents, allStudents$X2018.2019.Pass.Rate!="NA")
allStudents = subset(allStudents, allStudents$X2020.2021.Pass.Rate!=100)
allStudents = subset(allStudents, allStudents$X2018.2019.Pass.Rate!=0)
allStudents = subset(allStudents, allStudents$X2020.2021.Pass.Rate!=0)
allStudents = subset(allStudents, allStudents$X2018.2019.Pass.Rate!=100)
# ======================= SCIENCE ======================= #
science = subset(df, df$Subject=="Science")
science = subset(science, df$X2020.2021.Pass.Rate!="<")
science$X2020.2021.Pass.Rate = as.double(science$X2020.2021.Pass.Rate)
science$X2018.2019.Pass.Rate = as.double(science$X2018.2019.Pass.Rate)
science = subset(science, science$X2020.2021.Pass.Rate!="NA")
science = subset(science, science$X2018.2019.Pass.Rate!="NA")
science = subset(science, science$X2020.2021.Pass.Rate!=100)
science = subset(science, science$X2018.2019.Pass.Rate!=0)
science = subset(science, science$X2020.2021.Pass.Rate!=0)
science = subset(science, science$X2018.2019.Pass.Rate!=100)
# ======================= MATH ======================= #
math = subset(df, df$Subject=="Mathematics")
math = subset(math, math$X2020.2021.Pass.Rate!="<")
math$X2020.2021.Pass.Rate = as.double(math$X2020.2021.Pass.Rate)
math$X2018.2019.Pass.Rate = as.double(math$X2018.2019.Pass.Rate)
math = subset(math, math$X2020.2021.Pass.Rate!="NA")
math = subset(math, math$X2018.2019.Pass.Rate!="NA")
math = subset(math, math$X2020.2021.Pass.Rate!=100)
math = subset(math, math$X2018.2019.Pass.Rate!=0)
math = subset(math, math$X2020.2021.Pass.Rate!=0)
math = subset(math, math$X2018.2019.Pass.Rate!=100)
# ======================= READING ======================= #
reading = subset(df, df$Subject=="English: Reading")
reading = subset(reading, reading$X2020.2021.Pass.Rate!="<")
reading$X2020.2021.Pass.Rate = as.double(reading$X2020.2021.Pass.Rate)
reading$X2018.2019.Pass.Rate = as.double(reading$X2018.2019.Pass.Rate)
reading = subset(reading, reading$X2020.2021.Pass.Rate!="NA")
reading = subset(reading, reading$X2018.2019.Pass.Rate!="NA")
reading = subset(reading, reading$X2020.2021.Pass.Rate!=100)
reading = subset(reading, reading$X2018.2019.Pass.Rate!=0)
reading = subset(reading, reading$X2020.2021.Pass.Rate!=0)
reading = subset(reading, reading$X2018.2019.Pass.Rate!=100)
# ======================= GENDER ======================= #
genders = subset(df, df$Subgroup=="Male"|df$Subgroup=="Female")
genders = subset(genders, genders$X2020.2021.Pass.Rate!="<")
genders$X2020.2021.Pass.Rate = as.double(genders$X2020.2021.Pass.Rate)
# ======================= WHITE ======================= #
white = subset(df, df$Subgroup=="White")
white = subset(white, white$X2020.2021.Pass.Rate!="<")
white$X2020.2021.Pass.Rate = as.double(white$X2020.2021.Pass.Rate)
white$X2018.2019.Pass.Rate = as.double(white$X2018.2019.Pass.Rate)
# ======================= ASIAN ======================= #
asian = subset(df, df$Subgroup=="Asian")
asian = subset(asian, asian$X2020.2021.Pass.Rate!="<")
asian$X2020.2021.Pass.Rate = as.double(asian$X2020.2021.Pass.Rate)
asian$X2018.2019.Pass.Rate = as.double(asian$X2018.2019.Pass.Rate)
# ======================= BLACK ======================= #
black = subset(df, df$Subgroup=="Black")
black = subset(black, black$X2020.2021.Pass.Rate!="<")
black$X2020.2021.Pass.Rate = as.double(black$X2020.2021.Pass.Rate)
black$X2018.2019.Pass.Rate = as.double(black$X2018.2019.Pass.Rate)
# ======================= HISPANIC ======================= #
hispanic = subset(df, df$Subgroup=="Hispanic")
hispanic = subset(hispanic, hispanic$X2020.2021.Pass.Rate!="<")
hispanic$X2020.2021.Pass.Rate = as.double(hispanic$X2020.2021.Pass.Rate)
hispanic$X2018.2019.Pass.Rate = as.double(hispanic$X2018.2019.Pass.Rate)
# ======================= ECON DISADV ======================= #
econ = subset(df, df$Subgroup=="Economically Disadvantaged")
econ = subset(econ, econ$X2020.2021.Pass.Rate!="<")
econ$X2020.2021.Pass.Rate = as.double(econ$X2020.2021.Pass.Rate)
econ$X2018.2019.Pass.Rate = as.double(econ$X2018.2019.Pass.Rate)
# ======================= Histogram with density plot ======================= #
gg1 = ggplot(allStudents, aes(x=allStudents$X2020.2021.Pass.Rate)) + 
  geom_histogram(aes(y=..density..), colour="black", 
  fill="green", binwidth = 1)+
  geom_density(alpha=.5, fill="#0072fc") +
  labs(title="Distribution of SOL pass rates in VA Public Schools for all students 2021",
       x="2020-2021 Pass Rate", y = "Density")
# ======================= Distributions of grades by Gender ======================= #

gg2 = ggplot(genders, aes(x=genders$X2020.2021.Pass.Rate, 
                    color=genders$Subgroup, fill=genders$Subgroup)) + 
  labs(fill='Gender',color='Gender')+
  geom_histogram(aes(y=..density..),
  position="identity", alpha=0.4,binwidth = 1) + 
  geom_density(alpha=0.0)+
  ggtitle("Distributions of Male and Female SOL pass rates.") +
  labs(x="2020-2021 Pass Rate",
  title="Distributions of Male and Female SOL pass rates.", y="Density")

plot_grid(gg1,gg2)

# Distributions of all students by subject
new.distMatrix <- function() {
  math_all = subset(math, math$Subgroup=="All Students")
  p1 <- ggplot(math_all, aes(x=math_all$X2020.2021.Pass.Rate)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="#0072fc", binwidth = 1)+
    geom_density(alpha=.5, fill="#0072fc") +
    labs(title="Math",x="Pass rate of school", y = "Density")
  
  sci_all = subset(science, science$Subgroup=="All Students")
  p2 <- ggplot(sci_all, aes(x=sci_all$X2020.2021.Pass.Rate)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="#8900bf", binwidth = 1)+
    geom_density(alpha=.5, fill="#8900bf") +
    labs(title="Science",x="Pass rate of school", y = "Density")
  
  read_all = subset(reading, reading$Subgroup=="All Students")
  p3 <- ggplot(read_all, aes(x=read_all$X2020.2021.Pass.Rate)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="#00bf39", binwidth = 1)+
    geom_density(alpha=.5, fill="#00bf39") +
    labs(title="Reading",x="Pass rate of school", y = "Density")
  Subject = allStudents$Subject
  p4 <- ggplot(allStudents, aes(x=allStudents$X2020.2021.Pass.Rate, color=Subject, fill=Subject)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
    geom_density(alpha=0.0)+
    scale_color_manual(values=c("#00bf39", "#0072fc", "#8900bf"))+
    scale_fill_manual(values=c("#00bf39", "#0072fc", "#8900bf"))+
    labs(title="",x="Pass rate of school", y = "Density")
  
  row = plot_grid(p1, p2, p3, nrow = 1)
  plot_grid(p4,row, nrow = 2)
}
new.distMatrix()

#Density plots

new.densityMatrix <- function() {
  # ================== RACES ====================================
  
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  readRace = races3
  
  # =========  ====== ======= Distributions of grades by subgroup
  ## ONLY RACE
  
  m1 = ggplot(mathRace, aes(x=mathRace$X2020.2021.Pass.Rate, fill=mathRace$Subgroup)) +
    geom_density(alpha=0.4)+ggtitle("Math")+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  m2 = ggplot(scienceRace, aes(x=scienceRace$X2020.2021.Pass.Rate, fill=scienceRace$Subgroup)) +
    geom_density(alpha=0.4)+ggtitle("Science")+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  m3 = ggplot(readRace, aes(x=readRace$X2020.2021.Pass.Rate, fill=readRace$Subgroup)) +
    geom_density(alpha=0.4)+ggtitle("Reading")+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  
  legend7 = ggplot(readRace, aes(x=readRace$X2020.2021.Pass.Rate, fill=readRace$Subgroup)) +
    geom_density(alpha=0.4)+ labs(fill = "Only Race")
  
  
  r1 = plot_grid(m1,m2,m3,nrow = 1)
  r11 = plot_grid(r1, get_legend(legend7), nrow = 1,rel_widths = c(4, 1))
  
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  #races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  #races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  #races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  readRace = races3
  
  ## With other factors
  m4 = ggplot(mathRace, aes(x=mathRace$X2020.2021.Pass.Rate, fill=mathRace$Subgroup)) +
    geom_density(alpha=0.4)+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  m5 = ggplot(scienceRace, aes(x=scienceRace$X2020.2021.Pass.Rate, fill=scienceRace$Subgroup)) +
    geom_density(alpha=0.4)+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  m6 = ggplot(readRace, aes(x=readRace$X2020.2021.Pass.Rate, fill=readRace$Subgroup)) +
    geom_density(alpha=0.4)+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  legend77 = ggplot(readRace, aes(x=readRace$X2020.2021.Pass.Rate, fill=readRace$Subgroup)) +
    geom_density(alpha=0.4)+ labs(fill = "Race and Economic Disadvantage")
  
  r2 = plot_grid(m4,m5,m6,nrow = 1)
  r22 = plot_grid(r2, get_legend(legend77), nrow = 1,rel_widths = c(4, 1))
  
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  #races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  #races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  #races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  readRace = races3
  
  ## With other factors
  m7 = ggplot(mathRace, aes(x=mathRace$X2020.2021.Pass.Rate, fill=mathRace$Subgroup)) +
    geom_density(alpha=0.4)+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  m8 = ggplot(scienceRace, aes(x=scienceRace$X2020.2021.Pass.Rate, fill=scienceRace$Subgroup)) +
    geom_density(alpha=0.4)+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  m9 = ggplot(readRace, aes(x=readRace$X2020.2021.Pass.Rate, fill=readRace$Subgroup)) +
    geom_density(alpha=0.4)+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")
  legend777 = ggplot(readRace, aes(x=readRace$X2020.2021.Pass.Rate, fill=readRace$Subgroup)) +
    geom_density(alpha=0.4)+ labs(fill = "Race and disability")
  
  
  r3 = plot_grid(m7,m8,m9,nrow = 1)
  r33 = plot_grid(r3, get_legend(legend777), nrow = 1,rel_widths = c(4, 1))
  
  plot_grid(r11,r22,r33,nrow=3)
}
new.densityMatrix()

#Box plots wizard
new.boxplotMatrix <- function() {
  
  pl1 = ggplot(math, aes(x=math$Subgroup, y=math$X2020.2021.Pass.Rate, fill=math$Subgroup)) +
    geom_boxplot() +coord_flip() +
    ggtitle('MATH SOL') +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")+
    scale_fill_brewer(palette="Spectral")
  
  pl2 = ggplot(science, aes(x=science$Subgroup, y=science$X2020.2021.Pass.Rate, fill=science$Subgroup)) +
    geom_boxplot() + coord_flip() + 
    ggtitle('SCIENCE SOL') +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")+
    scale_fill_brewer(palette="Spectral")
  
  pl3 = ggplot(reading, aes(x=reading$Subgroup, y=reading$X2020.2021.Pass.Rate, fill=reading$Subgroup)) +
    geom_boxplot() +coord_flip() +
    ggtitle('READING SOL')+
    theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),legend.position="none")+
    scale_fill_brewer(palette="Spectral")
  
  legend = ggplot(reading, aes(x=reading$Subgroup, y=reading$X2020.2021.Pass.Rate, fill=reading$Subgroup)) +
    geom_boxplot() +coord_flip() +
    ggtitle('READING SOL') + theme(legend.position="bottom")+labs(fill="")+
    scale_fill_brewer(palette="Spectral")
  g = plot_grid(pl1,pl2,pl3, nrow = 1)
  plot_grid(g,get_legend(legend), nrow = 2,rel_heights = c(4, 1))
}
new.boxplotMatrix()

## Scatter plots // Linear regression
new.scatterMatrix <- function() {
  #Asian
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="Black")
  races1 = subset(races1, races1$Subgroup!="Hispanic")
  races1 = subset(races1, races1$Subgroup!="White")
  mathRace = races1
  
  
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="Black")
  races2 = subset(races2, races2$Subgroup!="Hispanic")
  races2 = subset(races2, races2$Subgroup!="White")
  scienceRace = races2
  
  
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="Black")
  races3 = subset(races3, races3$Subgroup!="Hispanic")
  races3 = subset(races3, races3$Subgroup!="White")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  cor1 = cor(mathRace$X2020.2021.Pass.Rate,mathRace$X2018.2019.Pass.Rate)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  cor2 = cor(scienceRace$X2020.2021.Pass.Rate,scienceRace$X2018.2019.Pass.Rate)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  cor3 = cor(readRace$X2020.2021.Pass.Rate,readRace$X2018.2019.Pass.Rate)
  model3
  
  aCor = c(cor1,cor2,cor3)
  
  g1 = ggplot(mathRace, aes(x=mathRace$X2018.2019.Pass.Rate, 
                            y=mathRace$X2020.2021.Pass.Rate, 
                            color=mathRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -37.273, slope = 1.189,
                linetype="solid", size=0.5)+
    annotate("label",x = 60.8,y = 90,label = "y = -37.273 +1.189x")+
    geom_density_2d(alpha = 0.75,color="blue") + theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                             x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g2 = ggplot(scienceRace, aes(x=scienceRace$X2018.2019.Pass.Rate, 
                               y=scienceRace$X2020.2021.Pass.Rate, 
                               color=scienceRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = 7.6231, slope = 0.7334,
                linetype="solid", size=0.5)+
    annotate("label",x = 60.8,y = 90,label = "y = 7.6231 +0.7334x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g3 = ggplot(readRace, aes(x=readRace$X2018.2019.Pass.Rate, 
                            y=readRace$X2020.2021.Pass.Rate, 
                            color=readRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = 15.0954, slope = 0.7395,
                linetype="solid", size=0.5)+
    annotate("label",x = 60.8,y = 90,label = "y = 15.0954 +0.7395x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  
  
  pg1 = plot_grid(g1,g2,g3, nrow = 1)+panel_border()
  
  
  #White
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="Black")
  races1 = subset(races1, races1$Subgroup!="Hispanic")
  races1 = subset(races1, races1$Subgroup!="Asian")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="Black")
  races2 = subset(races2, races2$Subgroup!="Hispanic")
  races2 = subset(races2, races2$Subgroup!="Asian")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="Black")
  races3 = subset(races3, races3$Subgroup!="Hispanic")
  races3 = subset(races3, races3$Subgroup!="Asian")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  cor1 = cor(mathRace$X2020.2021.Pass.Rate,mathRace$X2018.2019.Pass.Rate)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  cor2 = cor(scienceRace$X2020.2021.Pass.Rate,scienceRace$X2018.2019.Pass.Rate)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  cor3 = cor(readRace$X2020.2021.Pass.Rate,readRace$X2018.2019.Pass.Rate)
  model3
  
  wCor = c(cor1,cor2,cor3)
  
  
  g4 = ggplot(mathRace, aes(x=mathRace$X2018.2019.Pass.Rate, 
                            y=mathRace$X2020.2021.Pass.Rate, 
                            color=mathRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -46.136, slope = 1.237,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = -46.136 +1.237x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g5 = ggplot(scienceRace, aes(x=scienceRace$X2018.2019.Pass.Rate, 
                               y=scienceRace$X2020.2021.Pass.Rate, 
                               color=scienceRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -18.8054, slope = 0.9631,
                linetype="solid", size=0.5)+
    annotate("label",x = 60.8,y = 90,label = "y = -18.8054 +0.9631x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g6 = ggplot(readRace, aes(x=readRace$X2018.2019.Pass.Rate, 
                            y=readRace$X2020.2021.Pass.Rate, 
                            color=readRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -8.817, slope = 1.008,
                linetype="solid", size=0.5)+
    annotate("label",x = 60.8,y = 90,label = "y = -8.817 +1.008x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  pg2 = plot_grid(g4,g5,g6, nrow = 1)+panel_border()
  
  
  #Black
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="White")
  races1 = subset(races1, races1$Subgroup!="Hispanic")
  races1 = subset(races1, races1$Subgroup!="Asian")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="White")
  races2 = subset(races2, races2$Subgroup!="Hispanic")
  races2 = subset(races2, races2$Subgroup!="Asian")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="White")
  races3 = subset(races3, races3$Subgroup!="Hispanic")
  races3 = subset(races3, races3$Subgroup!="Asian")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  cor1 = cor(mathRace$X2020.2021.Pass.Rate,mathRace$X2018.2019.Pass.Rate)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  cor2 = cor(scienceRace$X2020.2021.Pass.Rate,scienceRace$X2018.2019.Pass.Rate)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  cor3 = cor(readRace$X2020.2021.Pass.Rate,readRace$X2018.2019.Pass.Rate)
  model3
  
  bCor = c(cor1,cor2,cor3)
  
  
  g7 = ggplot(mathRace, aes(x=mathRace$X2018.2019.Pass.Rate, 
                            y=mathRace$X2020.2021.Pass.Rate, 
                            color=mathRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -14.3513, slope = 0.7141,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = -14.3513 +0.7141x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g8 = ggplot(scienceRace, aes(x=scienceRace$X2018.2019.Pass.Rate, 
                               y=scienceRace$X2020.2021.Pass.Rate, 
                               color=scienceRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = 2.9204, slope = 0.5024,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = 2.9204 +0.5024x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g9 = ggplot(readRace, aes(x=readRace$X2018.2019.Pass.Rate, 
                            y=readRace$X2020.2021.Pass.Rate, 
                            color=readRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = 4.6681, slope = 0.7735,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = 4.6681 +0.7735x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  pg3 = plot_grid(g7,g8,g9, nrow = 1)+panel_border()
  
  #Hispanic
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="Black")
  races1 = subset(races1, races1$Subgroup!="White")
  races1 = subset(races1, races1$Subgroup!="Asian")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="Black")
  races2 = subset(races2, races2$Subgroup!="White")
  races2 = subset(races2, races2$Subgroup!="Asian")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="Black")
  races3 = subset(races3, races3$Subgroup!="White")
  races3 = subset(races3, races3$Subgroup!="Asian")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  cor1 = cor(mathRace$X2020.2021.Pass.Rate,mathRace$X2018.2019.Pass.Rate)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  cor2 = cor(scienceRace$X2020.2021.Pass.Rate,scienceRace$X2018.2019.Pass.Rate)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  cor3 = cor(readRace$X2020.2021.Pass.Rate,readRace$X2018.2019.Pass.Rate)
  model3
  
  hCor = c(cor1,cor2,cor3)
  
  g10 = ggplot(mathRace, aes(x=mathRace$X2018.2019.Pass.Rate, 
                             y=mathRace$X2020.2021.Pass.Rate, 
                             color=mathRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -23.7126, slope = 0.8706,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = -23.7126 +0.8706x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g11 = ggplot(scienceRace, aes(x=scienceRace$X2018.2019.Pass.Rate, 
                                y=scienceRace$X2020.2021.Pass.Rate, 
                                color=scienceRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -5.1983, slope = 0.6624,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = -5.1983 +0.6624x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  g12 = ggplot(readRace, aes(x=readRace$X2018.2019.Pass.Rate, 
                             y=readRace$X2020.2021.Pass.Rate, 
                             color=readRace$Div.Num)) +
    geom_point()+
    geom_abline(intercept = -0.6660, slope = 0.8478,
                linetype="solid", size=0.5)+
    annotate("label",x = 40.8,y = 90,label = "y = -0.6660 +0.8478x")+
    geom_density_2d(alpha = 0.75,color="blue")+ theme(legend.position = "none",axis.title.y=element_blank(),axis.title.x=element_blank())+labs(title="",
                                                            x ="2018-2019 Pass rate", y = "2020-2021 Pass rate")
  
  
  pg4 = plot_grid(g10,g11,g12, nrow = 1)+panel_border()
  title <- ggdraw() + 
    draw_label(
      "                                 Math                                                                  Science                                                                  Reading",
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  plot = plot_grid(title,pg1,pg2,pg3,pg4, ncol = 1,rel_heights = c(0.1, 1,1,1,1),
            labels = c('', 'Asian', 'White', 'Black', 'Hispanic'))+ theme(plot.margin = margin(30, 30, -50, 50))
  plot <- add_sub(plot, "2018-2019 Pass Rate", hjust = 1)
  plot <- add_sub(plot, "2020-2021 Pass Rate", -0.02, 3.5, angle = 90)
  ggdraw(plot)
  
  # cors = data.frame(aCor,wCor,bCor,hCor)
  # rownames(cors) = c("Math","Science","Reading")
  # cors = t(cors)
  # rownames(cors) = c("Asian","White","Black","Hispanic")
  # corrplot(cors, method="pie")
}
new.scatterMatrix()

## Scatter plots // Linear regression
new.scatterRes <- function() {
  #Asian
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="Black")
  races1 = subset(races1, races1$Subgroup!="Hispanic")
  races1 = subset(races1, races1$Subgroup!="White")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="Black")
  races2 = subset(races2, races2$Subgroup!="Hispanic")
  races2 = subset(races2, races2$Subgroup!="White")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="Black")
  races3 = subset(races3, races3$Subgroup!="Hispanic")
  races3 = subset(races3, races3$Subgroup!="White")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  model3
  
  g1 = ggplot(model1) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                              axis.title.y=element_blank(),
                              axis.title.x=element_blank())
  
  g2 = ggplot(model2) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  g3 = ggplot(model3) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  
  pg1 = plot_grid(g1,g2,g3, nrow = 1)+panel_border()
  
  
  #White
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="Black")
  races1 = subset(races1, races1$Subgroup!="Hispanic")
  races1 = subset(races1, races1$Subgroup!="Asian")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="Black")
  races2 = subset(races2, races2$Subgroup!="Hispanic")
  races2 = subset(races2, races2$Subgroup!="Asian")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="Black")
  races3 = subset(races3, races3$Subgroup!="Hispanic")
  races3 = subset(races3, races3$Subgroup!="Asian")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  model3
  
  
  g4 = ggplot(model1) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  
  g5 = ggplot(model2) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  g6 = ggplot(model3) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank()) 
  pg2 = plot_grid(g4,g5,g6, nrow = 1)+panel_border()
  
  
  #Black
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="White")
  races1 = subset(races1, races1$Subgroup!="Hispanic")
  races1 = subset(races1, races1$Subgroup!="Asian")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="White")
  races2 = subset(races2, races2$Subgroup!="Hispanic")
  races2 = subset(races2, races2$Subgroup!="Asian")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="White")
  races3 = subset(races3, races3$Subgroup!="Hispanic")
  races3 = subset(races3, races3$Subgroup!="Asian")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  model3
  
  
  g7 = ggplot(model1) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  
  g8 = ggplot(model2) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  g9 = ggplot(model3) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  pg3 = plot_grid(g7,g8,g9, nrow = 1)+panel_border()
  
  #Hispanic
  races1 = subset(math, math$Subgroup!="Female")
  races1 = subset(races1, races1$Subgroup!="Male")
  races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
  races1 = subset(races1, races1$Subgroup!="English Learners")
  races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
  races1 = subset(races1, races1$Subgroup!="All Students")
  races1 = subset(races1, races1$Subgroup!="Black")
  races1 = subset(races1, races1$Subgroup!="White")
  races1 = subset(races1, races1$Subgroup!="Asian")
  mathRace = races1
  
  races2 = subset(science, science$Subgroup!="Female")
  races2 = subset(races2, races2$Subgroup!="Male")
  races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
  races2 = subset(races2, races2$Subgroup!="English Learners")
  races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
  races2 = subset(races2, races2$Subgroup!="All Students")
  races2 = subset(races2, races2$Subgroup!="Black")
  races2 = subset(races2, races2$Subgroup!="White")
  races2 = subset(races2, races2$Subgroup!="Asian")
  scienceRace = races2
  
  races3 = subset(reading, reading$Subgroup!="Female")
  races3 = subset(races3, races3$Subgroup!="Male")
  races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
  races3 = subset(races3, races3$Subgroup!="English Learners")
  races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
  races3 = subset(races3, races3$Subgroup!="All Students")
  races3 = subset(races3, races3$Subgroup!="Black")
  races3 = subset(races3, races3$Subgroup!="White")
  races3 = subset(races3, races3$Subgroup!="Asian")
  readRace = races3
  
  model1 <- lm(mathRace$X2020.2021.Pass.Rate~mathRace$X2018.2019.Pass.Rate, data = mathRace)
  model1
  model2 <- lm(scienceRace$X2020.2021.Pass.Rate~scienceRace$X2018.2019.Pass.Rate, data = scienceRace)
  model2
  model3 <- lm(readRace$X2020.2021.Pass.Rate~readRace$X2018.2019.Pass.Rate, data = readRace)
  model3
  
  g10 = ggplot(model1) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  
  g11 = ggplot(model2) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  g12 = ggplot(model3) + 
    geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())
  pg4 = plot_grid(g10,g11,g12, nrow = 1)+panel_border()
  title <- ggdraw() + 
    draw_label(
      "                                 Math                                                                  Science                                                                  Reading",
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  plot = plot_grid(title,pg1,pg2,pg3,pg4, ncol = 1,rel_heights = c(0.1, 1,1,1,1),
                   labels = c('', 'Asian', 'White', 'Black', 'Hispanic'))+ theme(plot.margin = margin(30, 30, -50, 50))
  plot <- add_sub(plot, "Predicted", hjust = 1)
  plot <- add_sub(plot, "Residuals", -0.02, 3.5, angle = 90)
  ggdraw(plot)
}
new.scatterRes()



#========================= Multiple Regression =========================#



reading = subset(allStudents, allStudents$Subject=="English: Reading")
math = subset(allStudents, allStudents$Subject=="Mathematics")
science = subset(allStudents, allStudents$Subject=="Science")

readModel = lm(reading$X2020.2021.Pass.Rate~reading$X2018.2019.Pass.Rate)
mathModel = lm(math$X2020.2021.Pass.Rate~math$X2018.2019.Pass.Rate)
scienceModel = lm(science$X2020.2021.Pass.Rate~science$X2018.2019.Pass.Rate)

cor1 = cor(reading$X2020.2021.Pass.Rate~reading$X2018.2019.Pass.Rate)
cor2 = cor(math$X2020.2021.Pass.Rate~math$X2018.2019.Pass.Rate)
cor3 = cor(science$X2020.2021.Pass.Rate~science$X2018.2019.Pass.Rate)

proPlot = ggplot(allStudents, aes(x=allStudents$X2018.2019.Pass.Rate,
                        y=allStudents$X2020.2021.Pass.Rate, color=allStudents$Subject)) +
  geom_point() + geom_smooth(method = lm, size=1) + 
  geom_abline(intercept = -22.601,slope=1.169, color="red")+ 
  geom_abline(intercept = -58.635,slope=1.357, color="green")+ 
  geom_abline(intercept = -35.994,slope=1.116, color="blue")+
  annotate("label",x = 20.8,y = 90,label = "y = 22.601 + 1.169x", color="red")+
  annotate("label",x = 20.8,y = 80,label = "y = -58.635 +1.357x", color="darkgreen")+
  annotate("label",x = 20.8,y = 70,label = "y = -35.994 +1.116x", color="blue") + 
  labs(title="Multiple regression for all students using test subject as predictor",
       x="pass rate 2018-2019 all students",y="pass rate 2020-2021 all students", 
       color="Tested Subject")
proPlot2 = ggplot(allStudents, aes(x=allStudents$X2018.2019.Pass.Rate,
    y=allStudents$X2020.2021.Pass.Rate, 
    color=allStudents$Subject)) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  geom_point() + stat_density_2d(aes(fill = ..level.., 
  alpha=allStudents$Div.Num), geom = "polygon", colour="white")+
  labs(title="",
       x="pass rate 2018-2019 all students",y="pass rate 2020-2021 all students", 
       color="Tested Subject")+ 
  theme(legend.position = "none", 
        axis.title.y=element_blank(), axis.title.x=element_blank())

g777 = ggplot(readModel) + 
  geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                              axis.title.y=element_blank(),
                                              axis.title.x=element_blank())

g7777 = ggplot(mathModel) + 
  geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                              axis.title.y=element_blank(),
                                              axis.title.x=element_blank())
g77777 = ggplot(scienceModel) + 
  geom_point(aes(x=.fitted, y=.resid))+ theme(legend.position = "none",
                                              axis.title.y=element_blank(),
                                              axis.title.x=element_blank())

pg77 = plot_grid(proPlot2,proPlot, nrow = 1,rel_widths = c(0.2,1))
pg7 = plot_grid(g777,g7777,g77777, nrow = 1,labels = c('Reading Residuals', 'Math Residuals', 'Science Residuals'))+panel_border()

plot_grid(proPlot+panel_border(), pg7, ncol = 1)

# ========================= T TEST ========================= #

#Black
races1 = subset(math, math$Subgroup!="Female")
races1 = subset(races1, races1$Subgroup!="Male")
races1 = subset(races1, races1$Subgroup!="Economically Disadvantaged")
races1 = subset(races1, races1$Subgroup!="English Learners")
races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
races1 = subset(races1, races1$Subgroup!="All Students")
races1 = subset(races1, races1$Subgroup!="White")
races1 = subset(races1, races1$Subgroup!="Hispanic")
races1 = subset(races1, races1$Subgroup!="Asian")
mathRace = races1

races2 = subset(science, science$Subgroup!="Female")
races2 = subset(races2, races2$Subgroup!="Male")
races2 = subset(races2, races2$Subgroup!="Economically Disadvantaged")
races2 = subset(races2, races2$Subgroup!="English Learners")
races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
races2 = subset(races2, races2$Subgroup!="All Students")
races2 = subset(races2, races2$Subgroup!="White")
races2 = subset(races2, races2$Subgroup!="Hispanic")
races2 = subset(races2, races2$Subgroup!="Asian")
scienceRace = races2

races3 = subset(reading, reading$Subgroup!="Female")
races3 = subset(races3, races3$Subgroup!="Male")
races3 = subset(races3, races3$Subgroup!="Economically Disadvantaged")
races3 = subset(races3, races3$Subgroup!="English Learners")
races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
races3 = subset(races3, races3$Subgroup!="All Students")
races3 = subset(races3, races3$Subgroup!="White")
races3 = subset(races3, races3$Subgroup!="Hispanic")
races3 = subset(races3, races3$Subgroup!="Asian")
readRace = races3

#Econ
races1 = subset(math, math$Subgroup!="Female")
races1 = subset(races1, races1$Subgroup!="Male")
races1 = subset(races1, races1$Subgroup!="Black")
races1 = subset(races1, races1$Subgroup!="English Learners")
races1 = subset(races1, races1$Subgroup!="Students with Disabilities")
races1 = subset(races1, races1$Subgroup!="All Students")
races1 = subset(races1, races1$Subgroup!="White")
races1 = subset(races1, races1$Subgroup!="Hispanic")
races1 = subset(races1, races1$Subgroup!="Asian")
mathEcon = races1

races2 = subset(science, science$Subgroup!="Female")
races2 = subset(races2, races2$Subgroup!="Male")
races2 = subset(races2, races2$Subgroup!="Black")
races2 = subset(races2, races2$Subgroup!="English Learners")
races2 = subset(races2, races2$Subgroup!="Students with Disabilities")
races2 = subset(races2, races2$Subgroup!="All Students")
races2 = subset(races2, races2$Subgroup!="White")
races2 = subset(races2, races2$Subgroup!="Hispanic")
races2 = subset(races2, races2$Subgroup!="Asian")
scienceRace = races2

races3 = subset(reading, reading$Subgroup!="Female")
races3 = subset(races3, races3$Subgroup!="Male")
races3 = subset(races3, races3$Subgroup!="Black")
races3 = subset(races3, races3$Subgroup!="English Learners")
races3 = subset(races3, races3$Subgroup!="Students with Disabilities")
races3 = subset(races3, races3$Subgroup!="All Students")
races3 = subset(races3, races3$Subgroup!="White")
races3 = subset(races3, races3$Subgroup!="Hispanic")
races3 = subset(races3, races3$Subgroup!="Asian")
readRace = races3


xb = favstats(mathRace$X2020.2021.Pass.Rate)$mean
xe = favstats(mathEcon$X2020.2021.Pass.Rate)$mean
sb = favstats(mathRace$X2020.2021.Pass.Rate)$sd
se = favstats(mathEcon$X2020.2021.Pass.Rate)$sd  

t.test(mathRace$X2020.2021.Pass.Rate,
       mathEcon$X2020.2021.Pass.Rate,alternative = "greater")



A = length(math$X2020.2021.Pass.Rate)
A2 = length(reading$X2020.2021.Pass.Rate)
A3 = length(science$X2020.2021.Pass.Rate)
length(science$X2020.2021.Pass.Rate) = A

MATH = math$X2020.2021.Pass.Rate
READING = reading$X2020.2021.Pass.Rate
SCIENCE = science$X2020.2021.Pass.Rate




threedData = data.frame(MATH,READING,SCIENCE) 
s3d <- scatterplot3d(threedData, color="darkGreen", angle=55, pch = 16)
my.lm <- lm(threedData$READING ~ threedData$MATH + threedData$SCIENCE)
s3d$plane3d(my.lm)


# Build 10 images -> save them at .png format
#png(file="~/Desktop/math/example%02d.png", width=480, height=480)
#par(bg="white")
for (i in c(360:1)){
  plot.new()
  threedData = data.frame(MATH,READING,SCIENCE) 
  s3d <- scatterplot3d(threedData, color="darkGreen", angle=i, pch = 16)
  my.lm <- lm(threedData$READING ~ threedData$MATH + threedData$SCIENCE)
  s3d$plane3d(my.lm)
}
dev.off()

# Use image magick
system("convert -delay 80 *.png animated_count_down.gif")

# Remove png files
file.remove(list.files(pattern=".png"))






allStudents2 = df
allStudents2 = subset(allStudents2, allStudents2$X2020.2021.Pass.Rate!="<")
allStudents2$X2020.2021.Pass.Rate = as.double(allStudents2$X2020.2021.Pass.Rate)
allStudents2$X2018.2019.Pass.Rate = as.double(allStudents2$X2018.2019.Pass.Rate)
allStudents2 = subset(allStudents2, allStudents2$X2020.2021.Pass.Rate!="NA")
allStudents2 = subset(allStudents2, allStudents2$X2018.2019.Pass.Rate!="NA")
allStudents2 = subset(allStudents2, allStudents2$X2020.2021.Pass.Rate!=100)
allStudents2 = subset(allStudents2, allStudents2$X2018.2019.Pass.Rate!=0)
allStudents2 = subset(allStudents2, allStudents2$X2020.2021.Pass.Rate!=0)
allStudents2 = subset(allStudents2, allStudents2$X2018.2019.Pass.Rate!=100)
allStudents2 = data.frame(allStudents2$X2020.2021.Pass.Rate,allStudents2$Subgroup,allStudents2$Subject)
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="All Students")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="English Learners")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="Economically Disadvantaged")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="Students with Disabilities")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="Female")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="Male")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="Asian")
allStudents2 = subset(allStudents2, allStudents2$allStudents2.Subgroup!="Hispanic")

allStudents2$allStudents2.Subgroup= replace(allStudents2$allStudents2.Subgroup,allStudents2$allStudents2.Subgroup=="Black", 1)
allStudents2$allStudents2.Subgroup= replace(allStudents2$allStudents2.Subgroup, allStudents2$allStudents2.Subgroup=="White", 0)

allStudents2
logMath = subset(allStudents2, allStudents2$allStudents2.Subject=="Mathematics")
logRead = subset(allStudents2, allStudents2$allStudents2.Subject=="English: Reading")
logSci = subset(allStudents2, allStudents2$allStudents2.Subject=="Science")

# Logistic R
View(logMath)
isBlack=as.integer(logMath$allStudents2.Subgroup)
score=logMath$allStudents2.X2020.2021.Pass.Rate
mod=glm(isBlack~score, family=binomial)
summary(mod)
summary(mod)$coefficients
p = predict.glm(mod)
p
probs = exp(p)/(1+exp(p))
probs
class = probs
class = replace(class, class>=.5, "Black")
class = replace(class, class<.5, "White")
class = data.frame(class)
class
errorRate = class
errorRate = replace(errorRate, errorRate=="Black", 1)
errorRate = replace(errorRate, errorRate=="White", 0)
errorRates = errorRate == logMath$allStudents2.Subgroup
totalT = sum(errorRates, na.rm = FALSE)
dim(logMath)
errRate = ((3165 - totalT) / 3165) 
successRate = totalT / 3165
errRate
successRate

#plot logistic regression curve
l1 = ggplot(logMath, aes(x=logMath$allStudents2.X2020.2021.Pass.Rate, y=as.integer(logMath$allStudents2.Subgroup))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm",method.args = list(family=binomial)) + labs(x="Pass rate",y="White        Black", title = "Math")


isBlack=as.integer(logRead$allStudents2.Subgroup)
score=logRead$allStudents2.X2020.2021.Pass.Rate
mod=glm(isBlack~score, family=binomial)
summary(mod)
summary(mod)$coefficients
p = predict.glm(mod)
p
probs = exp(p)/(1+exp(p))
probs
class = probs
class = replace(class, class>=.5, "Black")
class = replace(class, class<.5, "White")
class = data.frame(class)
errorRate = class
errorRate = replace(errorRate, errorRate=="Black", 1)
errorRate = replace(errorRate, errorRate=="White", 0)
errorRates = errorRate == logRead$allStudents2.Subgroup
totalT = sum(errorRates, na.rm = FALSE)
dim(logRead)
errRate = ((3165 - totalT) / 3165) 
successRate = totalT / 3165
errRate
successRate

#plot logistic regression curve
l2 = ggplot(logRead, aes(x=logRead$allStudents2.X2020.2021.Pass.Rate, y=as.integer(logRead$allStudents2.Subgroup))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm",method.args = list(family=binomial)) + labs(x="Pass rate",y="White        Black", title = "Reading")


isBlack=as.integer(logSci$allStudents2.Subgroup)
score=logSci$allStudents2.X2020.2021.Pass.Rate
mod=glm(isBlack~score, family=binomial)
summary(mod)
summary(mod)$coefficients
p = predict.glm(mod)
p
probs = exp(p)/(1+exp(p))
probs
class = probs
class = replace(class, class>=.5, "Black")
class = replace(class, class<.5, "White")
class = data.frame(class)
errorRate = class
errorRate = replace(errorRate, errorRate=="Black", 1)
errorRate = replace(errorRate, errorRate=="White", 0)
errorRates = errorRate == logSci$allStudents2.Subgroup
totalT = sum(errorRates, na.rm = FALSE)
dim(logSci)
errRate = ((2666 - totalT) / 2666) 
successRate = totalT / 2666
errRate
successRate

#plot logistic regression curve
l3 = ggplot(logSci, aes(x=logSci$allStudents2.X2020.2021.Pass.Rate, y=as.integer(logSci$allStudents2.Subgroup))) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm",method.args = list(family=binomial)) + labs(x="Pass rate",y="White        Black", title = "Science")

plot_grid(l1,l2,l3, nrow = 1)
  
