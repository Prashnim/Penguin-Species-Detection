# global
library(shiny)
library(palmerpenguins)
library(plotly)
library(tidyverse)
library(corrplot)
library(dplyr)
library(gsubfn)
library(ggplot2)
# QQ-plot
library(car)
library(stats)
library(caret)
library(class)
library(stats)
library(prediction)
library(verification)
library(plyr) # NEED
library(e1071)
library(vtree)
library(DT)
library(kernlab)
library(bslib)

set.seed(1)
################ Basic Info about data set #########################
#head(penguins_raw)
#str(penguins_raw)
#ncol(penguins_raw)
#nrow(penguins_raw)
#colnames(penguins_raw)
#summary(penguins_raw)
############################# Data Cleaning ##################################
cleandata <- penguins_raw %>% dplyr::select(-c('studyName','Sample Number','Stage',
                                               'Individual ID','Clutch Completion',
                                               'Date Egg','Sex','Delta 15 N (o/oo)','Delta 13 C (o/oo)',
                                               'Comments','Region'))
### Remove NA rows
CleanestPenguins <- na.omit(cleandata)
#################### Lets add dummy variables #################################
# Dummy for species
CleanestPenguins$Species_num <- apply(CleanestPenguins, 1, FUN = function(x){
  if(x["Species"]=="Gentoo penguin (Pygoscelis papua)")x["Species_num"]=1 
  else if(x["Species"]=="Adelie Penguin (Pygoscelis adeliae)") x["Species_num"]=0 
  else x["Species_num"]=-1})
#### Shorten the Species name
CleanestPenguins$Species <- gsub(" .*","",CleanestPenguins$Species)

### Dummy for Island
CleanestPenguins$Island_num <- apply(CleanestPenguins, 1, 
                                     FUN = function(x) {
                                       if(x["Island"]=="Torgersen") 
                                         x["Island_num"]=1 
                                       else if(x["Island"]=="Biscoe")
                                         x["Island_num"]=0
                                       else
                                         x["Island_num"]=-1
                                     })
##### Change names to avoid confusion
colnames(CleanestPenguins)[5] <- "Flipper_Length_mm"
colnames(CleanestPenguins)[3] <- "Culmen_Length_mm"
colnames(CleanestPenguins)[4] <- "Culmen_Depth_mm"
colnames(CleanestPenguins)[6] <- "Body_Mass_g"

#################### Data Analysis ########################
df <- CleanestPenguins

########################## COUNT GRPAHS #######################
#### Species COUNT 
#install.packages("vtree")
vtree(CleanestPenguins, "Species")

#### Species COUNT 
ggplot(df,aes(x = Species, color = Species, fill = Species)) +
  geom_bar()

#### ISLAND COUNT 
speciesXisland <-aggregate(df$Species, by=list(df$Island,df$Species), FUN=length)
speciesXisland
plot_ly(data = speciesXisland, #pass in the data to be visualized
        x = ~Group.1 ~Group.2,
        y = ~x,
        color = ~Group.1,
        type = "bar",
        mode = NULL
) %>% layout(title ="Data Set Visualization",
             xaxis = list(title = "Species"),
             yaxis = list(title = "Count")) 

############################# ANOVA  ################################
dat <- df %>% dplyr::select(Species,Flipper_Length_mm,Culmen_Length_mm,
                            Body_Mass_g,Culmen_Depth_mm)

########### Aim and hypotheses of ANOVA ###################
## Are flipper length different for the 3 species of penguins?
# NULL: The three species are equal in terms of flipper length
# H1: At least ONE mean is different (one species is different from 
# the other 2 species in terms of flipper length )
dep_vars <- cbind(df$Flipper_Length_mm, df$Culmen_Length_mm, df$Culmen_Depth_mm, df$Body_Mass_g)
res_aov <- aov(dep_vars ~ Species, data = dat)
summary(res_aov)

########################## Histogram #########################
## Flipper Length
res_aovFlipper <- aov(Flipper_Length_mm ~ Species, data = dat)
hist(res_aovFlipper$residuals)
## Culmen Length
res_aovCulmen_Length <- aov(Culmen_Length_mm ~ Species, data = dat)
hist(res_aovCulmen_Length$residuals)
## Culmen Depth
res_aovCulmen_Depth <- aov(Culmen_Depth_mm ~ Species, data = dat)
hist(res_aovCulmen_Depth$residuals)
## Body Mass
res_aovBodyMass <- aov(Body_Mass_g ~ Species, data = dat)
hist(res_aovBodyMass$residuals)
###################### QQPlot #######################
# Q-Q-plots roughly follow straight line and most of them within the confidence bands
# indicating that residuals follow a normal distribution
qqPlot(res_aovFlipper$residuals,  main = "Flipper residuals",
       id = FALSE # id = FALSE to remove point identification
)
# Q-Q-plots roughly follow straight line and most of them within the confidence bands
# indicating that residuals follow a normal distribution
qqPlot(res_aovCulmen_Length$residuals, main = "Culmen Length residuals",
       id = FALSE # id = FALSE to remove point identification
)
# Q-Q-plots roughly follow straight line and most of them within the confidence bands
# indicating that residuals follow a normal distribution
qqPlot(res_aovCulmen_Depth$residuals,  main = "Culmen Depth residuals",
       id = FALSE # id = FALSE to remove point identification
)
# Q-Q-plots roughly follow straight line and most of them within the confidence bands
# indicating that residuals follow a normal distribution
qqPlot(res_aovBodyMass$residuals, main = "Body Mass residuals",
       id = FALSE # id = FALSE to remove point identification
)

############# Equality of variances - homogeneity
# Check whether the variances are equal across species or not. This will impact 
# if  ANOVA or the Welch test is used
# verifiy VISUALLY with boxplot
## Boxplot shows similar variance for different species
# seen by the fact that the boxes have comparable size for all species.
####################### BOX PLOTS #################################
ggplot(dat) +
  aes(x = Species, y = Flipper_Length_mm, fill= Species) +
  geom_boxplot() + ggtitle("Plot of length \n by dose") 

ggplot(dat) +
  aes(x = Species, y = Culmen_Length_mm, fill= Species) +
  geom_boxplot()

ggplot(dat) +
  aes(x = Species, y = Culmen_Depth_mm, fill= Species) +
  geom_boxplot()

ggplot(dat) +
  aes(x = Species, y = Body_Mass_g, fill= Species) +
  geom_boxplot()
## All assumptions of ANOVA are met thus we implement
### Compute some descriptive statistics 
# such as the mean and standard deviation by species.
# Mean is also the lowest for Adelie and highest for Gentoo BUT not enought to
# conclude that flippers are significantly different in the 3 populations of penguins.
####################  ANOVA in R ###########################
# Given p-values smaller than 0.05 we reject the null hypothesis that all means are equal
# Can conclude that at least one species is different than the others in terms of flipper
# p-value < 2.2e-16
#### Flipper
res_aovFlipper <- aov(dat$Flipper_Length_mm ~ Species, data = dat)
summary(res_aovFlipper)
#### Culmen_Length
res_aovCulmen_Length <- aov(dat$Culmen_Length_mm ~ Species, data = dat)
summary(res_aovCulmen_Length)
#### Culmen_Depth
res_aovCulmen_Depth <- aov(dat$Culmen_Depth_mm ~ Species, data = dat)
summary(res_aovCulmen_Depth)
#### Body_Mass
res_aovBodyMass <- aov(dat$Body_Mass_g ~ Species, data = dat)
summary(res_aovBodyMass)
######################### END OF ANOVA #########################
################# Standard Devations and Mean #############################
DescriptiveStatsFlipper <- aggregate(Flipper_Length_mm ~ Species, data = dat,
                                     function (x) round(c(mean = mean(x), sd = sd(x)),2))
DescriptiveStatsFlipper 


DescriptiveStatsCulmen_Length <- aggregate(Culmen_Length_mm ~ Species, data = dat,
                                           function (x) round(c(mean = mean(x), sd = sd(x)),2))
DescriptiveStatsCulmen_Length



DescriptiveStatsCulmen_Depth <- aggregate(Culmen_Depth_mm ~ Species, data = dat,
                                          function (x) round(c(mean = mean(x), sd = sd(x)),2))
DescriptiveStatsCulmen_Depth



DescriptiveStatsBodyMass <- aggregate(Body_Mass_g ~ Species, data = dat,
                                      function (x) round(c(mean = mean(x), sd = sd(x)),2))
DescriptiveStatsBodyMass 


##################### Tukey #############################
# If null hypothesis rejected we want to know which are different
# To test this need to compare groups 2 by 2 
# 1.Chinstrap versus Adelie
# 2. Gentoo vs. Adelie
# 3. Gentoo vs. Chinstrap
# all smaller than 0.05 so we reject the null for all comparisons which means
# ALL SPECIES are significantly different in terms of flipper length
post_testFlipper <- TukeyHSD(res_aovFlipper,conf.level=.95)
post_testFlipper
# Visualization of Tukey
plot(post_testFlipper)

post_testCulmen_Length <- TukeyHSD(res_aovCulmen_Length,conf.level=.95)
post_testCulmen_Length
# Visualization of Tukey
plot(post_testCulmen_Length)

post_testCulmen_Depth <- TukeyHSD(res_aovCulmen_Depth,conf.level=.95)
post_testCulmen_Depth
# Visualization of Tukey
plot(post_testCulmen_Depth)

post_testBodyMass <- TukeyHSD(res_aovBodyMass,conf.level=.95)
post_testBodyMass
# Visualization of Tukey
plot(post_testBodyMass)
################## END OF TUKEY #################
############# Correlcation plot ###############
df_cor <- cor(df %>% dplyr::select(-c(Species, Island)))
corrplot(df_cor)
df
############### CORR AS NUMS
dat <- (dat %>% dplyr::select(-c(Species)))
cor(dat)
############### REMOVE THE DUM VARS
df <- (df %>% dplyr::select(-c(Species_num, Island_num)))
############################### KNN ############################################

#  Construct the standardized dataset with the response variable, the 4 quantitative variables normalized 
#  with the scale function and the sex variable normalized with the 1,-1 assignment explained previously.
#
standardized = cbind( df[,1], scale( df[, 3:6] ))

# Define an 80-20 split of the training and test data.
# -------------------------------------------------------------------------------------
training.individuals = createDataPartition(standardized$Species, p= 0.8 , list = FALSE)
training.individuals
#X variables include bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g,  sex (converted to -1,1)
# Y variable is just the species class
# train data is the standardized subset of the full data set
# test data is the complement of the training data set.
# -----------------------------------------------------------------
train.X  =  standardized[training.individuals,  2:5]
test.X   =  standardized[-training.individuals,  2:5]
train.Y  =  standardized[ training.individuals,    1]
test.Y   =  standardized[-training.individuals,    1]
dim(train.X)
dim(test.X)

# Processing
# k-fold cross-validation is performed on training data to 
# determine optimal parameter k for model
# The maximum accuracy is reached with values of k
knn.model = train( Species ~. , 
                   data = standardized , 
                   method = "knn" ,
                   trControl = trainControl("cv", number = 5 ), 
                   tuneLength = 10 )
knn.model

###### Graph of accuracy and number of neighbors
plot(knn.model)

knnPredict <- predict(knn.model, newdata = test.X)
##### Confusion Matric ##########
knn.confmatrix = table(predicted=knnPredict, true=test.Y)
knn.confmatrix
## Test accuracy rate
sum(diag(knn.confmatrix)/sum(knn.confmatrix))
# Test error rate
1 - sum(diag(knn.confmatrix)/sum(knn.confmatrix))
############ ROC CURVE ###############
diag(knn.confmatrix)
# TP: The actual value 
# and predicted value should be the same. the value of cell 1 is the TP value
knn.tp <- diag(knn.confmatrix)
knn.tp
# FP: The sum of values of the corresponding column except for the TP value.
knn.fp <- colSums(knn.confmatrix) - knn.tp
knn.fp
# FN: The sum of values of corresponding rows except for the TP value
knn.fn <- rowSums(knn.confmatrix) - knn.tp
knn.fn
# TN: The sum of values of all columns 
# and rows except the values of that class that we are calculating the values for.
knn.tn <- sum(knn.confmatrix) - (knn.tp + knn.fp + knn.fn)
knn.tn

knn.tpr = knn.tp / (knn.tp + knn.fn)
knn.tpr
knn.fpr = knn.fp / (knn.fp + knn.tn)
################################ 4 RESULTS #################
# Accuracy 
knn.Accuracy <- sum(knn.tp + knn.tn)/sum(knn.tp + knn.tn + knn.fp + knn.fn)
knn.Accuracy
# Precision 
knn.Precision <- sum(knn.tp)/sum(knn.tp + knn.fp)
knn.Precision
# Recall 
knn.Recall <- sum(knn.tp)/sum(knn.tp + knn.fn)
knn.Recall
## F1-score
knn.F1Score <- 2*(knn.Precision * knn.Recall)/(knn.Precision + knn.Recall)
knn.F1Score

################## ROC ###########################
roc_data <- data.frame(knn.tpr = c(0, knn.tpr, 1), knn.fpr = c(0, knn.fpr, 1))
ggplot(roc_data, aes(x = knn.fpr, y = knn.tpr)) + geom_line() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)",
       title = "ROC Curve")
######################### Plotting KNN ###################
plot.df = data.frame(test.X, predicted = knnPredict)
head(plot.df)
tail(plot.df)
plot.df$predicted
# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Flipper_Length_mm, 
                      y = plot.df$Body_Mass_g, 
                      predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]

boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Flipper_Length_mm, Body_Mass_g, color = predicted, 
                    fill = predicted)) + geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)

########### Plot Grid KNN ############
pl = seq(min(standardized$Culmen_Length_mm), max(standardized$Culmen_Length_mm), by = 0.05)
pw = seq(min(standardized$Culmen_Depth_mm), max(standardized$Culmen_Depth_mm), by = 0.05 )

lgrid = expand.grid(Culmen_Length_mm=pl ,Culmen_Depth_mm = pw , Flipper_Length_mm = 0, 
                    Body_Mass_g = 0)

knnPredGrid = predict(knn.model, newdata=lgrid)

num_knnPredGrid = as.numeric(knnPredGrid)
num_knnpred = as.numeric(knnPredict)

test = cbind(test.X, test.Y)
test$Pred = num_knnpred
ggplot(data=lgrid) + stat_contour(aes(x=Culmen_Length_mm, y= Culmen_Depth_mm, z = num_knnPredGrid), bins = 2 ) +
  geom_point( aes(x=Culmen_Length_mm, y=Culmen_Depth_mm, color = knnPredGrid),  size = 1, shape = 19, alpha = 0.2) +
  geom_point( data= test , aes(x=Culmen_Length_mm, y=Culmen_Depth_mm, color= knnPredict) , size = 4, alpha = 0.8, shape =24 ) +
  ggtitle("KNN Decision Boundary for Penguins Data with k=5 neighbor")


######################### SVM #####################################

#80% training set
svm.training <- df[ training.individuals,]

#20% test set
svm.testing  <- df[-training.individuals,]

fit_control <- trainControl(## 15-fold CV
  method = "repeatedcv",
  number = 15,
  ## repeated ten times
  repeats = 15,
  classProbs = TRUE)

svm <- train(Species ~ .,
             data = svm.training, 
             method = "svmRadial", 
             trControl = fit_control,
             metric = "Accuracy")
svm

dim(svm.testing)
dim(svm.training)
svm.pred <-  predict(svm, svm.testing)
svm.pred
svm.testing$svm.pred <- svm.pred
ggplot(svm.testing, aes(x = Species, y = svm.pred, color = Species)) +
  geom_jitter(size = 3) 

table(svm.testing$Species, svm.testing$svm.pred)
svm.confmatrix <- table(pred = svm.pred, true =  svm.testing$Species)
## Test accuracy rate
sum(diag(svm.confmatrix)/sum(svm.confmatrix))
# Test error rate
1 - sum(diag(svm.confmatrix)/sum(svm.confmatrix))
############ ROC CURVE ###############
diag(svm.confmatrix)
# TP: The actual value 
# and predicted value should be the same. the value of cell 1 is the TP value
svm.tp <- diag(svm.confmatrix)
svm.tp
# FP: The sum of values of the corresponding column except for the TP value.
svm.fp <- colSums(svm.confmatrix) - svm.tp
svm.fp
# FN: The sum of values of corresponding rows except for the TP value
svm.fn <- rowSums(svm.confmatrix) - svm.tp
svm.fn
# TN: The sum of values of all columns 
# and rows except the values of that class that we are calculating the values for.
svm.tn <- sum(svm.confmatrix) - (svm.tp + svm.fp + svm.fn)
svm.tn

svm.tpr = svm.tp / (svm.tp + svm.fn)
svm.fpr = svm.fp / (svm.fp + svm.tn)
################################ 4 RESULTS #################
# Accuracy 
svm.Accuracy <- sum(svm.tp + svm.tn)/sum(svm.tp + svm.tn + svm.fp + svm.fn)
svm.Accuracy
# Precision 
svm.Precision <- sum(svm.tp)/sum(svm.tp + svm.fp)
svm.Precision
# Recall 
svm.Recall <- sum(svm.tp)/sum(svm.tp + svm.fn)
svm.Recall
## F1-score
svm.F1Score <- 2*(svm.Precision * svm.Recall)/(svm.Precision + svm.Recall)
svm.F1Score
################## ROC ###########################
# https://stats.stackexchange.com/questions/222173/calculate-true-positive-rate-tpr-and-false-positive-rate-fpr-from-prediction
roc_data <- data.frame(svm.tpr = c(0, svm.tpr, 1), svm.fpr = c(0, svm.fpr, 1))
ggplot(roc_data, aes(x = svm.fpr, y = svm.tpr)) + geom_line() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)",
       title = "ROC Curve")

############################################ UI ###################################
ui <- navbarPage(title = "Penguins Species Detection",
  tabPanel(title = "Introduction", 
      fluidPage(style="padding-top: 80px;",
        theme = bs_theme(version = 4, bootswatch = "minty"),
        tags$figure(
          align = "center",
          tags$img(
            src = "SpeciesOfPenguins.png",
            width = 900,
            alt = "Wide Shot of all penguins"
          ),
          tags$figcaption("Artwork by @allison_horst")
        ),
        h1("Penguin Species Detection", style = "color:lightcoral"),p("By Kelly, Prashnim"),
        h4("This projects goal is to use a machine learning algorithm to identify
        the species of a given penguin."),
        h4("The algorithm will learn by looking
    at a training set and its values. After training the algorithm we evaluate if the algorithm 
           can assign species on a test set."),  
           h4("The",strong("Methods"), "used are KNN and SVM"),
        h1("Information about Dataset",style = "color:deepskyblue"),
        h4("The library chosen is called",strong("palmerpenguins"), "there are two datasets in the library",
          strong("penguins"),"and", strong("penguins_raw"), ".For this project 
          penguins_raw was chosen because it held more information that can help teach the algorithm."
          ),
        h3("What does the data look like?",style = "color:mediumpurple"),
        h4("Dimensions:
           17 Columns and 344 observations"),
        datatable(penguins_raw,options = list(dom = 't', pageLength = 3)),
        h3("Column Names",style = "color:mediumpurple"),verbatimTextOutput("InfoColNames"),
        h3("Summary of data",style = "color:mediumpurple"),verbatimTextOutput("SummaryInfo"),
        h1("Plan",style = "color:lightcoral"),
        tags$figure(
          align = "center",
          tags$img(
            src = "Diagram.png",
            width = 900,
            alt = "Diagram for project"
          )),
      )),
#///////////////////////////// Data Cleaning /////////////////////////////#
  tabPanel(title = "Data Preprocessing ",
           fluidPage(style="padding-top: 80px;",
             theme = bs_theme(version = 4, bootswatch = "minty"),
             h1("Data Cleaning",style = "color:deepskyblue"),
             tags$figure(
               align = "center",
               tags$img(
                 src = "BetterImage.png",
                 width = 900,
                 alt = "Little picture of palmer penguins"
               ),
               tags$figcaption("by Laura Navarro")
             ),
             h3("Data cleaning is important for any machine learning project. Why?"),
             h4(" 1. Improve Model Accuracy: Data cleaning will improve our models accuracy 
          because it reduces the likelihood of errors in the output.", br(),
                br(),"2. Eliminate Bias: Cleaning data helps eliminate bias in the data. 
          Recall that bias occurs due to error or inconsistence in data.", br(), 
                br(),"3. Improve Efficiency: Removing irrelelvant data and redundant 
          features will reduce the complexity of the data. ",br(),
                br(),"4. Reduce Costs: Data cleaning reduces cost by finding 
          and correcting errors early on
          before the later stages of the project."),
                 h1("Raw Data", style = "color:lightcoral"),
                 datatable(penguins_raw,options = list(dom = 't', pageLength = 5)),
                 h2("Irrelelvant data",style = "color:deepskyblue"),
                 h3("For this project we only want data that can help with identification
                     11 out of 17 columns were removed."),
                     h3("Brief explantation: Much of the columns removed like",
                     code("studyName"),"and", code("Sample Number"),"for example
                         is more helpful for a researcher and not so much the algorithm. 
                        This data could slow down our algorithm, but even worse 
                        an error that could affect the training and testing leading to misclassifcation."), 
                 h3("Some other columns like",code("Clutch Completion"), "and",code("Stage"), 
                    "is data that does not have much correlation with an individual species"),
                 fixedPage(verbatimTextOutput("RemoveColumns")),
              
             #### Missing Data ########
               h2("Removing missing data", style = "color:lightcoral"), 
               h3("Besides removing columns that do not benefit our test 
        there are also missing values that need to be removed simply using the function",
                 strong("na.omit")), br(), fixedPage(verbatimTextOutput("RemoveNA")),
            ####### Change col names #####
               h3("While our actual data is much cleaner the names of columns 
        and species also need to be cleaned up. To avoid confusion R that may experience 
        we have removed spaces between column names and replaced with underscores."),
             fixedPage(verbatimTextOutput("ChangeColNames")),
               h3("Having the scientific names of the penguin species is not needed for this project
        to remove this gsub function is used"),
             ######### Dummy variables #####
             h3("Creating Dummy Variables", style = "color:deepskyblue"),
             fixedPage(verbatimTextOutput("DummyVar")),
              ######## Clean Data Table #############
               datatable(CleanestPenguins,options = list(dom = 't', pageLength = 5)))),
  
################ Data Visulatization & Analysis #################################
  navbarMenu("Data Visulatization & Analysis",
           tabPanel(title = "Count and Correlaction",
                  fluidPage(style="padding-top: 80px;",
                    theme = bs_theme(version = 4, bootswatch = "minty"),
                    h1("Count", style = "color:lightcoral"),
                h4("Bar charts are fundamental and a useful tool to explore and understanding data.
                      these plots helps compare the values of the data set."),
                    fluidRow(
                    column(3 ,h2("Species Count",style = "color:mediumpurple"),
                           h3("Adelie: 151", style = "color:coral"), br(), 
                           h3("Gentoo: 123", style = "color:deepskyblue"), br(),
                           h3("Chinstrap: 68", style = "color:lime"),
                           br(), h3("This graph shows the balance of the data set 
                                    between observations."), br(),
                           h3 ("Most common species is the
                                    Adelie and the least common is the Chinstrap.")),
                    column(5,offset = 2,
                                    plotlyOutput(outputId = "BarSpeciesCount", 
                                                 width = '700px', height = '700px'))),
                    br(),
                    fluidRow(
                      column(3 ,h2("Species Based Island Count Plot",style = "color:mediumpurple"),
                             h3("Shows which species of penguins live on a island")
                             ,br(),h3("Chinstrap penguins only on Dream island"),br(),
                                      h3("Gentoo penguins only live on Biscoe Island."),br(),
                                      h3("Adelie lives on all islands.")),
                      column(3 ,offset = 2,
                             plotlyOutput(outputId = "BarIslandCount",
                                          width = '800px', height = '500px'))),
                ##### Get Averages ######
                  br(), br(),
                  h1("Averages of Varaibles",style = "color:deepskyblue"),
                br(),
                fluidRow(
                    column(6,
                           fixedPage(
                             h3("Average of Flipper"),
                             h5("Gentoo have the largest average flippers while 
                                Adelie have the smallest flippers."),
                             h5("Low Standard Deviation"),
                             verbatimTextOutput("DescriptiveStatsFlipper"),
                             h3("Average of Culmen Length"),
                             h5("Chinstrap have the largest average culmen length, 
                                and Adelie have the smallest."),
                             h5("Low Standard Deviation"),
                           verbatimTextOutput("DescriptiveStatsCulmen_Length"),
                           h3("Average of Culmen Depth"),
                           h5("Chinstrap have the largest average culmen depth, 
                                and Gentoo have the smallest."),
                           h5("Low Standard Deviation"),
                           verbatimTextOutput("DescriptiveStatsCulmen_Depth"),
                           h3("Average of Body Mass"),
                           h5("Gentoo have greatest average body mass, 
                                Adelie are the smallest."),
                           h5("High Standard Deviation"),
                           verbatimTextOutput("DescriptiveStatsBodyMass"))),
                    br(),br(),
                    column(3,
                           tags$img(
                             style="padding-top: 80px;",
                             src = "culmen_depth.png",
                             width = 500,
                             alt = "Showing measurments"
                           ),
                           tags$figcaption("Artwork by @allison_horst")
                    )),
                ############ Correlation Plot ###########
                h1("Correlation Matrix",style = "color:lightcoral"),
                fluidRow(
                 h4("A correlation matrix helps summarize a large amount of data 
                   when we wish to see patterns in the data.",br(),br(),"Blue: Positive Correlation",br(),br(),
                             "Red: Negative Correlation"),
                  column(12,offset = 3,
                    plotOutput(outputId= "CorrPlot",  width = '700px', height = '800px'))),
                    h3("Summary",style = "color:mediumpurple"),
                       h4("The matrix is showing that",code("Flipper_Length"),"has high correlation with the following", 
                       code("Body_Mass"),",",code("Culmen_Length"),"and",code("Species_num")),
                h4("Other variables as well have correlation with others", code("Body_Mass"),"for example also", 
                   "has postive correlations with", code("Culmen_Length"),"and",code("Species_num")),
                h4("Something unexcepted was that", code("Culmen_Depth"),"has only negative correlations with all other variables"),
                h1("Moving from graphs",style = "color:deepskyblue"),
                h4("Looking at graphs are nice and stright foward to understand, but we need to examine correlation closer so 
                   it's time to look at numbers with the help of ANOVA.",br(),br(),"Lets make our way over to numbers!"),
column(12, offset = 3,HTML('<iframe class="embed-responsive-item" id="ytplayer" 
                           type="text/html" width="560" height="315" 
                           src="https://www.youtube.com/embed/-ceryY0q-ac?playlist=-ceryY0q-ac&autoplay=1&mute=1&loop=1" 
                           title="YouTube video player" frameborder="0" allow="accelerometer; loop = 1;autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                    )),
      ######## ANOVA ############
           tabPanel(title = "ANOVA",
                    fluidPage(style="padding-top: 90px;",
                      theme = bs_theme(version = 4, bootswatch = "minty"),
                      h1("Analysis of Variance",style = "color:lightcoral"),
                      h3("What is being tested?",style = "color:mediumpurple"),
                      h4("Recall that ANOVA is a statistical test to compare groups to see if they are significantly different"),
                      h4("For this project we want to test if",code("Flipper_Length"),",",code("Culmen_Length"),"and other numeric variables are different among the 3 penguins species."),
                      h3("Aim and Hypotheses of ANOVA",style = "color:mediumpurple"),
                      h4( "Aim: Are variables different for the 3 species of penguins?"),
                      h4( "Hypotheses",br(),"H0: The three species are equal in terms the variables.", 
                          br(),
                          "H1: One species is different from
                the other 2 species in terms of the different variables."),
                      h1("Checking Assumptions",style = "color:deepskyblue"),
                      h4("Before performing ANOVA we need to check that three assumption are met"),
                      h4("1. Normality" , br(),
                        "2. Equal Variances", br(),
                        "3. Independence",br(),
                        "If these assumptions aren't met then the results could be unreliable."),
                      h3("Testing Normality",style = "color:mediumpurple"),
                      h4("Normality is that each sample was drawn from a normally distributed population."),
                      h4("We can test this visually by using two plots, a histogram and a QQPlot"),
                      h4("The histogram shows a normality if it forms a bell curve 
                         which indicates the residuals follow a normal distribution."),
                      fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput(outputId = "HistFlipper"),
                                    plotOutput(outputId = "HistCulmenLength"))),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput(outputId = "HistCulmenDepth"),
                                  plotOutput(outputId = "HistBodyMass")),
                      h4("The QQ-plot checks normality by plotting the points, if the points
                      form a roughly straight line, and the points lie in the confidence interval.
                         This indicates that the sample has normal distribution."),
                      fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput(outputId = "QQPlotFlipper"),
                                    plotOutput(outputId = "QQPlotCulmenLength"))),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput(outputId = "QQPlotCulmenDepth"),
                                  plotOutput(outputId = "QQPlotBodyMass")),
                      h3("Variances are equal among groups",style = "color:mediumpurple"),
                      h4("Assuming the residuals follow a normal distribution we can check whether the
             variances are equal across species or not."),
                      h4("Using boxplots we can visual check equal variances. 
                         If the length of the boxplot is the same on both sides then we can assume equal variances."),
                      fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput(outputId = "BoxPlot"),
                                    plotOutput(outputId = "BoxPlotCulmen_Length"))),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput(outputId = "BoxPlotCulmen_Depth"),
                                  plotOutput(outputId = "BoxPlot_BodyMass")),
                      h1("ANOVA",style = "color:lightcoral"),
                      fluidRow(
                      column(6,h4(strong("F-statistic:"),"The larger the value the greater the variation 
                                  between sample means relative to the variation within the samples.", br(),br(),
                                  "The lowest value is from ",code("Body_Mass"), "
                                  at 343.63 the rest are higher. Thus we can say that there is a difference 
                                  for all variables between groups."),br(),
                             h4(strong("P-Value:"), "All variables have  p-values that are less than the significance level of 0.05.
                             This tells us that the difference between the group means is statistically significant.
                             Also that H0 hypothesis is rejected because we have proved at least one group was different.")),
                     column(6,verbatimTextOutput("anovaTable"))), 
                      h1("Tukey HSD test", style = "color:deepskyblue"),
                    fluidRow(
                     column(6,h4("We have showed that at least one group is different, but we want to know which ones are different.
                        To see which groups are different we compare groups 2 by 2.
                                 ",br(),"1. Chinstrap vs Adelie",br(),"2. Gentoo vs Adelie"
                                 ,br(),"3. Gentoo vs Chinstrap"),
                            h4(code("Pr(>|t|)"),"shows the adjusted p-value of each comparison. 
                               Like with ANOVA we want the p-values to be smaller than 0.05."),
                            h4("Most pass",strong("except"),code("Culmen_Depth_mm"), 
                               code("Body_Mass_g"),"for Chinstrap vs Adelie"),
                            tags$figure(
                              align = "center",
                              tags$img(
                                src = "palmerpenguins.png",
                                width = 400,
                                height = 400,
                                alt = "Icon of penguins"
                              ),
                              tags$figcaption("Artwork by @allison_horst")
                            )),
                      column(6,verbatimTextOutput("TukeyTest"))),
                      fluidRow(
                        h3("Plotting Tukey",style = "color:mediumpurple"),
                        splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput(outputId = "TukeyPlot"),
                                    plotOutput(outputId = "TukeyPlotCulmen_Length"))),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput(outputId = "TukeyPlotCulmen_Depth"),
                                  plotOutput(outputId = "TukeyPlot_BodyMass")),
                    column(12, offset = 4,
                           h3("Now with all the analysis done lets dive in!"),
                           HTML('<iframe width="560" height="315" 
                              src="https://www.youtube.com/embed/6-aQ7rKq_Ns?playlist=6-aQ7rKq_Ns&autoplay=1&loop=1&mute=1"
                                title="YouTube video player" frameborder="0" 
                                allow="accelerometer; autoplay; clipboard-write; 
                                encrypted-media; gyroscope; picture-in-picture; 
                                web-share" allowfullscreen></iframe>'))
                    ))),
############################ KNN ##########################
           tabPanel(title = "KNN",
                    fluidPage(style="padding-top: 80px;",
                      theme = bs_theme(version = 4, bootswatch = "minty"),
                      h1("K-Nearest Neighbords (KNN)",style = "color:lightcoral"),
                      h4("A non-parametric classification method for estimating the
                         likelihood that a data point will become a member of one group, or another based on
                         the nearest points."),
                      h3("Spliting Data",style = "color:mediumpurple"),
                      h4("Performing an 80-20 split allocating 
                         80% of the data is for training and 20% for test. 
                         We also scale the variables to reduce bias and improve predictive accuracy."),
                      verbatimTextOutput("SplitData"),
                      h3("Training",style = "color:mediumpurple"),
                      h4("k-fold cross-validation is performed on training data to 
 determine optimal parameter k for model."),
                      verbatimTextOutput("CrossVaildation"),
                      h4("k = 5 gives the most optimal model. Which be can shown by the plot
                         comparing the accuracy and the number of neighbors."),
                      plotOutput(outputId = "KNNAccuracy"), 
                      h3("Testing",style = "color:mediumpurple"),
                      h4("Now that we have trained the model lets test the algorithm!"),
                      verbatimTextOutput("KnnPredict"),
                      h4("The confusion matrix represents the prediction summary.
                         KNN performs quite well only misclassifying one penguin."),
                      tableOutput(outputId = "ConfusionMaxtrix"),
                      h3("Graphing",style = "color:mediumpurple"),  
                      plotOutput(outputId = "ConvexHull"),
                ########## ROC #############
                      h3("ROC Plot",style = "color:mediumpurple"),
                      h4("The ROC plot measures the functioning and results of the KNN algorithm."),
                      h4("True Postive(TP): The actual value and predicted value should be the same, 
    the value of cell 1 is the TP value"),verbatimTextOutput("TP"),
                      h4("False Positive (FP): The sum of values of the corresponding column except for the TP value."),
                      verbatimTextOutput("FP"),
                      h4("False Negative (FN): The sum of values of corresponding rows except for the TP value"),
                      verbatimTextOutput("FN"),
                      h4("True Negative (TN): The sum of values of all columns and rows except the values of that class 
    that we are calculating the values for."),verbatimTextOutput("TN"),
                      h4("True positive rate (TPR), and False positive rate FPR"),
                      verbatimTextOutput("TPR_FPR"), 
                      plotOutput(outputId = "ROCplot"),
                      )),
############## SVM ##################
          tabPanel(title = "SVM",
                   fluidPage(style="padding-top: 80px;",
                     theme = bs_theme(version = 4, bootswatch = "minty"),
                     h1("Support Vector Machines (SVM)",style = "color:deepskyblue"),
                     h4("SVM is a supervised machine learning classification algorithm. 
                        In SVM each point is plotted in n-dimensional space 
                        with the value of each feature being the value of a particular coordinate.
                        Then, classification is performed by finding the hyper-plane 
                        that best differentiates the classes. "),
                     h3("Split Data",style = "color:mediumpurple"),
                     h4("The same 80-20 training and testing data used in KNN is used here"),
                     verbatimTextOutput("SplitDataSVM"),
                     h3("Training",style = "color:mediumpurple"), 
                     h3("Given a labeled training data the algorithm outputs an optimal hyperplane that
                        categorizes the examples."),
                     verbatimTextOutput("TrainingSVM"),
                     plotOutput(outputId = "SVMGraphing"),
                     h3("Testing",style = "color:mediumpurple"),
                     h4("The confusion matrix represents the prediction summary.
                         SVM performs well only misclassifying two penguins."),
                     verbatimTextOutput("SVMConfMaxtrix"),
                     tableOutput(outputId = "SVMConfMaxtrixTable"),
                     h3("ROC",style = "color:mediumpurple"),
                     h4("True Postive(TP): The actual value and predicted value should be the same, 
    the value of cell 1 is the TP value"),verbatimTextOutput("SVMTP"),
                     h4("False Positive (FP): The sum of values of the corresponding column except for the TP value."),
                     verbatimTextOutput("SVMFP"),
                     h4("False Negative (FN): The sum of values of corresponding rows except for the TP value"),
                     verbatimTextOutput("SVMFN"),
                     h4("True Negative (TN): The sum of values of all columns and rows except the values of that class 
    that we are calculating the values for."),verbatimTextOutput("SVMTN"),
                     h4("True positive rate (TPR), and False positive rate FPR"),
                     verbatimTextOutput("SVMTPR_FPR"), 
                     plotOutput(outputId = "SVMROCplot"))),
tabPanel(title = "Results",
         fluidPage(style="padding-top: 80px;",
           theme = bs_theme(version = 4, bootswatch = "minty"),
           h1("Results", style = "color:lightcoral"),
           h4("We have used two different algorithms to 
              identify species of penguins. So lets compare which did the best!"),
           fluidRow(
          column(6, h3("KNN Results",style = "color:mediumpurple"),
           verbatimTextOutput("KNN4Results")),
           column(6, h3("SVM Results", style = "color:mediumpurple"),
           verbatimTextOutput("SVM4Results"))),
           fluidRow(
             column(width = 6, h3("KNN Confusion Matrix", style = "color:mediumpurple"),
           tableOutput(outputId = "ResultsKNNConfMaxtrixTable")),
           column(width = 6, h3("SVM Confusion Matrix", style = "color:mediumpurple"),
                  tableOutput(outputId = "ResultsSVMConfMaxtrixTable")),
          column(width = 6,
          plotOutput(outputId = "HeatKNNConfMatrix")),
          column(width = 6,
          plotOutput(outputId = "HeatSVMConfMatrix"))),
          h1("Conclusion", style = "color:mediumpurple"),
          h4("Looking at ",code("Accuracy"),",", code("Precision"),",", 
             code("Recall"), ",and", code("F1-Score"), "KNN performs better than SVM
             for this dataset. KNN misclassified one penguin while SVM misclassified two."),
          h4("This does not mean KNN is always better SVM. KNN performs 
             better if the training data is large than SVM. 
             SVM however performs better when there is less training data to relay upon.")
           )),
tabPanel(title = "End",
         fluidPage(style="padding-top: 80px;",
                   theme = bs_theme(version = 4, bootswatch = "minty"),
         fluidRow( style="padding-top: 200px;",
           column(12, offset = 5 ,h1("Thank You!",style = "color:mediumpurple"))
         ),
         fluidRow(style="padding-top: 100px;",
                  column(12, offset = 4,
         HTML('<iframe width="560" height="315" 
         src="https://www.youtube.com/embed/tKPv_2hicY0?playlist=tKPv_2hicY0&autoplay=1&loop=1&mute=1" 
        title="YouTube video player" frameborder="0" 
                                allow="accelerometer; autoplay; clipboard-write; 
                                encrypted-media; gyroscope; picture-in-picture; 
                                web-share" allowfullscreen></iframe>'))),
         fluidRow(style="padding-top: 100px;",
           column(6,offset = 5,h3("References")),
         h4("Ian_Muchi[R]i. 
            “Support Vector Machines.” RPubs, 2 Sept. 2020, https://rpubs.com/Ian_M/666939."),
         h4("“K-Nn Classifier in R Programming.” GeeksforGeeks, 
            GeeksforGeeks, 22 June 2020, 
            https://www.geeksforgeeks.org/k-nn-classifier-in-r-programming/."),
         h4("Allison Horst. “Palmerpenguins R Data Package.” 
            R Data Package •, 2020, https://allisonhorst.github.io/palmerpenguins/. "))))

                     ,inverse = TRUE, position = "fixed-top")

##################################### SERVER ###################################
server <- function(input, output) {
######### Intro ##################
  output$InfoAboutData <- renderPrint({
    print(head(penguins_raw))
  })
  output$NumbersOfColRow <- renderPrint({
    print(ncol(penguins_raw))
    print(nrow(penguins_raw))
    
  })
  output$InfoColNames <- renderPrint({
    print(colnames(penguins_raw))
  })
  output$SummaryInfo <- renderPrint({
    print(summary(penguins_raw))
  })
  #/////////////////// Cleaning data ///////////////////////////////////////#
  output$RemoveColumns <- renderPrint({
    cat("cleandata <- penguins_raw %>% dplyr::select(-c('studyName','Sample Number','Stage',
                                                   'Individual ID','Clutch Completion',
                                                   'Date Egg','Sex','Delta 15 N (o/oo)','Delta 13 C (o/oo)',
                                                   'Comments','Region'))",  sep = "\n")
  })
  output$RemoveNA <- renderPrint({
    cat("CleanestPenguins <- na.omit(cleandata)",  sep = "\n")
  })
  output$ChangeColNames <- renderPrint({
    cat("
    colnames(CleanestPenguins)[5] <- Flipper_Length_mm
    colnames(CleanestPenguins)[3] <- Culmen_Length_mm
    colnames(CleanestPenguins)[4] <- Culmen_Depth_mm
    colnames(CleanestPenguins)[6] <- Body_Mass_g",  sep = "\n")
  })
  output$DummyVar <- renderPrint({
   cat("CleanestPenguins$Island_num <- apply(CleanestPenguins, 1, 
                                         FUN = function(x) {
                                           if(x['Island']=='Torgersen') 
                                             x['Island_num']=1 
                                           else if(x['Island'']== 'Biscoe')
                                             x['Island_num']=0
                                           else
                                             x['Island_num']=-1
                                         }) ", sep = "\n")
    
    # Dummy for species
   cat("CleanestPenguins$Species_num <- apply(CleanestPenguins, 1, 
   FUN = function(x){
      if(x['Species']=='Gentoo')x['Species_num']=1 
      else if(x['Species']=='Adelie') x['Species_num']=0 
      else x['Species_num']=-1})", sep = "\n")
  })
############################## Count Plots ###################################
  ########## Bar Plot ############
  output$BarSpeciesCount <-  renderPlotly({
    ggplot(df,aes(x = Species, color = Species, fill = Species)) +
      geom_bar() + ggtitle("Species Count")
    
  })
  output$BarIslandCount <-  renderPlotly({
    plot_ly(data = speciesXisland, #pass in the data to be visualized
            x = ~Group.1 ~Group.2,
            y = ~x,
            color = ~Group.1,
            type = "bar",
            mode = NULL
    ) %>% layout(title ="Island & Species Count",
                 xaxis = list(title = "Species"),
                 yaxis = list(title = "Count")) 
    
  })
  ######### Averages #########
  output$DescriptiveStatsFlipper <- renderPrint({
    print(DescriptiveStatsFlipper)
  })
  output$DescriptiveStatsCulmen_Length <- renderPrint({
    print(DescriptiveStatsCulmen_Length)
  })
  output$DescriptiveStatsCulmen_Depth <- renderPrint({
    print(DescriptiveStatsCulmen_Depth)
  })
  output$DescriptiveStatsBodyMass <- renderPrint({
    print(DescriptiveStatsBodyMass)
  })
  ##### Corrlation Plot ###########
  output$CorrPlot <- renderPlot({
    corrplot(df_cor, type = "upper",order = "hclust")
  })
########################//// ANOVA ////####################################  
  ################# Histogram ################################
  output$HistFlipper <- renderPlot({
    hist(res_aovFlipper$residuals) 
  })
  output$HistCulmenLength <- renderPlot({
    hist(res_aovCulmen_Length$residuals) 
  })
  output$HistCulmenDepth <- renderPlot({
    hist(res_aovCulmen_Depth$residuals)
  })
  output$HistBodyMass <- renderPlot({
    hist(res_aovBodyMass$residuals) 
  })
  ############ QQPlot #############################
  output$QQPlotFlipper <- renderPlot({
    # Q-Q-plots roughly follow straight line and most of them within the confidence bands
    # indicating that residuals follow a normal distribution
    qqPlot(res_aovFlipper$residuals, main = "Flipper_Length_residuals",
           id = FALSE # id = FALSE to remove point identification
    )
  })
  output$QQPlotCulmenLength <- renderPlot({
    # Q-Q-plots roughly follow straight line and most of them within the confidence bands
    # indicating that residuals follow a normal distribution
    qqPlot(res_aovCulmen_Length$residuals, main = "Culmen_Length_residuals",
           id = FALSE # id = FALSE to remove point identification
    )
  })
  output$QQPlotCulmenDepth <- renderPlot({
    # Q-Q-plots roughly follow straight line and most of them within the confidence bands
    # indicating that residuals follow a normal distribution
    qqPlot(res_aovCulmen_Depth$residuals, main = "Culmen_Dpeth_residuals",
           id = FALSE # id = FALSE to remove point identification
    )
  })
  output$QQPlotBodyMass <- renderPlot({
    # Q-Q-plots roughly follow straight line and most of them within the confidence bands
    # indicating that residuals follow a normal distribution
    qqPlot(res_aovBodyMass$residuals, main = "Body_Mass_residuals",
           id = FALSE # id = FALSE to remove point identification
    )
  })
  ############ BoxPlot #########################
  output$BoxPlot <- renderPlot({
    ggplot(df) +
      aes(x = Species, y = Flipper_Length_mm, fill= Species) +
      geom_boxplot() + ggtitle("Flipper_Length") 
  })
  
  output$BoxPlotCulmen_Length <- renderPlot({ 
    ggplot(df) +
      aes(x = Species, y = Culmen_Length_mm, fill= Species) +
      geom_boxplot() + ggtitle("Culmen_Length") 
  })
  
  output$BoxPlotCulmen_Depth  <- renderPlot({
    ggplot(df) +
      aes(x = Species, y = Culmen_Depth_mm, fill= Species) +
      geom_boxplot() + ggtitle("Culmen_Depth")
  })
  
  output$BoxPlot_BodyMass <- renderPlot({
    ggplot(df) +
      aes(x = Species, y = Body_Mass_g, fill= Species) +
      geom_boxplot() + ggtitle("Body_Mass_g")
  })
  ######### Anova Table ############
  output$anovaTable <- renderPrint({
    cat("dep_vars <- cbind(df$Flipper_Length_mm, df$Culmen_Length_mm, df$Culmen_Depth_mm, df$Body_Mass_g)
res_aov <- aov(dep_vars ~ Species, data = dat)", sep = "\n")
    print(summary(res_aov))
  })
  ################ Tuckey Test ##################
  output$TukeyTest <- renderPrint({
    print(post_testFlipper)
    print(post_testCulmen_Length)
    print(post_testCulmen_Depth)
    print(post_testBodyMass)
  })
  #### Tukey Test graphing
  output$TukeyPlot <- renderPlot(plot(post_testFlipper))
  
  output$TukeyPlotCulmen_Length <- renderPlot({ 
    plot(post_testCulmen_Length)
  })
  
  output$TukeyPlotCulmen_Depth  <- renderPlot({
    plot(post_testCulmen_Depth)
  })
  
  output$TukeyPlot_BodyMass <- renderPlot({
    plot(post_testBodyMass)
  })
  
  ######################### END OF ANOVA #########################
  ########################### KNN ########################################
  output$SplitData <- renderPrint({
    cat("
  standardized = cbind( df[,1], scale( df[, 3:6] ))
  Data is split 80% - 20%
        training.individuals = createDataPartition(standardized$Species, p= 0.8 , list = FALSE)", sep = "\n")
    cat("
train.X  =  standardized[training.individuals,  2:5]
test.X   =  standardized[-training.individuals,  2:5]" , sep = "\n")
    cat("
train.Y  =  standardized[ training.individuals,    1]
test.Y   =  standardized[-training.individuals,    1]")
  })
  output$CrossVaildation <- renderPrint({
    print(knn.model)
    ###### Graph of accuracy and number of neighbors
    output$KNNAccuracy <- renderPlot(plot(knn.model))
  })
  output$KnnPredict <- renderPrint({
    cat("knnPredict <- predict(knn.model, newdata = test.X)", sep = "\n")
    table(knnPredict,test.Y)
  })
  output$ConfusionMaxtrix <- renderTable(table(knnPredict,test.Y))
  output$TP <- renderPrint({
    cat("tp <- diag(knn.confmatrix)",sep = "\n")
  })
  output$FP <- renderPrint({
    cat("fp <- colSums(knn.confmatrix) - knn.tp",sep = "\n")
  })
  output$FN <- renderPrint({
    cat("fn <- rowSums(knn.confmatrix) - knn.tp",sep = "\n")
  })
  output$TN <- renderPrint({
    cat("tn <- sum(knn.confmatrix) - (knn.tp + knn.fp + knn.fn)",sep = "\n")
  })
  output$TPR_FPR <- renderPrint({
    cat("knn.tpr = knn.tp / (knn.tp + knn.fn)" ,sep = "\n")
    print(knn.tpr)
    cat("knn.fpr = knn.fp / (knn.fp + knn.tn)",sep = "\n")
    print(knn.fpr)
  })
  output$ROCplot <- renderPlot({
    roc_data <- data.frame(knn.tpr = c(0, knn.tpr, 1), knn.fpr = c(0, knn.fpr, 1))
   ggplot(roc_data, aes(x = knn.fpr, y = knn.tpr)) + geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)",
          title = "ROC Curve")
  })
  ######################### Plotting KNN ###################
  output$ConvexHull <- renderPlot({
    ggplot(data=lgrid) + stat_contour(aes(x=Culmen_Length_mm, y= Culmen_Depth_mm, z = num_knnPredGrid), bins = 2 ) +
      geom_point( aes(x=Culmen_Length_mm, y=Culmen_Depth_mm, color = knnPredGrid),  size = 1, shape = 19, alpha = 0.2) +
      geom_point( data= test , aes(x=Culmen_Length_mm, y=Culmen_Depth_mm, color= knnPredict) , size = 4, alpha = 0.8, shape =24 ) +
      ggtitle("KNN Decision Boundary for Penguins Data with k=5 neighbor")
    
  })
############################# SVM ###########################################
  output$SplitDataSVM <- renderPrint({
    cat("#80% training set
        svm.training <- df[ training.individuals,]" ,sep = "\n")
  
  
  cat("#20% test set
      svm.testing  <- df[-training.individuals,]" ,sep = "\n")
})
output$TrainingSVM <- renderPrint({
  cat("
  fit_control <- trainControl(## 15-fold CV
  method = 'repeatedcv',
  number = 15,
  ## repeated ten times
  repeats = 15,
  classProbs=TRUE)

svm <- train(Species ~ .,
             data = svm.training, 
             method = 'svmRadial', 
             trControl = fit_control,
             metric = 'Accuracy')", sep = "\n")
  print(svm)
  cat("svm.pred <-  predict(svm, svm.testing)",sep = "\n")
  ## SVM Graphing
  output$SVMGraphing <- renderPlot(ggplot(svm.testing, aes(x = Species, y = svm.pred, color = Species)) +
    geom_jitter(size = 3))
  })
output$SVMConfMaxtrix <- renderPrint({
  print(svm.confmatrix)
})
output$SVMConfMaxtrixTable<- renderTable(table(svm.pred, svm.testing$Species))
output$SVMTP <- renderPrint({
  cat("svm.tp <- diag(svm.confmatrix)",sep = "\n")
})
output$SVMFP <- renderPrint({
  cat("svm.fp <- colSums(svm.confmatrix) - svm.tp",sep = "\n")
})
output$SVMFN <- renderPrint({
  cat("svm.fn <- rowSums(svm.confmatrix) - svm.tp",sep = "\n")
})
output$SVMTN <- renderPrint({
  cat("svm.tn <- sum(svm.confmatrix) - (svm.tp + svm.fp + svm.fn)",sep = "\n")
})
output$SVMTPR_FPR <- renderPrint({
  cat("svm.tpr = svm.tp / (svm.tp + svm.fn)" ,sep = "\n")
  print(svm.tpr)
  cat("svm.fpr = svm.fp / (svm.fp + svm.tn)",sep = "\n")
  print(svm.fpr)
})

#### ROC ####
output$SVMROCplot <- renderPlot({
  roc_data <- data.frame(svm.tpr = c(0, svm.tpr, 1), svm.fpr = c(0, svm.fpr, 1))
  ggplot(roc_data, aes(x = svm.fpr, y = svm.tpr)) + geom_line() + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)",
         title = "ROC Curve")
})
################### Results ############################
########### 4 Results #########
output$SVM4Results <- renderPrint({
  # Accuracy 
  cat("svm.Accuracy <- sum(svm.tp + svm.tn)/sum(svm.tp + svm.tn + svm.fp + svm.fn)", sep = "\n")
  print(svm.Accuracy)
  # Precision 
  cat(svm.Precision <- sum(svm.tp)/sum(svm.tp + svm.fp), sep = "\n")
  print(svm.Precision)
  # Recall 
  cat("svm.Recall <- sum(svm.tp)/sum(svm.tp + svm.fn)", sep = "\n")
  print(svm.Recall)
  ## F1-score
  cat("svm.F1Score <- 2*(svm.Precision * svm.Recall)/(svm.Precision + svm.Recall)", sep = "\n")
  print(svm.F1Score)
})
########### 4 Results #########
output$KNN4Results <- renderPrint({
  # Accuracy 
  cat("knn.Accuracy <- sum(knn.tp + knn.tn)/sum(knn.tp + knn.tn + knn.fp + knn.fn)", sep = "\n")
  print(knn.Accuracy)
  # Precision 
  cat("knn.Precision <- sum(knn.tp)/sum(knn.tp + knn.fp)",sep = "\n")
  print(knn.Precision)
  # Recall 
  cat("knn.Recall <- sum(knn.tp)/sum(knn.tp + knn.fn)", sep = "\n")
  print(knn.Recall)
  ## F1-score
  cat("knn.F1Score <- 2*(knn.Precision * knn.Recall)/(knn.Precision + knn.Recall)", sep = "\n")
  print(knn.F1Score)
})
output$ResultsSVMConfMaxtrixTable<- renderTable({
  table(svm.pred, svm.testing$Species)
    })
output$ResultsKNNConfMaxtrixTable<- renderTable({
  table(knnPredict, test.Y)
})
############# HeatMap ############
output$HeatKNNConfMatrix <- renderPlot({
  ################# KNN Heatmap
  actual = as.data.frame(table(test.Y))
  names(actual) = c("Actual","ActualFreq")
  #build confusion matrix
  confusion = as.data.frame(table(test.Y, knnPredict))
  names(confusion) = c("Actual","Predicted","Freq")
  
  #calculate percentage of test cases based on actual frequency
  confusion = merge(confusion, actual, by=c('Actual'))
  confusion$Percent = confusion$Freq/confusion$ActualFreq*100
  KNNtile <- ggplot() +
    geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
    labs(x="Actual",y="Predicted")
  KNNtile = KNNtile + 
    geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
    scale_fill_gradient(low="dodgerblue",high="firebrick")
  
  # lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
  KNNtile = KNNtile + 
    geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 
  
  #render
  KNNtile
})
output$HeatSVMConfMatrix <- renderPlot({
  ################# SVM HeatMap
  actual = as.data.frame(table(svm.testing$Species))
  names(actual) = c("Actual","ActualFreq")
  #build confusion matrix
  confusion = as.data.frame(table(svm.testing$Species, svm.pred))
  names(confusion) = c("Actual","Predicted","Freq")
  
  #calculate percentage of test cases based on actual frequency
  confusion = merge(confusion, actual, by=c('Actual'))
  confusion$Percent = confusion$Freq/confusion$ActualFreq*100
  
  SVMtile <- ggplot() +
    geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
    labs(x="Actual",y="Predicted")
  SVMtile = SVMtile + 
    geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
    scale_fill_gradient(low="dodgerblue",high="firebrick")
  
  # lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
  SVMtile = SVMtile + 
    geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 
  
  #render
  SVMtile
})

}

shinyApp(ui,server)


#  In results put Conf Matrix side by side or all in one table
#  OR Make a conf matrix with heat  DONE

# Fix Correlation Spelling error DONE
# Move KNN graph before ROC DONE
###### Add all the text explaining KNN and SVM
## Talk about how SVM on average performs better than KNN
## Add Diagram to Intro slide at bottom     DONE

# 

##### Slides
# Prashnim : Intro 
# Kelly: Data Cleaning
# Kelly: Count and Correlation
# Prashnim: ANOVA
# Prashnim : KNN
# Kelly: SVM
# Kelly: Results