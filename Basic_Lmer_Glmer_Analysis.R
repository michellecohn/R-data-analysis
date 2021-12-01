##########################################################
#
# Basic lme4 analysis
#
# Mixed effects modeling & plotting
#
#  (1) Logistic regression (binomial dependent variable)
#  (2) Linear regression (continuous dependent variable)
#
# Reads in data from a .csv or .txt file
# Assumes data is in a long format (vs. 'wide')
#
# MC 12/1/21
##########################################################

# Load in the packages 
#(if you don't have them already, uncomment and run the following lines)
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("ggplot2")

library(lme4)
library(lmerTest)
library(ggplot2)

# Load in your data (if .csv)
data= read.csv("my_data.csv", header = TRUE)

# (If your data is tab separated (i.e., .txt))
#data= read.csv("my_data.csv", header = TRUE, sep = "\t)

########## Define factor levels ###########
dat$Condition = factor(dat$Condition, levels = c("A", "B"))
dat$Gender = factor(dat$Gender, levels = c("Female", "Male", "Nonbinary"))

######### Have any continuous variable predictors? #####
# Need to center them (and possibly scale as well)
dat$Age.centered = dat$Age - mean(dat$Age.centered)

######## Decide whether you want sum coding (shows difference from grand mean)
options(contrasts = rep("contr.sum", 2)) # set contrasts

######### or contrast coding (shows differences from 1 level of a factor)
#options (contrasts = rep("contr.treatment", 2))


##################################################################
################# For a binomial dependent variable ##############
################# e.g., Accurate vs. Inaccurate; 0 or 1 ##########
##################################################################

glmer_model =glmer(Accuracy~Condition*Gender +  (1 + Condition | Subject) + (1|Word)
                           , binomial, data=dat, control=glmerControl(optimizer="bobyqa"))
summary(glmer_model)


###### Plotting!
accuracy_summarized <- ddply(dat,.(Condition, Gender),summarize,
                            Mean=mean(na.omit(Accuracy)),
                            Err=sqrt(var(na.omit(Accuracy))/length(na.omit(Accuracy))))

accuracy_plot <- ggplot(mask.data.combined, aes(x=Condition, y=Mean, shape=Gender, group=Gender, color=Gender))+
  geom_point(size=6)+
  geom_line(aes(linetype=Gender), size=.9)+
  geom_errorbar(aes(ymin=Mean-Err, ymax=Mean+Err), width=.2)+
  ylab("Word Identification Accuracy by Condition & Gender") + 
  scale_color_manual(values=c("orange", "deepskyblue3", ""))+ 
 # facet_wrap(~SNR*Block)
accuracy_plot+theme_bw()+theme(text=element_text(size=21))+theme(axis.title.x = element_blank())+ theme(legend.title=element_blank())#+theme(legend.position=c(.2, .9))#+facet_wrap(~speaker)


##################################################################
################# For a continuous dependent variable ############
################# e.g., ages (18-30), measurement (0.100-0.650ms)#
##################################################################
# If you have a continous predictor (here, Age.centered), also center your dependent variable

lmer_model = lmer(height.centered ~ Age.centered*Condition + (1+ Condition|Subject)+ (1| Word), 
                  data = dat, na.action = na.omit)
summary(lmer_model)


###### Plotting!
height_summarized <- ddply(dat,.(Age, Condition),summarize,
                Mean=mean(height, na.rm = TRUE),
                Err=sqrt(var(na.omit(height))/length(na.omit(height))))

height_plot <- ggplot(height_summarized, aes(x=Age, y=Mean, color = Condition, group = Condition, shape =Condition))+
  geom_point(size=2)+
  ggtitle("Height by age & condition")+
  xlab("Age")+
  ylab("Height")+
  scale_color_manual(values=c("orange",  "deepskyblue3")) +
  geom_smooth(method = "lm", se = TRUE, fullrange=FALSE, level=0.95, aes(linetype = Condition), size = 1) #+ 
height_plot + theme_bw()+theme(text=element_text(size=14))




