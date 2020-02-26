# Clears workspace:  
rm (list = ls(all = TRUE)) 
# Sets working directories:
setwd("") # Insert your working directory 
# Load packages:
library(tidyverse)
library(ggpubr)
library(BEST)
library(lsr)
# Part 1: Load data ------------------
# All data
everyone <- read.csv("/explain-intervention/data/anonymized.csv", header = T) 
everyone$X <- NULL

# Subsetting 
## Children
children <- everyone[which(everyone$group=='children'),] # child data

children_base <- children[which(children$condition=='baseline'),]
children_base$theta <- EIG_PTS_children_base$BUGSoutput$mean$theta
children_base$tau <- EIG_PTS_children_base$BUGSoutput$mean$tau

children_exp <- children[which(children$condition=='explanation'),]
children_exp$theta <- EIG_PTS_children_exp$BUGSoutput$mean$theta
children_exp$tau <- EIG_PTS_children_exp$BUGSoutput$mean$tau

children_rep <- children[which(children$condition=='report'),]
children_rep$theta <- EIG_PTS_children_rep$BUGSoutput$mean$theta
children_rep$tau <- EIG_PTS_children_rep$BUGSoutput$mean$tau

# theta & stuff
acc_theta <- lm(data=children_base, ave_acc ~ theta)
summary(acc_theta)

acc_theta <- lm(data=children_exp, ave_acc ~ theta)
summary(acc_theta)

acc_theta <- lm(data=children_rep, ave_acc ~ theta)
summary(acc_theta)
## Adults
adults <- everyone[which(everyone$group=='adults'),] # adult data  
adults_base <- adults[which(adults$condition=='baseline'),]
adults_exp <- adults[which(adults$condition=='explanation'),]
adults_rep <- adults[which(adults$condition=='report'),]

# Part 2: Demographics ------------------
# Age (children: months; adults: years)
## Mean age
with(everyone, tapply(age,
                   list(Group=group,
                        Condition=condition),
                   mean, na.rm=TRUE))
## Min age
with(everyone, tapply(age,
                      list(Group=group,
                           Condition=condition),
                      min, na.rm=TRUE))
## Max age
with(everyone, tapply(age,
                      list(Group=group,
                           Condition=condition),
                      max, na.rm=TRUE))

# Gender (# of females)
with(everyone, tapply(gender=="F",
                      list(Group=group,
                           Condition=condition),
                      sum, na.rm=TRUE))

# Part 3: Raw EIG & PTS ------------------
# Q1: Did learners identify the correct causal structures in the end?
## Summary
### Table
with(everyone, tapply(ave_acc,
                      list(Condition=condition,
                           Group=group),
                      mean, na.rm=TRUE))

with(everyone, tapply(ave_acc,
                      list(Condition=condition,
                           Group=group),
                      sd, na.rm=TRUE))

with(everyone, tapply(ave_acc,
                      list(Condition=condition,
                           Group=group),
                      max, na.rm=TRUE))

with(everyone, tapply(ave_acc,
                      list(Group=group,
                           Condition=condition),
                      min, na.rm=TRUE))

### Plot
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

accuracy <- summarySE(everyone, measurevar="ave_acc", groupvars=c("group","condition"))

p <- ggplot(accuracy, aes(x=group, y=ave_acc, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity", 
           colour = "black",
           size = .3) +
  geom_errorbar(aes(ymin=ave_acc-se, ymax=ave_acc+se),
                size = .3,
                width =.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  geom_hline(yintercept= .5, linetype="dashed", color = "red") +
  xlab("group") +
  ylab("mean accuracy") +  
  theme_bw()

p <- p + theme(text = element_text(size=40))

## Compared to chance
t.test(children_base$ave_acc, mu=.5) # At chance
cohensD(children_base$ave_acc, mu=.5) 
t.test(children_exp$ave_acc, mu=.5)
t.test(children_rep$ave_acc, mu=.5)
t.test(adults_base$ave_acc, mu=.5)
t.test(adults_exp$ave_acc, mu=.5)
t.test(adults_rep$ave_acc, mu=.5)

children_base$theta <- EIG_PTS_children_base$BUGSoutput$mean$theta
children_exp$theta <- EIG_PTS_children_exp$BUGSoutput$mean$theta
children_rep$theta <- EIG_PTS_children_rep$BUGSoutput$mean$theta

library(psych)

r_base <- cor(children_base$theta, children_base$ave_acc)
n_base <- nrow(children_base)

r_exp <- cor(children_exp$theta, children_exp$ave_acc)
n_exp <- nrow(children_exp)

r_rep <- cor(children_rep$theta, children_rep$ave_acc)
n_rep <- nrow(children_rep)

paired.r(r_base,r_exp,NULL, n_base, n_exp)
paired.r(r_base,r_rep,NULL, n_base, n_rep)
paired.r(r_rep,r_exp,NULL, n_rep, n_exp)

t.test(children_rep$ave_acc, children_base$ave_ac)
t.test(children_exp$ave_acc, children_base$ave_ac)
cohensD(children_rep$ave_acc, children_base$ave_ac)
## Condition differences
condition.mod <- lm(ave_acc ~ group * condition, data = children)
summary(condition.mod)

# plots
install.packages("ggpubr")
library(ggpubr)

# Default method = "kruskal.test" for multiple groups
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means()

# Change method to anova
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means(method = "anova")

children_acc <- children[ , c("condition","ave_acc")]
children_acc$condition <- factor(children_acc$condition, levels=c("baseline", "explanation", "report"), labels=c("Baseline", "Explanation", "Report"))

# Visualize: Specify the comparisons you want
font_size <- 12
my_comparisons <- list( c("Baseline", "Explanation"), c("Baseline", "Report"), c("Explanation", "Report"))

p <- ggboxplot(children_acc, x = "condition", y = "ave_acc",
          color = "condition", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..)) +
  stat_compare_means(size = font_size)

# Add global p-value
p <- p + xlab("Condition") + ylab("Proportion of correct choices")+
  theme(text = element_text(size=40), legend.position="none")
p$layers[[2]]$aes_params$textsize <- font_size
p
# Part 3: Model comparison ------------------
# Load the models
load("IG_children_base.rda")
load("PTS_children_base.rda")
load("rand_children_base.rda")
load("EIG_PTS_children_base.rda")
load("PTS_rand_children_base.rda")

load("IG_children_exp.rda")
load("PTS_children_exp.rda")
load("rand_children_exp.rda")
load("EIG_PTS_children_exp.rda")
load("PTS_rand_children_exp.rda")

load("IG_children_rep.rda")
load("PTS_children_rep.rda")
load("rand_children_rep.rda")
load("EIG_PTS_children_rep.rda")
load("PTS_rand_children_rep.rda")

load("IG_adults_base.rda")
load("PTS_adults_base.rda")
load("rand_adults_base.rda")
load("EIG_PTS_adults_base.rda")
load("PTS_rand_adults_base.rda")

load("IG_adults_exp.rda")
load("PTS_adults_exp.rda")
load("rand_adults_exp.rda")
load("EIG_PTS_adults_exp.rda")
load("PTS_rand_adults_exp.rda")

load("IG_adults_rep.rda")
load("PTS_adults_rep.rda")
load("rand_adults_rep.rda")
load("EIG_PTS_adults_rep.rda")
load("PTS_rand_adults_rep.rda")

# DIC
IG_children_base$BUGSoutput$DIC
PTS_children_base$BUGSoutput$DIC
rand_children_base$BUGSoutput$DIC
EIG_PTS_children_base$BUGSoutput$DIC
PTS_rand_children_base$BUGSoutput$DIC

IG_children_exp$BUGSoutput$DIC
PTS_children_exp$BUGSoutput$DIC
rand_children_exp$BUGSoutput$DIC
EIG_PTS_children_exp$BUGSoutput$DIC
PTS_rand_children_exp$BUGSoutput$DIC

IG_children_rep$BUGSoutput$DIC
PTS_children_rep$BUGSoutput$DIC
rand_children_rep$BUGSoutput$DIC
EIG_PTS_children_rep$BUGSoutput$DIC
PTS_rand_children_rep$BUGSoutput$DIC

IG_adults_base$BUGSoutput$DIC
PTS_adults_base$BUGSoutput$DIC
rand_adults_base$BUGSoutput$DIC
EIG_PTS_adults_base$BUGSoutput$DIC
PTS_rand_adults_base$BUGSoutput$DIC

IG_adults_exp$BUGSoutput$DIC
PTS_adults_exp$BUGSoutput$DIC
rand_adults_exp$BUGSoutput$DIC
EIG_PTS_adults_exp$BUGSoutput$DIC
PTS_rand_adults_exp$BUGSoutput$DIC

IG_adults_rep$BUGSoutput$DIC
PTS_adults_rep$BUGSoutput$DIC
rand_adults_rep$BUGSoutput$DIC
EIG_PTS_adults_rep$BUGSoutput$DIC
PTS_rand_adults_rep$BUGSoutput$DIC

# EIG's weight theta
mean(EIG_PTS_children_base$BUGSoutput$mean$theta)
mean(EIG_PTS_children_exp$BUGSoutput$mean$theta)
mean(EIG_PTS_children_rep$BUGSoutput$mean$theta)

mean(EIG_PTS_adults_base$BUGSoutput$mean$theta)
mean(EIG_PTS_adults_exp$BUGSoutput$mean$theta)
mean(EIG_PTS_adults_rep$BUGSoutput$mean$theta)

# Differences in theta 
load("children_base_mu.Rdata")
load("children_exp_mu.Rdata")
load("children_rep_mu.Rdata")
load("adults_base_mu.Rdata")
load("adults_exp_mu.Rdata")
load("adults_rep_mu.Rdata")


Report <- adults_rep_mu
Explanation <- adults_exp_mu
Baseline <- adults_base_mu

## Plot
df <- data.frame(Baseline, Explanation, Report)
dfs <- stack(df)
is.factor(dfs[,2])

cdat <- ddply(dfs, "ind", summarise, rating.mean=mean(values))
cdat

df <- data.frame(Baseline, Explanation, Report)
dfs <- stack(df)
is.factor(dfs[,2])

cdat <- ddply(dfs, "ind", summarise, rating.mean=mean(values))
cdat

ggplot(dfs, aes(x=values)) + 
  geom_density(aes(group=ind, colour=ind), alpha=0.3, size=1.5)+
  xlab(expression(paste("Mean of", " ", theta))) + ylab("Density")+
  theme(text = element_text(size=55))+
  scale_color_manual(values=c("#003262", "#FDB515", "#8c1515"),name="Condition")

## Compare mean of theta 
### Step 1: take 10,000 samles from each model 
samp_exp <- sample(Explanation, 10000, replace = FALSE, prob = NULL)
samp_rep <- sample(Report, 10000, replace = FALSE, prob = NULL)
samp_base <- sample(Baseline, 10000, replace = FALSE, prob = NULL)
### Step 2: (pair samples randomly and) compute the difference
diff_exp_rep <- samp_exp - samp_rep
diff_exp_base <- samp_exp - samp_base
diff_rep_base <- samp_rep - samp_base
### Step 3: compute the 95% Highest Density Interval (HDI) 
hdi(diff_exp_rep, prob = 0.95, trans = NULL) # this 95% HDI includes 0: no credible difference
hdi(diff_exp_base, prob = 0.95, trans = NULL) # this 95% HDI includes 0: no credible difference
hdi(diff_rep_base, prob = 0.95, trans = NULL) # this 95% HDI includes 0: no credible difference

# What mistakes did learners make?
## Recode intervention choices
everyone$p1[everyone$p1 == 1] <- "n1"
everyone$p1[everyone$p1 == 2] <- "n2"
everyone$p1[everyone$p1 == 3] <- "n3"
everyone$p3[everyone$p3 == 1] <- "n1"
everyone$p3[everyone$p3 == 2] <- "n2"
everyone$p3[everyone$p3 == 3] <- "n3"
everyone$p5[everyone$p5 == 1] <- "n1"
everyone$p5[everyone$p5 == 2] <- "n2"
everyone$p5[everyone$p5 == 3] <- "n3"
everyone$p2[everyone$p2 == 1] <- "n1"
everyone$p2[everyone$p2 == 2] <- "n3"
everyone$p2[everyone$p2 == 3] <- "n2"
everyone$p4[everyone$p4 == 1] <- "n1"
everyone$p4[everyone$p4 == 2] <- "n3"
everyone$p4[everyone$p4 == 3] <- "n2"
everyone$p6[everyone$p6 == 1] <- "n1"
everyone$p6[everyone$p6 == 2] <- "n3"
everyone$p6[everyone$p6 == 3] <- "n2"

# Type 1: Chain vs. Chain (EIG: n2; PTS: n1)
adults_base_type1 <- c(adults_base$p1, adults_base$p2)
adults_base_type1 <- as.factor(adults_base_type1)
summary(adults_base_type1)/length(adults_base_type1)*100

adults_exp_type1 <- c(adults_exp$p1, adults_exp$p2)
adults_exp_type1 <- as.factor(adults_exp_type1)
summary(adults_exp_type1)/length(adults_exp_type1)*100

adults_rep_type1 <- c(adults_rep$p1, adults_rep$p2)
adults_rep_type1 <- as.factor(adults_rep_type1)
summary(adults_rep_type1)/length(adults_rep_type1)*100

children_base_type1 <- c(children_base$p1, children_base$p2)
children_base_type1 <- as.factor(children_base_type1)
summary(children_base_type1)/length(children_base_type1)*100

children_exp_type1 <- c(children_exp$p1, children_exp$p2)
children_exp_type1 <- as.factor(children_exp_type1)
summary(children_exp_type1)/length(children_exp_type1)*100

children_rep_type1 <- c(children_rep$p1, children_rep$p2)
children_rep_type1 <- as.factor(children_rep_type1)
summary(children_rep_type1)/length(children_rep_type1)*100

# Type 2: Chain vs.One-Link (EIG: n1; PTS: n1 = n2)
adults_base_type2 <- c(adults_base$p3, adults_base$p4)
adults_base_type2 <- as.factor(adults_base_type2)
summary(adults_base_type2)/length(adults_base_type2)*100

adults_exp_type2 <- c(adults_exp$p3, adults_exp$p4)
adults_exp_type2 <- as.factor(adults_exp_type2)
summary(adults_exp_type2)/length(adults_exp_type2)*100

adults_rep_type2 <- c(adults_rep$p3, adults_rep$p4)
adults_rep_type2 <- as.factor(adults_rep_type2)
summary(adults_rep_type2)/length(adults_rep_type2)*100

children_base_type2 <- c(children_base$p3, children_base$p4)
children_base_type2 <- as.factor(children_base_type2)
summary(children_base_type2)/length(children_base_type2)*100

children_exp_type2 <- c(children_exp$p3, children_exp$p4)
children_exp_type2 <- as.factor(children_exp_type2)
summary(children_exp_type2)/length(children_exp_type2)*100

children_rep_type2 <- c(children_rep$p3, children_rep$p4)
children_rep_type2 <- as.factor(children_rep_type2)
summary(children_rep_type2)/length(children_rep_type2)*100

# Type 3: Common Effect vs.One-Link (EIG: n2; PTS: n3 > n2)
adults_base_type3 <- c(adults_base$p5, adults_base$p6)
adults_base_type3 <- as.factor(adults_base_type3)
summary(adults_base_type3)/length(adults_base_type3)*100

adults_exp_type3 <- c(adults_exp$p5, adults_exp$p6)
adults_exp_type3 <- as.factor(adults_exp_type3)
summary(adults_exp_type3)/length(adults_exp_type3)*100

adults_rep_type3 <- c(adults_rep$p5, adults_rep$p6)
adults_rep_type3 <- as.factor(adults_rep_type3)
summary(adults_rep_type3)/length(adults_rep_type3)*100

children_base_type3 <- c(children_base$p5, children_base$p6)
children_base_type3 <- as.factor(children_base_type3)
summary(children_base_type3)/length(children_base_type3)*100

children_exp_type3 <- c(children_exp$p5, children_exp$p6)
children_exp_type3 <- as.factor(children_exp_type3)
summary(children_exp_type3)/length(children_exp_type3)*100

children_rep_type3 <- c(children_rep$p5, children_rep$p6)
children_rep_type3 <- as.factor(children_rep_type3)
summary(children_rep_type3)/length(children_rep_type3)*100

# cor: theta & acc
adults_base$theta <- EIG_PTS_adults_base$BUGSoutput$mean$theta
cor.test(adults_base$theta, adults_base$ave_acc)

adults_exp$theta <- EIG_PTS_adults_exp$BUGSoutput$mean$theta
cor.test(adults_exp$theta, adults_exp$ave_acc)

adults_rep$theta <- EIG_PTS_adults_rep$BUGSoutput$mean$theta
cor.test(adults_rep$theta, adults_rep$ave_acc)

children_base$theta <- EIG_PTS_children_base$BUGSoutput$mean$theta
cor.test(children_base$theta, children_base$ave_acc)

children_exp$theta <- EIG_PTS_children_exp$BUGSoutput$mean$theta
cor.test(children_exp$theta, children_exp$ave_acc)

children_rep$theta <- EIG_PTS_children_rep$BUGSoutput$mean$theta
cor.test(children_rep$theta, children_rep$ave_acc)
# cor: tau & theta
adults_base$tau <- EIG_PTS_adults_base$BUGSoutput$mean$tau
cor.test(adults_base$theta, adults_base$tau)

adults_exp$tau <- EIG_PTS_adults_exp$BUGSoutput$mean$tau
cor.test(adults_exp$theta, adults_exp$tau)

adults_rep$tau <- EIG_PTS_adults_rep$BUGSoutput$mean$tau
cor.test(adults_rep$theta, adults_rep$tau)


children_base$tau <- EIG_PTS_children_base$BUGSoutput$mean$tau
cor.test(children_base$theta, children_base$tau)

children_exp$tau <- EIG_PTS_children_exp$BUGSoutput$mean$tau
cor.test(children_exp$theta, children_exp$tau)

children_rep$tau <- EIG_PTS_children_rep$BUGSoutput$mean$tau
cor.test(children_rep$theta, children_rep$tau)

#### transform data 
children_wide <- children
children_wide$order <- factor(children_wide$order)
library(tidyr)
children_long <- gather(children_wide, puzzle, choice, p1:p6, factor_key=TRUE)

intervention <- children_long[,c("order", "condition", "puzzle", "choice")]
intervention$choice <- factor(intervention$choice)
intervention$puzzle <- factor(intervention$puzzle)
intervention$condition <- factor(intervention$condition, levels=c("baseline", "explanation", "report"), labels=c("Baseline", "Explanation", "Report"))

type1 <- c('p1', 'p2')
type2 <- c('p3', 'p4')
type3 <- c('p5', 'p6')

chain_cc <- intervention[intervention$puzzle %in% type1,]
chain_one <- intervention[intervention$puzzle %in% type2,]
ce_one <- intervention[intervention$puzzle %in% type3,]

# what if I need % instead of counts?
# (https://datacarpentry.org/r-socialsci/04-ggplot2/index.html)
## type 1

### actual choices
percent_choice <- chain_cc %>%
  count(choice, condition) %>%
  group_by(condition) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

p_type1<- 
  ggplot(percent_choice, aes(x = condition, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "dodge") +    
  scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1))

p_type1 <- p_type1 + 
  ylab("Proportion of Choices") +  
  scale_fill_discrete(name="Choice") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=50), 
        axis.text = element_text(size=50),
        legend.position="none")

### theorectial choices
percent_model <- data.frame(choice = c(1, 2, 3, 1, 2, 3),
                 strat = c(rep("EIG",3), rep("PTS", 3)),
                 percent = c(0, 1, 0, 2/3, 1/3, 0))

percent_model$choice <- factor(percent_model$choice)
percent_model$strat <- factor(percent_model$strat)

m_type1 <- 
  ggplot(percent_model, aes(x = strat, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "dodge") +    
  scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1))

m_type1 <- m_type1 + 
  ylab("Probability of Each Choice") +  
  scale_fill_discrete(name="Choice") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=50), 
        axis.text = element_text(size=50))

top <- plot_grid(p_type1, m_type1, align = 'h', rel_widths = c(1, 1.2))

## type 2
### actual choices
percent_choice <- chain_one %>%
  count(choice, condition) %>%
  group_by(condition) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

p_type2<- 
ggplot(percent_choice, aes(x = condition, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "dodge") +    
  scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1))

p_type2 <- p_type2 +
  xlab("Condition") + 
  ylab("Proportion of Choices") +  
  scale_fill_discrete(name="Choice") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=50), 
        axis.text = element_text(size=50),
        legend.position="none")

### theorectial choices
percent_model <- data.frame(choice = c(1, 2, 3, 1, 2, 3),
                            strat = c(rep("EIG",3), rep("PTS", 3)),
                            percent = c(1, 0, 0, 1/2, 1/2, 0))

percent_model$choice <- factor(percent_model$choice)
percent_model$strat <- factor(percent_model$strat)

m_type2 <- 
  ggplot(percent_model, aes(x = strat, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "dodge") +    
  scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1))

m_type2 <- m_type2 + 
  ylab("Probability of Each Choice") +  
  scale_fill_discrete(name="Choice") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=50), 
        axis.text = element_text(size=50))

middle <- plot_grid(p_type2, m_type2, align = 'h', rel_widths = c(1, 1.2))

## type 3
### actual choices
percent_choice <- ce_one %>%
  count(choice, condition) %>%
  group_by(condition) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()

p_type3<- 
  ggplot(percent_choice, aes(x = condition, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "dodge") +    
  scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1))

p_type3 <- p_type3 +
  xlab("Condition") + 
  ylab("Proportion of Choices") +  
  scale_fill_discrete(name="Choice") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=50), 
        axis.text = element_text(size=50),
        legend.position="none")
### theorectial choices
percent_model <- data.frame(choice = c(1, 2, 3, 1, 2, 3),
                            strat = c(rep("EIG",3), rep("PTS", 3)),
                            percent = c(0, 1, 0, 0, 1/3, 2/3))

percent_model$choice <- factor(percent_model$choice)
percent_model$strat <- factor(percent_model$strat)

m_type3 <- 
  ggplot(percent_model, aes(x = strat, y = percent, fill = choice)) +
  geom_bar(stat = "identity", position = "dodge") +    
  scale_y_continuous(labels = scales::percent_format(), limits=c(0, 1))

m_type3 <- m_type3 + 
  ylab("Probability of Each Choice") +  
  scale_fill_discrete(name="Choice") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=50), 
        axis.text = element_text(size=50))

bottom <- plot_grid(p_type3, m_type3, align = 'h', rel_widths = c(1, 1.2))


together <- plot_grid(top, middle, bottom, ncol = 1, rel_heights = c(1, 1, 1))


## grid
### tutorial: https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html
require(cowplot)
acc_plot <- plot_grid(p_type1 + theme(legend.position="none"),
                   p_type2 + theme(legend.position="none"),
                   p_type3 + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("A", "B", "C"),
                   label_size = 30,
                   hjust = -1,
                   nrow = 1
)

legend <- get_legend(p_type1)

acc_plot <- plot_grid(acc_plot, legend, rel_widths = c(3, .3))

acc_plot

### raw scores
## compare to chance levels

puzzle.name <- paste("p", 1:6, sep="") 
m <- 6 # of puzzles for each participant 
l <- 3 # of nodes in each puzzle

# Children: Baseline
## Basic info
node <- everyone[puzzle.name] # 

n <- nrow(everyone)
m <- 6
l <- 3

### According to IG
IG <- c(c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0), c(1.0, 0.0, 0.0), c(1.0, 0.0, 0.0), c(0.0, 1.0, 0.0), c(0.0, 0.0, 1.0))
IG <- matrix(IG, nrow=6, ncol=3, byrow=T)
V_IG <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_IG[i,j,k] <- IG[j,k] 
    }
  }
} 

### According to PTS
PTS <- c(c(1.0, 0.5, 0.0), c(1.0, 0.0, 0.5), c(1.0, 1.0, 0.0), c(1.0, 0.0, 1.0), c(0.0, 0.5, 1.0), c(0.0, 1.0, 0.5))
PTS <- matrix(PTS, nrow=m, ncol=3, byrow=T)
V_PTS <- array(0, dim=c(n, m, l)) 
for (i in 1:n){
  for (j in 1:m){
    for (k in 1:l)
    {
      V_PTS[i,j,k] <- PTS[j,k] 
    }
  }
} 



PTS_score <- matrix(0, nrow=n, ncol=m, byrow=T)
ave.PTS <- rep(0,n)

IG_score <- matrix(0, nrow=n, ncol=m, byrow=T)
ave.IG <- rep(0,n)

##### logistic

colnames(IG_score) <- paste("ig_p", 1:6, sep="") 
everyone <- cbind(everyone, IG_score)

children <- everyone[which(everyone$group=='children'),] # child data
children_base <- children[which(children$condition=='baseline'),]
children_exp <- children[which(children$condition=='explanation'),]
children_rep <- children[which(children$condition=='report'),]

base_p1 <- glm(acc_p1~ig_p1,family=binomial(link='logit'), data=children_base)
base_p2 <- glm(acc_p2~ig_p2,family=binomial(link='logit'), data=children_base)
base_p3 <- glm(acc_p3~ig_p3,family=binomial(link='logit'), data=children_base)
base_p4 <- glm(acc_p4~ig_p4,family=binomial(link='logit'), data=children_base)
base_p5 <- glm(acc_p5~ig_p5,family=binomial(link='logit'), data=children_base)
base_p6 <- glm(acc_p6~ig_p6,family=binomial(link='logit'), data=children_base)

summary(base_p1)
summary(base_p2)
summary(base_p3)
summary(base_p4)
summary(base_p5)
summary(base_p6)

exp_p1 <- glm(acc_p1~ig_p1,family=binomial(link='logit'), data=children_exp)
exp_p2 <- glm(acc_p2~ig_p2,family=binomial(link='logit'), data=children_exp)
exp_p3 <- glm(acc_p3~ig_p3,family=binomial(link='logit'), data=children_exp)
exp_p4 <- glm(acc_p4~ig_p4,family=binomial(link='logit'), data=children_exp)
exp_p5 <- glm(acc_p5~ig_p5,family=binomial(link='logit'), data=children_exp)
exp_p6 <- glm(acc_p6~ig_p6,family=binomial(link='logit'), data=children_exp)

summary(exp_p1)
summary(exp_p2)
summary(exp_p3)
summary(exp_p4)
summary(exp_p5)
summary(exp_p6)

rep_p1 <- glm(acc_p1~ig_p1,family=binomial(link='logit'), data=children_rep)
rep_p2 <- glm(acc_p2~ig_p2,family=binomial(link='logit'), data=children_rep)
rep_p3 <- glm(acc_p3~ig_p3,family=binomial(link='logit'), data=children_rep)
rep_p4 <- glm(acc_p4~ig_p4,family=binomial(link='logit'), data=children_rep)
rep_p5 <- glm(acc_p5~ig_p5,family=binomial(link='logit'), data=children_rep)
rep_p6 <- glm(acc_p6~ig_p6,family=binomial(link='logit'), data=children_rep)

summary(rep_p1)
summary(rep_p2)
summary(rep_p3)
summary(rep_p4)
summary(rep_p5)
summary(rep_p6)

#### correlations


##------------- 3. distribution of theta -------------##
# load data
load("children_base_PTS_mu.Rdata")
load("children_exp_PTS_mu.Rdata")
load("children_rep_PTS_mu.Rdata")

Report <- children_rep_PTS_mu
Explanation <- children_exp_PTS_mu
Baseline <- children_base_PTS_mu

# Plot
df <- data.frame(Baseline, Explanation, Report)
dfs <- stack(df)
is.factor(dfs[,2])

cdat <- ddply(dfs, "ind", summarise, rating.mean=mean(values))
cdat

df <- data.frame(Baseline, Explanation, Report)
dfs <- stack(df)
is.factor(dfs[,2])

cdat <- ddply(dfs, "ind", summarise, rating.mean=mean(values))
cdat

ggplot(dfs, aes(x=values)) + 
  geom_density(aes(group=ind, colour=ind), alpha=0.3, size=1.5)+
  xlab(expression(paste("Mean of", " ", theta))) + ylab("Density")+
  theme(text = element_text(size=55))+
  scale_color_manual(values=c("#003262", "#FDB515", "#8c1515"),name="Condition")


intervened <- 0

for (i in 1:n){
  for (j in 1:m){
    intervened <- node[i,j]
    PTS_score[i,j] <- V_PTS[i,j,intervened]
  }
  ave.PTS[i] <- mean(PTS_score[i,])
}

t.test(ave.PTS, mu=mean(PTS))
cohensD(ave.PTS, mu=mean(PTS))

for (i in 1:n){
  for (j in 1:m){
    intervened <- node[i,j]
    IG_score[i,j] <- V_IG[i, j,intervened]
  }
  ave.IG[i] <- mean(IG_score[i,])
}

t.test(ave.IG, mu=mean(IG))
cohensD(ave.IG, mu=mean(IG))

everyone$ave_PTS <- ave.PTS
everyone$ave_IG <- ave.IG

write.csv(everyone, file = "everyone.csv")


sd(children_base$ave_PTS)

t.test(children_base$ave_PTS, mu = .55)
library(lsr)
cohensD(children_base$ave_PTS, mu = .55)

t.test(children_base$ave_IG, mu = 1/3)
cohensD(children_base$ave_IG, mu = 1/3)
sd(children_base$ave_IG)
mean(children_base$ave_IG)

mean(children_rep$ave_PTS)

### chi-sq
percent_choice <- chain_cc %>%
  count(choice, condition) %>%
  group_by(condition) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup()
