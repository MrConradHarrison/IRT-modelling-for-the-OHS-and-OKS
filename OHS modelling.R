
# Install and load correct package versions
if(!require(pacman)) install.packages("pacman")

pacman::p_install_version(
  c("dplyr", "ggplot2", "finalfit", "questionr", "MVN", "tidyselect", "psych", 
    "GPArotation", "mokken", "qgraph", "lavaan", "MBESS", "mirt", "lordif", 
    "ggridges", "cowplot"),
  c("1.0.9", "3.3.6", "1.0.4", "0.7.7", "5.9", "1.1.2", "2.2.5", "2022.4-1", 
    "3.0.6", "1.9.2", "0.6-11", "4.9.1", "1.36.1", "0.3-3", "0.5.3", "1.1.1"))

pacman::p_load(dplyr, ggplot2, finalfit, questionr,
               MVN, tidyselect, psych, GPArotation,
               mokken, qgraph, lavaan, MBESS,
               mirt, lordif, ggridges, cowplot)

# Source functions
source("Functions.R")

# Import data
ohs.dataset <- readRDS("OHS.RDS")

#### Demographics, score distribution, and missing data frequency ####

# Dataset dimensions
dim(ohs.dataset)

# Year of operation
table(ohs.dataset$Year)
histo(ohs.dataset, "Year")

# Age band
table(ohs.dataset$"Age Band")
histo(ohs.dataset, "Age Band")

# Gender
table(ohs.dataset$Gender)
histo(ohs.dataset, "Gender")

# Pre op scores
histo(ohs.dataset, "pre.op.sumscore")
histo(ohs.dataset, "post.op.sumscore")

# Count missing variables
freq.na(ohs.dataset)


#### Preparation of item response data ####

# Filter necessary data fields
preop.ohs <- ohs.dataset |> 
  mutate(ID = seq(1:nrow(ohs.dataset))) |>
  filter(Procedure == "Hip Replacement") |>
  filter(is.na(pre.op.sumscore) == FALSE) |>
  dplyr::select("Hip Replacement Pre-Op Q Pain" : "Hip Replacement Pre-Op Q Work", 
         "Gender", "Age", "ID")
  

# Rename items
colnames <- c("Pain", "Sudden pain", "Night pain", "Washing", "Transport", 
              "Dressing", "Shopping", "Walking", "Limping","Stairs", "Standing",
              "Work", "Gender", "Age", "ID")

colnames(preop.ohs) <- colnames

items <- preop.ohs |>
  dplyr::select("Pain":"Work")


#### Factorial structure ####

# Check multivariate normality with Mardia's test
# Stick to sample of 10 000 as you'll exhaust vector memory otherwise
set.seed(123)
s <- sample.int(n = nrow(items), size = 10000, replace = FALSE)

mvn(items[s,],
    mvnTest = "mardia")

# Bartlett's test
items |>
  cortest.bartlett(n = nrow(items))

# KMO
items |>
  KMO()

# Scree plot
set.seed(123)
no.factors <- items |>
  fa.parallel(fm="minres", 
              fa="fa", 
              cor = "poly",
              correct = 0) 
# Note, parallel analysis is sensitive to sample size

scree.plot(no.factors)

# Mokken
items[s,] |>
  as.data.frame() |>
  coefH()

items[s,] |>
  as.data.frame() |>
  aisp()

# Network visualisation
network(items)
      
# CFA

# Remove spaces in item names
cfa.items <- items |>
  rename("Sudden.pain" = "Sudden pain") |>
  rename("Night.pain" = "Night pain")

# Specify model
model <- 'Function =~ Pain + Sudden.pain + Night.pain + Washing + 
          Transport + Dressing + Shopping + Walking + Limping + Stairs +
          Standing + Work'

# Fit model
fit <- cfa(model, cfa.items, ordered = TRUE)

# Model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)

# 95% CIs for RMSEA
ci.rmsea(rmsea=.075, df=54, N=317642, conf.level=.95)

#### GRM ####

# Fit model
grm <- mirt(items, model = 1, itemtype = "graded", SE = TRUE)

# Look at parameters
coef(grm, IRTpars = TRUE, simplifty = TRUE)

# Fit statistics
M2(grm)

# Local dependency
res <- residuals(grm, type="Q3")
res[res<0.2] <- NA
res

# DIF
# Omit NAs
DIF.data <- preop.ohs |> 
  na.omit()

# Specify items
DIF.items <- DIF.data[,1:12]

# Set factor levels
DIF.gen <- DIF.data$Gender

DIF.gen <- factor(DIF.gen,
                  levels = c("Male", "Female"),
                  labels = c("Male", "Female"))

DIF.age <- DIF.data$Age |>
  as.character()

DIF.age <- factor(DIF.age,
                  levels = c("< 60 years", "60 years +"),
                  labels = c("< 60 years", "60 years +"))

# Look for DIF by gender - no items flagged
lordif(DIF.items, DIF.gen, criterion = "R2", pseudo.R2 = "Nagelkerke")

# Look for DIF by age - no items flagged
lordif(DIF.items, DIF.age, criterion = "R2", pseudo.R2 = "Nagelkerke")

# Correlation of sum scores and EAP scores
EAPcor(grm, items)

#### Information plots ####

# Calculate EAP fscores and test info for pre-ops
preop.fscores <- fscores(grm)
preop.info <- testinfo(grm, preop.fscores)

# Calculate EAP fscores and test info for post-ops
postop.ohs <- ohs.dataset |>
  filter(Procedure == "Hip Replacement") |>
  filter(is.na(post.op.sumscore) == FALSE) |>
  dplyr::select("Hip Replacement Post-Op Q Pain" : "Hip Replacement Post-Op Q Work")

colnames(postop.ohs) <- colnames[1:12]
postop.fscores <-fscores(grm, response.pattern = postop.ohs)[,1]
postop.info <- testinfo(grm, postop.fscores)

# Create a dataframe with fscore, test info, and pre/post columns
df <- data.frame(Theta = c(preop.fscores, postop.fscores),
                 Information = c(preop.info, postop.info),
                 Group = c(rep("Preoperative", length(preop.fscores)), rep("Postoperative", length(postop.fscores))))

# Create plot
# Test info goes up to 12 (12 times higher than the y axis of scaled 
# density plot)
coef <- 12
min.fscore <- fscores(grm, response.pattern = rep(0,12))[1]
max.fscore <- fscores(grm, response.pattern = rep(4,12))[1]

ggplot(df, aes(x = Theta)) +
  geom_density(aes(x = Theta, y = ..scaled.., fill = Group), alpha = 0.3, colour = "black") +
  scale_fill_brewer(type = "qual", palette = "Accent") +
  theme_classic() +
  scale_color_manual(name = "", values = c("Information" = "firebrick")) +
  scale_y_continuous(name = "Response Frequency", sec.axis = sec_axis(~.*coef, name = "Test information"), expand = expansion(add = c(0,0.05))) +
  scale_x_continuous(name = expression(theta), limits = c(min.fscore,max.fscore), expand = c(0, 0)) +
  geom_line(aes(y = Information/coef, colour = "Information"), size = 1) +
  scale_alpha_manual(values = c(1)) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = c(0.1, 0.8)) +
  guides(fill = guide_legend(reverse=TRUE))


# Item information
item.info.plot(grm = grm,
               items = items,
               min.fscore = min.fscore,
               max.fscore = max.fscore,
               preop.fscores = preop.fscores,
               postop.fscores = postop.fscores)








