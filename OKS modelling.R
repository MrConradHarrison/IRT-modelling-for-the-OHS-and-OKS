
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

####  Import data ####
oks.dataset <- readRDS("OKS.RDS")

#### Demographics, score distribution, and missing data frequency ####

# Dataset dimensions
dim(oks.dataset)

# Year of operation
table(oks.dataset$Year)
histo(oks.dataset, "Year")

# Age band
table(oks.dataset$"Age Band")
histo(oks.dataset, "Age Band")

# Gender
table(oks.dataset$Gender)
histo(oks.dataset, "Gender")

# Pre op scores
histo(oks.dataset, "pre.op.sumscore")
histo(oks.dataset, "post.op.sumscore")

# Count missing variables
freq.na(oks.dataset)


#### Preparation of item response data ####

# Filter necessary data fields
preop.oks <- oks.dataset |>
  mutate(ID = seq(1:nrow(oks.dataset))) |>
  filter(Procedure == "Knee Replacement") |>
  filter(is.na(pre.op.sumscore) == FALSE) |>
  dplyr::select("Knee Replacement Pre-Op Q Pain" : "Knee Replacement Pre-Op Q Stairs", 
         "Gender", "Age", "ID")


# Rename items
colnames <- c("Pain", "Night pain", "Washing", "Transport", "Walking", 
              "Standing", "Limping", "Kneeling", "Work", "Give way", "Shopping", 
              "Stairs", "Gender", "Age", "ID")

colnames(preop.oks) <- colnames

items <- preop.oks |>
  dplyr::select("Pain":"Stairs")


#### Factorial structure ####

# Check multivariate normality with Mardia's test
# Stick to random 10 000 as you'll exhaust vector memory otherwise
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
  rename("Night.pain" = "Night pain") |>
  rename("Confidence" = "Give way")

# Specify model
model <- 'Function =~ Pain + Night.pain + Washing + Transport +
          Walking + Standing + Limping + Kneeling + Work + Confidence +
          Shopping + Stairs'

# Fit model
fit <- cfa(model, cfa.items, ordered = TRUE)

# Model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)

# 95% CIs for RMSEA
ci.rmsea(rmsea=.060, df=54, N=351182, conf.level=.95)

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
DIF.data <- preop.oks |> 
  na.omit()

# Specify items
DIF.items <- DIF.data[,1:12]

# Set factor levels
DIF.gen <- DIF.data$Gender |> 
  factor(levels = c("Male", "Female"))

DIF.age <- DIF.data$Age |> 
  as.character()

# Look for DIF by gender - item 8 flagged
gendif <- lordif(DIF.items, DIF.gen, criterion = "R2", pseudo.R2 = "Nagelkerke")
summary(gendif)
plot.lordif(gendif, labels = c("Male", "Female"))

# Look for DIF by age - no items flagged
lordif(DIF.items, DIF.age, criterion = "R2", pseudo.R2 = "Nagelkerke")

# Correlation of sum scores and EAP scores
EAPcor(grm, items)

#### Information plots ####

# Calculate EAP fscores and test info for pre-ops
preop.fscores <- fscores(grm)
preop.info <- testinfo(grm, preop.fscores)

# Calculate EAP fscores and test info for post-ops
postop.oks <- oks.dataset |>
  filter(Procedure == "Knee Replacement") |>
  filter(is.na(post.op.sumscore) == FALSE) |>
  dplyr::select("Knee Replacement Post-Op Q Pain" : "Knee Replacement Post-Op Q Stairs")

colnames(postop.oks) <- colnames[1:12]
postop.fscores <-fscores(grm, response.pattern = postop.oks)[,1]
postop.info <- testinfo(grm, postop.fscores)

# Create a dataframe with fscore, test info, and pre/post columns
df <- data.frame(Theta = c(preop.fscores, postop.fscores),
                 Information = c(preop.info, postop.info),
                 Group = c(rep("Preoperative", length(preop.fscores)), rep("Postoperative", length(postop.fscores))))

# Create plot
# Test info goes up to 12 (12 times higher than the y axis of scaled 
# density plot)
coef <- 10
min.fscore <- fscores(grm, response.pattern = rep(0,12))[1]
max.fscore <- fscores(grm, response.pattern = rep(4,12))[1]

ggplot(df, aes(x = Theta)) +
  geom_density(aes(x = Theta, y = ..scaled.., fill = Group), alpha = 0.3, colour = "black") +
  scale_fill_brewer(type = "qual", palette = "Accent") +
  theme_classic() +
  scale_color_manual(name = "", values = c("Information" = "firebrick")) +
  scale_y_continuous(name = "Response Frequency", sec.axis = sec_axis(~.*coef, name = "Test information"), expand = expansion(add = c(0,0.05))) +
  scale_x_continuous(name = "Latent construct", limits = c(min.fscore,max.fscore), expand = c(0, 0)) +
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


