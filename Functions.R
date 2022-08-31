
# This script contains custom functions used in the OHS and OKS modelling
# scripts

#### Histogram ####

histo <- function(x, y){
  
  df <-  x |>
    dplyr::select(y) |>
    table() |>
    as.data.frame() 
  
  colnames(df) <- c(".", "Freq")
  
    p <- ggplot(df) +
    geom_bar(aes(x = ., y = Freq), stat = "identity", fill = "cyan4") +
    theme_minimal() +
    xlab("") +
    ylab("Frequency")
  
  return(p)
  
}


#### Scree plot ####

scree.plot <- function (x, 
                        xlim = 4,
                        translucency = 0.5,
                        colour = "darkmagenta") {
  
  
  fa.values <- x$fa.values
  
  factors <- seq(from = 1, to = length(fa.values))
  
  value.table <- cbind(factors, fa.values) |> 
    as.data.frame() |>
    slice(1:xlim)
  
  colnames(value.table) <- c("Factors", "Eigenvalues")
  
  table <- value.table
  
  p <- ggplot(table, aes(x = Factors, y = Eigenvalues)) +
    geom_line(size = 1, alpha = translucency, colour =colour) +
    geom_point(size = 3, alpha = translucency, colour = colour) +
    scale_x_continuous(name='Factor Number', breaks=(1:xlim)) +
    theme_minimal() +
    labs(colour = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme(text = element_text(size = 20))
  
  p
}


#### Covariance network ####

network <- function(items){
  
  cormatrix <- cor_auto(items)
  
  qgraph(cormatrix, 
         graph = "glasso", 
         layout = "spring", 
         sampleSize = nrow(items), 
         vsize = 7, 
         cut = 0, 
         maximum = .45, 
         borderwidth = 1.5, 
         labels = names(items),
         label.cex = 1.2, 
         curve = 0.3, 
         curveAll = T, 
         posCol = "cyan4",
         node.width = 1.2, 
         node.height = 1.2)
  
  
}



#### Correlation of sum scores and EAP scores ####

EAPcor <- function(model, items){


fscores <- fscores(grm)
sumscores <- rowSums(items)
cor <- cor(fscores, sumscores)

score.df <- cbind(fscores, sumscores) |> 
  as.data.frame()

ggplot(score.df, aes(x = F1, y = sumscores)) +
  geom_point(colour = "seagreen4", alpha = 0.1, size = 0.3) +
  theme_minimal() +
  xlab("EAP score") +
  ylab("Sum-score") +
  annotate("text", x = -2.5, y = 43, label = paste("r = ", as.character(round(cor, 3)),sep =""))

}

#### Item info plots ####

item.info.plot <- function(grm, items, min.fscore, max.fscore, preop.fscores, 
                           postop.fscores){

# Set a sequence of 300 thetas
theta.seq <- seq(min.fscore, max.fscore, length.out = 300)

# Function for getting the info for each item
get.info <- function(item){
  
  info <- grm |> 
    extract.item(item) |> 
    iteminfo(Theta = theta.seq)
  
  return(info)
  
}

# Create a list with info for each item and item name
list <- NULL 

for (i in 1:12){
  
  list[[i]] <- cbind(theta.seq, get.info(i), rep(names(items)[i])) |> 
    as.data.frame()
  
}


df <- rbind(list[[1]], list[[2]], list[[3]], list[[4]], list[[5]], list[[6]],
            list[[7]],list[[8]], list[[9]], list[[10]], list[[11]], list[[12]])

colnames(df) <- c("Theta", "Information", "Item")

df$Theta <- as.numeric(df$Theta)
df$Information <- as.numeric(df$Information)

df2 <- data.frame(Theta = c(preop.fscores, postop.fscores),
                  Group = c(rep("Pre-op", length(preop.fscores)), rep("Post-op", length(postop.fscores))))

p1 <- ggplot(as.data.frame(df), aes(x = Theta, height = Information, y = Item, alpha = 0.01), fill = "darkmagenta") +
  geom_ridgeline(alpha = 0.2, scale = 0.7) +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_x_continuous(name = expression(theta), limits = c(min.fscore,max.fscore), expand = c(0, 0)) +
  ylab("Item information") +
  theme(axis.title.y = element_text(angle=90, vjust = 0, hjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p2 <- ggplot(df2) +
  geom_density(aes(x = Theta, y = ..scaled.., fill = Group), alpha = 0.3, colour = "black") +
  theme_ridges() + 
  scale_fill_brewer(type = "qual", palette = "Accent") +
  ylab("Frequency") +
  theme(axis.title.y = element_text(angle=0, vjust = 0.5, hjust = 0),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.position = c(0.8, 1.1)) +
  scale_x_continuous(name = expression(theta), limits = c(min.fscore,max.fscore), expand = c(0, 0)) +
  theme(axis.title.x = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse=TRUE))


plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", rel_heights = c(4,1))


}








  