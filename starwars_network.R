# R network analysis of Star Wars - The Attack of the Clones (episode 2).
# O. Gimenez, January 2016

# This is a free adaptation of two clever analyses made by others:
# The Star Wars Social Network by Evelina Gabasov (http://evelinag.com/blog/2015/12-15-star-wars-social-network) in which F# was mostly used to analyse the Star wars social networks 
# Analyzing networks of characters in 'Love Actually' by David Robinson (http://varianceexplained.org/r/love-actually-network/) in which R was used to analyse the links between the characters of the movie Love Actually.

# The aim here is to do reproduce Evelina's analysis using R only, using David's contribution plus several tweaks I found here and there on the internet.

#-------- PARSING

library(dplyr)
library(stringr)
library(tidyr)

# I found the script in doc format here: www.theforce.net/timetales/ep2se.doc
# which I converted in txt format for convenience
# read file line by line 
raw <- readLines("attack-of-the-clones.txt")

# create data frame
lines <- data_frame(raw = raw) 

# get rid of leading and trailing white spaces
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
lines= mutate(lines,raw=trim(raw))

# get rid of the empty lines
lines2 <- filter(lines, raw != "")

# detect scenes: begin by EXT. or INT.
lines3 <-  mutate(lines2, is_scene = str_detect(raw, "T."),scene = cumsum(is_scene)) 

# drop lines that start with EXT. or INT.
lines4 <- filter(lines3,!is_scene)

# distinguish characters from what they say
lines5 <- separate(lines4, raw, c("speaker", "dialogue"), sep = ":", fill = "left",extra='drop')

# read in aliases (from Evelina's post)
aliases <- read.table('aliases.csv',sep=',',header=T,colClasses = "character")
aliases$Alias
aliases$Name

# assign unique name to characters
# http://stackoverflow.com/questions/28593265/is-there-a-function-like-switch-which-works-inside-of-dplyrmutate
multipleReplace <- function(x, what, by) {
  stopifnot(length(what)==length(by))               
  ind <- match(x, what)
  ifelse(is.na(ind),x,by[ind])
}
lines6 <- mutate(lines5,speaker=multipleReplace(speaker,what=aliases$Alias,by=aliases$Name))

# read in actual names (from Evelina's post)
actual.names <- read.csv('characters.csv',header=F,colClasses = "character")
actual.names <- c(as.matrix(actual.names))
# filter out non-characters
lines7 <- filter(lines6,speaker %in% actual.names)

# group by scene
lines8 <- group_by(lines7, scene, line = cumsum(!is.na(speaker))) 

lines9 <- summarize(lines8, speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

# Count the lines-per-scene-per-character
# Turn the result into a binary speaker-by-scene matrix
by_speaker_scene <- count(lines9, scene, speaker)
by_speaker_scene

library(reshape2)
speaker_scene_matrix <-acast(by_speaker_scene , speaker ~ scene, fun.aggregate = length)

dim(speaker_scene_matrix)

#-------- ANALYSIS

# Hierarchical clustering
norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)
h <- hclust(dist(norm, method = "manhattan"))
plot(h)

# Use tree to give an ordering that puts similar characters close together:
ordering <- h$labels[h$order]
ordering

# This ordering can be used to make other graphs more informative. For instance, we can visualize a timeline of all scenes:
scenes <-  filter(by_speaker_scene, n() > 1) # scenes with > 1 character
scenes2 <- ungroup(scenes)
scenes3 <- mutate(scenes2, scene = as.numeric(factor(scene)),
           character = factor(speaker, levels = ordering))
library(ggplot2)
ggplot(scenes3, aes(scene, character)) +
    geom_point() +
    geom_path(aes(group = scene))

# Create a cooccurence matrix (http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix) 
# containing how many times two characters share scenes
cooccur <- speaker_scene_matrix %*% t(speaker_scene_matrix)
heatmap(cooccur)

# Graphical representation of the network
# Here the nodes represent characters in the movies. The characters are connected by a link if they both speak in the same scene. And the more the characters speak together, the thicker the link between them.  
 
library(igraph)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
plot(g, edge.width = E(g)$weight)

# compute standard network features, degree and betweeness
degree(g)
betweenness(g)

# get a nicer representation of the network
# see http://tagteam.harvard.edu/hub_feeds/1981/feed_items/1388531 for formating from igraph to d3Network 
library(d3Network)
library(networkD3)
sg <- simplify(g)
df <- get.edgelist(g, names=TRUE)
df <- as.data.frame(df)
colnames(df) <- c('source', 'target')
df$value <- rep(1, nrow(df))
# get communities
fc <- fastgreedy.community(g)
com <- membership(fc)
node.info <- data.frame(name=names(com), group=as.vector(com))
links <- data.frame(source=match(df$source, node.info$name)-1,target=match(df$target, node.info$name)-1,value=df$value)

forceNetwork(Links = links, Nodes = node.info,Source = "source", Target = "target",Value = "value", NodeID = "name",Group = "group", opacity = 1, opacityNoHover=1)

# Here the nodes represent characters in the movies. The characters are connected by a link if they both speak in the same scene. 
# The colors are for groups obtained by some algorithms


