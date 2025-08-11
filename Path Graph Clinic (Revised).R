#  Loading in relevant packages
library(tidyverse)
library(visNetwork)
library(igraph)
library(readxl)

# Setting working directory
setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")
#setwd("C:/Users/mcoppedg/Dropbox/VisualizingPoliticsInstructor/StudentWork/StudentWorkS2024/Clinics/Networks/Clark")
# Loading in my Excel files
Nodes <- read_excel("Nodes.xlsx")
Edges <- read_excel("Edges.xlsx")

# Making the Graph
visNetwork(nodes = Nodes, edges = Edges, 
           height = "450px", width = "750px",
           footer = "What Factors Impact Happiness/Unhappiness?") %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -1000, 
                                     avoidOverlap = 1)) %>%
  visIgraphLayout() %>%
  visEdges(smooth = "enabled",
    arrows = list(to = list(enabled = TRUE, scaleFactor = .5))) %>%
  visNodes() %>%
  visOptions(highlightNearest = TRUE)

## Questions
# First, do you know how to change the font size or color or anything 
# for my edge labels (“Yes” and “No”)? They are kind of difficult to 
# read on my current graphic, and I can’t figure out how to fix that. 
# Second, do you know if it possible to control where the edge labels 
# lie on the arrows? I would prefer if they were all in the middle -- 
# I really had to spread the elements out to avoid having any edge 
# labels blocked by arrowheads. 

## MC
# It looks like you have plenty of space, and it's good to spread things out, but
# I do see that the edge labels are too small to read. 

# The font.size for your edges had been set in the Edges df, so the most 
# direct way to increase it is to modify that parameter in the df:

Edges$font.size <- 2*Edges$font.size

# Now:
visNetwork(nodes = Nodes, edges = Edges, 
           height = "450px", width = "750px",
           footer = "What Factors Impact Happiness/Unhappiness?") %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -1000, 
                                     avoidOverlap = 1)) %>%
  visIgraphLayout() %>%
  visEdges(smooth = "enabled",
           arrows = list(to = list(enabled = TRUE, scaleFactor = .5))) %>%
  visNodes() %>%
  visOptions(highlightNearest = TRUE)
 # Note: You probably don't need "Yes" and "No" represented by both
# a label and a solid or dashed line. Choose one or the other. 

# 
# Finally, do you think it would make my graphic more readable if I 
# had curved lines instead of straight lines? If so, how would I change 
# that? Is it simply a matter of setting the edge lengths in advance?
# 

## MC: Usually curved edges look better. To get curved lines, specify 
# Here is some code for some gentle curving:

visNetwork(nodes = Nodes, edges = Edges, 
           height = "450px", width = "750px",
           footer = "What Factors Impact Happiness/Unhappiness?") %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -1000, 
                                     avoidOverlap = 1)) %>%
  visIgraphLayout() %>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = .2),
           arrows = list(to = list(enabled = TRUE, scaleFactor = .5))) %>%
  visNodes() %>%
  visOptions(highlightNearest = TRUE)

###########################################################################
# Let's remove the labels altogether and add a legend instead
Edges <- Edges %>%
  select(-label, -title)

visNetwork(nodes = Nodes, edges = Edges, 
           height = "450px", width = "750px",
           footer = "What Factors Impact Happiness/Unhappiness?") %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -1000, 
                                     avoidOverlap = 1)) %>%
  visIgraphLayout() %>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = .2),
           arrows = list(to = list(enabled = TRUE, scaleFactor = .5))) %>%
  visNodes() %>%
  visOptions(highlightNearest = TRUE) %>%
  visLegend(useGroups = FALSE, addEdges = data.frame(label = c("Yes", "No"),
                                                     color = c("black", "black"),
                                                     width = c(3,3),
                                                     dashes = c(FALSE, TRUE),
                                                     font.align = c("top", "top")))

  
