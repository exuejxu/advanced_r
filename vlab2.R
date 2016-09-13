# Assignment 1

#tab-separated txt can not be added by read.table
#jobs <- read.table("C:/Users/Sam/Desktop/visualization/labb2/jobs.txt") 


# Read data into R,tab-delimited/tad-separated you would use the sep="\t".
jobs <- read.csv("C:/Users/Sam/Desktop/visualization/labb2/jobs.txt",
                 sep='\t')

jobs$Group = c("W", "Sc", "W", "W", "w", "S", "W", "W", "W", "W","Sc",
               "W","Sc","S","S","Sc","w", "E", "E", "E", "E", "E", "E", "E","E","E")


#install.packages("portfolio")
library(portfolio)

# Create a treemap
map.market(id = jobs$Country, area = jobs$SI, 
group = jobs$Group, color = jobs$Fin, main = "Jobs in finance & services industries in European")

#install.packages("aplpack")
library(aplpack)

# Create a chernoff faces
faces(jobs[2:10], face.type=1,main="random faces")

install.packages("gclus")
library(gclus)

jobs.cor <- cor(jobs[2:10])
ord <- order.single(jobs.cor)
jobs2 <- jobs[,ord]
faces(jobs2, face.type=1, main="reordered random face")

