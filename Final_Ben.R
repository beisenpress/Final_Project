#### Load Libraries #######
library(FNN)
library(calibrate)


#### Set working Directory #####

wd = ''
if(grepl("stefano",Sys.info()["user"])){
  wd ='/Users/stefano/Google Drive/1 Booth/2 Lectures/Digital Algo Marketing/37304 Digital and Algorithmic Marketing/Final Project'
}else if(grepl("BENS-XPS13",Sys.info()["nodename"])){
  wd = "C:/Users/ben/Dropbox/Chicago Booth/37304 Digital Marketing/Final Project/Final_Project"
}else{
  wd = "C:/Users/dtull/OneDrive/Documents/Booth/Q6 Classes/Digital and Algorithmic Marketing (37304-01)/Final Project"
}
setwd(wd)



# reads initial data set
movies = read.csv(file="./movie_metadata.csv",head=TRUE,sep=",")

dt = data.table(movies)
dt[, PIK := .I] # adds primary key to make sure that we don't lose records along the way

# get rid of rows with empty fields
dt = dt[complete.cases(dt)]
dt = dt[plot_keywords != ""]

# Save full dataset 
dt_full <- dt


#### Find similar lead actors #######

# Write a function that uses K means clustering to find similar actors
# The function takes the name of a lead actor as an input, as well as the number of neighbors to use and the input dataset
# It outputs up to 3 actors who 
similar.actor1 <- function(name, num_neighbors, dt){
  
  # Create a seed dataset that only contains 
  actor_seed <- subset(dt, actor_1_name == name)
  
  # Turn the dataset of the seed actor into a model matrix
  actor_seed_model <- model.matrix(~ duration + country + content_rating + budget, data = actor_seed)
  
  # Create a dataset excluding the actor
  dt_no_act <- dt[actor_1_name != name,]
  
  # Turn the dataset excluding the actor into a model
  dt_model <- model.matrix(~ duration + country + content_rating + budget, data = dt_no_act)
  
  # Get the index position of the nearest neighbors for every movie
  m10 = get.knnx(dt_model,actor_seed_model,k=num_neighbors,algorithm='brute')
  
  # Get the entries for the nearest neighbors
  neighbors <- dt_no_act[as.vector(m10$nn.index),]
  
  # Make a table out of the nearest neighbors to count how many times a given actor appears as a neighbor
  # Drop levels to exclude actors not in the list
  neighbor_table <- table(droplevels(neighbors$actor_1_name))
  
  # Sort to put most frequent overlap at the top
  neighbor_table <- sort(neighbor_table, decreasing = TRUE)
  
  # Print top three actors
  print(rownames(neighbor_table)[1:3])
                                         
                                         
}

# Run the function
similar.actor1("Keanu Reeves", 10, dt_full)


###### Use k Means to categorize actors work #######

# An acutomated way to group actor's movies by comercial sucess and critical acclaim
# Inspired by this post: https://fivethirtyeight.com/datalab/the-three-types-of-adam-sandler-movies/

movie.cluster1 <- function(actor_name, num_clusters, dt){
  
  # Create a dataset of just one actor
  actor_dt <- subset(dt, actor_1_name == actor_name)
  
  # K Means Clustering
  out = kmeans(actor_dt[,c("gross","imdb_score")],centers=num_clusters,nstart=20)
  # Centers is number of clusters
  # nstart is how many starting points to use. Different starting points can give different answers.  

  # Plot the movies, colored by cluster
  plot(actor_dt[,c("gross","imdb_score")],col=out$cluster,pch=19,xlab="Gross",ylab="IMDB Score", 
          main = paste(actor_name,"Movies in",num_clusters, "Clusters"))
  
  # Add Movie Labels
  textxy(actor_dt$gross, actor_dt$imdb_score, labs = actor_dt$movie_title)
  
  #Plot the cluster centers
  points(out$centers,col=1:K,pch=4,cex=2)
  
}

movie.cluster1("Keanu Reeves", 4, dt_full)
