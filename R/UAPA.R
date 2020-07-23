library(readr)
install.packages("plyr")
install.packages("igraph")

#cargando el edgelist coautoría UAPA
UAPA_edgelist <- read_csv("~/Documents/Curriculum/LATEST/CV_Maya/PROYECTOS/Yeni/UAPA_edgelist_new.csv")
View(UAPA_edgelist_new)
library(igraph)
#creando el grafo
G <- graph.edgelist(as.matrix(UAPA_edgelist), directed = FALSE)
G
plot(G) #visualizando

G1 <- layout_nicely(G) #visualizando

plot(G, vertex.label.color = "black", layout = G1) 

is.weighted(G) # tiene los pesos de cada relación?

farthest_vertices(G) # vértices más lejanos entre sí

get_diameter(G) #distancia entre los vértices más lejanos entre sí

conteo_conexiones <- degree(G, mode = c("all")) #conteo de las conexiones que tiene cada autor
table(conteo_conexiones) # resumen de cantidad de conexiones vs cantidad de autores con la misma cantidad de conexiones
hist(conteo_conexiones, breaks = 30) #visualizando histograma del conteo de conexiones y autores

V(G)$name[degree(G)==max(degree(G))] # identificar el autor con más conexiones en la red. 
puentes <- betweenness(G, normalized = TRUE)
puentes # podría covertir esto en un DF y seleccionar los autores con mayor betweenness
hist(puentes, breaks = 80) # Visually examine the distribution of betweenness scores
#visualizando los puentes y su peso
plot(G,
  vertex.label = NA,
  edge.color = 'black',
  vertex.size = sqrt(puentes)+1,
  edge.arrow.size = 0.05,
  layout = layout_nicely(G)) 
#visualizando los puentes sin peso específico. Acá se visualizan mejor las conexiones
plot(G,
vertex.label = NA,
edge.color = 'black',
vertex.size = puentes,
edge.arrow.size = 0.05,
layout = layout_nicely(G))

#Creando un df para el análisis de la centralidad
centralidad <- degree(G) #cómputo
centralidad #comrpobación
centralidad_df <- as.data.frame(centralidad) #convertir en data frame
centralidad_df <-cbind(autor = rownames(centralidad_df), centralidad_df) #asignar un index
rownames(centralidad_df) <- 1:nrow(centralidad_df)
centr <- arrange(centralidad_df, -centralidad) #ordenar descendente
head(centr) #comprobar

#Creando un df para el análisis del puntaje de autoridad
autoridad <- authority.score(G)$vector #cómputo
autoridad_df <- as.data.frame(autoridad) #convertir en data frame
autoridad_df <-cbind(autor = rownames(autoridad_df), autoridad_df) #asignar un index
rownames(autoridad_df) <- 1:nrow(autoridad_df)
aut <- arrange(autoridad_df, -autoridad) #ordenar descendente
head(aut) #comprobar
head(aut, 10) #comprobar



#Creando un df donde se pueda visualizar quiénes son los autores con mayor índice de betweeness
library(dplyr)
puentes_df <- as.data.frame(puentes)
head(puentes_df)
puentes_df <- cbind(autor = rownames(puentes_df), puentes_df)# pasando los autores de índice
rownames(puentes_df) <- 1:nrow(puentes_df)                   # a columna
head(puentes_df)
puentes_imp <- arrange(puentes_df, -puentes)
head(puentes_imp)

#Representación de relaciones de M.Cruz
M.Cruz <- make_ego_graph(G, diameter(G), nodes = 'Cruz  Magdalena', mode = c("all"))[[1]]

# Obtener la distancia en pasos desde el vértice de M.Cruz
dists <- distances(M.Cruz, "Cruz  Magdalena")

# Crear una paleta de colores.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")

# asignar colores a la red de M.Cruz.
V(M.Cruz)$color <- colors[dists+1]

# Visualizar la red a partir de la distancia en pasos desde el vértice M.Cruz.
plot(M.Cruz,
vertex.label = dists,
vertex.label.color = "white",
vertex.label.cex = .6,
edge.color = 'black',
vertex.size = 7,
edge.arrow.size = .05,
main = "Distancia en pasos desde M.Cruz"
)

#Representación de relaciones de Úrsula Puentes
U.Puentes <- make_ego_graph(G, diameter(G), nodes = 'Puentes Puentes  Ursula', mode = c("all"))[[1]]

# Obtener la distancia en pasos desde el vértice de U.Puentes
dists <- distances(U.Puentes, "Puentes Puentes  Ursula")

# Crear una paleta de colores.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")

# asignar colores a la red de U.Puentes.
V(U.Puentes)$color <- colors[dists+1]

# Visualizar la red a partir de la distancia en pasos desde el vértice U.Puentes.
plot(U.Puentes,
vertex.label = dists,
vertex.label.color = "white",
vertex.label.cex = .6,
edge.color = 'black',
vertex.size = 7,
edge.arrow.size = .05,
main = "Distancia en pasos desde U.Puentes"
)


#Representación de relaciones de Rodríguez Cabral  Jovanny María
JM.Rod <- make_ego_graph(G, diameter(G), nodes = 'Rodríguez Cabral  Jovanny María', mode = c("all"))[[1]]
# Obtener la distancia en pasos desde el vértice de JM.Rod
dists <- distances(JM.Rod, "Rodríguez Cabral  Jovanny María")
# Crear una paleta de colores.
colors <- c("black", "red", "orange", "blue", "dodgerblue", "cyan")
# asignar colores a la red de JM.Rod.
V(JM.Rod)$color <- colors[dists+1]
# Visualizar la red a partir de la distancia en pasos desde el vértice JM.Rod.
plot(JM.Rod,
vertex.label = dists,
vertex.label.color = "white",
vertex.label.cex = .6,
edge.color = 'black',
vertex.size = 7,
edge.arrow.size = .05,
main = "Distancia en pasos desde JM.Rod"
)

# Get density of a graph
G_dens <- edge_density(G)
G_dens

# Get the diameter of the graph g
diameter(G, directed = FALSE)
# Get the average path length of the graph g
G_ave_path_length <- mean_distance(G, directed = FALSE)
G_ave_path_length

# Identify the largest cliques in the network
largest_cliques(G)
# Determine all maximal cliques in the network and assign to object 'clq'
clq <- max_cliques(G)
# Calculate the size of each maximal clique.
table(unlist(lapply(clq, length)))



#Grafos interactivos 3D
library(threejs)
# Set a vertex attribute called 'color' to 'dodgerblue'
G3D <- set_vertex_attr(G, "color", value = "dodgerblue")
# Redraw the graph and make the vertex size 1
graphjs(G3D, vertex.size = 1)

#poniendo las etiquetas
Gnames <- set_vertex_attr(G, "label", value = V(G)$name)
Gnames <-graphjs(Gnames, vertex.size = 1, repulsion = 2)
points3d(Gnames, vertices(Gnames), color="black", pch=V(G)$name, size = 0.2)




# Create numerical vector of vertex eigenvector centralities. 
ec <- as.numeric(eigen_centrality(G)$vector)
# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 2
v <- 2*sqrt(ec)
# Plot threejs plot of graph setting vertex size to v
label <- set_vertex_attr(G, "label", value = V(G)$name)
#incorporando etiquetas
eigen <- graphjs(label, vertex.size = v,  main = "Puntaje de autoridad")
points3d(eigen, vertices(eigen), size = 0.1, color="black", pch=V(G)$name)
#grafo plano
autoridad <- authority.score(G)$vector
plot(G, main = "Puntaje de autoridad",vertex.label.cex = .8,label.dist = 0.5, asp = 0.7, vertex.size = autoridad*25, layout = layout_nicely(G))


#Comunidades
gc = edge.betweenness.community(G)
sizes(gc)
plot(gc, G, main = "Comunidades de autoría", vertex.size = 7, asp = 0.7, layout = layout_nicely(G))

# Create an object 'i' containin the memberships of the fast-greedy community detection
i <-  membership(gc)
# Check the number of different communities
sizes(gc)
# Add a color attribute to each vertex, setting the vertex color based on community membership
G_com <- set_vertex_attr(G, "color", value = c("midnightblue", "red", "orange", "blue", "dodgerblue", "cyan","brown","gold", "deeppink", "gray54", "darkorchid", "forestgreen", "plum2", "antiquewhite","aquamarine")[i])
# Plot the graph using threejs
graphjs(G_com)

#Proceso para visualizar un grafo con relaciones ponderadas
##Constuir matriz de adyacencia
adj.mat <- as_adjacency_matrix(G_pond, type = "both", names = TRUE, sparse = FALSE)
adj.mat

#Crear el grafo
G_pond <- graph.adjacency(adj.mat, weighted = TRUE, mode = "undirected",diag = FALSE)

#visualizar el grafo
plot(G_pond, edge.width=E(G_pond)$weight)

#comunidades 3D
G_com <- set_vertex_attr(G, "color", value = c("aliceblue", "lavenderblush2", "orange", "deepskyblue", "burlywood1", "cyan","darkolivegreen1","gold", "deeppink", "gray54", "darkorchid", "forestgreen", "plum2", "antiquewhite","aquamarine")[i])
com3D <- graphjs(G_com,  vertex.size = 1.5, edge.color= "dodgerblue", main = "Comunidades de autoría")
points3d(com3D, vertices(com3D), size = 0.1, color="black", pch=V(G)$name)
