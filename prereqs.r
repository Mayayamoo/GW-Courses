source("courseparsing.r")

library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)
# Create a graph from the prereq relationship table. should be nicer and neater but lost interest, will come back to this later

prereq_relationships <- cl.prereq2 %>%
    filter(!is.na(prerequisites)) %>%
    select(course = sub_course, Title, prerequisites) %>%
    mutate(prerequisites = str_split(prerequisites, ", ")) %>%
    unnest(prerequisites)


prereq_graph <- graph_from_data_frame(
  d = prereq_relationships %>% select(prerequisites, course),
  directed = TRUE
)

# Calculate the number of prerequisites for each course
prereq_counts <- distances(
  prereq_graph,
  mode = "in"
)

# Create a data frame with the results
prereq_map <- data.frame(
  course = names(V(prereq_graph)),
  direct_prereqs = degree(prereq_graph, mode = "in"),
  all_prereqs = sapply(V(prereq_graph), function(v) {
    # Count all ancestors (prerequisites and their prerequisites)
    ancestors <- subcomponent(prereq_graph, v, mode = "in")
    length(ancestors) - 1  # Subtract 1 to exclude the course itself
  }),
  max_depth = sapply(V(prereq_graph), function(v) {
    # Find the maximum path length among all predecessors
    predecessors <- subcomponent(prereq_graph, v, mode = "in")
    if(length(predecessors) <= 1) return(0)  # No prerequisites
    
    # Calculate all shortest paths from prerequisites to this course
    dist <- distances(
      prereq_graph, 
      v = predecessors[predecessors != v], 
      to = v, 
      mode = "out"
    )
    
    # Return the maximum finite distance
    max_dist <- max(dist[is.finite(dist)])
    if(max_dist == -Inf) return(0) else return(max_dist)
  })
)

# Convert to tidygraph object
prereq_tidy <- as_tbl_graph(prereq_graph)

# Add node attributes - FIXED VERSION
prereq_tidy <- prereq_tidy %>%
  activate(nodes) %>%
  mutate(
    subject = str_extract(name, "^[A-Z]+"),
    prereq_count = centrality_degree(mode = "in")  # Changed from degree() to centrality_degree()
  )

# Plot with better styling
network <- ggraph(prereq_tidy, 
                  # Use layout_with_fr with correct parameters
                  layout = "fr", 
                  niter = 10000,        # More iterations for better spacing
                  area = 15*vcount(prereq_graph)^2,  # Larger area
                  repulserad = vcount(prereq_graph)^2.5  # Stronger repulsion
                 ) +
  geom_edge_link(arrow = arrow(length = unit(2, "mm")), 
                edge_alpha = 0.3,
                end_cap = circle(3, "mm")) +
  geom_node_point(aes(color = subject, size = prereq_count + 1)) + 
  geom_node_text(aes(label = name), 
                repel = TRUE, 
                size = 3,
                max.overlaps = 30) +  # Allow more repulsion
  scale_size_continuous(range = c(2, 6)) +
  theme_graph() +
  theme(legend.position = "right")

# Display at larger size (doubles the default dimensions)
ggsave("prerequisite_network.png", 
       plot = network, 
       width = 20, 
       height = 16, 
       dpi = 300)

# Also print to screen with specified dimensions
print(network)
