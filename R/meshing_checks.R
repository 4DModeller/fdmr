mesh_checker <- function(mesh, n_obs) {
  # Suppose the number of observation data points is "n_obs", and
  # our mesh is an object obtained using INLA::inla.mesh.2d

  # Warning 1: if the mesh has a very high resolution?
  # We could check if the number of mesh nodes is equal or greater than n_obs via:
  mesh$n >= n_obs

  # We could check if the number of triangles is equal or greater than n_obs via:
  nrow(mesh$graph$tv) >= n_obs

  # Warning 2: if there are isolated triangles in the mesh?
  # Create a dataframe that stores all edges of the mesh, and the corresponding indexed vertices V1,V2
  edge_df <- base::cbind(
    c(mesh$graph$tv[, 1], mesh$graph$tv[, 1], mesh$graph$tv[, 2]),
    c(mesh$graph$tv[, 2], mesh$graph$tv[, 3], mesh$graph$tv[, 3])
  )

  # Create a dataframe that stores the number of edges that each vertex is incident with
  vertex_n_edges <- data.frame(table(as.numeric(edge_df)))
  colnames(vertex_n_edges) <- c("v", "n_edges")

  # If a vertex has only two incident edges, and those edges form a triangle with two other vertices,
  # then there is a risk that the triangle could become isolated from the rest of the graph.
  which(vertex_n_edges$n_edges <= 2)

  # Note:
  # In order for the triangle to be fully isolated, none of the two incident edges
  # should connect to any other vertices outside the triangle

  # Warning 3: do the triangle shapes in a mesh look normal?
  # e.g., check if there is a right triangle in the mesh
  nrow(mesh$graph$tv) # the number of triangles in the mesh

  vertex_edge_lengths <- data.frame(
    "v1" = mesh$graph$tv[, 1],
    "v2" = mesh$graph$tv[, 2],
    "v3" = mesh$graph$tv[, 3],
    "v1to2" = rep(NA, nrow(mesh$graph$tv)),
    "v1to3" = rep(NA, nrow(mesh$graph$tv)),
    "v2to3" = rep(NA, nrow(mesh$graph$tv))
  )

  for (i in 1:nrow(mesh$graph$tv)) {
    # i<-1
    # Now construct a matrix that stores the node/vertex indices comprising each edge in each triangle.
    # Each row in the matrix represents an edge of the triangle with two nodes
    each.tri.edge <- base::cbind(
      c(mesh$graph$tv[i, 1], mesh$graph$tv[i, 1], mesh$graph$tv[i, 2]),
      c(mesh$graph$tv[i, 2], mesh$graph$tv[i, 3], mesh$graph$tv[i, 3])
    )
    # Now create a graph from an edge list matrix
    graph_temp <- igraph::graph_from_edgelist(each.tri.edge, directed = FALSE)

    # Compute the Euclidean distance for each edge in the triangle
    edge.tri.length <- rep(NA, 3)
    edge.tri.length[1] <- sqrt(sum((mesh$loc[each.tri.edge[1, 1], ] - mesh$loc[each.tri.edge[1, 2], ])^2))
    edge.tri.length[2] <- sqrt(sum((mesh$loc[each.tri.edge[2, 1], ] - mesh$loc[each.tri.edge[2, 2], ])^2))
    edge.tri.length[3] <- sqrt(sum((mesh$loc[each.tri.edge[3, 1], ] - mesh$loc[each.tri.edge[3, 2], ])^2))
    vertex_edge_lengths[i, c("v1to2", "v1to3", "v2to3")] <- edge.tri.length
  }

  # Interpret "vertex_edge_lengths":
  # each row indicates a triangle
  # Column v1,v2,v3 correspond to vertices 1,2,3 respectively
  # v1to2 represents the edge length between vertices 1 and 2
  # v1to3 represents the edge length between vertices 1 and 3
  # v2to3 represents the edge length between vertices 2 and 3

  # Then we could check if there is a right triangle based on edge distances.
  # e.g., using Pythagorean theorem: a right triangle has a^2+b^2=c^2;

  # the triangle is obtuse if the sum of the squares of the two shorter sides of a triangle
  # is smaller than the square of the longest side;

  # the triangle is acute if the sum of the squares of the two shorter sides of a triangle
  # is greater than the square of the longest side.

  # Some papers suggested that the triangles closer to equilateral triangles work best.
}
