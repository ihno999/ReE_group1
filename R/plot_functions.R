plot_collaboration_network <- function(rd, field_label) {
    # Create node sets
    project_nodes <- rd$projects %>%
        dplyr::transmute(
            id = project_id,
            label = paste0("Project ", project_id),
            group = "Project"
        )

    researcher_nodes <- rd$researchers %>%
        dplyr::distinct(researcher_id, name) %>%
        dplyr::transmute(
            id = researcher_id,
            label = name,
            group = "Researcher"
        )

    company_nodes <- rd$companies %>%
        dplyr::distinct(company_id, name = sectors) %>%
        dplyr::transmute(
            id = company_id,
            label = name,
            group = "Company"
        )

    nodes <- dplyr::bind_rows(project_nodes, researcher_nodes, company_nodes)

    # Create edges
    edges_r <- rd$researchers %>%
        dplyr::transmute(from = project_id, to = researcher_id)
    edges_c <- rd$companies %>%
        dplyr::transmute(from = project_id, to = company_id)
    edges <- dplyr::bind_rows(edges_r, edges_c)

    # Define colors per node group
    visNetwork::visNetwork(nodes, edges,
        main = paste("Collaboration Network - Field:", field_label)
    ) %>%
        visNetwork::visGroups(groupname = "Project", color = "#E69F00") %>%
        visNetwork::visGroups(groupname = "Researcher", color = "#56B4E9") %>%
        visNetwork::visGroups(groupname = "Company", color = "#009E73") %>%
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visNetwork::visPhysics(stabilization = TRUE)
}
