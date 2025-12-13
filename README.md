# RnE (Group 1) 

An interactive dashboard to visualize the structure and relationships within an organization. The dashboard should provide insights into expertise areas, teams, projects, and external contacts . A key goal is to enable an overview of the research group’s activities  (projects, staff involvement, external collaborations) and to allow exploration of relationships, such as finding the shortest path between two individuals.

# What does the application do, and why?

Our application allows the customer to explore the relationships between researchers, projects, and companies through intuitive visualizations. The goal is to help our customer identify the right people for new projects by showing who has relevant expertise and connections. This enables better resource allocation and helps identify potential collaborators when starting new initiatives.

# Key features
## 1. Interactive Network Visualization (Researchers/Graph)

- A network graph showing connections between researchers, projects, and companies. It provides an intuitive overview of the entire relationship networks.
- Filtering options: By individual researcher, company and by project field

## 2. Researcher Detail (Researchers/Researcher Details)

- The researcher details page shows the distribution of how much companies were involved with the certain project fields for a given researcher.

## 3. Contact information (Companies/Company & Companies/People)

- The contact page is where you can filter by company/researcher and their projects to get the contacts details between the companies contact and the researcher


# Setup
> `R>` before a command means it should be run from an R console.

Download R: [https://www.r-project.org/](https://www.r-project.org/).

Intellij plugins:
 - CSV Editor, Martin Sommer
 - Mermaid, JetBrains s.r.o.
 - R Language, JetBrains s.r.o.

Install required packages:

[//]: # ( - `R> install.packages&#40;"renv"&#41;`)
 - `R> renv::restore()`

Run the app with `R> shiny::runApp()`



# Developers
- Brecht De Roover
- Ihno Van de Sande
- Matej Vesel

------------

# Miscellaneous
## How To
Install package: `install.packages("package-name")`\
Record the packages: `renv::snapshot()`

## Diagram
```mermaid

```

## Folder structure
Shiny (R) with renv:
```
my_shiny_app/
├── app.R                    # Main app file (single-file approach)
│   OR
├── ui.R                     # UI definition (multi-file approach)
├── server.R                 # Server logic (multi-file approach)
├── global.R                 # Global variables, libraries, and data loading
├── renv.lock               # Locked package versions (auto-generated)
├── renv/                   # renv system files (auto-generated)
│   ├── activate.R          # Project activation script
│   ├── library/            # Project-specific package library
│   └── settings.dcf        # renv settings
├── .Rprofile               # R startup script (sources renv/activate.R)
├── data/                   # Data files
│   ├── raw/                # Original data files
│   └── processed/          # Cleaned data files
├── R/                      # Helper functions (no modules)
│   ├── data_functions.R    # Data processing functions
│   ├── plot_functions.R    # Plotting functions
│   └── helper_functions.R  # Utility functions
├── www/                    # Static files
│   ├── style.css          # Custom CSS
│   └── images/            # Images and icons
└── README.md              # Project documentation
```

### Key Components Explained

**Core Files:** \
- `app.R`: Complete single-file app with UI and server\
- `global.R`: Load packages, source functions, prepare data\
- `ui.R` / `server.R`: Separate UI and server (alternative to single file)

**Helper Functions (`R/` folder):**\
- `data_functions.R`: Data cleaning, filtering, aggregation functions\
- `plot_functions.R`: Reusable plotting functions with ggplot2/plotly\
- `helper_functions.R`: General utility functions

## Best Practices
For Shiny (R):
- Keep all helper functions in separate files within the `R/` folder
- Use descriptive function names that explain what they do
- Make functions pure (same input = same output) when possible
- Use reactive expressions for expensive computations
- Source all helper functions in `global.R`
- Keep the main app file focused on UI layout and reactive logic
- Test functions independently before integrating into the app


Function-Based Shiny Apps:
- Keep all helper functions in separate files within the `R/` folder
- Use descriptive function names that explain what they do
- Make functions pure (same input = same output) when possible
- Use reactive expressions for expensive computations
- Source all helper functions in `global.R`
- Keep the main app file focused on UI layout and reactive logic
- Test functions independently before integrating into the app