# 🧭 From Analytical Questions to Dashboard

### Theoretical Guide

------------------------------------------------------------------------

## 🎯 Learning Objectives

By the end of this module, you will be able to: \
1. Translate **analytical questions** into measurable data metrics.\
2. Design a **data-to-dashboard flow** using **Mermaid.js diagrams**.\
3. Understand the **reactive programming model** in Shiny (R or Python).\
4. Structure an end-to-end dashboard creation process logically before coding.

------------------------------------------------------------------------

## 🧩 1. From Analytical Questions to Metrics

### Principle

Dashboards exist to **answer business questions**.\
You have already translated these business questions to **analytical questions**.\
Now you can move forward in designing the dashboard.

**Steps:** \
1. Define what **data** and **metrics** are needed to answer each question.\
2. Define what **interactivity** is needed to answer each question.\
2. Choose appropriate **visualizations** and **interactive elements**

**Example thinking pattern:** \> Question → Metric → Data Source → Visualization → Interactivity

| Example |   |
|--------------------------------------|----------------------------------|
| “How are monthly sales evolving for each department” | → Metric: total sales per month → Visualization: line chart, Interactivity: dropdown with departments |
| “Which region performs best?” | → Metric: average revenue per region → Visualization: bar chart |

------------------------------------------------------------------------

## 🧮 2. From Metrics to Data Requirements

### Principle

Each metric relies on one or more **data transformations**.

**Steps:** \
1. Identify **input data sources** (CSV, database, API, etc.)\
2. Plan how data will be **cleaned and transformed** (filtering, aggregation, joining).\
3. Determine **derived variables** (e.g., retention rate, conversion rate).\
4. Document assumptions and definitions for consistency.

> IMPORTANT! Make sure you try to work as much as possible from the same reactive dataset to keep interactivity over the different visualisations/tabs in the dashboard!

**Conceptual flow:**

``` mermaid
flowchart TD
  A[Raw Data] --> B[Cleaning]
  B --> C[Transformation]
  C --> D[Aggregation]
  D --> E[Metrics Ready for Dashboard]
```

------------------------------------------------------------------------

## ⚙️ 3. Designing the Dashboard Data Flow

### Principle

A dashboard integrates **data processing** and **user interactivity**.\
Before coding, visualize how data flows through the system. For this you will make use of mermaid.js.

**Core Components:** \
- **Inputs:** user choices (date range, filters, metric type)\
- **Reactives:** dynamic computations that update when inputs change\
- **Outputs:** charts, KPIs, tables

**Typical flow diagram:**

``` mermaid
graph TD
  A[User Inputs] --> B[Reactive Data Filters]
  B --> C[Reactive Metrics]
  C --> D[Visual Outputs]
```

------------------------------------------------------------------------

## 🧠 4. Understanding Reactivity in Shiny

### Principle

Shiny (both R and Python) uses a **reactive programming model**.\
When an input changes, all dependent computations and outputs update automatically. See websites for details.

**Main reactive elements:** \

| Type | Purpose |
|------|---------|
| `Input` | User-controlled variables (e.g. date, dropdown, checkbox) |
| `Reactive` | Expressions that depend on inputs or other reactives |
| `Output` | Visualizations, tables, or values displayed in the app |


**Reactive logic example (conceptual):**

``` mermaid
graph TD
  InputA[Select Date Range] --> Reactive1[Filter Data]
  Reactive1 --> Reactive2[Aggregate Metrics]
  Reactive2 --> Output1[Plot]
  Reactive2 --> Output2[KPI Boxes]
```

------------------------------------------------------------------------

## 🧩 5. Structuring the Dashboard Development Process

### Step-by-Step Process:

1.  **Define Business Questions**
    -   What decisions does the dashboard support?\
    -   What questions must it answer?
2.  **Identify Data Sources**
    -   What data is available?\
    -   Are data formats compatible and clean?
3.  **Design Metrics and KPIs**
    -   Define how each metric is calculated.\
    -   Align with business logic and terminology.
4.  **Create a Data Flow Diagram**
    -   Use **Mermaid.js** to visualize steps from raw data → transformations → outputs.
5.  **Design the Reactive Flow**
    -   Map how user inputs trigger updates to data and outputs.
6.  **Prototype the Dashboard**
    -   Create a minimal working Shiny app with placeholder charts.
7.  **Iterate and Improve**
    -   Get user feedback, adjust metrics or layout, refine usability.

------------------------------------------------------------------------

## 🔁 6. The Dashboard Design Loop

Dashboard creation is **iterative** — each version improves based on user feedback.

``` mermaid
flowchart LR
  A[Business Questions] --> B[Data Collection & Processing]
  B --> C[Dashboard Prototype]
  C --> D[User Feedback]
  D --> A
```

**Tips for iteration:** 
- Start simple; add complexity gradually.\
- Validate each metric against reality.\
- Keep visuals clean and interpretable.\
- Document every data transformation.

------------------------------------------------------------------------

## 🧭 7. Guiding Principles for Effective Dashboards

| Principle         | Explanation                                        |
|-------------------|----------------------------------------------------|
| **Clarity**       | Every chart should answer one question clearly.    |
| **Consistency**   | Use consistent colors, units, and terminology.     |
| **Minimalism**    | Show only what supports decision-making.           |
| **Interactivity** | Let users explore data dynamically via inputs.     |
| **Traceability**  | Make it easy to trace visuals back to source data. |

------------------------------------------------------------------------

## 🗺️ 8. Summary Diagram

### The End-to-End Dashboard Design Concept

``` mermaid
flowchart TD
  A[Analytical Questions] --> B[Data Requirements]
  B --> C[Data Collection & Cleaning]
  C --> D[Metric Design]
  D --> E[Dashboard Flow Design]
  E --> F[Shiny Implementation]
  F --> G[Feedback & Refinement]
```

------------------------------------------------------------------------

## 📚 Suggested Learning Activities

-   Sketch a Mermaid.js diagram for any existing dashboard.\
-   Identify all reactive dependencies in a Shiny example app.\
-   Discuss how changing a business question alters the data flow.\
-   Critique a poorly designed dashboard and redesign its flow conceptually.

------------------------------------------------------------------------

## Prototype Your Dashboard Design
Use the [Mermaid Live Editor](https://mermaid.live/) to prototype your dashboard's data flow.\
Export the diagram as an image to include in your project documentation.

### Tips and tricks for shiny dashboards
- Use `reactiveVal()` or `reactiveValues()` to store intermediate states.
- Modularize your code with Shiny modules for better organization.
- Use `req()` to ensure inputs are available before processing.
- Leverage `observeEvent()` for actions triggered by user inputs.
- Use `validate()` and `need()` to handle input validation gracefully.
- Test your app with different input combinations to ensure robustness.
- Make use of Shiny's built-in themes or customize CSS for better aesthetics.
- Make use of the existing modules in the `shiny` package to speed up development. Don't reinvent the wheel!

------------------------------------------------------------------------


# Technically Speaking
## Folder structure for clean code

### Recommended Basic Shiny App Structure


#### For Shiny (R) with renv:
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


#### For Shiny for Python:
```
my_shiny_app/
├── app.py                   # Main app file
├── requirements.txt         # Python dependencies
├── data/                    # Data files
│   ├── raw/                 # Original data files
│   └── processed/           # Cleaned data files
├── modules/                 # Python modules/functions
│   ├── __init__.py         # Makes it a package
│   ├── data_functions.py   # Data processing functions
│   ├── plot_functions.py   # Plotting functions
│   └── helper_functions.py # Utility functions
├── static/                  # Static files (equivalent to www/)
│   ├── style.css           # Custom CSS
│   └── images/             # Images and icons
└── README.md               # Project documentation
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

### Example Structure Implementation

#### For Shiny (R):

**global.R:**

```r
# Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)

# Source helper functions
source("../R/data_functions.R")
source("../R/plot_functions.R")
source("../R/helper_functions.R")

# Load and prepare data once
sales_data <- read.csv("data/processed/sales_clean.csv") %>%
  mutate(date = as.Date(date))
```

**R/data_functions.R:**
```r
# Filter data based on user inputs
filter_sales_data <- function(data, date_range, selected_regions) {
  data %>%
    filter(
      date >= date_range[1],
      date <= date_range[2],
      region %in% selected_regions
    )
}

# Calculate summary metrics
calculate_monthly_sales <- function(data) {
  data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month, region) %>%
    summarise(
      total_sales = sum(sales, na.rm = TRUE),
      avg_sales = mean(sales, na.rm = TRUE),
      .groups = "drop"
    )
}
```

**Simple app.R structure:**
```r
# Source global file
source("global.R")

# Define UI and Server
ui <- fluidPage(...)
server <- function(input, output, session) {...}

# Run the app
shinyApp(ui = ui, server = server)
```

#### For Shiny for Python:

**app.py:**
```python
from shiny import App, render, reactive, ui
import pandas as pd
from modules.data_functions import filter_sales_data, calculate_monthly_sales
from modules.plot_functions import create_sales_plot, create_summary_table

# Load data once
sales_data = pd.read_csv("data/processed/sales_clean.csv")
sales_data['date'] = pd.to_datetime(sales_data['date'])

# Define UI
app_ui = ui.page_fluid(
    ui.panel_title("Sales Dashboard"),
    ui.layout_sidebar(
        ui.panel_sidebar(
            ui.input_date_range("date_range", "Select Date Range"),
            ui.input_checkbox_group("regions", "Select Regions",
                                   choices=list(sales_data['region'].unique()))
        ),
        ui.panel_main(
            ui.output_plot("sales_plot"),
            ui.output_data_frame("summary_table")
        )
    )
)

# Define Server
def server(input, output, session):
    
    @reactive.calc
    def filtered_data():
        return filter_sales_data(
            sales_data, 
            input.date_range(), 
            input.regions()
        )
    
    @reactive.calc  
    def monthly_data():
        return calculate_monthly_sales(filtered_data())
    
    @output
    @render.plot
    def sales_plot():
        return create_sales_plot(monthly_data())
    
    @output
    @render.data_frame
    def summary_table():
        return create_summary_table(monthly_data())

def filter_sales_data(data, date_range, selected_regions):
    """Filter data based on user inputs"""
    return data[
        (data['date'] >= date_range[0]) &
        (data['date'] <= date_range[1]) &
        (data['region'].isin(selected_regions))
    ]

def calculate_monthly_sales(data):
    """Calculate summary metrics"""
    return (data
            .assign(month=data['date'].dt.to_period('M'))
            .groupby(['month', 'region'])
            .agg({'sales': ['sum', 'mean']})
            .reset_index())
```

### Best Practices:

#### For Shiny (R):
- Keep all helper functions in separate files within the `R/` folder
- Use descriptive function names that explain what they do
- Make functions pure (same input = same output) when possible
- Use reactive expressions for expensive computations
- Source all helper functions in `global.R`
- Keep the main app file focused on UI layout and reactive logic
- Test functions independently before integrating into the app

#### For Shiny for Python:
- Organize helper functions in Python modules within the `modules/` folder
- Use Python package structure with `__init__.py` files
- Import functions explicitly at the top of `app.py`
- Use `@reactive.calc` decorators for reactive computations
- Use `requirements.txt` to manage Python dependencies
- Follow Python naming conventions (snake_case for functions)
- Use type hints for better code documentation and IDE support


### Best Practices for Function-Based Shiny Apps:
- Keep all helper functions in separate files within the `R/` folder
- Use descriptive function names that explain what they do
- Make functions pure (same input = same output) when possible
- Use reactive expressions for expensive computations
- Source all helper functions in `global.R`
- Keep the main app file focused on UI layout and reactive logic
- Test functions independently before integrating into the app
