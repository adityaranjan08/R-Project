
# Load Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(treemapify)

# Load & Clean Data
data <- read.csv("C:/Users/adity/Dropbox/PC/Documents/MTECH/SEM2/Data Cleaning - R/Project/GlassDoorDataset.csv", stringsAsFactors = FALSE)
data$salary_avg_estimate <- as.numeric(gsub("[^0-9]", "", data$salary_avg_estimate))
data <- data %>% filter(!is.na(industry) & industry != "")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Glassdoor DS Jobs 2024"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Salary & Ratings", tabName = "salary", icon = icon("money-bill")),
      menuItem("Company Stats", tabName = "company", icon = icon("building")),
      menuItem("Sector Insights", tabName = "sector", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        fluidRow(
          box(title = "Total Jobs", width = 4, status = "primary", solidHeader = TRUE, h3(nrow(data))),
          box(title = "Unique Companies", width = 4, status = "info", solidHeader = TRUE, h3(length(unique(data$company)))),
          box(title = "Unique Locations", width = 4, status = "success", solidHeader = TRUE, h3(length(unique(data$location))))
        ),
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE,
              "This dashboard explores Data Science job trends on Glassdoor for 2024 including company ratings, salaries, industry patterns, and job distributions."),
          box(plotOutput("ratingHist"), width = 12)
        )
      ),
      tabItem(tabName = "salary",
        fluidRow(
          box(plotOutput("topSalary"), width = 6),
          box(plotOutput("topRating"), width = 6)
        )
      ),
      tabItem(tabName = "company",
        fluidRow(
          box(plotOutput("employmentType"), width = 6),
          box(plotOutput("companySizeBar"), width = 6)
        )
      ),
      tabItem(tabName = "sector",
        fluidRow(
          box(plotOutput("wlbSector"), width = 6),
          box(plotOutput("industryDist"), width = 6)
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # 1. Histogram – Distribution of Ratings
  output$ratingHist <- renderPlot({
    ggplot(data, aes(x = company_rating)) +
      geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
      labs(title = "Distribution of Company Ratings", x = "Rating", y = "Count")
  })
  
  # 2. Horizontal Bar – Top 10 Companies by Salary
  output$topSalary <- renderPlot({
    data %>%
      group_by(company) %>%
      summarise(avg_salary = mean(salary_avg_estimate, na.rm = TRUE)) %>%
      arrange(desc(avg_salary)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = avg_salary, y = reorder(company, avg_salary))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(title = "Top 10 Companies by Avg Salary", x = "Avg Salary", y = "Company")
  })
  
  # 3. Dot Plot – Top 10 Companies by Rating
  output$topRating <- renderPlot({
    data %>%
      filter(!is.na(company_rating) & !is.na(company)) %>%
      group_by(company) %>%
      summarise(avg_rating = round(mean(company_rating, na.rm = TRUE), 2),
                count = n()) %>%
      filter(count >= 3) %>%
      arrange(desc(avg_rating)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = avg_rating, y = reorder(company, avg_rating))) +
      geom_point(color = "darkgreen", size = 5) +
      labs(title = "Top 10 Companies by Avg Rating", x = "Avg Rating", y = "Company") +
      theme_minimal()
  })
  
  # 4. Pie Chart – Employment Type Distribution
  output$employmentType <- renderPlot({
    emp_data <- data %>%
      filter(!is.na(employment_type)) %>%
      mutate(employment_type = ifelse(employment_type == "Unknown", "Other / Not Disclosed", employment_type)) %>%
      count(employment_type)
    
    ggplot(emp_data, aes(x = "", y = n, fill = employment_type)) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Employment Type Distribution", fill = "Type")
  })
  
  
  # 5. Colored Bar Chart – Company Size Distribution (Fixed)
  output$companySizeBar <- renderPlot({
    size_data <- data %>%
      filter(!is.na(company_size)) %>%
      mutate(company_size = ifelse(company_size == "Unknown", "Other / Not Disclosed", company_size)) %>%
      count(company_size) %>%
      arrange(desc(n))
    
    ggplot(size_data, aes(x = reorder(company_size, n), y = n, fill = company_size)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      labs(title = "Company Size Distribution", x = "Company Size", y = "Job Count") +
      theme_minimal()
  })
  
  
  # 6. Line Chart – Work-Life Balance by Sector
  output$wlbSector <- renderPlot({
    data %>%
      filter(!is.na(sector)) %>%
      group_by(sector) %>%
      summarise(avg_wlb = mean(work_life_balance_rating, na.rm = TRUE)) %>%
      arrange(desc(avg_wlb)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(sector, avg_wlb), y = avg_wlb, group = 1)) +
      geom_line(color = "tomato", size = 1.2) +
      geom_point(color = "black", size = 3) +
      coord_flip() +
      labs(title = "Top Sectors by Work-Life Balance", x = "Sector", y = "Avg WLB Rating")
  })
  
  # 7. Horizontal Bar Chart – Top Industries by Job Count (Fixed)
  output$industryDist <- renderPlot({
    industry_data <- data %>%
      filter(!is.na(industry) & industry != "" & industry != "--") %>%
      count(industry) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    ggplot(industry_data, aes(x = reorder(industry, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Industries by Job Count", x = "Industry", y = "Job Count") +
      theme_minimal()
  })
}


# Launch App
shinyApp(ui, server)
