# ------------------------------------------------------------
# Shiny App: 2024 ACS Divorce Rates + K-means Clustering
# ------------------------------------------------------------
library(DT)

library(shiny)
library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(ggplot2)
library(htmltools)  # for HTML()

options(tigris_use_cache = TRUE)

# ⭐ 关键：不再在线 get_acs，只读你刚保存的 RDS ⭐
state_data <- readRDS("state_data_2024.rds")

# -------------------- 2. UI ----------------------------

ui <- fluidPage(
  withMathJax(),
  titlePanel("State-Level Divorce Rates in the U.S.: An Interactive Mapping and K-means Clustering Analysis Based on 2024 ACS"),
  
  tabsetPanel(
    # -------- Tab 1: Map & summary --------------------------------
    tabPanel(
      "Map & Summary",
      sidebarLayout(
        sidebarPanel(
          helpText("Data source: U.S. Census Bureau, 2024 ACS 1-year, Table B06008"),
          
          selectInput(
            inputId = "group",
            label   = "Divorce rate by place of birth:",
            choices = list(
              "All (15+)"                        = "all",
              "Born in this state"              = "born_state",
              "Born in other states"            = "other_state",
              "U.S. citizens born outside U.S." = "native_abroad",
              "Foreign-born population"         = "foreign_born"
            ),
            selected = "all"
          ),
          
          checkboxInput(
            inputId = "show_percent",
            label   = "Show percentage (×100)",
            value   = TRUE
          ),
          
          hr(),
          h4("Distribution of divorce rates"),
          plotOutput("hist_plot", height = "250px"),
          
          br(),
          h5("Summary statistics of divorce rate across states"),
          tableOutput("summary_stats"),
          br(),
          h5("Interpretation for selected group"),
          htmlOutput("group_summary")   #解释文字现在就在 summary_stats 下方了
        ),
        
        mainPanel(
          leafletOutput("map", width = "100%", height = "600px"),
          br(),
          h4("Divorce rates by state (sorted by selected group)"),
          fluidRow(
            column(
              width = 6,
              tableOutput("summary_table")
            ),
            column(
              width = 6,
              plotOutput("bar_plot", height = "800px")
            )
          )
        )
      )
    ),
    
    # -------- Tab 2: K-means clustering ---------------------------
    tabPanel(
      "K-means clustering",
      h4("Overview of K-means clustering"),
      helpText(HTML("
    K-means clustering is used to group U.S. states by similarity in divorce rates 
    and sociodemographic indicators. The algorithm assigns each state to the nearest 
    cluster centroid and iteratively updates these centroids until convergence.<br><br>
    
    The objective minimized by K-means is:<br>
    $$ \\sum_{j=1}^{k} \\sum_{x_i \\in C_j} \\| x_i - \\mu_j \\|^2, $$<br>
    where lower values indicate tighter, more coherent clusters.<br><br>
    
    The number of clusters \\(k\\) is chosen using the elbow method, which examines 
    how total within-cluster sum of squares decreases as \\(k\\) increases.<br><br>
    
    Because K-means may converge to local minima, this app uses 
    <strong>25 random initializations</strong> (nstart = 25) and selects the best solution.
  ")),
      sidebarLayout(
        sidebarPanel(
          helpText("Cluster states using divorce rates and socio-demographic indicators."),
          
          checkboxInput(
            "scale_vars",
            "Scale variables before clustering",
            value = TRUE
          ),
          
          sliderInput(
            "k_range",
            "Range of k for elbow plot",
            min   = 2,
            max   = 10,
            value = c(2, 6),
            step  = 1
          ),
          
          numericInput(
            "k_chosen",
            "Number of clusters k (for final solution)",
            value = 3,
            min   = 2,
            max   = 10,
            step  = 1
          ),
          
          tags$hr(),
          tags$strong("Choosing the Number of Clusters"),
          helpText(
            "The WSS shows a steep drop from k = 2 to k = 3, but the marginal improvement ",
            "decreases markedly for k > 3. This 'elbow' indicates that three clusters ",
            "balance compactness and interpretability; larger k would over-partition the ",
            "data without revealing substantially new structure."
          ),
          
          # 下面这一块只在 k = 3 时显示：PCA + Cluster Profiles
          conditionalPanel(
            condition = "input.k_chosen == 3",
            
            tags$strong("PCA Visualization"),
            helpText(
              "Each point in this figure represents a state, positioned by its scores on ",
              "the first two principal components calculated from all divorce and socio-",
              "demographic variables, and colored by its K-means cluster (k = 3). ",
              "The horizontal axis (PC1) provides clear separation among the clusters: ",
              "states in Cluster 2 (blue) lie on the far left and correspond to low divorce ",
              "rates and strong socioeconomic conditions, states in Cluster 1 (red) lie on ",
              "the far right and reflect high divorce rates and economic disadvantage, and ",
              "Cluster 3 (green) occupies the middle range with moderate divorce and mixed ",
              "socioeconomic profiles. ",
              "The vertical axis (PC2) primarily reflects within-cluster variation rather ",
              "than forming additional clearly separated groups. ",
              "Overall, the PCA projection shows that the three clusters occupy distinct ",
              "regions of the multivariate space, indicating that the K-means solution ",
              "captures meaningful structure in the joint distribution of divorce rates and ",
              "socio-demographic indicators across states."
            
            ),
            
            tags$br(),
            tags$strong("Cluster Profiles: Sociodemographic and Divorce-Rate Characteristics Across U.S. States"),
            
            helpText(
              "Cluster 1 — Low divorce, high socioeconomic advantage. Representative states ",
              "include California, New York, Massachusetts, New Jersey, Connecticut, Hawaii, ",
              "Washington, and Colorado. These states have the lowest divorce rates across ",
              "birth-origin groups, high median income and education, and low poverty and ",
              "unemployment, despite high levels of migration."
            ),
            
            helpText(
              "Cluster 2 — High divorce, high economic stress. Representative states include ",
              "New Mexico, West Virginia, Kentucky, Oklahoma, Arkansas, Alabama, Mississippi, ",
              "and Louisiana. These states show the highest divorce rates for almost all birth ",
              "groups and are characterized by high poverty and unemployment, low median ",
              "income, and lower educational attainment, concentrated in the Deep South and ",
              "Appalachia."
            ),
            
            helpText(
              "Cluster 3 — Moderate divorce, mixed socioeconomic profile. Representative ",
              "states include Maine, Vermont, Oregon, Indiana, Idaho, Missouri, Ohio, Georgia, ",
              "Wisconsin, and Montana. Divorce rates and socioeconomic indicators are in the ",
              "middle range: incomes and education are moderate, poverty is higher than in ",
              "Cluster 1 but lower than in Cluster 2, and the cluster mixes Midwest, New ",
              "England, and Mountain West states, forming a heterogeneous middle group."
            )
          )
        ),
        
        mainPanel(
          h4("Elbow plot: total within-cluster sum of squares vs. k"),
          plotOutput("elbow_plot", height = "300px"),
          
          br(),
          h4("K-means clustering of states (PCA 2D projection)"),
          plotOutput("cluster_plot", height = "350px"),
          
          br(),
          h4("K-means clusters on U.S. map"),
          leafletOutput("cluster_map", height = "350px"),
          
          br(),
          h4("Cluster membership and key averages"),
          DTOutput("cluster_table"),
          
          br(),
          h4("Divorce rates by place of birth within clusters"),
          DTOutput("cluster_divorce_table"),
          
          br(),
          h4("Conclusion"),
          verbatimTextOutput("cluster_conclusion")
        )
      )
    ),
    
    # -------- Tab 3: Data exploration --------------------------- 
    tabPanel(
      "Data exploration",
      sidebarLayout(
        sidebarPanel(
          helpText("Explore distributions and relationships of divorce and socio-demographic indicators."),
          
          selectInput(
            "eda_var",
            "Choose a variable:",
            choices = list(
              # Divorce-related rates (from B06008)
              "Divorce rate: all (15+)"                   = "divorce_rate_all",
              "Divorce rate: born in state"               = "divorce_rate_born_state",
              "Divorce rate: other U.S. state"            = "divorce_rate_other_state",
              "Divorce rate: U.S. citizens born abroad"   = "divorce_rate_native_abroad",
              "Divorce rate: foreign-born population"     = "divorce_rate_foreign_born",
              
              # Income & poverty (B19013, B17001)
              "Median household income"                   = "median_income",
              "Poverty rate"                              = "poverty_rate",
              
              # Labor market & education (B23025, B15003)
              "Unemployment rate"                         = "unemployment_rate",
              "Share with college degree or above"        = "college_plus_rate",
              
              # Migration / nativity (B06008, B03003)
              "Share foreign-born"                        = "foreign_born_share",
              "Share White alone"                         = "white_share",
              "Share Hispanic/Latino"                     = "hispanic_share",
              
              # Age & housing (B01002, B25003, B25064, B25077)
              "Median age"                                = "median_age",
              "Owner-occupied housing rate"               = "owner_occ_rate",
              "Median gross rent"                         = "median_rent",
              "Median home value"                         = "median_home_value"
            ),
            selected = "divorce_rate_all"
          ),
          
          br(),
          h5("Heatmap interpretation"),
          htmlOutput("heatmap_note")
        ),
        mainPanel(
          h4("Histogram of selected variable across states"),
          plotOutput("eda_hist", height = "260px"),
          
          br(),
          h4("Boxplot of selected variable by overall divorce level"),
          helpText("States are split into two groups by the median of overall divorce rate (all 15+)."),
          plotOutput("eda_box", height = "260px"),
          
          br(),
          h4("Correlation heatmap of key indicators"),
          plotOutput("corr_plot", height = "420px")
        )
      )
    
    
    )
  )
)


# -------------------- 3. SERVER ----------------------------

server <- function(input, output, session) {
  cluster_colors <- c(
    "1" = "#e41a1c",   # red
    "2" = "#377eb8",   # blue
    "3" = "#4daf4a"    # green
  )
  
  output$heatmap_note <- renderUI({
    HTML(paste(
      "<b>Heatmap Interpretation</b><br>",
      "The correlation heatmap shows that divorce rates across different demographic groups ",
      "are generally positively correlated, suggesting that states tend to exhibit consistent ",
      "patterns of marital stability regardless of subgroup. Among socioeconomic factors, ",
      "median income is negatively correlated with divorce rates, while poverty and unemployment ",
      "show positive correlations—indicating that economic stress is associated with higher ",
      "divorce levels. Education also shows a negative relationship with divorce, whereas housing ",
      "stability indicators display relatively weaker correlations. Overall, states with weaker ",
      "economic conditions and larger socially vulnerable populations tend to experience higher ",
      "divorce rates, whereas wealthier, better-educated, and more stable states exhibit ",
      "substantially lower divorce levels."
    ))
  })
  
  
  # ---- Text under k-chosen on K-means tab --------------------
  output$k_explanation <- renderUI({
    
    # 永远显示的第一段
    part1 <- paste0(
      "<b>Choosing the Number of Clusters</b><br>",
      "The WSS shows a steep drop from K=2 to K=3, but the marginal improvement ",
      "decreases markedly for K>3. This “elbow” indicates that three clusters is an ",
      "appropriate balance between compactness and interpretability. ",
      "A larger k would over-partition the data without revealing substantially new structure.<br><br>"
    )
    
    # 第二段仅在 k = 3 时显示
    if (input$k_chosen == 3) {
      part2 <- paste0(
        "<b>PCA Visualization</b><br>",
        "Although clustering is performed on all ten variables, PCA is used solely for visualization—",
        "to display high-dimensional structure in a 2D plot. ",
        "The first two principal components summarize:<br>",
        "<b>PC1:</b> socioeconomic gradient, where higher values correspond to lower income, ",
        "higher poverty, and lower college attainment.<br>",
        "<b>PC2:</b> variation in divorce patterns across demographic subgroups, ",
        "where higher values correspond to elevated divorce rates among more mobile populations ",
        "(e.g., born out-of-state)."
      )
    } else {
      part2 <- ""
    }
    
    HTML(paste0(part1, part2))
  })
  
  
  # ---- 3.1 Divorce rate data for Tab 1 ----------------------------
  
  rate_data <- reactive({
    df <- state_data |>
      mutate(
        rate = dplyr::case_when(
          input$group == "all"           ~ divorced_all / total,
          input$group == "born_state"    ~ born_state_divorced    / born_state_total,
          input$group == "other_state"   ~ other_state_divorced   / other_state_total,
          input$group == "native_abroad" ~ native_abroad_divorced / native_abroad_total,
          input$group == "foreign_born"  ~ foreign_born_divorced  / foreign_born_total,
          TRUE                           ~ NA_real_
        )
      )
    
    if (input$show_percent) {
      df <- df |>
        mutate(rate_display = rate * 100)
    } else {
      df <- df |>
        mutate(rate_display = rate)
    }
    
    df
  })
  
  # ---- 3.2 Map ----------------------------------------------------
  
  output$map <- renderLeaflet({
    df <- rate_data()
    
    pal <- colorNumeric(
      palette  = "YlOrRd",
      domain   = df$rate_display,
      na.color = "#f0f0f0"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Divorce rate: %s%s",
      df$NAME,
      ifelse(is.na(df$rate_display), "NA",
             sprintf("%.2f", df$rate_display)),
      ifelse(input$show_percent, "%%", "")
    ) |>
      lapply(HTML)
    
    leaflet(df) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor   = ~pal(rate_display),
        weight      = 1,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#666",
          dashArray   = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style    = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) |>
      addLegend(
        pal      = pal,
        values   = ~rate_display,
        opacity  = 0.7,
        title    = if (input$show_percent) "Divorce rate (%)" else "Divorce rate",
        position = "bottomright"
      )
  })
  
  # ---- 3.3 Histogram & summary stats ------------------------------
  
  output$hist_plot <- renderPlot({
    df <- rate_data()
    vals <- df$rate_display
    
    ggplot(df, aes(x = vals)) +
      geom_histogram(
        bins = 15,
        fill = "#2c7fb8",
        color = "white",
        alpha = 0.8
      ) +
      labs(
        x = if (input$show_percent) "Divorce rate (%)" else "Divorce rate",
        y = "Frequency",
        title = "Distribution of state divorce rates"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
      )
  })
  
  output$summary_stats <- renderTable({
    df   <- rate_data()
    vals <- df$rate_display
    vals <- vals[!is.na(vals)]
    
    data.frame(
      Statistic = c(
        "Mean",
        "Median",
        "Variance",
        "Min",
        "25% quantile",
        "75% quantile",
        "Max"
      ),
      Value = c(
        mean(vals),
        median(vals),
        var(vals),
        min(vals),
        quantile(vals, 0.25),
        quantile(vals, 0.75),
        max(vals)
      )
    )
  })
  # ---- 3.4b Text summary for selected group ----------------------
  output$group_summary <- renderUI({
    txt <- switch(
      input$group,
      
      "all" = paste(
        "Overall, the average state-level divorce rate is approximately 10.75%,",
        "with most states falling between 9.95% (25th percentile) and 11.60% (75th percentile).",
        "However, several states—such as Maine, New Mexico, Nevada, West Virginia, and Kentucky—show substantially higher rates, exceeding 12%.",
        "In contrast, states in the Midwest and Northeast tend to cluster at the lower end of the distribution, typically below 10%.",
        "The histogram further illustrates that the majority of states are concentrated around the 10–11% range, suggesting moderate dispersion.",
        "Taken together, the geographic patterns and statistical summary highlight non-uniform divorce patterns across the U.S.,",
        "with notable hotspots in both the Mountain West and parts of the South."
      ),
      
      "born_state" = paste(
        "When examining divorce rates for individuals who were born in the state where they currently reside,",
        "a clear geographic pattern emerges. The national average divorce rate is approximately 10.26%,",
        "with most states clustering between 9.36% (25th percentile) and 11.27% (75th percentile).",
        "However, several states in the Northeast and parts of the South—such as Maine (13.17%), Vermont (12.57%),",
        "Kentucky (12.46%), and West Virginia (12.38%)—show noticeably elevated divorce rates, highlighted on the map by darker tones.",
        "In contrast, states in the Mountain West and parts of the Midwest tend to exhibit lower rates, some below 8%,",
        "suggesting more stable marriage patterns among native-born residents.",
        "The histogram further shows a slightly right-skewed distribution, with a concentration around 10–11% and a small number of high-rate outliers."
      ),
      
      "other_state" = paste(
        "For individuals who were born in other U.S. states and currently reside elsewhere,",
        "divorce rates tend to be noticeably higher compared with native-born residents.",
        "The average statewide divorce rate for this group is 11.80%, with a median of 12.05%,",
        "reflecting a right-skewed distribution and greater variability.",
        "The map reveals several high-rate clusters across the South and Southwest,",
        "with states such as New Mexico (14.87%), Nevada (14.84%), Florida (14.07%),",
        "and Arkansas (13.91%) reaching some of the highest rates in the country.",
        "In contrast, parts of the Midwest and Northeast exhibit lower rates, with some states around 8–10%.",
        "The histogram further shows a concentration of states between 11–13%,",
        "indicating that non-native residents experience elevated divorce rates in many regions."
      ),
      
      "native_abroad" = paste(
        "For U.S. citizens who were born outside the United States, divorce rates show greater variability",
        "and include some of the highest state-level values among all birth-origin groups.",
        "The mean divorce rate is 12.01%, and the median is 11.38%, but the distribution is notably wide,",
        "with state rates ranging from 5.35% to 21.37%.",
        "The map highlights several states with substantially elevated values—most prominently New Hampshire (21.37%),",
        "West Virginia (18.92%), Oklahoma (18.04%), and Wyoming (17.90%)—indicating that certain regions",
        "experience disproportionately high divorce rates among foreign-born U.S. citizens.",
        "In contrast, a handful of states across the Midwest and West appear lighter on the map,",
        "reflecting rates closer to 6–8%. The histogram further supports the large spread:",
        "while many states cluster around 10–14%, multiple high-end outliers push the distribution’s upper tail to nearly 22%."
      ),
      
      "foreign_born" = paste(
        "Among the foreign-born population, divorce rates across U.S. states are generally lower",
        "compared with U.S.-born groups. The mean divorce rate is 8.28%, and the distribution is relatively concentrated,",
        "with most states falling between 7.43% (25th percentile) and 9.15% (75th percentile).",
        "The map indicates a few states with higher divorce rates among foreign-born residents—most notably Florida (11.99%),",
        "Nevada (11.88%), and Alaska (11.11%)—while many states in the Midwest and Northeast appear in lighter colors,",
        "reflecting rates near 7–8%. The histogram supports this pattern, showing a clustering of states around the 7–9% range",
        "with only a small number of upper-end outliers."
      ),
      
      # 默认兜底（理论上不会用到）
      "No description available for this group."
    )
    
    # 用 <p> 包起来，方便显示多行文字
    HTML(txt)
  })
  
  
  # ---- 3.4 Sorted table & bar plot -------------------------------
  
  output$summary_table <- renderTable({
    rate_data() |>
      st_drop_geometry() |>
      select(
        State       = NAME,
        DivorceRate = rate_display
      ) |>
      arrange(desc(DivorceRate))
  })
  
  output$bar_plot <- renderPlot({
    df <- rate_data() |>
      st_drop_geometry() |>
      select(
        State       = NAME,
        DivorceRate = rate_display
      ) |>
      arrange(desc(DivorceRate))
    
    ggplot(df, aes(
      x = DivorceRate,
      y = reorder(State, DivorceRate)
    )) +
      geom_col() +
      labs(
        x = if (input$show_percent) "Divorce rate (%)" else "Divorce rate",
        y = "State",
        title = "Divorce rate by state"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8)
      )
  })
  
  # ---------------- 3.5 Clustering data (Tab 2) -------------------
  clust_data <- reactive({
    df <- state_data |>
      mutate(
        divorce_rate_all           = divorced_all / total,
        divorce_rate_born_state    = born_state_divorced    / born_state_total,
        divorce_rate_other_state   = other_state_divorced   / other_state_total,
        divorce_rate_native_abroad = native_abroad_divorced / native_abroad_total,
        divorce_rate_foreign_born  = foreign_born_divorced  / foreign_born_total,
        poverty_rate = dplyr::if_else(
          poverty_total > 0,
          poverty_below / poverty_total,
          NA_real_
        ),
        unemployment_rate = dplyr::if_else(
          unemployment_total > 0,
          unemployment_unemp / unemployment_total,
          NA_real_
        ),
        college_plus_rate = dplyr::if_else(
          edu_total > 0,
          (edu_bachelor + edu_master + edu_prof + edu_phd) / edu_total,
          NA_real_
        ),
        foreign_born_share = dplyr::if_else(
          total > 0,
          foreign_born_total / total,
          NA_real_
        )
      ) |>
      st_drop_geometry() |>
      select(
        State = NAME,
        divorce_rate_all,
        divorce_rate_born_state,
        divorce_rate_other_state,
        divorce_rate_native_abroad,
        divorce_rate_foreign_born,
        median_income,
        poverty_rate,
        unemployment_rate,
        college_plus_rate,
        foreign_born_share
      )
    
    # 先去掉有 NA / Inf / NaN 的行
    df <- df |>
      filter(dplyr::if_all(-State, ~ is.finite(.)))
    
    # 数值部分
    num <- df[, -1, drop = FALSE]
    
    # 根据需要标准化
    if (input$scale_vars) {
      num <- scale(num)
      num <- as.data.frame(num)
    } else {
      num <- as.data.frame(num)
    }
    
    # 删掉 sd=0 的常数列
    keep_cols <- sapply(num, function(v) sd(v, na.rm = TRUE) > 0)
    num <- num[, keep_cols, drop = FALSE]
    
    # 返回干净数据
    out <- cbind(State = df$State, num)
    out
  })
  
  kmeans_fit <- reactive({
    df <- clust_data()
    X  <- as.matrix(df[, -1])
    set.seed(123)
    kmeans(X, centers = input$k_chosen, nstart = 25)
  })
  
  # ---- 3.6 Elbow plot -------------------------------------------
  output$elbow_plot <- renderPlot({
    df <- clust_data()
    X  <- as.matrix(df[, -1])
    
    k_min <- input$k_range[1]
    k_max <- input$k_range[2]
    ks    <- k_min:k_max
    
    set.seed(123)
    wss <- sapply(ks, function(k) {
      kmeans(X, centers = k, nstart = 25)$tot.withinss
    })
    
    elbow_df <- data.frame(k = ks, wss = wss)
    
    ggplot(elbow_df, aes(x = k, y = wss)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Number of clusters k",
        y = "Total within-cluster sum of squares",
        title = "Elbow plot for k-means clustering"
      ) +
      theme_minimal()
  })
  
  # ---- 3.7 Cluster scatter (PCA 2D) -----------------------------
  output$cluster_plot <- renderPlot({
    df <- clust_data()
    km <- kmeans_fit()
    
    X   <- as.matrix(df[, -1])
    pca <- prcomp(X, scale. = FALSE)
    
    scores <- as.data.frame(pca$x[, 1:2])
    scores$State   <- df$State
    scores$cluster <- factor(km$cluster)
    
    ggplot(scores, aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(size = 3) +
      scale_color_manual(values = cluster_colors)+
      labs(
        title = paste0("K-means clustering of states (k = ", input$k_chosen, ")"),
        x = "PC1 (from divorce & socio-demographics)",
        y = "PC2"
      ) +
      theme_minimal()
  })
  
  # ---- 3.7b Cluster map on U.S. map -----------------------------
  output$cluster_map <- renderLeaflet({
    df <- clust_data()
    km <- kmeans_fit()
    
    # 给每个州加上 cluster 标签
    df$cluster <- factor(km$cluster)
    
    # 把 cluster 信息并回原始 sf（state_data 有几何）
    cluster_sf <- state_data |>
      left_join(df |> select(State, cluster),
                by = c("NAME" = "State"))
    
    # 颜色映射（按 cluster 上色）
    pal <- colorFactor(
      palette  = cluster_colors,   # ⭐ 使用同一套颜色
      domain   = names(cluster_colors),
      na.color = "#dddddd"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Cluster: %s",
      cluster_sf$NAME,
      ifelse(is.na(cluster_sf$cluster), "Not clustered", cluster_sf$cluster)
    ) |>
      lapply(HTML)
    
    leaflet(cluster_sf) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        fillColor   = ~pal(cluster),
        weight      = 1,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#666",
          dashArray   = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style    = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) |>
      addLegend(
        pal      = pal,
        values   = ~cluster,
        opacity  = 0.7,
        title    = "K-means cluster",
        position = "bottomright"
      )
  })
  
  # ---- 3.8b Divorce rates by place of birth within clusters ----
  output$cluster_divorce_table <- DT::renderDT({
    df <- clust_data()
    km <- kmeans_fit()
    
    # 加上 cluster 标签
    df$cluster <- factor(km$cluster)
    
    # 只保留州名、cluster 和五个离婚率
    tab <- df |>
      dplyr::select(
        State,
        cluster,
        divorce_rate_all,
        divorce_rate_born_state,
        divorce_rate_other_state,
        divorce_rate_native_abroad,
        divorce_rate_foreign_born
      ) |>
      dplyr::arrange(cluster, dplyr::desc(divorce_rate_all))
    
    # 需要画条形的列（全部用同一个量纲）
    numeric_cols <- c(
      "divorce_rate_all",
      "divorce_rate_born_state",
      "divorce_rate_other_state",
      "divorce_rate_native_abroad",
      "divorce_rate_foreign_born"
    )
    
    # ⭐ 关键：所有这些列共用同一个 range，这样条形长度可比
    global_range <- range(tab[, numeric_cols], na.rm = TRUE)
    
    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        dom = "tip",
        ordering = TRUE
      )
    )
    
    # 数值格式（百分比的话可以 *100 再 format，你现在是比例就保留 2–3 位小数）
    dt <- DT::formatRound(dt, columns = numeric_cols, digits = 3)
    
    # 使用同一个 global_range 画 data bar
    for (col in numeric_cols) {
      dt <- DT::formatStyle(
        dt,
        columns = col,
        background = DT::styleColorBar(global_range, "#9ecae1"),
        backgroundSize = "100% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left center"
      )
    }
    
    dt
  })
  
  
  # ---- 3.8 Cluster table (with data bars) -----------------------
  output$cluster_table <- DT::renderDT({
    df <- clust_data()
    km <- kmeans_fit()
    
    df$cluster <- factor(km$cluster)
    
    tab <- df |>
      select(
        State,
        cluster,
        divorce_rate_all,
        median_income,
        poverty_rate,
        unemployment_rate,
        college_plus_rate,
        foreign_born_share
      ) |>
      arrange(cluster, desc(divorce_rate_all))
    
    # 除了 State / cluster 之外，其他列都是需要加色条的数值列
    numeric_cols <- setdiff(names(tab), c("State", "cluster"))
    
    # 基础 datatable
    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        dom = "tip",   # 只显示表格 + 分页 + 信息
        ordering = TRUE
      )
    )
    # ⭐ 把所有数值列格式化为 2 位小数
    dt <- DT::formatRound(dt, columns = numeric_cols, digits = 2)
    
    # 给每个数值列加「数据条」样式
    for (col in numeric_cols) {
      rng <- range(tab[[col]], na.rm = TRUE)
      dt <- DT::formatStyle(
        dt,
        columns = col,
        background = DT::styleColorBar(rng, "#9ecae1"),
        backgroundSize = "100% 70%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left center"
      )
    }
    
    dt
  })
  # ---- 3.10 Data exploration (Tab 3) --------------------------- 
  
  # 用 state_data 构造一个专门用于 EDA 的数据框
  eda_data <- reactive({
    state_data |>
      st_drop_geometry() |>
      # 先算各种 divorce rate（来自 B06008）
      mutate(
        divorce_rate_all           = divorced_all           / total,
        divorce_rate_born_state    = born_state_divorced    / born_state_total,
        divorce_rate_other_state   = other_state_divorced   / other_state_total,
        divorce_rate_native_abroad = native_abroad_divorced / native_abroad_total,
        divorce_rate_foreign_born  = foreign_born_divorced  / foreign_born_total
      ) |>
      # 选一批用于展示的变量（覆盖 10+ 个 ACS 表）
      transmute(
        State = NAME,
        divorce_rate_all,
        divorce_rate_born_state,
        divorce_rate_other_state,
        divorce_rate_native_abroad,
        divorce_rate_foreign_born,
        median_income,
        poverty_rate,
        unemployment_rate,
        college_plus_rate,
        foreign_born_share,
        median_age,
        owner_occ_rate,
        median_rent,
        median_home_value,
        white_share,
        hispanic_share
      ) |>
      # 删掉含有非有限值的州
      filter(dplyr::if_all(-State, ~ is.finite(.)))
  })
  
  # 直方图：选中的变量在 50 州的分布
  output$eda_hist <- renderPlot({
    df  <- eda_data()
    var <- input$eda_var
    
    x <- df[[var]]
    
    pretty_name <- switch(
      var,
      "divorce_rate_all"            = "Divorce rate: all (15+)",
      "divorce_rate_born_state"     = "Divorce rate: born in state",
      "divorce_rate_other_state"    = "Divorce rate: other U.S. state",
      "divorce_rate_native_abroad"  = "Divorce rate: U.S. citizens born abroad",
      "divorce_rate_foreign_born"   = "Divorce rate: foreign-born population",
      "median_income"               = "Median household income",
      "poverty_rate"                = "Poverty rate",
      "unemployment_rate"           = "Unemployment rate",
      "college_plus_rate"           = "Share with college degree or above",
      "foreign_born_share"          = "Share foreign-born",
      "median_age"                  = "Median age",
      "owner_occ_rate"              = "Owner-occupied housing rate",
      "median_rent"                 = "Median gross rent",
      "median_home_value"           = "Median home value",
      "white_share"                 = "Share White alone",
      "hispanic_share"              = "Share Hispanic/Latino",
      var
    )
    
    plot_df <- data.frame(x = x)
    
    ggplot(plot_df, aes(x = x)) +
      geom_histogram(
        bins  = 15,
        color = "white",
        fill  = "#2c7fb8",
        alpha = 0.8
      ) +
      labs(
        x     = pretty_name,
        y     = "Number of states",
        title = paste("Distribution of", pretty_name, "across states")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # 箱线图：按 overall divorce rate（高/低）分组比较
  output$eda_box <- renderPlot({
    df <- eda_data()
    
    # 用 overall divorce rate 的中位数把州分成高/低两组
    med_div <- median(df$divorce_rate_all, na.rm = TRUE)
    
    df <- df |>
      mutate(
        divorce_group = if_else(
          divorce_rate_all > med_div,
          "High divorce (above median)",
          "Low divorce (at/below median)"
        )
      )
    
    var <- input$eda_var
    pretty_name <- switch(
      var,
      "divorce_rate_all"            = "Divorce rate: all (15+)",
      "divorce_rate_born_state"     = "Divorce rate: born in state",
      "divorce_rate_other_state"    = "Divorce rate: other U.S. state",
      "divorce_rate_native_abroad"  = "Divorce rate: U.S. citizens born abroad",
      "divorce_rate_foreign_born"   = "Divorce rate: foreign-born population",
      "median_income"               = "Median household income",
      "poverty_rate"                = "Poverty rate",
      "unemployment_rate"           = "Unemployment rate",
      "college_plus_rate"           = "Share with college degree or above",
      "foreign_born_share"          = "Share foreign-born",
      "median_age"                  = "Median age",
      "owner_occ_rate"              = "Owner-occupied housing rate",
      "median_rent"                 = "Median gross rent",
      "median_home_value"           = "Median home value",
      "white_share"                 = "Share White alone",
      "hispanic_share"              = "Share Hispanic/Latino",
      var
    )
    
    ggplot(df, aes(x = divorce_group, y = .data[[var]])) +
      geom_boxplot(fill = "#9ecae1", alpha = 0.8) +
      labs(
        x = "Overall divorce level (all 15+)",
        y = pretty_name,
        title = paste("Comparison of", pretty_name, "by overall divorce level")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # 相关热图：所有这些指标之间的相关性
  output$corr_plot <- renderPlot({
    df <- eda_data()
    
    num <- df[, -1, drop = FALSE]  # 去掉 State
    
    cor_mat <- stats::cor(num, use = "pairwise.complete.obs")
    
    cor_df <- as.data.frame(as.table(cor_mat))
    names(cor_df) <- c("Var1", "Var2", "Corr")
    
    ggplot(cor_df, aes(x = Var1, y = Var2, fill = Corr)) +
      geom_tile() +
      scale_fill_gradient2(
        limits   = c(-1, 1),
        midpoint = 0
      ) +
      coord_equal() +
      labs(
        x = NULL,
        y = NULL,
        fill = "Correlation",
        title = "Correlation between divorce and socio-demographic indicators"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  
  
  # ---- 3.9 Conclusion -------------------------------------------
  output$cluster_conclusion <- renderPrint({
    df <- clust_data()
    km <- kmeans_fit()
    
    df$cluster <- factor(km$cluster)
    
    cluster_means <- df |>
      group_by(cluster) |>
      summarise(
        n_states     = n(),
        mean_divorce = mean(divorce_rate_all),
        mean_income  = mean(median_income),
        mean_poverty = mean(poverty_rate),
        mean_college = mean(college_plus_rate),
        .groups = "drop"
      )
    
    print(cluster_means)
    
    cat("\nInterpretation:\n")
    cat(
      "States are clustered using divorce rates and socio-demographic indicators.\n",
      "Clusters with higher average divorce rates tend to have lower median income,\n",
      "higher poverty and unemployment, and lower college attainment, whereas\n",
      "clusters with lower divorce rates are generally richer and better-educated.\n",
      "This suggests that socioeconomic conditions are strongly related to state-level\n",
      "patterns in divorce.\n"
    )
  })
  
}

shinyApp(ui = ui, server = server)
